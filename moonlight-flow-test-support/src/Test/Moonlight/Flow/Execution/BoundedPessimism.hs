{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}

module Test.Moonlight.Flow.Execution.BoundedPessimism
  ( BoundedPessimismConfig (..),
    BoundedPessimismFailure (..),
    BoundedMaintenanceError (..),
    BoundedPessimismReport (..),
    boundedPessimismUnitConfig,
    boundedPessimismSoakConfig,
    runBoundedPessimismWorkGuarantee,
    boundedPessimismWorkGuarantee,
  )
where

import Control.Exception
  ( Exception,
    throwIO,
  )
import Control.Monad
  ( when,
  )
import Data.Bifunctor
  ( first,
  )
import Data.Bits
  ( shiftR,
    xor,
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Data.Time.Clock
  ( UTCTime,
    addUTCTime,
    getCurrentTime,
  )
import Data.Vector qualified as Vector
import Data.Word
  ( Word64,
  )
import Moonlight.Flow.Execution.Factor.Enumerate
  ( enumerateBagRows,
  )
import Moonlight.Flow.Execution.Factor.Run
  ( FactorRunError,
    runFactor,
  )
import Moonlight.Flow.Execution.Factor.Types
  ( FactorCache,
    FactorDemand (FactorDemandRows),
    factorInputFromStoreView,
    FactorRunResult (..),
    FactorRunSpec (..),
    emptyFactorCache,
  )
import Moonlight.Flow.Execution.Observe.Provenance.GC
  ( defaultProvGCConfig,
  )
import Moonlight.Flow.Execution.Observe.Telemetry
  ( defaultRepairTelemetryConfig,
    MaintenanceMetrics (..),
    NodeAction (..),
    NodeMaintenance (..),
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Delta.Signed
  ( MultiplicityChange (..)
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchFromList,
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    tupleKeyFromInts,
    tupleKeyToInts,
  )
import Moonlight.Flow.Plan.Query.Core
  ( BagId (..),
    DecompPlan,
    FactorNode (..),
    SlotId,
    mkAtomId,
    mkDecompBag,
    mkDecompPlan,
    mkSlotId,
  )
import Moonlight.Differential.Row.Block
import Moonlight.Flow.Model.RowIdentity
  ( rowBlockIdentityForAtom,
  )
import Moonlight.Flow.Storage.Relation
  ( atomRowsFromTupleKeys,
    relationFromAtomRows,
    RelationPatchError,
  )
import Moonlight.Flow.Storage.Store
  ( Store,
    storeFromRelations,
  )
import Moonlight.Flow.Storage.View
  ( unrestrictedView,
  )
import Test.Tasty.HUnit
  ( Assertion,
  )

type RowsByAtom =
  IntMap (Set RowTupleKey)

data BoundedPessimismConfig = BoundedPessimismConfig
  { bpcLeafCount :: {-# UNPACK #-} !Int,
    bpcRootCount :: {-# UNPACK #-} !Int,
    bpcRowsPerRoot :: {-# UNPACK #-} !Int,
    bpcAuditPeriod :: {-# UNPACK #-} !Int,
    bpcSeed :: {-# UNPACK #-} !Word64,
    bpcMaxIterations :: !(Maybe Int),
    bpcDurationSeconds :: !(Maybe Int),
    bpcMinimumSavedNumerator :: {-# UNPACK #-} !Int,
    bpcMinimumSavedDenominator :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)

data BoundedPessimismReport = BoundedPessimismReport
  { bprOperations :: {-# UNPACK #-} !Int,
    bprProperSubsetOperations :: {-# UNPACK #-} !Int,
    bprReferenceWork :: !Integer,
    bprIncrementalWork :: !Integer,
    bprMaxReferenceWork :: {-# UNPACK #-} !Int,
    bprMaxIncrementalWork :: {-# UNPACK #-} !Int,
    bprReferenceNodes :: !(Maybe (Set FactorNode))
  }
  deriving stock (Eq, Show)

data BoundedMaintenanceError
  = BoundedMaintenanceRowBuild !RowBuildError
  | BoundedMaintenanceRelationBuild !RelationPatchError
  | BoundedMaintenanceFactorRun !FactorRunError
  deriving stock (Show)

data BoundedPessimismFailure
  = BoundedPessimismInitialRunFailed !BoundedMaintenanceError
  | BoundedPessimismReferenceRunFailed {-# UNPACK #-} !Int !BoundedMaintenanceError
  | BoundedPessimismIncrementalRunFailed {-# UNPACK #-} !Int !BoundedMaintenanceError
  | BoundedPessimismRowsMismatch {-# UNPACK #-} !Int !(Set RowTupleKey) !(Set RowTupleKey)
  | BoundedPessimismMissingAtomRows {-# UNPACK #-} !Int
  | BoundedPessimismEmptyAtomRows {-# UNPACK #-} !Int
  | BoundedPessimismMalformedAtomRow {-# UNPACK #-} !Int !RowTupleKey
  | BoundedPessimismFreshRowExhausted {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | BoundedPessimismWorkEscapedReference {-# UNPACK #-} !Int !(Set FactorNode) !(Set FactorNode)
  | BoundedPessimismWorkNotProper {-# UNPACK #-} !Int !(Set FactorNode) !(Set FactorNode)
  | BoundedPessimismReferenceWorkUnexpected {-# UNPACK #-} !Int !(Set FactorNode) !(Set FactorNode)
  | BoundedPessimismIncrementalWorkNotLocal {-# UNPACK #-} !Int !(Set FactorNode) !(Set FactorNode)
  | BoundedPessimismReferenceWorkSetDrift {-# UNPACK #-} !Int !(Set FactorNode) !(Set FactorNode)
  | BoundedPessimismCumulativeMarginTooSmall
      {-# UNPACK #-} !Int
      !Integer
      !Integer
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  | BoundedPessimismNoOperationsCompleted
  deriving stock (Show)

instance Exception BoundedPessimismFailure

data BoundedFixture = BoundedFixture
  { bfLeafCount :: {-# UNPACK #-} !Int,
    bfFullSchema :: ![SlotId],
    bfAtomSchemas :: !(IntMap [SlotId]),
    bfDecomp :: !DecompPlan,
    bfInitialRows :: !RowsByAtom
  }

data BoundedState = BoundedState
  { bsRowsByAtom :: !RowsByAtom,
    bsCache :: !FactorCache,
    bsRng :: !Rng,
    bsReport :: !BoundedPessimismReport
  }

data BoundedEdit = BoundedEdit
  { beAtomKey :: {-# UNPACK #-} !Int,
    beRemovedRow :: !RowTupleKey,
    beInsertedRow :: !RowTupleKey,
    beDelta :: !RowDelta
  }

data FactorObservation = FactorObservation
  { foRows :: !(Set RowTupleKey),
    foCache :: !FactorCache,
    foMetrics :: !MaintenanceMetrics
  }

newtype Rng = Rng
  { unRng :: Word64
  }
  deriving stock (Eq, Ord, Show, Read)

boundedPessimismUnitConfig :: BoundedPessimismConfig
boundedPessimismUnitConfig =
  BoundedPessimismConfig
    { bpcLeafCount = 4,
      bpcRootCount = 4,
      bpcRowsPerRoot = 2,
      bpcAuditPeriod = 16,
      bpcSeed = 0x6f2d_5b1a_4c93_2287,
      bpcMaxIterations = Just 128,
      bpcDurationSeconds = Nothing,
      bpcMinimumSavedNumerator = 1,
      bpcMinimumSavedDenominator = 3
    }

boundedPessimismSoakConfig :: BoundedPessimismConfig
boundedPessimismSoakConfig =
  boundedPessimismUnitConfig
    { bpcAuditPeriod = 512,
      bpcMaxIterations = Nothing,
      bpcDurationSeconds = Just 60
    }

boundedPessimismWorkGuarantee :: Assertion
boundedPessimismWorkGuarantee =
  runBoundedPessimismWorkGuarantee boundedPessimismUnitConfig *> pure ()

runBoundedPessimismWorkGuarantee ::
  BoundedPessimismConfig ->
  IO BoundedPessimismReport
runBoundedPessimismWorkGuarantee rawConfig = do
  let !config =
        normalizeConfig rawConfig
      !fixture =
        buildFixture config
  initial <-
    either
      (throwIO . BoundedPessimismInitialRunFailed)
      pure
      (runRowsDemand fixture (bfInitialRows fixture) emptyFactorCache IntMap.empty)
  start <- getCurrentTime
  let maybeDeadline =
        fmap
          (\seconds -> addUTCTime (fromIntegral seconds) start)
          (bpcDurationSeconds config)
      initialState =
        BoundedState
          { bsRowsByAtom = bfInitialRows fixture,
            bsCache = foCache initial,
            bsRng = Rng (bpcSeed config `xor` 0x9e37_79b9_7f4a_7c15),
            bsReport = emptyReport
          }
  finalState <- runLoop fixture config maybeDeadline 0 initialState
  let report =
        bsReport finalState
  when (bprOperations report == 0) $
    throwIO BoundedPessimismNoOperationsCompleted
  auditReport config (bprOperations report) report
  pure report

runLoop ::
  BoundedFixture ->
  BoundedPessimismConfig ->
  Maybe UTCTime ->
  Int ->
  BoundedState ->
  IO BoundedState
runLoop fixture config maybeDeadline !iteration !state0 = do
  continue <- shouldContinue config maybeDeadline iteration
  if not continue
    then pure state0
    else do
      state1 <- runOne fixture config iteration state0
      when ((iteration + 1) `rem` bpcAuditPeriod config == 0) $
        auditReport config (iteration + 1) (bsReport state1)
      runLoop fixture config maybeDeadline (iteration + 1) state1

shouldContinue ::
  BoundedPessimismConfig ->
  Maybe UTCTime ->
  Int ->
  IO Bool
shouldContinue config maybeDeadline iteration = do
  timeAllowed <-
    case maybeDeadline of
      Nothing ->
        pure True
      Just deadline -> do
        now <- getCurrentTime
        pure (now < deadline)
  let iterationAllowed =
        case bpcMaxIterations config of
          Nothing ->
            True
          Just maxIterations ->
            iteration < maxIterations
  pure (timeAllowed && iterationAllowed)

runOne ::
  BoundedFixture ->
  BoundedPessimismConfig ->
  Int ->
  BoundedState ->
  IO BoundedState
runOne fixture config iteration state0 = do
  (rng1, edit) <-
    either
      throwIO
      pure
      (generateEdit fixture config iteration (bsRowsByAtom state0) (bsRng state0))
  let !rows1 =
        applyEdit edit (bsRowsByAtom state0)
      !deltas =
        IntMap.singleton (beAtomKey edit) (beDelta edit)

  reference <-
    either
      (throwIO . BoundedPessimismReferenceRunFailed iteration)
      pure
      (runRowsDemand fixture rows1 emptyFactorCache IntMap.empty)

  incremental <-
    either
      (throwIO . BoundedPessimismIncrementalRunFailed iteration)
      pure
      (runRowsDemand fixture rows1 (bsCache state0) deltas)

  validateStep fixture iteration edit reference incremental

  report1 <-
    either
      throwIO
      pure
      (recordStep fixture iteration reference incremental (bsReport state0))

  pure
    BoundedState
      { bsRowsByAtom = rows1,
        bsCache = foCache incremental,
        bsRng = rng1,
        bsReport = report1
      }

validateStep ::
  BoundedFixture ->
  Int ->
  BoundedEdit ->
  FactorObservation ->
  FactorObservation ->
  IO ()
validateStep fixture iteration edit reference incremental = do
  when (foRows reference /= foRows incremental) $
    throwIO
      ( BoundedPessimismRowsMismatch
          iteration
          (foRows reference)
          (foRows incremental)
      )

  let referenceWork =
        rederivedNodes (foMetrics reference)
      incrementalWork =
        rederivedNodes (foMetrics incremental)
      expectedReferenceWork =
        expectedFreshReferenceNodes fixture
      expectedIncrementalWork =
        expectedIncrementalNodes edit

  when (referenceWork /= expectedReferenceWork) $
    throwIO
      ( BoundedPessimismReferenceWorkUnexpected
          iteration
          expectedReferenceWork
          referenceWork
      )

  when (incrementalWork /= expectedIncrementalWork) $
    throwIO
      ( BoundedPessimismIncrementalWorkNotLocal
          iteration
          expectedIncrementalWork
          incrementalWork
      )

  when (not (incrementalWork `Set.isSubsetOf` referenceWork)) $
    throwIO
      ( BoundedPessimismWorkEscapedReference
          iteration
          referenceWork
          incrementalWork
      )

  when (not (incrementalWork `Set.isProperSubsetOf` referenceWork)) $
    throwIO
      ( BoundedPessimismWorkNotProper
          iteration
          referenceWork
          incrementalWork
      )

recordStep ::
  BoundedFixture ->
  Int ->
  FactorObservation ->
  FactorObservation ->
  BoundedPessimismReport ->
  Either BoundedPessimismFailure BoundedPessimismReport
recordStep fixture iteration reference incremental report0 = do
  let referenceWork =
        rederivedNodes (foMetrics reference)
      incrementalWork =
        rederivedNodes (foMetrics incremental)
      expectedReferenceWork =
        expectedFreshReferenceNodes fixture

  when (referenceWork /= expectedReferenceWork) $
    Left
      ( BoundedPessimismReferenceWorkUnexpected
          iteration
          expectedReferenceWork
          referenceWork
      )

  case bprReferenceNodes report0 of
    Nothing ->
      pure ()
    Just previousReferenceWork ->
      when (previousReferenceWork /= referenceWork) $
        Left
          ( BoundedPessimismReferenceWorkSetDrift
              iteration
              previousReferenceWork
              referenceWork
          )

  let !referenceCount =
        Set.size referenceWork
      !incrementalCount =
        Set.size incrementalWork

  pure
    report0
      { bprOperations = bprOperations report0 + 1,
        bprProperSubsetOperations = bprProperSubsetOperations report0 + 1,
        bprReferenceWork = bprReferenceWork report0 + fromIntegral referenceCount,
        bprIncrementalWork = bprIncrementalWork report0 + fromIntegral incrementalCount,
        bprMaxReferenceWork = max (bprMaxReferenceWork report0) referenceCount,
        bprMaxIncrementalWork = max (bprMaxIncrementalWork report0) incrementalCount,
        bprReferenceNodes = Just referenceWork
      }

auditReport ::
  BoundedPessimismConfig ->
  Int ->
  BoundedPessimismReport ->
  IO ()
auditReport config iteration report =
  when (bprReferenceWork report > 0) $ do
    let !saved =
          bprReferenceWork report - bprIncrementalWork report
        !lhs =
          saved * fromIntegral (bpcMinimumSavedDenominator config)
        !rhs =
          bprReferenceWork report * fromIntegral (bpcMinimumSavedNumerator config)
    when (lhs < rhs) $
      throwIO
        ( BoundedPessimismCumulativeMarginTooSmall
            iteration
            (bprReferenceWork report)
            (bprIncrementalWork report)
            (bpcMinimumSavedNumerator config)
            (bpcMinimumSavedDenominator config)
        )

emptyReport :: BoundedPessimismReport
emptyReport =
  BoundedPessimismReport
    { bprOperations = 0,
      bprProperSubsetOperations = 0,
      bprReferenceWork = 0,
      bprIncrementalWork = 0,
      bprMaxReferenceWork = 0,
      bprMaxIncrementalWork = 0,
      bprReferenceNodes = Nothing
    }

normalizeConfig :: BoundedPessimismConfig -> BoundedPessimismConfig
normalizeConfig config =
  let noStop =
        bpcMaxIterations config == Nothing
          && bpcDurationSeconds config == Nothing
   in config
        { bpcLeafCount = max 2 (bpcLeafCount config),
          bpcRootCount = max 1 (bpcRootCount config),
          bpcRowsPerRoot = max 1 (bpcRowsPerRoot config),
          bpcAuditPeriod = max 1 (bpcAuditPeriod config),
          bpcMaxIterations =
            if noStop
              then Just 1
              else fmap (max 0) (bpcMaxIterations config),
          bpcDurationSeconds = fmap (max 0) (bpcDurationSeconds config),
          bpcMinimumSavedNumerator = max 0 (bpcMinimumSavedNumerator config),
          bpcMinimumSavedDenominator = max 1 (bpcMinimumSavedDenominator config)
        }

runRowsDemand ::
  BoundedFixture ->
  RowsByAtom ->
  FactorCache ->
  IntMap RowDelta ->
  Either BoundedMaintenanceError FactorObservation
runRowsDemand fixture rowsByAtom cache atomDeltas = do
  store <- storeFromRows fixture rowsByAtom
  let view =
        unrestrictedView
  result <-
    first BoundedMaintenanceFactorRun $
      runFactor
        FactorRunSpec
          { frsDecomp = bfDecomp fixture,
            frsInput =
              factorInputFromStoreView store view atomDeltas,
            frsCache = cache,
            frsGc = defaultProvGCConfig,
            frsRepairTelemetry = defaultRepairTelemetryConfig,
            frsDemand = FactorDemandRows
          }
  pure
    FactorObservation
      { foRows =
          Set.fromList
            ( enumerateBagRows
                (bfFullSchema fixture)
                (bfDecomp fixture)
                (frrPreSealCache result)
            ),
        foCache = frrCache result,
        foMetrics = frrMetrics result
      }

storeFromRows ::
  BoundedFixture ->
  RowsByAtom ->
  Either BoundedMaintenanceError Store
storeFromRows fixture rowsByAtom = do
  rowBlocks <- first BoundedMaintenanceRowBuild (IntMap.traverseWithKey (relationFromRows fixture) rowsByAtom)
  relations <- first BoundedMaintenanceRelationBuild (traverse relationFromAtomRows rowBlocks)
  pure (storeFromRelations relations)

relationFromRows ::
  BoundedFixture ->
  Int ->
  Set RowTupleKey ->
  Either RowBuildError (RowBlock 'Canonical)
relationFromRows fixture atomKey rows =
  let schema =
        IntMap.findWithDefault
          (atomSchema atomKey)
          atomKey
          (bfAtomSchemas fixture)
   in atomRowsFromTupleKeys
        (rowBlockIdentityForAtom 0 0 0 (mkAtomId atomKey) 0)
        (Vector.fromList schema)
        rows

buildFixture ::
  BoundedPessimismConfig ->
  BoundedFixture
buildFixture config =
  let !leafCount =
        bpcLeafCount config
      !fullSchema =
        rootSlot : fmap leafSlot [0 .. leafCount - 1]
      !atomSchemas =
        IntMap.fromList
          [ (atomKey, atomSchema atomKey)
          | atomKey <- [0 .. leafCount - 1]
          ]
      !initialRows =
        IntMap.fromList
          [ (atomKey, initialRowsForAtom config atomKey)
          | atomKey <- [0 .. leafCount - 1]
          ]
   in BoundedFixture
        { bfLeafCount = leafCount,
          bfFullSchema = fullSchema,
          bfAtomSchemas = atomSchemas,
          bfDecomp = starDecomp leafCount,
          bfInitialRows = initialRows
        }

starDecomp ::
  Int ->
  DecompPlan
starDecomp leafCount =
  mkDecompPlan
    rootBag
    bags
    parents
    children
    separators
    atomOwners
  where
    bags =
      IntMap.fromList
        ( (unBag rootBag, mkDecompBag rootBag [rootSlot] IntSet.empty)
            : [ ( unBag child,
                  mkDecompBag child (atomSchema atomKey) (IntSet.singleton atomKey)
                )
              | atomKey <- [0 .. leafCount - 1],
                let child = leafBag atomKey
              ]
        )

    parents =
      IntMap.fromList
        [ (unBag (leafBag atomKey), rootBag)
        | atomKey <- [0 .. leafCount - 1]
        ]

    children =
      IntMap.singleton
        (unBag rootBag)
        [ leafBag atomKey
        | atomKey <- [0 .. leafCount - 1]
        ]

    separators =
      Map.fromList
        [ ((leafBag atomKey, rootBag), [rootSlot])
        | atomKey <- [0 .. leafCount - 1]
        ]

    atomOwners =
      IntMap.fromList
        [ (atomKey, leafBag atomKey)
        | atomKey <- [0 .. leafCount - 1]
        ]

generateEdit ::
  BoundedFixture ->
  BoundedPessimismConfig ->
  Int ->
  RowsByAtom ->
  Rng ->
  Either BoundedPessimismFailure (Rng, BoundedEdit)
generateEdit fixture _config iteration rowsByAtom rng0 = do
  let (rng1, atomOffset) =
        uniformInt (bfLeafCount fixture) rng0
      atomKey =
        atomOffset
  rows <-
    case IntMap.lookup atomKey rowsByAtom of
      Nothing ->
        Left (BoundedPessimismMissingAtomRows atomKey)
      Just rowsValue ->
        Right rowsValue
  when (Set.null rows) $
    Left (BoundedPessimismEmptyAtomRows atomKey)

  let (rng2, rowOffset) =
        uniformInt (Set.size rows) rng1

  removed <-
    case selectSetIndex rowOffset rows of
      Nothing ->
        Left (BoundedPessimismEmptyAtomRows atomKey)
      Just rowValue ->
        Right rowValue

  rootKey <- rootKeyOfRow atomKey removed

  inserted <- freshReplacementRow atomKey rootKey iteration rows

  let !deltaRows =
        plainRowPatchFromList
          [ (removed, MultiplicityChange (-1)),
            (inserted, MultiplicityChange 1)
          ]

  pure
    ( rng2,
      BoundedEdit
        { beAtomKey = atomKey,
          beRemovedRow = removed,
          beInsertedRow = inserted,
          beDelta = deltaRows
        }
    )

applyEdit ::
  BoundedEdit ->
  RowsByAtom ->
  RowsByAtom
applyEdit edit =
  IntMap.adjust updateRows (beAtomKey edit)
  where
    updateRows rows =
      Set.insert
        (beInsertedRow edit)
        (Set.delete (beRemovedRow edit) rows)

initialRowsForAtom ::
  BoundedPessimismConfig ->
  Int ->
  Set RowTupleKey
initialRowsForAtom config atomKey =
  Set.fromList
    [ rowForAtomVariant atomKey rootKey variant
    | rootKey <- [0 .. bpcRootCount config - 1],
      variant <- [0 .. bpcRowsPerRoot config - 1]
    ]

freshReplacementRow ::
  Int ->
  Int ->
  Int ->
  Set RowTupleKey ->
  Either BoundedPessimismFailure RowTupleKey
freshReplacementRow atomKey rootKey iteration existingRows =
  case Set.lookupMin candidates of
    Just candidate ->
      Right candidate
    Nothing ->
      Left (BoundedPessimismFreshRowExhausted atomKey rootKey iteration)
  where
    candidates =
      Set.filter
        (`Set.notMember` existingRows)
        ( Set.fromList
            [ rowForAtomValue
                atomKey
                rootKey
                (freshLeafValue atomKey rootKey iteration probe)
            | probe <- [0 .. Set.size existingRows]
            ]
        )

rowForAtomVariant ::
  Int ->
  Int ->
  Int ->
  RowTupleKey
rowForAtomVariant atomKey rootKey variant =
  rowForAtomValue
    atomKey
    rootKey
    ((atomKey + 1) * 1000000 + rootKey * 1000 + variant)

rowForAtomValue ::
  Int ->
  Int ->
  Int ->
  RowTupleKey
rowForAtomValue _atomKey rootKey leafValue =
  tupleKeyFromInts [rootKey, leafValue]

freshLeafValue ::
  Int ->
  Int ->
  Int ->
  Int ->
  Int
freshLeafValue atomKey rootKey iteration probe =
  (atomKey + 1) * 100000000
    + rootKey * 1000000
    + iteration * 97
    + probe

rootKeyOfRow ::
  Int ->
  RowTupleKey ->
  Either BoundedPessimismFailure Int
rootKeyOfRow atomKey row =
  case tupleKeyToInts row of
    rootKey : _ ->
      Right rootKey
    [] ->
      Left (BoundedPessimismMalformedAtomRow atomKey row)

selectSetIndex ::
  Int ->
  Set value ->
  Maybe value
selectSetIndex indexValue values
  | indexValue < 0 =
      Nothing
  | indexValue >= Set.size values =
      Nothing
  | otherwise =
      Just (Set.elemAt indexValue values)

expectedFreshReferenceNodes ::
  BoundedFixture ->
  Set FactorNode
expectedFreshReferenceNodes fixture =
  Set.fromList
    ( fmap FactorNodeBag allBags
        <> fmap FactorNodeBagBelief allBags
        <> fmap (`FactorNodeSeparator` rootBag) leafBags
    )
  where
    leafBags =
      fmap leafBag [0 .. bfLeafCount fixture - 1]

    allBags =
      rootBag : leafBags

expectedIncrementalNodes ::
  BoundedEdit ->
  Set FactorNode
expectedIncrementalNodes edit =
  Set.fromList
    [ FactorNodeBag editedLeaf,
      FactorNodeBagBelief editedLeaf
    ]
  where
    editedLeaf =
      leafBag (beAtomKey edit)

rederivedNodes ::
  MaintenanceMetrics ->
  Set FactorNode
rederivedNodes metrics =
  Map.keysSet
    ( Map.filter
        (isRederivedAction . nmAction)
        (mmNodes metrics)
    )

isRederivedAction :: NodeAction -> Bool
isRederivedAction action =
  case action of
    NodeBuilt ->
      True
    NodePatched ->
      True
    NodeReused ->
      False

rootSlot :: SlotId
rootSlot =
  mkSlotId 0

leafSlot :: Int -> SlotId
leafSlot atomKey =
  mkSlotId (atomKey + 1)

atomSchema :: Int -> [SlotId]
atomSchema atomKey =
  [rootSlot, leafSlot atomKey]

rootBag :: BagId
rootBag =
  BagId 0

leafBag :: Int -> BagId
leafBag atomKey =
  BagId (atomKey + 1)

unBag :: BagId -> Int
unBag (BagId value) =
  value

nextWord64 :: Rng -> (Rng, Word64)
nextWord64 (Rng seed0) =
  let !seed1 =
        seed0 + 0x9e37_79b9_7f4a_7c15
      !z0 =
        seed1
      !z1 =
        (z0 `xor` (z0 `shiftR` 30)) * 0xbf58_476d_1ce4_e5b9
      !z2 =
        (z1 `xor` (z1 `shiftR` 27)) * 0x94d0_49bb_1331_11eb
      !z3 =
        z2 `xor` (z2 `shiftR` 31)
   in (Rng seed1, z3)

uniformInt :: Int -> Rng -> (Rng, Int)
uniformInt bound rng0
  | bound <= 1 =
      let (rng1, _word) =
            nextWord64 rng0
       in (rng1, 0)
  | otherwise =
      let (rng1, word) =
            nextWord64 rng0
       in (rng1, fromIntegral (word `rem` fromIntegral bound))
