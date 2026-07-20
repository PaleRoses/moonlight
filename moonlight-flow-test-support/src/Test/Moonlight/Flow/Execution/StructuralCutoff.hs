{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}

module Test.Moonlight.Flow.Execution.StructuralCutoff
  ( CutoffChannel (..),
    StructuralCutoffConfig (..),
    StructuralCutoffFailure (..),
    StructuralMaintenanceError (..),
    StructuralCutoffMetadataCounter (..),
    StructuralCutoffPrediction (..),
    StructuralCutoffReport (..),
    structuralCutoffUnitConfig,
    structuralCutoffSoakConfig,
    runStructuralCutoffWitness,
    structuralCutoffWitness,
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
import Data.Foldable
  ( traverse_,
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Data.Word
  ( Word64,
  )
import Moonlight.Flow.Execution.Factor.Run
  ( FactorRunError,
    factorRunTelemetry,
    runFactor,
  )
import Moonlight.Flow.Execution.Factor.Types
  ( FactorCache (..),
    FactorDemand (FactorDemandMaintenance),
    FactorEntry (..),
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
    FactorCacheTelemetry (..),
    MaintenanceMetrics (..),
    NodeAction (..),
    NodeMaintenance (..),
    ProvTelemetry (..),
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Delta.Signed
  ( MultiplicityChange (..)
  )
import Moonlight.Flow.Internal.Digest
  ( wordOfInt,
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchFromList,
  )
import Moonlight.Differential.Row.Tuple
  ( AssignmentTupleKey,
    RowTupleKey,
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
    slotIdKey,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
    stableDigest128,
  )
import Moonlight.Differential.Row.Block
import Moonlight.Flow.Model.RowIdentity
  ( rowBlockIdentityForAtom,
  )
import Moonlight.Differential.Index.IndexedRows
  ( indexedRowsPayloadMap,
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

data CutoffChannel
  = RowMembership
  | FactorMaintenance
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

data StructuralCutoffConfig = StructuralCutoffConfig
  { sccLeafCount :: {-# UNPACK #-} !Int,
    sccRootCount :: {-# UNPACK #-} !Int,
    sccRowsPerRoot :: {-# UNPACK #-} !Int,
    sccAuditPeriod :: {-# UNPACK #-} !Int,
    sccSeed :: {-# UNPACK #-} !Word64,
    sccMaxIterations :: {-# UNPACK #-} !Int,
    sccArenaSlackFactor :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)

data StructuralCutoffPrediction = StructuralCutoffPrediction
  { scpIteration :: {-# UNPACK #-} !Int,
    scpAtomKey :: {-# UNPACK #-} !Int,
    scpChangedSlots :: !IntSet,
    scpSeparatorSlots :: !IntSet,
    scpRowMembershipCutoffs :: !(Set FactorNode),
    scpMaintenanceReachable :: !(Set FactorNode),
    scpMaintenanceCutoffs :: !(Set FactorNode),
    scpDigest :: !StableDigest128
  }
  deriving stock (Eq, Show)

data StructuralCutoffReport = StructuralCutoffReport
  { scrOperations :: {-# UNPACK #-} !Int,
    scrRowMembershipClaims :: {-# UNPACK #-} !Int,
    scrMaintenanceClaims :: {-# UNPACK #-} !Int,
    scrMaxRederivedNodes :: {-# UNPACK #-} !Int,
    scrMaxArenaNodes :: {-# UNPACK #-} !Int,
    scrLastPredictionDigest :: !(Maybe StableDigest128)
  }
  deriving stock (Eq, Show)

data StructuralCutoffMetadataCounter
  = StructuralCutoffLocalFactors
  | StructuralCutoffMessages
  | StructuralCutoffBagBeliefs
  | StructuralCutoffSupportMemoNodes
  | StructuralCutoffArenaNodes
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

data StructuralMaintenanceError
  = StructuralMaintenanceRowBuild !RowBuildError
  | StructuralMaintenanceRelationBuild !RelationPatchError
  | StructuralMaintenanceFactorRun !FactorRunError
  deriving stock (Show)

data StructuralCutoffFailure
  = StructuralCutoffInitialRunFailed !StructuralMaintenanceError
  | StructuralCutoffReferenceRunFailed {-# UNPACK #-} !Int !StructuralMaintenanceError
  | StructuralCutoffIncrementalRunFailed {-# UNPACK #-} !Int !StructuralMaintenanceError
  | StructuralCutoffMissingFactorNode {-# UNPACK #-} !Int !FactorNode
  | StructuralCutoffReferenceMismatch {-# UNPACK #-} !Int !FactorNode !(Set AssignmentTupleKey) !(Set AssignmentTupleKey)
  | StructuralCutoffRowMembershipViolated {-# UNPACK #-} !Int !StructuralCutoffPrediction !FactorNode !(Set AssignmentTupleKey) !(Set AssignmentTupleKey)
  | StructuralCutoffWorkEscapedPrediction {-# UNPACK #-} !Int !StructuralCutoffPrediction !(Set FactorNode)
  | StructuralCutoffExpectedSourcePatchMissing {-# UNPACK #-} !Int !StructuralCutoffPrediction !FactorNode !(Set FactorNode)
  | StructuralCutoffMetadataBoundExceeded {-# UNPACK #-} !Int !StructuralCutoffMetadataCounter {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | StructuralCutoffMissingAtomRows {-# UNPACK #-} !Int
  | StructuralCutoffEmptyAtomRows {-# UNPACK #-} !Int
  | StructuralCutoffMalformedAtomRow {-# UNPACK #-} !Int !RowTupleKey
  | StructuralCutoffFreshRowExhausted {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | StructuralCutoffNoOperationsCompleted
  deriving stock (Show)

instance Exception StructuralCutoffFailure

data StructuralFixture = StructuralFixture
  { sfLeafCount :: {-# UNPACK #-} !Int,
    sfAtomSchemas :: !(IntMap [SlotId]),
    sfDecomp :: !DecompPlan,
    sfInitialRows :: !RowsByAtom,
    sfMaintenanceNodes :: !(Set FactorNode),
    sfActiveCellUpperBound :: {-# UNPACK #-} !Int
  }

data StructuralObservation = StructuralObservation
  { soCache :: !FactorCache,
    soMembership :: !(Map FactorNode (Set AssignmentTupleKey)),
    soMetrics :: !MaintenanceMetrics,
    soTelemetry :: !FactorCacheTelemetry
  }
  deriving stock (Eq, Show)

data StructuralState = StructuralState
  { ssRowsByAtom :: !RowsByAtom,
    ssObservation :: !StructuralObservation,
    ssRng :: !Rng,
    ssReport :: !StructuralCutoffReport
  }

data StructuralEdit = StructuralEdit
  { seAtomKey :: {-# UNPACK #-} !Int,
    seRemovedRow :: !RowTupleKey,
    seInsertedRow :: !RowTupleKey,
    seDelta :: !RowDelta
  }
  deriving stock (Eq, Show)

newtype Rng = Rng
  { unRng :: Word64
  }
  deriving stock (Eq, Ord, Show, Read)

structuralCutoffUnitConfig :: StructuralCutoffConfig
structuralCutoffUnitConfig =
  StructuralCutoffConfig
    { sccLeafCount = 6,
      sccRootCount = 8,
      sccRowsPerRoot = 3,
      sccAuditPeriod = 32,
      sccSeed = 0x97c2_9d64_5f31_07bb,
      sccMaxIterations = 256,
      sccArenaSlackFactor = 96
    }

structuralCutoffSoakConfig :: StructuralCutoffConfig
structuralCutoffSoakConfig =
  structuralCutoffUnitConfig
    { sccLeafCount = 16,
      sccRootCount = 64,
      sccRowsPerRoot = 4,
      sccAuditPeriod = 512,
      sccMaxIterations = 100_000,
      sccArenaSlackFactor = 128
    }

structuralCutoffWitness :: Assertion
structuralCutoffWitness =
  runStructuralCutoffWitness structuralCutoffUnitConfig *> pure ()

runStructuralCutoffWitness ::
  StructuralCutoffConfig ->
  IO StructuralCutoffReport
runStructuralCutoffWitness rawConfig = do
  let !config =
        normalizeConfig rawConfig
      !fixture =
        buildFixture config
  initial <-
    either
      (throwIO . StructuralCutoffInitialRunFailed)
      pure
      (runMaintenance fixture (sfInitialRows fixture) emptyFactorCache IntMap.empty)
  let initialState =
        StructuralState
          { ssRowsByAtom = sfInitialRows fixture,
            ssObservation = initial,
            ssRng = Rng (sccSeed config `xor` 0x9e37_79b9_7f4a_7c15),
            ssReport = emptyReport
          }
  finalState <- runLoop fixture config 0 initialState
  let report =
        ssReport finalState
  when (scrOperations report == 0) $
    throwIO StructuralCutoffNoOperationsCompleted
  pure report

runLoop ::
  StructuralFixture ->
  StructuralCutoffConfig ->
  Int ->
  StructuralState ->
  IO StructuralState
runLoop fixture config !iteration !state0
  | iteration >= sccMaxIterations config =
      pure state0
  | otherwise = do
      state1 <- runOne fixture config iteration state0
      when ((iteration + 1) `rem` sccAuditPeriod config == 0) $
        auditMetadata fixture config (iteration + 1) (soTelemetry (ssObservation state1))
      runLoop fixture config (iteration + 1) state1

runOne ::
  StructuralFixture ->
  StructuralCutoffConfig ->
  Int ->
  StructuralState ->
  IO StructuralState
runOne fixture config iteration state0 = do
  (rng1, edit) <-
    either
      throwIO
      pure
      (generateEdit fixture iteration (ssRowsByAtom state0) (ssRng state0))
  let !prediction =
        predictEdit fixture iteration edit
      !rows1 =
        applyEdit edit (ssRowsByAtom state0)
      !deltas =
        IntMap.singleton (seAtomKey edit) (seDelta edit)
  reference <-
    either
      (throwIO . StructuralCutoffReferenceRunFailed iteration)
      pure
      (runMaintenance fixture rows1 emptyFactorCache IntMap.empty)
  incremental <-
    either
      (throwIO . StructuralCutoffIncrementalRunFailed iteration)
      pure
      (runMaintenance fixture rows1 (soCache (ssObservation state0)) deltas)
  validateStep fixture config iteration prediction (ssObservation state0) reference incremental
  pure
    StructuralState
      { ssRowsByAtom = rows1,
        ssObservation = incremental,
        ssRng = rng1,
        ssReport = recordReport prediction incremental (ssReport state0)
      }

validateStep ::
  StructuralFixture ->
  StructuralCutoffConfig ->
  Int ->
  StructuralCutoffPrediction ->
  StructuralObservation ->
  StructuralObservation ->
  StructuralObservation ->
  IO ()
validateStep fixture config iteration prediction before reference incremental = do
  validateReferenceRoot iteration reference incremental
  validateRowMembershipCutoffs iteration prediction before incremental
  validateMaintenanceCutoffs iteration prediction incremental
  auditMetadata fixture config iteration (soTelemetry incremental)

validateReferenceRoot ::
  Int ->
  StructuralObservation ->
  StructuralObservation ->
  IO ()
validateReferenceRoot iteration reference incremental = do
  expected <- requireNodeRows iteration FactorNodeRoot reference
  actual <- requireNodeRows iteration FactorNodeRoot incremental
  when (expected /= actual) $
    throwIO (StructuralCutoffReferenceMismatch iteration FactorNodeRoot expected actual)

validateRowMembershipCutoffs ::
  Int ->
  StructuralCutoffPrediction ->
  StructuralObservation ->
  StructuralObservation ->
  IO ()
validateRowMembershipCutoffs iteration prediction before after =
  traverse_
    validateNode
    (Set.toAscList (scpRowMembershipCutoffs prediction))
  where
    validateNode node = do
      beforeRows <- requireNodeRows iteration node before
      afterRows <- requireNodeRows iteration node after
      when (beforeRows /= afterRows) $
        throwIO
          ( StructuralCutoffRowMembershipViolated
              iteration
              prediction
              node
              beforeRows
              afterRows
          )

validateMaintenanceCutoffs ::
  Int ->
  StructuralCutoffPrediction ->
  StructuralObservation ->
  IO ()
validateMaintenanceCutoffs iteration prediction observation = do
  let !rederived =
        rederivedNodes (soMetrics observation)
      !escaped =
        rederived `Set.difference` scpMaintenanceReachable prediction
      !sourceNode =
        FactorNodeBag (leafBag (scpAtomKey prediction))
  when (not (Set.null escaped)) $
    throwIO (StructuralCutoffWorkEscapedPrediction iteration prediction escaped)
  when (Set.notMember sourceNode rederived) $
    throwIO (StructuralCutoffExpectedSourcePatchMissing iteration prediction sourceNode rederived)

requireNodeRows ::
  Int ->
  FactorNode ->
  StructuralObservation ->
  IO (Set AssignmentTupleKey)
requireNodeRows iteration node observation =
  case Map.lookup node (soMembership observation) of
    Nothing ->
      throwIO (StructuralCutoffMissingFactorNode iteration node)
    Just rows ->
      pure rows

runMaintenance ::
  StructuralFixture ->
  RowsByAtom ->
  FactorCache ->
  IntMap RowDelta ->
  Either StructuralMaintenanceError StructuralObservation
runMaintenance fixture rowsByAtom cache atomDeltas = do
  store <- storeFromRows fixture rowsByAtom
  let view =
        unrestrictedView
  result <-
    first StructuralMaintenanceFactorRun $
      runFactor
        FactorRunSpec
          { frsDecomp = sfDecomp fixture,
            frsInput =
              factorInputFromStoreView store view atomDeltas,
            frsCache = cache,
            frsGc = defaultProvGCConfig,
            frsRepairTelemetry = defaultRepairTelemetryConfig,
            frsDemand = FactorDemandMaintenance
          }
  pure
    StructuralObservation
      { soCache = frrCache result,
        soMembership = factorMembershipByNode (frrPreSealCache result),
        soMetrics = frrMetrics result,
        soTelemetry = factorRunTelemetry result
      }

factorMembershipByNode ::
  FactorCache ->
  Map FactorNode (Set AssignmentTupleKey)
factorMembershipByNode cache =
  Map.map (Set.fromList . Map.keys . indexedRowsPayloadMap . feFactor) (fcFactors cache)
{-# INLINE factorMembershipByNode #-}

predictEdit ::
  StructuralFixture ->
  Int ->
  StructuralEdit ->
  StructuralCutoffPrediction
predictEdit fixture iteration edit =
  let !atomKey =
        seAtomKey edit
      !changedSlots =
        changedSlotKeys fixture edit
      !separatorSlots =
        IntSet.singleton (slotIdKey rootSlot)
      !changedBag =
        leafBag atomKey
      !sourceNode =
        FactorNodeBag changedBag
      !separatorNode =
        FactorNodeSeparator changedBag rootBag
      !rowReachable =
        Set.singleton sourceNode
      !rowCutoffs =
        sfMaintenanceNodes fixture `Set.difference` rowReachable
      !maintenanceReachable =
        Set.fromList [sourceNode, separatorNode, FactorNodeRoot]
      !maintenanceCutoffs =
        sfMaintenanceNodes fixture `Set.difference` maintenanceReachable
      !digest =
        predictionDigest
          iteration
          atomKey
          changedSlots
          separatorSlots
          rowCutoffs
          maintenanceReachable
          maintenanceCutoffs
   in StructuralCutoffPrediction
        { scpIteration = iteration,
          scpAtomKey = atomKey,
          scpChangedSlots = changedSlots,
          scpSeparatorSlots = separatorSlots,
          scpRowMembershipCutoffs = rowCutoffs,
          scpMaintenanceReachable = maintenanceReachable,
          scpMaintenanceCutoffs = maintenanceCutoffs,
          scpDigest = digest
        }

predictionDigest ::
  Int ->
  Int ->
  IntSet ->
  IntSet ->
  Set FactorNode ->
  Set FactorNode ->
  Set FactorNode ->
  StableDigest128
predictionDigest iteration atomKey changedSlots separatorSlots rowCutoffs maintenanceReachable maintenanceCutoffs =
  stableDigest128
    ( [0x7374727563744375, wordOfInt iteration, wordOfInt atomKey]
        <> intSetWords 0x6368616e676564 changedSlots
        <> intSetWords 0x736570617261746f separatorSlots
        <> factorNodeSetWords 0x726f774375746f66 rowCutoffs
        <> factorNodeSetWords 0x6d61696e52656163 maintenanceReachable
        <> factorNodeSetWords 0x6d61696e4375746f maintenanceCutoffs
    )
{-# INLINE predictionDigest #-}

intSetWords :: Word64 -> IntSet -> [Word64]
intSetWords tag values =
  tag : wordOfInt (IntSet.size values) : fmap wordOfInt (IntSet.toAscList values)
{-# INLINE intSetWords #-}

factorNodeSetWords :: Word64 -> Set FactorNode -> [Word64]
factorNodeSetWords tag nodes =
  tag : wordOfInt (Set.size nodes) : foldMap factorNodeWords (Set.toAscList nodes)
{-# INLINE factorNodeSetWords #-}

factorNodeWords :: FactorNode -> [Word64]
factorNodeWords node =
  case node of
    FactorNodeBag bag ->
      [0x01, wordOfInt (unBag bag)]
    FactorNodeSeparator child parent ->
      [0x02, wordOfInt (unBag child), wordOfInt (unBag parent)]
    FactorNodeBagBelief bag ->
      [0x03, wordOfInt (unBag bag)]
    FactorNodeRoot ->
      [0x04]
{-# INLINE factorNodeWords #-}

changedSlotKeys :: StructuralFixture -> StructuralEdit -> IntSet
changedSlotKeys fixture edit =
  case IntMap.lookup (seAtomKey edit) (sfAtomSchemas fixture) of
    Nothing ->
      IntSet.empty
    Just schema ->
      changedSlotKeysForRows schema (seRemovedRow edit) (seInsertedRow edit)
{-# INLINE changedSlotKeys #-}

changedSlotKeysForRows ::
  [SlotId] ->
  RowTupleKey ->
  RowTupleKey ->
  IntSet
changedSlotKeysForRows schema oldRow newRow =
  let oldValues =
        tupleKeyToInts oldRow
      newValues =
        tupleKeyToInts newRow
   in if length oldValues /= length schema || length newValues /= length schema
        then IntSet.fromList (fmap slotIdKey schema)
        else
          IntSet.fromList
            [ slotIdKey slot
            | (slot, (oldValue, newValue)) <- zip schema (zip oldValues newValues),
              oldValue /= newValue
            ]
{-# INLINE changedSlotKeysForRows #-}

auditMetadata ::
  StructuralFixture ->
  StructuralCutoffConfig ->
  Int ->
  FactorCacheTelemetry ->
  IO ()
auditMetadata fixture config iteration telemetry = do
  assertBound StructuralCutoffLocalFactors (fctLocalFactors telemetry) (sfLeafCount fixture + 1)
  assertBound StructuralCutoffMessages (fctMessages telemetry) (sfLeafCount fixture)
  assertBound StructuralCutoffBagBeliefs (fctBagBeliefs telemetry) 0
  assertBound StructuralCutoffSupportMemoNodes (fctSupportMemoNodes telemetry) 0
  assertBound StructuralCutoffArenaNodes (ptArenaNodes (fctProv telemetry)) arenaBound
  where
    arenaBound =
      max 1 (sccArenaSlackFactor config) * max 1 (sfActiveCellUpperBound fixture)

    assertBound counter actual bound =
      when (actual > bound) $
        throwIO (StructuralCutoffMetadataBoundExceeded iteration counter actual bound)

recordReport ::
  StructuralCutoffPrediction ->
  StructuralObservation ->
  StructuralCutoffReport ->
  StructuralCutoffReport
recordReport prediction observation report =
  let !rederived =
        rederivedNodes (soMetrics observation)
      !arenaNodes =
        ptArenaNodes (fctProv (soTelemetry observation))
   in report
        { scrOperations = scrOperations report + 1,
          scrRowMembershipClaims = scrRowMembershipClaims report + Set.size (scpRowMembershipCutoffs prediction),
          scrMaintenanceClaims = scrMaintenanceClaims report + Set.size (scpMaintenanceCutoffs prediction),
          scrMaxRederivedNodes = max (scrMaxRederivedNodes report) (Set.size rederived),
          scrMaxArenaNodes = max (scrMaxArenaNodes report) arenaNodes,
          scrLastPredictionDigest = Just (scpDigest prediction)
        }

emptyReport :: StructuralCutoffReport
emptyReport =
  StructuralCutoffReport
    { scrOperations = 0,
      scrRowMembershipClaims = 0,
      scrMaintenanceClaims = 0,
      scrMaxRederivedNodes = 0,
      scrMaxArenaNodes = 0,
      scrLastPredictionDigest = Nothing
    }

normalizeConfig :: StructuralCutoffConfig -> StructuralCutoffConfig
normalizeConfig config =
  config
    { sccLeafCount = max 2 (sccLeafCount config),
      sccRootCount = max 1 (sccRootCount config),
      sccRowsPerRoot = max 1 (sccRowsPerRoot config),
      sccAuditPeriod = max 1 (sccAuditPeriod config),
      sccMaxIterations = max 1 (sccMaxIterations config),
      sccArenaSlackFactor = max 1 (sccArenaSlackFactor config)
    }

buildFixture :: StructuralCutoffConfig -> StructuralFixture
buildFixture config =
  let !leafCount =
        sccLeafCount config
      !atomSchemas =
        IntMap.fromList [(atomKey, atomSchema atomKey) | atomKey <- [0 .. leafCount - 1]]
      !initialRows =
        IntMap.fromList [(atomKey, initialRowsForAtom config atomKey) | atomKey <- [0 .. leafCount - 1]]
      !maintenanceNodes =
        maintenanceNodesForStar leafCount
      !activeCells =
        leafCount * sccRootCount config * sccRowsPerRoot config
          + leafCount * sccRootCount config
          + 2
   in StructuralFixture
        { sfLeafCount = leafCount,
          sfAtomSchemas = atomSchemas,
          sfDecomp = starDecomp leafCount,
          sfInitialRows = initialRows,
          sfMaintenanceNodes = maintenanceNodes,
          sfActiveCellUpperBound = activeCells
        }

maintenanceNodesForStar :: Int -> Set FactorNode
maintenanceNodesForStar leafCount =
  Set.fromList
    ( FactorNodeRoot
        : FactorNodeBag rootBag
        : [FactorNodeBag (leafBag atomKey) | atomKey <- [0 .. leafCount - 1]]
          <> [FactorNodeSeparator (leafBag atomKey) rootBag | atomKey <- [0 .. leafCount - 1]]
    )
{-# INLINE maintenanceNodesForStar #-}

storeFromRows :: StructuralFixture -> RowsByAtom -> Either StructuralMaintenanceError Store
storeFromRows fixture rowsByAtom = do
  rowBlocks <- first StructuralMaintenanceRowBuild (IntMap.traverseWithKey (relationFromRows fixture) rowsByAtom)
  relations <- first StructuralMaintenanceRelationBuild (traverse relationFromAtomRows rowBlocks)
  pure (storeFromRelations relations)
{-# INLINE storeFromRows #-}

relationFromRows :: StructuralFixture -> Int -> Set RowTupleKey -> Either RowBuildError (RowBlock 'Canonical)
relationFromRows fixture atomKey rows =
  let schema =
        IntMap.findWithDefault (atomSchema atomKey) atomKey (sfAtomSchemas fixture)
   in atomRowsFromTupleKeys
        (rowBlockIdentityForAtom 0 0 0 (mkAtomId atomKey) 0)
        (Vector.fromList schema)
        rows
{-# INLINE relationFromRows #-}

starDecomp :: Int -> DecompPlan
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
            : [ (unBag child, mkDecompBag child (atomSchema atomKey) (IntSet.singleton atomKey))
              | atomKey <- [0 .. leafCount - 1],
                let child = leafBag atomKey
              ]
        )

    parents =
      IntMap.fromList [(unBag (leafBag atomKey), rootBag) | atomKey <- [0 .. leafCount - 1]]

    children =
      IntMap.singleton rootBagKey [leafBag atomKey | atomKey <- [0 .. leafCount - 1]]

    separators =
      Map.fromList [((leafBag atomKey, rootBag), [rootSlot]) | atomKey <- [0 .. leafCount - 1]]

    atomOwners =
      IntMap.fromList [(atomKey, leafBag atomKey) | atomKey <- [0 .. leafCount - 1]]

initialRowsForAtom :: StructuralCutoffConfig -> Int -> Set RowTupleKey
initialRowsForAtom config atomKey =
  Set.fromList
    [ rowForAtomVariant atomKey rootKey variant
    | rootKey <- [0 .. sccRootCount config - 1],
      variant <- [0 .. sccRowsPerRoot config - 1]
    ]

generateEdit ::
  StructuralFixture ->
  Int ->
  RowsByAtom ->
  Rng ->
  Either StructuralCutoffFailure (Rng, StructuralEdit)
generateEdit fixture iteration rowsByAtom rng0 = do
  let (rng1, atomKey) =
        uniformInt (sfLeafCount fixture) rng0
  rows <-
    case IntMap.lookup atomKey rowsByAtom of
      Nothing -> Left (StructuralCutoffMissingAtomRows atomKey)
      Just rowsValue -> Right rowsValue
  when (Set.null rows) $
    Left (StructuralCutoffEmptyAtomRows atomKey)
  let (rng2, rowOffset) =
        uniformInt (Set.size rows) rng1
  removed <-
    case selectSetIndex rowOffset rows of
      Nothing -> Left (StructuralCutoffEmptyAtomRows atomKey)
      Just rowValue -> Right rowValue
  rootKey <- rootKeyOfRow atomKey removed
  inserted <- freshReplacementRow atomKey rootKey iteration rows
  let !delta =
        plainRowPatchFromList
          [ (removed, MultiplicityChange (-1)),
            (inserted, MultiplicityChange 1)
          ]
  pure
    ( rng2,
      StructuralEdit
        { seAtomKey = atomKey,
          seRemovedRow = removed,
          seInsertedRow = inserted,
          seDelta = delta
        }
    )

applyEdit :: StructuralEdit -> RowsByAtom -> RowsByAtom
applyEdit edit =
  IntMap.adjust
    (Set.insert (seInsertedRow edit) . Set.delete (seRemovedRow edit))
    (seAtomKey edit)
{-# INLINE applyEdit #-}

freshReplacementRow ::
  Int ->
  Int ->
  Int ->
  Set RowTupleKey ->
  Either StructuralCutoffFailure RowTupleKey
freshReplacementRow atomKey rootKey iteration existingRows =
  firstCandidate [0 .. Set.size existingRows + 8]
  where
    firstCandidate [] =
      Left (StructuralCutoffFreshRowExhausted atomKey rootKey iteration)
    firstCandidate (probe : probes) =
      let candidate =
            rowForAtomValue atomKey rootKey (freshLeafValue atomKey rootKey iteration probe)
       in if Set.member candidate existingRows
            then firstCandidate probes
            else Right candidate

rootKeyOfRow :: Int -> RowTupleKey -> Either StructuralCutoffFailure Int
rootKeyOfRow atomKey row =
  case tupleKeyToInts row of
    rootKey : _ -> Right rootKey
    [] -> Left (StructuralCutoffMalformedAtomRow atomKey row)
{-# INLINE rootKeyOfRow #-}

selectSetIndex :: Int -> Set value -> Maybe value
selectSetIndex indexValue values
  | indexValue < 0 =
      Nothing
  | otherwise =
      selectListIndex indexValue (Set.toAscList values)
{-# INLINE selectSetIndex #-}

selectListIndex :: Int -> [value] -> Maybe value
selectListIndex _ [] =
  Nothing
selectListIndex indexValue (value : rest)
  | indexValue == 0 =
      Just value
  | otherwise =
      selectListIndex (indexValue - 1) rest
{-# INLINE selectListIndex #-}

rederivedNodes :: MaintenanceMetrics -> Set FactorNode
rederivedNodes metrics =
  Map.keysSet (Map.filter (isRederivedAction . nmAction) (mmNodes metrics))
{-# INLINE rederivedNodes #-}

isRederivedAction :: NodeAction -> Bool
isRederivedAction action =
  case action of
    NodeBuilt -> True
    NodePatched -> True
    NodeReused -> False
{-# INLINE isRederivedAction #-}

rootSlot :: SlotId
rootSlot =
  mkSlotId 0
{-# INLINE rootSlot #-}

leafSlot :: Int -> SlotId
leafSlot atomKey =
  mkSlotId (atomKey + 1)
{-# INLINE leafSlot #-}

atomSchema :: Int -> [SlotId]
atomSchema atomKey =
  [rootSlot, leafSlot atomKey]
{-# INLINE atomSchema #-}

rootBag :: BagId
rootBag =
  BagId 0
{-# INLINE rootBag #-}

rootBagKey :: Int
rootBagKey =
  unBag rootBag
{-# INLINE rootBagKey #-}

leafBag :: Int -> BagId
leafBag atomKey =
  BagId (atomKey + 1)
{-# INLINE leafBag #-}

unBag :: BagId -> Int
unBag (BagId value) =
  value
{-# INLINE unBag #-}

rowForAtomVariant :: Int -> Int -> Int -> RowTupleKey
rowForAtomVariant atomKey rootKey variant =
  rowForAtomValue atomKey rootKey ((atomKey + 1) * 1_000_000 + rootKey * 1_000 + variant)
{-# INLINE rowForAtomVariant #-}

rowForAtomValue :: Int -> Int -> Int -> RowTupleKey
rowForAtomValue _atomKey rootKey leafValue =
  tupleKeyFromInts [rootKey, leafValue]
{-# INLINE rowForAtomValue #-}

freshLeafValue :: Int -> Int -> Int -> Int -> Int
freshLeafValue atomKey rootKey iteration probe =
  (atomKey + 1) * 100_000_000
    + rootKey * 1_000_000
    + iteration * 97
    + probe
{-# INLINE freshLeafValue #-}

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
{-# INLINE nextWord64 #-}

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
{-# INLINE uniformInt #-}
