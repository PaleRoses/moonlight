{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Flow.Runtime.ProjectionSoakSpec
  ( tests,
  )
where

import Data.Bifunctor (first)
import Control.Exception
  ( Exception,
    throwIO,
  )
import Control.Monad
  ( unless,
    when,
  )
import Data.Bits
  ( shiftR,
    xor,
  )
import Data.Foldable qualified as Foldable
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
import Data.Void
  ( Void,
  )
import Data.Word
  ( Word64,
  )
import Moonlight.Delta.Operator
  ( OpResult (..),
    Operator (..),
  )
import Moonlight.Core
  ( LiveEpoch,
    QueryId,
    QuotientEpoch,
    atomIdKey,
    initialLiveEpoch,
    initialQuotientEpoch,
    mkAtomId,
    mkQueryId,
    mkSlotId,
    nextQuotientEpoch,
  )
import Moonlight.Differential.Frontier
  ( emptyRuntimeFrontier,
    emptyTraceRetention,
    frontierAdvanceVisibleMin,
    frontierWithTraceRetention,
  )
import Moonlight.Differential.Proposition
  ( PropositionKey (..),
  )
import Moonlight.Differential.Time
  ( FrontierStamp, frontierStamp,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    CarrierAddressBook (..),
    queryAtomCarrier,
    queryBagCarrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    carrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Frontier
  ( RelDiffFrontier,
    emptyRelDiffFrontier,
  )
import Moonlight.Flow.Carrier.Store
  ( CarrierStore,
    CarrierStoreDiagnostics (..),
    TraceId,
    carrierSnapshotLatestTrace,
    carrierStoreDiagnostics,
    commitCarrierDelta,
    emptyCarrierStore,
    lookupCarrierSnapshot,
    validateCarrierStore,
  )
import Moonlight.Flow.Carrier.View.Query
  ( visibleCarrierNow,
  )
import Moonlight.Flow.Carrier.Core.Summary
  ( CarrierBatchSummaryOps (..),
    CarrierStoreSummaryEntry (..),
  )
import Moonlight.Flow.Carrier.Core.Origin
  ( OriginEvent (OriginCompacted, OriginLocal),
    RelationalOrigin (..),
    emptyDerivationRoute,
    originAddParent,
    originMerge,
  )
import Moonlight.Flow.Carrier.Engine.Project
  ( CarrierProjectState (..),
  )
import Moonlight.Flow.Carrier.Morphism.Core.Program
  ( emptyCarrierMorphismRuntime,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    mkRelationalCarrierTime,
  )
import Moonlight.Flow.Carrier.View.Section
  ( RelationalSection (..),
  )
import Moonlight.Flow.Model.Delta
  ( QuotientPatch (..),
    mkAtomPatch
  )
import Moonlight.Differential.Row.Delta
  ( RowDeltaError
  )
import Moonlight.Delta.Signed
  ( Multiplicity (..),
    addMultiplicity,
    multiplicityValue,
    subtractMultiplicity,
    zeroMultiplicity
  )
import Moonlight.Flow.Model.Id
  ( BagId (..),
  )
import Moonlight.Differential.Row.Patch
  ( EpochTransition (..),
    plainRowPatchNull,
    plainRowPatchFromMultiplicityMap
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope,
    relationalScopeFromSets,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (..),
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    tupleKeyFromInts,
    tupleKeyToInts,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
    mkRuntimeBoundary,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128 (..),
  )
import Moonlight.Flow.Plan.Residual
  ( emptyResidualTheoryRegistry,
  )
import Moonlight.Flow.Execution.Observe.Telemetry
  ( defaultRepairTelemetryConfig,
  )
import Moonlight.Flow.Runtime.Carrier.Emit
  ( AtomCarrierEmitSpec,
    atomCarrierEmitSpec,
  )
import Moonlight.Flow.Runtime.Carrier.Emit
  ( FactorCarrierEmitSpec,
    factorCarrierEmitSpec,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
  )
import Moonlight.Flow.Runtime.Kernel.Config
  ( RuntimeConfig (..),
    mkRelDiffRuntime,
    mkRelDiffRuntimeConfig,
  )
import Moonlight.Flow.Runtime.Kernel.Operators
  ( RuntimeCarrierOperators (..),
  )
import Moonlight.Flow.Runtime.Carrier.Store
  ( commitCarrierDeltas,
    deltaAgainstCurrent,
  )
import Moonlight.Flow.Runtime.Engine.Patch.Apply
  ( applyQuotientPatch,
  )
import Moonlight.Flow.Runtime.Engine.Compaction
  ( compactRuntimeBefore,
  )
import Moonlight.Flow.Runtime.Carrier.State
  ( runtimeIndexOps,
  )
import Moonlight.Flow.Runtime.Core.State qualified as Core
import Moonlight.Flow.Runtime.Error
  ( RuntimeError (..),
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Core.Patch.Validation
  ( CanonicalityOracle (..),
  )
import Moonlight.Flow.Runtime.Topology.Routing
  ( Shard (..),
  )
import Moonlight.Flow.Runtime.Topology.Site.Types
  ( GeneratedContextShape (..),
    GeneratedQueryBinding (..),
    GeneratedRoutingSource (..),
    GeneratedSiteState (..),
    emptyGeneratedRoutingSource,
    emptyGeneratedSiteState,
    generatedContextShapeDigest,
    refreshGeneratedSiteDigest,
  )
import Moonlight.Flow.Carrier.Reuse
  ( ReuseMode (ExactOnly),
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( testCase,
  )
import Moonlight.FiniteLattice
  ( ContextLattice,
    singletonContextLattice
  )
import Moonlight.FiniteLattice
  ( principalSupport
  )


type Ctx = Int

type Prop = Int

type Evidence = ()

type JoinState = ()

type JoinErr = Void

type Runtime = RelDiffRuntime Ctx Prop RuntimeBoundary Evidence JoinState JoinErr

type Rows = Map RowTupleKey Multiplicity

runtimeIndexStores ::
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  IntMap (CarrierStore ctx Carrier prop boundary evidence)
runtimeIndexStores =
  runtimeIndexOps . rdrState

runtimeQuotientEpoch ::
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  QuotientEpoch
runtimeQuotientEpoch =
  Core.rsQuotientEpoch . rdrState

runtimeLiveEpoch ::
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  LiveEpoch
runtimeLiveEpoch =
  Core.rsLiveEpoch . rdrState

runtimeNextFrontierStamp ::
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  FrontierStamp
runtimeNextFrontierStamp =
  Core.rsNextFrontierStamp . rdrState

newtype Rng = Rng
  { unRng :: Word64
  }
  deriving stock (Eq, Ord, Show, Read)

data ProjectionSoakConfig = ProjectionSoakConfig
  { pscLaneCount :: {-# UNPACK #-} !Int,
    pscAuditPeriod :: {-# UNPACK #-} !Int,
    pscSeed :: {-# UNPACK #-} !Word64,
    pscCompactOnAudit :: !Bool
  }
  deriving stock (Eq, Show)

data ProjectionSoakFixture = ProjectionSoakFixture
  { psfQueryId :: !QueryId,
    psfProp :: !(PropositionKey Prop),
    psfAtomBoundary :: !RuntimeBoundary,
    psfFactorBoundary :: !RuntimeBoundary,
    psfAtomAddrs :: !(IntMap (CarrierAddr Ctx Carrier Prop)),
    psfFactorAddrs :: !(IntMap (CarrierAddr Ctx Carrier Prop)),
    psfInitialRows :: !(IntMap Rows),
    psfGeneratedSite :: !(GeneratedSiteState Ctx Prop)
  }

data ProjectionSoakState = ProjectionSoakState
  { pssRuntime :: !Runtime,
    pssRows :: !(IntMap Rows),
    pssRng :: !Rng
  }

data EditColumn
  = KeptColumn
  | DroppedColumn
  deriving stock (Eq, Ord, Show, Read)

data ProjectionEdit = ProjectionEdit
  { peLane :: {-# UNPACK #-} !Int,
    peColumn :: !EditColumn,
    peRemovedRows :: !Rows,
    peInsertedRows :: !Rows,
    peScope :: !RelationalScope
  }
  deriving stock (Eq, Show)

data VisibleStamp = VisibleStamp
  { vstTraceId :: !TraceId
  }
  deriving stock (Eq, Show)

data VisibleSnapshot = VisibleSnapshot
  { vsRows :: !Rows,
    vsStamp :: !(Maybe VisibleStamp)
  }
  deriving stock (Eq, Show)

data ProjectionSoakFailure
  = ProjectionPatchBuildFailed !RowDeltaError
  | ProjectionRuntimeFailed !String
  | ProjectionMissingIndexStore
  | ProjectionMissingLane !Int
  | ProjectionCutoffRowsChanged !Int !VisibleSnapshot !VisibleSnapshot
  | ProjectionCutoffTraceAdvanced !Int !(Maybe VisibleStamp) !(Maybe VisibleStamp)
  | ProjectionMaterialRowsMismatch !Int !Rows !Rows
  | ProjectionMaterialTraceDidNotAdvance !Int !(Maybe VisibleStamp) !(Maybe VisibleStamp)
  | ProjectionReplayInvalid !Shard !String
  | ProjectionReverseIndexUnbounded !Shard !CarrierStoreDiagnostics !Int
  deriving stock (Eq, Show)

instance Exception ProjectionSoakFailure

tests :: TestTree
tests =
  testGroup
    "projection maintenance"
    [ testCase "repeated patches preserve cutoff rows and materialized rows" projectionMaintenanceSoakTest
    ]

projectionMaintenanceSoakTest :: IO ()
projectionMaintenanceSoakTest = do
  let config =
        ProjectionSoakConfig
          { pscLaneCount = 8,
            pscAuditPeriod = 8,
            pscSeed = 0x5e8e9a2f4c31d17b,
            pscCompactOnAudit = False
          }
  fixture <- eitherDie (ProjectionRuntimeFailed . show) (buildProjectionSoakFixture config)
  runtime0 <- eitherDie (ProjectionRuntimeFailed . show) (mkProjectionRuntime config fixture)
  let state0 =
        ProjectionSoakState
          { pssRuntime = runtime0,
            pssRows = psfInitialRows fixture,
            pssRng = Rng (pscSeed config `xor` 0x9e3779b97f4a7c15)
          }
  stateN <-
    Foldable.foldlM
      (runAuditedProjectionIteration fixture config)
      state0
      [0 .. 63]
  _ <- auditProjectionSoak fixture config stateN
  pure ()

runAuditedProjectionIteration ::
  ProjectionSoakFixture ->
  ProjectionSoakConfig ->
  ProjectionSoakState ->
  Int ->
  IO ProjectionSoakState
runAuditedProjectionIteration fixture config state0 iteration = do
  state1 <- runProjectionIteration fixture config iteration state0
  if (iteration + 1) `rem` pscAuditPeriod config == 0
    then auditProjectionSoak fixture config state1
    else pure state1

runProjectionIteration ::
  ProjectionSoakFixture ->
  ProjectionSoakConfig ->
  Int ->
  ProjectionSoakState ->
  IO ProjectionSoakState
runProjectionIteration fixture config iteration state0 = do
  let (rng1, edit) = generateProjectionEdit fixture config iteration (pssRows state0) (pssRng state0)
      lane = peLane edit
      before = snapshotFactorLane fixture (pssRuntime state0) lane
      patchEither = quotientPatchForEdit fixture (pssRuntime state0) edit
  patch <- eitherDie ProjectionPatchBuildFailed patchEither
  runtimeAfterPatch <-
    eitherDie
      (ProjectionRuntimeFailed . show)
      (applyQuotientPatch patch (pssRuntime state0))
  runtime1 <-
    eitherDie
      (ProjectionRuntimeFailed . show)
      ( recomputeFactors
          fixture
          (projectionRepairTime runtimeAfterPatch)
          (peScope edit)
          runtimeAfterPatch
      )
  let rows1 = applyEditRows edit (pssRows state0)
      after = snapshotFactorLane fixture runtime1 lane
  validateProjectionEdit fixture lane rows1 before after edit
  pure
    state0
      { pssRuntime = runtime1,
        pssRows = rows1,
        pssRng = rng1
      }

quotientPatchForEdit ::
  ProjectionSoakFixture ->
  Runtime ->
  ProjectionEdit ->
  Either RowDeltaError QuotientPatch
quotientPatchForEdit _fixture runtime edit = do
  patch <- mkAtomPatch (peRemovedRows edit) (peInsertedRows edit)
  let epoch = runtimeQuotientEpoch runtime
      atomKey = peLane edit
  pure
    QuotientPatch
      { qpEpoch =
          EpochTransition
            { etBefore = epoch,
              etAfter = nextQuotientEpoch epoch
            },
        qpScope = peScope edit,
        qpAtomScopeByAtom = IntMap.empty,
        qpEvents = IntMap.singleton atomKey patch
      }

generateProjectionEdit ::
  ProjectionSoakFixture ->
  ProjectionSoakConfig ->
  Int ->
  IntMap Rows ->
  Rng ->
  (Rng, ProjectionEdit)
generateProjectionEdit fixture config iteration rowsByLane rng0 =
  let (rng1, lane) = uniformInt (pscLaneCount config) rng0
      (rng2, bucket) = uniformInt 100 rng1
      column = if bucket < 70 then DroppedColumn else KeptColumn
      oldRows = IntMap.findWithDefault (initialRowsForLane lane) lane rowsByLane
      oldRow = currentLaneRow lane oldRows
      newRow = mutateRow lane column iteration oldRow
      removedRows = Map.singleton oldRow (Multiplicity 1)
      insertedRows = Map.singleton newRow (Multiplicity 1)
   in ( rng2,
        ProjectionEdit
          { peLane = lane,
            peColumn = column,
            peRemovedRows = removedRows,
            peInsertedRows = insertedRows,
            peScope = scopeForEdit fixture lane column
          }
      )

currentLaneRow :: Int -> Rows -> RowTupleKey
currentLaneRow lane rows =
  maybe (initialRow lane) fst (Map.lookupMin rows)

mutateRow :: Int -> EditColumn -> Int -> RowTupleKey -> RowTupleKey
mutateRow lane column iteration row =
  let values = tupleKeyToInts row
      freshValue salt = lane * 1000000 + iteration * 17 + salt
   in tupleKeyFromInts
        [ case (column, ix) of
            (KeptColumn, 1) -> freshValue 3
            (DroppedColumn, 2) -> freshValue 5
            _ -> value
        | (ix, value) <- zip [0 :: Int ..] values
        ]

applyEditRows :: ProjectionEdit -> IntMap Rows -> IntMap Rows
applyEditRows edit =
  IntMap.insert (peLane edit) (applyRowsDelta (peRemovedRows edit) (peInsertedRows edit))

applyRowsDelta :: Rows -> Rows -> Rows
applyRowsDelta removed inserted =
  Map.foldlWithKey' removeRow inserted removed
  where
    removeRow rows row removedMultiplicity =
      case subtractMultiplicity (Map.findWithDefault zeroMultiplicity row rows) removedMultiplicity of
        Just nextMultiplicity
          | multiplicityValue nextMultiplicity > 0 ->
              Map.insert row nextMultiplicity rows
        _ ->
          Map.delete row rows

scopeForEdit :: ProjectionSoakFixture -> Int -> EditColumn -> RelationalScope
scopeForEdit _fixture lane column =
  let dirtyKey = dirtyKeyOf lane column
      resultKey = factorResultKey lane
   in relationalScopeFromSets
        (IntSet.singleton dirtyKey)
        (IntSet.singleton resultKey)
        (IntSet.singleton lane)
        (IntSet.singleton resultKey)
        (IntSet.fromList [dirtyKey, resultKey])

validateProjectionEdit ::
  ProjectionSoakFixture ->
  Int ->
  IntMap Rows ->
  VisibleSnapshot ->
  VisibleSnapshot ->
  ProjectionEdit ->
  IO ()
validateProjectionEdit _fixture lane rowsByLane before after edit =
  case peColumn edit of
    DroppedColumn -> do
      unless (vsRows before == vsRows after) $
        dieFailure (ProjectionCutoffRowsChanged lane before after)
      unless (vsStamp before == vsStamp after) $
        dieFailure (ProjectionCutoffTraceAdvanced lane (vsStamp before) (vsStamp after))
    KeptColumn -> do
      let expectedRows = expectedFactorRows lane rowsByLane
      unless (vsRows after == expectedRows) $
        dieFailure (ProjectionMaterialRowsMismatch lane expectedRows (vsRows after))
      unless (vsStamp before /= vsStamp after) $
        dieFailure (ProjectionMaterialTraceDidNotAdvance lane (vsStamp before) (vsStamp after))

auditProjectionSoak ::
  ProjectionSoakFixture ->
  ProjectionSoakConfig ->
  ProjectionSoakState ->
  IO ProjectionSoakState
auditProjectionSoak _fixture config state0 = do
  validateReplay state0
  validateReverseIndexes config state0
  if pscCompactOnAudit config
    then do
      compacted <- eitherDie (ProjectionRuntimeFailed . show) (compactRuntimeBefore (frontierForCompaction (pssRuntime state0)) (pssRuntime state0))
      pure state0 {pssRuntime = compacted}
    else pure state0

validateReplay :: ProjectionSoakState -> IO ()
validateReplay state =
  IntMap.foldlWithKey'
    ( \ioUnit shardKeyValue store -> do
        ioUnit
        case validateCarrierStore projectionLattice store of
          Right () -> pure ()
          Left replayError -> dieFailure (ProjectionReplayInvalid (Shard shardKeyValue) (show replayError))
    )
    (pure ())
    (runtimeIndexStores (pssRuntime state))

validateReverseIndexes :: ProjectionSoakConfig -> ProjectionSoakState -> IO ()
validateReverseIndexes config state =
  IntMap.foldlWithKey'
    ( \ioUnit shardKeyValue store -> do
        ioUnit
        let diagnostics = carrierStoreDiagnostics store
            entryBound = max 1 (csdTraceEntries diagnostics)
            memberBound = max 1 (pscLaneCount config * 4 * entryBound)
            bad =
              csdTraceDepMembers diagnostics > memberBound
                || csdTraceTopoMembers diagnostics > memberBound
                || csdTraceRootMembers diagnostics > memberBound
                || csdTraceResultMembers diagnostics > memberBound
        when bad $
          dieFailure (ProjectionReverseIndexUnbounded (Shard shardKeyValue) diagnostics memberBound)
    )
    (pure ())
    (runtimeIndexStores (pssRuntime state))

mkProjectionRuntime ::
  ProjectionSoakConfig ->
  ProjectionSoakFixture ->
  Either String Runtime
mkProjectionRuntime config fixture = do
  seededStore <- seedCarrierStore fixture
  runtimeConfig <-
    first show $
      mkRelDiffRuntimeConfig
        RuntimeConfig
          { rcQuotientEpoch = initialQuotientEpoch,
            rcLiveEpoch = initialLiveEpoch,
            rcNextFrontierStamp = frontierStamp 0,
            rcCanonicalityOracle = projectionOracle config,
            rcAtomCarrierEmitSpec = projectionAtomEmitSpec fixture,
            rcFactorCarrierEmitSpec = projectionFactorEmitSpec fixture,
            rcCarrierOperators =
              RuntimeCarrierOperators
                { rcoProjectOperator =
                    noOpOperator,
                  rcoRestrictOperator =
                    noOpOperator
                },
            rcCarrierSummaryOps = projectionCarrierSummaryOps,
            rcFrontier = emptyRelDiffFrontier,
            rcProjectStates = IntMap.singleton 0 (dummyProjectState fixture),
            rcRestrictStates = IntMap.singleton 0 emptyCarrierMorphismRuntime,
            rcIndexStates = IntMap.singleton (shardKey (Shard 0)) seededStore,
            rcVisibleCacheBudgetBytes = 4096,
            rcVisibleSectionBytes = visibleSectionSize,
            rcContextLattice = projectionLattice,
            rcRepairTelemetry = defaultRepairTelemetryConfig,
            rcGeneratedSite = psfGeneratedSite fixture,
            rcFactorPrograms = Map.empty,
            rcQueryBindings = Map.empty,
            rcReuseMode = ExactOnly,
            rcResidualTheoryRegistry = emptyResidualTheoryRegistry
          }
  pure (mkRelDiffRuntime runtimeConfig)

buildProjectionSoakFixture :: ProjectionSoakConfig -> Either String ProjectionSoakFixture
buildProjectionSoakFixture config = do
  atomBoundary <- first show (mkRuntimeBoundary [mkSlotId 0, mkSlotId 1, mkSlotId 2] IntSet.empty IntMap.empty)
  factorBoundary <- first show (mkRuntimeBoundary [mkSlotId 1] IntSet.empty IntMap.empty)
  let queryId = mkQueryId 7001
      propKey = PropositionKey 0
      lanes = [0 .. pscLaneCount config - 1]
      atomAddrs = IntMap.fromAscList [(lane, atomAddr queryId propKey lane) | lane <- lanes]
      factorAddrs = IntMap.fromAscList [(lane, factorAddr queryId propKey lane) | lane <- lanes]
      source = generatedRoutingSource queryId propKey atomAddrs factorAddrs
      site =
        refreshGeneratedSiteDigest
          emptyGeneratedSiteState
            { gssContexts =
                Map.singleton 0 (projectionGeneratedContextShape queryId propKey),
              gssRouteSource = source
            }
  pure
    ProjectionSoakFixture
      { psfQueryId = queryId,
        psfProp = propKey,
        psfAtomBoundary = atomBoundary,
        psfFactorBoundary = factorBoundary,
        psfAtomAddrs = atomAddrs,
        psfFactorAddrs = factorAddrs,
        psfInitialRows = IntMap.fromAscList [(lane, initialRowsForLane lane) | lane <- lanes],
        psfGeneratedSite = site
      }

generatedRoutingSource ::
  QueryId ->
  PropositionKey Prop ->
  IntMap (CarrierAddr Ctx Carrier Prop) ->
  IntMap (CarrierAddr Ctx Carrier Prop) ->
  GeneratedRoutingSource Ctx Prop
generatedRoutingSource queryId _propKey atomAddrs factorAddrs =
  emptyGeneratedRoutingSource
    { grsAtomSubscribers =
        IntMap.fromAscList [(lane, [(queryId, mkAtomId lane)]) | lane <- IntMap.keys atomAddrs],
      grsIndexShardsByCarrier =
        Map.fromAscList
          [ (addr, Shard 0)
          | addr <- IntMap.elems atomAddrs <> IntMap.elems factorAddrs
          ],
      grsRestrictShardsByCarrier = Map.empty
    }

projectionGeneratedContextShape ::
  QueryId ->
  PropositionKey Prop ->
  GeneratedContextShape Prop
projectionGeneratedContextShape queryId propKey =
  refreshGeneratedContextShape
    GeneratedContextShape
      { gcsShapeDigest = projectionEmptyDigest,
        gcsQueryBindings =
          Map.singleton
            queryId
            GeneratedQueryBinding
              { gqbProp = propKey,
                gqbProjectShard = Shard 0
              },
        gcsIndexShardsByProp = Map.singleton propKey (Shard 0)
      }

refreshGeneratedContextShape ::
  GeneratedContextShape Prop ->
  GeneratedContextShape Prop
refreshGeneratedContextShape shape =
  shape {gcsShapeDigest = generatedContextShapeDigest shape}

projectionEmptyDigest :: StableDigest128
projectionEmptyDigest =
  StableDigest128 0 0

seedCarrierStore :: ProjectionSoakFixture -> Either String (CarrierStore Ctx Carrier Prop RuntimeBoundary Evidence)
seedCarrierStore fixture =
  Foldable.foldlM insertOne emptyCarrierStore initialDeltas
  where
    initialDeltas =
      [ snapshotDelta fixture (initialEventTime (psfQueryId fixture)) addr boundary rows mempty
      | (lane, addr) <- IntMap.toAscList (psfAtomAddrs fixture),
        let rows = IntMap.findWithDefault Map.empty lane (psfInitialRows fixture),
        let boundary = psfAtomBoundary fixture
      ]
        <> [ snapshotDelta fixture (initialEventTime (psfQueryId fixture)) addr boundary rows mempty
           | (lane, addr) <- IntMap.toAscList (psfFactorAddrs fixture),
             let rows = expectedFactorRows lane (psfInitialRows fixture),
             let boundary = psfFactorBoundary fixture
           ]

    insertOne ::
      CarrierStore Ctx Carrier Prop RuntimeBoundary Evidence ->
      RelationalCarrierDelta Ctx Carrier Prop RuntimeBoundary Evidence ->
      Either String (CarrierStore Ctx Carrier Prop RuntimeBoundary Evidence)
    insertOne store deltaValue =
      first show (commitCarrierDelta projectionLattice deltaValue store)

recomputeFactors ::
  ProjectionSoakFixture ->
  RelationalCarrierTime Ctx ->
  RelationalScope ->
  Runtime ->
  Either (RelationalRuntimeError Ctx Prop RuntimeBoundary Evidence) Runtime
recomputeFactors fixture eventTime scope runtime0 = do
  store <-
    case indexStoreOf runtime0 of
      Nothing -> Left (RuntimeMissingIndexShard (Shard 0))
      Just value -> Right value
  let atomRows =
        IntMap.map (\addr -> visibleCarrierNow addr store) (psfAtomAddrs fixture)
      desiredDeltas =
        [ snapshotDelta fixture eventTime addr (psfFactorBoundary fixture) rows scope
        | (lane, addr) <- IntMap.toAscList (psfFactorAddrs fixture),
          let rows = expectedFactorRows lane atomRows
        ]
  materialDeltas <- traverse (`deltaAgainstCurrent` runtime0) desiredDeltas
  fst
    <$> commitCarrierDeltas
      (filter (not . plainRowPatchNull . deRows) materialDeltas)
      runtime0

projectionRepairTime :: Runtime -> RelationalCarrierTime Ctx
projectionRepairTime runtime =
  mkRelationalCarrierTime
    0
    (runtimeQuotientEpoch runtime)
    (runtimeLiveEpoch runtime)
    PhaseProject
    (runtimeNextFrontierStamp runtime)

snapshotDelta ::
  ProjectionSoakFixture ->
  RelationalCarrierTime Ctx ->
  CarrierAddr Ctx Carrier Prop ->
  RuntimeBoundary ->
  Rows ->
  RelationalScope ->
  RelationalCarrierDelta Ctx Carrier Prop RuntimeBoundary Evidence
snapshotDelta fixture eventTime addr boundary rows scope =
  RelationalCarrierDelta
    { deAddr = addr,
      deTime = eventTime,
      deSupport = principalSupport 0,
      deBoundary = boundary,
      deEvidence = (),
      deRows = (plainRowPatchFromMultiplicityMap rows),
      deOrigin =
        RelationalOrigin
          { roEvent = OriginLocal (psfQueryId fixture),
            roRoute = emptyDerivationRoute
          },
      deScope = scope,
      dePayload = ()
    }

projectionCarrierSummaryOps ::
  CarrierBatchSummaryOps
    Ctx
    Carrier
    Prop
    RuntimeBoundary
    Evidence
    (CarrierStoreSummaryEntry Ctx Carrier Prop RuntimeBoundary Evidence)
projectionCarrierSummaryOps =
  CarrierBatchSummaryOps
    { cbsoSummaryBoundary = \_addr entries -> csseBoundary (Foldable.maximumBy compareSummaryEntryTime entries),
      cbsoSummaryEvidence = \_addr _entries -> (),
      cbsoSummaryOrigin =
        \addr entries ->
          originAddParent
            addr
            (originMerge OriginCompacted (fmap csseOrigin entries))
    }

compareSummaryEntryTime ::
  CarrierStoreSummaryEntry Ctx Carrier Prop RuntimeBoundary Evidence ->
  CarrierStoreSummaryEntry Ctx Carrier Prop RuntimeBoundary Evidence ->
  Ordering
compareSummaryEntryTime left right = compare (csseTime left) (csseTime right)

noOpOperator :: Operator time state input output err
noOpOperator =
  Operator
    { opStep = \stateValue _timedInput -> Right OpResult {orState = stateValue, orEmit = []},
      opFlush = \stateValue -> Right OpResult {orState = stateValue, orEmit = []}
    }

projectionOracle :: ProjectionSoakConfig -> CanonicalityOracle RowTupleKey
projectionOracle config =
  CanonicalityOracle
    { isCanonicalRowAt = \_epoch _row -> True,
      canonicalizeRowAt = \_epoch row -> row,
      expectedRowWidthAt = \_epoch atomId ->
        if atomIdKey atomId >= 0 && atomIdKey atomId < pscLaneCount config
          then Just 3
          else Nothing,
      dirtyKeysOfRowAt = \_epoch row -> dirtyKeysOfTuple row,
      dirtyTopoForDirtyKey = \dirtyKey -> IntSet.singleton (dirtyKey `quot` 2),
      dirtyTopoForAtom = \atomId -> IntSet.singleton (atomIdKey atomId)
    }

projectionAtomEmitSpec :: ProjectionSoakFixture -> AtomCarrierEmitSpec Ctx Prop RuntimeBoundary Evidence
projectionAtomEmitSpec fixture =
  atomCarrierEmitSpec
    CarrierAddressBook
      { cabContextOfQuery = const 0,
        cabPropOfQuery = const (psfProp fixture)
      }
    (const (principalSupport 0))
    (\_queryId _atomId _rows -> psfAtomBoundary fixture)
    (const ())

projectionFactorEmitSpec :: ProjectionSoakFixture -> FactorCarrierEmitSpec Ctx Prop RuntimeBoundary Evidence
projectionFactorEmitSpec fixture =
  factorCarrierEmitSpec
    CarrierAddressBook
      { cabContextOfQuery = const 0,
        cabPropOfQuery = const (psfProp fixture)
      }
    (\_queryId _payload -> principalSupport 0)
    (\_queryId _carrier _schema -> psfFactorBoundary fixture)
    (\_queryId _payload -> ())

dummyProjectState :: ProjectionSoakFixture -> CarrierProjectState Ctx Prop RuntimeBoundary Evidence
dummyProjectState fixture =
  CarrierProjectState
    { cpsAddressBook =
        CarrierAddressBook
          { cabContextOfQuery = const 0,
            cabPropOfQuery = const (psfProp fixture)
          },
      cpsSupportOf = \_delta _slice -> principalSupport 0,
      cpsBoundaryOf = \_queryId _carrier -> psfFactorBoundary fixture,
      cpsEvidenceOf = \_delta _slice -> ()
    }

visibleSectionSize :: RelationalSection Ctx Carrier Prop -> Int
visibleSectionSize sectionValue = max 1 (Map.size (rsCarriers sectionValue))

snapshotFactorLane :: ProjectionSoakFixture -> Runtime -> Int -> VisibleSnapshot
snapshotFactorLane fixture runtime lane =
  case (indexStoreOf runtime, IntMap.lookup lane (psfFactorAddrs fixture)) of
    (Just store, Just addr) ->
      VisibleSnapshot
        { vsRows = visibleCarrierNow addr store,
          vsStamp = currentVisibleStamp store addr
        }
    _ -> VisibleSnapshot {vsRows = Map.empty, vsStamp = Nothing}

currentVisibleStamp ::
  CarrierStore Ctx Carrier Prop RuntimeBoundary Evidence ->
  CarrierAddr Ctx Carrier Prop ->
  Maybe VisibleStamp
currentVisibleStamp store addr = do
  snapshot <- lookupCarrierSnapshot addr store
  pure VisibleStamp {vstTraceId = carrierSnapshotLatestTrace snapshot}

indexStoreOf :: Runtime -> Maybe (CarrierStore Ctx Carrier Prop RuntimeBoundary Evidence)
indexStoreOf runtime =
  IntMap.lookup (shardKey (Shard 0)) (runtimeIndexStores runtime)

expectedFactorRows :: Int -> IntMap Rows -> Rows
expectedFactorRows lane rowsByLane =
  projectKeptColumn (IntMap.findWithDefault Map.empty lane rowsByLane)

projectKeptColumn :: Rows -> Rows
projectKeptColumn =
  Map.filter ((/= 0) . multiplicityValue)
    . Map.fromListWith addMultiplicity
    . fmap projectOne
    . Map.toAscList
  where
    projectOne :: (RowTupleKey, Multiplicity) -> (RowTupleKey, Multiplicity)
    projectOne (row, multiplicity) =
      ( tupleKeyFromInts $
          case tupleKeyToInts row of
            _lane : kept : _dropped : _rest -> [kept]
            _ -> [],
        multiplicity
      )

initialRowsForLane :: Int -> Rows
initialRowsForLane lane = Map.singleton (initialRow lane) (Multiplicity 1)

initialRow :: Int -> RowTupleKey
initialRow lane = tupleKeyFromInts [lane, lane * 1000000 + 1, lane * 1000000 + 2]

dirtyKeysOfTuple :: RowTupleKey -> IntSet
dirtyKeysOfTuple row =
  case tupleKeyToInts row of
    lane : _ -> IntSet.fromList [dirtyKeyOf lane KeptColumn, dirtyKeyOf lane DroppedColumn]
    [] -> IntSet.empty

dirtyKeyOf :: Int -> EditColumn -> Int
dirtyKeyOf lane column =
  lane * 2
    + case column of
      KeptColumn -> 0
      DroppedColumn -> 1

factorResultKey :: Int -> Int
factorResultKey = id

atomAddr :: QueryId -> PropositionKey Prop -> Int -> CarrierAddr Ctx Carrier Prop
atomAddr queryId propKey lane =
  carrierAddr 0 propKey (queryAtomCarrier queryId (mkAtomId lane))

factorAddr :: QueryId -> PropositionKey Prop -> Int -> CarrierAddr Ctx Carrier Prop
factorAddr queryId propKey lane =
  carrierAddr 0 propKey (queryBagCarrier queryId (BagId lane))

initialEventTime :: QueryId -> RelationalCarrierTime Ctx
initialEventTime _queryId =
  mkRelationalCarrierTime 0 initialQuotientEpoch initialLiveEpoch PhaseProject (frontierStamp 0)

frontierForCompaction :: Runtime -> RelDiffFrontier Ctx RelationalPhase
frontierForCompaction runtime =
  frontierAdvanceVisibleMin
    ( mkRelationalCarrierTime
        0
        (runtimeQuotientEpoch runtime)
        (runtimeLiveEpoch runtime)
        PhaseAmalgamate
        (runtimeNextFrontierStamp runtime)
    )
    (frontierWithTraceRetention (Just emptyTraceRetention) emptyRuntimeFrontier)

projectionLattice :: ContextLattice Ctx
projectionLattice =
  singletonContextLattice 0

nextWord64 :: Rng -> (Rng, Word64)
nextWord64 (Rng seed0) =
  let !seed1 = seed0 + 0x9e3779b97f4a7c15
      !z0 = seed1
      !z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
      !z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
      !z3 = z2 `xor` (z2 `shiftR` 31)
   in (Rng seed1, z3)

uniformInt :: Int -> Rng -> (Rng, Int)
uniformInt bound rng0
  | bound <= 1 = (fst (nextWord64 rng0), 0)
  | otherwise =
      let (rng1, word) = nextWord64 rng0
       in (rng1, fromIntegral (word `rem` fromIntegral bound))

eitherDie :: (err -> ProjectionSoakFailure) -> Either err a -> IO a
eitherDie mkFailure = either (dieFailure . mkFailure) pure

dieFailure :: ProjectionSoakFailure -> IO a
dieFailure = throwIO
