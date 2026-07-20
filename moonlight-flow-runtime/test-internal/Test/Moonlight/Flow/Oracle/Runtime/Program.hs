{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Test.Moonlight.Flow.Oracle.Runtime.Program
  ( Ctx,
    Evidence,
    Prop,
    TestRuntime,
    RuntimeProgramCase (..),
    RuntimeProgramCaseError (..),
    RuntimeTriangleOptions (..),
    TriangleAtomOrder (..),
    carrierTime,
    currentSnapshot,
    defaultRuntimeTriangleOptions,
    extraSingletonRowsLike,
    factorSpec,
    insertAtomSnapshots,
    insertSnapshots,
    programCacheEmpty,
    programCacheEntryCount,
    rowsOf,
    runtimeFromProgramCases,
    runtimeProgramCanonicalDigest,
    runtimeProgramShapeForSnapshot,
    runtimeTriangleCase,
    runtimeUnaryCase,
    visibleReferenceSnapshots,
  )
where

import Data.Bifunctor (first)
import Data.IntSet qualified as IntSet
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Void
  ( Void,
  )
import Data.Word
  ( Word64,
  )
import Moonlight.Core
  ( AtomId,
    QueryId,
    SlotId,
    initialLiveEpoch,
    initialQuotientEpoch,
    atomIdKey,
    mkQueryId,
  )
import Moonlight.Differential.Proposition
  ( PropositionKey (..),
  )
import Moonlight.Differential.Time
  ( frontierStamp,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    CarrierAddressBook (..),
    QueryCarrierNode (..),
    queryAtomCarrier,
    queryFactorCarrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caCarrier,
    carrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Origin
  ( OriginEvent (OriginAtom, OriginFactor),
    RelationalOrigin (..),
    emptyDerivationRoute,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    mkRelationalCarrierTime,
  )
import Moonlight.Flow.Execution.Factor.Types
  ( FactorEntry (..),
    FactorRunResult (..),
    emptyFactorCache,
    factorCacheEntries,
  )
import Moonlight.Flow.Execution.Factor.Core
  ( Factor,
  )
import Moonlight.Flow.Execution.Subsumption.FactorShape
  ( FactorShapeNodeManifest (..),
    FactorShapeError (..),
    factorShapeFromManifestBoundary,
    lookupFactorShapeManifestNode,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Delta.Signed
  ( MultiplicityChange (..)
  )
import Moonlight.Differential.Row.Patch
  (
    plainRowPatchFromList,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (..),
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Flow.Model.Schema.Boundary
  ( BoundaryShape (..),
    RuntimeBoundary,
    RuntimeBoundaryError,
    boundaryShape,
    mkBoundary,
    mkRuntimeBoundary,
    runtimeBoundaryDigest,
  )
import Moonlight.Flow.Plan.Query.Core
  ( FactorNode (..),
    orderedSlotNub,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.Flow.Plan.Shape
  ( CanonicalizationResult (..),
  )
import Moonlight.Flow.Plan.Shape.Term
  ( PlanShape (..),
    PlanStage (FactorShape),
  )
import Moonlight.Flow.Runtime.Spec.Schema
  ( emptyRuntimeInitialData,
    runtimeContextSchema,
    runtimeSchema,
  )
import Moonlight.Flow.Runtime.Types
  ( RuntimeCreateError,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
  )
import Moonlight.Flow.Runtime.Spec.Schema
  ( RuntimePlan (..),
    RuntimeSpec (..),
    runtimePlanProjectionFromSlots,
  )
import Moonlight.Flow.Runtime.Backend
  ( RuntimeBackend (..),
    defaultBackend,
  )
import Moonlight.Flow.Runtime.Types
  ( defaultRuntimeCreateOptions,
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Carrier.Store
  ( commitCarrierDeltas,
    currentCarrierMaybe,
  )
import Moonlight.Flow.Runtime.Kernel.Create
  ( createRelDiffRuntimeWithBackend,
  )
import Moonlight.Flow.Runtime.Core.State qualified as Core
import Moonlight.Flow.Runtime.Factor.Input
  ( factorInputSignatureFromAtomReadouts,
  )
import Moonlight.Flow.Runtime.Topology
  ( updateRuntimePlanReuse,
  )
import Moonlight.Flow.Runtime.Factor.State
  ( RuntimeFactorState (..),
    lookupFactorProgram,
  )
import Moonlight.Flow.Runtime.Factor.Internal.Cache
  ( FactorCacheState (..),
  )
import Moonlight.Flow.Runtime.Factor.Program
  ( FactorProgram (..),
    factorProgramCanonical,
    factorProgramFactorShapeManifest,
    factorProgramRepairKey,
    factorProgramSpec,
  )
import Moonlight.Flow.Runtime.Spec.FactorProgram
  ( FactorProgramSpec,
    RepairProgramKey,
  )
import Moonlight.Flow.Carrier.Reuse
  ( ReuseMode,
  )
import Moonlight.Flow.Runtime.Carrier.Emit
  ( FactorCarrierEmitSpec,
    factorCarrierEmitSpec,
  )
import Moonlight.Flow.Runtime.Carrier.Emit.Factor
  ( factorNodeCarrierVisible,
  )
import Moonlight.Flow.Carrier.Reuse
  ( PlanReuseRegistrationEntry (..),
    PlanReuseState,
    emptyPlanReuseState,
  )
import Moonlight.Flow.Carrier.Reuse
  ( SubsumptionRegistrationError,
  )
import Moonlight.Flow.Carrier.Reuse
  ( registerFactorCarrierShapes,
  )
import Moonlight.Differential.Index.IndexedRows
  ( indexedRowsPayloadMap,
  )
import Test.Moonlight.Flow.Execution.RelProgram
  ( RelExecutionError (..),
    RelProgram,
    atom,
    programFactorRunCached,
    programFactorProgram,
    programRuntimeAtomInputs,
    programRuntimeAtoms,
    programWithQueryId,
  )
import Moonlight.Flow.Model.Scope
import Moonlight.FiniteLattice
  ( principalSupport
  )

-- | The runtime reuse properties use one declared relational program and observe
-- the runtime artifacts derived from it. This replaces the old bespoke fixture
-- worlds: the local section is the program, the overlap obligations are the
-- factor snapshots, and the runtime state is glued from those derived views.
type Ctx = Int

type Prop = Int

type Evidence = ()

type TestRuntime =
  RelDiffRuntime Ctx Prop RuntimeBoundary Evidence () Void

data RuntimeProgramCase ctx prop evidence = RuntimeProgramCase
  { rpcQueryId :: !QueryId,
    rpcRelProgram :: !RelProgram,
    rpcRuntimeProgram :: !FactorProgram,
    rpcAtomSnapshots :: ![RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence],
    rpcCarrierSnapshots :: ![RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence],
    rpcPlanReuse :: !(PlanReuseState ctx prop)
  }

data RuntimeProgramCaseError
  = RuntimeProgramExecutionFailed !RelExecutionError
  | RuntimeProgramBoundaryFailed !FactorNode !RuntimeBoundaryError
  | RuntimeProgramShapeFailed !FactorShapeError
  | RuntimeProgramRegistrationFailed !SubsumptionRegistrationError
  deriving stock (Show)

data TriangleAtomOrder
  = TriangleAtomsForward
  | TriangleAtomsReversed
  deriving stock (Eq, Ord, Show, Read)

data RuntimeTriangleOptions = RuntimeTriangleOptions
  { rtoName :: !String,
    rtoQueryId :: !QueryId,
    rtoAtomKeyOffset :: !Int,
    rtoSlotKeyOffset :: !Int,
    rtoAtomOrder :: !TriangleAtomOrder
  }
  deriving stock (Eq, Show)

defaultRuntimeTriangleOptions :: RuntimeTriangleOptions
defaultRuntimeTriangleOptions =
  RuntimeTriangleOptions
    { rtoName = "runtime-triangle",
      rtoQueryId = mkQueryId 0,
      rtoAtomKeyOffset = 0,
      rtoSlotKeyOffset = 0,
      rtoAtomOrder = TriangleAtomsForward
    }
{-# INLINE defaultRuntimeTriangleOptions #-}

runtimeTriangleCase ::
  Ord ctx =>
  Ord prop =>
  RuntimeTriangleOptions ->
  RelationalCarrierTime ctx ->
  ctx ->
  prop ->
  evidence ->
  Either RuntimeProgramCaseError (RuntimeProgramCase ctx prop evidence)
runtimeTriangleCase options =
  runtimeProgramCase (rtoQueryId options) (triangleProgramWith options)
{-# INLINE runtimeTriangleCase #-}

runtimeUnaryCase ::
  Ord ctx =>
  Ord prop =>
  RuntimeTriangleOptions ->
  RelationalCarrierTime ctx ->
  ctx ->
  prop ->
  evidence ->
  Either RuntimeProgramCaseError (RuntimeProgramCase ctx prop evidence)
runtimeUnaryCase options =
  runtimeProgramCase (rtoQueryId options) (unaryProgramWith options)
{-# INLINE runtimeUnaryCase #-}

runtimeProgramCase ::
  Ord ctx =>
  Ord prop =>
  QueryId ->
  RelProgram ->
  RelationalCarrierTime ctx ->
  ctx ->
  prop ->
  evidence ->
  Either RuntimeProgramCaseError (RuntimeProgramCase ctx prop evidence)
runtimeProgramCase queryIdValue relProgram eventTime contextValue propValue evidenceValue = do
  runtimeProgram <-
    first RuntimeProgramExecutionFailed $
      programFactorProgram relProgram
  factorResult <-
    first RuntimeProgramExecutionFailed $
      programFactorRunCached emptyFactorCache relProgram
  let atomInputs =
        programRuntimeAtomInputs relProgram
  atomSnapshots <-
    traverse
      (atomSnapshotEntry queryIdValue eventTime contextValue propValue evidenceValue)
      atomInputs
  snapshots <-
    traverse
      (snapshotEntry runtimeProgram queryIdValue eventTime contextValue propValue evidenceValue)
      (factorCacheEntries (frrCache factorResult))
  let inputDigest =
        factorInputSignatureFromAtomReadouts
          (IntMap.fromAscList (fmap atomInputSignatureReadout atomInputs))
  planReuse <-
    first RuntimeProgramRegistrationFailed $
      registerFactorCarrierShapes
        queryIdValue
        (factorProgramCanonical runtimeProgram)
        (factorProgramFactorShapeManifest runtimeProgram)
        inputDigest
        (fmap registrationEntryFromSnapshot (filter visibleSnapshot snapshots))
        emptyPlanReuseState
  pure
    RuntimeProgramCase
      { rpcQueryId = queryIdValue,
        rpcRelProgram = relProgram,
        rpcRuntimeProgram = runtimeProgram,
        rpcAtomSnapshots = atomSnapshots,
        rpcCarrierSnapshots = snapshots,
        rpcPlanReuse = planReuse
      }
{-# INLINE runtimeProgramCase #-}

runtimeProgramCanonicalDigest :: RuntimeProgramCase ctx prop evidence -> StableDigest128
runtimeProgramCanonicalDigest =
  psDigest . crPlan . factorProgramCanonical . rpcRuntimeProgram
{-# INLINE runtimeProgramCanonicalDigest #-}

runtimeProgramShapeForSnapshot ::
  RuntimeProgramCase ctx prop evidence ->
  RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence ->
  Either RuntimeProgramCaseError (PlanShape 'FactorShape)
runtimeProgramShapeForSnapshot runtimeCase snapshot =
  case caCarrier (deAddr snapshot) of
    QueryCarrier _queryId (QueryFactor node) ->
      case lookupFactorShapeManifestNode node (factorProgramFactorShapeManifest (rpcRuntimeProgram runtimeCase)) of
        Nothing ->
          Left (RuntimeProgramShapeFailed (FactorShapeMissingManifestNode node))
        Just manifestNode ->
          first RuntimeProgramShapeFailed $
            factorShapeFromManifestBoundary
              (factorProgramCanonical (rpcRuntimeProgram runtimeCase))
              manifestNode
              (deBoundary snapshot)
    QueryCarrier _queryId (QueryAtom atomId) ->
      Left (RuntimeProgramShapeFailed (FactorShapeMissingAtom (atomIdKey atomId)))
    DerivedCarrier {} ->
      Left (RuntimeProgramShapeFailed (FactorShapeMissingManifestNode FactorNodeRoot))
{-# INLINE runtimeProgramShapeForSnapshot #-}

visibleReferenceSnapshots ::
  RuntimeProgramCase ctx prop evidence ->
  [RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence]
visibleReferenceSnapshots =
  filter visibleSnapshot . rpcCarrierSnapshots
{-# INLINE visibleReferenceSnapshots #-}

visibleSnapshot ::
  RelationalCarrierDelta ctx Carrier prop boundary evidence ->
  Bool
visibleSnapshot snapshot =
  case caCarrier (deAddr snapshot) of
    QueryCarrier _queryId (QueryFactor node) ->
      factorNodeCarrierVisible node
    QueryCarrier {} ->
      False
    DerivedCarrier {} ->
      False
{-# INLINE visibleSnapshot #-}

registrationEntryFromSnapshot ::
  RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence ->
  PlanReuseRegistrationEntry ctx prop
registrationEntryFromSnapshot snapshot =
  PlanReuseRegistrationEntry
    { prreAddr = deAddr snapshot,
      prreTime = deTime snapshot,
      prreBoundary = deBoundary snapshot,
      prreScope = deScope snapshot
    }
{-# INLINE registrationEntryFromSnapshot #-}

atomSnapshotEntry ::
  QueryId ->
  RelationalCarrierTime ctx ->
  ctx ->
  prop ->
  evidence ->
  (AtomId, [SlotId], [RowTupleKey]) ->
  Either
    RuntimeProgramCaseError
    (RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence)
atomSnapshotEntry queryIdValue eventTime contextValue propValue evidenceValue (atomId, schemaValue, rowsValue) =
  Right
    RelationalCarrierDelta
      { deAddr =
          carrierAddr contextValue (PropositionKey propValue) (queryAtomCarrier queryIdValue atomId),
        deTime = eventTime,
        deSupport = principalSupport contextValue,
        deBoundary =
          boundaryForSchema schemaValue,
        deEvidence = evidenceValue,
        deRows =
          coveredRowsFromAtomRows rowsValue,
        deOrigin =
          RelationalOrigin
            { roEvent = OriginAtom queryIdValue atomId,
              roRoute = emptyDerivationRoute
            },
        deScope = mempty,
        dePayload = ()
      }
{-# INLINE atomSnapshotEntry #-}

atomInputSignatureReadout ::
  (AtomId, [SlotId], [RowTupleKey]) ->
  (Int, (RuntimeBoundary, RowDelta))
atomInputSignatureReadout (atomId, schemaValue, rowsValue) =
  ( atomIdKey atomId,
    (boundaryForSchema schemaValue, coveredRowsFromAtomRows rowsValue)
  )
{-# INLINE atomInputSignatureReadout #-}

coveredRowsFromAtomRows :: [RowTupleKey] -> RowDelta
coveredRowsFromAtomRows rowsValue =
  plainRowPatchFromList
    (fmap (\rowValue -> (rowValue, MultiplicityChange 1)) rowsValue)
{-# INLINE coveredRowsFromAtomRows #-}

snapshotEntry ::
  FactorProgram ->
  QueryId ->
  RelationalCarrierTime ctx ->
  ctx ->
  prop ->
  evidence ->
  (FactorNode, FactorEntry) ->
  Either
    RuntimeProgramCaseError
    (RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence)
snapshotEntry runtimeProgram queryIdValue eventTime contextValue propValue evidenceValue (node, entry) = do
  nodeManifest <-
    case lookupFactorShapeManifestNode node (factorProgramFactorShapeManifest runtimeProgram) of
      Nothing ->
        Left (RuntimeProgramShapeFailed (FactorShapeMissingManifestNode node))
      Just manifestValue ->
        Right manifestValue
  boundary <-
    first (RuntimeProgramBoundaryFailed node) $
      mkRuntimeBoundary
        (fsnmOutputSchema nodeManifest)
        IntSet.empty
        IntMap.empty
  pure
    RelationalCarrierDelta
      { deAddr =
          carrierAddr contextValue (PropositionKey propValue) (queryFactorCarrier queryIdValue node),
        deTime = eventTime,
        deSupport = principalSupport contextValue,
        deBoundary = boundary,
        deEvidence = evidenceValue,
        deRows = (factorSnapshotRows (feFactor entry)),
        deOrigin =
          RelationalOrigin
            { roEvent = OriginFactor queryIdValue node,
              roRoute = emptyDerivationRoute
            },
        deScope = mempty {rsDeps = DepsDelta IntSet.empty, rsTopo = TopoDelta IntSet.empty},
        dePayload = ()
      }
{-# INLINE snapshotEntry #-}

factorSnapshotRows :: Factor -> RowDelta
factorSnapshotRows factorValue =
  plainRowPatchFromList
    [ (tupleKeyFromRepKeys (tupleKeyToRepKeys assignmentKey), MultiplicityChange 1)
    | assignmentKey <- Map.keys (indexedRowsPayloadMap factorValue)
    ]
{-# INLINE factorSnapshotRows #-}

triangleProgramWith :: RuntimeTriangleOptions -> RelProgram
triangleProgramWith options =
  programWithQueryId
    (rtoName options)
    (rtoQueryId options)
    (rtoSlotKeyOffset options)
    (fmap atomForLogicalKey (orderedLogicalAtoms (rtoAtomOrder options)))
    Nothing
  where
    atomForLogicalKey logicalKey =
      atom
        (rtoAtomKeyOffset options + logicalKey)
        (fmap (rtoSlotKeyOffset options +) (logicalColumns logicalKey))
        (logicalRows logicalKey)
{-# INLINE triangleProgramWith #-}

unaryProgramWith :: RuntimeTriangleOptions -> RelProgram
unaryProgramWith options =
  programWithQueryId
    (rtoName options)
    (rtoQueryId options)
    (rtoSlotKeyOffset options)
    [ atom
        (rtoAtomKeyOffset options)
        [rtoSlotKeyOffset options]
        [[1], [2]]
    ]
    Nothing
{-# INLINE unaryProgramWith #-}

orderedLogicalAtoms :: TriangleAtomOrder -> [Int]
orderedLogicalAtoms orderValue =
  case orderValue of
    TriangleAtomsForward ->
      [0, 1, 2]
    TriangleAtomsReversed ->
      [2, 1, 0]
{-# INLINE orderedLogicalAtoms #-}

logicalColumns :: Int -> [Int]
logicalColumns logicalKey =
  case logicalKey of
    0 ->
      [0, 1]
    1 ->
      [1, 2]
    _ ->
      [0, 2]
{-# INLINE logicalColumns #-}

logicalRows :: Int -> [[Int]]
logicalRows logicalKey =
  case logicalKey of
    0 ->
      [[1, 2], [1, 3]]
    1 ->
      [[2, 4], [3, 5]]
    _ ->
      [[1, 4]]
{-# INLINE logicalRows #-}

carrierTime :: Ctx -> Word64 -> RelationalCarrierTime Ctx
carrierTime contextValue stamp =
  mkRelationalCarrierTime
    contextValue
    initialQuotientEpoch
    initialLiveEpoch
    PhaseProject
    (frontierStamp (fromIntegral stamp))
{-# INLINE carrierTime #-}

factorSpec :: FactorCarrierEmitSpec Ctx Prop RuntimeBoundary Evidence
factorSpec =
  factorCarrierEmitSpec
    CarrierAddressBook
      { cabContextOfQuery = const 0,
        cabPropOfQuery = const (PropositionKey 0)
      }
    (\_queryId _payload -> principalSupport 0)
    (\_queryId _carrier schema -> boundaryForSchema schema)
    (\_queryId _payload -> ())
{-# INLINE factorSpec #-}

runtimeFromProgramCases ::
  [RuntimeProgramCase Ctx Prop Evidence] ->
  PlanReuseState Ctx Prop ->
  ReuseMode ->
  Either (RuntimeCreateError Ctx Prop) TestRuntime
runtimeFromProgramCases runtimeCases planReuse demandPolicy =
  case createRelDiffRuntimeWithBackend (runtimeProgramBackend demandPolicy) (runtimeProgramCasesSpec runtimeCases) defaultRuntimeCreateOptions of
    Left err ->
      Left err
    Right runtime ->
      Right
        ( unsafeSetRuntimePlanReuse planReuse
            (unsafeSetRuntimeFactorPrograms (runtimeProgramCasesFactorPrograms runtimeCases) runtime)
        )
{-# INLINE runtimeFromProgramCases #-}

runtimeProgramBackend :: ReuseMode -> RuntimeBackend Ctx Prop Evidence () Void
runtimeProgramBackend demandPolicy =
  defaultBackend {rbReuseMode = demandPolicy}
{-# INLINE runtimeProgramBackend #-}

runtimeProgramCasesSpec :: [RuntimeProgramCase Ctx Prop Evidence] -> RuntimeSpec Ctx Prop
runtimeProgramCasesSpec runtimeCases =
  RuntimeSpec
    { rsSchema =
        runtimeSchema
          [ ( 0,
              runtimeContextSchema
                (foldMap (programRuntimeAtoms . rpcRelProgram) runtimeCases)
                [PropositionKey 0]
            )
          ],
      rsPlans = fmap runtimeCasePlan runtimeCases,
      rsInitialData = emptyRuntimeInitialData
    }
{-# INLINE runtimeProgramCasesSpec #-}

runtimeCasePlan :: RuntimeProgramCase Ctx Prop Evidence -> RuntimePlan Ctx Prop
runtimeCasePlan runtimeCase =
  RuntimePlan
    { rpContext = 0,
      rpProp = PropositionKey 0,
      rpProjection =
        runtimePlanProjectionFromSlots
          runtimeCaseSchema
          runtimeCaseSchema,
      rpProgram = factorSpecFromProgram (rpcRuntimeProgram runtimeCase)
    }
  where
    runtimeCaseSchema =
      orderedSlotNub
        [ slot
        | (_atomId, columns, _rows) <- programRuntimeAtomInputs (rpcRelProgram runtimeCase),
          slot <- columns
        ]
{-# INLINE runtimeCasePlan #-}

factorSpecFromProgram :: FactorProgram -> FactorProgramSpec
factorSpecFromProgram =
  factorProgramSpec
{-# INLINE factorSpecFromProgram #-}

runtimeProgramCasesFactorPrograms :: [RuntimeProgramCase ctx prop evidence] -> Map RepairProgramKey FactorProgram
runtimeProgramCasesFactorPrograms =
  Map.fromList . fmap (\runtimeCase -> (factorProgramRepairKey (rpcRuntimeProgram runtimeCase), rpcRuntimeProgram runtimeCase))
{-# INLINE runtimeProgramCasesFactorPrograms #-}

insertSnapshots ::
  [RelationalCarrierDelta Ctx Carrier Prop RuntimeBoundary Evidence] ->
  TestRuntime ->
  Either (RelationalRuntimeError Ctx Prop RuntimeBoundary Evidence) TestRuntime
insertSnapshots deltas runtime =
  fst <$> commitCarrierDeltas deltas runtime
{-# INLINE insertSnapshots #-}

insertAtomSnapshots ::
  [RuntimeProgramCase Ctx Prop Evidence] ->
  TestRuntime ->
  Either (RelationalRuntimeError Ctx Prop RuntimeBoundary Evidence) TestRuntime
insertAtomSnapshots runtimeCases runtime =
  fst <$> commitCarrierDeltas (foldMap rpcAtomSnapshots runtimeCases) runtime
{-# INLINE insertAtomSnapshots #-}

currentSnapshot ::
  CarrierAddr Ctx Carrier Prop ->
  TestRuntime ->
  Either
    (RelationalRuntimeError Ctx Prop RuntimeBoundary Evidence)
    (Maybe (RelationalCarrierDelta Ctx Carrier Prop RuntimeBoundary Evidence))
currentSnapshot =
  currentCarrierMaybe
{-# INLINE currentSnapshot #-}

programCacheEntryCount :: QueryId -> TestRuntime -> Int
programCacheEntryCount queryId runtime =
  case runtimeFactorProgramMaybe queryId runtime of
    Nothing ->
      0
    Just runtimeProgram ->
      Map.size (fcsNodes (fpCacheState runtimeProgram))
{-# INLINE programCacheEntryCount #-}

programCacheEmpty :: QueryId -> TestRuntime -> Bool
programCacheEmpty queryId runtime =
  programCacheEntryCount queryId runtime == 0
{-# INLINE programCacheEmpty #-}

runtimeFactorProgramMaybe ::
  QueryId ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Maybe FactorProgram
runtimeFactorProgramMaybe =
  lookupFactorProgram
{-# INLINE runtimeFactorProgramMaybe #-}

unsafeSetRuntimeFactorPrograms ::
  Map RepairProgramKey FactorProgram ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
unsafeSetRuntimeFactorPrograms programs runtime =
  runtime
    { rdrState =
        Core.mapRuntimeFactorSection
          ( \factorState ->
              factorState {rfsPrograms = programs}
          )
          (rdrState runtime)
    }
{-# INLINE unsafeSetRuntimeFactorPrograms #-}

unsafeSetRuntimePlanReuse ::
  (Ord ctx, Ord prop) =>
  PlanReuseState ctx prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
unsafeSetRuntimePlanReuse planReuse runtime =
  runtime
    { rdrState =
        Core.mapRuntimeTopologySection
          (updateRuntimePlanReuse planReuse)
          state0
    }
  where
    state0 =
      rdrState runtime
{-# INLINE unsafeSetRuntimePlanReuse #-}

rowsOf ::
  RelationalCarrierDelta ctx carrier prop boundary evidence ->
  RowDelta
rowsOf =
  deRows
{-# INLINE rowsOf #-}

extraSingletonRowsLike ::
  RelationalCarrierDelta ctx carrier prop RuntimeBoundary evidence ->
  RowDelta
extraSingletonRowsLike snapshot =
  plainRowPatchFromList [(extraRow, MultiplicityChange 1)]
  where
    width =
      length (bsSchema (boundaryShape (deBoundary snapshot)))

    extraRow =
      tupleKeyFromRepKeys [RepKey (1000000 + ordinal) | ordinal <- [0 .. width - 1]]
{-# INLINE extraSingletonRowsLike #-}

boundaryForSchema :: [SlotId] -> RuntimeBoundary
boundaryForSchema schema =
  mkBoundary
    runtimeBoundaryDigest
    BoundaryShape
      { bsSchema = schema,
        bsSensitive = Set.empty,
        bsSlotKeys = Map.empty
      }
{-# INLINE boundaryForSchema #-}
