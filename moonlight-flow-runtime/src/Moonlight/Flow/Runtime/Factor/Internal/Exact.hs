{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Runtime.Factor.Internal.Exact
  ( ExactFactorRepairPrepared (..),
    ExactFactorRepairResult (..),
    prepareExactFactorRepair,
    runPreparedExactFactorRepair,
    runExactFactorRepair,
    exactFactorRepairPreparedShareable,
  )
where

import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( fromMaybe,
  )
import Moonlight.Core
  ( QueryId,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Delta.Emit
  ( CarrierEmitSpec (..),
  )
import Moonlight.Flow.Carrier.Store
  ( carrierReadFrontierFromTime,
    carrierSnapshotRows,
    lookupCarrierSnapshot,
  )
import Moonlight.Flow.Carrier.View.Query
  ( carrierBoundaryLatestTraceNow,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )
import Moonlight.Flow.Execution.Factor.Run
  ( runFactor,
  )
import Moonlight.Flow.Execution.Factor.Types
  ( FactorCache,
    FactorDemand (..),
    FactorRunResult (..),
    FactorRunSpec (..),
  )
import Moonlight.Flow.Execution.Observe.Provenance.GC
  ( defaultProvGCConfig,
  )
import Moonlight.Flow.Execution.Observe.Telemetry
  ( MaintenanceMetrics (..),
    NodeAction (..),
    NodeMaintenance (..),
  )
import Moonlight.Flow.Execution.Subsumption.FactorShape
  ( FactorShapeNodeManifest (..),
    lookupFactorShapeManifestNode,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Differential.Row.Patch
  ( emptyPlainRowPatch,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.Flow.Plan.Query.Core
  ( FactorNode,
  )
import Moonlight.Flow.Runtime.Carrier.Emit
  ( AtomCarrierEmitSpec,
    FactorCarrierEmitSpec,
    FactorCarrierPayload (..),
    factorMaintenanceDeltas,
  )
import Moonlight.Flow.Runtime.Carrier.Emit.Factor
  ( factorNodeCarrierVisible,
  )
import Moonlight.Flow.Runtime.Carrier.Store.Internal
  ( carrierStoreAtRouting,
  )
import Moonlight.Flow.Runtime.Factor.Input
  ( FactorInputFrame (..),
    atomBoundaryDigestsFromReadouts,
    factorInputFrameRuntime,
    factorInputSignatureFromAtomReadouts,
  )
import Moonlight.Flow.Runtime.Factor.Internal.Cache
  ( factorAtomReadsAt,
    factorCacheStateFromCacheAtWithInput,
  )
import Moonlight.Flow.Runtime.Factor.Program
  ( FactorProgram (..),
    factorProgramCanonical,
    factorProgramDecompPlan,
    factorProgramFactorNodes,
    factorProgramFactorShapeManifest,
    factorProgramQueryPlan,
  )
import Moonlight.Flow.Runtime.Factor.Request
  ( FactorRepairCause,
    FactorRepairRequest (..),
    repairCauseIsFull,
    repairCauseRelationalScope,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
    RuntimeEnv (..),
    rsRouting,
  )
import Moonlight.Flow.Runtime.Error
  ( RuntimeError (..),
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
    RelationalRuntimeOpFailure (..),
  )
import Moonlight.Flow.Runtime.Core.RepairStats
  ( RuntimeRepairInputStats,
  )

data ExactFactorRepairPrepared ctx prop boundary evidence joinState joinErr = ExactFactorRepairPrepared
  { efrpRequest :: !(FactorRepairRequest ctx prop),
    efrpProgram :: !FactorProgram,
    efrpInputFrame :: !(FactorInputFrame ctx prop boundary evidence joinState joinErr),
    efrpFactorReadouts :: !(Map FactorNode (RuntimeBoundary, RowDelta))
  }

data ExactFactorRepairResult ctx prop boundary evidence joinState joinErr = ExactFactorRepairResult
  { efrrRuntime :: !(RelDiffRuntime ctx prop boundary evidence joinState joinErr),
    efrrProgram :: !FactorProgram,
    efrrEmittedDeltas :: ![RelationalCarrierDelta ctx Carrier prop boundary evidence],
    efrrRegistrationNodes :: ![FactorNode],
    efrrMaintenanceMetrics :: !MaintenanceMetrics,
    efrrInputStats :: !RuntimeRepairInputStats,
    efrrPreSealCache :: !FactorCache,
    efrrInputSignature :: !StableDigest128
  }

prepareExactFactorRepair ::
  (boundary ~ RuntimeBoundary, Ord ctx, Ord prop) =>
  AtomCarrierEmitSpec ctx prop boundary evidence ->
  FactorCarrierEmitSpec ctx prop boundary evidence ->
  FactorRepairRequest ctx prop ->
  FactorProgram ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (ExactFactorRepairPrepared ctx prop boundary evidence joinState joinErr)
prepareExactFactorRepair atomSpec factorSpec request program runtime0 = do
  inputFrame <-
    factorInputFrameRuntime
      atomSpec
      (frrQueryId request)
      request
      program
      runtime0
  factorReadouts <-
    factorOutputReadoutsRuntime
      factorSpec
      (frrQueryId request)
      program
      runtime0
  pure
    ExactFactorRepairPrepared
      { efrpRequest = request,
        efrpProgram = program,
        efrpInputFrame = inputFrame,
        efrpFactorReadouts = factorReadouts
      }
{-# INLINE prepareExactFactorRepair #-}

runExactFactorRepair ::
  (boundary ~ RuntimeBoundary, Ord ctx, Ord prop) =>
  FactorCarrierEmitSpec ctx prop boundary evidence ->
  RelationalCarrierTime ctx ->
  FactorRepairRequest ctx prop ->
  FactorProgram ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (ExactFactorRepairResult ctx prop boundary evidence joinState joinErr)
runExactFactorRepair spec eventTime request program runtime0 = do
  prepared <-
    prepareExactFactorRepair
      (reAtomCarrierEmitSpec (rdrEnv runtime0))
      (reFactorCarrierEmitSpec (rdrEnv runtime0))
      request
      program
      runtime0
  runPreparedExactFactorRepair spec eventTime prepared
{-# INLINE runExactFactorRepair #-}

runPreparedExactFactorRepair ::
  (boundary ~ RuntimeBoundary) =>
  FactorCarrierEmitSpec ctx prop boundary evidence ->
  RelationalCarrierTime ctx ->
  ExactFactorRepairPrepared ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (ExactFactorRepairResult ctx prop boundary evidence joinState joinErr)
runPreparedExactFactorRepair spec eventTime prepared = do
  result <-
    case
      runFactor
        FactorRunSpec
          { frsDecomp = factorProgramDecompPlan program,
            frsInput = fifInput arrangedInput,
            frsCache = fifCache arrangedInput,
            frsGc = defaultProvGCConfig,
            frsRepairTelemetry = reRepairTelemetry (rdrEnv (fifRuntime arrangedInput)),
            frsDemand = FactorDemandMaintenance
          }
    of
      Left obstruction ->
        Left (RuntimeOpFailure (RelationalRuntimeFactorCarrierRepairFailed queryId obstruction))
      Right factorResult ->
        Right factorResult
  let postCache =
        frrCache result
      preSealCache =
        frrPreSealCache result
      frontier =
        carrierReadFrontierFromTime eventTime
      atomReads =
        factorAtomReadsAt
          frontier
          (atomBoundaryDigestsFromReadouts (fifAtomReadouts arrangedInput))
      programAfterRepair =
        program
          { fpCacheState =
              factorCacheStateFromCacheAtWithInput
                (Just (fifPreparedInput arrangedInput))
                atomReads
                frontier
                postCache
          }
      runtime1 =
        fifRuntime arrangedInput
      metrics =
        frrMetrics result
      emittedRowDeltas =
        fmap
          forgetCarrierPayload
          ( factorMaintenanceDeltas
              spec
              eventTime
              queryId
              dirtyKeys
              metrics
              preSealCache
          )
  pure
    ExactFactorRepairResult
      { efrrRuntime = runtime1,
        efrrProgram = programAfterRepair,
        efrrEmittedDeltas = emittedRowDeltas,
        efrrRegistrationNodes = changedVisibleFactorNodes metrics,
        efrrMaintenanceMetrics = metrics,
        efrrInputStats = fifInputStats arrangedInput,
        efrrPreSealCache = preSealCache,
        efrrInputSignature = factorInputSignatureFromAtomReadouts (fifAtomReadouts arrangedInput)
      }
  where
    request =
      efrpRequest prepared

    program =
      efrpProgram prepared

    arrangedInput =
      efrpInputFrame prepared

    queryId =
      frrQueryId request

    dirtyKeys =
      repairCauseRelationalScope (frrCause request)
{-# INLINE runPreparedExactFactorRepair #-}

factorOutputReadoutsRuntime ::
  (boundary ~ RuntimeBoundary, Ord ctx, Ord prop) =>
  FactorCarrierEmitSpec ctx prop boundary evidence ->
  QueryId ->
  FactorProgram ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (Map FactorNode (RuntimeBoundary, RowDelta))
factorOutputReadoutsRuntime spec queryId program runtime =
  Map.fromList
    <$> traverse
      (factorNodeReadoutRuntime spec queryId program runtime)
    [ node
    | node <- factorProgramFactorNodes program,
      factorNodeCarrierVisible node
    ]
{-# INLINE factorOutputReadoutsRuntime #-}

factorNodeReadoutRuntime ::
  (boundary ~ RuntimeBoundary, Ord ctx, Ord prop) =>
  FactorCarrierEmitSpec ctx prop boundary evidence ->
  QueryId ->
  FactorProgram ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  FactorNode ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (FactorNode, (RuntimeBoundary, RowDelta))
factorNodeReadoutRuntime spec queryId program runtime node =
  case lookupFactorShapeManifestNode node (factorProgramFactorShapeManifest program) of
    Nothing ->
      Right (node, (defaultBoundary [], emptyPlainRowPatch))
    Just nodeManifest -> do
      (_shard, store0) <-
        carrierStoreAtRouting
          (rsRouting (rdrState runtime))
          addr
          runtime
      let !boundaryValue =
            fromMaybe
              (defaultBoundary schema)
              (carrierBoundaryLatestTraceNow addr store0)
          !rows =
            maybe
              emptyPlainRowPatch
              carrierSnapshotRows
              (lookupCarrierSnapshot addr store0)
      pure (node, (boundaryValue, rows))
      where
        schema =
          fsnmOutputSchema nodeManifest

        addr =
          cesAddrOf spec (queryId, emptyPayload schema)
  where
    defaultBoundary schema =
      cesBoundaryOf spec (queryId, emptyPayload schema)

    emptyPayload schema =
      FactorCarrierPayload
        { fcpRelationalScope = mempty,
          fcpNode = node,
          fcpSchema = schema,
          fcpRows = emptyPlainRowPatch
        }
{-# INLINE factorNodeReadoutRuntime #-}

exactFactorRepairPreparedShareable ::
  ExactFactorRepairPrepared ctx prop RuntimeBoundary evidence joinState joinErr ->
  ExactFactorRepairPrepared ctx prop RuntimeBoundary evidence joinState joinErr ->
  Bool
exactFactorRepairPreparedShareable left right =
  repairProgramShapeEqual (efrpProgram left) (efrpProgram right)
    && frrCause (efrpRequest left) == frrCause (efrpRequest right)
    && fifAtomReadouts (efrpInputFrame left) == fifAtomReadouts (efrpInputFrame right)
    && efrpFactorReadouts left == efrpFactorReadouts right
    && exactCacheShareable
      (frrCause (efrpRequest left))
      (efrpProgram left)
      (efrpProgram right)
{-# INLINE exactFactorRepairPreparedShareable #-}

exactCacheShareable ::
  FactorRepairCause ->
  FactorProgram ->
  FactorProgram ->
  Bool
exactCacheShareable cause left right =
  repairCauseIsFull cause || fpCacheState left == fpCacheState right
{-# INLINE exactCacheShareable #-}

repairProgramShapeEqual ::
  FactorProgram ->
  FactorProgram ->
  Bool
repairProgramShapeEqual left right =
  factorProgramQueryPlan left == factorProgramQueryPlan right
    && factorProgramCanonical left == factorProgramCanonical right
    && factorProgramFactorShapeManifest left == factorProgramFactorShapeManifest right
    && factorProgramDecompPlan left == factorProgramDecompPlan right
{-# INLINE repairProgramShapeEqual #-}

changedVisibleFactorNodes :: MaintenanceMetrics -> [FactorNode]
changedVisibleFactorNodes metrics =
  [ node
    | (node, nodeMetrics) <- Map.toAscList (mmNodes metrics),
      factorNodeCarrierVisible node,
      nodeActionRegistersShape (nmAction nodeMetrics)
  ]
{-# INLINE changedVisibleFactorNodes #-}

nodeActionRegistersShape :: NodeAction -> Bool
nodeActionRegistersShape action =
  case action of
    NodeBuilt ->
      True
    NodePatched ->
      True
    NodeReused ->
      False
{-# INLINE nodeActionRegistersShape #-}

forgetCarrierPayload ::
  RelationalCarrierDeltaP ctx carrier prop boundary evidence payload ->
  RelationalCarrierDelta ctx carrier prop boundary evidence
forgetCarrierPayload delta =
  delta
    { dePayload = ()
    }
{-# INLINE forgetCarrierPayload #-}
