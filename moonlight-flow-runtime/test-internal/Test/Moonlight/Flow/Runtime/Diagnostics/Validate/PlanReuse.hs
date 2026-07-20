{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
module Test.Moonlight.Flow.Runtime.Diagnostics.Validate.PlanReuse
  ( PlanReuseSemanticValidationMode (..),
    PlanReuseSemanticValidationError (..),
    validatePlanReuseSemantics,
    validatePlanReuseInvalidationSemantics,
  )
where
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( QueryId,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
    caProp,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Delta.Emit
  ( CarrierEmitSpec (..),
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
import Moonlight.Flow.Carrier.Morphism.Engine
  ( CarrierReuseOps (..),
    checkedReuseSupportProject,
    runCarrierReuseMorphism,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    mkRelationalCarrierTime,
  )
import Moonlight.Flow.Execution.Factor.Run
  ( runFactor,
  )
import Moonlight.Flow.Execution.Factor.Types
  ( FactorDemand (..),
    FactorRunResult (..),
    FactorRunSpec (..),
    emptyFactorCache,
  )
import Moonlight.Flow.Execution.Observe.Provenance.GC
  ( defaultProvGCConfig,
  )
import Moonlight.Flow.Execution.Observe.Telemetry
  ( defaultRepairTelemetryConfig,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Delta.Signed
  ( multiplicityValue,
    zeroMultiplicity
  )
import Moonlight.Flow.Model.Scope
import Moonlight.Differential.Row.Patch
  ( emptyPlainRowPatch,
    plainRowPatchChangeMap,
    positivePlainRowPatchRows,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (PhaseSubsumption),
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Flow.Plan.Query.Core
  ( FactorNode (..),
  )
import Moonlight.Flow.Runtime.Factor.Input
  ( FactorInputFrame (..),
    factorInputFrameRuntime,
  )
import Moonlight.Flow.Runtime.Carrier.Emit
  ( FactorCarrierEmitSpec,
    FactorCarrierPayload (..),
    factorSnapshotDeltas,
  )
import Moonlight.Flow.Runtime.Factor.Request
  ( FactorFullRepairReason (FullRepairManual),
    FactorRepairRequest (..),
    fullRepair,
  )
import Moonlight.Flow.Runtime.Factor.Program
  ( FactorProgram (..),
    factorProgramDecompPlan,
    factorProgramQueryId,
    factorProgramRepairKey,
  )
import Moonlight.Flow.Runtime.Spec.FactorProgram
  ( RepairProgramKey,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
    RuntimeEnv (..),
    rsPlanReuse,
  )
import Moonlight.Flow.Runtime.Core.State
  ( rsLiveEpoch,
    rsNextFrontierStamp,
    rsQuotientEpoch,
  )
import Moonlight.Flow.Runtime.Factor.State
  ( runtimeFactorPrograms,
  )
import Moonlight.Flow.Runtime.Carrier.Store
  ( currentCarrierMaybe,
  )
import Moonlight.Flow.Carrier.Reuse
  ( PlanReuseInvalidationPostconditionError,
    PlanReuseInvariantError (..),
    SubsumptionIndexInvariantError,
    planReuseCarrierReuses,
    validatePlanReuseState,
    validatePlanReuseInvalidationPostcondition,
  )
import Test.Moonlight.Flow.Runtime.Diagnostics.Validate
  ( RuntimeTraceValidationError,
    validateRuntimeTrace,
  )
data PlanReuseSemanticValidationMode
  = ValidatePlanReuseOff
  | ValidatePlanReuseSampled !Int
  | ValidatePlanReuseAll
  deriving stock (Eq, Ord, Show, Read)
data PlanReuseSemanticValidationError ctx prop boundary evidence
  = PlanReuseRuntimeTraceInvalid !(RuntimeTraceValidationError ctx prop boundary evidence)
  | PlanReuseStateInvalid !(PlanReuseInvariantError ctx prop)
  | PlanReuseSubsumptionOwnershipInvalid !(SubsumptionIndexInvariantError ctx prop)
  | PlanReuseInvalidationPostconditionInvalid !(PlanReuseInvalidationPostconditionError ctx prop)
  | PlanReuseExactRecomputeFailed !QueryId
  | PlanReuseExactCurrentMissing !(CarrierAddr ctx Carrier prop)
  | PlanReuseExactRowsMismatch !(CarrierAddr ctx Carrier prop) !RowDelta !RowDelta
  | PlanReuseUnexpectedExactByCoverReuse !(CarrierReuseId ctx prop)
  | PlanReuseLowerBoundCurrentMissing !(CarrierReuseId ctx prop) !(CarrierAddr ctx Carrier prop)
  | PlanReuseLowerBoundExactTargetMissing !(CarrierReuseId ctx prop) !(CarrierAddr ctx Carrier prop)
  | PlanReuseLowerBoundNotSubset !(CarrierReuseId ctx prop) !(CarrierAddr ctx Carrier prop) !RowDelta !RowDelta
  | PlanReuseReplaySourceMissingTargetStillCurrent !(CarrierReuseId ctx prop) !(CarrierAddr ctx Carrier prop)
  | PlanReuseReplayProjectionFailed !(CarrierReuseId ctx prop) !(CarrierReuseError ctx prop evidence)
  | PlanReuseReplayTargetMissing !(CarrierReuseId ctx prop) !(CarrierAddr ctx Carrier prop)
  | PlanReuseReplayTargetMismatch !(CarrierReuseId ctx prop) !(CarrierAddr ctx Carrier prop) !RowDelta !RowDelta
  deriving stock (Eq, Show)
validatePlanReuseSemantics ::
  ( Ord ctx,
    Ord prop,
    Eq evidence
  ) =>
  PlanReuseSemanticValidationMode ->
  FactorCarrierEmitSpec ctx prop RuntimeBoundary evidence ->
  RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr ->
  Either [PlanReuseSemanticValidationError ctx prop RuntimeBoundary evidence] ()
validatePlanReuseSemantics mode spec runtime =
  finish $
    baseValidationErrors runtime
      <> case mode of
        ValidatePlanReuseOff ->
          []
        ValidatePlanReuseSampled sampleCount ->
          semanticValidationErrors (Sampled sampleCount) spec runtime
        ValidatePlanReuseAll ->
          semanticValidationErrors AllCandidates spec runtime
{-# INLINE validatePlanReuseSemantics #-}
validatePlanReuseInvalidationSemantics ::
  ( Ord ctx,
    Ord prop
  ) =>
  RelationalScope ->
  RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr ->
  Either [PlanReuseSemanticValidationError ctx prop RuntimeBoundary evidence] ()
validatePlanReuseInvalidationSemantics dirty runtime =
  finish $
    fmap PlanReuseInvalidationPostconditionInvalid invalidationErrors
  where
    invalidationErrors =
      case validatePlanReuseInvalidationPostcondition (scopeDeps dirty) (scopeTopo dirty) (rsPlanReuse (rdrState runtime)) of
        Right () ->
          []
        Left errors ->
          errors
{-# INLINE validatePlanReuseInvalidationSemantics #-}
data CandidateSelection
  = Sampled !Int
  | AllCandidates
  deriving stock (Eq, Ord, Show)
baseValidationErrors ::
  ( Ord ctx,
    Ord prop,
    Eq evidence
  ) =>
  RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr ->
  [PlanReuseSemanticValidationError ctx prop RuntimeBoundary evidence]
baseValidationErrors runtime =
  runtimeTraceErrors <> planReuseErrors
  where
    runtimeTraceErrors =
      case validateRuntimeTrace runtime of
        Right () ->
          []
        Left traceError ->
          [PlanReuseRuntimeTraceInvalid traceError]
    planReuseErrors =
      case validatePlanReuseState (rsPlanReuse (rdrState runtime)) of
        Right () ->
          []
        Left errors ->
          fmap liftPlanReuseInvariant errors
{-# INLINE baseValidationErrors #-}

liftPlanReuseInvariant ::
  PlanReuseInvariantError ctx prop ->
  PlanReuseSemanticValidationError ctx prop boundary evidence
liftPlanReuseInvariant err =
  case err of
    PlanReuseReuseRegistryInvariant {} ->
      PlanReuseStateInvalid err
    PlanReuseSubsumptionInvariant subsumptionError ->
      PlanReuseSubsumptionOwnershipInvalid subsumptionError
    PlanReuseMaterializationInvariant {} ->
      PlanReuseStateInvalid err
{-# INLINE liftPlanReuseInvariant #-}
semanticValidationErrors ::
  (Ord ctx, Ord prop) =>
  CandidateSelection ->
  FactorCarrierEmitSpec ctx prop RuntimeBoundary evidence ->
  RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr ->
  [PlanReuseSemanticValidationError ctx prop RuntimeBoundary evidence]
semanticValidationErrors selection spec runtime =
  case recomputedFactorSnapshots spec runtime of
    Left queryId ->
      [PlanReuseExactRecomputeFailed queryId]
    Right exactByAddr ->
      exactCarrierErrors selection exactByAddr runtime
        <> reuseSemanticErrors selection exactByAddr runtime
{-# INLINE semanticValidationErrors #-}
recomputedFactorSnapshots ::
  (Ord ctx, Ord prop) =>
  FactorCarrierEmitSpec ctx prop RuntimeBoundary evidence ->
  RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr ->
  Either QueryId (Map (CarrierAddr ctx Carrier prop) (RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence))
recomputedFactorSnapshots spec runtime =
  Map.foldlWithKey'
    recomputeOne
    (Right Map.empty)
    (runtimeFactorPrograms runtime)
  where
    recomputeOne eitherAcc _repairKey program = do
      acc <- eitherAcc
      let queryId =
            factorProgramQueryId program
      snapshots <- recomputeProgramSnapshots spec runtime queryId program
      pure (Map.union (Map.fromList [(deAddr snapshot, snapshot) | snapshot <- snapshots]) acc)
{-# INLINE recomputedFactorSnapshots #-}
recomputeProgramSnapshots ::
  (Ord ctx, Ord prop) =>
  FactorCarrierEmitSpec ctx prop RuntimeBoundary evidence ->
  RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr ->
  QueryId ->
  FactorProgram ->
  Either QueryId [RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence]
recomputeProgramSnapshots spec runtime queryId program = do
  factorInput <-
    case
      factorInputFrameRuntime
        (reAtomCarrierEmitSpec (rdrEnv runtime))
        queryId
        (validationRepairRequest spec (factorProgramRepairKey program) queryId)
        program
        runtime
    of
      Left _runtimeError ->
        Left queryId
      Right arrangedInput ->
        Right (fifInput arrangedInput)
  case
    runFactor
      FactorRunSpec
        { frsDecomp = factorProgramDecompPlan program,
          frsInput = factorInput,
          frsCache = emptyFactorCache,
          frsGc = defaultProvGCConfig,
            frsRepairTelemetry = defaultRepairTelemetryConfig,
          frsDemand = FactorDemandMaintenance
        }
    of
      Left _obstruction ->
        Left queryId
      Right result ->
        Right $
          fmap forgetCarrierPayload $
            factorSnapshotDeltas
              spec
              (validationEventTime spec runtime queryId)
              queryId
              mempty
              (frrCache result)
{-# INLINE recomputeProgramSnapshots #-}

validationRepairRequest ::
  FactorCarrierEmitSpec ctx prop RuntimeBoundary evidence ->
  RepairProgramKey ->
  QueryId ->
  FactorRepairRequest ctx prop
validationRepairRequest spec repairKey queryId =
  let rootPayload =
        FactorCarrierPayload
          { fcpRelationalScope = mempty,
            fcpNode = FactorNodeRoot,
            fcpSchema = [],
            fcpRows = emptyPlainRowPatch
          }
      rootAddr =
        cesAddrOf spec (queryId, rootPayload)
   in FactorRepairRequest
        { frrContext = caContext rootAddr,
          frrProp = caProp rootAddr,
          frrRepairKey = repairKey,
          frrQueryId = queryId,
          frrCause = fullRepair FullRepairManual
        }
{-# INLINE validationRepairRequest #-}

validationEventTime ::
  FactorCarrierEmitSpec ctx prop RuntimeBoundary evidence ->
  RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr ->
  QueryId ->
  RelationalCarrierTime ctx
validationEventTime spec runtime queryId =
  let rootPayload =
        FactorCarrierPayload
          { fcpRelationalScope = mempty,
            fcpNode = FactorNodeRoot,
            fcpSchema = [],
            fcpRows = emptyPlainRowPatch
          }
      rootAddr =
        cesAddrOf spec (queryId, rootPayload)
   in mkRelationalCarrierTime
        (caContext rootAddr)
        (rsQuotientEpoch (rdrState runtime))
        (rsLiveEpoch (rdrState runtime))
        PhaseSubsumption
        (rsNextFrontierStamp (rdrState runtime))
{-# INLINE validationEventTime #-}
exactCarrierErrors ::
  (Ord ctx, Ord prop) =>
  CandidateSelection ->
  Map (CarrierAddr ctx Carrier prop) (RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence) ->
  RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr ->
  [PlanReuseSemanticValidationError ctx prop RuntimeBoundary evidence]
exactCarrierErrors selection exactByAddr runtime =
  concatMap validateExact selected
  where
    selected =
      selectCandidates selection (Map.toAscList exactByAddr)
    validateExact (addr, expectedSnapshot) =
      case currentCarrierMaybe addr runtime of
        Left _runtimeError ->
          [PlanReuseExactCurrentMissing addr]
        Right Nothing ->
          []
        Right (Just actualSnapshot)
          | rowsOf actualSnapshot /= rowsOf expectedSnapshot ->
              [PlanReuseExactRowsMismatch addr (rowsOf actualSnapshot) (rowsOf expectedSnapshot)]
          | otherwise ->
              []
{-# INLINE exactCarrierErrors #-}
reuseSemanticErrors ::
  (Ord ctx, Ord prop) =>
  CandidateSelection ->
  Map (CarrierAddr ctx Carrier prop) (RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence) ->
  RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr ->
  [PlanReuseSemanticValidationError ctx prop RuntimeBoundary evidence]
reuseSemanticErrors selection exactByAddr runtime =
  concatMap
    (validateReuse exactByAddr runtime)
    (selectCandidates selection (planReuseCarrierReuses (rsPlanReuse (rdrState runtime))))
{-# INLINE reuseSemanticErrors #-}
validateReuse ::
  (Ord ctx, Ord prop) =>
  Map (CarrierAddr ctx Carrier prop) (RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence) ->
  RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr ->
  (CarrierReuseId ctx prop, CarrierReuse ctx prop) ->
  [PlanReuseSemanticValidationError ctx prop RuntimeBoundary evidence]
validateReuse exactByAddr runtime (reuseId, reuse) =
  case cruCoverageRule reuse of
    DowngradeToLowerBound ->
      validateLowerBoundReuse reuseId reuse exactByAddr runtime
    PreserveExact ->
      validateExactReplayReuse reuseId reuse exactByAddr runtime
    ExactByCover ->
      [PlanReuseUnexpectedExactByCoverReuse reuseId]
    ObstructProjection _tokens ->
      []
{-# INLINE validateReuse #-}
validateLowerBoundReuse ::
  (Ord ctx, Ord prop) =>
  CarrierReuseId ctx prop ->
  CarrierReuse ctx prop ->
  Map (CarrierAddr ctx Carrier prop) (RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence) ->
  RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr ->
  [PlanReuseSemanticValidationError ctx prop RuntimeBoundary evidence]
validateLowerBoundReuse reuseId reuse exactByAddr runtime =
  case carrierReuseExpectedTarget reuse of
    Nothing ->
      []
    Just derivedAddr ->
      case currentCarrierMaybe derivedAddr runtime of
        Left _runtimeError ->
          [PlanReuseLowerBoundCurrentMissing reuseId derivedAddr]
        Right Nothing ->
          []
        Right (Just lowerBoundSnapshot) ->
          case Map.lookup (rwTargetCarrier (cruWitness reuse)) exactByAddr of
            Nothing ->
              [PlanReuseLowerBoundExactTargetMissing reuseId (rwTargetCarrier (cruWitness reuse))]
            Just exactSnapshot
              | rowDeltaSubsetOf (rowsOf lowerBoundSnapshot) (rowsOf exactSnapshot) ->
                  []
              | otherwise ->
                  [PlanReuseLowerBoundNotSubset reuseId derivedAddr (rowsOf lowerBoundSnapshot) (rowsOf exactSnapshot)]
{-# INLINE validateLowerBoundReuse #-}
validateExactReplayReuse ::
  (Ord ctx, Ord prop) =>
  CarrierReuseId ctx prop ->
  CarrierReuse ctx prop ->
  Map (CarrierAddr ctx Carrier prop) (RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence) ->
  RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr ->
  [PlanReuseSemanticValidationError ctx prop RuntimeBoundary evidence]
validateExactReplayReuse reuseId reuse _exactByAddr runtime =
  case carrierReuseExpectedTarget reuse of
    Nothing ->
      []
    Just targetAddr ->
      case currentCarrierMaybe (rwSourceCarrier (cruWitness reuse)) runtime of
        Left _runtimeError ->
          staleSourceErrors targetAddr
        Right Nothing ->
          staleSourceErrors targetAddr
        Right (Just sourceSnapshot) ->
          replaySource targetAddr sourceSnapshot
  where
    staleSourceErrors targetAddr =
      case currentCarrierMaybe targetAddr runtime of
        Right (Just targetSnapshot)
          | not (rowDeltaNullish (rowsOf targetSnapshot)) ->
              [PlanReuseReplaySourceMissingTargetStillCurrent reuseId targetAddr]
        _ ->
          []
    replaySource targetAddr sourceSnapshot =
      case runCarrierReuseMorphism (semanticCarrierReuseOps sourceSnapshot) reuse sourceSnapshot of
        Left projectionError ->
          [PlanReuseReplayProjectionFailed reuseId projectionError]
        Right replayedSnapshot ->
          case currentCarrierMaybe targetAddr runtime of
            Left _runtimeError ->
              [PlanReuseReplayTargetMissing reuseId targetAddr]
            Right Nothing ->
              [PlanReuseReplayTargetMissing reuseId targetAddr]
            Right (Just actualTarget)
              | rowsOf replayedSnapshot == rowsOf actualTarget ->
                  []
              | otherwise ->
                  [PlanReuseReplayTargetMismatch reuseId targetAddr (rowsOf actualTarget) (rowsOf replayedSnapshot)]
    semanticCarrierReuseOps ::
      RelationalCarrierDelta ctx carrier prop boundary evidence ->
      CarrierReuseOps ctx reuseProp reuseEvidence
    semanticCarrierReuseOps sourceSnapshot =
      CarrierReuseOps
        { croEventTime = deTime sourceSnapshot,
          croEvidenceOf = \_witness _rule _boundary evidence -> Right evidence,
          croSupportProject = checkedReuseSupportProject
        }
{-# INLINE validateExactReplayReuse #-}
rowsOf ::
  RelationalCarrierDelta ctx Carrier prop boundary evidence ->
  RowDelta
rowsOf =
  deRows
{-# INLINE rowsOf #-}
rowDeltaSubsetOf ::
  RowDelta ->
  RowDelta ->
  Bool
rowDeltaSubsetOf projected exact =
  Map.foldlWithKey'
    ( \acc row projectedMultiplicity ->
        acc
          && multiplicityValue projectedMultiplicity > 0
          && multiplicityValue projectedMultiplicity
            <= multiplicityValue (Map.findWithDefault zeroMultiplicity row exactRows)
    )
    True
    projectedRows
  where
    projectedRows =
      positivePlainRowPatchRows projected
    exactRows =
      positivePlainRowPatchRows exact
{-# INLINE rowDeltaSubsetOf #-}
rowDeltaNullish ::
  RowDelta ->
  Bool
rowDeltaNullish =
  Map.null . plainRowPatchChangeMap
{-# INLINE rowDeltaNullish #-}
selectCandidates ::
  CandidateSelection ->
  [(key, value)] ->
  [(key, value)]
selectCandidates selection candidates =
  case selection of
    AllCandidates ->
      candidates
    Sampled sampleCount ->
      take (max 0 sampleCount) candidates
{-# INLINE selectCandidates #-}
forgetCarrierPayload ::
  RelationalCarrierDeltaP ctx carrier prop boundary evidence payload ->
  RelationalCarrierDelta ctx carrier prop boundary evidence
forgetCarrierPayload delta =
  delta {dePayload = ()}
{-# INLINE forgetCarrierPayload #-}
finish ::
  [error] ->
  Either [error] ()
finish errors =
  case errors of
    [] ->
      Right ()
    _ ->
      Left errors
{-# INLINE finish #-}
