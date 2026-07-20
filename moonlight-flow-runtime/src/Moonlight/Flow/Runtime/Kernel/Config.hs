{-# LANGUAGE DerivingStrategies #-}
module Moonlight.Flow.Runtime.Kernel.Config
  ( RuntimeConfig (..),
    RelDiffRuntimeConfig,
    RuntimeReplayValidation (..),
    RuntimeConfigError (..),
    mkRelDiffRuntimeConfig,
    mkRelDiffRuntimeConfigWithReplayValidation,
    mkRelDiffRuntime,
    emptyRelDiffRuntime,
  )
where

import Control.Monad
  ( unless,
  )
import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( LiveEpoch,
    QueryId,
    QuotientEpoch,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Flow.Carrier.Core.Frontier
  ( RelDiffFrontier,
  )
import Moonlight.Flow.Carrier.Store
  ( CarrierStore,
    CarrierStoreError,
    validateCarrierStore,
  )
import Moonlight.Flow.Carrier.Core.Summary
  ( CarrierBatchSummaryOps,
    CarrierStoreSummaryEntry,
  )
import Moonlight.Flow.Carrier.Engine.Project
  ( CarrierProjectState,
  )
import Moonlight.Flow.Carrier.Morphism.Core.Program
  ( CarrierMorphismRuntime,
  )
import Moonlight.Flow.Carrier.View.Section
  ( RelationalSection,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase,
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )
import Moonlight.Flow.Execution.Observe.Telemetry
  ( RepairTelemetryConfig,
  )
import Moonlight.Flow.Runtime.Factor.Program
  ( FactorProgram,
    factorProgramQueryId,
    validateFactorProgram,
  )
import Moonlight.Flow.Runtime.Spec.FactorProgram
  ( FactorProgramError,
    RepairProgramKey,
  )
import Moonlight.Flow.Runtime.Carrier.Emit
  ( AtomCarrierEmitSpec,
  )
import Moonlight.Flow.Runtime.Carrier.Emit
  ( FactorCarrierEmitSpec,
  )
import Moonlight.Flow.Runtime.Core.Patch.Validation
  ( CanonicalityOracle,
  )
import Moonlight.Flow.Runtime.Core.Replay.Policy
  ( RuntimeReplayValidation (..),
  )
import Moonlight.Flow.Carrier.Core.Topology
  ( CarrierTopology,
  )
import Moonlight.Flow.Runtime.Topology.Validate
  ( RuntimeTopologyValidationError,
    validateRuntimeTopology,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnv (..),
    RuntimeEnvelope (..),
  )
import Moonlight.Flow.Runtime.Kernel.Operators
  ( RuntimeCarrierOperators,
  )
import Moonlight.Flow.Runtime.Core.State qualified as Core
import Moonlight.Flow.Runtime.Topology.Routing
  ( RuntimeRouting,
    RuntimeRoutingError,
    Shard (..),
  )
import Moonlight.Flow.Runtime.Topology.Site.Types
  ( GeneratedSiteState (..),
  )
import Moonlight.Flow.Carrier.Reuse
  ( emptyPlanReuseState,
  )
import Moonlight.Flow.Carrier.Reuse.Config
  ( ReuseMode,
  )
import Moonlight.Differential.Time
  ( FrontierStamp,
  )
import Moonlight.Flow.Plan.Residual
  ( ResidualTheoryRegistry,
  )
import Moonlight.Flow.Runtime.Engine.State
  ( emptyRuntimeEngineState,
  )
import Moonlight.Flow.Runtime.Carrier.State
  ( emptyRuntimeCarrierState,
  )
import Moonlight.Flow.Runtime.Factor.State.Types
  ( RuntimeQueryBinding,
    emptyRuntimeFactorState,
  )
import Moonlight.Flow.Runtime.Topology
  ( RuntimeTopology,
    RuntimeTopologyError (..),
    RuntimeTopologySource (..),
    compileRuntimeTopology,
    runtimeCarrierTopology,
    runtimeRouting,
  )
import Moonlight.FiniteLattice
  ( ContextLattice
  )
import Moonlight.Differential.Runtime.Schedule
  ( ScheduleError,
    SchedulePriorityPlan,
  )
import Moonlight.Flow.Runtime.Engine.Queue.Scheduler
  ( runtimeDataflowPriorityPlan,
  )
type RuntimeConfigError :: Type -> Type -> Type -> Type -> Type -> Type
data RuntimeConfigError ctx prop boundary evidence joinErr
  = RuntimeConfigNegativeVisibleCacheBudget !Int
  | RuntimeConfigNegativeProjectShardKey !Int
  | RuntimeConfigNegativeRestrictShardKey !Int
  | RuntimeConfigNegativeIndexShardKey !Int
  | RuntimeConfigFactorProgramInvalid
      !QueryId
      !FactorProgramError
  | RuntimeConfigTopologyInvalid
      ![RuntimeTopologyValidationError ctx prop]
  | RuntimeConfigIndexReplayFailed
      !Shard
      !(CarrierStoreError ctx Carrier prop boundary evidence)
  | RuntimeConfigRoutingFailed
      !(RuntimeRoutingError ctx prop)
  | RuntimeConfigSchedulePriorityInvalid
      !(ScheduleError RelationalPhase)
  deriving stock (Eq, Show)
type RuntimeConfig :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data RuntimeConfig ctx prop boundary evidence joinState joinErr = RuntimeConfig
  { rcQuotientEpoch :: !QuotientEpoch,
    rcLiveEpoch :: !LiveEpoch,
    rcNextFrontierStamp :: !FrontierStamp,
    rcCanonicalityOracle :: !(CanonicalityOracle RowTupleKey),
    rcAtomCarrierEmitSpec :: !(AtomCarrierEmitSpec ctx prop boundary evidence),
    rcFactorCarrierEmitSpec :: !(FactorCarrierEmitSpec ctx prop boundary evidence),
    rcCarrierOperators ::
      !(RuntimeCarrierOperators ctx prop boundary evidence),
    rcCarrierSummaryOps ::
      !( CarrierBatchSummaryOps
           ctx
           Carrier
           prop
           boundary
           evidence
           (CarrierStoreSummaryEntry ctx Carrier prop boundary evidence)
       ),
    rcFrontier :: !(RelDiffFrontier ctx RelationalPhase),
    rcProjectStates :: !(IntMap (CarrierProjectState ctx prop boundary evidence)),
    rcRestrictStates :: !(IntMap (CarrierMorphismRuntime ctx Carrier prop boundary evidence)),
    rcIndexStates :: !(IntMap (CarrierStore ctx Carrier prop boundary evidence)),
    rcVisibleCacheBudgetBytes :: !Int,
    rcVisibleSectionBytes :: RelationalSection ctx Carrier prop -> Int,
    rcContextLattice :: !(ContextLattice ctx),
    rcRepairTelemetry :: !RepairTelemetryConfig,
    rcGeneratedSite :: !(GeneratedSiteState ctx prop),
    rcFactorPrograms :: !(Map RepairProgramKey FactorProgram),
    rcQueryBindings :: !(Map QueryId RuntimeQueryBinding),
    rcReuseMode :: !ReuseMode,
    rcResidualTheoryRegistry :: !ResidualTheoryRegistry
  }
type RelDiffRuntimeConfig :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data RelDiffRuntimeConfig ctx prop boundary evidence joinState joinErr = RelDiffRuntimeConfig
  { rdrcConfig :: !(RuntimeConfig ctx prop boundary evidence joinState joinErr),
    rdrcTopology :: !(RuntimeTopology ctx prop),
    rdrcSchedulePriorityPlan :: !(SchedulePriorityPlan RelationalPhase)
  }
mkRelDiffRuntimeConfig ::
  (Ord ctx, Ord prop, Eq boundary, Eq evidence) =>
  RuntimeConfig ctx prop boundary evidence joinState joinErr ->
  Either
    (RuntimeConfigError ctx prop boundary evidence joinErr)
    (RelDiffRuntimeConfig ctx prop boundary evidence joinState joinErr)
mkRelDiffRuntimeConfig =
  mkRelDiffRuntimeConfigWithReplayValidation RuntimeReplayValidationDisabled
{-# INLINE mkRelDiffRuntimeConfig #-}

mkRelDiffRuntimeConfigWithReplayValidation ::
  (Ord ctx, Ord prop, Eq boundary, Eq evidence) =>
  RuntimeReplayValidation ->
  RuntimeConfig ctx prop boundary evidence joinState joinErr ->
  Either
    (RuntimeConfigError ctx prop boundary evidence joinErr)
    (RelDiffRuntimeConfig ctx prop boundary evidence joinState joinErr)
mkRelDiffRuntimeConfigWithReplayValidation replayValidation config = do
  schedulePriorityPlan <-
    first RuntimeConfigSchedulePriorityInvalid runtimeDataflowPriorityPlan
  topology <-
    first runtimeConfigTopologyError $
      compileRuntimeTopology
        RuntimeTopologySource
          { rtsGeneratedSite = rcGeneratedSite config,
            rtsPlanReuse = emptyPlanReuseState
          }
  let routing =
        runtimeRouting topology
      carrierTopology =
        runtimeCarrierTopology topology
  unless (rcVisibleCacheBudgetBytes config >= 0) $
    Left (RuntimeConfigNegativeVisibleCacheBudget (rcVisibleCacheBudgetBytes config))
  validateNonNegativeKeys RuntimeConfigNegativeProjectShardKey (rcProjectStates config)
  validateNonNegativeKeys RuntimeConfigNegativeRestrictShardKey (rcRestrictStates config)
  validateNonNegativeKeys RuntimeConfigNegativeIndexShardKey (rcIndexStates config)
  validateFactorPrograms config
  validateRuntimeTopologyConfig routing carrierTopology config
  validateIndexReplayWith replayValidation config
  pure
    RelDiffRuntimeConfig
      { rdrcConfig = config,
        rdrcTopology = topology,
        rdrcSchedulePriorityPlan = schedulePriorityPlan
      }
{-# INLINE mkRelDiffRuntimeConfigWithReplayValidation #-}
mkRelDiffRuntime ::
  RelDiffRuntimeConfig ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
mkRelDiffRuntime relConfig =
  RelDiffRuntime
    { rdrState =
        Core.RuntimeState
          { Core.rsClock =
              Core.RuntimeClockState
                { Core.rcsQuotientEpoch = rcQuotientEpoch config,
                  Core.rcsLiveEpoch = rcLiveEpoch config,
                  Core.rcsNextFrontierStamp = rcNextFrontierStamp config
                },
            Core.rsSeedState = Core.emptyRuntimeSeedState,
            Core.rsTopology = topology,
            Core.rsEngine =
              emptyRuntimeEngineState
                (rdrcSchedulePriorityPlan relConfig)
                (rcFrontier config),
            Core.rsCarrier =
              emptyRuntimeCarrierState
                (rcVisibleCacheBudgetBytes config)
                (rcProjectStates config)
                (rcRestrictStates config)
                (rcIndexStates config),
            Core.rsFactor =
              emptyRuntimeFactorState
                (rcFactorPrograms config)
                (rcQueryBindings config)
          },
      rdrEnv =
        RuntimeEnv
          { reCanonicalityOracle = rcCanonicalityOracle config,
            reAtomCarrierEmitSpec = rcAtomCarrierEmitSpec config,
            reFactorCarrierEmitSpec = rcFactorCarrierEmitSpec config,
            reCarrierOperators = rcCarrierOperators config,
            reCarrierSummaryOps = rcCarrierSummaryOps config,
            reVisibleSectionBytes = rcVisibleSectionBytes config,
            reContextLattice = rcContextLattice config,
            reRepairTelemetry = rcRepairTelemetry config,
            reReuseMode = rcReuseMode config,
            reResidualTheoryRegistry = rcResidualTheoryRegistry config
          }
    }
  where
    config =
      rdrcConfig relConfig

    topology =
      rdrcTopology relConfig

runtimeConfigTopologyError ::
  RuntimeTopologyError ctx prop ->
  RuntimeConfigError ctx prop boundary evidence joinErr
runtimeConfigTopologyError topologyError =
  case topologyError of
    RuntimeTopologyRoutingError routingError ->
      RuntimeConfigRoutingFailed routingError
{-# INLINE runtimeConfigTopologyError #-}

emptyRelDiffRuntime ::
  RelDiffRuntimeConfig ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
emptyRelDiffRuntime =
  mkRelDiffRuntime
validateNonNegativeKeys ::
  (Int -> RuntimeConfigError ctx prop boundary evidence joinErr) ->
  IntMap a ->
  Either (RuntimeConfigError ctx prop boundary evidence joinErr) ()
validateNonNegativeKeys mkError =
  IntMap.foldlWithKey'
    ( \eitherUnit shardKey _ -> do
        eitherUnit
        unless (shardKey >= 0) $
          Left (mkError shardKey)
    )
    (Right ())
validateFactorPrograms ::
  RuntimeConfig ctx prop boundary evidence joinState joinErr ->
  Either (RuntimeConfigError ctx prop boundary evidence joinErr) ()
validateFactorPrograms config =
  Map.foldlWithKey'
    ( \eitherUnit _repairKey program ->
        eitherUnit *> validateFactorProgramConfig (factorProgramQueryId program) program
    )
    (Right ())
    (rcFactorPrograms config)
{-# INLINE validateFactorPrograms #-}

validateRuntimeTopologyConfig ::
  (Ord ctx, Ord prop) =>
  RuntimeRouting ctx prop ->
  CarrierTopology ctx Carrier prop ->
  RuntimeConfig ctx prop boundary evidence joinState joinErr ->
  Either (RuntimeConfigError ctx prop boundary evidence joinErr) ()
validateRuntimeTopologyConfig routing carrierTopology config =
  first RuntimeConfigTopologyInvalid $
    validateRuntimeTopology
      routing
      (Map.keysSet (rcQueryBindings config))
      (rcRestrictStates config)
      (rcIndexStates config)
      carrierTopology
{-# INLINE validateRuntimeTopologyConfig #-}

validateFactorProgramConfig ::
  QueryId ->
  FactorProgram ->
  Either (RuntimeConfigError ctx prop boundary evidence joinErr) ()
validateFactorProgramConfig queryId =
  first (RuntimeConfigFactorProgramInvalid queryId)
    . validateFactorProgram
{-# INLINE validateFactorProgramConfig #-}
validateIndexReplay ::
  (Ord ctx, Ord prop, Eq boundary, Eq evidence) =>
  RuntimeConfig ctx prop boundary evidence joinState joinErr ->
  Either (RuntimeConfigError ctx prop boundary evidence joinErr) ()
validateIndexReplay config =
  IntMap.foldlWithKey'
    ( \eitherUnit shardKey indexState -> do
        eitherUnit
        case validateCarrierStore (rcContextLattice config) indexState of
          Left replayError ->
            Left (RuntimeConfigIndexReplayFailed (Shard shardKey) replayError)
          Right () ->
            Right ()
    )
    (Right ())
    (rcIndexStates config)

validateIndexReplayWith ::
  (Ord ctx, Ord prop, Eq boundary, Eq evidence) =>
  RuntimeReplayValidation ->
  RuntimeConfig ctx prop boundary evidence joinState joinErr ->
  Either (RuntimeConfigError ctx prop boundary evidence joinErr) ()
validateIndexReplayWith replayValidation config =
  case replayValidation of
    RuntimeReplayValidationDisabled ->
      Right ()
    RuntimeReplayValidationEnabled ->
      validateIndexReplay config
{-# INLINE validateIndexReplayWith #-}
