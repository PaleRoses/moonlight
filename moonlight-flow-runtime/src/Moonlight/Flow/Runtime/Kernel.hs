{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Runtime.Kernel
  ( RuntimeState,
    RuntimeEnv (..),
    RelDiffRuntime,
    RuntimeEnvelope (..),

    -- core state
    Core.RuntimeClockState (..),
    Core.RuntimeSeedState (..),
    Core.initialRuntimeClockState,
    Core.emptyRuntimeSeedState,
    Core.runtimeSeedStateFromPatch,
    Core.runtimeSeedStateSettled,
    Core.rsQuotientEpoch,
    Core.rsLiveEpoch,
    Core.rsNextFrontierStamp,

    -- topology projections
    rsGeneratedSite,
    rsRouting,
    rsCarrierTopology,
    rsPlanReuse,
  )
where

import Data.Kind
  ( Type,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Flow.Carrier.Core.Summary
  ( CarrierBatchSummaryOps,
    CarrierStoreSummaryEntry,
  )
import Moonlight.Flow.Carrier.Core.Topology
  ( CarrierTopology,
  )
import Moonlight.Flow.Carrier.Reuse
  ( PlanReuseState,
  )
import Moonlight.Flow.Carrier.Reuse.Config
  ( ReuseMode,
  )
import Moonlight.Flow.Carrier.View.Section
  ( RelationalSection,
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )
import Moonlight.Flow.Execution.Observe.Telemetry
  ( RepairTelemetryConfig,
  )
import Moonlight.Flow.Plan.Residual
  ( ResidualTheoryRegistry,
  )
import Moonlight.Flow.Runtime.Carrier.Emit
  ( AtomCarrierEmitSpec,
    FactorCarrierEmitSpec,
  )
import Moonlight.Flow.Runtime.Carrier.State
  ( RuntimeCarrierState,
  )
import Moonlight.Flow.Runtime.Core.Env
  ( RuntimeEnvelope (..),
  )
import Moonlight.Flow.Runtime.Core.Patch.Validation
  ( CanonicalityOracle,
  )
import Moonlight.Flow.Runtime.Kernel.Operators
  ( RuntimeCarrierOperators,
  )
import Moonlight.Flow.Runtime.Core.State qualified as Core
import Moonlight.Flow.Runtime.Engine.State
  ( RuntimeEngineState,
  )
import Moonlight.Flow.Runtime.Factor.State.Types
  ( RuntimeFactorState,
  )
import Moonlight.Flow.Runtime.Topology
  ( RuntimeTopology,
    runtimeCarrierTopology,
    runtimeGeneratedSite,
    runtimePlanReuse,
    runtimeRouting,
  )
import Moonlight.Flow.Runtime.Topology.Routing
  ( RuntimeRouting,
  )
import Moonlight.Flow.Runtime.Topology.Site.Types
  ( GeneratedSiteState,
  )
import Moonlight.FiniteLattice
  ( ContextLattice
  )

type RuntimeState :: Type -> Type -> Type -> Type -> Type
type RuntimeState ctx prop boundary evidence =
  Core.RuntimeState
    (RuntimeTopology ctx prop)
    (RuntimeEngineState ctx prop boundary evidence)
    (RuntimeCarrierState ctx prop boundary evidence)
    RuntimeFactorState

type RuntimeEnv :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data RuntimeEnv ctx prop boundary evidence joinState joinErr = RuntimeEnv
  { reCanonicalityOracle :: !(CanonicalityOracle RowTupleKey),
    reAtomCarrierEmitSpec :: !(AtomCarrierEmitSpec ctx prop boundary evidence),
    reFactorCarrierEmitSpec :: !(FactorCarrierEmitSpec ctx prop boundary evidence),
    reCarrierOperators ::
      !(RuntimeCarrierOperators ctx prop boundary evidence),
    reCarrierSummaryOps ::
      !( CarrierBatchSummaryOps
           ctx
           Carrier
           prop
           boundary
           evidence
           (CarrierStoreSummaryEntry ctx Carrier prop boundary evidence)
       ),
    reVisibleSectionBytes :: RelationalSection ctx Carrier prop -> Int,
    reContextLattice :: !(ContextLattice ctx),
    reRepairTelemetry :: !RepairTelemetryConfig,
    reReuseMode :: !ReuseMode,
    reResidualTheoryRegistry :: !ResidualTheoryRegistry
  }

type RelDiffRuntime :: Type -> Type -> Type -> Type -> Type -> Type -> Type
type RelDiffRuntime ctx prop boundary evidence joinState joinErr =
  RuntimeEnvelope
    (RuntimeState ctx prop boundary evidence)
    (RuntimeEnv ctx prop boundary evidence joinState joinErr)

rsGeneratedSite :: RuntimeState ctx prop boundary evidence -> GeneratedSiteState ctx prop
rsGeneratedSite =
  runtimeGeneratedSite . Core.rsTopology
{-# INLINE rsGeneratedSite #-}

rsRouting :: RuntimeState ctx prop boundary evidence -> RuntimeRouting ctx prop
rsRouting =
  runtimeRouting . Core.rsTopology
{-# INLINE rsRouting #-}

rsCarrierTopology :: RuntimeState ctx prop boundary evidence -> CarrierTopology ctx Carrier prop
rsCarrierTopology =
  runtimeCarrierTopology . Core.rsTopology
{-# INLINE rsCarrierTopology #-}

rsPlanReuse :: RuntimeState ctx prop boundary evidence -> PlanReuseState ctx prop
rsPlanReuse =
  runtimePlanReuse . Core.rsTopology
{-# INLINE rsPlanReuse #-}
