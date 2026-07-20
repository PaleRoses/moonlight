{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Moonlight.Flow.Runtime.Types
  ( Runtime (..),
    RuntimeSection (..),
    RuntimeSeedMode (..),
    RuntimeSeedChunk (..),
    RuntimeSeedProgress (..),
    RuntimeCreateOptions (..),
    defaultRuntimeCreateOptions,
    RuntimeBackendError (..),
    RuntimeCreateError (..),
    RuntimeApplyError (..),
    RuntimeReadError (..),
    RuntimeReuseStats (..),
    RuntimeReuseDiagnostics (..),
    RuntimeDiagnostics (..),
    RepairTelemetryLevel (..),
    RepairTelemetryConfig (..),
    summaryRepairTelemetryConfig,
    detailedRepairTelemetryConfig,
    defaultRepairTelemetryConfig,
    RuntimeRepairNodeAction (..),
    RuntimeRepairNodeStats (..),
    RepairTelemetry (..),
    emptyRepairTelemetry,
    repairTelemetryDifference,
    repairTelemetryWeight,
    RuntimeRepairStats (..),
    runtimeReuseStatsFromPlanReuseStats,
    runtimeReuseDiagnosticsFromPlanReuseDiagnostics,
  )
where

import Moonlight.Core
  ( AtomId,
    QueryId,
    SlotId,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.View.Section
  ( RelationalSection,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Flow.Execution.Factor.Run
  ( FactorRunError,
  )
import Moonlight.Flow.Execution.Observe.Telemetry
  ( RepairTelemetryConfig (..),
    RepairTelemetryLevel (..),
    defaultRepairTelemetryConfig,
    detailedRepairTelemetryConfig,
    summaryRepairTelemetryConfig,
  )
import Moonlight.Flow.Query
  ( QueryError,
  )
import Moonlight.Flow.Model.Family
  ( AtomFamilyDecodeError,
  )
import Moonlight.Flow.Runtime.Kernel.Config
  ( RuntimeConfigError,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
  )
import Moonlight.Flow.Runtime.Backend
  ( RuntimeBackendError (..),
  )
import Moonlight.Flow.Runtime.Core.Hydrate
  ( RuntimeSeedChunk (..),
    RuntimeSeedProgress (..),
  )
import Moonlight.Flow.Runtime.Core.RepairStats
  ( RuntimeRepairNodeAction (..),
    RuntimeRepairNodeStats (..),
    RepairTelemetry (..),
    emptyRepairTelemetry,
    repairTelemetryDifference,
    repairTelemetryWeight,
    RuntimeRepairStats (..),
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Spec.FactorProgram
  ( FactorProgramError,
  )
import Moonlight.Flow.Carrier.Reuse
  ( PlanReuseDiagnostics (..),
    PlanReuseStats (..),
  )
import Moonlight.Flow.Runtime.Spec.Schema
  ( RuntimeSpecError,
  )
import Moonlight.Flow.Runtime.Topology.Site.Types
  ( GeneratedSiteValidationError,
  )
import Moonlight.Flow.Runtime.Topology.Subscription.Builder
  ( CarrierSubscriptionBuildError,
  )
import Moonlight.Flow.Runtime.Topology.Routing
  ( Shard,
  )

data Runtime ctx prop where
  Runtime ::
    (Ord ctx, Ord prop, Show evidence, Semigroup evidence, Show joinErr) =>
    !(RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr) ->
    Runtime ctx prop

newtype RuntimeSection ctx prop = RuntimeSection
  { unRuntimeSection :: RelationalSection ctx Carrier prop
  }

data RuntimeSeedMode
  = RuntimeSeedEager
  | RuntimeSeedDeferred
  deriving stock (Eq, Ord, Show, Read)

data RuntimeCreateOptions = RuntimeCreateOptions
  { rcoVisibleCacheBudgetBytes :: {-# UNPACK #-} !Int,
    rcoSeedMode :: !RuntimeSeedMode,
    rcoRepairTelemetry :: !RepairTelemetryConfig
  }
  deriving stock (Eq, Ord, Show, Read)

defaultRuntimeCreateOptions :: RuntimeCreateOptions
defaultRuntimeCreateOptions =
  RuntimeCreateOptions
    { rcoVisibleCacheBudgetBytes = 4 * 1024 * 1024,
      rcoSeedMode = RuntimeSeedEager,
      rcoRepairTelemetry = defaultRepairTelemetryConfig
    }
{-# INLINE defaultRuntimeCreateOptions #-}

data RuntimeCreateError ctx prop where
  RuntimeCreateSpecError ::
    !(RuntimeSpecError ctx prop) ->
    RuntimeCreateError ctx prop
  RuntimeCreateBackendError ::
    !(RuntimeBackendError ctx prop) ->
    RuntimeCreateError ctx prop
  RuntimeCreateGeneratedSiteInvalid ::
    ![GeneratedSiteValidationError ctx prop] ->
    RuntimeCreateError ctx prop
  RuntimeCreateCarrierSubscriptionBuildFailed ::
    !(CarrierSubscriptionBuildError ctx prop) ->
    RuntimeCreateError ctx prop
  RuntimeCreateFactorProgramInvalid ::
    !QueryId ->
    !FactorProgramError ->
    RuntimeCreateError ctx prop
  RuntimeCreateConfigRejected ::
    (Show evidence, Show joinErr) =>
    !(RuntimeConfigError ctx prop RuntimeBoundary evidence joinErr) ->
    RuntimeCreateError ctx prop
  RuntimeCreateSeedRejected ::
    !(RuntimeApplyError ctx prop) ->
    RuntimeCreateError ctx prop

deriving stock instance
  (Show ctx, Show prop) =>
  Show (RuntimeCreateError ctx prop)

data RuntimeApplyError ctx prop where
  RuntimeApplyRejected ::
    Show evidence =>
    !(RelationalRuntimeError ctx prop RuntimeBoundary evidence) ->
    RuntimeApplyError ctx prop
  RuntimeApplyInvalidSeedChunk ::
    !RuntimeSeedChunk ->
    RuntimeApplyError ctx prop
  RuntimeApplySeedPending ::
    !RuntimeSeedProgress ->
    RuntimeApplyError ctx prop

deriving stock instance
  (Show ctx, Show prop) =>
  Show (RuntimeApplyError ctx prop)

data RuntimeReadError ctx prop
  = RuntimeReadPlanMissing !QueryId
  | RuntimeReadSeedPending !QueryId
  | RuntimeReadFamilySchemaMismatch !QueryId !AtomId ![SlotId] ![SlotId]
  | RuntimeReadFamilyProjectionFailed !QueryId !AtomId !QueryError
  | RuntimeReadFamilyDecodeFailed !QueryId !AtomId !AtomFamilyDecodeError
  | RuntimeReadPlanRootUnrouted !QueryId
  | RuntimeReadIndexUnavailable !QueryId
  | RuntimeReadFactorRowsUnavailable !QueryId
  | RuntimeReadFactorRowsObstructed !QueryId !FactorRunError
  | RuntimeReadCarrierUnrouted !(CarrierAddr ctx Carrier prop)
  | RuntimeReadCarrierStoreUnavailable !(CarrierAddr ctx Carrier prop)
  | RuntimeReadCarrierIndexShardUnavailable !Shard
  | RuntimeReadCarrierRuntimeFailure
  deriving stock (Eq, Ord, Show)

data RuntimeReuseStats = RuntimeReuseStats
  { rrsRegisteredNew :: {-# UNPACK #-} !Int,
    rrsExactHits :: {-# UNPACK #-} !Int,
    rrsContainmentHits :: {-# UNPACK #-} !Int,
    rrsLowerBoundEmits :: {-# UNPACK #-} !Int,
    rrsExactProjectionEmits :: {-# UNPACK #-} !Int,
    rrsObstructedProjections :: {-# UNPACK #-} !Int,
    rrsStaleRejected :: {-# UNPACK #-} !Int,
    rrsResidualRejected :: {-# UNPACK #-} !Int,
    rrsBoundaryRejected :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show, Read)

data RuntimeReuseDiagnostics = RuntimeReuseDiagnostics
  { rrdStats :: !RuntimeReuseStats,
    rrdRegisteredFactorShapes :: {-# UNPACK #-} !Int,
    rrdRegisteredReuses :: {-# UNPACK #-} !Int,
    rrdInstalledMaterializations :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show, Read)

data RuntimeDiagnostics = RuntimeDiagnostics
  { rdReuseStats :: !RuntimeReuseStats,
    rdReuseDiagnostics :: !RuntimeReuseDiagnostics,
    rdRepairStats :: !RuntimeRepairStats
  }
  deriving stock (Eq, Show)

runtimeReuseStatsFromPlanReuseStats :: PlanReuseStats -> RuntimeReuseStats
runtimeReuseStatsFromPlanReuseStats stats =
  RuntimeReuseStats
    { rrsRegisteredNew = prsRegisteredNew stats,
      rrsExactHits = prsExactHits stats,
      rrsContainmentHits = prsContainmentHits stats,
      rrsLowerBoundEmits = prsLowerBoundEmits stats,
      rrsExactProjectionEmits = prsExactProjectionEmits stats,
      rrsObstructedProjections = prsObstructedProjections stats,
      rrsStaleRejected = prsStaleRejected stats,
      rrsResidualRejected = prsResidualRejected stats,
      rrsBoundaryRejected = prsBoundaryRejected stats
    }
{-# INLINE runtimeReuseStatsFromPlanReuseStats #-}

runtimeReuseDiagnosticsFromPlanReuseDiagnostics ::
  PlanReuseDiagnostics ->
  RuntimeReuseDiagnostics
runtimeReuseDiagnosticsFromPlanReuseDiagnostics diagnostics =
  RuntimeReuseDiagnostics
    { rrdStats =
        runtimeReuseStatsFromPlanReuseStats (prdStats diagnostics),
      rrdRegisteredFactorShapes = prdRegisteredShapes diagnostics,
      rrdRegisteredReuses = prdRegisteredReuses diagnostics,
      rrdInstalledMaterializations = prdInstalledMaterializations diagnostics
    }
{-# INLINE runtimeReuseDiagnosticsFromPlanReuseDiagnostics #-}
