{-# LANGUAGE BangPatterns #-}

module Moonlight.Flow.Runtime.Engine.Patch.Schedule
  ( scheduleQuotientPatch,
    scheduleInitialQuotientPatch,
    scheduleQuotientPatchWithRepairMode,
    PreparedQuotientPatchSchedule (..),
    prepareQuotientPatchScheduleWithRepairMode,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Set
  ( Set,
  )
import Moonlight.Core
  ( nextLiveEpoch,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Reuse
  ( scrReuseId,
  )
import Moonlight.Flow.Model.Delta
  ( QuotientPatch (..)
  )
import Moonlight.Differential.Row.Patch
  ( EpochTransition (..),
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Flow.Runtime.Carrier.Reuse.State
  ( dropSelectedCarrierReusesRuntime,
    invalidateRuntimePlanReuseByPatch,
    selectStaleCarrierReusesRuntime,
    selectStaleInstalledReuseMaterializationsRuntime,
  )
import Moonlight.Flow.Runtime.Core.Patch.Validation
  ( validateQuotientPatch,
  )
import Moonlight.Flow.Runtime.Core.State qualified as Core
import Moonlight.Flow.Runtime.Engine.Patch.Schedule.AtomEvents
  ( atomEventOps,
    canonicalizeScopedAtomEvents,
  )
import Moonlight.Flow.Runtime.Engine.Patch.Schedule.Retraction
  ( retractStaleCarrierArtifactsWithFanout,
    staleCarrierArtifactAddrs,
  )
import Moonlight.Flow.Runtime.Engine.Schedule.Enqueue
  ( scheduleRuntimeDataflowOps,
  )
import Moonlight.Flow.Runtime.Error
  ( RuntimeError (..),
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Execution.IR
  ( RuntimeDataflowOp,
  )
import Moonlight.Flow.Runtime.Factor.Request
  ( FactorFullRepairReason (FullRepairContextInstalled),
  )
import Moonlight.Flow.Runtime.Factor.State
  ( factorQueryRepresentativeQueryId,
    factorQueryRepairKey,
    factorRepairKeyIsCold,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnv (..),
    RuntimeEnvelope (..),
    rsCarrierTopology,
    rsRouting,
  )
import Moonlight.Flow.Runtime.Topology.Lowering.Impact
  ( impactFromScopedAtomEvents,
    lowerImpactToDataflowOps,
  )
import Moonlight.Flow.Runtime.Topology.Lowering.Types
  ( RuntimeRepairRoute (..),
    RuntimeRepairRouting (..),
  )
import Moonlight.Flow.Runtime.Topology.Routing.Events
  ( quotientPatchEvents,
  )

scheduleQuotientPatch ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  QuotientPatch ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
scheduleQuotientPatch =
  scheduleQuotientPatchWithRepairMode False
{-# INLINE scheduleQuotientPatch #-}

scheduleInitialQuotientPatch ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  QuotientPatch ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
scheduleInitialQuotientPatch =
  scheduleQuotientPatchWithRepairMode True
{-# INLINE scheduleInitialQuotientPatch #-}

scheduleQuotientPatchWithRepairMode ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  Bool ->
  QuotientPatch ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
scheduleQuotientPatchWithRepairMode forceFullRepair patch runtime =
  prepareQuotientPatchScheduleWithRepairMode forceFullRepair patch runtime
    >>= schedulePreparedQuotientPatch
{-# INLINE scheduleQuotientPatchWithRepairMode #-}

data PreparedQuotientPatchSchedule ctx prop boundary evidence joinState joinErr = PreparedQuotientPatchSchedule
  { pqpsRuntimeReady :: !(RelDiffRuntime ctx prop boundary evidence joinState joinErr),
    pqpsOps :: ![RuntimeDataflowOp ctx prop boundary evidence],
    pqpsSubsumptionReplayAddrs :: !(Set (CarrierAddr ctx Carrier prop))
  }

prepareQuotientPatchScheduleWithRepairMode ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  Bool ->
  QuotientPatch ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (PreparedQuotientPatchSchedule ctx prop boundary evidence joinState joinErr)
prepareQuotientPatchScheduleWithRepairMode forceFullRepair patch0 runtime0 = do
  patch <-
    first PatchValidation $
      validateQuotientPatch
        (reCanonicalityOracle (rdrEnv runtime0))
        (Core.rsQuotientEpoch (rdrState runtime0))
        patch0
  let liveEpoch =
        nextLiveEpoch (Core.rsLiveEpoch (rdrState runtime0))
      scopedAtomEvents =
        canonicalizeScopedAtomEvents
          runtime0
          (quotientPatchEvents (rsRouting (rdrState runtime0)) patch)
      impact =
        impactFromScopedAtomEvents scopedAtomEvents
      staleReuses =
        selectStaleCarrierReusesRuntime
          (qpScope patch)
          runtime0
      staleInstalledMaterializations =
        selectStaleInstalledReuseMaterializationsRuntime
          (qpScope patch)
          runtime0
      runtimeEpoch =
        runtime0
          { rdrState =
              Core.mapRuntimeClockState
                ( \clock ->
                    clock
                      { Core.rcsQuotientEpoch = etAfter (qpEpoch patch),
                        Core.rcsLiveEpoch = liveEpoch
                      }
                )
                (rdrState runtime0)
          }
  runtimeRetracted <-
    retractStaleCarrierArtifactsWithFanout
      staleReuses
      staleInstalledMaterializations
      runtimeEpoch
  let runtimeReady =
        invalidateRuntimePlanReuseByPatch
          patch
          ( dropSelectedCarrierReusesRuntime
              (fmap scrReuseId staleReuses)
              runtimeRetracted
          )
      repairRouting =
        repairRoutingForRuntime forceFullRepair runtimeReady
      plannedOps =
        lowerImpactToDataflowOps
          repairRouting
          FullRepairContextInstalled
          (rsCarrierTopology (rdrState runtimeReady))
          impact
  atomOps <-
    atomEventOps scopedAtomEvents runtimeReady
  Right
    PreparedQuotientPatchSchedule
      { pqpsRuntimeReady = runtimeReady,
        pqpsOps = atomOps <> plannedOps,
        pqpsSubsumptionReplayAddrs =
          staleCarrierArtifactAddrs staleReuses staleInstalledMaterializations
      }

schedulePreparedQuotientPatch ::
  Ord ctx =>
  PreparedQuotientPatchSchedule ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
schedulePreparedQuotientPatch prepared =
  scheduleRuntimeDataflowOps
    (pqpsOps prepared)
    (pqpsRuntimeReady prepared)

repairRoutingForRuntime ::
  Bool ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RuntimeRepairRouting
repairRoutingForRuntime forceFullRepair runtime =
  RuntimeRepairRouting
    { rrRepairRouteOfQuery = repairRouteOfQuery,
      rrRepairIsCold =
        \repairKey ->
          forceFullRepair || factorRepairKeyIsCold runtime repairKey
    }
  where
    repairRouteOfQuery queryId = do
      repairKey <- factorQueryRepairKey runtime queryId
      representativeQueryId <- factorQueryRepresentativeQueryId runtime queryId
      pure
        RuntimeRepairRoute
          { rrtRepairKey = repairKey,
            rrtRepresentativeQueryId = representativeQueryId
          }
