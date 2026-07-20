module Moonlight.Flow.Runtime.Engine.Patch.Replay
  ( runtimeQuotientPatchReplaySelection,
  )
where

import Moonlight.Flow.Model.Delta
  ( QuotientPatch
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (PhaseSubsumption),
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Flow.Runtime.Core.Replay.Policy
  ( RuntimeReplaySelection,
    mergeRuntimeReplaySelections,
    runtimeReplaySelectionFromCarrierAddrs,
    runtimeReplaySelectionFromDataflowOps,
  )
import Moonlight.Flow.Runtime.Engine.Patch.Schedule
  ( PreparedQuotientPatchSchedule (..),
    prepareQuotientPatchScheduleWithRepairMode,
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
  )

runtimeQuotientPatchReplaySelection ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  QuotientPatch ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RuntimeReplaySelection ctx prop)
runtimeQuotientPatchReplaySelection =
  runtimeQuotientPatchReplaySelectionWithRepairMode False
{-# INLINE runtimeQuotientPatchReplaySelection #-}

runtimeQuotientPatchReplaySelectionWithRepairMode ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  Bool ->
  QuotientPatch ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RuntimeReplaySelection ctx prop)
runtimeQuotientPatchReplaySelectionWithRepairMode forceFullRepair patch runtime =
  preparedQuotientPatchReplaySelection
    <$> prepareQuotientPatchScheduleWithRepairMode forceFullRepair patch runtime
{-# INLINE runtimeQuotientPatchReplaySelectionWithRepairMode #-}

preparedQuotientPatchReplaySelection ::
  (Ord ctx, Ord prop) =>
  PreparedQuotientPatchSchedule ctx prop boundary evidence joinState joinErr ->
  RuntimeReplaySelection ctx prop
preparedQuotientPatchReplaySelection prepared =
  mergeRuntimeReplaySelections
    (runtimeReplaySelectionFromDataflowOps (pqpsOps prepared))
    ( runtimeReplaySelectionFromCarrierAddrs
        PhaseSubsumption
        (pqpsSubsumptionReplayAddrs prepared)
    )
{-# INLINE preparedQuotientPatchReplaySelection #-}
