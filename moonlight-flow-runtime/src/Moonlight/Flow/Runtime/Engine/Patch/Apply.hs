module Moonlight.Flow.Runtime.Engine.Patch.Apply
  ( applyQuotientPatch,
    applyInitialQuotientPatch,
  )
where

import Moonlight.Flow.Model.Delta
  ( QuotientPatch
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Flow.Runtime.Engine.Patch.Schedule
  ( scheduleQuotientPatchWithRepairMode,
  )
import Moonlight.Flow.Runtime.Engine.Step.Settle
  ( settleRuntime,
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
  )

applyQuotientPatch ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop,
    Semigroup evidence
  ) =>
  QuotientPatch ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
applyQuotientPatch =
  applyQuotientPatchWithRepairMode False
{-# INLINE applyQuotientPatch #-}

applyInitialQuotientPatch ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop,
    Semigroup evidence
  ) =>
  QuotientPatch ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
applyInitialQuotientPatch =
  applyQuotientPatchWithRepairMode True
{-# INLINE applyInitialQuotientPatch #-}

applyQuotientPatchWithRepairMode ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop,
    Semigroup evidence
  ) =>
  Bool ->
  QuotientPatch ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
applyQuotientPatchWithRepairMode forceFullRepair patch0 runtime0 =
  scheduleQuotientPatchWithRepairMode forceFullRepair patch0 runtime0
    >>= settleRuntime
{-# INLINE applyQuotientPatchWithRepairMode #-}
