{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Runtime.Engine.Queue.Types
  ( RuntimeDataflowQueue,
    emptyRuntimeDataflowQueue,
    runtimeDataflowQueueFrontier,
    runtimeDataflowQueuePriorityPlan,
    runtimeDataflowQueueEmpty,
    runtimeDataflowQueueCellsEmpty,
    runtimeDataflowQueueCells,
    runtimeDataflowQueuedOps,
    enqueueRuntimeDataflowBatch,
    dequeueRuntimeDataflowQueue,
    completeRuntimeDataflowCapability,
  )
where

import Data.Kind
  ( Type,
  )
import Data.List.NonEmpty
  ( NonEmpty,
  )
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Differential.Frontier
  ( RuntimeCapability,
    RuntimeFrontierError,
  )
import Moonlight.Differential.Runtime.Schedule
  ( ProgressSchedule,
    ScheduleError,
    SchedulePriorityPlan,
    ScheduleCell (..),
    ScheduledWork (..),
    mkProgressSchedule,
    scheduleCellsEmpty,
    scheduleComplete,
    scheduleDequeue,
    scheduleEnqueue,
    scheduleFrontier,
    schedulePriorityPlan,
    scheduleQuiescent,
    scheduleWork,
  )
import Moonlight.Flow.Carrier.Core.Frontier
  ( RelDiffFrontier,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalRuntimeEpoch,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase,
  )
import Moonlight.Flow.Runtime.Execution.IR
  ( RuntimeDataflowOp,
  )


type RuntimeDataflowQueue :: Type -> Type -> Type -> Type -> Type
type RuntimeDataflowQueue ctx prop boundary evidence =
  ProgressSchedule
    ctx
    RelationalRuntimeEpoch
    RelationalPhase
    RelationalPhase
    (NonEmpty (RuntimeDataflowOp ctx prop boundary evidence))

emptyRuntimeDataflowQueue ::
  SchedulePriorityPlan RelationalPhase ->
  RelDiffFrontier ctx RelationalPhase ->
  RuntimeDataflowQueue ctx prop boundary evidence
emptyRuntimeDataflowQueue =
  mkProgressSchedule
{-# INLINE emptyRuntimeDataflowQueue #-}

runtimeDataflowQueueFrontier ::
  RuntimeDataflowQueue ctx prop boundary evidence ->
  RelDiffFrontier ctx RelationalPhase
runtimeDataflowQueueFrontier =
  scheduleFrontier
{-# INLINE runtimeDataflowQueueFrontier #-}

runtimeDataflowQueuePriorityPlan ::
  RuntimeDataflowQueue ctx prop boundary evidence ->
  SchedulePriorityPlan RelationalPhase
runtimeDataflowQueuePriorityPlan =
  schedulePriorityPlan
{-# INLINE runtimeDataflowQueuePriorityPlan #-}

runtimeDataflowQueueEmpty ::
  RuntimeDataflowQueue ctx prop boundary evidence ->
  Bool
runtimeDataflowQueueEmpty =
  scheduleQuiescent
{-# INLINE runtimeDataflowQueueEmpty #-}

runtimeDataflowQueueCellsEmpty ::
  RuntimeDataflowQueue ctx prop boundary evidence ->
  Bool
runtimeDataflowQueueCellsEmpty =
  scheduleCellsEmpty
{-# INLINE runtimeDataflowQueueCellsEmpty #-}

runtimeDataflowQueueCells ::
  RuntimeDataflowQueue ctx prop boundary evidence ->
  [ ScheduledWork
      ctx
      RelationalRuntimeEpoch
      RelationalPhase
      RelationalPhase
      (NonEmpty (RuntimeDataflowOp ctx prop boundary evidence))
  ]
runtimeDataflowQueueCells =
  scheduleWork
{-# INLINE runtimeDataflowQueueCells #-}

runtimeDataflowQueuedOps ::
  RuntimeDataflowQueue ctx prop boundary evidence ->
  [RuntimeDataflowOp ctx prop boundary evidence]
runtimeDataflowQueuedOps =
  concatMap runtimeDataflowWorkOps . scheduleWork
{-# INLINE runtimeDataflowQueuedOps #-}

enqueueRuntimeDataflowBatch ::
  Ord ctx =>
  RelationalPhase ->
  RuntimeCapability ctx RelationalRuntimeEpoch RelationalPhase ->
  NonEmpty (RuntimeDataflowOp ctx prop boundary evidence) ->
  RuntimeDataflowQueue ctx prop boundary evidence ->
  Either
    (ScheduleError RelationalPhase)
    (RuntimeDataflowQueue ctx prop boundary evidence)
enqueueRuntimeDataflowBatch =
  scheduleEnqueue
{-# INLINE enqueueRuntimeDataflowBatch #-}

dequeueRuntimeDataflowQueue ::
  RuntimeDataflowQueue ctx prop boundary evidence ->
  Maybe
    ( ScheduledWork ctx RelationalRuntimeEpoch RelationalPhase RelationalPhase (NonEmpty (RuntimeDataflowOp ctx prop boundary evidence)),
      RuntimeDataflowQueue ctx prop boundary evidence
    )
dequeueRuntimeDataflowQueue =
  scheduleDequeue
{-# INLINE dequeueRuntimeDataflowQueue #-}

completeRuntimeDataflowCapability ::
  Ord ctx =>
  RuntimeCapability ctx RelationalRuntimeEpoch RelationalPhase ->
  RuntimeDataflowQueue ctx prop boundary evidence ->
  Either
    (RuntimeFrontierError ctx RelationalRuntimeEpoch RelationalPhase)
    (RuntimeDataflowQueue ctx prop boundary evidence)
completeRuntimeDataflowCapability =
  scheduleComplete
{-# INLINE completeRuntimeDataflowCapability #-}

runtimeDataflowWorkOps ::
  ScheduledWork ctx epoch phase priority (NonEmpty payload) ->
  [payload]
runtimeDataflowWorkOps =
  NonEmpty.toList . scheduleCellPayload . scheduledWorkCell
{-# INLINE runtimeDataflowWorkOps #-}
