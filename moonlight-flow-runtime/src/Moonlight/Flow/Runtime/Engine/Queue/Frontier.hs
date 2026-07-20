module Moonlight.Flow.Runtime.Engine.Queue.Frontier
  ( enqueueScheduledRuntimeDataflowOp,
    completeScheduledRuntimeDataflowOp,
    setRuntimeDataflowQueueFrontier,
    runtimeDataflowQueueProgressFrontier,
  )
where

import Data.Foldable qualified as Foldable
import Data.List.NonEmpty
  ( NonEmpty (..),
  )
import Moonlight.Delta.Time
  ( timedValue,
  )
import Moonlight.Differential.Frontier
  ( RuntimeFrontierError,
    frontierPendingCounts,
    frontierWithPendingCounts,
    mintRootRuntimeCapability,
  )
import Moonlight.Differential.Runtime.Schedule
  ( ScheduleError,
    scheduleRetargetFrontier,
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
import Moonlight.Flow.Runtime.Engine.Queue.Scheduler
  ( runtimeDataflowProgressTime,
    runtimeDataflowScheduledPhase,
  )
import Moonlight.Flow.Runtime.Engine.Queue.Types
  ( RuntimeDataflowQueue,
    completeRuntimeDataflowCapability,
    enqueueRuntimeDataflowBatch,
    runtimeDataflowQueueFrontier,
  )
import Moonlight.Flow.Runtime.Execution.IR
  ( ScheduledRuntimeDataflowOp,
    runtimeDataflowOpProgressPointstamps,
  )

enqueueScheduledRuntimeDataflowOp ::
  Ord ctx =>
  ScheduledRuntimeDataflowOp ctx prop boundary evidence ->
  RuntimeDataflowQueue ctx prop boundary evidence ->
  Either
    (ScheduleError RelationalPhase)
    (RuntimeDataflowQueue ctx prop boundary evidence)
enqueueScheduledRuntimeDataflowOp scheduledOp =
  enqueueRuntimeDataflowBatch
    (runtimeDataflowScheduledPhase op)
    (mintRootRuntimeCapability progressTime)
    (op :| [])
  where
    op =
      timedValue scheduledOp

    progressTime =
      runtimeDataflowProgressTime scheduledOp
{-# INLINE enqueueScheduledRuntimeDataflowOp #-}

completeScheduledRuntimeDataflowOp ::
  Ord ctx =>
  ScheduledRuntimeDataflowOp ctx prop boundary evidence ->
  RuntimeDataflowQueue ctx prop boundary evidence ->
  Either
    (RuntimeFrontierError ctx RelationalRuntimeEpoch RelationalPhase)
    (RuntimeDataflowQueue ctx prop boundary evidence)
completeScheduledRuntimeDataflowOp scheduledOp queue =
  Foldable.foldlM
    ( \queue timeValue ->
        completeRuntimeDataflowCapability
          (mintRootRuntimeCapability timeValue)
          queue
    )
    queue
    (runtimeDataflowOpProgressPointstamps scheduledOp)
{-# INLINE completeScheduledRuntimeDataflowOp #-}

setRuntimeDataflowQueueFrontier ::
  RelDiffFrontier ctx RelationalPhase ->
  RuntimeDataflowQueue ctx prop boundary evidence ->
  RuntimeDataflowQueue ctx prop boundary evidence
setRuntimeDataflowQueueFrontier frontier queue =
  scheduleRetargetFrontier frontier queue
{-# INLINE setRuntimeDataflowQueueFrontier #-}

runtimeDataflowQueueProgressFrontier ::
  RelDiffFrontier ctx RelationalPhase ->
  RuntimeDataflowQueue ctx prop boundary evidence ->
  RelDiffFrontier ctx RelationalPhase
runtimeDataflowQueueProgressFrontier requestedFrontier queue =
  frontierWithPendingCounts
    (frontierPendingCounts (runtimeDataflowQueueFrontier queue))
    requestedFrontier
{-# INLINE runtimeDataflowQueueProgressFrontier #-}
