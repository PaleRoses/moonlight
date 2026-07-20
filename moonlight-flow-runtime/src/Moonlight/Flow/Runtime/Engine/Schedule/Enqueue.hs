module Moonlight.Flow.Runtime.Engine.Schedule.Enqueue
  ( enqueueScheduledRuntimeDataflowOpRuntime,
    scheduleRuntimeDataflowOp,
    scheduleRuntimeDataflowOps,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.Bifunctor
  ( first,
  )
import Moonlight.Delta.Time
  ( Timed (..),
  )
import Moonlight.Flow.Runtime.Engine.Queue.Frontier
  ( enqueueScheduledRuntimeDataflowOp,
  )
import Moonlight.Flow.Runtime.Engine.Schedule.Time
  ( allocateExecutionTimeForDataflowOp,
  )
import Moonlight.Flow.Runtime.Engine.State
  ( runtimeEngineQueue,
    setRuntimeEngineQueue,
  )
import Moonlight.Flow.Runtime.Error
  ( RuntimeError (..),
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Execution.IR
  ( RuntimeDataflowOp,
    ScheduledRuntimeDataflowOp,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
  )

enqueueScheduledRuntimeDataflowOpRuntime ::
  Ord ctx =>
  ScheduledRuntimeDataflowOp ctx prop boundary evidence ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
enqueueScheduledRuntimeDataflowOpRuntime op runtime =
  let state =
        rdrState runtime
   in fmap
        (\queue -> runtime {rdrState = setRuntimeEngineQueue queue state})
        ( first RuntimeSchedulePriorityInvalid
            (enqueueScheduledRuntimeDataflowOp op (runtimeEngineQueue state))
        )
{-# INLINE enqueueScheduledRuntimeDataflowOpRuntime #-}

scheduleRuntimeDataflowOp ::
  Ord ctx =>
  RuntimeDataflowOp ctx prop boundary evidence ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
scheduleRuntimeDataflowOp op runtime0 = do
  (runtime1, eventTime) <-
    allocateExecutionTimeForDataflowOp op runtime0
  enqueueScheduledRuntimeDataflowOpRuntime
    (Timed eventTime op)
    runtime1
{-# INLINE scheduleRuntimeDataflowOp #-}

scheduleRuntimeDataflowOps ::
  Ord ctx =>
  [RuntimeDataflowOp ctx prop boundary evidence] ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
scheduleRuntimeDataflowOps ops runtime0 =
  foldM
    (\runtime op -> scheduleRuntimeDataflowOp op runtime)
    runtime0
    ops
{-# INLINE scheduleRuntimeDataflowOps #-}
