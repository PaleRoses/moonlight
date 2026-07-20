module Moonlight.Flow.Runtime.Engine.Schedule.Feedback
  ( delayScheduledRuntimeDataflowOpFeedback,
  )
where

import Moonlight.Delta.Time
  ( Timed (..),
  )
import Moonlight.Flow.Carrier.Core.Time
  ( delayRelationalCarrierFeedback,
  )
import Moonlight.Flow.Runtime.Execution.IR
  ( ScheduledRuntimeDataflowOp,
  )

delayScheduledRuntimeDataflowOpFeedback ::
  ScheduledRuntimeDataflowOp ctx prop boundary evidence ->
  Maybe (ScheduledRuntimeDataflowOp ctx prop boundary evidence)
delayScheduledRuntimeDataflowOpFeedback op =
  (`Timed` timedValue op) <$> delayRelationalCarrierFeedback (timedAt op)
{-# INLINE delayScheduledRuntimeDataflowOpFeedback #-}
