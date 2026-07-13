{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Saturation.Context.Runtime.Schedule.Decision
  ( RuntimeScheduleDecision (..),
    runtimeScheduleDecisionScheduledCount,
  )
where

import Data.Kind (Type)
import Data.Vector (Vector)
import Moonlight.Control.Schedule
  ( TracePolicy,
  )
import Moonlight.Control.Schedule.Round
  ( ScheduleTrace,
    SchedulerState,
  )
import Moonlight.Saturation.Context.Runtime.Match.Batch
  ( MatchBatch,
    matchBatchLength,
  )
import Moonlight.Saturation.Context.Runtime.Match.Pipeline
  ( CandidatePipelineCounts,
  )

type RuntimeScheduleDecision :: Type -> Type -> Type
data RuntimeScheduleDecision group match = RuntimeScheduleDecision
  { rsdScheduledMatches :: !(MatchBatch match),
    rsdSchedulerState :: !(SchedulerState group),
    rsdTracePolicy :: !TracePolicy,
    rsdTraceDelta :: !(Vector (ScheduleTrace group)),
    rsdAllCandidatesScheduled :: !Bool,
    rsdPipelineCounts :: !(CandidatePipelineCounts group)
  }
  deriving stock (Eq, Show)

runtimeScheduleDecisionScheduledCount ::
  RuntimeScheduleDecision group match ->
  Int
runtimeScheduleDecisionScheduledCount =
  matchBatchLength . rsdScheduledMatches
{-# INLINE runtimeScheduleDecisionScheduledCount #-}
