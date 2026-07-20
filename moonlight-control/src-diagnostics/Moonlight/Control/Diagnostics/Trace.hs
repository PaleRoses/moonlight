{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Control.Diagnostics.Trace
  ( RoundMetrics (..),
    RoundTrace (..),
    TraceLog,
    emptyTraceLog,
    singletonTraceLog,
    appendTraceLog,
    appendTraceLogWithPolicy,
    traceLogRounds,
    traceLogDropBeforeIteration,
    traceLogPrefixes,
  )
where

import Moonlight.Control.Schedule
  ( TracePolicy,
  )
import Moonlight.Control.Schedule.Round
  ( ScheduleTrace,
  )
import Moonlight.Control.Trace.RoundLog
  ( RoundLog,
    appendRoundLog,
    appendRoundLogsWithPolicy,
    emptyRoundLog,
    roundLogDropWhile,
    roundLogPrefixes,
    roundLogRounds,
    singletonRoundLog,
  )

-- | Generic per-round engine metrics for traceable worklist-style runs.
data RoundMetrics = RoundMetrics
  { rmIteration :: !Int,
    rmNodeCountBefore :: !Int,
    rmNodeCountAfter :: !Int,
    rmBaseEligibleCount :: !Int,
    rmContextEligibleCount :: !Int,
    rmAggregatedEligibleCount :: !Int,
    rmGuidedCount :: !Int,
    rmScheduledCount :: !Int,
    rmFactsChanged :: !Bool,
    rmFactRoundCount :: !Int,
    rmContextRevision :: !Int
  }
  deriving stock (Eq, Show, Read)

data RoundTrace ruleKey schedulerGroup = RoundTrace
  { roundTraceMetrics :: !RoundMetrics,
    roundTraceSchedule :: ![ScheduleTrace schedulerGroup]
  }
  deriving stock (Eq, Show, Read)

type TraceLog ruleKey schedulerGroup =
  RoundLog (RoundTrace ruleKey schedulerGroup)

emptyTraceLog :: TraceLog ruleKey schedulerGroup
emptyTraceLog =
  emptyRoundLog

singletonTraceLog ::
  RoundTrace ruleKey schedulerGroup ->
  TraceLog ruleKey schedulerGroup
singletonTraceLog =
  singletonRoundLog

appendTraceLog ::
  RoundTrace ruleKey schedulerGroup ->
  TraceLog ruleKey schedulerGroup ->
  TraceLog ruleKey schedulerGroup
appendTraceLog =
  appendRoundLog

appendTraceLogWithPolicy ::
  TracePolicy ->
  TraceLog ruleKey schedulerGroup ->
  TraceLog ruleKey schedulerGroup ->
  TraceLog ruleKey schedulerGroup
appendTraceLogWithPolicy tracePolicy previousLog deltaLog =
  appendRoundLogsWithPolicy
    roundTraceSchedule
    (\scheduleEntries roundTrace -> roundTrace {roundTraceSchedule = scheduleEntries})
    tracePolicy
    previousLog
    deltaLog

traceLogRounds ::
  TraceLog ruleKey schedulerGroup ->
  [RoundTrace ruleKey schedulerGroup]
traceLogRounds =
  roundLogRounds

traceLogDropBeforeIteration ::
  Int ->
  TraceLog ruleKey schedulerGroup ->
  TraceLog ruleKey schedulerGroup
traceLogDropBeforeIteration minimumIteration =
  roundLogDropWhile
    ((< minimumIteration) . rmIteration . roundTraceMetrics)

traceLogPrefixes ::
  TraceLog ruleKey schedulerGroup ->
  [TraceLog ruleKey schedulerGroup]
traceLogPrefixes =
  roundLogPrefixes
