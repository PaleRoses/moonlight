{-# LANGUAGE BangPatterns #-}

-- | Reporting vocabulary of the engine: stop reasons, per-round observations
-- and metrics, the retained round log, and the final report.
--
-- An 'Observation' is the complete record of one round, including full gate
-- and schedule traces; evidence policies consume observations. An
-- 'EngineRound' is the retained form whose traces have been trimmed by the
-- report 'TracePolicy'.
module Moonlight.Control.Engine.Report
  ( StopReason (..),
    RoundMetrics (..),
    Observation (..),
    roundMetricsFromObservation,
    RoundSummary (..),
    EngineRound (..),
    engineRoundMetrics,
    EngineRoundLog,
    emptyEngineRoundLog,
    singletonEngineRoundLog,
    appendEngineRoundLog,
    appendEngineRoundLogWithPolicy,
    engineRoundLogRounds,
    EngineReport (..),
  )
where

import Numeric.Natural (Natural)

import Moonlight.Control.Count
  ( WorkCount,
    WorkCoverage,
  )
import Moonlight.Control.Schedule
  ( TracePolicy,
  )
import Moonlight.Control.Schedule.Round
  ( ScheduleTrace,
    SchedulerState,
  )
import Moonlight.Control.Trace
  ( Trace,
  )
import Moonlight.Control.Trace.RoundLog
  ( RoundLog,
    appendRoundLog,
    emptyRoundLog,
    retainRoundLogWithPolicy,
    roundLogRounds,
    singletonRoundLog,
  )
import Moonlight.Control.Weight
  ( PriorityProfile,
  )

-- | Why a run stopped.
data StopReason
  = Converged
  | NoCandidateWork
  | NoRunnableWork
  | SchedulerBlocked
  | RoundLimitReached
  | ProgramCompleted
  | StoppedByPolicy !String
  deriving stock (Eq, Show, Read)

-- | The trace-free numeric core of an 'Observation'.
data RoundMetrics = RoundMetrics
  { rmRound :: !Int,
    rmCandidateCount :: !WorkCount,
    rmGatedCount :: !WorkCount,
    rmScheduledCount :: !Natural,
    rmAppliedCount :: !Natural,
    rmDroppedByGate :: !WorkCount,
    rmSuppressedCount :: !WorkCount,
    rmDeferredByBudgetCount :: !WorkCount,
    rmCoverage :: !WorkCoverage,
    rmProgressed :: !Bool
  }
  deriving stock (Eq, Show, Read)

-- | The complete record of one engine round, with full traces.
data Observation group traceEntry evidence = Observation
  { obRound :: !Int,
    obCandidateCount :: !WorkCount,
    obGatedCount :: !WorkCount,
    obScheduledCount :: !Natural,
    obAppliedCount :: !Natural,
    obDroppedByGate :: !WorkCount,
    obSuppressedCount :: !WorkCount,
    obDeferredByBudgetCount :: !WorkCount,
    obCoverage :: !WorkCoverage,
    obProgressed :: !Bool,
    obEvidence :: !evidence,
    obGateTrace :: ![traceEntry],
    obScheduleTrace :: ![ScheduleTrace group]
  }
  deriving stock (Eq, Show, Read)

-- | Project the numeric core of an observation. O(1).
roundMetricsFromObservation ::
  Observation group traceEntry evidence ->
  RoundMetrics
roundMetricsFromObservation observation =
  RoundMetrics
    { rmRound = obRound observation,
      rmCandidateCount = obCandidateCount observation,
      rmGatedCount = obGatedCount observation,
      rmScheduledCount = obScheduledCount observation,
      rmAppliedCount = obAppliedCount observation,
      rmDroppedByGate = obDroppedByGate observation,
      rmSuppressedCount = obSuppressedCount observation,
      rmDeferredByBudgetCount = obDeferredByBudgetCount observation,
      rmCoverage = obCoverage observation,
      rmProgressed = obProgressed observation
    }

-- | The phase summary the engine hands to the machine's trace: one round of
-- one named phase.
data RoundSummary = RoundSummary
  { rsPhaseName :: !String,
    rsRound :: !Int,
    rsCandidateCount :: !WorkCount,
    rsGatedCount :: !WorkCount,
    rsScheduledCount :: !Natural,
    rsAppliedCount :: !Natural,
    rsSuppressedCount :: !WorkCount,
    rsDeferredByBudgetCount :: !WorkCount,
    rsCoverage :: !WorkCoverage,
    rsProgressed :: !Bool,
    rsStopReason :: !(Maybe StopReason)
  }
  deriving stock (Eq, Show, Read)

-- | A retained round: the observation as trimmed by the report trace policy,
-- plus the stop decision taken after it.
data EngineRound group traceEntry evidence = EngineRound
  { roundObservation :: !(Observation group traceEntry evidence),
    roundStopReason :: !(Maybe StopReason)
  }
  deriving stock (Eq, Show, Read)

-- | The numeric core of a retained round. O(1).
engineRoundMetrics ::
  EngineRound group traceEntry evidence ->
  RoundMetrics
engineRoundMetrics =
  roundMetricsFromObservation . roundObservation

-- | The engine's round log: a 'RoundLog' of retained rounds.
type EngineRoundLog group traceEntry evidence =
  RoundLog (EngineRound group traceEntry evidence)

data EngineTraceEntry group traceEntry
  = EngineGateTraceEntry !traceEntry
  | EngineScheduleTraceEntry !(ScheduleTrace group)
  deriving stock (Eq, Show, Read)

emptyEngineRoundLog :: EngineRoundLog group traceEntry evidence
emptyEngineRoundLog = emptyRoundLog

singletonEngineRoundLog ::
  EngineRound group traceEntry evidence ->
  EngineRoundLog group traceEntry evidence
singletonEngineRoundLog = singletonRoundLog

appendEngineRoundLog ::
  EngineRound group traceEntry evidence ->
  EngineRoundLog group traceEntry evidence ->
  EngineRoundLog group traceEntry evidence
appendEngineRoundLog = appendRoundLog

-- | Append a round and re-trim the log's trace entries under the policy.
appendEngineRoundLogWithPolicy ::
  TracePolicy ->
  EngineRound group traceEntry evidence ->
  EngineRoundLog group traceEntry evidence ->
  EngineRoundLog group traceEntry evidence
appendEngineRoundLogWithPolicy tracePolicy roundValue =
  retainEngineRoundLogWithPolicy tracePolicy
    . appendEngineRoundLog roundValue

retainEngineRoundLogWithPolicy ::
  TracePolicy ->
  EngineRoundLog group traceEntry evidence ->
  EngineRoundLog group traceEntry evidence
retainEngineRoundLogWithPolicy =
  retainRoundLogWithPolicy
    engineRoundTraceEntries
    setRoundTraceEntries

engineRoundTraceEntries ::
  EngineRound group traceEntry evidence ->
  [EngineTraceEntry group traceEntry]
engineRoundTraceEntries roundValue =
  let observation =
        roundObservation roundValue
   in fmap EngineGateTraceEntry (obGateTrace observation)
        <> fmap EngineScheduleTraceEntry (obScheduleTrace observation)

setRoundTraceEntries ::
  [EngineTraceEntry group traceEntry] ->
  EngineRound group traceEntry evidence ->
  EngineRound group traceEntry evidence
setRoundTraceEntries traceEntries roundValue =
  let observation =
        roundObservation roundValue
      (!gateTrace, !scheduleTrace) =
        splitEngineTraceEntries traceEntries
   in roundValue
        { roundObservation =
            observation
              { obGateTrace = gateTrace,
                obScheduleTrace = scheduleTrace
              }
        }

splitEngineTraceEntries ::
  [EngineTraceEntry group traceEntry] ->
  ([traceEntry], [ScheduleTrace group])
splitEngineTraceEntries = foldMap splitEngineTraceEntry

splitEngineTraceEntry ::
  EngineTraceEntry group traceEntry ->
  ([traceEntry], [ScheduleTrace group])
splitEngineTraceEntry traceEntry =
  case traceEntry of
    EngineGateTraceEntry gateEntry ->
      ([gateEntry], [])
    EngineScheduleTraceEntry scheduleEntry ->
      ([], [scheduleEntry])

-- | The retained rounds in execution order. O(n).
engineRoundLogRounds ::
  EngineRoundLog group traceEntry evidence ->
  [EngineRound group traceEntry evidence]
engineRoundLogRounds = roundLogRounds

-- | The final outcome of a run.
data EngineReport state group traceEntry evidence = EngineReport
  { erFinalState :: !state,
    erStopReason :: !StopReason,
    erRounds :: ![EngineRound group traceEntry evidence],
    erSchedulerState :: !(SchedulerState group),
    erDynamicPriorityProfile :: !(PriorityProfile group),
    erProgramTrace :: !(Trace RoundSummary)
  }
  deriving stock (Eq, Show)
