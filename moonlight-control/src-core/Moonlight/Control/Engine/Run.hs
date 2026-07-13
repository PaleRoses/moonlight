{-# LANGUAGE BangPatterns #-}

-- | The flagship interpretation of the control algebra: each 'PhaseDecl'
-- phase of the plan's program runs one gated, scheduled, evidence-fed round
-- against a 'WorkSource'.
--
-- The machine composes the enclosing scoped modalities into the modality
-- each round receives: its gate wraps the candidate space, its weight merges
-- into the scheduler configuration after the dynamic profile.
--
-- Engine execution is monadic, but 'Moonlight.Control.Class.attempt'
-- remains state-value rollback only. If a phase performs external effects
-- before returning no-progress control, those effects are not undone by the
-- machine. Effectful sources that need transactional attempts must provide
-- that transactionality inside their own boundary.
module Moonlight.Control.Engine.Run
  ( EngineFailure (..),
    EngineRuntime (..),
    EngineRoundResult (..),
    initialEngineRuntime,
    runEngineRound,
    runEngine,
  )
where

import Data.Foldable qualified as Foldable
import Data.Maybe (fromMaybe)

import Moonlight.Control.Candidate
  ( candidateSpaceAvailableCount,
  )
import Moonlight.Control.Engine.Evidence
  ( EvidencePolicy (..),
    applyEvidencePolicies,
  )
import Moonlight.Control.Engine.Plan
  ( PhaseDecl (..),
    Plan (..),
    roundBudgetNatural,
    stopAfterRound,
  )
import Moonlight.Control.Engine.Report
  ( EngineReport (..),
    EngineRound (..),
    EngineRoundLog,
    Observation (..),
    RoundMetrics (..),
    RoundSummary (..),
    StopReason (..),
    appendEngineRoundLogWithPolicy,
    emptyEngineRoundLog,
    engineRoundLogRounds,
    engineRoundMetrics,
  )
import Moonlight.Control.Engine.Work
  ( ApplyResult (..),
    WorkSource (..),
  )
import Moonlight.Control.Gate
  ( GateCompatibilityError,
    GatePullTrace (..),
    gateCandidateSpace,
    validateGateScheduler,
  )
import Moonlight.Control.Machine
  ( Execution (..),
    Progress (..),
    continueVerdict,
    progressFromBool,
    runPhases,
    terminalVerdict,
  )
import Moonlight.Control.Modality
  ( Modality (..),
  )
import Moonlight.Control.Program
  ( programContexts,
  )
import Moonlight.Control.Schedule
  ( SchedulerConfig (..),
    TracePolicy (..),
    canonicalTracePolicy,
    mergePriorityProfile,
    traceLastEntries,
  )
import Moonlight.Control.Schedule.Round
  ( ScheduleOutcome (..),
    SchedulerState,
    emptySchedulerState,
    replaceSchedulerTraceDelta,
    scheduleCandidateSpace,
  )
import Moonlight.Control.Trace
  ( Report (..),
    Trace (..),
  )
import Moonlight.Control.Weight
  ( PriorityProfile,
    emptyPriorityProfile,
  )

-- | A typed engine failure: an incompatible gate, or a failed batch
-- application.
data EngineFailure err group
  = EngineGateIncompatible !(GateCompatibilityError group)
  | EngineApplyFailed !err
  deriving stock (Eq, Show)

-- | The engine state threaded between rounds.
data EngineRuntime group traceEntry evidence = EngineRuntime
  { ertRoundIndex :: !Int,
    ertSchedulerState :: !(SchedulerState group),
    ertDynamicPriorityProfile :: !(PriorityProfile group),
    ertRoundLog :: !(EngineRoundLog group traceEntry evidence)
  }
  deriving stock (Eq, Show)

data EngineRunState state group traceEntry evidence = EngineRunState
  { ersEngineState :: !state,
    ersRuntime :: !(EngineRuntime group traceEntry evidence),
    ersStopReason :: !(Maybe StopReason)
  }

data ExecutedRound state group traceEntry evidence = ExecutedRound
  { exNextState :: !state,
    exSchedulerState :: !(SchedulerState group),
    exDynamicPriorityProfile :: !(PriorityProfile group),
    exTracePolicy :: !TracePolicy,
    exRound :: !(EngineRound group traceEntry evidence)
  }

-- | The outcome of one externally driven round.
data EngineRoundResult state group traceEntry evidence = EngineRoundResult
  { rrState :: !state,
    rrRuntime :: !(EngineRuntime group traceEntry evidence),
    rrRound :: !(EngineRound group traceEntry evidence)
  }
  deriving stock (Eq, Show)

initialEngineRuntime :: EngineRuntime group traceEntry evidence
initialEngineRuntime =
  EngineRuntime
    { ertRoundIndex = 0,
      ertSchedulerState = emptySchedulerState,
      ertDynamicPriorityProfile = emptyPriorityProfile,
      ertRoundLog = emptyEngineRoundLog
    }

-- | Run one round under an explicit modality, outside the machine.
runEngineRound ::
  (Monad m, Ord group) =>
  Plan view group match traceEntry evidence ->
  Modality view group match traceEntry group ->
  PhaseDecl ->
  WorkSource m state view group match evidence err ->
  EngineRuntime group traceEntry evidence ->
  state ->
  m (Either (EngineFailure err group) (EngineRoundResult state group traceEntry evidence))
runEngineRound plan modality decl source runtime state =
  case validateGateScheduler (modalityGate modality) (planInitialSchedulerConfig plan) of
    Left gateError ->
      pure (Left (EngineGateIncompatible gateError))
    Right () -> do
      let runState =
            EngineRunState
              { ersEngineState = state,
                ersRuntime = runtime,
                ersStopReason = Nothing
              }
      executedResult <-
        executeEngineRound plan source modality decl runState
      pure
        ( fmap
            (engineRoundResultFromExecuted plan decl runState)
            executedResult
        )

-- | Interpret the plan's program against the source, running each phase as
-- one round until the program completes or the stop policy terminates it.
runEngine ::
  (Monad m, Ord group) =>
  Plan view group match traceEntry evidence ->
  WorkSource m state view group match evidence err ->
  state ->
  m (Either (EngineFailure err group) (EngineReport state group traceEntry evidence))
runEngine plan source initialState =
  case validateEnginePlan plan of
    Left gateError ->
      pure (Left (EngineGateIncompatible gateError))
    Right () -> do
      phasesResult <-
        runPhases
          (planProgram plan)
          (runEnginePhase plan source)
          (EngineRunState initialState initialEngineRuntime Nothing)
      pure
        ( fmap
            ( \(_updatedProgram, report) ->
                engineReportFromRunState
                  (reportTrace report)
                  (reportState report)
            )
            phasesResult
        )
{-# INLINABLE runEngine #-}

validateEnginePlan ::
  Plan view group match traceEntry evidence ->
  Either (GateCompatibilityError group) ()
validateEnginePlan plan =
  Foldable.traverse_
    ( \modality ->
        validateGateScheduler
          (modalityGate modality)
          (planInitialSchedulerConfig plan)
    )
    (programContexts (planProgram plan))

runEnginePhase ::
  (Monad m, Ord group) =>
  Plan view group match traceEntry evidence ->
  WorkSource m state view group match evidence err ->
  Modality view group match traceEntry group ->
  PhaseDecl ->
  EngineRunState state group traceEntry evidence ->
  m
    ( Either
        (EngineFailure err group)
        ( PhaseDecl,
          Execution
            (EngineRunState state group traceEntry evidence)
            (EngineRound group traceEntry evidence)
            RoundSummary
        )
    )
runEnginePhase plan source modality decl runState =
  case ersStopReason runState of
    Just _stopReason ->
      pure
        ( Right
            ( decl,
              Execution
                { seState = runState,
                  seLatestReport = Nothing,
                  seTrace = SkipTrace,
                  seVerdict = terminalVerdict NoProgress
                }
            )
        )
    Nothing -> do
      executedResult <-
        executeEngineRound plan source modality decl runState
      pure
        ( fmap
            (finalizeExecutedRound plan decl runState)
            executedResult
        )
{-# INLINABLE runEnginePhase #-}

finalizeExecutedRound ::
  Plan view group match traceEntry evidence ->
  PhaseDecl ->
  EngineRunState state group traceEntry evidence ->
  ExecutedRound state group traceEntry evidence ->
  ( PhaseDecl,
    Execution
      (EngineRunState state group traceEntry evidence)
      (EngineRound group traceEntry evidence)
      RoundSummary
  )
finalizeExecutedRound plan decl runState executed =
  let roundWithoutStop = exRound executed
      observation = roundObservation roundWithoutStop
      stopReason = stopAfterRound (planStopPolicy plan) observation
      roundWithStop = roundWithoutStop {roundStopReason = stopReason}
      previousRuntime = ersRuntime runState
      nextRuntime =
        EngineRuntime
          { ertRoundIndex = ertRoundIndex previousRuntime + 1,
            ertSchedulerState = exSchedulerState executed,
            ertDynamicPriorityProfile = exDynamicPriorityProfile executed,
            ertRoundLog =
              appendEngineRoundLogWithPolicy
                (exTracePolicy executed)
                roundWithStop
                (ertRoundLog previousRuntime)
          }
      nextRunState =
        EngineRunState
          { ersEngineState = exNextState executed,
            ersRuntime = nextRuntime,
            ersStopReason = stopReason
          }
      progress =
        progressFromBool (obProgressed observation)
      verdict =
        case stopReason of
          Nothing ->
            continueVerdict progress
          Just _ ->
            terminalVerdict progress
   in ( decl,
        Execution
          { seState = nextRunState,
            seLatestReport = Just roundWithStop,
            seTrace = PhaseTrace (roundSummary decl roundWithStop),
            seVerdict = verdict
          }
      )

executeEngineRound ::
  (Monad m, Ord group) =>
  Plan view group match traceEntry evidence ->
  WorkSource m state view group match evidence err ->
  Modality view group match traceEntry group ->
  PhaseDecl ->
  EngineRunState state group traceEntry evidence ->
  m (Either (EngineFailure err group) (ExecutedRound state group traceEntry evidence))
executeEngineRound plan source modality decl runState = do
  rawCandidateSpace <- wsCandidateSpace source state
  rawCandidateCount <- candidateSpaceAvailableCount rawCandidateSpace

  let view = wsView source state
      gatedSpace = gateCandidateSpace (modalityGate modality) view rawCandidateSpace
      reportSchedulerConfig = currentSchedulerConfig plan runtime modality
      reportTracePolicy = scTracePolicy reportSchedulerConfig
      scheduleTracePolicy = schedulerTracePolicyForEvidence (planEvidencePolicies plan) reportTracePolicy
      schedulerConfig =
        reportSchedulerConfig
          { scTracePolicy = scheduleTracePolicy
          }

  gatedCandidateCount <- candidateSpaceAvailableCount gatedSpace
  scheduleOutcome <-
    scheduleCandidateSpace
      schedulerConfig
      (roundBudgetNatural (fromMaybe (planRoundBudget plan) (pdBudget decl)))
      (ertRoundIndex runtime)
      gatedSpace
      (ertSchedulerState runtime)

  applyOutcome <-
    wsApplyScheduled source (soScheduledBatch scheduleOutcome) state

  pure $
    case applyOutcome of
      Left err ->
        Left (EngineApplyFailed err)
      Right applied ->
        let !appliedCount = arAppliedCount applied
            evidence = arEvidence applied
            !progressed = wsProgressed source evidence
            pullTrace = soPullMeta scheduleOutcome
            !coverage = soCoverage scheduleOutcome <> gptCoverage pullTrace
            fullGateTrace =
              gptTrace pullTrace
            reportGateTrace =
              retainedReportTraceDelta reportTracePolicy fullGateTrace
            fullScheduleTrace =
              soSchedulerTraceDelta scheduleOutcome
            reportScheduleTrace =
              retainedReportTraceDelta reportTracePolicy fullScheduleTrace
            baseObservation =
              Observation
                { obRound = ertRoundIndex runtime,
                  obCandidateCount = rawCandidateCount,
                  obGatedCount = gatedCandidateCount,
                  obScheduledCount = soScheduledCount scheduleOutcome,
                  obAppliedCount = appliedCount,
                  obDroppedByGate = gptRejectedCount pullTrace,
                  obSuppressedCount = soSuppressedCount scheduleOutcome,
                  obDeferredByBudgetCount = soDeferredByBudgetCount scheduleOutcome,
                  obCoverage = coverage,
                  obProgressed = progressed,
                  obEvidence = evidence,
                  obGateTrace = fullGateTrace,
                  obScheduleTrace = fullScheduleTrace
                }
            reportObservation =
              baseObservation
                { obGateTrace = reportGateTrace,
                  obScheduleTrace = reportScheduleTrace
                }
            roundValue =
              EngineRound
                { roundObservation = reportObservation,
                  roundStopReason = Nothing
                }
            nextDynamicPriority =
              applyEvidencePolicies
                (planEvidencePolicies plan)
                baseObservation
                (ertDynamicPriorityProfile runtime)
            reportSchedulerState =
              replaceSchedulerTraceDelta
                reportTracePolicy
                (ertSchedulerState runtime)
                fullScheduleTrace
                (soSchedulerState scheduleOutcome)
         in Right
              ExecutedRound
                { exNextState = arState applied,
                  exSchedulerState = reportSchedulerState,
                  exDynamicPriorityProfile = nextDynamicPriority,
                  exTracePolicy = reportTracePolicy,
                  exRound = roundValue
                }
  where
    state = ersEngineState runState
    runtime = ersRuntime runState
{-# INLINABLE executeEngineRound #-}

currentSchedulerConfig ::
  Ord group =>
  Plan view group match traceEntry evidence ->
  EngineRuntime group traceEntry evidence ->
  Modality view group match traceEntry group ->
  SchedulerConfig group
currentSchedulerConfig plan runtime modality =
  mergePriorityProfile
    (modalityWeight modality)
    ( mergePriorityProfile
        (ertDynamicPriorityProfile runtime)
        (planInitialSchedulerConfig plan)
    )

schedulerTracePolicyForEvidence ::
  [EvidencePolicy source group] ->
  TracePolicy ->
  TracePolicy
schedulerTracePolicyForEvidence evidencePolicies reportTracePolicy =
  case (any epNeedsScheduleTrace evidencePolicies, canonicalTracePolicy reportTracePolicy) of
    (True, NoTrace) -> traceLastEntries 1
    _ -> reportTracePolicy

retainedReportTraceDelta ::
  TracePolicy ->
  [traceEntry] ->
  [traceEntry]
retainedReportTraceDelta reportTracePolicy traceDelta =
  case canonicalTracePolicy reportTracePolicy of
    NoTrace -> []
    _ -> traceDelta

engineRoundResultFromExecuted ::
  Plan view group match traceEntry evidence ->
  PhaseDecl ->
  EngineRunState state group traceEntry evidence ->
  ExecutedRound state group traceEntry evidence ->
  EngineRoundResult state group traceEntry evidence
engineRoundResultFromExecuted plan decl runState executed =
  let (_decl, execution) =
        finalizeExecutedRound plan decl runState executed
      nextRunState = seState execution
      roundValue =
        case seLatestReport execution of
          Nothing -> exRound executed
          Just finalizedRound -> finalizedRound
   in EngineRoundResult
        { rrState = ersEngineState nextRunState,
          rrRuntime = ersRuntime nextRunState,
          rrRound = roundValue
        }

roundSummary ::
  PhaseDecl ->
  EngineRound group traceEntry evidence ->
  RoundSummary
roundSummary decl roundValue =
  let metrics = engineRoundMetrics roundValue
   in RoundSummary
        { rsPhaseName = pdName decl,
          rsRound = rmRound metrics,
          rsCandidateCount = rmCandidateCount metrics,
          rsGatedCount = rmGatedCount metrics,
          rsScheduledCount = rmScheduledCount metrics,
          rsAppliedCount = rmAppliedCount metrics,
          rsSuppressedCount = rmSuppressedCount metrics,
          rsDeferredByBudgetCount = rmDeferredByBudgetCount metrics,
          rsCoverage = rmCoverage metrics,
          rsProgressed = rmProgressed metrics,
          rsStopReason = roundStopReason roundValue
        }

engineReportFromRunState ::
  Trace RoundSummary ->
  EngineRunState state group traceEntry evidence ->
  EngineReport state group traceEntry evidence
engineReportFromRunState programTrace runState =
  let runtime = ersRuntime runState
   in EngineReport
        { erFinalState = ersEngineState runState,
          erStopReason =
            fromMaybe ProgramCompleted (ersStopReason runState),
          erRounds =
            engineRoundLogRounds (ertRoundLog runtime),
          erSchedulerState =
            ertSchedulerState runtime,
          erDynamicPriorityProfile =
            ertDynamicPriorityProfile runtime,
          erProgramTrace =
            programTrace
        }
