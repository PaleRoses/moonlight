{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | The canonical interpreter of control programs.
--
-- 'interpret' runs a normalized program against a monadic phase runner. The
-- runner receives the modal context composed from all enclosing
-- 'Moonlight.Control.Class.scoped' regions (outermost-left, by the context
-- 'Monoid'), the phase, and the current state; it returns the (possibly
-- rewritten) phase and an 'Execution'. The machine rebuilds the program on
-- ascent — including scoped regions — so self-rewriting programs round-trip.
--
-- Semantics enforced here, which the algebra's negative laws reflect:
--
-- * A choice branch that neither progresses nor terminates is /rejected/:
--   its state is discarded and the next branch runs from the initial state.
-- * 'Moonlight.Control.Class.attempt' rolls back to the initial state unless
--   the body progressed or terminated.
-- * Repetition stops as soon as an iteration does not continue, and a
--   'Continue' disposition is downgraded to 'Stop' when the counter
--   exhausts.
-- * A 'Terminal' disposition short-circuits the remainder of every
--   enclosing sequence and repetition.
module Moonlight.Control.Machine
  ( Progress (..),
    Disposition (..),
    Verdict (..),
    Execution (..),
    stopVerdict,
    continueVerdict,
    terminalVerdict,
    verdictForProgress,
    progressFromBool,
    progressToBool,
    verdictProgressed,
    verdictContinues,
    verdictTerminal,
    executionProgressed,
    executionContinues,
    executionTerminal,
    executionRetainsState,
    interpret,
    runPhases,
    latestReportOr,
  )
where

import Control.Applicative ((<|>))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Numeric.Natural (Natural)

import Moonlight.Control.Class (Control (..), sequenceAll)
import Moonlight.Control.Program.Internal
  ( Program (..),
    normalize,
    orFromList,
    orSpine,
    seqSpine,
  )
import Moonlight.Control.Trace
  ( ChoiceBranchIndex (..),
    Report (..),
    Trace (..),
    TryOutcome (..),
  )

-- | Whether a region changed the state.
type Progress :: Type
data Progress
  = NoProgress
  | Progressed
  deriving stock (Eq, Ord, Show, Read)

-- | What the region asks of its enclosing context: stop normally, continue
-- iterating, or terminate the whole run.
type Disposition :: Type
data Disposition
  = Stop
  | Continue
  | Terminal
  deriving stock (Eq, Ord, Show, Read)

-- | Progress and disposition of an executed region.
type Verdict :: Type
data Verdict = Verdict
  { verdictProgress :: !Progress,
    verdictDisposition :: !Disposition
  }
  deriving stock (Eq, Ord, Show, Read)

-- | The full outcome of executing a region: resulting state, the most recent
-- phase report, the region's trace, and its verdict.
type Execution :: Type -> Type -> Type -> Type
data Execution state report phaseSummary = Execution
  { seState :: !state,
    seLatestReport :: !(Maybe report),
    seTrace :: !(Trace phaseSummary),
    seVerdict :: !Verdict
  }

type Stack :: Type -> Type -> Type -> Type -> Type -> Type
type Stack ctx phase state report phaseSummary =
  [Frame ctx phase state report phaseSummary]

type Frame :: Type -> Type -> Type -> Type -> Type -> Type
data Frame ctx phase state report phaseSummary
  = SequenceFrame !(SequenceAcc ctx phase report phaseSummary)
  | ChoiceFrame !(ChoiceAcc ctx phase state phaseSummary)
  | RepeatFrame !(RepeatAcc ctx phase report phaseSummary)
  | TryFrame !state
  | ScopedFrame !ctx

type SequenceAcc :: Type -> Type -> Type -> Type -> Type
data SequenceAcc ctx phase report phaseSummary = SequenceAcc
  { seqAccContext :: !ctx,
    seqAccRemaining :: ![Program ctx phase],
    seqAccUpdatedReversed :: ![Program ctx phase],
    seqAccTracesReversed :: ![Trace phaseSummary],
    seqAccLatestReport :: !(Maybe report),
    seqAccVerdict :: !Verdict
  }

type ChoiceAcc :: Type -> Type -> Type -> Type -> Type
data ChoiceAcc ctx phase state phaseSummary = ChoiceAcc
  { choiceAccContext :: !ctx,
    choiceAccBranchIndex :: !Natural,
    choiceAccUpdatedReversed :: ![Program ctx phase],
    choiceAccRejectedReversed :: ![Trace phaseSummary],
    choiceAccRemaining :: ![Program ctx phase],
    choiceAccInitialState :: !state
  }

type RepeatAcc :: Type -> Type -> Type -> Type -> Type
data RepeatAcc ctx phase report phaseSummary = RepeatAcc
  { repeatAccContext :: !ctx,
    repeatAccOriginalCount :: !Natural,
    repeatAccRemainingCount :: !Natural,
    repeatAccTracesReversed :: ![Trace phaseSummary],
    repeatAccLatestReport :: !(Maybe report),
    repeatAccProgress :: !Progress
  }

type Machine :: Type -> Type -> Type -> Type -> Type -> Type
data Machine ctx phase state report phaseSummary
  = Descend
      !(Stack ctx phase state report phaseSummary)
      !ctx
      !(Program ctx phase)
      !state
  | Ascend
      !(Stack ctx phase state report phaseSummary)
      !(Program ctx phase)
      !(Execution state report phaseSummary)

-- | Run a program against a phase runner, starting from the empty modal
-- context. The program is normalized once at entry; the returned program is
-- the (possibly self-rewritten) updated program in normal form.
interpret ::
  (Monad m, Monoid ctx) =>
  (ctx -> phase -> state -> m (Either err (phase, Execution state report phaseSummary))) ->
  Program ctx phase ->
  state ->
  m (Either err (Program ctx phase, Execution state report phaseSummary))
interpret runPhase program initialState =
  runMachine runPhase (Descend [] mempty (normalize program) initialState)
{-# INLINABLE interpret #-}

runMachine ::
  (Monad m, Monoid ctx) =>
  (ctx -> phase -> state -> m (Either err (phase, Execution state report phaseSummary))) ->
  Machine ctx phase state report phaseSummary ->
  m (Either err (Program ctx phase, Execution state report phaseSummary))
runMachine runPhase machineState =
  case machineState of
    Descend frames context program state ->
      descend runPhase frames context program state
    Ascend [] updatedProgram execution ->
      pure (Right (updatedProgram, execution))
    Ascend (frame : frames) updatedProgram execution ->
      runMachine runPhase (resumeFrame frames frame updatedProgram execution)
{-# INLINABLE runMachine #-}

descend ::
  (Monad m, Monoid ctx) =>
  (ctx -> phase -> state -> m (Either err (phase, Execution state report phaseSummary))) ->
  Stack ctx phase state report phaseSummary ->
  ctx ->
  Program ctx phase ->
  state ->
  m (Either err (Program ctx phase, Execution state report phaseSummary))
descend runPhase frames context program state =
  case program of
    Skip ->
      runMachine runPhase (Ascend frames Skip (skipExecution state))
    Phase phaseValue -> do
      phaseResult <-
        runPhase context phaseValue state
      case phaseResult of
        Left err ->
          pure (Left err)
        Right (updatedPhase, phaseExecution) ->
          runMachine runPhase (Ascend frames (Phase updatedPhase) phaseExecution)
    Seq {} ->
      runMachine runPhase (descendSequence frames context (seqSpine program) state)
    Or {} ->
      runMachine runPhase (descendChoice frames context (orSpine program) state)
    UpTo repeatCount body ->
      runMachine runPhase (continueRepeat frames context repeatCount repeatCount body state [] Nothing NoProgress)
    Attempt body ->
      runMachine runPhase (Descend (TryFrame state : frames) context body state)
    Scoped innerContext body ->
      runMachine runPhase (Descend (ScopedFrame innerContext : frames) (context <> innerContext) body state)
{-# INLINABLE descend #-}

descendSequence ::
  Stack ctx phase state report phaseSummary ->
  ctx ->
  NonEmpty (Program ctx phase) ->
  state ->
  Machine ctx phase state report phaseSummary
descendSequence frames context (firstSegment :| remainingSegments) =
  Descend
    ( SequenceFrame
        SequenceAcc
          { seqAccContext = context,
            seqAccRemaining = remainingSegments,
            seqAccUpdatedReversed = [],
            seqAccTracesReversed = [],
            seqAccLatestReport = Nothing,
            seqAccVerdict = stopVerdict NoProgress
          }
        : frames
    )
    context
    firstSegment

descendChoice ::
  Stack ctx phase state report phaseSummary ->
  ctx ->
  NonEmpty (Program ctx phase) ->
  state ->
  Machine ctx phase state report phaseSummary
descendChoice frames context (firstBranch :| remainingBranches) state =
  Descend
    ( ChoiceFrame
        ChoiceAcc
          { choiceAccContext = context,
            choiceAccBranchIndex = 0,
            choiceAccUpdatedReversed = [],
            choiceAccRejectedReversed = [],
            choiceAccRemaining = remainingBranches,
            choiceAccInitialState = state
          }
        : frames
    )
    context
    firstBranch
    state

resumeFrame ::
  Monoid ctx =>
  Stack ctx phase state report phaseSummary ->
  Frame ctx phase state report phaseSummary ->
  Program ctx phase ->
  Execution state report phaseSummary ->
  Machine ctx phase state report phaseSummary
resumeFrame frames frame updatedProgram execution =
  case frame of
    SequenceFrame sequenceAcc ->
      resumeSequence frames sequenceAcc updatedProgram execution
    ChoiceFrame choiceAcc ->
      resumeChoice frames choiceAcc updatedProgram execution
    RepeatFrame repeatAcc ->
      resumeRepeat frames repeatAcc updatedProgram execution
    TryFrame initialState ->
      Ascend
        frames
        (attempt updatedProgram)
        (tryExecution initialState execution)
    ScopedFrame innerContext ->
      Ascend
        frames
        (scoped innerContext updatedProgram)
        execution

resumeSequence ::
  Monoid ctx =>
  Stack ctx phase state report phaseSummary ->
  SequenceAcc ctx phase report phaseSummary ->
  Program ctx phase ->
  Execution state report phaseSummary ->
  Machine ctx phase state report phaseSummary
resumeSequence frames acc updatedProgram execution =
  let !nextUpdatedReversed =
        updatedProgram : seqAccUpdatedReversed acc
      !nextTracesReversed =
        seTrace execution : seqAccTracesReversed acc
      !nextLatestReport =
        seLatestReport execution <|> seqAccLatestReport acc
      !nextVerdict =
        sequenceVerdictStep (seqAccVerdict acc) (seVerdict execution)
      finishSequence finalSegments =
        Ascend
          frames
          (sequenceAll finalSegments)
          Execution
            { seState = seState execution,
              seLatestReport = nextLatestReport,
              seTrace = sequenceTraceFromList (reverse nextTracesReversed),
              seVerdict = nextVerdict
            }
   in case seqAccRemaining acc of
        remainingSegments | verdictTerminal nextVerdict ->
          finishSequence (reverse nextUpdatedReversed <> remainingSegments)
        [] ->
          finishSequence (reverse nextUpdatedReversed)
        nextSegment : restSegments ->
          Descend
            ( SequenceFrame
                acc
                  { seqAccRemaining = restSegments,
                    seqAccUpdatedReversed = nextUpdatedReversed,
                    seqAccTracesReversed = nextTracesReversed,
                    seqAccLatestReport = nextLatestReport,
                    seqAccVerdict = nextVerdict
                  }
                : frames
            )
            (seqAccContext acc)
            nextSegment
            (seState execution)

resumeChoice ::
  Stack ctx phase state report phaseSummary ->
  ChoiceAcc ctx phase state phaseSummary ->
  Program ctx phase ->
  Execution state report phaseSummary ->
  Machine ctx phase state report phaseSummary
resumeChoice frames acc updatedProgram execution =
  let !nextUpdatedReversed =
        updatedProgram : choiceAccUpdatedReversed acc
   in case (executionRetainsState execution, choiceAccRemaining acc) of
        (False, nextBranch : restBranches) ->
          Descend
            ( ChoiceFrame
                acc
                  { choiceAccBranchIndex = choiceAccBranchIndex acc + 1,
                    choiceAccUpdatedReversed = nextUpdatedReversed,
                    choiceAccRejectedReversed = seTrace execution : choiceAccRejectedReversed acc,
                    choiceAccRemaining = restBranches
                  }
                : frames
            )
            (choiceAccContext acc)
            nextBranch
            (choiceAccInitialState acc)
        _ ->
          Ascend
            frames
            (choiceFromList (reverse nextUpdatedReversed <> choiceAccRemaining acc))
            (choiceExecution (choiceAccBranchIndex acc) (choiceAccRejectedReversed acc) execution)

resumeRepeat ::
  Monoid ctx =>
  Stack ctx phase state report phaseSummary ->
  RepeatAcc ctx phase report phaseSummary ->
  Program ctx phase ->
  Execution state report phaseSummary ->
  Machine ctx phase state report phaseSummary
resumeRepeat frames acc updatedProgram execution =
  let !nextTracesReversed =
        seTrace execution : repeatAccTracesReversed acc
      !nextLatestReport =
        seLatestReport execution <|> repeatAccLatestReport acc
      !nextProgress =
        combineProgress (repeatAccProgress acc) (verdictProgress (seVerdict execution))
      finishRepeat disposition =
        Ascend
          frames
          (upTo (repeatAccOriginalCount acc) updatedProgram)
          ( repeatExecution
              (seState execution)
              nextLatestReport
              nextTracesReversed
              nextProgress
              disposition
          )
   in if executionTerminal execution
        then finishRepeat Terminal
        else
          if executionContinues execution
            then
              continueRepeat
                frames
                (repeatAccContext acc)
                (repeatAccOriginalCount acc)
                (repeatAccRemainingCount acc - 1)
                updatedProgram
                (seState execution)
                nextTracesReversed
                nextLatestReport
                nextProgress
            else finishRepeat Stop

continueRepeat ::
  Monoid ctx =>
  Stack ctx phase state report phaseSummary ->
  ctx ->
  Natural ->
  Natural ->
  Program ctx phase ->
  state ->
  [Trace phaseSummary] ->
  Maybe report ->
  Progress ->
  Machine ctx phase state report phaseSummary
continueRepeat frames context originalCount remainingCount currentBody currentState tracesReversed latestReport progress
  | remainingCount == 0 =
      Ascend
        frames
        (upTo originalCount currentBody)
        (repeatExecution currentState latestReport tracesReversed progress Stop)
  | otherwise =
      Descend
        ( RepeatFrame
            RepeatAcc
              { repeatAccContext = context,
                repeatAccOriginalCount = originalCount,
                repeatAccRemainingCount = remainingCount,
                repeatAccTracesReversed = tracesReversed,
                repeatAccLatestReport = latestReport,
                repeatAccProgress = progress
              }
            : frames
        )
        context
        currentBody
        currentState

skipExecution ::
  state ->
  Execution state report phaseSummary
skipExecution state =
  Execution
    { seState = state,
      seLatestReport = Nothing,
      seTrace = SkipTrace,
      seVerdict = stopVerdict NoProgress
    }

sequenceTraceFromList ::
  [Trace phaseSummary] ->
  Trace phaseSummary
sequenceTraceFromList traces =
  case traces of
    [] ->
      SkipTrace
    [singleTrace] ->
      singleTrace
    firstTrace : remainingTraces ->
      SequenceTrace (firstTrace :| remainingTraces)

choiceFromList ::
  [Program ctx phase] ->
  Program ctx phase
choiceFromList branches =
  case branches of
    [] ->
      Skip
    firstBranch : remainingBranches ->
      orFromList (firstBranch :| remainingBranches)

choiceExecution ::
  Natural ->
  [Trace phaseSummary] ->
  Execution state report phaseSummary ->
  Execution state report phaseSummary
choiceExecution branchIndex rejectedReversed chosenExecution =
  chosenExecution
    { seTrace =
        ChoiceTrace
          { ctBranchIndex = ChoiceBranchIndex branchIndex,
            ctRejected = reverse rejectedReversed,
            ctChosen = seTrace chosenExecution
          }
    }

repeatExecution ::
  state ->
  Maybe report ->
  [Trace phaseSummary] ->
  Progress ->
  Disposition ->
  Execution state report phaseSummary
repeatExecution state latestReport reversedTraces progress disposition =
  Execution
    { seState = state,
      seLatestReport = latestReport,
      seTrace = RepeatTrace (reverse reversedTraces),
      seVerdict = Verdict progress disposition
    }

tryExecution ::
  state ->
  Execution state report phaseSummary ->
  Execution state report phaseSummary
tryExecution initialState nestedExecution =
  if executionRetainsState nestedExecution
    then
      nestedExecution
        { seTrace =
            TryTrace
              TryApplied
              (seTrace nestedExecution)
        }
    else
      Execution
        { seState = initialState,
          seLatestReport = Nothing,
          seTrace =
            TryTrace
              TrySkipped
              (seTrace nestedExecution),
          seVerdict = stopVerdict NoProgress
        }

-- | A 'Stop' verdict with the given progress.
stopVerdict :: Progress -> Verdict
stopVerdict = verdictWith Stop

-- | A 'Continue' verdict with the given progress.
continueVerdict :: Progress -> Verdict
continueVerdict = verdictWith Continue

-- | A 'Terminal' verdict with the given progress.
terminalVerdict :: Progress -> Verdict
terminalVerdict = verdictWith Terminal

verdictWith :: Disposition -> Progress -> Verdict
verdictWith disposition progress =
  Verdict
    { verdictProgress = progress,
      verdictDisposition = disposition
    }

-- | 'continueVerdict' 'Progressed' when the flag is set, otherwise
-- 'stopVerdict' 'NoProgress'.
verdictForProgress :: Bool -> Verdict
verdictForProgress progressed =
  if progressed
    then continueVerdict Progressed
    else stopVerdict NoProgress

progressFromBool :: Bool -> Progress
progressFromBool progressed =
  if progressed
    then Progressed
    else NoProgress

progressToBool :: Progress -> Bool
progressToBool progress =
  case progress of
    NoProgress ->
      False
    Progressed ->
      True

verdictProgressed :: Verdict -> Bool
verdictProgressed =
  progressToBool . verdictProgress

verdictContinues :: Verdict -> Bool
verdictContinues verdict =
  verdictDisposition verdict == Continue

verdictTerminal :: Verdict -> Bool
verdictTerminal verdict =
  verdictDisposition verdict == Terminal

executionProgressed ::
  Execution state report phaseSummary ->
  Bool
executionProgressed =
  verdictProgressed . seVerdict

executionContinues ::
  Execution state report phaseSummary ->
  Bool
executionContinues =
  verdictContinues . seVerdict

executionTerminal ::
  Execution state report phaseSummary ->
  Bool
executionTerminal =
  verdictTerminal . seVerdict

-- | Whether the region's outcome is kept by 'Moonlight.Control.Class.attempt'
-- and choice: it progressed or terminated.
executionRetainsState ::
  Execution state report phaseSummary ->
  Bool
executionRetainsState execution =
  executionProgressed execution || executionTerminal execution

combineProgress :: Progress -> Progress -> Progress
combineProgress leftProgress rightProgress =
  case (leftProgress, rightProgress) of
    (Progressed, _) ->
      Progressed
    (_, Progressed) ->
      Progressed
    _ ->
      NoProgress

sequenceVerdictStep :: Verdict -> Verdict -> Verdict
sequenceVerdictStep accumulatedVerdict childVerdict =
  Verdict
    { verdictProgress =
        combineProgress
          (verdictProgress accumulatedVerdict)
          (verdictProgress childVerdict),
      verdictDisposition =
        sequenceDispositionStep
          (verdictDisposition accumulatedVerdict)
          (verdictDisposition childVerdict)
    }

sequenceDispositionStep :: Disposition -> Disposition -> Disposition
sequenceDispositionStep accumulatedDisposition childDisposition =
  case childDisposition of
    Terminal ->
      Terminal
    Continue ->
      Continue
    Stop ->
      case accumulatedDisposition of
        Continue ->
          Continue
        Terminal ->
          Terminal
        Stop ->
          Stop

-- | 'interpret' followed by projection into a 'Report'.
runPhases ::
  (Monad m, Monoid ctx) =>
  Program ctx phase ->
  (ctx -> phase -> state -> m (Either err (phase, Execution state report phaseSummary))) ->
  state ->
  m (Either err (Program ctx phase, Report state report phaseSummary))
runPhases program runPhase initialState = do
  result <-
    interpret runPhase program initialState
  pure
    ( fmap
        ( \(updatedProgram, execution) ->
            ( updatedProgram,
              Report
                { reportState = seState execution,
                  reportTrace = seTrace execution,
                  reportLastPhase = seLatestReport execution
                }
            )
        )
        result
    )
{-# INLINABLE runPhases #-}

-- | The most recent phase report, or the default. O(1).
latestReportOr :: report -> Execution state report phaseSummary -> report
latestReportOr defaultReport =
  fromMaybe defaultReport . seLatestReport
