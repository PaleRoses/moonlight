-- | Execution traces of control programs.
--
-- A 'Trace' mirrors the shape of the program region that produced it:
-- skipped regions, executed phases, sequence segments, the chosen and
-- rejected branches of a choice, repetition iterations, and the outcome of
-- speculative attempts.
module Moonlight.Control.Trace
  ( ChoiceBranchIndex (..),
    TryOutcome (..),
    Trace (..),
    phaseSummaries,
    PhaseSummary (..),
    Report (..),
    totalIterations,
    totalMatchesApplied,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Monoid (Sum (..))
import Numeric.Natural (Natural)

-- | Zero-based index of the chosen branch of a choice.
newtype ChoiceBranchIndex = ChoiceBranchIndex
  { choiceBranchIndexValue :: Natural
  }
  deriving stock (Eq, Ord, Show, Read)

-- | Whether a speculative attempt kept its body's outcome or rolled back.
data TryOutcome
  = TryApplied
  | TrySkipped
  deriving stock (Eq, Ord, Show, Read)

-- | The shape-preserving execution trace of a program region.
data Trace phaseSummary
  = SkipTrace
  | PhaseTrace !phaseSummary
  | SequenceTrace !(NonEmpty (Trace phaseSummary))
  | ChoiceTrace
      { ctBranchIndex :: !ChoiceBranchIndex,
        ctRejected :: ![Trace phaseSummary],
        ctChosen :: !(Trace phaseSummary)
      }
  | RepeatTrace ![Trace phaseSummary]
  | TryTrace
      !TryOutcome
      !(Trace phaseSummary)
  deriving stock (Eq, Ord, Show, Read)

-- | All phase summaries in trace order, including those of rejected choice
-- branches. O(n).
phaseSummaries :: Trace phaseSummary -> [phaseSummary]
phaseSummaries traceValue =
  case traceValue of
    SkipTrace ->
      []
    PhaseTrace phaseSummary ->
      [phaseSummary]
    SequenceTrace nestedTraces ->
      foldMap phaseSummaries nestedTraces
    ChoiceTrace {ctRejected, ctChosen} ->
      foldMap phaseSummaries ctRejected
        <> phaseSummaries ctChosen
    RepeatTrace iterationTraces ->
      foldMap phaseSummaries iterationTraces
    TryTrace _tryOutcome nestedTrace ->
      phaseSummaries nestedTrace

-- | The standard per-phase observation record.
data PhaseSummary budget result annotation = PhaseSummary
  { spsName :: !String,
    spsBudget :: !budget,
    spsUsedGuidance :: !Bool,
    spsResult :: !result,
    spsIterations :: !Int,
    spsMatchesApplied :: !Int,
    spsFactRounds :: !Int,
    spsGuideRounds :: !Int,
    spsProgressed :: !Bool,
    spsAnnotation :: !(Maybe annotation)
  }
  deriving stock (Eq, Ord, Show, Read)

-- | Final state, full trace, and the last phase report of a completed run.
data Report state report phaseSummary = Report
  { reportState :: !state,
    reportTrace :: !(Trace phaseSummary),
    reportLastPhase :: !(Maybe report)
  }

-- | Total iterations across all executed phases. O(n).
totalIterations ::
  Report state report (PhaseSummary budget result annotation) ->
  Int
totalIterations =
  getSum . foldMap (Sum . spsIterations) . phaseSummaries . reportTrace

-- | Total matches applied across all executed phases. O(n).
totalMatchesApplied ::
  Report state report (PhaseSummary budget result annotation) ->
  Int
totalMatchesApplied =
  getSum . foldMap (Sum . spsMatchesApplied) . phaseSummaries . reportTrace
