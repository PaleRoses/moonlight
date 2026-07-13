-- | The executable plan of the engine: a control 'Program' over 'PhaseDecl'
-- payloads with 'Moonlight.Control.Modality.Modality' contexts, an initial
-- scheduler configuration, a default round budget, a stop policy, and
-- evidence policies.
--
-- A phase carries only its name and an optional budget. Gates and scheduling
-- weights are not phase properties: they are scoped over program regions
-- with 'Moonlight.Control.Modality.gated' and
-- 'Moonlight.Control.Modality.weighted', and the machine composes the
-- enclosing scopes into the modality each round receives.
module Moonlight.Control.Engine.Plan
  ( RoundLimit,
    roundLimit,
    canonicalRoundLimit,
    roundLimitValue,
    RoundBudget,
    roundBudget,
    canonicalRoundBudget,
    roundBudgetValue,
    roundBudgetNatural,
    PhaseDecl (..),
    phaseDecl,
    EngineProgram,
    StopPolicy (..),
    neverStopPolicy,
    fixedPointStopPolicy,
    Plan (..),
    singletonPlan,
  )
where

import Numeric.Natural (Natural)

import Moonlight.Control.Class
  ( phase,
  )
import Moonlight.Control.Count
  ( WorkCoverage (..),
    workCountKnownZero,
    workCountMayBePositive,
  )
import Moonlight.Control.Engine.Evidence
  ( EvidencePolicy,
  )
import Moonlight.Control.Engine.Report
  ( Observation (..),
    StopReason (..),
  )
import Moonlight.Control.Modality
  ( Modality,
  )
import Moonlight.Control.Program
  ( Program,
  )
import Moonlight.Control.Schedule
  ( SchedulerConfig,
  )

-- | A validated positive bound on the number of rounds.
newtype RoundLimit = RoundLimit
  { roundLimitValue :: Int
  }
  deriving stock (Eq, Ord, Show)

-- | 'Just' a limit when positive. O(1).
roundLimit :: Int -> Maybe RoundLimit
roundLimit rawLimit = if rawLimit > 0 then Just (RoundLimit rawLimit) else Nothing

-- | Clamp to at least one round. O(1).
canonicalRoundLimit :: Int -> RoundLimit
canonicalRoundLimit = RoundLimit . max 1

-- | A validated positive per-round match budget.
newtype RoundBudget = RoundBudget
  { roundBudgetValue :: Int
  }
  deriving stock (Eq, Ord, Show)

-- | 'Just' a budget when positive. O(1).
roundBudget :: Int -> Maybe RoundBudget
roundBudget rawBudget = if rawBudget > 0 then Just (RoundBudget rawBudget) else Nothing

-- | Clamp to a budget of at least one. O(1).
canonicalRoundBudget :: Int -> RoundBudget
canonicalRoundBudget = RoundBudget . max 1

roundBudgetNatural :: RoundBudget -> Natural
roundBudgetNatural = fromIntegral . roundBudgetValue

-- | One engine phase: a name, and an optional budget overriding the plan's
-- default.
data PhaseDecl = PhaseDecl
  { pdName :: !String,
    pdBudget :: !(Maybe RoundBudget)
  }
  deriving stock (Eq, Ord, Show)

phaseDecl :: String -> Maybe RoundBudget -> PhaseDecl
phaseDecl phaseName budget =
  PhaseDecl
    { pdName = phaseName,
      pdBudget = budget
    }

-- | The program type the engine interprets.
type EngineProgram view group match traceEntry =
  Program (Modality view group match traceEntry group) PhaseDecl

-- | A per-round stop decision over the round's observation.
newtype StopPolicy observation = StopPolicy
  { stopAfterRound :: observation -> Maybe StopReason
  }

neverStopPolicy :: StopPolicy observation
neverStopPolicy = StopPolicy (const Nothing)

-- | Stop at the round limit, on provably exhausted work, or on complete
-- non-progress. Suppressed, deferred, or incompletely covered work defers
-- the decision to a later round.
fixedPointStopPolicy ::
  RoundLimit ->
  StopPolicy (Observation group traceEntry evidence)
fixedPointStopPolicy limit =
  StopPolicy $ \observation ->
    if obRound observation >= roundLimitValue limit - 1
      then Just RoundLimitReached
      else
        if obScheduledCount observation == 0
          then stopForUnscheduled observation
          else
            if not (obProgressed observation)
              then stopForNonProgress observation
              else Nothing

stopForUnscheduled :: Observation group traceEntry evidence -> Maybe StopReason
stopForUnscheduled observation
  | workCountKnownZero (obCandidateCount observation) =
      Just NoCandidateWork
  | workCountMayBePositive (obSuppressedCount observation)
      || workCountMayBePositive (obDeferredByBudgetCount observation) =
      Nothing
  | obCoverage observation /= WorkCoverageComplete =
      Nothing
  | otherwise =
      Just NoRunnableWork

stopForNonProgress :: Observation group traceEntry evidence -> Maybe StopReason
stopForNonProgress observation
  | workCountMayBePositive (obDeferredByBudgetCount observation)
      || workCountMayBePositive (obSuppressedCount observation) =
      Nothing
  | obCoverage observation /= WorkCoverageComplete =
      Nothing
  | otherwise =
      Just Converged

-- | Everything the engine needs besides the 'WorkSource' and the state.
data Plan view group match traceEntry evidence = Plan
  { planInitialSchedulerConfig :: !(SchedulerConfig group),
    planProgram :: !(EngineProgram view group match traceEntry),
    planRoundBudget :: !RoundBudget,
    planStopPolicy :: !(StopPolicy (Observation group traceEntry evidence)),
    planEvidencePolicies :: ![EvidencePolicy (Observation group traceEntry evidence) group]
  }

-- | A plan of one unscoped phase. O(1).
singletonPlan ::
  Ord group =>
  SchedulerConfig group ->
  RoundBudget ->
  PhaseDecl ->
  StopPolicy (Observation group traceEntry evidence) ->
  Plan view group match traceEntry evidence
singletonPlan schedulerConfig budget decl stopPolicy =
  Plan
    { planInitialSchedulerConfig = schedulerConfig,
      planProgram = phase decl,
      planRoundBudget = budget,
      planStopPolicy = stopPolicy,
      planEvidencePolicies = []
    }
