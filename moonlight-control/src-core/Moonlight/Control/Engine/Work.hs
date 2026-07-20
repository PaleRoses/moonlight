-- | The work boundary of the engine: how the engine sees a domain. A
-- 'WorkSource' produces candidate spaces from state and applies scheduled
-- batches; everything else — gating, scheduling, evidence, stopping — is
-- engine-owned.
module Moonlight.Control.Engine.Work
  ( ApplyResult (..),
    applyResult,
    WorkSource (..),
  )
where

import Numeric.Natural (Natural)

import Moonlight.Control.Candidate
  ( CandidateSpace,
    ScheduledBatch,
  )

-- | The outcome of applying a scheduled batch.
data ApplyResult state evidence = ApplyResult
  { arState :: !state,
    arEvidence :: !evidence,
    arAppliedCount :: !Natural
  }
  deriving stock (Eq, Ord, Show, Read)

-- | Build an 'ApplyResult', clamping the applied count to non-negative. O(1).
applyResult ::
  state ->
  evidence ->
  Int ->
  ApplyResult state evidence
applyResult state evidence appliedCount =
  ApplyResult
    { arState = state,
      arEvidence = evidence,
      arAppliedCount = fromIntegral (max 0 appliedCount)
    }

-- | A domain seen as schedulable work.
data WorkSource m state view group match evidence err = WorkSource
  { wsView :: state -> view,
    wsCandidateSpace :: state -> m (CandidateSpace m group () match),
    wsApplyScheduled :: ScheduledBatch group match -> state -> m (Either err (ApplyResult state evidence)),
    wsProgressed :: evidence -> Bool
  }
