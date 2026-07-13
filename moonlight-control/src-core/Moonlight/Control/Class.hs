{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | The control algebra.
--
-- A control program describes /what to attempt and in what order/ over a
-- vocabulary of phases, with modal contexts scoping guidance and scheduling
-- weight over regions of the program. The canonical carrier is
-- 'Moonlight.Control.Program.Program'; every other instance is a fold of
-- that structure, never a rival authority.
--
-- == Laws
--
-- Positive laws (hold in every instance; equality is the instance's
-- observational equality):
--
-- * 'andThen' is associative with 'skip' as left and right identity.
-- * 'orElse' is associative.
-- * @'upTo' 0 x = 'skip'@, @'upTo' n 'skip' = 'skip'@.
-- * @'attempt' 'skip' = 'skip'@.
-- * @'scoped' (a '<>' b) = 'scoped' a . 'scoped' b@ and
--   @'scoped' g 'skip' = 'skip'@ — contexts act on phases only, as a
--   monoid action.
-- * @'scoped' 'mempty'@ is observationally the identity.
--
-- Negative laws (typed obstructions; see
-- @Moonlight.Control.Laws.obstructionLaws@ — these are deliberate and any
-- lawful instance must preserve them):
--
-- * @'orElse' x 'skip' ≠ x@ and @'orElse' 'skip' x ≠ x@ — 'orElse' has
--   /no identity/. A skipped branch is an observable non-progressing
--   alternative: it shifts the chosen-branch index, appears among rejected
--   traces, and a non-progressing right operand discards its state and
--   re-runs from the initial state.
-- * @'attempt' ('attempt' x) ≠ 'attempt' x@ — the nested attempt is
--   trace-observable, though state, report, and verdict agree.
-- * @'upTo' 1 x ≠ x@ — repetition downgrades a continuing disposition to a
--   stop when the counter exhausts and wraps the trace.
-- * @'upTo' m ('upTo' n x) ≠ 'upTo' (m * n) x@ — the inner counter resets
--   per outer iteration.
--
-- Consequently the canonical carrier has no 'Control.Applicative.Alternative'
-- or 'Control.Monad.MonadPlus' instance, deliberately and permanently.
module Moonlight.Control.Class
  ( Control (..),
    sequenceAll,
    choices,
  )
where

import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty (..))
import Numeric.Natural (Natural)

-- | Control programs over a phase vocabulary 'PhaseOf' with modal contexts
-- 'ContextOf'. The 'Monoid' superclass on 'ContextOf' is what makes
-- 'scoped' a monoid action rather than documentation.
--
-- All methods are required to be O(1).
type Control :: Type -> Constraint
class Monoid (ContextOf c) => Control c where
  type PhaseOf c :: Type
  type ContextOf c :: Type

  -- | The empty program: no phases, no progress, identity for 'andThen'. O(1).
  skip :: c

  -- | A single phase. O(1).
  phase :: PhaseOf c -> c

  -- | Sequential composition. Associative; 'skip' is its identity.
  -- A terminal phase short-circuits the remainder. O(1).
  andThen :: c -> c -> c

  -- | Ordered choice: run the left branch; if it neither progresses nor
  -- terminates, discard its state and run the right branch from the initial
  -- state. Associative; has /no identity/ ('skip' branches are observable). O(1).
  orElse :: c -> c -> c

  -- | Bounded repetition: run the body up to @n@ times, stopping early when
  -- an iteration does not continue. @'upTo' 0@ is 'skip'. O(1).
  upTo :: Natural -> c -> c

  -- | Speculative execution: keep the body's outcome if it progressed or
  -- terminated, otherwise roll back to the initial state. O(1).
  attempt :: c -> c

  -- | Scope a modal context over a region. Contexts compose outermost-left
  -- by the 'Monoid' of 'ContextOf' before reaching each phase. O(1).
  scoped :: ContextOf c -> c -> c

-- | Sequence a list of programs left to right. O(n).
sequenceAll :: Control c => [c] -> c
sequenceAll = foldr andThen skip

-- | Ordered choice over a non-empty list of branches. O(n).
choices :: Control c => NonEmpty c -> c
choices (firstBranch :| remainingBranches) =
  case remainingBranches of
    [] -> firstBranch
    nextBranch : restBranches -> orElse firstBranch (choices (nextBranch :| restBranches))
