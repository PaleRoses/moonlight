module Moonlight.Constraint.Pure.WFC.Search
  ( SearchContext (..),
    searchWithContext,
    consumeBacktrack,
  )
where

import Data.Kind (Type)
import Data.Word (Word32)
import Moonlight.Constraint.Pure.CSP (ConstraintSatisfactionProblem)
import Moonlight.Constraint.Pure.WFC.Algebra
  ( assignSlot,
    completeAssignment,
    propagateCSP,
  )
import Moonlight.Constraint.Pure.WFC.Types
  ( SlotId,
    WFCError,
    WFCSearchResult (..),
  )

type SearchContext :: Type -> Type -> Type -> Type
data SearchContext context slot value = SearchContext
  { searchSelectSlot ::
      context ->
      ConstraintSatisfactionProblem (SlotId slot) value ->
      Maybe (SlotId slot),
    searchCandidates ::
      context ->
      ConstraintSatisfactionProblem (SlotId slot) value ->
      SlotId slot ->
      Either (WFCError slot) ([value], context)
  }

searchWithContext ::
  (Ord slot, Ord value) =>
  SearchContext context slot value ->
  Word32 ->
  context ->
  ConstraintSatisfactionProblem (SlotId slot) value ->
  Either (WFCError slot) (Word32, context, WFCSearchResult slot value)
searchWithContext searchContext remainingBacktracks context problem =
  case completeAssignment problem of
    Just assignment ->
      Right (remainingBacktracks, context, WFCSolved assignment)
    Nothing ->
      case searchSelectSlot searchContext context problem of
        Nothing ->
          Right (remainingBacktracks, context, WFCUnsatisfiable)
        Just slotId -> do
          (candidates, nextContext) <-
            searchCandidates searchContext context problem slotId
          exploreCandidates remainingBacktracks nextContext problem slotId candidates

  where
    exploreCandidates backtrackBudget currentContext currentProblem slotId candidates =
      case candidates of
        [] ->
          Right (backtrackBudget, currentContext, WFCUnsatisfiable)
        candidate : remainingCandidates -> do
          assignedProblem <- assignSlot slotId candidate currentProblem
          propagated <- propagateCSP assignedProblem
          case propagated of
            Nothing ->
              advanceAfterFailure
                backtrackBudget
                currentContext
                currentProblem
                slotId
                remainingCandidates
            Just propagatedProblem -> do
              (remainingAfterBranch, contextAfterBranch, branchResult) <-
                searchWithContext
                  searchContext
                  backtrackBudget
                  currentContext
                  propagatedProblem
              case branchResult of
                WFCSolved assignment ->
                  Right (remainingAfterBranch, contextAfterBranch, WFCSolved assignment)
                WFCBacktrackLimitReached ->
                  Right (remainingAfterBranch, contextAfterBranch, WFCBacktrackLimitReached)
                WFCUnsatisfiable ->
                  advanceAfterFailure
                    remainingAfterBranch
                    contextAfterBranch
                    currentProblem
                    slotId
                    remainingCandidates

    advanceAfterFailure backtrackBudget currentContext currentProblem slotId remainingCandidates =
      case remainingCandidates of
        [] ->
          Right (backtrackBudget, currentContext, WFCUnsatisfiable)
        _ ->
          case consumeBacktrack backtrackBudget of
            Nothing ->
              Right (0, currentContext, WFCBacktrackLimitReached)
            Just remainingAfterBacktrack ->
              exploreCandidates
                remainingAfterBacktrack
                currentContext
                currentProblem
                slotId
                remainingCandidates

consumeBacktrack :: Word32 -> Maybe Word32
consumeBacktrack remainingBacktracks =
  case remainingBacktracks of
    0 -> Nothing
    _ -> Just (remainingBacktracks - 1)
