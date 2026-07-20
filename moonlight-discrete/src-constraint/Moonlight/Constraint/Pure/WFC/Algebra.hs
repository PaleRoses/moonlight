module Moonlight.Constraint.Pure.WFC.Algebra
  ( assignSlot,
    completeAssignment,
    propagateCSP,
    selectNextSlot,
    slotCandidates,
  )
where

import qualified Data.Map.Strict as Map
import Moonlight.Constraint.Pure.CSP
  ( ConstraintSatisfactionProblem (..),
    domainCardinality,
    domainMember,
    domainSingleton,
    domainSingletonValue,
    domainToAscList,
    lookupDomain,
    mac3,
  )
import Moonlight.Constraint.Pure.WFC.Types
  ( SlotId,
    WFCError (..),
  )

assignSlot ::
  (Ord slot, Ord value) =>
  SlotId slot ->
  value ->
  ConstraintSatisfactionProblem (SlotId slot) value ->
  Either (WFCError slot) (ConstraintSatisfactionProblem (SlotId slot) value)
assignSlot slotId value problem =
  case Map.lookup slotId (cspDomains problem) of
    Nothing -> Left (WFCAssignmentMissingSlot slotId)
    Just currentDomain
      | domainMember value currentDomain ->
          Right
            problem
              { cspDomains =
                  Map.insert slotId (domainSingleton value) (cspDomains problem)
              }
      | otherwise -> Left (WFCAssignmentValueOutsideDomain slotId)

completeAssignment ::
  ConstraintSatisfactionProblem (SlotId slot) value ->
  Maybe (Map.Map (SlotId slot) value)
completeAssignment =
  traverse domainSingletonValue . cspDomains

propagateCSP ::
  (Ord slot, Ord value) =>
  ConstraintSatisfactionProblem (SlotId slot) value ->
  Either (WFCError slot) (Maybe (ConstraintSatisfactionProblem (SlotId slot) value))
propagateCSP problem =
  case mac3 problem of
    Left err -> Left (WFCCSPError err)
    Right result -> Right result

selectNextSlot ::
  ConstraintSatisfactionProblem (SlotId slot) value ->
  Maybe (SlotId slot)
selectNextSlot problem =
  fmap fst $
    foldr chooseCandidate Nothing $
      fmap
        (\(slotId, domainValue) -> (slotId, domainCardinality domainValue))
        (Map.toAscList (cspDomains problem))
  where
    chooseCandidate :: (SlotId slot, Int) -> Maybe (SlotId slot, Int) -> Maybe (SlotId slot, Int)
    chooseCandidate candidate best =
      let (_, candidateCardinality) = candidate
       in if candidateCardinality <= 1
            then best
            else case best of
              Nothing -> Just candidate
              Just (_, bestCardinality)
                | candidateCardinality <= bestCardinality -> Just candidate
                | otherwise -> best

slotCandidates ::
  Ord slot =>
  ConstraintSatisfactionProblem (SlotId slot) value ->
  SlotId slot ->
  Either (WFCError slot) [value]
slotCandidates problem slotId =
  case lookupDomain problem slotId of
    Left err -> Left (WFCCSPError err)
    Right domainValue -> Right (domainToAscList domainValue)
