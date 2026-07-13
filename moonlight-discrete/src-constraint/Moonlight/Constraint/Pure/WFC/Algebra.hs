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
    Domain,
    domainNull,
    domainSingleton,
    domainToAscList,
    lookupDomain,
    mac3,
  )
import Moonlight.Constraint.Pure.WFC.Types
  ( SlotId,
    WFCError (..),
  )

assignSlot ::
  Ord slot =>
  SlotId slot ->
  value ->
  ConstraintSatisfactionProblem (SlotId slot) value ->
  ConstraintSatisfactionProblem (SlotId slot) value
assignSlot slotId value problem =
  problem
    { cspDomains =
        Map.insert slotId (domainSingleton value) (cspDomains problem)
    }

completeAssignment ::
  ConstraintSatisfactionProblem (SlotId slot) value ->
  Maybe (Map.Map (SlotId slot) value)
completeAssignment =
  traverse singletonDomainValue . cspDomains

propagateCSP ::
  (Ord slot, Ord value) =>
  ConstraintSatisfactionProblem (SlotId slot) value ->
  Either (WFCError slot) (Maybe (ConstraintSatisfactionProblem (SlotId slot) value))
propagateCSP problem =
  if any domainNull (Map.elems (cspDomains problem))
    then pure Nothing
    else
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
        (\(slotId, domainValue) -> (slotId, length (domainToAscList domainValue)))
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

singletonDomainValue :: Domain value -> Maybe value
singletonDomainValue domainValue =
  case domainToAscList domainValue of
    [value] -> Just value
    _ -> Nothing
