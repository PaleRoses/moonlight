{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Constraint.Pure.CSP
  ( Domain,
    domainFromList,
    domainFromSet,
    domainSingleton,
    domainToAscList,
    domainNull,
    Arc (..),
    reverseArc,
    BinaryConstraint (..),
    ConstraintSatisfactionProblem (..),
    CSPError (..),
    allArcs,
    lookupDomain,
    revise,
    mac3,
  )
where

import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)

type Domain :: Type -> Type
newtype Domain value = Domain
  { unDomain :: Set value
  }
  deriving stock (Eq, Ord, Show, Read)

domainFromList :: Ord value => [value] -> Domain value
domainFromList = domainFromSet . Set.fromList

domainFromSet :: Set value -> Domain value
domainFromSet = Domain

domainSingleton :: value -> Domain value
domainSingleton = Domain . Set.singleton

domainToAscList :: Domain value -> [value]
domainToAscList = Set.toAscList . unDomain

domainNull :: Domain value -> Bool
domainNull = Set.null . unDomain

domainFilter :: (value -> Bool) -> Domain value -> Domain value
domainFilter predicate = Domain . Set.filter predicate . unDomain

type Arc :: Type -> Type
data Arc variable = Arc
  { arcSource :: variable,
    arcTarget :: variable
  }
  deriving stock (Eq, Ord, Show, Read)

reverseArc :: Arc variable -> Arc variable
reverseArc (Arc source target) = Arc target source

type BinaryConstraint :: Type -> Type -> Type
data BinaryConstraint variable value = BinaryConstraint
  { binaryConstraintArc :: Arc variable,
    binaryConstraintSatisfied :: value -> value -> Bool
  }

type ConstraintSatisfactionProblem :: Type -> Type -> Type
data ConstraintSatisfactionProblem variable value = ConstraintSatisfactionProblem
  { cspDomains :: Map variable (Domain value),
    cspConstraints :: [BinaryConstraint variable value]
  }

type CSPError :: Type -> Type
data CSPError variable
  = MissingDomain variable
  deriving stock (Eq, Ord, Show, Read)

lookupDomain ::
  Ord variable =>
  ConstraintSatisfactionProblem variable value ->
  variable ->
  Either (CSPError variable) (Domain value)
lookupDomain problem variable =
  maybe
    (Left (MissingDomain variable))
    Right
    (Map.lookup variable (cspDomains problem))

setDomain ::
  Ord variable =>
  variable ->
  Domain value ->
  ConstraintSatisfactionProblem variable value ->
  ConstraintSatisfactionProblem variable value
setDomain variable domainValue problem =
  problem {cspDomains = Map.insert variable domainValue (cspDomains problem)}

allArcs ::
  Ord variable =>
  ConstraintSatisfactionProblem variable value ->
  [Arc variable]
allArcs =
  Set.toAscList
    . foldr
      ( \constraint ->
          let arcValue = binaryConstraintArc constraint
           in Set.insert arcValue . Set.insert (reverseArc arcValue)
      )
      Set.empty
    . cspConstraints

arcPredicates ::
  Eq variable =>
  ConstraintSatisfactionProblem variable value ->
  Arc variable ->
  [value -> value -> Bool]
arcPredicates problem targetArc =
  foldr collect [] (cspConstraints problem)
  where
    collect constraint predicates =
      let constraintArcValue = binaryConstraintArc constraint
          predicate = binaryConstraintSatisfied constraint
       in if constraintArcValue == targetArc
            then predicate : predicates
            else
              if reverseArc constraintArcValue == targetArc
                then (\sourceValue targetValue -> predicate targetValue sourceValue) : predicates
                else predicates

revise ::
  (Ord variable, Ord value) =>
  ConstraintSatisfactionProblem variable value ->
  Arc variable ->
  Either (CSPError variable) (ConstraintSatisfactionProblem variable value, Bool)
revise problem arcValue@(Arc source target) = do
  sourceDomain <- lookupDomain problem source
  targetDomain <- lookupDomain problem target
  let predicates = arcPredicates problem arcValue
      hasSupport sourceValue =
        case predicates of
          [] -> True
          _ ->
            any
              (\targetValue -> all (\predicate -> predicate sourceValue targetValue) predicates)
              (domainToAscList targetDomain)
      revisedSourceDomain = domainFilter hasSupport sourceDomain
  pure
    ( setDomain source revisedSourceDomain problem,
      revisedSourceDomain /= sourceDomain
    )

neighborsOf ::
  Ord variable =>
  ConstraintSatisfactionProblem variable value ->
  variable ->
  [variable]
neighborsOf problem focus =
  Set.toAscList $
    foldr collect Set.empty (cspConstraints problem)
  where
    collect constraint neighbors =
      case binaryConstraintArc constraint of
        Arc left right
          | left == focus -> Set.insert right neighbors
          | right == focus -> Set.insert left neighbors
          | otherwise -> neighbors

neighborArcs ::
  Ord variable =>
  ConstraintSatisfactionProblem variable value ->
  Arc variable ->
  [Arc variable]
neighborArcs problem (Arc source target) =
  map (\neighbor -> Arc neighbor source) $
    filter (/= target) (neighborsOf problem source)

mac3 ::
  forall variable value.
  (Ord variable, Ord value) =>
  ConstraintSatisfactionProblem variable value ->
  Either (CSPError variable) (Maybe (ConstraintSatisfactionProblem variable value))
mac3 problem = go problem (allArcs problem)
  where
    go ::
      ConstraintSatisfactionProblem variable value ->
      [Arc variable] ->
      Either (CSPError variable) (Maybe (ConstraintSatisfactionProblem variable value))
    go current [] = pure (Just current)
    go current (arcValue@(Arc source _) : remainingArcs) = do
      (revisedProblem, wasRevised) <- revise current arcValue
      if wasRevised
        then do
          revisedSourceDomain <- lookupDomain revisedProblem source
          if domainNull revisedSourceDomain
            then pure Nothing
            else go revisedProblem (neighborArcs revisedProblem arcValue <> remainingArcs)
        else go revisedProblem remainingArcs
