{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Saturation.Core.Termination
  ( SaturationBudget (..),
    SaturationTermination (..),
    TerminationGoal (..),
    alwaysContinue,
    goal,
    checkTerminationGoal,
    contramapGoal,
  )
where

import Data.Functor.Contravariant (Contravariant (..))
import Data.Kind (Type)

type SaturationBudget :: Type
data SaturationBudget = SaturationBudget
  { sbMaxIterations :: !Int,
    sbMaxNodes :: !Int
  }
  deriving stock (Eq, Ord, Show, Read)

type SaturationTermination :: Type
data SaturationTermination
  = ReachedFixedPoint
  | ReachedGoal
  | HitIterationLimit
  | HitNodeLimit
  deriving stock (Eq, Ord, Show, Read)

type TerminationGoal :: Type -> Type
newtype TerminationGoal state = TerminationGoal
  { unTerminationGoal :: state -> Bool
  }

instance Contravariant TerminationGoal where
  contramap f (TerminationGoal predicate) =
    TerminationGoal (predicate . f)

instance Semigroup (TerminationGoal state) where
  TerminationGoal leftPredicate <> TerminationGoal rightPredicate =
    TerminationGoal $ \state ->
      leftPredicate state || rightPredicate state

instance Monoid (TerminationGoal state) where
  mempty =
    alwaysContinue

alwaysContinue :: TerminationGoal state
alwaysContinue =
  TerminationGoal (const False)

goal :: (state -> Bool) -> TerminationGoal state
goal =
  TerminationGoal

checkTerminationGoal :: TerminationGoal state -> state -> Bool
checkTerminationGoal (TerminationGoal predicate) =
  predicate

contramapGoal ::
  (leftState -> rightState) ->
  TerminationGoal rightState ->
  TerminationGoal leftState
contramapGoal =
  contramap
