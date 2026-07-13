{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Section.Stalk.Discrete
  ( DiscreteMismatch (..),
    DiscreteRepairObstruction (..),
    discreteStalkAlgebra,
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Moonlight.Sheaf.Section.Stalk
  ( MergeObstruction,
    RepairInput (..),
    StalkAlgebra (..),
    StalkRestrictionKernel (..),
    mismatchObstruction,
  )

type DiscreteMismatch :: Type -> Type
data DiscreteMismatch a = DiscreteMismatch
  { discreteMismatchLeft :: !a,
    discreteMismatchRight :: !a
  }
  deriving stock (Eq, Ord, Show, Read)

type DiscreteRepairObstruction :: Type -> Type
data DiscreteRepairObstruction a
  = DiscreteMergeConflict !(NonEmpty a)
  | DiscreteRestrictionConflict !a !a
  deriving stock (Eq, Ord, Show, Read)

discreteStalkAlgebra ::
  Eq a =>
  StalkAlgebra witness a (DiscreteMismatch a) (DiscreteRepairObstruction a)
discreteStalkAlgebra =
  StalkAlgebra
    { saRestrictionKernel = const StalkRestrictionIdentity,
      saMismatches = discreteMismatches,
      saMerge = discreteMerge,
      saRepair = discreteRepair,
      saNormalize = id
    }
{-# INLINEABLE discreteStalkAlgebra #-}

discreteMismatches ::
  Eq a =>
  a ->
  a ->
  [DiscreteMismatch a]
discreteMismatches leftValue rightValue =
  [DiscreteMismatch leftValue rightValue | leftValue /= rightValue]
{-# INLINE discreteMismatches #-}

discreteMerge ::
  Eq a =>
  a ->
  a ->
  Either (MergeObstruction (DiscreteMismatch a)) a
discreteMerge leftValue rightValue =
  maybe
    (Right leftValue)
    Left
    (mismatchObstruction (discreteMismatches leftValue rightValue))
{-# INLINE discreteMerge #-}

discreteRepair ::
  Eq a =>
  RepairInput witness a (DiscreteMismatch a) ->
  Either (DiscreteRepairObstruction a) a
discreteRepair repairInput =
  case repairInput of
    RepairMergeInput values@(firstValue :| remainingValues) _mismatches ->
      if all (== firstValue) remainingValues
        then Right firstValue
        else Left (DiscreteMergeConflict values)
    RepairRestrictionInput _witness restrictedValue targetValue _mismatches ->
      if restrictedValue == targetValue
        then Right targetValue
        else Left (DiscreteRestrictionConflict restrictedValue targetValue)
{-# INLINE discreteRepair #-}
