{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Operator.Fixpoint
  ( SemiNaiveBudget (..),
    SemiNaiveDivergence (..),
    semiNaiveFixpoint,
    semiNaiveFixpointM,
    semiNaiveFixpointFromM,
  )
where

import Data.Functor.Identity
  ( Identity (..),
  )
import Data.Kind
  ( Type,
  )
import Numeric.Natural
  ( Natural,
  )

import Moonlight.Algebra
  ( Semiring,
  )
import Moonlight.Core (AdditiveGroup)
import Moonlight.Differential.Algebra.ZSet
  ( ZSet,
    zsetNull,
  )
import Moonlight.Differential.Operator.Aggregate
  ( distinctZSet,
    positiveSupportMember,
  )
import Moonlight.Differential.Operator.Linear
  ( filterZSet,
  )

type SemiNaiveBudget :: Type
newtype SemiNaiveBudget = SemiNaiveBudget
  { semiNaiveBudgetRounds :: Natural
  }
  deriving stock (Eq, Ord, Show)

type SemiNaiveDivergence :: Type -> Type -> Type
data SemiNaiveDivergence a weight = SemiNaiveDivergence
  { sndRoundsSpent :: !Natural,
    sndResidualDelta :: !(ZSet a weight),
    sndAccumulated :: !(ZSet a weight)
  }
  deriving stock (Eq, Show)

-- | RecursiveFixpoint contract: semi-naive fixpoint over the support lattice.
-- The uniqueness and cycle laws are preconditioned on monotone,
-- support-strict bodies in this lattice; arbitrary functions are not promoted
-- into a fake theorem.  Each round derives from the
-- previous frontier only, clamps the result through 'distinctZSet', and keeps
-- the entries not already accumulated.  Weights never leave @{0, 1}@, so the
-- Z-weighted divergent recursions of raw DBSP feedback are unrepresentable
-- here; the only divergence mode is a genuinely growing support, reported as a
-- typed obstruction when the budget is exhausted.
semiNaiveFixpoint ::
  (Ord a, AdditiveGroup weight, Semiring weight, Ord weight) =>
  SemiNaiveBudget ->
  (ZSet a weight -> ZSet a weight) ->
  ZSet a weight ->
  Either (SemiNaiveDivergence a weight) (ZSet a weight)
semiNaiveFixpoint budget step =
  runIdentity . semiNaiveFixpointM budget (Identity . step)
{-# INLINABLE semiNaiveFixpoint #-}

-- | 'semiNaiveFixpoint' with the round body in an arbitrary monad, so a
-- frontier derivation that can itself refuse (nested fixpoints, foreign
-- faults) threads its effect through the rounds; the budget and clamp
-- discipline are identical.
semiNaiveFixpointM ::
  (Monad m, Ord a, AdditiveGroup weight, Semiring weight, Ord weight) =>
  SemiNaiveBudget ->
  (ZSet a weight -> m (ZSet a weight)) ->
  ZSet a weight ->
  m (Either (SemiNaiveDivergence a weight) (ZSet a weight))
semiNaiveFixpointM budget step seed =
  go (semiNaiveBudgetRounds budget) seedSupport seedSupport
  where
    seedSupport =
      distinctZSet seed

    go remaining accumulated frontier
      | zsetNull frontier =
          pure (Right accumulated)
      | remaining == 0 =
          pure
            ( Left
                SemiNaiveDivergence
                  { sndRoundsSpent = semiNaiveBudgetRounds budget,
                    sndResidualDelta = frontier,
                    sndAccumulated = accumulated
                  }
            )
      | otherwise = do
          derived <- step frontier
          let fresh =
                freshAgainst accumulated (distinctZSet derived)
          go (remaining - 1) (accumulated <> fresh) fresh

    freshAgainst ::
      (Ord a, AdditiveGroup weight, Ord weight) =>
      ZSet a weight ->
      ZSet a weight ->
      ZSet a weight
    freshAgainst accumulated =
      filterZSet (\value -> not (positiveSupportMember value accumulated))
{-# INLINABLE semiNaiveFixpointM #-}

-- | 'semiNaiveFixpointM' resumed from a prior converged @accumulated@ set with a
-- distinct initial @frontier@ of newly-active facts, so incremental maintenance
-- continues iteration from the previous fixpoint instead of from the seed.
-- Frontier facts already present in @accumulated@ do not re-propagate (the
-- semi-naive discipline); both arguments are clamped to the support lattice.
-- @semiNaiveFixpointM b s x@ equals @semiNaiveFixpointFromM b s mempty x@.
semiNaiveFixpointFromM ::
  (Monad m, Ord a, AdditiveGroup weight, Semiring weight, Ord weight) =>
  SemiNaiveBudget ->
  (ZSet a weight -> m (ZSet a weight)) ->
  ZSet a weight ->
  ZSet a weight ->
  m (Either (SemiNaiveDivergence a weight) (ZSet a weight))
semiNaiveFixpointFromM budget step accumulated0 frontier0 =
  go (semiNaiveBudgetRounds budget) (accumulatedSupport <> fresh0) fresh0
  where
    accumulatedSupport =
      distinctZSet accumulated0

    fresh0 =
      freshAgainst accumulatedSupport (distinctZSet frontier0)

    go remaining accumulated frontier
      | zsetNull frontier =
          pure (Right accumulated)
      | remaining == 0 =
          pure
            ( Left
                SemiNaiveDivergence
                  { sndRoundsSpent = semiNaiveBudgetRounds budget,
                    sndResidualDelta = frontier,
                    sndAccumulated = accumulated
                  }
            )
      | otherwise = do
          derived <- step frontier
          let fresh =
                freshAgainst accumulated (distinctZSet derived)
          go (remaining - 1) (accumulated <> fresh) fresh

    freshAgainst ::
      (Ord a, AdditiveGroup weight, Ord weight) =>
      ZSet a weight ->
      ZSet a weight ->
      ZSet a weight
    freshAgainst accumulated =
      filterZSet (\value -> not (positiveSupportMember value accumulated))
{-# INLINABLE semiNaiveFixpointFromM #-}
