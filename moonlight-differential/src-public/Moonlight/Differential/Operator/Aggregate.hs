{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Operator.Aggregate
  ( countByKey,
    GroupView,
    GroupChange (..),
    mkGroupView,
    groupViewIntegrated,
    groupViewReduced,
    groupViewAdvance,
    positiveSupportMember,
    distinctZSet,
    distinctDelta,
  )
where

import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map

import Moonlight.Algebra
  ( MultiplicativeMonoid (one), Semiring,
  )
import Moonlight.Core (AdditiveGroup (..), AdditiveMonoid (..))
import Moonlight.Differential.Algebra.ZSet
  ( IndexedZSet,
    ZSet,
    indexedZSetFold,
    indexedZSetLookup,
    zsetEmpty,
    zsetFold,
    zsetInsert,
    zsetLookup,
  )

-- | Linear reduction from an indexed family to one weight per key.  This is
-- aggregate-shaped, not an endomorphic Z-set map, so its law is stated over the
-- explicit indexed domain: @countByKey (left <> right) = countByKey left <>
-- countByKey right@.
countByKey ::
  (Ord key, Eq weight, AdditiveGroup weight) =>
  IndexedZSet key a weight ->
  ZSet key weight
countByKey =
  indexedZSetFold
    (\acc key group -> zsetInsert key (groupWeight group) acc)
    zsetEmpty
  where
    groupWeight ::
      AdditiveGroup weight =>
      ZSet value weight ->
      weight
    groupWeight =
      zsetFold (\acc _value weight -> add acc weight) zero

type GroupView :: Type -> Type -> Type -> Type -> Type
data GroupView key a weight reduced = GroupView
  { groupViewIntegrated :: !(IndexedZSet key a weight),
    groupViewReduced :: !(Map key reduced)
  }
  deriving stock (Eq, Show)

type GroupChange :: Type -> Type
data GroupChange reduced
  = GroupReduced !reduced
  | GroupVanished
  deriving stock (Eq, Show)

mkGroupView ::
  Ord key =>
  (ZSet a weight -> reduced) ->
  IndexedZSet key a weight ->
  GroupView key a weight reduced
mkGroupView reducer integrated =
  GroupView
    { groupViewIntegrated = integrated,
      groupViewReduced =
        indexedZSetFold
          (\acc key group -> Map.insert key (reducer group) acc)
          Map.empty
          integrated
    }

groupViewAdvance ::
  (Ord key, Ord a, Eq weight, AdditiveGroup weight) =>
  (ZSet a weight -> reduced) ->
  IndexedZSet key a weight ->
  GroupView key a weight reduced ->
  (Map key (GroupChange reduced), GroupView key a weight reduced)
groupViewAdvance reducer delta view =
  ( changes,
    GroupView
      { groupViewIntegrated = advanced,
        groupViewReduced = Map.foldrWithKey applyChange (groupViewReduced view) changes
      }
  )
  where
    advanced =
      groupViewIntegrated view <> delta

    changes =
      indexedZSetFold
        ( \acc key _deltaGroup ->
            Map.insert
              key
              ( maybe
                  GroupVanished
                  (GroupReduced . reducer)
                  (indexedZSetLookup key advanced)
              )
              acc
        )
        Map.empty
        delta

    applyChange ::
      Ord key =>
      key ->
      GroupChange reduced ->
      Map key reduced ->
      Map key reduced
    applyChange key change reductions =
      case change of
        GroupReduced reducedValue ->
          Map.insert key reducedValue reductions
        GroupVanished ->
          Map.delete key reductions

-- | Non-linear aggregate delta family.  The materialized view keeps exactly
-- the positive support with unit weights and is idempotent:
-- @distinctZSet . distinctZSet = distinctZSet@.
distinctZSet ::
  (Ord a, AdditiveGroup weight, Semiring weight, Ord weight) =>
  ZSet a weight ->
  ZSet a weight
distinctZSet =
  zsetFold
    ( \acc value weight ->
        if weight > zero
          then zsetInsert value one acc
          else acc
    )
    zsetEmpty

positiveSupportMember ::
  (Ord a, AdditiveGroup weight, Ord weight) =>
  a ->
  ZSet a weight ->
  Bool
positiveSupportMember value zset =
  zsetLookup value zset > zero
{-# INLINE positiveSupportMember #-}

distinctDelta ::
  (Ord a, AdditiveGroup weight, Semiring weight, Ord weight) =>
  ZSet a weight ->
  ZSet a weight ->
  ZSet a weight
distinctDelta integrated delta =
  zsetFold step zsetEmpty delta
  where
    advanced =
      integrated <> delta

    step acc value _weight =
      case (positiveSupportMember value integrated, positiveSupportMember value advanced) of
        (False, True) ->
          zsetInsert value one acc
        (True, False) ->
          zsetInsert value (neg one) acc
        _ ->
          acc
