module Moonlight.Differential.Operator.Linear
  ( mapZSet,
    filterZSet,
    flatMapZSet,
    indexBy,
  )
where

import Data.Foldable qualified as Foldable
import Moonlight.Core (AdditiveGroup)
import Moonlight.Differential.Algebra.ZSet
  ( IndexedZSet,
    ZSet,
    indexedZSetEmpty,
    indexedZSetInsert,
    zsetEmpty,
    zsetFold,
    zsetInsert,
  )

-- | DBSP linear operator.  These operators preserve the abelian-group
-- structure of Z-sets: @Q (left <> right) = Q left <> Q right@ and
-- @Q mempty = mempty@.  Their delta form is therefore the operator itself.
mapZSet ::
  (Ord b, Eq weight, AdditiveGroup weight) =>
  (a -> b) ->
  ZSet a weight ->
  ZSet b weight
mapZSet transform =
  zsetFold
    (\acc value weight -> zsetInsert (transform value) weight acc)
    zsetEmpty

filterZSet ::
  (Ord a, Eq weight, AdditiveGroup weight) =>
  (a -> Bool) ->
  ZSet a weight ->
  ZSet a weight
filterZSet keep =
  zsetFold
    ( \acc value weight ->
        if keep value
          then zsetInsert value weight acc
          else acc
    )
    zsetEmpty

flatMapZSet ::
  (Foldable outputs, Ord b, Eq weight, AdditiveGroup weight) =>
  (a -> outputs b) ->
  ZSet a weight ->
  ZSet b weight
flatMapZSet transform =
  zsetFold
    ( \acc value weight ->
        Foldable.foldl'
          (\rows result -> zsetInsert result weight rows)
          acc
          (transform value)
    )
    zsetEmpty
{-# INLINE flatMapZSet #-}

indexBy ::
  (Ord key, Ord a, Eq weight, AdditiveGroup weight) =>
  (a -> key) ->
  ZSet a weight ->
  IndexedZSet key a weight
indexBy keyOf =
  zsetFold
    (\acc value weight -> indexedZSetInsert (keyOf value) value weight acc)
    indexedZSetEmpty
