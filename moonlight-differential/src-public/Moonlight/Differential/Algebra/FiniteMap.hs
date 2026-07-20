{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Algebra.FiniteMap
  ( FiniteMap,
    empty,
    singleton,
    insert,
    fromList,
    fromMap,
    toAscList,
    toMap,
    lookup,
    null,
    size,
    foldWithKey,
    foldMapWithKey,
    union,
    unions,
    negateMap,
    difference,
  )
where

import Data.Foldable qualified as Foldable
import Data.Kind
  ( Type,
  )
import Data.List qualified as List
import Data.Ord
  ( comparing,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Prelude hiding
  ( lookup,
    null,
  )

import Moonlight.Core (AdditiveGroup (..), AdditiveMonoid (..))
import Moonlight.Algebra qualified as Algebra

type FiniteMap :: Type -> Type -> Type
newtype FiniteMap key value = FiniteMap
  { unFiniteMap :: Map key value
  }
  deriving stock (Eq, Ord, Show)

instance (Ord key, Eq value, AdditiveGroup value) => Semigroup (FiniteMap key value) where
  (<>) =
    union

instance (Ord key, Eq value, AdditiveGroup value) => Monoid (FiniteMap key value) where
  mempty =
    empty

instance (Ord key, Eq value, AdditiveGroup value) => AdditiveMonoid (FiniteMap key value) where
  zero =
    empty

  add =
    union

instance (Ord key, Eq value, AdditiveGroup value) => AdditiveGroup (FiniteMap key value) where
  neg =
    negateMap

instance (Ord key, Eq value, AdditiveGroup value) => Algebra.Group (FiniteMap key value) where
  groupInverse =
    negateMap

  groupDifference =
    difference

instance (Ord key, Eq value, AdditiveGroup value) => Algebra.AbelianGroup (FiniteMap key value)

empty :: FiniteMap key value
empty =
  FiniteMap Map.empty

singleton ::
  (Eq value, AdditiveGroup value) =>
  key ->
  value ->
  FiniteMap key value
singleton key value
  | value == zero =
      empty
  | otherwise =
      FiniteMap (Map.singleton key value)

insert ::
  (Ord key, Eq value, AdditiveGroup value) =>
  key ->
  value ->
  FiniteMap key value ->
  FiniteMap key value
insert key value finiteMap@(FiniteMap rows)
  | value == zero =
      finiteMap
  | otherwise =
      FiniteMap (Map.alter alterValue key rows)
  where
    alterValue Nothing =
      Just value
    alterValue (Just oldValue) =
      let !newValue =
            add oldValue value
       in if newValue == zero
            then Nothing
            else Just newValue

fromList ::
  (Ord key, Eq value, AdditiveGroup value) =>
  [(key, value)] ->
  FiniteMap key value
fromList entries =
  FiniteMap (Map.fromDistinctAscList (collapse sorted))
  where
    sorted =
      List.sortBy
        (comparing fst)
        [ (key, value)
        | (key, value) <- entries,
          value /= zero
        ]
    collapse [] =
      []
    collapse ((key, value) : rest) =
      go key value rest
    go key !acc ((nextKey, nextValue) : rest)
      | nextKey == key =
          go key (add acc nextValue) rest
    go key !acc rest
      | acc == zero =
          collapse rest
      | otherwise =
          (key, acc) : collapse rest
{-# INLINABLE fromList #-}

fromMap ::
  (Eq value, AdditiveGroup value) =>
  Map key value ->
  FiniteMap key value
fromMap =
  FiniteMap . Map.filter (/= zero)

toAscList :: FiniteMap key value -> [(key, value)]
toAscList (FiniteMap rows) =
  Map.toAscList rows

toMap :: FiniteMap key value -> Map key value
toMap =
  unFiniteMap

lookup ::
  (Ord key, AdditiveGroup value) =>
  key ->
  FiniteMap key value ->
  value
lookup key (FiniteMap rows) =
  Map.findWithDefault zero key rows

null :: FiniteMap key value -> Bool
null (FiniteMap rows) =
  Map.null rows

size :: FiniteMap key value -> Int
size (FiniteMap rows) =
  Map.size rows

foldWithKey ::
  (acc -> key -> value -> acc) ->
  acc ->
  FiniteMap key value ->
  acc
foldWithKey step initial (FiniteMap rows) =
  Map.foldlWithKey' step initial rows
{-# INLINE foldWithKey #-}

foldMapWithKey ::
  Monoid result =>
  (key -> value -> result) ->
  FiniteMap key value ->
  result
foldMapWithKey project (FiniteMap rows) =
  Map.foldMapWithKey project rows

union ::
  (Ord key, Eq value, AdditiveGroup value) =>
  FiniteMap key value ->
  FiniteMap key value ->
  FiniteMap key value
union (FiniteMap left) (FiniteMap right)
  | Map.null left =
      FiniteMap right
  | Map.null right =
      FiniteMap left
  | Just (key, value) <- singletonMapEntry right =
      insert key value (FiniteMap left)
  | Just (key, value) <- singletonMapEntry left =
      insert key value (FiniteMap right)
  | otherwise =
      FiniteMap (Map.mergeWithKey combineFiniteMapValues id id left right)

singletonMapEntry :: Map key value -> Maybe (key, value)
singletonMapEntry rows
  | Map.size rows == 1 =
      fst <$> Map.minViewWithKey rows
  | otherwise =
      Nothing
{-# INLINE singletonMapEntry #-}

combineFiniteMapValues ::
  (Eq value, AdditiveGroup value) =>
  key ->
  value ->
  value ->
  Maybe value
combineFiniteMapValues _key leftValue rightValue =
  let !newValue =
        add leftValue rightValue
   in if newValue == zero
        then Nothing
        else Just newValue

unions ::
  (Foldable maps, Ord key, Eq value, AdditiveGroup value) =>
  maps (FiniteMap key value) ->
  FiniteMap key value
unions =
  Foldable.foldl' union empty

negateMap ::
  AdditiveGroup value =>
  FiniteMap key value ->
  FiniteMap key value
negateMap (FiniteMap rows) =
  FiniteMap (Map.map neg rows)

difference ::
  (Ord key, Eq value, AdditiveGroup value) =>
  FiniteMap key value ->
  FiniteMap key value ->
  FiniteMap key value
difference left right =
  union left (negateMap right)
