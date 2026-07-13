{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Algebra.ZSet
  ( ZSet,
    IndexedZSet,
    Timed (..),
    zsetEmpty,
    zsetSingleton,
    zsetInsert,
    zsetFromList,
    zsetToAscList,
    zsetLookup,
    zsetDifference,
    zsetNegate,
    zsetUnions,
    zsetNull,
    zsetSize,
    zsetFold,
    indexedZSetEmpty,
    indexedZSetSingleton,
    indexedZSetInsert,
    indexedZSetFromList,
    indexedZSetUnions,
    indexedZSetLookup,
    indexedZSetLookupValue,
    indexedZSetToAscList,
    indexedZSetDifference,
    indexedZSetKeySet,
    indexedZSetFilter,
    indexedZSetNull,
    indexedZSetCellCount,
    indexedZSetFold,
  )
where

import Data.Foldable qualified as Foldable
import Data.Kind
  ( Type,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )

import Moonlight.Core (AdditiveGroup (..), AdditiveMonoid (..))
import Moonlight.Algebra qualified as Algebra
import Moonlight.Differential.Algebra.FiniteMap
  ( FiniteMap,
  )
import Moonlight.Differential.Algebra.FiniteMap qualified as FiniteMap

type ZSet :: Type -> Type -> Type
newtype ZSet value weight = ZSet
  { unZSet :: FiniteMap value weight
  }
  deriving stock (Eq, Ord, Show)

type IndexedZSet :: Type -> Type -> Type -> Type
data IndexedZSet key value weight = IndexedZSet
  { indexedZSetRows :: !(FiniteMap key (ZSet value weight)),
    indexedZSetCells :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show)

type Timed :: Type -> Type -> Type
data Timed time value = Timed
  { timedTime :: !time,
    timedValue :: !value
  }
  deriving stock (Eq, Show, Read)

instance (Ord time, Ord value) => Ord (Timed time value) where
  compare left right =
    compare (timedValue left) (timedValue right)
      <> compare (timedTime left) (timedTime right)

instance (Ord value, Eq weight, AdditiveGroup weight) => Semigroup (ZSet value weight) where
  ZSet left <> ZSet right =
    ZSet (left <> right)

instance (Ord value, Eq weight, AdditiveGroup weight) => Monoid (ZSet value weight) where
  mempty =
    zsetEmpty

instance (Ord value, Eq weight, AdditiveGroup weight) => AdditiveMonoid (ZSet value weight) where
  zero =
    zsetEmpty

  add =
    (<>)

instance (Ord value, Eq weight, AdditiveGroup weight) => AdditiveGroup (ZSet value weight) where
  neg =
    zsetNegate

instance (Ord value, Eq weight, AdditiveGroup weight) => Algebra.Group (ZSet value weight) where
  groupInverse =
    zsetNegate

  groupDifference =
    zsetDifference

instance (Ord value, Eq weight, AdditiveGroup weight) => Algebra.AbelianGroup (ZSet value weight)

instance (Ord key, Ord value, Eq weight, AdditiveGroup weight) => Semigroup (IndexedZSet key value weight) where
  (<>) =
    indexedZSetPlus

instance (Ord key, Ord value, Eq weight, AdditiveGroup weight) => Monoid (IndexedZSet key value weight) where
  mempty =
    indexedZSetEmpty

instance (Ord key, Ord value, Eq weight, AdditiveGroup weight) => AdditiveMonoid (IndexedZSet key value weight) where
  zero =
    indexedZSetEmpty

  add =
    indexedZSetPlus

instance (Ord key, Ord value, Eq weight, AdditiveGroup weight) => AdditiveGroup (IndexedZSet key value weight) where
  neg =
    indexedZSetNegate

instance (Ord key, Ord value, Eq weight, AdditiveGroup weight) => Algebra.Group (IndexedZSet key value weight) where
  groupInverse =
    indexedZSetNegate

  groupDifference =
    indexedZSetDifference

instance (Ord key, Ord value, Eq weight, AdditiveGroup weight) => Algebra.AbelianGroup (IndexedZSet key value weight)

zsetEmpty :: ZSet value weight
zsetEmpty =
  ZSet FiniteMap.empty

zsetSingleton ::
  (Eq weight, AdditiveGroup weight) =>
  value ->
  weight ->
  ZSet value weight
zsetSingleton =
  ZSet .: FiniteMap.singleton

zsetInsert ::
  (Ord value, Eq weight, AdditiveGroup weight) =>
  value ->
  weight ->
  ZSet value weight ->
  ZSet value weight
zsetInsert value weight (ZSet rows) =
  ZSet (FiniteMap.insert value weight rows)

zsetFromList ::
  (Ord value, Eq weight, AdditiveGroup weight) =>
  [(value, weight)] ->
  ZSet value weight
zsetFromList =
  ZSet . FiniteMap.fromList
{-# INLINE zsetFromList #-}

zsetToAscList :: ZSet value weight -> [(value, weight)]
zsetToAscList (ZSet rows) =
  FiniteMap.toAscList rows

zsetLookup ::
  (Ord value, AdditiveGroup weight) =>
  value ->
  ZSet value weight ->
  weight
zsetLookup value (ZSet rows) =
  FiniteMap.lookup value rows

zsetDifference ::
  (Ord value, Eq weight, AdditiveGroup weight) =>
  ZSet value weight ->
  ZSet value weight ->
  ZSet value weight
zsetDifference (ZSet left) (ZSet right) =
  ZSet (FiniteMap.difference left right)

zsetNegate ::
  AdditiveGroup weight =>
  ZSet value weight ->
  ZSet value weight
zsetNegate (ZSet rows) =
  ZSet (FiniteMap.negateMap rows)

zsetUnions ::
  (Foldable sets, Ord value, Eq weight, AdditiveGroup weight) =>
  sets (ZSet value weight) ->
  ZSet value weight
zsetUnions =
  Foldable.foldl' (<>) zsetEmpty

zsetNull :: ZSet value weight -> Bool
zsetNull (ZSet rows) =
  FiniteMap.null rows

zsetSize :: ZSet value weight -> Int
zsetSize (ZSet rows) =
  FiniteMap.size rows
{-# INLINE zsetSize #-}

zsetFold ::
  (acc -> value -> weight -> acc) ->
  acc ->
  ZSet value weight ->
  acc
zsetFold step initial (ZSet rows) =
  FiniteMap.foldWithKey
    step
    initial
    rows
{-# INLINE zsetFold #-}

indexedZSetEmpty :: IndexedZSet key value weight
indexedZSetEmpty =
  IndexedZSet
    { indexedZSetRows = FiniteMap.empty,
      indexedZSetCells = 0
    }

indexedZSetSingleton ::
  (Ord value, Eq weight, AdditiveGroup weight) =>
  key ->
  value ->
  weight ->
  IndexedZSet key value weight
indexedZSetSingleton key value weight
  | weight == zero =
      indexedZSetEmpty
  | otherwise =
      IndexedZSet
        { indexedZSetRows = FiniteMap.singleton key (zsetSingleton value weight),
          indexedZSetCells = 1
        }

indexedZSetInsert ::
  (Ord key, Ord value, Eq weight, AdditiveGroup weight) =>
  key ->
  value ->
  weight ->
  IndexedZSet key value weight ->
  IndexedZSet key value weight
indexedZSetInsert key value weight indexedSet
  | weight == zero =
      indexedSet
  | otherwise =
      IndexedZSet
        { indexedZSetRows =
            FiniteMap.insert key (zsetSingleton value weight) (indexedZSetRows indexedSet),
          indexedZSetCells =
            indexedZSetCells indexedSet + indexedZSetInsertCellDelta key value weight indexedSet
        }

indexedZSetInsertCellDelta ::
  (Ord key, Ord value, Eq weight, AdditiveGroup weight) =>
  key ->
  value ->
  weight ->
  IndexedZSet key value weight ->
  Int
indexedZSetInsertCellDelta key value weight indexedSet =
  case indexedZSetLookup key indexedSet of
    Nothing ->
      1
    Just values ->
      liveCellDelta
        (zsetLookup value values /= zero)
        (add (zsetLookup value values) weight /= zero)
{-# INLINE indexedZSetInsertCellDelta #-}

liveCellDelta :: Bool -> Bool -> Int
liveCellDelta oldLive newLive =
  case (oldLive, newLive) of
    (False, True) ->
      1
    (True, False) ->
      -1
    _ ->
      0
{-# INLINE liveCellDelta #-}

indexedZSetFromList ::
  (Ord key, Ord value, Eq weight, AdditiveGroup weight) =>
  [(key, value, weight)] ->
  IndexedZSet key value weight
indexedZSetFromList entries =
  case entries of
    [] ->
      indexedZSetEmpty
    [(key, value, weight)] ->
      indexedZSetSingleton key value weight
    _ ->
      indexedZSetFromMany entries

indexedZSetFromMany ::
  (Ord key, Ord value, Eq weight, AdditiveGroup weight) =>
  [(key, value, weight)] ->
  IndexedZSet key value weight
indexedZSetFromMany entries =
  indexedZSetFromRows
    ( FiniteMap.fromMap
        (Map.map zsetFromWeightMap (nestedIndexedWeights entries))
    )

nestedIndexedWeights ::
  (Ord key, Ord value, Eq weight, AdditiveGroup weight) =>
  [(key, value, weight)] ->
  Map.Map key (Map.Map value weight)
nestedIndexedWeights =
  Foldable.foldl' insertNestedEntry Map.empty

insertNestedEntry ::
  (Ord key, Ord value, Eq weight, AdditiveGroup weight) =>
  Map.Map key (Map.Map value weight) ->
  (key, value, weight) ->
  Map.Map key (Map.Map value weight)
insertNestedEntry nestedRows (key, value, weight)
  | weight == zero =
      nestedRows
  | otherwise =
      Map.alter
        (alterNestedWeight value weight)
        key
        nestedRows

indexedZSetPlus ::
  (Ord key, Ord value, Eq weight, AdditiveGroup weight) =>
  IndexedZSet key value weight ->
  IndexedZSet key value weight ->
  IndexedZSet key value weight
indexedZSetPlus left right
  | indexedZSetCells left == 0 =
      right
  | indexedZSetCells right == 0 =
      left
  | otherwise =
      indexedZSetFromRows (indexedZSetRows left <> indexedZSetRows right)
{-# INLINE indexedZSetPlus #-}

indexedZSetFromRows ::
  FiniteMap key (ZSet value weight) ->
  IndexedZSet key value weight
indexedZSetFromRows rows =
  IndexedZSet
    { indexedZSetRows = rows,
      indexedZSetCells = indexedZSetRowsCellCount rows
    }
{-# INLINE indexedZSetFromRows #-}

indexedZSetUnions ::
  (Foldable sets, Ord key, Ord value, Eq weight, AdditiveGroup weight) =>
  sets (IndexedZSet key value weight) ->
  IndexedZSet key value weight
indexedZSetUnions =
  indexedZSetFromRows . Foldable.foldl' mergeIndexedZSetRows FiniteMap.empty
{-# INLINE indexedZSetUnions #-}

mergeIndexedZSetRows ::
  (Ord key, Ord value, Eq weight, AdditiveGroup weight) =>
  FiniteMap key (ZSet value weight) ->
  IndexedZSet key value weight ->
  FiniteMap key (ZSet value weight)
mergeIndexedZSetRows rows indexedSet =
  rows <> indexedZSetRows indexedSet
{-# INLINE mergeIndexedZSetRows #-}

indexedZSetRowsCellCount :: FiniteMap key (ZSet value weight) -> Int
indexedZSetRowsCellCount =
  FiniteMap.foldWithKey
    (\count _key values -> count + zsetSize values)
    0
{-# INLINE indexedZSetRowsCellCount #-}

alterNestedWeight ::
  (Ord value, Eq weight, AdditiveGroup weight) =>
  value ->
  weight ->
  Maybe (Map.Map value weight) ->
  Maybe (Map.Map value weight)
alterNestedWeight value weight Nothing =
  Just (Map.singleton value weight)
alterNestedWeight value weight (Just weights) =
  let updatedWeights =
        Map.alter
          (combineNestedWeight weight)
          value
          weights
   in if Map.null updatedWeights
        then Nothing
        else Just updatedWeights

combineNestedWeight ::
  (Eq weight, AdditiveGroup weight) =>
  weight ->
  Maybe weight ->
  Maybe weight
combineNestedWeight weight Nothing =
  Just weight
combineNestedWeight weight (Just oldWeight) =
  let newWeight =
        add oldWeight weight
   in if newWeight == zero
        then Nothing
        else Just newWeight

zsetFromWeightMap ::
  (Eq weight, AdditiveGroup weight) =>
  Map.Map value weight ->
  ZSet value weight
zsetFromWeightMap =
  ZSet . FiniteMap.fromMap

indexedZSetLookup ::
  Ord key =>
  key ->
  IndexedZSet key value weight ->
  Maybe (ZSet value weight)
indexedZSetLookup key =
  Map.lookup key . FiniteMap.toMap . indexedZSetRows

indexedZSetLookupValue ::
  (Ord key, Ord value, AdditiveGroup weight) =>
  key ->
  value ->
  IndexedZSet key value weight ->
  weight
indexedZSetLookupValue key value rows =
  maybe
    zero
    (zsetLookup value)
    (indexedZSetLookup key rows)

indexedZSetToAscList :: IndexedZSet key value weight -> [(key, ZSet value weight)]
indexedZSetToAscList =
  FiniteMap.toAscList . indexedZSetRows

indexedZSetDifference ::
  (Ord key, Ord value, Eq weight, AdditiveGroup weight) =>
  IndexedZSet key value weight ->
  IndexedZSet key value weight ->
  IndexedZSet key value weight
indexedZSetDifference left right =
  indexedZSetPlus left (indexedZSetNegate right)

indexedZSetNegate ::
  (Ord value, Eq weight, AdditiveGroup weight) =>
  IndexedZSet key value weight ->
  IndexedZSet key value weight
indexedZSetNegate indexedSet =
  indexedSet {indexedZSetRows = FiniteMap.negateMap (indexedZSetRows indexedSet)}
{-# INLINE indexedZSetNegate #-}

indexedZSetKeySet :: IndexedZSet key value weight -> Set key
indexedZSetKeySet =
  Map.keysSet . FiniteMap.toMap . indexedZSetRows

indexedZSetFilter ::
  (Ord key, Ord value, Eq weight, AdditiveGroup weight) =>
  (key -> value -> weight -> Bool) ->
  IndexedZSet key value weight ->
  IndexedZSet key value weight
indexedZSetFilter keep =
  indexedZSetFold filterKey indexedZSetEmpty
  where
    filterKey acc key values =
      zsetFold
        ( \filtered value weight ->
            if keep key value weight
              then indexedZSetInsert key value weight filtered
              else filtered
        )
        acc
        values

indexedZSetNull :: IndexedZSet key value weight -> Bool
indexedZSetNull =
  (== 0) . indexedZSetCells
{-# INLINE indexedZSetNull #-}

indexedZSetCellCount :: IndexedZSet key value weight -> Int
indexedZSetCellCount =
  indexedZSetCells
{-# INLINE indexedZSetCellCount #-}

indexedZSetFold ::
  (acc -> key -> ZSet value weight -> acc) ->
  acc ->
  IndexedZSet key value weight ->
  acc
indexedZSetFold step initial indexedSet =
  FiniteMap.foldWithKey
    step
    initial
    (indexedZSetRows indexedSet)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) outer inner left right =
  outer (inner left right)
