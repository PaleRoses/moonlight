{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Collection
  ( Collection,
    IndexedCollection,
    collectionZSet,
    indexedCollectionZSet,
    emptyCollection,
    singletonCollection,
    collectionFromList,
    collectionToAscList,
    indexedCollectionToAscList,
    concatCollections,
    concatenateCollections,
    negateCollection,
    differenceCollections,
    flatMapCollection,
    mapCollection,
    filterCollection,
    concatIndexedCollections,
    negateIndexedCollection,
    differenceIndexedCollections,
    deindexCollection,
    indexCollectionBy,
    joinCollections,
    countCollectionByKey,
    distinctCollection,
    iterateCollection,
    InputCollection,
    InputAdvance (..),
    RelationPlan (..),
    RelationChanges (..),
    RelationBootstrapError (..),
    RelationAdvanceError (..),
    RelationValidationError (..),
    Update (..),
    inputPlan,
    inputState,
    inputTrace,
    inputArrangement,
    inputRows,
    bootstrapInput,
    advanceInput,
    validateInput,
  )
where

import Data.Foldable qualified as Foldable
import Data.Kind
  ( Type,
  )
import Data.List.NonEmpty
  ( NonEmpty,
  )
import Moonlight.Algebra
  ( Semiring,
  )
import Moonlight.Core (AdditiveGroup)
import Moonlight.Core
  ( PartialOrder,
  )
import Moonlight.Differential.Algebra.ZSet
  ( IndexedZSet,
    ZSet,
    indexedZSetDifference,
    indexedZSetEmpty,
    indexedZSetFold,
    indexedZSetToAscList,
    zsetDifference,
    zsetEmpty,
    zsetFold,
    zsetFromList,
    zsetInsert,
    zsetNegate,
    zsetSingleton,
    zsetToAscList,
  )
import Moonlight.Differential.Arrangement
  ( Arrangement,
  )
import Moonlight.Differential.Batch
  ( Batch,
    fromUpdates,
  )
import Moonlight.Differential.Index.IndexedRows
  ( IndexedRows,
  )
import Moonlight.Differential.Operator.Aggregate
  ( countByKey,
    distinctZSet,
  )
import Moonlight.Differential.Operator.Fixpoint
  ( SemiNaiveBudget,
    SemiNaiveDivergence,
    semiNaiveFixpoint,
  )
import Moonlight.Differential.Operator.Join
  ( joinIndexed,
  )
import Moonlight.Differential.Operator.Linear
  ( filterZSet,
    flatMapZSet,
    indexBy,
    mapZSet,
  )
import Moonlight.Differential.Relation
  ( CoreRelationViews (..),
    RelationAdvance (..),
    RelationAdvanceError (..),
    RelationBootstrapError (..),
    RelationChanges (..),
    RelationPlan (..),
    RelationState,
    RelationValidationError (..),
    advanceRelation,
    bootstrapRelation,
    relationTrace,
    relationViews,
    validateRelation,
  )
import Moonlight.Differential.Trace
  ( Trace,
    traceFromUpdates,
  )
import Moonlight.Differential.Update
  ( Update (..),
  )

type Collection :: Type -> Type -> Type
newtype Collection value weight = Collection
  { collectionZSet :: ZSet value weight
  }
  deriving stock (Eq, Ord, Show)

type IndexedCollection :: Type -> Type -> Type -> Type
newtype IndexedCollection key value weight = IndexedCollection
  { indexedCollectionZSet :: IndexedZSet key value weight
  }
  deriving stock (Eq, Ord, Show)

instance (Ord value, Eq weight, AdditiveGroup weight) => Semigroup (Collection value weight) where
  (<>) =
    concatCollections

instance (Ord value, Eq weight, AdditiveGroup weight) => Monoid (Collection value weight) where
  mempty =
    emptyCollection

instance (Ord key, Ord value, Eq weight, AdditiveGroup weight) => Semigroup (IndexedCollection key value weight) where
  (<>) =
    concatIndexedCollections

instance (Ord key, Ord value, Eq weight, AdditiveGroup weight) => Monoid (IndexedCollection key value weight) where
  mempty =
    IndexedCollection indexedZSetEmpty

type InputCollection :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data InputCollection time key val weight layout rowKey payload = InputCollection
  { inputPlan :: !(RelationPlan time key val weight layout rowKey payload),
    inputState :: !(RelationState time key val weight layout rowKey payload)
  }

type InputAdvance :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data InputAdvance time key val weight layout rowKey payload = InputAdvance
  { inputAdvanceBatch :: !(Batch time key val weight),
    inputAdvanceChanges :: !(RelationChanges rowKey payload),
    inputAdvanceNext :: !(InputCollection time key val weight layout rowKey payload)
  }

emptyCollection :: Collection value weight
emptyCollection =
  Collection zsetEmpty
{-# INLINE emptyCollection #-}

singletonCollection ::
  (Eq weight, AdditiveGroup weight) =>
  value ->
  weight ->
  Collection value weight
singletonCollection value weight =
  Collection (zsetSingleton value weight)
{-# INLINE singletonCollection #-}

collectionFromList ::
  (Ord value, Eq weight, AdditiveGroup weight) =>
  [(value, weight)] ->
  Collection value weight
collectionFromList =
  Collection . zsetFromList
{-# INLINE collectionFromList #-}

collectionToAscList :: Collection value weight -> [(value, weight)]
collectionToAscList =
  zsetToAscList . collectionZSet
{-# INLINE collectionToAscList #-}

indexedCollectionToAscList :: IndexedCollection key value weight -> [(key, value, weight)]
indexedCollectionToAscList =
  foldMap flattenKeyRows . indexedZSetToAscList . indexedCollectionZSet
  where
    flattenKeyRows :: (key, ZSet value weight) -> [(key, value, weight)]
    flattenKeyRows (key, rows) =
      fmap (\(value, weight) -> (key, value, weight)) (zsetToAscList rows)
{-# INLINE indexedCollectionToAscList #-}

concatCollections ::
  (Ord value, Eq weight, AdditiveGroup weight) =>
  Collection value weight ->
  Collection value weight ->
  Collection value weight
concatCollections left right =
  Collection (collectionZSet left <> collectionZSet right)
{-# INLINE concatCollections #-}

concatenateCollections ::
  (Foldable collections, Ord value, Eq weight, AdditiveGroup weight) =>
  collections (Collection value weight) ->
  Collection value weight
concatenateCollections =
  Foldable.foldl' concatCollections emptyCollection
{-# INLINE concatenateCollections #-}

negateCollection ::
  AdditiveGroup weight =>
  Collection value weight ->
  Collection value weight
negateCollection =
  Collection . zsetNegate . collectionZSet
{-# INLINE negateCollection #-}

differenceCollections ::
  (Ord value, Eq weight, AdditiveGroup weight) =>
  Collection value weight ->
  Collection value weight ->
  Collection value weight
differenceCollections left right =
  Collection (zsetDifference (collectionZSet left) (collectionZSet right))
{-# INLINE differenceCollections #-}

flatMapCollection ::
  (Foldable outputs, Ord result, Eq weight, AdditiveGroup weight) =>
  (value -> outputs result) ->
  Collection value weight ->
  Collection result weight
flatMapCollection transform =
  Collection . flatMapZSet transform . collectionZSet
{-# INLINE flatMapCollection #-}

mapCollection ::
  (Ord result, Eq weight, AdditiveGroup weight) =>
  (value -> result) ->
  Collection value weight ->
  Collection result weight
mapCollection transform =
  Collection . mapZSet transform . collectionZSet
{-# INLINE mapCollection #-}

filterCollection ::
  (Ord value, Eq weight, AdditiveGroup weight) =>
  (value -> Bool) ->
  Collection value weight ->
  Collection value weight
filterCollection keep =
  Collection . filterZSet keep . collectionZSet
{-# INLINE filterCollection #-}

concatIndexedCollections ::
  (Ord key, Ord value, Eq weight, AdditiveGroup weight) =>
  IndexedCollection key value weight ->
  IndexedCollection key value weight ->
  IndexedCollection key value weight
concatIndexedCollections left right =
  IndexedCollection (indexedCollectionZSet left <> indexedCollectionZSet right)
{-# INLINE concatIndexedCollections #-}

negateIndexedCollection ::
  (Ord key, Ord value, Eq weight, AdditiveGroup weight) =>
  IndexedCollection key value weight ->
  IndexedCollection key value weight
negateIndexedCollection indexed =
  IndexedCollection (indexedZSetDifference indexedZSetEmpty (indexedCollectionZSet indexed))
{-# INLINE negateIndexedCollection #-}

differenceIndexedCollections ::
  (Ord key, Ord value, Eq weight, AdditiveGroup weight) =>
  IndexedCollection key value weight ->
  IndexedCollection key value weight ->
  IndexedCollection key value weight
differenceIndexedCollections left right =
  IndexedCollection (indexedZSetDifference (indexedCollectionZSet left) (indexedCollectionZSet right))
{-# INLINE differenceIndexedCollections #-}

deindexCollection ::
  (Ord result, Eq weight, AdditiveGroup weight) =>
  (key -> value -> result) ->
  IndexedCollection key value weight ->
  Collection result weight
deindexCollection project =
  Collection
    . indexedZSetFold collectKeyRows zsetEmpty
    . indexedCollectionZSet
  where
    collectKeyRows acc key rows =
      zsetFold
        (\indexedRows value weight -> zsetInsert (project key value) weight indexedRows)
        acc
        rows
{-# INLINE deindexCollection #-}

indexCollectionBy ::
  (Ord key, Ord value, Eq weight, AdditiveGroup weight) =>
  (value -> key) ->
  Collection value weight ->
  IndexedCollection key value weight
indexCollectionBy keyOf =
  IndexedCollection . indexBy keyOf . collectionZSet
{-# INLINE indexCollectionBy #-}

joinCollections ::
  (Ord key, Ord left, Ord right, Eq weight, AdditiveGroup weight, Semiring weight) =>
  IndexedCollection key left weight ->
  IndexedCollection key right weight ->
  Collection (key, left, right) weight
joinCollections left right =
  Collection (joinIndexed (indexedCollectionZSet left) (indexedCollectionZSet right))
{-# INLINE joinCollections #-}

countCollectionByKey ::
  (Ord key, Eq weight, AdditiveGroup weight) =>
  IndexedCollection key value weight ->
  Collection key weight
countCollectionByKey =
  Collection . countByKey . indexedCollectionZSet
{-# INLINE countCollectionByKey #-}

distinctCollection ::
  (Ord value, AdditiveGroup weight, Semiring weight, Ord weight) =>
  Collection value weight ->
  Collection value weight
distinctCollection =
  Collection . distinctZSet . collectionZSet
{-# INLINE distinctCollection #-}

iterateCollection ::
  (Ord value, AdditiveGroup weight, Semiring weight, Ord weight) =>
  SemiNaiveBudget ->
  (Collection value weight -> Collection value weight) ->
  Collection value weight ->
  Either (SemiNaiveDivergence value weight) (Collection value weight)
iterateCollection budget step seed =
  Collection
    <$> semiNaiveFixpoint
      budget
      (collectionZSet . step . Collection)
      (collectionZSet seed)
{-# INLINE iterateCollection #-}

bootstrapInput ::
  (Foldable updates, Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight, Ord rowKey, Eq payload, AdditiveGroup payload) =>
  RelationPlan time key val weight layout rowKey payload ->
  updates (Update time key val weight) ->
  Either
    (RelationBootstrapError time key val weight rowKey layout)
    (InputCollection time key val weight layout rowKey payload)
bootstrapInput plan updates =
  InputCollection plan <$> bootstrapRelation plan (traceFromUpdates updates)
{-# INLINE bootstrapInput #-}

advanceInput ::
  (Foldable updates, PartialOrder time, Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight, Ord rowKey, Eq payload, AdditiveGroup payload) =>
  updates (Update time key val weight) ->
  InputCollection time key val weight layout rowKey payload ->
  Either
    (RelationAdvanceError time key val weight rowKey layout)
    (InputAdvance time key val weight layout rowKey payload)
advanceInput updates input =
  toInputAdvance <$> advanceRelation (inputPlan input) batch (inputState input)
  where
    batch =
      fromUpdates updates

    toInputAdvance advance =
      InputAdvance
        { inputAdvanceBatch = relationInputBatch advance,
          inputAdvanceChanges = relationChanges advance,
          inputAdvanceNext =
            InputCollection
              { inputPlan = inputPlan input,
                inputState = relationNextState advance
              }
        }
{-# INLINE advanceInput #-}

inputTrace :: InputCollection time key val weight layout rowKey payload -> Trace time key val weight
inputTrace =
  relationTrace . inputState
{-# INLINE inputTrace #-}

inputArrangement :: InputCollection time key val weight layout rowKey payload -> Arrangement time key val weight
inputArrangement =
  relationByKey . relationViews . inputState
{-# INLINE inputArrangement #-}

inputRows :: InputCollection time key val weight layout rowKey payload -> IndexedRows layout rowKey payload
inputRows =
  relationRows . relationViews . inputState
{-# INLINE inputRows #-}

validateInput ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight, Ord rowKey, Eq payload, AdditiveGroup payload) =>
  InputCollection time key val weight layout rowKey payload ->
  Either (NonEmpty (RelationValidationError time key val weight rowKey layout)) ()
validateInput input =
  validateRelation (inputPlan input) (inputState input)
{-# INLINE validateInput #-}
