{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Storage.Store
  ( Store,
    StoragePatch (..),
    StoragePatchResult (..),
    StorageError (..),
    storeFromRelations,
    storeFromRelationsWithPlan,
    storeFromPlan,
    storeWithPlannedAtomRows,
    storeRelations,
    lookupRelation,
    storeSeparatorCache,
    applyStoragePatch,
    lookupSeparatorIndex,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Moonlight.Core
  ( AtomId,
    SlotId,
    atomIdKey,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Differential.Row.Delta
  ( rowDeltaNull
  )
import Moonlight.Differential.Row.Patch
  ( emptyPlainRowPatch,
    positivePlainRowPatchRows,
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope,
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )
import Moonlight.Differential.Row.Block
  ( RowLayout,
  )
import Moonlight.Flow.Storage.Plan
  ( CompiledStoragePlan (..),
  )
import Moonlight.Flow.Storage.Relation
  ( Relation,
    RelationPatchError,
    RowIdDelta,
    applyRelationPatchTracked,
    normalizeRowIdDelta,
    relationFromRows,
    relationLayout,
    rowIdDeltaNull,
  )
import Moonlight.Flow.Storage.Separator
  ( SeparatorIndex,
    SeparatorSpec (..),
    applySeparatorIndexDelta,
    buildSeparatorIndex,
  )


type Store :: Type
data Store = Store
  { stRelations :: !(IntMap Relation),
    stSeparators :: !(Map SeparatorSpec SeparatorIndex),
    stSeparatorsByAtom :: !(IntMap [SeparatorSpec])
  }
  deriving stock (Eq, Show)

type StoragePatch :: Type
data StoragePatch = StoragePatch
  { spScope :: !RelationalScope,
    spRowsByAtom :: !(IntMap RowDelta)
  }
  deriving stock (Eq, Show)

type StoragePatchResult :: Type
data StoragePatchResult = StoragePatchResult
  { sprStore :: !Store,
    sprScope :: !RelationalScope,
    sprRowIdDeltas :: !(IntMap (RowIdDelta RowTupleKey))
  }
  deriving stock (Eq, Show)

type StorageError :: Type
data StorageError
  = StorageMissingAtomKey !Int
  | StorageUnexpectedAtomKey !Int
  | StorageMissingPlannedRelation !Int
  | StorageRelationLayoutMismatch !Int !RowLayout !RowLayout
  | StorageRelationBuildError !Int !RelationPatchError
  | StorageRelationPatchKeyError !Int !RelationPatchError
  deriving stock (Eq, Show)

storeFromRelations :: IntMap Relation -> Store
storeFromRelations relations =
  Store
    { stRelations = relations,
      stSeparators = Map.empty,
      stSeparatorsByAtom = IntMap.empty
    }
{-# INLINE storeFromRelations #-}

storeFromRelationsWithPlan ::
  CompiledStoragePlan ->
  IntMap Relation ->
  Either StorageError Store
storeFromRelationsWithPlan plan relations0 = do
  rejectUnexpectedAtoms (cspLayouts plan) relations0
  relations <-
    IntMap.traverseWithKey
      (plannedRelation relations0)
      (cspLayouts plan)
  pure
    Store
      { stRelations = relations,
        stSeparators = buildPlannedSeparators plan relations,
        stSeparatorsByAtom = cspSeparatorsByAtom plan
      }
{-# INLINE storeFromRelationsWithPlan #-}

storeFromPlan ::
  CompiledStoragePlan ->
  IntMap RowDelta ->
  Either StorageError Store
storeFromPlan plan initialRowsByAtom = do
  rejectUnexpectedAtoms (cspLayouts plan) initialRowsByAtom
  relations <-
    IntMap.traverseWithKey
      buildRelation
      (cspLayouts plan)
  storeFromRelationsWithPlan plan relations
  where
    buildRelation atomKey schema =
      first
        (StorageRelationBuildError atomKey)
        ( relationFromRows
            schema
            ( positivePlainRowPatchRows
                (IntMap.findWithDefault emptyPlainRowPatch atomKey initialRowsByAtom)
            )
        )
{-# INLINE storeFromPlan #-}

storeWithPlannedAtomRows ::
  CompiledStoragePlan ->
  Int ->
  RowDelta ->
  Store ->
  Either StorageError Store
storeWithPlannedAtomRows plan atomKey rows store = do
  schema <-
    case IntMap.lookup atomKey (cspLayouts plan) of
      Nothing ->
        Left (StorageUnexpectedAtomKey atomKey)
      Just layout ->
        Right layout
  relation <-
    first
      (StorageRelationBuildError atomKey)
      (relationFromRows schema (positivePlainRowPatchRows rows))
  let relations =
        IntMap.insert atomKey relation (stRelations store)
      separators =
        Foldable.foldl'
          (\cache separator -> Map.insert separator (buildSeparatorIndex relation (ssSlots separator)) cache)
          (stSeparators store)
          (IntMap.findWithDefault [] atomKey (cspSeparatorsByAtom plan))
  pure
    Store
      { stRelations = relations,
        stSeparators = separators,
        stSeparatorsByAtom = cspSeparatorsByAtom plan
      }
{-# INLINE storeWithPlannedAtomRows #-}

rejectUnexpectedAtoms :: IntMap expected -> IntMap actual -> Either StorageError ()
rejectUnexpectedAtoms expected actual =
  case IntMap.lookupMin (IntMap.difference actual expected) of
    Nothing ->
      Right ()
    Just (atomKey, _) ->
      Left (StorageUnexpectedAtomKey atomKey)
{-# INLINE rejectUnexpectedAtoms #-}

plannedRelation ::
  IntMap Relation ->
  Int ->
  RowLayout ->
  Either StorageError Relation
plannedRelation relations atomKey schema =
  case IntMap.lookup atomKey relations of
    Nothing ->
      Left (StorageMissingPlannedRelation atomKey)
    Just relation
      | relationLayout relation == schema ->
          Right relation
      | otherwise ->
          Left (StorageRelationLayoutMismatch atomKey schema (relationLayout relation))
{-# INLINE plannedRelation #-}

buildPlannedSeparators ::
  CompiledStoragePlan ->
  IntMap Relation ->
  Map SeparatorSpec SeparatorIndex
buildPlannedSeparators plan relations =
  Map.fromList
    [ (separator, buildSeparatorIndex relation (ssSlots separator))
      | separator <- Set.toAscList (cspSeparators plan),
        Just relation <- [IntMap.lookup (atomIdKey (ssAtom separator)) relations]
    ]
{-# INLINE buildPlannedSeparators #-}

storeRelations :: Store -> IntMap Relation
storeRelations =
  stRelations
{-# INLINE storeRelations #-}

lookupRelation :: AtomId -> Store -> Maybe Relation
lookupRelation atomId store =
  IntMap.lookup (atomIdKey atomId) (storeRelations store)
{-# INLINE lookupRelation #-}

storeSeparatorCache :: Store -> Map SeparatorSpec SeparatorIndex
storeSeparatorCache =
  stSeparators
{-# INLINE storeSeparatorCache #-}

applyStoragePatch ::
  StoragePatch ->
  Store ->
  Either StorageError StoragePatchResult
applyStoragePatch patch store0 = do
  (!store1, !rowDeltas) <-
    IntMap.foldlWithKey'
      step
      (Right (store0, IntMap.empty))
      (spRowsByAtom patch)
  pure
    StoragePatchResult
      { sprStore = store1,
        sprScope = spScope patch,
        sprRowIdDeltas = rowDeltas
      }
  where
    step eitherState _atomKey rowDelta
      | rowDeltaNull rowDelta =
          eitherState
    step eitherState atomKey rowDelta = do
      (!store, !deltas) <- eitherState
      (!store', !rowIdDelta) <- patchRelationByKey atomKey rowDelta store
      pure
        ( store',
          if rowIdDeltaNull rowIdDelta
            then deltas
            else IntMap.insert atomKey rowIdDelta deltas
        )
{-# INLINE applyStoragePatch #-}

patchRelationByKey ::
  Int ->
  RowDelta ->
  Store ->
  Either StorageError (Store, RowIdDelta RowTupleKey)
patchRelationByKey atomKey rowDelta store = do
  relation0 <-
    maybe
      (Left (StorageMissingAtomKey atomKey))
      Right
      (IntMap.lookup atomKey (storeRelations store))
  patchRelation atomKey (StorageRelationPatchKeyError atomKey) relation0 rowDelta store
{-# INLINE patchRelationByKey #-}

patchRelation ::
  Int ->
  (RelationPatchError -> StorageError) ->
  Relation ->
  RowDelta ->
  Store ->
  Either StorageError (Store, RowIdDelta RowTupleKey)
patchRelation atomKey wrapError relation0 rowDelta store = do
  (!relation1, !rowIdDelta0) <-
    first
      wrapError
      (applyRelationPatchTracked rowDelta relation0)
  let !rowIdDelta =
        normalizeRowIdDelta rowIdDelta0
      !sepCache1 =
        updateTouchedSeparators atomKey relation1 rowIdDelta store
      !store1 =
        store
          { stRelations = IntMap.insert atomKey relation1 (storeRelations store),
            stSeparators = sepCache1
          }
  pure (store1, rowIdDelta)
{-# INLINE patchRelation #-}

updateTouchedSeparators ::
  Int ->
  Relation ->
  RowIdDelta RowTupleKey ->
  Store ->
  Map SeparatorSpec SeparatorIndex
updateTouchedSeparators atomKey relation rowIdDelta store
  | rowIdDeltaNull rowIdDelta =
      storeSeparatorCache store
  | otherwise =
      Foldable.foldl'
        ( \cache separator ->
            Map.adjust
              (applySeparatorIndexDelta relation rowIdDelta)
              separator
              cache
        )
        (storeSeparatorCache store)
        (IntMap.findWithDefault [] atomKey (stSeparatorsByAtom store))
{-# INLINE updateTouchedSeparators #-}

lookupSeparatorIndex :: AtomId -> [SlotId] -> Store -> Maybe SeparatorIndex
lookupSeparatorIndex atomId sep store =
  Map.lookup (SeparatorSpec atomId (Vector.fromList sep)) (storeSeparatorCache store)
{-# INLINE lookupSeparatorIndex #-}
