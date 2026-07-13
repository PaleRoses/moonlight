{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Storage.Restriction
  ( Restriction,
    emptyRestriction,
    restrictionFromSlots,
    restrictRootSlot,
    restrictPinnedRow,
    restrictionSlotValues,
    restrictionSlotValueSets,
    restrictionPinnedRowsByAtom,
    applyRestriction,
    restrictionDigest,
  )
where

import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List qualified as List
import Data.Word (Word64)
import Moonlight.Core
  ( AtomId,
    SlotId,
    atomIdKey,
    slotIdKey,
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Differential.Index.IndexedRows
  ( indexedRowsColumnIndex,
    indexedRowsNextRowId,
    indexedRowsValueIndex,
  )
import Moonlight.Flow.Storage.Relation
  ( Relation (..),
    rowIdForRow,
  )
import Moonlight.Flow.Storage.Store
  ( Store,
    storeRelations,
  )
import Moonlight.Flow.Storage.View
  ( View,
    intersectViewRows,
    setViewRows,
  )
import Moonlight.Differential.Index.RowIdSet
  ( rowIdSetUnionIntoIntSet,
  )
import Moonlight.Differential.Index.RowSet
  ( emptyRowSet,
    rowSetFromIntSetWithUniverse,
    singletonRowSet,
  )
import Moonlight.Flow.Internal.Digest
  ( digestWordsHigh,
    digestWordsLow,
    mix64,
    wordOfInt,
  )

type PinnedAtomRow :: Type
data PinnedAtomRow
  = PinnedAtomRow !RowTupleKey
  | PinnedAtomRowImpossible
  deriving stock (Eq, Show)

instance Semigroup PinnedAtomRow where
  PinnedAtomRowImpossible <> _ =
    PinnedAtomRowImpossible
  _ <> PinnedAtomRowImpossible =
    PinnedAtomRowImpossible
  PinnedAtomRow left <> PinnedAtomRow right
    | left == right =
        PinnedAtomRow left
    | otherwise =
        PinnedAtomRowImpossible
  {-# INLINE (<>) #-}

type Restriction :: Type
data Restriction = Restriction
  { vrSlotValues :: !(IntMap (HashSet RepKey)),
    vrPinnedRows :: !(IntMap PinnedAtomRow)
  }
  deriving stock (Eq, Show)

instance Semigroup Restriction where
  left <> right =
    Restriction
      { vrSlotValues =
          IntMap.unionWith
            HashSet.intersection
            (vrSlotValues left)
            (vrSlotValues right),
        vrPinnedRows =
          IntMap.unionWith
            (<>)
            (vrPinnedRows left)
            (vrPinnedRows right)
      }
  {-# INLINE (<>) #-}

instance Monoid Restriction where
  mempty =
    emptyRestriction
  {-# INLINE mempty #-}

emptyRestriction :: Restriction
emptyRestriction =
  Restriction
    { vrSlotValues = IntMap.empty,
      vrPinnedRows = IntMap.empty
    }
{-# INLINE emptyRestriction #-}

restrictionFromSlots ::
  IntMap (HashSet RepKey) ->
  Restriction
restrictionFromSlots pinnedSlots =
  Restriction
    { vrSlotValues = pinnedSlots,
      vrPinnedRows = IntMap.empty
    }
{-# INLINE restrictionFromSlots #-}

restrictRootSlot ::
  SlotId ->
  IntSet ->
  Restriction
restrictRootSlot rootSlot rootKeys =
  restrictionFromSlots
    ( IntMap.singleton
        (slotIdKey rootSlot)
        (HashSet.fromList [RepKey key | key <- IntSet.toList rootKeys])
    )
{-# INLINE restrictRootSlot #-}

restrictPinnedRow ::
  AtomId ->
  RowTupleKey ->
  Restriction
restrictPinnedRow atomId row =
  Restriction
    { vrSlotValues = IntMap.empty,
      vrPinnedRows =
        IntMap.singleton
          (atomIdKey atomId)
          (PinnedAtomRow row)
    }
{-# INLINE restrictPinnedRow #-}

restrictionSlotValues ::
  Restriction ->
  SlotId ->
  Maybe (HashSet RepKey)
restrictionSlotValues restriction slot =
  IntMap.lookup (slotIdKey slot) (vrSlotValues restriction)
{-# INLINE restrictionSlotValues #-}

restrictionSlotValueSets :: Restriction -> IntMap (HashSet RepKey)
restrictionSlotValueSets =
  vrSlotValues
{-# INLINE restrictionSlotValueSets #-}

restrictionPinnedRowsByAtom :: Restriction -> IntMap (Maybe RowTupleKey)
restrictionPinnedRowsByAtom =
  fmap pinnedAtomRowValue . vrPinnedRows
  where
    pinnedAtomRowValue pinned =
      case pinned of
        PinnedAtomRowImpossible ->
          Nothing
        PinnedAtomRow row ->
          Just row
{-# INLINE restrictionPinnedRowsByAtom #-}

applyRestriction ::
  Restriction ->
  Store ->
  View ->
  View
applyRestriction restriction store =
  restrictPinnedRows store (vrPinnedRows restriction)
    . restrictSlotValues store (vrSlotValues restriction)
{-# INLINE applyRestriction #-}

restrictSlotValues ::
  Store ->
  IntMap (HashSet RepKey) ->
  View ->
  View
restrictSlotValues store pinnedSlots view0 =
  IntMap.foldlWithKey'
    (restrictAtomSlotValues store pinnedSlots)
    view0
    (storeRelations store)
{-# INLINE restrictSlotValues #-}

restrictAtomSlotValues ::
  Store ->
  IntMap (HashSet RepKey) ->
  View ->
  Int ->
  Relation ->
  View
restrictAtomSlotValues store pinnedSlots view atomKey relation =
  IntMap.foldlWithKey'
    restrictSlot
    view
    pinnedSlots
  where
    restrictSlot acc slotKey allowedReps
      | IntMap.notMember slotKey (indexedRowsColumnIndex (relRows relation)) =
          acc
      | otherwise =
          let !matchingIds =
                HashSet.foldl'
                  (unionAllowedRepBucket relation slotKey)
                  IntSet.empty
                  allowedReps
              !matching =
                rowSetFromIntSetWithUniverse
                  (indexedRowsNextRowId (relRows relation))
                  matchingIds
           in intersectViewRows store atomKey matching acc
{-# INLINE restrictAtomSlotValues #-}

unionAllowedRepBucket ::
  Relation ->
  Int ->
  IntSet ->
  RepKey ->
  IntSet
unionAllowedRepBucket relation slotKey rowIds (RepKey repKey) =
  case IntMap.lookup slotKey (indexedRowsValueIndex (relRows relation)) >>= IntMap.lookup repKey of
    Nothing ->
      rowIds
    Just bucket ->
      rowIdSetUnionIntoIntSet bucket rowIds
{-# INLINE unionAllowedRepBucket #-}

restrictPinnedRows ::
  Store ->
  IntMap PinnedAtomRow ->
  View ->
  View
restrictPinnedRows store pinnedRows view0 =
  IntMap.foldlWithKey'
    (restrictPinnedRowAt store)
    view0
    pinnedRows
{-# INLINE restrictPinnedRows #-}

restrictPinnedRowAt ::
  Store ->
  View ->
  Int ->
  PinnedAtomRow ->
  View
restrictPinnedRowAt store view atomKey pinned =
  case pinned of
    PinnedAtomRowImpossible ->
      setViewRows store atomKey emptyRowSet view
    PinnedAtomRow row ->
      case IntMap.lookup atomKey (storeRelations store) of
        Nothing ->
          setViewRows store atomKey emptyRowSet view
        Just relation ->
          case rowIdForRow relation row of
            Nothing ->
              setViewRows store atomKey emptyRowSet view
            Just rowId ->
              intersectViewRows store atomKey (singletonRowSet rowId) view
{-# INLINE restrictPinnedRowAt #-}

restrictionDigest ::
  Restriction ->
  (Word64, Word64)
restrictionDigest restriction =
  let words0 =
        [0x7669657752657374]
          <> IntMap.foldMapWithKey slotWords (vrSlotValues restriction)
          <> IntMap.foldMapWithKey pinnedWords (vrPinnedRows restriction)
   in (digestWordsHigh words0, digestWordsLow words0)
{-# INLINE restrictionDigest #-}

slotWords ::
  Int ->
  HashSet RepKey ->
  [Word64]
slotWords slotKey reps =
  [0x10, wordOfInt slotKey, wordOfInt (HashSet.size reps)]
    <> fmap repWord (List.sort (HashSet.toList reps))
  where
    repWord (RepKey repKey) =
      mix64 0x11 (wordOfInt repKey)
{-# INLINE slotWords #-}

pinnedWords ::
  Int ->
  PinnedAtomRow ->
  [Word64]
pinnedWords atomKey pinned =
  case pinned of
    PinnedAtomRowImpossible ->
      [0x20, wordOfInt atomKey, 0]
    PinnedAtomRow row ->
      [0x21, wordOfInt atomKey, wordOfInt (tupleKeyWidth row)]
        <> fmap wordOfInt (tupleKeyToInts row)
{-# INLINE pinnedWords #-}
