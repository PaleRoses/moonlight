{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Index.RowArrangement
  ( IndexedRowArrangement (..),
    indexedRowArrangementFromRows,
    indexedRowArrangementFromRowsWithSections,
    indexedRowArrangementWithRows,
    indexedRowArrangementWithDirtyRows,
    indexedRowArrangementRestrictToDirtyRows,
    indexedRowArrangementWithDirtyKeys,
    indexedRowArrangementRestrictRowsByPins,
    indexedRowArrangementColumnIndex,
    indexedRowArrangementLayout,
    indexedRowArrangementValueIndex,
    indexedRowArrangementKeyAt,
    indexedRowArrangementPayloadAt,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Differential.Index.IndexedRows
  ( IndexedRows,
    indexedRowsColumnIndex,
    indexedRowsIdByKey,
    indexedRowsKeyAt,
    indexedRowsLayout,
    indexedRowsLiveRows,
    indexedRowsPayloadAtRowId,
    indexedRowsRestrictRowsByPins,
    indexedRowsValueIndex,
  )
import Moonlight.Differential.Index.RowIdSet
  ( RowIdSet,
  )
import Moonlight.Differential.Index.RowId
  ( RowId,
  )
import Moonlight.Differential.Index.RowSet
  ( RowSet,
    emptyRowSet,
    rowSetFromIntSetCanonical,
  )

-- | A physical arrangement over indexed rows plus the two local sections WCOJ
-- needs for incremental descent: visible rows and dirty rows.
--
-- The constructor stays public intentionally: this is caller-owned operational
-- state, not the canonical indexed-row invariant.  The authoritative row
-- universe lives in 'IndexedRows'; the visible and dirty sections are local
-- execution cuts, and callers such as dense-flow planning may stage speculative
-- sections before restriction or pin filtering normalizes them.
type IndexedRowArrangement :: Type -> Type -> Type -> Type
data IndexedRowArrangement layout key payload = IndexedRowArrangement
  { indexedRowArrangementRows :: !(IndexedRows layout key payload),
    indexedRowArrangementVisibleRows :: !RowSet,
    indexedRowArrangementDirtyRows :: !RowSet
  }
  deriving stock (Eq, Show)

indexedRowArrangementFromRows :: IndexedRows layout key payload -> IndexedRowArrangement layout key payload
indexedRowArrangementFromRows rows =
  IndexedRowArrangement
    { indexedRowArrangementRows = rows,
      indexedRowArrangementVisibleRows = rowSetFromIntSetCanonical (indexedRowsLiveRows rows),
      indexedRowArrangementDirtyRows = emptyRowSet
    }

indexedRowArrangementFromRowsWithSections ::
  IndexedRows layout key payload ->
  RowSet ->
  RowSet ->
  IndexedRowArrangement layout key payload
indexedRowArrangementFromRowsWithSections rows visibleRows dirtyRows =
  IndexedRowArrangement
    { indexedRowArrangementRows = rows,
      indexedRowArrangementVisibleRows = visibleRows,
      indexedRowArrangementDirtyRows = dirtyRows
    }

indexedRowArrangementWithRows ::
  RowSet ->
  RowSet ->
  IndexedRowArrangement layout key payload ->
  IndexedRowArrangement layout key payload
indexedRowArrangementWithRows visibleRows dirtyRows arrangement =
  arrangement
    { indexedRowArrangementVisibleRows = visibleRows,
      indexedRowArrangementDirtyRows = dirtyRows
    }

indexedRowArrangementWithDirtyRows ::
  RowSet ->
  IndexedRowArrangement layout key payload ->
  IndexedRowArrangement layout key payload
indexedRowArrangementWithDirtyRows dirtyRows arrangement =
  indexedRowArrangementWithRows
    (indexedRowArrangementVisibleRows arrangement)
    dirtyRows
    arrangement

indexedRowArrangementRestrictToDirtyRows ::
  IndexedRowArrangement layout key payload ->
  IndexedRowArrangement layout key payload
indexedRowArrangementRestrictToDirtyRows arrangement =
  let !dirtyRows =
        indexedRowArrangementDirtyRows arrangement
   in indexedRowArrangementWithRows dirtyRows dirtyRows arrangement

indexedRowArrangementWithDirtyKeys ::
  Ord key =>
  Set key ->
  IndexedRowArrangement layout key payload ->
  IndexedRowArrangement layout key payload
indexedRowArrangementWithDirtyKeys dirtyKeys arrangement =
  indexedRowArrangementWithDirtyRows
    (rowSetFromIntSetCanonical (dirtyRowIdsForKeys dirtyKeys arrangement))
    arrangement

indexedRowArrangementRestrictRowsByPins ::
  IntMap Int ->
  IndexedRowArrangement layout key payload ->
  IndexedRowArrangement layout key payload
indexedRowArrangementRestrictRowsByPins pins arrangement =
  indexedRowArrangementWithRows
    (restrictRowsByPins pins arrangement (indexedRowArrangementVisibleRows arrangement))
    (restrictRowsByPins pins arrangement (indexedRowArrangementDirtyRows arrangement))
    arrangement

indexedRowArrangementColumnIndex :: IndexedRowArrangement layout key payload -> IntMap Int
indexedRowArrangementColumnIndex =
  indexedRowsColumnIndex . indexedRowArrangementRows

indexedRowArrangementLayout :: IndexedRowArrangement layout key payload -> layout
indexedRowArrangementLayout =
  indexedRowsLayout . indexedRowArrangementRows

indexedRowArrangementValueIndex :: IndexedRowArrangement layout key payload -> IntMap (IntMap RowIdSet)
indexedRowArrangementValueIndex =
  indexedRowsValueIndex . indexedRowArrangementRows

indexedRowArrangementKeyAt :: IndexedRowArrangement layout key payload -> RowId -> Maybe key
indexedRowArrangementKeyAt arrangement rowId =
  indexedRowsKeyAt rowId (indexedRowArrangementRows arrangement)

indexedRowArrangementPayloadAt ::
  Ord key =>
  IndexedRowArrangement layout key payload ->
  RowId ->
  Maybe payload
indexedRowArrangementPayloadAt arrangement rowId =
  indexedRowsPayloadAtRowId rowId (indexedRowArrangementRows arrangement)

dirtyRowIdsForKeys ::
  Ord key =>
  Set key ->
  IndexedRowArrangement layout key payload ->
  IntSet
dirtyRowIdsForKeys dirtyKeys arrangement =
  IntSet.fromList
    [ rowId
    | key <- Set.toAscList dirtyKeys,
      rowId <- maybeToList (Map.lookup key (indexedRowsIdByKey (indexedRowArrangementRows arrangement)))
    ]

restrictRowsByPins ::
  IntMap Int ->
  IndexedRowArrangement layout key payload ->
  RowSet ->
  RowSet
restrictRowsByPins pins arrangement rows =
  indexedRowsRestrictRowsByPins (indexedRowArrangementRows arrangement) rows pins
