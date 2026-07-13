module Test.Moonlight.Differential.Index.IndexedRows
  ( indexedRowsWithLiveRowsForValidation,
    indexedRowsWithKeyByRowIdForValidation,
    indexedRowsWithIdByKeyForValidation,
    indexedRowsWithValueIndexForValidation,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntSet
  ( IntSet,
  )
import Data.Map.Strict
  ( Map,
  )
import Moonlight.Differential.Index.IndexedRows
  ( IndexedRows,
  )
import Moonlight.Differential.Index.RowIdSet
  ( RowIdSet,
  )
import Moonlight.Differential.Internal.Index.IndexedRows
  ( irIdByKey,
    irKeyByRowId,
    irLiveRows,
    irValueIx,
  )

indexedRowsWithLiveRowsForValidation ::
  IntSet ->
  IndexedRows layout key payload ->
  IndexedRows layout key payload
indexedRowsWithLiveRowsForValidation liveRows rows =
  rows {irLiveRows = liveRows}
{-# INLINE indexedRowsWithLiveRowsForValidation #-}

indexedRowsWithKeyByRowIdForValidation ::
  IntMap key ->
  IndexedRows layout key payload ->
  IndexedRows layout key payload
indexedRowsWithKeyByRowIdForValidation keyByRowId rows =
  rows {irKeyByRowId = keyByRowId}
{-# INLINE indexedRowsWithKeyByRowIdForValidation #-}

indexedRowsWithIdByKeyForValidation ::
  Map key Int ->
  IndexedRows layout key payload ->
  IndexedRows layout key payload
indexedRowsWithIdByKeyForValidation idByKey rows =
  rows {irIdByKey = idByKey}
{-# INLINE indexedRowsWithIdByKeyForValidation #-}

indexedRowsWithValueIndexForValidation ::
  IntMap (IntMap RowIdSet) ->
  IndexedRows layout key payload ->
  IndexedRows layout key payload
indexedRowsWithValueIndexForValidation valueIndex rows =
  rows {irValueIx = valueIndex}
{-# INLINE indexedRowsWithValueIndexForValidation #-}
