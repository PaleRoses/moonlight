module Moonlight.Flow.Storage.Index.TupleFormat
  ( emptyIndexedRows,
    tupleKeyIndexedFormat,
    indexedTupleArrangementValueAt,
    repKeyPins,
    rowLayoutColumnIndex,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Vector qualified as Vector
import Moonlight.Core
  ( SlotId,
    slotIdKey,
  )
import Moonlight.Differential.Index.IndexedRows
  ( IndexedRowBindingError (..),
    IndexedRowFormat,
    IndexedRows,
    indexedRowFormat,
    indexedRowsColumnIndex,
  )
import Moonlight.Differential.Index.IndexedRows qualified as IndexedRows
import Moonlight.Differential.Index.RowArrangement
  ( IndexedRowArrangement,
    indexedRowArrangementKeyAt,
    indexedRowArrangementRows,
  )
import Moonlight.Differential.Index.RowId
  ( RowId,
  )
import Moonlight.Differential.Row.Tuple
  ( RepKey (..),
    TupleKey,
    tupleKeyIndex,
    tupleKeyIndexInt,
    tupleKeyWidth,
  )
import Moonlight.Differential.Row.Block
  ( RowLayout,
  )

emptyIndexedRows :: RowLayout -> IndexedRows RowLayout key payload
emptyIndexedRows =
  IndexedRows.emptyIndexedRows rowLayoutColumnIndex

rowLayoutColumnIndex :: RowLayout -> IntMap Int
rowLayoutColumnIndex =
  Vector.ifoldl'
    (\index ix sid -> IntMap.insert (slotIdKey sid) ix index)
    IntMap.empty

tupleKeyIndexedFormat :: IndexedRowFormat RowLayout (TupleKey tupleRole)
tupleKeyIndexedFormat =
  indexedRowFormat
    tupleKeyWidth
    Vector.length
    ( \schema row step initial ->
        if tupleKeyWidth row /= Vector.length schema
          then Left (IndexedRowWidthMismatch row (Vector.length schema) (tupleKeyWidth row))
          else
            Vector.ifoldl'
              ( \eitherAcc ix slot ->
                  eitherAcc >>= \acc ->
                    case tupleKeyIndexInt row ix of
                      Nothing ->
                        Left (IndexedRowBindingsRejected schema row)
                      Just repKey ->
                        Right (step (slotIdKey slot) repKey acc)
              )
              (Right initial)
              schema
    )

indexedTupleArrangementValueAt ::
  IndexedRowArrangement RowLayout (TupleKey tupleRole) payload ->
  SlotId ->
  RowId ->
  Maybe RepKey
indexedTupleArrangementValueAt arrangement sid rowId = do
  key <- indexedRowArrangementKeyAt arrangement rowId
  col <- IntMap.lookup (slotIdKey sid) (indexedRowsColumnIndex rows)
  tupleKeyIndex key col
  where
    rows =
      indexedRowArrangementRows arrangement

repKeyPins :: IntMap RepKey -> IntMap Int
repKeyPins =
  IntMap.map unRepKey
