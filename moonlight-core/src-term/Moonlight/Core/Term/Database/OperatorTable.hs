module Moonlight.Core.Term.Database.OperatorTable where

import Data.Foldable (toList)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.Kind (Type)
import Data.Maybe (isJust, mapMaybe)
import Moonlight.Core.Term.Database.Index
import Moonlight.Core.Term.Database.Types
import Prelude

emptyOperatorTable :: Foldable f => f () -> OperatorTable f
emptyOperatorTable shape =
  OperatorTable
    { opShape = shape,
      opArity = arity,
      rowMap = IntMap.empty,
      nextRowId = 0,
      resultIx = emptyResultIndex,
      childColumnIx = emptyChildColumnValueIndex arity,
      exactIx = emptyExactIndex arity,
      exactResultIx = emptyExactResultIndex arity,
      tupleIdentityIx = emptyTupleIdentityIndex,
      childUserIx = emptyChildUserIndex
    }
  where
    arity = length (toList shape)
{-# INLINE emptyOperatorTable #-}

operatorTableFromRows :: Foldable f => f () -> [DatabaseRow] -> OperatorTable f
operatorTableFromRows shape rows =
  OperatorTable
    { opShape = shape,
      opArity = arity,
      rowMap = IntMap.fromDistinctAscList indexedRows,
      nextRowId = rowCount,
      resultIx =
        foldl'
          (\resultIndex (rowKey, row) ->
             insertResultIndex rowKey (rowResult row) resultIndex)
          emptyResultIndex
          indexedRows,
      childColumnIx =
        foldl'
          (\childColumnIndexes (rowKey, row) ->
             insertChildColumnValueIndex rowKey (rowChildren row) childColumnIndexes)
          (emptyChildColumnValueIndex arity)
          indexedRows,
      exactIx =
        foldl'
          (\exactIndex (rowKey, row) ->
             insertExactIndex rowKey (rowChildren row) exactIndex)
          (emptyExactIndex arity)
          indexedRows,
      exactResultIx =
        foldl'
          (\exactResultIndex (_rowKey, row) ->
             insertExactResultIndex (rowResult row) (rowChildren row) exactResultIndex)
          (emptyExactResultIndex arity)
          indexedRows,
      tupleIdentityIx =
        foldl'
          (\tupleIdentityIndex (rowKey, row) ->
             insertTupleIdentityIndex rowKey row tupleIdentityIndex)
          emptyTupleIdentityIndex
          indexedRows,
      childUserIx =
        foldl'
          (\childUsers (rowKey, row) ->
             foldr
               (\childKey -> insertChildUserIndex rowKey childKey)
               childUsers
               (rowChildren row))
          emptyChildUserIndex
          indexedRows
    }
  where
    arity = length (toList shape)
    rowCount = length rows
    indexedRows = zip (ascendingRowKeys rowCount) rows
{-# INLINE operatorTableFromRows #-}

tableLiveRows :: OperatorTable f -> RowIdSet
tableLiveRows table =
  RowIdSet (IntMap.keysSet (rowMap table))
{-# INLINE tableLiveRows #-}

tableLiveRowCount :: OperatorTable f -> Int
tableLiveRowCount =
  IntMap.size . rowMap
{-# INLINE tableLiveRowCount #-}

applyOperatorTableRowEdit :: OperatorTableRowEdit -> OperatorTable f -> OperatorTable f
applyOperatorTableRowEdit edit table =
  case edit of
    InsertOperatorTableRow rowKey row ->
      table
        { resultIx =
            insertResultIndex rowKey (rowResult row) (resultIx table),
          childColumnIx =
            insertChildColumnValueIndex rowKey (rowChildren row) (childColumnIx table),
          exactIx =
            insertExactIndex rowKey (rowChildren row) (exactIx table),
          exactResultIx =
            insertExactResultIndex (rowResult row) (rowChildren row) (exactResultIx table),
          tupleIdentityIx =
            insertTupleIdentityIndex rowKey row (tupleIdentityIx table),
          childUserIx =
            foldr
              (insertChildUserIndex rowKey)
              (childUserIx table)
              (rowChildren row)
        }
    DeleteOperatorTableRow rowKey row ->
      table
        { resultIx =
            deleteResultIndex rowKey (rowResult row) (resultIx table),
          childColumnIx =
            deleteChildColumnValueIndex rowKey (rowChildren row) (childColumnIx table),
          exactIx =
            deleteExactIndex rowKey (rowChildren row) (exactIx table),
          exactResultIx =
            deleteExactResultIndex (rowResult row) (rowChildren row) (exactResultIx table),
          tupleIdentityIx =
            deleteTupleIdentityIndex row (tupleIdentityIx table),
          childUserIx =
            foldr
              (deleteChildUserIndex rowKey)
              (childUserIx table)
              (rowChildren row)
        }
{-# INLINE applyOperatorTableRowEdit #-}

insertEncodedRow :: DatabaseRow -> OperatorTable f -> OperatorTable f
insertEncodedRow row table
  | encodedRowPresent row table = table
  | otherwise = appendEncodedRow row table

insertEncodedRowsWithInsertedRows :: [DatabaseRow] -> OperatorTable f -> ([(RowId, DatabaseRow)], OperatorTable f)
insertEncodedRowsWithInsertedRows rows table =
  case indexedAcceptedRows of
    [] ->
      ([], table)
    _ ->
      ( fmap (\(rowKey, row) -> (RowId rowKey, row)) indexedAcceptedRows,
        foldl'
          (\currentTable (rowKey, row) ->
             applyOperatorTableRowEdit (InsertOperatorTableRow rowKey row) currentTable)
          tableWithRows
          indexedAcceptedRows
      )
  where
    acceptance =
      acceptEncodedRows rows table
    indexedAcceptedRows =
      reverse (acceptedReversedRows acceptance)
    acceptedRowMap =
      IntMap.fromDistinctAscList indexedAcceptedRows
    tableWithRows =
      table
        { rowMap =
            case IntMap.null (rowMap table) of
              True ->
                acceptedRowMap
              False ->
                IntMap.union (rowMap table) acceptedRowMap,
          nextRowId =
            acceptedNextRowId acceptance
        }
{-# INLINE insertEncodedRowsWithInsertedRows #-}

type EncodedRowAcceptance :: Type
data EncodedRowAcceptance = EncodedRowAcceptance
  { acceptedReversedRows :: ![(Int, DatabaseRow)],
    acceptedNextRowId :: !Int,
    acceptedTupleIdentity :: !TupleIdentityIndex
  }

acceptEncodedRows :: [DatabaseRow] -> OperatorTable f -> EncodedRowAcceptance
acceptEncodedRows rows table =
  foldl'
    acceptEncodedRow
    ( EncodedRowAcceptance
        { acceptedReversedRows = [],
          acceptedNextRowId = nextRowId table,
          acceptedTupleIdentity = tupleIdentityIx table
        }
    )
    rows
  where

    acceptEncodedRow ::
      EncodedRowAcceptance ->
      DatabaseRow ->
      EncodedRowAcceptance
    acceptEncodedRow acceptance row
      | isJust (lookupTupleIdentityIndex row (acceptedTupleIdentity acceptance)) =
          acceptance
      | otherwise =
          acceptance
            { acceptedReversedRows =
                (rowKey, row) : acceptedReversedRows acceptance,
              acceptedNextRowId = rowKey + 1,
              acceptedTupleIdentity =
                insertTupleIdentityIndex rowKey row (acceptedTupleIdentity acceptance)
            }
      where
        rowKey =
          acceptedNextRowId acceptance
{-# INLINE acceptEncodedRows #-}

appendEncodedRow :: DatabaseRow -> OperatorTable f -> OperatorTable f
appendEncodedRow row table =
  applyOperatorTableRowEdit (InsertOperatorTableRow rowKey row) $
    table
      { rowMap = IntMap.insert rowKey row (rowMap table),
        nextRowId = rowKey + 1
      }
  where
    rowKey = nextRowId table

encodedRowPresent :: DatabaseRow -> OperatorTable f -> Bool
encodedRowPresent row table =
  isJust (lookupTupleIdentityIndex row (tupleIdentityIx table))

operatorTableRows :: OperatorTable f -> [(RowId, DatabaseRow)]
operatorTableRows table =
  rowsForIds table (tableLiveRows table)

operatorTableRowsForResultKey :: Int -> OperatorTable f -> [(RowId, DatabaseRow)]
operatorTableRowsForResultKey resultKey table =
  rowsForIds table (lookupResultIndex resultKey (derivedResultIndex table))

lookupChildResultKeys :: [Int] -> OperatorTable f -> IntSet
lookupChildResultKeys childKeys table =
  lookupExactResultIndex childKeys (exactResultIx table)

rowsForIds :: OperatorTable f -> RowIdSet -> [(RowId, DatabaseRow)]
rowsForIds table rowIds =
  mapMaybe rowForId (rowIdSetToAscList rowIds)
  where
    rowForId rowKey =
      fmap (\row -> (RowId rowKey, row)) (liveTableRow rowKey table)

deleteEncodedRows :: DatabaseRow -> OperatorTable f -> OperatorTable f
deleteEncodedRows row table =
  maybe table (`deleteEncodedRow` table) (lookupTupleIdentityIndex row (tupleIdentityIx table))

deleteEncodedRow :: Int -> OperatorTable f -> OperatorTable f
deleteEncodedRow rowKey table =
  maybe table (\row -> deleteIndexedRow rowKey row table) (liveTableRow rowKey table)

deleteIndexedRow :: Int -> DatabaseRow -> OperatorTable f -> OperatorTable f
deleteIndexedRow rowKey row table =
  applyOperatorTableRowEdit (DeleteOperatorTableRow rowKey row) $
    table
      { rowMap = IntMap.delete rowKey (rowMap table)
      }

liveTableRow :: Int -> OperatorTable f -> Maybe DatabaseRow
liveTableRow rowKey =
  tableRow rowKey
{-# INLINE liveTableRow #-}

tableRow :: Int -> OperatorTable f -> Maybe DatabaseRow
tableRow rowKey table =
  IntMap.lookup rowKey (rowMap table)
{-# INLINE tableRow #-}

ascendingRowKeys :: Int -> [Int]
ascendingRowKeys count
  | count <= 0 = []
  | otherwise = [0 .. count - 1]
{-# INLINE ascendingRowKeys #-}

childValueAt :: Int -> [Int] -> Maybe Int
childValueAt childIndex children =
  case drop childIndex children of
    childValue : _remainingChildren -> Just childValue
    [] -> Nothing
{-# INLINE childValueAt #-}

keepNonEmptyTable :: OperatorTable f -> Maybe (OperatorTable f)
keepNonEmptyTable table
  | IntMap.null (rowMap table) = Nothing
  | otherwise = Just table
