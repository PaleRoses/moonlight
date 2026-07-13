{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Core.Term.Database.Table where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Core.DenseKey (DenseKey (..))
import Moonlight.Core.Language (Language)
import Moonlight.Core.Term.Database.Encode
import Moonlight.Core.Term.Database.OperatorTable
import Moonlight.Core.Term.Database.Types
import Prelude

insertTuple ::
  (DenseKey key, Language f) =>
  key ->
  f key ->
  Database f key ->
  Database f key
insertTuple resultValue tupleValue =
  insertEncodedDatabaseRow
    (extractOperator tupleValue)
    (encodedRow resultValue tupleValue)

insertTuples ::
  forall key f.
  (DenseKey key, Language f) =>
  [(key, f key)] ->
  Database f key ->
  Database f key
insertTuples entries database =
  snd (insertTuplesWithInsertedRows entries database)

insertTuplesWithInsertedRows ::
  forall key f.
  (DenseKey key, Language f) =>
  [(key, f key)] ->
  Database f key ->
  (Map (Operator f) [(RowId, DatabaseRow)], Database f key)
insertTuplesWithInsertedRows entries database =
  Map.foldlWithKey'
    insertOperatorRows
    (Map.empty, database)
    (foldl' collectRowsByOperator Map.empty entries)
  where
    insertOperatorRows ::
      (Map (Operator f) [(RowId, DatabaseRow)], Database f key) ->
      Operator f ->
      [DatabaseRow] ->
      (Map (Operator f) [(RowId, DatabaseRow)], Database f key)
    insertOperatorRows (insertedRows, currentDatabase) operator rows =
      ( insertOperatorInsertedRows operator operatorInsertedRows insertedRows,
        updatedDatabase
      )
      where
        (operatorInsertedRows, updatedDatabase) =
          insertEncodedDatabaseRowsWithInsertedRows operator (reverse rows) currentDatabase

    collectRowsByOperator ::
      Map (Operator f) [DatabaseRow] ->
      (key, f key) ->
      Map (Operator f) [DatabaseRow]
    collectRowsByOperator operatorRows (resultValue, tupleValue) =
      Map.insertWith
        (<>)
        (extractOperator tupleValue)
        [encodedRow resultValue tupleValue]
        operatorRows

    insertOperatorInsertedRows ::
      Operator f ->
      [(RowId, DatabaseRow)] ->
      Map (Operator f) [(RowId, DatabaseRow)] ->
      Map (Operator f) [(RowId, DatabaseRow)]
    insertOperatorInsertedRows operator rows insertedRows =
      case rows of
        [] ->
          insertedRows
        _ ->
          Map.insert operator rows insertedRows

insertEncodedDatabaseRow ::
  (Foldable f, forall a. Ord a => Ord (f a)) =>
  Operator f ->
  DatabaseRow ->
  Database f key ->
  Database f key
insertEncodedDatabaseRow operator@(Operator shape) row database =
  case nextRowId updatedTable == nextRowId table of
    True ->
      database
    False ->
      invalidateOperatorArrangements operator $
        database
          { operatorTables =
              Map.insert operator updatedTable (operatorTables database)
          }
  where
    table =
      Map.findWithDefault
        (emptyOperatorTable shape)
        operator
        (operatorTables database)
    updatedTable =
      insertEncodedRow row table

insertEncodedDatabaseRowsWithInsertedRows ::
  (Foldable f, forall a. Ord a => Ord (f a)) =>
  Operator f ->
  [DatabaseRow] ->
  Database f key ->
  ([(RowId, DatabaseRow)], Database f key)
insertEncodedDatabaseRowsWithInsertedRows _operator [] database =
  ([], database)
insertEncodedDatabaseRowsWithInsertedRows operator@(Operator shape) rows database =
  case insertedRows of
    [] ->
      ([], database)
    _ ->
      ( insertedRows,
        invalidateOperatorArrangements operator $
          database
            { operatorTables =
                Map.insert operator updatedTable (operatorTables database)
            }
      )
  where
    table =
      Map.findWithDefault
        (emptyOperatorTable shape)
        operator
        (operatorTables database)
    (insertedRows, updatedTable) =
      insertEncodedRowsWithInsertedRows rows table

deleteRow ::
  (forall a. Ord a => Ord (f a)) =>
  Operator f ->
  RowId ->
  Database f key ->
  Database f key
deleteRow operator (RowId rowKey) database =
  deleteOperatorRows operator (deleteEncodedRow rowKey) database

deleteTuple ::
  (DenseKey key, Language f) =>
  key ->
  f key ->
  Database f key ->
  Database f key
deleteTuple resultValue tupleValue database =
  deleteOperatorRows operator (deleteEncodedRows row) database
  where
    operator =
      extractOperator tupleValue
    row =
      encodedRow resultValue tupleValue

deleteOperatorRows ::
  (forall a. Ord a => Ord (f a)) =>
  Operator f ->
  (OperatorTable f -> OperatorTable f) ->
  Database f key ->
  Database f key
deleteOperatorRows operator deleteRows database =
  case Map.lookup operator (operatorTables database) of
    Nothing ->
      database
    Just table ->
      let updatedTable =
            deleteRows table
       in if tableLiveRowCount updatedTable == tableLiveRowCount table
            then database
            else
              invalidateOperatorArrangements operator $
                database
                  { operatorTables =
                      Map.update
                        (const (keepNonEmptyTable updatedTable))
                        operator
                        (operatorTables database)
                  }

invalidateOperatorArrangements :: (forall a. Ord a => Ord (f a)) => Operator f -> Database f key -> Database f key
invalidateOperatorArrangements operator database =
  case Map.null (arrangements database) of
    True ->
      database
    False ->
      database
        { arrangements =
            Map.delete operator (arrangements database)
        }
{-# INLINE invalidateOperatorArrangements #-}

compact :: Foldable f => Database f key -> Database f key
compact database =
  database
    { operatorTables =
        Map.mapMaybe compactOperatorTable (operatorTables database),
      arrangements = Map.empty
    }
{-# INLINE compact #-}

compactOperatorTable :: Foldable f => OperatorTable f -> Maybe (OperatorTable f)
compactOperatorTable table =
  keepNonEmptyTable (operatorTableFromRows (opShape table) (snd <$> operatorTableRows table))
{-# INLINE compactOperatorTable #-}
