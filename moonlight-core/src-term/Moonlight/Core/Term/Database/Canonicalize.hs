{-# LANGUAGE QuantifiedConstraints #-}
-- | Canonicalisation of a term 'Database' under a key-renaming, plus entry
-- enumeration and tuple rehydration.
module Moonlight.Core.Term.Database.Canonicalize
  ( canonicalizeDatabase,
    canonicalizeDirtyRows,
    canonicalizeDirtyRowsInEditor,
  )
where

import Control.Monad.ST (ST)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Moonlight.Core.DenseKey (DenseKey (..))
import Moonlight.Core.Language (Language)
import Moonlight.Core.Term.Database
  ( Database,
    DatabaseEditor,
    DatabaseRow,
    DatabaseRowDelta (..),
    Operator (..),
    RowId,
    RowIdSet,
    TermCommitResult (..),
    TermCommand (..),
    rowChildren,
    rowResult,
    dirtyRowsForKeys,
    rowEntry,
    operatorRows,
    editorQueueCommands,
    editorSnapshot,
    operatorRowsForIds,
    commitTermCommands,
    mapRowKeys,
    normalizeTermCommands,
    runCommandTransaction,
  )
import Prelude

-- | Rewrite every key in the database through the supplied renaming in a
-- single pass.
--
-- A congruence-closed renaming returns the rewritten database as 'Right'. Rows
-- that collide after rewriting return the residual congruence unions as
-- 'Left'; the partially rewritten database is deliberately not published.
-- Callers holding an open union-find should discharge those commands and
-- re-invoke with the updated renaming.
canonicalizeDatabase ::
  (DenseKey key, Language f) =>
  (key -> key) ->
  Database f key ->
  Either [TermCommand f key] (Database f key)
canonicalizeDatabase canonicalize db =
  case runCommandTransaction db (canonicalizeDirtyRowsInEditor (allKeys db) canonicalize) of
    (_result, [], canonicalDatabase) ->
      Right canonicalDatabase
    (_result, residualCommands, _partiallyCanonicalDatabase) ->
      Left residualCommands

canonicalizeDirtyRows ::
  (DenseKey key, Language f) =>
  IntSet ->
  (key -> key) ->
  Database f key ->
  (DatabaseRowDelta f, [TermCommand f key], Database f key)
canonicalizeDirtyRows dirtyKeys canonicalize db =
  canonicalizationResult plan (commitTermCommands (planCommands plan) db)
  where
    plan =
      canonicalizationPlan dirtyKeys canonicalize db

canonicalizeDirtyRowsInEditor ::
  (DenseKey key, Language f) =>
  IntSet ->
  (key -> key) ->
  DatabaseEditor s f key ->
  ST s (DatabaseRowDelta f, [TermCommand f key])
canonicalizeDirtyRowsInEditor dirtyKeys canonicalize editor = do
  snapshot <- editorSnapshot editor
  let plan =
        canonicalizationPlan dirtyKeys canonicalize snapshot
  commitResult <- editorQueueCommands (planCommands plan) editor
  let (delta, commands, _canonicalDb) =
        canonicalizationResult plan commitResult
  pure (delta, commands)

type CanonicalizationPlan :: (Type -> Type) -> Type -> Type
data CanonicalizationPlan f key = CanonicalizationPlan
  { deletedRows :: !(Map (Operator f) [(RowId, DatabaseRow)]),
    planCommands :: ![TermCommand f key]
  }

canonicalizationPlan ::
  (DenseKey key, Language f) =>
  IntSet ->
  (key -> key) ->
  Database f key ->
  CanonicalizationPlan f key
canonicalizationPlan dirtyKeys canonicalize db =
  CanonicalizationPlan
    { deletedRows = dirtyRows,
      planCommands = rowCommands
    }
  where
    dirtyRows =
      rowsSelectedBy (dirtyRowsForKeys db dirtyKeys) db

    candidateInsertedRows =
      fmap (fmap (canonicalizeRow canonicalize . snd)) dirtyRows

    rowCommands =
      deleteRowCommands dirtyRows <> insertRowCommands candidateInsertedRows

canonicalizationResult ::
  (Ord key, Language f) =>
  CanonicalizationPlan f key ->
  TermCommitResult f key ->
  (DatabaseRowDelta f, [TermCommand f key], Database f key)
canonicalizationResult plan commitResult =
  (delta, commands, committedDatabase commitResult)
  where
    delta =
      DatabaseRowDelta
        { rowsDeleted = deletedRows plan,
          rowsInserted = insertedRows commitResult
        }

    commands =
      normalizeTermCommands (planCommands plan <> residualCommands commitResult)

deleteRowCommands ::
  Map (Operator f) [(RowId, DatabaseRow)] ->
  [TermCommand f key]
deleteRowCommands =
  Map.foldMapWithKey operatorDeleteCommands
  where
    operatorDeleteCommands ::
      Operator f ->
      [(RowId, DatabaseRow)] ->
      [TermCommand f key]
    operatorDeleteCommands operator rows =
      fmap (DeleteRow operator . fst) rows

insertRowCommands ::
  (DenseKey key, Language f) =>
  Map (Operator f) [DatabaseRow] ->
  [TermCommand f key]
insertRowCommands =
  Map.foldMapWithKey operatorInsertCommands
  where
    operatorInsertCommands ::
      (DenseKey key, Language f) =>
      Operator f ->
      [DatabaseRow] ->
      [TermCommand f key]
    operatorInsertCommands operator =
      mapMaybe (fmap (uncurry InsertTerm) . rowEntry operator)

rowsSelectedBy ::
  (forall a. Ord a => Ord (f a)) =>
  Map (Operator f) RowIdSet ->
  Database f key ->
  Map (Operator f) [(RowId, DatabaseRow)]
rowsSelectedBy selectedRows db =
  Map.mapWithKey
    (\operator rowIds -> operatorRowsForIds operator rowIds db)
    selectedRows

canonicalizeRow :: DenseKey key => (key -> key) -> DatabaseRow -> DatabaseRow
canonicalizeRow canonicalize row =
  mapRowKeys canonicalizeKey row
  where
    canonicalizeKey =
      encodeDenseKey . canonicalize . decodeDenseKey

allKeys :: Database f key -> IntSet
allKeys db =
  IntSet.fromList
    [ key
      | rows <- Map.elems (operatorRows db),
        (_rowId, row) <- rows,
        key <- rowResult row : rowChildren row
    ]
