{-# LANGUAGE QuantifiedConstraints #-}

module Moonlight.Core.Term.Database.Projection where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Core.Term.Database.Index
import Moonlight.Core.Term.Database.OperatorTable
import Moonlight.Core.Term.Database.Types
import Prelude

dirtyRowsForKeys :: Database f key -> IntSet -> Map (Operator f) RowIdSet
dirtyRowsForKeys database keys =
  Map.filter (not . rowIdSetNull) (fmap dirtyRows tables)
  where
    tables =
      operatorTables database
    dirtyRows table =
      rowIdSetUnion
        (RowIdSet (intMapDependentsOfMany keyList (derivedResultIndex table)))
        (RowIdSet (intMapDependentsOfMany keyList (derivedChildUserIndex table)))
    keyList =
      IntSet.toAscList keys

operatorRows :: Database f key -> Map (Operator f) [(RowId, DatabaseRow)]
operatorRows =
  fmap operatorTableRows . operatorTables

resultKeys :: Database f key -> IntSet
resultKeys database =
  Map.foldl' collectTable IntSet.empty tables
  where
    tables =
      operatorTables database
    collectTable :: IntSet -> OperatorTable f -> IntSet
    collectTable accumulatedResultKeys table =
      rowIdSetFoldl'
        (insertLiveTableRowResult table)
        accumulatedResultKeys
        (tableLiveRows table)
{-# INLINE resultKeys #-}

resultKeysUsingAnyChildKey :: IntSet -> Database f key -> IntSet
resultKeysUsingAnyChildKey keys database =
  Map.foldl' collectTable IntSet.empty tables
  where
    tables =
      operatorTables database
    keyList =
      IntSet.toAscList keys
    collectTable :: IntSet -> OperatorTable f -> IntSet
    collectTable accumulatedResultKeys table =
      rowIdSetFoldl'
        (insertLiveTableRowResult table)
        accumulatedResultKeys
        (RowIdSet (intMapDependentsOfMany keyList (derivedChildUserIndex table)))
{-# INLINE resultKeysUsingAnyChildKey #-}

insertLiveTableRowResult :: OperatorTable f -> IntSet -> Int -> IntSet
insertLiveTableRowResult table accumulatedResultKeys rowKey =
  maybe
    accumulatedResultKeys
    (\row -> IntSet.insert (rowResult row) accumulatedResultKeys)
    (liveTableRow rowKey table)
{-# INLINE insertLiveTableRowResult #-}

rowsForOperator :: (forall a. Ord a => Ord (f a)) => Operator f -> Database f key -> [(RowId, DatabaseRow)]
rowsForOperator operator database =
  maybe [] operatorTableRows (Map.lookup operator (operatorTables database))

operatorRowsForIds ::
  (forall a. Ord a => Ord (f a)) =>
  Operator f ->
  RowIdSet ->
  Database f key ->
  [(RowId, DatabaseRow)]
operatorRowsForIds operator rowIds database =
  maybe [] (`rowsForIds` rowIds) (Map.lookup operator (operatorTables database))

rowsForResultKey :: (forall a. Ord a => Ord (f a)) => Int -> Operator f -> Database f key -> [(RowId, DatabaseRow)]
rowsForResultKey resultKey operator database =
  maybe [] (operatorTableRowsForResultKey resultKey) (Map.lookup operator (operatorTables database))

rowsForResultKeys :: (forall a. Ord a => Ord (f a)) => IntSet -> Operator f -> Database f key -> [(RowId, DatabaseRow)]
rowsForResultKeys encodedResultKeys operator database =
  maybe [] rowsForTable (Map.lookup operator (operatorTables database))
  where
    rowsForTable table =
      rowsForIds table (RowIdSet (intMapDependentsOfMany (IntSet.toAscList encodedResultKeys) (derivedResultIndex table)))
