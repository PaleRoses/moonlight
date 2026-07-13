{-# LANGUAGE QuantifiedConstraints #-}

module Moonlight.Core.Term.Database.Arrangement where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Primitive.SmallArray qualified as SmallArray
import Data.Set qualified as Set
import Data.Vector.Unboxed qualified as U
import Moonlight.Core.Term.Database.Index
import Moonlight.Core.Term.Database.OperatorTable
import Moonlight.Core.Term.Database.Types
import Prelude

arrangementRowsForPrefix ::
  (Foldable f, forall a. Ord a => Ord (f a)) =>
  Operator f ->
  ArrangementKey ->
  ArrangementPrefix ->
  Database f key ->
  Either ArrangementValidationError ([(RowId, DatabaseRow)], Database f key)
arrangementRowsForPrefix operator key prefix database = do
  validateArrangementKeyForOperator operator key
  validateArrangementPrefixForKey key prefix
  pure $
    case Map.lookup operator (operatorTables database) of
      Nothing ->
        ([], database)
      Just table ->
        let (rowIds, arrangedDatabase) =
              forceArrangementPrefix operator key prefix table database
         in (rowsForIds table rowIds, arrangedDatabase)

relationStats ::
  (Foldable f, forall a. Ord a => Ord (f a)) =>
  [ArrangementKey] ->
  Operator f ->
  Database f key ->
  Either ArrangementValidationError (Maybe RelationStats)
relationStats prefixKeys operator database =
  traverse_ (validateArrangementKeyForOperator operator) prefixKeys
    *> Right (fmap (operatorRelationStats prefixKeys) (Map.lookup operator (operatorTables database)))

forceArrangementPrefix ::
  (forall a. Ord a => Ord (f a)) =>
  Operator f ->
  ArrangementKey ->
  ArrangementPrefix ->
  OperatorTable f ->
  Database f key ->
  (RowIdSet, Database f key)
forceArrangementPrefix operator key prefix table database =
  case cachedArrangementRows prefix =<< cachedArrangement operator key database of
    Just rowIds ->
      (rowIds, database)
    Nothing ->
      let arrangement =
            fromMaybe
              (emptyArrangement key (tableLiveRows table))
              (cachedArrangement operator key database)
          forcedArrangement =
            forceArrangementPrefixLevel table key prefix arrangement
          rowIds =
            fromMaybe emptyRowIdSet (cachedArrangementRows prefix forcedArrangement)
       in (rowIds, storeArrangement operator forcedArrangement database)

cachedArrangement :: (forall a. Ord a => Ord (f a)) => Operator f -> ArrangementKey -> Database f key -> Maybe Arrangement
cachedArrangement operator key database =
  Map.lookup key =<< Map.lookup operator (arrangements database)
{-# INLINE cachedArrangement #-}

cachedArrangementRows :: ArrangementPrefix -> Arrangement -> Maybe RowIdSet
cachedArrangementRows (ArrangementPrefix prefixValues) arrangement =
  arrangementNodeRowsForPrefix prefixValues (arrangementRoot arrangement)
{-# INLINE cachedArrangementRows #-}

emptyArrangement :: ArrangementKey -> RowIdSet -> Arrangement
emptyArrangement key rowIds =
  Arrangement
    { arrangementOrder = key,
      arrangementRoot = OffsetLeaf rowIds
    }
{-# INLINE emptyArrangement #-}

forceArrangementPrefixLevel :: OperatorTable f -> ArrangementKey -> ArrangementPrefix -> Arrangement -> Arrangement
forceArrangementPrefixLevel table key (ArrangementPrefix prefixValues) arrangement =
  arrangement
    { arrangementRoot = forcedRoot
    }
  where
    forcedRoot =
      fromMaybe
        (arrangementRoot arrangement)
        (forceArrangementNode table key 0 prefixValues (arrangementRoot arrangement))
{-# INLINE forceArrangementPrefixLevel #-}

arrangementNodeRowsForPrefix :: [Int] -> ArrangementNode -> Maybe RowIdSet
arrangementNodeRowsForPrefix prefixValues node =
  case prefixValues of
    [] ->
      Just (arrangementNodeRows node)
    prefixValue : remainingPrefix ->
      case node of
        OffsetLeaf _rowIds ->
          Nothing
        PrefixBranch _rowIds branches ->
          Map.lookup prefixValue branches >>= arrangementNodeRowsForPrefix remainingPrefix
{-# INLINE arrangementNodeRowsForPrefix #-}

forceArrangementNode :: OperatorTable f -> ArrangementKey -> Int -> [Int] -> ArrangementNode -> Maybe ArrangementNode
forceArrangementNode table key depth prefixValues node =
  case prefixValues of
    [] ->
      Just node
    prefixValue : remainingPrefix -> do
      column <- arrangementColumnAt depth key
      let rowIds =
            arrangementNodeRows node
          branches =
            arrangementNodeBranches node
          childNode =
            Map.findWithDefault
              (OffsetLeaf (prefixChildRows table column prefixValue rowIds))
              prefixValue
              branches
      forcedChild <- forceArrangementNode table key (depth + 1) remainingPrefix childNode
      Just (PrefixBranch rowIds (Map.insert prefixValue forcedChild branches))
{-# INLINE forceArrangementNode #-}

arrangementNodeRows :: ArrangementNode -> RowIdSet
arrangementNodeRows node =
  case node of
    OffsetLeaf rowIds ->
      rowIds
    PrefixBranch rowIds _branches ->
      rowIds
{-# INLINE arrangementNodeRows #-}

arrangementNodeBranches :: ArrangementNode -> Map Int ArrangementNode
arrangementNodeBranches node =
  case node of
    OffsetLeaf _rowIds ->
      Map.empty
    PrefixBranch _rowIds branches ->
      branches
{-# INLINE arrangementNodeBranches #-}

arrangementColumnAt :: Int -> ArrangementKey -> Maybe Column
arrangementColumnAt depth key
  | depth < 0 =
      Nothing
  | otherwise =
      columnAt depth (arrangementKeyColumns key)
{-# INLINE arrangementColumnAt #-}

columnAt :: Int -> [Column] -> Maybe Column
columnAt depth columns =
  case drop depth columns of
    column : _remainingColumns ->
      Just column
    [] ->
      Nothing
{-# INLINE columnAt #-}

prefixChildRows :: OperatorTable f -> Column -> Int -> RowIdSet -> RowIdSet
prefixChildRows table column prefixValue rowIds =
  rowIdSetIntersection rowIds (columnValueRows table column prefixValue)
{-# INLINE prefixChildRows #-}

columnValueRows :: OperatorTable f -> Column -> Int -> RowIdSet
columnValueRows table column value =
  case column of
    ResultColumn ->
      lookupResultIndex value (derivedResultIndex table)
    ChildColumn childIndex ->
      maybe
        emptyRowIdSet
        (RowIdSet . IntMap.findWithDefault IntSet.empty value)
        (childColumnValueIndexAt childIndex (derivedChildColumnValueIndex table))
{-# INLINE columnValueRows #-}

storeArrangement :: (forall a. Ord a => Ord (f a)) => Operator f -> Arrangement -> Database f key -> Database f key
storeArrangement operator arrangement database =
  database
    { arrangements =
        Map.alter
          (Just . Map.insert (arrangementOrder arrangement) arrangement . fromMaybe Map.empty)
          operator
          (arrangements database)
    }
{-# INLINE storeArrangement #-}

arrangementRowValues :: OperatorTable f -> ArrangementKey -> Int -> Maybe [Int]
arrangementRowValues table key rowKey =
  traverse (tableColumnValue table rowKey) (arrangementKeyColumns key)
{-# INLINE arrangementRowValues #-}

tableColumnValue :: OperatorTable f -> Int -> Column -> Maybe Int
tableColumnValue table rowKey column =
  case column of
    ResultColumn ->
      rowResult <$> tableRow rowKey table
    ChildColumn childIndex ->
      tableRow rowKey table >>= childValueAt childIndex . rowChildren
{-# INLINE tableColumnValue #-}

childColumnValueIndexAt :: Int -> ChildColumnValueIndex -> Maybe (IntMap IntSet)
childColumnValueIndexAt childIndex childColumnIndexes
  | childIndex < 0 = Nothing
  | otherwise =
      case childColumnIndexes of
        NullaryChildColumnValueIndex ->
          Nothing
        UnaryChildColumnValueIndex values
          | childIndex == 0 -> Just values
          | otherwise -> Nothing
        BinaryChildColumnValueIndex leftValues rightValues
          | childIndex == 0 -> Just leftValues
          | childIndex == 1 -> Just rightValues
          | otherwise -> Nothing
        NaryChildColumnValueIndex values
          | childIndex >= SmallArray.sizeofSmallArray values -> Nothing
          | otherwise -> Just (SmallArray.indexSmallArray values childIndex)
{-# INLINE childColumnValueIndexAt #-}

operatorRelationStats :: [ArrangementKey] -> OperatorTable f -> RelationStats
operatorRelationStats prefixKeys table =
  RelationStats
    { rowCount = nextRowId table,
      liveRowCount = rowIdSetSize (tableLiveRows table),
      distinctPerColumn =
        U.fromList (fmap (distinctColumnValueCount table) relationColumns),
      distinctPerPrefix =
        Map.fromList
          (fmap (\key -> (key, distinctArrangementPrefixCount table key)) prefixKeys),
      maximumBucketSize = maximumExactBucketSize table
    }
  where
    relationColumns =
      ResultColumn : fmap ChildColumn (ascendingRowKeys (opArity table))
{-# INLINE operatorRelationStats #-}

distinctColumnValueCount :: OperatorTable f -> Column -> Int
distinctColumnValueCount table column =
  case column of
    ResultColumn ->
      IntMap.size (derivedResultIndex table)
    ChildColumn childIndex ->
      maybe 0 IntMap.size (childColumnValueIndexAt childIndex (derivedChildColumnValueIndex table))
{-# INLINE distinctColumnValueCount #-}

distinctArrangementPrefixCount :: OperatorTable f -> ArrangementKey -> Int
distinctArrangementPrefixCount table key =
  Set.size $
    rowIdSetFoldl'
      insertArrangementValue
      Set.empty
      (tableLiveRows table)
  where
    insertArrangementValue values rowKey =
      maybe values (`Set.insert` values) (arrangementRowValues table key rowKey)
{-# INLINE distinctArrangementPrefixCount #-}

maximumExactBucketSize :: OperatorTable f -> Int
maximumExactBucketSize =
  maximumChildTupleBucketSize . derivedExactIndex
{-# INLINE maximumExactBucketSize #-}

maximumChildTupleBucketSize :: ChildTupleIndex -> Int
maximumChildTupleBucketSize childTupleIndex =
  case childTupleIndex of
    NullaryChildTupleIndex keys ->
      IntSet.size keys
    UnaryChildTupleIndex byChild ->
      IntMap.foldl' maximumBucket 0 byChild
    BinaryChildTupleIndex byLeftChild ->
      IntMap.foldl' (\maximumSize byRightChild -> IntMap.foldl' maximumBucket maximumSize byRightChild) 0 byLeftChild
    NaryChildTupleIndex byChildren ->
      Map.foldl' maximumBucket 0 byChildren
  where
    maximumBucket maximumSize keys =
      max maximumSize (IntSet.size keys)
{-# INLINE maximumChildTupleBucketSize #-}
