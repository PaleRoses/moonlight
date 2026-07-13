module Moonlight.Core.Term.Database.Index where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Primitive.SmallArray qualified as SmallArray
import Moonlight.Core.Term.Database.Types
import Prelude

derivedResultIndex :: OperatorTable f -> ResultIndex
derivedResultIndex =
  resultIx
{-# INLINE derivedResultIndex #-}

derivedChildColumnValueIndex :: OperatorTable f -> ChildColumnValueIndex
derivedChildColumnValueIndex =
  childColumnIx
{-# INLINE derivedChildColumnValueIndex #-}

derivedExactIndex :: OperatorTable f -> ExactIndex
derivedExactIndex =
  exactIx
{-# INLINE derivedExactIndex #-}

derivedChildUserIndex :: OperatorTable f -> ChildUserIndex
derivedChildUserIndex =
  childUserIx
{-# INLINE derivedChildUserIndex #-}

emptyExactIndex :: Int -> ExactIndex
emptyExactIndex =
  emptyChildTupleIndex

emptyExactResultIndex :: Int -> ExactResultIndex
emptyExactResultIndex =
  emptyChildTupleIndex

emptyTupleIdentityIndex :: TupleIdentityIndex
emptyTupleIdentityIndex =
  Map.empty

emptyChildTupleIndex :: Int -> ChildTupleIndex
emptyChildTupleIndex arity =
  case arity of
    0 -> NullaryChildTupleIndex IntSet.empty
    1 -> UnaryChildTupleIndex IntMap.empty
    2 -> BinaryChildTupleIndex IntMap.empty
    _ -> NaryChildTupleIndex Map.empty

emptyResultIndex :: ResultIndex
emptyResultIndex =
  IntMap.empty

emptyChildColumnValueIndex :: Int -> ChildColumnValueIndex
emptyChildColumnValueIndex arity =
  case arity of
    0 -> NullaryChildColumnValueIndex
    1 -> UnaryChildColumnValueIndex IntMap.empty
    2 -> BinaryChildColumnValueIndex IntMap.empty IntMap.empty
    _ -> NaryChildColumnValueIndex (SmallArray.smallArrayFromList (replicate arity IntMap.empty))
{-# INLINE emptyChildColumnValueIndex #-}

emptyChildUserIndex :: ChildUserIndex
emptyChildUserIndex =
  IntMap.empty

lookupExactIndex :: [Int] -> ExactIndex -> RowIdSet
lookupExactIndex children =
  RowIdSet . lookupChildTupleIndex children
{-# INLINE lookupExactIndex #-}

lookupExactResultIndex :: [Int] -> ExactResultIndex -> IntSet
lookupExactResultIndex =
  lookupChildTupleIndex
{-# INLINE lookupExactResultIndex #-}

lookupChildTupleIndex :: [Int] -> ChildTupleIndex -> IntSet
lookupChildTupleIndex children childTupleIndex =
  case childTupleIndex of
    NullaryChildTupleIndex keys ->
      case children of
        [] -> keys
        _nonNullaryChildren -> IntSet.empty
    UnaryChildTupleIndex byChild ->
      case children of
        [child] -> IntMap.findWithDefault IntSet.empty child byChild
        _nonUnaryChildren -> IntSet.empty
    BinaryChildTupleIndex byLeftChild ->
      case children of
        [leftChild, rightChild] ->
          maybe IntSet.empty (IntMap.findWithDefault IntSet.empty rightChild) (IntMap.lookup leftChild byLeftChild)
        _nonBinaryChildren -> IntSet.empty
    NaryChildTupleIndex byChildren ->
      Map.findWithDefault IntSet.empty children byChildren
{-# INLINE lookupChildTupleIndex #-}

lookupResultIndex :: Int -> ResultIndex -> RowIdSet
lookupResultIndex resultKey =
  RowIdSet . IntMap.findWithDefault IntSet.empty resultKey
{-# INLINE lookupResultIndex #-}

intMapDependentsOfMany :: [Int] -> IntMap IntSet -> IntSet
intMapDependentsOfMany keys indexValue =
  foldMap (\key -> IntMap.findWithDefault IntSet.empty key indexValue) keys
{-# INLINE intMapDependentsOfMany #-}

insertExactIndex :: Int -> [Int] -> ExactIndex -> ExactIndex
insertExactIndex =
  insertChildTupleIndex
{-# INLINE insertExactIndex #-}

insertExactResultIndex :: Int -> [Int] -> ExactResultIndex -> ExactResultIndex
insertExactResultIndex =
  insertChildTupleIndex
{-# INLINE insertExactResultIndex #-}

insertTupleIdentityIndex :: Int -> DatabaseRow -> TupleIdentityIndex -> TupleIdentityIndex
insertTupleIdentityIndex rowKey row =
  Map.insert (tupleIdentity row) rowKey
{-# INLINE insertTupleIdentityIndex #-}

lookupTupleIdentityIndex :: DatabaseRow -> TupleIdentityIndex -> Maybe Int
lookupTupleIdentityIndex row =
  Map.lookup (tupleIdentity row)
{-# INLINE lookupTupleIdentityIndex #-}

deleteTupleIdentityIndex :: DatabaseRow -> TupleIdentityIndex -> TupleIdentityIndex
deleteTupleIdentityIndex row =
  Map.delete (tupleIdentity row)
{-# INLINE deleteTupleIdentityIndex #-}

tupleIdentity :: DatabaseRow -> TupleIdentity
tupleIdentity row =
  TupleIdentity (rowChildren row) (rowResult row)
{-# INLINE tupleIdentity #-}

insertChildTupleIndex :: Int -> [Int] -> ChildTupleIndex -> ChildTupleIndex
insertChildTupleIndex dependent children childTupleIndex =
  case childTupleIndex of
    NullaryChildTupleIndex keys ->
      case children of
        [] -> NullaryChildTupleIndex (IntSet.insert dependent keys)
        _nonNullaryChildren -> childTupleIndex
    UnaryChildTupleIndex byChild ->
      case children of
        [child] -> UnaryChildTupleIndex (insertIntSetAtKey child dependent byChild)
        _nonUnaryChildren -> childTupleIndex
    BinaryChildTupleIndex byLeftChild ->
      case children of
        [leftChild, rightChild] ->
          BinaryChildTupleIndex $
            IntMap.insertWith
              (IntMap.unionWith IntSet.union)
              leftChild
              (IntMap.singleton rightChild (IntSet.singleton dependent))
              byLeftChild
        _nonBinaryChildren -> childTupleIndex
    NaryChildTupleIndex byChildren ->
      NaryChildTupleIndex (Map.insertWith IntSet.union children (IntSet.singleton dependent) byChildren)
{-# INLINE insertChildTupleIndex #-}

insertResultIndex :: Int -> Int -> ResultIndex -> ResultIndex
insertResultIndex rowKey resultKey =
  insertIntSetAtKey resultKey rowKey
{-# INLINE insertResultIndex #-}

insertChildUserIndex :: Int -> Int -> ChildUserIndex -> ChildUserIndex
insertChildUserIndex rowKey childKey =
  insertIntSetAtKey childKey rowKey
{-# INLINE insertChildUserIndex #-}

deleteExactIndex :: Int -> [Int] -> ExactIndex -> ExactIndex
deleteExactIndex =
  deleteChildTupleIndex
{-# INLINE deleteExactIndex #-}

deleteExactResultIndex :: Int -> [Int] -> ExactResultIndex -> ExactResultIndex
deleteExactResultIndex =
  deleteChildTupleIndex
{-# INLINE deleteExactResultIndex #-}

deleteResultIndex :: Int -> Int -> ResultIndex -> ResultIndex
deleteResultIndex rowKey resultKey =
  removeIntSetAtIntKey resultKey rowKey
{-# INLINE deleteResultIndex #-}

deleteChildUserIndex :: Int -> Int -> ChildUserIndex -> ChildUserIndex
deleteChildUserIndex rowKey childKey =
  removeIntSetAtIntKey childKey rowKey
{-# INLINE deleteChildUserIndex #-}

deleteChildTupleIndex :: Int -> [Int] -> ChildTupleIndex -> ChildTupleIndex
deleteChildTupleIndex dependent children childTupleIndex =
  case childTupleIndex of
    NullaryChildTupleIndex keys ->
      case children of
        [] -> NullaryChildTupleIndex (IntSet.delete dependent keys)
        _nonNullaryChildren -> childTupleIndex
    UnaryChildTupleIndex byChild ->
      case children of
        [child] -> UnaryChildTupleIndex (removeIntSetAtIntKey child dependent byChild)
        _nonUnaryChildren -> childTupleIndex
    BinaryChildTupleIndex byLeftChild ->
      case children of
        [leftChild, rightChild] ->
          BinaryChildTupleIndex $
            IntMap.update
              (nonEmptyIntMap . removeIntSetAtIntKey rightChild dependent)
              leftChild
              byLeftChild
        _nonBinaryChildren -> childTupleIndex
    NaryChildTupleIndex byChildren ->
      NaryChildTupleIndex (removeIntSetAtMapKey children dependent byChildren)
{-# INLINE deleteChildTupleIndex #-}

insertIntSetAtKey :: Int -> Int -> IntMap IntSet -> IntMap IntSet
insertIntSetAtKey key dependent =
  IntMap.insertWith IntSet.union key (IntSet.singleton dependent)
{-# INLINE insertIntSetAtKey #-}

removeIntSetAtIntKey :: Int -> Int -> IntMap IntSet -> IntMap IntSet
removeIntSetAtIntKey key dependent =
  IntMap.update (nonEmptyIntSet . IntSet.delete dependent) key
{-# INLINE removeIntSetAtIntKey #-}

removeIntSetAtMapKey :: Ord key => key -> Int -> Map key IntSet -> Map key IntSet
removeIntSetAtMapKey key dependent =
  Map.update (nonEmptyIntSet . IntSet.delete dependent) key
{-# INLINE removeIntSetAtMapKey #-}

nonEmptyIntSet :: IntSet -> Maybe IntSet
nonEmptyIntSet values
  | IntSet.null values = Nothing
  | otherwise = Just values
{-# INLINE nonEmptyIntSet #-}

nonEmptyIntMap :: IntMap value -> Maybe (IntMap value)
nonEmptyIntMap values
  | IntMap.null values = Nothing
  | otherwise = Just values
{-# INLINE nonEmptyIntMap #-}

insertChildColumnValueIndex :: Int -> [Int] -> ChildColumnValueIndex -> ChildColumnValueIndex
insertChildColumnValueIndex rowKey children childColumnIndexes =
  case childColumnIndexes of
    NullaryChildColumnValueIndex ->
      childColumnIndexes
    UnaryChildColumnValueIndex values ->
      case children of
        [child] -> UnaryChildColumnValueIndex (insertIntSetAtKey child rowKey values)
        _nonUnaryChildren -> childColumnIndexes
    BinaryChildColumnValueIndex leftValues rightValues ->
      case children of
        [leftChild, rightChild] ->
          BinaryChildColumnValueIndex
            (insertIntSetAtKey leftChild rowKey leftValues)
            (insertIntSetAtKey rightChild rowKey rightValues)
        _nonBinaryChildren -> childColumnIndexes
    NaryChildColumnValueIndex values ->
      case length children == SmallArray.sizeofSmallArray values of
        True ->
          NaryChildColumnValueIndex $
            SmallArray.smallArrayFromList $
              zipWith
                (\childValueRows childKey -> insertIntSetAtKey childKey rowKey childValueRows)
                (smallArrayToList values)
                children
        False ->
          childColumnIndexes
{-# INLINE insertChildColumnValueIndex #-}

deleteChildColumnValueIndex :: Int -> [Int] -> ChildColumnValueIndex -> ChildColumnValueIndex
deleteChildColumnValueIndex rowKey children childColumnIndexes =
  case childColumnIndexes of
    NullaryChildColumnValueIndex ->
      childColumnIndexes
    UnaryChildColumnValueIndex values ->
      case children of
        [child] -> UnaryChildColumnValueIndex (removeIntSetAtIntKey child rowKey values)
        _nonUnaryChildren -> childColumnIndexes
    BinaryChildColumnValueIndex leftValues rightValues ->
      case children of
        [leftChild, rightChild] ->
          BinaryChildColumnValueIndex
            (removeIntSetAtIntKey leftChild rowKey leftValues)
            (removeIntSetAtIntKey rightChild rowKey rightValues)
        _nonBinaryChildren -> childColumnIndexes
    NaryChildColumnValueIndex values ->
      case length children == SmallArray.sizeofSmallArray values of
        True ->
          NaryChildColumnValueIndex $
            SmallArray.smallArrayFromList $
              zipWith
                (\childValueRows childKey -> removeIntSetAtIntKey childKey rowKey childValueRows)
                (smallArrayToList values)
                children
        False ->
          childColumnIndexes
{-# INLINE deleteChildColumnValueIndex #-}
