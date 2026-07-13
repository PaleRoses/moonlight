{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

-- | Two-tier row-id sets (sorted small array | IntSet); construction from raw
-- IntSets is a specified projection onto nonnegative members, and
-- 'validateRowIdSet' is the after-the-fact representation law hook.
module Moonlight.Differential.Index.RowIdSet
  ( RowIdSet,
    RowIdSetError (..),
    rowIdSetSmallLimit,
    emptyRowIdSet,
    singletonRowIdSet,
    rowIdSetFromList,
    rowIdSetFromIntSetCanonical,
    rowIdSetToList,
    rowIdSetToIntSet,
    rowIdSetNull,
    rowIdSetSize,
    rowIdSetMember,
    rowIdSetInsert,
    rowIdSetDelete,
    rowIdSetUnion,
    rowIdSetIntersection,
    rowIdSetIntersects,
    rowIdSetIntersectionWithIntSet,
    rowIdSetIntersectsIntSet,
    rowIdSetUnionIntoIntSet,
    rowIdSetAny,
    rowIdSetFoldl',
    validateRowIdSet,
  )
where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Primitive.PrimArray (PrimArray)
import Data.Primitive.PrimArray qualified as PrimArray
import Moonlight.Differential.Internal.Index.RowIdSet
  ( RowIdSet (..),
  )
import Moonlight.Differential.Internal.Index.RowId
  ( RowId (..),
  )
import Moonlight.Differential.Index.RowId
  ( rowIdInt,
  )
import Moonlight.Differential.Index.SmallIntArray
  ( smallIntArrayFoldl',
    smallIntArrayFromAscList,
    smallIntArrayMember,
    smallIntArrayToAscList,
    validateSmallIntArrayAscending,
  )

rowIdSetSmallLimit :: Int
rowIdSetSmallLimit = 32

type RowIdSetError :: Type
data RowIdSetError
  = RowIdSetSmallTooLarge !Int
  | RowIdSetNegativeMember !Int
  | RowIdSetSmallNotStrictlyAscending
      !Int
      -- ^ Position of the offending element.
      !Int
      -- ^ Previous element.
      !Int
      -- ^ Current element.
  | RowIdSetLargeTooSmall !Int
  deriving stock (Eq, Ord, Show)

emptyRowIdSet :: RowIdSet
emptyRowIdSet =
  RowIdSetSmall (smallIntArrayFromAscList [])
{-# INLINE emptyRowIdSet #-}

singletonRowIdSet :: RowId -> RowIdSet
singletonRowIdSet rowId =
  singletonRowIdSetInt (rowIdInt rowId)
{-# INLINE singletonRowIdSet #-}

singletonRowIdSetInt :: Int -> RowIdSet
singletonRowIdSetInt rowId =
  RowIdSetSmall (smallIntArrayFromAscList [rowId])
{-# INLINE singletonRowIdSetInt #-}

rowIdSetFromList :: [RowId] -> RowIdSet
rowIdSetFromList =
  rowIdSetFromIntSetCanonical . IntSet.fromList . fmap rowIdInt
{-# INLINE rowIdSetFromList #-}

rowIdSetFromIntSetCanonical :: IntSet -> RowIdSet
rowIdSetFromIntSetCanonical rowIds
  | IntSet.size nonnegativeRowIds <= rowIdSetSmallLimit =
      RowIdSetSmall (smallIntArrayFromAscList (IntSet.toAscList nonnegativeRowIds))
  | otherwise =
      RowIdSetLarge nonnegativeRowIds
  where
    !nonnegativeRowIds =
      nonnegativeIntSet rowIds
{-# INLINE rowIdSetFromIntSetCanonical #-}

fromDistinctAscListCanonical :: [Int] -> RowIdSet
fromDistinctAscListCanonical values
  | length nonnegativeValues <= rowIdSetSmallLimit =
      RowIdSetSmall (smallIntArrayFromAscList nonnegativeValues)
  | otherwise =
      RowIdSetLarge (IntSet.fromDistinctAscList nonnegativeValues)
  where
    !nonnegativeValues =
      filter (>= 0) values
{-# INLINE fromDistinctAscListCanonical #-}

rowIdSetToList :: RowIdSet -> [RowId]
rowIdSetToList =
  fmap RowId . rowIdSetToIntList
{-# INLINE rowIdSetToList #-}

rowIdSetToIntList :: RowIdSet -> [Int]
rowIdSetToIntList = \case
  RowIdSetSmall values ->
    smallIntArrayToAscList values
  RowIdSetLarge values ->
    IntSet.toAscList values
{-# INLINE rowIdSetToIntList #-}

rowIdSetToIntSet :: RowIdSet -> IntSet
rowIdSetToIntSet = \case
  RowIdSetSmall values ->
    IntSet.fromDistinctAscList (smallIntArrayToAscList values)
  RowIdSetLarge values ->
    values
{-# INLINE rowIdSetToIntSet #-}

rowIdSetNull :: RowIdSet -> Bool
rowIdSetNull = \case
  RowIdSetSmall values ->
    PrimArray.sizeofPrimArray values == 0
  RowIdSetLarge values ->
    IntSet.null values
{-# INLINE rowIdSetNull #-}

rowIdSetSize :: RowIdSet -> Int
rowIdSetSize = \case
  RowIdSetSmall values ->
    PrimArray.sizeofPrimArray values
  RowIdSetLarge values ->
    IntSet.size values
{-# INLINE rowIdSetSize #-}

rowIdSetFoldl' :: (acc -> RowId -> acc) -> acc -> RowIdSet -> acc
rowIdSetFoldl' step =
  rowIdSetFoldlInt' (\acc rowId -> step acc (RowId rowId))
{-# INLINE rowIdSetFoldl' #-}

rowIdSetAny :: (RowId -> Bool) -> RowIdSet -> Bool
rowIdSetAny predicate = \case
  RowIdSetSmall values ->
    any (predicate . RowId) (smallIntArrayToAscList values)
  RowIdSetLarge values ->
    IntSet.foldr (\rowId rest -> predicate (RowId rowId) || rest) False values
{-# INLINE rowIdSetAny #-}

rowIdSetFoldlInt' :: (acc -> Int -> acc) -> acc -> RowIdSet -> acc
rowIdSetFoldlInt' step initial = \case
  RowIdSetSmall values ->
    smallIntArrayFoldl' step initial values
  RowIdSetLarge values ->
    IntSet.foldl' step initial values
{-# INLINE rowIdSetFoldlInt' #-}

rowIdSetUnionIntoIntSet :: RowIdSet -> IntSet -> IntSet
rowIdSetUnionIntoIntSet rowIds acc =
  rowIdSetFoldlInt' (\set rowId -> IntSet.insert rowId set) acc rowIds
{-# INLINE rowIdSetUnionIntoIntSet #-}

rowIdSetMember :: RowId -> RowIdSet -> Bool
rowIdSetMember =
  rowIdSetMemberInt . rowIdInt
{-# INLINE rowIdSetMember #-}

rowIdSetMemberInt :: Int -> RowIdSet -> Bool
rowIdSetMemberInt target = \case
  RowIdSetSmall values ->
    smallIntArrayMember target values
  RowIdSetLarge values ->
    IntSet.member target values
{-# INLINE rowIdSetMemberInt #-}

rowIdSetInsert :: RowId -> RowIdSet -> RowIdSet
rowIdSetInsert rowId set =
  rowIdSetInsertInt (rowIdInt rowId) set
{-# INLINE rowIdSetInsert #-}

rowIdSetInsertInt :: Int -> RowIdSet -> RowIdSet
rowIdSetInsertInt rowId set =
  case set of
    RowIdSetSmall values
      | smallIntArrayMember rowId values ->
          set
      | otherwise ->
          fromDistinctAscListCanonical (smallInsert rowId values)
    RowIdSetLarge values ->
      rowIdSetFromIntSetCanonical (IntSet.insert rowId values)
{-# INLINE rowIdSetInsertInt #-}

smallInsert :: Int -> PrimArray Int -> [Int]
smallInsert rowId values =
  reverse (go 0 False [])
  where
    !count =
      PrimArray.sizeofPrimArray values

    go !ix !inserted !acc
      | ix == count =
          if inserted
            then acc
            else rowId : acc
      | otherwise =
          let !value = PrimArray.indexPrimArray values ix
           in if not inserted && rowId < value
                then go (ix + 1) True (value : rowId : acc)
                else go (ix + 1) inserted (value : acc)
{-# INLINE smallInsert #-}

rowIdSetDelete :: RowId -> RowIdSet -> RowIdSet
rowIdSetDelete rowId set =
  rowIdSetDeleteInt (rowIdInt rowId) set
{-# INLINE rowIdSetDelete #-}

rowIdSetDeleteInt :: Int -> RowIdSet -> RowIdSet
rowIdSetDeleteInt rowId set =
  case set of
    RowIdSetSmall values
      | smallIntArrayMember rowId values ->
          fromDistinctAscListCanonical (smallDelete rowId values)
      | otherwise ->
          set
    RowIdSetLarge values ->
      rowIdSetFromIntSetCanonical (IntSet.delete rowId values)
{-# INLINE rowIdSetDeleteInt #-}

smallDelete :: Int -> PrimArray Int -> [Int]
smallDelete rowId values =
  reverse (go 0 [])
  where
    !count =
      PrimArray.sizeofPrimArray values

    go !ix !acc
      | ix == count =
          acc
      | otherwise =
          let !value = PrimArray.indexPrimArray values ix
           in if value == rowId
                then go (ix + 1) acc
                else go (ix + 1) (value : acc)
{-# INLINE smallDelete #-}

rowIdSetUnion :: RowIdSet -> RowIdSet -> RowIdSet
rowIdSetUnion left right
  | rowIdSetNull left = right
  | rowIdSetNull right = left
  | otherwise =
      case (left, right) of
        (RowIdSetSmall leftValues, RowIdSetSmall rightValues) ->
          smallUnion leftValues rightValues
        _ ->
          rowIdSetFromIntSetCanonical
            (IntSet.union (rowIdSetToIntSet left) (rowIdSetToIntSet right))
{-# INLINE rowIdSetUnion #-}

smallUnion :: PrimArray Int -> PrimArray Int -> RowIdSet
smallUnion left right =
  fromDistinctAscListCanonical (reverse (go 0 0 []))
  where
    !leftCount =
      PrimArray.sizeofPrimArray left

    !rightCount =
      PrimArray.sizeofPrimArray right

    copyLeft !ix !acc
      | ix == leftCount = acc
      | otherwise =
          copyLeft
            (ix + 1)
            (PrimArray.indexPrimArray left ix : acc)

    copyRight !ix !acc
      | ix == rightCount = acc
      | otherwise =
          copyRight
            (ix + 1)
            (PrimArray.indexPrimArray right ix : acc)

    go !leftIx !rightIx !acc
      | leftIx == leftCount =
          copyRight rightIx acc
      | rightIx == rightCount =
          copyLeft leftIx acc
      | otherwise =
          let !leftValue = PrimArray.indexPrimArray left leftIx
              !rightValue = PrimArray.indexPrimArray right rightIx
           in case compare leftValue rightValue of
                LT ->
                  go (leftIx + 1) rightIx (leftValue : acc)
                EQ ->
                  go (leftIx + 1) (rightIx + 1) (leftValue : acc)
                GT ->
                  go leftIx (rightIx + 1) (rightValue : acc)
{-# INLINE smallUnion #-}

rowIdSetIntersection :: RowIdSet -> RowIdSet -> RowIdSet
rowIdSetIntersection left right
  | rowIdSetNull left = emptyRowIdSet
  | rowIdSetNull right = emptyRowIdSet
  | otherwise =
      case (left, right) of
        (RowIdSetSmall leftValues, RowIdSetSmall rightValues) ->
          smallIntersection leftValues rightValues
        _ ->
          rowIdSetFromIntSetCanonical
            (IntSet.intersection (rowIdSetToIntSet left) (rowIdSetToIntSet right))
{-# INLINE rowIdSetIntersection #-}

smallIntersection :: PrimArray Int -> PrimArray Int -> RowIdSet
smallIntersection left right =
  fromDistinctAscListCanonical (reverse (go 0 0 []))
  where
    !leftCount =
      PrimArray.sizeofPrimArray left

    !rightCount =
      PrimArray.sizeofPrimArray right

    go !leftIx !rightIx !acc
      | leftIx == leftCount =
          acc
      | rightIx == rightCount =
          acc
      | otherwise =
          let !leftValue = PrimArray.indexPrimArray left leftIx
              !rightValue = PrimArray.indexPrimArray right rightIx
           in case compare leftValue rightValue of
                LT ->
                  go (leftIx + 1) rightIx acc
                EQ ->
                  go (leftIx + 1) (rightIx + 1) (leftValue : acc)
                GT ->
                  go leftIx (rightIx + 1) acc
{-# INLINE smallIntersection #-}

rowIdSetIntersects :: RowIdSet -> RowIdSet -> Bool
rowIdSetIntersects left right
  | rowIdSetNull left = False
  | rowIdSetNull right = False
  | otherwise =
      case (left, right) of
        (RowIdSetSmall leftValues, RowIdSetSmall rightValues) ->
          smallIntersectsSmall leftValues rightValues
        (RowIdSetSmall values, RowIdSetLarge set) ->
          smallIntersectsIntSet values set
        (RowIdSetLarge set, RowIdSetSmall values) ->
          smallIntersectsIntSet values set
        (RowIdSetLarge leftSet, RowIdSetLarge rightSet) ->
          intSetIntersects leftSet rightSet
{-# INLINE rowIdSetIntersects #-}

smallIntersectsSmall :: PrimArray Int -> PrimArray Int -> Bool
smallIntersectsSmall left right =
  go 0 0
  where
    !leftCount =
      PrimArray.sizeofPrimArray left

    !rightCount =
      PrimArray.sizeofPrimArray right

    go !leftIx !rightIx
      | leftIx == leftCount = False
      | rightIx == rightCount = False
      | otherwise =
          let !leftValue = PrimArray.indexPrimArray left leftIx
              !rightValue = PrimArray.indexPrimArray right rightIx
           in case compare leftValue rightValue of
                LT -> go (leftIx + 1) rightIx
                EQ -> True
                GT -> go leftIx (rightIx + 1)
{-# INLINE smallIntersectsSmall #-}

rowIdSetIntersectionWithIntSet :: RowIdSet -> IntSet -> IntSet
rowIdSetIntersectionWithIntSet rowIds active
  | IntSet.null active =
      IntSet.empty
  | rowIdSetNull rowIds =
      IntSet.empty
  | otherwise =
      case rowIds of
        RowIdSetSmall values ->
          smallIntersectionWithIntSet values active
        RowIdSetLarge values ->
          IntSet.intersection values active
{-# INLINE rowIdSetIntersectionWithIntSet #-}

smallIntersectionWithIntSet :: PrimArray Int -> IntSet -> IntSet
smallIntersectionWithIntSet values active =
  go 0 IntSet.empty
  where
    !count =
      PrimArray.sizeofPrimArray values

    go !ix !acc
      | ix == count =
          acc
      | otherwise =
          let !rowId = PrimArray.indexPrimArray values ix
              !acc' =
                if IntSet.member rowId active
                  then IntSet.insert rowId acc
                  else acc
           in go (ix + 1) acc'
{-# INLINE smallIntersectionWithIntSet #-}

rowIdSetIntersectsIntSet :: RowIdSet -> IntSet -> Bool
rowIdSetIntersectsIntSet rowIds active
  | IntSet.null active =
      False
  | rowIdSetNull rowIds =
      False
  | otherwise =
      case rowIds of
        RowIdSetSmall values ->
          smallIntersectsIntSet values active
        RowIdSetLarge values ->
          intSetIntersects values active
{-# INLINE rowIdSetIntersectsIntSet #-}

smallIntersectsIntSet :: PrimArray Int -> IntSet -> Bool
smallIntersectsIntSet values active =
  go 0
  where
    !count =
      PrimArray.sizeofPrimArray values

    go !ix
      | ix == count =
          False
      | otherwise =
          let !rowId = PrimArray.indexPrimArray values ix
           in IntSet.member rowId active || go (ix + 1)
{-# INLINE smallIntersectsIntSet #-}

intSetIntersects :: IntSet -> IntSet -> Bool
intSetIntersects left right
  | IntSet.size left <= IntSet.size right =
      IntSet.foldr (\rowId acc -> IntSet.member rowId right || acc) False left
  | otherwise =
      IntSet.foldr (\rowId acc -> IntSet.member rowId left || acc) False right
{-# INLINE intSetIntersects #-}

validateRowIdSet :: RowIdSet -> Either RowIdSetError ()
validateRowIdSet = \case
  RowIdSetSmall values ->
    let !smallCount = PrimArray.sizeofPrimArray values
     in if smallCount > rowIdSetSmallLimit
          then Left (RowIdSetSmallTooLarge smallCount)
          else validateSmall values
  RowIdSetLarge values ->
    let !largeCount = IntSet.size values
     in case negativeIntSetMinimum values of
          Just negativeMember ->
            Left (RowIdSetNegativeMember negativeMember)
          Nothing ->
            if largeCount <= rowIdSetSmallLimit
              then Left (RowIdSetLargeTooSmall largeCount)
              else Right ()
{-# INLINE validateRowIdSet #-}

validateSmall :: PrimArray Int -> Either RowIdSetError ()
validateSmall values =
  case negativeSmallValue values of
    Just negativeMember ->
      Left (RowIdSetNegativeMember negativeMember)
    Nothing ->
      validateSmallIntArrayAscending RowIdSetSmallNotStrictlyAscending values
{-# INLINE validateSmall #-}

negativeSmallValue :: PrimArray Int -> Maybe Int
negativeSmallValue =
  smallIntArrayFoldl'
    ( \found value ->
        case found of
          Just negativeMember ->
            Just negativeMember
          Nothing
            | value < 0 -> Just value
            | otherwise -> Nothing
    )
    Nothing
{-# INLINE negativeSmallValue #-}

negativeIntSetMinimum :: IntSet -> Maybe Int
negativeIntSetMinimum values =
  case IntSet.lookupMin values of
    Just minimumValue
      | minimumValue < 0 -> Just minimumValue
    _ -> Nothing
{-# INLINE negativeIntSetMinimum #-}

nonnegativeIntSet :: IntSet -> IntSet
nonnegativeIntSet =
  IntSet.filter (>= 0)
{-# INLINE nonnegativeIntSet #-}
