{-# LANGUAGE BangPatterns #-}

module Moonlight.Differential.Index.SmallIntArray
  ( smallIntArrayFromAscList,
    smallIntArrayToAscList,
    smallIntArrayFoldl',
    smallIntArrayMember,
    validateSmallIntArrayAscending,
  )
where

import Data.Primitive.PrimArray (PrimArray)
import Data.Primitive.PrimArray qualified as PrimArray

smallIntArrayFromAscList :: [Int] -> PrimArray Int
smallIntArrayFromAscList =
  PrimArray.primArrayFromList
{-# INLINE smallIntArrayFromAscList #-}

smallIntArrayToAscList :: PrimArray Int -> [Int]
smallIntArrayToAscList =
  PrimArray.primArrayToList
{-# INLINE smallIntArrayToAscList #-}

smallIntArrayFoldl' ::
  (acc -> Int -> acc) ->
  acc ->
  PrimArray Int ->
  acc
smallIntArrayFoldl' =
  PrimArray.foldlPrimArray'
{-# INLINE smallIntArrayFoldl' #-}

smallIntArrayMember ::
  Int ->
  PrimArray Int ->
  Bool
smallIntArrayMember target values =
  go 0 (PrimArray.sizeofPrimArray values)
  where
    go !lo !hi
      | lo >= hi =
          False
      | otherwise =
          let !mid =
                lo + ((hi - lo) `quot` 2)
              !value =
                PrimArray.indexPrimArray values mid
           in case compare target value of
                LT ->
                  go lo mid
                EQ ->
                  True
                GT ->
                  go (mid + 1) hi
{-# INLINE smallIntArrayMember #-}

validateSmallIntArrayAscending ::
  (Int -> Int -> Int -> err) ->
  PrimArray Int ->
  Either err ()
validateSmallIntArrayAscending mkError values
  | count <= 1 =
      Right ()
  | otherwise =
      go 1 (PrimArray.indexPrimArray values 0)
  where
    !count =
      PrimArray.sizeofPrimArray values

    go !ix !previous
      | ix == count =
          Right ()
      | otherwise =
          let !current =
                PrimArray.indexPrimArray values ix
           in if previous < current
                then go (ix + 1) current
                else Left (mkError ix previous current)
{-# INLINE validateSmallIntArrayAscending #-}
