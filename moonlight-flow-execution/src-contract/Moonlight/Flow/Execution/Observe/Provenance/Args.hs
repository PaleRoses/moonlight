{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Execution.Observe.Provenance.Args
  ( ProvArgs,
    emptyProvArgs,
    provArgsFromSet,
    provArgsSingleton,
    provArgsLength,
    provArgsIndex,
    provArgsToInts,
    provArgsToIds,
    provArgsFoldl',
    compareProvArgs,
    ProvArgsMerge (..),
    provArgsMergeChoice,
    mergedProvArgsCount,
    fillMergedProvArgs,
  )
where

import Control.Monad.ST
  ( ST,
    runST,
  )
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Primitive.PrimArray (PrimArray)
import Data.Primitive.PrimArray qualified as PrimArray
import Moonlight.Flow.Execution.Observe.Provenance.Id (ProvId (..))
import Moonlight.Flow.Internal.PrimArray (primArrayFromListStrict)

type ProvArgs :: Type
newtype ProvArgs = ProvArgs (PrimArray Int)

instance Eq ProvArgs where
  left == right =
    compareProvArgs left right == EQ
  {-# INLINE (==) #-}

instance Ord ProvArgs where
  compare =
    compareProvArgs
  {-# INLINE compare #-}

instance Show ProvArgs where
  showsPrec precedence args =
    showParen (precedence > 10) $
      showString "ProvArgs " . shows (fmap ProvId (provArgsToInts args))

emptyProvArgs :: ProvArgs
emptyProvArgs =
  ProvArgs (primArrayFromListStrict [])
{-# INLINE emptyProvArgs #-}

provArgsFromSet :: IntSet -> ProvArgs
provArgsFromSet =
  ProvArgs . primArrayFromListStrict . IntSet.toAscList
{-# INLINE provArgsFromSet #-}

provArgsSingleton :: ProvId -> ProvArgs
provArgsSingleton (ProvId rawId) =
  ProvArgs (primArrayFromListStrict [rawId])
{-# INLINE provArgsSingleton #-}

provArgsLength :: ProvArgs -> Int
provArgsLength (ProvArgs values) =
  PrimArray.sizeofPrimArray values
{-# INLINE provArgsLength #-}

provArgsIndex :: ProvArgs -> Int -> Maybe ProvId
provArgsIndex (ProvArgs values) ix =
  if ix < 0 || ix >= PrimArray.sizeofPrimArray values
    then Nothing
    else Just (ProvId (PrimArray.indexPrimArray values ix))
{-# INLINE provArgsIndex #-}

provArgsToInts :: ProvArgs -> [Int]
provArgsToInts (ProvArgs values) =
  PrimArray.primArrayToList values
{-# INLINE provArgsToInts #-}

provArgsToIds :: ProvArgs -> [ProvId]
provArgsToIds =
  fmap ProvId . provArgsToInts
{-# INLINE provArgsToIds #-}

provArgsFoldl' :: (acc -> ProvId -> acc) -> acc -> ProvArgs -> acc
provArgsFoldl' step initial (ProvArgs values) =
  PrimArray.foldlPrimArray' (\acc rawId -> step acc (ProvId rawId)) initial values
{-# INLINE provArgsFoldl' #-}

compareProvArgs :: ProvArgs -> ProvArgs -> Ordering
compareProvArgs (ProvArgs left) (ProvArgs right) =
  compare left right
{-# INLINE compareProvArgs #-}

type ProvArgsMerge :: Type
data ProvArgsMerge
  = ProvArgsUseLeft
  | ProvArgsUseRight
  | ProvArgsUseMerged !ProvArgs
  deriving stock (Eq, Show)

provArgsMergeChoice :: ProvArgs -> ProvArgs -> ProvArgsMerge
provArgsMergeChoice left@(ProvArgs leftValues) right@(ProvArgs rightValues)
  | leftCount == 0 =
      ProvArgsUseRight
  | rightCount == 0 =
      ProvArgsUseLeft
  | left == right =
      ProvArgsUseLeft
  | mergedCount == leftCount =
      ProvArgsUseLeft
  | mergedCount == rightCount =
      ProvArgsUseRight
  | otherwise =
      ProvArgsUseMerged $
        ProvArgs $
          runST $ do
            target <- PrimArray.newPrimArray mergedCount
            fillMergedProvArgs leftValues rightValues target
            PrimArray.unsafeFreezePrimArray target
  where
    !leftCount =
      PrimArray.sizeofPrimArray leftValues

    !rightCount =
      PrimArray.sizeofPrimArray rightValues

    !mergedCount =
      mergedProvArgsCount leftValues rightValues
{-# INLINE provArgsMergeChoice #-}

mergedProvArgsCount ::
  PrimArray Int ->
  PrimArray Int ->
  Int
mergedProvArgsCount left right =
  go 0 0 0
  where
    !leftCount =
      PrimArray.sizeofPrimArray left

    !rightCount =
      PrimArray.sizeofPrimArray right

    go !leftIx !rightIx !count
      | leftIx == leftCount =
          count + (rightCount - rightIx)
      | rightIx == rightCount =
          count + (leftCount - leftIx)
      | otherwise =
          let !leftValue =
                PrimArray.indexPrimArray left leftIx
              !rightValue =
                PrimArray.indexPrimArray right rightIx
           in case compare leftValue rightValue of
                LT ->
                  go (leftIx + 1) rightIx (count + 1)
                EQ ->
                  go (leftIx + 1) (rightIx + 1) (count + 1)
                GT ->
                  go leftIx (rightIx + 1) (count + 1)
{-# INLINE mergedProvArgsCount #-}

fillMergedProvArgs ::
  PrimArray Int ->
  PrimArray Int ->
  PrimArray.MutablePrimArray s Int ->
  ST s ()
fillMergedProvArgs left right target =
  go 0 0 0
  where
    !leftCount =
      PrimArray.sizeofPrimArray left

    !rightCount =
      PrimArray.sizeofPrimArray right

    go !leftIx !rightIx !writeIx
      | leftIx == leftCount =
          copyRemaining right rightCount rightIx writeIx
      | rightIx == rightCount =
          copyRemaining left leftCount leftIx writeIx
      | otherwise =
          let !leftValue =
                PrimArray.indexPrimArray left leftIx
              !rightValue =
                PrimArray.indexPrimArray right rightIx
           in case compare leftValue rightValue of
                LT -> do
                  PrimArray.writePrimArray target writeIx leftValue
                  go (leftIx + 1) rightIx (writeIx + 1)
                EQ -> do
                  PrimArray.writePrimArray target writeIx leftValue
                  go (leftIx + 1) (rightIx + 1) (writeIx + 1)
                GT -> do
                  PrimArray.writePrimArray target writeIx rightValue
                  go leftIx (rightIx + 1) (writeIx + 1)

    copyRemaining !values !valueCount !readIx !writeIx
      | readIx == valueCount =
          pure ()
      | otherwise = do
          PrimArray.writePrimArray
            target
            writeIx
            (PrimArray.indexPrimArray values readIx)
          copyRemaining values valueCount (readIx + 1) (writeIx + 1)
{-# INLINE fillMergedProvArgs #-}
