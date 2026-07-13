{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Sheaf.Section.Store.Descent.Frontier
  ( DenseGenerationFrontier,
    DenseRestrictionValidity,
    DenseDescentArena (..),
    newDenseGenerationFrontier,
    clearDenseGenerationFrontier,
    insertDenseGenerationFrontier,
    insertDenseGenerationVector,
    insertDenseGenerationIntSet,
    insertDenseGenerationRange,
    denseGenerationFrontierNull,
    foldDenseGenerationFrontierM,
    denseGenerationFrontierToIntSet,
    denseGenerationFrontierScope,
    mergeDenseGenerationFrontier,
    newDenseRestrictionValidity,
    clearDenseRestrictionValidity,
    markRestrictionValid,
    markRestrictionInvalid,
    restrictionIsValid,
    newDenseDescentArena,
    clearDenseDescentArena,
  )
where

import Control.Monad.ST (ST)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Vector.Unboxed qualified as UVector
import Data.Vector.Unboxed.Mutable qualified as UMutable
import Moonlight.Delta.Scope
  ( Scope,
    dirtyScope,
    fullScope,
  )

data DenseGenerationFrontier s = DenseGenerationFrontier
  { dgfMarks :: !(UMutable.MVector s Int),
    dgfMembers :: !(UMutable.MVector s Int),
    dgfGeneration :: !(STRef s Int),
    dgfSize :: !(STRef s Int)
  }

newtype DenseRestrictionValidity s = DenseRestrictionValidity
  { drvMarks :: UMutable.MVector s Bool
  }

data DenseDescentArena s = DenseDescentArena
  { ddaFrontier :: !(DenseGenerationFrontier s),
    ddaNextFrontier :: !(DenseGenerationFrontier s),
    ddaLocalDirtyObjects :: !(DenseGenerationFrontier s),
    ddaRestrictionFrontier :: !(DenseGenerationFrontier s),
    ddaValidRestrictions :: !(DenseRestrictionValidity s)
  }

newDenseGenerationFrontier :: Int -> ST s (DenseGenerationFrontier s)
newDenseGenerationFrontier capacity = do
  marks <- UMutable.replicate capacity (-1)
  members <- UMutable.new capacity
  generation <- newSTRef 0
  size <- newSTRef 0
  pure
    DenseGenerationFrontier
      { dgfMarks = marks,
        dgfMembers = members,
        dgfGeneration = generation,
        dgfSize = size
      }

clearDenseGenerationFrontier :: DenseGenerationFrontier s -> ST s ()
clearDenseGenerationFrontier frontier = do
  generation <- readSTRef (dgfGeneration frontier)
  if generation == maxBound
    then do
      UMutable.set (dgfMarks frontier) (-1)
      writeSTRef (dgfGeneration frontier) 0
    else writeSTRef (dgfGeneration frontier) (generation + 1)
  writeSTRef (dgfSize frontier) 0
{-# INLINE clearDenseGenerationFrontier #-}

insertDenseGenerationFrontier :: DenseGenerationFrontier s -> Int -> ST s ()
insertDenseGenerationFrontier frontier ordinal
  | ordinal < 0 || ordinal >= UMutable.length (dgfMarks frontier) =
      pure ()
  | otherwise = do
      generation <- readSTRef (dgfGeneration frontier)
      mark <- UMutable.read (dgfMarks frontier) ordinal
      if mark == generation
        then pure ()
        else do
          size <- readSTRef (dgfSize frontier)
          UMutable.write (dgfMarks frontier) ordinal generation
          UMutable.write (dgfMembers frontier) size ordinal
          writeSTRef (dgfSize frontier) (size + 1)
{-# INLINE insertDenseGenerationFrontier #-}

insertDenseGenerationVector :: DenseGenerationFrontier s -> UVector.Vector Int -> ST s ()
insertDenseGenerationVector frontier =
  UVector.foldM' (\() ordinal -> insertDenseGenerationFrontier frontier ordinal) ()
{-# INLINE insertDenseGenerationVector #-}

insertDenseGenerationIntSet :: DenseGenerationFrontier s -> IntSet -> ST s ()
insertDenseGenerationIntSet frontier =
  IntSet.foldl' (\insertAction ordinal -> insertAction *> insertDenseGenerationFrontier frontier ordinal) (pure ())
{-# INLINE insertDenseGenerationIntSet #-}

insertDenseGenerationRange :: DenseGenerationFrontier s -> Int -> ST s ()
insertDenseGenerationRange frontier count =
  let insertOrdinal ordinal
        | ordinal >= count = pure ()
        | otherwise = insertDenseGenerationFrontier frontier ordinal *> insertOrdinal (ordinal + 1)
   in insertOrdinal 0

denseGenerationFrontierNull :: DenseGenerationFrontier s -> ST s Bool
denseGenerationFrontierNull frontier =
  (== 0) <$> readSTRef (dgfSize frontier)
{-# INLINE denseGenerationFrontierNull #-}

foldDenseGenerationFrontierM ::
  (acc -> Int -> ST s acc) ->
  acc ->
  DenseGenerationFrontier s ->
  ST s acc
foldDenseGenerationFrontierM step initial frontier = do
  size <- readSTRef (dgfSize frontier)
  let foldMember memberIndex acc
        | memberIndex >= size = pure acc
        | otherwise = do
            ordinal <- UMutable.read (dgfMembers frontier) memberIndex
            next <- step acc ordinal
            foldMember (memberIndex + 1) next
  foldMember 0 initial
{-# INLINE foldDenseGenerationFrontierM #-}

denseGenerationFrontierToIntSet :: DenseGenerationFrontier s -> ST s IntSet
denseGenerationFrontierToIntSet =
  foldDenseGenerationFrontierM (\keys ordinal -> pure (IntSet.insert ordinal keys)) IntSet.empty

denseGenerationFrontierScope :: Bool -> DenseGenerationFrontier s -> ST s (Scope IntSet)
denseGenerationFrontierScope isFull frontier
  | isFull = pure fullScope
  | otherwise = dirtyScope <$> denseGenerationFrontierToIntSet frontier

mergeDenseGenerationFrontier :: DenseGenerationFrontier s -> DenseGenerationFrontier s -> ST s ()
mergeDenseGenerationFrontier target source =
  foldDenseGenerationFrontierM
    (\() ordinal -> insertDenseGenerationFrontier target ordinal)
    ()
    source

newDenseRestrictionValidity :: Int -> ST s (DenseRestrictionValidity s)
newDenseRestrictionValidity restrictionCountValue =
  DenseRestrictionValidity <$> UMutable.replicate restrictionCountValue False

clearDenseRestrictionValidity :: DenseRestrictionValidity s -> ST s ()
clearDenseRestrictionValidity validity =
  UMutable.set (drvMarks validity) False

markRestrictionValid :: DenseRestrictionValidity s -> Int -> ST s ()
markRestrictionValid validity restrictionKey
  | restrictionKey < 0 || restrictionKey >= UMutable.length (drvMarks validity) =
      pure ()
  | otherwise =
      UMutable.write (drvMarks validity) restrictionKey True
{-# INLINE markRestrictionValid #-}

markRestrictionInvalid :: DenseRestrictionValidity s -> Int -> ST s ()
markRestrictionInvalid validity restrictionKey
  | restrictionKey < 0 || restrictionKey >= UMutable.length (drvMarks validity) =
      pure ()
  | otherwise =
      UMutable.write (drvMarks validity) restrictionKey False
{-# INLINE markRestrictionInvalid #-}

restrictionIsValid :: DenseRestrictionValidity s -> Int -> ST s Bool
restrictionIsValid validity restrictionKey
  | restrictionKey < 0 || restrictionKey >= UMutable.length (drvMarks validity) =
      pure False
  | otherwise =
      UMutable.read (drvMarks validity) restrictionKey
{-# INLINE restrictionIsValid #-}

newDenseDescentArena :: Int -> Int -> ST s (DenseDescentArena s)
newDenseDescentArena objectCountValue restrictionCountValue = do
  frontier <- newDenseGenerationFrontier objectCountValue
  nextFrontier <- newDenseGenerationFrontier objectCountValue
  localDirtyObjects <- newDenseGenerationFrontier objectCountValue
  restrictionFrontier <- newDenseGenerationFrontier restrictionCountValue
  validRestrictions <- newDenseRestrictionValidity restrictionCountValue
  pure
    DenseDescentArena
      { ddaFrontier = frontier,
        ddaNextFrontier = nextFrontier,
        ddaLocalDirtyObjects = localDirtyObjects,
        ddaRestrictionFrontier = restrictionFrontier,
        ddaValidRestrictions = validRestrictions
      }

clearDenseDescentArena :: DenseDescentArena s -> ST s ()
clearDenseDescentArena arena = do
  clearDenseGenerationFrontier (ddaFrontier arena)
  clearDenseGenerationFrontier (ddaNextFrontier arena)
  clearDenseGenerationFrontier (ddaLocalDirtyObjects arena)
  clearDenseGenerationFrontier (ddaRestrictionFrontier arena)
  clearDenseRestrictionValidity (ddaValidRestrictions arena)
