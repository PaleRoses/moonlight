{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | The set of faces still owed a refinement decision.
module Moonlight.Triangulation.Internal.FaceQueue
  ( FaceQueue
  , newFaceQueue
  , pushFace
  , popFace
  ) where

import Control.Monad.ST (ST)
import qualified Data.Vector.Unboxed.Mutable as MUV
import Data.Word (Word32)
import Moonlight.Triangulation.Internal.PackedIndex (packIndex)

-- A worst-first heap has to be told a score for every face it is offered, so
-- the caller derives one — circumradius, area, the encroachment verdict — for
-- faces that are then found acceptable and dropped. Ruppert's termination does
-- not rest on that order; it rests on every bad face being reached before the
-- run ends. So this only has to be a set with a discipline for draining it.
--
-- A stack is that, and it makes membership O(1) with nothing to compute. The
-- pending flags are what keep it a set: a face touched by several insertions
-- before it is drained is decided once, not once per touch.
--
-- The size lives in an unboxed cell for the reason the growable stack's does:
-- it is written on every push and every pop, and a boxed counter allocates a
-- box per write once the size leaves the shared small-'Int' range.
data FaceQueue s = FaceQueue
  { fqFaces :: !(MUV.MVector s Word32)
  , fqPending :: !(MUV.MVector s Bool)
  , fqSize :: !(MUV.MVector s Int)
  }

newFaceQueue :: Int -> ST s (FaceQueue s)
newFaceQueue capacity = do
  let size = max 1 capacity
  fqFaces <- MUV.new size
  fqPending <- MUV.replicate size False
  fqSize <- MUV.replicate 1 0
  pure FaceQueue{..}

-- The checked read of the pending flag is the one bound this module does not
-- establish itself: the face arrives from mesh topology. Once it succeeds,
-- uniqueness proves the stack write: at most one slot exists for each pending
-- flag, and the two vectors have the same length.
pushFace :: FaceQueue s -> Int -> ST s ()
pushFace FaceQueue{fqFaces, fqPending, fqSize} face = do
  pending <- MUV.read fqPending face
  if pending
    then pure ()
    else do
      size <- MUV.unsafeRead fqSize 0
      MUV.unsafeWrite fqFaces size (packIndex face)
      MUV.unsafeWrite fqPending face True
      MUV.unsafeWrite fqSize 0 (size + 1)

popFace :: FaceQueue s -> ST s (Maybe Int)
popFace FaceQueue{fqFaces, fqPending, fqSize} = do
  size <- MUV.unsafeRead fqSize 0
  if size == 0
    then pure Nothing
    else do
      let !index = size - 1
      face <- fromIntegral <$> MUV.unsafeRead fqFaces index
      MUV.unsafeWrite fqPending face False
      MUV.unsafeWrite fqSize 0 index
      pure (Just face)
