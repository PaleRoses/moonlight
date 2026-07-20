module Moonlight.Flow.Internal.PrimArray
  ( primArrayFromListStrict,
  )
where

import Data.Primitive.PrimArray (PrimArray)
import Data.Primitive.PrimArray qualified as PrimArray

primArrayFromListStrict :: [Int] -> PrimArray Int
primArrayFromListStrict =
  PrimArray.primArrayFromList
{-# INLINE primArrayFromListStrict #-}
