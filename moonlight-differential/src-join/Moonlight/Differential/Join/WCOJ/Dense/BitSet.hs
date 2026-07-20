{-# LANGUAGE BangPatterns #-}

-- | Mutable bit-set primitives for dense WCOJ frames; the unsafe slot
-- operations perform no bounds checks — callers own slot-key validity
-- ('validSlotKey') and word capacity ('bitWordsForSlots').
module Moonlight.Differential.Join.WCOJ.Dense.BitSet
  ( bitsPerWord64,
    bitWordsForSlots,
    slotBitWord,
    unsafeTestSlotBit,
    unsafeSetSlotBit,
    unsafeClearSlotBit,
    validSlotKey,
  )
where

import Control.Monad.ST (ST)
import Data.Bits ((.&.), (.|.), complement, shiftL)
import Data.Primitive.PrimArray qualified as PrimArray
import Data.Word (Word64)

bitsPerWord64 :: Int
bitsPerWord64 =
  64
{-# INLINE bitsPerWord64 #-}

bitWordsForSlots :: Int -> Int
bitWordsForSlots slotCount
  | slotCount <= 0 =
      0
  | otherwise =
      ((slotCount - 1) `quot` bitsPerWord64) + 1
{-# INLINE bitWordsForSlots #-}

slotBitWord :: Int -> (Int, Word64)
slotBitWord slotKey =
  let !wordIx = slotKey `quot` bitsPerWord64
      !bitIx = slotKey - (wordIx * bitsPerWord64)
      !mask = (1 :: Word64) `shiftL` bitIx
   in (wordIx, mask)
{-# INLINE slotBitWord #-}

unsafeTestSlotBit :: PrimArray.MutablePrimArray s Word64 -> Int -> ST s Bool
unsafeTestSlotBit bits slotKey = do
  let (!wordIx, !mask) = slotBitWord slotKey
  word <- PrimArray.readPrimArray bits wordIx
  pure ((word .&. mask) /= 0)
{-# INLINE unsafeTestSlotBit #-}

unsafeSetSlotBit :: PrimArray.MutablePrimArray s Word64 -> Int -> ST s ()
unsafeSetSlotBit bits slotKey = do
  let (!wordIx, !mask) = slotBitWord slotKey
  word <- PrimArray.readPrimArray bits wordIx
  PrimArray.writePrimArray bits wordIx (word .|. mask)
{-# INLINE unsafeSetSlotBit #-}

unsafeClearSlotBit :: PrimArray.MutablePrimArray s Word64 -> Int -> ST s ()
unsafeClearSlotBit bits slotKey = do
  let (!wordIx, !mask) = slotBitWord slotKey
  word <- PrimArray.readPrimArray bits wordIx
  PrimArray.writePrimArray bits wordIx (word .&. complement mask)
{-# INLINE unsafeClearSlotBit #-}

validSlotKey :: Int -> Int -> Bool
validSlotKey slotUniverse slotKey =
  slotKey >= 0 && slotKey < slotUniverse
{-# INLINE validSlotKey #-}
