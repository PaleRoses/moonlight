{-# LANGUAGE BangPatterns #-}

module Moonlight.Flow.Internal.Digest
  ( mix64,
    digestWordsLow,
    digestWordsHigh,
    digestWords128,
    wordOfInt,
    boolWord,
    maybeWord64DigestWords,
    fingerprintWord64ToInt,
    stableHashString64,
  )
where

import Data.Bits ((.&.), shiftR, xor)
import Data.Foldable qualified as Foldable
import Data.Word (Word64)

mix64 :: Word64 -> Word64 -> Word64
mix64 hashValue value =
  let !x0 = hashValue `xor` value
      !x1 = (x0 `xor` (x0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
      !x2 = (x1 `xor` (x1 `shiftR` 27)) * 0x94d049bb133111eb
   in x2 `xor` (x2 `shiftR` 31)
{-# INLINE mix64 #-}

digestWordsLow :: Foldable f => f Word64 -> Word64
digestWordsLow =
  Foldable.foldl' mix64 0xcbf29ce484222325
{-# INLINE digestWordsLow #-}

digestWordsHigh :: Foldable f => f Word64 -> Word64
digestWordsHigh =
  Foldable.foldl' mix64 0x9e3779b97f4a7c15
{-# INLINE digestWordsHigh #-}

digestWords128 :: Foldable f => f Word64 -> (Word64, Word64)
digestWords128 words0 =
  (digestWordsHigh words0, digestWordsLow words0)
{-# INLINE digestWords128 #-}

wordOfInt :: Int -> Word64
wordOfInt =
  fromIntegral
{-# INLINE wordOfInt #-}

boolWord :: Bool -> Word64
boolWord value =
  if value then 1 else 0
{-# INLINE boolWord #-}

maybeWord64DigestWords :: Maybe Word64 -> [Word64]
maybeWord64DigestWords Nothing =
  [0]
maybeWord64DigestWords (Just digest) =
  [1, digest]
{-# INLINE maybeWord64DigestWords #-}

fingerprintWord64ToInt :: Word64 -> Int
fingerprintWord64ToInt word =
  fromIntegral (word .&. fromIntegral (maxBound :: Int))
{-# INLINE fingerprintWord64ToInt #-}

stableHashString64 :: String -> Word64
stableHashString64 =
  Foldable.foldl'
    (\hashValue ch -> mix64 hashValue (fromIntegral (fromEnum ch)))
    0x811c9dc5
{-# INLINE stableHashString64 #-}
