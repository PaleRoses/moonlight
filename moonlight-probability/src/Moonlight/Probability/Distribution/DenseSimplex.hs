{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Moonlight.Probability.Distribution.DenseSimplex
  ( DenseSimplex,
    DenseSimplexError (..),
    denseSimplexCardinality,
    mkDenseSimplex,
    singletonDenseSimplex,
    denseSimplexFromWeights,
    denseSimplexFromFunction,
    denseSimplexFromSparse,
    denseSimplexFromBoxedDoubles,
    denseSimplexFromUnboxedDoubles,
    pureDenseSimplex,
    uniformDenseSimplex,
    denseSimplexAt,
    denseSimplexToList,
    denseSimplexToMap,
    denseSimplexBlend,
    denseSimplexInterference,
    denseSimplexShannonEntropy,
    denseSimplexSupportSize,
    denseSimplexDominance,
    dominantDenseKey,
    dominantDenseEntry,
    denseSimplexTopEntries,
    denseSimplexEmitThresholded,
    blendDenseMixtures,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Kind (Type)
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..), comparing)
import Data.Vector qualified as VB
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MVU
import Moonlight.Core (clampUnitInterval)
import Moonlight.Probability.Core (probValue)
import Moonlight.Probability.Distribution.Simplex
  ( SimplexWeights,
    simplexWeightsToMap,
  )
import Prelude

simplexEpsilon :: Double
simplexEpsilon = 1.0e-9

type DenseSimplex :: Type -> Type
newtype DenseSimplex key = DenseSimplex (Vector Double)
  deriving stock (Eq, Show)

type DenseSimplexError :: Type
data DenseSimplexError
  = DenseSimplexNegativeWeight Int Double
  | DenseSimplexNonFiniteWeight Int Double
  | DenseSimplexNotNormalized Double
  deriving stock (Eq, Show)

denseSimplexCardinality :: forall key. (Bounded key, Enum key) => Int
denseSimplexCardinality =
  fromEnum (maxBound @key) - fromEnum (minBound @key) + 1

keyToIndex :: forall key. (Bounded key, Enum key) => key -> Int
keyToIndex k = fromEnum k - fromEnum (minBound @key)

indexToKey :: forall key. (Bounded key, Enum key) => Int -> key
indexToKey i = toEnum (i + fromEnum (minBound @key))

mkDenseSimplex ::
  forall key.
  (Bounded key, Enum key, Ord key) =>
  Map key Double ->
  Either DenseSimplexError (DenseSimplex key)
mkDenseSimplex weights =
  let !n = denseSimplexCardinality @key
      raw = VU.generate n $ \i ->
        Map.findWithDefault 0.0 (indexToKey @key i) weights
   in case VU.ifoldl' checkEntry (Right 0.0) raw of
        Left e -> Left e
        Right total ->
          if abs (total - 1.0) > simplexEpsilon
            then Left (DenseSimplexNotNormalized total)
            else Right (DenseSimplex raw)
  where
    checkEntry ::
      Either DenseSimplexError Double ->
      Int ->
      Double ->
      Either DenseSimplexError Double
    checkEntry (Left e) _ _ = Left e
    checkEntry (Right !acc) !i !v
      | isNaN v || isInfinite v = Left (DenseSimplexNonFiniteWeight i v)
      | v < 0.0 = Left (DenseSimplexNegativeWeight i v)
      | otherwise = Right (acc + v)

singletonDenseSimplex ::
  forall key.
  (Bounded key, Enum key) =>
  key ->
  DenseSimplex key
singletonDenseSimplex = pureDenseSimplex

{-# INLINE denseSimplexFromWeights #-}
denseSimplexFromWeights ::
  forall key.
  (Bounded key, Enum key) =>
  key ->
  Map key Double ->
  DenseSimplex key
denseSimplexFromWeights basepoint weights =
  let !n = denseSimplexCardinality @key
      !bIdx = keyToIndex @key basepoint
   in runST $ do
        buf <- MVU.replicate n 0.0
        MVU.unsafeWrite buf bIdx simplexEpsilon
        !total <- populateSparse @key buf weights simplexEpsilon
        if total <= simplexEpsilon || isNaN total || isInfinite total
          then pure (pureDenseSimplex basepoint)
          else do
            let !inv = recip total
            normalizeBuffer buf n inv
            frozen <- VU.unsafeFreeze buf
            pure (DenseSimplex frozen)

populateSparse ::
  forall key s.
  (Bounded key, Enum key) =>
  MVU.MVector s Double ->
  Map key Double ->
  Double ->
  ST s Double
populateSparse buf weights seed = go (Map.toList weights) seed
  where
    go [] !acc = pure acc
    go ((k, raw) : rest) !acc = do
      let !clean = if isNaN raw || isInfinite raw then 0.0 else max 0.0 raw
      if clean <= 0.0
        then go rest acc
        else do
          let !idx = keyToIndex @key k
          prev <- MVU.unsafeRead buf idx
          MVU.unsafeWrite buf idx (prev + clean)
          go rest (acc + clean)

normalizeBuffer :: MVU.MVector s Double -> Int -> Double -> ST s ()
normalizeBuffer buf n inv = go 0
  where
    go !i
      | i >= n = pure ()
      | otherwise = do
          v <- MVU.unsafeRead buf i
          MVU.unsafeWrite buf i (v * inv)
          go (i + 1)

-- | Build a DenseSimplex directly from a boxed vector of Doubles
-- that is already aligned with the enum index (vals[i] = weight at
-- indexToKey i). Skips Map construction entirely. Lenient: normalizes,
-- guards NaN/Inf, falls back to singleton at basepoint on degenerate sum.
{-# INLINE denseSimplexFromBoxedDoubles #-}
denseSimplexFromBoxedDoubles ::
  forall key.
  (Bounded key, Enum key) =>
  key ->
  VB.Vector Double ->
  DenseSimplex key
denseSimplexFromBoxedDoubles basepoint vals =
  let !n = denseSimplexCardinality @key
      !bIdx = keyToIndex @key basepoint
      !inputLen = VB.length vals
   in runST $ do
        buf <- MVU.replicate n 0.0
        MVU.unsafeWrite buf bIdx simplexEpsilon
        let !limit = min n inputLen
            fill !i !acc
              | i >= limit = pure acc
              | otherwise = do
                  let !raw = VB.unsafeIndex vals i
                      !clean = if isNaN raw || isInfinite raw then 0.0 else max 0.0 raw
                  if clean > 0.0
                    then do
                      prev <- MVU.unsafeRead buf i
                      MVU.unsafeWrite buf i (prev + clean)
                      fill (i + 1) (acc + clean)
                    else fill (i + 1) acc
        !total <- fill 0 simplexEpsilon
        if total <= simplexEpsilon || isNaN total || isInfinite total
          then pure (pureDenseSimplex basepoint)
          else do
            let !inv = recip total
            normalizeBuffer buf n inv
            frozen <- VU.unsafeFreeze buf
            pure (DenseSimplex frozen)

{-# INLINE denseSimplexFromUnboxedDoubles #-}
denseSimplexFromUnboxedDoubles ::
  forall key.
  (Bounded key, Enum key) =>
  key ->
  VU.Vector Double ->
  DenseSimplex key
denseSimplexFromUnboxedDoubles basepoint vals =
  let !n = denseSimplexCardinality @key
      !bIdx = keyToIndex @key basepoint
      !inputLen = VU.length vals
   in runST $ do
        buf <- MVU.replicate n 0.0
        MVU.unsafeWrite buf bIdx simplexEpsilon
        let !limit = min n inputLen
            fill !i !acc
              | i >= limit = pure acc
              | otherwise = do
                  let !raw = VU.unsafeIndex vals i
                      !clean = if isNaN raw || isInfinite raw then 0.0 else max 0.0 raw
                  if clean > 0.0
                    then do
                      prev <- MVU.unsafeRead buf i
                      MVU.unsafeWrite buf i (prev + clean)
                      fill (i + 1) (acc + clean)
                    else fill (i + 1) acc
        !total <- fill 0 simplexEpsilon
        if total <= simplexEpsilon || isNaN total || isInfinite total
          then pure (pureDenseSimplex basepoint)
          else do
            let !inv = recip total
            normalizeBuffer buf n inv
            frozen <- VU.unsafeFreeze buf
            pure (DenseSimplex frozen)

{-# INLINE denseSimplexFromFunction #-}
denseSimplexFromFunction ::
  forall key.
  (Bounded key, Enum key) =>
  key ->
  (key -> Double) ->
  DenseSimplex key
denseSimplexFromFunction basepoint f =
  let !n = denseSimplexCardinality @key
      !bIdx = keyToIndex @key basepoint
      raw =
        VU.generate n $ \i ->
          let v = f (indexToKey @key i)
              clean = if isNaN v || isInfinite v then 0.0 else max 0.0 v
              extra = if i == bIdx then simplexEpsilon else 0.0
           in clean + extra
      !total = VU.foldl' (+) 0.0 raw
   in if total <= simplexEpsilon || isNaN total || isInfinite total
        then pureDenseSimplex basepoint
        else DenseSimplex (VU.map (/ total) raw)

denseSimplexFromSparse ::
  forall key.
  (Bounded key, Enum key, Ord key) =>
  SimplexWeights key ->
  DenseSimplex key
denseSimplexFromSparse sw =
  let m = simplexWeightsToMap sw
      !n = denseSimplexCardinality @key
   in DenseSimplex
        ( VU.generate n $ \i ->
            let k = indexToKey @key i
             in maybe 0.0 probValue (Map.lookup k m)
        )

{-# INLINE pureDenseSimplex #-}
pureDenseSimplex ::
  forall key.
  (Bounded key, Enum key) =>
  key ->
  DenseSimplex key
pureDenseSimplex selected =
  let !n = denseSimplexCardinality @key
      !sIdx = keyToIndex @key selected
   in DenseSimplex
        (VU.generate n (\i -> if i == sIdx then 1.0 else 0.0))

uniformDenseSimplex ::
  forall key.
  (Bounded key, Enum key) =>
  DenseSimplex key
uniformDenseSimplex =
  let !n = denseSimplexCardinality @key
      !p = 1.0 / fromIntegral n
   in DenseSimplex (VU.replicate n p)

{-# INLINE denseSimplexAt #-}
denseSimplexAt ::
  forall key.
  (Bounded key, Enum key) =>
  DenseSimplex key ->
  key ->
  Double
denseSimplexAt (DenseSimplex v) k =
  VU.unsafeIndex v (keyToIndex @key k)

denseSimplexToList ::
  forall key.
  (Bounded key, Enum key) =>
  DenseSimplex key ->
  [(key, Double)]
denseSimplexToList (DenseSimplex v) =
  VU.ifoldr' (\i p acc -> (indexToKey @key i, p) : acc) [] v

denseSimplexToMap ::
  forall key.
  (Bounded key, Enum key, Ord key) =>
  DenseSimplex key ->
  Map key Double
denseSimplexToMap (DenseSimplex v) =
  Map.fromList
    ( VU.ifoldr'
        (\i p acc -> (indexToKey @key i, p) : acc)
        []
        v
    )

denseSimplexBlend ::
  Double ->
  DenseSimplex key ->
  DenseSimplex key ->
  DenseSimplex key
denseSimplexBlend alpha (DenseSimplex l) (DenseSimplex r) =
  let !a = clampUnitInterval alpha
      !b = 1.0 - a
   in DenseSimplex (VU.zipWith (\x y -> a * x + b * y) l r)

denseSimplexInterference ::
  DenseSimplex key ->
  DenseSimplex key ->
  Double
denseSimplexInterference (DenseSimplex l) (DenseSimplex r) =
  let !overlap = VU.sum (VU.zipWith min l r)
      (!li, !lv) = argmaxIdx l
      (!ri, !rv) = argmaxIdx r
      !dominanceGap
        | lv <= 0.0 && rv <= 0.0 = 1.0
        | li == ri = abs (lv - rv)
        | otherwise = 1.0 - overlap
   in clampUnitInterval (0.5 * overlap + 0.5 * (1.0 - dominanceGap))

argmaxIdx :: Vector Double -> (Int, Double)
argmaxIdx v
  | VU.null v = (-1, 0.0)
  | otherwise =
      VU.ifoldl'
        ( \best@(_, bv) i cv ->
            if cv > bv then (i, cv) else best
        )
        (0, VU.unsafeHead v)
        v

denseSimplexShannonEntropy :: DenseSimplex key -> Double
denseSimplexShannonEntropy (DenseSimplex v) =
  negate
    ( VU.foldl'
        (\acc p -> if p > simplexEpsilon then acc + p * log p else acc)
        0.0
        v
    )

denseSimplexSupportSize :: DenseSimplex key -> Int
denseSimplexSupportSize (DenseSimplex v) =
  VU.foldl' (\acc p -> if p > simplexEpsilon then acc + 1 else acc) 0 v

denseSimplexDominance :: DenseSimplex key -> Double
denseSimplexDominance (DenseSimplex v)
  | VU.null v = 0.0
  | otherwise = snd (argmaxIdx v)

dominantDenseKey ::
  forall key.
  (Bounded key, Enum key) =>
  DenseSimplex key ->
  Maybe key
dominantDenseKey = fmap fst . dominantDenseEntry

{-# INLINE dominantDenseEntry #-}
dominantDenseEntry ::
  forall key.
  (Bounded key, Enum key) =>
  DenseSimplex key ->
  Maybe (key, Double)
dominantDenseEntry (DenseSimplex v)
  | VU.null v = Nothing
  | otherwise =
      let (i, x) = argmaxIdx v
       in Just (indexToKey @key i, x)

denseSimplexTopEntries ::
  forall key.
  (Bounded key, Enum key) =>
  DenseSimplex key ->
  [(key, Double)]
denseSimplexTopEntries (DenseSimplex v) =
  sortBy
    (comparing (Down . snd))
    (VU.ifoldr' (\i p acc -> (indexToKey @key i, p) : acc) [] v)

denseSimplexEmitThresholded ::
  forall key value.
  (Bounded key, Enum key) =>
  Double ->
  (key -> value) ->
  DenseSimplex key ->
  [value]
denseSimplexEmitThresholded threshold project (DenseSimplex v) =
  VU.ifoldr'
    (\i p acc -> if p >= threshold then project (indexToKey @key i) : acc else acc)
    []
    v

blendDenseMixtures ::
  forall key.
  NonEmpty (DenseSimplex key) ->
  DenseSimplex key
blendDenseMixtures xs =
  let !total = fromIntegral (NonEmpty.length xs) :: Double
      !inv = recip total
      step :: DenseSimplex key -> DenseSimplex key -> DenseSimplex key
      step (DenseSimplex acc) (DenseSimplex v) =
        DenseSimplex (VU.zipWith (+) acc v)
      (DenseSimplex summed) = case xs of first :| rest -> foldl' step first rest
   in DenseSimplex (VU.map (* inv) summed)
