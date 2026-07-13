{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Order.LocallyFinite
  ( LocallyFiniteOrder (..),
    RootedLocallyFiniteOrder (..),
    integralSamplerGeneric,
    foldMapGroup,
    scaleInteger,
    mobius,
    mobiusProduct,
  )
where

import Data.Foldable qualified as Foldable
import Data.Kind
  ( Type,
  )
import Data.List qualified as List
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Numeric.Natural
  ( Natural,
  )

import Moonlight.Core
  ( PartialOrder (..),
  )
import Moonlight.Core (AdditiveGroup (..), AdditiveMonoid (..))

class PartialOrder time => LocallyFiniteOrder time where
  -- | Closed finite interval @{point | lower <= point <= upper}@.
  -- Returns '[]' when @lower@ is not below @upper@.
  interval :: time -> time -> [time]

  intervalSize :: time -> time -> Natural
  intervalSize lower upper =
    List.genericLength (interval lower upper)

  mobiusCoefficient :: Ord time => time -> time -> Integer
  mobiusCoefficient =
    mobiusGeneric

  mobiusSupport :: Ord time => time -> time -> [(time, Integer)]
  mobiusSupport lower upper =
    fmap
      (\source -> (source, mobiusCoefficient source upper))
      (interval lower upper)

  -- | Shared memoization of a sampler over this order; the default performs
  -- no memoization and instances override with a representation whose cells
  -- are computed once and shared across samples.
  memoTime :: (time -> value) -> time -> value
  memoTime =
    id

class LocallyFiniteOrder time => RootedLocallyFiniteOrder time where
  leastTime :: time

  -- | Rooted prefix integral of a sampler; the default folds the full rooted
  -- interval per sample and instances override with prefix-memoized or
  -- tensorized passes that agree with 'integralSamplerGeneric' pointwise.
  integralSampler ::
    AdditiveGroup value =>
    (time -> value) ->
    time ->
    value
  integralSampler =
    integralSamplerGeneric

integralSamplerGeneric ::
  (RootedLocallyFiniteOrder time, AdditiveGroup value) =>
  (time -> value) ->
  time ->
  value
integralSamplerGeneric sample target =
  foldMapGroup sample (interval leastTime target)
{-# INLINE integralSamplerGeneric #-}

foldMapGroup ::
  (Foldable values, AdditiveGroup group) =>
  (value -> group) ->
  values value ->
  group
foldMapGroup project =
  Foldable.foldl' (\acc value -> add acc (project value)) zero
{-# INLINE foldMapGroup #-}

scaleInteger ::
  AdditiveGroup group =>
  Integer ->
  group ->
  group
scaleInteger coefficient value
  | coefficient < 0 =
      neg (scalePositiveInteger (negate coefficient) value)
  | otherwise =
      scalePositiveInteger coefficient value
{-# INLINE scaleInteger #-}

scalePositiveInteger ::
  AdditiveGroup group =>
  Integer ->
  group ->
  group
scalePositiveInteger coefficient value =
  Foldable.foldl' add zero (List.genericReplicate coefficient value)
{-# INLINE scalePositiveInteger #-}

-- | Derived Möbius coefficient for the interval incidence algebra of the order.
-- The coefficient is memoized for the finite interval rooted at the lower
-- endpoint, so diamond-shaped intervals do not recompute predecessor cones.
mobius ::
  (Ord time, LocallyFiniteOrder time) =>
  time ->
  time ->
  Integer
mobius =
  mobiusCoefficient
{-# INLINE mobius #-}

mobiusGeneric ::
  (Ord time, LocallyFiniteOrder time) =>
  time ->
  time ->
  Integer
mobiusGeneric lower upper
  | not (lower `leq` upper) =
      0
  | otherwise =
      fst (mobiusMemo lower intervalPoints Map.empty upper)
  where
    intervalPoints =
      interval lower upper
{-# INLINE mobiusGeneric #-}

mobiusMemo ::
  (Ord time, LocallyFiniteOrder time) =>
  time ->
  [time] ->
  Map time Integer ->
  time ->
  (Integer, Map time Integer)
mobiusMemo lower intervalPoints coefficients point =
  case Map.lookup point coefficients of
    Just coefficient ->
      (coefficient, coefficients)
    Nothing ->
      let (predecessorSum, coefficients') =
            Foldable.foldl'
              accumulatePredecessor
              (0, coefficients)
              (List.filter (`lt` point) intervalPoints)
          coefficient =
            if point == lower
              then 1
              else negate predecessorSum
       in (coefficient, Map.insert point coefficient coefficients')
  where
    accumulatePredecessor (runningSum, memo) predecessor =
      let (coefficient, memo') =
            mobiusMemo lower intervalPoints memo predecessor
       in (runningSum + coefficient, memo')
{-# INLINE mobiusMemo #-}

mobiusProduct ::
  ( Ord left,
    Ord right,
    LocallyFiniteOrder left,
    LocallyFiniteOrder right
  ) =>
  (left, right) ->
  (left, right) ->
  Integer
mobiusProduct (leftLower, rightLower) (leftUpper, rightUpper) =
  mobius leftLower leftUpper * mobius rightLower rightUpper
{-# INLINE mobiusProduct #-}

type NaturalTrie :: Type -> Type
data NaturalTrie value
  = NaturalTrie ~value ~(NaturalTrie value) ~(NaturalTrie value)

naturalTrie :: (Natural -> value) -> NaturalTrie value
naturalTrie sample =
  NaturalTrie
    (sample 0)
    (naturalTrie (\point -> sample (2 * point + 1)))
    (naturalTrie (\point -> sample (2 * point + 2)))

naturalTrieLookup :: NaturalTrie value -> Natural -> value
naturalTrieLookup (NaturalTrie value left right) point
  | point == 0 =
      value
  | odd point =
      naturalTrieLookup left ((point - 1) `div` 2)
  | otherwise =
      naturalTrieLookup right ((point - 2) `div` 2)

instance LocallyFiniteOrder Natural where
  interval lower upper
    | lower `leq` upper =
        [lower .. upper]
    | otherwise =
        []

  mobiusCoefficient lower upper
    | lower == upper =
        1
    | lower + 1 == upper =
        -1
    | lower `lt` upper =
        0
    | otherwise =
        0
  {-# INLINE mobiusCoefficient #-}

  mobiusSupport lower upper
    | not (lower `leq` upper) =
        []
    | lower == upper =
        [(upper, 1)]
    | otherwise =
        [(upper - 1, -1), (upper, 1)]
  {-# INLINE mobiusSupport #-}

  memoTime =
    naturalTrieLookup . naturalTrie

instance RootedLocallyFiniteOrder Natural where
  leastTime =
    0

  integralSampler sample =
    prefix
    where
      prefix =
        memoTime step

      step point
        | point == 0 =
            sample 0
        | otherwise =
            add (prefix (point - 1)) (sample point)

instance (Ord left, Ord right, LocallyFiniteOrder left, LocallyFiniteOrder right) => LocallyFiniteOrder (left, right) where
  interval (leftLower, rightLower) (leftUpper, rightUpper) =
    (,)
      <$> interval leftLower leftUpper
      <*> interval rightLower rightUpper

  mobiusCoefficient =
    mobiusProduct
  {-# INLINE mobiusCoefficient #-}

  mobiusSupport (leftLower, rightLower) (leftUpper, rightUpper) =
    [ ((left, right), leftCoefficient * rightCoefficient)
    | (left, leftCoefficient) <- mobiusSupport leftLower leftUpper,
      (right, rightCoefficient) <- mobiusSupport rightLower rightUpper,
      leftCoefficient * rightCoefficient /= 0
    ]
  {-# INLINE mobiusSupport #-}

  memoTime sample =
    let curriedMemo =
          memoTime (\left -> memoTime (\right -> sample (left, right)))
     in \(left, right) -> curriedMemo left right

instance (Ord left, Ord right, RootedLocallyFiniteOrder left, RootedLocallyFiniteOrder right) => RootedLocallyFiniteOrder (left, right) where
  leastTime =
    (leastTime, leastTime)

  integralSampler sample =
    \(left, right) -> columnIntegrals right left
    where
      rowIntegrals =
        memoTime (\left -> integralSampler (\right -> sample (left, right)))

      columnIntegrals =
        memoTime (\right -> integralSampler (\left -> rowIntegrals left right))
