{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Stream
  ( Stream,
    ScalarLinearMap (..),
    stream,
    streamAt,
    streamNaturalPrefix,
    streamNaturalProductPrefix,
    constant,
    mapStream,
    zipWithStream,
    delay,
    differentiate,
    integrate,
    incrementalize,
    applyScalarLinearMap,
    mapScalarLinearStream,
    differentiateNaturalPrefix,
    integrateNaturalPrefix,
    incrementalizeNaturalPrefix,
    incrementalizeScalarLinearNaturalPrefix,
    differentiateNaturalProductPrefix,
    integrateNaturalProductPrefix,
    differentiateNaturalProductRows,
    integrateNaturalProductRows,
  )
where

import Data.Foldable qualified as Foldable
import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Traversable
  ( mapAccumL,
  )
import Numeric.Natural
  ( Natural,
  )

import Moonlight.Core (AdditiveGroup (..), AdditiveMonoid (..))
import Moonlight.Differential.Order.LocallyFinite
  ( RootedLocallyFiniteOrder (..),
    foldMapGroup,
    mobiusSupport,
    scaleInteger,
  )

type Stream :: Type -> Type -> Type
newtype Stream time value = Stream
  { streamAtRaw :: time -> value
  }

type ScalarLinearMap :: Type
data ScalarLinearMap = ScaleByInteger !Integer
  deriving stock (Eq, Ord, Show)

streamAt :: Stream time value -> time -> value
streamAt =
  streamAtRaw
{-# INLINE streamAt #-}

stream :: (time -> value) -> Stream time value
stream =
  Stream
{-# INLINE stream #-}

streamNaturalPrefix ::
  Int ->
  Stream Natural value ->
  [value]
streamNaturalPrefix prefixLength values =
  fmap
    (streamAt values . fromIntegral)
    (take prefixLength ([0 ..] :: [Int]))
{-# INLINE streamNaturalPrefix #-}

streamNaturalProductPrefix ::
  Int ->
  Stream (Natural, Natural) value ->
  [[value]]
streamNaturalProductPrefix sideLength values =
  fmap
    rowAt
    (take sideLength ([0 ..] :: [Int]))
  where
    rowAt left =
      fmap
        (\right -> streamAt values (fromIntegral left, fromIntegral right))
        (take sideLength ([0 ..] :: [Int]))
{-# INLINE streamNaturalProductPrefix #-}

constant :: value -> Stream time value
constant value =
  Stream (const value)
{-# INLINE constant #-}

mapStream ::
  (left -> right) ->
  Stream time left ->
  Stream time right
mapStream transform values =
  Stream (transform . streamAt values)
{-# INLINE mapStream #-}

zipWithStream ::
  (left -> right -> result) ->
  Stream time left ->
  Stream time right ->
  Stream time result
zipWithStream combine left right =
  Stream
    ( \time ->
        combine
          (streamAt left time)
          (streamAt right time)
    )
{-# INLINE zipWithStream #-}

delay ::
  AdditiveGroup value =>
  Stream Natural value ->
  Stream Natural value
delay values =
  Stream
    ( \time ->
        if time == 0
          then zero
          else streamAt values (time - 1)
    )
{-# INLINE delay #-}

differentiate ::
  (Ord time, RootedLocallyFiniteOrder time, AdditiveGroup value) =>
  Stream time value ->
  Stream time value
differentiate values =
  Stream
    ( \target ->
        foldMapGroup
          ( \(source, coefficient) ->
              if coefficient == 0
                then zero
                else scaleInteger coefficient (streamAt values source)
          )
          (mobiusSupport leastTime target)
    )
{-# INLINE differentiate #-}

integrate ::
  (RootedLocallyFiniteOrder time, AdditiveGroup value) =>
  Stream time value ->
  Stream time value
integrate deltas =
  Stream (integralSampler (streamAt deltas))
{-# INLINE integrate #-}

incrementalize ::
  ( Ord time,
    RootedLocallyFiniteOrder time,
    AdditiveGroup input,
    AdditiveGroup output
  ) =>
  (Stream time input -> Stream time output) ->
  Stream time input ->
  Stream time output
incrementalize query =
  differentiate . query . integrate
{-# INLINE incrementalize #-}

applyScalarLinearMap ::
  AdditiveGroup value =>
  ScalarLinearMap ->
  value ->
  value
applyScalarLinearMap linearMap =
  case linearMap of
    ScaleByInteger 0 ->
      const zero
    ScaleByInteger 1 ->
      id
    ScaleByInteger (-1) ->
      neg
    ScaleByInteger 2 ->
      \value -> add value value
    ScaleByInteger coefficient ->
      scaleInteger coefficient
{-# INLINE applyScalarLinearMap #-}

mapScalarLinearStream ::
  AdditiveGroup value =>
  ScalarLinearMap ->
  Stream time value ->
  Stream time value
mapScalarLinearStream linearMap =
  mapStream (applyScalarLinearMap linearMap)
{-# INLINE mapScalarLinearStream #-}

type NaturalPrefixScan :: Type -> Type
data NaturalPrefixScan value = NaturalPrefixScan !value [value]

naturalPrefixScanValues :: NaturalPrefixScan value -> [value]
naturalPrefixScanValues (NaturalPrefixScan _ values) =
  values

differentiateNaturalPrefix ::
  AdditiveGroup value =>
  Int ->
  Stream Natural value ->
  [value]
differentiateNaturalPrefix prefixLength values =
  differentiateNaturalValues (streamNaturalPrefix prefixLength values)
{-# INLINE differentiateNaturalPrefix #-}

differentiateNaturalValues ::
  AdditiveGroup value =>
  [value] ->
  [value]
differentiateNaturalValues values =
  reverse
    ( naturalPrefixScanValues
        ( Foldable.foldl'
            collectDifference
            (NaturalPrefixScan zero [])
            values
        )
    )
  where
    collectDifference :: AdditiveGroup value => NaturalPrefixScan value -> value -> NaturalPrefixScan value
    collectDifference (NaturalPrefixScan previous differences) current =
      let difference =
            sub current previous
       in difference `seq` NaturalPrefixScan current (difference : differences)
{-# INLINE differentiateNaturalValues #-}

integrateNaturalPrefix ::
  AdditiveGroup value =>
  Int ->
  Stream Natural value ->
  [value]
integrateNaturalPrefix prefixLength deltas =
  integrateNaturalValues (streamNaturalPrefix prefixLength deltas)
{-# INLINE integrateNaturalPrefix #-}

integrateNaturalValues ::
  AdditiveGroup value =>
  [value] ->
  [value]
integrateNaturalValues values =
  reverse
    ( naturalPrefixScanValues
        ( Foldable.foldl'
            collectIntegral
            (NaturalPrefixScan zero [])
            values
        )
    )
  where
    collectIntegral :: AdditiveGroup value => NaturalPrefixScan value -> value -> NaturalPrefixScan value
    collectIntegral (NaturalPrefixScan accumulated integrals) delta =
      let next =
            add accumulated delta
       in next `seq` NaturalPrefixScan next (next : integrals)
{-# INLINE integrateNaturalValues #-}

incrementalizeNaturalPrefix ::
  (AdditiveGroup input, AdditiveGroup output) =>
  (Stream Natural input -> Stream Natural output) ->
  Int ->
  Stream Natural input ->
  [output]
incrementalizeNaturalPrefix query prefixLength inputs =
  differentiateNaturalPrefix
    prefixLength
    (query (naturalPrefixIntegralStream (integrateNaturalPrefix prefixLength inputs)))
{-# INLINE incrementalizeNaturalPrefix #-}

incrementalizeScalarLinearNaturalPrefix ::
  AdditiveGroup value =>
  ScalarLinearMap ->
  Int ->
  Stream Natural value ->
  [value]
incrementalizeScalarLinearNaturalPrefix linearMap prefixLength inputs =
  fmap
    (applyScalarLinearMap linearMap)
    (streamNaturalPrefix prefixLength inputs)
{-# INLINE incrementalizeScalarLinearNaturalPrefix #-}

differentiateNaturalProductPrefix ::
  AdditiveGroup value =>
  Int ->
  Stream (Natural, Natural) value ->
  [[value]]
differentiateNaturalProductPrefix sideLength values =
  differentiateNaturalProductRows (streamNaturalProductPrefix sideLength values)
{-# INLINE differentiateNaturalProductPrefix #-}

integrateNaturalProductPrefix ::
  AdditiveGroup value =>
  Int ->
  Stream (Natural, Natural) value ->
  [[value]]
integrateNaturalProductPrefix sideLength values =
  integrateNaturalProductRows (streamNaturalProductPrefix sideLength values)
{-# INLINE integrateNaturalProductPrefix #-}

integrateNaturalProductRows ::
  AdditiveGroup value =>
  [[value]] ->
  [[value]]
integrateNaturalProductRows =
  integrateNaturalProductOuterRows . fmap integrateNaturalValues
{-# INLINE integrateNaturalProductRows #-}

differentiateNaturalProductRows ::
  AdditiveGroup value =>
  [[value]] ->
  [[value]]
differentiateNaturalProductRows =
  fmap differentiateNaturalValues . differentiateNaturalProductOuterRows
{-# INLINE differentiateNaturalProductRows #-}

integrateNaturalProductOuterRows ::
  AdditiveGroup value =>
  [[value]] ->
  [[value]]
integrateNaturalProductOuterRows rows =
  snd (mapAccumL integrateOuterRow [] rows)
  where
    integrateOuterRow :: AdditiveGroup cell => [cell] -> [cell] -> ([cell], [cell])
    integrateOuterRow previousRow currentRow =
      (integrals, integrals)
      where
        width =
          length currentRow

        aboveRow =
          zeroPaddedPrefix width previousRow

        integrals =
          zipWith add currentRow aboveRow
{-# INLINE integrateNaturalProductOuterRows #-}

differentiateNaturalProductOuterRows ::
  AdditiveGroup value =>
  [[value]] ->
  [[value]]
differentiateNaturalProductOuterRows rows =
  snd (mapAccumL differentiateOuterRow [] rows)
  where
    differentiateOuterRow :: AdditiveGroup cell => [cell] -> [cell] -> ([cell], [cell])
    differentiateOuterRow previousRow currentRow =
      (currentRow, differences)
      where
        width =
          length currentRow

        aboveRow =
          zeroPaddedPrefix width previousRow

        differences =
          zipWith sub currentRow aboveRow
{-# INLINE differentiateNaturalProductOuterRows #-}

zeroPaddedPrefix ::
  AdditiveGroup value =>
  Int ->
  [value] ->
  [value]
zeroPaddedPrefix width values =
  take width (values <> repeat zero)
{-# INLINE zeroPaddedPrefix #-}

naturalPrefixIntegralStream ::
  AdditiveGroup value =>
  [value] ->
  Stream Natural value
naturalPrefixIntegralStream values =
  Stream
    ( \time ->
        Map.findWithDefault defaultValue time samples
    )
  where
    samples =
      naturalPrefixMap values

    defaultValue =
      Foldable.foldl' (\_ value -> value) zero values
{-# INLINE naturalPrefixIntegralStream #-}

naturalPrefixMap :: [value] -> Map Natural value
naturalPrefixMap values =
  Map.fromAscList (zip (fmap fromIntegral ([0 ..] :: [Int])) values)
{-# INLINE naturalPrefixMap #-}
