{-# OPTIONS_GHC -O3 -fllvm -optlo-O3 -optlc-O3 #-}

-- | The binary64 coordinate kernel and exact predicate boundary.
module Moonlight.Triangulation.Scalar
  ( scalarName
  , scalarByteSize
  , scalarBinaryFormat
  , scalarEpsilon
  , scalarUnitRoundoff
  , scalarCcwErrorBound
  , scalarInCircleErrorBound
  , orient2dCoordinates
  , inCircleCoordinates
  , circumradiusSquaredWithinCoordinates
  , BinaryFormat
  , formatRadix
  , formatMantissaDigits
  , formatExponentRange
  , minimumAllowedCoordinate
  , maximumAllowedCoordinate
  , isFinite
  , canonicalScalarZero
  ) where

import Moonlight.Triangulation.Internal.Dyadic
  ( exactCircumradiusSquaredWithin
  , exactInCircleDet
  , exactOrientSignDouble
  )

-- | The coordinate component of canonical point identity. IEEE signed zeros
-- compare equal but hash differently by bits; every coordinate-keyed owner
-- therefore normalizes them before storage or hashing.
canonicalScalarZero :: Double -> Double
canonicalScalarZero value
  | value == 0 = 0
  | otherwise = value
{-# INLINE canonicalScalarZero #-}

-- | Machine format of the coordinate scalar.
data BinaryFormat = BinaryFormat
  { formatRadix :: !Integer
    -- ^ Numeric base of the significand.
  , formatMantissaDigits :: !Int
    -- ^ Number of base-'formatRadix' digits in the significand.
  , formatExponentRange :: !(Int, Int)
    -- ^ Inclusive minimum and exclusive maximum exponent bounds.
  }
  deriving stock (Eq, Show)

-- | Stable name of the coordinate scalar.
scalarName :: String
scalarName = "binary64"

-- | Bytes occupied by one coordinate component.
scalarByteSize :: Int
scalarByteSize = 8

-- | Runtime-confirmed binary format of 'Double'.
scalarBinaryFormat :: BinaryFormat
scalarBinaryFormat =
  BinaryFormat
    { formatRadix = floatRadix (0 :: Double)
    , formatMantissaDigits = floatDigits (0 :: Double)
    , formatExponentRange = floatRange (0 :: Double)
    }

-- | Difference between one and the next representable value above one.
scalarEpsilon :: Double
scalarEpsilon = 2.220446049250313e-16

-- | Maximum relative rounding error of one binary64 operation.
scalarUnitRoundoff :: Double
scalarUnitRoundoff = 1.1102230246251565e-16

-- | Error coefficient for the filtered orientation predicate.
scalarCcwErrorBound :: Double
scalarCcwErrorBound = 3.3306690738754716e-16

-- | Error coefficient for the filtered in-circle predicate.
scalarInCircleErrorBound :: Double
scalarInCircleErrorBound = 1.1102230246251577e-15

-- | Exact orientation ordering of three binary64 coordinate pairs.
orient2dCoordinates
  :: Double -> Double -> Double -> Double -> Double -> Double
  -> Ordering
orient2dCoordinates = filteredOrient2dDouble
{-# INLINE orient2dCoordinates #-}

-- | Exact in-circle ordering of four binary64 coordinate pairs.
inCircleCoordinates
  :: Double -> Double -> Double -> Double
  -> Double -> Double -> Double -> Double
  -> Ordering
inCircleCoordinates = filteredInCircle scalarInCircleErrorBound
{-# INLINE inCircleCoordinates #-}

-- | Closed exact circumradius membership at a finite, non-negative binary64
-- threshold. Invalid thresholds and collinear triples are outside.
circumradiusSquaredWithinCoordinates
  :: Double
  -> Double -> Double -> Double -> Double -> Double -> Double
  -> Bool
circumradiusSquaredWithinCoordinates threshold ax ay bx by cx cy
    | threshold < 0 || not (isFinite threshold) = False
    | not (isFinite ax && isFinite ay && isFinite bx
        && isFinite by && isFinite cx && isFinite cy) = False
    | isFinite difference && isFinite tolerance
        && tolerance > 0 && abs difference > tolerance = difference <= 0
    | otherwise = exactCircumradiusSquaredWithin threshold ax ay bx by cx cy
   where
    !abx = bx - ax
    !aby = by - ay
    !acx = cx - ax
    !acy = cy - ay
    !bcx = cx - bx
    !bcy = cy - by
    !abSquared = abx * abx + aby * aby
    !acSquared = acx * acx + acy * acy
    !bcSquared = bcx * bcx + bcy * bcy
    !determinant = abx * acy - aby * acx
    !radiusNumerator = abSquared * acSquared * bcSquared
    !thresholdDenominator = 4 * determinant * determinant * threshold
    !difference = radiusNumerator - thresholdDenominator
    !permanent = abs radiusNumerator + abs thresholdDenominator
    !tolerance = 128 * scalarUnitRoundoff * permanent

{-# INLINE circumradiusSquaredWithinCoordinates #-}

-- | Whether a scalar is neither infinite nor NaN. Pure subtraction avoids the
-- FFI calls used by base's predicates in the supported GHC.
isFinite :: Double -> Bool
isFinite value = value - value == 0
{-# INLINE isFinite #-}

-- The binary64 kernel pairs the approximation test with the
-- fixed-precision exact sign, which answers the dyadic determinant's sign in
-- machine words rather than allocated Integers whenever the exponent spread
-- allows, and defers to the dyadic determinant when it does not.
filteredOrient2dDouble
  :: Double -> Double -> Double -> Double -> Double -> Double -> Ordering
filteredOrient2dDouble ax ay bx by cx cy
  | abs determinant > errorBound * determinantSum = compare determinant 0
  | otherwise = exactOrientSignDouble ax ay bx by cx cy
 where
  errorBound = 3.3306690738754716e-16
  !left = (ax - cx) * (by - cy)
  !right = (ay - cy) * (bx - cx)
  !determinant = left - right
  !determinantSum = abs left + abs right
{-# INLINE filteredOrient2dDouble #-}

filteredInCircle
  :: Double
  -> Double -> Double -> Double -> Double
  -> Double -> Double -> Double -> Double
  -> Ordering
filteredInCircle errorBound ax ay bx by cx cy dx dy
  | abs determinant > errorBound * permanent = compare determinant 0
  | otherwise = compare (exactInCircleDet ax ay bx by cx cy dx dy) 0
 where
  !adx = ax - dx
  !ady = ay - dy
  !bdx = bx - dx
  !bdy = by - dy
  !cdx = cx - dx
  !cdy = cy - dy
  !abdet = adx * bdy - bdx * ady
  !bcdet = bdx * cdy - cdx * bdy
  !cadet = cdx * ady - adx * cdy
  !alift = adx * adx + ady * ady
  !blift = bdx * bdx + bdy * bdy
  !clift = cdx * cdx + cdy * cdy
  !determinant = alift * bcdet + blift * cadet + clift * abdet
  !permanent =
    (abs (bdx * cdy) + abs (cdx * bdy)) * alift
      + (abs (cdx * ady) + abs (adx * cdy)) * blift
      + (abs (adx * bdy) + abs (bdx * ady)) * clift
{-# INLINE filteredInCircle #-}

-- | The smallest coordinate magnitude the exact predicates accept.
minimumAllowedCoordinate :: Double
minimumAllowedCoordinate = 1.793662034335766e-43

-- | The largest coordinate magnitude the exact predicates accept.
maximumAllowedCoordinate :: Double
maximumAllowedCoordinate = 3.2138760885179806e60
