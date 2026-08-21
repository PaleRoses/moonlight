{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Normalized exact rational arithmetic without geometric dependencies.
module Moonlight.Triangulation.Internal.ExactRational
  ( ExactRational
  , ExactArithmeticError (..)
  , exactRational
  , exactRationalFromDouble
  , exactRationalFromFiniteDouble
  , exactRationalFromDyadic
  , exactRationalNumerator
  , exactRationalDenominator
  , exactRationalIsZero
  , exactDivide
  , exactSignum
  ) where

import Control.DeepSeq (NFData)
import Data.Bits (shiftL)
import Data.Ratio (Ratio, (%))
import qualified Data.Ratio as Ratio
import GHC.Generics (Generic)

-- | A checked wrapper around a reduced ratio with a strictly positive
-- denominator. 'Ratio' owns normalization, including the unique zero
-- representation @0 / 1@.
newtype ExactRational = ExactRational (Ratio Integer)
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num)
  deriving anyclass (NFData)

-- | Typed refusals from exact rational construction and division.
data ExactArithmeticError
  = ExactZeroDenominator
  | ExactZeroDivisor
  | ExactNaNInput
  | ExactInfiniteInput
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Construct a reduced rational, moving any sign onto the numerator and
-- refusing a zero denominator.
exactRational :: Integer -> Integer -> Either ExactArithmeticError ExactRational
exactRational _ 0 = Left ExactZeroDenominator
exactRational numerator denominator = Right (ExactRational (numerator % denominator))

-- | Convert a binary64 value without loss, refusing NaN and infinities.
exactRationalFromDouble :: Double -> Either ExactArithmeticError ExactRational
exactRationalFromDouble value
  | isNaN value = Left ExactNaNInput
  | isInfinite value = Left ExactInfiniteInput
  | otherwise = Right (exactRationalFromFiniteDouble value)

-- | Convert an already-admitted finite coordinate without loss. The caller
-- supplies a finite value, such as a coordinate inside a validated
-- @QueryPoint@, so this worker does not repeat the admission refusal.
exactRationalFromFiniteDouble :: Double -> ExactRational
exactRationalFromFiniteDouble value =
  uncurry exactRationalFromDyadic (decodeFloat value)
{-# INLINE exactRationalFromFiniteDouble #-}

-- | Construct @numerator * 2^power@ without a partial denominator path.
exactRationalFromDyadic :: Integer -> Int -> ExactRational
exactRationalFromDyadic numerator power
  | power >= 0 = fromInteger (numerator `shiftL` power)
  | otherwise = ExactRational (numerator % (1 `shiftL` negate power))
{-# INLINE exactRationalFromDyadic #-}

-- | Read the reduced numerator.
exactRationalNumerator :: ExactRational -> Integer
exactRationalNumerator (ExactRational value) = Ratio.numerator value
{-# INLINE exactRationalNumerator #-}

-- | Read the strictly positive reduced denominator.
exactRationalDenominator :: ExactRational -> Integer
exactRationalDenominator (ExactRational value) = Ratio.denominator value
{-# INLINE exactRationalDenominator #-}

-- | Test whether the exact value is zero.
exactRationalIsZero :: ExactRational -> Bool
exactRationalIsZero (ExactRational value) = Ratio.numerator value == 0
{-# INLINE exactRationalIsZero #-}

-- | Divide by a nonzero exact rational, refusing a zero divisor explicitly.
exactDivide
  :: ExactRational
  -> ExactRational
  -> Either ExactArithmeticError ExactRational
exactDivide (ExactRational left) (ExactRational right)
  | Ratio.numerator right == 0 = Left ExactZeroDivisor
  | otherwise = Right (ExactRational (left / right))

-- | Compare an exact rational with zero through its canonical numerator.
exactSignum :: ExactRational -> Ordering
exactSignum (ExactRational value) = compare (Ratio.numerator value) 0
{-# INLINE exactSignum #-}
