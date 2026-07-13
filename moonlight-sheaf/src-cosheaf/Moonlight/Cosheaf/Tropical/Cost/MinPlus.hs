{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Cosheaf.Tropical.Cost.MinPlus
  ( MinPlusWeight (..),
    TropicalCostParseFailure (..),
    minPlusZero,
    minPlusOne,
    minPlusFinite,
    minPlusInfinity,
    minPlusAdd,
    minPlusMul,
    minPlusSum,
    minPlusProduct,
    minPlusFromRational,
    parseMinPlusWeight,
  )
where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Read (readMaybe)

type MinPlusWeight :: Type
data MinPlusWeight
  = MinPlusFinite !Rational
  | MinPlusInfinity
  deriving stock (Eq, Ord, Show, Read)

type TropicalCostParseFailure :: Type
data TropicalCostParseFailure
  = TropicalCostEmpty
  | TropicalCostInvalidText !Text
  | TropicalCostNaN
  | TropicalCostNegativeInfinity
  | TropicalCostUnsupportedInfinityText !Text
  deriving stock (Eq, Ord, Show)

minPlusZero :: MinPlusWeight
minPlusZero =
  MinPlusInfinity
{-# INLINE minPlusZero #-}

minPlusOne :: MinPlusWeight
minPlusOne =
  MinPlusFinite 0
{-# INLINE minPlusOne #-}

minPlusFinite :: Rational -> MinPlusWeight
minPlusFinite =
  MinPlusFinite
{-# INLINE minPlusFinite #-}

minPlusInfinity :: MinPlusWeight
minPlusInfinity =
  MinPlusInfinity
{-# INLINE minPlusInfinity #-}

minPlusAdd :: MinPlusWeight -> MinPlusWeight -> MinPlusWeight
minPlusAdd =
  min
{-# INLINE minPlusAdd #-}

minPlusMul :: MinPlusWeight -> MinPlusWeight -> MinPlusWeight
minPlusMul MinPlusInfinity _rightWeight =
  MinPlusInfinity
minPlusMul _leftWeight MinPlusInfinity =
  MinPlusInfinity
minPlusMul (MinPlusFinite leftValue) (MinPlusFinite rightValue) =
  MinPlusFinite (leftValue + rightValue)
{-# INLINE minPlusMul #-}

minPlusSum :: Foldable foldable => foldable MinPlusWeight -> MinPlusWeight
minPlusSum =
  foldl' minPlusAdd minPlusZero
{-# INLINE minPlusSum #-}

minPlusProduct :: Foldable foldable => foldable MinPlusWeight -> MinPlusWeight
minPlusProduct =
  foldl' minPlusMul minPlusOne
{-# INLINE minPlusProduct #-}

minPlusFromRational :: Rational -> Either TropicalCostParseFailure MinPlusWeight
minPlusFromRational =
  Right . MinPlusFinite
{-# INLINE minPlusFromRational #-}

parseMinPlusWeight :: Text -> Either TropicalCostParseFailure MinPlusWeight
parseMinPlusWeight rawText
  | Text.null strippedText =
      Left TropicalCostEmpty
  | normalizedText == Text.pack "nan" =
      Left TropicalCostNaN
  | normalizedText == Text.pack "infinity" || strippedText == Text.pack "∞" =
      Left (TropicalCostUnsupportedInfinityText strippedText)
  | normalizedText == Text.pack "+infinity" || strippedText == Text.pack "+∞" =
      Left (TropicalCostUnsupportedInfinityText strippedText)
  | normalizedText == Text.pack "-infinity" || strippedText == Text.pack "-∞" =
      Left TropicalCostNegativeInfinity
  | otherwise =
      maybe
        (Left (TropicalCostInvalidText strippedText))
        minPlusFromRational
        (readMaybe (Text.unpack strippedText))
  where
    strippedText =
      Text.strip rawText

    normalizedText =
      Text.toCaseFold strippedText
