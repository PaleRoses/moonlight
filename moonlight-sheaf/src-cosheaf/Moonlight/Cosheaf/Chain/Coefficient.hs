{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Cosheaf.Chain.Coefficient
  ( CoefficientOps (..),
    PivotOps (..),
    intCoefficientOps,
    integerCoefficientOps,
    rationalCoefficientOps,
    gf2CoefficientOps,
    intUnitPivotOps,
    integerUnitPivotOps,
    rationalPivotOps,
    gf2PivotOps,
  )
where

import Data.Kind (Type)
import Moonlight.LinAlg
  ( GF2 (..),
  )

-- | Explicit arithmetic dictionary for coefficient-sensitive assembly.
-- 'Num' alone is too vague for the Morse boundary: additive zero, additive
-- inverse, multiplication, and integer orientation coercion are part of the
-- interface, not folklore.
type CoefficientOps :: Type -> Type
data CoefficientOps coefficient = CoefficientOps
  { coIsZero :: coefficient -> Bool,
    coFromInteger :: Integer -> coefficient
  }

-- | Cancellation is a strictly stronger obligation than coefficient arithmetic.
-- Integer chains only cancel ±1; rational chains cancel every nonzero pivot.
type PivotOps :: Type -> Type
data PivotOps coefficient = PivotOps
  { poCoefficientOps :: !(CoefficientOps coefficient),
    poUnitInverse :: coefficient -> Maybe coefficient
  }

intCoefficientOps :: CoefficientOps Int
intCoefficientOps =
  numericCoefficientOps 0

integerCoefficientOps :: CoefficientOps Integer
integerCoefficientOps =
  numericCoefficientOps 0

rationalCoefficientOps :: CoefficientOps Rational
rationalCoefficientOps =
  numericCoefficientOps 0

gf2CoefficientOps :: CoefficientOps GF2
gf2CoefficientOps =
  numericCoefficientOps GF2Zero

numericCoefficientOps :: (Eq coefficient, Num coefficient) => coefficient -> CoefficientOps coefficient
numericCoefficientOps zeroValue =
  CoefficientOps
    { coIsZero = (== zeroValue),
      coFromInteger = fromInteger
    }

intUnitPivotOps :: PivotOps Int
intUnitPivotOps =
  unitPivotOps intCoefficientOps signedUnitInverse

integerUnitPivotOps :: PivotOps Integer
integerUnitPivotOps =
  unitPivotOps integerCoefficientOps signedUnitInverse

rationalPivotOps :: PivotOps Rational
rationalPivotOps =
  unitPivotOps rationalCoefficientOps nonzeroReciprocal

gf2PivotOps :: PivotOps GF2
gf2PivotOps =
  unitPivotOps gf2CoefficientOps gf2UnitInverse

unitPivotOps :: CoefficientOps coefficient -> (coefficient -> Maybe coefficient) -> PivotOps coefficient
unitPivotOps coefficientOps unitInverse =
  PivotOps
    { poCoefficientOps = coefficientOps,
      poUnitInverse = unitInverse
    }

signedUnitInverse :: (Eq coefficient, Num coefficient) => coefficient -> Maybe coefficient
signedUnitInverse coefficientValue
  | coefficientValue == 1 =
      Just 1
  | coefficientValue == -1 =
      Just (-1)
  | otherwise =
      Nothing

nonzeroReciprocal :: (Eq coefficient, Fractional coefficient) => coefficient -> Maybe coefficient
nonzeroReciprocal coefficientValue
  | coefficientValue == 0 =
      Nothing
  | otherwise =
      Just (recip coefficientValue)

gf2UnitInverse :: GF2 -> Maybe GF2
gf2UnitInverse coefficientValue =
  case coefficientValue of
    GF2Zero -> Nothing
    GF2One -> Just GF2One
