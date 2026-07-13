{-# LANGUAGE AllowAmbiguousTypes #-}

module Moonlight.LinAlg.Pure.Dense.Types
  ( Vector,
    Matrix,
    fromListVector,
    fromListMatrix,
    matrixRows,
    toListVector,
    toListMatrix,
    vectorLength,
    matrixShape,
    matrixDenseRows,
    matrixToRows,
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.TypeNats (KnownNat, Nat, natVal)
import Moonlight.Core (MoonlightError (..))
import Moonlight.LinAlg.Internal.Discrete ()
import Moonlight.LinAlg.Internal.Storage (checkFlatLength)
import Moonlight.LinAlg.Pure.Dense.Rows
  ( DenseRows,
    denseRowsToLists,
    mkDenseRowsFromFlat,
    mkDenseRowsWithShape,
  )
import Prelude

type Vector :: Nat -> Type -> Type
newtype Vector (n :: Nat) a = Vector
  { vectorPayload :: [a]
  }

type Matrix :: Nat -> Nat -> Type -> Type
newtype Matrix (r :: Nat) (c :: Nat) a = Matrix
  { matrixPayload :: [a]
  }

expectedLength :: forall n. KnownNat n => Int
expectedLength = fromIntegral (natVal (Proxy @n))

fromListVector :: forall n a. KnownNat n => [a] -> Either MoonlightError (Vector n a)
fromListVector values
  | length values /= expectedLength @n =
      Left
        ( InvariantViolation
            ( "vector length mismatch: expected "
                <> show (expectedLength @n)
                <> " values but received "
                <> show (length values)
            )
        )
  | otherwise = Right (Vector values)

fromListMatrix :: forall r c a. (KnownNat r, KnownNat c) => [a] -> Either MoonlightError (Matrix r c a)
fromListMatrix values = do
  let rowCount = expectedLength @r
      columnCount = expectedLength @c
  checkFlatLength rowCount columnCount values
  Right (Matrix values)

matrixRows :: forall r c a. (KnownNat r, KnownNat c) => [[a]] -> Either MoonlightError (Matrix r c a)
matrixRows rowValues = do
  denseRowsValue <-
    mkDenseRowsWithShape
      (expectedLength @r)
      (expectedLength @c)
      rowValues
  Right (Matrix (concat (denseRowsToLists denseRowsValue)))

toListVector :: Vector n a -> [a]
toListVector = vectorPayload

toListMatrix :: Matrix r c a -> [a]
toListMatrix = matrixPayload

vectorLength :: forall n a. KnownNat n => Vector n a -> Int
vectorLength _ = expectedLength @n

matrixShape :: forall r c a. (KnownNat r, KnownNat c) => Matrix r c a -> (Int, Int)
matrixShape _ = (expectedLength @r, expectedLength @c)

matrixDenseRows :: forall r c a. (KnownNat r, KnownNat c) => Matrix r c a -> Either MoonlightError (DenseRows a)
matrixDenseRows matrixValue =
  mkDenseRowsFromFlat
    (expectedLength @r)
    (expectedLength @c)
    (toListMatrix matrixValue)

matrixToRows :: forall r c a. (KnownNat r, KnownNat c) => Matrix r c a -> Either MoonlightError [[a]]
matrixToRows =
  fmap denseRowsToLists . matrixDenseRows
