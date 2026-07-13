{-# LANGUAGE StrictData #-}

-- | Flat row-major dense storage for hot Double kernels.
--
-- Nested lists remain the validation and authoring surface; this module owns
-- contiguous row-major execution.
module Moonlight.LinAlg.Pure.Dense.Flat
  ( DenseDoubleMatrix,
    mkDenseDoubleMatrixRowMajor,
    mkDenseDoubleMatrixRows,
    trustedDenseDoubleMatrixRowMajor,
    denseDoubleMatrixShape,
    denseDoubleMatrixToRowMajorVector,
    denseDoubleMatrixToRows,
    denseDoubleMatrixVectorProduct,
  )
where

import Data.Kind (Type)
import Data.Vector.Storable qualified as S
import Moonlight.Core (MoonlightError (..))
import Moonlight.LinAlg.Pure.Dense.Rows
  ( denseRowsShape,
    denseRowsToLists,
    mkDenseRows,
  )
import Prelude

type DenseDoubleMatrix :: Type
data DenseDoubleMatrix = DenseDoubleMatrix
  { denseDoubleMatrixRowCount :: !Int,
    denseDoubleMatrixColumnCount :: !Int,
    denseDoubleMatrixPayload :: !(S.Vector Double)
  }
  deriving stock (Eq, Show)

mkDenseDoubleMatrixRowMajor :: Int -> Int -> S.Vector Double -> Either MoonlightError DenseDoubleMatrix
mkDenseDoubleMatrixRowMajor rowCount columnCount rowMajorValues
  | rowCount < 0 || columnCount < 0 =
      Left (InvariantViolation "dense Double matrix dimensions must be non-negative")
  | S.length rowMajorValues /= expectedLength =
      Left
        ( InvariantViolation
            ( "dense Double row-major payload length mismatch: expected "
                <> show expectedLength
                <> " values but received "
                <> show (S.length rowMajorValues)
            )
        )
  | S.any (not . isFiniteDouble) rowMajorValues =
      Left (InvariantViolation "dense Double row-major payload requires finite entries")
  | otherwise =
      Right
        ( trustedDenseDoubleMatrixRowMajor
            rowCount
            columnCount
            rowMajorValues
        )
  where
    expectedLength = rowCount * columnCount

trustedDenseDoubleMatrixRowMajor :: Int -> Int -> S.Vector Double -> DenseDoubleMatrix
trustedDenseDoubleMatrixRowMajor rowCount columnCount rowMajorValues =
  DenseDoubleMatrix
    { denseDoubleMatrixRowCount = rowCount,
      denseDoubleMatrixColumnCount = columnCount,
      denseDoubleMatrixPayload = rowMajorValues
    }

mkDenseDoubleMatrixRows :: [[Double]] -> Either MoonlightError DenseDoubleMatrix
mkDenseDoubleMatrixRows rowValues = do
  denseRowsValue <- mkDenseRows rowValues
  let (rowCount, columnCount) = denseRowsShape denseRowsValue
  mkDenseDoubleMatrixRowMajor
    rowCount
    columnCount
    (S.fromList (concat (denseRowsToLists denseRowsValue)))

denseDoubleMatrixShape :: DenseDoubleMatrix -> (Int, Int)
denseDoubleMatrixShape matrixValue =
  (denseDoubleMatrixRowCount matrixValue, denseDoubleMatrixColumnCount matrixValue)

denseDoubleMatrixToRowMajorVector :: DenseDoubleMatrix -> S.Vector Double
denseDoubleMatrixToRowMajorVector = denseDoubleMatrixPayload

denseDoubleMatrixToRows :: DenseDoubleMatrix -> [[Double]]
denseDoubleMatrixToRows matrixValue =
  fmap rowValues [0 .. denseDoubleMatrixRowCount matrixValue - 1]
  where
    columnCount = denseDoubleMatrixColumnCount matrixValue
    payload = denseDoubleMatrixPayload matrixValue
    rowValues rowIndex =
      S.toList (S.slice (rowIndex * columnCount) columnCount payload)

denseDoubleMatrixVectorProduct :: DenseDoubleMatrix -> S.Vector Double -> Either MoonlightError (S.Vector Double)
denseDoubleMatrixVectorProduct matrixValue vectorValue =
  if S.length vectorValue /= denseDoubleMatrixColumnCount matrixValue
    then
      Left
        ( InvariantViolation
            ( "dense Double matrix/vector shape mismatch (matrix="
                <> show (denseDoubleMatrixShape matrixValue)
                <> ", vector="
                <> show (S.length vectorValue)
                <> ")"
            )
        )
    else
      Right
        ( S.generate
            (denseDoubleMatrixRowCount matrixValue)
            (denseDoubleMatrixRowDot matrixValue vectorValue)
        )
{-# INLINE denseDoubleMatrixVectorProduct #-}

denseDoubleMatrixRowDot :: DenseDoubleMatrix -> S.Vector Double -> Int -> Double
denseDoubleMatrixRowDot matrixValue vectorValue rowIndex =
  S.ifoldl' accumulateEntry 0.0 vectorValue
  where
    columnCount = denseDoubleMatrixColumnCount matrixValue
    rowOffset = rowIndex * columnCount
    payload = denseDoubleMatrixPayload matrixValue

    accumulateEntry accumulator columnIndex vectorEntry =
      accumulator
        + (payload `S.unsafeIndex` (rowOffset + columnIndex))
          * vectorEntry
{-# INLINE denseDoubleMatrixRowDot #-}

isFiniteDouble :: Double -> Bool
isFiniteDouble value =
  not (isNaN value || isInfinite value)
