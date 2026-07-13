{-# LANGUAGE BangPatterns #-}

module Moonlight.LinAlg.Pure.Sparse.Solver.Common
  ( validateSparseSystemInput,
    sparseDiagonal,
    solverEpsilon,
    shiftedDiagonalValue,
  )
where

import Data.Vector.Unboxed qualified as U
import Moonlight.LinAlg.Pure.Sparse.Solver.Types (SparseIterativeFailure (..))
import Moonlight.LinAlg.Pure.Sparse.Types
  ( SparseCSR,
    csrCols,
    csrColumnIndicesVector,
    csrRows,
    csrRowOffsetsVector,
    csrValuesVector,
  )
import Prelude

validateSparseSystemInput ::
  SparseCSR Double ->
  U.Vector Double ->
  U.Vector Double ->
  Either SparseIterativeFailure ()
validateSparseSystemInput sparseMatrix rhsValues initialGuess =
  if csrRows sparseMatrix /= csrCols sparseMatrix
    then Left (SparseInvalidInput "sparse iterative solver expects a square system matrix")
    else
      if U.length rhsValues /= csrRows sparseMatrix
        then Left (SparseInvalidInput "sparse iterative solver expects RHS length equal to matrix dimension")
        else
          if U.length initialGuess /= csrRows sparseMatrix
            then Left (SparseInvalidInput "sparse iterative solver expects initial guess length equal to matrix dimension")
            else Right ()

sparseDiagonal ::
  SparseCSR Double ->
  Either SparseIterativeFailure (U.Vector Double)
sparseDiagonal sparseMatrix =
  Right
    ( U.generate
        (csrRows sparseMatrix)
        (diagonalAt sparseMatrix)
    )

diagonalAt :: SparseCSR Double -> Int -> Double
diagonalAt sparseMatrix rowIndex =
  findDiagonal startOffset
  where
    !rowOffsets = csrRowOffsetsVector sparseMatrix
    !columnIndices = csrColumnIndicesVector sparseMatrix
    !values = csrValuesVector sparseMatrix
    !startOffset = rowOffsets `U.unsafeIndex` rowIndex
    !endOffset = rowOffsets `U.unsafeIndex` (rowIndex + 1)

    findDiagonal !entryIndex
      | entryIndex >= endOffset = 0.0
      | otherwise =
          let !columnIndex =
                columnIndices `U.unsafeIndex` entryIndex
           in case compare columnIndex rowIndex of
                LT -> findDiagonal (entryIndex + 1)
                EQ -> values `U.unsafeIndex` entryIndex
                GT -> 0.0
{-# INLINE diagonalAt #-}

solverEpsilon :: Double
solverEpsilon = 1.0e-12

shiftedDiagonalValue :: Double -> Double -> Double
shiftedDiagonalValue shiftValue diagonalValue
  | diagonalValue >= 0.0 = diagonalValue + shiftValue
  | otherwise = diagonalValue - shiftValue
