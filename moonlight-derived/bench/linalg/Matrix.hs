{-# LANGUAGE NamedFieldPuns #-}

module Matrix
  ( benchmarks
  , probeCases
  ) where

import Data.Vector qualified as Vector
import Fixture
  ( BenchmarkFixture (..)
  , BenchmarkResult
  , ProbeCase
  , ProbeFamily (..)
  , benchmarkEitherWith
  , checksumBlockedMatGF2
  , checksumDenseMatGF2
  )
import Registry
  ( BenchCase
  , benchCase
  , familyBenchmarks
  , hostileProbeCases
  )
import Moonlight.Derived.Matrix
  ( blockCatChecked
  , denseFromEntriesWithChecked
  , denseMatCols
  , denseMatRows
  , entriesToBlockedMatGF2Checked
  , fromExpandedChecked
  , hcatChecked
  , matAddChecked
  , matMulChecked
  , vcatChecked
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix (transposeMat)
import Moonlight.LinAlg (GF2)
import Test.Tasty.Bench (Benchmark)

benchmarks :: [BenchmarkFixture] -> Benchmark
benchmarks fixtures =
  familyBenchmarks "matrix" matrixFamilies fixtures

probeCases :: [BenchmarkFixture] -> [ProbeCase]
probeCases =
  hostileProbeCases "matrix" ProbeFamilyStructural matrixFamilies

matrixFamilies :: [BenchCase]
matrixFamilies =
  [ benchCase "dense-add" runDenseAdd
  , benchCase "dense-mul-transpose" runDenseMulTranspose
  , benchCase "dense-hcat" runDenseHcat
  , benchCase "dense-vcat" runDenseVcat
  , benchCase "dense-block-cat" runDenseBlockCat
  , benchCase "dense-from-entries" runDenseFromEntries
  , benchCase "blocked-from-expanded" runBlockedFromExpanded
  , benchCase "blocked-from-entries" runBlockedFromEntries
  ]

runDenseAdd :: BenchmarkFixture -> BenchmarkResult
runDenseAdd fixture =
  benchmarkEitherWith
    checksumDenseMatGF2
    (matAddChecked (bfExpandedDense fixture) (bfExpandedDense fixture))

runDenseMulTranspose :: BenchmarkFixture -> BenchmarkResult
runDenseMulTranspose fixture =
  benchmarkEitherWith
    checksumDenseMatGF2
    (matMulChecked (bfExpandedDense fixture) (transposeMat (bfExpandedDense fixture)))

runDenseHcat :: BenchmarkFixture -> BenchmarkResult
runDenseHcat fixture =
  benchmarkEitherWith
    checksumDenseMatGF2
    (hcatChecked [bfExpandedDense fixture, bfExpandedDense fixture])

runDenseVcat :: BenchmarkFixture -> BenchmarkResult
runDenseVcat fixture =
  benchmarkEitherWith
    checksumDenseMatGF2
    (vcatChecked [bfExpandedDense fixture, bfExpandedDense fixture])

runDenseBlockCat :: BenchmarkFixture -> BenchmarkResult
runDenseBlockCat fixture =
  benchmarkEitherWith
    checksumDenseMatGF2
    ( blockCatChecked
        [ [bfExpandedDense fixture, bfExpandedDense fixture]
        , [bfExpandedDense fixture, bfExpandedDense fixture]
        ]
    )

runDenseFromEntries :: BenchmarkFixture -> BenchmarkResult
runDenseFromEntries fixture =
  benchmarkEitherWith
    checksumDenseMatGF2
    (denseFromEntriesWithChecked rowCount columnCount entries entryLocation entryCoefficient)
  where
    rowCount =
      denseMatRows (bfExpandedDense fixture)
    columnCount =
      denseMatCols (bfExpandedDense fixture)
    entries =
      matrixEntries rowCount columnCount

runBlockedFromExpanded :: BenchmarkFixture -> BenchmarkResult
runBlockedFromExpanded fixture =
  benchmarkEitherWith
    checksumBlockedMatGF2
    (fromExpandedChecked (bfExpandedRows fixture) (bfExpandedCols fixture) (bfExpandedDense fixture))

runBlockedFromEntries :: BenchmarkFixture -> BenchmarkResult
runBlockedFromEntries fixture =
  benchmarkEitherWith
    checksumBlockedMatGF2
    ( entriesToBlockedMatGF2Checked
        snd
        snd
        rowCells
        columnCells
        rowCount
        columnCount
        entries
        entryLocation
        entryIsOdd
    )
  where
    rowCount =
      Vector.length (bfExpandedRows fixture)
    columnCount =
      Vector.length (bfExpandedCols fixture)
    rowCells =
      Vector.toList (Vector.indexed (bfExpandedRows fixture))
    columnCells =
      Vector.toList (Vector.indexed (bfExpandedCols fixture))
    entries =
      matrixEntries rowCount columnCount

data MatrixEntry = MatrixEntry
  { meRow :: !Int
  , meColumn :: !Int
  , meOdd :: !Bool
  }

matrixEntries :: Int -> Int -> [MatrixEntry]
matrixEntries rowCount columnCount =
  [ MatrixEntry rowIndex columnIndex (entryParity rowIndex columnIndex)
  | rowIndex <- [0 .. rowCount - 1]
  , columnIndex <- [0 .. columnCount - 1]
  , entryParity rowIndex columnIndex
  ]

entryLocation :: MatrixEntry -> (Int, Int)
entryLocation MatrixEntry{meRow, meColumn} =
  (meRow, meColumn)

entryCoefficient :: MatrixEntry -> GF2
entryCoefficient entryValue =
  if entryIsOdd entryValue then 1 else 0

entryIsOdd :: MatrixEntry -> Bool
entryIsOdd =
  meOdd

entryParity :: Int -> Int -> Bool
entryParity rowIndex columnIndex =
  rowIndex == columnIndex || (rowIndex * 17 + columnIndex * 31) `mod` 7 == 0
