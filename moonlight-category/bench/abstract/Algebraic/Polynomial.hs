{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Algebraic.Polynomial
  ( polynomialBenchmarks,
  )
where

import Data.Function ((&))
import Data.Kind (Type)
import Moonlight.Category.Pure.CoveringFamily (Exists (..))
import Moonlight.Category.Pure.PolynomialFunctor
  ( Direction,
    ParameterizedDirection,
    ParameterizedPolynomialFunctor (..),
    PolynomialFunctor (..),
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

polynomialBenchmarks :: Benchmark
polynomialBenchmarks =
  bgroup
    "PolynomialFunctor"
    [ bench "allPositions batch x512" (nf (batchWeight polynomialPositionsWeight) sampleBatch),
      bench "positionsAt full/trimmed alternation batch x512" (nf parameterizedPolynomialPositionsBatchWeight sampleBatch),
      bench "direction witnesses batch x512" (nf (batchWeight polynomialDirectionWeight) sampleBatch)
    ]
type DemoPolynomial :: Type
data DemoPolynomial

type RootPosition :: Type
data RootPosition

type BranchPosition :: Type
data BranchPosition

type DemoParameterizedPolynomial :: Type
data DemoParameterizedPolynomial

type FullSliceRootPosition :: Type
data FullSliceRootPosition

type FullSliceBranchPosition :: Type
data FullSliceBranchPosition

type TrimmedSliceRootPosition :: Type
data TrimmedSliceRootPosition

instance PolynomialFunctor DemoPolynomial where
  data Position DemoPolynomial position where
    RootWitness :: Position DemoPolynomial RootPosition
    BranchWitness :: Position DemoPolynomial BranchPosition
  type Direction DemoPolynomial RootPosition = Bool
  type Direction DemoPolynomial BranchPosition = Maybe Bool
  allPositions = [Exists RootWitness, Exists BranchWitness]

instance ParameterizedPolynomialFunctor DemoParameterizedPolynomial where
  type PolynomialParameter DemoParameterizedPolynomial = Bool

  data ParameterizedPosition DemoParameterizedPolynomial position where
    FullSliceRootWitness :: ParameterizedPosition DemoParameterizedPolynomial FullSliceRootPosition
    FullSliceBranchWitness :: ParameterizedPosition DemoParameterizedPolynomial FullSliceBranchPosition
    TrimmedSliceRootWitness :: ParameterizedPosition DemoParameterizedPolynomial TrimmedSliceRootPosition

  type ParameterizedDirection DemoParameterizedPolynomial FullSliceRootPosition = Bool
  type ParameterizedDirection DemoParameterizedPolynomial FullSliceBranchPosition = Maybe Bool
  type ParameterizedDirection DemoParameterizedPolynomial TrimmedSliceRootPosition = ()

  positionsAt includeBranch =
    if includeBranch
      then [Exists FullSliceRootWitness, Exists FullSliceBranchWitness]
      else [Exists TrimmedSliceRootWitness]

polynomialPositionsWeight :: Int -> Int
polynomialPositionsWeight seed =
  seed + sum (fmap polynomialPositionWeight (allPositions @DemoPolynomial))

parameterizedPolynomialPositionsBatchWeight :: [Int] -> Int
parameterizedPolynomialPositionsBatchWeight =
  sum . fmap (\seed -> seed + parameterizedPolynomialPositionsWeight (even seed))

parameterizedPolynomialPositionsWeight :: Bool -> Int
parameterizedPolynomialPositionsWeight includeBranch =
  positionsAt @DemoParameterizedPolynomial includeBranch
    & fmap parameterizedPolynomialPositionWeight
    & sum

polynomialDirectionWeight :: Int -> Int
polynomialDirectionWeight seed =
  seed + boolWeight rootDirectionWitness + maybe 0 boolWeight (branchDirectionWitness seed) + boolWeight fullSliceRootDirectionWitness + maybe 0 boolWeight (fullSliceBranchDirectionWitness seed) + trimmedSliceRootDirectionWeight trimmedSliceRootDirectionWitness

rootDirectionWitness :: Direction DemoPolynomial RootPosition
rootDirectionWitness = True

branchDirectionWitness :: Int -> Direction DemoPolynomial BranchPosition
branchDirectionWitness seed = Just (even seed)

fullSliceRootDirectionWitness :: ParameterizedDirection DemoParameterizedPolynomial FullSliceRootPosition
fullSliceRootDirectionWitness = True

fullSliceBranchDirectionWitness :: Int -> ParameterizedDirection DemoParameterizedPolynomial FullSliceBranchPosition
fullSliceBranchDirectionWitness seed = Just (odd seed)

trimmedSliceRootDirectionWitness :: ParameterizedDirection DemoParameterizedPolynomial TrimmedSliceRootPosition
trimmedSliceRootDirectionWitness = ()

polynomialPositionWeight :: Exists (Position DemoPolynomial) -> Int
polynomialPositionWeight (Exists witness) =
  case witness of
    RootWitness -> 1
    BranchWitness -> 2

parameterizedPolynomialPositionWeight :: Exists (ParameterizedPosition DemoParameterizedPolynomial) -> Int
parameterizedPolynomialPositionWeight (Exists witness) =
  case witness of
    FullSliceRootWitness -> 3
    FullSliceBranchWitness -> 5
    TrimmedSliceRootWitness -> 7

trimmedSliceRootDirectionWeight :: () -> Int
trimmedSliceRootDirectionWeight () = 1

sampleBatch :: [Int]
sampleBatch = [0 .. 511]

batchWeight :: (Int -> Int) -> [Int] -> Int
batchWeight weight =
  sum . fmap weight

boolWeight :: Bool -> Int
boolWeight value =
  if value then 1 else 0
