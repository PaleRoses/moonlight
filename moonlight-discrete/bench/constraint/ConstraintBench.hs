module ConstraintBench
  ( constraintBenchmarks,
  )
where

import Control.DeepSeq (NFData (rnf))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Constraint
  ( CNF,
    ConstraintExpr (..),
    dpll,
    normalize,
    toCNF,
  )
import BenchSupport (caseLabel, keys, mediumSizes, smallSizes)
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)
import Test.Tasty.HUnit (assertFailure, testCase)

data DPLLBenchmarkFailure
  = DPLLUnexpectedUnsatisfiable
  deriving stock (Show)

instance NFData DPLLBenchmarkFailure where
  rnf DPLLUnexpectedUnsatisfiable = ()

constraintBenchmarks :: Benchmark
constraintBenchmarks =
  bgroup
    "constraint"
    [ bgroup "normalize" (fmap normalizeBenchmark mediumSizes),
      bgroup "cnf" (fmap cnfBenchmark smallSizes),
      bgroup "dpll" (fmap dpllBenchmark smallSizes)
    ]

normalizeBenchmark :: Int -> Benchmark
normalizeBenchmark size =
  bench (caseLabel "clauses" size) (nf normalizeWeight size)

cnfBenchmark :: Int -> Benchmark
cnfBenchmark size =
  bench (caseLabel "clauses" size) (nf cnfWeight size)

dpllBenchmark :: Int -> Benchmark
dpllBenchmark size =
  let label = caseLabel "clauses" size
      fixture = toCNF (constraintExpression size)
   in case dpllWeight fixture of
        Left obstruction ->
          testCase label (assertFailure (show obstruction))
        Right _ ->
          bench label (nf dpllWeight fixture)

normalizeWeight :: Int -> Int
normalizeWeight =
  length . normalize . constraintExpression

cnfWeight :: Int -> Int
cnfWeight =
  sum . fmap Set.size . toCNF . constraintExpression

dpllWeight :: CNF Int -> Either DPLLBenchmarkFailure Int
dpllWeight fixture =
  case dpll fixture of
    Nothing -> Left DPLLUnexpectedUnsatisfiable
    Just assignment -> Right (Map.size assignment)

constraintExpression :: Int -> ConstraintExpr Int
constraintExpression size =
  And (fmap implicationClause (keys size))

implicationClause :: Int -> ConstraintExpr Int
implicationClause key =
  Or [Not (Atom key), Atom (key + 1)]
