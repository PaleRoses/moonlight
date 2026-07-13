module ConstraintBench
  ( constraintBenchmarks,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Constraint
  ( ConstraintExpr (..),
    dpll,
    normalize,
    toCNF,
  )
import BenchSupport (caseLabel, keys, mediumSizes, smallSizes)
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

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
  bench (caseLabel "clauses" size) (nf dpllWeight size)

normalizeWeight :: Int -> Int
normalizeWeight =
  length . normalize . constraintExpression

cnfWeight :: Int -> Int
cnfWeight =
  sum . fmap Set.size . toCNF . constraintExpression

dpllWeight :: Int -> Int
dpllWeight =
  maybe 0 Map.size . dpll . toCNF . constraintExpression

constraintExpression :: Int -> ConstraintExpr Int
constraintExpression size =
  And (fmap implicationClause (keys size))

implicationClause :: Int -> ConstraintExpr Int
implicationClause key =
  Or [Not (Atom key), Atom (key + 1)]
