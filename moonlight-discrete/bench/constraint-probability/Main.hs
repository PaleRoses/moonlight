module Main (main) where

import ConstraintProbabilityBench (constraintProbabilityBenchmarks)
import Moonlight.Pale.Bench.Runner (runBenchmark)

main :: IO ()
main =
  runBenchmark constraintProbabilityBenchmarks
