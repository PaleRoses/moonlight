module Main (main) where

import ConstraintBench (constraintBenchmarks)
import Moonlight.Pale.Bench.Runner (runBenchmark)

main :: IO ()
main =
  runBenchmark constraintBenchmarks
