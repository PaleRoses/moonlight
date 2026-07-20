module Main (main) where

import BenchSupport (runValidatedBenchmark)
import ObstructionBench (obstructionBenchmarks)

main :: IO ()
main =
  runValidatedBenchmark obstructionBenchmarks
