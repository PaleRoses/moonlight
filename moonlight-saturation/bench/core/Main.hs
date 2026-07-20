module Main (main) where

import BenchSupport (runValidatedBenchmark)
import CoreBench (coreBenchmarks)

main :: IO ()
main =
  runValidatedBenchmark coreBenchmarks
