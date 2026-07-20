module Main (main) where

import BenchSupport (runValidatedBenchmark)
import ContextBench (contextBenchmarks)

main :: IO ()
main =
  runValidatedBenchmark contextBenchmarks
