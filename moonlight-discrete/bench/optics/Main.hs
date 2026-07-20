module Main (main) where

import Moonlight.Pale.Bench.Runner (runBenchmark)
import OpticsBench (opticsBenchmarks)

main :: IO ()
main =
  runBenchmark opticsBenchmarks
