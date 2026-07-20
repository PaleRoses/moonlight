module Main (main) where

import GraphBench (graphBenchmarks)
import Moonlight.Pale.Bench.Runner (runBenchmark)

main :: IO ()
main =
  runBenchmark graphBenchmarks
