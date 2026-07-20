module Main (main) where

import AutomataBench (automataBenchmarks)
import Moonlight.Pale.Bench.Runner (runBenchmark)

main :: IO ()
main =
  runBenchmark automataBenchmarks
