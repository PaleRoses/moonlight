module Main (main) where

import Moonlight.Pale.Bench.Runner (runBenchmark)
import RepairIndexBench (repairIndexBenchmarks)

main :: IO ()
main =
  runBenchmark repairIndexBenchmarks
