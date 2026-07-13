module Main
  ( main,
  )
where

import AbstractBench (abstractBenchmarks)
import Moonlight.Pale.Bench.Runner (runBenchmark)

main :: IO ()
main =
  runBenchmark abstractBenchmarks
