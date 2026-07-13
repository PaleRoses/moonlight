module Main
  ( main,
  )
where

import Moonlight.Pale.Bench.Runner (runBenchmark)
import SimplicialBench (simplicialBenchmarks)

main :: IO ()
main =
  runBenchmark simplicialBenchmarks
