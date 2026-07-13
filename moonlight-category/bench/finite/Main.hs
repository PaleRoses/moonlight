module Main
  ( main,
  )
where

import FiniteBench (finiteBenchmarks)
import Moonlight.Pale.Bench.Runner (runBenchmark)

main :: IO ()
main =
  runBenchmark finiteBenchmarks
