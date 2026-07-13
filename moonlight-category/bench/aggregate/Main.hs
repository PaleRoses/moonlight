module Main
  ( main,
  )
where

import AbstractBench (abstractBenchmarks)
import FiniteBench (finiteBenchmarks)
import IndexedBench (indexedBenchmarks)
import Moonlight.Pale.Bench.Runner (runBenchmarks)
import SimplicialBench (simplicialBenchmarks)
import SiteBench (siteBenchmarks)

main :: IO ()
main =
  runBenchmarks
    [ abstractBenchmarks,
      finiteBenchmarks,
      siteBenchmarks,
      indexedBenchmarks,
      simplicialBenchmarks
    ]
