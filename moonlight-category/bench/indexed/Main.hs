module Main
  ( main,
  )
where

import IndexedBench (indexedBenchmarks)
import Moonlight.Pale.Bench.Runner (runBenchmark)

main :: IO ()
main =
  runBenchmark indexedBenchmarks
