module Main
  ( main,
  )
where

import Moonlight.Pale.Bench.Runner (runBenchmark)
import SiteBench (siteBenchmarks)

main :: IO ()
main =
  runBenchmark siteBenchmarks
