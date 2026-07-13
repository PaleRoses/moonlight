module Main
  ( main,
  )
where

import FiniteLatticeBench
  ( finiteLatticeBenchmarkSuite,
  )
import Moonlight.Pale.Bench.Runner
  ( runBenchmark,
  )

main :: IO ()
main =
  runBenchmark finiteLatticeBenchmarkSuite
