module Main
  ( main,
  )
where

import AbstractBench
  ( abstractBenchmarks,
  )
import FiniteLatticeBench
  ( finiteLatticeBenchmarkSuite,
  )
import Moonlight.Pale.Bench.Runner
  ( runBenchmarks,
  )

main :: IO ()
main =
  runBenchmarks
    [ abstractBenchmarks,
      finiteLatticeBenchmarkSuite
    ]
