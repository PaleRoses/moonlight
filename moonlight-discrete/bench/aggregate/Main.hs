module Main (main) where

import AutomataBench (automataBenchmarks)
import ConstraintBench (constraintBenchmarks)
import ConstraintProbabilityBench (constraintProbabilityBenchmarks)
import GraphBench (graphBenchmarks)
import Moonlight.Pale.Bench.Runner (runBenchmarks)
import OpticsBench (opticsBenchmarks)

main :: IO ()
main =
  runBenchmarks
    [ automataBenchmarks,
      constraintBenchmarks,
      constraintProbabilityBenchmarks,
      graphBenchmarks,
      opticsBenchmarks
    ]
