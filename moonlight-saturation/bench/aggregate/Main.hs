module Main (main) where

import BenchSupport (runValidatedBenchmarks)
import ContextBench (contextBenchmarks)
import CoreBench (coreBenchmarks)
import ObstructionBench (obstructionBenchmarks)
import ProtocolBench (protocolBenchmarks)

main :: IO ()
main =
  runValidatedBenchmarks
    [ coreBenchmarks,
      protocolBenchmarks,
      contextBenchmarks,
      obstructionBenchmarks
    ]
