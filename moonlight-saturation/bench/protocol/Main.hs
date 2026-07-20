module Main (main) where

import BenchSupport (runValidatedBenchmark)
import ProtocolBench (protocolBenchmarks)

main :: IO ()
main =
  runValidatedBenchmark protocolBenchmarks
