module Moonlight.Pale.Bench.Runner
  ( runBenchmark,
    runBenchmarks,
  )
where

import Test.Tasty.Bench (Benchmark, defaultMain)

runBenchmark :: Benchmark -> IO ()
runBenchmark =
  runBenchmarks . pure

runBenchmarks :: [Benchmark] -> IO ()
runBenchmarks =
  defaultMain
