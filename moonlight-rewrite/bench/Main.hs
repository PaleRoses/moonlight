module Main
  ( main,
  )
where

import Algebra (algebraBenchmarkPreflight, algebraBenchmarks)
import Proof (proofBenchmarkPreflight, proofBenchmarks)
import Public (publicBenchmarkPreflight, publicBenchmarks)
import Runtime (runtimeBenchmarkPreflight, runtimeBenchmarks)
import System (systemBenchmarkPreflight, systemBenchmarks)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main = do
  sequence_
    [ algebraBenchmarkPreflight,
      publicBenchmarkPreflight,
      proofBenchmarkPreflight,
      runtimeBenchmarkPreflight,
      systemBenchmarkPreflight
    ]
  defaultMain
    [ algebraBenchmarks,
      publicBenchmarks,
      proofBenchmarks,
      runtimeBenchmarks,
      systemBenchmarks
    ]
