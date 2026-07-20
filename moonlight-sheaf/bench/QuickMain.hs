module Main
  ( main,
  )
where

import Moonlight.Sheaf.Bench.Operation (operationBenchmarks)
import Moonlight.Sheaf.Bench.StoreDescent (storeDescentBenchmarks)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  defaultMain
    [ operationBenchmarks,
      storeDescentBenchmarks
    ]
