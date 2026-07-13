module Main
  ( main,
  )
where

import Allocation
  ( allocationReportMain,
  )
import Groups
  ( arrangementJoinBenchmarks,
    batchTraceBenchmarks,
    collectionEdslComparisonBenchmarks,
    decomposedDbspDdBenchmarks,
    operatorBenchmarks,
    projectionBenchmarks,
    reverseRowProjectionIndexBenchmarks,
    rowIndexBenchmarks,
    rowsCacheBenchmarks,
    runtimeSettleBenchmarks,
    storageKernelBenchmarks,
    streamCalculusBenchmarks,
    traceCompactionBenchmarks,
    traceReadDescriptionBenchmarks,
    wcojBenchmarks,
  )
import System.Environment
  ( getArgs,
  )
import Test.Tasty.Bench
  ( defaultMain,
  )

main :: IO ()
main = do
  args <- getArgs
  case args of
    "--allocation-report" : _ ->
      allocationReportMain
    _ ->
      defaultMain
        [ batchTraceBenchmarks,
          storageKernelBenchmarks,
          decomposedDbspDdBenchmarks,
          streamCalculusBenchmarks,
          operatorBenchmarks,
          collectionEdslComparisonBenchmarks,
          arrangementJoinBenchmarks,
          projectionBenchmarks,
          traceCompactionBenchmarks,
          traceReadDescriptionBenchmarks,
          rowIndexBenchmarks,
          reverseRowProjectionIndexBenchmarks,
          rowsCacheBenchmarks,
          runtimeSettleBenchmarks,
          wcojBenchmarks
        ]
