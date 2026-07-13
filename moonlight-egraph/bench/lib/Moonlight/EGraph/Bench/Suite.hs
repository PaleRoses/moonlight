module Moonlight.EGraph.Bench.Suite
  ( main,
    egraphBenchmarks,
  ) where

import Moonlight.EGraph.Bench.Suite.Context (contextBenchmarks)
import Moonlight.EGraph.Bench.Suite.Core (coreBenchmarks)
import Moonlight.EGraph.Bench.Suite.Exact.ColoredEGraphs (coloredEGraphBenchmarks)
import Moonlight.EGraph.Bench.Suite.Exact.SlottedEGraphs (slottedEGraphBenchmarks)
import Moonlight.EGraph.Bench.Suite.Exact.Thesy (thesyBenchmarks)
import Moonlight.EGraph.Bench.Suite.Exact.VersionedEGraphs (versionedEGraphBenchmarks)
import Moonlight.EGraph.Bench.Suite.PureSaturation (pureSaturationBenchmarks)
import Moonlight.EGraph.Bench.Suite.Relational (relationalBenchmarks)
import Test.Tasty.Bench (Benchmark, bgroup, defaultMain)

main :: IO ()
main =
  defaultMain egraphBenchmarks

egraphBenchmarks :: [Benchmark]
egraphBenchmarks =
  [ coreBenchmarks,
    contextBenchmarks,
    relationalBenchmarks,
    pureSaturationBenchmarks,
    bgroup
      "exact"
      [ coloredEGraphBenchmarks,
        slottedEGraphBenchmarks,
        thesyBenchmarks,
        versionedEGraphBenchmarks
      ]
  ]
