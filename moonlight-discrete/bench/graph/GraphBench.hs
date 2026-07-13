module GraphBench
  ( graphBenchmarks,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import BenchSupport (caseLabel, keys, largeSizes)
import Moonlight.Graph
  ( buildLocalAdjFromIntMaps,
    closedStarAdjInt,
    countLocalEdgesInt,
    cyclicCellsFromChildrenInt,
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

graphBenchmarks :: Benchmark
graphBenchmarks =
  bgroup
    "graph"
    [ bgroup "cyclic-cells" (fmap cyclicCellsBenchmark largeSizes),
      bgroup "closed-star-edge-count" (fmap closedStarEdgeBenchmark largeSizes)
    ]

cyclicCellsBenchmark :: Int -> Benchmark
cyclicCellsBenchmark size =
  bench (caseLabel "cycle" size) (nf cyclicCellCount size)

closedStarEdgeBenchmark :: Int -> Benchmark
closedStarEdgeBenchmark size =
  bench (caseLabel "cycle" size) (nf closedStarEdgeCount size)

cyclicCellCount :: Int -> Int
cyclicCellCount =
  IntSet.size . cyclicCellsFromChildrenInt . cycleChildren

closedStarEdgeCount :: Int -> Int
closedStarEdgeCount size =
  let adjacency = buildLocalAdjFromIntMaps (cycleParents size) (cycleChildren size)
   in countLocalEdgesInt adjacency (closedStarAdjInt adjacency 0)

cycleChildren :: Int -> IntMap (IntMap Int)
cycleChildren size =
  IntMap.fromAscList (fmap (childEntry size) (keys size))

childEntry :: Int -> Int -> (Int, IntMap Int)
childEntry size key =
  (key, IntMap.singleton ((key + 1) `mod` size) 1)

cycleParents :: Int -> IntMap IntSet.IntSet
cycleParents size =
  IntMap.fromAscList (fmap (parentEntry size) (keys size))

parentEntry :: Int -> Int -> (Int, IntSet.IntSet)
parentEntry size key =
  ((key + 1) `mod` size, IntSet.singleton key)
