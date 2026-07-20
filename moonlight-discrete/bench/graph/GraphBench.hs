module GraphBench
  ( graphBenchmarks,
  )
where

import Control.DeepSeq (NFData (rnf))
import Data.Bifunctor (first)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import BenchSupport (caseLabel, keys, largeSizes)
import Moonlight.Graph
  ( ChildMulti,
    LocalAdj,
    LocalTopologyError (..),
    buildLocalAdjFromChildren,
    closedStarAdj,
    countLocalEdges,
    cyclicCellsFromChildren,
    cyclicCellsFromAdjacency,
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)
import Test.Tasty.HUnit (assertFailure, testCase)

data GraphBenchmarkFailure
  = GraphBenchmarkInvalidTopology !LocalTopologyError
  deriving stock (Show)

instance NFData GraphBenchmarkFailure where
  rnf (GraphBenchmarkInvalidTopology obstruction) =
    case obstruction of
      NonPositiveChildMultiplicity parentCell childCell multiplicity ->
        rnf parentCell `seq` rnf childCell `seq` rnf multiplicity

graphBenchmarks :: Benchmark
graphBenchmarks =
  bgroup
    "graph"
    [ bgroup "cyclic-cells" (fmap cyclicCellsEndToEndBenchmark largeSizes),
      bgroup "cyclic-cells-kernel" (fmap cyclicCellsKernelBenchmark largeSizes),
      bgroup "closed-star-edge-count" (fmap closedStarEdgeEndToEndBenchmark largeSizes),
      bgroup "closed-star-edge-count-kernel" (fmap closedStarEdgeKernelBenchmark largeSizes)
    ]

cyclicCellsEndToEndBenchmark :: Int -> Benchmark
cyclicCellsEndToEndBenchmark =
  endToEndTopologyBenchmark cyclicCellCount

cyclicCellsKernelBenchmark :: Int -> Benchmark
cyclicCellsKernelBenchmark size =
  topologyBenchmark
    (caseLabel "cycle" size)
    (IntSet.size . cyclicCellsFromAdjacency)
    (cycleChildren size)

closedStarEdgeEndToEndBenchmark :: Int -> Benchmark
closedStarEdgeEndToEndBenchmark =
  endToEndTopologyBenchmark closedStarEdgeCountFromSize

closedStarEdgeKernelBenchmark :: Int -> Benchmark
closedStarEdgeKernelBenchmark size =
  topologyBenchmark
    (caseLabel "cycle" size)
    closedStarEdgeCount
    (cycleChildren size)

endToEndTopologyBenchmark ::
  (Int -> Either GraphBenchmarkFailure Int) ->
  Int ->
  Benchmark
endToEndTopologyBenchmark measure size =
  let label = caseLabel "cycle" size
   in case measure size of
        Left obstruction ->
          testCase label (assertFailure (show obstruction))
        Right _ ->
          bench label (nf measure size)

topologyBenchmark ::
  String ->
  (IntMap LocalAdj -> Int) ->
  ChildMulti ->
  Benchmark
topologyBenchmark label measure children =
  case buildLocalAdjFromChildren children of
    Left obstruction ->
      testCase label (assertFailure ("invalid graph benchmark fixture: " <> show obstruction))
    Right adjacency ->
      bench label (nf measure adjacency)

closedStarEdgeCount :: IntMap LocalAdj -> Int
closedStarEdgeCount adjacency =
  countLocalEdges adjacency (closedStarAdj adjacency 0)

cyclicCellCount :: Int -> Either GraphBenchmarkFailure Int
cyclicCellCount =
  first GraphBenchmarkInvalidTopology
    . fmap IntSet.size
    . cyclicCellsFromChildren
    . cycleChildren

closedStarEdgeCountFromSize :: Int -> Either GraphBenchmarkFailure Int
closedStarEdgeCountFromSize =
  first GraphBenchmarkInvalidTopology
    . fmap closedStarEdgeCount
    . buildLocalAdjFromChildren
    . cycleChildren

cycleChildren :: Int -> ChildMulti
cycleChildren size =
  IntMap.fromAscList (fmap (childEntry size) (keys size))

childEntry :: Int -> Int -> (Int, IntMap Int)
childEntry size key =
  (key, IntMap.singleton ((key + 1) `mod` size) 1)
