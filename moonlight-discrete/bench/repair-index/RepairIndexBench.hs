module RepairIndexBench
  ( repairIndexBenchmarks,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import BenchSupport (caseLabel, keys, largeSizes)
import Moonlight.Repair.Index
  ( RepairIndex (..),
    repairDirtyResultClosure,
    repairSupport,
    repairTouchedTupleCount,
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

repairIndexBenchmarks :: Benchmark
repairIndexBenchmarks =
  bgroup
    "repair-index"
    [ bgroup "support" (fmap supportBenchmark largeSizes),
      bgroup "dirty-result-closure" (fmap dirtyClosureBenchmark largeSizes),
      bgroup "touched-tuples" (fmap touchedTuplesBenchmark largeSizes)
    ]

supportBenchmark :: Int -> Benchmark
supportBenchmark size =
  bench (caseLabel "chain" size) (nf supportSize size)

dirtyClosureBenchmark :: Int -> Benchmark
dirtyClosureBenchmark size =
  bench (caseLabel "chain" size) (nf dirtyClosureSize size)

touchedTuplesBenchmark :: Int -> Benchmark
touchedTuplesBenchmark size =
  bench (caseLabel "chain" size) (nf touchedTupleCount size)

supportSize :: Int -> Int
supportSize size =
  IntSet.size (repairSupport (repairIndex size) (seedSet size))

dirtyClosureSize :: Int -> Int
dirtyClosureSize size =
  IntSet.size (repairDirtyResultClosure (repairIndex size) (seedSet size))

touchedTupleCount :: Int -> Int
touchedTupleCount size =
  repairTouchedTupleCount (repairIndex size) (seedSet size)

repairIndex :: Int -> RepairIndex Int
repairIndex size =
  RepairIndex
    { riParents = parentMap size,
      riChildren = childMap size,
      riTuplesByResult = tupleMap size
    }

parentMap :: Int -> IntMap IntSet
parentMap size =
  IntMap.fromAscList (fmap parentEntry (keys size))

parentEntry :: Int -> (Int, IntSet)
parentEntry key =
  (key, IntSet.fromAscList (take 2 (keys key)))

childMap :: Int -> IntMap (IntMap Int)
childMap size =
  IntMap.fromAscList (fmap childEntry (keys size))

childEntry :: Int -> (Int, IntMap Int)
childEntry key =
  (key, IntMap.fromAscList [(key + 1, 1), (key + 2, 1)])

tupleMap :: Int -> IntMap [Int]
tupleMap size =
  IntMap.fromAscList (fmap tupleEntry (keys size))

tupleEntry :: Int -> (Int, [Int])
tupleEntry key =
  (key, [key, key + 1])

seedSet :: Int -> IntSet
seedSet size =
  IntSet.fromAscList [0, max 0 (size `div` 2), max 0 (size - 1)]
