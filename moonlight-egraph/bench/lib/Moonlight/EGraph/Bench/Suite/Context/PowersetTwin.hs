-- | Powerset twin K-sweep benchmark for paper evidence.
module Moonlight.EGraph.Bench.Suite.Context.PowersetTwin
  ( powersetTwinBenchmarks,
  ) where

import Control.Exception (evaluate)
import Data.Bifunctor (first)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Moonlight.EGraph.Pure.Context.Core
  ( checkedContextRestrictionMismatchesAt,
    contextCachedObjectsForExecution,
    contextAuthoredUnionPairs,
    contextPreparedObjects,
    contextVisibleClassKeys,
  )
import Moonlight.EGraph.Pure.Types (ClassId)
import Moonlight.EGraph.Test.Context.Powerset
  ( PowersetContext,
    PowersetTwinGraph,
    PowersetTwinObstruction,
    PowersetTwinWorkload (..),
    powersetTwinWorkload,
  )
import Moonlight.Sheaf.Context.Algebra
  ( contextEquivalentAt,
    propagationTargets,
    restrictionMap,
  )
import Moonlight.Sheaf.Context.Witness (contextRestrictionIdentity)
import Moonlight.Sheaf.Descent.Context
  ( DescentReport (..),
    fullDescentCheck,
  )
import Moonlight.Sheaf.Obstruction (obstructionReport)
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    nfIO,
  )

powersetTwinBenchmarks :: Benchmark
powersetTwinBenchmarks =
  bgroup "powerset-twin" (fmap powersetTwinBench powersetTwinSizes)

powersetTwinSizes :: [Int]
powersetTwinSizes =
  [3 .. 7]

powersetTwinBench :: Int -> Benchmark
powersetTwinBench atomCount =
  bench
    ("n=" <> show atomCount)
    (nfIO (powersetTwinDigestIO atomCount))

powersetTwinDigestIO :: Int -> IO Int
powersetTwinDigestIO atomCount =
  either
    (ioError . userError . ("invalid powerset twin benchmark fixture: " <>) . show)
    ( either
        (ioError . userError . ("powerset twin semantic obstruction: " <>))
        evaluate
        . powersetTwinDigest
    )
    (powersetTwinWorkload (powersetBenchmarkAtoms atomCount))

powersetBenchmarkAtoms :: Int -> [Char]
powersetBenchmarkAtoms atomCount =
  take atomCount ['a' .. 'z']

powersetTwinDigest :: PowersetTwinWorkload -> Either String Int
powersetTwinDigest workload =
  sum
    <$> traverse
      (graphDigest classA classB contexts pairs)
      [ptwDenseGraph workload, ptwSymbolicGraph workload]
  where
    classA =
      ptwClassA workload
    classB =
      ptwClassB workload
    contexts =
      ptwProbeContexts workload
    pairs =
      ptwProbePairs workload

graphDigest ::
  ClassId ->
  ClassId ->
  [PowersetContext] ->
  [(PowersetContext, PowersetContext)] ->
  PowersetTwinGraph ->
  Either String Int
graphDigest classA classB contexts pairs graph = do
  contextDigests <- traverse (contextDigest classA classB graph) contexts
  restrictionDigests <- traverse (restrictionDigest graph) pairs
  pure
    ( sum
        [ length (contextPreparedObjects graph),
          length (contextCachedObjectsForExecution graph),
          sum (fmap (length . (`contextAuthoredUnionPairs` graph)) contexts),
          sum contextDigests,
          sum restrictionDigests,
          descentReportDigest (fullDescentCheck graph)
        ]
    )

contextDigest ::
  ClassId ->
  ClassId ->
  PowersetTwinGraph ->
  PowersetContext ->
  Either String Int
contextDigest classA classB graph contextValue = do
  visibleClassKeys <- first show (contextVisibleClassKeys contextValue graph)
  equivalent <- first show (contextEquivalentAt contextValue classA classB graph)
  targets <- first show (propagationTargets contextValue classA classB graph)
  restrictionIdentity <- first show (contextRestrictionIdentity contextValue graph)
  restrictionMismatches <- first show (checkedContextRestrictionMismatchesAt contextValue graph)
  pure
    ( sum
        [ IntSet.size visibleClassKeys,
          boolDigest equivalent,
          length targets,
          boolDigest restrictionIdentity,
          length restrictionMismatches,
          length (obstructionReport classA classB contextValue graph :: [PowersetTwinObstruction])
        ]
    )

restrictionDigest ::
  PowersetTwinGraph ->
  (PowersetContext, PowersetContext) ->
  Either String Int
restrictionDigest graph (sourceContext, targetContext) =
  first show (IntMap.size <$> restrictionMap sourceContext targetContext graph)

descentReportDigest :: DescentReport ctx refusal obstruction -> Int
descentReportDigest report =
  sum
    [ drContextCount report,
      drObstructionCount report,
      boolDigest (drSatisfied report)
    ]

boolDigest :: Bool -> Int
boolDigest =
  fromEnum
