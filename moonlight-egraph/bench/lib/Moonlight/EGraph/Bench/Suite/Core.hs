{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Bench.Suite.Core
  ( coreBenchmarks,
  ) where

import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Data.IntSet qualified as IntSet
import Moonlight.Core
  ( ClassId (..),
    Operator (..),
    Substitution,
    emptySubstitution,
    insertSubst,
  )
import Moonlight.Core qualified as EGraph
import Moonlight.EGraph.Bench.Corpus
  ( RingCompiledQuery,
    addXYPattern,
    adjacentPairs,
    buildArithGraph,
    buildRingGraph,
    caseLabel,
    compileRingPatternQuery,
    ringAdd,
    ringNum,
  )
import Moonlight.EGraph.Bench.Harness.Digest (graphDigest)
import Moonlight.EGraph.Bench.Harness.Run (requireRight)
import Moonlight.EGraph.Effect.CoveringSurface (SurfaceKind)
import Moonlight.EGraph.Pure.Change (EGraphMutationResult (..))
import Moonlight.EGraph.Pure.Kernel.HashCons (insertTermTrackedWithClassFootprint)
import Moonlight.EGraph.Pure.Rebuild
  ( merge,
    rebuild,
  )
import Moonlight.EGraph.Pure.Relational
  ( EGraphPreparedMatchState,
    emptyEGraphPreparedMatchState,
    markEGraphPreparedMatchStateDirty,
    wcojPreparedDeltaMatchCompiledWithRoots,
    wcojPreparedMatchCompiledWithRoots,
  )
import Moonlight.EGraph.Pure.Relational.Source (structuralRowsForOperator)
import Moonlight.EGraph.Pure.Types
  ( EGraph,
    eGraphPendingClassUnions,
    eGraphStore,
  )
import Moonlight.EGraph.Test.Arith.Core qualified as Arith
import Moonlight.EGraph.Test.Ring.Core qualified as Ring
import Moonlight.Sheaf.Context.Site (UnitContextSiteOwner)
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
  )

coreBenchmarks :: Benchmark
coreBenchmarks =
  bgroup
    "core"
    [ bgroup "rebuild-wave" rebuildBenches,
      bgroup "prepared-matching-round-over-round" preparedMatchingBenches
    ]

data RebuildFixture = RebuildFixture
  { rbfGraph :: !(EGraph Arith.ArithF Arith.NodeCount)
  }

data PreparedMatchFixture = PreparedMatchFixture
  { pmfQuery :: !RingCompiledQuery,
    pmfGraph :: !(EGraph Ring.RingF Ring.NodeCount),
    pmfState :: !(EGraphPreparedMatchState UnitContextSiteOwner SurfaceKind Ring.RingF)
  }

instance NFData RebuildFixture where
  rnf fixture =
    graphDigest (rbfGraph fixture) `seq` ()

instance NFData PreparedMatchFixture where
  rnf fixture =
    graphDigest (pmfGraph fixture) `seq` ()

rebuildBenches :: [Benchmark]
rebuildBenches =
  rebuildBench
    <$> fmap
      (\mergeCount -> (4000, mergeCount))
      [1, 40, 400]

rebuildBench :: (Int, Int) -> Benchmark
rebuildBench (termCount, mergeCount) =
  env (prepareRebuildFixture termCount mergeCount) $ \fixture ->
    bench
      (caseLabel [("N", termCount), ("M", mergeCount)])
      (nf rebuildDigest fixture)

preparedMatchingBenches :: [Benchmark]
preparedMatchingBenches =
  preparedMatchingBench
    <$> [ ("full-rematch", preparedFullRematchDigest),
          ("delta-rematch", preparedDeltaRematchDigest),
          ("store-scan-floor", preparedStoreScanFloorDigest)
        ]
    <*> [1000, 4000]

preparedMatchingBench :: (String, PreparedMatchFixture -> Int) -> Int -> Benchmark
preparedMatchingBench (label, digest) termCount =
  env (preparePreparedMatchFixture termCount) $ \fixture ->
    bench
      (label <> "/" <> caseLabel [("N", termCount)])
      (nf digest fixture)


prepareRebuildFixture :: Int -> Int -> IO RebuildFixture
prepareRebuildFixture termCount mergeCount = do
  (baseGraph, classIds) <- requireRight "arith graph allocation" (buildArithGraph termCount)
  let graphWithPendingUnions =
        foldl'
          (\graph (leftClass, rightClass) -> merge leftClass rightClass graph)
          baseGraph
          (take mergeCount (adjacentPairs classIds))
      fixture =
        RebuildFixture graphWithPendingUnions
  evaluate (length (eGraphPendingClassUnions graphWithPendingUnions) + graphDigest graphWithPendingUnions) *> pure fixture

rebuildDigest :: RebuildFixture -> Int
rebuildDigest =
  graphDigest . rebuild . rbfGraph

preparePreparedMatchFixture :: Int -> IO PreparedMatchFixture
preparePreparedMatchFixture termCount = do
  compiledQuery <- requireRight "ring pattern compilation" (compileRingPatternQuery addXYPattern)
  (graph, _classIds) <- requireRight "ring graph allocation" (buildRingGraph termCount)
  (stateAfterFirstMatch, firstMatches) <-
    requireRight
      "initial prepared match"
      (wcojPreparedMatchCompiledWithRoots compiledQuery graph emptyEGraphPreparedMatchState)
  EGraphMutationResult
    { emrResult = (_insertedClass, dirtyKeys),
      emrGraph = mutatedGraph
    } <-
    requireRight "prepared match mutation allocation" $
      insertTermTrackedWithClassFootprint
        (ringAdd (ringNum termCount) (ringNum (termCount + 1)))
        graph
  let dirtyState =
        markEGraphPreparedMatchStateDirty dirtyKeys stateAfterFirstMatch
      fixture =
        PreparedMatchFixture
          { pmfQuery = compiledQuery,
            pmfGraph = mutatedGraph,
            pmfState = dirtyState
          }
  evaluate (length firstMatches + graphDigest mutatedGraph + IntSet.size dirtyKeys + preparedFullRematchDigest fixture) *> pure fixture

preparedFullRematchDigest :: PreparedMatchFixture -> Int
preparedFullRematchDigest fixture =
  either
    (const (-1))
    (length . snd)
    ( wcojPreparedMatchCompiledWithRoots
        (pmfQuery fixture)
        (pmfGraph fixture)
        (pmfState fixture)
    )

preparedDeltaRematchDigest :: PreparedMatchFixture -> Int
preparedDeltaRematchDigest fixture =
  either
    (const (-1))
    (length . snd)
    ( wcojPreparedDeltaMatchCompiledWithRoots
        (pmfQuery fixture)
        (pmfGraph fixture)
        (pmfState fixture)
    )

preparedStoreScanFloorDigest :: PreparedMatchFixture -> Int
preparedStoreScanFloorDigest =
  length . preparedStoreScanFloorMatches . pmfGraph

preparedStoreScanFloorMatches :: EGraph Ring.RingF Ring.NodeCount -> [(ClassId, Substitution)]
preparedStoreScanFloorMatches graph =
  fmap structuralAddRowMatch (structuralRowsForOperator (eGraphStore graph) (Operator (Ring.Add () ())))

structuralAddRowMatch :: (Int, [Int]) -> (ClassId, Substitution)
structuralAddRowMatch (resultKey, childKeys) =
  ( ClassId resultKey,
    foldl'
      (\substitution (patternKey, childKey) -> insertSubst (EGraph.mkPatternVar patternKey) (ClassId childKey) substitution)
      emptySubstitution
      (zip [0 ..] childKeys)
  )
