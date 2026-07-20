{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Bench.Suite.Exact.ColoredEGraphs
  ( coloredEGraphBenchmarks,
  ) where

import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( ClassId,
    Language,
    classIdKey,
  )
import Moonlight.EGraph.Bench.Corpus
  ( allBenchmarkAnatomyRegions,
    anatomyPropagationTargets,
    buildArithGraph,
    caseLabel,
    nonOverlappingPairs,
    requireFirstPair,
  )
import Moonlight.EGraph.Bench.Harness.Digest
  ( contextGraphDigest,
    graphDigest,
  )
import Moonlight.EGraph.Bench.Harness.Run (requireRight)
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    contextMerge,
    emptyContextEGraphFromSite,
  )
import Moonlight.EGraph.Pure.Rebuild
  ( merge,
    rebuild,
  )
import Moonlight.EGraph.Pure.Types (EGraph)
import Moonlight.EGraph.Test.Arith.Core qualified as Arith
import Moonlight.EGraph.Test.Context.Anatomy
  ( AnatomyRegion (..),
    coarseAnatomyLattice,
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    withPreparedContextSiteFromFiniteLattice,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
  )

coloredEGraphBenchmarks :: Benchmark
coloredEGraphBenchmarks =
  bgroup "colored-egraphs" coloredEGraphBaselineBenches

newtype MaterializedColoredEGraph f analysis context = MaterializedColoredEGraph (Map.Map context (EGraph f analysis))

data ColoredWriteFixture owner = ColoredWriteFixture
  { cwfContextGraph :: !(ContextEGraph owner Arith.ArithF Arith.NodeCount AnatomyRegion),
    cwfColoredGraph :: !(MaterializedColoredEGraph Arith.ArithF Arith.NodeCount AnatomyRegion),
    cwfAuthoringContext :: !AnatomyRegion,
    cwfTargetContexts :: ![AnatomyRegion],
    cwfMergePair :: !(ClassId, ClassId)
  }

instance NFData (ColoredWriteFixture owner) where
  rnf fixture =
    contextGraphDigest (cwfContextGraph fixture)
      `seq` materializedColoredGraphDigest (cwfColoredGraph fixture)
      `seq` length (cwfTargetContexts fixture)
      `seq` classIdKey (fst (cwfMergePair fixture))
      `seq` classIdKey (snd (cwfMergePair fixture))
      `seq` ()

coloredEGraphBaselineBenches :: [Benchmark]
coloredEGraphBaselineBenches =
  [ bgroup
      "write-amplification"
      (coloredWriteBench <$> ((,) <$> [Local, ArmLeft, Upper] <*> coloredBaselineTermCounts))
  ]

coloredBaselineTermCounts :: [Int]
coloredBaselineTermCounts =
  [1000, 4000, 100000]

coloredWriteBench :: (AnatomyRegion, Int) -> Benchmark
coloredWriteBench (authoringContext, termCount) =
  withPreparedContextSiteFromFiniteLattice coarseAnatomyLattice $ \site ->
    env (prepareColoredWriteFixture site authoringContext termCount) $ \fixture ->
      bgroup
        (show authoringContext <> "/" <> caseLabel [("N", termCount)])
        [ bench "our-context-egraph" (nf coloredWriteOurDigest fixture),
          bench "materialized-colored-egraph" (nf coloredWriteMaterializedDigest fixture)
        ]

prepareColoredWriteFixture ::
  PreparedContextSite owner AnatomyRegion ->
  AnatomyRegion ->
  Int ->
  IO (ColoredWriteFixture owner)
prepareColoredWriteFixture site authoringContext termCount = do
  (baseGraph, classIds) <- requireRight "colored write graph allocation" (buildArithGraph termCount)
  let targetContexts =
        anatomyPropagationTargets authoringContext
  mergePair <- requireFirstPair "colored write" (nonOverlappingPairs classIds)
  let fixture =
        ColoredWriteFixture
          { cwfContextGraph = emptyContextEGraphFromSite site baseGraph,
            cwfColoredGraph = materializedColoredEGraph allBenchmarkAnatomyRegions baseGraph,
            cwfAuthoringContext = authoringContext,
            cwfTargetContexts = targetContexts,
            cwfMergePair = mergePair
          }
  putStrLn
    ( "colored-egraph-baseline/write "
        <> show authoringContext
        <> " N="
        <> show termCount
        <> ": target colors "
        <> show targetContexts
        <> ", our digest "
        <> show (coloredWriteOurDigest fixture)
        <> ", materialized colored digest "
        <> show (coloredWriteMaterializedDigest fixture)
    )
  evaluate (coloredWriteOurDigest fixture + coloredWriteMaterializedDigest fixture)
    *> pure fixture

coloredWriteOurDigest :: ColoredWriteFixture owner -> Int
coloredWriteOurDigest fixture =
  either
    (const (-1))
    contextGraphDigest
    ( contextMerge
        (cwfAuthoringContext fixture)
        (fst (cwfMergePair fixture))
        (snd (cwfMergePair fixture))
        (cwfContextGraph fixture)
    )

coloredWriteMaterializedDigest :: ColoredWriteFixture owner -> Int
coloredWriteMaterializedDigest fixture =
  materializedColoredGraphDigest
    ( materializedColoredMerge
        (cwfTargetContexts fixture)
        (fst (cwfMergePair fixture))
        (snd (cwfMergePair fixture))
        (cwfColoredGraph fixture)
    )

materializedColoredEGraph :: Ord context => [context] -> EGraph f analysis -> MaterializedColoredEGraph f analysis context
materializedColoredEGraph contexts baseGraph =
  MaterializedColoredEGraph (Map.fromList ((\contextValue -> (contextValue, baseGraph)) <$> contexts))

materializedColoredMerge ::
  (Language f, Ord context) =>
  [context] ->
  ClassId ->
  ClassId ->
  MaterializedColoredEGraph f analysis context ->
  MaterializedColoredEGraph f analysis context
materializedColoredMerge targetContexts leftClass rightClass (MaterializedColoredEGraph graphs) =
  MaterializedColoredEGraph
    ( foldl'
        (\currentGraphs contextValue -> Map.adjust (rebuild . merge leftClass rightClass) contextValue currentGraphs)
        graphs
        targetContexts
    )

materializedColoredGraphDigest :: MaterializedColoredEGraph f analysis context -> Int
materializedColoredGraphDigest =
  materializedColoredFold (\total coloredGraph -> total + graphDigest coloredGraph) 0

materializedColoredFold ::
  (value -> EGraph f analysis -> value) ->
  value ->
  MaterializedColoredEGraph f analysis context ->
  value
materializedColoredFold combine initialValue (MaterializedColoredEGraph graphs) =
  Map.foldl' combine initialValue graphs
