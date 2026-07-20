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

module Moonlight.EGraph.Bench.Suite.Context
  ( contextBenchmarks,
  ) where

import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Numeric.Natural (Natural)
import Moonlight.Core
  ( ClassId (..),
    classIdKey,
  )
import Moonlight.EGraph.Bench.Corpus
  ( ArithCompiledQuery,
    activateContextMerges,
    arithAddXYPattern,
    buildArithGraph,
    caseLabel,
    compileArithPatternQuery,
    requireFirstPair,
    requireMergePairs,
  )
import Moonlight.EGraph.Bench.Harness.Digest
  ( contextGraphDigest,
  )
import Moonlight.EGraph.Bench.Harness.Run (requireRight)
import Moonlight.EGraph.Bench.Scale.Chimera qualified as ChimeraScale
import Moonlight.EGraph.Bench.Suite.Context.CaseLiftCore (caseLiftCoreBenchmarks)
import Moonlight.EGraph.Bench.Suite.Context.PowersetTwin (powersetTwinBenchmarks)
import Moonlight.EGraph.Bench.Suite.Context.SheafStress (sheafStressBenchmarks)
import Moonlight.EGraph.Effect.CoveringSurface (SurfaceKind)
import Moonlight.EGraph.Pure.Context
  ( ContextDeltaError,
    ContextEGraph,
    beginContextRebaseBatch,
    commitContextRebaseBatch,
    contextMerge,
    contextPreparedObjects,
    emptyContextEGraphFromSite,
    globalMerge,
    planContextMerges,
    stageContextMerges,
  )
import Moonlight.EGraph.Pure.Context.AnnotatedDelta
  ( AnnotatedDeltaBuckets,
    AnnotatedDeltaFrontier,
    contextAnnotatedDeltaBuckets,
    contextAnnotatedDeltaDirtyFrontier,
  )
import Moonlight.EGraph.Pure.Context
  ( cegBase,
    cegContextAnalysisDeltas,
    cegContextFibers,
    cegContextRevision,
    cegSite,
  )
import Moonlight.EGraph.Pure.Relational
  ( EGraphPreparedMatchState,
    emptyEGraphPreparedMatchState,
    markEGraphPreparedMatchStateAnnotatedDirty,
    refreshEGraphPreparedMatchStateAnnotatedRevisions,
    wcojPreparedAnnotatedContextDeltaMatchCompiledWithRoots,
    wcojPreparedMatchCompiledWithRoots,
  )
import Moonlight.EGraph.Pure.Saturation.Matching (annotatedDeltaFrontierKeys)
import Moonlight.EGraph.Pure.Types (EGraph)
import Moonlight.EGraph.Test.Arith.Core qualified as Arith
import Moonlight.EGraph.Test.Context.Anatomy
  ( AnatomyRegion (..),
    coarseAnatomyLattice,
  )
import Moonlight.Sheaf.Context.Region
  ( ContextRegion,
    RegionTable,
    fromGeneratorKeys,
    regionCubeCount,
    regionGeneratorKeys,
    regionJoin,
    regionMeet,
    regionSize,
  )
import Moonlight.Sheaf.Context.Site
  ( ContextObjectKey,
    PowersetSitePreparationError,
    PreparedContextSite,
    contextObjectKeyFor,
    contextObjectKeyValue,
    preparedRegionTable,
    withPreparedContextSiteFromFiniteLattice,
    withPreparedContextSiteFromPowersetAtoms,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
  )

contextBenchmarks :: Benchmark
contextBenchmarks =
  bgroup
    "context"
    ( ChimeraScale.chimeraContextKernelBenchmarks
        <> [ bgroup "merge-rebase" contextMergeBenches,
             bgroup "write-amplification" contextWriteAmplificationBenches,
             bgroup "symbolic-regions-exponential-receipt" symbolicRegionReceiptBenches,
             powersetTwinBenchmarks,
             caseLiftCoreBenchmarks,
             sheafStressBenchmarks
           ]
    )

data ContextMergeFixture owner = ContextMergeFixture
  { cmfContextGraph :: !(ContextEGraph owner Arith.ArithF Arith.NodeCount AnatomyRegion),
    cmfLeftClass :: !ClassId,
    cmfRightClass :: !ClassId
  }

data WriteAmplificationFixture owner = WriteAmplificationFixture
  { wafContextGraph :: !(ContextEGraph owner Arith.ArithF Arith.NodeCount AnatomyRegion),
    wafLeftClass :: !ClassId,
    wafRightClass :: !ClassId
  }

instance NFData (ContextMergeFixture owner) where
  rnf fixture =
    contextMergeFixtureDigest fixture `seq` ()

instance NFData (WriteAmplificationFixture owner) where
  rnf fixture =
    contextGraphDigest (wafContextGraph fixture)
      `seq` classIdKey (wafLeftClass fixture)
      `seq` classIdKey (wafRightClass fixture)
      `seq` ()

contextMergeBenches :: [Benchmark]
contextMergeBenches =
  contextMergeBench
    <$> [ (1, 1000),
          (1, 4000),
          (8, 1000),
          (8, 4000)
        ]

contextMergeBench :: (Int, Int) -> Benchmark
contextMergeBench (contextCount, termCount) =
  withPreparedContextSiteFromFiniteLattice coarseAnatomyLattice $ \contextSite ->
    env (prepareContextMergeFixture contextSite contextCount termCount) $ \fixture ->
      bench
        (caseLabel [("K", contextCount), ("N", termCount)])
        (nf contextGlobalMergeDigest fixture)

contextWriteAmplificationBenches :: [Benchmark]
contextWriteAmplificationBenches =
  [ contextWriteAmplificationBench region termCount
    | region <- [Upper, ArmLeft, Local],
      termCount <- [1000, 4000]
  ]

contextWriteAmplificationBench :: AnatomyRegion -> Int -> Benchmark
contextWriteAmplificationBench region termCount =
  withPreparedContextSiteFromFiniteLattice coarseAnatomyLattice $ \contextSite ->
    env (prepareWriteAmplificationFixture contextSite region termCount) $ \fixture ->
      bgroup
        (show region <> "/" <> caseLabel [("N", termCount)])
        [ bench "commit" (nf (contextWriteAmplificationCommitDigest region) fixture),
          bench "commit+first-read" (nf (contextWriteAmplificationDigest region) fixture)
        ]

prepareWriteAmplificationFixture ::
  PreparedContextSite owner AnatomyRegion ->
  AnatomyRegion ->
  Int ->
  IO (WriteAmplificationFixture owner)
prepareWriteAmplificationFixture contextSite region termCount = do
  (baseGraph, classIds) <- requireRight "write amplification graph allocation" (buildArithGraph termCount)
  mergePairs <- requireMergePairs "write amplification" 9 classIds
  (benchLeft, benchRight) <- requireFirstPair "write amplification bench pair" (drop 8 mergePairs)
  contextGraph <-
    requireRight
      "write amplification activation"
      ( activateContextMerges
          (take 8 mergePairs)
          (emptyContextEGraphFromSite contextSite baseGraph)
      )
  mergedGraph <-
    requireRight
      "write amplification probe merge"
      (contextMerge region benchLeft benchRight contextGraph)
  let fixture =
        WriteAmplificationFixture
          { wafContextGraph = contextGraph,
            wafLeftClass = benchLeft,
            wafRightClass = benchRight
          }
  putStrLn
    ( "context-write-amplification "
        <> show region
        <> " N="
        <> show termCount
        <> ": fibers before "
        <> show (Map.size (cegContextFibers contextGraph))
        <> ", after one merge "
        <> show (Map.size (cegContextFibers mergedGraph))
    )
  evaluate (contextWriteAmplificationDigest region fixture) *> pure fixture

contextWriteAmplificationDigest :: AnatomyRegion -> WriteAmplificationFixture owner -> Int
contextWriteAmplificationDigest region fixture =
  either
    (const (-1))
    contextGraphDigest
    ( contextMerge
        region
        (wafLeftClass fixture)
        (wafRightClass fixture)
        (wafContextGraph fixture)
    )

contextWriteAmplificationCommitDigest :: AnatomyRegion -> WriteAmplificationFixture owner -> Int
contextWriteAmplificationCommitDigest region fixture =
  either
    (const (-1))
    contextCommitDigest
    ( contextMerge
        region
        (wafLeftClass fixture)
        (wafRightClass fixture)
        (wafContextGraph fixture)
    )

contextCommitDigest :: ContextEGraph owner f analysis context -> Int
contextCommitDigest contextGraph =
  fromIntegral (cegContextRevision contextGraph)
    + Map.size (cegContextFibers contextGraph)
    + Map.size (cegContextAnalysisDeltas contextGraph)


data SymbolicReceiptFixture owner = SymbolicReceiptFixture
  { srfContextGraph :: !(ContextEGraph owner Arith.ArithF Arith.NodeCount (Set Int)),
    srfQuery :: !ArithCompiledQuery,
    srfHeldStates :: ![(ContextObjectKey owner, EGraphPreparedMatchState owner SurfaceKind Arith.ArithF)],
    srfRegionTable :: !(RegionTable owner),
    srfAuthoredRegion :: !(ContextRegion owner),
    srfAuthoredKeys :: ![ContextObjectKey owner],
    srfRounds :: ![SymbolicRoundPlan]
  }

data SymbolicRoundPlan = SymbolicRoundPlan
  { srpLabel :: !String,
    srpContexts :: ![Set Int],
    srpMergePairs :: ![(ClassId, ClassId)]
  }

instance NFData (SymbolicReceiptFixture owner) where
  rnf fixture =
    symbolicReceiptDigest fixture `seq` ()

symbolicRegionReceiptBenches :: [Benchmark]
symbolicRegionReceiptBenches =
  [ bench "site-preparation/n=20/K=2^20" (nf symbolicSitePreparationDigest 20),
    symbolicSteadyStateReceiptBench
  ]

symbolicSteadyStateReceiptBench :: Benchmark
symbolicSteadyStateReceiptBench =
  either setupFailure id $
    withPreparedContextSiteFromPowersetAtoms
      [0 .. symbolicReceiptAtomCount - 1]
      ( \contextSite ->
          env (prepareSymbolicReceiptFixture contextSite 1000) $ \fixture ->
            bench benchmarkName (nf symbolicReceiptDigest fixture)
      )
  where
    benchmarkName =
      "steady-state-rounds/N=1000/n=20/authored=8/diffs=1,2,4,8"

    setupFailure :: PowersetSitePreparationError Int -> Benchmark
    setupFailure failure =
      env
        ( (ioError (userError ("symbolic powerset site: " <> show failure))) ::
            IO ()
        )
        (const (bench benchmarkName (nf id ())))

symbolicReceiptDigest :: SymbolicReceiptFixture owner -> Int
symbolicReceiptDigest fixture =
  foldl'
    (\total plan -> total + symbolicRoundDigest fixture plan)
    (length (srfAuthoredKeys fixture))
    (srfRounds fixture)

prepareSymbolicReceiptFixture ::
  PreparedContextSite owner (Set Int) ->
  Int ->
  IO (SymbolicReceiptFixture owner)
prepareSymbolicReceiptFixture contextSite termCount = do
  compiledQuery <- requireRight "symbolic arith pattern compilation" (compileArithPatternQuery arithAddXYPattern)
  (baseGraph, classIds) <- requireRight "symbolic receipt graph allocation" (buildArithGraph termCount)
  let authoredContexts =
        symbolicAuthoredContexts
  initialPairs <- requireMergePairs "symbolic receipt initial authored deltas" (length authoredContexts) classIds
  roundPairs <- requireMergePairs "symbolic receipt round deltas" (length authoredContexts) (drop (2 * length authoredContexts) classIds)
  contextGraph <-
    requireRight
      "symbolic receipt activation"
      ( activateSymbolicContextMerges
          (zip authoredContexts initialPairs)
          (emptyContextEGraphFromSite contextSite baseGraph)
      )
  contextKeys <-
    traverse
      ( \contextValue ->
          requireRight
            "symbolic receipt context key"
            (contextObjectKeyFor (cegSite contextGraph) contextValue)
      )
      (contextPreparedObjects contextGraph)
  (warmState, warmMatches) <-
    requireRight
      "symbolic receipt warm state"
      ( wcojPreparedMatchCompiledWithRoots
          compiledQuery
          (cegBase contextGraph)
          emptyEGraphPreparedMatchState
      )
  heldStates <-
    traverse
      ( \contextKey -> do
          (heldState, _heldMatches) <-
            requireRight
              "symbolic receipt held state"
              ( wcojPreparedAnnotatedContextDeltaMatchCompiledWithRoots
                  (contextAnnotatedDeltaBuckets contextGraph)
                  contextKey
                  (cegContextRevision contextGraph)
                  compiledQuery
                  (cegBase contextGraph)
                  warmState
              )
          pure (contextKey, heldState)
      )
      contextKeys
  authoredKeys <-
    traverse
      ( \contextValue ->
          requireRight
            "symbolic receipt authored key"
            (contextObjectKeyFor (cegSite contextGraph) contextValue)
      )
      authoredContexts
  let table =
        preparedRegionTable (cegSite contextGraph)
      authoredRegion =
        fromGeneratorKeys table authoredKeys
      rounds =
        symbolicRoundPlans authoredContexts roundPairs
      fixture =
        SymbolicReceiptFixture
          { srfContextGraph = contextGraph,
            srfQuery = compiledQuery,
            srfHeldStates = heldStates,
            srfRegionTable = table,
            srfAuthoredRegion = authoredRegion,
            srfAuthoredKeys = authoredKeys,
            srfRounds = rounds
          }
  putStrLn (symbolicReceiptHeader termCount fixture warmMatches)
  _ <- traverse (putStrLn . symbolicRoundReceipt fixture) rounds
  counterfactual <-
    requireRight
      "symbolic DNF counterfactual"
      (symbolicDnfCounterfactual table authoredKeys)
  putStrLn counterfactual
  evaluate (symbolicReceiptDigest fixture) *> pure fixture

symbolicReceiptAtomCount :: Int
symbolicReceiptAtomCount =
  20

symbolicSmallCounterfactualAtomCount :: Int
symbolicSmallCounterfactualAtomCount =
  8

symbolicAuthoredContexts :: [Set Int]
symbolicAuthoredContexts =
  Set.fromList
    <$> [ [0, 1, 2],
          [0, 3, 4],
          [0, 5, 6],
          [1, 3, 5],
          [1, 4, 7],
          [2, 3, 6],
          [2, 5, 7],
          [4, 6, 7]
        ]

symbolicRoundPlans :: [Set Int] -> [(ClassId, ClassId)] -> [SymbolicRoundPlan]
symbolicRoundPlans authoredContexts roundPairs =
  [ SymbolicRoundPlan
      { srpLabel = "authored-diff=" <> show diffCount,
        srpContexts = take diffCount authoredContexts,
        srpMergePairs = take diffCount roundPairs
      }
    | diffCount <- [1, 2, 4, 8]
  ]

activateSymbolicContextMerges ::
  [(Set Int, (ClassId, ClassId))] ->
  ContextEGraph owner Arith.ArithF Arith.NodeCount (Set Int) ->
  Either
    (ContextDeltaError Arith.ArithF (Set Int))
    (ContextEGraph owner Arith.ArithF Arith.NodeCount (Set Int))
activateSymbolicContextMerges authoredDeltas contextGraph =
  foldM
    (\graphValue (contextValue, (leftClass, rightClass)) -> contextMerge contextValue leftClass rightClass graphValue)
    contextGraph
    authoredDeltas

symbolicSitePreparationDigest :: Int -> Int
symbolicSitePreparationDigest atomCount =
  either
    (const (-1))
    id
    ( withPreparedContextSiteFromPowersetAtoms
        [0 .. atomCount - 1]
        ( \site ->
            either
              (const (-1))
              contextObjectKeyValue
              (contextObjectKeyFor site (Set.fromList (filter even [0 .. atomCount - 1])))
        )
    )

symbolicReceiptHeader :: Int -> SymbolicReceiptFixture owner -> [match] -> String
symbolicReceiptHeader termCount fixture warmMatches =
  "symbolic-regions exponential receipt n="
    <> show symbolicReceiptAtomCount
    <> " K=2^"
    <> show symbolicReceiptAtomCount
    <> "="
    <> show (2 ^ symbolicReceiptAtomCount :: Int)
    <> " N="
    <> show termCount
    <> " authored-contexts="
    <> show (length (srfAuthoredKeys fixture))
    <> " inhabited-closure-contexts="
    <> show (length (srfHeldStates fixture))
    <> " authored-cubes="
    <> show (regionCubeCount (srfRegionTable fixture) (srfAuthoredRegion fixture))
    <> " authored-generators="
    <> show (length (regionGeneratorKeys (srfRegionTable fixture) (srfAuthoredRegion fixture)))
    <> " warm-base-matches="
    <> show (length warmMatches)

symbolicRoundReceipt :: SymbolicReceiptFixture owner -> SymbolicRoundPlan -> String
symbolicRoundReceipt fixture plan =
  "symbolic-regions round "
    <> srpLabel plan
    <> ": changed-contexts="
    <> show (length (srpContexts plan))
    <> ", changed-cubes="
    <> show (regionCubeCount table deltaRegion)
    <> ", overlap-cubes="
    <> show (regionCubeCount table overlapRegion)
    <> ", next-cubes="
    <> show (regionCubeCount table nextRegion)
    <> ", dirty-class-keys="
    <> show (symbolicRoundDirtyKeyCount fixture plan)
    <> ", match-digest="
    <> show (symbolicRoundDigest fixture plan)
  where
    table =
      srfRegionTable fixture
    deltaRegion =
      fromGeneratorKeys table (symbolicRoundContextKeys fixture plan)
    overlapRegion =
      regionMeet (srfAuthoredRegion fixture) deltaRegion
    nextRegion =
      regionJoin (srfAuthoredRegion fixture) deltaRegion

symbolicDnfCounterfactual ::
  RegionTable owner ->
  [ContextObjectKey owner] ->
  Either String String
symbolicDnfCounterfactual table authoredKeys =
  renderCounterfactual <$> smallPointTerms
  where
    smallPointTerms = do
      preparedResult <-
        first show $
          withPreparedContextSiteFromPowersetAtoms
            [0 .. symbolicSmallCounterfactualAtomCount - 1]
            ( \smallSite -> do
                smallKeys <-
                  first show $
                    traverse
                      (contextObjectKeyFor smallSite)
                      symbolicAuthoredContexts
                pure $
                  regionSize
                    (fromGeneratorKeys (preparedRegionTable smallSite) smallKeys)
            )
      preparedResult

    multiplier =
      2 ^ (symbolicReceiptAtomCount - symbolicSmallCounterfactualAtomCount) :: Int

    renderCounterfactual exactPointTerms =
      "materialized counterfactual extrapolated-DNF: n="
        <> show symbolicSmallCounterfactualAtomCount
        <> " exact-point-terms="
        <> show exactPointTerms
        <> "; multiplier=2^("
        <> show symbolicReceiptAtomCount
        <> "-"
        <> show symbolicSmallCounterfactualAtomCount
        <> ")="
        <> show multiplier
        <> "; n="
        <> show symbolicReceiptAtomCount
        <> " point-DNF="
        <> show exactPointTerms
        <> "*"
        <> show multiplier
        <> "="
        <> show (exactPointTerms * multiplier)
        <> " of K=2^"
        <> show symbolicReceiptAtomCount
        <> "="
        <> show (2 ^ symbolicReceiptAtomCount :: Int)
        <> "; symbolic-cubes="
        <> show (regionCubeCount table (fromGeneratorKeys table authoredKeys))

symbolicRoundContextKeys ::
  SymbolicReceiptFixture owner ->
  SymbolicRoundPlan ->
  [ContextObjectKey owner]
symbolicRoundContextKeys fixture plan =
  [ contextKey
    | contextValue <- srpContexts plan,
      Right contextKey <- [contextObjectKeyFor (cegSite (srfContextGraph fixture)) contextValue]
  ]

symbolicRoundDigest :: SymbolicReceiptFixture owner -> SymbolicRoundPlan -> Int
symbolicRoundDigest fixture plan =
  either (const (-1)) digest (symbolicRoundState fixture plan)
  where
    digest (frontier, roundBuckets, roundRevision, roundBase) =
      either (const (-1)) id $
        foldM
          ( \total (contextKey, heldState) -> do
              (_roundState, roundMatches) <-
                first
                  (const ())
                  ( wcojPreparedAnnotatedContextDeltaMatchCompiledWithRoots
                      roundBuckets
                      contextKey
                      roundRevision
                      (srfQuery fixture)
                      roundBase
                      (symbolicDirtyStateFor frontier contextKey roundRevision heldState)
                  )
              pure (total + length roundMatches)
          )
          0
          (srfHeldStates fixture)

symbolicRoundDirtyKeyCount :: SymbolicReceiptFixture owner -> SymbolicRoundPlan -> Int
symbolicRoundDirtyKeyCount fixture plan =
  either
    (const (-1))
    ( \(frontier, _, _, _) ->
        foldl'
          ( \total (contextKey, _) ->
              total
                + maybe
                  0
                  (IntSet.size . annotatedDeltaFrontierKeys)
                  (IntMap.lookup (contextObjectKeyValue contextKey) frontier)
          )
          0
          (srfHeldStates fixture)
    )
    (symbolicRoundState fixture plan)

symbolicRoundState ::
  SymbolicReceiptFixture owner ->
  SymbolicRoundPlan ->
  Either
    ()
    ( IntMap.IntMap (AnnotatedDeltaFrontier Arith.ArithF),
      AnnotatedDeltaBuckets owner Arith.ArithF,
      Natural,
      EGraph Arith.ArithF Arith.NodeCount
    )
symbolicRoundState fixture plan = do
  stagedBatch <-
    first
      (const ())
      ( foldM
          ( \batchValue (contextValue, (leftClass, rightClass)) ->
              planContextMerges [contextValue] leftClass rightClass batchValue
                >>= (`stageContextMerges` batchValue)
          )
          (beginContextRebaseBatch (srfContextGraph fixture))
          (zip (srpContexts plan) (srpMergePairs plan))
      )
  (_report, nextGraph) <- first (const ()) (commitContextRebaseBatch stagedBatch)
  pure
    ( contextAnnotatedDeltaDirtyFrontier nextGraph,
      contextAnnotatedDeltaBuckets nextGraph,
      cegContextRevision nextGraph,
      cegBase nextGraph
    )

symbolicDirtyStateFor ::
  IntMap.IntMap (AnnotatedDeltaFrontier Arith.ArithF) ->
  ContextObjectKey owner ->
  Natural ->
  EGraphPreparedMatchState owner SurfaceKind Arith.ArithF ->
  EGraphPreparedMatchState owner SurfaceKind Arith.ArithF
symbolicDirtyStateFor frontier contextKey roundRevision heldState =
  maybe
    (refreshEGraphPreparedMatchStateAnnotatedRevisions roundRevision heldState)
    ( \frontierValue ->
        let dirtyKeys = annotatedDeltaFrontierKeys frontierValue
         in if IntSet.null dirtyKeys
              then refreshEGraphPreparedMatchStateAnnotatedRevisions roundRevision heldState
              else markEGraphPreparedMatchStateAnnotatedDirty dirtyKeys heldState
    )
    (IntMap.lookup (contextObjectKeyValue contextKey) frontier)


prepareContextMergeFixture ::
  PreparedContextSite owner AnatomyRegion ->
  Int ->
  Int ->
  IO (ContextMergeFixture owner)
prepareContextMergeFixture contextSite contextCount termCount = do
  (baseGraph, classIds) <- requireRight "context merge graph allocation" (buildArithGraph termCount)
  mergePairs <- requireMergePairs "context merge" contextCount classIds
  (leftClass, rightClass) <- requireFirstPair "context global merge" mergePairs
  contextGraph <-
    requireRight
      "context merge activation"
      ( activateContextMerges
          mergePairs
          (emptyContextEGraphFromSite contextSite baseGraph)
      )
  let fixture =
        ContextMergeFixture
          { cmfContextGraph = contextGraph,
            cmfLeftClass = leftClass,
            cmfRightClass = rightClass
          }
  evaluate (contextMergeFixtureDigest fixture + contextGlobalMergeDigest fixture) *> pure fixture

contextGlobalMergeDigest :: ContextMergeFixture owner -> Int
contextGlobalMergeDigest fixture =
  either
    (const (-1))
    contextGraphDigest
    ( globalMerge
        (cmfLeftClass fixture)
        (cmfRightClass fixture)
        (cmfContextGraph fixture)
    )


contextMergeFixtureDigest :: ContextMergeFixture owner -> Int
contextMergeFixtureDigest fixture =
  contextGraphDigest (cmfContextGraph fixture)
    + classIdKey (cmfLeftClass fixture)
    + classIdKey (cmfRightClass fixture)
