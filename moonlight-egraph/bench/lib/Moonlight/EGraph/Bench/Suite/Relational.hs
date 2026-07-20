{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Bench.Suite.Relational
  ( relationalBenchmarks,
  ) where

import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Numeric.Natural (Natural)
import Moonlight.Core (ClassId (..))
import Moonlight.EGraph.Bench.Corpus
  ( ArithCompiledQuery,
    activateContextMerges,
    arithAddXXPattern,
    arithAddXYPattern,
    buildArithGraph,
    caseLabel,
    compileArithPatternQuery,
    mergeableAnatomyRegions,
    nonOverlappingPairs,
    requireFirstPair,
    requireMergePairs,
  )
import Moonlight.EGraph.Bench.Harness.Run (requireRight)
import Moonlight.EGraph.Effect.CoveringSurface (SurfaceKind)
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    beginContextRebaseBatch,
    commitContextRebaseBatch,
    emptyContextEGraphFromSite,
    planContextMerges,
    stageContextMerges,
  )
import Moonlight.EGraph.Pure.Context.AnnotatedDelta
  ( AnnotatedDeltaBuckets,
    absorbedRowsAtKey,
    annotatedRowsAtKey,
    contextAnnotatedDeltaBuckets,
    contextAnnotatedDeltaDirtyFrontier,
    deriveAnnotatedDeltaBuckets,
  )
import Moonlight.EGraph.Pure.Context
  ( cegBase,
    cegContextRevision,
    cegSite,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.EGraph.Pure.Relational
  ( EGraphPreparedMatchState,
    PreparedBaseMatchMemo,
    emptyEGraphPreparedMatchState,
    emptyPreparedBaseMatchMemo,
    markEGraphPreparedMatchStateAnnotatedDirty,
    refreshEGraphPreparedMatchStateAnnotatedRevisions,
    wcojPreparedAnnotatedContextDeltaMatchCompiledWithRoots,
    wcojPreparedDeltaMatchCompiledWithRoots,
    wcojPreparedMatchCompiledWithRoots,
    wcojPreparedSharedBaseDeltaMatchCompiledWithRoots,
  )
import Moonlight.EGraph.Pure.Saturation.Matching (annotatedDeltaFrontierKeys)
import Moonlight.EGraph.Pure.Types (EGraph)
import Moonlight.EGraph.Test.Arith.Core qualified as Arith
import Moonlight.EGraph.Test.Context.Anatomy
  ( AnatomyRegion,
    coarseAnatomyLattice,
  )
import Moonlight.Sheaf.Context.Site
  ( ContextObjectKey,
    PreparedContextSite,
    contextObjectKeyFor,
    contextObjectKeyValue,
    withPreparedContextSiteFromFiniteLattice,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
  )

relationalBenchmarks :: Benchmark
relationalBenchmarks =
  bgroup "relational" [bgroup "annotated-matching" contextAnnotatedMatchingBenches]

data AnnotatedMatchingFixture owner = AnnotatedMatchingFixture
  { amfBuckets :: !(AnnotatedDeltaBuckets owner Arith.ArithF),
    amfBaseGraph :: !(EGraph Arith.ArithF Arith.NodeCount),
    amfContextGraph :: !(ContextEGraph owner Arith.ArithF Arith.NodeCount AnatomyRegion),
    amfContextKeys :: ![ContextObjectKey owner],
    amfContextRevision :: !Natural,
    amfQuery :: !ArithCompiledQuery,
    amfWarmState :: !(EGraphPreparedMatchState owner SurfaceKind Arith.ArithF),
    amfHeldStates :: ![(ContextObjectKey owner, EGraphPreparedMatchState owner SurfaceKind Arith.ArithF)],
    amfRoundMergeRegion :: !AnatomyRegion,
    amfRoundMergePair :: !(ClassId, ClassId)
  }

instance NFData (AnnotatedMatchingFixture owner) where
  rnf fixture =
    annotatedComposedColdDigest fixture
      `seq` annotatedVariantWarmDigest fixture
      `seq` annotatedPostMergeRoundDigest fixture
      `seq` annotatedPostMergeRoundFreshDigest fixture
      `seq` ()

contextAnnotatedMatchingBenches :: [Benchmark]
contextAnnotatedMatchingBenches =
  concatMap
    ( \gridPoint ->
        [ annotatedMatchingBench "composed-cold" annotatedComposedColdDigest gridPoint,
          annotatedMatchingBench "composed-cold-shared" annotatedComposedColdSharedDigest gridPoint,
          annotatedMatchingBench "variant-warm" annotatedVariantWarmDigest gridPoint,
          annotatedMatchingBench "post-merge-round" annotatedPostMergeRoundDigest gridPoint,
          annotatedMatchingBench "post-merge-round-fresh" annotatedPostMergeRoundFreshDigest gridPoint
        ]
    )
    ((,) <$> [1, 2, 4, 8] <*> [1000, 4000])

annotatedMatchingBench ::
  String ->
  (forall owner. AnnotatedMatchingFixture owner -> Int) ->
  (Int, Int) ->
  Benchmark
annotatedMatchingBench label digest (contextCount, termCount) =
  withPreparedContextSiteFromFiniteLattice coarseAnatomyLattice $ \site ->
    env (prepareAnnotatedMatchingFixture site contextCount termCount) $ \fixture ->
      bench
        (label <> "/" <> caseLabel [("K", contextCount), ("N", termCount)])
        (nf digest fixture)

prepareAnnotatedMatchingFixture ::
  PreparedContextSite owner AnatomyRegion ->
  Int ->
  Int ->
  IO (AnnotatedMatchingFixture owner)
prepareAnnotatedMatchingFixture site contextCount termCount = do
  (baseGraph, _classIds) <- requireRight "annotated matching graph allocation" (buildArithGraph termCount)
  numClassIds <-
    requireRight "annotated matching class allocation" $
      traverse
        (\index -> fst <$> addTerm (Arith.numTerm index) baseGraph)
        [0 .. 2 * contextCount + 1]
  mergePairs <- requireMergePairs "annotated matching" contextCount numClassIds
  roundMergePair <-
    requireFirstPair
      "annotated matching post-merge round"
      (drop contextCount (nonOverlappingPairs numClassIds))
  roundMergeRegion <-
    case take contextCount mergeableAnatomyRegions of
      region : _ ->
        pure region
      [] ->
        fail "annotated matching fixture expected at least one active region"
  let emptyContextGraph = emptyContextEGraphFromSite site baseGraph
  contextGraph <-
    requireRight
      "annotated matching activation"
      (activateContextMerges mergePairs emptyContextGraph)
  compiledQuery <-
    requireRight "arith pattern compilation" (compileArithPatternQuery arithAddXYPattern)
  nonlinearQuery <-
    requireRight "arith nonlinear pattern compilation" (compileArithPatternQuery arithAddXXPattern)
  contextKeys <-
    traverse
      ( \region ->
          requireRight
            "annotated matching context key"
            (contextObjectKeyFor (cegSite contextGraph) region)
      )
      (take contextCount mergeableAnatomyRegions)
  (warmState, warmMatches) <-
    requireRight
      "annotated matching warm state"
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
              "annotated matching held state"
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
  let fixture =
        AnnotatedMatchingFixture
          { amfBuckets = contextAnnotatedDeltaBuckets contextGraph,
            amfBaseGraph = cegBase contextGraph,
            amfContextGraph = contextGraph,
            amfContextKeys = contextKeys,
            amfContextRevision = cegContextRevision contextGraph,
            amfQuery = compiledQuery,
            amfWarmState = warmState,
            amfHeldStates = heldStates,
            amfRoundMergeRegion = roundMergeRegion,
            amfRoundMergePair = roundMergePair
          }
      variantCounts =
        fmap
          (\contextKey -> annotatedVariantMatchCount fixture contextKey)
          contextKeys
      addTag =
        Arith.Add () ()
      bucketRowCounts =
        fmap
          ( \contextKey ->
              ( length (annotatedRowsAtKey addTag contextKey (amfBuckets fixture)),
                length (absorbedRowsAtKey addTag contextKey (amfBuckets fixture))
              )
          )
          contextKeys
      coldVariantCounts =
        fmap
          ( \contextKey ->
              annotatedComposedMatchCount emptyEGraphPreparedMatchState fixture contextKey
                - length warmMatches
          )
          contextKeys
      nonlinearVariantCounts =
        fmap
          ( \contextKey ->
              either (const (-1)) (length . snd) $
                wcojPreparedAnnotatedContextDeltaMatchCompiledWithRoots
                  (amfBuckets fixture)
                  contextKey
                  (amfContextRevision fixture)
                  nonlinearQuery
                  (amfBaseGraph fixture)
                  emptyEGraphPreparedMatchState
          )
          contextKeys
  putStrLn
    ( "context-annotated-matching K="
        <> show contextCount
        <> " N="
        <> show termCount
        <> ": base matches "
        <> show (length warmMatches)
        <> ", variant matches per context "
        <> show variantCounts
        <> ", cold variant matches per context "
        <> show coldVariantCounts
        <> ", nonlinear variant matches per context "
        <> show nonlinearVariantCounts
        <> ", (variant,absorbed) Add rows per context "
        <> show bucketRowCounts
        <> ", post-merge round replay total "
        <> show (annotatedPostMergeRoundDigest fixture)
        <> ", post-merge fresh total "
        <> show (annotatedPostMergeRoundFreshDigest fixture)
    )
  evaluate
    ( annotatedComposedColdDigest fixture
        + annotatedVariantWarmDigest fixture
    )
    *> pure fixture

annotatedComposedColdDigest :: AnnotatedMatchingFixture owner -> Int
annotatedComposedColdDigest fixture =
  foldl'
    ( \total contextKey ->
        total + annotatedComposedMatchCount emptyEGraphPreparedMatchState fixture contextKey
    )
    0
    (amfContextKeys fixture)

annotatedComposedColdSharedDigest :: AnnotatedMatchingFixture owner -> Int
annotatedComposedColdSharedDigest fixture =
  snd $
    foldl'
      ( \(memo, total) contextKey ->
          let (nextMemo, count) =
                annotatedComposedSharedMatchCount memo fixture contextKey
           in (nextMemo, total + count)
      )
      (emptyPreparedBaseMatchMemo, 0)
      (amfContextKeys fixture)

annotatedComposedSharedMatchCount ::
  PreparedBaseMatchMemo Arith.ArithF ->
  AnnotatedMatchingFixture owner ->
  ContextObjectKey owner ->
  (PreparedBaseMatchMemo Arith.ArithF, Int)
annotatedComposedSharedMatchCount memo fixture contextKey =
  either (const (memo, -1)) id $ do
    (nextMemo, baseState, baseMatches) <-
      wcojPreparedSharedBaseDeltaMatchCompiledWithRoots
        (amfQuery fixture)
        (amfBaseGraph fixture)
        memo
        emptyEGraphPreparedMatchState
    (_variantState, variantMatches) <-
      wcojPreparedAnnotatedContextDeltaMatchCompiledWithRoots
        (amfBuckets fixture)
        contextKey
        (amfContextRevision fixture)
        (amfQuery fixture)
        (amfBaseGraph fixture)
        baseState
    pure (nextMemo, length baseMatches + length variantMatches)

annotatedVariantWarmDigest :: AnnotatedMatchingFixture owner -> Int
annotatedVariantWarmDigest fixture =
  foldl'
    ( \total contextKey ->
        total + annotatedComposedMatchCount (amfWarmState fixture) fixture contextKey
    )
    0
    (amfContextKeys fixture)

annotatedComposedMatchCount ::
  EGraphPreparedMatchState owner SurfaceKind Arith.ArithF ->
  AnnotatedMatchingFixture owner ->
  ContextObjectKey owner ->
  Int
annotatedComposedMatchCount initialState fixture contextKey =
  either (const (-1)) id $ do
    (baseState, baseMatches) <-
      wcojPreparedDeltaMatchCompiledWithRoots
        (amfQuery fixture)
        (amfBaseGraph fixture)
        initialState
    (_variantState, variantMatches) <-
      wcojPreparedAnnotatedContextDeltaMatchCompiledWithRoots
        (amfBuckets fixture)
        contextKey
        (amfContextRevision fixture)
        (amfQuery fixture)
        (amfBaseGraph fixture)
        baseState
    pure (length baseMatches + length variantMatches)

annotatedPostMergeRoundDigest :: AnnotatedMatchingFixture owner -> Int
annotatedPostMergeRoundDigest fixture =
  either (const (-1)) id $ do
    let initialBatch = beginContextRebaseBatch (amfContextGraph fixture)
    mergePlan <-
      first (const ())
        ( planContextMerges
            [amfRoundMergeRegion fixture]
            (fst (amfRoundMergePair fixture))
            (snd (amfRoundMergePair fixture))
            initialBatch
        )
    stagedBatch <-
      first (const ()) (stageContextMerges mergePlan initialBatch)
    (_report, nextGraph) <- first (const ()) (commitContextRebaseBatch stagedBatch)
    let frontier = contextAnnotatedDeltaDirtyFrontier nextGraph
        roundBuckets = contextAnnotatedDeltaBuckets nextGraph
        roundRevision = cegContextRevision nextGraph
        roundBase = cegBase nextGraph
    foldM
      ( \total (contextKey, heldState) -> do
          let dirtyKeys =
                maybe
                  IntSet.empty
                  annotatedDeltaFrontierKeys
                  (IntMap.lookup (contextObjectKeyValue contextKey) frontier)
              roundState
                | IntSet.null dirtyKeys =
                    refreshEGraphPreparedMatchStateAnnotatedRevisions roundRevision heldState
                | otherwise =
                    markEGraphPreparedMatchStateAnnotatedDirty dirtyKeys heldState
          (_roundState, roundMatches) <-
            first
              (const ())
              ( wcojPreparedAnnotatedContextDeltaMatchCompiledWithRoots
                  roundBuckets
                  contextKey
                  roundRevision
                  (amfQuery fixture)
                  roundBase
                  roundState
              )
          pure (total + length roundMatches)
      )
      0
      (amfHeldStates fixture)

annotatedPostMergeRoundFreshDigest :: AnnotatedMatchingFixture owner -> Int
annotatedPostMergeRoundFreshDigest fixture =
  either (const (-1)) id $ do
    let initialBatch = beginContextRebaseBatch (amfContextGraph fixture)
    mergePlan <-
      first (const ())
        ( planContextMerges
            [amfRoundMergeRegion fixture]
            (fst (amfRoundMergePair fixture))
            (snd (amfRoundMergePair fixture))
            initialBatch
        )
    stagedBatch <-
      first (const ()) (stageContextMerges mergePlan initialBatch)
    (_report, nextGraph) <- first (const ()) (commitContextRebaseBatch stagedBatch)
    roundBuckets <- first (const ()) (deriveAnnotatedDeltaBuckets nextGraph)
    let roundRevision = cegContextRevision nextGraph
        roundBase = cegBase nextGraph
    foldM
      ( \total (contextKey, _heldState) -> do
          (_roundState, roundMatches) <-
            first
              (const ())
              ( wcojPreparedAnnotatedContextDeltaMatchCompiledWithRoots
                  roundBuckets
                  contextKey
                  roundRevision
                  (amfQuery fixture)
                  roundBase
                  (amfWarmState fixture)
              )
          pure (total + length roundMatches)
      )
      0
      (amfHeldStates fixture)

annotatedVariantMatchCount :: AnnotatedMatchingFixture owner -> ContextObjectKey owner -> Int
annotatedVariantMatchCount fixture contextKey =
  either (const (-1)) (length . snd) $
    wcojPreparedAnnotatedContextDeltaMatchCompiledWithRoots
      (amfBuckets fixture)
      contextKey
      (amfContextRevision fixture)
      (amfQuery fixture)
      (amfBaseGraph fixture)
      emptyEGraphPreparedMatchState
