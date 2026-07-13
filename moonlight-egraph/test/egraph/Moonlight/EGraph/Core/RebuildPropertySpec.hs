{-# LANGUAGE ImportQualifiedPost #-}

module Moonlight.EGraph.Core.RebuildPropertySpec
  ( tests,
  )
where

import Moonlight.Saturation.Matching qualified as GenericMatching
import Control.Monad (foldM)
import Data.IntMap.Strict ( IntMap )
import Data.List ( nub )
import Moonlight.Algebra ( JoinSemilattice(join) )
import Moonlight.EGraph.Pure.Kernel.HashCons ( addTerm )
import Moonlight.EGraph.Pure.Rebuild ( merge, rebuildWithDelta )
import Moonlight.EGraph.Pure.Types
    ( classIdKey,
      ClassId(..),
      EGraph,
      eGraphUnionFind,
      eGraphAnalysis,
      emptyEGraph )
import Moonlight.EGraph.Test.Arith.Core
    ( ArithF,
      NodeCount(..),
      addTermNode,
      analysisSpec,
      mulTermNode,
      numTerm )
import Data.Fix ( Fix )
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.QuickCheck
    ( Gen,
      Property,
      chooseInt,
      counterexample,
      forAll,
      shuffle,
      testProperty,
      vectorOf,
      (===) )
import Data.IntMap.Strict qualified as IntMap
    ( lookup,
      empty,
      fromList,
      findWithDefault,
      foldlWithKey',
      toList,
      insertWith )
import Moonlight.Core qualified as UnionFind
    ( find )
import Moonlight.Core (UnionFindAllocationError)

tests :: TestTree
tests =
  testGroup
    "Rebuild properties (Cut 1 safety net)"
    [ testProperty "prop_repair_analysis_monotone" prop_repair_analysis_monotone,
      testProperty "prop_repair_converges_independent_of_order" prop_repair_converges_independent_of_order,
      testProperty "prop_unified_rebuild_equivalence" prop_unified_rebuild_equivalence,
      testProperty "prop_canonicalize_threaded_invariant" prop_canonicalize_threaded_invariant
    ]

-- | Small corpus of arith terms used to populate a test e-graph.
arithCorpus :: [Fix ArithF]
arithCorpus =
  [ numTerm 0,
    numTerm 1,
    numTerm 2,
    numTerm 3,
    numTerm 4,
    addTermNode (numTerm 1) (numTerm 2),
    addTermNode (numTerm 2) (numTerm 3),
    mulTermNode (numTerm 1) (numTerm 3),
    mulTermNode (numTerm 2) (numTerm 4),
    addTermNode (addTermNode (numTerm 1) (numTerm 2)) (numTerm 3),
    mulTermNode (addTermNode (numTerm 1) (numTerm 2)) (numTerm 3)
  ]

-- | Construct a fresh e-graph populated with the corpus, returning each class id.
populatedGraph :: Either UnionFindAllocationError (EGraph ArithF NodeCount, [ClassId])
populatedGraph =
  fmap (fmap reverse) $
    foldM
      ( \(graph, classIds) term ->
          fmap
            (\(classIdValue, graphNext) -> (graphNext, classIdValue : classIds))
            (addTerm term graph)
      )
      (emptyEGraph analysisSpec, [])
      arithCorpus

-- | A pending merge expressed as an index pair into the populated class id list.
newtype MergeSequence = MergeSequence {runMergeSequence :: [(Int, Int)]}
  deriving stock (Eq, Show)

-- | Generate a non-trivial merge sequence over the populated corpus.
genMergeSequence :: Gen MergeSequence
genMergeSequence = do
  mergeCount <- chooseInt (1, 5)
  pairs <-
    vectorOf
      mergeCount
      ( (,) <$> chooseInt (0, length arithCorpus - 1)
          <*> chooseInt (0, length arithCorpus - 1)
      )
  pure (MergeSequence (filter (\(leftIndex, rightIndex) -> leftIndex /= rightIndex) pairs))

-- | Apply a merge sequence to a freshly populated graph.
data RebuildFixtureError
  = RebuildFixtureAllocationFailed UnionFindAllocationError
  | RebuildFixtureClassIndexMissing Int
  deriving stock (Eq, Show)

applyMerges :: MergeSequence -> Either RebuildFixtureError (EGraph ArithF NodeCount, [ClassId])
applyMerges mergeSequence =
  fmapLeft RebuildFixtureAllocationFailed populatedGraph
    >>= \(graph0, classIds) ->
      fmap (\graph -> (graph, classIds)) $
        foldM
          (mergeIndexedClasses (IntMap.fromList (zip [0 ..] classIds)))
          graph0
          (runMergeSequence mergeSequence)

mergeIndexedClasses :: IntMap ClassId -> EGraph ArithF NodeCount -> (Int, Int) -> Either RebuildFixtureError (EGraph ArithF NodeCount)
mergeIndexedClasses classIds graph (leftIndex, rightIndex) = do
  leftClassId <- maybe (Left (RebuildFixtureClassIndexMissing leftIndex)) Right (IntMap.lookup leftIndex classIds)
  rightClassId <- maybe (Left (RebuildFixtureClassIndexMissing rightIndex)) Right (IntMap.lookup rightIndex classIds)
  pure (merge leftClassId rightClassId graph)

withFixture :: Either RebuildFixtureError fixture -> (fixture -> Property) -> Property
withFixture fixtureResult continue =
  either
    (\fixtureError -> counterexample ("fixture construction failed: " <> show fixtureError) (False === True))
    continue
    fixtureResult

fmapLeft :: (left -> mappedLeft) -> Either left right -> Either mappedLeft right
fmapLeft mapError =
  either (Left . mapError) Right

-- | Canonical representative via the graph's union-find.
canonicalOf :: ClassId -> EGraph ArithF NodeCount -> ClassId
canonicalOf classIdValue graph =
  fst (UnionFind.find classIdValue (eGraphUnionFind graph))

-- | Property 1 — Analysis values only grow under rebuild.
--
-- For every class key present in the pre-rebuild analysis map, the canonical
-- representative's post-rebuild analysis value must be >= the pre-rebuild value
-- under the semilattice order (enforced by @asJoinChanged@ in the repair BFS).
prop_repair_analysis_monotone :: Property
prop_repair_analysis_monotone =
  forAll genMergeSequence $ \mergeSequence ->
    withFixture (applyMerges mergeSequence) $ \(preGraph, _) ->
      let
        (_, _, postGraph) = rebuildWithDelta preGraph
        preAnalysis = eGraphAnalysis preGraph
        postAnalysis = eGraphAnalysis postGraph
        semijoinValidatees =
          [ (key, preValue, postValue)
            | (key, preValue) <- IntMap.toList preAnalysis,
              let canonicalKey = classIdKey (canonicalOf (ClassId key) postGraph)
                  postValue = IntMap.findWithDefault preValue canonicalKey postAnalysis
          ]
        monotone =
          all
            (\(_, preValue, postValue) -> postValue == join preValue postValue)
            semijoinValidatees
       in counterexample
          ("merges = " <> show (runMergeSequence mergeSequence) <> ", witnesses = " <> show semijoinValidatees)
          (monotone === True)

-- | Property 2 — Repair converges to the same analysis regardless of merge order.
--
-- Two permutations of the same merge set must agree on every original class id
-- after looking up its analysis via each graph's own canonical map. Min-biased
-- canonical representatives may differ between orderings; the invariant is that
-- the analysis at the equivalence class is stable, not the key labelling.
prop_repair_converges_independent_of_order :: Property
prop_repair_converges_independent_of_order =
  forAll genMergeSequence $ \mergeSequence ->
    forAll (shuffle (runMergeSequence mergeSequence)) $ \reorderedPairs ->
      withFixture (applyMerges mergeSequence) $ \(originalGraph, classIds) ->
        withFixture (applyMerges (MergeSequence reorderedPairs)) $ \(reorderedGraph, _) ->
          let originalPost = snd3 (rebuildWithDelta originalGraph)
              reorderedPost = snd3 (rebuildWithDelta reorderedGraph)
              disagreements =
                [ (origKey, lookupCanonical originalPost origKey, lookupCanonical reorderedPost origKey)
                  | classIdValue <- classIds,
                    let origKey = classIdKey classIdValue,
                    lookupCanonical originalPost origKey /= lookupCanonical reorderedPost origKey
                ]
           in counterexample
            ( "original = "
                <> show (runMergeSequence mergeSequence)
                <> ", reordered = "
                <> show reorderedPairs
                <> ", disagreements = "
                <> show disagreements
            )
            (disagreements === [])
  where
    snd3 :: (left, middle, right) -> right
    snd3 (_, _, x) = x

-- | Look up an analysis value for an original class id via the graph's own
-- canonical map. Returns the value attached to that equivalence class.
lookupCanonical :: EGraph ArithF NodeCount -> Int -> Maybe NodeCount
lookupCanonical graph origKey =
  let canonKey = classIdKey (canonicalOf (ClassId origKey) graph)
   in IntMap.lookup canonKey (eGraphAnalysis graph)

-- | Project an analysis map onto canonical representatives under a graph's
-- union-find. Collapsing by @join@ so the reprojection is order-independent.
canonicalizeAnalysis ::
  IntMap NodeCount ->
  EGraph ArithF NodeCount ->
  IntMap NodeCount
canonicalizeAnalysis analysisMap graph =
  IntMap.foldlWithKey'
    ( \acc key value ->
        let canonicalKey = classIdKey (canonicalOf (ClassId key) graph)
         in IntMap.insertWith join canonicalKey value acc
    )
    IntMap.empty
    analysisMap

-- | Property 3 — Rebuild is idempotent on the analysis map.
--
-- The load-bearing gate for Cut 1: once the worklist absorbs
-- @repairAnalysisFromDatabase@, a second rebuild must still leave the analysis
-- fixed. Any interleaving that fails to reach the least fixpoint will diverge
-- between @rebuild@ and @rebuild . rebuild@.
prop_unified_rebuild_equivalence :: Property
prop_unified_rebuild_equivalence =
  forAll genMergeSequence $ \mergeSequence ->
    withFixture (applyMerges mergeSequence) $ \(preGraph, _) ->
      let (_, _, firstPass) = rebuildWithDelta preGraph
          (_, _, secondPass) = rebuildWithDelta firstPass
       in counterexample
          ("merges = " <> show (runMergeSequence mergeSequence))
          ( canonicalizeAnalysis (eGraphAnalysis firstPass) firstPass
              === canonicalizeAnalysis (eGraphAnalysis secondPass) secondPass
          )

-- | Property 4 — The canonicalize map produced by rebuild is idempotent.
--
-- Round-6 stale-ID hazard guard: every class id must canonicalize to a fixed
-- point. @GenericMatching.maAdvanceState@ in the cert cache depends on this invariant when
-- canonicalizing cached roots + dep footprints through the latest union-find.
prop_canonicalize_threaded_invariant :: Property
prop_canonicalize_threaded_invariant =
  forAll genMergeSequence $ \mergeSequence ->
    withFixture (applyMerges mergeSequence) $ \(preGraph, classIds) ->
      let (_, _, postGraph) = rebuildWithDelta preGraph
          canonicalize classIdValue = canonicalOf classIdValue postGraph
          witnesses =
            [ (classIdValue, firstCanonical, secondCanonical)
              | classIdValue <- nub classIds,
                let firstCanonical = canonicalize classIdValue
                    secondCanonical = canonicalize firstCanonical
            ]
          idempotent =
            all (\(_, firstCanonical, secondCanonical) -> firstCanonical == secondCanonical) witnesses
       in counterexample
          ("merges = " <> show (runMergeSequence mergeSequence) <> ", witnesses = " <> show witnesses)
          (idempotent === True)
