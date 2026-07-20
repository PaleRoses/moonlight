module Moonlight.EGraph.Rewrite.StructuralStoreSpec
  ( tests,
  )
where

import Data.List ( sort )
import Moonlight.EGraph.Pure.Analysis ( AnalysisSpec(..) )
import Moonlight.EGraph.Pure.Kernel.HashCons ( addTerm )
import Moonlight.EGraph.Pure.Rebuild
    ( merge,
      rebuildWithDelta,
      EGraphRebuildDelta(erdDirtyResultKeys, erdTopologyClassKeys,
                         EGraphRebuildDelta) )
import Moonlight.EGraph.Pure.Structural.Store
    ( StructuralStore,
      emptyStructuralStore,
      insertCanonicalTuple,
      seStore,
      structuralChildrenByResult,
      structuralParentKeysByChild,
      structuralResultKeys,
      structuralTuplesForResultKey )
import Moonlight.EGraph.Pure.Relational
    ( EGraphPreparedBase,
      atomizeCompiledPatternQuery,
      buildPreparedBase,
      preparedBaseRowBlocks )
import Moonlight.Flow.Storage.Relation ( materializeAtomRow )
import Moonlight.EGraph.Pure.Types
    ( ClassId(..),
      ENode (..),
      canonicalizeClassId,
      classIdKey,
      eGraphAnalysis,
      eGraphStore,
      emptyEGraph )
import Moonlight.EGraph.Test.Arith.Core
    ( ArithF,
      addTermNode,
      analysisSpec,
      mulTermNode,
      numTerm )
import Moonlight.EGraph.Test.Arith.Core qualified as ArithCore
import Moonlight.EGraph.Test.Ring.Core ( RingF(..), ringAdd )
import Moonlight.EGraph.Test.Saturation.Helpers
    ( addXYPattern, buildGraph, compileRingPatternQuery )
import Data.Fix ( Fix(..) )
import Moonlight.Differential.Row.Tuple (tupleKeyTouches)
import Moonlight.Core (Language, Pattern (..))
import Moonlight.Core qualified as EGraph
import Moonlight.Differential.Row.Block
    ( RowBlock, RowBuildError, RowState(Canonical), foldRowBlock,
      rowBlockCount )
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit
    ( assertBool, assertFailure, testCase, (@?=) )
import Data.IntMap.Strict qualified as IntMap
    ( IntMap,
      elems,
      null,
      lookup,
      fromList,
      empty,
      foldlWithKey',
      findWithDefault )
import Data.IntSet qualified as IntSet
    ( member, singleton, size, empty )
import Moonlight.EGraph.Test.Ring.Core qualified as Ring
    ( ringMul )
import Moonlight.Pale.Test.Site.Assertion (expectRight)

structuralStoreForTuples :: forall f. Language f => IntMap.IntMap [ENode f] -> StructuralStore f
structuralStoreForTuples =
  IntMap.foldlWithKey' insertResultTuples emptyStructuralStore
  where
    insertResultTuples :: StructuralStore f -> Int -> [ENode f] -> StructuralStore f
    insertResultTuples store resultKey =
      foldl'
        ( \currentStore enode ->
            seStore (insertCanonicalTuple (ClassId resultKey) enode currentStore)
        )
        store

preparedBaseRows ::
  EGraphPreparedBase capability f ->
  Either RowBuildError (IntMap.IntMap (RowBlock 'Canonical))
preparedBaseRows =
  preparedBaseRowBlocks 0

traceAnalysisSpec :: AnalysisSpec ArithF [Int]
traceAnalysisSpec =
  AnalysisSpec
    { asMake = \arithNode ->
        case arithNode of
          ArithCore.Num value -> [value]
          ArithCore.Var value -> [value]
          _ -> foldMap id arithNode,
      asJoin = (<>),
      asJoinChanged = \oldValue newValue ->
        let joinedValue = oldValue <> newValue
         in (joinedValue, joinedValue /= oldValue)
    }

tests :: TestTree
tests =
  testGroup
    "StructuralStore and rebuild"
    [ testCase "child graph contains children of parent classes" $ do
        (c1, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
        (c2, graph2) <- expectRight (addTerm (numTerm 2) graph1)
        (cAdd, graph3) <- expectRight (addTerm (addTermNode (numTerm 1) (numTerm 2)) graph2)
        let (_delta, rebuiltGraph) = rebuildWithDelta (merge c1 c2 graph3)
            childGraph = structuralChildrenByResult (eGraphStore rebuiltGraph)
            kAdd = classIdKey cAdd
            childrenOfAdd = IntMap.findWithDefault IntMap.empty kAdd childGraph
        assertBool
              "add node has children in the index"
              (not (IntMap.null childrenOfAdd)),
      testCase "shared repair children preserves multiplicity" $ do
        (c1, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
        (_, graph2) <- expectRight (addTerm (numTerm 2) graph1)
        (cAdd, graph3) <- expectRight (addTerm (addTermNode (numTerm 1) (numTerm 2)) graph2)
        (cMul, graph4) <- expectRight (addTerm (mulTermNode (numTerm 1) (numTerm 2)) graph3)
        let (_delta, rebuiltGraph) = rebuildWithDelta (merge cAdd cMul graph4)
            childGraph = structuralChildrenByResult (eGraphStore rebuiltGraph)
            kMerged = classIdKey cAdd
            k1 = classIdKey c1
            childrenOfMerged = IntMap.findWithDefault IntMap.empty kMerged childGraph
            multOfC1 = IntMap.findWithDefault 0 k1 childrenOfMerged
        assertBool
              "merged add+mul class references child with multiplicity >= 2"
              (multOfC1 >= 2),
      testCase "tuplesByResult maps class to its producing e-nodes" $ do
        (c1, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
        (c2, graph2) <- expectRight (addTerm (numTerm 2) graph1)
        (cAdd, graph3) <- expectRight (addTerm (addTermNode (numTerm 1) (numTerm 2)) graph2)
        let (_delta, rebuiltGraph) = rebuildWithDelta (merge c1 c2 graph3)
        assertBool
              "add class has producing e-nodes"
              (not (null (structuralTuplesForResultKey (classIdKey cAdd) (eGraphStore rebuiltGraph)))),
      testCase "no pending rebuild retains producing nodes in the structural store" $ do
        (_, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
        (_, graph2) <- expectRight (addTerm (numTerm 2) graph1)
        (cAdd, graph3) <- expectRight (addTerm (addTermNode (numTerm 1) (numTerm 2)) graph2)
        let (_delta, rebuiltGraph) = rebuildWithDelta graph3
        assertBool
              "structural store should expose existing producing e-nodes even without pending deltas"
              (not (null (structuralTuplesForResultKey (classIdKey cAdd) (eGraphStore rebuiltGraph)))),
      testCase "tuple-restricted store derives parent and child views from retained tuples" $
        let resultKey = 30
            visibleChildKey = 10
            retainedTuple =
              ENode (Add (ClassId visibleChildKey) (ClassId visibleChildKey))
            tuplesByResult =
              IntMap.fromList [(resultKey, [retainedTuple])]
            restrictedStore = structuralStoreForTuples tuplesByResult
         in do
              structuralTuplesForResultKey resultKey restrictedStore @?= [retainedTuple]
              IntMap.findWithDefault IntSet.empty visibleChildKey (structuralParentKeysByChild restrictedStore)
                @?= IntSet.singleton resultKey
              IntMap.findWithDefault IntMap.empty resultKey (structuralChildrenByResult restrictedStore)
                @?= IntMap.fromList [(visibleChildKey, 2)],
      testCase "analysis merge keeps canonical value before absorbed value" $ do
        (canonicalInput, graph1) <- expectRight (addTerm (numTerm 2) (emptyEGraph traceAnalysisSpec))
        (absorbedInput, graph2) <- expectRight (addTerm (numTerm 1) graph1)
        let (_delta, rebuiltGraph) =
              rebuildWithDelta (merge canonicalInput absorbedInput graph2)
            canonicalOutput =
              canonicalizeClassId rebuiltGraph canonicalInput
        IntMap.lookup (classIdKey canonicalOutput) (eGraphAnalysis rebuiltGraph)
              @?= Just [2, 1],
      testCase "structural parent incidence covers all database edges" $ do
        (c1, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
        (c2, graph2) <- expectRight (addTerm (numTerm 2) graph1)
        (_, graph3) <- expectRight (addTerm (addTermNode (numTerm 1) (numTerm 2)) graph2)
        (_, graph4) <- expectRight (addTerm (mulTermNode (numTerm 1) (numTerm 2)) graph3)
        let (_delta, rebuiltGraph) = rebuildWithDelta (merge c1 c2 graph4)
            parentMap = structuralParentKeysByChild (eGraphStore rebuiltGraph)
            allParentSets = foldMap id (IntMap.elems parentMap)
        assertBool
              "parent map references at least 2 distinct parent classes"
              (IntSet.size allParentSets >= 2),
      testCase "rebuild delta does not perform eager topology invalidation" $ do
        (c1, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
        (c2, graph2) <- expectRight (addTerm (numTerm 2) graph1)
        (_, graph3) <- expectRight (addTerm (numTerm 3) graph2)
        (_, graph4) <- expectRight (addTerm (addTermNode (numTerm 1) (numTerm 3)) graph3)
        (_, graph5) <- expectRight (addTerm (addTermNode (numTerm 2) (numTerm 3)) graph4)
        (_, graph6) <-
          expectRight
            ( addTerm
                (addTermNode (addTermNode (numTerm 1) (numTerm 3)) (addTermNode (numTerm 2) (numTerm 3)))
                graph5
            )
        let (EGraphRebuildDelta {erdTopologyClassKeys}, _rebuilt) =
              rebuildWithDelta (merge c1 c2 graph6)
        erdTopologyClassKeys @?= IntSet.empty
    , testCase "structural store retains unrelated result tuples beyond the dirty repair slice" $ do
        (c1, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
        (c2, graph2) <- expectRight (addTerm (numTerm 2) graph1)
        (_, graph3) <- expectRight (addTerm (numTerm 3) graph2)
        (_, graph4) <- expectRight (addTerm (numTerm 4) graph3)
        (_, graph5) <- expectRight (addTerm (numTerm 5) graph4)
        (_, graph6) <- expectRight (addTerm (addTermNode (numTerm 1) (numTerm 3)) graph5)
        (_, graph7) <- expectRight (addTerm (addTermNode (numTerm 2) (numTerm 3)) graph6)
        (unrelatedParent, graph8) <- expectRight (addTerm (addTermNode (numTerm 4) (numTerm 5)) graph7)
        let (EGraphRebuildDelta {erdDirtyResultKeys}, rebuiltGraph) =
              rebuildWithDelta (merge c1 c2 graph8)
            indexedResultKeys =
              structuralResultKeys (eGraphStore rebuiltGraph)
            unrelatedKey = classIdKey unrelatedParent
        assertBool
                "structural store should still contain the unrelated parent tuple"
                (IntSet.member unrelatedKey indexedResultKeys)
        assertBool
                "dirty repair slice should exclude the unrelated parent"
                (not (IntSet.member unrelatedKey erdDirtyResultKeys))
        assertBool
                "structural store contains a strictly larger set than the dirty repair slice"
                (IntSet.size indexedResultKeys > IntSet.size erdDirtyResultKeys)
    , testCase "prepared base currently materializes unrelated query rows beyond the dirty repair slice" $
        let ringNum value = Fix (Num value)
         in case
              buildGraph
                [ ringNum 1
                , ringNum 2
                , ringNum 3
                , ringNum 4
                , ringNum 5
                , ringAdd (ringNum 1) (ringNum 3)
                , ringAdd (ringNum 2) (ringNum 3)
                , ringAdd (ringNum 4) (ringNum 5)
                ]
            of
              Right (graph0, [c1, c2, _, _, _, _, _, unrelatedParent]) -> do
                compiledQuery <-
                  expectRight (compileRingPatternQuery addXYPattern)
                queryPlan <-
                  expectRight (atomizeCompiledPatternQuery compiledQuery)
                let (EGraphRebuildDelta {erdDirtyResultKeys}, rebuiltGraph) =
                      rebuildWithDelta (merge c1 c2 graph0)
                let rebuiltPreparedBase =
                      buildPreparedBase queryPlan rebuiltGraph
                atomRelations <-
                  expectRight (preparedBaseRows rebuiltPreparedBase)
                let unrelatedKey =
                      classIdKey unrelatedParent
                    unrelatedDirty =
                      IntSet.singleton unrelatedKey
                    relationTouches rel =
                      foldRowBlock
                        (\acc desc -> acc || tupleKeyTouches unrelatedDirty (materializeAtomRow rel desc))
                        False
                        rel
                    unrelatedTouched =
                      any relationTouches atomRelations
                    indexedResultKeys =
                      structuralResultKeys (eGraphStore rebuiltGraph)
                assertBool
                  "dirty repair slice excludes the unrelated add root"
                  (not (IntSet.member unrelatedKey erdDirtyResultKeys))
                assertBool
                  "structural store still contains the unrelated add root"
                  (IntSet.member unrelatedKey indexedResultKeys)
                assertBool
                  "base atom relations still materialize rows touching the unrelated add root"
                  unrelatedTouched
              Right _ ->
                assertFailure "expected buildGraph to return eight classes"
              Left allocationError ->
                assertFailure ("buildGraph allocation failed: " <> show allocationError)
    , testCase "prepared base preserves exact per-atom slice counts for shared-prefix nested queries" $ do
        let ringNum value = Fix (Num value)
            sharedMul =
              Ring.ringMul
                (ringNum 1)
                (ringNum 2)
            outerLeft =
              ringAdd
                sharedMul
                (ringNum 0)
            outerRight =
              ringAdd
                sharedMul
                (ringNum 1)
            compiledQueryResult =
              compileRingPatternQuery
                (PatternNode (Add (PatternNode (Mul (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1)))) (PatternVar (EGraph.mkPatternVar 2))))
        (graph0, _) <-
          expectRight
            ( buildGraph
                [ ringNum 0
                , ringNum 1
                , ringNum 2
                , sharedMul
                , outerLeft
                , outerRight
                ]
            )
        compiledQuery <- expectRight compiledQueryResult
        queryPlan <- expectRight (atomizeCompiledPatternQuery compiledQuery)
        let preparedBase = buildPreparedBase queryPlan graph0
        preparedRows <- expectRight (preparedBaseRows preparedBase)
        let sliceCounts =
              sort
                ( fmap
                    rowBlockCount
                    (IntMap.elems preparedRows)
                )
        sliceCounts @?= [1, 2]
    ]
