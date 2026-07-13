module Moonlight.EGraph.Rewrite.RepairIndexSpec
  ( tests,
  )
where

import Data.List ( sort )
import Data.Bifunctor ( first )
import Moonlight.EGraph.Pure.Analysis.Spec ( AnalysisSpec(..) )
import Moonlight.EGraph.Pure.Kernel.HashCons ( addTerm )
import Moonlight.EGraph.Pure.Rebuild
    ( BaseRepairIndex(briIndex),
      merge,
      rebuildWithDelta,
      EGraphRebuildDelta(erdDirtyResultKeys, erdTopologyClassKeys,
                         erdImpactedClassKeys, EGraphRebuildDelta) )
import Moonlight.EGraph.Pure.Rebuild.Index
    ( canonicalizeClassKeys,
      baseRepairIndexFromStore )
import Moonlight.EGraph.Pure.Structural.Store
    ( StructuralStore,
      emptyStructuralStore,
      insertCanonicalTuple,
      seStore )
import Moonlight.EGraph.Pure.Relational
    ( EGraphPreparedBase,
      atomizeCompiledPatternQuery,
      buildPreparedBase,
      preparedBaseRowBlocks )
import Moonlight.Flow.Storage.Relation ( materializeAtomRow )
import Moonlight.Repair.Index
  ( RepairIndex (..),
  )
import Moonlight.EGraph.Pure.Types
    ( ClassId(..),
      ENode (..),
      canonicalizeClassId,
      classIdKey,
      eGraphAnalysis,
      emptyEGraph )
import Moonlight.EGraph.Test.Arith.Core
    ( ArithF,
      NodeCount,
      addTermNode,
      analysisSpec,
      mulTermNode,
      numTerm )
import Moonlight.EGraph.Test.Context.ThreeLevel
    ( Scope )
import Moonlight.EGraph.Test.Arith.Core qualified as ArithCore
import Moonlight.EGraph.Test.Ring.Core ( RingF(..), ringAdd )
import Moonlight.EGraph.Test.Saturation.Helpers
    ( addXYPattern, buildGraph, compileRingPatternQuery )
import Data.Fix ( Fix(..) )
import Moonlight.Differential.Row.Tuple
import Moonlight.Core
import Moonlight.Core qualified as EGraph
import Moonlight.Differential.Row.Block
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit
    ( assertBool, assertFailure, testCase, (@?=) )
import Data.IntMap.Strict qualified as IntMap
    ( IntMap,
      elems,
      intersectionWith,
      keysSet,
      null,
      lookup,
      insert,
      fromList,
      size,
      empty,
      foldr,
      foldlWithKey',
      findWithDefault,
      mapMaybeWithKey )
import Data.IntSet qualified as IntSet
    ( fromList, member, null, singleton, size, empty )
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
    "BaseRepairIndex"
    [ testCase "parent map contains parents of merged classes" $ do
        (c1, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
        (c2, graph2) <- expectRight (addTerm (numTerm 2) graph1)
        (_, graph3) <- expectRight (addTerm (addTermNode (numTerm 1) (numTerm 2)) graph2)
        let (_delta, index, _rebuilt) = rebuildWithDelta (merge c1 c2 graph3)
            parentMap = riParents (briIndex index)
            k1 = classIdKey c1
            k2 = classIdKey c2
            parentsOfK1 = IntMap.findWithDefault IntSet.empty k1 parentMap
            parentsOfK2 = IntMap.findWithDefault IntSet.empty k2 parentMap
        assertBool
              "merged class has at least one parent in the index"
              (not (IntSet.null parentsOfK1) || not (IntSet.null parentsOfK2)),
      testCase "child graph contains children of parent classes" $ do
        (c1, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
        (c2, graph2) <- expectRight (addTerm (numTerm 2) graph1)
        (cAdd, graph3) <- expectRight (addTerm (addTermNode (numTerm 1) (numTerm 2)) graph2)
        let (_delta, index, _rebuilt) = rebuildWithDelta (merge c1 c2 graph3)
            childGraph = riChildren (briIndex index)
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
        let (_delta, index, _rebuilt) = rebuildWithDelta (merge cAdd cMul graph4)
            childGraph = riChildren (briIndex index)
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
        let (_delta, index, _rebuilt) = rebuildWithDelta (merge c1 c2 graph3)
            tuples = riTuplesByResult (briIndex index)
            kAdd = classIdKey cAdd
        assertBool
              "add class has producing e-nodes"
              (not (null (IntMap.findWithDefault [] kAdd tuples))),
      testCase "no pending merges yields empty index" $ do
        (_, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
        let (_delta, index, _rebuilt) = rebuildWithDelta graph1
        riParents (briIndex index) @?= IntMap.empty
        riChildren (briIndex index) @?= IntMap.empty,
      testCase "no pending indexed rebuild returns a usable repair index" $ do
        (_, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
        (_, graph2) <- expectRight (addTerm (numTerm 2) graph1)
        (cAdd, graph3) <- expectRight (addTerm (addTermNode (numTerm 1) (numTerm 2)) graph2)
        let (_delta, index, _rebuilt) = rebuildWithDelta graph3
        assertBool
              "indexed rebuild should expose existing producing e-nodes even without pending deltas"
              (not (null (IntMap.findWithDefault [] (classIdKey cAdd) (riTuplesByResult (briIndex index))))),
      testCase "tuple-restricted repair index rebuilds parent and child maps from retained tuples" $
        let resultKey = 30
            visibleChildKey = 10
            retainedTuple =
              ENode (Add (ClassId visibleChildKey) (ClassId visibleChildKey))
            tuplesByResult =
              IntMap.fromList [(resultKey, [retainedTuple])]
            restrictedIndex =
              baseRepairIndexFromStore (structuralStoreForTuples tuplesByResult)
         in do
              riTuplesByResult (briIndex restrictedIndex) @?= tuplesByResult
              IntMap.findWithDefault IntSet.empty visibleChildKey (riParents (briIndex restrictedIndex))
                @?= IntSet.singleton resultKey
              IntMap.findWithDefault IntMap.empty resultKey (riChildren (briIndex restrictedIndex))
                @?= IntMap.fromList [(visibleChildKey, 2)],
      testCase "canonicalizeClassKeys collapses non-injective representatives" $
        canonicalizeClassKeys
          (\classId -> if classId == ClassId 2 then ClassId 1 else classId)
          (IntSet.fromList [1, 2, 4])
          @?= IntSet.fromList [1, 4],
      testCase "analysis merge keeps canonical value before absorbed value" $ do
        (canonicalInput, graph1) <- expectRight (addTerm (numTerm 2) (emptyEGraph traceAnalysisSpec))
        (absorbedInput, graph2) <- expectRight (addTerm (numTerm 1) graph1)
        let (_delta, _index, rebuiltGraph) =
              rebuildWithDelta (merge canonicalInput absorbedInput graph2)
            canonicalOutput =
              canonicalizeClassId rebuiltGraph canonicalInput
        IntMap.lookup (classIdKey canonicalOutput) (eGraphAnalysis rebuiltGraph)
              @?= Just [2, 1],
      testCase "index covers all parent-child edges in the database" $ do
        (c1, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
        (c2, graph2) <- expectRight (addTerm (numTerm 2) graph1)
        (_, graph3) <- expectRight (addTerm (addTermNode (numTerm 1) (numTerm 2)) graph2)
        (_, graph4) <- expectRight (addTerm (mulTermNode (numTerm 1) (numTerm 2)) graph3)
        let (_delta, index, _rebuilt) = rebuildWithDelta (merge c1 c2 graph4)
            parentMap = riParents (briIndex index)
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
        let (EGraphRebuildDelta {erdTopologyClassKeys}, _index, _rebuilt) =
              rebuildWithDelta (merge c1 c2 graph6)
        erdTopologyClassKeys @?= IntSet.empty
    , testCase "repair index currently materializes unrelated result tuples beyond the dirty repair slice" $ do
        (c1, graph1) <- expectRight (addTerm (numTerm 1) (emptyEGraph analysisSpec))
        (c2, graph2) <- expectRight (addTerm (numTerm 2) graph1)
        (_, graph3) <- expectRight (addTerm (numTerm 3) graph2)
        (_, graph4) <- expectRight (addTerm (numTerm 4) graph3)
        (_, graph5) <- expectRight (addTerm (numTerm 5) graph4)
        (_, graph6) <- expectRight (addTerm (addTermNode (numTerm 1) (numTerm 3)) graph5)
        (_, graph7) <- expectRight (addTerm (addTermNode (numTerm 2) (numTerm 3)) graph6)
        (unrelatedParent, graph8) <- expectRight (addTerm (addTermNode (numTerm 4) (numTerm 5)) graph7)
        let (EGraphRebuildDelta {erdDirtyResultKeys}, index, _rebuilt) =
              rebuildWithDelta (merge c1 c2 graph8)
            indexedResultKeys =
              IntMap.keysSet (riTuplesByResult (briIndex index))
            unrelatedKey = classIdKey unrelatedParent
        assertBool
                "repair index should still contain the unrelated parent tuple"
                (IntSet.member unrelatedKey indexedResultKeys)
        assertBool
                "dirty repair slice should exclude the unrelated parent"
                (not (IntSet.member unrelatedKey erdDirtyResultKeys))
        assertBool
                "repair index currently materializes a strictly larger set than the dirty repair slice"
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
                let (EGraphRebuildDelta {erdDirtyResultKeys}, repairIndex, rebuiltGraph) =
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
                      IntMap.keysSet (riTuplesByResult (briIndex repairIndex))
                assertBool
                  "dirty repair slice excludes the unrelated add root"
                  (not (IntSet.member unrelatedKey erdDirtyResultKeys))
                assertBool
                  "repair index still contains the unrelated add root"
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
            rowCounts =
              sort
                ( fmap
                    rowBlockCount
                    (IntMap.elems preparedRows)
                )
        sliceCounts @?= [1, 2]
        rowCounts @?= [1, 2]
    ]
