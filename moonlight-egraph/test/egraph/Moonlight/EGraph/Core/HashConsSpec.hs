{-# LANGUAGE DerivingStrategies #-}

module Moonlight.EGraph.Core.HashConsSpec
  ( tests,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import Control.Monad (foldM)
import Moonlight.Core (UnionFindAllocationError)
import Moonlight.Core (emptyTheorySpec)
import Moonlight.Core (emptyUnionFind, find)
import Moonlight.EGraph.Pure.Delta (eGraphEditDeltaClassUnions, emptyEGraphEditDelta)
import Moonlight.EGraph.Pure.Change
  ( EGraphMutationResult (..),
    EGraphMutationTrace (..),
    appendEGraphMutationTrace,
    emptyEGraphMutationTrace,
    observedClassUnionPairs,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons
  ( canonicalizeENode,
    insertENodeTracked,
    insertTermTracked,
    insertTermsTracked,
    lookupENodeAll,
    lookupLeastENode,
  )
import Moonlight.EGraph.Pure.Rebuild (merge, rebuild)
import Moonlight.EGraph.Pure.Structural.Store
  ( StructuralLookup (..),
    StructuralStore,
    emptyStructuralStore,
    insertCanonicalTuple,
    seStore,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    ENode (ENode),
    eGraphClassCount,
    eGraphRevision,
    eGraphUnionFind,
    initialEGraphRevision,
  )
import Moonlight.EGraph.Pure.Types.Internal (EGraph (..))
import Moonlight.EGraph.Test.Arith.Core (ArithF (Add), NodeCount (..), analysisSpec)
import Moonlight.EGraph.Test.Arith.Fixture
  ( emptyArithGraph,
    insertArith,
    one,
    onePlusTwo,
    seedArith,
    seedArithPair,
    two,
  )
import Data.Fix (Fix)
import Moonlight.Pale.Test.Site.Assertion (expectRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

data MutationTraceProjection f = MutationTraceProjection
  { mtpRevisionBefore :: !String,
    mtpRevisionAfter :: !String,
    mtpPhaseBefore :: !String,
    mtpPhaseAfter :: !String,
    mtpTouchedClassKeys :: ![Int],
    mtpInsertedClassKeys :: ![Int],
    mtpAnalysisChangedKeys :: ![Int],
    mtpObservedClassUnions :: ![(ClassId, ClassId)],
    mtpRebuildTraceCount :: !Int
  }
  deriving stock (Eq, Show)

tests :: TestTree
tests =
  testGroup
    "hash-cons"
    [ testCase "addTerm interns identical structure once" $ do
        (firstClassId, graph1) <- expectRight (seedArith onePlusTwo)
        (secondClassId, graph2) <- expectRight (insertArith onePlusTwo graph1)
        secondClassId @?= firstClassId
        eGraphClassCount graph2 @?= eGraphClassCount graph1,
      testCase "insertTermsTracked is equivalent to folded insertTermTracked" $ do
        let terms =
              [ one,
                two,
                onePlusTwo,
                onePlusTwo
              ]
        EGraphMutationResult
          { emrResult = batchedClassIds,
            emrTrace = batchedTrace,
            emrGraph = batchedGraph
          } <- expectRight (insertTermsTracked terms emptyArithGraph)
        (foldedGraph, foldedSteps) <-
          expectRight (foldM insertOne (emptyArithGraph, []) terms)
        let foldedClassIds =
              fmap fst foldedSteps
            foldedTrace =
              foldl'
                appendEGraphMutationTrace
                (emptyEGraphMutationTrace emptyArithGraph)
                (fmap snd foldedSteps)
        batchedClassIds @?= foldedClassIds
        eGraphClassCount batchedGraph @?= eGraphClassCount foldedGraph
        traceProjection batchedTrace @?= traceProjection foldedTrace,
      testCase "addTerm advances the e-graph revision" $ do
        (_, graph1) <- expectRight (seedArith one)
        eGraphRevision emptyArithGraph @?= initialEGraphRevision
        eGraphRevision graph1 > eGraphRevision emptyArithGraph @?= True,
      testCase "canonicalizeENode normalizes merged children" $ do
        (leftLeafClass, rightLeafClass, graph) <- expectRight (seedArithPair one two)
        let mergedGraph = rebuild (merge leftLeafClass rightLeafClass graph)
            (canonicalLeafClass, canonicalUnionFind) = find leftLeafClass (eGraphUnionFind mergedGraph)
            (canonicalENode, _) =
              canonicalizeENode
                (ENode (Add leftLeafClass rightLeafClass))
                canonicalUnionFind
        canonicalENode @?= ENode (Add canonicalLeafClass canonicalLeafClass),
      testCase "lookupENodeAll exposes ambiguous structural owners" $
        lookupENodeAll duplicateStructuralNode graphWithDuplicateStructuralOwners
          @?= StructuralAmbiguous (ClassId 3 :| [ClassId 5]),
      testCase "lookupLeastENode names the old least-owner policy" $
        let graph =
              graphWithDuplicateStructuralOwners
         in lookupLeastENode (ENode (Add (ClassId 10) (ClassId 20))) graph
              @?= Just (ClassId 3),
      testCase "insertENodeTracked stages ambiguous structural owners as class unions" $ do
        EGraphMutationResult {emrGraph = updatedGraph} <-
          expectRight
            ( insertENodeTracked
                (ENode (Add (ClassId 10) (ClassId 20)))
                (NodeCount 3)
                graphWithDuplicateStructuralOwners
            )
        eGraphEditDeltaClassUnions (egPendingDelta updatedGraph)
          @?= [(ClassId 3, ClassId 5)]
    ]

graphWithDuplicateStructuralOwners :: EGraph ArithF NodeCount
graphWithDuplicateStructuralOwners =
  EGraph
    { egUnionFind = emptyUnionFind,
      egStore = duplicateStructuralOwnerStore,
      egAnalysis = IntMap.empty,
      egPendingDelta = emptyEGraphEditDelta,
      egAnalysisSpec = analysisSpec,
      egTheorySpec = emptyTheorySpec,
      egRevision = initialEGraphRevision
    }

duplicateStructuralOwnerStore :: StructuralStore ArithF
duplicateStructuralOwnerStore =
  seStore
    ( insertCanonicalTuple
        (ClassId 3)
        duplicateStructuralNode
        ( seStore
            ( insertCanonicalTuple
                (ClassId 5)
                duplicateStructuralNode
                emptyStructuralStore
            )
        )
    )

duplicateStructuralNode :: ENode ArithF
duplicateStructuralNode =
  ENode (Add (ClassId 10) (ClassId 20))

insertOne ::
  (EGraph ArithF NodeCount, [(ClassId, EGraphMutationTrace ArithF)]) ->
  Fix ArithF ->
  Either UnionFindAllocationError (EGraph ArithF NodeCount, [(ClassId, EGraphMutationTrace ArithF)])
insertOne (graph, reversedSteps) termValue = do
  EGraphMutationResult
    { emrResult = classId,
      emrTrace = traceValue,
      emrGraph = nextGraph
    } <- insertTermTracked termValue graph
  pure (nextGraph, reversedSteps <> [(classId, traceValue)])

traceProjection ::
  EGraphMutationTrace f ->
  MutationTraceProjection f
traceProjection traceValue =
  MutationTraceProjection
    { mtpRevisionBefore = show (emtRevisionBefore traceValue),
      mtpRevisionAfter = show (emtRevisionAfter traceValue),
      mtpPhaseBefore = show (emtPhaseBefore traceValue),
      mtpPhaseAfter = show (emtPhaseAfter traceValue),
      mtpTouchedClassKeys = IntSet.toAscList (emtTouchedClassKeys traceValue),
      mtpInsertedClassKeys = IntSet.toAscList (emtInsertedClassKeys traceValue),
      mtpAnalysisChangedKeys = IntSet.toAscList (emtAnalysisChangedKeys traceValue),
      mtpObservedClassUnions = observedClassUnionPairs (emtObservedClassUnions traceValue),
      mtpRebuildTraceCount = length (emtRebuildTraces traceValue)
    }
