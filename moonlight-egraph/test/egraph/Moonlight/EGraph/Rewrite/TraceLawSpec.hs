{-# LANGUAGE RankNTypes #-}

module Moonlight.EGraph.Rewrite.TraceLawSpec
  ( tests,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Set qualified as Set
import Moonlight.EGraph.Pure.Change
  ( EGraphMutationResult (..),
    EGraphMutationTrace (..),
    GraphPhase,
    appendEGraphMutationTrace,
    emptyEGraphMutationTrace,
    observedClassUnionPairs,
  )
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    appendContextMutationTrace,
    beginContextRebaseBatch,
    commitContextRebaseBatch,
    ContextMutationTrace (..),
    contextMutationTraceFromBase,
    ContextRebaseReport (..),
    withEmptyContextEGraph,
    planContextMerges,
    stageContextMerges,
    stageTermAtContext,
  )
import Moonlight.EGraph.Pure.Context.Proof
  ( ProofEGraph,
    emptyProofEGraph,
  )
import Moonlight.EGraph.Pure.Extraction
  ( AnalysisCostAlgebra,
    ExtractionCacheObstruction,
    ExtractionChoiceCache,
    ExtractionWorkBudget (..),
    ExtractionResult,
    advanceExtractionChoiceCache,
    eccRevision,
    extractAllCached,
    extractionChoiceCacheFromStableGraph,
    liftCostAlgebra,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons
  ( insertTermTracked,
  )
import Moonlight.EGraph.Pure.Rebuild
  ( EGraphRebuildDelta (..),
    equateClassesTracked,
  )
import Moonlight.EGraph.Pure.Saturation.Substrate
  ( EGraphSaturationChangeSummary (..),
    eGraphSaturationChangeTrace,
  )
import Moonlight.Sheaf.Context.Site (UnitContextSiteOwner)
import Moonlight.EGraph.Pure.Saturation.Apply
  ( ProofTraceProjectionError (..),
    proofUpdateFromTrace,
  )
import Moonlight.EGraph.Pure.Saturation.Matching
  ( matchingDeltaFromContextMutationTrace,
    matchingDeltaFromMutationTrace,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId,
    EGraphRevision,
    classIdKey,
    emptyEGraph,
  )
import Moonlight.EGraph.Test.Arith.Core
  ( ArithF,
    NodeCount,
    analysisSpec,
    numTerm,
  )
import Moonlight.EGraph.Test.Arith.Cost
  ( arithCost,
  )
import Moonlight.EGraph.Test.Context.ThreeLevel
  ( Scope (..),
  )
import Moonlight.EGraph.Test.Context.ThreeLevelArith
  ( fixtureContextGraph,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    (@?=),
    assertBool,
    assertFailure,
    testCase,
  )
import Moonlight.FiniteLattice
  ( ContextLattice,
    latticeContext
  )

withFixtureContextGraph ::
  (forall owner. (ClassId, ClassId, ContextEGraph owner ArithF NodeCount Scope) -> Assertion) ->
  Assertion
withFixtureContextGraph useFixture =
  either (assertFailure . show) id (fixtureContextGraph useFixture)

tests :: TestTree
tests =
  testGroup
    "trace law"
    [ testCase "matching projection is a mutation-trace homomorphism" $ do
        let graph0 = emptyEGraph analysisSpec
        EGraphMutationResult
          { emrTrace = firstTrace,
            emrGraph = graph1
          } <- expectRight (insertTermTracked (numTerm 1) graph0)
        EGraphMutationResult
          { emrTrace = secondTrace
          } <- expectRight (insertTermTracked (numTerm 2) graph1)
        let appendedTrace = appendEGraphMutationTrace firstTrace secondTrace
        matchingDeltaFromMutationTrace appendedTrace
          @?= matchingDeltaFromMutationTrace firstTrace <> matchingDeltaFromMutationTrace secondTrace,
      testCase "matching projection is a context-trace homomorphism" $ withFixtureContextGraph $ \(sumClassId, oneClassId, contextGraph0) -> do
        let initialBatch = beginContextRebaseBatch contextGraph0
        mergePlan <- expectRight (planContextMerges [ModuleCtx] sumClassId oneClassId initialBatch)
        firstBatch <- expectRight (stageContextMerges mergePlan initialBatch)
        (firstReport, contextGraph1) <- expectRight (commitContextRebaseBatch firstBatch)
        (_localClassId, secondBatch) <- expectRight (stageTermAtContext LocalCtx (numTerm 7) (beginContextRebaseBatch contextGraph1))
        (secondReport, _contextGraph2) <- expectRight (commitContextRebaseBatch secondBatch)
        let firstTrace = crrTrace firstReport
            secondTrace = crrTrace secondReport
            appendedTrace = appendContextMutationTrace firstTrace secondTrace
        matchingDeltaFromContextMutationTrace appendedTrace
          @?= matchingDeltaFromContextMutationTrace firstTrace <> matchingDeltaFromContextMutationTrace secondTrace,
      testCase "saturation summary preserves application trace lineage order" $ withFixtureContextGraph $ \(sumClassId, oneClassId, contextGraph0) -> do
        let initialBatch = beginContextRebaseBatch contextGraph0
        mergePlan <- expectRight (planContextMerges [ModuleCtx] sumClassId oneClassId initialBatch)
        firstBatch <- expectRight (stageContextMerges mergePlan initialBatch)
        (firstReport, contextGraph1) <- expectRight (commitContextRebaseBatch firstBatch)
        (_localClassId, secondBatch) <- expectRight (stageTermAtContext LocalCtx (numTerm 7) (beginContextRebaseBatch contextGraph1))
        (secondReport, contextGraph2) <- expectRight (commitContextRebaseBatch secondBatch)
        let expectedTrace =
              appendContextMutationTrace (crrTrace firstReport) (crrTrace secondReport)
            summary =
              EGraphSaturationChangeSummary
                { egscApplicationTraces = [crrTrace firstReport, crrTrace secondReport],
                  egscRebuildDeltas = [],
                  egscProofRestrictionRegistryConstructions = 0,
                  egscProofExtractionTableConstructions = 0
                }
            traceValue =
              eGraphSaturationChangeTrace contextGraph0 contextGraph2 summary
        emtRevisionBefore (cmtBaseTrace traceValue) @?= emtRevisionBefore (cmtBaseTrace expectedTrace)
        emtRevisionAfter (cmtBaseTrace traceValue) @?= emtRevisionAfter (cmtBaseTrace expectedTrace)
        matchingDeltaFromContextMutationTrace traceValue @?= matchingDeltaFromContextMutationTrace expectedTrace,
      testCase "saturation summary projects rebuild dirty keys to context fibers" $ withFixtureContextGraph $ \(_sumClassId, _oneClassId, contextGraph0) -> do
        (localClassId, localBatch) <- expectRight (stageTermAtContext LocalCtx (numTerm 7) (beginContextRebaseBatch contextGraph0))
        (_localReport, contextGraph1) <- expectRight (commitContextRebaseBatch localBatch)
        let localKey =
              classIdKey localClassId
            summary =
              EGraphSaturationChangeSummary
                { egscApplicationTraces = [],
                  egscRebuildDeltas =
                    [ EGraphRebuildDelta
                        { erdImpactedClassKeys = IntSet.singleton localKey,
                          erdDirtyResultKeys = IntSet.empty,
                          erdTopologyClassKeys = IntSet.empty
                        }
                    ],
                  egscProofRestrictionRegistryConstructions = 0,
                  egscProofExtractionTableConstructions = 0
                }
            traceValue =
              eGraphSaturationChangeTrace contextGraph0 contextGraph1 summary
        cmtContextTouchedKeys traceValue @?= IntSet.singleton localKey
        cmtDirtyContexts traceValue @?= Set.singleton LocalCtx,
      testCase "saturation summary adds proof artifact construction counts" $
        let firstSummary :: EGraphSaturationChangeSummary UnitContextSiteOwner Scope ArithF
            firstSummary =
              mempty
                { egscProofRestrictionRegistryConstructions = 2,
                  egscProofExtractionTableConstructions = 3
                }
            secondSummary :: EGraphSaturationChangeSummary UnitContextSiteOwner Scope ArithF
            secondSummary =
              mempty
                { egscProofRestrictionRegistryConstructions = 5,
                  egscProofExtractionTableConstructions = 7
                }
            combinedSummary = firstSummary <> secondSummary
         in do
              egscProofRestrictionRegistryConstructions combinedSummary @?= 7
              egscProofExtractionTableConstructions combinedSummary @?= 10,
      testCase "extraction cache advance composes over appended traces" $ do
        let graph0 = emptyEGraph analysisSpec
        EGraphMutationResult
          { emrTrace = firstTrace,
            emrGraph = graph1
          } <- expectRight (insertTermTracked (numTerm 1) graph0)
        EGraphMutationResult
          { emrTrace = secondTrace,
            emrGraph = graph2
          } <- expectRight (insertTermTracked (numTerm 2) graph1)
        let appendedTrace = appendEGraphMutationTrace firstTrace secondTrace
        cacheValue <- expectCache (extractionChoiceCacheFromStableGraph cacheBudget arithChoiceAlgebra graph0)
        let appendedAdvance =
              advanceExtractionChoiceCache cacheBudget appendedTrace graph2 cacheValue
            sequentialAdvance =
              advanceExtractionChoiceCache cacheBudget firstTrace graph1 cacheValue
                >>= advanceExtractionChoiceCache cacheBudget secondTrace graph2
        assertBool
          "appended extraction cache advance must equal sequential advance"
          (fmap extractionChoiceCacheView appendedAdvance == fmap extractionChoiceCacheView sequentialAdvance),
      testCase "empty extraction traces preserve caches under append" $ do
        let graph0 = emptyEGraph analysisSpec
            emptyTrace = emptyEGraphMutationTrace graph0
            appendedTrace = appendEGraphMutationTrace emptyTrace emptyTrace
        cacheValue <- expectCache (extractionChoiceCacheFromStableGraph cacheBudget arithChoiceAlgebra graph0)
        advancedCache <- expectCache (advanceExtractionChoiceCache cacheBudget appendedTrace graph0 cacheValue)
        assertBool
          "empty appended trace advance must preserve every cached extraction"
          (extractionChoiceCacheView advancedCache == extractionChoiceCacheView cacheValue),
      testCase "mutation trace append obeys identity and associativity" $ do
        let graph0 = emptyEGraph analysisSpec
        EGraphMutationResult
          { emrTrace = firstTrace,
            emrGraph = graph1
          } <- expectRight (insertTermTracked (numTerm 1) graph0)
        EGraphMutationResult
          { emrTrace = secondTrace,
            emrGraph = graph2
          } <- expectRight (insertTermTracked (numTerm 2) graph1)
        EGraphMutationResult
          { emrTrace = thirdTrace
          } <- expectRight (insertTermTracked (numTerm 3) graph2)
        traceSummary (appendEGraphMutationTrace (emptyEGraphMutationTrace graph0) firstTrace)
          @?= traceSummary firstTrace
        traceSummary (appendEGraphMutationTrace firstTrace (emptyEGraphMutationTrace graph1))
          @?= traceSummary firstTrace
        traceSummary (appendEGraphMutationTrace (appendEGraphMutationTrace firstTrace secondTrace) thirdTrace)
          @?= traceSummary (appendEGraphMutationTrace firstTrace (appendEGraphMutationTrace secondTrace thirdTrace)),
      testCase "proof projection accepts insertions and obstructs proofless unions" $ do
        let graph0 = emptyEGraph analysisSpec
        EGraphMutationResult
          { emrResult = firstClass,
            emrTrace = insertTrace,
            emrGraph = graph1
          } <- expectRight (insertTermTracked (numTerm 1) graph0)
        EGraphMutationResult
          { emrResult = secondClass,
            emrGraph = graph2
          } <- expectRight (insertTermTracked (numTerm 2) graph1)
        let EGraphMutationResult
              { emrTrace = mergeTrace
              } = equateClassesTracked firstClass secondClass graph2
        withEmptyContextEGraph traceScopeLattice graph2 $ \contextGraph -> do
          let proofGraph = emptyProofEGraph contextGraph
          case proofUpdateFromTrace (contextMutationTraceFromBase insertTrace) proofGraph of
            Right _ -> pure ()
            Left proofError -> assertFailure ("insert trace should not need proof justification, got " <> show proofError)
          case proofUpdateFromTrace (contextMutationTraceFromBase mergeTrace) proofGraph of
            Left (ProofTraceProjectionMissingJustification unionKeys) ->
              assertBool "expected proofless union obstruction keys" (not (IntSet.null unionKeys))
            Right _ ->
              assertFailure "proofless union trace must not update proof"
    ]

traceScopeLattice :: ContextLattice Scope
traceScopeLattice =
  case latticeContext of
    Right latticeValue -> latticeValue
    Left compileError ->
      error ("invalid trace-law Scope lattice fixture: " <> show compileError)

expectRight :: Show errorValue => Either errorValue result -> IO result
expectRight resultValue =
  case resultValue of
    Right value -> pure value
    Left errorValue -> assertFailure ("expected Right, got " <> show errorValue)

cacheBudget :: ExtractionWorkBudget
cacheBudget =
  ExtractionWorkBudget 1024

arithChoiceAlgebra :: AnalysisCostAlgebra ArithF NodeCount Int
arithChoiceAlgebra =
  liftCostAlgebra arithCost

expectCache :: Either ExtractionCacheObstruction result -> IO result
expectCache =
  either (assertFailure . ("expected extraction choice cache, got obstruction: " <>) . show) pure

extractionChoiceCacheView ::
  ExtractionChoiceCache ArithF NodeCount Int ->
  (EGraphRevision, IntMap.IntMap (ExtractionResult ArithF Int))
extractionChoiceCacheView cacheValue =
  (eccRevision cacheValue, extractAllCached cacheValue)

traceSummary ::
  EGraphMutationTrace f ->
  ( EGraphRevision,
    EGraphRevision,
    GraphPhase,
    GraphPhase,
    IntSet.IntSet,
    IntSet.IntSet,
    IntSet.IntSet,
    [(ClassId, ClassId)],
    Int
  )
traceSummary traceValue =
  ( emtRevisionBefore traceValue,
    emtRevisionAfter traceValue,
    emtPhaseBefore traceValue,
    emtPhaseAfter traceValue,
    emtTouchedClassKeys traceValue,
    emtInsertedClassKeys traceValue,
    emtAnalysisChangedKeys traceValue,
    observedClassUnionPairs (emtObservedClassUnions traceValue),
    length (emtRebuildTraces traceValue)
  )
