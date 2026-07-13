{-# LANGUAGE TypeApplications #-}

module Moonlight.EGraph.Context.ControlledInductionSpec
  ( tests,
  )
where

import Data.IntSet qualified as IntSet
import Data.Foldable (traverse_)
import Moonlight.Core (UnionFindAllocationError)
import Moonlight.Delta.Scope qualified as Delta
import Moonlight.EGraph.Pure.Change (EGraphMutationResult (..))
import Moonlight.EGraph.Pure.Context
  ( ContextMutationTrace,
    ContextRebaseReport (..),
    beginContextRebaseBatch,
    commitContextRebaseBatch,
    contextMutationTraceTouchedKeys,
    emptyContextEGraph,
    planContextMerges,
    stageContextMerges,
  )
import Moonlight.EGraph.Pure.Context.Core
  ( ContextEGraph,
    cegContextRevision,
  )
import Moonlight.EGraph.Pure.Context.Proof
  ( ProofEGraph,
    emptyProofEGraph,
  )
import Moonlight.EGraph.Pure.Extraction
  ( AnalysisCostAlgebra (..),
    CostAlgebra (..),
    ExtractionConvergenceReport (..),
    ExtractionFixpointBudget (..),
    extractBounded,
    stableExtractionSnapshotFromEGraph,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.EGraph.Pure.Rebuild
  ( equateClassesTracked,
    rebuildTracked,
  )
import Moonlight.EGraph.Pure.Saturation.Apply
  ( ProofTraceProjectionError (..),
    proofUpdateFromTrace,
  )
import Moonlight.EGraph.Pure.Saturation.Extraction
  ( ContextualExtractionObstruction (..),
    ContextualExtractionSection,
    contextualExtractBounded,
    contextualExtractFromSection,
    contextualExtractionSectionBounded,
  )
import Moonlight.EGraph.Pure.Saturation.Matching
  ( matchingDeltaFromContextMutationTrace,
    matchingFrontierFromDelta,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId,
    EGraph,
    emptyEGraph,
  )
import Moonlight.EGraph.Test.Arith.Core
  ( ArithF (..),
    NodeCount,
    addTermNode,
    analysisSpec,
    negTermNode,
    numTerm,
  )
import Moonlight.EGraph.Test.Case (HUnitCase (..), hunitCases)
import Moonlight.EGraph.Test.Context.ThreeLevel (Scope (ModuleCtx))
import Data.Fix (Fix)
import Moonlight.Sheaf.Twist.Cost
  ( CostOverlay,
    guardedCostOverlay,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    (@?=),
  )
import Moonlight.FiniteLattice
  ( ContextLattice,
    latticeContext
  )

tests :: TestTree
tests =
  testGroup "controlled induction" . hunitCases $
    [ HUnitCase "bounded extraction succeeds for monotone tree cost" boundedExtractionSucceeds,
      HUnitCase "bounded contextual extraction section answers multiple point queries" contextualExtractionSectionAnswersPointQueries,
      HUnitCase "bounded contextual extraction obstructs when the budget undershoots the class count" boundedContextualExtractionRejectsUndershotBudget,
      HUnitCase "local merge trace remains proof-obstructed without rewrite evidence" localMergeTraceRejectedByProofProjection,
      HUnitCase "local merge trace widens frontier and changes runtime cache identity" localMergeTraceInvalidatesFrontierAndCacheIdentity
    ]

boundedExtractionBudget :: ExtractionFixpointBudget
boundedExtractionBudget =
  ExtractionFixpointBudget 8

boundedExtractionSucceeds :: Assertion
boundedExtractionSucceeds = do
  seed <- requireRight arithmeticSeed
  let targetClass =
        asOnePlusTwo seed
      graph =
        asGraph seed
   in case stableExtractionSnapshotFromEGraph graph of
        Nothing ->
          assertFailure "expected stable extraction snapshot"
        Just snapshot ->
          case extractBounded boundedExtractionBudget arithCost targetClass snapshot of
            Right (Just _result) ->
              pure ()
            Right Nothing ->
              assertFailure "expected bounded extraction to find the target class"
            Left report ->
              assertFailure ("monotone bounded extraction did not converge: " <> show report)

contextualExtractionSectionAnswersPointQueries :: Assertion
contextualExtractionSectionAnswersPointQueries = do
  seed <- requireRight arithmeticSeed
  let contextGraph =
        moduleContextGraph (asGraph seed)
   in case contextualExtractionSectionBounded boundedExtractionBudget ModuleCtx mempty arithCost contextGraph of
        Left obstruction ->
          assertFailure ("expected contextual extraction section, got " <> show obstruction)
        Right section ->
          traverse_
            (assertSectionHasRepresentative section)
            [asOne seed, asOnePlusTwo seed]

assertSectionHasRepresentative ::
  ContextualExtractionSection ArithF NodeCount Scope Int ->
  ClassId ->
  Assertion
assertSectionHasRepresentative section classId =
  case contextualExtractFromSection classId section of
    Right (_, Just _result) ->
      pure ()
    Right (_, Nothing) ->
      assertFailure "expected section point query to find the target class"
    Left obstruction ->
      assertFailure ("expected section point query, got " <> show obstruction)

undershotBudget :: ExtractionFixpointBudget
undershotBudget =
  ExtractionFixpointBudget 2

boundedContextualExtractionRejectsUndershotBudget :: Assertion
boundedContextualExtractionRejectsUndershotBudget = do
  (targetClass, contextGraph) <- requireRight negationChainContextGraph
  case contextualExtractBounded undershotBudget ModuleCtx mempty arithCost targetClass contextGraph of
        Left (ContextualExtractionDidNotConverge report) ->
          assertConvergenceReportWellFormed undershotBudget report
        Left obstruction ->
          assertFailure ("expected budget obstruction, got " <> show obstruction)
        Right _ ->
          assertFailure "expected bounded contextual extraction to reject"

negationChainContextGraph :: Either UnionFindAllocationError (ClassId, ContextEGraph ArithF NodeCount Scope)
negationChainContextGraph = do
  (chainClass, dirtyGraph) <-
    addTerm
      (negTermNode (negTermNode oneTerm))
      (emptyEGraph analysisSpec)
  let EGraphMutationResult {emrGraph = stableGraph} =
        rebuildTracked Nothing dirtyGraph
  pure (chainClass, moduleContextGraph stableGraph)

assertConvergenceReportWellFormed ::
  ExtractionFixpointBudget ->
  ExtractionConvergenceReport ->
  Assertion
assertConvergenceReportWellFormed expectedBudget report = do
  ecrBudget report @?= expectedBudget
  assertBool "expected at least one extraction class" (ecrTotalClassCount report > 0)
  ecrTotalClassCount report @?= ecrResolvedClassCount report + ecrUnresolvedClassCount report

localMergeTraceRejectedByProofProjection :: Assertion
localMergeTraceRejectedByProofProjection = do
  fixture <- localMergeFixture
  let proofGraph =
        emptyProofEGraph (lmfBefore fixture) :: ProofEGraph ArithF NodeCount Scope ()
  case proofUpdateFromTrace (lmfTrace fixture) proofGraph of
    Left (ProofTraceProjectionMissingJustification unionKeys) ->
      assertBool "expected proof obstruction to carry local union keys" (not (IntSet.null unionKeys))
    Right _ ->
      assertFailure "local class-union trace must not update proof without rewrite evidence"

localMergeTraceInvalidatesFrontierAndCacheIdentity :: Assertion
localMergeTraceInvalidatesFrontierAndCacheIdentity = do
  fixture <- localMergeFixture
  let touchedKeys =
        contextMutationTraceTouchedKeys (lmfTrace fixture)
      frontierKeys =
        Delta.scopeKeys
          ( matchingFrontierFromDelta
              (matchingDeltaFromContextMutationTrace (lmfTrace fixture))
          )
      beforeRevision =
        cegContextRevision (lmfBefore fixture)
      afterRevision =
        cegContextRevision (lmfAfter fixture)
  assertBool "expected local merge trace to touch class keys" (not (IntSet.null touchedKeys))
  assertBool
    "expected matching frontier to cover local merge touched keys"
    (maybe True (touchedKeys `IntSet.isSubsetOf`) frontierKeys)
  assertBool
    "expected local context merge to bump context revision"
    (afterRevision > beforeRevision)

data LocalMergeFixture = LocalMergeFixture
  { lmfBefore :: !(ContextEGraph ArithF NodeCount Scope),
    lmfAfter :: !(ContextEGraph ArithF NodeCount Scope),
    lmfTrace :: !(ContextMutationTrace Scope ArithF)
  }

localMergeFixture :: IO LocalMergeFixture
localMergeFixture = do
  seed <- requireRight arithmeticSeed
  let graph =
        asGraph seed
      oneClass =
        asOne seed
      twoClass =
        asTwo seed
      contextGraph =
        moduleContextGraph graph
  let initialBatch = beginContextRebaseBatch contextGraph
  mergePlan <- requireRight (planContextMerges [ModuleCtx] oneClass twoClass initialBatch)
  mergeBatch <- requireRight (stageContextMerges mergePlan initialBatch)
  (mergeReport, mergedContextGraph) <-
    requireRight (commitContextRebaseBatch mergeBatch)
  pure
    LocalMergeFixture
      { lmfBefore = contextGraph,
        lmfAfter = mergedContextGraph,
        lmfTrace = crrTrace mergeReport
      }

moduleContextGraph :: EGraph ArithF NodeCount -> ContextEGraph ArithF NodeCount Scope
moduleContextGraph =
  emptyContextEGraph inductionScopeLattice

inductionScopeLattice :: ContextLattice Scope
inductionScopeLattice =
  case latticeContext of
    Right latticeValue -> latticeValue
    Left compileError ->
      error ("invalid controlled-induction Scope lattice fixture: " <> show compileError)

requireRight :: Show errorValue => Either errorValue result -> IO result
requireRight resultValue =
  case resultValue of
    Left errorValue ->
      assertFailure ("expected Right, got " <> show errorValue)
    Right result ->
      pure result

data ArithmeticSeed = ArithmeticSeed
  { asOne :: !ClassId,
    asTwo :: !ClassId,
    asOnePlusTwo :: !ClassId,
    asGraph :: !(EGraph ArithF NodeCount)
  }

arithmeticSeed :: Either UnionFindAllocationError ArithmeticSeed
arithmeticSeed = do
  (oneClass, graph1) <-
    addTerm oneTerm (emptyEGraph analysisSpec)
  (twoClass, graph2) <-
    addTerm twoTerm graph1
  (sumClass, graph3) <-
    addTerm onePlusTwoTerm graph2
  pure
    ArithmeticSeed
      { asOne = oneClass,
        asTwo = twoClass,
        asOnePlusTwo = sumClass,
        asGraph = graph3
      }

oneTerm :: Fix ArithF
oneTerm =
  numTerm 1

twoTerm :: Fix ArithF
twoTerm =
  numTerm 2

onePlusTwoTerm :: Fix ArithF
onePlusTwoTerm =
  addTermNode oneTerm twoTerm

arithCost :: CostAlgebra ArithF Int
arithCost =
  CostAlgebra $
    \arithNode ->
      case arithNode of
        Num _ ->
          1
        Var _ ->
          1
        Add leftCost rightCost ->
          leftCost + rightCost + 1
        Mul leftCost rightCost ->
          leftCost + rightCost + 1
        Neg childCost ->
          childCost + 1
