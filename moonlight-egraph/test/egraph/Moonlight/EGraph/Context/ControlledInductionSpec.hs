{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

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
    withEmptyContextEGraph,
    planContextMerges,
    stageContextMerges,
  )
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    cegContextRevision,
  )
import Moonlight.EGraph.Pure.Context.Proof
  ( ProofEGraph,
    emptyProofEGraph,
  )
import Moonlight.EGraph.Pure.Extraction
  ( CostAlgebra (..),
    ExtractionBudgetExhaustion (..),
    ExtractionWorkBudget (..),
    extractBounded,
    stableExtractionSnapshotFromEGraph,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.EGraph.Pure.Rebuild
  ( rebuildTracked,
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

boundedExtractionBudget :: ExtractionWorkBudget
boundedExtractionBudget =
  ExtractionWorkBudget 8

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
  withModuleContextGraph (asGraph seed) $ \contextGraph ->
    case contextualExtractionSectionBounded boundedExtractionBudget ModuleCtx mempty arithCost contextGraph of
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

undershotBudget :: ExtractionWorkBudget
undershotBudget =
  ExtractionWorkBudget 2

boundedContextualExtractionRejectsUndershotBudget :: Assertion
boundedContextualExtractionRejectsUndershotBudget =
  withNegationChainContextGraph $ \(targetClass, contextGraph) ->
    case contextualExtractBounded undershotBudget ModuleCtx mempty arithCost targetClass contextGraph of
      Left (ContextualExtractionBudgetExhausted report) ->
        assertBudgetExhaustionWellFormed undershotBudget report
      Left obstruction ->
        assertFailure ("expected budget obstruction, got " <> show obstruction)
      Right _ ->
        assertFailure "expected bounded contextual extraction to reject"

negationChainContextGraph ::
  (forall owner. (ClassId, ContextEGraph owner ArithF NodeCount Scope) -> result) ->
  Either UnionFindAllocationError result
negationChainContextGraph useContextGraph = do
  (chainClass, dirtyGraph) <-
    addTerm
      (negTermNode (negTermNode oneTerm))
      (emptyEGraph analysisSpec)
  let EGraphMutationResult {emrGraph = stableGraph} =
        rebuildTracked dirtyGraph
  pure (withModuleContextGraph stableGraph (useContextGraph . (,) chainClass))

withNegationChainContextGraph ::
  (forall owner. (ClassId, ContextEGraph owner ArithF NodeCount Scope) -> Assertion) ->
  Assertion
withNegationChainContextGraph useContextGraph =
  either (assertFailure . show) id (negationChainContextGraph useContextGraph)

assertBudgetExhaustionWellFormed ::
  ExtractionWorkBudget ->
  ExtractionBudgetExhaustion ->
  Assertion
assertBudgetExhaustionWellFormed expectedBudget report = do
  ebeBudget report @?= expectedBudget
  ebeConsumedWorkSteps report @?= extractionWorkBudgetSteps expectedBudget
  assertBool "expected at least one extraction class" (ebeTotalClassCount report > 0)
  ebeTotalClassCount report @?= ebeResolvedClassCount report + ebeUnresolvedClassCount report

localMergeTraceRejectedByProofProjection :: Assertion
localMergeTraceRejectedByProofProjection =
  withLocalMergeFixture $ \fixture -> do
    let proofGraph =
          emptyProofEGraph (lmfBefore fixture)
    case proofUpdateFromTrace (lmfTrace fixture) proofGraph of
      Left (ProofTraceProjectionMissingJustification unionKeys) ->
        assertBool "expected proof obstruction to carry local union keys" (not (IntSet.null unionKeys))
      Right _ ->
        assertFailure "local class-union trace must not update proof without rewrite evidence"

localMergeTraceInvalidatesFrontierAndCacheIdentity :: Assertion
localMergeTraceInvalidatesFrontierAndCacheIdentity =
  withLocalMergeFixture $ \fixture -> do
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

data LocalMergeFixture owner = LocalMergeFixture
  { lmfBefore :: !(ContextEGraph owner ArithF NodeCount Scope),
    lmfAfter :: !(ContextEGraph owner ArithF NodeCount Scope),
    lmfTrace :: !(ContextMutationTrace owner Scope ArithF)
  }

withLocalMergeFixture ::
  (forall owner. LocalMergeFixture owner -> Assertion) ->
  Assertion
withLocalMergeFixture useFixture = do
  seed <- requireRight arithmeticSeed
  let graph =
        asGraph seed
      oneClass =
        asOne seed
      twoClass = asTwo seed
  withModuleContextGraph graph $ \contextGraph -> do
    let initialBatch = beginContextRebaseBatch contextGraph
    mergePlan <- requireRight (planContextMerges [ModuleCtx] oneClass twoClass initialBatch)
    mergeBatch <- requireRight (stageContextMerges mergePlan initialBatch)
    (mergeReport, mergedContextGraph) <-
      requireRight (commitContextRebaseBatch mergeBatch)
    useFixture
      LocalMergeFixture
        { lmfBefore = contextGraph,
          lmfAfter = mergedContextGraph,
          lmfTrace = crrTrace mergeReport
        }

withModuleContextGraph ::
  EGraph ArithF NodeCount ->
  (forall owner. ContextEGraph owner ArithF NodeCount Scope -> result) ->
  result
withModuleContextGraph =
  withEmptyContextEGraph inductionScopeLattice

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
