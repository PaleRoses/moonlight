{-# LANGUAGE RankNTypes #-}

module Moonlight.EGraph.Context.ContextualSectionCacheSpec
  ( tests,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    activateContext,
    beginContextRebaseBatch,
    commitContextRebaseBatch,
    planContextMerges,
    stageContextMerges,
    stageTermAtContext,
  )
import Moonlight.EGraph.Pure.Context
  ( ambientRepresentativeAnalysisValuesFor,
    cegBase,
  )
import Moonlight.EGraph.Pure.Extraction
  ( AnalysisCostAlgebra,
    ExtractionResult,
    ExtractionWorkBudget (..),
    extractAllFromChoiceSection,
    liftCostAlgebra,
  )
import Moonlight.EGraph.Pure.Saturation.Extraction
  ( ContextScope (..),
    ContextualSectionCache,
    ContextualSectionObstruction (..),
    advanceContextualSections,
    cesChoiceSection,
    contextualSectionCacheBounded,
    cscSections,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId,
    eGraphAnalysis,
  )
import Moonlight.EGraph.Test.Arith.Core
  ( ArithF,
    NodeCount,
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
    fixtureModuleMergedContextGraph,
  )
import Moonlight.Sheaf.Twist.Cost
  ( CostOverlay,
  )
import System.Mem.StableName
  ( eqStableName,
    makeStableName,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "contextual section cache"
    [ testCase "advance equals fresh build" advanceEqualsFreshBuild,
      testCase "composition across two commits" compositionAcrossTwoCommits,
      testCase "foreign lineage obstructs" foreignLineageObstructs,
      testCase "context-local merge keeps the untouched fiber verbatim" contextLocalMergeKeepsUntouchedFiberVerbatim,
      QC.testProperty
        "cached and ephemeral representative analysis agree on the requested base-key set"
        cachedAndEphemeralRepresentativeAnalysisAgree
    ]

type CacheView = Map Scope (IntMap (ExtractionResult ArithF Int))

cacheBudget :: ExtractionWorkBudget
cacheBudget =
  ExtractionWorkBudget 4096

contextScope :: ContextScope Scope
contextScope =
  Objects (Set.fromList [GlobalCtx, ModuleCtx, LocalCtx])

costOverlay :: CostOverlay Scope (AnalysisCostAlgebra ArithF NodeCount Int)
costOverlay =
  mempty

costAlgebraValue :: AnalysisCostAlgebra ArithF NodeCount Int
costAlgebraValue =
  liftCostAlgebra arithCost

cacheView ::
  ContextualSectionCache ArithF NodeCount Scope Int ->
  CacheView
cacheView =
  fmap (extractAllFromChoiceSection . cesChoiceSection) . cscSections

advanceEqualsFreshBuild :: Assertion
advanceEqualsFreshBuild = withFixtureContextGraph $ \sumClassId oneClassId contextGraph0 -> do
  let initialBatch = beginContextRebaseBatch contextGraph0
  cache0 <- expectRight (contextualSectionCacheBounded cacheBudget contextScope costOverlay costAlgebraValue contextGraph0)
  mergePlan <- expectRight (planContextMerges [ModuleCtx] sumClassId oneClassId initialBatch)
  firstBatch <-
    expectRight (stageContextMerges mergePlan initialBatch)
  (report1, contextGraph1) <-
    expectRight (commitContextRebaseBatch firstBatch)
  advancedCache <-
    expectRight
      (advanceContextualSections cacheBudget contextScope report1 contextGraph1 cache0)
  freshCache <-
    expectRight
      (contextualSectionCacheBounded cacheBudget contextScope costOverlay costAlgebraValue contextGraph1)
  assertCacheViewsEqual advancedCache freshCache

compositionAcrossTwoCommits :: Assertion
compositionAcrossTwoCommits = withFixtureContextGraph $ \sumClassId oneClassId contextGraph0 -> do
  let initialBatch = beginContextRebaseBatch contextGraph0
  cache0 <- expectRight (contextualSectionCacheBounded cacheBudget contextScope costOverlay costAlgebraValue contextGraph0)
  mergePlan <- expectRight (planContextMerges [ModuleCtx] sumClassId oneClassId initialBatch)
  firstBatch <-
    expectRight (stageContextMerges mergePlan initialBatch)
  (report1, contextGraph1) <-
    expectRight (commitContextRebaseBatch firstBatch)
  advancedCache1 <-
    expectRight
      (advanceContextualSections cacheBudget contextScope report1 contextGraph1 cache0)
  (_localClassId, secondBatch) <-
    expectRight
      (stageTermAtContext LocalCtx (numTerm 7) (beginContextRebaseBatch contextGraph1))
  (report2, contextGraph2) <-
    expectRight (commitContextRebaseBatch secondBatch)
  advancedCache2 <-
    expectRight
      (advanceContextualSections cacheBudget contextScope report2 contextGraph2 advancedCache1)
  freshCache <-
    expectRight
      (contextualSectionCacheBounded cacheBudget contextScope costOverlay costAlgebraValue contextGraph2)
  assertCacheViewsEqual advancedCache2 freshCache

foreignLineageObstructs :: Assertion
foreignLineageObstructs = withFixtureContextGraph $ \sumClassId oneClassId contextGraph0 -> do
  let initialBatch = beginContextRebaseBatch contextGraph0
  cache0 <- expectRight (contextualSectionCacheBounded cacheBudget contextScope costOverlay costAlgebraValue contextGraph0)
  mergePlan <- expectRight (planContextMerges [ModuleCtx] sumClassId oneClassId initialBatch)
  firstBatch <-
    expectRight (stageContextMerges mergePlan initialBatch)
  (_report1, contextGraph1) <-
    expectRight (commitContextRebaseBatch firstBatch)
  (_localClassId, secondBatch) <-
    expectRight
      (stageTermAtContext LocalCtx (numTerm 7) (beginContextRebaseBatch contextGraph1))
  (report2, contextGraph2) <-
    expectRight (commitContextRebaseBatch secondBatch)
  case advanceContextualSections cacheBudget contextScope report2 contextGraph2 cache0 of
    Left (ContextualSectionContextLineageMismatch _ _) ->
      pure ()
    Left obstruction ->
      assertFailure ("expected lineage mismatch, got " <> show obstruction)
    Right _cacheValue ->
      assertFailure "expected lineage mismatch, got Right"

-- The locality witness the maintained cache exists to provide: a merge staged
-- inside @ModuleCtx@ is invisible to the base, so @GlobalCtx@ (which sits below
-- @ModuleCtx@ in the scope lattice and inherits none of its unions) keeps its
-- section as the /same heap object/.  @ModuleCtx@ — the merged scope — and
-- @LocalCtx@ — which sits above it and inherits the union — both recompute.
-- The advance==fresh agreement pins soundness; the verbatim/recompute split
-- pins that we do not dirty every context.  (Base mutations, by contrast, are
-- shared across all fibers and force a global recompute — see
-- 'advanceContextualSections'.)
contextLocalMergeKeepsUntouchedFiberVerbatim :: Assertion
contextLocalMergeKeepsUntouchedFiberVerbatim = withFixtureContextGraph $ \sumClassId oneClassId contextGraph0 -> do
  let initialBatch = beginContextRebaseBatch contextGraph0
  cache0 <- expectRight (contextualSectionCacheBounded cacheBudget contextScope costOverlay costAlgebraValue contextGraph0)
  mergePlan <- expectRight (planContextMerges [ModuleCtx] sumClassId oneClassId initialBatch)
  mergeBatch <-
    expectRight (stageContextMerges mergePlan initialBatch)
  (report1, contextGraph1) <-
    expectRight (commitContextRebaseBatch mergeBatch)
  advancedCache <-
    expectRight
      (advanceContextualSections cacheBudget contextScope report1 contextGraph1 cache0)
  freshCache <-
    expectRight
      (contextualSectionCacheBounded cacheBudget contextScope costOverlay costAlgebraValue contextGraph1)
  assertCacheViewsEqual advancedCache freshCache
  globalShared <- sectionShared GlobalCtx cache0 advancedCache
  moduleShared <- sectionShared ModuleCtx cache0 advancedCache
  localShared <- sectionShared LocalCtx cache0 advancedCache
  assertBool "global fiber kept verbatim when only the module scope is merged" globalShared
  assertBool "module fiber recomputed because it is the merged scope" (not moduleShared)
  assertBool "local fiber recomputed because it inherits the module merge" (not localShared)

assertCacheViewsEqual ::
  ContextualSectionCache ArithF NodeCount Scope Int ->
  ContextualSectionCache ArithF NodeCount Scope Int ->
  Assertion
assertCacheViewsEqual leftCache rightCache =
  assertBool
    "advanced contextual extraction cache must equal a fresh build"
    (cacheView leftCache == cacheView rightCache)

cachedAndEphemeralRepresentativeAnalysisAgree :: Scope -> QC.Property
cachedAndEphemeralRepresentativeAnalysisAgree contextValue =
  case
      fixtureModuleMergedContextGraph $ \(_, _, ephemeralGraph) ->
        QC.forAll
          (QC.sublistOf (IntMap.keys (eGraphAnalysis (cegBase ephemeralGraph))))
          (cachedAndEphemeralAgreeForKeys contextValue ephemeralGraph)
    of
    Left contextError ->
      QC.counterexample ("context fixture construction failed: " <> show contextError) False
    Right propertyValue ->
      propertyValue

withFixtureContextGraph ::
  (forall owner. ClassId -> ClassId -> ContextEGraph owner ArithF NodeCount Scope -> Assertion) ->
  Assertion
withFixtureContextGraph useFixture =
  expectRight
    ( fixtureContextGraph $ \(sumClassId, oneClassId, contextGraph) ->
        useFixture sumClassId oneClassId contextGraph
    )
    >>= id

cachedAndEphemeralAgreeForKeys ::
  Scope ->
  ContextEGraph owner ArithF NodeCount Scope ->
  [Int] ->
  QC.Property
cachedAndEphemeralAgreeForKeys contextValue ephemeralGraph requestedKeys =
  case activateContext contextValue ephemeralGraph of
    Left supportError ->
      QC.counterexample ("context activation failed: " <> show supportError) False
    Right cachedGraph ->
      QC.counterexample
        ( "cached and ephemeral analysis diverged for "
            <> show contextValue
            <> " at base keys "
            <> show requestedKeys
        )
        ( ambientRepresentativeAnalysisValuesFor contextValue requestedBaseKeys cachedGraph
            QC.=== ambientRepresentativeAnalysisValuesFor contextValue requestedBaseKeys ephemeralGraph
        )
  where
    requestedBaseKeys = IntSet.fromList requestedKeys

-- Verbatim witness: the maintained cache returns the /same heap object/ for an
-- untouched fiber, so its stable name matches.  A recomputed fiber is a freshly
-- allocated record with a distinct name.
sectionShared ::
  Scope ->
  ContextualSectionCache ArithF NodeCount Scope Int ->
  ContextualSectionCache ArithF NodeCount Scope Int ->
  IO Bool
sectionShared scope beforeCache afterCache =
  case (Map.lookup scope (cscSections beforeCache), Map.lookup scope (cscSections afterCache)) of
    (Just beforeSection, Just afterSection) ->
      sectionsShareIdentity beforeSection afterSection
    _ ->
      assertFailure ("missing section for " <> show scope)

sectionsShareIdentity :: a -> a -> IO Bool
sectionsShareIdentity leftSection rightSection = do
  leftName <- leftSection `seq` makeStableName leftSection
  rightName <- rightSection `seq` makeStableName rightSection
  pure (eqStableName leftName rightName)

expectRight :: Show errorValue => Either errorValue result -> IO result
expectRight resultValue =
  case resultValue of
    Right value ->
      pure value
    Left errorValue ->
      assertFailure ("expected Right, got " <> show errorValue)
