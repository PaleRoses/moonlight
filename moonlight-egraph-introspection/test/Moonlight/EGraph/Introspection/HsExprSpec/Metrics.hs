module Moonlight.EGraph.Introspection.HsExprSpec.Metrics
  ( tests,
  )
where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( ConvertedModule (..),
    HsExprF,
    HsExprInsertionMetrics (..),
    ScopeCtx,
    TopLevelBinding (..),
    convertHaskellSource,
    convertedModuleContextLattice,
    identityInsertionSeeding,
    insertConvertedModule,
    insertScopedExprWithSupport,
    measureHaskellSourceInsertionMetrics,
  )
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec (..))
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    contextCachedObjectsForExecution,
    emptyContextEGraph,
    materializeAmbientPayloadFor,
  )
import Moonlight.EGraph.Pure.Context.Core
  ( cegBase,
    cegClassSupportIndex,
  )
import Moonlight.Core (Language)
import Moonlight.EGraph.Pure.Types (ClassId, emptyEGraph, eGraphClasses)
import Moonlight.Sheaf.Section.Context.Payload
  ( payloadMapToAnalysisMap,
    payloadMapToRepresentativeMap,
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSupportError,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "metrics"
    [ testCase
        "scope-local dirty frontier stays within prepared query contexts"
        testScopeLocalInsertionNarrowsDirtyFanout,
      testCase
        "batched insertion preserves contextual semantics against eager refresh"
        testBatchedInsertionPreservesContextualSemantics
    ]

testScopeLocalInsertionNarrowsDirtyFanout :: IO ()
testScopeLocalInsertionNarrowsDirtyFanout =
  case measureHaskellSourceInsertionMetrics "ScopeTower.hs" (scopeTowerSource 24) of
    Left failureValue ->
      assertFailure ("unexpected insertion-metrics failure: " <> show failureValue)
    Right insertionMetrics ->
      do
        assertBool
          "the synthetic tower should create a nontrivial context site"
          (himObservedContextCount insertionMetrics > 2)
        assertBool
          "the experiment only means something if rebase fanout is recorded"
          (himRebaseCount insertionMetrics > 0)
        assertBool
          "batched rebasing should not dirty more contexts than the prepared query site"
          (himRebaseDirtyContextCount insertionMetrics <= himObservedContextCount insertionMetrics)
        assertBool
          "dirty fanout should never exceed support width across insertion"
          (himRebaseDirtyContextCount insertionMetrics <= himTotalSupportContextCount insertionMetrics)
        assertEqual
          "insertion without contextual unions should not create regional parent edges"
          0
          (himFinalRegionalParentEdgeCount insertionMetrics)
        assertEqual
          "insertion without contextual unions should not create absorbed regional rows"
          0
          (himFinalRegionalAbsorbedRowCount insertionMetrics)

testBatchedInsertionPreservesContextualSemantics :: IO ()
testBatchedInsertionPreservesContextualSemantics =
  case convertHaskellSource "ScopeTower.hs" (scopeTowerSource 24) of
    Left failureValue ->
      assertFailure ("unexpected conversion failure: " <> show failureValue)
    Right convertedModule ->
      case (insertConvertedModuleBatched convertedModule, insertConvertedModuleEager convertedModule) of
        (Right (batchedClassIds, batchedGraph), Right (eagerClassIds, eagerGraph)) ->
          do
            assertEqual "top-level class ids should agree" eagerClassIds batchedClassIds
            assertEqual
              "base e-graph classes should agree after insertion"
              (eGraphClasses (cegBase eagerGraph))
              (eGraphClasses (cegBase batchedGraph))
            assertEqual
              "class support should agree after insertion"
              (cegClassSupportIndex eagerGraph)
              (cegClassSupportIndex batchedGraph)
            eagerClassSections <- expectRight (contextClassSections eagerGraph)
            batchedClassSections <- expectRight (contextClassSections batchedGraph)
            assertEqual
              "contextual class sections should agree after insertion"
              eagerClassSections
              batchedClassSections
            eagerAnalysisSections <- expectRight (contextAnalysisSections eagerGraph)
            batchedAnalysisSections <- expectRight (contextAnalysisSections batchedGraph)
            assertEqual
              "contextual analysis sections should agree after insertion"
              eagerAnalysisSections
              batchedAnalysisSections
            assertEqual
              "active analysis contexts should agree after insertion"
              (contextCachedObjectsForExecution eagerGraph)
              (contextCachedObjectsForExecution batchedGraph)
        (Left failureValue, _) ->
          assertFailure ("unexpected batched lattice failure: " <> failureValue)
        (_, Left failureValue) ->
          assertFailure ("unexpected eager lattice failure: " <> failureValue)

contextClassSections ::
  (Language f, Ord c) =>
  ContextEGraph f a c ->
  Either (PreparedContextSupportError c) (Map.Map c (IntMap.IntMap ClassId))
contextClassSections contextGraph =
  fmap Map.fromList $
    traverse
      ( \contextValue ->
          fmap
            ((,) contextValue . payloadMapToRepresentativeMap)
            (materializeAmbientPayloadFor contextValue contextGraph)
      )
      (contextCachedObjectsForExecution contextGraph)

contextAnalysisSections ::
  (Language f, Ord c) =>
  ContextEGraph f a c ->
  Either (PreparedContextSupportError c) (Map.Map c (IntMap.IntMap a))
contextAnalysisSections contextGraph =
  fmap Map.fromList $
    traverse
      ( \contextValue ->
          fmap
            ((,) contextValue . payloadMapToAnalysisMap)
            (materializeAmbientPayloadFor contextValue contextGraph)
      )
      (contextCachedObjectsForExecution contextGraph)

expectRight :: Show error => Either error value -> IO value
expectRight =
  either (assertFailure . show) pure

insertConvertedModuleBatched :: ConvertedModule -> Either String ([ClassId], ContextEGraph HsExprF () ScopeCtx)
insertConvertedModuleBatched convertedModule = do
  contextGraph0 <- emptyHsExprContextGraph convertedModule
  fmap (\(classIds, _, contextGraph) -> (classIds, contextGraph)) (first show (insertConvertedModule identityInsertionSeeding convertedModule contextGraph0))

insertConvertedModuleEager :: ConvertedModule -> Either String ([ClassId], ContextEGraph HsExprF () ScopeCtx)
insertConvertedModuleEager convertedModule =
  do
    contextGraph0 <- emptyHsExprContextGraph convertedModule
    (contextGraph1, reversedClassIds) <-
      foldM
        ( \(graphValue, classIds) bindingValue -> do
            (classId, _, nextGraph) <-
              first
                show
                (insertScopedExprWithSupport (cmScopeIndex convertedModule) (tlbScopedTerm bindingValue) graphValue)
            pure (nextGraph, classId : classIds)
        )
        (contextGraph0, [])
        (cmBindings convertedModule)
    pure (reverse reversedClassIds, contextGraph1)

emptyHsExprContextGraph :: ConvertedModule -> Either String (ContextEGraph HsExprF () ScopeCtx)
emptyHsExprContextGraph convertedModule =
  fmap
    (\latticeValue -> emptyContextEGraph latticeValue (emptyEGraph trivialAnalysis))
    (first show (convertedModuleContextLattice convertedModule))

trivialAnalysis :: AnalysisSpec HsExprF ()
trivialAnalysis =
  AnalysisSpec
    { asMake = const (),
      asJoin = \_ _ -> (),
      asJoinChanged = \_ _ -> ((), False)
    }

scopeTowerSource :: Int -> String
scopeTowerSource depth =
  unlines
    [ "module ScopeTower where",
      "tower x0 = " <> nestedLets depth
    ]

nestedLets :: Int -> String
nestedLets depth =
  foldr
    (\level body -> "let x" <> show level <> " = x" <> show (level - 1) <> " in " <> body)
    ("x" <> show depth)
    [1 .. depth]
