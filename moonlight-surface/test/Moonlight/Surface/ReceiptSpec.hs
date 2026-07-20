{-# LANGUAGE TypeApplications #-}

module Moonlight.Surface.ReceiptSpec
  ( tests,
  )
where

import Data.Fix (Fix)
import Data.IntMap.Strict qualified as IntMap
import Moonlight.Core (ClassId, Substitution, UnionFindAllocationError, emptySubstitution, insertSubst, mkPatternVar)
import Moonlight.EGraph.Pure.Change (EGraphMutationResult (..))
import Moonlight.EGraph.Pure.Extraction (ExtractionResult (..), extract, stableExtractionSnapshotFromEGraph)
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.EGraph.Pure.Rebuild (rebuild)
import Moonlight.EGraph.Pure.Rewrite.Env (EGraphRewriteEnv (..), emptyEGraphRewriteEnv)
import Moonlight.EGraph.Pure.Rewrite.Program (runExecutableRewriteMatchEGraphCommitted)
import Moonlight.EGraph.Pure.Saturation.Substrate (EGraphU)
import Moonlight.EGraph.Pure.Types (EGraph, canonicalizeClassId, emptyEGraph, eGraphAnalysis)
import Moonlight.EGraph.Test.Saturation (SaturationBudget (..), saturate, saturationReportBaseGraph)
import Moonlight.Rewrite.Runtime (emptyRewriteRuntimeCapabilities, withRuntimeGuardCapabilityResolver)
import Moonlight.Rewrite.Runtime (ExecutableRewriteMatch (..))
import Moonlight.Rewrite.Runtime (RulePlan)
import Moonlight.Rewrite.System (CompiledGuard)
import Moonlight.Rewrite.System (FactStore, FactTuple (..), emptyFactStore, insertFact)
import Moonlight.Saturation.Substrate (compileRewriteRules)
import Moonlight.Saturation.Substrate.Types (TrivialContext)
import Moonlight.Surface.Language
  ( SurfaceAnalysis,
    SurfaceCapability,
    SurfaceF,
    SurfaceLiteralValue (..),
    SurfaceValue (..),
    SurfaceView (..),
    cube,
    diff,
    inter,
    lit,
    scale,
    sphere,
    surfaceAnalysis,
    surfaceAnalysisValue,
    surfaceAnalysisValueOf,
    surfaceCost,
    surfaceGuardCapabilityResolver,
    surfaceReify,
    translate,
    union,
    vadd,
    vec,
    viewSurface,
  )
import Moonlight.Surface.Laws
  ( SurfaceLawError,
    SurfaceRewriteRule,
    surfaceLawRules,
    surfaceNonDegenerateScaleFactId,
    surfaceScaleDiffHoistRule,
    surfaceScaleInterHoistRule,
    surfaceTranslateIdentityRule,
  )
import Moonlight.Pale.Test.Site.Assertion (expectRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "saturation receipt"
    [ testCase "shared translate factors out under node-count extraction" assertSharedTranslateExtraction,
      testCase "nested translates extract symbolically and reify to one materialized vector" assertNestedTranslateReification,
      testCase "known root analysis reifies to its literal value" assertKnownRootReifiesToLiteral,
      testCase "sound surface saturation does not produce analysis conflicts on fixtures" assertSaturationNoConflict,
      testCase "translate identity reads analytically-known zero through guard capability" assertAnalyticTranslateIdentity,
      testCase "fact-gated scale hoists discriminate present and absent non-degeneracy facts on one graph" assertFactGatedScaleHoists
    ]

assertSharedTranslateExtraction :: Assertion
assertSharedTranslateExtraction =
  case surfaceLawRules of
    Left lawError ->
      assertFailure ("surface law emission failed: " <> show lawError)
    Right rules -> do
      (rootClass, graph) <- expectRight (addTerm duplicatedTranslateTerm (emptyEGraph surfaceAnalysis))
      case saturate @SurfaceCapability (SaturationBudget 8 2048) rules graph of
        Left _ ->
          assertFailure "surface saturation failed"
        Right report ->
          case extractSurface (saturationReportBaseGraph report) rootClass of
            Nothing ->
              assertFailure "surface extraction failed"
            Just extractionResult -> do
              erCost extractionResult @?= 10
              assertFactoredTranslate (viewSurface (erTerm extractionResult))

assertNestedTranslateReification :: Assertion
assertNestedTranslateReification =
  case surfaceLawRules of
    Left lawError ->
      assertFailure ("surface law emission failed: " <> show lawError)
    Right rules -> do
      (rootClass, graph) <- expectRight (addTerm nestedTranslateTerm (emptyEGraph surfaceAnalysis))
      case saturate @SurfaceCapability (SaturationBudget 8 2048) rules graph of
        Left _ ->
          assertFailure "surface saturation failed"
        Right report ->
          case extractSurface (saturationReportBaseGraph report) rootClass of
            Nothing ->
              assertFailure "surface extraction failed"
            Just extractionResult ->
              viewSurface (surfaceReify (erTerm extractionResult))
                @?= SurfaceTranslateView expectedNestedVectorView expectedSphereView

assertKnownRootReifiesToLiteral :: Assertion
assertKnownRootReifiesToLiteral = do
  let term = vadd (vec (lit 1) (lit 2) (lit 3)) (vec (lit 4) (lit 5) (lit 6))
  (rootClass, graph) <- expectRight (addTerm term (emptyEGraph surfaceAnalysis))
  surfaceAnalysisValueOf graph rootClass @?= SurfaceKnown (SurfaceVector 5 7 9)
  case extractSurface graph rootClass of
    Nothing ->
      assertFailure "surface extraction failed"
    Just extractionResult ->
      viewSurface (surfaceReify (erTerm extractionResult)) @?= expectedNestedVectorView

assertSaturationNoConflict :: Assertion
assertSaturationNoConflict =
  case surfaceLawRules of
    Left lawError ->
      assertFailure ("surface law emission failed: " <> show lawError)
    Right rules -> do
      (_, graph) <- expectRight (addSurfaceTerms fixtureCorpus (emptyEGraph surfaceAnalysis))
      case saturate @SurfaceCapability (SaturationBudget 8 4096) rules graph of
        Left _ ->
          assertFailure "surface saturation failed"
        Right report ->
          let conflicts = IntMap.filter ((== SurfaceConflict) . surfaceAnalysisValue) (eGraphAnalysis (saturationReportBaseGraph report))
           in assertBool ("surface analysis conflicts: " <> show conflicts) (IntMap.null conflicts)

assertAnalyticTranslateIdentity :: Assertion
assertAnalyticTranslateIdentity = do
  rule <- expectRule "translate identity" surfaceTranslateIdentityRule
  compiledRule <- compileSurfaceRule rule
  let zeroVector = vadd (vec (lit 1) (lit 2) (lit 3)) (vec (lit (-1)) (lit (-2)) (lit (-3)))
      body = sphere (lit 1)
  (vectorClass, graph1) <- expectRight (addTerm zeroVector (emptyEGraph surfaceAnalysis))
  (bodyClass, graph2) <- expectRight (addTerm body graph1)
  (rootClass, graph3) <- expectRight (addTerm (translate zeroVector body) graph2)
  let rewriteEnv = surfaceRewriteEnv emptyFactStore graph3
      rewriteMatch = executableSurfaceMatch compiledRule rootClass [(0, vectorClass), (1, bodyClass)]
  case runExecutableRewriteMatchEGraphCommitted rewriteEnv rewriteMatch graph3 of
    Left rewriteError ->
      assertFailure ("translate identity rewrite failed: " <> show rewriteError)
    Right rewriteResult ->
      let graph4 = rebuild (emrGraph rewriteResult)
       in canonicalizeClassId graph4 rootClass @?= canonicalizeClassId graph4 bodyClass

assertFactGatedScaleHoists :: Assertion
assertFactGatedScaleHoists = do
  interRule <- compileSurfaceRule =<< expectRule "scale inter hoist" surfaceScaleInterHoistRule
  diffRule <- compileSurfaceRule =<< expectRule "scale diff hoist" surfaceScaleDiffHoistRule
  fixture <- expectRight scaleHoistFixture
  let factStore = insertFact surfaceNonDegenerateScaleFactId (FactTuple [shfNonDegenerateVectorClass fixture]) emptyFactStore
  assertScaleHoistAccepted fixture factStore interRule (shfNonDegenerateInterRoot fixture) (shfNonDegenerateVectorClass fixture) SurfaceInterView
  assertScaleHoistRefused fixture factStore interRule (shfMissingInterRoot fixture) (shfMissingVectorClass fixture)
  assertScaleHoistAccepted fixture factStore diffRule (shfNonDegenerateDiffRoot fixture) (shfNonDegenerateVectorClass fixture) SurfaceDiffView
  assertScaleHoistRefused fixture factStore diffRule (shfMissingDiffRoot fixture) (shfMissingVectorClass fixture)

assertScaleHoistAccepted :: ScaleHoistFixture -> FactStore -> SurfaceRulePlan -> ClassId -> ClassId -> (SurfaceView -> SurfaceView -> SurfaceView) -> Assertion
assertScaleHoistAccepted fixture factStore rule rootClass vectorClass bodyView =
  let rewriteEnv = surfaceRewriteEnv factStore (shfGraph fixture)
      rewriteMatch = executableSurfaceMatch rule rootClass [(0, vectorClass), (1, shfSphereClass fixture), (2, shfCubeClass fixture)]
   in case runExecutableRewriteMatchEGraphCommitted rewriteEnv rewriteMatch (shfGraph fixture) of
        Left rewriteError ->
          assertFailure ("scale hoist rewrite failed: " <> show rewriteError)
        Right rewriteResult ->
          case extractSurface (rebuild (emrGraph rewriteResult)) rootClass of
            Nothing ->
              assertFailure "surface extraction failed"
            Just extractionResult ->
              viewSurface (erTerm extractionResult)
                @?= SurfaceScaleView (viewSurface (shfNonDegenerateVectorTerm fixture)) (bodyView expectedSphereView expectedCubeView)

assertScaleHoistRefused :: ScaleHoistFixture -> FactStore -> SurfaceRulePlan -> ClassId -> ClassId -> Assertion
assertScaleHoistRefused fixture factStore rule rootClass vectorClass =
  let rewriteEnv = surfaceRewriteEnv factStore (shfGraph fixture)
      rewriteMatch = executableSurfaceMatch rule rootClass [(0, vectorClass), (1, shfSphereClass fixture), (2, shfCubeClass fixture)]
   in case runExecutableRewriteMatchEGraphCommitted rewriteEnv rewriteMatch (shfGraph fixture) of
        Left _ -> pure ()
        Right _ -> assertFailure "scale hoist rewrite unexpectedly accepted without non-degeneracy fact"

type SurfaceRulePlan = RulePlan (CompiledGuard SurfaceCapability SurfaceF) SurfaceF

compileSurfaceRule :: SurfaceRewriteRule -> IO SurfaceRulePlan
compileSurfaceRule rule =
  case compileRewriteRules @(EGraphU SurfaceCapability SurfaceF SurfaceAnalysis TrivialContext) [rule] of
    Right [compiledRule] ->
      pure compiledRule
    Right compiledRules ->
      assertFailure ("expected one compiled rule, got " <> show (length compiledRules))
    Left compileError ->
      assertFailure ("surface rule compilation failed: " <> show compileError)

expectRule :: String -> Either SurfaceLawError SurfaceRewriteRule -> IO SurfaceRewriteRule
expectRule label =
  \case
    Right rule -> pure rule
    Left lawError -> assertFailure (label <> " emission failed: " <> show lawError)

surfaceRewriteEnv :: FactStore -> EGraph SurfaceF SurfaceAnalysis -> EGraphRewriteEnv SurfaceCapability SurfaceF
surfaceRewriteEnv factStore graph =
  emptyEGraphRewriteEnv
    { ereFactStore = factStore,
      ereRuntimeCapabilities =
        withRuntimeGuardCapabilityResolver
          (surfaceGuardCapabilityResolver graph)
          emptyRewriteRuntimeCapabilities
    }

executableSurfaceMatch :: SurfaceRulePlan -> ClassId -> [(Int, ClassId)] -> ExecutableRewriteMatch (CompiledGuard SurfaceCapability SurfaceF) guardEvidence guideEvidence SurfaceF
executableSurfaceMatch rule rootClass bindings =
  ExecutableRewriteMatch rule rootClass Nothing Nothing (surfaceSubstitution bindings)

surfaceSubstitution :: [(Int, ClassId)] -> Substitution
surfaceSubstitution =
  foldr
    (\(patternKey, classId) substitution -> insertSubst (mkPatternVar patternKey) classId substitution)
    emptySubstitution

extractSurface :: EGraph SurfaceF SurfaceAnalysis -> ClassId -> Maybe (ExtractionResult SurfaceF Int)
extractSurface graph rootClass =
  stableExtractionSnapshotFromEGraph graph >>= extract surfaceCost rootClass

addSurfaceTerms :: [Fix SurfaceF] -> EGraph SurfaceF SurfaceAnalysis -> Either UnionFindAllocationError ([ClassId], EGraph SurfaceF SurfaceAnalysis)
addSurfaceTerms terms initialGraph =
  foldr addOne (Right ([], initialGraph)) terms
  where
    addOne :: Fix SurfaceF -> Either UnionFindAllocationError ([ClassId], EGraph SurfaceF SurfaceAnalysis) -> Either UnionFindAllocationError ([ClassId], EGraph SurfaceF SurfaceAnalysis)
    addOne term accumulatedTerms = do
      (classIds, graph) <- accumulatedTerms
      (classId, nextGraph) <- addTerm term graph
      pure (classId : classIds, nextGraph)

data ScaleHoistFixture = ScaleHoistFixture
  { shfGraph :: !(EGraph SurfaceF SurfaceAnalysis),
    shfNonDegenerateVectorTerm :: !(Fix SurfaceF),
    shfNonDegenerateVectorClass :: !ClassId,
    shfMissingVectorClass :: !ClassId,
    shfSphereClass :: !ClassId,
    shfCubeClass :: !ClassId,
    shfNonDegenerateInterRoot :: !ClassId,
    shfMissingInterRoot :: !ClassId,
    shfNonDegenerateDiffRoot :: !ClassId,
    shfMissingDiffRoot :: !ClassId
  }

scaleHoistFixture :: Either UnionFindAllocationError ScaleHoistFixture
scaleHoistFixture = do
  let nonDegenerateVector = vec (lit 2) (lit 3) (lit 4)
      missingVector = vec (lit 5) (lit 6) (lit 7)
      sphereTerm = sphere (lit 1)
      cubeTerm = cube (lit 2)
  (nonDegenerateVectorClass, graph1) <- addTerm nonDegenerateVector (emptyEGraph surfaceAnalysis)
  (missingVectorClass, graph2) <- addTerm missingVector graph1
  (sphereClass, graph3) <- addTerm sphereTerm graph2
  (cubeClass, graph4) <- addTerm cubeTerm graph3
  (nonDegenerateInterRoot, graph5) <- addTerm (inter (scale nonDegenerateVector sphereTerm) (scale nonDegenerateVector cubeTerm)) graph4
  (missingInterRoot, graph6) <- addTerm (inter (scale missingVector sphereTerm) (scale missingVector cubeTerm)) graph5
  (nonDegenerateDiffRoot, graph7) <- addTerm (diff (scale nonDegenerateVector sphereTerm) (scale nonDegenerateVector cubeTerm)) graph6
  (missingDiffRoot, graph8) <- addTerm (diff (scale missingVector sphereTerm) (scale missingVector cubeTerm)) graph7
  pure
    ScaleHoistFixture
      { shfGraph = graph8,
        shfNonDegenerateVectorTerm = nonDegenerateVector,
        shfNonDegenerateVectorClass = nonDegenerateVectorClass,
        shfMissingVectorClass = missingVectorClass,
        shfSphereClass = sphereClass,
        shfCubeClass = cubeClass,
        shfNonDegenerateInterRoot = nonDegenerateInterRoot,
        shfMissingInterRoot = missingInterRoot,
        shfNonDegenerateDiffRoot = nonDegenerateDiffRoot,
        shfMissingDiffRoot = missingDiffRoot
      }

duplicatedTranslateTerm :: Fix SurfaceF
duplicatedTranslateTerm =
  union
    (translate sharedVector (sphere (lit 1)))
    (translate sharedVector (cube (lit 2)))

nestedTranslateTerm :: Fix SurfaceF
nestedTranslateTerm =
  translate
    (vec (lit 1) (lit 2) (lit 3))
    (translate (vec (lit 4) (lit 5) (lit 6)) (sphere (lit 1)))

fixtureCorpus :: [Fix SurfaceF]
fixtureCorpus =
  [ duplicatedTranslateTerm,
    nestedTranslateTerm,
    scale (vec (lit 2) (lit 3) (lit 4)) (scale (vec (lit 5) (lit 6) (lit 7)) (cube (lit 1))),
    inter (scale (vec (lit 2) (lit 3) (lit 4)) (sphere (lit 1))) (scale (vec (lit 2) (lit 3) (lit 4)) (cube (lit 2)))
  ]

sharedVector :: Fix SurfaceF
sharedVector =
  vec (lit 1) (lit 0) (lit 0)

assertFactoredTranslate :: SurfaceView -> Assertion
assertFactoredTranslate =
  \case
    SurfaceTranslateView vectorValue body -> do
      vectorValue @?= expectedVectorView
      assertUnionOfSphereAndCube body
    otherView ->
      assertFailure ("expected factored translate, got " <> show otherView)

assertUnionOfSphereAndCube :: SurfaceView -> Assertion
assertUnionOfSphereAndCube =
  \case
    SurfaceUnionView left right
      | (left, right) == (expectedSphereView, expectedCubeView) || (left, right) == (expectedCubeView, expectedSphereView) ->
          pure ()
    otherView ->
      assertFailure ("expected union of sphere and cube, got " <> show otherView)

expectedVectorView :: SurfaceView
expectedVectorView =
  SurfaceVecView (SurfaceLitView 1) (SurfaceLitView 0) (SurfaceLitView 0)

expectedNestedVectorView :: SurfaceView
expectedNestedVectorView =
  SurfaceVecView (SurfaceLitView 5) (SurfaceLitView 7) (SurfaceLitView 9)

expectedSphereView :: SurfaceView
expectedSphereView =
  SurfaceSphereView (SurfaceLitView 1)

expectedCubeView :: SurfaceView
expectedCubeView =
  SurfaceCubeView (SurfaceLitView 2)
