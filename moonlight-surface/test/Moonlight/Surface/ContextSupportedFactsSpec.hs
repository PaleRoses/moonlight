{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Moonlight.Surface.ContextSupportedFactsSpec
  ( tests,
  )
where

import Data.Foldable (foldlM, traverse_)
import Data.Kind (Type)
import Moonlight.Core (ClassId, Pattern (..), RewriteRuleId, mkPatternVar)
import Moonlight.EGraph.Pure.Context (ContextEGraph, activateContext, cegSite, emptyContextEGraph)
import Moonlight.EGraph.Pure.Context.Proof (ProofGraph (pgGraph))
import Moonlight.EGraph.Pure.Extraction (ExtractionResult (..), extractFromTable, liftCostAlgebra)
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.EGraph.Pure.Saturation.Extraction (contextualExtractionTable)
import Moonlight.EGraph.Pure.Saturation.Matching (MatchingStrategy (GenericJoinMatching))
import Moonlight.EGraph.Pure.Saturation.Substrate (EGraphU)
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingProofEGraph,
    emptySaturatingProofEGraph,
    sceContextGraph,
  )
import Moonlight.EGraph.Pure.Types (EGraph, emptyEGraph)
import Moonlight.FiniteLattice (ContextLattice, contextLatticeFromClosedOrder)
import Moonlight.Rewrite.ProofContext (principalSupport)
import Moonlight.Rewrite.ProofContext (defaultProofAnnotationBuilder)
import Moonlight.Rewrite.System (data GuardRoot)
import Moonlight.Rewrite.System (FactRule, FactRuleId (..), RawFactRule (..))
import Moonlight.Saturation.Context.Runtime.Report (srCarrier)
import Moonlight.Saturation.Context.Driver (crrResult)
import Moonlight.Saturation.Context.Program.Spec (PlanSpec, planSpec, staticRewriteContextSnapshot, withSchedulerConfig)
import Moonlight.Saturation.Core (SaturationBudget (..))
import Moonlight.Saturation.Substrate (SatGraph)
import Moonlight.Saturation.Support.Core (SupportSaturationReportFor)
import Moonlight.Sheaf.Twist.SupportedRuleSpec qualified as SheafTwist
import Moonlight.Surface.Language
  ( SurfaceAnalysis,
    SurfaceCapability,
    SurfaceF (..),
    SurfaceView (..),
    cube,
    diff,
    lit,
    scale,
    sphere,
    surfaceAnalysis,
    surfaceCost,
    viewSurface,
    vec,
  )
import Moonlight.Surface.Laws
  ( SurfaceLawError,
    SurfaceRewriteRule,
    surfaceLawRuleIdBase,
    surfaceNonDegenerateScaleFactId,
    surfaceScaleDiffHoistRule,
  )
import Moonlight.EGraph.Test.Saturation
  ( deterministicSchedulerConfig,
    emptyRewriteRuntimeCapabilities,
    prepareEGraphSupportPlan,
    runEGraphSupportPlan,
  )
import Moonlight.Pale.Test.Site.Assertion (expectRight, expectRightWithLabel)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "context-supported facts over a four-object diamond lattice"
    [ testCase "principal arm fact support fires on the arm and its up-set only" assertPrincipalArmFactSupport,
      testCase "bottom fact support reproduces global fact visibility" assertBottomFactSupport
    ]

type SurfaceSupportU :: Type
type SurfaceSupportU = EGraphU SurfaceCapability SurfaceF SurfaceAnalysis SurfaceSupportContext

type SurfaceSupportReport :: Type
type SurfaceSupportReport =
  SupportSaturationReportFor
    SurfaceSupportU
    (SaturatingProofEGraph SurfaceCapability SurfaceF SurfaceAnalysis SurfaceSupportContext ())

data SurfaceSupportContext
  = SurfaceSupportBottom
  | SurfaceSupportLeft
  | SurfaceSupportRight
  | SurfaceSupportTop
  deriving stock (Eq, Ord, Show)

data SupportFixture = SupportFixture
  { sfRootClass :: !ClassId,
    sfInitialGraph :: !(EGraph SurfaceF SurfaceAnalysis),
    sfUnhoistedView :: !SurfaceView,
    sfHoistedView :: !SurfaceView
  }

assertPrincipalArmFactSupport :: Assertion
assertPrincipalArmFactSupport = do
  fixture <- supportFixture
  report <- runSurfaceSupportFixture SurfaceSupportLeft fixture
  let contextGraph = sceContextGraph (pgGraph (srCarrier report))
  assertContextExtraction contextGraph (sfRootClass fixture) SurfaceSupportLeft (sfHoistedView fixture)
  assertContextExtraction contextGraph (sfRootClass fixture) SurfaceSupportTop (sfHoistedView fixture)
  assertContextExtraction contextGraph (sfRootClass fixture) SurfaceSupportBottom (sfUnhoistedView fixture)
  assertContextExtraction contextGraph (sfRootClass fixture) SurfaceSupportRight (sfUnhoistedView fixture)

assertBottomFactSupport :: Assertion
assertBottomFactSupport = do
  fixture <- supportFixture
  report <- runSurfaceSupportFixture SurfaceSupportBottom fixture
  let contextGraph = sceContextGraph (pgGraph (srCarrier report))
  traverse_
    ( \contextValue ->
        assertContextExtraction contextGraph (sfRootClass fixture) contextValue (sfHoistedView fixture)
    )
    surfaceSupportContexts

runSurfaceSupportFixture :: SurfaceSupportContext -> SupportFixture -> IO SurfaceSupportReport
runSurfaceSupportFixture factContext fixture = do
  rule <- expectRule "scale diff hoist" surfaceScaleDiffHoistRule
  contextGraph <- activeSurfaceContextGraph (sfInitialGraph fixture)
  let site = cegSite contextGraph
  ruleBook <-
    expectRightWithLabel "surface supported rule book" $
      SheafTwist.supportedRuleBook
        site
        [SheafTwist.SupportedRuleSpec (principalSupport SurfaceSupportBottom) rule]
  factBook <-
    expectRightWithLabel "surface supported fact book" $
      SheafTwist.supportedFactBook
        site
        [SheafTwist.SupportedFactSpec (principalSupport factContext) surfaceNonDegenerateScaleFactRule]
  let proofGraph = emptySaturatingProofEGraph contextGraph
  supportPlan <-
    expectRightWithLabel "surface support planning" $
      prepareEGraphSupportPlan
      Nothing
      (const (staticRewriteContextSnapshot emptyRewriteRuntimeCapabilities))
      surfaceSupportPlanSpec
      ruleBook
      factBook
      proofGraph
  supportRun <-
    expectRightWithLabel "surface support saturation" $
      runEGraphSupportPlan defaultProofAnnotationBuilder mempty supportPlan proofGraph
  pure (crrResult supportRun)

surfaceSupportPlanSpec ::
  PlanSpec SurfaceSupportU (SatGraph SurfaceSupportU) RewriteRuleId
surfaceSupportPlanSpec =
  withSchedulerConfig
    deterministicSchedulerConfig
    (planSpec (SaturationBudget 8 2048) GenericJoinMatching emptyRewriteRuntimeCapabilities)

activeSurfaceContextGraph :: EGraph SurfaceF SurfaceAnalysis -> IO (ContextEGraph SurfaceF SurfaceAnalysis SurfaceSupportContext)
activeSurfaceContextGraph graph = do
  lattice <- expectRightWithLabel "surface support lattice" surfaceSupportLattice
  expectRightWithLabel "surface context activation" $
    foldlM
      (flip activateContext)
      (emptyContextEGraph lattice graph)
      surfaceSupportContexts

surfaceSupportContexts :: [SurfaceSupportContext]
surfaceSupportContexts =
  [SurfaceSupportBottom, SurfaceSupportLeft, SurfaceSupportRight, SurfaceSupportTop]

surfaceSupportLattice :: Either String (ContextLattice SurfaceSupportContext)
surfaceSupportLattice =
  firstShow
    ( contextLatticeFromClosedOrder
        SurfaceSupportTop
        SurfaceSupportBottom
        surfaceSupportContexts
        surfaceSupportLeq
        surfaceSupportJoin
        surfaceSupportMeet
    )

surfaceSupportLeq :: SurfaceSupportContext -> SurfaceSupportContext -> Bool
surfaceSupportLeq left right =
  left == right || left == SurfaceSupportBottom || right == SurfaceSupportTop

surfaceSupportJoin :: SurfaceSupportContext -> SurfaceSupportContext -> SurfaceSupportContext
surfaceSupportJoin left right =
  case (left, right) of
    (SurfaceSupportBottom, value) -> value
    (value, SurfaceSupportBottom) -> value
    (SurfaceSupportTop, _) -> SurfaceSupportTop
    (_, SurfaceSupportTop) -> SurfaceSupportTop
    (SurfaceSupportLeft, SurfaceSupportLeft) -> SurfaceSupportLeft
    (SurfaceSupportRight, SurfaceSupportRight) -> SurfaceSupportRight
    _ -> SurfaceSupportTop

surfaceSupportMeet :: SurfaceSupportContext -> SurfaceSupportContext -> SurfaceSupportContext
surfaceSupportMeet left right =
  case (left, right) of
    (SurfaceSupportTop, value) -> value
    (value, SurfaceSupportTop) -> value
    (SurfaceSupportBottom, _) -> SurfaceSupportBottom
    (_, SurfaceSupportBottom) -> SurfaceSupportBottom
    (SurfaceSupportLeft, SurfaceSupportLeft) -> SurfaceSupportLeft
    (SurfaceSupportRight, SurfaceSupportRight) -> SurfaceSupportRight
    _ -> SurfaceSupportBottom

surfaceNonDegenerateScaleFactRule :: FactRule SurfaceCapability SurfaceF
surfaceNonDegenerateScaleFactRule =
  FactRule
    { frId = FactRuleId (surfaceLawRuleIdBase + 101),
      frName = "surface-context-nondegenerate-scale",
      frPattern =
        PatternNode
          ( SurfaceVec
              (PatternVar (mkPatternVar 0))
              (PatternVar (mkPatternVar 1))
              (PatternVar (mkPatternVar 2))
          ),
      frProjection = [GuardRoot],
      frFactId = surfaceNonDegenerateScaleFactId,
      frCondition = Nothing
    }

supportFixture :: IO SupportFixture
supportFixture = do
  let scaleVector = vec (lit 2) (lit 3) (lit 4)
      sphereTerm = sphere (lit 1)
      cubeTerm = cube (lit 2)
      unhoistedTerm = diff (scale scaleVector sphereTerm) (scale scaleVector cubeTerm)
  (rootClass, graph) <- expectRight (addTerm unhoistedTerm (emptyEGraph surfaceAnalysis))
  pure
    SupportFixture
      { sfRootClass = rootClass,
        sfInitialGraph = graph,
        sfUnhoistedView =
          SurfaceDiffView
            (SurfaceScaleView expectedVectorView expectedSphereView)
            (SurfaceScaleView expectedVectorView expectedCubeView),
        sfHoistedView = SurfaceScaleView expectedVectorView (SurfaceDiffView expectedSphereView expectedCubeView)
      }

assertContextExtraction ::
  ContextEGraph SurfaceF SurfaceAnalysis SurfaceSupportContext ->
  ClassId ->
  SurfaceSupportContext ->
  SurfaceView ->
  Assertion
assertContextExtraction contextGraph rootClass contextValue expectedView =
  case contextualExtractionTable contextValue contextGraph of
    Left obstruction ->
      assertFailure
        ( "surface contextual extraction table failed at "
            <> show contextValue
            <> ": "
            <> show obstruction
        )
    Right table ->
      case extractFromTable (liftCostAlgebra surfaceCost) rootClass table of
        Nothing ->
          assertFailure ("surface extraction failed at " <> show contextValue)
        Just extractionResult ->
          viewSurface (erTerm extractionResult) @?= expectedView

expectRule :: String -> Either SurfaceLawError SurfaceRewriteRule -> IO SurfaceRewriteRule
expectRule label =
  \case
    Right rule -> pure rule
    Left lawError -> assertFailure (label <> " emission failed: " <> show lawError)

firstShow :: Show err => Either err value -> Either String value
firstShow =
  either (Left . show) Right

expectedVectorView :: SurfaceView
expectedVectorView =
  SurfaceVecView (SurfaceLitView 2) (SurfaceLitView 3) (SurfaceLitView 4)

expectedSphereView :: SurfaceView
expectedSphereView =
  SurfaceSphereView (SurfaceLitView 1)

expectedCubeView :: SurfaceView
expectedCubeView =
  SurfaceCubeView (SurfaceLitView 2)
