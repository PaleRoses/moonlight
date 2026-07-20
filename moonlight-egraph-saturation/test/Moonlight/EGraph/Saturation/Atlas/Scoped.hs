{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Moonlight.EGraph.Saturation.Atlas.Scoped
  ( tests,
  )
where

import Data.Bifunctor (first)
import Data.Fix (Fix)
import Data.Foldable (traverse_)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Moonlight.Core
  ( BinderId (..),
    ClassId,
    OrderedFix (..),
    RewriteRuleId (..),
    UnionFindAllocationError,
  )
import Moonlight.EGraph.Pure.Change (EGraphMutationResult (..))
import Moonlight.EGraph.Pure.Context
  ( ContextDeltaError,
    ContextEGraph,
    contextMerge,
    withEmptyContextEGraph,
  )
import Moonlight.EGraph.Pure.Context (cegSite)
import Moonlight.EGraph.Pure.Context.Proof
  ( ProofGraph (pgGraph),
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (insertTermsTracked)
import Moonlight.EGraph.Pure.Saturation.Matching (MatchingStrategy (GenericJoinMatching))
import Moonlight.EGraph.Pure.Saturation.Substrate (EGraphU)
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingProofEGraph,
    emptySaturatingProofEGraph,
    sceContextGraph,
  )
import Moonlight.EGraph.Pure.Types (emptyEGraph)
import Moonlight.EGraph.Test.Saturation
  ( deterministicSchedulerConfig,
    emptyRewriteRuntimeCapabilities,
    prepareEGraphSupportPlan,
    runEGraphSupportPlan,
    srCarrier,
  )
import Moonlight.EGraph.Test.Scale.Run
  ( AtlasAgreementObstruction,
    AtlasReferenceObstruction,
    AtlasRunObstruction,
    assertReferenceAgreement,
    atlasExtractionTerm,
    extractAtlasAtContext,
    runAtlasProgram,
    runAtlasReferences,
  )
import Moonlight.EGraph.Test.Scale.Site
  ( ScaleContext,
    ScaleSite,
    ScaleSiteError,
    SupportProbe (..),
    scaleSiteBottom,
    scaleSiteContexts,
    scaleSiteLattice,
    scaleSitePrimaryProbe,
    scaleSiteSampledContexts,
    scaleSiteSecondaryProbe,
    scaledTree,
  )
import Moonlight.EGraph.Test.Scoped.Core
  ( ScopedF,
    scopedAnalysisSpec,
    scopedApp,
    scopedBetaContractum,
    scopedBetaRedex,
    scopedBetaRule,
    scopedBinderIndependentFactRule,
    scopedBinderSubstAlgebra,
    scopedCost,
    scopedEtaContractum,
    scopedEtaRedex,
    scopedFactGatedEtaRule,
    scopedFree,
    scopedLam,
    scopedLocal,
    scopedLocalEtaRule,
  )
import Moonlight.Rewrite.ProofContext (principalSupport)
import Moonlight.Rewrite.ProofContext (defaultProofAnnotationBuilder)
import Moonlight.Rewrite.Runtime
  ( RewriteRuntimeCapabilities,
    withRuntimeBinderSubstAlgebra,
  )
import Moonlight.Rewrite.System
  ( GuardCapabilityResolver,
    RewriteCondition,
  )
import Moonlight.Rewrite.System
  ( FactRule,
    FactRuleId (..),
  )
import Moonlight.Rewrite.System (RawRewriteRule)
import Moonlight.Saturation.Context.Error (SaturationError)
import Moonlight.Saturation.Context.Driver (crrResult)
import Moonlight.Saturation.Context.Program.Spec
  ( PlanSpec,
    planSpec,
    staticRewriteContextSnapshot,
    withSchedulerConfig,
  )
import Moonlight.Saturation.Core (SaturationBudget (..))
import Moonlight.Saturation.Substrate (SatGraph, TrivialContext)
import Moonlight.Saturation.Support.Core
  ( SupportSaturationReportFor,
    SupportScheduleGroup,
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSupportError,
    UnitContextSiteOwner,
  )
import Moonlight.Sheaf.Twist.SupportedRuleSpec qualified as SheafTwist
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
  )

type ScopedAtlasU owner = EGraphU owner () ScopedF () ScaleContext

type ScopedAtlasReport owner =
  SupportSaturationReportFor
    (ScopedAtlasU owner)
    (SaturatingProofEGraph owner () ScopedF () ScaleContext ())

type ScopedAtlasRuleBook owner =
  SheafTwist.SupportedRuleBook
    owner
    ScaleContext
    (RawRewriteRule (RewriteCondition () ScopedF) ScopedF)

type ScopedAtlasFactBook owner =
  SheafTwist.SupportedFactBook
    owner
    ScaleContext
    (FactRule () ScopedF)

data ScopedSupportCase
  = ScopedArmSupport
  | ScopedBottomSupport
  | ScopedIntersectingSupport
  deriving stock (Eq, Ord, Show)

data ScopedAtlasObstruction owner
  = ScopedSiteFailed !ScaleSiteError
  | ScopedSecondaryProbeMissing
  | ScopedPopulationMalformed
  | ScopedAllocationFailed !UnionFindAllocationError
  | ScopedOverlayFailed !(ContextDeltaError ScopedF ScaleContext)
  | ScopedRuleBookFailed !(PreparedContextSupportError ScaleContext)
  | ScopedFactBookFailed !(PreparedContextSupportError ScaleContext)
  | ScopedPlanFailed !(SaturationError (ScopedAtlasU owner) (SupportScheduleGroup (ScopedAtlasU owner)))
  | ScopedRunFailed
      !( AtlasRunObstruction
           ScaleContext
           (SaturationError (ScopedAtlasU owner) (SupportScheduleGroup (ScopedAtlasU owner)))
       )
  | ScopedReferenceFailed
      !( AtlasReferenceObstruction
           ScaleContext
           (SaturationError (EGraphU UnitContextSiteOwner () ScopedF () TrivialContext) RewriteRuleId)
       )
  | ScopedAgreementFailed !(AtlasAgreementObstruction ScaleContext)
  deriving stock (Show)

data ScopedRoots = ScopedRoots
  { srGlobalBetaRedex :: !ClassId,
    srGlobalBetaTarget :: !ClassId,
    srOverlayBetaRedex :: !ClassId,
    srOverlayBetaTarget :: !ClassId,
    srOverlayArgument :: !ClassId,
    srOverlayAlias :: !ClassId,
    srInlineRedex :: !ClassId,
    srInlineTarget :: !ClassId,
    srPureRedex :: !ClassId,
    srPureTarget :: !ClassId
  }

data ScopedAtlasResult owner = ScopedAtlasResult
  { sarSite :: !ScaleSite,
    sarRoots :: !ScopedRoots,
    sarInitialGraph :: !(ContextEGraph owner ScopedF () ScaleContext),
    sarRuleBook :: !(ScopedAtlasRuleBook owner),
    sarFactBook :: !(ScopedAtlasFactBook owner),
    sarReport :: !(ScopedAtlasReport owner)
  }

tests :: TestTree
tests =
  testGroup
    "atlas-scoped"
    [ testCase "global beta substitutes through every scope" globalBetaLaw,
      testCase "local inline persists through its full child up-set" monotonicityLaw,
      testCase "local inline never escapes its child scope" scopeEscapeLaw,
      testCase "bottom inline degenerates to every scope" degeneracyLaw,
      testCase "pure binding fires only where fact and rule scopes meet" factGatingLaw,
      testCase "post-match substitution reads the child overlay quotient" postMatchOverlayLaw,
      testCase "support driver agrees with exhaustive scoped saturation" agreementLaw
    ]

globalBetaLaw :: Assertion
globalBetaLaw =
  withScopedAtlas 32 160 ScopedArmSupport $ \result ->
    traverse_
      (assertExtracts result (srGlobalBetaRedex (sarRoots result)) scopedBetaContractum)
      (NonEmpty.toList (scaleSiteContexts (sarSite result)))

monotonicityLaw :: Assertion
monotonicityLaw =
  withScopedAtlas 32 160 ScopedArmSupport $ \result -> do
    let probe = scaleSitePrimaryProbe (sarSite result)
    traverse_
      (assertExtracts result (srInlineRedex (sarRoots result)) inlineTarget)
      (NonEmpty.toList (supportProbeUpset probe))

scopeEscapeLaw :: Assertion
scopeEscapeLaw =
  withScopedAtlas 32 160 ScopedArmSupport $ \result -> do
    let site = sarSite result
        inside = Set.fromList (NonEmpty.toList (supportProbeUpset (scaleSitePrimaryProbe site)))
        outside = filter (`Set.notMember` inside) (NonEmpty.toList (scaleSiteContexts site))
    traverse_
      (assertExtracts result (srInlineRedex (sarRoots result)) inlineRedex)
      outside

degeneracyLaw :: Assertion
degeneracyLaw =
  withScopedAtlas 32 160 ScopedBottomSupport $ \result ->
    traverse_
      (assertExtracts result (srInlineRedex (sarRoots result)) inlineTarget)
      (NonEmpty.toList (scaleSiteContexts (sarSite result)))

factGatingLaw :: Assertion
factGatingLaw =
  withScopedAtlas 32 160 ScopedIntersectingSupport $ \result -> do
    let site = sarSite result
        primaryUpset = Set.fromList (NonEmpty.toList (supportProbeUpset (scaleSitePrimaryProbe site)))
        secondaryUpset =
          maybe
            Set.empty
            (Set.fromList . NonEmpty.toList . supportProbeUpset)
            (scaleSiteSecondaryProbe site)
        overlap = Set.intersection primaryUpset secondaryUpset
    traverse_
      ( \contextValue ->
          assertExtracts
            result
            (srPureRedex (sarRoots result))
            (if Set.member contextValue overlap then pureTarget else pureRedex)
            contextValue
      )
      (NonEmpty.toList (scaleSiteContexts site))

postMatchOverlayLaw :: Assertion
postMatchOverlayLaw =
  withScopedAtlas 32 160 ScopedArmSupport $ \result -> do
    let probe = scaleSitePrimaryProbe (sarSite result)
    traverse_
      (assertExtracts result (srOverlayBetaRedex (sarRoots result)) overlayBetaTarget)
      (NonEmpty.toList (supportProbeUpset probe))

agreementLaw :: Assertion
agreementLaw =
  withScopedAtlas 8 24 ScopedArmSupport $ \exhaustiveResult -> do
    assertScopedAgreement
      exhaustiveResult
      (NonEmpty.toList (scaleSiteContexts (sarSite exhaustiveResult)))
    withScopedAtlas 32 160 ScopedArmSupport $ \sampledResult ->
      assertScopedAgreement
        sampledResult
        (NonEmpty.toList (scaleSiteSampledContexts (sarSite sampledResult)))

assertScopedAgreement :: ScopedAtlasResult owner -> [ScaleContext] -> Assertion
assertScopedAgreement result contexts = do
  let rootClasses = scopedRootClasses (sarRoots result)
  references <-
    expectAtlas $
      first ScopedReferenceFailed $
        runAtlasReferences
          scopedReferencePlan
          (sarRuleBook result)
          (sarFactBook result)
          (sarInitialGraph result)
          contexts
  expectAtlas $
    first ScopedAgreementFailed $
      assertReferenceAgreement
        scopedCost
        contexts
        rootClasses
        (sceContextGraph (pgGraph (srCarrier (sarReport result))))
        references

withScopedAtlas ::
  Int ->
  Int ->
  ScopedSupportCase ->
  (forall owner. ScopedAtlasResult owner -> Assertion) ->
  Assertion
withScopedAtlas contextCount termCount supportCase useResult = do
  site <- expectAtlas (first ScopedSiteFailed (scaledTree contextCount))
  secondaryProbe <- expectAtlas (maybe (Left ScopedSecondaryProbeMissing) Right (scaleSiteSecondaryProbe site))
  let terms = scopedFixtureTerms termCount
  mutation <-
    expectAtlas . first ScopedAllocationFailed $
      insertTermsTracked terms (emptyEGraph scopedAnalysisSpec)
  roots <- expectAtlas (maybe (Left ScopedPopulationMalformed) Right (scopedRootsFromClasses (emrResult mutation)))
  let primaryContext = supportProbeAnchor (scaleSitePrimaryProbe site)
      secondaryContext = supportProbeAnchor secondaryProbe
      inlineContext =
        case supportCase of
          ScopedBottomSupport -> scaleSiteBottom site
          _ -> primaryContext
      gatedContext =
        case supportCase of
          ScopedIntersectingSupport -> secondaryContext
          _ -> scaleSiteBottom site
  withEmptyContextEGraph (scaleSiteLattice site) (emrGraph mutation) $ \contextGraph -> do
    initialContextGraph <-
      expectAtlas . first ScopedOverlayFailed $
        contextMerge
          primaryContext
          (srOverlayArgument roots)
          (srOverlayAlias roots)
          contextGraph
    ruleBook <-
      expectAtlas . first ScopedRuleBookFailed $
        SheafTwist.supportedRuleBook
          (cegSite initialContextGraph)
          [ SheafTwist.SupportedRuleSpec (principalSupport (scaleSiteBottom site)) (scopedBetaRule (RewriteRuleId 0) globalBinder),
            SheafTwist.SupportedRuleSpec (principalSupport primaryContext) (scopedBetaRule (RewriteRuleId 1) overlayBinder),
            SheafTwist.SupportedRuleSpec (principalSupport inlineContext) (scopedLocalEtaRule (RewriteRuleId 2) inlineBinder "inline"),
            SheafTwist.SupportedRuleSpec (principalSupport gatedContext) (scopedFactGatedEtaRule (RewriteRuleId 3) pureBinder)
          ]
    factBook <-
      expectAtlas . first ScopedFactBookFailed $
        SheafTwist.supportedFactBook
          (cegSite initialContextGraph)
          [ SheafTwist.SupportedFactSpec
              (principalSupport primaryContext)
              (scopedBinderIndependentFactRule (FactRuleId 0) "pure")
          ]
    let proofGraph0 = emptySaturatingProofEGraph initialContextGraph
    supportPlan <-
      expectAtlas . first ScopedPlanFailed $
        prepareEGraphSupportPlan
          Nothing
          (const (staticRewriteContextSnapshot scopedRuntimeCapabilities))
          scopedAtlasPlan
          ruleBook
          factBook
          proofGraph0
    report <-
      expectAtlas . first ScopedRunFailed $
        runAtlasProgram
          (NonEmpty.toList (scaleSiteContexts site))
          (fmap crrResult . runEGraphSupportPlan defaultProofAnnotationBuilder mempty supportPlan)
          proofGraph0
    useResult
      ScopedAtlasResult
        { sarSite = site,
          sarRoots = roots,
          sarInitialGraph = initialContextGraph,
          sarRuleBook = ruleBook,
          sarFactBook = factBook,
          sarReport = report
        }

scopedAtlasPlan ::
  PlanSpec (ScopedAtlasU owner) (SatGraph (ScopedAtlasU owner)) RewriteRuleId
scopedAtlasPlan =
  withSchedulerConfig
    deterministicSchedulerConfig
    (planSpec scopedBudget GenericJoinMatching scopedRuntimeCapabilities)

scopedReferencePlan ::
  PlanSpec
    (EGraphU UnitContextSiteOwner () ScopedF () TrivialContext)
    (SatGraph (EGraphU UnitContextSiteOwner () ScopedF () TrivialContext))
    RewriteRuleId
scopedReferencePlan =
  withSchedulerConfig
    deterministicSchedulerConfig
    (planSpec scopedBudget GenericJoinMatching scopedRuntimeCapabilities)

scopedRuntimeCapabilities ::
  RewriteRuntimeCapabilities (GuardCapabilityResolver ()) ScopedF
scopedRuntimeCapabilities =
  withRuntimeBinderSubstAlgebra
    scopedBinderSubstAlgebra
    emptyRewriteRuntimeCapabilities

scopedBudget :: SaturationBudget
scopedBudget =
  SaturationBudget
    { sbMaxIterations = 8,
      sbMaxNodes = 20000
    }

scopedFixtureTerms :: Int -> [Fix ScopedF]
scopedFixtureTerms termCount =
  [ scopedBetaRedex globalBinder,
    scopedBetaContractum,
    overlayBetaRedex,
    overlayBetaTarget,
    overlayArgument,
    overlayAlias,
    inlineRedex,
    inlineTarget,
    pureRedex,
    pureTarget
  ]
    <> fmap scopedBackgroundTerm [0 .. termCount - 1]

scopedRootsFromClasses :: [ClassId] -> Maybe ScopedRoots
scopedRootsFromClasses =
  \case
    globalRedexClass : globalTargetClass : overlayRedexClass : overlayTargetClass : overlayArgumentClass : overlayAliasClass : inlineRedexClass : inlineTargetClass : pureRedexClass : pureTargetClass : _ ->
      Just
        ScopedRoots
          { srGlobalBetaRedex = globalRedexClass,
            srGlobalBetaTarget = globalTargetClass,
            srOverlayBetaRedex = overlayRedexClass,
            srOverlayBetaTarget = overlayTargetClass,
            srOverlayArgument = overlayArgumentClass,
            srOverlayAlias = overlayAliasClass,
            srInlineRedex = inlineRedexClass,
            srInlineTarget = inlineTargetClass,
            srPureRedex = pureRedexClass,
            srPureTarget = pureTargetClass
          }
    _ -> Nothing

scopedRootClasses :: ScopedRoots -> [ClassId]
scopedRootClasses roots =
  [ srGlobalBetaRedex roots,
    srGlobalBetaTarget roots,
    srOverlayBetaRedex roots,
    srOverlayBetaTarget roots,
    srOverlayArgument roots,
    srOverlayAlias roots,
    srInlineRedex roots,
    srInlineTarget roots,
    srPureRedex roots,
    srPureTarget roots
  ]

scopedBackgroundTerm :: Int -> Fix ScopedF
scopedBackgroundTerm termIndex =
  let binderId = BinderId (1000 + termIndex)
      freeName = "scope-" <> show termIndex
   in scopedApp
        (scopedLam binderId (scopedLocal binderId))
        (scopedFree freeName)

globalBinder :: BinderId
globalBinder =
  BinderId 0

overlayBinder :: BinderId
overlayBinder =
  BinderId 1

inlineBinder :: BinderId
inlineBinder =
  BinderId 2

pureBinder :: BinderId
pureBinder =
  BinderId 3

overlayArgument :: Fix ScopedF
overlayArgument =
  scopedApp (scopedFree "long") (scopedFree "argument")

overlayAlias :: Fix ScopedF
overlayAlias =
  scopedFree "short"

overlayBetaRedex :: Fix ScopedF
overlayBetaRedex =
  scopedApp
    (scopedLam overlayBinder (scopedApp (scopedFree "f") (scopedLocal overlayBinder)))
    overlayArgument

overlayBetaTarget :: Fix ScopedF
overlayBetaTarget =
  scopedApp (scopedFree "f") overlayAlias

inlineRedex :: Fix ScopedF
inlineRedex =
  scopedEtaRedex inlineBinder "inline"

inlineTarget :: Fix ScopedF
inlineTarget =
  scopedEtaContractum "inline"

pureRedex :: Fix ScopedF
pureRedex =
  scopedEtaRedex pureBinder "pure"

pureTarget :: Fix ScopedF
pureTarget =
  scopedEtaContractum "pure"

assertExtracts ::
  ScopedAtlasResult owner ->
  ClassId ->
  Fix ScopedF ->
  ScaleContext ->
  Assertion
assertExtracts result rootClass expectedTerm contextValue =
  case
      extractAtlasAtContext
        scopedCost
        contextValue
        rootClass
        (sceContextGraph (pgGraph (srCarrier (sarReport result))))
    of
      Left extractionError ->
        assertFailure
          ( "scoped extraction failed at "
              <> show contextValue
              <> ": "
              <> show extractionError
          )
      Right extraction ->
        assertBool
          ("unexpected scoped extraction at " <> show contextValue)
          (OrderedFix (atlasExtractionTerm extraction) == OrderedFix expectedTerm)

expectAtlas :: Show errorValue => Either errorValue value -> IO value
expectAtlas outcome =
  case outcome of
    Left obstruction -> assertFailure (show obstruction)
    Right value -> pure value
