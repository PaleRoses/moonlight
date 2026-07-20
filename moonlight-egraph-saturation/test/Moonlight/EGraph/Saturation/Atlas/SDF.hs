{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Moonlight.EGraph.Saturation.Atlas.SDF
  ( tests,
  )
where

import Data.Bifunctor (first)
import Data.Fix (Fix)
import Data.Foldable (traverse_)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core
  ( ClassId,
    OrderedFix (..),
    RewriteRuleId,
    UnionFindAllocationError,
    classIdKey,
  )
import Moonlight.EGraph.Pure.Change (EGraphMutationResult (..))
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    withEmptyContextEGraph,
  )
import Moonlight.EGraph.Pure.Context (cegSite)
import Moonlight.EGraph.Pure.Context.Proof
  ( ProofGraph (pgGraph),
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (insertTermsTracked)
import Moonlight.EGraph.Pure.Extraction
  ( ExtractionResult (..),
    ExtractionTable,
    extractAllFromTable,
    extractionCanonicalClass,
    liftCostAlgebra,
  )
import Moonlight.EGraph.Pure.Saturation.Extraction
  ( contextualExtractionTable,
  )
import Moonlight.EGraph.Pure.Saturation.Matching (MatchingStrategy (GenericJoinMatching))
import Moonlight.EGraph.Pure.Saturation.Substrate (EGraphU)
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingProofEGraph,
    emptySaturatingProofEGraph,
    sceContextGraph,
  )
import Moonlight.EGraph.Pure.Types
  ( emptyEGraph,
  )
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
    scaledDiamondStack,
  )
import Moonlight.EGraph.Test.SDF.Core
  ( Depth,
    SDFF,
    box,
    depthAnalysis,
    nonDegenerateRadiusFactRule,
    sdfCoarseApproximationRule,
    sdfComplementLaws,
    sdfCost,
    sdfEmpty,
    sdfLatticeLaws,
    sdfRawRewriteRule,
    sdfSmoothBlendLaws,
    sdfUnion,
    seededSDFTerms,
    smoothUnion,
    sphere,
  )
import Moonlight.FiniteLattice (upperCovers)
import Moonlight.Rewrite.ProofContext (principalSupport)
import Moonlight.Rewrite.ProofContext (defaultProofAnnotationBuilder)
import Moonlight.Rewrite.System (RewriteCondition)
import Moonlight.Rewrite.System (FactRule)
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

type SDFAtlasU owner = EGraphU owner () SDFF Depth ScaleContext

type SDFAtlasReport owner =
  SupportSaturationReportFor
    (SDFAtlasU owner)
    (SaturatingProofEGraph owner () SDFF Depth ScaleContext ())

type SDFAtlasRuleBook owner =
  SheafTwist.SupportedRuleBook
    owner
    ScaleContext
    (RawRewriteRule (RewriteCondition () SDFF) SDFF)

type SDFAtlasFactBook owner =
  SheafTwist.SupportedFactBook
    owner
    ScaleContext
    (FactRule () SDFF)

data SDFSupportCase
  = SDFArmSupport
  | SDFBottomSupport
  | SDFIntersectingSupport
  deriving stock (Eq, Ord, Show)

data SDFAtlasObstruction owner
  = SDFSiteFailed !ScaleSiteError
  | SDFSecondaryProbeMissing
  | SDFPopulationMalformed
  | SDFAllocationFailed !UnionFindAllocationError
  | SDFRuleBookFailed !(PreparedContextSupportError ScaleContext)
  | SDFFactBookFailed !(PreparedContextSupportError ScaleContext)
  | SDFPlanFailed !(SaturationError (SDFAtlasU owner) (SupportScheduleGroup (SDFAtlasU owner)))
  | SDFRunFailed
      !( AtlasRunObstruction
           ScaleContext
           (SaturationError (SDFAtlasU owner) (SupportScheduleGroup (SDFAtlasU owner)))
       )
  | SDFReferenceFailed
      !( AtlasReferenceObstruction
           ScaleContext
           (SaturationError (EGraphU UnitContextSiteOwner () SDFF Depth TrivialContext) RewriteRuleId)
       )
  | SDFAgreementFailed !(AtlasAgreementObstruction ScaleContext)
  deriving stock (Show)

data SDFRoots = SDFRoots
  { sdfCoarseRedexClass :: !ClassId,
    sdfCoarseTargetClass :: !ClassId,
    sdfGlobalRedexClass :: !ClassId,
    sdfGlobalTargetClass :: !ClassId,
    sdfGeneratedClasses :: ![ClassId]
  }

data SDFAtlasResult owner = SDFAtlasResult
  { sdfAtlasSite :: !ScaleSite,
    sdfAtlasRoots :: !SDFRoots,
    sdfAtlasInitialGraph :: !(ContextEGraph owner SDFF Depth ScaleContext),
    sdfAtlasRuleBook :: !(SDFAtlasRuleBook owner),
    sdfAtlasFactBook :: !(SDFAtlasFactBook owner),
    sdfAtlasReport :: !(SDFAtlasReport owner)
  }

tests :: TestTree
tests =
  testGroup
    "atlas-sdf"
    [ testCase "coarse approximation persists through its full LOD up-set" monotonicityLaw,
      testCase "coarse approximation refuses bottom and outside frontier" discriminationLaw,
      testCase "bottom approximation degenerates to every LOD" degeneracyLaw,
      testCase "positive-radius fact and coarse rule fire exactly on their LOD overlap" factGatingLaw,
      testCase "global CSG simplification remains available at every LOD" globalSimplificationLaw,
      testCase "extraction cost never worsens along LOD refinement" extractionCostLaw,
      testCase "support driver agrees with exhaustive generated SDF saturation" agreementLaw
    ]

monotonicityLaw :: Assertion
monotonicityLaw =
  withSDFAtlas 16 500 SDFArmSupport $ \result ->
    traverse_
      (assertExtracts result (sdfCoarseRedexClass (sdfAtlasRoots result)) coarseTarget)
      (NonEmpty.toList (supportProbeUpset (scaleSitePrimaryProbe (sdfAtlasSite result))))

discriminationLaw :: Assertion
discriminationLaw =
  withSDFAtlas 16 500 SDFArmSupport $ \result -> do
    let site = sdfAtlasSite result
        probe = scaleSitePrimaryProbe site
        refusalContexts =
          Set.toAscList
            (Set.fromList (scaleSiteBottom site : supportProbeFrontier probe))
    traverse_
      (assertExtracts result (sdfCoarseRedexClass (sdfAtlasRoots result)) coarseRedex)
      refusalContexts

degeneracyLaw :: Assertion
degeneracyLaw =
  withSDFAtlas 16 500 SDFBottomSupport $ \result ->
    traverse_
      (assertExtracts result (sdfCoarseRedexClass (sdfAtlasRoots result)) coarseTarget)
      (NonEmpty.toList (scaleSiteContexts (sdfAtlasSite result)))

factGatingLaw :: Assertion
factGatingLaw =
  withSDFAtlas 16 500 SDFIntersectingSupport $ \result -> do
    let site = sdfAtlasSite result
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
            (sdfCoarseRedexClass (sdfAtlasRoots result))
            (if Set.member contextValue overlap then coarseTarget else coarseRedex)
            contextValue
      )
      (NonEmpty.toList (scaleSiteContexts site))

globalSimplificationLaw :: Assertion
globalSimplificationLaw =
  withSDFAtlas 16 500 SDFArmSupport $ \result ->
    traverse_
      (assertExtracts result (sdfGlobalRedexClass (sdfAtlasRoots result)) globalTarget)
      (NonEmpty.toList (scaleSiteContexts (sdfAtlasSite result)))

extractionCostLaw :: Assertion
extractionCostLaw =
  withSDFAtlas 16 500 SDFArmSupport $ \result -> do
    coverEdges <- expectCoverEdges (sdfAtlasSite result)
    let saturatedContextGraph = sceContextGraph (pgGraph (srCarrier (sdfAtlasReport result)))
        contextGraphs =
          Map.fromList
            ( fmap
                (\contextValue -> (contextValue, saturatedContextGraph))
                (NonEmpty.toList (scaleSiteContexts (sdfAtlasSite result)))
            )
        observedRoots =
          sdfCoarseRedexClass (sdfAtlasRoots result)
            : sdfGeneratedClasses (sdfAtlasRoots result)
    contextualCosts <-
      Map.traverseWithKey extractionCostsAtContext contextGraphs
    traverse_
      ( \rootClass ->
          traverse_
            (assertCostRefines contextualCosts rootClass)
            coverEdges
      )
      observedRoots

agreementLaw :: Assertion
agreementLaw =
  withSDFAtlas 2 32 SDFArmSupport $ \exhaustiveResult -> do
    assertSDFAgreement
      exhaustiveResult
      (NonEmpty.toList (scaleSiteContexts (sdfAtlasSite exhaustiveResult)))
    withSDFAtlas 16 500 SDFArmSupport $ \sampledResult ->
      assertSDFAgreement
        sampledResult
        (NonEmpty.toList (scaleSiteSampledContexts (sdfAtlasSite sampledResult)))

assertSDFAgreement :: SDFAtlasResult owner -> [ScaleContext] -> Assertion
assertSDFAgreement result contexts = do
  let roots = sdfAgreementRoots (sdfAtlasRoots result)
  references <-
    expectAtlas $
      first SDFReferenceFailed $
        runAtlasReferences
          sdfReferencePlan
          (sdfAtlasRuleBook result)
          (sdfAtlasFactBook result)
          (sdfAtlasInitialGraph result)
          contexts
  expectAtlas $
    first SDFAgreementFailed $
      assertReferenceAgreement
        sdfCost
        contexts
        roots
        (sceContextGraph (pgGraph (srCarrier (sdfAtlasReport result))))
        references

withSDFAtlas ::
  Int ->
  Int ->
  SDFSupportCase ->
  (forall owner. SDFAtlasResult owner -> Assertion) ->
  Assertion
withSDFAtlas diamondCount termCount supportCase useResult = do
  site <- expectAtlas (first SDFSiteFailed (scaledDiamondStack diamondCount))
  secondaryProbe <- expectAtlas (maybe (Left SDFSecondaryProbeMissing) Right (scaleSiteSecondaryProbe site))
  mutation <-
    expectAtlas
      ( first SDFAllocationFailed
          ( insertTermsTracked
              (sdfFixtureTerms termCount)
              (emptyEGraph depthAnalysis)
          )
      )
  roots <- expectAtlas (maybe (Left SDFPopulationMalformed) Right (sdfRootsFromClasses (emrResult mutation)))
  let primaryContext = supportProbeAnchor (scaleSitePrimaryProbe site)
      secondaryContext = supportProbeAnchor secondaryProbe
      factContext =
        case supportCase of
          SDFBottomSupport -> scaleSiteBottom site
          _ -> primaryContext
      coarseRuleContext =
        case supportCase of
          SDFIntersectingSupport -> secondaryContext
          _ -> scaleSiteBottom site
  withEmptyContextEGraph (scaleSiteLattice site) (emrGraph mutation) $ \initialContextGraph -> do
    ruleBook <-
      expectAtlas . first SDFRuleBookFailed $
        SheafTwist.supportedRuleBook
          (cegSite initialContextGraph)
          ( fmap
              (SheafTwist.SupportedRuleSpec (principalSupport (scaleSiteBottom site)))
              sdfAtlasGlobalRules
              <> [ SheafTwist.SupportedRuleSpec
                     (principalSupport coarseRuleContext)
                     sdfCoarseApproximationRule
                 ]
          )
    factBook <-
      expectAtlas . first SDFFactBookFailed $
        SheafTwist.supportedFactBook
          (cegSite initialContextGraph)
          [ SheafTwist.SupportedFactSpec
              (principalSupport factContext)
              nonDegenerateRadiusFactRule
          ]
    let proofGraph0 = emptySaturatingProofEGraph initialContextGraph
    supportPlan <-
      expectAtlas . first SDFPlanFailed $
        prepareEGraphSupportPlan
          Nothing
          (const (staticRewriteContextSnapshot emptyRewriteRuntimeCapabilities))
          sdfAtlasPlan
          ruleBook
          factBook
          proofGraph0
    report <-
      expectAtlas . first SDFRunFailed $
        runAtlasProgram
          (NonEmpty.toList (scaleSiteContexts site))
          (fmap crrResult . runEGraphSupportPlan defaultProofAnnotationBuilder mempty supportPlan)
          proofGraph0
    useResult
      SDFAtlasResult
        { sdfAtlasSite = site,
          sdfAtlasRoots = roots,
          sdfAtlasInitialGraph = initialContextGraph,
          sdfAtlasRuleBook = ruleBook,
          sdfAtlasFactBook = factBook,
          sdfAtlasReport = report
        }

sdfAtlasGlobalRules :: [RawRewriteRule (RewriteCondition capability SDFF) SDFF]
sdfAtlasGlobalRules =
  fmap
    sdfRawRewriteRule
    (sdfLatticeLaws <> sdfComplementLaws <> sdfSmoothBlendLaws)

sdfAtlasPlan ::
  PlanSpec (SDFAtlasU owner) (SatGraph (SDFAtlasU owner)) RewriteRuleId
sdfAtlasPlan =
  withSchedulerConfig
    deterministicSchedulerConfig
    (planSpec sdfBudget GenericJoinMatching emptyRewriteRuntimeCapabilities)

sdfReferencePlan ::
  PlanSpec
    (EGraphU UnitContextSiteOwner () SDFF Depth TrivialContext)
    (SatGraph (EGraphU UnitContextSiteOwner () SDFF Depth TrivialContext))
    RewriteRuleId
sdfReferencePlan =
  withSchedulerConfig
    deterministicSchedulerConfig
    (planSpec sdfBudget GenericJoinMatching emptyRewriteRuntimeCapabilities)

sdfBudget :: SaturationBudget
sdfBudget =
  SaturationBudget
    { sbMaxIterations = 8,
      sbMaxNodes = 30000
    }

sdfFixtureTerms :: Int -> [Fix SDFF]
sdfFixtureTerms termCount =
  [coarseRedex, coarseTarget, globalRedex, globalTarget]
    <> sdfPopulation termCount

sdfPopulation :: Int -> [Fix SDFF]
sdfPopulation termCount =
  take termCount $
    seededSDFTerms 1103 1 ((termCount + 1) `div` 2)
      <> seededSDFTerms 2909 2 ((termCount + 1) `div` 2)

sdfRootsFromClasses :: [ClassId] -> Maybe SDFRoots
sdfRootsFromClasses =
  \case
    coarseRedexClass : coarseTargetClass : globalRedexClass : globalTargetClass : generatedClasses ->
      Just
        SDFRoots
          { sdfCoarseRedexClass = coarseRedexClass,
            sdfCoarseTargetClass = coarseTargetClass,
            sdfGlobalRedexClass = globalRedexClass,
            sdfGlobalTargetClass = globalTargetClass,
            sdfGeneratedClasses = generatedClasses
          }
    _ -> Nothing

sdfAgreementRoots :: SDFRoots -> [ClassId]
sdfAgreementRoots roots =
  [ sdfCoarseRedexClass roots,
    sdfCoarseTargetClass roots,
    sdfGlobalRedexClass roots,
    sdfGlobalTargetClass roots
  ]
    <> take 4 (sdfGeneratedClasses roots)

coarseRedex :: Fix SDFF
coarseRedex =
  smoothUnion 0.5 (sphere 2.0) (box 1.0 2.0 3.0)

coarseTarget :: Fix SDFF
coarseTarget =
  sdfUnion (sphere 2.0) (box 1.0 2.0 3.0)

globalRedex :: Fix SDFF
globalRedex =
  sdfUnion globalTarget sdfEmpty

globalTarget :: Fix SDFF
globalTarget =
  sphere 3.0

assertExtracts ::
  SDFAtlasResult owner ->
  ClassId ->
  Fix SDFF ->
  ScaleContext ->
  Assertion
assertExtracts result rootClass expectedTerm contextValue =
  case
      extractAtlasAtContext
        sdfCost
        contextValue
        rootClass
        (sceContextGraph (pgGraph (srCarrier (sdfAtlasReport result))))
    of
      Left extractionError ->
        assertFailure
          ( "SDF extraction failed at "
              <> show contextValue
              <> ": "
              <> show extractionError
          )
      Right extraction ->
        assertBool
          ("unexpected SDF extraction at " <> show contextValue)
          (OrderedFix (atlasExtractionTerm extraction) == OrderedFix expectedTerm)

expectCoverEdges :: ScaleSite -> IO [(ScaleContext, ScaleContext)]
expectCoverEdges site =
  fmap concat $
    traverse
      ( \lowerContext ->
          fmap (fmap ((,) lowerContext)) $
            expectAtlas $
              first ((,) lowerContext) $
                upperCovers (scaleSiteLattice site) lowerContext
      )
      (NonEmpty.toList (scaleSiteContexts site))

assertCostRefines ::
  Map.Map ScaleContext (ExtractionTable SDFF Depth, IntMap.IntMap Int) ->
  ClassId ->
  (ScaleContext, ScaleContext) ->
  Assertion
assertCostRefines localizedGraphs rootClass (lowerContext, upperContext) = do
  lowerCost <- extractionCostAt localizedGraphs rootClass lowerContext
  upperCost <- extractionCostAt localizedGraphs rootClass upperContext
  assertBool
    ("SDF extraction cost worsened from " <> show lowerContext <> " to " <> show upperContext)
    (upperCost <= lowerCost)

extractionCostAt ::
  Map.Map ScaleContext (ExtractionTable SDFF Depth, IntMap.IntMap Int) ->
  ClassId ->
  ScaleContext ->
  IO Int
extractionCostAt contextualTables rootClass contextValue =
  case Map.lookup contextValue contextualTables of
    Nothing -> assertFailure ("missing contextual SDF table at " <> show contextValue)
    Just (table, costs) ->
      case extractionCanonicalClass table rootClass of
        Nothing ->
          assertFailure
            ( "missing contextual SDF root at "
                <> show contextValue
                <> " for "
                <> show rootClass
            )
        Just canonicalRoot ->
          case IntMap.lookup (classIdKey canonicalRoot) costs of
            Nothing ->
              assertFailure
                ( "missing SDF extraction cost at "
                    <> show contextValue
                    <> " for "
                    <> show rootClass
                )
            Just cost -> pure cost

extractionCostsAtContext ::
  ScaleContext ->
  ContextEGraph owner SDFF Depth ScaleContext ->
  IO (ExtractionTable SDFF Depth, IntMap.IntMap Int)
extractionCostsAtContext contextValue contextGraph =
  case contextualExtractionTable contextValue contextGraph of
    Left obstruction ->
      assertFailure
        ( "contextual SDF extraction table failed at "
            <> show contextValue
            <> ": "
            <> show obstruction
        )
    Right table ->
      pure
        ( table,
          fmap erCost (extractAllFromTable (liftCostAlgebra sdfCost) table)
        )

expectAtlas :: Show errorValue => Either errorValue value -> IO value
expectAtlas outcome =
  case outcome of
    Left obstruction -> assertFailure (show obstruction)
    Right value -> pure value
