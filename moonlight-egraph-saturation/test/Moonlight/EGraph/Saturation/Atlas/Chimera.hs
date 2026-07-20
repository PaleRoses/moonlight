{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Moonlight.EGraph.Saturation.Atlas.Chimera
  ( tests,
  )
where

import Data.Bifunctor (first)
import Data.Bits (xor)
import Data.Char (ord)
import Data.Fix (Fix)
import Data.Foldable (traverse_)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Word (Word64)
import Moonlight.Control.Schedule (identitySchedulerRefinement)
import Moonlight.Core
  ( ClassId,
    OrderedFix (..),
    ProofStepId,
    RewriteRuleId,
    Substitution,
    UnionFindAllocationError,
  )
import Moonlight.EGraph.Pure.Change (EGraphMutationResult (..))
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    withEmptyContextEGraph,
  )
import Moonlight.EGraph.Pure.Context (cegSite)
import Moonlight.EGraph.Pure.Context.Proof
  ( ProofGraph (pgGraph),
    serializeProofLog,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (insertTermsTracked)
import Moonlight.EGraph.Pure.Saturation.Matching (MatchingStrategy (GenericJoinMatching))
import Moonlight.EGraph.Pure.Saturation.Substrate
  ( EGraphSaturationChangeSummary (..),
    EGraphU,
  )
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingProofEGraph,
    cssQueryRegistry,
    emptyContextSaturationState,
    emptySaturatingProofEGraph,
    sceContextGraph,
    sceSaturationState,
  )
import Moonlight.EGraph.Pure.Types (emptyEGraph)
import Moonlight.EGraph.Test.Chimera.Core
  ( TissueCount,
    TissueF,
    baseTissueCost,
    bone,
    cartilage,
    compatibleGraftReductionRule,
    graft,
    graftAssociativityRule,
    graftCommuteRule,
    graftIdempotenceRule,
    keratin,
    renderTissueFix,
    tissueAnalysis,
    tissueCompatibilityFactRule,
  )
import Moonlight.EGraph.Test.Chimera.Population (tissueTermsAtDepth)
import Moonlight.EGraph.Test.Saturation
  ( deterministicSchedulerConfig,
    emptyRewriteRuntimeCapabilities,
    prepareEGraphSupportPlan,
    runEGraphSupportPlan,
    srCarrier,
    srIterations,
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
import Moonlight.Rewrite.ProofContext (principalSupport)
import Moonlight.Rewrite.ProofContext
  ( ProofContextEvidence,
    ProofKind,
    ProofStep (..),
    SupportAwareProofEvidence,
    defaultProofAnnotationBuilder,
  )
import Moonlight.Rewrite.System (GuardEvidence, RewriteCondition)
import Moonlight.Rewrite.System (FactDerivation, FactRule)
import Moonlight.Rewrite.System (RawRewriteRule)
import Moonlight.Saturation.Context.Error (SaturationError)
import Moonlight.Saturation.Context.Driver
  ( ContextRunResult,
    carrierGoal,
    contextExecutionSpec,
    crrResult,
  )
import Moonlight.Saturation.Context.Match.State.Registry qualified as QueryRegistry
import Moonlight.Saturation.Context.Program.Plan
  ( Plan,
    planProgram,
  )
import Moonlight.Saturation.Context.Program.Spec
  ( PlanSpec,
    planSpec,
    staticRewriteContextSnapshot,
    withSchedulerConfig,
  )
import Moonlight.Saturation.Core (SaturationBudget (..))
import Moonlight.Saturation.Context.Runtime.PlanIdentity (compiledContextQueries)
import Moonlight.Saturation.Context.Runtime.Policy (RuntimePolicy (..))
import Moonlight.Saturation.Context.Runtime.Report (srTracePayload)
import Moonlight.Saturation.Context.Runtime.State (RuntimeState (..))
import Moonlight.Saturation.Substrate
  ( QueryIndex (registerQueries),
    SatGraph,
    SatObstruction,
    TrivialContext,
  )
import Moonlight.Saturation.Support.Algebra (supportRuntimePolicy)
import Moonlight.Saturation.Support.Core
  ( SupportSaturationReportFor,
    SupportScheduleGroup,
  )
import Moonlight.Saturation.Support.Driver qualified as SupportDriver
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSupportError,
    UnitContextSiteOwner,
  )
import Moonlight.Sheaf.Twist.SupportedRuleSpec qualified as SheafTwist
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    (@?=),
    assertBool,
    assertFailure,
    testCase,
  )

type ChimeraAtlasU owner = EGraphU owner () TissueF TissueCount ScaleContext

type ChimeraAtlasReport owner =
  SupportSaturationReportFor
    (ChimeraAtlasU owner)
    (SaturatingProofEGraph owner () TissueF TissueCount ScaleContext ())

type ChimeraAtlasRuleBook owner =
  SheafTwist.SupportedRuleBook
    owner
    ScaleContext
    (RawRewriteRule (RewriteCondition () TissueF) TissueF)

type ChimeraAtlasFactBook owner =
  SheafTwist.SupportedFactBook
    owner
    ScaleContext
    (FactRule () TissueF)

type ChimeraAtlasCarrier owner =
  SaturatingProofEGraph owner () TissueF TissueCount ScaleContext ()

type ChimeraAtlasPlan owner =
  Plan
    (ChimeraAtlasU owner)
    (ChimeraAtlasCarrier owner)
    (SupportScheduleGroup (ChimeraAtlasU owner))

type ChimeraAtlasRunResult owner =
  ContextRunResult
    (ChimeraAtlasU owner)
    (ChimeraAtlasCarrier owner)
    (SupportScheduleGroup (ChimeraAtlasU owner))
    (ChimeraAtlasReport owner)

data ChimeraSupportCase
  = ArmFactSupport
  | BottomFactSupport
  | IntersectingBookSupport
  deriving stock (Eq, Ord, Show)

data ChimeraQueryRegistryMode
  = RetainQueryRegistry
  | ReconstructQueryRegistryEachRound
  deriving stock (Eq, Ord, Show)

data ChimeraAtlasObstruction owner
  = ChimeraSiteFailed !ScaleSiteError
  | ChimeraSecondaryProbeMissing
  | ChimeraRuleBookFailed !(PreparedContextSupportError ScaleContext)
  | ChimeraFactBookFailed !(PreparedContextSupportError ScaleContext)
  | ChimeraPopulationMalformed
  | ChimeraAllocationFailed !UnionFindAllocationError
  | ChimeraPlanFailed !(SaturationError (ChimeraAtlasU owner) (SupportScheduleGroup (ChimeraAtlasU owner)))
  | ChimeraRunFailed
      !( AtlasRunObstruction
           ScaleContext
           (SaturationError (ChimeraAtlasU owner) (SupportScheduleGroup (ChimeraAtlasU owner)))
       )
  | ChimeraReferenceFailed
      !( AtlasReferenceObstruction
           ScaleContext
           (SaturationError (EGraphU UnitContextSiteOwner () TissueF TissueCount TrivialContext) RewriteRuleId)
       )
  | ChimeraAgreementFailed !(AtlasAgreementObstruction ScaleContext)
  deriving stock (Show)

data ChimeraAtlasResult owner = ChimeraAtlasResult
  { carSite :: !ScaleSite,
    carWitnessClass :: !ClassId,
    carRootClasses :: ![ClassId],
    carOriginalTerm :: !(Fix TissueF),
    carInitialGraph :: !(ContextEGraph owner TissueF TissueCount ScaleContext),
    carRuleBook :: !(ChimeraAtlasRuleBook owner),
    carFactBook :: !(ChimeraAtlasFactBook owner),
    carReport :: !(ChimeraAtlasReport owner)
  }

data PreparedChimeraAtlas owner = PreparedChimeraAtlas
  { pcaSite :: !ScaleSite,
    pcaWitnessClass :: !ClassId,
    pcaRootClasses :: ![ClassId],
    pcaOriginalTerm :: !(Fix TissueF),
    pcaInitialGraph :: !(ContextEGraph owner TissueF TissueCount ScaleContext),
    pcaRuleBook :: !(ChimeraAtlasRuleBook owner),
    pcaFactBook :: !(ChimeraAtlasFactBook owner),
    pcaPlan :: !(ChimeraAtlasPlan owner),
    pcaInitialCarrier :: !(ChimeraAtlasCarrier owner)
  }

data ChimeraProofStepContent = ChimeraProofStepContent
  { cpscId :: !ProofStepId,
    cpscKind :: !ProofKind,
    cpscLhsClass :: !ClassId,
    cpscRhsClass :: !ClassId,
    cpscLhsWitness :: !(Maybe (OrderedFix TissueF)),
    cpscRhsWitness :: !(Maybe (OrderedFix TissueF)),
    cpscSubstitution :: !Substitution,
    cpscGuardEvidence :: !(Maybe GuardEvidence),
    cpscFactDerivations :: !(Set.Set FactDerivation),
    cpscContextEvidence :: !(Maybe (ProofContextEvidence ScaleContext)),
    cpscSupportEvidence :: !(Maybe (SupportAwareProofEvidence ScaleContext)),
    cpscAnnotation :: !(),
    cpscTimestamp :: !Int
  }
  deriving stock (Eq)

tests :: TestTree
tests =
  testGroup
    "atlas-chimera"
    [ testCase "structural populations preserve requested cardinality" populationCardinalityLaw,
      testCase "arm fact support selects the cheaper graft on its full up-set" armSupportLaw,
      testCase "arm fact support refuses the bottom and outside frontier" discriminationLaw,
      testCase "bottom fact support degenerates to global visibility" degeneracyLaw,
      testCase "distinct rule and fact books fire exactly on their overlap" twoBookLaw,
      testCase "support driver agrees with exhaustive per-context saturation" agreementLaw,
      testCase "retained query registries preserve full ordered proof content" retainedRegistryDeterminismLaw
    ]

populationCardinalityLaw :: Assertion
populationCardinalityLaw =
  traverse_
    ( \(termDepth, termCount) ->
        let distinctCount =
              Set.size
                (Set.fromList (OrderedFix <$> tissueTermsAtDepth termDepth termCount))
         in assertBool
              ( "expected "
                  <> show termCount
                  <> " distinct depth-"
                  <> show termDepth
                  <> " chimera terms, found "
                  <> show distinctCount
              )
              (distinctCount == termCount)
    )
    [(0, 200), (3, 64)]

armSupportLaw :: Assertion
armSupportLaw =
  withChimeraAtlas 32 200 ArmFactSupport $ \result -> do
    let primaryProbe = scaleSitePrimaryProbe (carSite result)
    traverse_
      (assertExtracts result bone)
      (NonEmpty.toList (supportProbeUpset primaryProbe))

discriminationLaw :: Assertion
discriminationLaw =
  withChimeraAtlas 32 200 ArmFactSupport $ \result -> do
    let site = carSite result
        primaryProbe = scaleSitePrimaryProbe site
        refusalContexts =
          Set.toAscList
            (Set.fromList (scaleSiteBottom site : supportProbeFrontier primaryProbe))
    traverse_
      (assertExtracts result (carOriginalTerm result))
      refusalContexts

degeneracyLaw :: Assertion
degeneracyLaw =
  withChimeraAtlas 32 200 BottomFactSupport $ \result -> do
    traverse_
      (assertExtracts result bone)
      (NonEmpty.toList (scaleSiteContexts (carSite result)))

twoBookLaw :: Assertion
twoBookLaw =
  withChimeraAtlas 32 200 IntersectingBookSupport $ \result -> do
    let site = carSite result
        primaryUpset =
          Set.fromList
            (NonEmpty.toList (supportProbeUpset (scaleSitePrimaryProbe site)))
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
            (if Set.member contextValue overlap then bone else carOriginalTerm result)
            contextValue
      )
      (NonEmpty.toList (scaleSiteContexts site))

agreementLaw :: Assertion
agreementLaw =
  withChimeraAtlas 8 24 ArmFactSupport $ \exhaustiveResult -> do
    assertChimeraAgreement
      exhaustiveResult
      (NonEmpty.toList (scaleSiteContexts (carSite exhaustiveResult)))
    withChimeraAtlas 32 200 ArmFactSupport $ \sampledResult ->
      assertChimeraAgreement
        sampledResult
        (NonEmpty.toList (scaleSiteSampledContexts (carSite sampledResult)))

retainedRegistryDeterminismLaw :: Assertion
retainedRegistryDeterminismLaw =
  withPreparedChimeraAtlas 8 24 ArmFactSupport $ \preparedAtlas -> do
    retainedResult <-
      expectAtlas
        (runPreparedChimeraAtlas RetainQueryRegistry preparedAtlas)
    reconstructedResult <-
      expectAtlas
        (runPreparedChimeraAtlas ReconstructQueryRegistryEachRound preparedAtlas)
    let retainedReport = carReport retainedResult
        reconstructedReport = carReport reconstructedResult
        retainedProofSteps = serializeProofLog (srCarrier retainedReport)
        reconstructedProofSteps = serializeProofLog (srCarrier reconstructedReport)
        retainedProofContent = fmap chimeraProofStepContent retainedProofSteps
        reconstructedProofContent = fmap chimeraProofStepContent reconstructedProofSteps
        retainedDigest = chimeraProofContentDigest retainedProofSteps
        reconstructedDigest = chimeraProofContentDigest reconstructedProofSteps
        retainedRegistrySize =
          length
            ( QueryRegistry.registeredQueryIds
                ( cssQueryRegistry
                    (sceSaturationState (pgGraph (srCarrier retainedReport)))
                )
            )
        retainedSummary = srTracePayload retainedReport
    assertBool
      ("expected a genuinely multi-query retained registry, found " <> show retainedRegistrySize)
      (retainedRegistrySize >= 2)
    assertBool
      ("expected a multi-round Chimera Atlas execution, found " <> show (srIterations retainedReport))
      (srIterations retainedReport >= 2)
    assertBool "expected retained proof content to be nonempty" (not (null retainedProofContent))
    assertBool
      ( "retained and forced-fresh proof steps differ; retained digest="
          <> show retainedDigest
          <> ", reconstructed digest="
          <> show reconstructedDigest
      )
      (retainedProofContent == reconstructedProofContent)
    retainedDigest @?= reconstructedDigest
    assertBool
      "retained execution must construct at least one batch restriction registry"
      (egscProofRestrictionRegistryConstructions retainedSummary > 0)
    assertBool
      "retained execution must construct at least one contextual extraction table"
      (egscProofExtractionTableConstructions retainedSummary > 0)

assertChimeraAgreement :: ChimeraAtlasResult owner -> [ScaleContext] -> Assertion
assertChimeraAgreement result contexts = do
  references <-
    expectAtlas $
      first ChimeraReferenceFailed $
        runAtlasReferences
          chimeraReferencePlan
          (carRuleBook result)
          (carFactBook result)
          (carInitialGraph result)
          contexts
  expectAtlas $
    first ChimeraAgreementFailed $
      assertReferenceAgreement
        baseTissueCost
        contexts
        (carRootClasses result)
        (sceContextGraph (pgGraph (srCarrier (carReport result))))
        references

withChimeraAtlas ::
  Int ->
  Int ->
  ChimeraSupportCase ->
  (forall owner. ChimeraAtlasResult owner -> Assertion) ->
  Assertion
withChimeraAtlas contextCount termCount supportCase useResult =
  withPreparedChimeraAtlas contextCount termCount supportCase $ \preparedAtlas ->
    expectAtlas (runPreparedChimeraAtlas RetainQueryRegistry preparedAtlas)
      >>= useResult

withPreparedChimeraAtlas ::
  Int ->
  Int ->
  ChimeraSupportCase ->
  (forall owner. PreparedChimeraAtlas owner -> Assertion) ->
  Assertion
withPreparedChimeraAtlas contextCount termCount supportCase usePrepared = do
  site <- expectAtlas (first ChimeraSiteFailed (scaledTree contextCount))
  secondaryProbe <- expectAtlas (maybe (Left ChimeraSecondaryProbeMissing) Right (scaleSiteSecondaryProbe site))
  let originalTerm = graft keratin cartilage
  mutation <-
    expectAtlas
      ( first ChimeraAllocationFailed
          ( insertTermsTracked
              (bone : originalTerm : tissueTermsAtDepth 0 termCount)
              (emptyEGraph tissueAnalysis)
          )
      )
  (witnessClass, rootClasses) <- expectAtlas $
    case emrResult mutation of
      target : witness : backgroundRoots ->
        Right (witness, target : witness : take 4 backgroundRoots)
      _ -> Left ChimeraPopulationMalformed
  let primaryContext = supportProbeAnchor (scaleSitePrimaryProbe site)
      secondaryContext = supportProbeAnchor secondaryProbe
      reductionContext =
        case supportCase of
          IntersectingBookSupport -> secondaryContext
          _ -> scaleSiteBottom site
      factContext =
        case supportCase of
          BottomFactSupport -> scaleSiteBottom site
          _ -> primaryContext
  withEmptyContextEGraph (scaleSiteLattice site) (emrGraph mutation) $ \initialContextGraph -> do
    ruleBook <-
      expectAtlas . first ChimeraRuleBookFailed $
        SheafTwist.supportedRuleBook
          (cegSite initialContextGraph)
          [ SheafTwist.SupportedRuleSpec (principalSupport (scaleSiteBottom site)) graftCommuteRule,
            SheafTwist.SupportedRuleSpec (principalSupport primaryContext) graftAssociativityRule,
            SheafTwist.SupportedRuleSpec (principalSupport secondaryContext) graftIdempotenceRule,
            SheafTwist.SupportedRuleSpec (principalSupport reductionContext) compatibleGraftReductionRule
          ]
    factBook <-
      expectAtlas . first ChimeraFactBookFailed $
        SheafTwist.supportedFactBook
          (cegSite initialContextGraph)
          [SheafTwist.SupportedFactSpec (principalSupport factContext) tissueCompatibilityFactRule]
    let proofGraph0 = emptySaturatingProofEGraph initialContextGraph
    supportPlan <-
      expectAtlas . first ChimeraPlanFailed $
        prepareEGraphSupportPlan
          Nothing
          (const (staticRewriteContextSnapshot emptyRewriteRuntimeCapabilities))
          chimeraAtlasPlan
          ruleBook
          factBook
          proofGraph0
    usePrepared
      PreparedChimeraAtlas
        { pcaSite = site,
          pcaWitnessClass = witnessClass,
          pcaRootClasses = rootClasses,
          pcaOriginalTerm = originalTerm,
          pcaInitialGraph = initialContextGraph,
          pcaRuleBook = ruleBook,
          pcaFactBook = factBook,
          pcaPlan = supportPlan,
          pcaInitialCarrier = proofGraph0
        }

runPreparedChimeraAtlas ::
  ChimeraQueryRegistryMode ->
  PreparedChimeraAtlas owner ->
  Either (ChimeraAtlasObstruction owner) (ChimeraAtlasResult owner)
runPreparedChimeraAtlas registryMode preparedAtlas = do
  report <-
    first ChimeraRunFailed $
      runAtlasProgram
        (NonEmpty.toList (scaleSiteContexts (pcaSite preparedAtlas)))
        ( fmap crrResult
            . runChimeraAtlasPlan
              registryMode
              (pcaPlan preparedAtlas)
        )
        (pcaInitialCarrier preparedAtlas)
  pure
    ChimeraAtlasResult
      { carSite = pcaSite preparedAtlas,
        carWitnessClass = pcaWitnessClass preparedAtlas,
        carRootClasses = pcaRootClasses preparedAtlas,
        carOriginalTerm = pcaOriginalTerm preparedAtlas,
        carInitialGraph = pcaInitialGraph preparedAtlas,
        carRuleBook = pcaRuleBook preparedAtlas,
        carFactBook = pcaFactBook preparedAtlas,
        carReport = report
      }

runChimeraAtlasPlan ::
  ChimeraQueryRegistryMode ->
  ChimeraAtlasPlan owner ->
  ChimeraAtlasCarrier owner ->
  Either
    (SaturationError (ChimeraAtlasU owner) (SupportScheduleGroup (ChimeraAtlasU owner)))
    (ChimeraAtlasRunResult owner)
runChimeraAtlasPlan registryMode planValue =
  case registryMode of
    RetainQueryRegistry ->
      runEGraphSupportPlan
        defaultProofAnnotationBuilder
        mempty
        planValue
    ReconstructQueryRegistryEachRound ->
      SupportDriver.runSupportPlan
        ( contextExecutionSpec
            (reconstructingQueryRegistryPolicy planValue)
            (carrierGoal mempty)
        )
        planValue

reconstructingQueryRegistryPolicy ::
  ChimeraAtlasPlan owner ->
  RuntimePolicy
    (ChimeraAtlasU owner)
    (ChimeraAtlasCarrier owner)
    (SupportScheduleGroup (ChimeraAtlasU owner))
    (ChimeraAtlasReport owner)
reconstructingQueryRegistryPolicy planValue =
  retainedPolicy
    { rpApply =
        \rewriteContext supportedMatches runtimeState ->
          fmap
            (fmap chimeraCarrierWithoutQueryRegistry)
            (rpApply retainedPolicy rewriteContext supportedMatches runtimeState),
      rpBootstrap =
        \runtimeState ->
          reconstructRuntimeQueryRegistry planValue runtimeState
            >>= rpBootstrap retainedPolicy,
      rpRebuild =
        \runtimeState ->
          reconstructRuntimeQueryRegistry planValue runtimeState
            >>= rpRebuild retainedPolicy
    }
  where
    retainedPolicy =
      supportRuntimePolicy
        identitySchedulerRefinement
        defaultProofAnnotationBuilder

reconstructRuntimeQueryRegistry ::
  forall owner.
  ChimeraAtlasPlan owner ->
  RuntimeState
    (ChimeraAtlasU owner)
    (ChimeraAtlasCarrier owner)
    (SupportScheduleGroup (ChimeraAtlasU owner)) ->
  Either
    (SatObstruction (ChimeraAtlasU owner))
    ( RuntimeState
        (ChimeraAtlasU owner)
        (ChimeraAtlasCarrier owner)
        (SupportScheduleGroup (ChimeraAtlasU owner))
    )
reconstructRuntimeQueryRegistry planValue runtimeState = do
  let clearedCarrier =
        chimeraCarrierWithoutQueryRegistry (rsCarrier runtimeState)
  registeredGraph <-
    registerQueries @(ChimeraAtlasU owner)
      (compiledContextQueries @(ChimeraAtlasU owner) (planProgram planValue))
      (pgGraph clearedCarrier)
  pure
    runtimeState
      { rsCarrier =
          clearedCarrier
            { pgGraph = registeredGraph
            }
      }

chimeraCarrierWithoutQueryRegistry ::
  ChimeraAtlasCarrier owner ->
  ChimeraAtlasCarrier owner
chimeraCarrierWithoutQueryRegistry proofGraph =
  proofGraph
    { pgGraph =
        (pgGraph proofGraph)
          { sceSaturationState = emptyContextSaturationState
          }
    }

chimeraProofStepContent ::
  ProofStep TissueF ScaleContext () ->
  ChimeraProofStepContent
chimeraProofStepContent proofStep =
  ChimeraProofStepContent
    { cpscId = psId proofStep,
      cpscKind = psKind proofStep,
      cpscLhsClass = psLhsClass proofStep,
      cpscRhsClass = psRhsClass proofStep,
      cpscLhsWitness = OrderedFix <$> psLhsWitness proofStep,
      cpscRhsWitness = OrderedFix <$> psRhsWitness proofStep,
      cpscSubstitution = psSubstitution proofStep,
      cpscGuardEvidence = psGuardEvidence proofStep,
      cpscFactDerivations = psFactDerivations proofStep,
      cpscContextEvidence = psContextEvidence proofStep,
      cpscSupportEvidence = psSupportEvidence proofStep,
      cpscAnnotation = psAnnotation proofStep,
      cpscTimestamp = psTimestamp proofStep
    }

chimeraProofContentDigest ::
  [ProofStep TissueF ScaleContext ()] ->
  Word64
chimeraProofContentDigest =
  foldl' hashCharacter fnvOffsetBasis
    . foldMap renderFullProofStep
  where
    hashCharacter :: Word64 -> Char -> Word64
    hashCharacter currentHash character =
      (currentHash `xor` fromIntegral (ord character)) * fnvPrime

    renderFullProofStep :: ProofStep TissueF ScaleContext () -> String
    renderFullProofStep proofStep =
      show proofStep
        <> "|lhs="
        <> maybe "<none>" renderTissueFix (psLhsWitness proofStep)
        <> "|rhs="
        <> maybe "<none>" renderTissueFix (psRhsWitness proofStep)

    fnvOffsetBasis :: Word64
    fnvOffsetBasis = 14695981039346656037

    fnvPrime :: Word64
    fnvPrime = 1099511628211

chimeraAtlasPlan ::
  PlanSpec (ChimeraAtlasU owner) (SatGraph (ChimeraAtlasU owner)) RewriteRuleId
chimeraAtlasPlan =
  withSchedulerConfig
    deterministicSchedulerConfig
    (planSpec chimeraBudget GenericJoinMatching emptyRewriteRuntimeCapabilities)

chimeraReferencePlan ::
  PlanSpec
    (EGraphU UnitContextSiteOwner () TissueF TissueCount TrivialContext)
    (SatGraph (EGraphU UnitContextSiteOwner () TissueF TissueCount TrivialContext))
    RewriteRuleId
chimeraReferencePlan =
  withSchedulerConfig
    deterministicSchedulerConfig
    (planSpec chimeraBudget GenericJoinMatching emptyRewriteRuntimeCapabilities)

chimeraBudget :: SaturationBudget
chimeraBudget =
  SaturationBudget
    { sbMaxIterations = 12,
      sbMaxNodes = 50000
    }

assertExtracts :: ChimeraAtlasResult owner -> Fix TissueF -> ScaleContext -> Assertion
assertExtracts result expectedTerm contextValue =
  case
      extractAtlasAtContext
        baseTissueCost
        contextValue
        (carWitnessClass result)
        (sceContextGraph (pgGraph (srCarrier (carReport result))))
    of
      Left extractionError ->
        assertFailure
          ( "chimera extraction failed at "
              <> show contextValue
              <> ": "
              <> show extractionError
          )
      Right extraction ->
        assertBool
          ("unexpected chimera extraction at " <> show contextValue)
          (OrderedFix (atlasExtractionTerm extraction) == OrderedFix expectedTerm)

expectAtlas :: Show errorValue => Either errorValue value -> IO value
expectAtlas outcome =
  case outcome of
    Left obstruction -> assertFailure (show obstruction)
    Right value -> pure value
