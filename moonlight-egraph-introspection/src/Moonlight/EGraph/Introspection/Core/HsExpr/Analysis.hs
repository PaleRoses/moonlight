{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Introspection.Core.HsExpr.Analysis
  ( HsExprAnalysis (..),
    HsExprAnalysisError (..),
    HsExprPipelineFailure (..),
    HsExprBindingExtractionMetrics (..),
    HsExprPipelineMetrics (..),
    collectTags,
    analyzeHaskellSource,
    measureConvertedModuleInsertionMetrics,
    measureHaskellSourceInsertionMetrics,
    measureConvertedModulePipeline,
    measureHaskellSourcePipeline,
  )
where

import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import Moonlight.Algebra (JoinSemilattice (..))
import Moonlight.Core (HasConstructorTag (constructorTag), Pattern (..))
import Moonlight.EGraph.Introspection.Core.HsExpr.Spans
  ( HsExprInsertionMetrics,
    HsExprInsertionError,
    HsExprContextLatticeError,
    HsExprSiteRuleError,
    HsExprSupportRuleMetrics,
    convertedModuleContextLattice,
    hsExprDiagnosticSpans,
    hsExprRuntimeCapabilitiesForContextGraph,
    hsExprCapabilityGenerationForContextGraph,
    hsExprSupportRuleMetrics,
    identityInsertionSeeding,
    insertConvertedModuleWithMetrics,
  )
import Moonlight.EGraph.Introspection.Core.HsExpr.FreeScope
  ( FreeScopeWitness,
    HasFreeScopeWitness (..),
    hsExprFreeScopeWitness,
  )
import Moonlight.EGraph.Introspection.Core.HsExpr.Laws (hsExprSiteLawFamily)
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec, semilatticeAnalysis)
import Moonlight.EGraph.Pure.Context (ContextEGraph, emptyContextEGraph)
import Moonlight.EGraph.Pure.Context.Core (cegSite)
import Moonlight.EGraph.Pure.Extraction (ExtractionFixpointBudget (..), depthCost)
import Moonlight.EGraph.Pure.Context.Proof (ProofGraph (pgGraph))
import Moonlight.EGraph.Pure.Saturation.Extraction
  ( ContextualExtractionMetrics,
    ContextualExtractionObstruction,
    contextualExtractWithMetricsBounded,
  )
import Moonlight.EGraph.Pure.Saturation.Matching (MatchingStrategy (GenericJoinMatching))
import Moonlight.EGraph.Pure.Saturation.Guidance (egraphSupportGuidance)
import Moonlight.EGraph.Pure.Saturation.Substrate (EGraphU)
import Moonlight.EGraph.Saturation.Context.State
  ( emptySaturatingProofEGraph,
    sceContextGraph,
  )
import Moonlight.Saturation.Context.Driver
  ( carrierGoal,
    contextExecutionSpec,
    crrResult,
  )
import Moonlight.Saturation.Context.Error (SaturationError (..))
import Moonlight.Saturation.Context.Program.Spec
  ( RewriteContextSnapshot (..),
    deterministicSchedulerConfig,
    planSpec,
    withGuidance,
    withRewriteContext,
    withSchedulerConfig,
  )
import Moonlight.Saturation.Context.Runtime.Report (srCarrier)
import Moonlight.Saturation.Support.Core
  ( SupportSaturationMetrics,
    SupportScheduleGroup,
    supportSaturationMetricsFromReport,
  )
import Moonlight.Saturation.Support.Algebra (supportRuntimePolicy)
import Moonlight.Saturation.Support.Compile (compileSupportProgram)
import Moonlight.Saturation.Support.Driver
  ( prepareSupportPlan,
    runSupportPlan,
  )
import Moonlight.EGraph.Pure.Types (ClassId, emptyEGraph)
import Moonlight.Pale.Ghc.Expr
  ( ConvertedModule (..),
    ConvertedModuleMetrics,
    ConvertObstruction,
    HsExprF,
    HsExprTag,
    ScopeCtx (ActualScope),
    ScopeIndex,
    ScopedExpr (..),
    TopLevelBinding (..),
    convertHaskellSource,
    convertedModuleMetrics,
    matchesHsExprPattern,
  )
import Moonlight.Rewrite.ProofContext (ProofAnnotationBuilder, defaultProofAnnotationBuilder)
import Moonlight.Rewrite.System (LawBook (..), lawRule)
import Moonlight.EGraph.Introspection.Core.Rewrite (PatternRewriteError, RewriteMorphism, rewriteMorphismName)
import Moonlight.Rewrite.Algebra (prLeft, prRight)
import Moonlight.Saturation.Core (SaturationBudget)
import Moonlight.Control.Schedule (identitySchedulerRefinement)
import Moonlight.Sheaf.Context.Site (PreparedContextSupportError)
import Moonlight.Sheaf.Twist.SupportedRuleSpec qualified as SheafTwist

type HsExprAnalysis :: Type
data HsExprAnalysis = HsExprAnalysis
  { heaSourceTerms :: [Pattern HsExprF],
    heaTagProfile :: Set.Set HsExprTag,
    heaMatchSiteCounts :: Map String Int
  }
  deriving stock (Eq, Show)

type HsExprAnalysisError :: Type
data HsExprAnalysisError
  = HsExprConvertFailure !ConvertObstruction
  | HsExprPipelineError !HsExprPipelineFailure
  deriving stock (Show)

type HsExprPipelineFailure :: Type
data HsExprPipelineFailure
  = HsExprContextLatticeFailure !HsExprContextLatticeError
  | HsExprContextInsertionFailure !HsExprInsertionError
  | HsExprRewriteSpanFailure !(PatternRewriteError HsExprF)
  | HsExprSiteRuleFailure !HsExprSiteRuleError
  | HsExprSupportRuleBookFailure !(PreparedContextSupportError ScopeCtx)
  | HsExprSupportSaturationFailure !HsExprSupportSaturationError
  | HsExprContextualExtractionFailure !String !ScopeCtx !(ContextualExtractionObstruction ScopeCtx)
  | HsExprExtractionRootCountMismatch !Int !Int !Int
  deriving stock (Show)

type HsExprSupportSaturationError :: Type
type HsExprSupportSaturationError =
  SaturationError
    (EGraphU ScopeCtx HsExprF HsExprMetricAnalysis ScopeCtx)
    (SupportScheduleGroup (EGraphU ScopeCtx HsExprF HsExprMetricAnalysis ScopeCtx))

type HsExprBindingExtractionMetrics :: Type
data HsExprBindingExtractionMetrics = HsExprBindingExtractionMetrics
  { hbemBindingName :: !String,
    hbemExtractionMetrics :: !ContextualExtractionMetrics
  }
  deriving stock (Eq, Ord, Show)

type HsExprPipelineMetrics :: Type
data HsExprPipelineMetrics = HsExprPipelineMetrics
  { hpmConversionMetrics :: !ConvertedModuleMetrics,
    hpmSupportRuleMetrics :: !HsExprSupportRuleMetrics,
    hpmInsertionMetrics :: !HsExprInsertionMetrics,
    hpmSupportSaturationMetrics :: !(SupportSaturationMetrics (EGraphU ScopeCtx HsExprF HsExprMetricAnalysis ScopeCtx)),
    hpmExtractionMetrics :: ![HsExprBindingExtractionMetrics]
  }
  deriving stock (Eq, Show)

analyzeHaskellSource :: FilePath -> String -> Either HsExprAnalysisError HsExprAnalysis
analyzeHaskellSource sourcePath moduleContents =
  case convertHaskellSource sourcePath moduleContents of
    Left convertFailure ->
      Left (HsExprConvertFailure convertFailure)
    Right convertedModule ->
      let sourceTerms = fmap tlbTerm (cmBindings convertedModule)
       in do
            diagnosticSpans <-
              first
                (HsExprPipelineError . HsExprRewriteSpanFailure)
                (hsExprDiagnosticSpans convertedModule)
            Right
              HsExprAnalysis
                { heaSourceTerms = sourceTerms,
                  heaTagProfile = foldMap collectTags sourceTerms,
                  heaMatchSiteCounts = computeMatchSiteCounts sourceTerms diagnosticSpans
                }

measureHaskellSourcePipeline :: SaturationBudget -> FilePath -> String -> Either HsExprAnalysisError HsExprPipelineMetrics
measureHaskellSourcePipeline budgetValue sourcePath moduleContents =
  case convertHaskellSource sourcePath moduleContents of
    Left convertFailure ->
      Left (HsExprConvertFailure convertFailure)
    Right convertedModule ->
      first HsExprPipelineError (measureConvertedModulePipeline budgetValue convertedModule)

measureHaskellSourceInsertionMetrics :: FilePath -> String -> Either HsExprAnalysisError HsExprInsertionMetrics
measureHaskellSourceInsertionMetrics sourcePath moduleContents =
  case convertHaskellSource sourcePath moduleContents of
    Left convertFailure ->
      Left (HsExprConvertFailure convertFailure)
    Right convertedModule ->
      first HsExprPipelineError (measureConvertedModuleInsertionMetrics convertedModule)

measureConvertedModuleInsertionMetrics :: ConvertedModule -> Either HsExprPipelineFailure HsExprInsertionMetrics
measureConvertedModuleInsertionMetrics convertedModule =
  let graph0 = emptyEGraph (hsExprMetricAnalysisSpec (cmScopeIndex convertedModule))
   in do
      latticeValue <-
        first HsExprContextLatticeFailure (convertedModuleContextLattice convertedModule)
      let contextGraph0 = emptyContextEGraph latticeValue graph0
      (_, _, insertionMetrics, _) <-
        first HsExprContextInsertionFailure (insertConvertedModuleWithMetrics identityInsertionSeeding convertedModule contextGraph0)
      pure insertionMetrics

measureConvertedModulePipeline :: SaturationBudget -> ConvertedModule -> Either HsExprPipelineFailure HsExprPipelineMetrics
measureConvertedModulePipeline budgetValue convertedModule = do
  let graph0 = emptyEGraph (hsExprMetricAnalysisSpec (cmScopeIndex convertedModule))
  latticeValue <-
    first HsExprContextLatticeFailure (convertedModuleContextLattice convertedModule)
  let contextGraph0 = emptyContextEGraph latticeValue graph0
  (seedClasses, _, insertionMetrics, contextGraph1) <-
    first HsExprContextInsertionFailure (insertConvertedModuleWithMetrics identityInsertionSeeding convertedModule contextGraph0)
  lawBook <-
    first HsExprSiteRuleFailure (hsExprSiteLawFamily convertedModule)
  supportRuleBook <-
    first HsExprSupportRuleBookFailure
      ( SheafTwist.supportedRuleBook
          (cegSite contextGraph1)
          (fmap lawRule (lawBookEntries lawBook))
      )
  let
      proofGraph0 = emptySaturatingProofEGraph contextGraph1
      proofBuilder :: ProofAnnotationBuilder ScopeCtx ()
      proofBuilder = defaultProofAnnotationBuilder
      supportPlanSpec =
        withGuidance
          ( egraphSupportGuidance
              (hsExprRuntimeCapabilitiesForContextGraph contextGraph1)
              Nothing
          )
          ( withRewriteContext
              ( \proofGraph ->
                  RewriteContextSnapshot
                    { rcsCapabilityGeneration =
                        hsExprCapabilityGenerationForContextGraph (sceContextGraph (pgGraph proofGraph)),
                      rcsRewriteContext = hsExprRuntimeCapabilitiesForContextGraph (sceContextGraph (pgGraph proofGraph))
                    }
              )
              ( withSchedulerConfig
                  deterministicSchedulerConfig
                  ( planSpec
                      budgetValue
                      GenericJoinMatching
                      (hsExprRuntimeCapabilitiesForContextGraph contextGraph1)
                  )
              )
          )
  compiledProgram <-
    first
      (HsExprSupportSaturationFailure . SaturationCompileFailure)
      ( compileSupportProgram
          @(EGraphU ScopeCtx HsExprF HsExprMetricAnalysis ScopeCtx)
          (cegSite contextGraph1)
          supportRuleBook
          mempty
      )
  supportPlan <-
    first HsExprSupportSaturationFailure
      (prepareSupportPlan supportPlanSpec compiledProgram)
  supportRun <-
    first HsExprSupportSaturationFailure
      ( runSupportPlan
          ( contextExecutionSpec
              (supportRuntimePolicy identitySchedulerRefinement proofBuilder)
              (carrierGoal mempty)
          )
          supportPlan
          proofGraph0
      )
  let supportReport = crrResult supportRun
  let supportMetrics =
        supportSaturationMetricsFromReport
          pgGraph
          proofGraph0
          supportReport
      saturatedContextGraph = sceContextGraph (pgGraph (srCarrier supportReport))
      bindingContexts =
        fmap
          (ActualScope . seOccScope . tlbScopedTerm)
          (cmBindings convertedModule)
      bindingNames = fmap renderBindingName (cmBindings convertedModule)
  extractionInputs <-
    bindingExtractionInputs bindingNames bindingContexts seedClasses
  extractionMetrics <-
    traverse
      (bindingExtractionMetrics saturatedContextGraph)
      extractionInputs
  pure
    HsExprPipelineMetrics
      { hpmConversionMetrics = convertedModuleMetrics convertedModule,
        hpmSupportRuleMetrics = hsExprSupportRuleMetrics convertedModule,
        hpmInsertionMetrics = insertionMetrics,
        hpmSupportSaturationMetrics = supportMetrics,
        hpmExtractionMetrics = extractionMetrics
      }

bindingExtractionInputs ::
  [String] ->
  [ScopeCtx] ->
  [ClassId] ->
  Either HsExprPipelineFailure [(String, ScopeCtx, ClassId)]
bindingExtractionInputs bindingNames bindingContexts seedClasses =
  let extractionInputs =
        zip3 bindingNames bindingContexts seedClasses
      bindingNameCount =
        length bindingNames
      bindingContextCount =
        length bindingContexts
      seedClassCount =
        length seedClasses
      inputCount =
        length extractionInputs
   in if bindingNameCount == inputCount
        && bindingContextCount == inputCount
        && seedClassCount == inputCount
        then Right extractionInputs
        else Left (HsExprExtractionRootCountMismatch bindingNameCount bindingContextCount seedClassCount)

bindingExtractionMetrics ::
  ContextEGraph HsExprF HsExprMetricAnalysis ScopeCtx ->
  (String, ScopeCtx, ClassId) ->
  Either HsExprPipelineFailure HsExprBindingExtractionMetrics
bindingExtractionMetrics contextGraph (bindingName, scopeCtx, classId) = do
  (contextualMetrics, _) <-
    first
      (HsExprContextualExtractionFailure bindingName scopeCtx)
      (contextualExtractWithMetricsBounded hsExprMetricExtractionBudget scopeCtx mempty depthCost classId contextGraph)
  pure
    HsExprBindingExtractionMetrics
      { hbemBindingName = bindingName,
        hbemExtractionMetrics = contextualMetrics
      }

collectTags :: Pattern HsExprF -> Set.Set HsExprTag
collectTags = \case
  PatternVar {} ->
    Set.empty
  PatternNode nodeValue ->
    Set.insert (constructorTag nodeValue) (foldMap collectTags nodeValue)

computeMatchSiteCounts :: [Pattern HsExprF] -> [RewriteMorphism HsExprF] -> Map String Int
computeMatchSiteCounts sourceTerms =
  Map.fromList
    . fmap
      (\spanValue -> (rewriteMorphismName spanValue, countSpanMatchSites sourceTerms spanValue))

countSpanMatchSites :: [Pattern HsExprF] -> RewriteMorphism HsExprF -> Int
countSpanMatchSites sourceTerms spanValue =
  sourceTerms
    & foldMap hsExprSubterms
    & filter (matchesSpanDiagnosticSite spanValue)
    & length

matchesSpanDiagnosticSite :: RewriteMorphism HsExprF -> Pattern HsExprF -> Bool
matchesSpanDiagnosticSite spanValue termValue =
  diagnosticPatterns spanValue
    & any (`matchesHsExprPattern` termValue)

diagnosticPatterns :: RewriteMorphism HsExprF -> [Pattern HsExprF]
diagnosticPatterns spanValue =
  [prLeft spanValue, prRight spanValue]
    & filter (not . isDegenerateDiagnosticPattern)

isDegenerateDiagnosticPattern :: Pattern HsExprF -> Bool
isDegenerateDiagnosticPattern = \case
  PatternVar {} ->
    True
  PatternNode {} ->
    False

hsExprSubterms :: Pattern HsExprF -> [Pattern HsExprF]
hsExprSubterms patternValue =
  case patternValue of
    PatternVar {} ->
      [patternValue]
    PatternNode nodeValue ->
      patternValue : foldMap hsExprSubterms nodeValue

renderBindingName :: TopLevelBinding -> String
renderBindingName bindingValue =
  case tlbNames bindingValue of
    [] ->
      "_unnamed"
    bindingName : _ ->
      occNameString (rdrNameOcc bindingName)

type HsExprMetricNodeCount :: Type
newtype HsExprMetricNodeCount = HsExprMetricNodeCount Int
  deriving stock (Eq, Ord, Show)

instance JoinSemilattice HsExprMetricNodeCount where
  join (HsExprMetricNodeCount leftValue) (HsExprMetricNodeCount rightValue) =
    HsExprMetricNodeCount (max leftValue rightValue)

type HsExprMetricAnalysis :: Type
data HsExprMetricAnalysis = HsExprMetricAnalysis
  { hmaNodeCount :: !HsExprMetricNodeCount,
    hmaFreeScope :: !FreeScopeWitness
  }
  deriving stock (Eq, Ord, Show)

instance JoinSemilattice HsExprMetricAnalysis where
  join left right =
    HsExprMetricAnalysis
      { hmaNodeCount = join (hmaNodeCount left) (hmaNodeCount right),
        hmaFreeScope = join (hmaFreeScope left) (hmaFreeScope right)
      }

instance HasFreeScopeWitness HsExprMetricAnalysis where
  freeScopeWitness = hmaFreeScope

hsExprMetricAnalysisSpec :: ScopeIndex -> AnalysisSpec HsExprF HsExprMetricAnalysis
hsExprMetricAnalysisSpec scopeIndex =
  semilatticeAnalysis
    ( \nodeValue ->
        HsExprMetricAnalysis
          { hmaNodeCount =
              HsExprMetricNodeCount
                (1 + foldr (\(HsExprMetricNodeCount childCost) acc -> childCost + acc) 0 (fmap hmaNodeCount nodeValue)),
            hmaFreeScope =
              hsExprFreeScopeWitness scopeIndex (fmap hmaFreeScope nodeValue)
          }
    )

-- | The budget caps worklist finalizations (one per e-class), so it must
-- dominate any realistic class count; the worklist engine terminates
-- structurally and this is a resource bound only.
hsExprMetricExtractionBudget :: ExtractionFixpointBudget
hsExprMetricExtractionBudget =
  ExtractionFixpointBudget 4194304
