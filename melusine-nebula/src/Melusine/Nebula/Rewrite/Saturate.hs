{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.Rewrite.Saturate
  ( NebulaSaturationGoal,
    NebulaMatchingStrategy,
    MatchingStrategy (GenericJoinMatching, GenericJoinPerContextMatching),
    SaturationOptions (..),
    defaultSaturationOptions,
    contextEqualityGoal,
    SaturatedModule,
    smPlan,
    smRuntimeState,
    smLifecycleCounts,
    smMutationTrace,
    smTraceImpact,
    smTermination,
    smIterations,
    smMatchesApplied,
    smInitialNodeCount,
    smFinalNodeCount,
    smInitialClassCount,
    smFinalClassCount,
    smScheduledTotal,
    smRuleFires,
    SaturationLifecycleCounts (..),
    SaturationTraceImpact (..),
    RuleFire (..),
    smContextGraph,
    smProofSteps,
    saturateModule,
    saturateContextGraph,
    resumeSaturatedModule,
    saturateEditedContextGraph,
  )
where

import Data.Bifunctor (first)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Melusine.Nebula.Proof.Certificate
  ( NebulaProvenance,
    nebulaProofBuilder,
  )
import Melusine.Nebula.Core
  ( NebulaConfig (..),
    NebulaError (..),
    NebulaAnalysis,
    NebulaUniverse,
  )
import Melusine.Nebula.Rewrite.Corpus
  ( RuleCorpus,
    rcCompiledProgram,
    rcLawTable,
  )
import Melusine.Nebula.Source.Ingest (IngestedModule (..))
import Moonlight.Control.Schedule (identitySchedulerRefinement)
import Moonlight.Core (RewriteRuleId (..))
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( HsExprF,
    ScopeCtx,
    hsExprRuntimeCapabilitiesForContextGraph,
    hsExprCapabilityGenerationForContextGraph,
  )
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    ContextMutationTrace (..),
    contextCachedObjectsForExecution,
  )
import Moonlight.EGraph.Pure.Context.Core
  ( cegClassSupportIndex,
    cegSite,
    contextPreparedObjects,
  )
import Moonlight.EGraph.Pure.Context.Proof (ProofGraph (pgGraph))
import Moonlight.EGraph.Pure.Context.Proof qualified as ContextProof
import Moonlight.EGraph.Pure.Saturation.Guidance (egraphSupportGuidance)
import Moonlight.EGraph.Pure.Saturation.Matching
  ( MatchingStrategy (GenericJoinMatching, GenericJoinPerContextMatching),
  )
import Moonlight.EGraph.Pure.Saturation.Substrate (eGraphSaturationChangeTrace)
import Moonlight.EGraph.Pure.Types (ClassId)
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingProofEGraph,
    emptySaturatingContextEGraph,
    emptySaturatingProofEGraph,
    sceContextGraph,
  )
import Moonlight.Rewrite.ProofContext (ProofStep)
import Moonlight.Sheaf.Context.Algebra (contextEquivalentAt)
import Moonlight.Sheaf.Context.Site
  ( classSupportExplicitCarrierForKey,
    supportCarrierReachableObjects,
  )
import Moonlight.Saturation.Context.Program.Spec
  ( PlanSpec,
    RewriteContextSnapshot (..),
    deterministicSchedulerConfig,
    planSpec,
    withGuidance,
    withRewriteContext,
    withSchedulerConfig,
  )
import Moonlight.Saturation.Context.Driver
  ( ContextExecutionSpec,
    ContextRunResult (..),
    ResumableRuntimeState,
    carrierGoal,
    contextExecutionSpec,
    resumableRuntimeState,
  )
import Moonlight.Saturation.Context.Program.Plan (Plan)
import Moonlight.Saturation.Context.Runtime.Report
  ( ReportSummary (..),
    reportSummary,
    srTracePayload,
  )
import Moonlight.Saturation.Context.Runtime.State (rsCarrier)
import Moonlight.Saturation.Core
  ( SaturationBudget,
    SaturationTermination,
    TerminationGoal,
    goal,
  )
import Moonlight.Saturation.Support.Core
  ( SaturationRunMetrics (..),
    SupportSaturationReportFor,
    SupportScheduleGroup,
    SupportSaturationMetrics,
    supportSaturationMetricsFromReport,
  )
import Moonlight.Saturation.Support.Algebra (supportRuntimePolicy)
import Moonlight.Saturation.Support.Driver
  ( prepareSupportPlan,
    resumeSupportPlan,
    runSupportPlan,
  )
import Moonlight.Pale.Diagnostic.Section.Rewrite (RuleTrace (..))
import Moonlight.Pale.Diagnostic.Section.Saturation (SaturationIterationTrace (..), SaturationTrace (..))

type NebulaSaturationGoal :: Type
type NebulaSaturationGoal =
  TerminationGoal (SaturatingProofEGraph ScopeCtx HsExprF NebulaAnalysis ScopeCtx NebulaProvenance)

type NebulaMatchingStrategy :: Type
type NebulaMatchingStrategy =
  MatchingStrategy ScopeCtx ScopeCtx HsExprF NebulaAnalysis

type NebulaProofCarrier :: Type
type NebulaProofCarrier =
  SaturatingProofEGraph ScopeCtx HsExprF NebulaAnalysis ScopeCtx NebulaProvenance

type NebulaSupportPlan :: Type
type NebulaSupportPlan =
  Plan NebulaUniverse NebulaProofCarrier (SupportScheduleGroup NebulaUniverse)

type NebulaResumableRuntimeState :: Type
type NebulaResumableRuntimeState =
  ResumableRuntimeState NebulaUniverse NebulaProofCarrier (SupportScheduleGroup NebulaUniverse)

type NebulaSupportReport :: Type
type NebulaSupportReport =
  SupportSaturationReportFor NebulaUniverse NebulaProofCarrier

type NebulaRunResult :: Type
type NebulaRunResult =
  ContextRunResult
    NebulaUniverse
    NebulaProofCarrier
    (SupportScheduleGroup NebulaUniverse)
    NebulaSupportReport

type SaturationOptions :: Type
data SaturationOptions = SaturationOptions
  { soGoal :: !NebulaSaturationGoal,
    soMatchingStrategy :: !NebulaMatchingStrategy
  }

defaultSaturationOptions :: SaturationOptions
defaultSaturationOptions =
  SaturationOptions
    { soGoal = mempty,
      soMatchingStrategy = GenericJoinMatching
    }

contextEqualityGoal :: ScopeCtx -> ClassId -> ClassId -> NebulaSaturationGoal
contextEqualityGoal contextValue leftClassId rightClassId =
  goal
    ( \proofGraph ->
        either
          (const False)
          id
          ( contextEquivalentAt
              contextValue
              leftClassId
              rightClassId
              (sceContextGraph (pgGraph proofGraph))
          )
    )

type RuleFire :: Type
data RuleFire = RuleFire
  { rfRuleId :: !RewriteRuleId,
    rfMatchedTotal :: !Int,
    rfScheduledTotal :: !Int
  }
  deriving stock (Eq, Ord, Show)

type SaturatedModule :: Type
data SaturatedModule = SaturatedModule
  { saturatedModulePlan :: !NebulaSupportPlan,
    saturatedModuleRuntimeState :: !NebulaResumableRuntimeState,
    saturatedModuleLifecycleCounts :: !SaturationLifecycleCounts,
    saturatedModuleMutationTrace :: !(ContextMutationTrace ScopeCtx HsExprF),
    saturatedModuleTraceImpact :: !SaturationTraceImpact,
    saturatedModuleTermination :: !SaturationTermination,
    saturatedModuleIterations :: !Int,
    saturatedModuleMatchesApplied :: !Int,
    saturatedModuleInitialNodeCount :: !Int,
    saturatedModuleFinalNodeCount :: !Int,
    saturatedModuleInitialClassCount :: !Int,
    saturatedModuleFinalClassCount :: !Int,
    saturatedModuleScheduledTotal :: !Int,
    saturatedModuleRuleFires :: ![RuleFire]
  }

type SaturationLifecycleCounts :: Type
data SaturationLifecycleCounts = SaturationLifecycleCounts
  { slcPlanPreparations :: !Int,
    slcFreshRuns :: !Int,
    slcResumptions :: !Int
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup SaturationLifecycleCounts where
  left <> right =
    SaturationLifecycleCounts
      { slcPlanPreparations = slcPlanPreparations left + slcPlanPreparations right,
        slcFreshRuns = slcFreshRuns left + slcFreshRuns right,
        slcResumptions = slcResumptions left + slcResumptions right
      }

instance Monoid SaturationLifecycleCounts where
  mempty =
    SaturationLifecycleCounts
      { slcPlanPreparations = 0,
        slcFreshRuns = 0,
        slcResumptions = 0
      }

smPlan :: SaturatedModule -> NebulaSupportPlan
smPlan =
  saturatedModulePlan

smRuntimeState :: SaturatedModule -> NebulaResumableRuntimeState
smRuntimeState =
  saturatedModuleRuntimeState

smLifecycleCounts :: SaturatedModule -> SaturationLifecycleCounts
smLifecycleCounts =
  saturatedModuleLifecycleCounts

smMutationTrace :: SaturatedModule -> ContextMutationTrace ScopeCtx HsExprF
smMutationTrace =
  saturatedModuleMutationTrace

smTraceImpact :: SaturatedModule -> SaturationTraceImpact
smTraceImpact =
  saturatedModuleTraceImpact

smTermination :: SaturatedModule -> SaturationTermination
smTermination =
  saturatedModuleTermination

smIterations :: SaturatedModule -> Int
smIterations =
  saturatedModuleIterations

smMatchesApplied :: SaturatedModule -> Int
smMatchesApplied =
  saturatedModuleMatchesApplied

smInitialNodeCount :: SaturatedModule -> Int
smInitialNodeCount =
  saturatedModuleInitialNodeCount

smFinalNodeCount :: SaturatedModule -> Int
smFinalNodeCount =
  saturatedModuleFinalNodeCount

smInitialClassCount :: SaturatedModule -> Int
smInitialClassCount =
  saturatedModuleInitialClassCount

smFinalClassCount :: SaturatedModule -> Int
smFinalClassCount =
  saturatedModuleFinalClassCount

smScheduledTotal :: SaturatedModule -> Int
smScheduledTotal =
  saturatedModuleScheduledTotal

smRuleFires :: SaturatedModule -> [RuleFire]
smRuleFires =
  saturatedModuleRuleFires

smContextGraph :: SaturatedModule -> ContextEGraph HsExprF NebulaAnalysis ScopeCtx
smContextGraph =
  sceContextGraph . pgGraph . saturatedModuleProofCarrier

smProofSteps :: SaturatedModule -> [ProofStep HsExprF ScopeCtx NebulaProvenance]
smProofSteps =
  ContextProof.serializeProofLog . saturatedModuleProofCarrier

type SaturationTraceImpact :: Type
data SaturationTraceImpact = SaturationTraceImpact
  { stiTouchedClassKeys :: !Int,
    stiTouchedExplicitClassKeys :: !Int,
    stiTouchedDefaultClassKeys :: !Int,
    stiDirtyContexts :: !Int,
    stiExplicitDirtyContexts :: !Int,
    stiCachedContexts :: !Int
  }
  deriving stock (Eq, Ord, Show)

nebulaSupportPlanSpec ::
  SaturationOptions ->
  SaturationBudget ->
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  PlanSpec NebulaUniverse NebulaProofCarrier (SupportScheduleGroup NebulaUniverse)
nebulaSupportPlanSpec options budgetValue contextGraph =
  withGuidance
    ( egraphSupportGuidance
        (hsExprRuntimeCapabilitiesForContextGraph contextGraph)
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
                (soMatchingStrategy options)
                (hsExprRuntimeCapabilitiesForContextGraph contextGraph)
            )
        )
    )

nebulaSupportExecutionSpec ::
  NebulaSaturationGoal ->
  RuleCorpus ->
  ContextExecutionSpec
    NebulaUniverse
    NebulaProofCarrier
    (SupportScheduleGroup NebulaUniverse)
    NebulaSupportReport
nebulaSupportExecutionSpec saturationGoal corpus =
  contextExecutionSpec
    (supportRuntimePolicy identitySchedulerRefinement (nebulaProofBuilder (rcLawTable corpus)))
    (carrierGoal saturationGoal)

saturateModule :: SaturationOptions -> NebulaConfig -> IngestedModule -> RuleCorpus -> Either NebulaError SaturatedModule
saturateModule options config ingested corpus =
  saturateContextGraph options config (imContextGraph ingested) corpus

saturateContextGraph ::
  SaturationOptions ->
  NebulaConfig ->
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  RuleCorpus ->
  Either NebulaError SaturatedModule
saturateContextGraph options config contextGraph1 corpus = do
  let proofGraph0 = emptySaturatingProofEGraph contextGraph1
  supportPlan <-
    first (NebulaSaturationError . show) $
      prepareSupportPlan
        (nebulaSupportPlanSpec options (ncSaturationBudget config) contextGraph1)
        (rcCompiledProgram corpus)
  runResult <-
    first (NebulaSaturationError . show) $
      runSupportPlan
        (nebulaSupportExecutionSpec (soGoal options) corpus)
        supportPlan
        proofGraph0
  pure
    ( saturatedModuleFromRun
        supportPlan
        initialSaturationLifecycle
        contextGraph1
        proofGraph0
        runResult
    )

resumeSaturatedModule ::
  NebulaSaturationGoal ->
  RuleCorpus ->
  SaturatedModule ->
  Either NebulaError SaturatedModule
resumeSaturatedModule saturationGoal corpus saturated = do
  let initialGraph = smContextGraph saturated
      initialCarrier = saturatedModuleProofCarrier saturated
  runResult <-
    first (NebulaSaturationError . show) $
      resumeSupportPlan
        (nebulaSupportExecutionSpec saturationGoal corpus)
        (smPlan saturated)
        (smRuntimeState saturated)
  pure
    ( saturatedModuleFromRun
        (smPlan saturated)
        (smLifecycleCounts saturated <> resumedSaturationLifecycle)
        initialGraph
        initialCarrier
        runResult
    )

saturateEditedContextGraph ::
  NebulaSaturationGoal ->
  RuleCorpus ->
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  SaturatedModule ->
  Either NebulaError SaturatedModule
saturateEditedContextGraph saturationGoal corpus editedContextGraph saturated = do
  let editedCarrier =
        (saturatedModuleProofCarrier saturated)
          { pgGraph = emptySaturatingContextEGraph editedContextGraph
          }
  runResult <-
    first (NebulaSaturationError . show) $
      runSupportPlan
        (nebulaSupportExecutionSpec saturationGoal corpus)
        (smPlan saturated)
        editedCarrier
  pure
    ( saturatedModuleFromRun
        (smPlan saturated)
        (smLifecycleCounts saturated <> freshSaturationLifecycle)
        editedContextGraph
        editedCarrier
        runResult
    )

saturatedModuleProofCarrier :: SaturatedModule -> NebulaProofCarrier
saturatedModuleProofCarrier =
  rsCarrier . resumableRuntimeState . smRuntimeState

saturatedModuleFromRun ::
  NebulaSupportPlan ->
  SaturationLifecycleCounts ->
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  NebulaProofCarrier ->
  NebulaRunResult ->
  SaturatedModule
saturatedModuleFromRun supportPlan lifecycleCounts initialContextGraph initialCarrier runResult =
  let supportReport = crrResult runResult
      finalCarrier = rsCarrier (resumableRuntimeState (crrState runResult))
      finalContextGraph = sceContextGraph (pgGraph finalCarrier)
      runMetrics :: SupportSaturationMetrics NebulaUniverse
      runMetrics =
        supportSaturationMetricsFromReport
          pgGraph
          initialCarrier
          supportReport
      summary = reportSummary supportReport
      mutationTrace =
        eGraphSaturationChangeTrace initialContextGraph finalContextGraph (srTracePayload supportReport)
   in SaturatedModule
        { saturatedModulePlan = supportPlan,
          saturatedModuleRuntimeState = crrState runResult,
          saturatedModuleLifecycleCounts = lifecycleCounts,
          saturatedModuleMutationTrace = mutationTrace,
          saturatedModuleTraceImpact = saturationTraceImpact finalContextGraph mutationTrace,
          saturatedModuleTermination = rsrResult summary,
          saturatedModuleIterations = srmIterations runMetrics,
          saturatedModuleMatchesApplied = srmMatchesApplied runMetrics,
          saturatedModuleInitialNodeCount = srmInitialNodeCount runMetrics,
          saturatedModuleFinalNodeCount = srmFinalNodeCount runMetrics,
          saturatedModuleInitialClassCount = srmInitialClassCount runMetrics,
          saturatedModuleFinalClassCount = srmFinalClassCount runMetrics,
          saturatedModuleScheduledTotal = sum (fmap sitScheduledCount (stIterations (srmTrace runMetrics))),
          saturatedModuleRuleFires = ruleFiresFromTrace (srmTrace runMetrics)
        }

initialSaturationLifecycle :: SaturationLifecycleCounts
initialSaturationLifecycle =
  SaturationLifecycleCounts
    { slcPlanPreparations = 1,
      slcFreshRuns = 1,
      slcResumptions = 0
    }

freshSaturationLifecycle :: SaturationLifecycleCounts
freshSaturationLifecycle =
  SaturationLifecycleCounts
    { slcPlanPreparations = 0,
      slcFreshRuns = 1,
      slcResumptions = 0
    }

resumedSaturationLifecycle :: SaturationLifecycleCounts
resumedSaturationLifecycle =
  SaturationLifecycleCounts
    { slcPlanPreparations = 0,
      slcFreshRuns = 0,
      slcResumptions = 1
    }

saturationTraceImpact ::
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  ContextMutationTrace ScopeCtx HsExprF ->
  SaturationTraceImpact
saturationTraceImpact contextGraph traceValue =
  SaturationTraceImpact
    { stiTouchedClassKeys = IntSet.size touchedKeys,
      stiTouchedExplicitClassKeys = IntSet.size explicitKeys,
      stiTouchedDefaultClassKeys = IntSet.size defaultKeys,
      stiDirtyContexts = Set.size (cmtDirtyContexts traceValue),
      stiExplicitDirtyContexts = Set.size explicitDirtyContexts,
      stiCachedContexts = length (contextCachedObjectsForExecution contextGraph)
    }
  where
    touchedKeys =
      cmtContextTouchedKeys traceValue

    supportIndex =
      cegClassSupportIndex contextGraph

    explicitKeys =
      IntSet.filter
        (\classKey -> maybe False (const True) (classSupportExplicitCarrierForKey supportIndex classKey))
        touchedKeys

    defaultKeys =
      IntSet.difference touchedKeys explicitKeys

    site =
      cegSite contextGraph

    allContexts =
      Set.fromList (contextPreparedObjects contextGraph)

    explicitDirtyContexts =
      IntSet.foldr
        (\classKey contexts ->
          maybe
            contexts
            ( \carrier ->
                either
                  (const contexts)
                  (`Set.union` contexts)
                  (supportCarrierReachableObjects site allContexts carrier)
            )
            (classSupportExplicitCarrierForKey supportIndex classKey)
        )
        Set.empty
        explicitKeys

ruleFiresFromTrace :: SaturationTrace RewriteRuleId -> [RuleFire]
ruleFiresFromTrace saturationTrace =
  let ruleRows =
        [ (rtRuleId ruleTrace, (rtMatchedCount ruleTrace, rtScheduledCount ruleTrace))
        | iterationTrace <- stIterations saturationTrace,
          ruleTrace <- sitRuleTraces iterationTrace
        ]
      ruleTotals =
        Map.fromListWith
          (\(newMatched, newScheduled) (matched, scheduled) -> (matched + newMatched, scheduled + newScheduled))
          ruleRows
   in [ RuleFire
          { rfRuleId = ruleId,
            rfMatchedTotal = matchedTotal,
            rfScheduledTotal = scheduledTotal
          }
      | (ruleId, (matchedTotal, scheduledTotal)) <- Map.toAscList ruleTotals
      ]
