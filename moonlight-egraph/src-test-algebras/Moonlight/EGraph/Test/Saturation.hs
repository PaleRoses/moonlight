{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Moonlight.EGraph.Test.Saturation
  ( EGraphSaturationConfig,
    SaturationConfig,
    PlainEGraphRuntimeState,
    EGraphStrategyPhase (..),
    data SaturationConfig,
    scBudget,
    scMatchingStrategy,
    scSchedulerConfig,
    EGraphSaturationReport,
    EGraphProofSaturationReport,
    ContextualRewriteSaturationError (..),
    EGraphContextIterationState,
    ProofSaturationSpec (..),
    ExtractionPlan (..),
    SaturationBudget (..),
    SaturationTermination (..),
    BackoffConfig, backoffConfig, bcMatchLimit, bcCooldownRounds,
    SchedulerConfig (..),
    PlanSpec,
    deterministicSchedulerConfig,
    backoffSchedulerConfig,
    traceAllSchedulerConfig,
    traceLastSchedulerConfig,
    withSchedulerConfig,
    SaturationReport,
    srResult,
    srCarrier,
    srIterations,
    srMatchesApplied,
    srGuideTrace,
    srTrace,
    saturationReportBaseGraph,
    ProofSaturationReport,
    psrProofGraph,
    GuideCheckpoint (..),
    GuidanceConfig (..),
    RewriteRuntimeCapabilities,
    emptyRewriteRuntimeCapabilities,
    saturate,
    saturateWith,
    prepareEGraphSupportPlan,
    runEGraphSupportPlan,
    runEGraphSupportPlanObserved,
    runContextualRewriteSaturation,
    runContextualRewriteProofSaturation,
    runSaturationSpec,
    runProofSaturationSpec,
    runEqualitySaturation,
    saturateWithSchedulerRefinement,
    saturateByStrategyWithSchedulerRefinement,
    saturateByStrategy,
    genericJoinSaturationConfig,
    genericJoinPerContextSaturationConfig,
    genericJoinSaturationSpec,
  )
where

import Data.Bifunctor (bimap, first)
import Moonlight.Core (ZipMatch)
import Data.Functor.Identity (runIdentity)
import Data.Kind (Type)
import Moonlight.Algebra (JoinSemilattice)
import Moonlight.Core (ConstructorTag, HasConstructorTag, Language)
import Moonlight.EGraph.Pure.Context (ContextEGraph)
import Moonlight.EGraph.Pure.Context (cegBase, cegSite)
import Moonlight.EGraph.Pure.Extraction
  ( AnalysisCostAlgebra,
    CostAlgebra,
    ExtractionResult,
    extract,
    extractWithAnalysis,
    stableExtractionSnapshotFromEGraph,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.EGraph.Pure.Saturation.Apply (EGraphRewriteApplicationError (EGraphRewriteApplicationFailed))
import Moonlight.EGraph.Pure.Context.Proof (ProofGraph (pgGraph))
import Moonlight.EGraph.Pure.Saturation.Matching
  ( MatchingStrategy (GenericJoinMatching, GenericJoinPerContextMatching),
  )
import Moonlight.EGraph.Pure.Saturation.Substrate (EGraphU)
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingProofEGraph,
    sceContextGraph,
  )
import Moonlight.EGraph.Pure.Types (EGraph, RewriteRuleId (..), canonicalizeClassId, eGraphTheorySpec)
import Moonlight.EGraph.Test.Context.MaterializedOracle
  ( materializedContextGraphAt,
  )
import Moonlight.Core (TheorySpec, expandPatternByTheory)
import Moonlight.Rewrite.System
  ( CompiledGuard,
    RewriteCondition,
  )
import Moonlight.Rewrite.System
  ( CompiledFactRule,
    FactRule,
  )
import Moonlight.Core
  ( Pattern
  )
import Moonlight.Rewrite.Runtime (RewriteApplicationError (RewriteClassIdAllocationFailed), RulePlan)
import Moonlight.Rewrite.System (RawRewriteRule (..))
import Moonlight.Rewrite.ProofContext (ProofAnnotationBuilder)
import Moonlight.Saturation.Context.Driver
  ( ContextRunResult,
    carrierGoal,
    contextExecutionSpec,
  )
import Moonlight.Saturation.Support.Driver qualified as SupportDriver
import Moonlight.Saturation.Context.Runtime.Engine
  ( RuntimeObservedResult,
  )
import Moonlight.Saturation.Support.Core
  ( SupportSaturationReportFor,
    SupportScheduleGroup,
  )
import Moonlight.Saturation.Support.Algebra (supportRuntimePolicy)
import Moonlight.Saturation.Support.Compile (compileSupportProgram)
import Moonlight.EGraph.Pure.Saturation.Guidance (egraphSupportGuidance)
import Moonlight.Saturation.Substrate
  ( RewriteSystem (defaultRewriteContext),
    SaturationGraph,
    SatBaseGraph,
    SatGraph,
    SatMatchStrategy,
    SatSupportedMatch,
    TrivialContext,
    baseGraphEquals,
    compileFactRules,
    compileRewriteRules,
    embedBaseGraph,
    graphBase,
    rewriteRuleId,
  )
import Moonlight.Rewrite.Runtime
  ( RewriteRuntimeCapabilities,
    emptyRewriteRuntimeCapabilities,
  )
import Data.Fix (Fix)
import Moonlight.Sheaf.Twist.SupportedRuleSpec qualified as SheafTwist
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSupportError,
    UnitContextSiteOwner,
  )
import Moonlight.Saturation.Context.Program.Compile qualified as ContextProgram
import Moonlight.Saturation.Context.Runtime.Carrier.Plain qualified as ContextReport
import Moonlight.Saturation.Context.Runtime.Carrier.Proof qualified as ContextProof
import Moonlight.Saturation.Context.Runtime.Carrier.Schedule
  ( candidateSpaceForSupportedMatches,
    compareSupportedMatches,
    scheduleRefinedRoundSupportedMatches,
    supportedMatchRuleKey,
  )
import Moonlight.Saturation.Context.Runtime.Engine qualified as ContextReport
import Moonlight.Saturation.Context.Runtime.Report
  ( SaturationReport,
  )
import Moonlight.Saturation.Context.Runtime.Report qualified as ContextReport
import Moonlight.Saturation.Context.Runtime.State qualified as ContextReport
import Moonlight.Saturation.Context.Error
  ( SaturationCompileError (..),
    SaturationError (..),
    SaturationRunError (SaturationRunApplyFailed),
    SaturationProgramSite (..),
  )
import Moonlight.Saturation.Context.Program.Spec
  ( PlanSpec,
    RewriteContextSnapshot (..),
    SaturationGuidanceView,
    backoffSchedulerConfig,
    deterministicSchedulerConfig,
    emptyPriorityProfile,
    planSpec,
    planSpecGuidance,
    planSpecMatchingStrategy,
    planSpecRewriteContextSnapshot,
    planSpecSaturationBudget,
    planSpecSchedulerConfig,
    staticRewriteContextSnapshot,
    traceAllSchedulerConfig,
    traceLastSchedulerConfig,
    withGuidance,
    withRewriteContext,
    withSchedulerConfig,
  )
import Moonlight.Saturation.Context.Program.Plan
  ( Plan,
    ProgramStage (..),
    baseProgram,
  )
import Moonlight.Saturation.Core
  ( SaturationBudget (..),
    SaturationTermination (..),
    TerminationGoal,
  )
import Moonlight.Control.Gate
  ( GuideCheckpoint (..),
    GuideRoundTrace,
    GuidanceConfig (..),
    Gate,
    gateName,
  )
import Moonlight.Control.Schedule
  ( BackoffConfig, backoffConfig, bcMatchLimit, bcCooldownRounds,
    SchedulerConfig (..),
    SchedulerRefinement,
    identitySchedulerRefinement,
  )
import Moonlight.Control.Machine
  ( Execution (..),
    runPhases,
    verdictForProgress,
  )
import Moonlight.Control.Program
  ( Program,
  )
import Moonlight.Control.Trace
  ( PhaseSummary (..),
    Report,
    Trace (PhaseTrace),
  )
import Moonlight.Rewrite.System (nullFactStore)
import Moonlight.Pale.Diagnostic.Section.Saturation (SaturationTrace)
type EGraphSaturationConfig :: Type -> Type -> (Type -> Type) -> Type -> Type -> Type
type EGraphSaturationConfig owner capability f a c =
  PlanSpec (EGraphU owner capability f a c) (SatGraph (EGraphU owner capability f a c)) RewriteRuleId

type SaturationConfig :: Type -> Type -> Type
type SaturationConfig u schedulerGroup =
  PlanSpec u (SatGraph u) schedulerGroup

pattern SaturationConfig ::
  forall u schedulerGroup.
  RewriteSystem u =>
  SaturationBudget ->
  SatMatchStrategy u ->
  SchedulerConfig schedulerGroup ->
  SaturationConfig u schedulerGroup
pattern SaturationConfig
  { scBudget,
    scMatchingStrategy,
    scSchedulerConfig
  } <- ((\spec ->
          ( planSpecSaturationBudget spec,
            planSpecMatchingStrategy spec,
            planSpecSchedulerConfig spec
          )
        ) -> (scBudget, scMatchingStrategy, scSchedulerConfig))
  where
    SaturationConfig budget matchingStrategy schedulerConfig =
      withSchedulerConfig
        schedulerConfig
        (planSpec budget matchingStrategy (defaultRewriteContext @u))

type EGraphSaturationReport :: Type -> (Type -> Type) -> Type -> Type
type EGraphSaturationReport capability f a =
  SaturationReport (EGraphU UnitContextSiteOwner capability f a TrivialContext)

type EGraphProofSaturationReport :: Type -> Type -> (Type -> Type) -> Type -> Type -> Type -> Type
type EGraphProofSaturationReport owner capability f a c p =
  ProofSaturationReport (EGraphU owner capability f a c) (SaturatingProofEGraph owner capability f a c p)

type ContextualRewriteSaturationError :: Type -> (Type -> Type) -> Type -> Type -> Type
data ContextualRewriteSaturationError capability f a c
  = ContextualRewriteRuleLookupFailed !(PreparedContextSupportError c)
  | ContextualRewriteMaterializationFailed !(PreparedContextSupportError c)
  | ContextualRewriteSaturationFailed !(SaturationError (EGraphU UnitContextSiteOwner capability f a TrivialContext) RewriteRuleId)

instance Show c => Show (ContextualRewriteSaturationError capability f a c) where
  show contextualError =
    case contextualError of
      ContextualRewriteRuleLookupFailed lookupError ->
        "ContextualRewriteRuleLookupFailed " <> show lookupError
      ContextualRewriteMaterializationFailed materializationError ->
        "ContextualRewriteMaterializationFailed " <> show materializationError
      ContextualRewriteSaturationFailed _saturationError ->
        "ContextualRewriteSaturationFailed <saturation>"

type ProofSaturationReport :: Type -> Type -> Type
type ProofSaturationReport u proofGraph =
  ContextReport.SaturationReportOf u proofGraph RewriteRuleId ()

type EGraphContextIterationState :: Type -> Type -> (Type -> Type) -> Type -> Type -> Type
type EGraphContextIterationState owner capability f a c =
  ContextReport.RuntimeState
    (EGraphU owner capability f a c)
    (SatGraph (EGraphU owner capability f a c))
    RewriteRuleId

type PlainEGraphRuntimeState :: Type -> (Type -> Type) -> Type -> Type
type PlainEGraphRuntimeState capability f a =
  EGraphContextIterationState UnitContextSiteOwner capability f a TrivialContext

type SaturatingProofEGraphRuntimeState :: Type -> Type -> (Type -> Type) -> Type -> Type -> Type -> Type
type SaturatingProofEGraphRuntimeState owner capability f a c p =
  ContextReport.RuntimeState
    (EGraphU owner capability f a c)
    (SaturatingProofEGraph owner capability f a c p)
    RewriteRuleId

type EGraphGuidance :: Type -> Type -> (Type -> Type) -> Type -> Type -> Type
type EGraphGuidance owner capability f a c =
  Gate
    (SaturationGuidanceView (EGraphU owner capability f a c))
    ()
    (SatSupportedMatch (EGraphU owner capability f a c))
    GuideRoundTrace
    RewriteRuleId

type EGraphStrategyPhase :: Type -> Type -> (Type -> Type) -> Type -> Type -> Type
data EGraphStrategyPhase owner capability f a c = EGraphStrategyPhase
  { spName :: !String,
    spConfig :: !(EGraphSaturationConfig owner capability f a c),
    spGuidance :: !(EGraphGuidance owner capability f a c)
  }

srIterations :: ContextReport.SaturationReportOf (EGraphU owner capability f a c) carrier schedulerGroup tracePayload -> Int
srIterations =
  ContextReport.reportIterationCount

srMatchesApplied :: ContextReport.SaturationReportOf (EGraphU owner capability f a c) carrier schedulerGroup tracePayload -> Int
srMatchesApplied =
  ContextReport.reportMatchesApplied

srGuideTrace :: ContextReport.SaturationReportOf (EGraphU owner capability f a c) carrier schedulerGroup tracePayload -> [GuideRoundTrace]
srGuideTrace =
  ContextReport.reportGuideTrace

srTrace :: ContextReport.SaturationReportOf (EGraphU owner capability f a c) carrier RewriteRuleId tracePayload -> SaturationTrace RewriteRuleId
srTrace =
  ContextReport.reportDiagnosticTrace id

srResult :: ContextReport.SaturationReportOf u carrier schedulerGroup tracePayload -> SaturationTermination
srResult =
  ContextReport.srResult

srCarrier :: ContextReport.SaturationReportOf u carrier schedulerGroup tracePayload -> carrier
srCarrier =
  ContextReport.srCarrier

saturationReportBaseGraph ::
  forall u schedulerGroup tracePayload.
  SaturationGraph u =>
  ContextReport.SaturationReportOf u (SatGraph u) schedulerGroup tracePayload ->
  SatBaseGraph u
saturationReportBaseGraph =
  graphBase @u . ContextReport.srCarrier

psrProofGraph :: ProofSaturationReport (EGraphU owner capability f a c) (SaturatingProofEGraph owner capability f a c p) -> SaturatingProofEGraph owner capability f a c p
psrProofGraph =
  ContextReport.srCarrier

saturationReportHasFacts ::
  ContextReport.SaturationReportOf (EGraphU owner capability f a c) carrier schedulerGroup tracePayload ->
  Bool
saturationReportHasFacts =
  not . all nullFactStore . ContextReport.reportContextFacts

runCompiledPlainPhaseWithRuntimeBuilder ::
  forall capability f a.
  (Ord capability, Show capability, HasConstructorTag f, Show (ConstructorTag f), Show (f ()), Ord a, JoinSemilattice a) =>
  SchedulerRefinement (PlainEGraphRuntimeState capability f a) RewriteRuleId ->
  (SatGraph (EGraphU UnitContextSiteOwner capability f a TrivialContext) -> RewriteContextSnapshot (EGraphU UnitContextSiteOwner capability f a TrivialContext)) ->
  EGraphGuidance UnitContextSiteOwner capability f a TrivialContext ->
  EGraphSaturationConfig UnitContextSiteOwner capability f a TrivialContext ->
  [RulePlan (CompiledGuard capability f) f] ->
  [CompiledFactRule capability f] ->
  EGraph f a ->
  Either
    (SaturationError (EGraphU UnitContextSiteOwner capability f a TrivialContext) RewriteRuleId)
    (SaturationReport (EGraphU UnitContextSiteOwner capability f a TrivialContext))
runCompiledPlainPhaseWithRuntimeBuilder schedulerRefinement rewriteContextSnapshotForCarrier phaseGuidance saturationConfig compiledRules compiledFactRules baseGraph = do
  let compiledRuntimeProgram =
        baseProgram
          @'CompiledProgramStage
          @(EGraphU UnitContextSiteOwner capability f a TrivialContext)
          (rewriteRuleId @(EGraphU UnitContextSiteOwner capability f a TrivialContext))
          compiledRules
          compiledFactRules
      phaseSpec =
        withRewriteContext
          rewriteContextSnapshotForCarrier
          (withGuidance phaseGuidance saturationConfig)
  planValue <-
    first SaturationCompileFailure $
      ContextProgram.planFromCompiledProgram @(EGraphU UnitContextSiteOwner capability f a TrivialContext)
        phaseSpec
        compiledRuntimeProgram
  let runtimePolicy =
        ContextReport.plainRuntimePolicyWith @(EGraphU UnitContextSiteOwner capability f a TrivialContext)
          ( \_state matches ->
              candidateSpaceForSupportedMatches
                @(EGraphU UnitContextSiteOwner capability f a TrivialContext)
                (supportedMatchRuleKey @(EGraphU UnitContextSiteOwner capability f a TrivialContext))
                (compareSupportedMatches @(EGraphU UnitContextSiteOwner capability f a TrivialContext))
                matches
          )
          ( scheduleRefinedRoundSupportedMatches
              @(EGraphU UnitContextSiteOwner capability f a TrivialContext)
              @(SatGraph (EGraphU UnitContextSiteOwner capability f a TrivialContext))
              @RewriteRuleId
              schedulerRefinement
          )
  bimap SaturationRunFailure snd $
    ContextReport.runPlanWithPolicy @(EGraphU UnitContextSiteOwner capability f a TrivialContext)
      runtimePolicy
      planValue
      (embedBaseGraph @(EGraphU UnitContextSiteOwner capability f a TrivialContext) @(SatGraph (EGraphU UnitContextSiteOwner capability f a TrivialContext)) baseGraph)

runCompiledProofPhaseWithRuntimeBuilder ::
  forall owner capability f a c p.
  (Ord capability, Show capability, HasConstructorTag f, Show (ConstructorTag f), Show (f ()), Ord a, JoinSemilattice a, Ord c) =>
  SchedulerRefinement (SaturatingProofEGraphRuntimeState owner capability f a c p) RewriteRuleId ->
  (SaturatingProofEGraph owner capability f a c p -> RewriteContextSnapshot (EGraphU owner capability f a c)) ->
  ProofAnnotationBuilder c p ->
  Maybe c ->
  EGraphGuidance owner capability f a c ->
  EGraphSaturationConfig owner capability f a c ->
  [RulePlan (CompiledGuard capability f) f] ->
  [CompiledFactRule capability f] ->
  SaturatingProofEGraph owner capability f a c p ->
  Either
    (SaturationError (EGraphU owner capability f a c) RewriteRuleId)
    (EGraphProofSaturationReport owner capability f a c p)
runCompiledProofPhaseWithRuntimeBuilder schedulerRefinement rewriteContextSnapshotForCarrier proofAnnotationBuilder maybeActiveContext phaseGuidance saturationConfig compiledRules compiledFactRules proofGraph = do
  let compiledRuntimeProgram =
        baseProgram
          @'CompiledProgramStage
          @(EGraphU owner capability f a c)
          (rewriteRuleId @(EGraphU owner capability f a c))
          compiledRules
          compiledFactRules
      phaseSpec =
        withRewriteContext
          rewriteContextSnapshotForCarrier
          (withGuidance phaseGuidance saturationConfig)
  planValue <-
    first SaturationCompileFailure $
      ContextProgram.planFromCompiledProgram @(EGraphU owner capability f a c)
        phaseSpec
        compiledRuntimeProgram
  let runtimePolicy =
        ContextProof.proofRuntimePolicyWith @(EGraphU owner capability f a c) @p
          ( \_state matches ->
              candidateSpaceForSupportedMatches
                @(EGraphU owner capability f a c)
                (supportedMatchRuleKey @(EGraphU owner capability f a c))
                (compareSupportedMatches @(EGraphU owner capability f a c))
                matches
          )
          ( scheduleRefinedRoundSupportedMatches
              @(EGraphU owner capability f a c)
              @(SaturatingProofEGraph owner capability f a c p)
              @RewriteRuleId
              schedulerRefinement
          )
          proofAnnotationBuilder
          maybeActiveContext
  bimap SaturationRunFailure snd $
    ContextReport.runPlanWithPolicy @(EGraphU owner capability f a c)
      runtimePolicy
      planValue
      proofGraph

mkStrategyPhaseSummary ::
  EGraphStrategyPhase owner capability f a c ->
  ContextReport.SaturationReportOf u carrier schedulerGroup tracePayload ->
  Bool ->
  PhaseSummary SaturationBudget SaturationTermination ()
mkStrategyPhaseSummary guidePhase saturationReport progressed =
  PhaseSummary
    { spsName = spName guidePhase,
      spsBudget = planSpecSaturationBudget (spConfig guidePhase),
      spsUsedGuidance = not (null (gateName (spGuidance guidePhase))),
      spsResult = ContextReport.srResult saturationReport,
      spsIterations = ContextReport.reportIterationCount saturationReport,
      spsMatchesApplied = ContextReport.reportMatchesApplied saturationReport,
      spsFactRounds = ContextReport.reportFactRoundCount saturationReport,
      spsGuideRounds = ContextReport.reportGuideRoundCount saturationReport,
      spsProgressed = progressed,
      spsAnnotation = Nothing
    }

phaseProgressed ::
  forall owner capability f a c.
  (Ord capability, Show capability, HasConstructorTag f, Show (ConstructorTag f), Show (f ()), Ord a, JoinSemilattice a, Ord c) =>
  EGraph f a ->
  SaturationReport (EGraphU owner capability f a c) ->
  Bool
phaseProgressed initialGraph saturationReport
  | ContextReport.reportMatchesApplied saturationReport == 0 && not (saturationReportHasFacts saturationReport) =
      False
  | otherwise =
      not (baseGraphEquals @(EGraphU owner capability f a c) initialGraph (saturationReportBaseGraph saturationReport))
        || saturationReportHasFacts saturationReport

type CompiledEGraphProgram :: Type -> (Type -> Type) -> Type
data CompiledEGraphProgram capability f = CompiledEGraphProgram
  { cspRewrites :: ![RulePlan (CompiledGuard capability f) f],
    cspFactRules :: ![CompiledFactRule capability f]
  }

type ProofSaturationSpec :: Type -> Type -> (Type -> Type) -> Type -> Type -> Type -> Type
data ProofSaturationSpec owner capability f a c p = ProofSaturationSpec
  { pssSaturation :: !(PlanSpec (EGraphU owner capability f a c) (SatGraph (EGraphU owner capability f a c)) RewriteRuleId),
    pssGuidance :: !(Maybe (GuidanceConfig (Pattern f))),
    pssProofBuilder :: !(ProofAnnotationBuilder c p),
    pssActiveContext :: !(Maybe c)
  }

type ExtractionPlan :: (Type -> Type) -> Type -> Type -> Type
data ExtractionPlan f a cost
  = ExtractByCost !(CostAlgebra f cost)
  | ExtractByAnalysisCost !(AnalysisCostAlgebra f a cost)

compileEGraphProgram ::
  forall owner capability f a c.
  (Ord capability, Show capability,  HasConstructorTag f,
    Show (ConstructorTag f),
    Show (f ()),
    Ord a,
    JoinSemilattice a,
    Ord c
  ) =>
  EGraph f a ->
  PlanSpec (EGraphU owner capability f a c) (SatGraph (EGraphU owner capability f a c)) RewriteRuleId ->
  [RawRewriteRule (RewriteCondition capability f) f] ->
  Either (SaturationError (EGraphU owner capability f a c) RewriteRuleId) (CompiledEGraphProgram capability f)
compileEGraphProgram graph _planSpecValue rawRules = do
  compiledRewrites <-
    first (SaturationCompileFailure . SaturationRewriteRulesFailed BaseProgramSite)
      (compileRewriteRules @(EGraphU owner capability f a c) (expandEGraphRulesByTheory (eGraphTheorySpec graph) rawRules))
  pure CompiledEGraphProgram { cspRewrites = compiledRewrites, cspFactRules = [] }

expandEGraphRulesByTheory ::
  Language f =>
  TheorySpec f ->
  [RawRewriteRule (RewriteCondition capability f) f] ->
  [RawRewriteRule (RewriteCondition capability f) f]
expandEGraphRulesByTheory spec =
  zipWith
    (\expandedRuleId rule -> rule {rrId = RewriteRuleId expandedRuleId})
    [0 ..]
    . foldMap (\rule -> fmap (\lhs -> rule {rrLhs = lhs}) (expandPatternByTheory spec (rrLhs rule)))

supportPlanSpec ::
  (Ord capability, Show capability, HasConstructorTag f, ZipMatch f, Show (ConstructorTag f), Show (f ()), Ord a, JoinSemilattice a, Ord c) =>
  (SaturatingProofEGraph owner capability f a c p -> RewriteContextSnapshot (EGraphU owner capability f a c)) ->
  Maybe (GuidanceConfig (Pattern f)) ->
  SaturatingProofEGraph owner capability f a c p ->
  EGraphSaturationConfig owner capability f a c ->
  PlanSpec
    (EGraphU owner capability f a c)
    (SaturatingProofEGraph owner capability f a c p)
    (SupportScheduleGroup (EGraphU owner capability f a c))
supportPlanSpec rewriteContextSnapshotForProof maybeGuidance proofGraph saturationConfig =
  withGuidance
    (egraphSupportGuidance (rcsRewriteContext (rewriteContextSnapshotForProof proofGraph)) maybeGuidance)
    ( withRewriteContext
        rewriteContextSnapshotForProof
        ( withSchedulerConfig
            ((planSpecSchedulerConfig saturationConfig) {scPriorityProfile = emptyPriorityProfile})
            ( planSpec
                (planSpecSaturationBudget saturationConfig)
                (planSpecMatchingStrategy saturationConfig)
                (rcsRewriteContext (rewriteContextSnapshotForProof proofGraph))
            )
        )
    )

type EGraphSupportPlan owner capability f a c p =
  Plan
    (EGraphU owner capability f a c)
    (SaturatingProofEGraph owner capability f a c p)
    (SupportScheduleGroup (EGraphU owner capability f a c))

type EGraphSupportRunResult owner capability f a c p =
  ContextRunResult
    (EGraphU owner capability f a c)
    (SaturatingProofEGraph owner capability f a c p)
    (SupportScheduleGroup (EGraphU owner capability f a c))
    (SupportSaturationReportFor (EGraphU owner capability f a c) (SaturatingProofEGraph owner capability f a c p))

prepareEGraphSupportPlan ::
  forall owner capability f a c p.
  (Ord capability, Show capability, HasConstructorTag f, ZipMatch f, Show (ConstructorTag f), Show (f ()), Ord a, JoinSemilattice a, Ord c) =>
  Maybe (GuidanceConfig (Pattern f)) ->
  (SaturatingProofEGraph owner capability f a c p -> RewriteContextSnapshot (EGraphU owner capability f a c)) ->
  PlanSpec (EGraphU owner capability f a c) (SatGraph (EGraphU owner capability f a c)) RewriteRuleId ->
  SheafTwist.SupportedRuleBook owner c (RawRewriteRule (RewriteCondition capability f) f) ->
  SheafTwist.SupportedFactBook owner c (FactRule capability f) ->
  SaturatingProofEGraph owner capability f a c p ->
  Either
    (SaturationError (EGraphU owner capability f a c) (SupportScheduleGroup (EGraphU owner capability f a c)))
    (EGraphSupportPlan owner capability f a c p)
prepareEGraphSupportPlan maybeGuidance rewriteContextSnapshotForProof saturationConfig supportFamilyValue extraFactBook proofGraph = do
  compiledProgram <-
    first SaturationCompileFailure $
      compileSupportProgram
        @(EGraphU owner capability f a c)
        (cegSite (sceContextGraph (pgGraph proofGraph)))
        supportFamilyValue
        extraFactBook
  SupportDriver.prepareSupportPlan
    ( supportPlanSpec
        rewriteContextSnapshotForProof
        maybeGuidance
        proofGraph
        saturationConfig
    )
    compiledProgram

runEGraphSupportPlan ::
  forall owner capability f a c p.
  (Ord capability, Show capability, HasConstructorTag f, ZipMatch f, Show (ConstructorTag f), Show (f ()), Ord a, JoinSemilattice a, Ord c) =>
  ProofAnnotationBuilder c p ->
  TerminationGoal (SaturatingProofEGraph owner capability f a c p) ->
  EGraphSupportPlan owner capability f a c p ->
  SaturatingProofEGraph owner capability f a c p ->
  Either
    (SaturationError (EGraphU owner capability f a c) (SupportScheduleGroup (EGraphU owner capability f a c)))
    (EGraphSupportRunResult owner capability f a c p)
runEGraphSupportPlan proofBuilder terminationGoal planValue =
  SupportDriver.runSupportPlan
    ( contextExecutionSpec
        (supportRuntimePolicy identitySchedulerRefinement proofBuilder)
        (carrierGoal terminationGoal)
    )
    planValue

runEGraphSupportPlanObserved ::
  forall owner capability f a c p.
  (Ord capability, Show capability, HasConstructorTag f, ZipMatch f, Show (ConstructorTag f), Show (f ()), Ord a, JoinSemilattice a, Ord c) =>
  ProofAnnotationBuilder c p ->
  TerminationGoal (SaturatingProofEGraph owner capability f a c p) ->
  EGraphSupportPlan owner capability f a c p ->
  SaturatingProofEGraph owner capability f a c p ->
  IO
    ( RuntimeObservedResult
        (SaturationError (EGraphU owner capability f a c) (SupportScheduleGroup (EGraphU owner capability f a c)))
        (EGraphSupportRunResult owner capability f a c p)
    )
runEGraphSupportPlanObserved proofBuilder terminationGoal planValue =
  SupportDriver.runSupportPlanObserved
    ( contextExecutionSpec
        (supportRuntimePolicy identitySchedulerRefinement proofBuilder)
        (carrierGoal terminationGoal)
    )
    planValue

runContextualRewriteSaturation ::
  forall owner capability f a c.
  (Ord capability, Show capability, HasConstructorTag f, ZipMatch f, Show (ConstructorTag f), Show (f ()), Ord a, JoinSemilattice a, Ord c) =>
  Maybe (GuidanceConfig (Pattern f)) ->
  EGraphSaturationConfig UnitContextSiteOwner capability f a TrivialContext ->
  c ->
  SheafTwist.SupportedRuleBook owner c (RawRewriteRule (RewriteCondition capability f) f) ->
  ContextEGraph owner f a c ->
  Either (ContextualRewriteSaturationError capability f a c) (EGraphSaturationReport capability f a)
runContextualRewriteSaturation maybeGuidance saturationConfig contextValue rewriteFamilyValue contextGraph = do
  rules <-
    first
      ContextualRewriteRuleLookupFailed
      (SheafTwist.rulesActiveAt (cegSite contextGraph) contextValue rewriteFamilyValue)

  localizedGraph <-
    first ContextualRewriteMaterializationFailed
      (materializedContextGraphAt contextValue contextGraph)
  let planSpecValue =
        withGuidance
          (egraphSupportGuidance emptyRewriteRuntimeCapabilities maybeGuidance)
          saturationConfig
  first ContextualRewriteSaturationFailed $
    runSaturationSpec planSpecValue rules localizedGraph

runContextualRewriteProofSaturation ::
  forall owner capability f a c p.
  (Ord capability, Show capability, HasConstructorTag f, ZipMatch f, Show (ConstructorTag f), Show (f ()), Ord a, JoinSemilattice a, Ord c) =>
  ProofAnnotationBuilder c p ->
  Maybe (GuidanceConfig (Pattern f)) ->
  PlanSpec (EGraphU owner capability f a c) (SatGraph (EGraphU owner capability f a c)) RewriteRuleId ->
  c ->
  SheafTwist.SupportedRuleBook owner c (RawRewriteRule (RewriteCondition capability f) f) ->
  SaturatingProofEGraph owner capability f a c p ->
  Either (SaturationError (EGraphU owner capability f a c) RewriteRuleId) (EGraphProofSaturationReport owner capability f a c p)
runContextualRewriteProofSaturation proofCarrier maybeGuidance saturationConfig contextValue rewriteFamilyValue proofGraph = do
  rules <-
    first
      (SaturationCompileFailure . SaturationSupportContextLookupFailed)
      ( SheafTwist.rulesActiveAt
          (cegSite (sceContextGraph (pgGraph proofGraph)))
          contextValue
          rewriteFamilyValue
      )

  let proofSpec =
        ProofSaturationSpec
          { pssSaturation = saturationConfig,
            pssGuidance = maybeGuidance,
            pssProofBuilder = proofCarrier,
            pssActiveContext = Just contextValue
          }
  runProofSaturationSpec proofSpec rules proofGraph

saturate ::
  forall capability f a.
  (Ord capability, Show capability, HasConstructorTag f, Show (ConstructorTag f), Show (f ()), Ord a, JoinSemilattice a) =>
  SaturationBudget ->
  [RawRewriteRule (RewriteCondition capability f) f] ->
  EGraph f a ->
  Either (SaturationError (EGraphU UnitContextSiteOwner capability f a TrivialContext) RewriteRuleId) (EGraphSaturationReport capability f a)
saturate budget =
  runSaturationSpec
    ( genericJoinSaturationConfig budget
    )

saturateWith ::
  forall capability f a.
  (Ord capability, Show capability, HasConstructorTag f, Show (ConstructorTag f), Show (f ()), Ord a, JoinSemilattice a) =>
  EGraphSaturationConfig UnitContextSiteOwner capability f a TrivialContext ->
  [RawRewriteRule (RewriteCondition capability f) f] ->
  EGraph f a ->
  Either (SaturationError (EGraphU UnitContextSiteOwner capability f a TrivialContext) RewriteRuleId) (EGraphSaturationReport capability f a)
saturateWith saturationConfig =
  runSaturationSpec (saturationConfig)

saturateWithSchedulerRefinement ::
  forall capability f a.
  (Ord capability, Show capability, HasConstructorTag f, Show (ConstructorTag f), Show (f ()), Ord a, JoinSemilattice a) =>
  SchedulerRefinement (PlainEGraphRuntimeState capability f a) RewriteRuleId ->
  EGraphSaturationConfig UnitContextSiteOwner capability f a TrivialContext ->
  [FactRule capability f] ->
  [RawRewriteRule (RewriteCondition capability f) f] ->
  EGraph f a ->
  Either
    (SaturationError (EGraphU UnitContextSiteOwner capability f a TrivialContext) RewriteRuleId)
    (SaturationReport (EGraphU UnitContextSiteOwner capability f a TrivialContext))
saturateWithSchedulerRefinement schedulerRefinement saturationConfig factRules rawRules initialGraph = do
  compiledRules <-
    first
      (SaturationCompileFailure . SaturationRewriteRulesFailed BaseProgramSite)
      (compileRewriteRules @(EGraphU UnitContextSiteOwner capability f a TrivialContext) rawRules)
  compiledFactRules <-
    first
      (SaturationCompileFailure . SaturationFactRulesFailed BaseProgramSite)
      (compileFactRules @(EGraphU UnitContextSiteOwner capability f a TrivialContext) factRules)
  runCompiledPlainPhaseWithRuntimeBuilder
    schedulerRefinement
    (planSpecRewriteContextSnapshot saturationConfig)
    (planSpecGuidance saturationConfig)
    saturationConfig
    compiledRules
    compiledFactRules
    initialGraph

saturateByStrategyWithSchedulerRefinement ::
  forall capability f a.
  (Ord capability, Show capability, HasConstructorTag f, Show (ConstructorTag f), Show (f ()), Ord a, JoinSemilattice a) =>
  SchedulerRefinement (PlainEGraphRuntimeState capability f a) RewriteRuleId ->
  Program () (EGraphStrategyPhase UnitContextSiteOwner capability f a TrivialContext) ->
  [FactRule capability f] ->
  [RawRewriteRule (RewriteCondition capability f) f] ->
  EGraph f a ->
  Either
    (SaturationError (EGraphU UnitContextSiteOwner capability f a TrivialContext) RewriteRuleId)
    ( Report
        (EGraph f a)
        (SaturationReport (EGraphU UnitContextSiteOwner capability f a TrivialContext))
        (PhaseSummary SaturationBudget SaturationTermination ())
    )
saturateByStrategyWithSchedulerRefinement schedulerRefinement guideStrategy factRules rawRules initialGraph = do
  compiledRules <-
    first
      (SaturationCompileFailure . SaturationRewriteRulesFailed BaseProgramSite)
      (compileRewriteRules @(EGraphU UnitContextSiteOwner capability f a TrivialContext) rawRules)
  compiledFactRules <-
    first
      (SaturationCompileFailure . SaturationFactRulesFailed BaseProgramSite)
      (compileFactRules @(EGraphU UnitContextSiteOwner capability f a TrivialContext) factRules)
  fmap snd
    ( runIdentity
        ( runPhases
            guideStrategy
            (\() phase graph -> pure (runStrategyPhase compiledRules compiledFactRules phase graph))
            initialGraph
        )
    )
  where
    runStrategyPhase compiledRules compiledFactRules guidePhase graph = do
      saturationReport <-
        runCompiledPlainPhaseWithRuntimeBuilder
          schedulerRefinement
          (planSpecRewriteContextSnapshot (spConfig guidePhase))
          (spGuidance guidePhase)
          (spConfig guidePhase)
          compiledRules
          compiledFactRules
          graph
      let progressed = phaseProgressed graph saturationReport
      pure
        ( guidePhase,
          Execution
            { seState = saturationReportBaseGraph @(EGraphU UnitContextSiteOwner capability f a TrivialContext) saturationReport,
              seLatestReport = Just saturationReport,
              seTrace = PhaseTrace (mkStrategyPhaseSummary guidePhase saturationReport progressed),
              seVerdict = verdictForProgress progressed
            }
        )

saturateByStrategy ::
  forall capability f a.
  (Ord capability, Show capability, HasConstructorTag f, Show (ConstructorTag f), Show (f ()), Ord a, JoinSemilattice a) =>
  Program () (EGraphStrategyPhase UnitContextSiteOwner capability f a TrivialContext) ->
  [FactRule capability f] ->
  [RawRewriteRule (RewriteCondition capability f) f] ->
  EGraph f a ->
  Either
    (SaturationError (EGraphU UnitContextSiteOwner capability f a TrivialContext) RewriteRuleId)
    ( Report
        (EGraph f a)
        (SaturationReport (EGraphU UnitContextSiteOwner capability f a TrivialContext))
        (PhaseSummary SaturationBudget SaturationTermination ())
    )
saturateByStrategy =
  saturateByStrategyWithSchedulerRefinement identitySchedulerRefinement

runSaturationSpec ::
  forall capability f a.
  (Ord capability, Show capability, HasConstructorTag f, Show (ConstructorTag f), Show (f ()), Ord a, JoinSemilattice a) =>
  PlanSpec (EGraphU UnitContextSiteOwner capability f a TrivialContext) (SatGraph (EGraphU UnitContextSiteOwner capability f a TrivialContext)) RewriteRuleId ->
  [RawRewriteRule (RewriteCondition capability f) f] ->
  EGraph f a ->
  Either (SaturationError (EGraphU UnitContextSiteOwner capability f a TrivialContext) RewriteRuleId) (EGraphSaturationReport capability f a)
runSaturationSpec planSpecValue rawRules initialGraph =
  let saturationConfig = planSpecValue
   in do
        compiledProgram <-
          compileEGraphProgram @UnitContextSiteOwner @capability @f @a @TrivialContext initialGraph planSpecValue rawRules
        runCompiledPlainPhaseWithRuntimeBuilder
          identitySchedulerRefinement
          (planSpecRewriteContextSnapshot planSpecValue)
          (planSpecGuidance planSpecValue)
          saturationConfig
          (cspRewrites compiledProgram)
          (cspFactRules compiledProgram)
          initialGraph

runProofSaturationSpec ::
  forall owner capability f a c p.
  (Ord capability, Show capability, HasConstructorTag f, ZipMatch f, Show (ConstructorTag f), Show (f ()), Ord a, JoinSemilattice a, Ord c) =>
  ProofSaturationSpec owner capability f a c p ->
  [RawRewriteRule (RewriteCondition capability f) f] ->
  SaturatingProofEGraph owner capability f a c p ->
  Either (SaturationError (EGraphU owner capability f a c) RewriteRuleId) (EGraphProofSaturationReport owner capability f a c p)
runProofSaturationSpec proofSpec rawRules proofGraph =
  let saturationConfig = pssSaturation proofSpec
   in do
        compiledProgram <-
          compileEGraphProgram @owner @capability @f @a @c (cegBase (sceContextGraph (pgGraph proofGraph))) (saturationConfig) rawRules
        runCompiledProofPhaseWithRuntimeBuilder
          identitySchedulerRefinement
          (const (staticRewriteContextSnapshot emptyRewriteRuntimeCapabilities))
          (pssProofBuilder proofSpec)
          (pssActiveContext proofSpec)
          ( egraphSupportGuidance
              (emptyRewriteRuntimeCapabilities)
              (pssGuidance proofSpec)
          )
          saturationConfig
          (cspRewrites compiledProgram)
          (cspFactRules compiledProgram)
          proofGraph

runEqualitySaturation ::
  forall capability f a cost.
  (Ord capability, Show capability, HasConstructorTag f, Show (ConstructorTag f), Show (f ()), Ord a, JoinSemilattice a, Ord cost) =>
  PlanSpec (EGraphU UnitContextSiteOwner capability f a TrivialContext) (SatGraph (EGraphU UnitContextSiteOwner capability f a TrivialContext)) RewriteRuleId ->
  ExtractionPlan f a cost ->
  Fix f ->
  [RawRewriteRule (RewriteCondition capability f) f] ->
  EGraph f a ->
  Either
    (SaturationError (EGraphU UnitContextSiteOwner capability f a TrivialContext) RewriteRuleId)
    (Maybe (ExtractionResult f cost), EGraphSaturationReport capability f a)
runEqualitySaturation planSpecValue extractionPlan term rawRules initialGraph = do
  (initialClassId, seededGraph) <-
    first
      (SaturationRunFailure . SaturationRunApplyFailed . EGraphRewriteApplicationFailed . RewriteClassIdAllocationFailed)
      (addTerm term initialGraph)
  saturationReport <- runSaturationSpec planSpecValue rawRules seededGraph
  let saturatedGraph = saturationReportBaseGraph saturationReport
      targetClassId = canonicalizeClassId saturatedGraph initialClassId
      extractionResult =
        stableExtractionSnapshotFromEGraph saturatedGraph >>= \snapshot ->
          case extractionPlan of
            ExtractByCost costAlgebra ->
              extract costAlgebra targetClassId snapshot
            ExtractByAnalysisCost costAlgebra ->
              extractWithAnalysis costAlgebra targetClassId snapshot
  pure (extractionResult, saturationReport)

genericJoinSaturationConfig ::
  (HasConstructorTag f, Show (ConstructorTag f), Show (f ()), Ord capability, Show capability, Ord c) =>
  SaturationBudget ->
  PlanSpec (EGraphU owner capability f a c) (SatGraph (EGraphU owner capability f a c)) RewriteRuleId
genericJoinSaturationConfig budget =
  withSchedulerConfig
    deterministicSchedulerConfig
    (planSpec budget GenericJoinMatching emptyRewriteRuntimeCapabilities)

genericJoinPerContextSaturationConfig ::
  (HasConstructorTag f, Show (ConstructorTag f), Show (f ()), Ord capability, Show capability, Ord c) =>
  SaturationBudget ->
  PlanSpec (EGraphU owner capability f a c) (SatGraph (EGraphU owner capability f a c)) RewriteRuleId
genericJoinPerContextSaturationConfig budget =
  withSchedulerConfig
    deterministicSchedulerConfig
    (planSpec budget GenericJoinPerContextMatching emptyRewriteRuntimeCapabilities)

genericJoinSaturationSpec ::
  (HasConstructorTag f, Show (ConstructorTag f), Show (f ()), Ord capability, Show capability, Ord c) =>
  SaturationBudget ->
  PlanSpec (EGraphU owner capability f a c) (SatGraph (EGraphU owner capability f a c)) RewriteRuleId
genericJoinSaturationSpec =
  genericJoinSaturationConfig
