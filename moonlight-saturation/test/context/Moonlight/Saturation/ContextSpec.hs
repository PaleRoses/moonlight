{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Moonlight.Saturation.ContextSpec
  ( contextTests,
  )
where

import Data.Bifunctor (bimap)
import Data.Functor (void)
import Data.Functor.Identity
  ( Identity,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Moonlight.Core (RewriteRuleId (..))
import Moonlight.Delta.Scope
  ( dirtyScope,
    scopedDelta,
  )
import Moonlight.Core
  ( MatchActivationIndex (..),
    SiteIndex (..),
    SiteProgram (..),
  )
import Moonlight.Saturation.Context.Program.Compile
  ( compileBase,
    planFromCompiledProgram,
  )
import Moonlight.Saturation.Context.Driver
  ( ContextRunResult (..),
    ContextRunSpec (..),
    carrierGoal,
    compileContextProgram,
    contextRunSpec,
    plainContextRunSpec,
    resumeContextPlan,
    resumableRuntimeState,
    runContextPlan,
    runContextProgram,
  )
import Moonlight.Saturation.Context.Program.Plan
  ( planPlanSpec,
    planSchedulerConfig,
  )
import Moonlight.Saturation.Context.Program.Source
  ( ProgramM,
    activateBaseRewrite,
    base,
    context,
    fact,
    finishProgram,
    include,
    program,
    rewrite,
    supportBaseRewrite,
  )
import Moonlight.Saturation.Context.Program.Spec
  ( PlanSpec,
    RewriteContextSnapshot (..),
    defaultPlanSpec,
    deterministicSchedulerConfig,
    planSpec,
    planSpecSchedulerConfig,
    traceAllSchedulerConfig,
    validatePlanSpec,
    withGuidance,
    withRewriteContext,
    withSchedulerConfig,
  )
import Moonlight.Saturation.Context.Program.View
  ( SaturationRoundView,
  )
import Moonlight.Saturation.Context.Runtime.Carrier.Plain
  ( plainRuntimePolicy,
    plainRuntimePolicyWith,
  )
import Moonlight.Saturation.Context.Runtime.Carrier.Schedule
  ( candidateSpaceForSupportedMatches,
    compareSupportedMatches,
    scheduleRoundSupportedMatches,
  )
import Moonlight.Saturation.Context.Runtime.Match.Batch
  ( MatchBatch,
    matchBatchFromList,
  )
import Moonlight.Saturation.Context.Runtime.Facts
  ( deriveContextFactViews,
  )
import Moonlight.Saturation.Context.Runtime.Engine
  ( RuntimeIOTiming (..),
    RuntimeObservedResult (..),
    runPlanWithPolicy,
    runRuntime,
    resumeRuntime,
    resumePlanWithPolicy,
    runtimeStateFromCarrier,
    runPlanWithPolicyAndGoal,
    runPlanWithPolicyAndGoalWithApplyIO,
    runPlanWithPolicyAndGoalWithApplyIOObserved,
  )
import Moonlight.Saturation.Context.Error
  ( GateCompatibilityError (..),
    PlanCompileError (..),
    PlanSpecViolation (..),
    RuntimeResumeError (..),
    SaturationBudgetError (..),
    SaturationCompileError (..),
    SaturationError (..),
    SaturationRunError (..),
  )
import Moonlight.Saturation.Context.Runtime.Report
  ( SaturationReportOf,
    mkReport,
    reportContextFacts,
    reportIterationCount,
    reportFactRoundCount,
    reportFactRounds,
    reportMatchesApplied,
    reportScheduleTrace,
    srCarrier,
    srFinalCore,
    srResult,
  )
import Moonlight.Saturation.Context.Runtime.Policy
  ( RuntimePolicy (..),
  )
import Moonlight.Saturation.Context.Runtime.Schedule.Decision
  ( RuntimeScheduleDecision,
  )
import Moonlight.Saturation.Context.Runtime.State
  ( FactDerivationResult (..),
    FactViewKey (..),
    RuntimeCore (..),
    RuntimeReportWindow (..),
    RuntimeState (..),
    advanceRuntimeCoreFactViewGraphChanges,
    initialPlainRuntimeState,
    initialRuntimeCore,
    seedRuntimeStateFacts,
  )
import Moonlight.Saturation.Core
  ( ApplyOutcome (..),
    SaturationBudget (..),
    SaturationTermination (..),
    TerminationGoal (..),
  )
import Moonlight.Control.Gate
  ( Gate (..),
    GateValidation (..),
    MatchSelector (..),
    MatchSelectorResult (..),
    noGate,
  )
import Moonlight.Control.Candidate
  ( CandidateSpace,
  )
import Moonlight.Control.Schedule
  ( backoffConfig,
    ScheduleOrder (..),
    SchedulerConfig (..),
    canonicalSchedulerConfig,
    defaultSchedulerConfig,
    traceLastEntries,
  )
import Moonlight.Control.Schedule.Round
  ( ScheduleTrace (..),
  )
import Moonlight.Saturation.Substrate
  ( FactViewGraphChanges (..),
    SatApplicationResult,
    SatGraph,
    SatRewriteContext,
    SatSupportedMatch,
    graphClassCount,
    applyContextualMatches,
    mergeSupportedMatch,
    supportedMatchBasis,
    supportedMatchWitnesses,
  )
import Moonlight.Saturation.Test.ContextFixture
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Moonlight.FiniteLattice
  ( supportBasis
  )

contextTests :: TestTree
contextTests =
  testGroup
    "context"
    [ testGroup
        "semantic surface"
        [ testContextLatticeFixtureValid,
          sourceDslScopesOnlyFreshEmission,
          sourceDslPreservesSequencing,
          compiledPlanMatchesDirectRunner,
          compiledPlanMatchesIOApplyRunner,
          observedIOApplyRunnerReportsPhaseTiming,
          contextDriverRunsSourceProgram,
          contextDriverAcceptsCustomSchedulerGroup,
          contextDriverReportsCompileFailure,
          contextDriverContramapsCarrierGoal,
          contextDriverPreservesStateForResume,
          factViewCacheSkipsCleanDerivation,
          factViewMissingBaseArtifactsIsCacheMiss,
          factViewSeedGenerationTracksStoreChanges,
          factViewCapabilityGenerationInvalidatesAllContexts,
          factViewRuleKeyChangeRederivesOnlyAffectedContext,
          factViewRoundBaseChangeInvalidatesEveryContext,
          factViewRoundFiberChangeUsesVisibilityClosure,
          reportBoundaryLaundersStaleFactViews,
          reportBoundaryUsesCompleteFactViewLineage,
          factViewChangedAuthorUsesVisibilityClosure,
          factViewBaseChangeInvalidatesEveryContext,
          planSpecCanonicalizesSchedulerConfig,
          planSpecValidationAccumulatesObstructions,
          resumeWithoutStampedIdentityFails,
          resumeRejectsChangedCompiledRuleIdentity,
          generatedWorkloadSaturatesByScale,
          observedApplicationSuppressesVisibleRepeatScheduling,
          schedulerUsesPostEnumerationMatchState,
          matchingDeltasComposeFactAndRuntimeChanges,
          guidanceRejectionPreventsFixedPoint,
          supportGluingUsesPreparedSite
        ],
      testGroup
        "termination laws"
        [ goalStopsSaturation,
          iterationLimitIsObservable,
          nodeLimitIsObservable
        ]
    ]

testContextLatticeFixtureValid :: TestTree
testContextLatticeFixtureValid =
  testCase "shared context fixture has a validated finite lattice" $
    testContextLatticeValidation @?= Right ()

expectRight :: Show err => Either err value -> IO value
expectRight eitherValue =
  case eitherValue of
    Right value -> pure value
    Left err -> assertFailure (show err)

seedState :: [Int] -> TestContextState
seedState classIds =
  primeBaseContextState
    (initialPlainRuntimeState @TestSubstrate emptyTestMatchState (graphFromClasses classIds))

testSaturationConfig :: SaturationBudget -> SchedulerConfig RewriteRuleId -> PlanSpec TestSubstrate (SatGraph TestSubstrate) RewriteRuleId
testSaturationConfig budget schedulerConfig =
  withSchedulerConfig schedulerConfig (planSpec budget () ())

runProgram ::
  SaturationBudget ->
  SchedulerConfig RewriteRuleId ->
  TestSiteProgram ->
  TestGoal ->
  [Int] ->
  IO (TestMatchState, TestReport)
runProgram budget schedulerConfig siteProgram terminationGoal classIds =
  expectRight
    ( runSaturation
        budget
        schedulerConfig
        siteProgram
        terminationGoal
        emptyTestMatchState
        (seedState classIds)
    )

applyTestMatchesWithIOBoundary ::
  SatRewriteContext TestSubstrate ->
  NonEmpty (SatSupportedMatch TestSubstrate) ->
  RuntimeState TestSubstrate (SatGraph TestSubstrate) RewriteRuleId ->
  IO
    ( Either
        (SaturationRunError TestSubstrate)
        (ApplyOutcome (SatApplicationResult TestSubstrate) (RuntimeState TestSubstrate (SatGraph TestSubstrate) RewriteRuleId))
    )
applyTestMatchesWithIOBoundary rewriteContext matches state =
  pure
    ( bimap
        SaturationRunApplyFailed
        ( \carrierOutcome ->
            ApplyOutcome
              { aoState = state {rsCarrier = aoState carrierOutcome},
                aoEffect = aoEffect carrierOutcome
              }
        )
        ( applyContextualMatches
            @TestSubstrate
            rewriteContext
            (NonEmpty.toList matches)
            (rsCarrier state)
        )
    )

remediationProgram :: TestRule -> ProgramM TestSubstrate ()
remediationProgram remediationRule =
  base $ do
    ruleId <- rewrite remediationRule
    activateBaseRewrite ruleId

graphClassCountGoal :: Int -> TerminationGoal (SatGraph TestSubstrate)
graphClassCountGoal requiredClasses =
  TerminationGoal
    (\graph -> graphClassCount @TestSubstrate graph >= requiredClasses)

type IndustrialScheduleGroup :: Type
data IndustrialScheduleGroup
  = IndustrialRemediationGroup
  deriving stock (Eq, Ord, Show)

customIndustrialPolicy ::
  RuntimePolicy
    TestSubstrate
    (SatGraph TestSubstrate)
    IndustrialScheduleGroup
    (SaturationReportOf TestSubstrate (SatGraph TestSubstrate) IndustrialScheduleGroup ())
customIndustrialPolicy =
  plainRuntimePolicyWith
    @TestSubstrate
    @IndustrialScheduleGroup
    (\_state matches -> candidateSpaceForSupportedMatches @TestSubstrate (const IndustrialRemediationGroup) (compareSupportedMatches @TestSubstrate) matches)
    customIndustrialSchedule

customIndustrialSchedule ::
  SchedulerConfig IndustrialScheduleGroup ->
  SatRewriteContext TestSubstrate ->
  SaturationRoundView TestSubstrate ->
  CandidateSpace Identity IndustrialScheduleGroup () (SatSupportedMatch TestSubstrate) ->
  RuntimeState TestSubstrate (SatGraph TestSubstrate) IndustrialScheduleGroup ->
  RuntimeScheduleDecision IndustrialScheduleGroup (SatSupportedMatch TestSubstrate)
customIndustrialSchedule =
  scheduleRoundSupportedMatches
    @TestSubstrate

sourceDslScopesOnlyFreshEmission :: TestTree
sourceDslScopesOnlyFreshEmission =
  testCase "source DSL scopes fresh emission and preserves absolute include" $ do
    let baseRule =
          makeBaseRule 201 [1] True noEffect
        supportRule =
          makeBaseRule 202 [1] True noEffect
        contextRule =
          makeContextRule 203 LeftContext [1] True noEffect
        nestedBaseRule =
          makeBaseRule 204 [1] True noEffect
        includedRule =
          makeBaseRule 205 [1] True noEffect
        contextFact =
          makeFactRule 206 99
        supportValue =
          principalSupportOf LeftContext
        includedFragment =
          program (void (rewrite includedRule))
        fragment =
          program $ do
            baseRuleId <- rewrite baseRule
            supportRuleId <- rewrite supportRule
            context LeftContext $ do
              void (rewrite contextRule)
              void (fact contextFact)
              activateBaseRewrite baseRuleId
              base $ do
                activateBaseRewrite baseRuleId
                void (rewrite nestedBaseRule)
              supportBaseRewrite supportRuleId supportValue
            context RightContext (include includedFragment)
    sourceProgram <-
      expectRight (finishProgram @TestSubstrate fragment)
    siBase (spRewriteRules sourceProgram)
      @?= [baseRule, supportRule, nestedBaseRule, includedRule]
    siContexts (spRewriteRules sourceProgram)
      @?= Map.singleton LeftContext [contextRule]
    siBase (spFactRules sourceProgram)
      @?= []
    siContexts (spFactRules sourceProgram)
      @?= Map.singleton LeftContext [contextFact]
    maiBase (spRewriteActivation sourceProgram)
      @?= Set.singleton (trId baseRule)
    maiContexts (spRewriteActivation sourceProgram)
      @?= Map.singleton LeftContext (Set.singleton (trId baseRule))
    spBaseRewriteSupport sourceProgram
      @?= Map.singleton (trId supportRule) supportValue

sourceDslPreservesSequencing :: TestTree
sourceDslPreservesSequencing =
  testCase "source DSL preserves applicative and monadic sequencing" $ do
    let firstRule =
          makeBaseRule 211 [1] True noEffect
        secondRule =
          makeBaseRule 212 [1] True noEffect
        thirdRule =
          makeBaseRule 213 [1] True noEffect
        fragment =
          program $ do
            (firstRuleId, secondRuleId) <-
              liftA2 (,) (rewrite firstRule) (rewrite secondRule)
            activateBaseRewrite firstRuleId
            thirdRuleId <- rewrite thirdRule
            activateBaseRewrite secondRuleId
            activateBaseRewrite thirdRuleId
    sourceProgram <-
      expectRight (finishProgram @TestSubstrate fragment)
    siBase (spRewriteRules sourceProgram)
      @?= [firstRule, secondRule, thirdRule]
    maiBase (spRewriteActivation sourceProgram)
      @?= Set.fromList [trId firstRule, trId secondRule, trId thirdRule]

compiledPlanMatchesDirectRunner :: TestTree
compiledPlanMatchesDirectRunner =
  testCase "compiled program equals direct runner" $ do
    let addRule =
          makeBaseRule
            7
            [1]
            True
            (addClassEffect (IntSet.singleton 2) IntSet.empty IntSet.empty)
        factRule = makeFactRule 0 11
        budget = SaturationBudget 4 32
        siteProgram = siteProgramWith [addRule] Map.empty [factRule] Map.empty
        spec = testSaturationConfig budget deterministicSchedulerConfig
    (_directMatchState, directReport) <-
      runProgram budget defaultSchedulerConfig siteProgram (classCountGoal 2) [1]
    compiledPlan <-
      expectRight (compileBase @TestSubstrate spec [addRule] [factRule])
    (_compiledState, compiledReport) <-
      expectRight
        ( runPlanWithPolicyAndGoal
            @TestSubstrate
            (plainRuntimePolicy @TestSubstrate)
            compiledPlan
            (classCountGoal 2)
            (graphFromClasses [1])
        )
    srResult compiledReport @?= srResult directReport
    reportIterationCount compiledReport @?= reportIterationCount directReport
    reportMatchesApplied compiledReport @?= reportMatchesApplied directReport
    graphClassCount @TestSubstrate (srCarrier compiledReport)
      @?= graphClassCount @TestSubstrate (srCarrier directReport)

compiledPlanMatchesIOApplyRunner :: TestTree
compiledPlanMatchesIOApplyRunner =
  testCase "compiled program equals IO apply-boundary runner" $ do
    let addRule =
          makeBaseRule
            8
            [1]
            True
            (addClassEffect (IntSet.singleton 2) IntSet.empty IntSet.empty)
        budget =
          SaturationBudget 4 32
        spec =
          testSaturationConfig budget deterministicSchedulerConfig
    compiledPlan <-
      expectRight (compileBase @TestSubstrate spec [addRule] [])
    (_pureState, pureReport) <-
      expectRight
        ( runPlanWithPolicyAndGoal
            @TestSubstrate
            (plainRuntimePolicy @TestSubstrate)
            compiledPlan
            (classCountGoal 2)
            (graphFromClasses [1])
        )
    (_ioState, ioReport) <-
      expectRight
        =<< runPlanWithPolicyAndGoalWithApplyIO
          @TestSubstrate
          (plainRuntimePolicy @TestSubstrate)
          compiledPlan
          (classCountGoal 2)
          applyTestMatchesWithIOBoundary
          (graphFromClasses [1])
    srResult ioReport @?= srResult pureReport
    reportMatchesApplied ioReport @?= reportMatchesApplied pureReport
    graphClassCount @TestSubstrate (srCarrier ioReport)
      @?= graphClassCount @TestSubstrate (srCarrier pureReport)

observedIOApplyRunnerReportsPhaseTiming :: TestTree
observedIOApplyRunnerReportsPhaseTiming =
  testCase "IO apply-boundary runner reports round apply rebuild and commit timings" $ do
    let addRule =
          makeBaseRule
            9
            [1]
            True
            (addClassEffect (IntSet.singleton 2) IntSet.empty IntSet.empty)
        spec =
          testSaturationConfig (SaturationBudget 4 32) deterministicSchedulerConfig
    compiledPlan <-
      expectRight (compileBase @TestSubstrate spec [addRule] [])
    observedResult <-
      runPlanWithPolicyAndGoalWithApplyIOObserved
        @TestSubstrate
        (plainRuntimePolicy @TestSubstrate)
        compiledPlan
        (classCountGoal 2)
        applyTestMatchesWithIOBoundary
        (graphFromClasses [1])
    (_finalState, saturationReport) <-
      expectRight (rorResult observedResult)
    srResult saturationReport @?= ReachedGoal
    reportMatchesApplied saturationReport @?= 1
    timingHasRuntimeWork (rorTiming observedResult) @?= True
  where
    timingHasRuntimeWork :: RuntimeIOTiming -> Bool
    timingHasRuntimeWork timing =
      ritRoundBuildNanoseconds timing
        + ritApplyNanoseconds timing
        + ritRebuildNanoseconds timing
        + ritCommitNanoseconds timing
        > 0

contextDriverRunsSourceProgram :: TestTree
contextDriverRunsSourceProgram =
  testCase "context driver runs the source EDSL through the plain policy" $ do
    let remediationRule =
          makeBaseRule
            101
            [1]
            True
            (addClassEffect (IntSet.singleton 2) IntSet.empty IntSet.empty)
        spec =
          testSaturationConfig (SaturationBudget 4 32) deterministicSchedulerConfig
        runSpec =
          plainContextRunSpec @TestSubstrate spec mempty
    runResult <-
      expectRight
        ( runContextProgram
            @TestSubstrate
            runSpec
            (remediationProgram remediationRule)
            (graphFromClasses [1])
        )
    let saturationReport =
          crrResult runResult
    srResult saturationReport @?= ReachedFixedPoint
    reportMatchesApplied saturationReport @?= 1
    graphClassCount @TestSubstrate (srCarrier saturationReport) @?= 2

contextDriverAcceptsCustomSchedulerGroup :: TestTree
contextDriverAcceptsCustomSchedulerGroup =
  testCase "context driver keeps source compilation generic over scheduler group" $ do
    let remediationRule =
          makeBaseRule
            102
            [1]
            True
            (addClassEffect (IntSet.singleton 2) IntSet.empty IntSet.empty)
        spec :: PlanSpec TestSubstrate (SatGraph TestSubstrate) IndustrialScheduleGroup
        spec =
          withSchedulerConfig
            (traceAllSchedulerConfig deterministicSchedulerConfig)
            (defaultPlanSpec @TestSubstrate @IndustrialScheduleGroup (SaturationBudget 4 32) ())
        runSpec =
          contextRunSpec
            spec
            customIndustrialPolicy
            (carrierGoal mempty)
    runResult <-
      expectRight
        ( runContextProgram
            @TestSubstrate
            runSpec
            (remediationProgram remediationRule)
            (graphFromClasses [1])
        )
    let saturationReport =
          crrResult runResult
    srResult saturationReport @?= ReachedFixedPoint
    reportMatchesApplied saturationReport @?= 1
    fmap strGroup (reportScheduleTrace saturationReport) @?= [IndustrialRemediationGroup]
    graphClassCount @TestSubstrate (srCarrier saturationReport) @?= 2

contextDriverReportsCompileFailure :: TestTree
contextDriverReportsCompileFailure =
  testCase "context driver reports invalid plan specs as compile failures" $ do
    let remediationRule =
          makeBaseRule
            103
            [1]
            True
            noEffect
        invalidSpec =
          testSaturationConfig (SaturationBudget (-1) 32) deterministicSchedulerConfig
        runSpec =
          plainContextRunSpec @TestSubstrate invalidSpec mempty
    case runContextProgram
      @TestSubstrate
      runSpec
      (remediationProgram remediationRule)
      (graphFromClasses [1]) of
      Left (SaturationCompileFailure (SaturationPlanInvalid (PlanCompileError violations))) ->
        violations @?= PlanSaturationBudgetViolation (NegativeHitIterationLimit (-1)) :| []
      Left err ->
        assertFailure ("expected plan compile failure, got " <> show err)
      Right _report ->
        assertFailure "expected plan compile failure"

contextDriverContramapsCarrierGoal :: TestTree
contextDriverContramapsCarrierGoal =
  testCase "plain context run spec contramaps carrier goals through runtime state" $ do
    let remediationRule =
          makeBaseRule
            104
            [1]
            True
            (addClassEffect (IntSet.singleton 2) IntSet.empty IntSet.empty)
        spec =
          testSaturationConfig (SaturationBudget 4 32) deterministicSchedulerConfig
        runSpec =
          plainContextRunSpec @TestSubstrate spec (graphClassCountGoal 2)
    runResult <-
      expectRight
        ( runContextProgram
            @TestSubstrate
            runSpec
            (remediationProgram remediationRule)
            (graphFromClasses [1])
        )
    let saturationReport =
          crrResult runResult
    srResult saturationReport @?= ReachedGoal
    graphClassCount @TestSubstrate (srCarrier saturationReport) @?= 2

contextDriverPreservesStateForResume :: TestTree
contextDriverPreservesStateForResume =
  testCase "context driver preserves final runtime state for resume" $ do
    let remediationRule =
          makeBaseRule
            105
            [1]
            True
            (addClassEffect (IntSet.singleton 2) IntSet.empty IntSet.empty)
        spec =
          testSaturationConfig (SaturationBudget 4 32) deterministicSchedulerConfig
        runSpec =
          plainContextRunSpec @TestSubstrate spec mempty
    planValue <-
      expectRight
        ( compileContextProgram
            @TestSubstrate
            (crsPlanSpec runSpec)
            (remediationProgram remediationRule)
        )
    runResult <-
      expectRight
        ( runContextPlan
            @TestSubstrate
            (crsExecution runSpec)
            planValue
            (graphFromClasses [1])
        )
    reportMatchesApplied (crrResult runResult) @?= 1
    graphClassCount @TestSubstrate (rsCarrier (resumableRuntimeState (crrState runResult))) @?= 2
    rcTotalMatches (rsCore (resumableRuntimeState (crrState runResult))) @?= 1
    resumeResult <-
      expectRight
        ( resumeContextPlan
            @TestSubstrate
            (crsExecution runSpec)
            planValue
            (crrState runResult)
        )
    srResult (crrResult resumeResult) @?= ReachedFixedPoint
    reportMatchesApplied (crrResult resumeResult) @?= 0
    graphClassCount @TestSubstrate (rsCarrier (resumableRuntimeState (crrState resumeResult))) @?= 2
    rcTotalMatches (rsCore (resumableRuntimeState (crrState resumeResult))) @?= 1

observedFactProgram :: TestSiteProgram
observedFactProgram =
  observedFactProgramWithLeftRuleKey 301

observedFactProgramWithLeftRuleKey :: Int -> TestSiteProgram
observedFactProgramWithLeftRuleKey leftRuleKey =
  siteProgramWith
    []
    Map.empty
    [makeObservedFactRule 300 100]
    ( Map.fromList
        [ (LeftContext, [makeObservedFactRule leftRuleKey 101]),
          (RightContext, [makeObservedFactRule 302 102]),
          (TopContext, [makeObservedFactRule 303 103])
        ]
    )

observedFactPlanSpec :: PlanSpec TestSubstrate TestGraph RewriteRuleId
observedFactPlanSpec =
  testSaturationConfig
    (SaturationBudget 4 32)
    deterministicSchedulerConfig

observedFactCapabilityPlanSpec :: PlanSpec TestSubstrate TestGraph RewriteRuleId
observedFactCapabilityPlanSpec =
  withRewriteContext
    ( \graph ->
        RewriteContextSnapshot
          { rcsCapabilityGeneration = fromIntegral (tgCapabilityGeneration graph),
            rcsRewriteContext = ()
          }
    )
    observedFactPlanSpec

factViewCacheSkipsCleanDerivation :: TestTree
factViewCacheSkipsCleanDerivation =
  testCase "clean fact views reuse cached artifacts without replaying rounds" $ do
    planValue <-
      expectRight
        ( planFromCompiledProgram
            @TestSubstrate
            observedFactPlanSpec
            observedFactProgram
        )
    (derivedState, initialReport) <-
      expectRight
        ( runPlanWithPolicy
            @TestSubstrate
            (plainRuntimePolicy @TestSubstrate)
            planValue
            emptyTestGraph
        )
    reportFactRoundCount initialReport @?= 4
    reportFactRounds initialReport
      @?= fmap
        (TestRound . IntSet.singleton)
        [100, 101, 102, 103]
    (_resumedState, resumedReport) <-
      expectRight
        ( resumePlanWithPolicy
            @TestSubstrate
            (plainRuntimePolicy @TestSubstrate)
            planValue
            derivedState
        )
    reportFactRoundCount resumedReport @?= 0
    reportFactRounds resumedReport @?= []

factViewMissingBaseArtifactsIsCacheMiss :: TestTree
factViewMissingBaseArtifactsIsCacheMiss =
  testCase "a base cache key without both artifacts is a cache miss" $ do
    let keyOnlyCore =
          (initialRuntimeCore @TestSubstrate @RewriteRuleId)
            { rcFactViewKeys =
                Map.singleton
                  BaseContext
                  FactViewKey
                    { fvkBaseGeneration = 0,
                      fvkFiberGeneration = 0,
                      fvkInputGeneration = 0,
                      fvkFactRuleIds = [RewriteRuleId 300],
                      fvkCapabilityGeneration = 0
                    }
            }
    derivedFacts <-
      expectRight
        ( deriveContextFactViews
            @TestSubstrate
            ()
            0
            (SiteIndex [makeObservedFactRule 300 100] Map.empty)
            emptyTestGraph
            keyOnlyCore
        )
    fdrFactRoundCount derivedFacts @?= 1

factViewSeedGenerationTracksStoreChanges :: TestTree
factViewSeedGenerationTracksStoreChanges =
  testCase "fact-input generation changes only when a context seed changes" $ do
    planValue <-
      expectRight
        ( planFromCompiledProgram
            @TestSubstrate
            observedFactPlanSpec
            observedFactProgram
        )
    (derivedState, _initialReport) <-
      expectRight
        ( runPlanWithPolicy
            @TestSubstrate
            (plainRuntimePolicy @TestSubstrate)
            planValue
            emptyTestGraph
        )
    let changedSeedState =
          seedRuntimeStateFacts
            @TestSubstrate
            (Map.singleton LeftContext (IntSet.singleton 999))
            derivedState
    (changedState, changedReport) <-
      expectRight
        ( resumePlanWithPolicy
            @TestSubstrate
            (plainRuntimePolicy @TestSubstrate)
            planValue
            changedSeedState
        )
    reportFactRoundCount changedReport @?= 1
    reportFactRounds changedReport
      @?= [TestRound (IntSet.singleton 101)]
    let unchangedSeedState =
          seedRuntimeStateFacts
            @TestSubstrate
            (Map.singleton LeftContext (IntSet.singleton 999))
            changedState
    (_unchangedState, unchangedReport) <-
      expectRight
        ( resumePlanWithPolicy
            @TestSubstrate
            (plainRuntimePolicy @TestSubstrate)
            planValue
            unchangedSeedState
        )
    reportFactRoundCount unchangedReport @?= 0
    reportFactRounds unchangedReport @?= []

factViewCapabilityGenerationInvalidatesAllContexts :: TestTree
factViewCapabilityGenerationInvalidatesAllContexts =
  testCase "capability generation changes invalidate every cached fact view" $ do
    planValue <-
      expectRight
        ( planFromCompiledProgram
            @TestSubstrate
            observedFactCapabilityPlanSpec
            observedFactProgram
        )
    (derivedState, _initialReport) <-
      expectRight
        ( runPlanWithPolicy
            @TestSubstrate
            (plainRuntimePolicy @TestSubstrate)
            planValue
            emptyTestGraph
        )
    let capabilityChangedState =
          derivedState
            { rsCarrier =
                (rsCarrier derivedState)
                  { tgCapabilityGeneration = 1
                  }
            }
    (immediatelyTerminatedState, immediatelyTerminatedReport) <-
      expectRight
        ( resumeRuntime
            @TestSubstrate
            (plainRuntimePolicy @TestSubstrate)
            planValue
            (TerminationGoal (const True))
            capabilityChangedState
        )
    srResult immediatelyTerminatedReport @?= ReachedGoal
    reportFactRoundCount immediatelyTerminatedReport @?= 0
    rcCurrentFactCapabilityGeneration (rsCore immediatelyTerminatedState) @?= 1
    (_resumedState, resumedReport) <-
      expectRight
        ( resumePlanWithPolicy
            @TestSubstrate
            (plainRuntimePolicy @TestSubstrate)
            planValue
            capabilityChangedState
        )
    reportFactRoundCount resumedReport @?= 4
    reportFactRounds resumedReport
      @?= fmap
        (TestRound . IntSet.singleton)
        [100, 101, 102, 103]

factViewRuleKeyChangeRederivesOnlyAffectedContext :: TestTree
factViewRuleKeyChangeRederivesOnlyAffectedContext =
  testCase "an active fact-rule key change rederives only its context" $ do
    planValue <-
      expectRight
        ( planFromCompiledProgram
            @TestSubstrate
            observedFactPlanSpec
            observedFactProgram
        )
    (derivedState, _initialReport) <-
      expectRight
        ( runPlanWithPolicy
            @TestSubstrate
            (plainRuntimePolicy @TestSubstrate)
            planValue
            emptyTestGraph
        )
    derivedFacts <-
      expectRight
        ( deriveContextFactViews
            @TestSubstrate
            ()
            0
            (spFactRules (observedFactProgramWithLeftRuleKey 311))
            (rsCarrier derivedState)
            (rsCore derivedState)
        )
    fdrFactRoundCount derivedFacts @?= 1

factViewRoundBaseChangeInvalidatesEveryContext :: TestTree
factViewRoundBaseChangeInvalidatesEveryContext =
  testCase "round base changes rederive every cached fact view" $ do
    let baseChangingProgram =
          siteProgramWith
            [ makeBaseRule
                320
                [1]
                True
                (addClassEffect (IntSet.singleton 2) IntSet.empty IntSet.empty)
            ]
            Map.empty
            [makeObservedFactRule 300 100]
            ( Map.fromList
                [ (LeftContext, [makeObservedFactRule 301 101]),
                  (RightContext, [makeObservedFactRule 302 102]),
                  (TopContext, [makeObservedFactRule 303 103])
                ]
            )
    planValue <-
      expectRight
        ( planFromCompiledProgram
            @TestSubstrate
            observedFactPlanSpec
            baseChangingProgram
        )
    (finalState, report) <-
      expectRight
        ( runPlanWithPolicy
            @TestSubstrate
            (plainRuntimePolicy @TestSubstrate)
            planValue
            (graphFromClasses [1])
        )
    reportFactRoundCount report @?= 8
    fmap Seq.length (rcFactRoundsByContext (rsCore finalState))
      @?= Map.fromList
        [ (BaseContext, 2),
          (LeftContext, 2),
          (RightContext, 2),
          (TopContext, 2)
        ]

factViewRoundFiberChangeUsesVisibilityClosure :: TestTree
factViewRoundFiberChangeUsesVisibilityClosure =
  testCase "round fiber changes rederive exactly the inheriting fact views" $ do
    let leftChangingRule =
          makeContextRule
            321
            LeftContext
            [1]
            True
            (factViewFiberChangeEffect LeftContext)
        leftChangingProgram =
          siteProgramWith
            [leftChangingRule]
            (Map.singleton LeftContext [leftChangingRule])
            [makeObservedFactRule 300 100]
            ( Map.fromList
                [ (LeftContext, [makeObservedFactRule 301 101]),
                  (RightContext, [makeObservedFactRule 302 102]),
                  (TopContext, [makeObservedFactRule 303 103])
                ]
            )
    planValue <-
      expectRight
        ( planFromCompiledProgram
            @TestSubstrate
            observedFactPlanSpec
            leftChangingProgram
        )
    (finalState, report) <-
      expectRight
        ( runPlanWithPolicy
            @TestSubstrate
            (plainRuntimePolicy @TestSubstrate)
            planValue
            (graphFromClasses [1])
        )
    reportFactRoundCount report @?= 6
    fmap Seq.length (rcFactRoundsByContext (rsCore finalState))
      @?= Map.fromList
        [ (BaseContext, 1),
          (LeftContext, 2),
          (RightContext, 1),
          (TopContext, 2)
        ]

reportBoundaryLaundersStaleFactViews :: TestTree
reportBoundaryLaundersStaleFactViews =
  testCase "report boundary launders every stale fact artifact" $ do
    let quotientGraph =
          (graphFromClasses [1])
            { tgClasses =
                IntMap.singleton 1 (IntSet.fromList [1, 2])
            }
        storedKey =
          FactViewKey
            { fvkBaseGeneration = 0,
              fvkFiberGeneration = 0,
              fvkInputGeneration = 0,
              fvkFactRuleIds = [],
              fvkCapabilityGeneration = 0
            }
        staleStore =
          IntSet.singleton 2
        canonicalStore =
          IntSet.singleton 1
        staleCore =
          (initialRuntimeCore @TestSubstrate @RewriteRuleId)
            { rcContextFactInputs = Map.singleton BaseContext staleStore,
              rcContextFacts = Map.singleton BaseContext staleStore,
              rcContextFactDerivations = Map.singleton BaseContext staleStore,
              rcFactViewBaseGeneration = 1,
              rcCurrentFactRuleIdsByContext = Map.singleton BaseContext [],
              rcCurrentFactCapabilityGeneration = 0,
              rcFactViewKeys = Map.singleton BaseContext storedKey
            }
        staleState =
          (initialPlainRuntimeState @TestSubstrate emptyTestMatchState quotientGraph)
            { rsCore = staleCore
            }
    report <-
      expectRight
        ( mkReport
            (rpCarrier testRuntimePolicy)
            ReachedGoal
            RuntimeReportWindow
              { rrwInitialState = staleState,
                rrwFinalState = staleState
              }
        )
    let exportedCore =
          srFinalCore report
    reportContextFacts report
      @?= Map.singleton BaseContext canonicalStore
    rcContextFactInputs exportedCore
      @?= Map.singleton BaseContext canonicalStore
    rcContextFactDerivations exportedCore
      @?= Map.singleton BaseContext canonicalStore
    rcContextFacts (rsCore staleState)
      @?= Map.singleton BaseContext staleStore

reportBoundaryUsesCompleteFactViewLineage :: TestTree
reportBoundaryUsesCompleteFactViewLineage =
  testCase "report boundary checks live rule and capability lineage" $ do
    let quotientGraph =
          (graphFromClasses [1])
            { tgClasses =
                IntMap.singleton 1 (IntSet.fromList [1, 2])
            }
        staleStore =
          IntSet.singleton 2
        canonicalStore =
          IntSet.singleton 1
        contexts =
          [BaseContext, LeftContext]
        staleStores =
          Map.fromList
            (fmap (\contextValue -> (contextValue, staleStore)) contexts)
        storedKeys =
          Map.fromList
            [ ( BaseContext,
                FactViewKey
                  { fvkBaseGeneration = 0,
                    fvkFiberGeneration = 0,
                    fvkInputGeneration = 0,
                    fvkFactRuleIds = [RewriteRuleId 999],
                    fvkCapabilityGeneration = 1
                  }
              ),
              ( LeftContext,
                FactViewKey
                  { fvkBaseGeneration = 0,
                    fvkFiberGeneration = 0,
                    fvkInputGeneration = 0,
                    fvkFactRuleIds = [RewriteRuleId 301],
                    fvkCapabilityGeneration = 0
                  }
              )
            ]
        staleCore =
          (initialRuntimeCore @TestSubstrate @RewriteRuleId)
            { rcContextFacts = staleStores,
              rcCurrentFactRuleIdsByContext =
                Map.fromList
                  [ (BaseContext, [RewriteRuleId 300]),
                    (LeftContext, [RewriteRuleId 301])
                  ],
              rcCurrentFactCapabilityGeneration = 1,
              rcFactViewKeys = storedKeys
            }
        staleState =
          (initialPlainRuntimeState @TestSubstrate emptyTestMatchState quotientGraph)
            { rsCore = staleCore
            }
        expectedStores =
          Map.fromList
            (fmap (\contextValue -> (contextValue, canonicalStore)) contexts)
    report <-
      expectRight
        ( mkReport
            (rpCarrier testRuntimePolicy)
            ReachedGoal
            RuntimeReportWindow
              { rrwInitialState = staleState,
                rrwFinalState = staleState
              }
        )
    reportContextFacts report @?= expectedStores

cachedFactViewCore :: RuntimeCore TestSubstrate RewriteRuleId
cachedFactViewCore =
  (initialRuntimeCore @TestSubstrate @RewriteRuleId)
    { rcContextFactDerivations =
        Map.fromList
          [ (contextValue, IntSet.singleton (fromEnum contextValue))
          | contextValue <- [BaseContext, LeftContext, RightContext, TopContext]
          ],
      rcFactViewKeys =
        Map.fromList
          [ (contextValue, cachedFactViewKey)
          | contextValue <- [BaseContext, LeftContext, RightContext, TopContext]
          ]
    }
  where
    cachedFactViewKey =
      FactViewKey
        { fvkBaseGeneration = 0,
          fvkFiberGeneration = 0,
          fvkInputGeneration = 0,
          fvkFactRuleIds = [],
          fvkCapabilityGeneration = 0
        }

factViewChangedAuthorUsesVisibilityClosure :: TestTree
factViewChangedAuthorUsesVisibilityClosure =
  testCase "a changed fiber author invalidates every inheriting cached context" $ do
    advancedCore <-
      expectRight
        ( advanceRuntimeCoreFactViewGraphChanges
            @TestSubstrate
            testPreparedSite
            FactViewGraphChanges
              { fvgcBaseChanged = False,
                fvgcChangedFiberAuthors = Set.singleton LeftContext
              }
            cachedFactViewCore
        )
    rcFactViewFiberGenerations advancedCore
      @?= Map.fromList
        [ (LeftContext, 1),
          (TopContext, 1)
        ]
    Map.keysSet (rcContextFactDerivations advancedCore)
      @?= Set.fromList [BaseContext, RightContext]

factViewBaseChangeInvalidatesEveryContext :: TestTree
factViewBaseChangeInvalidatesEveryContext =
  testCase "a base change invalidates every cached fact view" $ do
    advancedCore <-
      expectRight
        ( advanceRuntimeCoreFactViewGraphChanges
            @TestSubstrate
            testPreparedSite
            FactViewGraphChanges
              { fvgcBaseChanged = True,
                fvgcChangedFiberAuthors = Set.empty
              }
            cachedFactViewCore
        )
    rcFactViewBaseGeneration advancedCore @?= 1
    rcContextFactDerivations advancedCore @?= Map.empty

planSpecCanonicalizesSchedulerConfig :: TestTree
planSpecCanonicalizesSchedulerConfig =
  testCase "plan spec exposes canonical scheduler config" $ do
    let rawSchedulerConfig =
          ( defaultSchedulerConfig
            { scOrder = BackoffByGroup (backoffConfig 1 0),
              scTracePolicy = traceLastEntries 0
            }
          ) ::
            SchedulerConfig RewriteRuleId
        expectedSchedulerConfig =
          canonicalSchedulerConfig rawSchedulerConfig
        budget =
          SaturationBudget 4 32
        spec :: PlanSpec TestSubstrate (SatGraph TestSubstrate) RewriteRuleId
        spec =
          withSchedulerConfig rawSchedulerConfig (planSpec budget () ())
        inertRule =
          makeBaseRule 99 [1] True noEffect
    planSpecSchedulerConfig spec @?= expectedSchedulerConfig
    compiledPlan <-
      expectRight (compileBase @TestSubstrate spec [inertRule] [])
    planSchedulerConfig compiledPlan @?= expectedSchedulerConfig
    planSpecSchedulerConfig (planPlanSpec compiledPlan) @?= expectedSchedulerConfig

planSpecValidationAccumulatesObstructions :: TestTree
planSpecValidationAccumulatesObstructions =
  testCase "plan validation accumulates independent obstructions" $ do
    let rawSchedulerConfig =
          ( defaultSchedulerConfig
            { scOrder = BackoffByGroup (backoffConfig 0 (-3))
            }
          ) ::
            SchedulerConfig RewriteRuleId
        expectedViolations =
          PlanSaturationBudgetViolation (NegativeHitIterationLimit (-1))
            :| [ PlanGuidanceCompatibilityViolation (GateRejectedScheduler rawSchedulerConfig)
               ]
        spec :: PlanSpec TestSubstrate (SatGraph TestSubstrate) RewriteRuleId
        spec =
          withGuidance rejectingGuidance $
            withSchedulerConfig rawSchedulerConfig $
              planSpec (SaturationBudget (-1) 32) () ()
        inertRule =
          makeBaseRule 99 [1] True noEffect

    validatePlanSpec spec @?= Left (PlanCompileError expectedViolations)
    case compileBase @TestSubstrate spec [inertRule] [] of
      Left (SaturationPlanInvalid (PlanCompileError violations)) ->
        violations @?= expectedViolations
      Left err ->
        assertFailure ("expected accumulated plan validation failure, got " <> show err)
      Right _plan ->
        assertFailure "expected accumulated plan validation failure"

rejectingGuidance :: TestGuidance
rejectingGuidance =
  noGate
    { gateValidation =
        GateValidation (Left . GateRejectedScheduler)
    }

resumeWithoutStampedIdentityFails :: TestTree
resumeWithoutStampedIdentityFails =
  testCase "resume rejects a state without a stamped plan identity" $ do
    let addRule =
          makeBaseRule
            13
            [1]
            True
            (addClassEffect (IntSet.singleton 2) IntSet.empty IntSet.empty)
        budget =
          SaturationBudget 4 32
        spec =
          testSaturationConfig budget deterministicSchedulerConfig
    compiledPlan <-
      expectRight (compileBase @TestSubstrate spec [addRule] [])
    case resumePlanWithPolicy
      @TestSubstrate
      (plainRuntimePolicy @TestSubstrate)
      compiledPlan
      (initialPlainRuntimeState @TestSubstrate emptyTestMatchState (graphFromClasses [1])) of
      Left (SaturationRunResumeIncompatible RuntimeResumeMissingPlanIdentity) ->
        pure ()
      Left err ->
        assertFailure ("unexpected resume error: " <> show err)
      Right _ ->
        assertFailure "resume succeeded without a stamped plan identity"

resumeRejectsChangedCompiledRuleIdentity :: TestTree
resumeRejectsChangedCompiledRuleIdentity =
  testCase "resume rejects a changed compiled rule identity" $ do
    let originalRule =
          makeBaseRule
            14
            [1]
            True
            noEffect
        changedRule =
          makeBaseRule
            14
            [1]
            True
            (addClassEffect (IntSet.singleton 2) IntSet.empty IntSet.empty)
        spec =
          testSaturationConfig (SaturationBudget 1 32) deterministicSchedulerConfig
    originalPlan <-
      expectRight (compileBase @TestSubstrate spec [originalRule] [])
    changedPlan <-
      expectRight (compileBase @TestSubstrate spec [changedRule] [])
    (stampedState, _saturationReport) <-
      expectRight
        ( runPlanWithPolicyAndGoal
            @TestSubstrate
            (plainRuntimePolicy @TestSubstrate)
            originalPlan
            mempty
            (graphFromClasses [1])
        )
    case resumePlanWithPolicy
      @TestSubstrate
      (plainRuntimePolicy @TestSubstrate)
      changedPlan
      stampedState of
      Left (SaturationRunResumeIncompatible RuntimeResumePlanChanged) ->
        pure ()
      Left err ->
        assertFailure ("unexpected resume error: " <> show err)
      Right _ ->
        assertFailure "resume accepted a changed compiled rule identity"

generatedWorkloadSaturatesByScale :: TestTree
generatedWorkloadSaturatesByScale =
  testCase "one generated rule saturates one thousand matches" $ do
    let roots = [1 .. 1000]
        addRule =
          makeBaseRule
            11
            roots
            True
            (addClassEffect (IntSet.singleton 1001) IntSet.empty IntSet.empty)
        siteProgram = siteProgramWith [addRule] Map.empty [] Map.empty
    (_matchState, saturationReport) <-
      runProgram (SaturationBudget 4 4096) defaultSchedulerConfig siteProgram mempty roots
    srResult saturationReport @?= ReachedFixedPoint
    reportMatchesApplied saturationReport @?= length roots
    graphClassCount @TestSubstrate (srCarrier saturationReport) @?= length roots * 2

observedApplicationSuppressesVisibleRepeatScheduling :: TestTree
observedApplicationSuppressesVisibleRepeatScheduling =
  testCase "observed application suppresses the same visible match without previewing it" $ do
    let persistentRule =
          makeBaseRule
            23
            [1]
            False
            (addClassEffect IntSet.empty IntSet.empty IntSet.empty)
        siteProgram = siteProgramWith [persistentRule] Map.empty [] Map.empty
    (matchState, saturationReport) <-
      runProgram (SaturationBudget 4 4096) defaultSchedulerConfig siteProgram mempty [1]
    srResult saturationReport @?= ReachedFixedPoint
    reportMatchesApplied saturationReport @?= 1
    tmsRecordedSchedules matchState @?= 1
    graphClassCount @TestSubstrate (srCarrier saturationReport) @?= 2

schedulerUsesPostEnumerationMatchState :: TestTree
schedulerUsesPostEnumerationMatchState =
  testCase "candidate-space construction sees post-enumeration match state" $ do
    let addRule =
          makeBaseRule
            24
            [1]
            True
            (addClassEffect (IntSet.singleton 2) IntSet.empty IntSet.empty)
        spec =
          testSaturationConfig (SaturationBudget 4 32) deterministicSchedulerConfig
        policy =
          plainRuntimePolicyWith
            matchReadyCandidateSpace
            (scheduleRoundSupportedMatches @TestSubstrate)
    compiledPlan <-
      expectRight (compileBase @TestSubstrate spec [addRule] [])
    (_finalState, saturationReport) <-
      expectRight
        ( runPlanWithPolicyAndGoal
            @TestSubstrate
            policy
            compiledPlan
            (classCountGoal 2)
            (graphFromClasses [1])
        )
    srResult saturationReport @?= ReachedGoal
    reportMatchesApplied saturationReport @?= 1
  where
    matchReadyCandidateSpace ::
      RuntimeState TestSubstrate (SatGraph TestSubstrate) RewriteRuleId ->
      MatchBatch (SatSupportedMatch TestSubstrate) ->
      CandidateSpace Identity RewriteRuleId () (SatSupportedMatch TestSubstrate)
    matchReadyCandidateSpace state matches =
      candidateSpaceForSupportedMatches
        @TestSubstrate
        (trId . tmRule . tsmInner)
        (compareSupportedMatches @TestSubstrate)
        ( if tmsRoundAdvances (rsMatchState state) > 0
            then matches
            else matchBatchFromList []
        )

matchingDeltasComposeFactAndRuntimeChanges :: TestTree
matchingDeltasComposeFactAndRuntimeChanges =
  testCase "round matching delta composes carried runtime and derived fact deltas" $ do
    let staleRuntimeDelta =
          scopedDelta
            (dirtyScope (IntSet.singleton 41))
            (Just (IntSet.singleton 7))
        addRule =
          makeBaseRule
            25
            [1]
            True
            noEffect
        factRule =
          makeFactRule 26 11
        spec =
          testSaturationConfig (SaturationBudget 1 32) deterministicSchedulerConfig
    compiledPlan <-
      expectRight (compileBase @TestSubstrate spec [addRule] [factRule])
    let initialState =
          runtimeStateFromCarrier
            @TestSubstrate
            compiledPlan
            (graphFromClasses [1])
        seededState =
          initialState
            { rsCore =
                (rsCore initialState)
                  { rcMatchingDelta = staleRuntimeDelta
                  }
            }
    (finalState, _saturationReport) <-
      expectRight
        ( runRuntime
            @TestSubstrate
            (plainRuntimePolicy @TestSubstrate)
            compiledPlan
            mempty
            seededState
        )
    case tmsSeenDeltas (rsMatchState finalState) of
      SawDirty keys payload : _ -> do
        keys @?= IntSet.fromList [11, 41]
        payload @?= Just (IntSet.singleton 7)
      observed ->
        assertFailure ("expected composed dirty delta, saw " <> show observed)

guidanceRejectionPreventsFixedPoint :: TestTree
guidanceRejectionPreventsFixedPoint =
  testCase "guidance rejection keeps the frontier incomplete" $ do
    let rejectedRule =
          makeBaseRule
            27
            [1]
            True
            noEffect
        spec =
          withGuidance
            dropAllGuidance
            (testSaturationConfig (SaturationBudget 2 32) deterministicSchedulerConfig)
    compiledPlan <-
      expectRight (compileBase @TestSubstrate spec [rejectedRule] [])
    (_finalState, saturationReport) <-
      expectRight
        ( runPlanWithPolicyAndGoal
            @TestSubstrate
            (plainRuntimePolicy @TestSubstrate)
            compiledPlan
            mempty
            (graphFromClasses [1])
        )
    srResult saturationReport @?= HitIterationLimit
    reportMatchesApplied saturationReport @?= 0

dropAllGuidance :: TestGuidance
dropAllGuidance =
  noGate
    { gateSelector =
        MatchSelector
          { matchSelectorName = "drop-all",
            matchSelectorPreservesCount = False,
            runMatchSelector =
              \_view _group matches ->
                MatchSelectorResult
                  { msrAcceptedMatches = [],
                    msrTrace = [],
                    msrRejectedCount = fromIntegral (length matches)
                  }
          }
    }

supportGluingUsesPreparedSite :: TestTree
supportGluingUsesPreparedSite =
  testCase "support gluing uses the prepared context site" $
    let baseRule = makeBaseRule 0 [1] False noEffect
        innerMatch = TestMatch baseRule 1
        leftMatch = supportedFor LeftContext innerMatch
        rightMatch = supportedFor RightContext innerMatch
     in do
          mergedMatch <-
            expectRight (mergeSupportedMatch @TestSubstrate emptyTestGraph leftMatch rightMatch)
          expectedSupport <-
            expectRight (supportBasis testContextLattice [LeftContext, RightContext])
          supportedMatchBasis @TestSubstrate mergedMatch
            @?= expectedSupport
          supportedMatchWitnesses @TestSubstrate mergedMatch
            @?= Map.fromList
              [ (LeftContext, IntSet.singleton 1),
                (RightContext, IntSet.singleton 1)
              ]

goalStopsSaturation :: TestTree
goalStopsSaturation =
  testCase "goal stops saturation after becoming true" $ do
    let addRule =
          makeBaseRule
            0
            [1]
            True
            (addClassEffect (IntSet.singleton 2) IntSet.empty IntSet.empty)
        siteProgram = siteProgramWith [addRule] Map.empty [] Map.empty
    (_matchState, saturationReport) <-
      runProgram (SaturationBudget 4 32) defaultSchedulerConfig siteProgram (classCountGoal 2) [1]
    srResult saturationReport @?= ReachedGoal
    graphClassCount @TestSubstrate (srCarrier saturationReport) @?= 2

iterationLimitIsObservable :: TestTree
iterationLimitIsObservable =
  testCase "iteration limit is observable" $ do
    let repeatingRule =
          makeBaseRule
            0
            [1]
            False
            (addClassEffect (IntSet.singleton 2) IntSet.empty IntSet.empty)
        siteProgram = siteProgramWith [repeatingRule] Map.empty [] Map.empty
    (_matchState, saturationReport) <-
      runProgram (SaturationBudget 1 32) defaultSchedulerConfig siteProgram mempty [1]
    srResult saturationReport @?= HitIterationLimit
    reportIterationCount saturationReport @?= 1

nodeLimitIsObservable :: TestTree
nodeLimitIsObservable =
  testCase "node limit is observable" $ do
    let repeatingRule =
          makeBaseRule
            0
            [1]
            False
            (addClassEffect (IntSet.singleton 2) IntSet.empty IntSet.empty)
        siteProgram = siteProgramWith [repeatingRule] Map.empty [] Map.empty
    (_matchState, saturationReport) <-
      runProgram (SaturationBudget 4 1) defaultSchedulerConfig siteProgram mempty [1]
    srResult saturationReport @?= HitNodeLimit
    graphClassCount @TestSubstrate (srCarrier saturationReport) @?= 2
