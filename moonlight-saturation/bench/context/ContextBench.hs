{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module ContextBench
  ( contextBenchmarks,
  )
where

import BenchSupport
import Control.DeepSeq (NFData (..))
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity, runIdentity)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Moonlight.Control.Candidate
import Moonlight.Control.Count (workCountLowerBoundToBoundedInt)
import Moonlight.Control.Schedule
import Moonlight.Control.Schedule.Round
import Moonlight.Core hiding (Plan)
import Moonlight.FiniteLattice (supportGenerators)
import Moonlight.Saturation.Context.Error
import Moonlight.Saturation.Context.Program.Compile
import Moonlight.Saturation.Context.Program.Plan
import Moonlight.Saturation.Context.Program.Source
import Moonlight.Saturation.Context.Program.Spec
import Moonlight.Saturation.Context.Runtime.Carrier.Plain
import Moonlight.Saturation.Context.Runtime.Carrier.Schedule
import Moonlight.Saturation.Context.Runtime.Engine
import Moonlight.Saturation.Context.Runtime.Match.Batch
import Moonlight.Saturation.Context.Runtime.Match.Pipeline
import Moonlight.Saturation.Context.Runtime.Report
import Moonlight.Saturation.Context.Runtime.Schedule.Decision
import Moonlight.Saturation.Core
import Moonlight.Saturation.Substrate
import Moonlight.Saturation.Test.ContextFixture
import Moonlight.Saturation.Test.ContextWorkload
import Test.Tasty.Bench (Benchmark)

data SiteDigest = SiteDigest !PopulationDigest !PopulationDigest
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data ContextProgramDigest
  = ContextProgramDigest !SiteDigest !SiteDigest !SiteDigest !PopulationDigest
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data ContextCompileInput = ContextCompileInput !Int !(Program 'SourceProgramStage TestSubstrate)

data ScheduleProfile
  = AllCandidatesAdmitted
  | MixedAdmissionOutcomes
  deriving stock (Enum, Eq, Ord, Show)

data ScheduleAdmissionRejection
  = BenchmarkAdmissionRejected
  deriving stock (Eq, Ord, Show)

data ContextFixtureObstruction
  = ContextSourceFixtureObstruction !(SaturationSupportError TestSubstrate)
  | ContextCompileFixtureObstruction !(SaturationCompileError TestSubstrate RewriteRuleId)
  deriving stock (Eq, Show)

data ScheduleInput
  = ScheduleInput !ScheduleProfile !TestGraph !(CandidateSpace Identity RewriteRuleId () TestSupportedMatch)

data ScheduleDigest = ScheduleDigest !PopulationDigest ![Int] !Int !PopulationDigest !Bool
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data ContextRuntimeInput = ContextRuntimeInput !(Plan TestSubstrate TestGraph RewriteRuleId) !TestGraph

data ContextRunDigest
  = ContextRunDigest !SaturationTermination !Int !Int !Int !Int !PopulationDigest !PopulationDigest
  deriving stock (Eq, Show)

instance NFData ContextRunDigest where
  rnf (ContextRunDigest termination iterations matches factRounds contexts factDigest graphDigest) =
    termination `seq` rnf (iterations, matches, factRounds, contexts, factDigest, graphDigest)

contextBenchmarks :: Either BenchmarkObstruction Benchmark
contextBenchmarks = do
  _validatedLattice <-
    requireBenchmarkFixture
      ContextBenchmarkLane
      "shared context lattice"
      testContextLatticeValidation
  validatedBenchmarkGroup
    "context"
    [ validatedBenchmarkFamily "compile-source-program" compileBenchmark contextScales,
      validatedBenchmarkFamily "schedule-all-admitted" (scheduleBenchmark AllCandidatesAdmitted) scheduleScales,
      validatedBenchmarkFamily "schedule-mixed-admission" (scheduleBenchmark MixedAdmissionOutcomes) scheduleScales,
      validatedBenchmarkFamily "run-contextual-plan-cold" runtimeBenchmark contextScales
    ]

contextScales :: [Int]
contextScales = [32, 256, 1024]

scheduleScales :: [Int]
scheduleScales = [64, 512, 4096]

compileBenchmark :: Int -> Either BenchmarkObstruction Benchmark
compileBenchmark totalRuleCount = do
  let caseName = benchmarkCaseLabel "total-rules" totalRuleCount
  input <-
    requireBenchmarkFixture
      ContextBenchmarkLane
      caseName
      (contextCompileInput totalRuleCount)
  let ContextCompileInput _ sourceProgram = input
  validatedPureBenchmark
    ContextBenchmarkLane
    caseName
    (Right (contextProgramDigest sourceProgram))
    forceContextCompileInput
    (forceEither rnf)
    compileContextDigest
    input

scheduleBenchmark :: ScheduleProfile -> Int -> Either BenchmarkObstruction Benchmark
scheduleBenchmark profile size =
  let caseName = benchmarkCaseLabel "candidates" size
      input = scheduleInput profile size
   in validatedPureBenchmark
        ContextBenchmarkLane
        caseName
        (expectedScheduleDigest profile size)
        forceScheduleInput
        rnf
        scheduleDigest
        input

runtimeBenchmark :: Int -> Either BenchmarkObstruction Benchmark
runtimeBenchmark size = do
  let caseName = benchmarkCaseLabel "roots" size
  input <-
    requireBenchmarkFixture
      ContextBenchmarkLane
      caseName
      (contextRuntimeInput size)
  validatedPureBenchmark
    ContextBenchmarkLane
    caseName
    (Right (expectedContextRunDigest size))
    forceContextRuntimeInput
    (forceEither rnf)
    runContextDigest
    input

contextCompileInput :: Int -> Either ContextFixtureObstruction ContextCompileInput
contextCompileInput totalRuleCount =
  let rulesPerSite = totalRuleCount `div` 8
      sourceFragment =
        program $ do
          baseRuleIds <- rewrites @TestSubstrate (rewriteRulesFrom 0 rulesPerSite BaseContext)
          void (facts @TestSubstrate (factRulesFrom (4 * rulesPerSite) rulesPerSite))
          activateBaseRewrites baseRuleIds
          traverse_ (\ruleId -> supportBaseRewrite ruleId (principalSupportOf LeftContext)) baseRuleIds
          traverse_ (emitContextSite rulesPerSite) [(LeftContext, 1, 5), (RightContext, 2, 6), (TopContext, 3, 7)]
   in first ContextSourceFixtureObstruction $ do
        sourceProgram <- finishProgram @TestSubstrate sourceFragment
        pure (ContextCompileInput totalRuleCount sourceProgram)
  where
    emitContextSite rulesPerSite (contextValue, rewriteFactor, factFactor) =
      context contextValue $ do
        void (rewrites @TestSubstrate (rewriteRulesFrom (rewriteFactor * rulesPerSite) rulesPerSite contextValue))
        void (facts @TestSubstrate (factRulesFrom (factFactor * rulesPerSite) rulesPerSite))
        activateBaseRewrites (fmap RewriteRuleId [0 .. rulesPerSite - 1])

rewriteRulesFrom :: Int -> Int -> TestContext -> [TestRule]
rewriteRulesFrom start count contextValue =
  [ case contextValue of
      BaseContext -> makeBaseRule ruleKey [ruleKey + 1] True noEffect
      LeftContext -> contextualRule
      RightContext -> contextualRule
      TopContext -> contextualRule
  | ruleKey <- [start .. start + count - 1]
  , let contextualRule = makeContextRule ruleKey contextValue [ruleKey + 1] True noEffect
  ]

factRulesFrom :: Int -> Int -> [TestFactRule]
factRulesFrom start count = [makeFactRule ruleKey (ruleKey + 1) | ruleKey <- [start .. start + count - 1]]

compileContextDigest ::
  ContextCompileInput ->
  Either (SaturationCompileError TestSubstrate RewriteRuleId) ContextProgramDigest
compileContextDigest (ContextCompileInput totalRuleCount sourceProgram) =
  fmap
    (contextProgramDigest . planProgram)
    ( compileSourceProgram @TestSubstrate
        (contextPlanSpec totalRuleCount)
        sourceProgram
    )

contextProgramDigest ::
  SiteProgram TestContext TestRule TestFactRule RewriteRuleId (SupportBasis TestContext) ->
  ContextProgramDigest
contextProgramDigest siteProgram =
  let rewriteIndex = spRewriteRules siteProgram
      factIndex = spFactRules siteProgram
      activationIndex = spRewriteActivation siteProgram
   in ContextProgramDigest
        (siteDigest (rewriteRuleIdKey . trId) rewriteIndex)
        (siteDigest (rewriteRuleIdKey . tfrId) factIndex)
        (activationDigest activationIndex)
        (populationDigest supportEntryChecksum (Map.toAscList (spBaseRewriteSupport siteProgram)))

siteDigest :: (rule -> Int) -> SiteIndex TestContext rule -> SiteDigest
siteDigest checksum index =
  SiteDigest
    (populationDigest checksum (siBase index))
    (contextualDigest checksum (siContexts index))

activationDigest :: MatchActivationIndex TestContext RewriteRuleId -> SiteDigest
activationDigest activation =
  SiteDigest
    (populationDigest rewriteRuleIdKey (maiBase activation))
    (contextualDigest rewriteRuleIdKey (maiContexts activation))

contextualDigest :: Foldable collection => (element -> Int) -> Map.Map TestContext (collection element) -> PopulationDigest
contextualDigest checksum =
  foldMap (\(contextValue, elements) -> PopulationDigest 0 (fromEnum contextValue) <> populationDigest checksum elements) . Map.toAscList

supportEntryChecksum :: (RewriteRuleId, SupportBasis TestContext) -> Int
supportEntryChecksum (ruleId, supportBasis) =
  rewriteRuleIdKey ruleId + supportBasisChecksum supportBasis

supportBasisChecksum :: SupportBasis TestContext -> Int
supportBasisChecksum = sum . fmap fromEnum . supportGenerators

scheduleInput :: ScheduleProfile -> Int -> ScheduleInput
scheduleInput profile size =
  let matches =
        fmap
          (\rootClass -> testSupportedMatch rootClass rootClass BaseContext)
          [1 .. size]
      groupOf = matchRuleKey @TestSubstrate . supportedMatchInner @TestSubstrate
      compareMatches left right =
        compare
          (matchKey @TestSubstrate (supportedMatchInner @TestSubstrate left))
          (matchKey @TestSubstrate (supportedMatchInner @TestSubstrate right))
      candidateSpace =
        candidateSpaceForSupportedMatches
          @TestSubstrate
          groupOf
          compareMatches
          (matchBatchFromList matches)
   in ScheduleInput profile (graphFromClasses [1 .. size]) candidateSpace

scheduleDigest :: ScheduleInput -> ScheduleDigest
scheduleDigest (ScheduleInput profile graph candidateSpace) =
  let decision =
        scheduleGatedSupportedMatches
          @TestSubstrate
          (admissionGateFor profile)
          ()
          benchmarkSchedulerConfig
          (testSaturationRoundView 0 graph)
          emptySchedulerState
          emptyTestMatchState
          candidateSpace
      scheduledMatches = matchBatchToList (rsdScheduledMatches decision)
      pipelineCounts = rsdPipelineCounts decision
      traceEntries = Vector.toList (rsdTraceDelta decision)
   in ScheduleDigest
        (populationDigest (tmRootClass . supportedMatchInner @TestSubstrate) scheduledMatches)
        (fmap (`pipelineCount` pipelineCounts) allPipelineStages)
        (pipelineGroupWeight pipelineCounts)
        (populationDigest scheduleTraceWeight traceEntries)
        (rsdAllCandidatesScheduled decision)

expectedScheduleDigest :: ScheduleProfile -> Int -> ScheduleDigest
expectedScheduleDigest profile size =
  let roots = [1 .. size]
      admittedRoots = filter (admittedBy profile) roots
      deferredCount = length (filter (deferredBy profile) roots)
      admittedCount = length admittedRoots
   in ScheduleDigest
        (PopulationDigest admittedCount (sum admittedRoots))
        (fmap (expectedPipelineCount profile size) allPipelineStages)
        admittedCount
        (PopulationDigest size (sumFromOne size + admittedCount))
        (deferredCount == 0)

allPipelineStages :: [CandidatePipelineStage]
allPipelineStages = [minBound .. maxBound]

expectedPipelineCount :: ScheduleProfile -> Int -> CandidatePipelineStage -> Int
expectedPipelineCount profile size stage =
  case stage of
    CandidateEligibleBase -> 0
    CandidateEligibleContext -> 0
    CandidateEligibleAggregated -> 0
    CandidateDroppedByGuidance -> 0
    CandidateGuided -> size
    CandidateRejectedByAdmission -> countBy (rejectedBy profile)
    CandidateDeferredByBudget -> countBy (deferredBy profile)
    CandidateAdmitted -> admittedCount
    CandidateScheduledBeforeValidation -> admittedCount
    CandidateNotSelectedByScheduler -> 0
    CandidateRejectedByValidation -> 0
    CandidateScheduled -> admittedCount
  where
    countBy predicate = length (filter predicate [1 .. size])
    admittedCount = countBy (admittedBy profile)

admissionGateFor ::
  ScheduleProfile ->
  MatchAdmissionGate TestSubstrate TestSupportedMatch ScheduleAdmissionRejection Int
admissionGateFor profile =
  MatchAdmissionGate
    { magMeasure =
        \_rewriteContext _facts _graph _matchState supportedMatch ->
          let rootClass = tmRootClass (supportedMatchInner @TestSubstrate supportedMatch)
           in if rejectedBy profile rootClass
                then Left BenchmarkAdmissionRejected
                else Right rootClass,
      magFitsRound =
        \_roundView rootClass -> not (deferredBy profile rootClass)
    }

admittedBy :: ScheduleProfile -> Int -> Bool
admittedBy profile rootClass =
  not (rejectedBy profile rootClass) && not (deferredBy profile rootClass)

rejectedBy :: ScheduleProfile -> Int -> Bool
rejectedBy profile rootClass =
  case profile of
    AllCandidatesAdmitted -> False
    MixedAdmissionOutcomes -> rootClass `mod` 3 == 0

deferredBy :: ScheduleProfile -> Int -> Bool
deferredBy profile rootClass =
  case profile of
    AllCandidatesAdmitted -> False
    MixedAdmissionOutcomes -> rootClass `mod` 3 == 1

benchmarkSchedulerConfig :: SchedulerConfig RewriteRuleId
benchmarkSchedulerConfig =
  defaultSchedulerConfig {scTracePolicy = TraceAll}

pipelineCount :: CandidatePipelineStage -> CandidatePipelineCounts RewriteRuleId -> Int
pipelineCount stage =
  fromIntegral . candidatePipelineCount stage

pipelineGroupWeight :: CandidatePipelineCounts RewriteRuleId -> Int
pipelineGroupWeight =
  sum
    . fmap (sum . fmap fromIntegral . Map.elems)
    . Map.elems
    . cpcGroupCounts

scheduleTraceWeight :: ScheduleTrace RewriteRuleId -> Int
scheduleTraceWeight traceEntry =
  rewriteRuleIdKey (strGroup traceEntry)
    + workCountLowerBoundToBoundedInt (strMatchedCount traceEntry)
    + workCountLowerBoundToBoundedInt (strFilteredCount traceEntry)
    + fromIntegral (strScheduledCount traceEntry)

candidateSpaceSetupWeight ::
  CandidateSpace Identity RewriteRuleId () TestSupportedMatch ->
  Int
candidateSpaceSetupWeight candidateSpace =
  length (show summaries)
    + sum (fmap forceGroup groups)
  where
    summaries = runIdentity (csGroupSummaries candidateSpace)
    groups = fmap cgsGroup summaries
    forceGroup :: RewriteRuleId -> Int
    forceGroup group =
      maybe 0 forceCandidateGroup (runIdentity (csLookupGroup candidateSpace group))
    forceCandidateGroup :: CandidateGroup Identity () TestSupportedMatch -> Int
    forceCandidateGroup candidateGroup =
      let availableCount = workCountLowerBoundToBoundedInt (runIdentity (cgAvailableCount candidateGroup))
          pulled =
            runIdentity
              (pullCandidateCursor (runIdentity (cgOpenCursor candidateGroup)) (pullRequest (fromIntegral availableCount)))
       in availableCount
            + length
              ( show
                  ( prMatches pulled,
                    prPulledCount pulled,
                    prRemainingCount pulled,
                    prCoverage pulled,
                    maybe False (const True) (prNextCursor pulled)
                  )
              )

contextRuntimeInput :: Int -> Either ContextFixtureObstruction ContextRuntimeInput
contextRuntimeInput size = do
  sourceProgram <-
    first
      ContextSourceFixtureObstruction
      (finishProgram @TestSubstrate (contextRuntimeFragment size))
  plan <-
    first ContextCompileFixtureObstruction
      ( compileSourceProgram @TestSubstrate
          (contextPlanSpec size)
          sourceProgram
      )
  pure (ContextRuntimeInput plan (graphFromClasses [1 .. size]))

contextRuntimeFragment :: Int -> ProgramFragment TestSubstrate
contextRuntimeFragment size =
  let roots = [1 .. size]
      sharedRule =
        (makeContextRule runtimeRuleKey LeftContext [] True noEffect)
          { trContextRoots = Map.fromList [(contextValue, roots) | contextValue <- [LeftContext, RightContext, TopContext]]
          }
   in program (traverse_ (emitRuntimeContext sharedRule) [(LeftContext, 0), (RightContext, size), (TopContext, 2 * size)])
  where
    emitRuntimeContext :: TestRule -> (TestContext, Int) -> ProgramM TestSubstrate ()
    emitRuntimeContext sharedRule (contextValue, offset) =
      context contextValue $ do
        void (rewrite @TestSubstrate sharedRule)
        void (facts @TestSubstrate (factsFor offset))
    factsFor offset = fmap (\rootClass -> makeFactRule (offset + rootClass) (offset + rootClass)) [1 .. size]

runContextDigest ::
  ContextRuntimeInput ->
  Either (SaturationRunError TestSubstrate) ContextRunDigest
runContextDigest (ContextRuntimeInput plan graph) =
  fmap
    (\(_finalState, report) ->
       let contextFacts = reportContextFacts report
           finalGraph = srCarrier report
        in ContextRunDigest
             (srResult report)
             (reportIterationCount report)
             (reportMatchesApplied report)
             (reportFactRoundCount report)
             (Map.size contextFacts)
             (Map.foldl' (\digest factValues -> digest <> intSetDigest factValues) mempty contextFacts)
             (PopulationDigest (IntMap.size (tgClasses finalGraph)) (testGraphChecksum finalGraph))
    )
    ( runPlanWithPolicyAndGoal
        @TestSubstrate
        (plainRuntimePolicy @TestSubstrate)
        plan
        mempty
        graph
    )

expectedContextRunDigest :: Int -> ContextRunDigest
expectedContextRunDigest size =
  ContextRunDigest
    ReachedFixedPoint
    1
    size
    3
    4
    (PopulationDigest (3 * size) (sumFromOne (3 * size)))
    (PopulationDigest size (3 * sumFromOne size + size + runtimeRuleKey))

testGraphChecksum :: TestGraph -> Int
testGraphChecksum graph =
  IntMap.foldlWithKey' (\total classId members -> total + classId + IntSet.foldl' (+) 0 members) 0 (tgClasses graph)
    + tgNodeCount graph
    + tgPendingMerges graph
    + tgRevision graph
    + tgCapabilityGeneration graph
    + IntSet.foldl' (+) 0 (tgDirtyImpacted graph)
    + IntSet.foldl' (+) 0 (tgDirtyKeys graph)
    + IntSet.foldl' (+) 0 (tgPayload graph)
    + fromEnum (tgUnionChanged graph)
    + IntMap.foldlWithKey'
      (\total ruleKey roots -> total + ruleKey + IntSet.foldl' (+) 0 roots)
      0
      (tgDisabledMatches graph)

intSetDigest :: IntSet.IntSet -> PopulationDigest
intSetDigest values = PopulationDigest (IntSet.size values) (IntSet.foldl' (+) 0 values)

contextPlanSpec :: Int -> PlanSpec TestSubstrate (SatGraph TestSubstrate) RewriteRuleId
contextPlanSpec size =
  withSchedulerConfig
    deterministicSchedulerConfig
    (planSpec (SaturationBudget 4 (size * 2 + 16)) () ())

runtimeRuleKey :: Int
runtimeRuleKey =
  900000

forceContextCompileInput :: ContextCompileInput -> ()
forceContextCompileInput (ContextCompileInput totalRuleCount sourceProgram) =
  rnf totalRuleCount `seq` forceTestProgram sourceProgram

forceScheduleInput :: ScheduleInput -> ()
forceScheduleInput (ScheduleInput profile graph candidateSpace) =
  rnf
    ( fromEnum profile,
      show graph,
      candidateSpaceSetupWeight candidateSpace
    )

forceContextRuntimeInput :: ContextRuntimeInput -> ()
forceContextRuntimeInput (ContextRuntimeInput plan graph) =
  forceTestProgram (planProgram plan) `seq` rnf (testGraphChecksum graph)

forceTestProgram :: SiteProgram TestContext TestRule TestFactRule RewriteRuleId (SupportBasis TestContext) -> ()
forceTestProgram siteProgram =
  let rewriteIndex = spRewriteRules siteProgram
      factIndex = spFactRules siteProgram
      activationIndex = spRewriteActivation siteProgram
   in rnf
        ( show
            ( siBase rewriteIndex,
              siContexts rewriteIndex,
              siBase factIndex,
              siContexts factIndex,
              spSupportedFactRules siteProgram,
              spSupportedRewriteRules siteProgram,
              maiBase activationIndex,
              maiContexts activationIndex,
              spBaseRewriteSupport siteProgram
            )
        )
