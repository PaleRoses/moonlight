{-# LANGUAGE TypeApplications #-}

module Moonlight.Saturation.SchedulerSpec
  ( schedulerTests,
  )
where

import Data.Map.Strict qualified as Map
import Data.Functor.Identity (runIdentity)
import Data.IntSet qualified as IntSet
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Numeric.Natural (Natural)
import Moonlight.Core (RewriteRuleId (..))
import Moonlight.Saturation.Context.Program.Spec (deterministicSchedulerConfig)
import Moonlight.Saturation.Context.Program.View
  ( SaturationRoundView (..),
  )
import Moonlight.Saturation.Context.Runtime.Carrier.Schedule
  ( MatchAdmissionGate (..),
    candidateSpaceForSupportedMatches,
    scheduleGatedSupportedMatches,
    trivialAdmissionGate,
  )
import Moonlight.Saturation.Context.Runtime.Match.Batch
  ( matchBatchFromList,
    matchBatchToList,
  )
import Moonlight.Saturation.Context.Runtime.Match.Pipeline
  ( CandidatePipelineStage (..),
    candidatePipelineCount,
  )
import Moonlight.Saturation.Context.Runtime.Schedule.Decision
  ( RuntimeScheduleDecision (..),
  )
import Moonlight.Control.Schedule
  ( backoffConfig,
    ScheduleOrder (..),
    SchedulerConfig (..),
    TracePolicy (..),
    defaultSchedulerConfig,
  )
import Moonlight.Control.Schedule.Round
  ( ScheduleOutcome (..),
    SchedulerState,
    ScheduleTrace (..),
    emptySchedulerState,
    schedulerCooldowns,
    scheduleCandidateSpace,
  )
import Moonlight.Control.Candidate
  ( finiteCandidateSpace,
    lengthNatural,
    scheduledBatchMatches,
  )
import Moonlight.Control.Count
  ( WorkCount,
    naturalToBoundedInt,
    workCountFromInt,
    workCountLowerBoundToBoundedInt,
  )
import Moonlight.Control.Weight
  ( PriorityEvidence (..),
    comparePriorityEvidence,
    criticalPriorityRank,
    nonCriticalPriorityRank,
  )
import Moonlight.Saturation.Substrate
  ( matchKey,
    matchRuleKey,
    supportedMatchBasis,
    supportedMatchInner,
  )
import Moonlight.Saturation.TestSupport
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

mkSupported :: Int -> Int -> TestContext -> TestSupportedMatch
mkSupported ruleKey rootClass contextValue =
  supportedFor
    contextValue
    (TestMatch (makeBaseRule ruleKey [rootClass] False noEffect) rootClass)

testRoundView :: Int -> TestGraph -> SaturationRoundView TestSubstrate
testRoundView iteration graph =
  SaturationRoundView
    { srvIteration = iteration,
      srvGraph = graph,
      srvBaseGraph = graph,
      srvFacts = IntSet.empty,
      srvFactDerivations = IntSet.empty,
      srvFactsChanged = False,
      srvFactRoundCount = 0,
      srvBaseEligibleMatchCount = 0,
      srvContextEligibleMatchCount = 0,
      srvAggregatedEligibleMatchCount = 0,
      srvContextRevision = 0
    }

schedulerTests :: TestTree
schedulerTests =
  testGroup
    "scheduler"
    [ testCase "deterministic scheduling orders matches by rule id" $
        let matches =
              [ mkSupported 1 10 BaseContext,
                mkSupported 0 11 BaseContext
              ]
            scheduled =
              scheduledBatchMatches . soScheduledBatch $
                scheduleTestMatches
                  deterministicSchedulerConfig
                  (comparing (matchKey @TestSubstrate . supportedMatchInner @TestSubstrate))
                  (matchRuleKey @TestSubstrate . supportedMatchInner @TestSubstrate)
                  0
                  matches
                  emptySchedulerState
         in fmap (matchRuleKey @TestSubstrate . supportedMatchInner @TestSubstrate) scheduled
              @?= [RewriteRuleId 0, RewriteRuleId 1],
      testCase "candidate-space scheduling traces candidate groups without matched-list ghosts" $
        let candidateMatches =
              [ mkSupported 1 11 BaseContext,
                mkSupported 2 12 BaseContext
              ]
            result =
              scheduleTestMatches
                (defaultSchedulerConfig {scTracePolicy = TraceAll})
                (comparing (matchKey @TestSubstrate . supportedMatchInner @TestSubstrate))
                (matchRuleKey @TestSubstrate . supportedMatchInner @TestSubstrate)
                0
                candidateMatches
                emptySchedulerState
            traceCounts :: ScheduleTrace RewriteRuleId -> (RewriteRuleId, WorkCount, WorkCount, Natural)
            traceCounts traceEntry =
              ( strGroup traceEntry,
                strMatchedCount traceEntry,
                strFilteredCount traceEntry,
                strScheduledCount traceEntry
              )
         in fmap traceCounts (soSchedulerTraceDelta result)
              @?= [ (RewriteRuleId 1, workCountFromInt 1, workCountFromInt 0, 1),
                    (RewriteRuleId 2, workCountFromInt 1, workCountFromInt 0, 1)
                  ],
      testCase "backoff scheduling suppresses cooled-down rules across rounds" $
        let matches =
              [ mkSupported 0 10 LeftContext,
                mkSupported 0 11 RightContext
              ]
            config :: SchedulerConfig RewriteRuleId
            config =
              defaultSchedulerConfig
                { scOrder = BackoffByGroup (backoffConfig 1 2),
                  scTracePolicy = TraceAll
                }
            compareMatch = comparing (matchKey @TestSubstrate . supportedMatchInner @TestSubstrate)
            groupOf = matchRuleKey @TestSubstrate . supportedMatchInner @TestSubstrate
            result0 = scheduleTestMatches config compareMatch groupOf 0 matches emptySchedulerState
            round0 = scheduledBatchMatches (soScheduledBatch result0)
            state1 = soSchedulerState result0
            result1 = scheduleTestMatches config compareMatch groupOf 1 matches state1
            round1 = scheduledBatchMatches (soScheduledBatch result1)
            state2 = soSchedulerState result1
            result2 = scheduleTestMatches config compareMatch groupOf 2 matches state2
            round2 = scheduledBatchMatches (soScheduledBatch result2)
            state3 = soSchedulerState result2
            result3 = scheduleTestMatches config compareMatch groupOf 3 matches state3
            round3 = scheduledBatchMatches (soScheduledBatch result3)
         in do
              fmap (tmRootClass . supportedMatchInner @TestSubstrate) round0
                @?= [10]
              fmap (supportedMatchBasis @TestSubstrate) round0
                @?= [principalSupportOf LeftContext]
              round1 @?= []
              round2 @?= []
              fmap (tmRootClass . supportedMatchInner @TestSubstrate) round3
                @?= [10]
              schedulerCooldowns state1 @?= Map.singleton (RewriteRuleId 0) 2
              schedulerCooldowns state2 @?= Map.singleton (RewriteRuleId 0) 1
              schedulerCooldowns state3 @?= Map.empty,
      testCase "backoff scheduling canonicalizes same-rule matches by full match key" $
        let matches =
              [ mkSupported 0 11 RightContext,
                mkSupported 0 10 LeftContext
              ]
            config :: SchedulerConfig RewriteRuleId
            config =
              defaultSchedulerConfig
                { scOrder = BackoffByGroup (backoffConfig 1 2),
                  scTracePolicy = NoTrace
                }
            scheduled =
              scheduledBatchMatches . soScheduledBatch $
                scheduleTestMatches
                  config
                  (comparing (matchKey @TestSubstrate . supportedMatchInner @TestSubstrate))
                  (matchRuleKey @TestSubstrate . supportedMatchInner @TestSubstrate)
                  0
                  matches
                  emptySchedulerState
         in do
              fmap (tmRootClass . supportedMatchInner @TestSubstrate) scheduled
                @?= [10]
              fmap (supportedMatchBasis @TestSubstrate) scheduled
                @?= [principalSupportOf LeftContext],
      testCase "comparePriorityEvidence sorts higher criticality rank before lower" $
        comparePriorityEvidence
          (mempty {peCriticalityRank = criticalPriorityRank})
          (mempty {peCriticalityRank = nonCriticalPriorityRank})
          @?= LT,
      testCase "gated scheduling validates only the scheduler-admitted prefix" $
        let graph =
              graphFromClasses [10, 11]
            staleMatch =
              mkSupported 0 10 BaseContext
            freshMatch =
              mkSupported 0 11 BaseContext
            matches =
              [staleMatch, freshMatch]
            matchState =
              emptyTestMatchState
                { tmsObservedSaturatedMatches =
                    Set.singleton
                      (RewriteRuleId 0, 10, principalSupportOf BaseContext)
                }
            config :: SchedulerConfig RewriteRuleId
            config =
              defaultSchedulerConfig
                { scOrder = BackoffByGroup (backoffConfig 1 2),
                  scTracePolicy = TraceAll
                }
            result =
              scheduleGatedSupportedMatches
                @TestSubstrate
                trivialAdmissionGate
                ()
                config
                (testRoundView 0 graph)
                emptySchedulerState
                matchState
                ( candidateSpaceForSupportedMatches
                    @TestSubstrate
                    (matchRuleKey @TestSubstrate . supportedMatchInner @TestSubstrate)
                    (comparing (matchKey @TestSubstrate . supportedMatchInner @TestSubstrate))
                    (matchBatchFromList matches)
                )
            traceSummary :: ScheduleTrace RewriteRuleId -> (Int, Int, Int, Int)
            traceSummary traceEntry =
              ( workCountLowerBoundToBoundedInt (strMatchedCount traceEntry),
                workCountLowerBoundToBoundedInt (strFilteredCount traceEntry),
                naturalToBoundedInt (strScheduledCount traceEntry),
                workCountLowerBoundToBoundedInt (strSuppressedCount traceEntry)
              )
            pipelineCounts =
              rsdPipelineCounts result
         in do
              matchBatchToList (rsdScheduledMatches result) @?= []
              rsdAllCandidatesScheduled result @?= False
              candidatePipelineCount CandidateGuided pipelineCounts @?= 2
              candidatePipelineCount CandidateAdmitted pipelineCounts @?= 1
              candidatePipelineCount CandidateScheduledBeforeValidation pipelineCounts @?= 1
              candidatePipelineCount CandidateNotSelectedByScheduler pipelineCounts @?= 0
              candidatePipelineCount CandidateRejectedByValidation pipelineCounts @?= 1
              candidatePipelineCount CandidateScheduled pipelineCounts @?= 0
              fmap traceSummary (Vector.toList (rsdTraceDelta result))
                @?= [(0, 1, 0, 0)],
      testCase "gated scheduling records cheap rejection and budget deferral as typed admission outcomes" $
        let graph =
              graphFromClasses [10, 11, 12]
            matches =
              [ mkSupported 0 10 BaseContext,
                mkSupported 0 11 BaseContext,
                mkSupported 0 12 BaseContext
              ]
            gate :: MatchAdmissionGate TestSubstrate TestSupportedMatch String Int
            gate =
              MatchAdmissionGate
                { magMeasure =
                    \_rewriteContext _factStore _graph _matchState supportedMatch ->
                      let rootClass =
                            tmRootClass (supportedMatchInner @TestSubstrate supportedMatch)
                       in if rootClass == 10
                            then Left "cheap-reject"
                            else Right rootClass,
                  magFitsRound =
                    \_roundView rootClass -> rootClass < 12
                }
            result =
              scheduleGatedSupportedMatches
                @TestSubstrate
                gate
                ()
                (defaultSchedulerConfig {scTracePolicy = TraceAll})
                (testRoundView 0 graph)
                emptySchedulerState
                emptyTestMatchState
                ( candidateSpaceForSupportedMatches
                    @TestSubstrate
                    (matchRuleKey @TestSubstrate . supportedMatchInner @TestSubstrate)
                    (comparing (matchKey @TestSubstrate . supportedMatchInner @TestSubstrate))
                    (matchBatchFromList matches)
                )
            traceSummary :: ScheduleTrace RewriteRuleId -> (Int, Int, Int)
            traceSummary traceEntry =
              ( workCountLowerBoundToBoundedInt (strMatchedCount traceEntry),
                workCountLowerBoundToBoundedInt (strFilteredCount traceEntry),
                naturalToBoundedInt (strScheduledCount traceEntry)
              )
            pipelineCounts =
              rsdPipelineCounts result
         in do
              fmap (tmRootClass . supportedMatchInner @TestSubstrate) (matchBatchToList (rsdScheduledMatches result))
                @?= [11]
              rsdAllCandidatesScheduled result @?= False
              candidatePipelineCount CandidateGuided pipelineCounts @?= 3
              candidatePipelineCount CandidateRejectedByAdmission pipelineCounts @?= 1
              candidatePipelineCount CandidateDeferredByBudget pipelineCounts @?= 1
              candidatePipelineCount CandidateAdmitted pipelineCounts @?= 1
              candidatePipelineCount CandidateScheduledBeforeValidation pipelineCounts @?= 1
              candidatePipelineCount CandidateRejectedByValidation pipelineCounts @?= 0
              candidatePipelineCount CandidateScheduled pipelineCounts @?= 1
              fmap traceSummary (Vector.toList (rsdTraceDelta result))
                @?= [(0, 0, 1)]
    ]

scheduleTestMatches ::
  Ord group =>
  SchedulerConfig group ->
  (match -> match -> Ordering) ->
  (match -> group) ->
  Int ->
  [match] ->
  SchedulerState group ->
  ScheduleOutcome group () match
scheduleTestMatches schedulerConfig compareMatch groupOf roundIndex matches schedulerState =
  runIdentity
    ( scheduleCandidateSpace
        schedulerConfig
        (lengthNatural orderedMatches)
        roundIndex
        (finiteCandidateSpace (fmap (\match -> (groupOf match, [match])) orderedMatches))
        schedulerState
    )
  where
    orderedMatches =
      sortBy compareMatch matches
