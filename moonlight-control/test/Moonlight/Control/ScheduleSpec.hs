module Moonlight.Control.ScheduleSpec
  ( tests,
  )
where

import Data.Foldable qualified as Foldable
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (sortBy)
import Data.Map.Strict qualified as Map
import Numeric.Natural (Natural)

import Moonlight.Control.Candidate
  ( CandidateCursor (..),
    CandidateGroup (..),
    CandidateGroupSummary (..),
    CandidateSpace (..),
    PullRequest (..),
    PullResult (..),
    ScheduledBatch (..),
    ScheduledMatch (..),
    finiteCandidateSpace,
    scheduledBatchCount,
    scheduledBatchMatches,
  )
import Moonlight.Control.Count
  ( WorkCount (..),
    WorkCoverage (..),
    workCountAtLeast,
    workCountExact,
    workCountUnknown,
  )
import Moonlight.Control.Schedule
  ( ScheduleOrder (..),
    SchedulerConfig (..),
    TracePolicy (..),
    backoffConfig,
    deficitRoundRobinConfig,
    defaultDeficitRoundRobinConfig,
    defaultSchedulerConfig,
    foldTracePolicy,
    traceLastEntries,
  )
import Moonlight.Control.Schedule.Round
  ( ScheduleOutcome (..),
    ScheduleTrace (..),
    SchedulerState,
    emptySchedulerState,
    orderCandidateGroupSummaries,
    positiveCooldown,
    scheduleCandidateSpace,
    schedulerCooldowns,
    schedulerTrace,
  )
import Moonlight.Control.Laws
  ( LawBundle (..),
    schedulerLaws,
  )
import Moonlight.Control.Weight
  ( lookupPriorityEvidence,
    nonCriticalPriorityRank,
    priorityEvidence,
    priorityEvidenceKey,
    priorityProfileFromList,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "scheduling round semantics"
    [ testCase "scheduled count matches scheduled batch length" testScheduledCountMatchesBatchLength,
      testCase "scheduled count never exceeds budget" testScheduledCountNeverExceedsBudget,
      testCase "NoTrace retains no scheduler trace entries" testNoTraceRetainsNone,
      testCase "TraceLast retains at most requested entries" testTraceLastAtMost,
      testCase "TraceAll retains every emitted trace entry" testTraceAllRetainsAll,
      testCase "scheduler pulls only the scheduled frontier" testSchedulerPullsOnlyScheduledFrontier,
      testCase "backoff cooldown bans a group without opening cursor" testCooldownDoesNotPullSuppressedGroup,
      testCase "zero-candidate group does not generate cooldown trace entry" testZeroCandidateCooldownTrace,
      testCase "deterministic order ignores stale backoff cooldown from prior config" testDeterministicIgnoresStaleBackoff,
      testCase "suppressed count is available without scheduler trace" testSuppressedCountWithoutTrace,
      testCase "unknown residual work count prevents false coverage completion" testUnknownResidualPreventsFalseConvergence,
      testCase "orderCandidateGroupSummaries agrees with reference sortBy/lookupPriorityEvidence" testOrderingAgreesWithReference,
      testCase "cached scheduler index preserves priority order on repeated frontier" testCachedIndexPreservesPriorityOrder,
      testCase "DRR carries deficit across a truncated round" testDeficitCarriesAcrossTruncation,
      testCase "budget-1 DRR rotation serves every stable group" testBudgetOneRotation,
      testCase "DRR service follows evidence-derived quantum ratios" testWeightedServiceRatio,
      testCase "AtLeast and Unknown counts do not cap DRR requests" testInexactCountsDoNotCapDrr,
      testCase "absolute bans preserve the c=2 round boundary" testAbsoluteBanBoundary,
      testCase "AIMD doubles, caps, recovers, and resets" testBackoffAimd,
      testCase "short cursor results do not install multiplicative backoff" testShortCursorDoesNotBackoff,
      testCase "schedule-order switches discard incompatible state" testModeSwitchDiscardsState,
      testCase "positiveCooldown returns Nothing for non-positive" testPositiveCooldown,
      testCase "foldTracePolicy dispatches to correct case" testFoldTracePolicy,
      lawBundleTree
        ( schedulerLaws
            (fmap (\(AScheduleOrder scheduleOrder) -> scheduleOrder) QC.arbitrary)
        ),
      QC.testProperty "orderCandidateGroupSummaries agrees with reference" prop_orderingAgreesWithReference
    ]

candidateSpaceInt :: [(Int, [Int])] -> CandidateSpace Identity Int () Int
candidateSpaceInt = finiteCandidateSpace

runScheduleInt ::
  ScheduleOrder ->
  TracePolicy ->
  Natural ->
  [(Int, [Int])] ->
  SchedulerState Int ->
  ScheduleOutcome Int () Int
runScheduleInt scheduleOrder tracePolicy budget groups state =
  runConfiguredSchedule
    defaultSchedulerConfig
      { scOrder = scheduleOrder,
        scTracePolicy = tracePolicy
      }
    budget
    0
    groups
    state

runConfiguredSchedule ::
  SchedulerConfig Int ->
  Natural ->
  Int ->
  [(Int, [Int])] ->
  SchedulerState Int ->
  ScheduleOutcome Int () Int
runConfiguredSchedule schedulerConfig budget roundIndex groups state =
  runIdentity
    ( scheduleCandidateSpace
        schedulerConfig
        budget
        roundIndex
        (candidateSpaceInt groups)
        state
    )

testScheduledCountMatchesBatchLength :: Assertion
testScheduledCountMatchesBatchLength = do
  let outcome = runScheduleInt ByRuleIdThenSubstitution NoTrace 10 [(1, [1, 2, 3])] emptySchedulerState
  soScheduledCount outcome @?= scheduledBatchCount (soScheduledBatch outcome)

testScheduledCountNeverExceedsBudget :: Assertion
testScheduledCountNeverExceedsBudget = do
  let outcome = runScheduleInt ByRuleIdThenSubstitution NoTrace 2 [(1, [1, 2, 3, 4, 5])] emptySchedulerState
  assertBool "scheduled count should not exceed budget" (soScheduledCount outcome <= 2)

testNoTraceRetainsNone :: Assertion
testNoTraceRetainsNone = do
  let outcome = runScheduleInt ByRuleIdThenSubstitution NoTrace 10 [(1, [1, 2])] emptySchedulerState
  schedulerTrace (soSchedulerState outcome) @?= []
  soSchedulerTraceDelta outcome @?= []

testTraceLastAtMost :: Assertion
testTraceLastAtMost = do
  let config =
        defaultSchedulerConfig
          { scOrder = ByRuleIdThenSubstitution,
            scTracePolicy = traceLastEntries 1
          }
      groups = [(1 :: Int, [1 :: Int, 2]), (2, [3, 4])]
      first =
        runIdentity (scheduleCandidateSpace config 10 0 (candidateSpaceInt groups) emptySchedulerState)
      second =
        runIdentity (scheduleCandidateSpace config 10 1 (candidateSpaceInt groups) (soSchedulerState first))
  assertBool "TraceLast 1 should retain at most 1 entry" (length (schedulerTrace (soSchedulerState second)) <= 1)

testTraceAllRetainsAll :: Assertion
testTraceAllRetainsAll = do
  let outcome = runScheduleInt ByRuleIdThenSubstitution TraceAll 10 [(1, [1, 2])] emptySchedulerState
  schedulerTrace (soSchedulerState outcome) @?= soSchedulerTraceDelta outcome

testSchedulerPullsOnlyScheduledFrontier :: Assertion
testSchedulerPullsOnlyScheduledFrontier = do
  let space :: CandidateSpace Identity String () Int
      space =
        CandidateSpace
          { csGroupSummaries =
              pure
                [ CandidateGroupSummary
                    { cgsGroup = "g",
                      cgsAvailableCount = workCountAtLeast 1000000
                    }
                ],
            csLookupGroup = \_ -> pure (Just (infiniteGroupIdentity 0))
          }
      outcome :: ScheduleOutcome String () Int
      outcome =
        runIdentity
          ( scheduleCandidateSpace
              defaultSchedulerConfig { scOrder = BackoffByGroup (backoffConfig 64 2) }
              64
              0
              space
              emptySchedulerState
          )
  soScheduledCount outcome @?= 64
  scheduledBatchCount (soScheduledBatch outcome) @?= 64

infiniteGroupIdentity :: Int -> CandidateGroup Identity () Int
infiniteGroupIdentity start =
  CandidateGroup
    { cgAvailableCount = pure (workCountAtLeast 1000000),
      cgOpenCursor = pure (infiniteCursorFrom start)
    }

infiniteCursorFrom :: Int -> CandidateCursor Identity () Int
infiniteCursorFrom n =
  CandidateCursor $ \req ->
    let limit = fromIntegral (pullRequestLimit req)
        pulled = take limit [n ..]
        pulledCount = length pulled
     in pure
          PullResult
            { prMatches = pulled,
              prPulledCount = fromIntegral pulledCount,
              prMeta = (),
              prRemainingCount = workCountUnknown,
              prCoverage = WorkCoverageUnknown,
              prNextCursor = Just (infiniteCursorFrom (n + pulledCount))
            }

testCooldownDoesNotPullSuppressedGroup :: Assertion
testCooldownDoesNotPullSuppressedGroup = do
  let firstRound :: ScheduleOutcome String () Int
      firstRound =
        runIdentity
          ( scheduleCandidateSpace
              defaultSchedulerConfig { scOrder = BackoffByGroup (backoffConfig 1 2) }
              64
              0
              (finiteCandidateSpace [("alpha", [1 :: Int, 2])])
              emptySchedulerState
          )
  openRef <- newIORef (0 :: Int)
  secondRound <-
    scheduleCandidateSpace
      defaultSchedulerConfig { scOrder = BackoffByGroup (backoffConfig 1 2) }
      64
      1
      (countedSpace openRef)
      (soSchedulerState firstRound)
  soScheduledCount secondRound @?= 0
  opened <- readIORef openRef
  opened @?= 0
  where
    countedSpace :: IORef Int -> CandidateSpace IO String () Int
    countedSpace ref =
      CandidateSpace
        { csGroupSummaries =
            pure
              [ CandidateGroupSummary
                  { cgsGroup = "alpha",
                    cgsAvailableCount = workCountAtLeast 100
                  }
              ],
          csLookupGroup = \_ -> do
            modifyIORef' ref (+ 1)
            pure
              ( Just
                  CandidateGroup
                    { cgAvailableCount = pure (workCountExact 10),
                      cgOpenCursor =
                        pure
                          ( CandidateCursor $ \req ->
                              let limit = fromIntegral (pullRequestLimit req)
                                  ms = take limit [1 :: Int .. 10]
                               in pure
                                    PullResult
                                      { prMatches = ms,
                                        prPulledCount = fromIntegral (length ms),
                                        prMeta = (),
                                        prRemainingCount = workCountExact (10 - fromIntegral (length ms)),
                                        prCoverage = WorkCoveragePartial,
                                        prNextCursor = Nothing
                                      }
                          )
                    }
              )
        }

testZeroCandidateCooldownTrace :: Assertion
testZeroCandidateCooldownTrace = do
  let config =
        defaultSchedulerConfig
          { scOrder = BackoffByGroup (backoffConfig 1 2),
            scTracePolicy = TraceAll
          }
      firstRound :: ScheduleOutcome String () Int
      firstRound =
        runIdentity
          ( scheduleCandidateSpace config 64 0
              (finiteCandidateSpace [("alpha", [1 :: Int, 2])])
              emptySchedulerState
          )
      secondRound :: ScheduleOutcome String () Int
      secondRound =
        runIdentity
          ( scheduleCandidateSpace config 64 1
              (finiteCandidateSpace [("alpha", [] :: [Int])])
              (soSchedulerState firstRound)
          )
  schedulerCooldowns (soSchedulerState firstRound) @?= Map.singleton "alpha" 2
  schedulerCooldowns (soSchedulerState secondRound) @?= Map.singleton "alpha" 1
  soSchedulerTraceDelta secondRound @?= []

testDeterministicIgnoresStaleBackoff :: Assertion
testDeterministicIgnoresStaleBackoff = do
  let backoffRound :: ScheduleOutcome String () Int
      backoffRound =
        runIdentity
          ( scheduleCandidateSpace
              defaultSchedulerConfig { scOrder = BackoffByGroup (backoffConfig 1 2) }
              64
              0
              (finiteCandidateSpace [("alpha", [1 :: Int, 2])])
              emptySchedulerState
          )
      deterministicRound :: ScheduleOutcome String () Int
      deterministicRound =
        runIdentity
          ( scheduleCandidateSpace
              defaultSchedulerConfig { scTracePolicy = TraceAll }
              64
              1
              (finiteCandidateSpace [("alpha", [3 :: Int])])
              (soSchedulerState backoffRound)
          )
  scheduledBatchMatches (soScheduledBatch deterministicRound) @?= [3]
  schedulerCooldowns (soSchedulerState deterministicRound) @?= Map.empty

testSuppressedCountWithoutTrace :: Assertion
testSuppressedCountWithoutTrace = do
  let outcome :: ScheduleOutcome String () Int
      outcome =
        runIdentity
          ( scheduleCandidateSpace
              defaultSchedulerConfig
                { scOrder = BackoffByGroup (backoffConfig 1 2),
                  scTracePolicy = NoTrace
                }
              64
              0
              (finiteCandidateSpace [("alpha", [1 :: Int, 2])])
              emptySchedulerState
          )
  soScheduledCount outcome @?= 1
  soSuppressedCount outcome @?= workCountExact 1
  scheduledBatchMatches (soScheduledBatch outcome) @?= [1]
  soSchedulerTraceDelta outcome @?= []

testUnknownResidualPreventsFalseConvergence :: Assertion
testUnknownResidualPreventsFalseConvergence = do
  let outcome :: ScheduleOutcome String () Int
      outcome =
        runIdentity
          ( scheduleCandidateSpace
              defaultSchedulerConfig
              10
              0
              unknownCoverageSpace
              emptySchedulerState
          )
  soCoverage outcome @?= WorkCoverageUnknown
  where
    unknownCoverageSpace :: CandidateSpace Identity String () Int
    unknownCoverageSpace =
      CandidateSpace
        { csGroupSummaries =
            pure
              [ CandidateGroupSummary
                  { cgsGroup = "u",
                    cgsAvailableCount = WorkCountUnknown
                  }
              ],
          csLookupGroup = \_ ->
            pure
              ( Just
                  CandidateGroup
                    { cgAvailableCount = pure WorkCountUnknown,
                      cgOpenCursor =
                        pure
                          ( CandidateCursor $ \_ ->
                              pure
                                PullResult
                                  { prMatches = [1 :: Int],
                                    prPulledCount = 1,
                                    prMeta = (),
                                    prRemainingCount = WorkCountUnknown,
                                    prCoverage = WorkCoverageUnknown,
                                    prNextCursor = Nothing
                                  }
                          )
                    }
              )
        }

testOrderingAgreesWithReference :: Assertion
testOrderingAgreesWithReference = do
  let profile =
        priorityProfileFromList
          [ (1 :: Int, priorityEvidence 0 3 0 nonCriticalPriorityRank),
            (2, priorityEvidence 0 1 0 nonCriticalPriorityRank),
            (3, priorityEvidence 0 2 0 nonCriticalPriorityRank)
          ]
      config = defaultSchedulerConfig { scPriorityProfile = profile }
      summaries =
        [ CandidateGroupSummary {cgsGroup = g, cgsAvailableCount = workCountExact 1}
        | g <- [1 :: Int, 2, 3]
        ]
      ordered = orderCandidateGroupSummaries config summaries
      reference =
        sortBy
          ( \a b ->
              compare
                (priorityEvidenceKey (lookupPriorityEvidence (cgsGroup a) profile))
                (priorityEvidenceKey (lookupPriorityEvidence (cgsGroup b) profile))
                <> compare (cgsGroup a) (cgsGroup b)
          )
          summaries
  fmap cgsGroup ordered @?= fmap cgsGroup reference

testCachedIndexPreservesPriorityOrder :: Assertion
testCachedIndexPreservesPriorityOrder = do
  let profile =
        priorityProfileFromList
          [ ("slow" :: String, priorityEvidence 0 1 0 nonCriticalPriorityRank),
            ("hot", priorityEvidence 0 5 0 nonCriticalPriorityRank),
            ("middle", priorityEvidence 0 3 0 nonCriticalPriorityRank)
          ]
      config =
        defaultSchedulerConfig
          { scPriorityProfile = profile,
            scTracePolicy = TraceAll
          }
      groups =
        [ ("slow", [1 :: Int]),
          ("middle", [2]),
          ("hot", [3])
        ]
      first :: ScheduleOutcome String () Int
      first =
        runIdentity (scheduleCandidateSpace config 64 0 (finiteCandidateSpace groups) emptySchedulerState)
      second :: ScheduleOutcome String () Int
      second =
        runIdentity (scheduleCandidateSpace config 64 1 (finiteCandidateSpace groups) (soSchedulerState first))
  fmap strGroup (soSchedulerTraceDelta second) @?= ["hot", "middle", "slow"]

testDeficitCarriesAcrossTruncation :: Assertion
testDeficitCarriesAcrossTruncation = do
  let config =
        defaultSchedulerConfig
          { scOrder = DeficitRoundRobin defaultDeficitRoundRobinConfig,
            scTracePolicy = TraceAll
          }
      groups =
        [ (1 :: Int, [11, 12, 13]),
          (2, [21, 22, 23])
        ]
      first = runConfiguredSchedule config 1 0 groups emptySchedulerState
      second = runConfiguredSchedule config 2 1 groups (soSchedulerState first)
  scheduledGroups second @?= [2, 2]

testBudgetOneRotation :: Assertion
testBudgetOneRotation = do
  let config =
        defaultSchedulerConfig
          { scOrder = DeficitRoundRobin defaultDeficitRoundRobinConfig
          }
      finalRun =
        runConfiguredRounds
          config
          1
          [(1 :: Int, [1, 1]), (2, [2, 2]), (3, [3, 3])]
          [0, 1, 2]
  scrScheduledGroups finalRun @?= [1, 2, 3]

testWeightedServiceRatio :: Assertion
testWeightedServiceRatio = do
  let profile =
        priorityProfileFromList
          [ (2 :: Int, priorityEvidence 0 1 0 nonCriticalPriorityRank)
          ]
      config =
        defaultSchedulerConfig
          { scOrder = DeficitRoundRobin defaultDeficitRoundRobinConfig,
            scPriorityProfile = profile
          }
      outcome =
        runConfiguredSchedule
          config
          6
          0
          [(1, replicate 8 1), (2, replicate 8 2)]
          emptySchedulerState
  scheduledGroups outcome @?= [2, 2, 2, 2, 2, 1]

testInexactCountsDoNotCapDrr :: Assertion
testInexactCountsDoNotCapDrr = do
  let profile =
        priorityProfileFromList
          [ ("lane" :: String, priorityEvidence 0 1 0 nonCriticalPriorityRank)
          ]
      config =
        defaultSchedulerConfig
          { scOrder = DeficitRoundRobin defaultDeficitRoundRobinConfig,
            scPriorityProfile = profile
          }
      runWithCount availableCount =
        runIdentity
          ( scheduleCandidateSpace
              config
              5
              0
              (inexactCountSpace availableCount)
              emptySchedulerState
          )
  soScheduledCount (runWithCount (workCountAtLeast 1)) @?= 5
  soScheduledCount (runWithCount WorkCountUnknown) @?= 5

testAbsoluteBanBoundary :: Assertion
testAbsoluteBanBoundary = do
  let config =
        defaultSchedulerConfig
          { scOrder = BackoffByGroup (backoffConfig 1 2),
            scTracePolicy = TraceAll
          }
      scheduleRun =
        runConfiguredRounds
          config
          64
          [(1 :: Int, [1, 2])]
          [0, 1, 2, 3]
  scrScheduledCounts scheduleRun @?= [1, 0, 0, 1]
  scrCooldownUntil scheduleRun @?= [Just 3, Just 3, Just 3, Just 8]

testBackoffAimd :: Assertion
testBackoffAimd = do
  let config =
        defaultSchedulerConfig
          { scOrder = BackoffByGroup (backoffConfig 2 2),
            scTracePolicy = TraceAll
          }
      cappedRun =
        Foldable.foldl'
          (advanceBackoffLimitRun config)
          BackoffLimitRun
            { blrState = emptySchedulerState,
              blrBannedUntil = []
            }
          [0, 3, 8, 17, 34, 67, 132]
      recoverySeed =
        Foldable.foldl'
          (advanceBackoffLimitRun config)
          BackoffLimitRun
            { blrState = emptySchedulerState,
              blrBannedUntil = []
            }
          [0, 3, 8]
      recovered =
        runIdentity
          ( scheduleCandidateSpace
              config
              64
              17
              shortBackoffSpace
              (blrState recoverySeed)
          )
      relimited =
        runIdentity
          ( scheduleCandidateSpace
              config
              64
              18
              backoffLimitSpace
              (soSchedulerState recovered)
          )
      drained =
        runIdentity
          ( scheduleCandidateSpace
              config
              64
              33
              (finiteCandidateSpace [("lane", [1 :: Int])] :: CandidateSpace Identity String () Int)
              (soSchedulerState relimited)
          )
      resetLimit =
        runIdentity
          ( scheduleCandidateSpace
              config
              64
              34
              backoffLimitSpace
              (soSchedulerState drained)
          )
  blrBannedUntil cappedRun @?= [3, 8, 17, 34, 67, 132, 197]
  fmap strCooldownUntil (soSchedulerTraceDelta recovered) @?= [Nothing]
  fmap strCooldownUntil (soSchedulerTraceDelta relimited) @?= [Just 33]
  fmap strCooldownUntil (soSchedulerTraceDelta resetLimit) @?= [Just 37]

testShortCursorDoesNotBackoff :: Assertion
testShortCursorDoesNotBackoff = do
  let config =
        defaultSchedulerConfig
          { scOrder = BackoffByGroup (backoffConfig 2 2),
            scTracePolicy = TraceAll
          }
      outcome =
        runIdentity
          ( scheduleCandidateSpace
              config
              64
              0
              shortBackoffSpace
              emptySchedulerState
          )
  soScheduledCount outcome @?= 1
  schedulerCooldowns (soSchedulerState outcome) @?= Map.empty
  fmap strCooldownUntil (soSchedulerTraceDelta outcome) @?= [Nothing]

testModeSwitchDiscardsState :: Assertion
testModeSwitchDiscardsState = do
  let backoffConfigValue =
        defaultSchedulerConfig
          { scOrder = BackoffByGroup (backoffConfig 1 2)
          }
      firstBackoff =
        runConfiguredSchedule
          backoffConfigValue
          64
          0
          [(1 :: Int, [1, 2])]
          emptySchedulerState
      ruleRound =
        runConfiguredSchedule
          defaultSchedulerConfig
          64
          1
          [(1 :: Int, [3])]
          (soSchedulerState firstBackoff)
      resumedBackoff =
        runConfiguredSchedule
          backoffConfigValue
          64
          2
          [(1 :: Int, [4, 5])]
          (soSchedulerState ruleRound)
      deficitConfig =
        defaultSchedulerConfig
          { scOrder = DeficitRoundRobin defaultDeficitRoundRobinConfig
          }
      firstDeficit =
        runConfiguredSchedule
          deficitConfig
          1
          0
          [(1 :: Int, [1, 1]), (2, [2, 2])]
          emptySchedulerState
      interveningRule =
        runConfiguredSchedule
          defaultSchedulerConfig
          0
          1
          []
          (soSchedulerState firstDeficit)
      resumedDeficit =
        runConfiguredSchedule
          deficitConfig
          1
          2
          [(1 :: Int, [1, 1]), (2, [2, 2])]
          (soSchedulerState interveningRule)
  scheduledGroups resumedBackoff @?= [1]
  scheduledGroups resumedDeficit @?= [1]

scheduledGroups :: ScheduleOutcome group meta match -> [group]
scheduledGroups =
  fmap smGroup . scheduledBatchMatchesWithGroups . soScheduledBatch

data ScheduleConfiguredRun = ScheduleConfiguredRun
  { scrState :: !(SchedulerState Int),
    scrScheduledGroups :: ![Int],
    scrScheduledCounts :: ![Natural],
    scrCooldownUntil :: ![Maybe Int]
  }

runConfiguredRounds ::
  SchedulerConfig Int ->
  Natural ->
  [(Int, [Int])] ->
  [Int] ->
  ScheduleConfiguredRun
runConfiguredRounds schedulerConfig budget groups =
  Foldable.foldl' advanceRound initialRun
  where
    initialRun =
      ScheduleConfiguredRun
        { scrState = emptySchedulerState,
          scrScheduledGroups = [],
          scrScheduledCounts = [],
          scrCooldownUntil = []
        }
    advanceRound scheduleRun roundIndex =
      let outcome =
            runConfiguredSchedule
              schedulerConfig
              budget
              roundIndex
              groups
              (scrState scheduleRun)
       in ScheduleConfiguredRun
            { scrState = soSchedulerState outcome,
              scrScheduledGroups = scrScheduledGroups scheduleRun <> scheduledGroups outcome,
              scrScheduledCounts = scrScheduledCounts scheduleRun <> [soScheduledCount outcome],
              scrCooldownUntil =
                scrCooldownUntil scheduleRun <> fmap strCooldownUntil (soSchedulerTraceDelta outcome)
            }

inexactCountSpace :: WorkCount -> CandidateSpace Identity String () Int
inexactCountSpace availableCount =
  CandidateSpace
    { csGroupSummaries =
        pure
          [ CandidateGroupSummary
              { cgsGroup = "lane",
                cgsAvailableCount = availableCount
              }
          ],
      csLookupGroup = \_ -> pure (Just (infiniteGroupIdentity 0))
    }

backoffLimitSpace :: CandidateSpace Identity String () Int
backoffLimitSpace =
  inexactCountSpace (workCountAtLeast 100)

shortBackoffSpace :: CandidateSpace Identity String () Int
shortBackoffSpace =
  CandidateSpace
    { csGroupSummaries =
        pure
          [ CandidateGroupSummary
              { cgsGroup = "lane",
                cgsAvailableCount = workCountAtLeast 100
              }
          ],
      csLookupGroup =
        \_ ->
          pure
            ( Just
                CandidateGroup
                  { cgAvailableCount = pure (workCountAtLeast 100),
                    cgOpenCursor =
                      pure
                        ( CandidateCursor $ \_ ->
                            pure
                              PullResult
                                { prMatches = [1],
                                  prPulledCount = 1,
                                  prMeta = (),
                                  prRemainingCount = workCountAtLeast 99,
                                  prCoverage = WorkCoveragePartial,
                                  prNextCursor = Nothing
                                }
                        )
                  }
            )
    }

data BackoffLimitRun = BackoffLimitRun
  { blrState :: !(SchedulerState String),
    blrBannedUntil :: ![Int]
  }

advanceBackoffLimitRun ::
  SchedulerConfig String ->
  BackoffLimitRun ->
  Int ->
  BackoffLimitRun
advanceBackoffLimitRun schedulerConfig backoffRun roundIndex =
  let outcome =
        runIdentity
          ( scheduleCandidateSpace
              schedulerConfig
              64
              roundIndex
              backoffLimitSpace
              (blrState backoffRun)
          )
      installedBans =
        [ bannedUntil
        | traceEntry <- soSchedulerTraceDelta outcome,
          Just bannedUntil <- [strCooldownUntil traceEntry]
        ]
   in BackoffLimitRun
        { blrState = soSchedulerState outcome,
          blrBannedUntil = blrBannedUntil backoffRun <> installedBans
        }

lawBundleTree :: LawBundle -> TestTree
lawBundleTree lawBundle =
  testGroup
    (lawBundleName lawBundle)
    (fmap (uncurry QC.testProperty) (lawBundleProperties lawBundle))

testPositiveCooldown :: Assertion
testPositiveCooldown = do
  positiveCooldown 0 @?= Nothing
  positiveCooldown (-1) @?= Nothing
  positiveCooldown 1 @?= Just 1
  positiveCooldown 3 @?= Just 3

testFoldTracePolicy :: Assertion
testFoldTracePolicy = do
  foldTracePolicy "no" (const "last") "all" NoTrace @?= "no"
  foldTracePolicy "no" show "all" (traceLastEntries 5) @?= "5"
  foldTracePolicy "no" (const "last") "all" TraceAll @?= "all"

newtype AScheduleOrder = AScheduleOrder ScheduleOrder
  deriving stock (Show)

newtype ACandidateGroups = ACandidateGroups [(Int, [Int])]
  deriving stock (Show)

instance QC.Arbitrary AScheduleOrder where
  arbitrary =
    QC.frequency
      [ (4, pure (AScheduleOrder ByRuleIdThenSubstitution)),
        ( 2,
          fmap
            (\(m, c) -> AScheduleOrder (BackoffByGroup (backoffConfig m c)))
            ((,) <$> QC.chooseInt (1, 6) <*> QC.chooseInt (0, 4))
        ),
        ( 2,
          fmap
            (\(b, q, c) -> AScheduleOrder (DeficitRoundRobin (deficitRoundRobinConfig b q c)))
            ((,,) <$> QC.chooseInt (1, 4) <*> QC.chooseInt (1, 12) <*> QC.chooseInt (1, 6))
        )
      ]
  shrink _ = []

instance QC.Arbitrary ACandidateGroups where
  arbitrary = do
    n <- QC.chooseInt (0, 8)
    groups <-
      QC.vectorOf
        n
        ( (,)
            <$> QC.chooseInt (-4, 4)
            <*> (QC.chooseInt (0, 8) >>= \k -> QC.vectorOf k (QC.chooseInt (-50, 50)))
        )
    pure (ACandidateGroups groups)
  shrink _ = []

prop_orderingAgreesWithReference :: ACandidateGroups -> QC.Property
prop_orderingAgreesWithReference (ACandidateGroups groups) =
  let profile =
        priorityProfileFromList
          [ (g, priorityEvidence 0 (fromIntegral (abs g)) 0 nonCriticalPriorityRank)
          | (g, _) <- groups
          ]
      config = defaultSchedulerConfig {scPriorityProfile = profile}
      summaries =
        [ CandidateGroupSummary
            { cgsGroup = g,
              cgsAvailableCount = workCountExact (fromIntegral (length ms))
            }
        | (g, ms) <- groups
        ]
      ordered = orderCandidateGroupSummaries config summaries
      reference =
        sortBy
          ( \a b ->
              compare
                (priorityEvidenceKey (lookupPriorityEvidence (cgsGroup a) profile))
                (priorityEvidenceKey (lookupPriorityEvidence (cgsGroup b) profile))
                <> compare (cgsGroup a) (cgsGroup b)
          )
          summaries
   in fmap cgsGroup ordered QC.=== fmap cgsGroup reference
