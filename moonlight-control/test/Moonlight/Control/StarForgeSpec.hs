module Moonlight.Control.StarForgeSpec
  ( tests,
  )
where

import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Numeric.Natural
  ( Natural,
  )

import Moonlight.Control.Candidate
  ( CandidateSpace,
    ScheduledBatch,
    finiteCandidateSpace,
    scheduledBatchMatches,
  )
import Moonlight.Control.Class
  ( phase,
  )
import Moonlight.Control.Engine.Plan
  ( Plan (..),
    canonicalRoundBudget,
    phaseDecl,
  )
import Moonlight.Control.Engine.Report
  ( EngineReport (..),
    EngineRound (..),
    Observation (..),
    StopReason (..),
  )
import Moonlight.Control.Engine.Run
  ( runEngine,
  )
import Moonlight.Control.Engine.Spec
  ( Validated,
    compilePlan,
  )
import Moonlight.Control.Engine.Work
  ( ApplyResult (..),
    WorkSource (..),
    applyResult,
  )
import Moonlight.Control.Engine.Parallel
  ( MatchExecution (..),
  )
import Moonlight.Control.Gate
  ( Gate (..),
    filterGroupSelectorWithTrace,
  )
import Moonlight.Control.Modality
  ( gated,
  )
import Moonlight.Control.Schedule
  ( ScheduleGroup (..),
  )
import Moonlight.Control.StarForge.Engine
  ( forgeParallelExecution,
    runStarForgeCampaign,
  )
import Moonlight.Control.StarForge.Model
  ( Constellation (..),
    Curse (..),
    ForgeError,
    ForgeEvidence (..),
    ForgeExecutionBatch (..),
    ForgeGateTrace (..),
    ForgeGroup,
    ForgeLane (..),
    ForgeMatch (..),
    ForgePhase (..),
    ForgeReport,
    ForgeState (..),
    ForgeSupport (..),
    Fragment (..),
    forgeTargetHeat,
  )
import Moonlight.Control.StarForge.Plan
  ( forgePriorityObservation,
    validatedForgeSpec,
  )
import Moonlight.Control.StarForge.Render
  ( emptyLaneRoundSummary,
    findReportRound,
    laneRoundSummaries,
    lrsApplied,
    lrsBackoffInstalled,
    lrsCooldownSuppressed,
    lrsScheduled,
    programTraceContainsRepeat,
    programTraceContainsTrySkipped,
    renderStarForgeCampaign,
    reportExecutionBatches,
  )
import Moonlight.Control.Weight
  ( criticalPriorityRank,
    lookupPriorityEvidence,
    priorityEvidence,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
    (@?=),
  )

tests :: TestTree
tests =
  testGroup
    "star forge conductor"
    [ testCase "conducts a guided scheduled speculative adaptive parallel engine campaign" testStarForgeCampaign,
      testCase "gated program equals pre-composing gate into candidate-space filtering" testGatedEquivalence
    ]

testStarForgeCampaign :: Assertion
testStarForgeCampaign =
  case validatedForgeSpec of
    Left errors ->
      assertFailure ("unexpected StarForge spec validation failure: " <> show (NonEmpty.toList errors))
    Right spec -> do
      parallelResult <-
        runStarForgeCampaign forgeParallelExecution spec
      sequentialResult <-
        runStarForgeCampaign SequentialMatches spec

      report <-
        expectRight parallelResult
      sequentialReport <-
        expectRight sequentialResult

      assertParallelSequentialAgreement sequentialReport report
      assertParallelWorkWasScheduled report

      let finalState =
            erFinalState report
          rounds =
            erRounds report

      fsConstellations finalState
        @?= Set.fromList [WolfStar, GlassCrown]

      assertBool
        "eclipse shadow must be rolled back with the skipped attempt branch"
        (Set.notMember EclipseShadow (fsCurses finalState))

      fsHeat finalState @?= forgeTargetHeat
      erStopReason report @?= Converged

      assertBool
        "expected a TryTrace TrySkipped witness in the program trace"
        (programTraceContainsTrySkipped (erProgramTrace report))

      assertBool
        "expected a RepeatTrace witness in the program trace"
        (programTraceContainsRepeat (erProgramTrace report))

      assertCommittedLaneRound 0 StabilizeFragment 3 3 rounds
      assertCommittedLaneRound 1 FusePair 2 2 rounds
      assertBackoffInstalled 1 FusePair rounds
      assertCommittedLaneRound 2 BreakCurse 1 1 rounds
      assertCooldownSuppressed 3 FusePair rounds
      assertCommittedLaneRound 3 CoolForge 1 1 rounds
      assertGateTraceRetained rounds

      assertDynamicPriorityAfterFusion rounds
      assertDynamicPriorityAfterCurseBreak rounds

      renderStarForgeCampaign report
        @?= [ "round 0: StabilizeFragment scheduled 3 / applied 3",
              "round 1: FusePair scheduled 2 / applied 2, backoff installed",
              "round 2: InvokeEclipse tried, skipped, state rolled back",
              "round 3: BreakCurse scheduled 1 / applied 1",
              "round 4: FusePair suppressed by cooldown; CoolForge scheduled 1 / applied 1",
              "final: converged: 2 constellations forged"
            ]

testGatedEquivalence :: Assertion
testGatedEquivalence =
  case validatedForgeSpec of
    Left errors ->
      assertFailure ("unexpected spec validation failure: " <> show (NonEmpty.toList errors))
    Right spec -> do
      let decl =
            phaseDecl "stabilize-equiv" (Just (canonicalRoundBudget 8))
          basePlan =
            compilePlan spec decl
          gatedPlan =
            basePlan {planProgram = gated stabGate (planProgram basePlan)}

      gatedResult <- runEngine gatedPlan mixedSource stabInitialState
      plainResult <- runEngine basePlan stabOnlySource stabInitialState

      case (gatedResult, plainResult) of
        (Right gatedReport, Right plainReport) -> do
          erFinalState gatedReport @?= erFinalState plainReport
          erStopReason gatedReport @?= erStopReason plainReport
          totalApplied gatedReport @?= totalApplied plainReport
        (Left err, _) ->
          assertFailure ("gated run failed: " <> show err)
        (_, Left err) ->
          assertFailure ("pre-filtered run failed: " <> show err)
  where
    totalApplied report =
      sum (fmap (obAppliedCount . roundObservation) (erRounds report))

stabInitialState :: ForgeState
stabInitialState =
  ForgeState
    { fsFragments = Set.empty,
      fsConstellations = Set.empty,
      fsCurses = Set.empty,
      fsHeat = 4
    }

stabGate ::
  Gate ForgeState ForgeGroup ForgeMatch ForgeGateTrace ForgeGroup
stabGate =
  Gate
    { gateSelector =
        filterGroupSelectorWithTrace
          "stabilize-equiv-gate"
          ( \_state _group match ->
              case match of
                Stabilize _ -> Right ()
                _ -> Left (WrongPhase StabilizeFirstPhase (stabLaneOfMatch match))
          ),
      gateValidation = mempty
    }

mixedSource ::
  WorkSource IO ForgeState ForgeState ForgeGroup ForgeMatch ForgeEvidence ForgeError
mixedSource =
  WorkSource
    { wsView = id,
      wsCandidateSpace = pure . mixedCandidateSpace,
      wsApplyScheduled = applyStabBatch,
      wsProgressed = feCommittedProgress
    }

stabOnlySource ::
  WorkSource IO ForgeState ForgeState ForgeGroup ForgeMatch ForgeEvidence ForgeError
stabOnlySource =
  WorkSource
    { wsView = id,
      wsCandidateSpace = pure . stabOnlyCandidateSpace,
      wsApplyScheduled = applyStabBatch,
      wsProgressed = feCommittedProgress
    }

mixedCandidateSpace ::
  ForgeState ->
  CandidateSpace IO ForgeGroup () ForgeMatch
mixedCandidateSpace _state =
  finiteCandidateSpace
    [ (SupportedGroup StabilizeFragment (FragmentSupport WolfFang), [Stabilize WolfFang]),
      (SupportedGroup StabilizeFragment (FragmentSupport StarShard), [Stabilize StarShard]),
      (SupportedGroup StabilizeFragment (FragmentSupport GlassShard), [Stabilize GlassShard]),
      (RuleGroup FusePair, [Fuse WolfFang StarShard WolfStar])
    ]

stabOnlyCandidateSpace ::
  ForgeState ->
  CandidateSpace IO ForgeGroup () ForgeMatch
stabOnlyCandidateSpace _state =
  finiteCandidateSpace
    [ (SupportedGroup StabilizeFragment (FragmentSupport WolfFang), [Stabilize WolfFang]),
      (SupportedGroup StabilizeFragment (FragmentSupport StarShard), [Stabilize StarShard]),
      (SupportedGroup StabilizeFragment (FragmentSupport GlassShard), [Stabilize GlassShard])
    ]

applyStabBatch ::
  ScheduledBatch ForgeGroup ForgeMatch ->
  ForgeState ->
  IO (Either ForgeError (ApplyResult ForgeState ForgeEvidence))
applyStabBatch batch state =
  let matches = scheduledBatchMatches batch
      (!nextState, !count) = Foldable.foldl' applyOne (state, 0 :: Natural) matches
      evidence =
        ForgeEvidence
          { feAppliedByLane =
              if count > 0
                then Map.fromList [(StabilizeFragment, count)]
                else Map.empty,
            feForged = Set.empty,
            feCursesBroken = Set.empty,
            feHeatDelta = 0,
            feHeatAfter = fsHeat nextState,
            feCursesAfter = fsCurses nextState,
            feCommittedProgress = count > 0,
            feFixedPoint = False,
            feExecution = []
          }
   in pure (Right (applyResult nextState evidence (fromIntegral count)))
  where
    applyOne (s, n) match =
      case match of
        Stabilize fragment ->
          if Set.notMember fragment (fsFragments s)
            then (s {fsFragments = Set.insert fragment (fsFragments s)}, n + 1)
            else (s, n)
        _ ->
          (s, n)

stabLaneOfMatch :: ForgeMatch -> ForgeLane
stabLaneOfMatch match =
  case match of
    Stabilize _ -> StabilizeFragment
    Fuse {} -> FusePair
    Break _ -> BreakCurse
    Cool _ -> CoolForge
    Eclipse -> InvokeEclipse

expectRight ::
  Show err =>
  Either err value ->
  IO value
expectRight result =
  case result of
    Right value ->
      pure value
    Left err ->
      assertFailure ("unexpected failure: " <> show err)

assertParallelSequentialAgreement ::
  ForgeReport ->
  ForgeReport ->
  Assertion
assertParallelSequentialAgreement sequentialReport parallelReport =
  assertEqual
    "parallel execution must preserve the full sequential report; the demonstration is a bisimulation, not an eligibility flag"
    sequentialReport
    parallelReport

assertParallelWorkWasScheduled ::
  ForgeReport ->
  Assertion
assertParallelWorkWasScheduled report = do
  let batches =
        reportExecutionBatches report
  assertBool
    "expected at least one non-empty scheduled batch in the parallel run"
    (any ((> 0) . febScheduledDeltaCount) batches)
  assertBool
    "expected at least one committed batch in the parallel run"
    (any ((> 0) . febCommittedDeltaCount) batches)

assertCommittedLaneRound ::
  Int ->
  ForgeLane ->
  Natural ->
  Natural ->
  [EngineRound ForgeGroup ForgeGateTrace ForgeEvidence] ->
  Assertion
assertCommittedLaneRound roundIndex lane expectedScheduled expectedApplied rounds =
  case findReportRound roundIndex rounds of
    Nothing ->
      assertFailure ("missing committed engine round " <> show roundIndex)
    Just roundValue -> do
      let summary =
            Map.findWithDefault emptyLaneRoundSummary lane (laneRoundSummaries roundValue)
      lrsScheduled summary @?= expectedScheduled
      lrsApplied summary @?= expectedApplied

assertBackoffInstalled ::
  Int ->
  ForgeLane ->
  [EngineRound ForgeGroup ForgeGateTrace ForgeEvidence] ->
  Assertion
assertBackoffInstalled roundIndex lane rounds =
  case findReportRound roundIndex rounds of
    Nothing ->
      assertFailure ("missing committed engine round " <> show roundIndex)
    Just roundValue ->
      assertBool
        ("expected backoff installation for " <> show lane <> " in round " <> show roundIndex)
        (maybe False lrsBackoffInstalled (Map.lookup lane (laneRoundSummaries roundValue)))

assertCooldownSuppressed ::
  Int ->
  ForgeLane ->
  [EngineRound ForgeGroup ForgeGateTrace ForgeEvidence] ->
  Assertion
assertCooldownSuppressed roundIndex lane rounds =
  case findReportRound roundIndex rounds of
    Nothing ->
      assertFailure ("missing committed engine round " <> show roundIndex)
    Just roundValue ->
      assertBool
        ("expected cooldown suppression for " <> show lane <> " in round " <> show roundIndex)
        (maybe False lrsCooldownSuppressed (Map.lookup lane (laneRoundSummaries roundValue)))

assertGateTraceRetained ::
  [EngineRound ForgeGroup ForgeGateTrace ForgeEvidence] ->
  Assertion
assertGateTraceRetained rounds =
  case findReportRound 1 rounds of
    Nothing ->
      assertFailure "missing fusion round"
    Just roundValue ->
      assertBool
        "expected rejected stabilization gate trace in retained engine observation"
        (WrongPhase PrimeFusionPhase StabilizeFragment `elem` obGateTrace (roundObservation roundValue))

assertDynamicPriorityAfterFusion ::
  [EngineRound ForgeGroup ForgeGateTrace ForgeEvidence] ->
  Assertion
assertDynamicPriorityAfterFusion rounds =
  case findReportRound 1 rounds of
    Nothing ->
      assertFailure "missing fusion round"
    Just roundValue ->
      lookupPriorityEvidence
        (SupportedGroup BreakCurse (CurseSupport MirrorHex))
        (forgePriorityObservation (roundObservation roundValue))
        @?= priorityEvidence 100 1 0 criticalPriorityRank

assertDynamicPriorityAfterCurseBreak ::
  [EngineRound ForgeGroup ForgeGateTrace ForgeEvidence] ->
  Assertion
assertDynamicPriorityAfterCurseBreak rounds =
  case findReportRound 2 rounds of
    Nothing ->
      assertFailure "missing curse-break round"
    Just roundValue ->
      lookupPriorityEvidence
        (RuleGroup FusePair)
        (forgePriorityObservation (roundObservation roundValue))
        @?= priorityEvidence 90 1 0 criticalPriorityRank
