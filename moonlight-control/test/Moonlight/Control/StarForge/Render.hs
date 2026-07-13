module Moonlight.Control.StarForge.Render
  ( LaneRoundSummary (..),
    emptyLaneRoundSummary,
    renderStarForgeCampaign,
    laneRoundSummaries,
    programTraceContainsTrySkipped,
    programTraceContainsRepeat,
    reportExecutionBatches,
    findReportRound,
  )
where

import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( mapMaybe,
  )
import Data.Set qualified as Set
import Numeric.Natural
  ( Natural,
  )

import Moonlight.Control.Count
  ( workCountMayBePositive,
  )
import Moonlight.Control.Engine.Report
  ( EngineReport (..),
    EngineRound (..),
    Observation (..),
    RoundSummary (..),
    StopReason (..),
  )
import Moonlight.Control.Schedule
  ( ScheduleGroup (..),
    sgRuleKey,
  )
import Moonlight.Control.Schedule.Round
  ( ScheduleTrace (..),
  )
import Moonlight.Control.StarForge.Model
  ( ForgeEvidence (..),
    ForgeExecutionBatch,
    ForgeGroup,
    ForgeGateTrace,
    ForgeLane (..),
    ForgeReport,
    ForgeState (..),
  )
import Moonlight.Control.Trace
  ( Trace (..),
    TryOutcome (..),
  )

data CampaignEvent
  = CampaignCommitted !RoundSummary !(Maybe (EngineRound ForgeGroup ForgeGateTrace ForgeEvidence))
  | CampaignTrySkipped !RoundSummary
  deriving stock (Eq, Show)

renderStarForgeCampaign ::
  ForgeReport ->
  [String]
renderStarForgeCampaign report =
  zipWith renderCampaignEvent [0 :: Int ..] (campaignEvents report)
    <> renderFinalOutcome report
  where
    renderCampaignEvent :: Int -> CampaignEvent -> String
    renderCampaignEvent eventIndex event =
      case event of
        CampaignTrySkipped _summary ->
          "round "
            <> show eventIndex
            <> ": "
            <> show InvokeEclipse
            <> " tried, skipped, state rolled back"
        CampaignCommitted _summary Nothing ->
          "round "
            <> show eventIndex
            <> ": committed phase missing retained report round"
        CampaignCommitted _summary (Just roundValue) ->
          "round "
            <> show eventIndex
            <> ": "
            <> List.intercalate "; " (renderLaneRound roundValue)

renderFinalOutcome :: ForgeReport -> [String]
renderFinalOutcome report =
  case erStopReason report of
    Converged ->
      [ "final: converged: "
          <> show (Set.size (fsConstellations (erFinalState report)))
          <> " constellations forged"
      ]
    _ ->
      []

campaignEvents ::
  ForgeReport ->
  [CampaignEvent]
campaignEvents report =
  collectTraceEvents roundsByIndex (erProgramTrace report)
  where
    roundsByIndex =
      Map.fromList
        (fmap indexRound (erRounds report))

    indexRound ::
      EngineRound ForgeGroup ForgeGateTrace ForgeEvidence ->
      (Int, EngineRound ForgeGroup ForgeGateTrace ForgeEvidence)
    indexRound roundValue =
      (obRound (roundObservation roundValue), roundValue)

collectTraceEvents ::
  Map Int (EngineRound ForgeGroup ForgeGateTrace ForgeEvidence) ->
  Trace RoundSummary ->
  [CampaignEvent]
collectTraceEvents roundsByIndex traceValue =
  case traceValue of
    SkipTrace ->
      []
    PhaseTrace summary ->
      [CampaignCommitted summary (Map.lookup (rsRound summary) roundsByIndex)]
    SequenceTrace nestedTraces ->
      foldMap (collectTraceEvents roundsByIndex) nestedTraces
    ChoiceTrace {ctRejected, ctChosen} ->
      foldMap (collectTraceEvents roundsByIndex) ctRejected
        <> collectTraceEvents roundsByIndex ctChosen
    RepeatTrace iterationTraces ->
      foldMap (collectTraceEvents roundsByIndex) iterationTraces
    TryTrace tryOutcome nestedTrace ->
      case tryOutcome of
        TryApplied ->
          collectTraceEvents roundsByIndex nestedTrace
        TrySkipped ->
          fmap CampaignTrySkipped (roundSummaries nestedTrace)

roundSummaries :: Trace RoundSummary -> [RoundSummary]
roundSummaries traceValue =
  case traceValue of
    SkipTrace ->
      []
    PhaseTrace summary ->
      [summary]
    SequenceTrace nestedTraces ->
      foldMap roundSummaries nestedTraces
    ChoiceTrace {ctRejected, ctChosen} ->
      foldMap roundSummaries ctRejected <> roundSummaries ctChosen
    RepeatTrace iterationTraces ->
      foldMap roundSummaries iterationTraces
    TryTrace _tryOutcome nestedTrace ->
      roundSummaries nestedTrace

data LaneRoundSummary = LaneRoundSummary
  { lrsScheduled :: !Natural,
    lrsApplied :: !Natural,
    lrsBackoffInstalled :: !Bool,
    lrsCooldownSuppressed :: !Bool
  }
  deriving stock (Eq, Ord, Show, Read)

emptyLaneRoundSummary :: LaneRoundSummary
emptyLaneRoundSummary =
  LaneRoundSummary
    { lrsScheduled = 0,
      lrsApplied = 0,
      lrsBackoffInstalled = False,
      lrsCooldownSuppressed = False
    }

renderLaneRound ::
  EngineRound ForgeGroup ForgeGateTrace ForgeEvidence ->
  [String]
renderLaneRound roundValue =
  mapMaybe renderLaneSummary laneNarrativeOrder
  where
    summaries =
      laneRoundSummaries roundValue

    renderLaneSummary lane =
      let summary =
            Map.findWithDefault emptyLaneRoundSummary lane summaries
       in if laneSummaryVisible summary
            then Just (renderLaneSummaryText lane summary)
            else Nothing

laneRoundSummaries ::
  EngineRound ForgeGroup ForgeGateTrace ForgeEvidence ->
  Map ForgeLane LaneRoundSummary
laneRoundSummaries roundValue =
  let observation =
        roundObservation roundValue
      scheduledSummary =
        Foldable.foldl'
          insertScheduleTrace
          Map.empty
          (obScheduleTrace observation)
      appliedSummary =
        fmap
          (\appliedCount -> emptyLaneRoundSummary {lrsApplied = appliedCount})
          (feAppliedByLane (obEvidence observation))
   in Map.unionWith mergeLaneRoundSummary scheduledSummary appliedSummary

insertScheduleTrace ::
  Map ForgeLane LaneRoundSummary ->
  ScheduleTrace ForgeGroup ->
  Map ForgeLane LaneRoundSummary
insertScheduleTrace summaries traceValue =
  Map.insertWith
    mergeLaneRoundSummary
    lane
    traceSummary
    summaries
  where
    lane =
      sgRuleKey (strGroup traceValue)

    traceSummary =
      emptyLaneRoundSummary
        { lrsScheduled = strScheduledCount traceValue,
          lrsBackoffInstalled =
            not (strSuppressedByCooldown traceValue)
              && strScheduledCount traceValue > 0
              && maybe False (const True) (strCooldownUntil traceValue),
          lrsCooldownSuppressed =
            strSuppressedByCooldown traceValue
              && workCountMayBePositive (strSuppressedCount traceValue)
        }

mergeLaneRoundSummary ::
  LaneRoundSummary ->
  LaneRoundSummary ->
  LaneRoundSummary
mergeLaneRoundSummary left right =
  LaneRoundSummary
    { lrsScheduled = lrsScheduled left + lrsScheduled right,
      lrsApplied = lrsApplied left + lrsApplied right,
      lrsBackoffInstalled = lrsBackoffInstalled left || lrsBackoffInstalled right,
      lrsCooldownSuppressed = lrsCooldownSuppressed left || lrsCooldownSuppressed right
    }

laneSummaryVisible ::
  LaneRoundSummary ->
  Bool
laneSummaryVisible summary =
  lrsScheduled summary > 0
    || lrsApplied summary > 0
    || lrsBackoffInstalled summary
    || lrsCooldownSuppressed summary

renderLaneSummaryText ::
  ForgeLane ->
  LaneRoundSummary ->
  String
renderLaneSummaryText lane summary
  | lrsCooldownSuppressed summary
      && lrsScheduled summary == 0
      && lrsApplied summary == 0 =
      show lane <> " suppressed by cooldown"
  | otherwise =
      show lane
        <> " scheduled "
        <> show (lrsScheduled summary)
        <> " / applied "
        <> show (lrsApplied summary)
        <> backoffSuffix
  where
    backoffSuffix =
      if lrsBackoffInstalled summary
        then ", backoff installed"
        else ""

laneNarrativeOrder :: [ForgeLane]
laneNarrativeOrder =
  [ StabilizeFragment,
    FusePair,
    BreakCurse,
    CoolForge,
    InvokeEclipse
  ]

programTraceContainsTrySkipped ::
  Trace RoundSummary ->
  Bool
programTraceContainsTrySkipped traceValue =
  case traceValue of
    SkipTrace ->
      False
    PhaseTrace _summary ->
      False
    SequenceTrace nestedTraces ->
      any programTraceContainsTrySkipped nestedTraces
    ChoiceTrace {ctRejected, ctChosen} ->
      any programTraceContainsTrySkipped ctRejected
        || programTraceContainsTrySkipped ctChosen
    RepeatTrace iterationTraces ->
      any programTraceContainsTrySkipped iterationTraces
    TryTrace tryOutcome nestedTrace ->
      tryOutcome == TrySkipped || programTraceContainsTrySkipped nestedTrace

programTraceContainsRepeat ::
  Trace RoundSummary ->
  Bool
programTraceContainsRepeat traceValue =
  case traceValue of
    SkipTrace ->
      False
    PhaseTrace _summary ->
      False
    SequenceTrace nestedTraces ->
      any programTraceContainsRepeat nestedTraces
    ChoiceTrace {ctRejected, ctChosen} ->
      any programTraceContainsRepeat ctRejected
        || programTraceContainsRepeat ctChosen
    RepeatTrace _iterationTraces ->
      True
    TryTrace _tryOutcome nestedTrace ->
      programTraceContainsRepeat nestedTrace

reportExecutionBatches ::
  ForgeReport ->
  [ForgeExecutionBatch]
reportExecutionBatches =
  foldMap (feExecution . obEvidence . roundObservation) . erRounds

findReportRound ::
  Int ->
  [EngineRound ForgeGroup ForgeGateTrace ForgeEvidence] ->
  Maybe (EngineRound ForgeGroup ForgeGateTrace ForgeEvidence)
findReportRound roundIndex =
  List.find ((== roundIndex) . obRound . roundObservation)
