
module Moonlight.Control.Diagnostics.Pale
  ( traceLogToPale,
    roundTraceToPale,
  )
where

import Moonlight.Control.Diagnostics.Trace
  ( RoundTrace (..),
    TraceLog,
    RoundMetrics (..),
    traceLogRounds,
  )
import Moonlight.Control.Schedule.Round
  ( ScheduleTrace (..),
    scheduleTraceBannedUntil,
    scheduleTraceSkippedByScheduler,
  )
import Moonlight.Control.Count
  ( naturalToBoundedInt,
    workCountLowerBoundToBoundedInt,
  )
import Moonlight.Pale.Diagnostic.Section.Rewrite qualified as PaleRewrite
import Moonlight.Pale.Diagnostic.Section.Saturation qualified as PaleSaturation

traceLogToPale ::
  (schedulerGroup -> ruleKey) ->
  TraceLog ruleKey schedulerGroup ->
  PaleSaturation.SaturationTrace ruleKey
traceLogToPale projectGroup =
  PaleSaturation.SaturationTrace
    . fmap (roundTraceToPale projectGroup)
    . traceLogRounds

roundTraceToPale ::
  (schedulerGroup -> ruleKey) ->
  RoundTrace ruleKey schedulerGroup ->
  PaleSaturation.SaturationIterationTrace ruleKey
roundTraceToPale projectGroup roundTrace =
  let metrics =
        roundTraceMetrics roundTrace
   in PaleSaturation.SaturationIterationTrace
        { PaleSaturation.sitIteration = rmIteration metrics,
          PaleSaturation.sitNodeCountBefore = rmNodeCountBefore metrics,
          PaleSaturation.sitNodeCountAfter = rmNodeCountAfter metrics,
          PaleSaturation.sitBaseEligibleCount = rmBaseEligibleCount metrics,
          PaleSaturation.sitContextEligibleCount = rmContextEligibleCount metrics,
          PaleSaturation.sitAggregatedEligibleCount = rmAggregatedEligibleCount metrics,
          PaleSaturation.sitGuidedCount = rmGuidedCount metrics,
          PaleSaturation.sitScheduledCount = rmScheduledCount metrics,
          PaleSaturation.sitFactsChanged = rmFactsChanged metrics,
          PaleSaturation.sitFactRoundCount = rmFactRoundCount metrics,
          PaleSaturation.sitContextRevision = rmContextRevision metrics,
          PaleSaturation.sitRuleTraces =
            fmap
              (scheduleTraceToPaleRuleTrace projectGroup)
              (roundTraceSchedule roundTrace)
        }

scheduleTraceToPaleRuleTrace ::
  (schedulerGroup -> ruleKey) ->
  ScheduleTrace schedulerGroup ->
  PaleRewrite.RuleTrace ruleKey
scheduleTraceToPaleRuleTrace projectGroup scheduleTrace =
  PaleRewrite.RuleTrace
    { PaleRewrite.rtRuleId =
        projectGroup (strGroup scheduleTrace),
      PaleRewrite.rtMatchedCount =
        workCountLowerBoundToBoundedInt (strMatchedCount scheduleTrace),
      PaleRewrite.rtFilteredCount =
        workCountLowerBoundToBoundedInt (strFilteredCount scheduleTrace),
      PaleRewrite.rtScheduledCount =
        naturalToBoundedInt (strScheduledCount scheduleTrace),
      PaleRewrite.rtSkippedByScheduler =
        scheduleTraceSkippedByScheduler scheduleTrace,
      PaleRewrite.rtBannedUntil =
        scheduleTraceBannedUntil scheduleTrace
    }
