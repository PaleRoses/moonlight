{-# LANGUAGE TupleSections #-}

module Moonlight.Control.Scheduling.Support
  ( SupportTraceView (..),
    scheduleTraceSupportView,
    SupportRuntimeKey (..),
    SupportRuntimeSupportStat (..),
    SupportRuntimeRuleStat (..),
    SupportRuntimeOverlay (..),
    supportRuntimeOverlayFromTrace,
    supportRuntimeOverlayFromScheduleTrace,
    supportRuntimeRulePriorityObservation,
    supportRuntimeSupportPriorityObservation,
    supportRuntimeRulePriorityProfile,
    supportRuntimeSupportPriorityProfile,
    supportRuntimeObservedRuleCount,
    supportRuntimeSuppressedRuleCount,
    supportRuntimeCooldownRuleCount,
  )
where

import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Numeric.Natural (Natural)
import Moonlight.Core (accumByKey)
import Moonlight.Control.Weight
  ( PriorityEvidence,
    PriorityObservation,
    PriorityProfile,
    observedScheduledPriorityEvidenceNatural,
    priorityRankFromBool,
    priorityProfileFromList,
  )
import Moonlight.Control.Schedule
  ( ScheduleGroup (..),
    sgRuleKey,
  )
import Moonlight.Control.Schedule.Round
  ( ScheduleTrace (..),
  )
import Moonlight.Control.Count
  ( SuppressionCounts,
    WorkCount,
    anyCooldownSuppressed,
    anySuppressed,
    singletonSuppressionCounts,
    suppressionScheduledCount,
  )

data SupportTraceView entry support rule = SupportTraceView
  { stvRound :: entry -> Int,
    stvSupport :: entry -> Maybe support,
    stvRuleId :: entry -> rule,
    stvMatchedCount :: entry -> WorkCount,
    stvScheduledCount :: entry -> Natural,
    stvSuppressedCount :: entry -> WorkCount,
    stvSuppressedByCooldown :: entry -> Bool
  }

scheduleTraceSupportView ::
  SupportTraceView
    (ScheduleTrace (ScheduleGroup rule support))
    support
    rule
scheduleTraceSupportView =
  SupportTraceView
    { stvRound = strRound,
      stvSupport = scheduleGroupSupport . strGroup,
      stvRuleId = sgRuleKey . strGroup,
      stvMatchedCount = strMatchedCount,
      stvScheduledCount = strScheduledCount,
      stvSuppressedCount = strSuppressedCount,
      stvSuppressedByCooldown = strSuppressedByCooldown
    }

scheduleGroupSupport :: ScheduleGroup rule support -> Maybe support
scheduleGroupSupport scheduleGroup =
  case scheduleGroup of
    RuleGroup _rule ->
      Nothing
    SupportedGroup _rule support ->
      Just support

data SupportRuntimeKey support rule = SupportRuntimeKey
  { srkRuleId :: rule,
    srkSupport :: support
  }
  deriving stock (Eq, Ord, Show)

data SupportRuntimeSupportStat support rule = SupportRuntimeSupportStat
  { srssKey :: !(SupportRuntimeKey support rule),
    srssCounts :: !SuppressionCounts
  }
  deriving stock (Eq, Show)

data SupportRuntimeRuleStat support rule = SupportRuntimeRuleStat
  { srrsRuleId :: !rule,
    srrsSupportStats :: ![SupportRuntimeSupportStat support rule],
    srrsCounts :: !SuppressionCounts
  }
  deriving stock (Eq, Show)

data SupportRuntimeOverlay support rule = SupportRuntimeOverlay
  { sroSupportStats :: ![SupportRuntimeSupportStat support rule],
    sroRuleStats :: ![SupportRuntimeRuleStat support rule]
  }
  deriving stock (Eq, Show)

supportRuntimeOverlayFromTrace ::
  (Ord support, Ord rule) =>
  SupportTraceView entry support rule ->
  [entry] ->
  SupportRuntimeOverlay support rule
supportRuntimeOverlayFromTrace traceView supportTrace =
  let supportCountsByKey =
        Map.fromListWith
          (<>)
          ( mapMaybe
              ( \entryValue ->
                  fmap
                    (, supportCountsFromTraceEntry traceView entryValue)
                    (supportRuntimeKey traceView entryValue)
              )
              supportTrace
          )
      supportStats =
        fmap
          supportStatFromCounts
          (Map.toAscList supportCountsByKey)
      supportStatsByRule =
        accumByKey
          (srkRuleId . srssKey)
          (: [])
          supportStats
      ruleCountsByRule =
        accumByKey
          (stvRuleId traceView)
          (supportCountsFromTraceEntry traceView)
          supportTrace
      ruleStats =
        fmap
          (ruleStatFromCounts supportStatsByRule)
          (Map.toAscList ruleCountsByRule)
   in SupportRuntimeOverlay
        { sroSupportStats = supportStats,
          sroRuleStats = ruleStats
        }

supportRuntimeOverlayFromScheduleTrace ::
  (Ord support, Ord rule) =>
  [ScheduleTrace (ScheduleGroup rule support)] ->
  SupportRuntimeOverlay support rule
supportRuntimeOverlayFromScheduleTrace =
  supportRuntimeOverlayFromTrace scheduleTraceSupportView

supportRuntimeRulePriorityObservation ::
  (Ord support, Ord rule, Ord key) =>
  SupportTraceView entry support rule ->
  (rule -> key) ->
  PriorityObservation [entry] key
supportRuntimeRulePriorityObservation traceView ruleKeyOf =
  supportRuntimeRulePriorityProfile ruleKeyOf
    . supportRuntimeOverlayFromTrace traceView

supportRuntimeSupportPriorityObservation ::
  (Ord support, Ord rule) =>
  SupportTraceView entry support rule ->
  PriorityObservation [entry] (ScheduleGroup rule support)
supportRuntimeSupportPriorityObservation traceView =
  supportRuntimeSupportPriorityProfile
    . supportRuntimeOverlayFromTrace traceView

supportRuntimeRulePriorityProfile ::
  Ord key =>
  (rule -> key) ->
  SupportRuntimeOverlay support rule ->
  PriorityProfile key
supportRuntimeRulePriorityProfile ruleKeyOf =
  supportRuntimePriorityProfileFromStats
    (ruleKeyOf . srrsRuleId)
    supportRuntimeRulePriorityContribution
    . sroRuleStats

supportRuntimeSupportPriorityProfile ::
  (Ord rule, Ord support) =>
  SupportRuntimeOverlay support rule ->
  PriorityProfile (ScheduleGroup rule support)
supportRuntimeSupportPriorityProfile =
  supportRuntimePriorityProfileFromStats
    (supportRuntimeScheduleGroup . srssKey)
    supportRuntimeSupportPriorityContribution
    . sroSupportStats

supportRuntimeScheduleGroup ::
  SupportRuntimeKey support rule ->
  ScheduleGroup rule support
supportRuntimeScheduleGroup supportKey =
  SupportedGroup
    (srkRuleId supportKey)
    (srkSupport supportKey)

supportRuntimePriorityProfileFromStats ::
  Ord key =>
  (stat -> key) ->
  (stat -> PriorityEvidence) ->
  [stat] ->
  PriorityProfile key
supportRuntimePriorityProfileFromStats keyOf evidenceOf stats =
  priorityProfileFromList
    [ (keyOf statValue, evidenceOf statValue)
    | statValue <- stats
    ]

supportRuntimeObservedRuleCount :: SupportRuntimeOverlay support rule -> Int
supportRuntimeObservedRuleCount =
  length . sroRuleStats

supportRuntimeSuppressedRuleCount :: SupportRuntimeOverlay support rule -> Int
supportRuntimeSuppressedRuleCount =
  length . filter (anySuppressed . srrsCounts) . sroRuleStats

supportRuntimeCooldownRuleCount :: SupportRuntimeOverlay support rule -> Int
supportRuntimeCooldownRuleCount =
  length . filter (anyCooldownSuppressed . srrsCounts) . sroRuleStats

supportRuntimeKey ::
  SupportTraceView entry support rule ->
  entry ->
  Maybe (SupportRuntimeKey support rule)
supportRuntimeKey traceView entryValue =
  fmap
    ( \supportValue ->
        SupportRuntimeKey
          { srkRuleId = stvRuleId traceView entryValue,
            srkSupport = supportValue
          }
    )
    (stvSupport traceView entryValue)

supportCountsFromTraceEntry ::
  SupportTraceView entry support rule ->
  entry ->
  SuppressionCounts
supportCountsFromTraceEntry traceView entryValue =
  singletonSuppressionCounts
    (stvRound traceView entryValue)
    (stvMatchedCount traceView entryValue)
    (stvScheduledCount traceView entryValue)
    (stvSuppressedCount traceView entryValue)
    (stvSuppressedByCooldown traceView entryValue)

supportStatFromCounts ::
  (SupportRuntimeKey support rule, SuppressionCounts) ->
  SupportRuntimeSupportStat support rule
supportStatFromCounts (supportKey, counts) =
  SupportRuntimeSupportStat
    { srssKey = supportKey,
      srssCounts = counts
    }

ruleStatFromCounts ::
  Ord rule =>
  Map.Map rule [SupportRuntimeSupportStat support rule] ->
  (rule, SuppressionCounts) ->
  SupportRuntimeRuleStat support rule
ruleStatFromCounts supportStatsByRule (ruleValue, counts) =
  SupportRuntimeRuleStat
    { srrsRuleId = ruleValue,
      srrsSupportStats = Map.findWithDefault [] ruleValue supportStatsByRule,
      srrsCounts = counts
    }

supportRuntimeRulePriorityContribution ::
  SupportRuntimeRuleStat support rule ->
  PriorityEvidence
supportRuntimeRulePriorityContribution =
  supportRuntimePriorityEvidenceFromCounts . srrsCounts

supportRuntimeSupportPriorityContribution ::
  SupportRuntimeSupportStat support rule ->
  PriorityEvidence
supportRuntimeSupportPriorityContribution =
  supportRuntimePriorityEvidenceFromCounts . srssCounts

supportRuntimePriorityEvidenceFromCounts ::
  SuppressionCounts ->
  PriorityEvidence
supportRuntimePriorityEvidenceFromCounts counts =
  observedScheduledPriorityEvidenceNatural
    (suppressionScheduledCount counts)
    ( priorityRankFromBool
        ( anySuppressed counts
            || anyCooldownSuppressed counts
        )
    )
