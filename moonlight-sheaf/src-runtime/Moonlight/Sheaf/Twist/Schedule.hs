{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Sheaf.Twist.Schedule
  ( BackoffConfig (..),
    SupportScheduleOrder (..),
    TracePolicy (..),
    SupportSchedulerConfig (..),
    defaultSupportSchedulerConfig,
    SupportDecisionStats (..),
    SupportMatchDecision (..),
    SupportSchedulerState (..),
    emptySupportSchedulerState,
    advanceCooldowns,
    positiveCooldown,
    deterministicSupportDecisionWith,
    backoffSupportDecisionWith,
    insertDecisionCooldown,
    groupSupportedMatchesWith,
    scheduleSupportedMatchesWith,
    scheduleSupportedRoundWith,
    supportedMatchGroupKey,
    SupportSchedulerView (..),
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sortBy, sortOn)
import Data.Map.Strict qualified as Map
import Data.Kind (Type)

type BackoffConfig :: Type
data BackoffConfig = BackoffConfig
  { bcMatchLimit :: !Int,
    bcCooldownRounds :: !Int
  }
  deriving stock (Eq, Ord, Show, Read)

type SupportScheduleOrder :: Type
data SupportScheduleOrder
  = ByRuleIdThenSubstitution
  | BackoffByRule !BackoffConfig
  deriving stock (Eq, Ord, Show, Read)

type TracePolicy :: Type
data TracePolicy
  = NoTrace
  | TraceLast !Int
  | TraceAll
  deriving stock (Eq, Ord, Show, Read)

type SupportSchedulerConfig :: Type
data SupportSchedulerConfig = SupportSchedulerConfig
  { sscOrder :: !SupportScheduleOrder,
    sscTracePolicy :: !TracePolicy
  }
  deriving stock (Eq, Ord, Show, Read)

defaultSupportSchedulerConfig :: SupportSchedulerConfig
defaultSupportSchedulerConfig =
  SupportSchedulerConfig
    { sscOrder = ByRuleIdThenSubstitution,
      sscTracePolicy = NoTrace
    }

type SupportDecisionStats :: Type
data SupportDecisionStats = SupportDecisionStats
  { sdsMatchedCount :: !Int,
    sdsScheduledCount :: !Int,
    sdsSuppressedCount :: !Int,
    sdsSuppressedByCooldown :: !Bool
  }
  deriving stock (Eq, Ord, Show)

type SupportMatchDecision :: Type -> Type -> Type
data SupportMatchDecision traceEntry match = SupportMatchDecision
  { smdRuleKey :: !Int,
    smdScheduledMatches :: ![match],
    smdTraceEntry :: !traceEntry,
    smdNextCooldown :: !(Maybe Int)
  }

type SupportSchedulerState :: Type -> Type
data SupportSchedulerState traceEntry = SupportSchedulerState
  { sssCooldowns :: !(IntMap Int),
    sssTrace :: ![traceEntry]
  }
  deriving stock (Eq, Show)

emptySupportSchedulerState :: SupportSchedulerState traceEntry
emptySupportSchedulerState =
  SupportSchedulerState
    { sssCooldowns = IntMap.empty,
      sssTrace = []
    }

advanceCooldowns :: IntMap Int -> IntMap Int
advanceCooldowns =
  IntMap.mapMaybe
    (\cooldown -> if cooldown <= 1 then Nothing else Just (cooldown - 1))

positiveCooldown :: Int -> Maybe Int
positiveCooldown cooldownRounds =
  if cooldownRounds > 0
    then Just cooldownRounds
    else Nothing

deterministicSupportDecisionWith ::
  (group -> Int) ->
  (Int -> group -> SupportDecisionStats -> traceEntry) ->
  Int ->
  (group, [match]) ->
  SupportMatchDecision traceEntry match
deterministicSupportDecisionWith ruleKeyOf traceFor roundIndex (groupValue, matches) =
  let matchedCount = length matches
      decisionStats =
        SupportDecisionStats
          { sdsMatchedCount = matchedCount,
            sdsScheduledCount = matchedCount,
            sdsSuppressedCount = 0,
            sdsSuppressedByCooldown = False
          }
   in SupportMatchDecision
        { smdRuleKey = ruleKeyOf groupValue,
          smdScheduledMatches = matches,
          smdTraceEntry = traceFor roundIndex groupValue decisionStats,
          smdNextCooldown = Nothing
        }

backoffSupportDecisionWith ::
  (group -> Int) ->
  (Int -> group -> SupportDecisionStats -> traceEntry) ->
  IntMap Int ->
  BackoffConfig ->
  Int ->
  (group, [match]) ->
  SupportMatchDecision traceEntry match
backoffSupportDecisionWith ruleKeyOf traceFor currentCooldowns backoffConfig roundIndex (groupValue, matches) =
  let ruleKey = ruleKeyOf groupValue
      currentCooldown = IntMap.findWithDefault 0 ruleKey currentCooldowns
      matchedCount = length matches
      scheduledCount =
        if currentCooldown > 0
          then 0
          else min matchedCount (max 0 (bcMatchLimit backoffConfig))
      scheduledMatches = take scheduledCount matches
      suppressedCount = matchedCount - scheduledCount
      nextCooldown =
        if currentCooldown > 0 || suppressedCount <= 0
          then Nothing
          else positiveCooldown (bcCooldownRounds backoffConfig)
      decisionStats =
        SupportDecisionStats
          { sdsMatchedCount = matchedCount,
            sdsScheduledCount = scheduledCount,
            sdsSuppressedCount = suppressedCount,
            sdsSuppressedByCooldown = currentCooldown > 0
          }
   in SupportMatchDecision
        { smdRuleKey = ruleKey,
          smdScheduledMatches = scheduledMatches,
          smdTraceEntry = traceFor roundIndex groupValue decisionStats,
          smdNextCooldown = nextCooldown
        }

insertDecisionCooldown ::
  SupportMatchDecision traceEntry match ->
  IntMap Int ->
  IntMap Int
insertDecisionCooldown decisionValue =
  maybe
    id
    (\cooldown -> IntMap.insert (smdRuleKey decisionValue) cooldown)
    (smdNextCooldown decisionValue)

scheduleSupportedMatchesWith ::
  (Ord support, Ord scheduleKey) =>
  (match -> Int) ->
  (match -> support) ->
  (match -> scheduleKey) ->
  [match] ->
  [match]
scheduleSupportedMatchesWith matchRuleKey matchSupport matchScheduleKey =
  sortOn
    (\matchValue -> (matchRuleKey matchValue, matchSupport matchValue, matchScheduleKey matchValue))

supportedMatchGroupKey ::
  (match -> Int) ->
  (match -> support) ->
  match ->
  (Int, support)
supportedMatchGroupKey matchRuleKey matchSupport matchValue =
  (matchRuleKey matchValue, matchSupport matchValue)

groupSupportedMatchesWith ::
  (Ord support, Ord scheduleKey) =>
  (Int -> Int -> Ordering) ->
  (match -> Int) ->
  (match -> support) ->
  (match -> scheduleKey) ->
  [match] ->
  [((Int, support), [match])]
groupSupportedMatchesWith ruleOrdering matchRuleKey matchSupport matchScheduleKey matches =
  sortBy compareGroups (Map.toAscList groupedMatches)
  where
    scheduledMatches =
      scheduleSupportedMatchesWith
        matchRuleKey
        matchSupport
        matchScheduleKey
        matches

    groupedMatches =
      Map.fromListWith (flip (<>))
        [ (supportedMatchGroupKey matchRuleKey matchSupport matchValue, [matchValue])
          | matchValue <- scheduledMatches
        ]

    compareGroups ((leftRuleKey, leftSupport), _) ((rightRuleKey, rightSupport), _) =
      ruleOrdering leftRuleKey rightRuleKey <> compare leftSupport rightSupport

scheduleSupportedRoundWith ::
  (Ord support, Ord scheduleKey) =>
  SupportSchedulerConfig ->
  (Int -> Int -> Ordering) ->
  (match -> Int) ->
  (match -> support) ->
  (match -> scheduleKey) ->
  (Int -> (Int, support) -> SupportDecisionStats -> traceEntry) ->
  Int ->
  [match] ->
  SupportSchedulerState traceEntry ->
  ([match], SupportSchedulerState traceEntry)
scheduleSupportedRoundWith schedulerConfig ruleOrdering matchRuleKey matchSupport matchScheduleKey traceFor roundIndex supportedMatches schedulerState =
  let groupedMatches =
        groupSupportedMatchesWith
          ruleOrdering
          matchRuleKey
          matchSupport
          matchScheduleKey
          supportedMatches
      decisions =
        case sscOrder schedulerConfig of
          ByRuleIdThenSubstitution ->
            fmap
              (deterministicSupportDecisionWith fst traceFor roundIndex)
              groupedMatches
          BackoffByRule backoffConfig ->
            fmap
              (backoffSupportDecisionWith fst traceFor (sssCooldowns schedulerState) backoffConfig roundIndex)
              groupedMatches
      scheduledMatches = decisions >>= smdScheduledMatches
      cooldownsAfterRound =
        case sscOrder schedulerConfig of
          ByRuleIdThenSubstitution ->
            IntMap.empty
          BackoffByRule _ ->
            foldr insertDecisionCooldown (advanceCooldowns (sssCooldowns schedulerState)) decisions
      traceAfterRound =
        appendTraceEntries
          (sscTracePolicy schedulerConfig)
          (sssTrace schedulerState)
          (fmap smdTraceEntry decisions)
   in ( scheduledMatches,
        SupportSchedulerState
          { sssCooldowns = cooldownsAfterRound,
            sssTrace = traceAfterRound
          }
      )

type SupportSchedulerView :: Type -> Type -> Type
data SupportSchedulerView host traceEntry = SupportSchedulerView
  { ssvIterationCount :: !Int,
    ssvTrace :: ![traceEntry],
    ssvHostState :: !host
  }
  deriving stock (Eq, Show)

appendTraceEntries :: TracePolicy -> [traceEntry] -> [traceEntry] -> [traceEntry]
appendTraceEntries tracePolicy existingTrace newEntries =
  case tracePolicy of
    NoTrace ->
      existingTrace
    TraceAll ->
      existingTrace <> newEntries
    TraceLast limitValue ->
      takeLast limitValue (existingTrace <> newEntries)

takeLast :: Int -> [a] -> [a]
takeLast limitValue values =
  let reversedValues = reverse values
   in reverse (take limitValue reversedValues)
