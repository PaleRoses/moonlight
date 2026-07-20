-- | Scheduling configuration vocabulary interpreted by 'Moonlight.Control.Schedule.Round'.
module Moonlight.Control.Schedule
  ( ScheduleGroup (..),
    sgRuleKey,
    ScheduleOrder (..),
    BackoffConfig,
    backoffConfig,
    bcMatchLimit,
    bcCooldownRounds,
    DeficitRoundRobinConfig,
    deficitRoundRobinConfig,
    defaultDeficitRoundRobinConfig,
    drrBaseQuantum,
    drrMaxQuantum,
    drrMaxCarryMultiplier,
    TracePolicy (NoTrace, TraceAll),
    traceLastEntries,
    traceLastEntryCount,
    foldTracePolicy,
    tracePolicyEmits,
    SchedulerConfig (..),
    canonicalBackoffConfig,
    canonicalScheduleOrder,
    canonicalTracePolicy,
    canonicalSchedulerConfig,
    defaultSchedulerConfig,
    withPriorityProfile,
    mergePriorityProfile,
    applyPriorityObservation,
    clearPriorityProfile,
    SchedulerRefinement (..),
    identitySchedulerRefinement,
    applySchedulerRefinement,
    SchedulingAnalysisMode (..),
  )
where

import Numeric.Natural (Natural)

import Moonlight.Control.Weight
  ( PriorityObservation,
    PriorityProfile,
    emptyPriorityProfile,
  )

-- | A schedulable group: a bare rule key, or a key carrying support
-- information.
data ScheduleGroup key support
  = RuleGroup !key
  | SupportedGroup !key !support
  deriving stock (Eq, Ord, Show, Read)

-- | The rule key of a group, ignoring support. O(1).
sgRuleKey :: ScheduleGroup key support -> key
sgRuleKey scheduleGroup =
  case scheduleGroup of
    RuleGroup key -> key
    SupportedGroup key _support -> key

-- | Per-group pull limit and cooldown applied when a group is limited by
-- backoff. Construct with 'backoffConfig'; values are clamped to validity.
data BackoffConfig = BackoffConfig
  { backoffMatchLimit :: !PositiveNatural,
    backoffCooldownRounds :: !NonNegativeInt
  }
  deriving stock (Eq, Ord, Show)

-- | Clamps the match limit to at least 1 and the cooldown to at least 0. O(1).
backoffConfig :: Int -> Int -> BackoffConfig
backoffConfig matchLimit cooldownRounds =
  BackoffConfig
    { backoffMatchLimit = positiveNaturalFromInt matchLimit,
      backoffCooldownRounds = nonNegativeIntFromInt cooldownRounds
    }

bcMatchLimit :: BackoffConfig -> Natural
bcMatchLimit = positiveNaturalValue . backoffMatchLimit

bcCooldownRounds :: BackoffConfig -> Int
bcCooldownRounds = nonNegativeIntValue . backoffCooldownRounds

-- | How groups are ordered and limited within a round.
data ScheduleOrder
  = ByRuleIdThenSubstitution
  | BackoffByGroup !BackoffConfig
  | DeficitRoundRobin !DeficitRoundRobinConfig
  deriving stock (Eq, Ord, Show)

data DeficitRoundRobinConfig = DeficitRoundRobinConfig
  { deficitRoundRobinBaseQuantum :: !PositiveNatural,
    deficitRoundRobinMaxQuantum :: !PositiveNatural,
    deficitRoundRobinMaxCarryMultiplier :: !PositiveNatural
  }
  deriving stock (Eq, Ord, Show)

deficitRoundRobinConfig :: Int -> Int -> Int -> DeficitRoundRobinConfig
deficitRoundRobinConfig rawBaseQuantum rawMaxQuantum rawMaxCarryMultiplier =
  let baseQuantum = positiveNaturalFromInt rawBaseQuantum
      maxQuantum =
        PositiveNatural
          (max (positiveNaturalValue baseQuantum) (fromIntegral (max 1 rawMaxQuantum)))
   in DeficitRoundRobinConfig
        { deficitRoundRobinBaseQuantum = baseQuantum,
          deficitRoundRobinMaxQuantum = maxQuantum,
          deficitRoundRobinMaxCarryMultiplier = positiveNaturalFromInt rawMaxCarryMultiplier
        }

defaultDeficitRoundRobinConfig :: DeficitRoundRobinConfig
defaultDeficitRoundRobinConfig =
  deficitRoundRobinConfig 1 32 4

drrBaseQuantum :: DeficitRoundRobinConfig -> Natural
drrBaseQuantum = positiveNaturalValue . deficitRoundRobinBaseQuantum

drrMaxQuantum :: DeficitRoundRobinConfig -> Natural
drrMaxQuantum = positiveNaturalValue . deficitRoundRobinMaxQuantum

drrMaxCarryMultiplier :: DeficitRoundRobinConfig -> Natural
drrMaxCarryMultiplier = positiveNaturalValue . deficitRoundRobinMaxCarryMultiplier

-- | How many schedule trace entries are retained across rounds. 'TraceLast'
-- is reachable only through 'traceLastEntries', which validates the count.
data TracePolicy
  = NoTrace
  | TraceLast !PositiveInt
  | TraceAll
  deriving stock (Eq, Ord, Show)

-- | Retain the last @n@ entries; non-positive @n@ yields 'NoTrace'. O(1).
traceLastEntries :: Int -> TracePolicy
traceLastEntries retainedCount =
  maybe NoTrace TraceLast (positiveIntIfPositive retainedCount)

-- | The retained-entry count of a last-@n@ policy. O(1).
traceLastEntryCount :: TracePolicy -> Maybe Int
traceLastEntryCount =
  foldTracePolicy Nothing Just Nothing

-- | The total eliminator of 'TracePolicy': no-trace, last-@n@, and all. O(1).
foldTracePolicy :: r -> (Int -> r) -> r -> TracePolicy -> r
foldTracePolicy onNoTrace onTraceLast onTraceAll tracePolicy =
  case tracePolicy of
    NoTrace -> onNoTrace
    TraceLast retainedCount -> onTraceLast (positiveIntValue retainedCount)
    TraceAll -> onTraceAll

-- | Whether the policy emits any trace entries at all. O(1).
tracePolicyEmits :: TracePolicy -> Bool
tracePolicyEmits =
  foldTracePolicy False (const True) True

newtype PositiveNatural = PositiveNatural
  { positiveNaturalValue :: Natural
  }
  deriving stock (Eq, Ord, Show)

positiveNaturalFromInt :: Int -> PositiveNatural
positiveNaturalFromInt rawValue =
  PositiveNatural (fromIntegral (max 1 rawValue))

newtype PositiveInt = PositiveInt
  { positiveIntValue :: Int
  }
  deriving stock (Eq, Ord, Show)

positiveIntIfPositive :: Int -> Maybe PositiveInt
positiveIntIfPositive rawValue =
  if rawValue > 0
    then Just (PositiveInt rawValue)
    else Nothing

newtype NonNegativeInt = NonNegativeInt
  { nonNegativeIntValue :: Int
  }
  deriving stock (Eq, Ord, Show)

nonNegativeIntFromInt :: Int -> NonNegativeInt
nonNegativeIntFromInt = NonNegativeInt . max 0

-- | The full scheduling configuration for a round: ordering policy, trace
-- retention, and the priority profile that orders groups.
data SchedulerConfig group = SchedulerConfig
  { scOrder :: !ScheduleOrder,
    scTracePolicy :: !TracePolicy,
    scPriorityProfile :: !(PriorityProfile group)
  }
  deriving stock (Eq, Ord, Show)

canonicalBackoffConfig :: BackoffConfig -> BackoffConfig
canonicalBackoffConfig rawBackoffConfig =
  backoffConfig
    (fromIntegral (bcMatchLimit rawBackoffConfig))
    (bcCooldownRounds rawBackoffConfig)

canonicalScheduleOrder :: ScheduleOrder -> ScheduleOrder
canonicalScheduleOrder scheduleOrder =
  case scheduleOrder of
    ByRuleIdThenSubstitution -> ByRuleIdThenSubstitution
    BackoffByGroup backoffPolicy -> BackoffByGroup (canonicalBackoffConfig backoffPolicy)
    DeficitRoundRobin deficitRoundRobinPolicy ->
      DeficitRoundRobin (canonicalDeficitRoundRobinConfig deficitRoundRobinPolicy)

canonicalDeficitRoundRobinConfig :: DeficitRoundRobinConfig -> DeficitRoundRobinConfig
canonicalDeficitRoundRobinConfig deficitRoundRobinPolicy =
  deficitRoundRobinConfig
    (fromIntegral (drrBaseQuantum deficitRoundRobinPolicy))
    (fromIntegral (drrMaxQuantum deficitRoundRobinPolicy))
    (fromIntegral (drrMaxCarryMultiplier deficitRoundRobinPolicy))

canonicalTracePolicy :: TracePolicy -> TracePolicy
canonicalTracePolicy tracePolicy =
  case tracePolicy of
    NoTrace -> NoTrace
    TraceAll -> TraceAll
    TraceLast retainedCount -> TraceLast retainedCount

canonicalSchedulerConfig :: SchedulerConfig group -> SchedulerConfig group
canonicalSchedulerConfig schedulerConfig =
  schedulerConfig
    { scOrder = canonicalScheduleOrder (scOrder schedulerConfig),
      scTracePolicy = canonicalTracePolicy (scTracePolicy schedulerConfig)
    }

-- | Rule-id ordering, no trace, empty priority profile.
defaultSchedulerConfig :: SchedulerConfig group
defaultSchedulerConfig =
  SchedulerConfig
    { scOrder = ByRuleIdThenSubstitution,
      scTracePolicy = NoTrace,
      scPriorityProfile = emptyPriorityProfile
    }

-- | Replace the priority profile. O(1).
withPriorityProfile ::
  PriorityProfile group ->
  SchedulerConfig group ->
  SchedulerConfig group
withPriorityProfile priorityProfile schedulerConfig =
  canonicalSchedulerConfig
    ( schedulerConfig
        { scPriorityProfile = priorityProfile
        }
    )

-- | Join a profile into the configuration's profile. O(n log n) in the
-- profile sizes.
mergePriorityProfile ::
  Ord group =>
  PriorityProfile group ->
  SchedulerConfig group ->
  SchedulerConfig group
mergePriorityProfile priorityProfile schedulerConfig =
  withPriorityProfile
    (scPriorityProfile schedulerConfig <> priorityProfile)
    schedulerConfig

-- | Observe a source and join the observed profile into the configuration.
applyPriorityObservation ::
  Ord group =>
  PriorityObservation source group ->
  source ->
  SchedulerConfig group ->
  SchedulerConfig group
applyPriorityObservation observe source =
  mergePriorityProfile (observe source)

-- | Drop the priority profile. O(1).
clearPriorityProfile ::
  SchedulerConfig group ->
  SchedulerConfig group
clearPriorityProfile schedulerConfig =
  canonicalSchedulerConfig
    ( schedulerConfig
        { scPriorityProfile = emptyPriorityProfile
        }
    )

-- | A state-derived refinement of the scheduler configuration: a priority
-- observation plus a trace-policy update.
data SchedulerRefinement state group = SchedulerRefinement
  { srPriorityObservation :: !(PriorityObservation state group),
    srTracePolicyUpdate :: !(TracePolicy -> TracePolicy)
  }

identitySchedulerRefinement :: SchedulerRefinement state group
identitySchedulerRefinement =
  SchedulerRefinement
    { srPriorityObservation = const emptyPriorityProfile,
      srTracePolicyUpdate = id
    }

applySchedulerRefinement ::
  Ord group =>
  SchedulerRefinement state group ->
  state ->
  SchedulerConfig group ->
  SchedulerConfig group
applySchedulerRefinement schedulerRefinement state schedulerConfig =
  canonicalSchedulerConfig
    ( schedulerConfig
        { scTracePolicy = srTracePolicyUpdate schedulerRefinement (scTracePolicy schedulerConfig),
          scPriorityProfile =
            scPriorityProfile schedulerConfig
              <> srPriorityObservation schedulerRefinement state
        }
    )

-- | When scheduling analysis runs: once from structure, or between rounds at
-- runtime.
data SchedulingAnalysisMode
  = StructuralOnce
  | RuntimeBetweenRounds
  deriving stock (Eq, Ord, Show, Read)
