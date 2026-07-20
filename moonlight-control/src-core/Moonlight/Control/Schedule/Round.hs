{-# LANGUAGE BangPatterns #-}

-- | Deterministic budgeted scheduling with retained policy state and traces.
module Moonlight.Control.Schedule.Round
  ( SchedulerState,
    ScheduleTrace (..),
    ScheduleOutcome (..),
    scheduleTraceSkippedByScheduler,
    scheduleTraceBannedUntil,
    emptySchedulerState,
    scheduleCandidateSpace,
    schedulerGroupOrdering,
    orderCandidateGroupSummaries,
    schedulerCooldowns,
    schedulerTrace,
    replaceSchedulerTraceDelta,
    positiveCooldown,
    retainTraceEntries,
    retainGroupedTraceEntries,
  )
where

import Data.Foldable qualified as Foldable
import Data.List (sortBy)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Numeric.Natural (Natural)

import Moonlight.Control.Candidate
  ( CandidateGroup (..),
    CandidateGroupSummary (..),
    CandidateSpace (..),
    PullResult (..),
    ScheduledBatch (..),
    ScheduledMatch (..),
    csGroupSummaries,
    csLookupGroup,
    lengthNatural,
    pullCandidateCursor,
    pullRequest,
  )
import Moonlight.Control.Count
  ( WorkCount,
    WorkCoverage (..),
    workCountKnownZero,
    workCountMayBePositive,
    workCountToMaybeExact,
    workCountZero,
    naturalToBoundedInt,
  )
import Moonlight.Control.Schedule
  ( DeficitRoundRobinConfig,
    ScheduleOrder (..),
    SchedulerConfig (..),
    TracePolicy,
    bcCooldownRounds,
    bcMatchLimit,
    canonicalSchedulerConfig,
    drrBaseQuantum,
    drrMaxCarryMultiplier,
    drrMaxQuantum,
    foldTracePolicy,
    tracePolicyEmits,
  )
import Moonlight.Control.Weight
  ( CriticalityRank (..),
    EvidenceCount (..),
    PriorityEvidence (..),
    PriorityProfile,
    comparePriorityEvidence,
    lookupPriorityEvidence,
    priorityEvidenceKey,
  )

data SchedulerState group = SchedulerState
  { ssOrderState :: !(ScheduleOrderState group),
    ssTrace :: !(Seq (ScheduleTrace group)),
    ssOrderedGroupIndex :: !(Maybe (OrderedGroupIndex group)),
    ssLastRound :: !(Maybe Int)
  }
  deriving stock (Eq, Show)

data ScheduleOrderState group
  = RuleOrderState
  | BackoffOrderState !(Map group GroupBackoffState)
  | DeficitRoundRobinState !(Map group Natural) !(Maybe group)
  deriving stock (Eq, Show)

data GroupBackoffState = GroupBackoffState
  { gbsBannedUntil :: !Int,
    gbsCooldownPenalty :: !Natural
  }
  deriving stock (Eq, Show)

-- | Internal cache for the previously sorted group frontier. The cache is
-- valid only for the exact same priority profile and the exact same unique
-- group set; any churn falls back to the stage-1 decorate-sort-undecorate
-- path. API-invisible, because scheduler state is exported abstractly.
data OrderedGroupIndex group = OrderedGroupIndex
  { ogiProfile :: !(PriorityProfile group),
    ogiGroups :: ![group]
  }
  deriving stock (Eq, Show)

-- | The per-group decision record of one round.
data ScheduleTrace group = ScheduleTrace
  { strRound :: !Int,
    strGroup :: !group,
    strMatchedCount :: !WorkCount,
    strFilteredCount :: !WorkCount,
    strScheduledCount :: !Natural,
    strSuppressedCount :: !WorkCount,
    strSuppressedByCooldown :: !Bool,
    strCooldownUntil :: !(Maybe Int),
    strCoverage :: !WorkCoverage
  }
  deriving stock (Eq, Show, Read)

-- | Everything one round produced: the scheduled batch, counts of
-- suppressed and budget-deferred work, pulled metadata, the next scheduler
-- state, and the round's trace delta.
data ScheduleOutcome group meta match = ScheduleOutcome
  { soScheduledBatch :: !(ScheduledBatch group match),
    soScheduledCount :: !Natural,
    soSuppressedCount :: !WorkCount,
    soDeferredByBudgetCount :: !WorkCount,
    soPullMeta :: !meta,
    soSchedulerState :: !(SchedulerState group),
    soTracePolicy :: !TracePolicy,
    soSchedulerTraceDelta :: ![ScheduleTrace group],
    soCoverage :: !WorkCoverage
  }
  deriving stock (Eq, Show)

scheduleTraceSkippedByScheduler :: ScheduleTrace group -> Bool
scheduleTraceSkippedByScheduler =
  workCountMayBePositive . strSuppressedCount

scheduleTraceBannedUntil :: ScheduleTrace group -> Maybe Int
scheduleTraceBannedUntil = strCooldownUntil

emptySchedulerState :: SchedulerState group
emptySchedulerState =
  SchedulerState
    { ssOrderState = RuleOrderState,
      ssTrace = Seq.empty,
      ssOrderedGroupIndex = Nothing,
      ssLastRound = Nothing
    }

prepareScheduleOrderState ::
  Ord group =>
  SchedulerConfig group ->
  ScheduleOrderState group ->
  [CandidateGroupSummary group] ->
  ScheduleOrderState group
prepareScheduleOrderState schedulerConfig previousOrderState orderedSummaries =
  case scOrder schedulerConfig of
    ByRuleIdThenSubstitution ->
      RuleOrderState
    BackoffByGroup {} ->
      BackoffOrderState
        ( case previousOrderState of
            BackoffOrderState backoffStates -> backoffStates
            _ -> Map.empty
        )
    DeficitRoundRobin deficitRoundRobinPolicy ->
      let (previousDeficits, previousCursor) =
            case previousOrderState of
              DeficitRoundRobinState deficits cursor -> (deficits, cursor)
              _ -> (Map.empty, Nothing)
          !nextDeficits =
            Map.fromList
              [ ( group,
                  min
                    (deficitCarryCap deficitRoundRobinPolicy evidence)
                    (Map.findWithDefault 0 group previousDeficits + deficitRoundRobinQuantum deficitRoundRobinPolicy evidence)
                )
              | summary <- orderedSummaries,
                not (workCountKnownZero (cgsAvailableCount summary)),
                let group = cgsGroup summary,
                let evidence = lookupPriorityEvidence group (scPriorityProfile schedulerConfig)
              ]
          !nextCursor = normalizeCursor previousCursor orderedSummaries
       in DeficitRoundRobinState nextDeficits nextCursor

summariesForScheduleOrder ::
  Eq group =>
  ScheduleOrderState group ->
  [CandidateGroupSummary group] ->
  [CandidateGroupSummary group]
summariesForScheduleOrder orderState orderedSummaries =
  case orderState of
    DeficitRoundRobinState _ maybeCursor ->
      maybe orderedSummaries (`rotateSummariesTo` orderedSummaries) maybeCursor
    _ ->
      orderedSummaries

rotateSummariesTo ::
  Eq group =>
  group ->
  [CandidateGroupSummary group] ->
  [CandidateGroupSummary group]
rotateSummariesTo cursor orderedSummaries =
  case break ((== cursor) . cgsGroup) orderedSummaries of
    (_, []) -> orderedSummaries
    (beforeCursor, fromCursor) -> fromCursor <> beforeCursor

normalizeCursor ::
  Eq group =>
  Maybe group ->
  [CandidateGroupSummary group] ->
  Maybe group
normalizeCursor maybeCursor orderedSummaries =
  case maybeCursor of
    Just cursor
      | any ((== cursor) . cgsGroup) orderedSummaries -> Just cursor
    _ -> cgsGroup <$> listToMaybe orderedSummaries

finalizeScheduleOrderState ::
  Eq group =>
  [CandidateGroupSummary group] ->
  Maybe group ->
  ScheduleOrderState group ->
  ScheduleOrderState group
finalizeScheduleOrderState orderedSummaries maybeLastScheduledGroup orderState =
  case orderState of
    DeficitRoundRobinState deficits currentCursor ->
      DeficitRoundRobinState
        deficits
        ( maybe
            (normalizeCursor currentCursor orderedSummaries)
            (`nextGroupAfter` orderedSummaries)
            maybeLastScheduledGroup
        )
    _ -> orderState

nextGroupAfter ::
  Eq group =>
  group ->
  [CandidateGroupSummary group] ->
  Maybe group
nextGroupAfter group orderedSummaries =
  lookup group (zip groups (drop 1 groups <> take 1 groups))
  where
    groups = fmap cgsGroup orderedSummaries

deficitForGroup :: Ord group => group -> ScheduleOrderState group -> Natural
deficitForGroup group orderState =
  case orderState of
    DeficitRoundRobinState deficits _ -> Map.findWithDefault 0 group deficits
    _ -> 0

deficitRoundRobinQuantum :: DeficitRoundRobinConfig -> PriorityEvidence -> Natural
deficitRoundRobinQuantum deficitRoundRobinPolicy evidence =
  min
    (drrMaxQuantum deficitRoundRobinPolicy)
    ( drrBaseQuantum deficitRoundRobinPolicy
        + 8 * boundedCriticalityRank (peCriticalityRank evidence)
        + 4 * evidenceBucket (peObservedTransitionCount evidence)
        + 2 * evidenceBucket (peObservedScheduledCount evidence)
        + evidenceBucket (peStructuralInfluence evidence)
    )

deficitCarryCap :: DeficitRoundRobinConfig -> PriorityEvidence -> Natural
deficitCarryCap deficitRoundRobinPolicy evidence =
  drrMaxCarryMultiplier deficitRoundRobinPolicy
    * deficitRoundRobinQuantum deficitRoundRobinPolicy evidence

boundedCriticalityRank :: CriticalityRank -> Natural
boundedCriticalityRank (CriticalityRank rankValue) =
  min 3 rankValue

evidenceBucket :: EvidenceCount -> Natural
evidenceBucket (EvidenceCount count)
  | count >= 16 = 3
  | count >= 4 = 2
  | count >= 1 = 1
  | otherwise = 0

-- | Run one scheduling round. The configuration is canonicalized once at
-- entry; group ordering decorates each group with its priority key exactly
-- once before sorting.
scheduleCandidateSpace ::
  (Monad m, Monoid meta, Ord group) =>
  SchedulerConfig group ->
  Natural ->
  Int ->
  CandidateSpace m group meta match ->
  SchedulerState group ->
  m (ScheduleOutcome group meta match)
scheduleCandidateSpace rawSchedulerConfig roundBudget roundIndex candidateSpace schedulerState = do
  rawSummaries <- csGroupSummaries candidateSpace
  let !orderedSelection =
        orderCandidateGroupSummariesWithIndex
          schedulerConfig
          (ssOrderedGroupIndex schedulerState)
          rawSummaries
      !baseOrderedSummaries = ogsSummaries orderedSelection
      !initialOrderState =
        prepareScheduleOrderState
          schedulerConfig
          (ssOrderState schedulerState)
          baseOrderedSummaries
      !roundSummaries =
        summariesForScheduleOrder initialOrderState baseOrderedSummaries
      !initialAcc =
        ScheduleAcc
          { sacRemainingBudget = roundBudget,
            sacScheduledChunks = Seq.empty,
            sacScheduledCount = 0,
            sacSuppressedCount = workCountZero,
            sacDeferredByBudgetCount = workCountZero,
            sacPullMeta = mempty,
            sacOrderState = initialOrderState,
            sacLastScheduledGroup = Nothing,
            sacTrace = Seq.empty,
            sacCoverage = WorkCoverageComplete
          }
  finalAcc <-
    Foldable.foldlM
      (scheduleCandidateGroup schedulerConfig candidateSpace traceEnabled roundIndex)
      initialAcc
      roundSummaries

  let !scheduledBatch =
        ScheduledBatch (Foldable.fold (sacScheduledChunks finalAcc))
      !traceDelta =
        Foldable.toList (sacTrace finalAcc)
      !retainedTrace =
        retainTraceEntries
          tracePolicy
          (ssTrace schedulerState)
          traceDelta
      !nextOrderState =
        finalizeScheduleOrderState
          baseOrderedSummaries
          (sacLastScheduledGroup finalAcc)
          (sacOrderState finalAcc)

  pure
    ScheduleOutcome
      { soScheduledBatch = scheduledBatch,
        soScheduledCount = sacScheduledCount finalAcc,
        soSuppressedCount = sacSuppressedCount finalAcc,
        soDeferredByBudgetCount = sacDeferredByBudgetCount finalAcc,
        soPullMeta = sacPullMeta finalAcc,
        soSchedulerState =
          SchedulerState
            { ssOrderState = nextOrderState,
              ssTrace = retainedTrace,
              ssOrderedGroupIndex = ogsNextIndex orderedSelection,
              ssLastRound = Just roundIndex
            },
        soTracePolicy = tracePolicy,
        soSchedulerTraceDelta = traceDelta,
        soCoverage = sacCoverage finalAcc
      }
  where
    !schedulerConfig = canonicalSchedulerConfig rawSchedulerConfig
    !tracePolicy = scTracePolicy schedulerConfig
    !traceEnabled = tracePolicyEmits tracePolicy
{-# INLINABLE scheduleCandidateSpace #-}

data ScheduleAcc group meta match = ScheduleAcc
  { sacRemainingBudget :: !Natural,
    sacScheduledChunks :: !(Seq [ScheduledMatch group match]),
    sacScheduledCount :: !Natural,
    sacSuppressedCount :: !WorkCount,
    sacDeferredByBudgetCount :: !WorkCount,
    sacPullMeta :: !meta,
    sacOrderState :: !(ScheduleOrderState group),
    sacLastScheduledGroup :: !(Maybe group),
    sacTrace :: !(Seq (ScheduleTrace group)),
    sacCoverage :: !WorkCoverage
  }

scheduleCandidateGroup ::
  (Monad m, Monoid meta, Ord group) =>
  SchedulerConfig group ->
  CandidateSpace m group meta match ->
  Bool ->
  Int ->
  ScheduleAcc group meta match ->
  CandidateGroupSummary group ->
  m (ScheduleAcc group meta match)
scheduleCandidateGroup schedulerConfig candidateSpace traceEnabled roundIndex acc summary
  | workCountKnownZero availableCount =
      pure (applyGroupScheduleDecision traceEnabled roundIndex GroupSkipped acc)
  | sacRemainingBudget acc == 0 =
      pure (applyGroupScheduleDecision traceEnabled roundIndex (GroupDeferred summary) acc)
  | suppressedByCooldown =
      pure (applyGroupScheduleDecision traceEnabled roundIndex (GroupSuppressedByCooldown currentBannedUntil summary) acc)
  | groupLimit == 0 =
      pure (applyGroupScheduleDecision traceEnabled roundIndex (GroupDeferred summary) acc)
  | otherwise = do
      maybeGroup <- csLookupGroup candidateSpace group
      case maybeGroup of
        Nothing ->
          pure (applyGroupScheduleDecision traceEnabled roundIndex (GroupDeferred summary) acc)
        Just candidateGroup -> do
          cursor <- cgOpenCursor candidateGroup
          pullOutcome <- pullCandidateCursor cursor (pullRequest groupLimit)
          pure
            ( applyGroupScheduleDecision
                traceEnabled
                roundIndex
                ( GroupPulled
                    PulledGroupDecision
                      { pgdSchedulerConfig = schedulerConfig,
                        pgdRemainingBudget = sacRemainingBudget acc,
                        pgdGroupLimit = groupLimit,
                        pgdSummary = summary,
                        pgdPullOutcome = pullOutcome
                      }
                )
                acc
            )
  where
    !group = cgsGroup summary
    !availableCount = cgsAvailableCount summary
    !currentBackoffState =
      case sacOrderState acc of
        BackoffOrderState backoffStates -> Map.lookup group backoffStates
        _ -> Nothing
    !currentBannedUntil = maybe roundIndex gbsBannedUntil currentBackoffState
    !suppressedByCooldown =
      case scOrder schedulerConfig of
        BackoffByGroup {} ->
          roundIndex < currentBannedUntil && workCountMayBePositive availableCount
        _ ->
          False
    !groupLimit =
      case scOrder schedulerConfig of
        ByRuleIdThenSubstitution ->
          availableWithinBudget availableCount (sacRemainingBudget acc)
        BackoffByGroup backoffPolicy ->
          min
            (availableWithinBudget availableCount (sacRemainingBudget acc))
            (bcMatchLimit backoffPolicy)
        DeficitRoundRobin {} ->
          min
            (availableWithinBudget availableCount (sacRemainingBudget acc))
            (deficitForGroup group (sacOrderState acc))
{-# INLINABLE scheduleCandidateGroup #-}

data GroupScheduleDecision m group meta match
  = GroupSkipped
  | GroupDeferred !(CandidateGroupSummary group)
  | GroupSuppressedByCooldown !Int !(CandidateGroupSummary group)
  | GroupPulled !(PulledGroupDecision m group meta match)

data PulledGroupDecision m group meta match = PulledGroupDecision
  { pgdSchedulerConfig :: !(SchedulerConfig group),
    pgdRemainingBudget :: !Natural,
    pgdGroupLimit :: !Natural,
    pgdSummary :: !(CandidateGroupSummary group),
    pgdPullOutcome :: !(PullResult m meta match)
  }

data ScheduleDelta group meta match = ScheduleDelta
  { sdScheduledChunk :: !(Maybe [ScheduledMatch group match]),
    sdScheduledCount :: !Natural,
    sdSuppressedCount :: !WorkCount,
    sdDeferredByBudgetCount :: !WorkCount,
    sdPullMeta :: !meta,
    sdNextOrderState :: !(Maybe (ScheduleOrderState group)),
    sdScheduledGroup :: !(Maybe group),
    sdCoverage :: !WorkCoverage,
    sdTrace :: !(Maybe (ScheduleTrace group))
  }

applyGroupScheduleDecision ::
  (Monoid meta, Ord group) =>
  Bool ->
  Int ->
  GroupScheduleDecision m group meta match ->
  ScheduleAcc group meta match ->
  ScheduleAcc group meta match
applyGroupScheduleDecision traceEnabled roundIndex decision =
  \acc ->
    applyScheduleDelta
      traceEnabled
      (groupScheduleDecisionDelta roundIndex (sacOrderState acc) decision)
      acc

groupScheduleDecisionDelta ::
  (Monoid meta, Ord group) =>
  Int ->
  ScheduleOrderState group ->
  GroupScheduleDecision m group meta match ->
  ScheduleDelta group meta match
groupScheduleDecisionDelta roundIndex orderState decision =
  case decision of
    GroupSkipped ->
      emptyScheduleDelta
    GroupDeferred summary ->
      let !deferredCount = cgsAvailableCount summary
          !nextCoverage = coverageForUnpulled deferredCount
       in emptyScheduleDelta
            { sdDeferredByBudgetCount = deferredCount,
              sdCoverage = nextCoverage,
              sdTrace =
                Just
                  ( traceForGroup
                      roundIndex
                      summary
                      0
                      workCountZero
                      False
                      Nothing
                      nextCoverage
                  )
            }
    GroupSuppressedByCooldown bannedUntil summary ->
      let !suppressedCount = cgsAvailableCount summary
       in emptyScheduleDelta
            { sdSuppressedCount = suppressedCount,
              sdCoverage = WorkCoveragePartial,
              sdTrace =
                Just
                  ( traceForGroup
                      roundIndex
                      summary
                      0
                      suppressedCount
                      True
                      (Just bannedUntil)
                      WorkCoveragePartial
                  )
            }
    GroupPulled pulledDecision ->
      let !scheduledMatches =
            fmap
              (ScheduledMatch (cgsGroup summary))
              (prMatches pullOutcome)
          !scheduledCount =
            lengthNatural (prMatches pullOutcome)
          !remainingCount =
            prRemainingCount pullOutcome
          !limitedByBackoff =
            case scOrder schedulerConfig of
              BackoffByGroup backoffPolicy ->
                scheduledCount == bcMatchLimit backoffPolicy
                  && workCountMayBePositive remainingCount
              _ ->
                False
          !limitedByBudget =
            workCountMayBePositive remainingCount
              && case scOrder schedulerConfig of
                ByRuleIdThenSubstitution ->
                  remainingBudget == groupLimit
                BackoffByGroup {} ->
                  not limitedByBackoff && remainingBudget == groupLimit
                DeficitRoundRobin {} ->
                  scheduledCount == groupLimit
          !suppressedResidual =
            if limitedByBackoff then remainingCount else workCountZero
          !deferredResidual =
            if limitedByBudget then remainingCount else workCountZero
          !nextOrderState =
            orderStateAfterPull
              roundIndex
              schedulerConfig
              summary
              scheduledCount
              remainingCount
              limitedByBackoff
              orderState
          !nextCoverage =
            prCoverage pullOutcome
              <> coverageForUnpulled suppressedResidual
              <> coverageForUnpulled deferredResidual
          !cooldownUntil =
            installedCooldownUntil limitedByBackoff nextOrderState (cgsGroup summary)
       in ScheduleDelta
            { sdScheduledChunk =
                if scheduledCount == 0
                  then Nothing
                  else Just scheduledMatches,
              sdScheduledCount = scheduledCount,
              sdSuppressedCount = suppressedResidual,
              sdDeferredByBudgetCount = deferredResidual,
              sdPullMeta = prMeta pullOutcome,
              sdNextOrderState = Just nextOrderState,
              sdScheduledGroup =
                if scheduledCount == 0
                  then Nothing
                  else Just (cgsGroup summary),
              sdCoverage = nextCoverage,
              sdTrace =
                Just
                  ( traceForGroup
                      roundIndex
                      summary
                      scheduledCount
                      suppressedResidual
                      False
                      cooldownUntil
                      nextCoverage
                  )
            }
      where
        schedulerConfig =
          pgdSchedulerConfig pulledDecision
        remainingBudget =
          pgdRemainingBudget pulledDecision
        groupLimit =
          pgdGroupLimit pulledDecision
        summary =
          pgdSummary pulledDecision
        pullOutcome =
          pgdPullOutcome pulledDecision

orderStateAfterPull ::
  Ord group =>
  Int ->
  SchedulerConfig group ->
  CandidateGroupSummary group ->
  Natural ->
  WorkCount ->
  Bool ->
  ScheduleOrderState group ->
  ScheduleOrderState group
orderStateAfterPull roundIndex schedulerConfig summary scheduledCount remainingCount limitedByBackoff orderState =
  case scOrder schedulerConfig of
    ByRuleIdThenSubstitution ->
      RuleOrderState
    BackoffByGroup backoffPolicy ->
      BackoffOrderState
        ( updateGroupBackoffState
            roundIndex
            (bcCooldownRounds backoffPolicy)
            group
            scheduledCount
            remainingCount
            limitedByBackoff
            ( case orderState of
                BackoffOrderState backoffStates -> backoffStates
                _ -> Map.empty
            )
        )
    DeficitRoundRobin {} ->
      let (deficits, cursor) =
            case orderState of
              DeficitRoundRobinState currentDeficits currentCursor ->
                (currentDeficits, currentCursor)
              _ ->
                (Map.empty, Nothing)
          !remainingDeficit =
            saturatingSubtractNatural
              (Map.findWithDefault 0 group deficits)
              scheduledCount
          !nextDeficits =
            if workCountKnownZero remainingCount
              then Map.delete group deficits
              else Map.insert group remainingDeficit deficits
       in DeficitRoundRobinState nextDeficits cursor
  where
    !group = cgsGroup summary

updateGroupBackoffState ::
  Ord group =>
  Int ->
  Int ->
  group ->
  Natural ->
  WorkCount ->
  Bool ->
  Map group GroupBackoffState ->
  Map group GroupBackoffState
updateGroupBackoffState roundIndex rawInitialPenalty group scheduledCount remainingCount limitedByBackoff backoffStates
  | workCountKnownZero remainingCount =
      Map.delete group backoffStates
  | limitedByBackoff =
      case nextBackoffPenalty initialPenalty (Map.lookup group backoffStates) of
        Nothing -> Map.delete group backoffStates
        Just nextPenalty ->
          Map.insert
            group
            GroupBackoffState
              { gbsBannedUntil = roundIndex + naturalToBoundedInt nextPenalty + 1,
                gbsCooldownPenalty = nextPenalty
              }
            backoffStates
  | scheduledCount > 0 =
      recoverGroupBackoffState roundIndex initialPenalty group backoffStates
  | otherwise =
      backoffStates
  where
    !initialPenalty = fromIntegral (max 0 rawInitialPenalty)

nextBackoffPenalty :: Natural -> Maybe GroupBackoffState -> Maybe Natural
nextBackoffPenalty initialPenalty maybeBackoffState
  | initialPenalty == 0 = Nothing
  | otherwise =
      Just
        ( case maybeBackoffState of
            Nothing -> initialPenalty
            Just backoffState ->
              min
                (max initialPenalty 64)
                (max initialPenalty (2 * gbsCooldownPenalty backoffState))
        )

recoverGroupBackoffState ::
  Ord group =>
  Int ->
  Natural ->
  group ->
  Map group GroupBackoffState ->
  Map group GroupBackoffState
recoverGroupBackoffState roundIndex initialPenalty group backoffStates =
  case Map.lookup group backoffStates of
    Nothing -> backoffStates
    Just backoffState ->
      let !recoveredPenalty =
            max
              initialPenalty
              (saturatingSubtractNatural (gbsCooldownPenalty backoffState) 1)
       in if recoveredPenalty == 0
            then Map.delete group backoffStates
            else
              Map.insert
                group
                backoffState
                  { gbsBannedUntil = roundIndex,
                    gbsCooldownPenalty = recoveredPenalty
                  }
                backoffStates

installedCooldownUntil ::
  Ord group =>
  Bool ->
  ScheduleOrderState group ->
  group ->
  Maybe Int
installedCooldownUntil limitedByBackoff orderState group =
  if limitedByBackoff
    then
      case orderState of
        BackoffOrderState backoffStates -> gbsBannedUntil <$> Map.lookup group backoffStates
        _ -> Nothing
    else Nothing

emptyScheduleDelta :: Monoid meta => ScheduleDelta group meta match
emptyScheduleDelta =
  ScheduleDelta
    { sdScheduledChunk = Nothing,
      sdScheduledCount = 0,
      sdSuppressedCount = workCountZero,
      sdDeferredByBudgetCount = workCountZero,
      sdPullMeta = mempty,
      sdNextOrderState = Nothing,
      sdScheduledGroup = Nothing,
      sdCoverage = WorkCoverageComplete,
      sdTrace = Nothing
    }

applyScheduleDelta ::
  (Monoid meta, Ord group) =>
  Bool ->
  ScheduleDelta group meta match ->
  ScheduleAcc group meta match ->
  ScheduleAcc group meta match
applyScheduleDelta traceEnabled delta acc =
  let !accWithDelta =
        acc
          { sacRemainingBudget =
              saturatingSubtractNatural (sacRemainingBudget acc) (sdScheduledCount delta),
            sacScheduledChunks =
              maybe
                (sacScheduledChunks acc)
                (sacScheduledChunks acc Seq.|>)
                (sdScheduledChunk delta),
            sacScheduledCount = sacScheduledCount acc + sdScheduledCount delta,
            sacSuppressedCount = sacSuppressedCount acc <> sdSuppressedCount delta,
            sacDeferredByBudgetCount = sacDeferredByBudgetCount acc <> sdDeferredByBudgetCount delta,
            sacPullMeta = sacPullMeta acc <> sdPullMeta delta,
            sacOrderState = maybe (sacOrderState acc) id (sdNextOrderState delta),
            sacLastScheduledGroup =
              case sdScheduledGroup delta of
                Nothing -> sacLastScheduledGroup acc
                Just group -> Just group,
            sacCoverage = sacCoverage acc <> sdCoverage delta
          }
   in maybe
        accWithDelta
        (\traceEntry -> appendTraceIfEnabled traceEnabled traceEntry accWithDelta)
        (sdTrace delta)
{-# INLINABLE applyScheduleDelta #-}

availableWithinBudget :: WorkCount -> Natural -> Natural
availableWithinBudget availableCount budget =
  case workCountToMaybeExact availableCount of
    Just exactAvailable -> min exactAvailable budget
    Nothing -> budget

coverageForUnpulled :: WorkCount -> WorkCoverage
coverageForUnpulled count =
  if workCountMayBePositive count
    then WorkCoveragePartial
    else WorkCoverageComplete

traceForGroup ::
  Int ->
  CandidateGroupSummary group ->
  Natural ->
  WorkCount ->
  Bool ->
  Maybe Int ->
  WorkCoverage ->
  ScheduleTrace group
traceForGroup roundIndex summary scheduledCount suppressedCount suppressedByCooldown cooldownUntil coverage =
  ScheduleTrace
    { strRound = roundIndex,
      strGroup = cgsGroup summary,
      strMatchedCount = cgsAvailableCount summary,
      strFilteredCount = workCountZero,
      strScheduledCount = scheduledCount,
      strSuppressedCount = suppressedCount,
      strSuppressedByCooldown = suppressedByCooldown,
      strCooldownUntil = cooldownUntil,
      strCoverage = coverage
    }

appendTraceIfEnabled ::
  Bool ->
  ScheduleTrace group ->
  ScheduleAcc group meta match ->
  ScheduleAcc group meta match
appendTraceIfEnabled traceEnabled traceEntry acc =
  if traceEnabled
    then acc {sacTrace = sacTrace acc Seq.|> traceEntry}
    else acc

-- | Order group summaries by descending priority evidence, then by group.
-- Each group's priority key is computed exactly once (decorate, sort,
-- undecorate). O(n log n) comparisons on precomputed keys.
orderCandidateGroupSummaries ::
  Ord group =>
  SchedulerConfig group ->
  [CandidateGroupSummary group] ->
  [CandidateGroupSummary group]
orderCandidateGroupSummaries schedulerConfig summaries =
  fmap snd (sortBy (comparing fst) (fmap decorate summaries))
  where
    priorityProfile = scPriorityProfile schedulerConfig
    decorate summary =
      let !group = cgsGroup summary
          !priorityKey = priorityEvidenceKey (lookupPriorityEvidence group priorityProfile)
       in ((priorityKey, group), summary)
{-# INLINABLE orderCandidateGroupSummaries #-}

data OrderedGroupSelection group = OrderedGroupSelection
  { ogsSummaries :: ![CandidateGroupSummary group],
    ogsNextIndex :: !(Maybe (OrderedGroupIndex group))
  }

orderCandidateGroupSummariesWithIndex ::
  Ord group =>
  SchedulerConfig group ->
  Maybe (OrderedGroupIndex group) ->
  [CandidateGroupSummary group] ->
  OrderedGroupSelection group
orderCandidateGroupSummariesWithIndex schedulerConfig maybeIndex summaries =
  case reuseOrderedGroupIndex priorityProfile maybeIndex summaries of
    Just orderedSummaries ->
      OrderedGroupSelection
        { ogsSummaries = orderedSummaries,
          ogsNextIndex = maybeIndex
        }
    Nothing ->
      let !orderedSummaries =
            orderCandidateGroupSummaries schedulerConfig summaries
       in OrderedGroupSelection
            { ogsSummaries = orderedSummaries,
              ogsNextIndex = buildOrderedGroupIndex priorityProfile orderedSummaries
            }
  where
    !priorityProfile = scPriorityProfile schedulerConfig
{-# INLINABLE orderCandidateGroupSummariesWithIndex #-}

reuseOrderedGroupIndex ::
  Ord group =>
  PriorityProfile group ->
  Maybe (OrderedGroupIndex group) ->
  [CandidateGroupSummary group] ->
  Maybe [CandidateGroupSummary group]
reuseOrderedGroupIndex priorityProfile maybeIndex summaries =
  case maybeIndex of
    Nothing ->
      Nothing
    Just orderedIndex
      | ogiProfile orderedIndex /= priorityProfile ->
          Nothing
      | not (uniqueGroupSummaries summaries) ->
          Nothing
      | Map.size summaryMap /= length indexedGroups ->
          Nothing
      | otherwise ->
          let !orderedSummaries = mapMaybe (`Map.lookup` summaryMap) indexedGroups
           in if length orderedSummaries == length indexedGroups
                then Just orderedSummaries
                else Nothing
      where
        !indexedGroups = ogiGroups orderedIndex
        !summaryMap = summaryMapByGroup summaries
{-# INLINABLE reuseOrderedGroupIndex #-}

buildOrderedGroupIndex ::
  Ord group =>
  PriorityProfile group ->
  [CandidateGroupSummary group] ->
  Maybe (OrderedGroupIndex group)
buildOrderedGroupIndex priorityProfile summaries
  | uniqueGroupSummaries summaries =
      Just
        OrderedGroupIndex
          { ogiProfile = priorityProfile,
            ogiGroups = fmap cgsGroup summaries
          }
  | otherwise =
      Nothing
{-# INLINABLE buildOrderedGroupIndex #-}

uniqueGroupSummaries ::
  Ord group =>
  [CandidateGroupSummary group] ->
  Bool
uniqueGroupSummaries summaries =
  Map.size (summaryMapByGroup summaries) == length summaries
{-# INLINABLE uniqueGroupSummaries #-}

summaryMapByGroup ::
  Ord group =>
  [CandidateGroupSummary group] ->
  Map group (CandidateGroupSummary group)
summaryMapByGroup =
  Map.fromList . fmap (\summary -> (cgsGroup summary, summary))
{-# INLINABLE summaryMapByGroup #-}

schedulerCooldowns :: SchedulerState group -> Map group Int
schedulerCooldowns schedulerState =
  case (ssOrderState schedulerState, ssLastRound schedulerState) of
    (BackoffOrderState backoffStates, Just lastRound) ->
      Map.mapMaybe
        (positiveCooldown . (\bannedUntil -> bannedUntil - lastRound - 1) . gbsBannedUntil)
        backoffStates
    _ -> Map.empty

schedulerTrace :: SchedulerState group -> [ScheduleTrace group]
schedulerTrace = Foldable.toList . ssTrace

replaceSchedulerTraceDelta ::
  TracePolicy ->
  SchedulerState group ->
  [ScheduleTrace group] ->
  SchedulerState group ->
  SchedulerState group
replaceSchedulerTraceDelta tracePolicy previousState traceDelta nextState =
  nextState
    { ssTrace =
        retainTraceEntries
          tracePolicy
          (ssTrace previousState)
          traceDelta
    }

positiveCooldown :: Int -> Maybe Int
positiveCooldown cooldownRounds =
  if cooldownRounds > 0
    then Just cooldownRounds
    else Nothing

-- | The two-group comparison realised by 'orderCandidateGroupSummaries':
-- descending priority evidence, then ascending group. O(log n) per call in
-- the profile size.
schedulerGroupOrdering ::
  Ord group =>
  SchedulerConfig group ->
  group ->
  group ->
  Ordering
schedulerGroupOrdering schedulerConfig leftGroup rightGroup =
  let priorityProfile = scPriorityProfile schedulerConfig
   in comparePriorityEvidence
        (lookupPriorityEvidence leftGroup priorityProfile)
        (lookupPriorityEvidence rightGroup priorityProfile)
        <> compare leftGroup rightGroup

retainTraceEntries :: TracePolicy -> Seq a -> [a] -> Seq a
retainTraceEntries tracePolicy previousEntries newEntries =
  foldTracePolicy
    Seq.empty
    (\retainedCount -> retainTraceLast retainedCount previousEntries newEntries)
    (previousEntries Seq.>< Seq.fromList newEntries)
    tracePolicy

retainTraceLast :: Int -> Seq a -> [a] -> Seq a
retainTraceLast retainedEntryCount previousEntries newEntries =
  let !newEntrySeq = Seq.fromList newEntries
      !newEntryCount = Seq.length newEntrySeq
   in if newEntryCount >= retainedEntryCount
        then Seq.drop (newEntryCount - retainedEntryCount) newEntrySeq
        else
          let !combinedEntries = previousEntries Seq.>< newEntrySeq
              !dropCount = max 0 (Seq.length combinedEntries - retainedEntryCount)
           in Seq.drop dropCount combinedEntries

-- | Apply a retention policy across rounds that each carry grouped entries,
-- keeping the last @n@ entries over the whole history. O(total entries).
retainGroupedTraceEntries ::
  (round -> [entry]) ->
  ([entry] -> round -> round) ->
  TracePolicy ->
  Seq round ->
  Seq round
retainGroupedTraceEntries groupedEntries setGroupedEntries tracePolicy rounds =
  foldTracePolicy
    (fmap (setGroupedEntries []) rounds)
    (\retainedCount -> retainLastGroupedTraceEntries groupedEntries setGroupedEntries retainedCount rounds)
    rounds
    tracePolicy

retainLastGroupedTraceEntries ::
  (round -> [entry]) ->
  ([entry] -> round -> round) ->
  Int ->
  Seq round ->
  Seq round
retainLastGroupedTraceEntries groupedEntries setGroupedEntries retainedCount rounds
  | retainedCount <= 0 =
      fmap (setGroupedEntries []) rounds
  | otherwise =
      let !retention =
            Foldable.foldl'
              retainRound
              GroupedTraceRetention
                { gtrRemainingEntries = retainedCount,
                  gtrRetainedRounds = []
                }
              (reverse (Foldable.toList rounds))
       in Seq.fromList (gtrRetainedRounds retention)
  where
    retainRound retention roundValue
      | gtrRemainingEntries retention <= 0 =
          retention
            { gtrRemainingEntries = 0,
              gtrRetainedRounds =
                setGroupedEntries [] roundValue : gtrRetainedRounds retention
            }
      | otherwise =
          let scheduleEntries =
                groupedEntries roundValue
              !entryCount =
                length scheduleEntries
              !remaining =
                gtrRemainingEntries retention
           in if entryCount <= remaining
                then
                  retention
                    { gtrRemainingEntries = remaining - entryCount,
                      gtrRetainedRounds = roundValue : gtrRetainedRounds retention
                    }
                else
                  let !dropCount =
                        entryCount - remaining
                      !retainedEntries =
                        drop dropCount scheduleEntries
                   in retention
                        { gtrRemainingEntries = 0,
                          gtrRetainedRounds =
                            setGroupedEntries retainedEntries roundValue : gtrRetainedRounds retention
                        }

data GroupedTraceRetention round = GroupedTraceRetention
  { gtrRemainingEntries :: !Int,
    gtrRetainedRounds :: ![round]
  }

saturatingSubtractNatural :: Natural -> Natural -> Natural
saturatingSubtractNatural leftValue rightValue =
  if leftValue <= rightValue
    then 0
    else leftValue - rightValue
