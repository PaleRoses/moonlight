{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Runtime.Carrier.Schedule
  ( MatchAdmissionGate (..),
    trivialAdmissionGate,
    candidateSpaceForSupportedMatches,
    scheduleGatedSupportedMatches,
    scheduleSupportedMatches,
    scheduleRoundSupportedMatches,
    scheduleRefinedRoundSupportedMatches,
    supportedMatchRuleKey,
    compareSupportedMatches,
  )
where

import Data.Foldable qualified as Foldable
import Data.Functor.Identity
  ( Identity,
    runIdentity,
  )
import Data.List
  ( sortBy,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Ord
  ( comparing,
  )
import Data.Vector qualified as Vector
import Numeric.Natural
  ( Natural,
  )
import Moonlight.Control.Gate
  ( Gate (..),
    GatePullTrace (..),
    MatchSelector (..),
    MatchSelectorResult (..),
    gateCandidateSpace,
  )
import Moonlight.Control.Candidate
  ( CandidateSpace,
    ScheduledBatch (..),
    ScheduledMatch (..),
    candidateSpaceAvailableCount,
    finiteCandidateSpace,
    scheduledBatchCount,
    scheduledBatchMatches,
  )
import Moonlight.Control.Count
  ( WorkCount,
    WorkCoverage (..),
    naturalToBoundedInt,
    workCountFromInt,
    workCountLowerBound,
    workCountLowerBoundToBoundedInt,
    workCountMinusExactLowerBound,
  )
import Moonlight.Control.Schedule
  ( SchedulerConfig,
    SchedulerRefinement,
    applySchedulerRefinement,
  )
import Moonlight.Control.Schedule.Round
  ( ScheduleOutcome (..),
    ScheduleTrace (..),
    SchedulerState,
    replaceSchedulerTraceDelta,
    scheduleCandidateSpace,
  )
import Moonlight.Core (Substitution)
import Moonlight.Saturation.Context.Program.View
  ( SaturationRoundView (..),
  )
import Moonlight.Saturation.Context.Runtime.Match.Batch
  ( MatchBatch,
    matchBatchFromList,
    matchBatchToList,
  )
import Moonlight.Saturation.Context.Runtime.Match.Pipeline
  ( CandidatePipelineCounts,
    candidatePipelineIncrement,
    candidatePipelineIncrementGroup,
    emptyCandidatePipelineCounts,
    nonNegativeDifference,
  )
import Moonlight.Saturation.Context.Runtime.Match.Pipeline qualified as Pipeline
import Moonlight.Saturation.Context.Runtime.Schedule.Decision
  ( RuntimeScheduleDecision (..),
  )
import Moonlight.Saturation.Context.Runtime.State
  ( RuntimeState (..),
  )
import Moonlight.Saturation.Substrate

data MatchAdmissionGate u match reject measure = MatchAdmissionGate
  { magMeasure ::
      SatRewriteContext u ->
      SatFactStore u ->
      SatGraph u ->
      SatMatchState u ->
      match ->
      Either reject measure,
    magFitsRound ::
      SaturationRoundView u ->
      measure ->
      Bool
  }

data CandidateGateOutcome reject measure match
  = CandidateRejectedCheap !reject !match
  | CandidateDeferredByBudget !measure !match
  | CandidateAdmitted !measure !match
  deriving stock (Eq, Show)

trivialAdmissionGate :: MatchAdmissionGate u match () ()
trivialAdmissionGate =
  MatchAdmissionGate
    { magMeasure = \_rewriteContext _factStore _graph _matchState _match -> Right (),
      magFitsRound = \_roundView _measure -> True
    }
{-# INLINE trivialAdmissionGate #-}

data AdmissionTrace = AdmissionTrace
  { atRejectedCheapCount :: !Natural,
    atDeferredByBudgetCount :: !Natural,
    atAdmittedCount :: !Natural
  }
  deriving stock (Eq, Show)

instance Semigroup AdmissionTrace where
  leftTrace <> rightTrace =
    AdmissionTrace
      { atRejectedCheapCount = atRejectedCheapCount leftTrace + atRejectedCheapCount rightTrace,
        atDeferredByBudgetCount = atDeferredByBudgetCount leftTrace + atDeferredByBudgetCount rightTrace,
        atAdmittedCount = atAdmittedCount leftTrace + atAdmittedCount rightTrace
      }

instance Monoid AdmissionTrace where
  mempty =
    AdmissionTrace
      { atRejectedCheapCount = 0,
        atDeferredByBudgetCount = 0,
        atAdmittedCount = 0
      }

admissionTraceObservedCount :: AdmissionTrace -> Natural
admissionTraceObservedCount traceValue =
  atRejectedCheapCount traceValue
    + atDeferredByBudgetCount traceValue
    + atAdmittedCount traceValue
{-# INLINE admissionTraceObservedCount #-}

admissionTraceRejectedCount :: AdmissionTrace -> Natural
admissionTraceRejectedCount traceValue =
  atRejectedCheapCount traceValue
    + atDeferredByBudgetCount traceValue
{-# INLINE admissionTraceRejectedCount #-}

traceAdmitted :: Natural -> AdmissionTrace
traceAdmitted countValue =
  mempty {atAdmittedCount = countValue}
{-# INLINE traceAdmitted #-}

data AdmissionSelection match = AdmissionSelection
  { asAcceptedReverse :: ![match],
    asTrace :: !AdmissionTrace
  }

emptyAdmissionSelection :: AdmissionSelection match
emptyAdmissionSelection =
  AdmissionSelection
    { asAcceptedReverse = [],
      asTrace = mempty
    }
{-# INLINE emptyAdmissionSelection #-}

admitCandidate ::
  forall u reject measure match.
  MatchAdmissionGate u match reject measure ->
  SatRewriteContext u ->
  SaturationRoundView u ->
  SatMatchState u ->
  match ->
  CandidateGateOutcome reject measure match
admitCandidate gate rewriteContext roundView matchState match =
  case magMeasure gate rewriteContext (srvFacts roundView) (srvGraph roundView) matchState match of
    Left reject ->
      CandidateRejectedCheap reject match
    Right measure
      | magFitsRound gate roundView measure ->
          CandidateAdmitted measure match
      | otherwise ->
          CandidateDeferredByBudget measure match
{-# INLINE admitCandidate #-}

selectAdmissionMatches ::
  forall u reject measure match.
  MatchAdmissionGate u match reject measure ->
  SatRewriteContext u ->
  SatMatchState u ->
  SaturationRoundView u ->
  [match] ->
  MatchSelectorResult match AdmissionTrace
selectAdmissionMatches gate rewriteContext matchState roundView =
  finishAdmissionSelection
    . Foldable.foldl' selectOne emptyAdmissionSelection
  where
    selectOne :: AdmissionSelection match -> match -> AdmissionSelection match
    selectOne selection match =
      case admitCandidate gate rewriteContext roundView matchState match of
        CandidateRejectedCheap _reject _rejectedMatch ->
          selection
            { asTrace =
                (asTrace selection)
                  { atRejectedCheapCount = atRejectedCheapCount (asTrace selection) + 1
                  }
            }
        CandidateDeferredByBudget _measure _deferredMatch ->
          selection
            { asTrace =
                (asTrace selection)
                  { atDeferredByBudgetCount = atDeferredByBudgetCount (asTrace selection) + 1
                  }
            }
        CandidateAdmitted _measure admittedMatch ->
          selection
            { asAcceptedReverse = admittedMatch : asAcceptedReverse selection,
              asTrace =
                (asTrace selection)
                  { atAdmittedCount = atAdmittedCount (asTrace selection) + 1
                  }
            }

    finishAdmissionSelection :: AdmissionSelection match -> MatchSelectorResult match AdmissionTrace
    finishAdmissionSelection selection =
      let traceValue =
            asTrace selection
       in MatchSelectorResult
            { msrAcceptedMatches = reverse (asAcceptedReverse selection),
              msrTrace =
                if admissionTraceObservedCount traceValue == 0
                  then []
                  else [traceValue],
              msrRejectedCount = admissionTraceRejectedCount traceValue
            }
{-# INLINE selectAdmissionMatches #-}

admissionGuidance ::
  forall u schedulerGroup reject measure.
  MatchAdmissionGate u (SatSupportedMatch u) reject measure ->
  SatRewriteContext u ->
  SatMatchState u ->
  Gate
    (SaturationRoundView u)
    schedulerGroup
    (SatSupportedMatch u)
    AdmissionTrace
    schedulerGroup
admissionGuidance gate rewriteContext matchState =
  Gate
    { gateSelector =
        MatchSelector
          { matchSelectorName = "admission",
            matchSelectorPreservesCount = False,
            runMatchSelector = \roundView _group matches ->
              selectAdmissionMatches gate rewriteContext matchState roundView matches
          },
      gateValidation = mempty
    }
{-# INLINE admissionGuidance #-}

candidateSpaceForSupportedMatches ::
  forall u group.
  Ord group =>
  (SatSupportedMatch u -> group) ->
  (SatSupportedMatch u -> SatSupportedMatch u -> Ordering) ->
  MatchBatch (SatSupportedMatch u) ->
  CandidateSpace Identity group () (SatSupportedMatch u)
candidateSpaceForSupportedMatches groupOf compareMatches matches =
  finiteCandidateSpace
    (fmap (\match -> (groupOf match, [match])) orderedMatches)
  where
    orderedMatches =
      sortBy compareMatches (matchBatchToList matches)
{-# INLINE candidateSpaceForSupportedMatches #-}

positiveDifference :: Int -> Int -> Maybe Int
positiveDifference leftCount rightCount =
  let !difference =
        leftCount - rightCount
   in if difference > 0
        then Just difference
        else Nothing
{-# INLINE positiveDifference #-}

countScheduledBatchBy ::
  Ord group =>
  ScheduledBatch group match ->
  Map group Int
countScheduledBatchBy =
  Foldable.foldl'
    ( \counts scheduledMatch ->
        Map.insertWith (+) (smGroup scheduledMatch) 1 counts
    )
    Map.empty
    . scheduledBatchMatchesWithGroups
{-# INLINE countScheduledBatchBy #-}

validationRejectedCounts ::
  Ord group =>
  ScheduledBatch group match ->
  ScheduledBatch group match ->
  Map group Int
validationRejectedCounts scheduledBatch validatedBatch =
  Map.differenceWith
    positiveDifference
    (countScheduledBatchBy scheduledBatch)
    (countScheduledBatchBy validatedBatch)
{-# INLINE validationRejectedCounts #-}

validateScheduledBatch ::
  forall u group.
  MatchingBackend u =>
  SatRewriteContext u ->
  SaturationRoundView u ->
  SatMatchState u ->
  ScheduledBatch group (SatSupportedMatch u) ->
  ScheduledBatch group (SatSupportedMatch u)
validateScheduledBatch rewriteContext roundView matchState scheduledBatch =
  ScheduledBatch
    ( fmap fst
        ( filterSupportedMatches
            @u
            rewriteContext
            (srvFacts roundView)
            matchState
            ( fmap
                (\scheduledMatch -> (scheduledMatch, smMatch scheduledMatch))
                (scheduledBatchMatchesWithGroups scheduledBatch)
            )
            (srvGraph roundView)
        )
    )
{-# INLINE validateScheduledBatch #-}

annotateScheduleTrace ::
  Ord group =>
  Map group Int ->
  Map group Int ->
  ScheduleTrace group ->
  ScheduleTrace group
annotateScheduleTrace validationAcceptedCounts validationRejectedCountsByGroup traceEntry =
  let group =
        strGroup traceEntry
      !validationRejectedCount =
        Map.findWithDefault 0 group validationRejectedCountsByGroup
      !scheduledCount =
        if validationRejectedCount > 0 || Map.member group validationAcceptedCounts
          then fromIntegral (max 0 (Map.findWithDefault 0 group validationAcceptedCounts))
          else strScheduledCount traceEntry
   in traceEntry
        { strFilteredCount =
            strFilteredCount traceEntry <> workCountFromInt validationRejectedCount,
          strScheduledCount = scheduledCount
        }
{-# INLINE annotateScheduleTrace #-}

schedulerNotSelectedCountsByGroup ::
  Ord group =>
  [ScheduleTrace group] ->
  Map group Int
schedulerNotSelectedCountsByGroup =
  Foldable.foldl'
    ( \counts traceEntry ->
        Map.insertWith
          (+)
          (strGroup traceEntry)
          ( workCountLowerBoundToBoundedInt
              (workCountMinusExactLowerBound (strMatchedCount traceEntry) (strScheduledCount traceEntry))
          )
          counts
    )
    Map.empty
{-# INLINE schedulerNotSelectedCountsByGroup #-}

candidatePipelineFromSchedule ::
  forall group meta match.
  Ord group =>
  WorkCount ->
  AdmissionTrace ->
  ScheduleOutcome group meta match ->
  ScheduledBatch group match ->
  Map group Int ->
  CandidatePipelineCounts group
candidatePipelineFromSchedule guidedAvailableCount admissionTrace scheduledOutcome validatedScheduledBatch validationRejectedCountsByGroup =
  let !guidedCount =
        workCountLowerBoundToBoundedInt guidedAvailableCount
      !admittedCount =
        naturalToBoundedInt (atAdmittedCount admissionTrace)
      !preScheduledCount =
        naturalToBoundedInt (scheduledBatchCount (soScheduledBatch scheduledOutcome))
      !validatedCount =
        naturalToBoundedInt (scheduledBatchCount validatedScheduledBatch)
      !rejectedByValidationCount =
        nonNegativeDifference preScheduledCount validatedCount
      !notSelectedBySchedulerCount =
        workCountLowerBoundToBoundedInt
          (soSuppressedCount scheduledOutcome <> soDeferredByBudgetCount scheduledOutcome)
      validationAcceptedCounts =
        countScheduledBatchBy validatedScheduledBatch
      notSelectedCountsByGroup =
        schedulerNotSelectedCountsByGroup (soSchedulerTraceDelta scheduledOutcome)
   in foldr
        ($)
        emptyCandidatePipelineCounts
        ( [ candidatePipelineIncrement Pipeline.CandidateGuided guidedCount,
            candidatePipelineIncrement
              Pipeline.CandidateRejectedByAdmission
              (naturalToBoundedInt (atRejectedCheapCount admissionTrace)),
            candidatePipelineIncrement
              Pipeline.CandidateDeferredByBudget
              (naturalToBoundedInt (atDeferredByBudgetCount admissionTrace)),
            candidatePipelineIncrement Pipeline.CandidateAdmitted admittedCount,
            candidatePipelineIncrement Pipeline.CandidateScheduledBeforeValidation preScheduledCount,
            candidatePipelineIncrement Pipeline.CandidateNotSelectedByScheduler notSelectedBySchedulerCount,
            candidatePipelineIncrement Pipeline.CandidateRejectedByValidation rejectedByValidationCount,
            candidatePipelineIncrement Pipeline.CandidateScheduled validatedCount
          ]
            <> fmap groupAccepted (Map.toAscList validationAcceptedCounts)
            <> fmap groupRejected (Map.toAscList validationRejectedCountsByGroup)
            <> fmap groupNotSelected (Map.toAscList notSelectedCountsByGroup)
        )
  where
    groupAccepted ::
      (group, Int) ->
      CandidatePipelineCounts group ->
      CandidatePipelineCounts group
    groupAccepted (group, countValue) =
      candidatePipelineIncrementGroup group Pipeline.CandidateScheduled countValue

    groupRejected ::
      (group, Int) ->
      CandidatePipelineCounts group ->
      CandidatePipelineCounts group
    groupRejected (group, countValue) =
      candidatePipelineIncrementGroup group Pipeline.CandidateRejectedByValidation countValue

    groupNotSelected ::
      (group, Int) ->
      CandidatePipelineCounts group ->
      CandidatePipelineCounts group
    groupNotSelected (group, countValue) =
      candidatePipelineIncrementGroup group Pipeline.CandidateNotSelectedByScheduler countValue
{-# INLINE candidatePipelineFromSchedule #-}

runtimeDecisionFromScheduleOutcome ::
  forall u schedulerGroup meta.
  ( MatchingBackend u,
    Ord schedulerGroup
  ) =>
  SatRewriteContext u ->
  SchedulerState schedulerGroup ->
  SaturationRoundView u ->
  SatMatchState u ->
  WorkCount ->
  AdmissionTrace ->
  ScheduleOutcome schedulerGroup meta (SatSupportedMatch u) ->
  RuntimeScheduleDecision schedulerGroup (SatSupportedMatch u)
runtimeDecisionFromScheduleOutcome rewriteContext schedulerState roundView matchState guidedAvailableCount admissionTrace scheduledOutcome =
  let validatedScheduledBatch =
        validateScheduledBatch @u
          rewriteContext
          roundView
          matchState
          (soScheduledBatch scheduledOutcome)
      validatedScheduledMatches =
        matchBatchFromList (scheduledBatchMatches validatedScheduledBatch)
      validationAcceptedCounts =
        countScheduledBatchBy validatedScheduledBatch
      validationRejectedCountsByGroup =
        validationRejectedCounts (soScheduledBatch scheduledOutcome) validatedScheduledBatch
      adjustedTraceDelta =
        fmap
          ( annotateScheduleTrace
              validationAcceptedCounts
              validationRejectedCountsByGroup
          )
          (soSchedulerTraceDelta scheduledOutcome)
      adjustedSchedulerState =
        replaceSchedulerTraceDelta
          (soTracePolicy scheduledOutcome)
          schedulerState
          adjustedTraceDelta
          (soSchedulerState scheduledOutcome)
      pipelineCounts =
        candidatePipelineFromSchedule
          guidedAvailableCount
          admissionTrace
          scheduledOutcome
          validatedScheduledBatch
          validationRejectedCountsByGroup
   in RuntimeScheduleDecision
        { rsdScheduledMatches = validatedScheduledMatches,
          rsdSchedulerState = adjustedSchedulerState,
          rsdTracePolicy = soTracePolicy scheduledOutcome,
          rsdTraceDelta = Vector.fromList adjustedTraceDelta,
          rsdAllCandidatesScheduled =
            soCoverage scheduledOutcome == WorkCoverageComplete
              && atDeferredByBudgetCount admissionTrace == 0,
          rsdPipelineCounts = pipelineCounts
        }
{-# INLINE runtimeDecisionFromScheduleOutcome #-}

scheduleGatedSupportedMatches ::
  forall u schedulerGroup reject measure.
  ( MatchingBackend u,
    Ord schedulerGroup
  ) =>
  MatchAdmissionGate u (SatSupportedMatch u) reject measure ->
  SatRewriteContext u ->
  SchedulerConfig schedulerGroup ->
  SaturationRoundView u ->
  SchedulerState schedulerGroup ->
  SatMatchState u ->
  CandidateSpace Identity schedulerGroup () (SatSupportedMatch u) ->
  RuntimeScheduleDecision schedulerGroup (SatSupportedMatch u)
scheduleGatedSupportedMatches gate rewriteContext schedulerConfig roundView schedulerState matchState candidateSpace =
  let guidedAvailableCount =
        runIdentity (candidateSpaceAvailableCount candidateSpace)
      admittedCandidateSpace =
        gateCandidateSpace
          (admissionGuidance @u gate rewriteContext matchState)
          roundView
          candidateSpace
      scheduledOutcome =
        runIdentity
          ( scheduleCandidateSpace
              schedulerConfig
              (workCountLowerBound guidedAvailableCount)
              (srvIteration roundView)
              admittedCandidateSpace
              schedulerState
          )
      admissionTrace =
        foldMap id (gptTrace (soPullMeta scheduledOutcome))
   in runtimeDecisionFromScheduleOutcome
        @u
        rewriteContext
        schedulerState
        roundView
        matchState
        guidedAvailableCount
        admissionTrace
        scheduledOutcome
{-# INLINE scheduleGatedSupportedMatches #-}

scheduleSupportedMatches ::
  forall u schedulerGroup.
  ( MatchingBackend u,
    Ord schedulerGroup
  ) =>
  SatRewriteContext u ->
  SchedulerConfig schedulerGroup ->
  SaturationRoundView u ->
  SchedulerState schedulerGroup ->
  SatMatchState u ->
  CandidateSpace Identity schedulerGroup () (SatSupportedMatch u) ->
  RuntimeScheduleDecision schedulerGroup (SatSupportedMatch u)
scheduleSupportedMatches rewriteContext schedulerConfig roundView schedulerState matchState candidateSpace =
  let guidedAvailableCount =
        runIdentity (candidateSpaceAvailableCount candidateSpace)
      scheduledOutcome =
        runIdentity
          ( scheduleCandidateSpace
              schedulerConfig
              (workCountLowerBound guidedAvailableCount)
              (srvIteration roundView)
              candidateSpace
              schedulerState
          )
      admissionTrace =
        traceAdmitted (workCountLowerBound guidedAvailableCount)
   in runtimeDecisionFromScheduleOutcome
        @u
        rewriteContext
        schedulerState
        roundView
        matchState
        guidedAvailableCount
        admissionTrace
        scheduledOutcome
{-# INLINE scheduleSupportedMatches #-}

scheduleRoundSupportedMatches ::
  forall u carrier schedulerGroup.
  ( MatchingBackend u,
    Ord schedulerGroup
  ) =>
  SchedulerConfig schedulerGroup ->
  SatRewriteContext u ->
  SaturationRoundView u ->
  CandidateSpace Identity schedulerGroup () (SatSupportedMatch u) ->
  RuntimeState u carrier schedulerGroup ->
  RuntimeScheduleDecision schedulerGroup (SatSupportedMatch u)
scheduleRoundSupportedMatches schedulerConfig rewriteContext roundView candidateSpace state =
  scheduleSupportedMatches
    @u
    rewriteContext
    schedulerConfig
    roundView
    (rsScheduler state)
    (rsMatchState state)
    candidateSpace
{-# INLINE scheduleRoundSupportedMatches #-}

scheduleRefinedRoundSupportedMatches ::
  forall u carrier schedulerGroup.
  ( MatchingBackend u,
    Ord schedulerGroup
  ) =>
  SchedulerRefinement (RuntimeState u carrier schedulerGroup) schedulerGroup ->
  SchedulerConfig schedulerGroup ->
  SatRewriteContext u ->
  SaturationRoundView u ->
  CandidateSpace Identity schedulerGroup () (SatSupportedMatch u) ->
  RuntimeState u carrier schedulerGroup ->
  RuntimeScheduleDecision schedulerGroup (SatSupportedMatch u)
scheduleRefinedRoundSupportedMatches schedulerRefinement schedulerConfig rewriteContext roundView candidateSpace state =
  scheduleRoundSupportedMatches
    @u
    (applySchedulerRefinement schedulerRefinement state schedulerConfig)
    rewriteContext
    roundView
    candidateSpace
    state
{-# INLINE scheduleRefinedRoundSupportedMatches #-}

supportedMatchRuleKey ::
  forall u.
  MatchView u =>
  SatSupportedMatch u ->
  SatRuleKey u
supportedMatchRuleKey =
  matchRuleKey @u . supportedMatchInner @u
{-# INLINE supportedMatchRuleKey #-}

supportedMatchOrderKey ::
  forall u.
  MatchView u =>
  SatSupportedMatch u ->
  (SatRuleKey u, SatClassId u, Substitution)
supportedMatchOrderKey =
  matchKey @u . supportedMatchInner @u

compareSupportedMatches ::
  forall u.
  (MatchView u, Ord (SatRuleKey u), Ord (SatClassId u)) =>
  SatSupportedMatch u ->
  SatSupportedMatch u ->
  Ordering
compareSupportedMatches =
  comparing (supportedMatchOrderKey @u)
{-# INLINE compareSupportedMatches #-}
