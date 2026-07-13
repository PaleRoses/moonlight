{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Control.Gate
  ( GuideMode (..),
    GuideCheckpoint (..),
    GuidanceConfig (..),
    GuideCheckpointHit (..),
    GuideEvidence (..),
    GuideSelection (..),
    GuideRoundTrace (..),
    GateCompatibilityError (..),
    MatchDecision (..),
    MatchSelectorResult (..),
    GatePullTrace (..),
    MatchSelector (..),
    GateValidation (..),
    Gate (..),
    noSelector,
    filterSelector,
    filterGroupSelector,
    filterGroupSelectorWithTrace,
    composeSelectors,
    noGate,
    gateName,
    gateCandidateSpace,
    validateGateScheduler,
  )
where

import Data.Foldable qualified as Foldable
import Data.List (intercalate)
import Numeric.Natural (Natural)

import Moonlight.Core
  ( GuidanceConfig (..),
    GuideCheckpoint (..),
    GuideCheckpointHit (..),
    GuideEvidence (..),
    GuideMode (..),
    GuideRoundTrace (..),
    GuideSelection (..),
  )
import Moonlight.Control.Candidate
  ( CandidateCursor (..),
    CandidateGroup (..),
    CandidateGroupSummary (..),
    CandidateSpace (..),
    PullRequest (..),
    PullResult (..),
    csGroupSummaries,
    csLookupGroup,
    lengthNatural,
    pullCandidateCursor,
    pullResult,
  )
import Moonlight.Control.Count
  ( WorkCount,
    WorkCoverage (..),
    workCountExact,
    workCountKnownZero,
    workCountUnknown,
    workCountZero,
  )
import Moonlight.Control.Schedule
  ( SchedulerConfig,
  )

data GateCompatibilityError schedulerKey
  = GateRejectedScheduler !(SchedulerConfig schedulerKey)
  deriving stock (Eq, Show)

data MatchDecision traceEntry
  = MatchAccepted
  | MatchRejected !(Maybe traceEntry)
  deriving stock (Eq, Show)

data MatchSelectorResult match traceEntry = MatchSelectorResult
  { msrAcceptedMatches :: ![match],
    msrTrace :: ![traceEntry],
    msrRejectedCount :: !Natural
  }
  deriving stock (Eq, Show)

instance Semigroup (MatchSelectorResult match traceEntry) where
  leftResult <> rightResult =
    MatchSelectorResult
      { msrAcceptedMatches =
          msrAcceptedMatches leftResult <> msrAcceptedMatches rightResult,
        msrTrace =
          msrTrace leftResult <> msrTrace rightResult,
        msrRejectedCount =
          msrRejectedCount leftResult + msrRejectedCount rightResult
      }

instance Monoid (MatchSelectorResult match traceEntry) where
  mempty =
    MatchSelectorResult
      { msrAcceptedMatches = [],
        msrTrace = [],
        msrRejectedCount = 0
      }

data GatePullTrace traceEntry = GatePullTrace
  { gptRawPulledCount :: !Natural,
    gptAcceptedCount :: !Natural,
    gptRejectedCount :: !WorkCount,
    gptTrace :: ![traceEntry],
    gptCoverage :: !WorkCoverage
  }
  deriving stock (Eq, Show)

instance Semigroup (GatePullTrace traceEntry) where
  leftTrace <> rightTrace =
    GatePullTrace
      { gptRawPulledCount =
          gptRawPulledCount leftTrace + gptRawPulledCount rightTrace,
        gptAcceptedCount =
          gptAcceptedCount leftTrace + gptAcceptedCount rightTrace,
        gptRejectedCount =
          gptRejectedCount leftTrace <> gptRejectedCount rightTrace,
        gptTrace =
          gptTrace leftTrace <> gptTrace rightTrace,
        gptCoverage =
          gptCoverage leftTrace <> gptCoverage rightTrace
      }

instance Monoid (GatePullTrace traceEntry) where
  mempty =
    GatePullTrace
      { gptRawPulledCount = 0,
        gptAcceptedCount = 0,
        gptRejectedCount = workCountZero,
        gptTrace = [],
        gptCoverage = WorkCoverageComplete
      }

data MatchSelector view group match traceEntry = MatchSelector
  { matchSelectorName :: !String,
    matchSelectorPreservesCount :: !Bool,
    runMatchSelector :: view -> group -> [match] -> MatchSelectorResult match traceEntry
  }

newtype GateValidation schedulerKey = GateValidation
  { runGateValidation ::
      SchedulerConfig schedulerKey ->
      Either (GateCompatibilityError schedulerKey) ()
  }

data Gate view group match traceEntry schedulerKey = Gate
  { gateSelector :: !(MatchSelector view group match traceEntry),
    gateValidation :: !(GateValidation schedulerKey)
  }

instance Semigroup (MatchSelector view group match traceEntry) where
  (<>) = composeSelectors

instance Monoid (MatchSelector view group match traceEntry) where
  mempty = noSelector

instance Semigroup (GateValidation schedulerKey) where
  leftValidation <> rightValidation =
    GateValidation $ \schedulerConfig -> do
      runGateValidation leftValidation schedulerConfig
      runGateValidation rightValidation schedulerConfig

instance Monoid (GateValidation schedulerKey) where
  mempty = GateValidation (const (Right ()))

instance Semigroup (Gate view group match traceEntry schedulerKey) where
  leftGate <> rightGate =
    Gate
      { gateSelector =
          gateSelector leftGate <> gateSelector rightGate,
        gateValidation =
          gateValidation leftGate <> gateValidation rightGate
      }

instance Monoid (Gate view group match traceEntry schedulerKey) where
  mempty = noGate

noSelector :: MatchSelector view group match traceEntry
noSelector =
  MatchSelector
    { matchSelectorName = "",
      matchSelectorPreservesCount = True,
      runMatchSelector = \_view _group matches ->
        MatchSelectorResult
          { msrAcceptedMatches = matches,
            msrTrace = [],
            msrRejectedCount = 0
          }
    }

filterSelector ::
  String ->
  (view -> match -> Bool) ->
  MatchSelector view group match traceEntry
filterSelector selectorName keep =
  filterGroupSelector selectorName (\view _group match -> keep view match)

filterGroupSelector ::
  String ->
  (view -> group -> match -> Bool) ->
  MatchSelector view group match traceEntry
filterGroupSelector selectorName keep =
  MatchSelector
    { matchSelectorName = selectorName,
      matchSelectorPreservesCount = False,
      runMatchSelector =
        selectMatchesByDecision
          ( \view group match ->
              if keep view group match
                then MatchAccepted
                else MatchRejected Nothing
          )
    }

filterGroupSelectorWithTrace ::
  String ->
  (view -> group -> match -> Either traceEntry ()) ->
  MatchSelector view group match traceEntry
filterGroupSelectorWithTrace selectorName decide =
  MatchSelector
    { matchSelectorName = selectorName,
      matchSelectorPreservesCount = False,
      runMatchSelector =
        selectMatchesByDecision
          ( \view group match ->
              case decide view group match of
                Right () -> MatchAccepted
                Left traceEntry -> MatchRejected (Just traceEntry)
          )
    }

composeSelectors ::
  MatchSelector view group match traceEntry ->
  MatchSelector view group match traceEntry ->
  MatchSelector view group match traceEntry
composeSelectors leftSelector rightSelector =
  MatchSelector
    { matchSelectorName =
        composeSelectorName
          (matchSelectorName leftSelector)
          (matchSelectorName rightSelector),
      matchSelectorPreservesCount =
        matchSelectorPreservesCount leftSelector
          && matchSelectorPreservesCount rightSelector,
      runMatchSelector =
        \view group matches ->
          let leftResult =
                runMatchSelector leftSelector view group matches
              rightResult =
                runMatchSelector
                  rightSelector
                  view
                  group
                  (msrAcceptedMatches leftResult)
           in MatchSelectorResult
                { msrAcceptedMatches =
                    msrAcceptedMatches rightResult,
                  msrTrace =
                    msrTrace leftResult <> msrTrace rightResult,
                  msrRejectedCount =
                    msrRejectedCount leftResult + msrRejectedCount rightResult
                }
    }

noGate :: Gate view group match traceEntry schedulerKey
noGate =
  Gate
    { gateSelector = noSelector,
      gateValidation = mempty
    }

gateName :: Gate view group match traceEntry schedulerKey -> String
gateName = matchSelectorName . gateSelector

gateCandidateSpace ::
  Monad m =>
  Gate view group match traceEntry schedulerKey ->
  view ->
  CandidateSpace m group sourceMeta match ->
  CandidateSpace m group (GatePullTrace traceEntry) match
gateCandidateSpace guidance view sourceSpace =
  CandidateSpace
    { csGroupSummaries =
        fmap
          (fmap (gateGroupSummary selector))
          (csGroupSummaries sourceSpace),
      csLookupGroup =
        \group ->
          fmap
            (fmap (gateCandidateGroup selector view group))
            (csLookupGroup sourceSpace group)
    }
  where
    selector = gateSelector guidance

gateGroupSummary ::
  MatchSelector view group match traceEntry ->
  CandidateGroupSummary group ->
  CandidateGroupSummary group
gateGroupSummary selector summary =
  summary
    { cgsAvailableCount =
        gateAvailableCount
          (matchSelectorPreservesCount selector)
          (cgsAvailableCount summary)
    }

gateAvailableCount :: Bool -> WorkCount -> WorkCount
gateAvailableCount preservesCount rawCount
  | preservesCount = rawCount
  | workCountKnownZero rawCount = workCountZero
  | otherwise = workCountUnknown

gateCandidateGroup ::
  Monad m =>
  MatchSelector view group match traceEntry ->
  view ->
  group ->
  CandidateGroup m sourceMeta match ->
  CandidateGroup m (GatePullTrace traceEntry) match
gateCandidateGroup selector view group sourceGroup =
  CandidateGroup
    { cgAvailableCount =
        fmap
          (gateAvailableCount (matchSelectorPreservesCount selector))
          (cgAvailableCount sourceGroup),
      cgOpenCursor =
        fmap
          (gateCandidateCursor selector view group)
          (cgOpenCursor sourceGroup)
    }

gateCandidateCursor ::
  Monad m =>
  MatchSelector view group match traceEntry ->
  view ->
  group ->
  CandidateCursor m sourceMeta match ->
  CandidateCursor m (GatePullTrace traceEntry) match
gateCandidateCursor selector view group sourceCursor
  | matchSelectorPreservesCount selector =
      gateCountPreservingCursor selector view group sourceCursor
gateCandidateCursor selector view group sourceCursor =
  CandidateCursor $ \request ->
    if pullRequestLimit request == 0
      then
        pure
          ( pullResult
              []
              mempty
              workCountUnknown
              WorkCoveragePartial
              (Just (gateCandidateCursor selector view group sourceCursor))
          )
      else do
        sourceResult <- pullCandidateCursor sourceCursor request
        let selected =
              runMatchSelector selector view group (prMatches sourceResult)
            nextCursor =
              fmap
                (gateCandidateCursor selector view group)
                (prNextCursor sourceResult)
            remainingCount =
              maybe workCountZero (const workCountUnknown) nextCursor
            coverage =
              prCoverage sourceResult
                <> maybe WorkCoverageComplete (const WorkCoveragePartial) nextCursor
        pure
          ( pullResult
              (msrAcceptedMatches selected)
              GatePullTrace
                { gptRawPulledCount = prPulledCount sourceResult,
                  gptAcceptedCount = lengthNatural (msrAcceptedMatches selected),
                  gptRejectedCount = workCountExact (msrRejectedCount selected),
                  gptTrace = msrTrace selected,
                  gptCoverage = coverage
                }
              remainingCount
              coverage
              nextCursor
          )

gateCountPreservingCursor ::
  Monad m =>
  MatchSelector view group match traceEntry ->
  view ->
  group ->
  CandidateCursor m sourceMeta match ->
  CandidateCursor m (GatePullTrace traceEntry) match
gateCountPreservingCursor selector view group sourceCursor =
  CandidateCursor $ \request -> do
    sourceResult <- pullCandidateCursor sourceCursor request
    pure
      PullResult
        { prMatches = prMatches sourceResult,
          prPulledCount = prPulledCount sourceResult,
          prMeta =
            GatePullTrace
              { gptRawPulledCount = prPulledCount sourceResult,
                gptAcceptedCount = prPulledCount sourceResult,
                gptRejectedCount = workCountZero,
                gptTrace = [],
                gptCoverage = prCoverage sourceResult
              },
          prRemainingCount = prRemainingCount sourceResult,
          prCoverage = prCoverage sourceResult,
          prNextCursor =
            fmap
              (gateCountPreservingCursor selector view group)
              (prNextCursor sourceResult)
        }

data SelectedChunk match traceEntry = SelectedChunk
  { selectedAcceptedReversed :: ![match],
    selectedTraceReversed :: ![traceEntry],
    selectedRejectedCount :: !Natural
  }

selectMatchesByDecision ::
  (view -> group -> match -> MatchDecision traceEntry) ->
  view ->
  group ->
  [match] ->
  MatchSelectorResult match traceEntry
selectMatchesByDecision decide view group =
  finishDecisionSelection
    . Foldable.foldl' selectOne
    SelectedChunk
      { selectedAcceptedReversed = [],
        selectedTraceReversed = [],
        selectedRejectedCount = 0
      }
  where
    selectOne selected match =
      case decide view group match of
        MatchAccepted ->
          selected
            { selectedAcceptedReversed =
                match : selectedAcceptedReversed selected
            }
        MatchRejected maybeTraceEntry ->
          selected
            { selectedTraceReversed =
                maybe
                  (selectedTraceReversed selected)
                  (: selectedTraceReversed selected)
                  maybeTraceEntry,
              selectedRejectedCount =
                selectedRejectedCount selected + 1
            }

    finishDecisionSelection ::
      SelectedChunk match traceEntry ->
      MatchSelectorResult match traceEntry
    finishDecisionSelection selected =
      MatchSelectorResult
        { msrAcceptedMatches =
            reverse (selectedAcceptedReversed selected),
          msrTrace =
            reverse (selectedTraceReversed selected),
          msrRejectedCount =
            selectedRejectedCount selected
        }

validateGateScheduler ::
  Gate view group match traceEntry schedulerKey ->
  SchedulerConfig schedulerKey ->
  Either (GateCompatibilityError schedulerKey) ()
validateGateScheduler = runGateValidation . gateValidation

composeSelectorName :: String -> String -> String
composeSelectorName leftName rightName =
  intercalate " > " (filter (not . null) [leftName, rightName])
