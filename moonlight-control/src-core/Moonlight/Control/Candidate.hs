{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Control.Candidate
  ( CandidateGroupSummary (..),
    PullRequest (..),
    pullRequest,
    PullResult (..),
    pullResult,
    CandidateCursor (..),
    pullCandidateCursor,
    CandidateGroup (..),
    CandidateSpace (..),
    candidateSpaceAvailableCount,
    ScheduledMatch (..),
    ScheduledBatch (..),
    emptyScheduledBatch,
    scheduledBatchMatches,
    scheduledBatchCount,
    finiteCandidateSpace,
    lengthNatural,
  )
where

import Data.Foldable qualified as Foldable
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Numeric.Natural (Natural)

import Moonlight.Control.Count
  ( WorkCount,
    WorkCoverage (..),
    workCountExact,
    workCoverageFromRemaining,
  )

type CandidateGroupSummary :: Type -> Type
data CandidateGroupSummary group = CandidateGroupSummary
  { cgsGroup :: !group,
    cgsAvailableCount :: !WorkCount
  }
  deriving stock (Eq, Show, Read)

type PullRequest :: Type
data PullRequest = PullRequest
  { pullRequestLimit :: !Natural
  }
  deriving stock (Eq, Ord, Show, Read)

pullRequest :: Natural -> PullRequest
pullRequest limit = PullRequest {pullRequestLimit = limit}

type CandidateCursor :: (Type -> Type) -> Type -> Type -> Type
newtype CandidateCursor m meta match = CandidateCursor
  { runCandidateCursor :: PullRequest -> m (PullResult m meta match)
  }

pullCandidateCursor ::
  CandidateCursor m meta match ->
  PullRequest ->
  m (PullResult m meta match)
pullCandidateCursor = runCandidateCursor

type PullResult :: (Type -> Type) -> Type -> Type -> Type
data PullResult m meta match = PullResult
  { prMatches :: ![match],
    prPulledCount :: !Natural,
    prMeta :: !meta,
    prRemainingCount :: !WorkCount,
    prCoverage :: !WorkCoverage,
    prNextCursor :: !(Maybe (CandidateCursor m meta match))
  }

pullResult ::
  [match] ->
  meta ->
  WorkCount ->
  WorkCoverage ->
  Maybe (CandidateCursor m meta match) ->
  PullResult m meta match
pullResult matches meta remainingCount coverage nextCursor =
  PullResult
    { prMatches = matches,
      prPulledCount = lengthNatural matches,
      prMeta = meta,
      prRemainingCount = remainingCount,
      prCoverage = coverage,
      prNextCursor = nextCursor
    }

type CandidateGroup :: (Type -> Type) -> Type -> Type -> Type
data CandidateGroup m meta match = CandidateGroup
  { cgAvailableCount :: m WorkCount,
    cgOpenCursor :: m (CandidateCursor m meta match)
  }

type CandidateSpace :: (Type -> Type) -> Type -> Type -> Type -> Type
data CandidateSpace m group meta match = CandidateSpace
  { csGroupSummaries :: m [CandidateGroupSummary group],
    csLookupGroup :: group -> m (Maybe (CandidateGroup m meta match))
  }

candidateSpaceAvailableCount ::
  Monad m =>
  CandidateSpace m group meta match ->
  m WorkCount
candidateSpaceAvailableCount candidateSpace =
  fmap
    (Foldable.foldl' (\total summary -> total <> cgsAvailableCount summary) mempty)
    (csGroupSummaries candidateSpace)

type ScheduledMatch :: Type -> Type -> Type
data ScheduledMatch group match = ScheduledMatch
  { smGroup :: !group,
    smMatch :: !match
  }
  deriving stock (Eq, Show, Read)

type ScheduledBatch :: Type -> Type -> Type
newtype ScheduledBatch group match = ScheduledBatch
  { scheduledBatchMatchesWithGroups :: [ScheduledMatch group match]
  }
  deriving stock (Eq, Show, Read)

emptyScheduledBatch :: ScheduledBatch group match
emptyScheduledBatch = ScheduledBatch []

scheduledBatchMatches :: ScheduledBatch group match -> [match]
scheduledBatchMatches = fmap smMatch . scheduledBatchMatchesWithGroups

scheduledBatchCount :: ScheduledBatch group match -> Natural
scheduledBatchCount = lengthNatural . scheduledBatchMatchesWithGroups

finiteCandidateSpace ::
  (Applicative m, Monoid meta, Ord group) =>
  [(group, [match])] ->
  CandidateSpace m group meta match
finiteCandidateSpace rawGroups =
  CandidateSpace
    { csGroupSummaries =
        pure
          [ CandidateGroupSummary
              { cgsGroup = group,
                cgsAvailableCount = workCountExact (lengthNatural matches)
              }
          | (group, matches) <- Map.toAscList groupedMatches
          ],
      csLookupGroup =
        \group -> pure (finiteCandidateGroup <$> Map.lookup group groupedMatches)
    }
  where
    groupedMatches =
      fmap Foldable.toList $
        Foldable.foldl'
          insertGroupMatches
          Map.empty
          rawGroups

    insertGroupMatches :: Ord group => Map group (Seq match) -> (group, [match]) -> Map group (Seq match)
    insertGroupMatches matchesByGroup (group, matches) =
      Map.insertWith
        (\newMatches existingMatches -> existingMatches <> newMatches)
        group
        (Seq.fromList matches)
        matchesByGroup

finiteCandidateGroup ::
  (Applicative m, Monoid meta) =>
  [match] ->
  CandidateGroup m meta match
finiteCandidateGroup matches =
  CandidateGroup
    { cgAvailableCount = pure (workCountExact (lengthNatural matches)),
      cgOpenCursor = pure (finiteCandidateCursor matches)
    }

finiteCandidateCursor ::
  (Applicative m, Monoid meta) =>
  [match] ->
  CandidateCursor m meta match
finiteCandidateCursor matches =
  CandidateCursor $ \request ->
    let (pulledMatches, remainingMatches) =
          splitAtNatural (pullRequestLimit request) matches
        !remainingCount =
          workCountExact (lengthNatural remainingMatches)
        nextCursor =
          if null remainingMatches
            then Nothing
            else Just (finiteCandidateCursor remainingMatches)
     in pure
          ( pullResult
              pulledMatches
              mempty
              remainingCount
              (workCoverageFromRemaining remainingCount)
              nextCursor
          )

splitAtNatural :: Natural -> [value] -> ([value], [value])
splitAtNatural limit values =
  reverseFirst (go limit [] values)
  where
    go :: Natural -> [value] -> [value] -> ([value], [value])
    go remaining reversedPrefix rest
      | remaining == 0 =
          (reversedPrefix, rest)
      | otherwise =
          case rest of
            [] ->
              (reversedPrefix, [])
            value : remainingValues ->
              go (remaining - 1) (value : reversedPrefix) remainingValues

    reverseFirst :: ([value], [value]) -> ([value], [value])
    reverseFirst (!reversedPrefix, !suffix) =
      (reverse reversedPrefix, suffix)

lengthNatural :: [value] -> Natural
lengthNatural = Foldable.foldl' (\count _ -> count + 1) 0
