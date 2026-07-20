{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Physical execution of already scheduled matches: the narrow, opt-in IO
-- boundary of the engine.
--
-- This module does not schedule matches, rank groups, update cooldowns,
-- update evidence, or alter trace policy. Parallel workers compute
-- independent deltas or analyses; the ordered merge alone owns the state
-- transition, so parallel execution is observationally equal to sequential
-- execution.
--
-- Expected failures are typed values in 'Either'. Unexpected runtime
-- exceptions remain exceptions and are left to @scheduler@ to propagate.
-- Lazy deltas can erase useful parallelism unless the worker forces the
-- useful part before returning; this boundary intentionally does not impose
-- an 'NFData' constraint. Chunking amortizes scheduler overhead while
-- preserving scheduled-result order.
module Moonlight.Control.Engine.Parallel
  ( MatchExecution (..),
    ParallelMatchExecution (..),
    traverseScheduledMatches,
    traverseScheduledBatch,
    applyScheduledMatchDeltas,
    applyScheduledBatchDeltas,
  )
where

import Control.Scheduler
  ( Comp,
  )
import Control.Scheduler qualified as Scheduler
import Data.Kind
  ( Type,
  )
import Data.List
  ( unfoldr,
  )
import Moonlight.Control.Candidate
  ( ScheduledBatch,
    scheduledBatchMatches,
  )
import Moonlight.Control.Engine.Work
  ( ApplyResult,
  )

-- | How scheduled matches are physically executed.
type MatchExecution :: Type
data MatchExecution
  = SequentialMatches
  | ParallelMatches !ParallelMatchExecution
  deriving stock (Eq, Show)

type ParallelMatchExecution :: Type
data ParallelMatchExecution = ParallelMatchExecution
  { pmeComp :: !Comp,
    pmeMinBatchSize :: !Int,
    pmeChunkSize :: !Int
  }
  deriving stock (Eq, Show)

-- | Run already scheduled matches and return deltas in scheduled-match
-- order.
--
-- Sequential execution short-circuits on the first expected typed failure.
-- Parallel execution runs the whole enabled batch and then sequences ordered
-- results, so typed failures are selected by scheduled order rather than by
-- completion order.
traverseScheduledMatches ::
  MatchExecution ->
  (match -> IO (Either err delta)) ->
  [match] ->
  IO (Either err [delta])
traverseScheduledMatches execution runMatch matches =
  case execution of
    SequentialMatches ->
      traverseScheduledMatchesSequential runMatch matches
    ParallelMatches parallelExecution
      | parallelBatchEnabled parallelExecution matches ->
          traverseScheduledMatchesParallel parallelExecution runMatch matches
      | otherwise ->
          traverseScheduledMatchesSequential runMatch matches

{-# INLINE traverseScheduledMatches #-}

traverseScheduledBatch ::
  MatchExecution ->
  (match -> IO (Either err delta)) ->
  ScheduledBatch group match ->
  IO (Either err [delta])
traverseScheduledBatch execution runMatch =
  traverseScheduledMatches execution runMatch . scheduledBatchMatches

{-# INLINE traverseScheduledBatch #-}

traverseScheduledMatchesParallel ::
  ParallelMatchExecution ->
  (match -> IO (Either err delta)) ->
  [match] ->
  IO (Either err [delta])
traverseScheduledMatchesParallel parallelExecution runMatch matches
  | chunkSize == 1 =
      fmap
        sequenceA
        ( Scheduler.traverseConcurrently
            (pmeComp parallelExecution)
            runMatch
            matches
        )
  | otherwise = do
      chunkResults <-
        Scheduler.traverseConcurrently
          (pmeComp parallelExecution)
          (traverseScheduledMatchChunk runMatch)
          (chunksOf chunkSize matches)
      pure (fmap concat (sequenceA chunkResults))
  where
    chunkSize =
      canonicalChunkSize (pmeChunkSize parallelExecution)

{-# INLINE traverseScheduledMatchesParallel #-}

traverseScheduledMatchChunk ::
  (match -> IO (Either err delta)) ->
  [match] ->
  IO (Either err [delta])
traverseScheduledMatchChunk runMatch =
  fmap sequenceA . traverse runMatch

{-# INLINE traverseScheduledMatchChunk #-}

-- | Execute scheduled matches as independent work, then let the
-- source-owned merge algebra perform the ordered state transition.
--
-- Empty batches are still passed to the merge function; evidence identity
-- and applied-count semantics belong to the concrete source.
applyScheduledMatchDeltas ::
  MatchExecution ->
  (match -> IO (Either err delta)) ->
  (state -> [delta] -> Either err (ApplyResult state evidence)) ->
  [match] ->
  state ->
  IO (Either err (ApplyResult state evidence))
applyScheduledMatchDeltas execution runMatch mergeDeltas matches state = do
  deltaResult <-
    traverseScheduledMatches execution runMatch matches
  pure (deltaResult >>= mergeDeltas state)

{-# INLINE applyScheduledMatchDeltas #-}

applyScheduledBatchDeltas ::
  MatchExecution ->
  (match -> IO (Either err delta)) ->
  (state -> [delta] -> Either err (ApplyResult state evidence)) ->
  ScheduledBatch group match ->
  state ->
  IO (Either err (ApplyResult state evidence))
applyScheduledBatchDeltas execution runMatch mergeDeltas scheduledBatch =
  applyScheduledMatchDeltas
    execution
    runMatch
    mergeDeltas
    (scheduledBatchMatches scheduledBatch)

{-# INLINE applyScheduledBatchDeltas #-}

traverseScheduledMatchesSequential ::
  (match -> IO (Either err delta)) ->
  [match] ->
  IO (Either err [delta])
traverseScheduledMatchesSequential runMatch =
  foldr step (pure (Right []))
  where
    step match remainingMatches = do
      matchResult <-
        runMatch match
      case matchResult of
        Left err ->
          pure (Left err)
        Right delta ->
          fmap (fmap (delta :)) remainingMatches

{-# INLINE traverseScheduledMatchesSequential #-}

parallelBatchEnabled ::
  ParallelMatchExecution ->
  [match] ->
  Bool
parallelBatchEnabled parallelExecution =
  hasAtLeast (canonicalMinBatchSize (pmeMinBatchSize parallelExecution))

{-# INLINE parallelBatchEnabled #-}

canonicalMinBatchSize :: Int -> Int
canonicalMinBatchSize =
  max 1

{-# INLINE canonicalMinBatchSize #-}

canonicalChunkSize :: Int -> Int
canonicalChunkSize =
  max 1

{-# INLINE canonicalChunkSize #-}

chunksOf :: Int -> [match] -> [[match]]
chunksOf chunkSize =
  unfoldr nextChunk
  where
    canonicalSize =
      canonicalChunkSize chunkSize

    nextChunk matches =
      case matches of
        [] ->
          Nothing
        _ ->
          Just (splitAt canonicalSize matches)

{-# INLINE chunksOf #-}

hasAtLeast :: Int -> [match] -> Bool
hasAtLeast !requiredCount matches
  | requiredCount <= 0 =
      True
  | otherwise =
      length (take requiredCount matches) == requiredCount

{-# INLINE hasAtLeast #-}
