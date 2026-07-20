{-# LANGUAGE BangPatterns #-}

module Main
  ( main,
  )
where

import Control.Monad
  ( unless,
    when,
  )
import Data.Time.Clock
  ( UTCTime,
    addUTCTime,
    diffUTCTime,
    getCurrentTime,
  )
import Data.Word
  ( Word64,
  )
import GHC.Stats
  ( GCDetails (..),
    RTSStats (..),
    getRTSStats,
    getRTSStatsEnabled,
  )
import Moonlight.Flow.Execution.Prepared.Cache
  ( JoinCacheMetrics (..),
    joinCacheMetrics,
  )
import Test.Moonlight.Flow.Execution.Prepared.Cache.Invariant
  ( PreparedCacheInvariantError,
    validatePreparedCacheInvariants,
  )
import Test.Moonlight.Flow.Execution.Prepared.CacheChurnModel
  ( ChurnState,
    applyChurnOp,
    cacheStateWithLimit,
    churnOpAt,
    footprintWidth,
  )
import System.Environment
  ( getArgs,
  )
import System.Exit
  ( die,
  )
import System.Mem
  ( performMajorGC,
  )
import Text.Printf
  ( printf,
  )

main :: IO ()
main = do
  args <- getArgs
  let seconds =
        durationSeconds args
      cacheLimit =
        256
      workingSet =
        cacheLimit + 17

  start <- getCurrentTime
  let deadline =
        addUTCTime (fromIntegral seconds) start
      st0 =
        cacheStateWithLimit cacheLimit

  finalIterations <-
    runUntil
      deadline
      cacheLimit
      workingSet
      0
      Nothing
      st0

  finish <- getCurrentTime
  printf
    "prepared-cache churn soak ok: iterations=%d seconds=%.3f\n"
    finalIterations
    (realToFrac (diffUTCTime finish start) :: Double)

durationSeconds :: [String] -> Int
durationSeconds args =
  case args of
    [] ->
      120
    firstArg : _ ->
      case readNonNegativeInt firstArg of
        Just value ->
          value
        Nothing ->
          120
{-# INLINE durationSeconds #-}

readNonNegativeInt :: String -> Maybe Int
readNonNegativeInt value =
  case reads value of
    [(parsed, "")]
      | parsed >= (0 :: Int) ->
          Just parsed
    _ ->
      Nothing
{-# INLINE readNonNegativeInt #-}

runUntil ::
  UTCTime ->
  Int ->
  Int ->
  Int ->
  Maybe Word64 ->
  ChurnState ->
  IO Int
runUntil deadline cacheLimit workingSet =
  go
  where
    go !iteration !baselineLiveBytes !st = do
      now <- getCurrentTime
      if now >= deadline
        then pure iteration
        else do
          let !st1 =
                applyChurnOp (churnOpAt workingSet iteration) st

          baselineLiveBytes1 <-
            if iteration `rem` sampleEvery == 0
              then sampleAndCheck cacheLimit iteration baselineLiveBytes st1
              else pure baselineLiveBytes

          go
            (iteration + 1)
            baselineLiveBytes1
            st1

sampleEvery :: Int
sampleEvery =
  4096

warmupIterations :: Int
warmupIterations =
  65_536

heapAllowanceBytes :: Word64
heapAllowanceBytes =
  64 * 1024 * 1024

sampleAndCheck ::
  Int ->
  Int ->
  Maybe Word64 ->
  ChurnState ->
  IO (Maybe Word64)
sampleAndCheck cacheLimit iteration baselineLiveBytes st = do
  case validatePreparedCacheInvariants st of
    Right () ->
      pure ()
    Left invariantError ->
      die ("prepared-cache invariant failed: " <> renderInvariantError invariantError)

  let metrics =
        joinCacheMetrics st

  checkMetrics cacheLimit metrics

  liveBytes <- sampleLiveBytes
  case liveBytes of
    Nothing ->
      pure baselineLiveBytes
    Just currentLiveBytes
      | iteration < warmupIterations ->
          pure baselineLiveBytes
      | otherwise ->
          case baselineLiveBytes of
            Nothing ->
              pure (Just currentLiveBytes)
            Just baseline ->
              if currentLiveBytes <= baseline + heapAllowanceBytes
                then pure baselineLiveBytes
                else
                  die
                    ( "prepared-cache heap did not reach steady state: baseline_live_bytes="
                        <> show baseline
                        <> " current_live_bytes="
                        <> show currentLiveBytes
                        <> " allowance_bytes="
                        <> show heapAllowanceBytes
                    )

renderInvariantError :: PreparedCacheInvariantError Int -> String
renderInvariantError =
  show
{-# INLINE renderInvariantError #-}

checkMetrics ::
  Int ->
  JoinCacheMetrics ->
  IO ()
checkMetrics cacheLimit metrics = do
  let entryBound =
        min cacheLimit (jcmPreparedEntries metrics)
      memberBound =
        footprintWidth * entryBound

  unless (jcmPreparedEntries metrics <= cacheLimit) $
    die ("prepared-entry limit exceeded: " <> show metrics)

  when
    ( jcmDepIndexMembers metrics > memberBound
        || jcmTopoIndexMembers metrics > memberBound
        || jcmRootIndexMembers metrics > memberBound
        || jcmResultIndexMembers metrics > memberBound
    )
    (die ("inverted-index member bound exceeded: " <> show metrics))

sampleLiveBytes :: IO (Maybe Word64)
sampleLiveBytes = do
  statsEnabled <- getRTSStatsEnabled
  if statsEnabled
    then do
      performMajorGC
      stats <- getRTSStats
      pure (Just (gcdetails_live_bytes (gc stats)))
    else pure Nothing
