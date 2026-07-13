-- | Generic fresh-input timing and RTS cost sampling for benchmark components.
module Moonlight.Pale.Bench.Measure
  ( TimedSample (..),
    timeFreshSample,
    FreshMeasurementFailure (..),
    FreshMeasurement (..),
    measureFreshSample,
  )
where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Bifunctor (first)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
  ( RTSStats,
    allocated_bytes,
    getRTSStats,
    getRTSStatsEnabled,
    max_live_bytes,
  )
import System.Mem (performMajorGC)

data TimedSample value = TimedSample
  { timedSampleElapsedNanoseconds :: !Word64,
    timedSampleValue :: !value,
    timedSampleDigest :: !Int
  }

timeFreshSample ::
  Int -> input -> (input -> Either errorValue value) -> (value -> Int) ->
  IO (Either errorValue (TimedSample value))
timeFreshSample sampleOrdinal input runSample digest = do
  start <- getMonotonicTimeNSec
  sampleResult <- evaluate (runSample (freshSampleInput sampleOrdinal input))
  traverse
    ( \sampleValue -> do
        sampleDigest <- evaluate (force (digest sampleValue))
        end <- getMonotonicTimeNSec
        pure (TimedSample (end - start) sampleValue sampleDigest)
    )
    sampleResult

data FreshMeasurementFailure errorValue
  = FreshMeasurementRtsStatsDisabled
  | FreshMeasurementActionFailed !errorValue
  deriving stock (Eq, Show)

data FreshMeasurement value = FreshMeasurement
  { freshMeasurementElapsedNanoseconds :: !Word64,
    freshMeasurementAllocatedBytes :: !Word64,
    freshMeasurementPeakLiveBytes :: !Word64,
    freshMeasurementValue :: !value,
    freshMeasurementDigest :: !Int
  }

measureFreshSample ::
  Int ->
  input ->
  (input -> IO (Either errorValue value)) ->
  (value -> Int) ->
  IO (Either (FreshMeasurementFailure errorValue) (FreshMeasurement value))
measureFreshSample sampleOrdinal input runSample digest =
  getRTSStatsEnabled >>= \statsEnabled ->
    if statsEnabled
      then measureWithStats
      else pure (Left FreshMeasurementRtsStatsDisabled)
  where
    measureWithStats = do
      beforeStats <- majorGcStats
      start <- getMonotonicTimeNSec
      sampleResult <- runSample (freshSampleInput sampleOrdinal input)
      traverse (finishMeasurement beforeStats start) (first FreshMeasurementActionFailed sampleResult)

    finishMeasurement beforeStats start sampleValue = do
      sampleDigest <- evaluate (force (digest sampleValue))
      end <- getMonotonicTimeNSec
      afterActionStats <- getRTSStats
      peakStats <- majorGcStats
      pure
        FreshMeasurement
          { freshMeasurementElapsedNanoseconds = end - start,
            freshMeasurementAllocatedBytes =
              allocated_bytes afterActionStats - allocated_bytes beforeStats,
            freshMeasurementPeakLiveBytes = max_live_bytes peakStats,
            freshMeasurementValue = sampleValue,
            freshMeasurementDigest = sampleDigest
          }

majorGcStats :: IO RTSStats
majorGcStats =
  performMajorGC *> getRTSStats

freshSampleInput :: Int -> value -> value
freshSampleInput sampleOrdinal value = sampleOrdinal `seq` value
{-# NOINLINE freshSampleInput #-}
