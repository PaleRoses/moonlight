-- | Generic fresh-input timing and RTS cost sampling for benchmark components.
module Moonlight.Pale.Bench.Measure
  ( TimedSample (..),
    timeFreshSample,
    FreshRtsCounter (..),
    FreshRtsSnapshot (..),
    FreshRtsDeltaObstruction (..),
    FreshRtsDelta (..),
    checkedFreshRtsDelta,
    FreshMeasurementFailure (..),
    FreshMeasurement (..),
    finalizeFreshMeasurement,
    measureFreshSample,
  )
where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Bifunctor (first)
import Data.Int (Int64)
import Data.Word (Word32, Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
  ( RTSStats,
    allocated_bytes,
    copied_bytes,
    cpu_ns,
    elapsed_ns,
    gc,
    gc_cpu_ns,
    gc_elapsed_ns,
    gcdetails_live_bytes,
    gcs,
    getRTSStats,
    getRTSStatsEnabled,
    major_gcs,
    max_live_bytes,
    mutator_cpu_ns,
    mutator_elapsed_ns,
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

-- | The closed set of monotone RTS counters used by fresh measurements.
data FreshRtsCounter
  = FreshRtsCounterGcs
  | FreshRtsCounterMajorGcs
  | FreshRtsCounterAllocatedBytes
  | FreshRtsCounterCopiedBytes
  | FreshRtsCounterMutatorCpuNanoseconds
  | FreshRtsCounterMutatorElapsedNanoseconds
  | FreshRtsCounterGcCpuNanoseconds
  | FreshRtsCounterGcElapsedNanoseconds
  | FreshRtsCounterCpuNanoseconds
  | FreshRtsCounterElapsedNanoseconds
  deriving stock (Eq, Show)

-- | Strict action-boundary projection of the cumulative RTS counters.
data FreshRtsSnapshot = FreshRtsSnapshot
  { freshRtsSnapshotGcs :: !Word32,
    freshRtsSnapshotMajorGcs :: !Word32,
    freshRtsSnapshotAllocatedBytes :: !Word64,
    freshRtsSnapshotCopiedBytes :: !Word64,
    freshRtsSnapshotMutatorCpuNanoseconds :: !Int64,
    freshRtsSnapshotMutatorElapsedNanoseconds :: !Int64,
    freshRtsSnapshotGcCpuNanoseconds :: !Int64,
    freshRtsSnapshotGcElapsedNanoseconds :: !Int64,
    freshRtsSnapshotCpuNanoseconds :: !Int64,
    freshRtsSnapshotElapsedNanoseconds :: !Int64,
    freshRtsSnapshotLiveBytes :: !Word64,
    freshRtsSnapshotMaxLiveBytes :: !Word64
  }
  deriving stock (Eq, Show)

data FreshRtsDeltaObstruction
  = FreshRtsCounterRegression !FreshRtsCounter !Integer !Integer
  deriving stock (Eq, Show)

-- | Checked action-local differences of every governed cumulative RTS counter.
data FreshRtsDelta = FreshRtsDelta
  { freshRtsDeltaGcs :: !Word64,
    freshRtsDeltaMajorGcs :: !Word64,
    freshRtsDeltaAllocatedBytes :: !Word64,
    freshRtsDeltaCopiedBytes :: !Word64,
    freshRtsDeltaMutatorCpuNanoseconds :: !Word64,
    freshRtsDeltaMutatorElapsedNanoseconds :: !Word64,
    freshRtsDeltaGcCpuNanoseconds :: !Word64,
    freshRtsDeltaGcElapsedNanoseconds :: !Word64,
    freshRtsDeltaCpuNanoseconds :: !Word64,
    freshRtsDeltaElapsedNanoseconds :: !Word64
  }
  deriving stock (Eq, Show)

checkedFreshRtsDelta ::
  FreshRtsSnapshot ->
  FreshRtsSnapshot ->
  Either FreshRtsDeltaObstruction FreshRtsDelta
checkedFreshRtsDelta beforeSnapshot afterSnapshot =
  FreshRtsDelta
    <$> checkedCounterDifference
      FreshRtsCounterGcs
      (freshRtsSnapshotGcs beforeSnapshot)
      (freshRtsSnapshotGcs afterSnapshot)
    <*> checkedCounterDifference
      FreshRtsCounterMajorGcs
      (freshRtsSnapshotMajorGcs beforeSnapshot)
      (freshRtsSnapshotMajorGcs afterSnapshot)
    <*> checkedCounterDifference
      FreshRtsCounterAllocatedBytes
      (freshRtsSnapshotAllocatedBytes beforeSnapshot)
      (freshRtsSnapshotAllocatedBytes afterSnapshot)
    <*> checkedCounterDifference
      FreshRtsCounterCopiedBytes
      (freshRtsSnapshotCopiedBytes beforeSnapshot)
      (freshRtsSnapshotCopiedBytes afterSnapshot)
    <*> checkedCounterDifference
      FreshRtsCounterMutatorCpuNanoseconds
      (freshRtsSnapshotMutatorCpuNanoseconds beforeSnapshot)
      (freshRtsSnapshotMutatorCpuNanoseconds afterSnapshot)
    <*> checkedCounterDifference
      FreshRtsCounterMutatorElapsedNanoseconds
      (freshRtsSnapshotMutatorElapsedNanoseconds beforeSnapshot)
      (freshRtsSnapshotMutatorElapsedNanoseconds afterSnapshot)
    <*> checkedCounterDifference
      FreshRtsCounterGcCpuNanoseconds
      (freshRtsSnapshotGcCpuNanoseconds beforeSnapshot)
      (freshRtsSnapshotGcCpuNanoseconds afterSnapshot)
    <*> checkedCounterDifference
      FreshRtsCounterGcElapsedNanoseconds
      (freshRtsSnapshotGcElapsedNanoseconds beforeSnapshot)
      (freshRtsSnapshotGcElapsedNanoseconds afterSnapshot)
    <*> checkedCounterDifference
      FreshRtsCounterCpuNanoseconds
      (freshRtsSnapshotCpuNanoseconds beforeSnapshot)
      (freshRtsSnapshotCpuNanoseconds afterSnapshot)
    <*> checkedCounterDifference
      FreshRtsCounterElapsedNanoseconds
      (freshRtsSnapshotElapsedNanoseconds beforeSnapshot)
      (freshRtsSnapshotElapsedNanoseconds afterSnapshot)

checkedCounterDifference ::
  (Integral counter) =>
  FreshRtsCounter ->
  counter ->
  counter ->
  Either FreshRtsDeltaObstruction Word64
checkedCounterDifference counter beforeValue afterValue
  | afterValue < beforeValue =
      Left
        ( FreshRtsCounterRegression
            counter
            (toInteger beforeValue)
            (toInteger afterValue)
        )
  | otherwise =
      Right
        ( fromInteger
            (toInteger afterValue - toInteger beforeValue)
        )

data FreshMeasurementFailure errorValue
  = FreshMeasurementRtsStatsDisabled
  | FreshMeasurementActionFailed !errorValue
  | FreshMeasurementRtsDeltaFailed !FreshRtsDeltaObstruction
  deriving stock (Eq, Show)

data FreshMeasurement value = FreshMeasurement
  { freshMeasurementElapsedNanoseconds :: !Word64,
    freshMeasurementRtsDelta :: !FreshRtsDelta,
    freshMeasurementRetainedLiveBytes :: !Word64,
    freshMeasurementPeakLiveBytesThroughAction :: !Word64,
    freshMeasurementValue :: !value,
    freshMeasurementDigest :: !Int
  }

-- | Pure checked gluing of the three action-boundary RTS observations.
-- The post-GC snapshot owns retained liveness; the immediate post-action
-- snapshot owns the cumulative maximum reached through the action.
finalizeFreshMeasurement ::
  Word64 ->
  FreshRtsSnapshot ->
  FreshRtsSnapshot ->
  FreshRtsSnapshot ->
  value ->
  Int ->
  Either FreshRtsDeltaObstruction (FreshMeasurement value)
finalizeFreshMeasurement elapsedNanoseconds beforeAction afterAction afterPostGc sampleValue sampleDigest =
  (\actionDelta ->
      FreshMeasurement
        { freshMeasurementElapsedNanoseconds = elapsedNanoseconds,
          freshMeasurementRtsDelta = actionDelta,
          freshMeasurementRetainedLiveBytes = freshRtsSnapshotLiveBytes afterPostGc,
          freshMeasurementPeakLiveBytesThroughAction = freshRtsSnapshotMaxLiveBytes afterAction,
          freshMeasurementValue = sampleValue,
          freshMeasurementDigest = sampleDigest
        }
  )
    <$> checkedFreshRtsDelta beforeAction afterAction

measureFreshSample ::
  Int ->
  input ->
  (input -> IO (Either errorValue value)) ->
  (value -> ()) ->
  (value -> Int) ->
  IO (Either (FreshMeasurementFailure errorValue) (FreshMeasurement value))
measureFreshSample sampleOrdinal input runSample timingReadiness digest =
  getRTSStatsEnabled >>= \statsEnabled ->
    if statsEnabled
      then measureWithStats
      else pure (Left FreshMeasurementRtsStatsDisabled)
  where
    measureWithStats = do
      beforeActionStats <- majorGcStats
      start <- getMonotonicTimeNSec
      sampleResult <- runSample (freshSampleInput sampleOrdinal input)
      fmap (>>= id) $
        traverse
          (finishMeasurement beforeActionStats start)
          (first FreshMeasurementActionFailed sampleResult)

    finishMeasurement beforeActionStats start sampleValue = do
      _ <- evaluate (force (timingReadiness sampleValue))
      end <- getMonotonicTimeNSec
      afterActionStats <- getRTSStats
      afterPostGcStats <- majorGcStats
      sampleDigest <- evaluate (force (digest sampleValue))
      pure
        ( first FreshMeasurementRtsDeltaFailed
            ( finalizeFreshMeasurement
                (end - start)
                (freshRtsSnapshotFromStats beforeActionStats)
                (freshRtsSnapshotFromStats afterActionStats)
                (freshRtsSnapshotFromStats afterPostGcStats)
                sampleValue
                sampleDigest
            )
        )

freshRtsSnapshotFromStats :: RTSStats -> FreshRtsSnapshot
freshRtsSnapshotFromStats stats =
  FreshRtsSnapshot
    { freshRtsSnapshotGcs = gcs stats,
      freshRtsSnapshotMajorGcs = major_gcs stats,
      freshRtsSnapshotAllocatedBytes = allocated_bytes stats,
      freshRtsSnapshotCopiedBytes = copied_bytes stats,
      freshRtsSnapshotMutatorCpuNanoseconds = mutator_cpu_ns stats,
      freshRtsSnapshotMutatorElapsedNanoseconds = mutator_elapsed_ns stats,
      freshRtsSnapshotGcCpuNanoseconds = gc_cpu_ns stats,
      freshRtsSnapshotGcElapsedNanoseconds = gc_elapsed_ns stats,
      freshRtsSnapshotCpuNanoseconds = cpu_ns stats,
      freshRtsSnapshotElapsedNanoseconds = elapsed_ns stats,
      freshRtsSnapshotLiveBytes = gcdetails_live_bytes (gc stats),
      freshRtsSnapshotMaxLiveBytes = max_live_bytes stats
    }

majorGcStats :: IO RTSStats
majorGcStats =
  performMajorGC *> getRTSStats

freshSampleInput :: Int -> value -> value
freshSampleInput sampleOrdinal value = sampleOrdinal `seq` value
{-# NOINLINE freshSampleInput #-}
