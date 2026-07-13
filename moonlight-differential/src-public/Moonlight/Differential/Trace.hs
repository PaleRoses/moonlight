{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Trace
  ( Trace,
    TraceSpine,
    traceSince,
    traceUpper,
    traceDescription,
    traceRecentBatches,
    traceSpine,
    traceSpineCompacted,
    traceSpineRecent,
    traceSpineCompactedLayerCount,
    traceSpineRecentBatchCount,
    traceSpinePhysicalBatchCount,
    traceSpinePhysicalRowCount,
    traceSpinePhysicalVirtualWeight,
    TraceCompactionFuel (..),
    TracePhysicalCompactionStepStats (..),
    TraceFrontierAdvanceError (..),
    emptyTrace,
    singletonTrace,
    traceFromBatch,
    traceFromBatches,
    traceAppendBatch,
    traceAdvanceSince,
    traceAdvanceUpper,
    traceCompactPhysicalBefore,
    compactTracePhysicalStep,
    coalesceTraceBatches,
    traceAccumUpTo,
    foldTraceAccumUpTo,
    snapshotTraceBatch,
    traceFromUpdates,
    foldTrace,
    foldTraceBatches,
    foldTraceBatchRows,
    foldTraceKeyRows,
    foldTraceKey,
    foldTraceKeyThrough,
    foldTraceKeyAfter,
    traceNull,
  )
where

import Data.Bits
  ( countLeadingZeros,
    finiteBitSize,
  )
import Data.Foldable qualified as Foldable
import Data.Kind
  ( Type,
  )
import Data.Sequence
  ( Seq,
    (><),
    (|>),
  )
import Data.Sequence qualified as Seq
import Data.Vector
  ( Vector,
  )
import Data.Vector qualified as Vector
import Numeric.Natural
  ( Natural,
  )
import Moonlight.Delta.Frontier
  ( UpperFrontier,
    emptyUpperFrontier,
    frontierPoints,
    mkFrontier,
    mkUpperFrontier,
    singletonUpperFrontier,
    upperFrontierPoints,
  )
import Moonlight.Core
  ( PartialOrder (..),
  )
import Moonlight.Core (AdditiveGroup)
import Moonlight.Differential.Algebra.ZSet qualified as ZSet
import Moonlight.Differential.Batch
  ( Batch,
    BatchMergeFuel (..),
    BatchMergeWork (..),
    BatchMerger,
    batchCoverNull,
    batchMergeDone,
    batchLower,
    batchNull,
    batchRowCount,
    batchUpper,
    beginBatchMerge,
    emptyBatch,
    finishBatchMerge,
    foldBatch,
    foldBatchKey,
    foldBatchKeyRows,
    fromUpdates,
    mergeBatches,
    singletonBatch,
    workBatchMergeMeasured,
  )
import Moonlight.Differential.Trace.Description
  ( TraceDescription,
    mergeUpperFrontier,
    upperFrontierAtOrBefore,
  )
import Moonlight.Differential.Trace.Description qualified as TraceDescription
import Moonlight.Differential.Update
  ( Update (..),
  )

type Trace :: Type -> Type -> Type -> Type -> Type
data Trace time key val weight = Trace
  { traceSince :: !(UpperFrontier time),
    traceUpper :: !(UpperFrontier time),
    traceSpine :: !(TraceSpine time key val weight)
  }
  deriving stock (Eq, Ord, Show)

type TraceSpine :: Type -> Type -> Type -> Type -> Type
data TraceSpine time key val weight = TraceSpine
  { traceSpineCompactedSlots :: !(Vector (TracePhysicalSlot time key val weight)),
    traceSpineRecent :: !(Seq (Batch time key val weight))
  }
  deriving stock (Eq, Ord, Show)

type TraceCompactionFuel :: Type
newtype TraceCompactionFuel = TraceCompactionFuel
  { unTraceCompactionFuel :: Natural
  }
  deriving stock (Eq, Ord, Show)

type TracePhysicalCompactionStepStats :: Type
data TracePhysicalCompactionStepStats = TracePhysicalCompactionStepStats
  { tracePhysicalCompactionBatchesConsumed :: !Int,
    tracePhysicalCompactionInputRowsVisited :: !Int,
    tracePhysicalCompactionMergeFuelConsumed :: !Natural,
    tracePhysicalCompactionActiveMergeCount :: !Int,
    tracePhysicalCompactionOutputLayers :: !Int
  }
  deriving stock (Eq, Ord, Show)

type TracePhysicalSlot :: Type -> Type -> Type -> Type -> Type
data TracePhysicalSlot time key val weight = TracePhysicalSlot
  { tracePhysicalSlotLoose :: !(Seq (Batch time key val weight)),
    tracePhysicalSlotMerging :: !(Maybe (TracePhysicalLayer time key val weight))
  }
  deriving stock (Eq, Ord, Show)

type TracePhysicalLayer :: Type -> Type -> Type -> Type -> Type
data TracePhysicalLayer time key val weight = TracePhysicalLayer
  { tracePhysicalLayerLevel :: {-# UNPACK #-} !Int,
    tracePhysicalLayerRowCount :: {-# UNPACK #-} !Int,
    tracePhysicalLayerState :: !(TracePhysicalLayerState time key val weight)
  }
  deriving stock (Eq, Ord, Show)

-- Source anchor:
--   differential-dataflow/src/trace/implementations/spine_fueled.rs: MergeState
--   feldera/crates/dbsp/src/trace/spine_async.rs: Merge
-- Layers carry either a completed batch or a resumable merge. Profile and cover
-- reads preserve the merge source sections until a later settlement promotes the
-- finished batch; that is the DBSP snapshot discipline, not eager flattening in
-- a more respectable hat.
type TracePhysicalLayerState :: Type -> Type -> Type -> Type -> Type
data TracePhysicalLayerState time key val weight
  = TracePhysicalLayerSingle !(Batch time key val weight)
  | TracePhysicalLayerMerging !(Seq (Batch time key val weight)) !(BatchMerger time key val weight)
  deriving stock (Eq, Ord, Show)

type TraceFuelWork :: Type -> Type
data TraceFuelWork state = TraceFuelWork
  { traceFuelWorkConsumed :: !Natural,
    traceFuelWorkState :: !state
  }

type TraceRecentCompaction :: Type -> Type -> Type -> Type -> Type
data TraceRecentCompaction time key val weight = TraceRecentCompaction
  { traceRecentCompactionEligible :: !(Seq (Batch time key val weight)),
    traceRecentCompactionRemaining :: !(Seq (Batch time key val weight))
  }

data TraceFrontierAdvanceError time = TraceFrontierRegression
  { traceFrontierCurrent :: !(UpperFrontier time),
    traceFrontierRequested :: !(UpperFrontier time)
  }
  deriving stock (Eq, Ord, Show)

instance
  (Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  Semigroup (Trace time key val weight)
  where
  left <> right =
    Trace
      { traceSince = mergeUpperFrontier (traceSince left) (traceSince right),
        traceUpper = mergeUpperFrontier (traceUpper left) (traceUpper right),
        traceSpine = appendTraceSpine (traceSpine left) (traceSpine right)
      }

instance
  (Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  Monoid (Trace time key val weight)
  where
  mempty =
    emptyTrace

emptyTrace :: Trace time key val weight
emptyTrace =
  Trace
    { traceSince = emptyUpperFrontier,
      traceUpper = emptyUpperFrontier,
      traceSpine = emptyTraceSpine
    }
{-# INLINABLE emptyTrace #-}

traceDescription :: (Ord time, PartialOrder time) => Trace time key val weight -> TraceDescription time
traceDescription traceValue =
  TraceDescription.traceDescription
    (mkFrontier (upperFrontierPoints (traceSince traceValue)))
    (traceUpper traceValue)
    (traceSince traceValue)
{-# INLINE traceDescription #-}

singletonTrace ::
  (Ord time, PartialOrder time, Eq weight, AdditiveGroup weight) =>
  Update time key val weight ->
  Trace time key val weight
singletonTrace =
  traceFromBatch . singletonBatch
{-# INLINABLE singletonTrace #-}

traceFromBatch :: (Ord time, PartialOrder time) => Batch time key val weight -> Trace time key val weight
traceFromBatch batch
  | batchNull batch =
      emptyTrace
  | otherwise =
      Trace
        { traceSince = mkUpperFrontier (frontierPoints (batchLower batch)),
          traceUpper = batchUpper batch,
          traceSpine = traceSpineFromBatch batch
        }
{-# INLINABLE traceFromBatch #-}

traceFromBatches ::
  (Foldable batches, Ord time, PartialOrder time) =>
  batches (Batch time key val weight) ->
  Trace time key val weight
traceFromBatches =
  Foldable.foldl' (\traceValue batch -> traceAppendBatch batch traceValue) emptyTrace
{-# INLINABLE traceFromBatches #-}

traceAppendBatch ::
  (Ord time, PartialOrder time) =>
  Batch time key val weight ->
  Trace time key val weight ->
  Trace time key val weight
traceAppendBatch batch traceValue
  | batchNull batch =
      traceValue
  | otherwise =
      traceValue
        { traceUpper = mergeUpperFrontier (traceUpper traceValue) (batchUpper batch),
          traceSpine = appendTraceSpineBatch batch (traceSpine traceValue)
        }
{-# INLINABLE traceAppendBatch #-}

traceAdvanceSince ::
  PartialOrder time =>
  UpperFrontier time ->
  Trace time key val weight ->
  Either (TraceFrontierAdvanceError time) (Trace time key val weight)
traceAdvanceSince since traceValue
  | upperFrontierAtOrBefore (traceSince traceValue) since =
      Right traceValue {traceSince = since}
  | otherwise =
      Left
        TraceFrontierRegression
          { traceFrontierCurrent = traceSince traceValue,
            traceFrontierRequested = since
          }
{-# INLINABLE traceAdvanceSince #-}

traceAdvanceUpper ::
  PartialOrder time =>
  UpperFrontier time ->
  Trace time key val weight ->
  Either (TraceFrontierAdvanceError time) (Trace time key val weight)
traceAdvanceUpper upper traceValue
  | upperFrontierAtOrBefore (traceUpper traceValue) upper =
      Right traceValue {traceUpper = upper}
  | otherwise =
      Left
        TraceFrontierRegression
          { traceFrontierCurrent = traceUpper traceValue,
            traceFrontierRequested = upper
          }
{-# INLINABLE traceAdvanceUpper #-}

coalesceTraceBatches ::
  (Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  UpperFrontier time ->
  Trace time key val weight ->
  Trace time key val weight
coalesceTraceBatches since =
  traceCompactPhysicalBefore since . advanceTraceSinceUnchecked since
{-# INLINE coalesceTraceBatches #-}

advanceTraceSinceUnchecked ::
  (Ord time, PartialOrder time) =>
  UpperFrontier time ->
  Trace time key val weight ->
  Trace time key val weight
advanceTraceSinceUnchecked since traceValue =
  traceValue {traceSince = mergeUpperFrontier (traceSince traceValue) since}
{-# INLINE advanceTraceSinceUnchecked #-}

traceCompactPhysicalBefore ::
  (Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  UpperFrontier time ->
  Trace time key val weight ->
  Trace time key val weight
traceCompactPhysicalBefore since traceValue
  | traceSpineEmpty (traceSpine traceValue) =
      traceValue
  | otherwise =
      traceValue
        { traceSpine = compactTraceSpine since (traceSpine traceValue)
        }
{-# INLINE traceCompactPhysicalBefore #-}

compactTracePhysicalStep ::
  (Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  TraceCompactionFuel ->
  UpperFrontier time ->
  Trace time key val weight ->
  (TracePhysicalCompactionStepStats, Trace time key val weight)
compactTracePhysicalStep fuel since traceValue =
  ( tracePhysicalCompactionStatsFromStep
      (traceRecentCompactionEligible recentCompaction)
      (traceFuelWorkConsumed compactionWork)
      nextTrace,
    nextTrace
  )
  where
    spine =
      traceSpine traceValue

    recentCompaction =
      traceRecentCompaction since spine

    compactionWork =
      compactTraceSpineWithFuelMeasured fuel recentCompaction spine

    nextTrace
      | traceSpineEmpty spine =
          traceValue
      | otherwise =
          traceValue
            { traceSpine = traceFuelWorkState compactionWork
            }
{-# INLINE compactTracePhysicalStep #-}

snapshotTraceBatch ::
  (Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  Trace time key val weight ->
  Batch time key val weight
snapshotTraceBatch =
  traceSpineToBatch . traceSpine
{-# INLINABLE snapshotTraceBatch #-}

traceSpineCompacted ::
  (Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  TraceSpine time key val weight ->
  Batch time key val weight
traceSpineCompacted spine =
  batchCoverToBatch
    (tracePhysicalSlotsMaterializedBatchCover (traceSpineCompactedSlots spine))
    Seq.empty
{-# INLINE traceSpineCompacted #-}

traceSpineCompactedLayerCount :: TraceSpine time key val weight -> Int
traceSpineCompactedLayerCount =
  tracePhysicalSlotsLayerCount . traceSpineCompactedSlots
{-# INLINE traceSpineCompactedLayerCount #-}

traceSpineRecentBatchCount :: TraceSpine time key val weight -> Int
traceSpineRecentBatchCount =
  Seq.length . traceSpineRecent
{-# INLINE traceSpineRecentBatchCount #-}

traceSpinePhysicalBatchCount :: TraceSpine time key val weight -> Int
traceSpinePhysicalBatchCount spine =
  tracePhysicalSlotsLayerCount (traceSpineCompactedSlots spine) + traceSpineRecentBatchCount spine
{-# INLINE traceSpinePhysicalBatchCount #-}

traceSpinePhysicalRowCount ::
  TraceSpine time key val weight ->
  Int
traceSpinePhysicalRowCount spine =
  physicalSlotRowCount (traceSpineCompactedSlots spine) + batchSeqRowCount (traceSpineRecent spine)
{-# INLINE traceSpinePhysicalRowCount #-}

traceSpinePhysicalVirtualWeight :: TraceSpine time key val weight -> Natural
traceSpinePhysicalVirtualWeight spine =
  physicalSlotVirtualWeight (traceSpineCompactedSlots spine)
    + fromIntegral (Seq.length (traceSpineRecent spine))
{-# INLINE traceSpinePhysicalVirtualWeight #-}

traceAccumUpTo ::
  (PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  time ->
  Trace time key val weight ->
  ZSet.IndexedZSet key val weight
traceAccumUpTo cutoff =
  traceSpineAccumUpTo cutoff . traceSpine
{-# INLINE traceAccumUpTo #-}

traceSpineAccumUpTo ::
  (PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  time ->
  TraceSpine time key val weight ->
  ZSet.IndexedZSet key val weight
traceSpineAccumUpTo cutoff spine =
  ZSet.indexedZSetFromList
    (traceAccumulationRowsRev (traceSpineFoldAccumUpTo collectTraceAccumulationRow emptyTraceAccumulation cutoff spine))
{-# INLINE traceSpineAccumUpTo #-}

foldTraceAccumUpTo ::
  PartialOrder time =>
  (acc -> key -> val -> weight -> acc) ->
  acc ->
  time ->
  Trace time key val weight ->
  acc
foldTraceAccumUpTo step initial cutoff =
  traceSpineFoldAccumUpTo
    (\accumulator key val weight -> step accumulator key val weight)
    initial
    cutoff
    . traceSpine
{-# INLINE foldTraceAccumUpTo #-}

traceSpineFoldAccumUpTo ::
  PartialOrder time =>
  (acc -> key -> val -> weight -> acc) ->
  acc ->
  time ->
  TraceSpine time key val weight ->
  acc
traceSpineFoldAccumUpTo step initial cutoff spine =
  let cutoffFrontier =
        singletonUpperFrontier cutoff
   in foldTraceSpineBatches
        (foldTraceAccumulationBatch step cutoff cutoffFrontier)
        initial
        spine
{-# INLINE traceSpineFoldAccumUpTo #-}

type TraceAccumulation :: Type -> Type -> Type -> Type
data TraceAccumulation key val weight = TraceAccumulation
  { traceAccumulationRowsRev :: ![(key, val, weight)]
  }

emptyTraceAccumulation :: TraceAccumulation key val weight
emptyTraceAccumulation =
  TraceAccumulation
    { traceAccumulationRowsRev = []
    }
{-# INLINE emptyTraceAccumulation #-}

foldTraceAccumulationBatch ::
  PartialOrder time =>
  (acc -> key -> val -> weight -> acc) ->
  time ->
  UpperFrontier time ->
  acc ->
  Batch time key val weight ->
  acc
foldTraceAccumulationBatch step cutoff cutoffFrontier accumulation batch
  | batchNull batch =
      accumulation
  | batchUpper batch `upperFrontierAtOrBefore` cutoffFrontier =
      foldBatch (foldTraceAccumulationRowUnfiltered step) accumulation batch
  | batchLowerStrictlyAfter cutoff batch =
      accumulation
  | otherwise =
      foldBatch (foldTraceAccumulationRow step cutoff) accumulation batch
{-# INLINE foldTraceAccumulationBatch #-}

batchLowerStrictlyAfter ::
  PartialOrder time =>
  time ->
  Batch time key val weight ->
  Bool
batchLowerStrictlyAfter cutoff =
  Foldable.all (lt cutoff) . frontierPoints . batchLower
{-# INLINE batchLowerStrictlyAfter #-}

foldTraceAccumulationRow ::
  PartialOrder time =>
  (acc -> key -> val -> weight -> acc) ->
  time ->
  acc ->
  time ->
  key ->
  val ->
  weight ->
  acc
foldTraceAccumulationRow step cutoff accumulation time key val weight
  | time `leq` cutoff =
      foldTraceAccumulationRowUnfiltered step accumulation time key val weight
  | otherwise =
      accumulation
{-# INLINE foldTraceAccumulationRow #-}

foldTraceAccumulationRowUnfiltered ::
  (acc -> key -> val -> weight -> acc) ->
  acc ->
  time ->
  key ->
  val ->
  weight ->
  acc
foldTraceAccumulationRowUnfiltered step accumulation _time key val weight =
  step accumulation key val weight
{-# INLINE foldTraceAccumulationRowUnfiltered #-}

collectTraceAccumulationRow ::
  TraceAccumulation key val weight ->
  key ->
  val ->
  weight ->
  TraceAccumulation key val weight
collectTraceAccumulationRow accumulation key val weight =
  accumulation
    { traceAccumulationRowsRev =
        (key, val, weight) : traceAccumulationRowsRev accumulation
    }
{-# INLINE collectTraceAccumulationRow #-}

traceFromUpdates ::
  (Foldable updates, Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  updates (Update time key val weight) ->
  Trace time key val weight
traceFromUpdates =
  traceFromBatch . fromUpdates
{-# INLINABLE traceFromUpdates #-}

foldTrace ::
  (Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  (acc -> time -> key -> val -> weight -> acc) ->
  acc ->
  Trace time key val weight ->
  acc
foldTrace step initial =
  foldBatch step initial . snapshotTraceBatch
{-# INLINABLE foldTrace #-}

foldTraceBatches ::
  (acc -> Batch time key val weight -> acc) ->
  acc ->
  Trace time key val weight ->
  acc
foldTraceBatches step initial =
  foldTraceSpineBatches step initial . traceSpine
{-# INLINE foldTraceBatches #-}

foldTraceBatchRows ::
  (acc -> time -> key -> val -> weight -> acc) ->
  acc ->
  Trace time key val weight ->
  acc
foldTraceBatchRows step =
  foldTraceBatches (foldBatch step)
{-# INLINE foldTraceBatchRows #-}

foldTraceKeyRows ::
  (Ord key, Ord time, Ord val, AdditiveGroup weight, Eq weight) =>
  (acc -> key -> ZSet.ZSet (ZSet.Timed time val) weight -> acc) ->
  acc ->
  Trace time key val weight ->
  acc
foldTraceKeyRows step =
  foldTraceBatches (foldBatchKeyRows step)
{-# INLINE foldTraceKeyRows #-}

foldTraceKey ::
  Ord key =>
  key ->
  (acc -> time -> val -> weight -> acc) ->
  acc ->
  Trace time key val weight ->
  acc
foldTraceKey key step =
  foldTraceBatches (foldBatchKey key step)
{-# INLINE foldTraceKey #-}

foldTraceKeyThrough ::
  (PartialOrder time, Ord key) =>
  time ->
  key ->
  (acc -> time -> val -> weight -> acc) ->
  acc ->
  Trace time key val weight ->
  acc
foldTraceKeyThrough upperBound key step =
  foldTraceBatches (foldTraceKeyThroughBatch upperBound (singletonUpperFrontier upperBound) key step)
{-# INLINE foldTraceKeyThrough #-}

foldTraceKeyAfter ::
  (PartialOrder time, Ord key) =>
  time ->
  key ->
  (acc -> time -> val -> weight -> acc) ->
  acc ->
  Trace time key val weight ->
  acc
foldTraceKeyAfter lowerBound key step =
  foldTraceBatches (foldTraceKeyAfterBatch lowerBound (singletonUpperFrontier lowerBound) key step)
{-# INLINE foldTraceKeyAfter #-}

foldTraceKeyThroughBatch ::
  (PartialOrder time, Ord key) =>
  time ->
  UpperFrontier time ->
  key ->
  (acc -> time -> val -> weight -> acc) ->
  acc ->
  Batch time key val weight ->
  acc
foldTraceKeyThroughBatch upperBound upperFrontier key step accumulation batch
  | batchNull batch =
      accumulation
  | batchUpper batch `upperFrontierAtOrBefore` upperFrontier =
      foldBatchKey key step accumulation batch
  | batchLowerStrictlyAfter upperBound batch =
      accumulation
  | otherwise =
      foldBatchKey key (foldTraceKeyThroughRow upperBound step) accumulation batch
{-# INLINE foldTraceKeyThroughBatch #-}

foldTraceKeyAfterBatch ::
  (PartialOrder time, Ord key) =>
  time ->
  UpperFrontier time ->
  key ->
  (acc -> time -> val -> weight -> acc) ->
  acc ->
  Batch time key val weight ->
  acc
foldTraceKeyAfterBatch lowerBound lowerFrontier key step accumulation batch
  | batchNull batch =
      accumulation
  | batchUpper batch `upperFrontierAtOrBefore` lowerFrontier =
      accumulation
  | batchLowerStrictlyAfter lowerBound batch =
      foldBatchKey key step accumulation batch
  | otherwise =
      foldBatchKey key (foldTraceKeyAfterRow lowerBound step) accumulation batch
{-# INLINE foldTraceKeyAfterBatch #-}

foldTraceKeyThroughRow ::
  PartialOrder time =>
  time ->
  (acc -> time -> val -> weight -> acc) ->
  acc ->
  time ->
  val ->
  weight ->
  acc
foldTraceKeyThroughRow upperBound step accumulation time val weight
  | time `leq` upperBound =
      step accumulation time val weight
  | otherwise =
      accumulation
{-# INLINE foldTraceKeyThroughRow #-}

foldTraceKeyAfterRow ::
  PartialOrder time =>
  time ->
  (acc -> time -> val -> weight -> acc) ->
  acc ->
  time ->
  val ->
  weight ->
  acc
foldTraceKeyAfterRow lowerBound step accumulation time val weight
  | lowerBound `lt` time =
      step accumulation time val weight
  | otherwise =
      accumulation
{-# INLINE foldTraceKeyAfterRow #-}

traceNull ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  Trace time key val weight ->
  Bool
traceNull traceValue =
  batchCoverNull (traceSpineReadableBatchCover (traceSpine traceValue))
{-# INLINE traceNull #-}

traceRecentBatches :: Trace time key val weight -> Seq (Batch time key val weight)
traceRecentBatches =
  traceSpineRecent . traceSpine
{-# INLINABLE traceRecentBatches #-}

emptyTraceSpine :: TraceSpine time key val weight
emptyTraceSpine =
  TraceSpine
    { traceSpineCompactedSlots = Vector.empty,
      traceSpineRecent = Seq.empty
    }
{-# INLINABLE emptyTraceSpine #-}

traceSpineFromBatch :: Batch time key val weight -> TraceSpine time key val weight
traceSpineFromBatch batch =
  TraceSpine
    { traceSpineCompactedSlots = Vector.empty,
      traceSpineRecent = Seq.singleton batch
    }
{-# INLINABLE traceSpineFromBatch #-}

foldTraceSpineBatches ::
  (acc -> Batch time key val weight -> acc) ->
  acc ->
  TraceSpine time key val weight ->
  acc
foldTraceSpineBatches step initial spine =
  Foldable.foldl' step initial (traceSpineBatchCover spine)
{-# INLINE foldTraceSpineBatches #-}

traceSpineBatchCover ::
  TraceSpine time key val weight ->
  Seq (Batch time key val weight)
traceSpineBatchCover spine =
  tracePhysicalSlotsBatchCover (traceSpineCompactedSlots spine)
    >< traceSpineRecent spine
{-# INLINE traceSpineBatchCover #-}

traceSpineReadableBatchCover ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  TraceSpine time key val weight ->
  Seq (Batch time key val weight)
traceSpineReadableBatchCover spine =
  tracePhysicalSlotsMaterializedBatchCover (traceSpineCompactedSlots spine)
    >< traceSpineRecent spine
{-# INLINE traceSpineReadableBatchCover #-}

tracePhysicalSlotsBatchCover ::
  Vector (TracePhysicalSlot time key val weight) ->
  Seq (Batch time key val weight)
tracePhysicalSlotsBatchCover =
  Vector.foldl'
    (\batches slot -> batches >< tracePhysicalSlotBatchCover slot)
    Seq.empty
{-# INLINE tracePhysicalSlotsBatchCover #-}

appendTraceSpine ::
  (Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  TraceSpine time key val weight ->
  TraceSpine time key val weight ->
  TraceSpine time key val weight
appendTraceSpine left right =
  TraceSpine
    { traceSpineCompactedSlots =
        Foldable.foldl'
          insertTracePhysicalBatch
          (traceSpineCompactedSlots left)
          (tracePhysicalSlotsMaterializedBatchCover (traceSpineCompactedSlots right)),
      traceSpineRecent =
        traceSpineRecent left >< traceSpineRecent right
    }
{-# INLINABLE appendTraceSpine #-}

appendTraceSpineBatch ::
  Batch time key val weight ->
  TraceSpine time key val weight ->
  TraceSpine time key val weight
appendTraceSpineBatch batch spine =
  spine
    { traceSpineRecent = traceSpineRecent spine |> batch
    }
{-# INLINABLE appendTraceSpineBatch #-}

compactTraceSpine ::
  (Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  UpperFrontier time ->
  TraceSpine time key val weight ->
  TraceSpine time key val weight
-- Source anchor:
--   differential-dataflow/src/trace/implementations/spine_fueled.rs: Spine::insert
--   feldera/crates/dbsp/src/trace/spine_async.rs: Spine::insert / CursorList
-- Eligible recent batches descend into the level spine one section at a time; no
-- synchronous prefix flatten/sort is allowed to usurp the physical cover.
compactTraceSpine since spine =
  compactTraceSpineUsing tracePhysicalSlotFuel since spine
{-# INLINE compactTraceSpine #-}

compactTraceSpineUsing ::
  (Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  (Int -> BatchMergeFuel) ->
  UpperFrontier time ->
  TraceSpine time key val weight ->
  TraceSpine time key val weight
compactTraceSpineUsing slotFuel since spine =
  traceFuelWorkState (compactTraceSpineUsingMeasured slotFuel (traceRecentCompaction since spine) spine)
{-# INLINE compactTraceSpineUsing #-}

compactTraceSpineWithFuelMeasured ::
  (Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  TraceCompactionFuel ->
  TraceRecentCompaction time key val weight ->
  TraceSpine time key val weight ->
  TraceFuelWork (TraceSpine time key val weight)
compactTraceSpineWithFuelMeasured fuel =
  compactTraceSpineUsingMeasured (const (traceCompactionBatchMergeFuel fuel))
{-# INLINE compactTraceSpineWithFuelMeasured #-}

compactTraceSpineUsingMeasured ::
  (Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  (Int -> BatchMergeFuel) ->
  TraceRecentCompaction time key val weight ->
  TraceSpine time key val weight ->
  TraceFuelWork (TraceSpine time key val weight)
compactTraceSpineUsingMeasured slotFuel recentCompaction spine
  | Seq.null (traceRecentCompactionEligible recentCompaction) =
      TraceFuelWork
        { traceFuelWorkConsumed = 0,
          traceFuelWorkState = spine
        }
  | otherwise =
      TraceFuelWork
        { traceFuelWorkConsumed = traceFuelWorkConsumed slotsWork,
          traceFuelWorkState =
            spine
              { traceSpineCompactedSlots = traceFuelWorkState slotsWork,
                traceSpineRecent = traceRecentCompactionRemaining recentCompaction
              }
        }
  where
    slotsWork =
      Foldable.foldl'
        insertCompactableBatch
        TraceFuelWork
          { traceFuelWorkConsumed = 0,
            traceFuelWorkState = traceSpineCompactedSlots spine
          }
        (traceRecentCompactionEligible recentCompaction)

    insertCompactableBatch work batch =
      let batchWork =
            insertTracePhysicalBatchUsingMeasured slotFuel (traceFuelWorkState work) batch
       in TraceFuelWork
            { traceFuelWorkConsumed =
                traceFuelWorkConsumed work + traceFuelWorkConsumed batchWork,
              traceFuelWorkState = traceFuelWorkState batchWork
            }
{-# INLINE compactTraceSpineUsingMeasured #-}

traceRecentCompaction ::
  PartialOrder time =>
  UpperFrontier time ->
  TraceSpine time key val weight ->
  TraceRecentCompaction time key val weight
traceRecentCompaction since spine =
  TraceRecentCompaction
    { traceRecentCompactionEligible = compactable,
      traceRecentCompactionRemaining = recent
    }
  where
    (compactable, recent) =
      Seq.spanl (batchFrontierAtOrBefore since) (traceSpineRecent spine)
{-# INLINE traceRecentCompaction #-}

traceCompactionBatchMergeFuel :: TraceCompactionFuel -> BatchMergeFuel
traceCompactionBatchMergeFuel (TraceCompactionFuel fuel) =
  BatchMergeFuel fuel
{-# INLINE traceCompactionBatchMergeFuel #-}

insertTracePhysicalBatch ::
  (Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  Vector (TracePhysicalSlot time key val weight) ->
  Batch time key val weight ->
  Vector (TracePhysicalSlot time key val weight)
insertTracePhysicalBatch =
  insertTracePhysicalBatchUsing tracePhysicalSlotFuel
{-# INLINE insertTracePhysicalBatch #-}

insertTracePhysicalBatchUsing ::
  (Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  (Int -> BatchMergeFuel) ->
  Vector (TracePhysicalSlot time key val weight) ->
  Batch time key val weight ->
  Vector (TracePhysicalSlot time key val weight)
insertTracePhysicalBatchUsing slotFuel slots batch =
  traceFuelWorkState (insertTracePhysicalBatchUsingMeasured slotFuel slots batch)
{-# INLINE insertTracePhysicalBatchUsing #-}

insertTracePhysicalBatchUsingMeasured ::
  (Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  (Int -> BatchMergeFuel) ->
  Vector (TracePhysicalSlot time key val weight) ->
  Batch time key val weight ->
  TraceFuelWork (Vector (TracePhysicalSlot time key val weight))
insertTracePhysicalBatchUsingMeasured slotFuel slots batch =
  settleTracePhysicalSlotsFromUsingMeasured slotFuel batchLevel
    (insertTracePhysicalBatchAtLevel batch slots)
  where
    batchLevel =
      physicalLayerLevel (batchRowCount batch)
{-# INLINE insertTracePhysicalBatchUsingMeasured #-}

insertTracePhysicalBatchAtLevel ::
  Batch time key val weight ->
  Vector (TracePhysicalSlot time key val weight) ->
  Vector (TracePhysicalSlot time key val weight)
insertTracePhysicalBatchAtLevel batch =
  alterTracePhysicalSlot
    (physicalLayerLevel (batchRowCount batch))
    (appendTracePhysicalSlotLoose batch)
{-# INLINE insertTracePhysicalBatchAtLevel #-}

type TracePhysicalSlotSettlement :: Type -> Type -> Type -> Type -> Type
data TracePhysicalSlotSettlement time key val weight
  = TracePhysicalSlotSettled !(Vector (TracePhysicalSlot time key val weight))
  | TracePhysicalSlotPromoted !(Batch time key val weight) !(Vector (TracePhysicalSlot time key val weight))

settleTracePhysicalSlotsFromUsingMeasured ::
  (Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  (Int -> BatchMergeFuel) ->
  Int ->
  Vector (TracePhysicalSlot time key val weight) ->
  TraceFuelWork (Vector (TracePhysicalSlot time key val weight))
settleTracePhysicalSlotsFromUsingMeasured slotFuel level slots =
  case traceFuelWorkState slotWork of
    TracePhysicalSlotSettled settled ->
      TraceFuelWork
        { traceFuelWorkConsumed =
            traceFuelWorkConsumed slotWork,
          traceFuelWorkState = settled
        }
    TracePhysicalSlotPromoted promotedBatch settled ->
      let promotedWork =
            settleTracePhysicalSlotsFromUsingMeasured slotFuel
              (level + 1)
              (insertTracePhysicalBatchAtLevel promotedBatch settled)
       in TraceFuelWork
            { traceFuelWorkConsumed =
                traceFuelWorkConsumed slotWork
                  + traceFuelWorkConsumed promotedWork,
              traceFuelWorkState = traceFuelWorkState promotedWork
            }
  where
    slotWork =
      settleTracePhysicalSlotAtUsingMeasured slotFuel level slots
{-# INLINE settleTracePhysicalSlotsFromUsingMeasured #-}

settleTracePhysicalSlotAtUsingMeasured ::
  (Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  (Int -> BatchMergeFuel) ->
  Int ->
  Vector (TracePhysicalSlot time key val weight) ->
  TraceFuelWork (TracePhysicalSlotSettlement time key val weight)
settleTracePhysicalSlotAtUsingMeasured slotFuel level slots =
  TraceFuelWork
    { traceFuelWorkConsumed =
        traceFuelWorkConsumed workedSlot,
      traceFuelWorkState = settlement
    }
  where
    slot =
      tracePhysicalSlotAt level slots

    workedSlot =
      workTracePhysicalSlotMeasured
        (slotFuel level)
        (startTracePhysicalSlotMerge level slot)

    workedSlotValue =
      traceFuelWorkState workedSlot

    settlement =
      maybe
        ( TracePhysicalSlotSettled
            (replaceTracePhysicalSlot level workedSlotValue slots)
        )
        ( \(promotedBatch, remainingSlot) ->
            TracePhysicalSlotPromoted
              promotedBatch
              (replaceTracePhysicalSlot level remainingSlot slots)
        )
        (tracePhysicalSlotPromotion slot)
{-# INLINE settleTracePhysicalSlotAtUsingMeasured #-}

emptyTracePhysicalSlot :: TracePhysicalSlot time key val weight
emptyTracePhysicalSlot =
  TracePhysicalSlot
    { tracePhysicalSlotLoose = Seq.empty,
      tracePhysicalSlotMerging = Nothing
    }
{-# INLINE emptyTracePhysicalSlot #-}

tracePhysicalSlotAt ::
  Int ->
  Vector (TracePhysicalSlot time key val weight) ->
  TracePhysicalSlot time key val weight
tracePhysicalSlotAt level slots =
  maybe emptyTracePhysicalSlot id (slots Vector.!? level)
{-# INLINE tracePhysicalSlotAt #-}

alterTracePhysicalSlot ::
  Int ->
  (TracePhysicalSlot time key val weight -> TracePhysicalSlot time key val weight) ->
  Vector (TracePhysicalSlot time key val weight) ->
  Vector (TracePhysicalSlot time key val weight)
alterTracePhysicalSlot level updateSlot slots =
  replaceTracePhysicalSlot
    level
    (updateSlot (tracePhysicalSlotAt level slots))
    (ensureTracePhysicalSlotLevel level slots)
{-# INLINE alterTracePhysicalSlot #-}

replaceTracePhysicalSlot ::
  Int ->
  TracePhysicalSlot time key val weight ->
  Vector (TracePhysicalSlot time key val weight) ->
  Vector (TracePhysicalSlot time key val weight)
replaceTracePhysicalSlot level slot slots =
  let ensured =
        ensureTracePhysicalSlotLevel level slots
   in Vector.take level ensured
        <> Vector.singleton slot
        <> Vector.drop (level + 1) ensured
{-# INLINE replaceTracePhysicalSlot #-}

ensureTracePhysicalSlotLevel ::
  Int ->
  Vector (TracePhysicalSlot time key val weight) ->
  Vector (TracePhysicalSlot time key val weight)
ensureTracePhysicalSlotLevel level slots
  | Vector.length slots > level =
      slots
  | otherwise =
      slots <> Vector.replicate (level + 1 - Vector.length slots) emptyTracePhysicalSlot
{-# INLINE ensureTracePhysicalSlotLevel #-}

appendTracePhysicalSlotLoose ::
  Batch time key val weight ->
  TracePhysicalSlot time key val weight ->
  TracePhysicalSlot time key val weight
appendTracePhysicalSlotLoose batch slot =
  slot
    { tracePhysicalSlotLoose =
        tracePhysicalSlotLoose slot |> batch
    }
{-# INLINE appendTracePhysicalSlotLoose #-}

startTracePhysicalSlotMerge ::
  (Ord time, PartialOrder time) =>
  Int ->
  TracePhysicalSlot time key val weight ->
  TracePhysicalSlot time key val weight
startTracePhysicalSlotMerge level slot =
  case (tracePhysicalSlotMerging slot, Seq.viewl (tracePhysicalSlotLoose slot)) of
    (Nothing, firstBatch Seq.:< restBatches) ->
      case Seq.viewl restBatches of
        secondBatch Seq.:< remainingBatches ->
          slot
            { tracePhysicalSlotLoose = remainingBatches,
              tracePhysicalSlotMerging = Just (beginTracePhysicalBatchMerge level firstBatch secondBatch)
            }
        Seq.EmptyL ->
          slot
    _ ->
      slot
{-# INLINE startTracePhysicalSlotMerge #-}

tracePhysicalSlotPromotion ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  TracePhysicalSlot time key val weight ->
  Maybe (Batch time key val weight, TracePhysicalSlot time key val weight)
tracePhysicalSlotPromotion slot =
  case tracePhysicalSlotMerging slot of
    Just layer ->
      fmap
        (\finishedBatch -> (finishedBatch, slot {tracePhysicalSlotMerging = Nothing}))
        (tracePhysicalLayerFinishedBatch layer)
    _ ->
      Nothing
{-# INLINE tracePhysicalSlotPromotion #-}

tracePhysicalSlotFuel :: Int -> BatchMergeFuel
tracePhysicalSlotFuel level =
  BatchMergeFuel (8 * (2 ^ max 0 level))
{-# INLINE tracePhysicalSlotFuel #-}

beginTracePhysicalBatchMerge ::
  (Ord time, PartialOrder time) =>
  Int ->
  Batch time key val weight ->
  Batch time key val weight ->
  TracePhysicalLayer time key val weight
beginTracePhysicalBatchMerge level left right =
  TracePhysicalLayer
    { tracePhysicalLayerLevel = level + 1,
      tracePhysicalLayerRowCount =
        batchRowCount left + batchRowCount right,
      tracePhysicalLayerState =
        TracePhysicalLayerMerging
          (Seq.fromList [left, right])
          (beginBatchMerge left right)
    }
{-# INLINE beginTracePhysicalBatchMerge #-}

tracePhysicalLayerBatches ::
  TracePhysicalLayer time key val weight ->
  Seq (Batch time key val weight)
tracePhysicalLayerBatches layer =
  case tracePhysicalLayerState layer of
    TracePhysicalLayerSingle batch ->
      Seq.singleton batch
    TracePhysicalLayerMerging sourceBatches _merger ->
      sourceBatches
{-# INLINE tracePhysicalLayerBatches #-}

tracePhysicalLayerMaterializedBatches ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  TracePhysicalLayer time key val weight ->
  Seq (Batch time key val weight)
tracePhysicalLayerMaterializedBatches layer =
  case tracePhysicalLayerState layer of
    TracePhysicalLayerSingle batch ->
      Seq.singleton batch
    TracePhysicalLayerMerging sourceBatches merger
      | batchMergeDone merger ->
          Seq.singleton (finishBatchMerge merger)
      | otherwise ->
          sourceBatches
{-# INLINE tracePhysicalLayerMaterializedBatches #-}

tracePhysicalLayerFinishedBatch ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  TracePhysicalLayer time key val weight ->
  Maybe (Batch time key val weight)
tracePhysicalLayerFinishedBatch layer =
  case tracePhysicalLayerState layer of
    TracePhysicalLayerSingle batch ->
      Just batch
    TracePhysicalLayerMerging _sourceBatches merger
      | batchMergeDone merger ->
          Just (finishBatchMerge merger)
      | otherwise ->
          Nothing
{-# INLINE tracePhysicalLayerFinishedBatch #-}

tracePhysicalSlotBatchCover ::
  TracePhysicalSlot time key val weight ->
  Seq (Batch time key val weight)
tracePhysicalSlotBatchCover slot =
  tracePhysicalSlotLoose slot
    >< maybe Seq.empty tracePhysicalLayerBatches (tracePhysicalSlotMerging slot)
{-# INLINE tracePhysicalSlotBatchCover #-}

tracePhysicalSlotMaterializedBatchCover ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  TracePhysicalSlot time key val weight ->
  Seq (Batch time key val weight)
tracePhysicalSlotMaterializedBatchCover slot =
  tracePhysicalSlotLoose slot
    >< maybe Seq.empty tracePhysicalLayerMaterializedBatches (tracePhysicalSlotMerging slot)
{-# INLINE tracePhysicalSlotMaterializedBatchCover #-}

tracePhysicalSlotsMaterializedBatchCover ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  Vector (TracePhysicalSlot time key val weight) ->
  Seq (Batch time key val weight)
tracePhysicalSlotsMaterializedBatchCover =
  Vector.foldl'
    (\batches slot -> batches >< tracePhysicalSlotMaterializedBatchCover slot)
    Seq.empty
{-# INLINE tracePhysicalSlotsMaterializedBatchCover #-}

tracePhysicalSlotsLayerCount ::
  Vector (TracePhysicalSlot time key val weight) ->
  Int
tracePhysicalSlotsLayerCount =
  Vector.foldl'
    (\count slot -> count + tracePhysicalSlotLayerCount slot)
    0
{-# INLINE tracePhysicalSlotsLayerCount #-}

tracePhysicalSlotLayerCount :: TracePhysicalSlot time key val weight -> Int
tracePhysicalSlotLayerCount slot =
  Seq.length (tracePhysicalSlotLoose slot)
    + maybe 0 (const 1) (tracePhysicalSlotMerging slot)
{-# INLINE tracePhysicalSlotLayerCount #-}

workTracePhysicalLayerMeasured ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  BatchMergeFuel ->
  TracePhysicalLayer time key val weight ->
  TraceFuelWork (TracePhysicalLayer time key val weight)
workTracePhysicalLayerMeasured fuel layer =
  case tracePhysicalLayerState layer of
    TracePhysicalLayerSingle _batch ->
      TraceFuelWork
        { traceFuelWorkConsumed = 0,
          traceFuelWorkState = layer
        }
    TracePhysicalLayerMerging sourceBatches merger ->
      TraceFuelWork
        { traceFuelWorkConsumed = batchMergeFuelConsumed work,
          traceFuelWorkState =
            layer
              { tracePhysicalLayerState =
                  TracePhysicalLayerMerging sourceBatches (batchMergeWorkMerger work)
              }
        }
      where
        work =
          workBatchMergeMeasured fuel merger
{-# INLINE workTracePhysicalLayerMeasured #-}

workTracePhysicalSlotMeasured ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  BatchMergeFuel ->
  TracePhysicalSlot time key val weight ->
  TraceFuelWork (TracePhysicalSlot time key val weight)
workTracePhysicalSlotMeasured fuel slot =
  maybe
    TraceFuelWork
      { traceFuelWorkConsumed = 0,
        traceFuelWorkState = slot
      }
    ( \layer ->
        let layerWork =
              workTracePhysicalLayerMeasured fuel layer
         in TraceFuelWork
              { traceFuelWorkConsumed =
                  traceFuelWorkConsumed layerWork,
                traceFuelWorkState =
                  slot
                    { tracePhysicalSlotMerging =
                        Just (traceFuelWorkState layerWork)
                    }
              }
    )
    (tracePhysicalSlotMerging slot)
{-# INLINE workTracePhysicalSlotMeasured #-}

physicalLayerLevel :: Int -> Int
physicalLayerLevel rowCount
  | rowCount <= 1 =
      0
  | otherwise =
      finiteBitSize rowCount - countLeadingZeros rowCount - 1
{-# INLINE physicalLayerLevel #-}

physicalSlotRowCount ::
  Vector (TracePhysicalSlot time key val weight) ->
  Int
physicalSlotRowCount =
  Vector.foldl'
    (\count slot -> count + tracePhysicalSlotRowCount slot)
    0
{-# INLINE physicalSlotRowCount #-}

tracePhysicalSlotRowCount :: TracePhysicalSlot time key val weight -> Int
tracePhysicalSlotRowCount slot =
  Foldable.foldl'
    (\count batch -> count + batchRowCount batch)
    (maybe 0 tracePhysicalLayerRowCount (tracePhysicalSlotMerging slot))
    (tracePhysicalSlotLoose slot)
{-# INLINE tracePhysicalSlotRowCount #-}

tracePhysicalCompactionStatsFromStep ::
  Seq (Batch time key val weight) ->
  Natural ->
  Trace time key val weight ->
  TracePhysicalCompactionStepStats
tracePhysicalCompactionStatsFromStep compactable mergeFuelConsumed traceValue =
  TracePhysicalCompactionStepStats
    { tracePhysicalCompactionBatchesConsumed = Seq.length compactable,
      tracePhysicalCompactionInputRowsVisited = batchSeqRowCount compactable,
      tracePhysicalCompactionMergeFuelConsumed = mergeFuelConsumed,
      tracePhysicalCompactionActiveMergeCount = traceSpineActiveMergeCount spine,
      tracePhysicalCompactionOutputLayers = traceSpineCompactedLayerCount spine
    }
  where
    spine =
      traceSpine traceValue
{-# INLINE tracePhysicalCompactionStatsFromStep #-}

traceSpineActiveMergeCount :: TraceSpine time key val weight -> Int
traceSpineActiveMergeCount =
  Vector.foldl'
    (\count slot -> count + tracePhysicalSlotActiveMergeCount slot)
    0
    . traceSpineCompactedSlots
{-# INLINE traceSpineActiveMergeCount #-}

tracePhysicalSlotActiveMergeCount :: TracePhysicalSlot time key val weight -> Int
tracePhysicalSlotActiveMergeCount slot =
  case tracePhysicalSlotMerging slot of
    Nothing ->
      0
    Just layer ->
      tracePhysicalLayerActiveMergeCount layer
{-# INLINE tracePhysicalSlotActiveMergeCount #-}

tracePhysicalLayerActiveMergeCount :: TracePhysicalLayer time key val weight -> Int
tracePhysicalLayerActiveMergeCount layer =
  case tracePhysicalLayerState layer of
    TracePhysicalLayerSingle _batch ->
      0
    TracePhysicalLayerMerging _sourceBatches _merger ->
      1
{-# INLINE tracePhysicalLayerActiveMergeCount #-}

batchSeqRowCount :: Seq (Batch time key val weight) -> Int
batchSeqRowCount =
  Foldable.foldl' (\count batch -> count + batchRowCount batch) 0
{-# INLINE batchSeqRowCount #-}

physicalSlotVirtualWeight :: Vector (TracePhysicalSlot time key val weight) -> Natural
physicalSlotVirtualWeight =
  Vector.foldl'
    (\count slot -> count + tracePhysicalSlotVirtualWeight slot)
    0
{-# INLINE physicalSlotVirtualWeight #-}

tracePhysicalSlotVirtualWeight :: TracePhysicalSlot time key val weight -> Natural
tracePhysicalSlotVirtualWeight slot =
  Foldable.foldl'
    (\count batch -> count + physicalBatchWeight batch)
    (maybe 0 physicalLayerWeight (tracePhysicalSlotMerging slot))
    (tracePhysicalSlotLoose slot)
{-# INLINE tracePhysicalSlotVirtualWeight #-}

physicalBatchWeight :: Batch time key val weight -> Natural
physicalBatchWeight =
  (2 ^) . physicalLayerLevel . batchRowCount
{-# INLINE physicalBatchWeight #-}

physicalLayerWeight :: TracePhysicalLayer time key val weight -> Natural
physicalLayerWeight layer =
  2 ^ tracePhysicalLayerLevel layer
{-# INLINE physicalLayerWeight #-}

traceSpineToBatch ::
  (Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  TraceSpine time key val weight ->
  Batch time key val weight
traceSpineToBatch spine =
  batchCoverToBatch
    (tracePhysicalSlotsMaterializedBatchCover (traceSpineCompactedSlots spine))
    (traceSpineRecent spine)
{-# INLINABLE traceSpineToBatch #-}

batchCoverToBatch ::
  (Ord time, PartialOrder time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  Seq (Batch time key val weight) ->
  Seq (Batch time key val weight) ->
  Batch time key val weight
batchCoverToBatch compacted recent =
  case (Seq.viewl compacted, Seq.viewl recent) of
    (Seq.EmptyL, Seq.EmptyL) ->
      emptyBatch
    (batch Seq.:< rest, Seq.EmptyL)
      | Seq.null rest ->
          batch
    (Seq.EmptyL, batch Seq.:< rest)
      | Seq.null rest ->
          batch
    _ ->
      mergeBatches (compacted >< recent)
{-# INLINE batchCoverToBatch #-}

traceSpineEmpty :: TraceSpine time key val weight -> Bool
traceSpineEmpty spine =
  tracePhysicalSlotsLayerCount (traceSpineCompactedSlots spine) == 0
    && Seq.null (traceSpineRecent spine)
{-# INLINE traceSpineEmpty #-}

batchFrontierAtOrBefore ::
  PartialOrder time =>
  UpperFrontier time ->
  Batch time key val weight ->
  Bool
batchFrontierAtOrBefore since batch =
  upperFrontierAtOrBefore (batchUpper batch) since

{-# INLINABLE batchFrontierAtOrBefore #-}
