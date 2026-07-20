{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Arrangement
  ( Arrangement,
    emptyArrangement,
    arrangeByKey,
    appendArrangementBatch,
    appendArrangementKeyRows,
    cursorAt,
    foldArrangement,
    foldArrangementKey,
    foldSliceThrough,
    foldSliceAfter,
  )
where

import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Kind
  ( Type,
  )

import Moonlight.Core (AdditiveGroup)
import Moonlight.Differential.Algebra.ZSet
  ( Timed (..),
    ZSet,
  )
import Moonlight.Differential.Algebra.ZSet qualified as ZSet
import Moonlight.Differential.Cursor
  ( Cursor,
    cursorMerge,
    cursorNull,
    cursorFromZSet,
    emptyCursor,
    foldCursorWithTime,
  )
import Moonlight.Differential.Batch
  ( Batch,
    foldBatchKeyRows,
  )
import Moonlight.Differential.Trace
  ( Trace,
    foldTraceBatchRows,
  )

type Arrangement :: Type -> Type -> Type -> Type -> Type
data Arrangement time key val weight = Arrangement
  { arrangementRowsByKey :: !(Map key (ArrangedRows time val weight))
  }
  deriving stock (Eq, Ord, Show)

type ArrangedRows :: Type -> Type -> Type -> Type
data ArrangedRows time val weight = ArrangedRows
  { arrangedRowsValueCursor :: !(Cursor time val weight),
    arrangedRowsByTime :: !(Map time (ZSet val weight))
  }
  deriving stock (Eq, Ord, Show)

emptyArrangement :: Arrangement time key val weight
emptyArrangement =
  Arrangement
    { arrangementRowsByKey = Map.empty
    }

arrangeByKey ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  Trace time key val weight ->
  Arrangement time key val weight
arrangeByKey traceValue =
  Arrangement
    { arrangementRowsByKey =
        packedIndexFromKeyRows
          (foldTraceBatchRows collectPackedTraceRow Map.empty traceValue)
    }

-- Source anchor:
--   feldera/crates/dbsp/src/trace.rs: Batcher / Builder
-- Arrangement construction is a builder fold over the trace batch cover.  Do
-- not manufacture one temporary key ZSet per local batch and then merge them.
collectPackedTraceRow ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  Map key (ZSet (Timed time val) weight) ->
  time ->
  key ->
  val ->
  weight ->
  Map key (ZSet (Timed time val) weight)
collectPackedTraceRow rows time key val weight =
  Map.alter
    (insertTraceRowAtKey time val weight)
    key
    rows
{-# INLINE collectPackedTraceRow #-}

insertTraceRowAtKey ::
  (Ord time, Ord val, Eq weight, AdditiveGroup weight) =>
  time ->
  val ->
  weight ->
  Maybe (ZSet (Timed time val) weight) ->
  Maybe (ZSet (Timed time val) weight)
insertTraceRowAtKey time val weight Nothing =
  keepNonEmptyZSet (ZSet.zsetSingleton timedValue weight)
  where
    timedValue =
      Timed
        { timedTime = time,
          timedValue = val
        }
insertTraceRowAtKey time val weight (Just keyRows) =
  keepNonEmptyZSet
    ( ZSet.zsetInsert
        Timed
          { timedTime = time,
            timedValue = val
          }
        weight
        keyRows
    )
{-# INLINE insertTraceRowAtKey #-}

appendPackedKeyRows ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  Map key (ArrangedRows time val weight) ->
  key ->
  ZSet (Timed time val) weight ->
  Map key (ArrangedRows time val weight)
appendPackedKeyRows rows key keyRows =
  Map.alter
    mergeAtKey
    key
    rows
  where
    mergeAtKey Nothing =
      arrangedRowsFromZSet keyRows
    mergeAtKey (Just arrangedRows) =
      keepNonEmpty (appendArrangedRowsZSet keyRows arrangedRows)
{-# INLINE appendPackedKeyRows #-}

appendArrangementBatch ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  Batch time key val weight ->
  Arrangement time key val weight ->
  Arrangement time key val weight
appendArrangementBatch batch arrangement =
  arrangement
    { arrangementRowsByKey =
        mergePackedIndexes
          (arrangementRowsByKey arrangement)
          (packedIndexFromBatch batch)
    }

appendArrangementKeyRows ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  key ->
  ZSet (Timed time val) weight ->
  Arrangement time key val weight ->
  Arrangement time key val weight
appendArrangementKeyRows key rows arrangement =
  arrangement
    { arrangementRowsByKey =
        appendPackedKeyRows
          (arrangementRowsByKey arrangement)
          key
          rows
    }
{-# INLINE appendArrangementKeyRows #-}

cursorAt ::
  Ord key =>
  key ->
  Arrangement time key val weight ->
  Cursor time val weight
cursorAt key arrangement =
  maybe emptyCursor arrangedRowsValueCursor (Map.lookup key (arrangementRowsByKey arrangement))

foldArrangement ::
  (acc -> time -> key -> val -> weight -> acc) ->
  acc ->
  Arrangement time key val weight ->
  acc
foldArrangement step initial arrangement =
  Map.foldlWithKey'
    ( \acc key rows ->
        foldCursorWithTime
          (\cursorAcc time val weight -> step cursorAcc time key val weight)
          acc
          (arrangedRowsValueCursor rows)
    )
    initial
    (arrangementRowsByKey arrangement)

foldArrangementKey ::
  Ord key =>
  key ->
  (acc -> time -> val -> weight -> acc) ->
  acc ->
  Arrangement time key val weight ->
  acc
foldArrangementKey key step initial arrangement =
  foldCursorWithTime step initial (cursorAt key arrangement)

foldSliceAfter ::
  (Ord time, Ord key) =>
  time ->
  key ->
  (acc -> time -> val -> weight -> acc) ->
  acc ->
  Arrangement time key val weight ->
  acc
foldSliceAfter lowerBound key step initial arrangement =
  foldArrangedRowsByTimeMap
    step
    initial
    (arrangedRowsTimesAfter lowerBound <$> Map.lookup key (arrangementRowsByKey arrangement))
{-# INLINE foldSliceAfter #-}

foldSliceThrough ::
  (Ord time, Ord key) =>
  time ->
  key ->
  (acc -> time -> val -> weight -> acc) ->
  acc ->
  Arrangement time key val weight ->
  acc
foldSliceThrough upperBound key step initial arrangement =
  foldArrangedRowsByTimeMap
    step
    initial
    (arrangedRowsTimesThrough upperBound <$> Map.lookup key (arrangementRowsByKey arrangement))
{-# INLINE foldSliceThrough #-}

-- Source anchor:
--   feldera/crates/dbsp/src/utils/advance_retreat.rs: advance over monotone ranges
--   feldera/crates/dbsp/src/trace/cursor/cursor_list.rs: seek_key / step_key
-- For total-time arrangements the time map is already ordered, so slice reads must restrict
-- by range before folding instead of scanning every bucket and asking a predicate to save us.
arrangedRowsTimesAfter ::
  Ord time =>
  time ->
  ArrangedRows time val weight ->
  Map time (ZSet val weight)
arrangedRowsTimesAfter lowerBound rows =
  snd (Map.split lowerBound (arrangedRowsByTime rows))
{-# INLINE arrangedRowsTimesAfter #-}

arrangedRowsTimesThrough ::
  Ord time =>
  time ->
  ArrangedRows time val weight ->
  Map time (ZSet val weight)
arrangedRowsTimesThrough upperBound rows =
  case Map.lookup upperBound buckets of
    Nothing ->
      beforeUpper
    Just values ->
      Map.insert upperBound values beforeUpper
  where
    buckets =
      arrangedRowsByTime rows
    (beforeUpper, _afterUpper) =
      Map.split upperBound buckets
{-# INLINE arrangedRowsTimesThrough #-}

foldArrangedRowsByTimeMap ::
  (acc -> time -> val -> weight -> acc) ->
  acc ->
  Maybe (Map time (ZSet val weight)) ->
  acc
foldArrangedRowsByTimeMap _step initial Nothing =
  initial
foldArrangedRowsByTimeMap step initial (Just rowsByTime) =
  Map.foldlWithKey'
    ( \acc time values ->
        ZSet.zsetFold (\timeAcc val weight -> step timeAcc time val weight) acc values
    )
    initial
    rowsByTime

packedIndexFromBatch ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  Batch time key val weight ->
  Map key (ArrangedRows time val weight)
packedIndexFromBatch =
  foldBatchKeyRows appendPackedKeyRows Map.empty
{-# INLINE packedIndexFromBatch #-}

mergePackedIndexes ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  Map key (ArrangedRows time val weight) ->
  Map key (ArrangedRows time val weight) ->
  Map key (ArrangedRows time val weight)
mergePackedIndexes =
  Map.mergeWithKey mergeKey id id
  where
    mergeKey ::
      (Ord time, Ord val, Eq weight, AdditiveGroup weight) =>
      key ->
      ArrangedRows time val weight ->
      ArrangedRows time val weight ->
      Maybe (ArrangedRows time val weight)
    mergeKey _key left right =
      keepNonEmpty (mergeArrangedRows left right)

packedIndexFromKeyRows ::
  (Ord time, Ord key, Ord val, Eq weight, AdditiveGroup weight) =>
  Map key (ZSet (Timed time val) weight) ->
  Map key (ArrangedRows time val weight)
packedIndexFromKeyRows =
  Map.foldlWithKey'
    ( \acc key rows ->
        case arrangedRowsFromZSet rows of
          Nothing ->
            acc
          Just arrangedRows ->
            Map.insert key arrangedRows acc
    )
    Map.empty
{-# INLINE packedIndexFromKeyRows #-}

arrangedRowsFromZSet ::
  (Ord time, Ord val, Eq weight, AdditiveGroup weight) =>
  ZSet (Timed time val) weight ->
  Maybe (ArrangedRows time val weight)
arrangedRowsFromZSet rows =
  arrangedRowsFromCursor (cursorFromZSet rows)
{-# INLINE arrangedRowsFromZSet #-}

arrangedRowsFromCursor ::
  (Ord time, Ord val, Eq weight, AdditiveGroup weight) =>
  Cursor time val weight ->
  Maybe (ArrangedRows time val weight)
arrangedRowsFromCursor cursor =
  keepNonEmpty
    ArrangedRows
      { arrangedRowsValueCursor = cursor,
        arrangedRowsByTime = timeBucketsFromCursor cursor
      }
{-# INLINE arrangedRowsFromCursor #-}

mergeArrangedRows ::
  (Ord time, Ord val, Eq weight, AdditiveGroup weight) =>
  ArrangedRows time val weight ->
  ArrangedRows time val weight ->
  ArrangedRows time val weight
mergeArrangedRows left right =
  ArrangedRows
    { arrangedRowsValueCursor = cursorMerge (arrangedRowsValueCursor left) (arrangedRowsValueCursor right),
      arrangedRowsByTime = mergeTimeBuckets (arrangedRowsByTime left) (arrangedRowsByTime right)
    }
{-# INLINE mergeArrangedRows #-}

appendArrangedRowsZSet ::
  (Ord time, Ord val, Eq weight, AdditiveGroup weight) =>
  ZSet (Timed time val) weight ->
  ArrangedRows time val weight ->
  ArrangedRows time val weight
appendArrangedRowsZSet rows arrangedRows =
  appendArrangedRowsCursor (cursorFromZSet rows) arrangedRows
{-# INLINE appendArrangedRowsZSet #-}

appendArrangedRowsCursor ::
  (Ord time, Ord val, Eq weight, AdditiveGroup weight) =>
  Cursor time val weight ->
  ArrangedRows time val weight ->
  ArrangedRows time val weight
appendArrangedRowsCursor cursor arrangedRows =
  ArrangedRows
    { arrangedRowsValueCursor = cursorMerge (arrangedRowsValueCursor arrangedRows) cursor,
      arrangedRowsByTime = mergeTimeBuckets (arrangedRowsByTime arrangedRows) (timeBucketsFromCursor cursor)
    }
{-# INLINE appendArrangedRowsCursor #-}

timeBucketsFromCursor ::
  (Ord time, Ord val, Eq weight, AdditiveGroup weight) =>
  Cursor time val weight ->
  Map time (ZSet val weight)
timeBucketsFromCursor =
  foldCursorWithTime insertTimedValue Map.empty
  where
    insertTimedValue ::
      (Ord time, Ord val, Eq weight, AdditiveGroup weight) =>
      Map time (ZSet val weight) ->
      time ->
      val ->
      weight ->
      Map time (ZSet val weight)
    insertTimedValue buckets time val weight =
      Map.alter (insertTimeBucketValue val weight) time buckets
{-# INLINE timeBucketsFromCursor #-}

insertTimeBucketValue ::
  (Ord val, Eq weight, AdditiveGroup weight) =>
  val ->
  weight ->
  Maybe (ZSet val weight) ->
  Maybe (ZSet val weight)
insertTimeBucketValue val weight Nothing =
  keepNonEmptyZSet (ZSet.zsetSingleton val weight)
insertTimeBucketValue val weight (Just bucket) =
  keepNonEmptyZSet (ZSet.zsetInsert val weight bucket)
{-# INLINE insertTimeBucketValue #-}

mergeTimeBuckets ::
  (Ord time, Ord val, Eq weight, AdditiveGroup weight) =>
  Map time (ZSet val weight) ->
  Map time (ZSet val weight) ->
  Map time (ZSet val weight)
mergeTimeBuckets =
  Map.mergeWithKey mergeBucket id id
  where
    mergeBucket ::
      (Ord val, Eq weight, AdditiveGroup weight) =>
      time ->
      ZSet val weight ->
      ZSet val weight ->
      Maybe (ZSet val weight)
    mergeBucket _time left right =
      keepNonEmptyZSet (left <> right)
{-# INLINE mergeTimeBuckets #-}

keepNonEmpty :: ArrangedRows time val weight -> Maybe (ArrangedRows time val weight)
keepNonEmpty rows
  | cursorNull (arrangedRowsValueCursor rows) =
      Nothing
  | otherwise =
      Just rows
{-# INLINE keepNonEmpty #-}

keepNonEmptyZSet :: ZSet val weight -> Maybe (ZSet val weight)
keepNonEmptyZSet rows
  | ZSet.zsetNull rows =
      Nothing
  | otherwise =
      Just rows
{-# INLINE keepNonEmptyZSet #-}
