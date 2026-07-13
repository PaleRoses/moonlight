module RowProjection where

import Control.DeepSeq (NFData (..))
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Differential.Batch
import Common
import RowIndex
import Moonlight.Differential.Index.IndexedRows
import Moonlight.Differential.Index.Reverse.Batch qualified as ReverseBatch
import Moonlight.Differential.Index.RowArrangement
import Moonlight.Differential.Index.RowProjection
import Moonlight.Differential.Index.RowSet (rowSetSize)
import Moonlight.Differential.Trace
import Moonlight.Differential.Update (Update (..))

data PreparedReverseIndex = PreparedReverseIndex
  { preparedReverseSource :: !(Map.Map Int IntSet.IntSet),
    preparedReverseQueries :: ![IntSet.IntSet]
  }

instance NFData PreparedReverseIndex where
  rnf preparedCase =
    Map.size (preparedReverseSource preparedCase)
      `seq` length (preparedReverseQueries preparedCase)
      `seq` ()

data PreparedRowProjection = PreparedRowProjection
  { preparedRowProjectionBatch :: !(Batch Int Int Int Int),
    preparedRowProjectionTrace :: !(Trace Int Int Int Int),
    preparedRowProjectionDirtyKeys :: !(Set (Int, Int)),
    preparedRowProjectionPins :: !(IntMap.IntMap Int)
  }

instance NFData PreparedRowProjection where
  rnf preparedCase =
    length (batchToUpdates (preparedRowProjectionBatch preparedCase))
      `seq` foldTraceBatches (\batchCount _batch -> batchCount + 1) (0 :: Int) (preparedRowProjectionTrace preparedCase)
      `seq` Set.size (preparedRowProjectionDirtyKeys preparedCase)
      `seq` IntMap.size (preparedRowProjectionPins preparedCase)
      `seq` ()

data PreparedRowProjectionDelta = PreparedRowProjectionDelta
  { preparedRowProjectionDeltaRows :: !(IndexedRows Int (Int, Int) Int),
    preparedRowProjectionDeltaBatch :: !(Batch Int Int Int Int)
  }

instance NFData PreparedRowProjectionDelta where
  rnf preparedCase =
    indexedRowsSize (preparedRowProjectionDeltaRows preparedCase)
      `seq` batchRowCount (preparedRowProjectionDeltaBatch preparedCase)
      `seq` ()

reverseIndexCase :: Int -> PreparedReverseIndex
reverseIndexCase size =
  PreparedReverseIndex
    { preparedReverseSource =
        Map.fromAscList
          (fmap (\member -> (member, IntSet.fromList [member `mod` 128, (member * 3) `mod` 128])) [0 .. size - 1]),
      preparedReverseQueries =
        fmap (\seed -> IntSet.fromList [seed `mod` 128, (seed * 5 + 1) `mod` 128]) [0 .. 63]
    }

rowProjectionCase :: Int -> PreparedRowProjection
rowProjectionCase size =
  PreparedRowProjection
    { preparedRowProjectionBatch = batchValue,
      preparedRowProjectionTrace = traceFromBatches (fmap (\update -> fromUpdates [update]) updates),
      preparedRowProjectionDirtyKeys =
        Set.fromList (rowProjectionKeyAt <$> [0, 7 .. min (size - 1) 512]),
      preparedRowProjectionPins = IntMap.singleton 1 (0 `mod` 17)
    }
  where
    updates =
      rowProjectionUpdateAt <$> [0 .. size - 1]
    batchValue =
      fromUpdates updates

rowProjectionCaseDense :: Int -> PreparedRowProjection
rowProjectionCaseDense size =
  PreparedRowProjection
    { preparedRowProjectionBatch = batchValue,
      preparedRowProjectionTrace = traceFromBatches (fmap (\update -> fromUpdatesDense [update]) updates),
      preparedRowProjectionDirtyKeys =
        Set.fromList (rowProjectionKeyAt <$> [0, 7 .. min (size - 1) 512]),
      preparedRowProjectionPins = IntMap.singleton 1 (0 `mod` 17)
    }
  where
    updates =
      rowProjectionUpdateAt <$> [0 .. size - 1]
    batchValue =
      fromUpdatesDense updates

rowProjectionDeltaCase :: Int -> Either String PreparedRowProjectionDelta
rowProjectionDeltaCase size = do
  rows <-
    eitherShow
      ( snapshotTraceToIndexedRows
          benchIndexedRowFormat
          indexedLayoutColumns
          2
          rowProjectionCell
          (traceFromBatches (fmap singletonBatch initialUpdates))
      )
  Right
    PreparedRowProjectionDelta
      { preparedRowProjectionDeltaRows = rows,
        preparedRowProjectionDeltaBatch = fromUpdates deltaUpdates
      }
  where
    initialUpdates =
      rowProjectionUpdateAt <$> [0 .. size - 1]

    deltaUpdates =
      rowProjectionUpdateAt . (+ size) <$> [0 .. size - 1]

rowProjectionDeltaCaseDense :: Int -> Either String PreparedRowProjectionDelta
rowProjectionDeltaCaseDense size = do
  rows <-
    eitherShow
      ( snapshotTraceToIndexedRows
          benchIndexedRowFormat
          indexedLayoutColumns
          2
          rowProjectionCell
          (traceFromBatches (fmap singletonBatch initialUpdates))
      )
  Right
    PreparedRowProjectionDelta
      { preparedRowProjectionDeltaRows = rows,
        preparedRowProjectionDeltaBatch = fromUpdatesDense deltaUpdates
      }
  where
    initialUpdates =
      rowProjectionUpdateAt <$> [0 .. size - 1]

    deltaUpdates =
      rowProjectionUpdateAt . (+ size) <$> [0 .. size - 1]

reverseIndexAddLookupWeight :: PreparedReverseIndex -> Int
reverseIndexAddLookupWeight preparedCase =
  Foldable.foldl'
    (\acc query -> acc + Set.size (ReverseBatch.lookupMany index query))
    0
    (preparedReverseQueries preparedCase)
  where
    index =
      Map.foldlWithKey'
        (\reverseIndex member keys -> ReverseBatch.addMembership member keys reverseIndex)
        IntMap.empty
        (preparedReverseSource preparedCase)

reverseIndexRebuildWeight :: PreparedReverseIndex -> Int
reverseIndexRebuildWeight preparedCase =
  IntMap.size
    ( ReverseBatch.rebuildIntAxisFromMap
        (\_member keys -> keys)
        (preparedReverseSource preparedCase)
    )

rowProjectionBatchRowsWeight :: PreparedRowProjection -> Either String Int
rowProjectionBatchRowsWeight preparedCase =
  fmap indexedRowsSize
    ( eitherShow
        ( batchToIndexedRows
        benchIndexedRowFormat
        indexedLayoutColumns
        2
        rowProjectionCell
        (preparedRowProjectionBatch preparedCase)
        )
    )

rowProjectionProjectBatchDeltaWeight :: PreparedRowProjection -> Either String Int
rowProjectionProjectBatchDeltaWeight preparedCase =
  projectedRowsDeltaWeight
    <$> projectRowProjectionBatchDelta (preparedRowProjectionBatch preparedCase)

projectRowProjectionBatchDelta ::
  Batch Int Int Int Int ->
  Either String (ProjectedRowsDelta (Int, Int) Int)
projectRowProjectionBatchDelta =
  eitherShow
    . ( projectBatchDelta rowProjectionCell ::
          Batch Int Int Int Int ->
          Either
            (IndexedRowsProjectionError Int Int Int Int (Int, Int) Int)
            (ProjectedRowsDelta (Int, Int) Int)
      )

projectedRowsDeltaWeight :: ProjectedRowsDelta rowKey payload -> Int
projectedRowsDeltaWeight (ProjectedRowsDelta deltas) =
  Map.size deltas

rowProjectionApplyBatchDeltaWeight :: PreparedRowProjectionDelta -> Either String Int
rowProjectionApplyBatchDeltaWeight preparedCase =
  appliedRowsDeltaWeight
    <$> do
      projectedDelta <-
        projectRowProjectionBatchDelta (preparedRowProjectionDeltaBatch preparedCase)
      eitherShow
        ( applyProjectedRowsDelta
            benchIndexedRowFormat
            projectedDelta
            (preparedRowProjectionDeltaRows preparedCase)
        )

appliedRowsDeltaWeight :: (RowChanges (Int, Int) Int, IndexedRows Int (Int, Int) Int) -> Int
appliedRowsDeltaWeight (changes, rows) =
  rowChangesWeight changes + indexedRowsSize rows

rowProjectionRebuildValueIndexWeight :: PreparedRowProjectionDelta -> Either String Int
rowProjectionRebuildValueIndexWeight preparedCase =
  indexedRowsSize
    <$> eitherShow
      (indexedRowsRebuildValueIndex benchIndexedRowFormat (preparedRowProjectionDeltaRows preparedCase))

rowProjectionTraceArrangementWeight :: PreparedRowProjection -> Either String Int
rowProjectionTraceArrangementWeight preparedCase =
  fmap indexedRowArrangementWeight
    ( eitherShow
        ( indexedRowArrangementFromRows
        <$> snapshotTraceToIndexedRows
          benchIndexedRowFormat
          indexedLayoutColumns
          2
          rowProjectionCell
          (preparedRowProjectionTrace preparedCase)
        )
    )

rowProjectionTraceArrangementViaBatchWeight :: PreparedRowProjection -> Either String Int
rowProjectionTraceArrangementViaBatchWeight preparedCase =
  fmap indexedRowArrangementWeight
    ( eitherShow
        ( indexedRowArrangementFromRows
        <$> batchToIndexedRows
          benchIndexedRowFormat
          indexedLayoutColumns
          2
          rowProjectionCell
          (snapshotTraceBatch (preparedRowProjectionTrace preparedCase))
        )
    )

rowArrangementDirtyRestrictWeight :: PreparedRowProjection -> Either String Int
rowArrangementDirtyRestrictWeight preparedCase =
  fmap
    ( indexedRowArrangementWeight
        . indexedRowArrangementRestrictRowsByPins (preparedRowProjectionPins preparedCase)
        . indexedRowArrangementWithDirtyKeys (preparedRowProjectionDirtyKeys preparedCase)
    )
    ( eitherShow
        ( indexedRowArrangementFromRows
        <$> batchToIndexedRows
          benchIndexedRowFormat
          indexedLayoutColumns
          2
          rowProjectionCell
          (preparedRowProjectionBatch preparedCase)
        )
    )

indexedRowArrangementWeight :: IndexedRowArrangement Int (Int, Int) Int -> Int
indexedRowArrangementWeight arrangement =
  indexedRowsSize (indexedRowArrangementRows arrangement)
    + rowSetSize (indexedRowArrangementVisibleRows arrangement)
    + rowSetSize (indexedRowArrangementDirtyRows arrangement)

rowProjectionUpdateAt :: Int -> Update Int Int Int Int
rowProjectionUpdateAt index =
  Update
    { updateTime = index `mod` 128,
      updateKey = index `mod` 64,
      updateVal = index `mod` 17,
      updateWeight = weightAt index
    }

rowProjectionCell :: Int -> Int -> Int -> Int -> Maybe ((Int, Int), Int)
rowProjectionCell time key value weight =
  Just (rowProjectionKey time key value, weight)

rowProjectionKeyAt :: Int -> (Int, Int)
rowProjectionKeyAt index =
  rowProjectionKey (index `mod` 128) (index `mod` 64) (index `mod` 17)

rowProjectionKey :: Int -> Int -> Int -> (Int, Int)
rowProjectionKey time key value =
  (time * 4096 + key, value)
