module Storage where

import Control.DeepSeq (NFData (..))
import Data.Foldable qualified as Foldable
import Moonlight.Delta.Frontier (singletonUpperFrontier)
import Moonlight.Differential.Algebra.ZSet qualified as ZSet
import Moonlight.Differential.Arrangement
import Moonlight.Differential.Batch
import Common
import RowIndex
import RowProjection
import RuntimeSettle
import Moonlight.Differential.Cursor (Cursor, cursorCellCount)
import Moonlight.Differential.Index.RowProjection (snapshotTraceToIndexedRows)
import Moonlight.Differential.Index.RowArrangement (indexedRowArrangementFromRows)
import Moonlight.Differential.Operator.Join (foldDeltaJoin)
import Moonlight.Differential.Trace
import Moonlight.Differential.Update (Update (..))
import Numeric.Natural (Natural)

type BenchCursor = Cursor Int Char Int

type BenchStorageZSet = ZSet.IndexedZSet Int Int Int

newtype PreparedCursorMerge = PreparedCursorMerge (BenchCursor, BenchCursor)

instance NFData PreparedCursorMerge where
  rnf (PreparedCursorMerge (leftCursor, rightCursor)) =
    cursorCellCount leftCursor `seq` cursorCellCount rightCursor `seq` ()

newtype PreparedIndexedZSetEntries = PreparedIndexedZSetEntries [(Int, Int, Int)]

instance NFData PreparedIndexedZSetEntries where
  rnf (PreparedIndexedZSetEntries entries) =
    indexedZSetEntryWeight entries `seq` ()

newtype PreparedIndexedZSetMerge = PreparedIndexedZSetMerge (BenchStorageZSet, BenchStorageZSet)

instance NFData PreparedIndexedZSetMerge where
  rnf (PreparedIndexedZSetMerge (leftRows, rightRows)) =
    ZSet.indexedZSetCellCount leftRows `seq` ZSet.indexedZSetCellCount rightRows `seq` ()

newtype PreparedIndexedZSetSections = PreparedIndexedZSetSections [[(Int, Int, Int)]]

instance NFData PreparedIndexedZSetSections where
  rnf (PreparedIndexedZSetSections sections) =
    indexedZSetSectionsWeight sections `seq` ()

batchBuildCellCount :: PreparedUpdates -> Int
batchBuildCellCount (PreparedUpdates updates) =
  length (batchToUpdates (fromUpdates updates :: BenchBatch))

batchSingletonBuildCellCount :: PreparedUpdates -> Int
batchSingletonBuildCellCount (PreparedUpdates updates) =
  Foldable.foldl'
    (\count updateValue -> count + length (batchToUpdates (singletonBatch updateValue :: BenchBatch)))
    0
    updates

traceAccumWeight :: PreparedUpdates -> Int
traceAccumWeight (PreparedUpdates updates) =
  ZSet.indexedZSetFold
    (\acc _key values -> ZSet.zsetFold (\valueAcc _value weight -> valueAcc + weight) acc values)
    0
    (traceAccumUpTo traceCutoff (traceFromUpdates updates))

indexedZSetConstructCellCount :: PreparedIndexedZSetEntries -> Int
indexedZSetConstructCellCount (PreparedIndexedZSetEntries entries) =
  ZSet.indexedZSetCellCount (ZSet.indexedZSetFromList entries :: BenchStorageZSet)

indexedZSetMergeCellCount :: PreparedIndexedZSetMerge -> Int
indexedZSetMergeCellCount (PreparedIndexedZSetMerge (leftRows, rightRows)) =
  ZSet.indexedZSetCellCount (leftRows <> rightRows)

indexedZSetUnionCellCount :: PreparedIndexedZSetSections -> Int
indexedZSetUnionCellCount (PreparedIndexedZSetSections sections) =
  ZSet.indexedZSetCellCount
    (ZSet.indexedZSetUnions (ZSet.indexedZSetFromList <$> sections) :: BenchStorageZSet)

newtype PreparedMergeInputs = PreparedMergeInputs (BenchBatch, BenchBatch)

instance NFData PreparedMergeInputs where
  rnf (PreparedMergeInputs (leftBatch, rightBatch)) =
    length (batchToUpdates leftBatch) `seq` length (batchToUpdates rightBatch) `seq` ()

newtype PreparedBatches = PreparedBatches [BenchBatch]

instance NFData PreparedBatches where
  rnf (PreparedBatches batches) =
    length batches `seq` ()

data TracePhysicalProfile = TracePhysicalProfile
  { tracePhysicalProfileBatchCount :: !Int,
    tracePhysicalProfileInputRows :: !Int,
    tracePhysicalProfilePhysicalBatches :: !Int,
    tracePhysicalProfilePhysicalRows :: !Int,
    tracePhysicalProfileCompactedLayers :: !Int,
    tracePhysicalProfileRecentBatches :: !Int,
    tracePhysicalProfileVirtualWeight :: !Natural
  }

instance NFData TracePhysicalProfile where
  rnf stats =
    tracePhysicalProfileBatchCount stats
      `seq` tracePhysicalProfileInputRows stats
      `seq` tracePhysicalProfilePhysicalBatches stats
      `seq` tracePhysicalProfilePhysicalRows stats
      `seq` tracePhysicalProfileCompactedLayers stats
      `seq` tracePhysicalProfileRecentBatches stats
      `seq` tracePhysicalProfileVirtualWeight stats
      `seq` ()

data PreparedBinaryBatchMerger = PreparedBinaryBatchMerger
  { preparedBinaryMergeLeft :: !BenchBatch,
    preparedBinaryMergeRight :: !BenchBatch,
    preparedBinaryMergeFuel :: !BatchMergeFuel
  }

instance NFData PreparedBinaryBatchMerger where
  rnf preparedCase =
    length (batchToUpdates (preparedBinaryMergeLeft preparedCase))
      `seq` length (batchToUpdates (preparedBinaryMergeRight preparedCase))
      `seq` unBatchMergeFuel (preparedBinaryMergeFuel preparedCase)
      `seq` ()

data PreparedBatchCover = PreparedBatchCover
  { preparedBatchCoverFanIn :: !Int,
    preparedBatchCoverExpectedPlan :: !BatchCoverPlan,
    preparedBatchCoverBatches :: ![BenchBatch]
  }

instance NFData PreparedBatchCover where
  rnf preparedCase =
    preparedBatchCoverFanIn preparedCase
      `seq` preparedBatchCoverExpectedPlan preparedCase
      `seq` Foldable.foldl'
        (\count batch -> count + batchRowCount batch)
        0
        (preparedBatchCoverBatches preparedCase)
      `seq` ()

type BenchTrace = Trace Int String Char Int

data PreparedTraceRead = PreparedTraceRead
  { preparedTraceReadCutoff :: !Int,
    preparedTraceReadTrace :: !BenchTrace
  }

instance NFData PreparedTraceRead where
  rnf preparedTrace =
    preparedTraceReadCutoff preparedTrace
      `seq` length (batchToUpdates (snapshotTraceBatch (preparedTraceReadTrace preparedTrace)))
      `seq` ()

newtype PreparedTraceNull = PreparedTraceNull BenchTrace

instance NFData PreparedTraceNull where
  rnf (PreparedTraceNull traceValue) =
    traceSpinePhysicalVirtualWeight (traceSpine traceValue)
      `seq` traceSpinePhysicalRowCount (traceSpine traceValue)
      `seq` ()

newtype PreparedTraceCompactNoOp = PreparedTraceCompactNoOp BenchTrace

instance NFData PreparedTraceCompactNoOp where
  rnf (PreparedTraceCompactNoOp traceValue) =
    traceSpineRecentBatchCount (traceSpine traceValue)
      `seq` traceSpinePhysicalVirtualWeight (traceSpine traceValue)
      `seq` ()

data PeriodicTraceState = PeriodicTraceState
  { periodicTraceBatchIndex :: !Int,
    periodicTraceValue :: !BenchTrace
  }

data PreparedDecomposedPipeline = PreparedDecomposedPipeline
  { preparedDecomposedPipelineUpdates :: ![BenchUpdate],
    preparedDecomposedPipelineBatches :: ![BenchBatch],
    preparedDecomposedPipelineTrace :: !BenchTrace,
    preparedDecomposedPipelineArrangement :: !BenchArrangement,
    preparedDecomposedPipelineDelta :: !BenchBatch,
    preparedDecomposedPipelineSettle :: !PreparedRuntimeSettle
  }

instance NFData PreparedDecomposedPipeline where
  rnf preparedCase =
    length (preparedDecomposedPipelineUpdates preparedCase)
      `seq` length (preparedDecomposedPipelineBatches preparedCase)
      `seq` traceSpinePhysicalVirtualWeight (traceSpine (preparedDecomposedPipelineTrace preparedCase))
      `seq` arrangementCellCount (preparedDecomposedPipelineArrangement preparedCase)
      `seq` batchRowCount (preparedDecomposedPipelineDelta preparedCase)
      `seq` rnf (preparedDecomposedPipelineSettle preparedCase)

data PreparedRetractionPipeline = PreparedRetractionPipeline
  { preparedRetractionPositiveTrace :: !BenchTrace,
    preparedRetractionDelta :: !BenchBatch,
    preparedRetractionTrace :: !BenchTrace,
    preparedRetractionArrangementBefore :: !BenchArrangement
  }

instance NFData PreparedRetractionPipeline where
  rnf preparedCase =
    traceSpinePhysicalVirtualWeight (traceSpine (preparedRetractionPositiveTrace preparedCase))
      `seq` batchRowCount (preparedRetractionDelta preparedCase)
      `seq` traceSpinePhysicalVirtualWeight (traceSpine (preparedRetractionTrace preparedCase))
      `seq` arrangementCellCount (preparedRetractionArrangementBefore preparedCase)
      `seq` ()

data PreparedSpinesLike = PreparedSpinesLike
  { preparedSpinesLikeBatches :: ![BenchBatch],
    preparedSpinesLikeArrangement :: !BenchArrangement,
    preparedSpinesLikeQueries :: ![String]
  }

instance NFData PreparedSpinesLike where
  rnf preparedCase =
    length (preparedSpinesLikeBatches preparedCase)
      `seq` arrangementCellCount (preparedSpinesLikeArrangement preparedCase)
      `seq` length (preparedSpinesLikeQueries preparedCase)
      `seq` ()

indexedZSetStorageEntries :: Int -> PreparedIndexedZSetEntries
indexedZSetStorageEntries size =
  PreparedIndexedZSetEntries (indexedZSetStorageEntryAt <$> [0 .. size - 1])

indexedZSetSingletonSections :: Int -> PreparedIndexedZSetSections
indexedZSetSingletonSections size =
  PreparedIndexedZSetSections (fmap (pure . indexedZSetStorageEntryAt) [0 .. size - 1])

indexedZSetStorageMergeCase :: Int -> PreparedIndexedZSetMerge
indexedZSetStorageMergeCase size =
  PreparedIndexedZSetMerge
    ( ZSet.indexedZSetFromList entries,
      ZSet.indexedZSetFromList (negateIndexedZSetEntry <$> entries)
    )
  where
    PreparedIndexedZSetEntries entries =
      indexedZSetStorageEntries size

indexedZSetStorageDisjointMergeCase :: Int -> PreparedIndexedZSetMerge
indexedZSetStorageDisjointMergeCase size =
  PreparedIndexedZSetMerge
    ( ZSet.indexedZSetFromList entries,
      ZSet.indexedZSetFromList (shiftIndexedZSetEntry size <$> entries)
    )
  where
    PreparedIndexedZSetEntries entries =
      indexedZSetStorageEntries size

indexedZSetStoragePartialCancelMergeCase :: Int -> PreparedIndexedZSetMerge
indexedZSetStoragePartialCancelMergeCase size =
  PreparedIndexedZSetMerge
    ( ZSet.indexedZSetFromList entries,
      ZSet.indexedZSetFromList (partialCancelIndexedZSetEntry size <$> entries)
    )
  where
    PreparedIndexedZSetEntries entries =
      indexedZSetStorageEntries size

indexedZSetStorageEntryAt :: Int -> (Int, Int, Int)
indexedZSetStorageEntryAt index =
  ( index `mod` 1024,
    ((index * 17) + (index `quot` 1024)) `mod` 4096,
    weightAt index
  )

negateIndexedZSetEntry :: (Int, Int, Int) -> (Int, Int, Int)
negateIndexedZSetEntry (key, value, weight) =
  (key, value, negate weight)

shiftIndexedZSetEntry :: Int -> (Int, Int, Int) -> (Int, Int, Int)
shiftIndexedZSetEntry size (key, value, weight) =
  (key + size + 1024, value + size + 4096, weight)

partialCancelIndexedZSetEntry :: Int -> (Int, Int, Int) -> (Int, Int, Int)
partialCancelIndexedZSetEntry size entry@(key, _value, _weight) =
  if even key
    then negateIndexedZSetEntry entry
    else shiftIndexedZSetEntry size entry

indexedZSetEntryWeight :: [(Int, Int, Int)] -> Int
indexedZSetEntryWeight =
  Foldable.foldl'
    ( \acc (key, value, weight) ->
        acc + key + value + weight
    )
    0

indexedZSetSectionsWeight :: [[(Int, Int, Int)]] -> Int
indexedZSetSectionsWeight =
  Foldable.foldl' (\acc section -> acc + indexedZSetEntryWeight section) 0

mergeCase :: Int -> PreparedMergeInputs
mergeCase size =
  PreparedMergeInputs
    ( fromUpdates (preparedUpdates (updateCase size)),
      fromUpdates (preparedUpdates (cancellationUpdateCase size))
    )

binaryBatchMergerCase :: Int -> BatchMergeFuel -> PreparedBinaryBatchMerger
binaryBatchMergerCase size fuel =
  PreparedBinaryBatchMerger
    { preparedBinaryMergeLeft = fromUpdates (monotoneUpdateAt <$> [0 .. size - 1]),
      preparedBinaryMergeRight = fromUpdates (shiftedMonotoneUpdateAt size <$> [0 .. size - 1]),
      preparedBinaryMergeFuel = fuel
    }

manySmallBatchCase :: Int -> PreparedBatches
manySmallBatchCase size =
  PreparedBatches
    ( fmap
        (\index -> fromUpdates [updateAt index])
        [0 .. size - 1]
    )

disjointBatchCoverCase :: Int -> Int -> PreparedBatchCover
disjointBatchCoverCase size fanIn =
  PreparedBatchCover
    { preparedBatchCoverFanIn = fanIn,
      preparedBatchCoverExpectedPlan = CoverStrictlyDisjoint,
      preparedBatchCoverBatches =
        disjointBatchCoverSectionBatch size fanIn <$> [0 .. fanIn - 1]
    }

boundaryOverlapBatchCoverCase :: Int -> Int -> PreparedBatchCover
boundaryOverlapBatchCoverCase size fanIn =
  PreparedBatchCover
    { preparedBatchCoverFanIn = fanIn,
      preparedBatchCoverExpectedPlan = CoverBoundaryOverlap,
      preparedBatchCoverBatches =
        boundaryOverlapBatchCoverSectionBatch size fanIn <$> [0 .. fanIn - 1]
    }

overlappingBatchCoverCase :: Int -> Int -> PreparedBatchCover
overlappingBatchCoverCase size fanIn =
  PreparedBatchCover
    { preparedBatchCoverFanIn = fanIn,
      preparedBatchCoverExpectedPlan = CoverKWayOverlap,
      preparedBatchCoverBatches =
        overlappingBatchCoverSectionBatch size fanIn <$> [0 .. fanIn - 1]
    }

disjointBatchCoverSectionBatch :: Int -> Int -> Int -> BenchBatch
disjointBatchCoverSectionBatch size fanIn section =
  fromUpdates (disjointCoverUpdateAt <$> [sectionStart .. sectionEnd])
  where
    sectionWidth =
      max 1 (size `quot` max 1 fanIn)

    sectionStart =
      section * sectionWidth

    sectionEnd =
      min (size - 1) (sectionStart + sectionWidth - 1)

boundaryOverlapBatchCoverSectionBatch :: Int -> Int -> Int -> BenchBatch
boundaryOverlapBatchCoverSectionBatch size fanIn section =
  fromUpdates (disjointCoverUpdateAt <$> [sectionStart .. sectionEnd])
  where
    sectionWidth =
      max 1 (size `quot` max 1 fanIn)

    sectionStart =
      if section == 0
        then 0
        else section * sectionWidth - 1

    sectionEnd =
      min (size - 1) (section * sectionWidth + sectionWidth - 1)

overlappingBatchCoverSectionBatch :: Int -> Int -> Int -> BenchBatch
overlappingBatchCoverSectionBatch size fanIn section =
  fromUpdates
    ( fmap
        (overlappingMonotoneUpdateAt fanIn section)
        [0 .. sectionWidth - 1]
    )
  where
    sectionWidth =
      max 1 (size `quot` max 1 fanIn)

disjointCoverUpdateAt :: Int -> BenchUpdate
disjointCoverUpdateAt index =
  Update
    { updateTime = index,
      updateKey = "merge-key-" <> show (1000000 + index),
      updateVal = 'a',
      updateWeight = 1
    }

periodicTraceBatchCase :: Int -> PreparedBatches
periodicTraceBatchCase size =
  PreparedBatches
    ( fmap
        (\index -> fromUpdates [monotoneUpdateAt index])
        [0 .. size - 1]
    )

retainedPrefixTraceReadCase :: Int -> PreparedTraceRead
retainedPrefixTraceReadCase size =
  PreparedTraceRead
    { preparedTraceReadCutoff = retainedPrefixCutoff size,
      preparedTraceReadTrace =
        coalesceTraceBatches
          (singletonUpperFrontier (retainedPrefixCutoff size))
          (traceFromBatches batches)
    }
  where
    PreparedBatches batches =
      periodicTraceBatchCase size

physicallyEmptyTraceNullCase :: Int -> PreparedTraceNull
physicallyEmptyTraceNullCase size =
  PreparedTraceNull
    ( traceCompactPhysicalBefore
        (singletonUpperFrontier (size + 1))
        (traceAppendBatch negativeBatch compactedPositive)
    )
  where
    positiveUpdates =
      monotoneUpdateAt <$> [0 .. size - 1]
    negativeUpdates =
      fmap (\updateValue -> updateValue {updateWeight = negate (updateWeight updateValue)}) positiveUpdates
    positiveBatch =
      fromUpdates positiveUpdates :: BenchBatch
    negativeBatch =
      fromUpdates negativeUpdates :: BenchBatch
    compactedPositive =
      coalesceTraceBatches (singletonUpperFrontier (size + 1)) (traceFromBatch positiveBatch)

traceCompactNoOpCase :: Int -> PreparedTraceCompactNoOp
traceCompactNoOpCase size =
  PreparedTraceCompactNoOp (traceFromBatches batches)
  where
    batches =
      fmap
        (\index -> fromUpdates [(monotoneUpdateAt index) {updateTime = index + 1}])
        [0 .. size - 1]

mergeTwoLargeCount :: PreparedMergeInputs -> Int
mergeTwoLargeCount (PreparedMergeInputs (leftBatch, rightBatch)) =
  batchRowCount (mergeBatch leftBatch rightBatch)

mergeManySmallCount :: PreparedBatches -> Int
mergeManySmallCount (PreparedBatches batches) =
  batchRowCount (mergeBatches batches)

binaryBatchMergerFuelWeight :: PreparedBinaryBatchMerger -> Int
binaryBatchMergerFuelWeight preparedCase =
  if batchMergeDone workedMerger
    then batchRowCount (finishBatchMerge workedMerger)
    else batchRowCount (preparedBinaryMergeLeft preparedCase) + batchRowCount (preparedBinaryMergeRight preparedCase)
  where
    workedMerger =
      workBatchMerge
        (preparedBinaryMergeFuel preparedCase)
        (beginBatchMerge (preparedBinaryMergeLeft preparedCase) (preparedBinaryMergeRight preparedCase))

binaryBatchMergerFinishWeight :: PreparedBinaryBatchMerger -> Int
binaryBatchMergerFinishWeight preparedCase =
  batchRowCount
    ( finishBatchMerge
        (beginBatchMerge (preparedBinaryMergeLeft preparedCase) (preparedBinaryMergeRight preparedCase))
    )

batchCoverMergeWeight :: PreparedBatchCover -> Int
batchCoverMergeWeight preparedCase =
  batchRowCount (mergeBatches (preparedBatchCoverBatches preparedCase))

checkedBatchCoverCase :: PreparedBatchCover -> IO PreparedBatchCover
checkedBatchCoverCase preparedCase =
  if actualPlan == preparedBatchCoverExpectedPlan preparedCase
    then pure preparedCase
    else fail ("batch cover fixture expected " <> show (preparedBatchCoverExpectedPlan preparedCase) <> " but measured " <> show actualPlan)
  where
    actualPlan =
      batchCoverPlan (preparedBatchCoverBatches preparedCase)

traceAppendToBatchCount :: PreparedBatches -> Int
traceAppendToBatchCount (PreparedBatches batches) =
  batchRowCount
    ( snapshotTraceBatch
        ( Foldable.foldl'
            (\traceValue batch -> traceAppendBatch batch traceValue)
            (traceFromBatch (mempty :: BenchBatch))
            batches
        )
    )

traceAppendOnlyStats :: PreparedBatches -> TracePhysicalProfile
traceAppendOnlyStats preparedBatches =
  tracePhysicalProfile preparedBatches (traceFromPreparedBatches preparedBatches)

traceAdvanceSinceStats :: PreparedBatches -> Either String TracePhysicalProfile
traceAdvanceSinceStats preparedBatches@(PreparedBatches batches) =
  case traceAdvanceSince (singletonUpperFrontier (length batches)) (traceFromPreparedBatches preparedBatches) of
    Left obstruction ->
      Left (show obstruction)
    Right traceValue ->
      Right (tracePhysicalProfile preparedBatches traceValue)

traceCompactPhysicalStats :: PreparedBatches -> TracePhysicalProfile
traceCompactPhysicalStats preparedBatches@(PreparedBatches batches) =
  tracePhysicalProfile preparedBatches (traceCompactPhysicalBefore (singletonUpperFrontier (length batches)) (traceFromPreparedBatches preparedBatches))

traceCompactPhysicalStepWeight :: PreparedBatches -> Int
traceCompactPhysicalStepWeight preparedBatches@(PreparedBatches batches) =
  tracePhysicalCompactionStepStatsWeight stats + traceProfileWeight traceValue
  where
    (stats, traceValue) =
      compactTracePhysicalStep
        (TraceCompactionFuel 64)
        (singletonUpperFrontier (length batches))
        (traceFromPreparedBatches preparedBatches)

tracePhysicalCompactionStepStatsWeight :: TracePhysicalCompactionStepStats -> Int
tracePhysicalCompactionStepStatsWeight stats =
  tracePhysicalCompactionBatchesConsumed stats
    + tracePhysicalCompactionInputRowsVisited stats
    + fromIntegral (tracePhysicalCompactionMergeFuelConsumed stats)
    + tracePhysicalCompactionActiveMergeCount stats
    + tracePhysicalCompactionOutputLayers stats

traceSnapshotBatchRowCount :: PreparedBatches -> Int
traceSnapshotBatchRowCount preparedBatches =
  batchRowCount (snapshotTraceBatch (traceFromPreparedBatches preparedBatches))

traceAppendPeriodicMaintenanceSnapshotCount :: PreparedBatches -> Int
traceAppendPeriodicMaintenanceSnapshotCount (PreparedBatches batches) =
  batchRowCount (snapshotTraceBatch (periodicTraceValue (Foldable.foldl' appendPeriodicBatch emptyPeriodicTraceState batches)))

traceAppendPeriodicPhysicalProfileWeight :: PreparedBatches -> Int
traceAppendPeriodicPhysicalProfileWeight (PreparedBatches batches) =
  traceProfileWeight (periodicTraceValue (Foldable.foldl' appendPeriodicBatch emptyPeriodicTraceState batches))

traceProfileWeight :: BenchTrace -> Int
traceProfileWeight traceValue =
  let spine =
        traceSpine traceValue
   in traceSpinePhysicalBatchCount spine
        + traceSpinePhysicalRowCount spine
        + fromIntegral (traceSpinePhysicalVirtualWeight spine)
        + traceSpineCompactedLayerCount spine
        + traceSpineRecentBatchCount spine

traceFromPreparedBatches :: PreparedBatches -> BenchTrace
traceFromPreparedBatches (PreparedBatches batches) =
  Foldable.foldl'
    (\traceValue batch -> traceAppendBatch batch traceValue)
    (traceFromBatch (mempty :: BenchBatch))
    batches

tracePhysicalProfile :: PreparedBatches -> BenchTrace -> TracePhysicalProfile
tracePhysicalProfile preparedBatches@(PreparedBatches _batches) traceValue =
  TracePhysicalProfile
    { tracePhysicalProfileBatchCount = preparedBatchCount preparedBatches,
      tracePhysicalProfileInputRows = preparedBatchRows preparedBatches,
      tracePhysicalProfilePhysicalBatches = traceSpinePhysicalBatchCount spine,
      tracePhysicalProfilePhysicalRows = traceSpinePhysicalRowCount spine,
      tracePhysicalProfileCompactedLayers = traceSpineCompactedLayerCount spine,
      tracePhysicalProfileRecentBatches = traceSpineRecentBatchCount spine,
      tracePhysicalProfileVirtualWeight = traceSpinePhysicalVirtualWeight spine
    }
  where
    spine =
      traceSpine traceValue

preparedBatchCount :: PreparedBatches -> Int
preparedBatchCount (PreparedBatches batches) =
  length batches

preparedBatchRows :: PreparedBatches -> Int
preparedBatchRows (PreparedBatches batches) =
  Foldable.foldl' (\count batch -> count + batchRowCount batch) 0 batches

traceRetainedPrefixReadWeight :: PreparedTraceRead -> Int
traceRetainedPrefixReadWeight preparedTrace =
  foldTraceAccumUpTo
    (\acc _key _value weight -> acc + weight)
    0
    (preparedTraceReadCutoff preparedTrace)
    (preparedTraceReadTrace preparedTrace)

traceRetainedPrefixMaterializedWeight :: PreparedTraceRead -> Int
traceRetainedPrefixMaterializedWeight preparedTrace =
  ZSet.indexedZSetFold
    (\acc _key values -> ZSet.zsetFold (\valueAcc _value weight -> valueAcc + weight) acc values)
    0
    (traceAccumUpTo (preparedTraceReadCutoff preparedTrace) (preparedTraceReadTrace preparedTrace))

traceKeyPrefixReadWeight :: PreparedTraceRead -> Int
traceKeyPrefixReadWeight preparedTrace =
  foldTraceKeyThrough
    (preparedTraceReadCutoff preparedTrace)
    hotKey
    (\acc _time _value weight -> acc + weight)
    0
    (preparedTraceReadTrace preparedTrace)

traceKeyPrefixViaArrangementWeight :: PreparedTraceRead -> Int
traceKeyPrefixViaArrangementWeight preparedTrace =
  foldSliceThrough
    (preparedTraceReadCutoff preparedTrace)
    hotKey
    (\acc _time _value weight -> acc + weight)
    0
    (arrangeByKey (preparedTraceReadTrace preparedTrace))

traceNullWeight :: PreparedTraceNull -> Int
traceNullWeight (PreparedTraceNull traceValue) =
  if traceNull traceValue
    then 1
    else 0

traceNullViaBatchWeight :: PreparedTraceNull -> Int
traceNullViaBatchWeight (PreparedTraceNull traceValue) =
  if batchNull (snapshotTraceBatch traceValue)
    then 1
    else 0

traceCompactNoOpProfileWeight :: PreparedTraceCompactNoOp -> Int
traceCompactNoOpProfileWeight (PreparedTraceCompactNoOp traceValue) =
  let spine =
        traceSpine (traceCompactPhysicalBefore (singletonUpperFrontier 0) traceValue)
   in traceSpinePhysicalBatchCount spine
        + traceSpinePhysicalRowCount spine
        + fromIntegral (traceSpinePhysicalVirtualWeight spine)
        + traceSpineCompactedLayerCount spine
        + traceSpineRecentBatchCount spine

appendPeriodicBatch :: PeriodicTraceState -> BenchBatch -> PeriodicTraceState
appendPeriodicBatch state batch =
  PeriodicTraceState
    { periodicTraceBatchIndex = nextIndex,
      periodicTraceValue =
        if nextIndex `mod` traceCompactionPeriod == 0
          then coalesceTraceBatches (singletonUpperFrontier (nextIndex - 1)) appendedTrace
          else appendedTrace
    }
  where
    nextIndex =
      periodicTraceBatchIndex state + 1
    appendedTrace =
      traceAppendBatch batch (periodicTraceValue state)

emptyPeriodicTraceState :: PeriodicTraceState
emptyPeriodicTraceState =
  PeriodicTraceState
    { periodicTraceBatchIndex = 0,
      periodicTraceValue = traceFromBatch (mempty :: BenchBatch)
    }

traceCompactionPeriod :: Int
traceCompactionPeriod =
  64

decomposedDbspDdSizes :: [Int]
decomposedDbspDdSizes =
  [512, 2048]

batchCoverFanIns :: [Int]
batchCoverFanIns =
  [8, 64]

decomposedPipelineCase :: Int -> PreparedDecomposedPipeline
decomposedPipelineCase size =
  PreparedDecomposedPipeline
    { preparedDecomposedPipelineUpdates = updates,
      preparedDecomposedPipelineBatches = batches,
      preparedDecomposedPipelineTrace = traceValue,
      preparedDecomposedPipelineArrangement = arrangeByKey traceValue,
      preparedDecomposedPipelineDelta = fromUpdates (preparedUpdates (shiftedUpdateCase size)),
      preparedDecomposedPipelineSettle = runtimeSettleCase size
    }
  where
    updates =
      monotoneUpdateAt <$> [0 .. size - 1]

    batches =
      fmap singletonBatch updates

    traceValue =
      periodicTraceValue (Foldable.foldl' appendPeriodicBatch emptyPeriodicTraceState batches)

decomposedPipelineBatchBuildWeight :: PreparedDecomposedPipeline -> Int
decomposedPipelineBatchBuildWeight preparedCase =
  Foldable.foldl'
    (\count updateValue -> count + batchRowCount (singletonBatch updateValue :: BenchBatch))
    0
    (preparedDecomposedPipelineUpdates preparedCase)

decomposedPipelineTraceIngestWeight :: PreparedDecomposedPipeline -> Int
decomposedPipelineTraceIngestWeight preparedCase =
  traceAppendPeriodicMaintenanceSnapshotCount (PreparedBatches (preparedDecomposedPipelineBatches preparedCase))

decomposedPipelineArrangementWeight :: PreparedDecomposedPipeline -> Int
decomposedPipelineArrangementWeight preparedCase =
  arrangementCellCount (arrangeByKey (preparedDecomposedPipelineTrace preparedCase))

decomposedPipelineJoinWeight :: PreparedDecomposedPipeline -> Int
decomposedPipelineJoinWeight preparedCase =
  foldDeltaJoin
    pairProjection
    (\count _time _key _value _weight -> count + 1)
    0
    (preparedDecomposedPipelineDelta preparedCase)
    (preparedDecomposedPipelineArrangement preparedCase)

decomposedPipelineProjectionWeight :: PreparedDecomposedPipeline -> Either String Int
decomposedPipelineProjectionWeight preparedCase =
  fmap
    (indexedRowArrangementWeight . indexedRowArrangementFromRows)
    ( eitherShow
        ( snapshotTraceToIndexedRows
        benchIndexedRowFormat
        indexedLayoutColumns
        2
        decomposedPipelineProjectionCell
        (preparedDecomposedPipelineTrace preparedCase)
        )
    )

decomposedPipelineSettleWeight :: PreparedDecomposedPipeline -> Either String Int
decomposedPipelineSettleWeight =
  runtimeSettleWeight . preparedDecomposedPipelineSettle

decomposedPipelineProjectionCell :: Int -> String -> Char -> Int -> Maybe ((Int, Int), Int)
decomposedPipelineProjectionCell time _key value weight =
  Just ((time, fromEnum value), weight)

retractionPipelineCase :: Int -> PreparedRetractionPipeline
retractionPipelineCase size =
  PreparedRetractionPipeline
    { preparedRetractionPositiveTrace = positiveTrace,
      preparedRetractionDelta = retractionBatch,
      preparedRetractionTrace = retractedTrace,
      preparedRetractionArrangementBefore = arrangeByKey positiveTrace
    }
  where
    positiveUpdates =
      monotoneUpdateAt <$> [0 .. size - 1]

    negativeUpdates =
      negateUpdateWeight <$> positiveUpdates

    positiveTrace =
      coalesceTraceBatches
        (singletonUpperFrontier (size + 1))
        (traceFromBatches (fmap singletonBatch positiveUpdates))

    retractionBatch =
      fromUpdates negativeUpdates

    retractedTrace =
      coalesceTraceBatches
        (singletonUpperFrontier (size + 1))
        (traceAppendBatch retractionBatch positiveTrace)

retractionTraceApplyWeight :: PreparedRetractionPipeline -> Int
retractionTraceApplyWeight preparedCase =
  traceSpinePhysicalBatchCount spine
    + traceSpinePhysicalRowCount spine
    + fromIntegral (traceSpinePhysicalVirtualWeight spine)
  where
    spine =
      traceSpine
        ( coalesceTraceBatches
            (singletonUpperFrontier (batchRowCount (preparedRetractionDelta preparedCase) + 1))
            (traceAppendBatch (preparedRetractionDelta preparedCase) (preparedRetractionPositiveTrace preparedCase))
        )

retractionMaterializedWeight :: PreparedRetractionPipeline -> Int
retractionMaterializedWeight =
  batchRowCount . snapshotTraceBatch . preparedRetractionTrace

retractionArrangementWeight :: PreparedRetractionPipeline -> Int
retractionArrangementWeight =
  arrangementCellCount . arrangeByKey . preparedRetractionTrace

retractionJoinWeight :: PreparedRetractionPipeline -> Int
retractionJoinWeight preparedCase =
  foldDeltaJoin
    pairProjection
    (\count _time _key _value _weight -> count + 1)
    0
    (preparedRetractionDelta preparedCase)
    (preparedRetractionArrangementBefore preparedCase)

retractionProjectionWeight :: PreparedRetractionPipeline -> Either String Int
retractionProjectionWeight preparedCase =
  fmap
    (indexedRowArrangementWeight . indexedRowArrangementFromRows)
    ( eitherShow
        ( snapshotTraceToIndexedRows
        benchIndexedRowFormat
        indexedLayoutColumns
        2
        decomposedPipelineProjectionCell
        (preparedRetractionTrace preparedCase)
        )
    )

spinesLikeCase :: Int -> PreparedSpinesLike
spinesLikeCase size =
  PreparedSpinesLike
    { preparedSpinesLikeBatches = batches,
      preparedSpinesLikeArrangement = arrangeByKey (traceFromBatches batches),
      preparedSpinesLikeQueries = keyAt <$> [0 .. 63]
    }
  where
    PreparedBatches batches =
      periodicTraceBatchCase size

ddSpinesLikeLoadArrangeWeight :: PreparedSpinesLike -> Int
ddSpinesLikeLoadArrangeWeight preparedCase =
  arrangementCellCount (arrangeByKey (traceFromBatches (preparedSpinesLikeBatches preparedCase)))

ddSpinesLikeKeyQueryWeight :: PreparedSpinesLike -> Int
ddSpinesLikeKeyQueryWeight preparedCase =
  Foldable.foldl'
    (\count key -> count + foldArrangementKey key (\acc _time _value _weight -> acc + 1) 0 (preparedSpinesLikeArrangement preparedCase))
    0
    (preparedSpinesLikeQueries preparedCase)

retainedPrefixCutoff :: Int -> Int
retainedPrefixCutoff size =
  size `quot` 2
