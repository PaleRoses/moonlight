{-# LANGUAGE DerivingStrategies #-}

module BatchTraceSpec
  ( tests,
  )
where

import Data.Foldable qualified as Foldable
import Data.Sequence qualified as Seq

import Moonlight.Delta.Frontier
  ( UpperFrontier,
    frontierPoints,
    singletonUpperFrontier,
    upperFrontierPoints,
  )
import Moonlight.Differential.Algebra.ZSet qualified as ZSet
import Moonlight.Differential.Batch
  ( Batch,
    BatchMergeFuel (..),
    BatchMergeWork (..),
    batchLower,
    batchMergeDone,
    batchNull,
    batchRowCount,
    batchToUpdates,
    beginBatchMerge,
    batchUpper,
    emptyBatch,
    finishBatchMerge,
    fromUpdates,
    mergeBatch,
    mergeBatches,
    singletonBatch,
    workBatchMerge,
    workBatchMergeMeasured,
  )
import Moonlight.Differential.Trace
  ( Trace,
    TraceCompactionFuel (..),
    TraceFrontierAdvanceError (..),
    TracePhysicalCompactionStepStats (..),
    coalesceTraceBatches,
    compactTracePhysicalStep,
    foldTraceBatches,
    traceAccumUpTo,
    traceAdvanceSince,
    traceAdvanceUpper,
    traceAppendBatch,
    traceRecentBatches,
    traceCompactPhysicalBefore,
    traceFromBatch,
    traceFromBatches,
    traceFromUpdates,
    traceNull,
    traceSince,
    traceSpine,
    traceSpineCompacted,
    traceSpineCompactedLayerCount,
    traceSpinePhysicalBatchCount,
    traceSpinePhysicalRowCount,
    traceSpinePhysicalVirtualWeight,
    traceSpineRecentBatchCount,
    snapshotTraceBatch,
    traceUpper,
  )
import Moonlight.Differential.Update
  ( Update (..),
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )

type TestBatch = Batch Int String Char Int

type TestTrace = Trace Int String Char Int

type TestTraceUpdate = Update Int String Char Int

tests :: TestTree
tests =
  testGroup
    "batch and trace laws"
    [ testCase "Batch consolidation removes zero weights and combines equal cells" batchConsolidatesUpdates,
      testCase "Batch singleton construction agrees with one-update consolidation" batchSingletonAgreesWithFromUpdates,
      testCase "Batch merge is associative modulo consolidation" batchMergeAssociative,
      testCase "Batch merge uses empty batch as identity" batchMergeEmptyIdentity,
      testCase "Batch merger measured fuel reports consumed work" batchMergerMeasuredFuelReportsConsumedWork,
      testCase "Batch merger can suspend and finish without changing denotation" batchMergerSuspendFinishDenotesMerge,
      testCase "Batch merger suffix drain consolidates boundary duplicates" batchMergerSuffixDrainConsolidatesBoundaryDuplicates,
      testCase "Batch mergeBatches agrees with repeated binary merge" batchMergeBatchesAgreesWithBinaryMerge,
      testCase "Batch mergeBatches k-way merge consolidates shared cells" batchMergeBatchesKWayConsolidatesSharedCells,
      testCase "Batch mergeBatches singleton cover denotes direct replay" batchMergeBatchesSingletonCoverDenotesReplay,
      testCase "Batch fold order is deterministic by key, value, then time" batchFoldOrderDeterministic,
      testCase "Batch folds round-trip through fromUpdates" batchFoldRoundTrip,
      testCase "Batch fromUpdates owns consolidation without Collection wrapper" batchFromUpdatesOwnsConsolidation,
      testCase "Trace over Batch folds back to the consolidated batch" traceOverBatchConsolidates,
      testCase "Trace append and compaction preserve consolidated denotation" traceAppendAndCompactionLaws,
      testCase "Trace singleton-prefix compaction denotes direct replay" traceSingletonPrefixCompactionDenotesReplay,
      testCase "Trace fueled physical compaction exposes physical-step stats" traceFueledPhysicalCompactionReportsStats,
      testCase "Trace physical compaction schedules preserve denotation and frontiers" tracePhysicalCompactionSchedulesPreserveDenotation,
      testCase "Trace physical reads snapshot active mergers without finishing them" tracePhysicalReadSnapshotsActiveMergerSources,
      testCase "Trace physical layers preserve virtual effort through cancellation" tracePhysicalLayerVirtualWeightKeepsEmptyBookkeeping,
      testCase "Trace null recognizes physically empty compacted layers" traceNullRecognizesPhysicallyEmptyCompactedLayers,
      testCase "Trace physical compaction is no-op when no recent batch is eligible" tracePhysicalCompactionNoOpWhenNothingEligible,
      testCase "Trace since merge and explicit advances are monotone" traceSinceAdvanceLaws,
      testCase "Trace accumulation gives the denotational prefix view" traceAccumUpToReadsPrefix
    ]

testBatch :: [Update Int String Char Int] -> TestBatch
testBatch =
  fromUpdates

batchConsolidatesUpdates :: IO ()
batchConsolidatesUpdates = do
  let batch =
        testBatch
          [ Update 0 "key" 'a' 1,
            Update 0 "key" 'a' 4,
            Update 1 "key" 'a' 3,
            Update 1 "key" 'a' (-3),
            Update 2 "key" 'b' 0
          ]
  assertEqual
    "equal cells consolidate and zero cells vanish"
    [Update 0 "key" 'a' 5]
    (batchToUpdates batch)

batchSingletonAgreesWithFromUpdates :: IO ()
batchSingletonAgreesWithFromUpdates = do
  let liveUpdate =
        Update 3 "key" 'z' (7 :: Int)
      zeroUpdate =
        Update 3 "key" 'z' (0 :: Int)
  assertEqual
    "singleton batch is the same denotation as one-update consolidation"
    (fromUpdates [liveUpdate] :: TestBatch)
    (singletonBatch liveUpdate)
  assertEqual
    "singleton batch tracks the one live cell directly"
    1
    (batchRowCount (singletonBatch liveUpdate :: TestBatch))
  assertBool
    "zero singleton is the empty batch"
    (batchNull (singletonBatch zeroUpdate :: TestBatch))

batchMergeAssociative :: IO ()
batchMergeAssociative = do
  let left =
        testBatch [Update 0 "a" 'x' 1, Update 1 "a" 'x' 2]
      middle =
        testBatch [Update 0 "a" 'x' 3, Update 2 "b" 'y' 4]
      right =
        testBatch [Update 1 "a" 'x' (-2), Update 3 "b" 'z' 5]
      merge =
        mergeBatch
  assertEqual
    "batch merge associates after consolidation"
    (merge (merge left middle) right)
    (merge left (merge middle right))

batchMergeEmptyIdentity :: IO ()
batchMergeEmptyIdentity = do
  let batch =
        testBatch [Update 0 "a" 'x' 1, Update 1 "b" 'y' 2]
      empty =
        emptyBatch :: TestBatch
  assertEqual
    "empty batch is the left identity"
    batch
    (mergeBatch empty batch)
  assertEqual
    "empty batch is the right identity"
    batch
    (mergeBatch batch empty)
  assertEqual
    "bulk merge ignores empty local sections"
    batch
    (mergeBatches [empty, batch, empty])

batchMergerMeasuredFuelReportsConsumedWork :: IO ()
batchMergerMeasuredFuelReportsConsumedWork = do
  let left =
        testBatch
          [ Update 0 "a" 'x' 1,
            Update 2 "b" 'y' 3
          ]
      right =
        testBatch
          [ Update 1 "a" 'z' 2,
            Update 3 "c" 'w' 4
          ]
      partialWork =
        workBatchMergeMeasured (BatchMergeFuel 1) (beginBatchMerge left right)
      completeWork =
        workBatchMergeMeasured (BatchMergeFuel 12) (beginBatchMerge left right)
      exhaustedWork =
        workBatchMergeMeasured (BatchMergeFuel 12) (batchMergeWorkMerger completeWork)
      expected =
        batchToUpdates (mergeBatch left right)
  assertEqual
    "measured merger reports one consumed step for one fuel unit"
    1
    (batchMergeFuelConsumed partialWork)
  assertBool
    "one consumed step leaves this four-row merge suspended"
    (not (batchMergeDone (batchMergeWorkMerger partialWork)))
  assertMeasuredBatchMergeLaw (BatchMergeFuel 12) completeWork expected
  assertBool
    "completed merge reports consumed fuel, not requested budget"
    (batchMergeFuelConsumed completeWork < 12)
  assertEqual
    "finished merger consumes no additional fuel"
    0
    (batchMergeFuelConsumed exhaustedWork)

assertMeasuredBatchMergeLaw ::
  BatchMergeFuel ->
  BatchMergeWork Int String Char Int ->
  [Update Int String Char Int] ->
  IO ()
assertMeasuredBatchMergeLaw (BatchMergeFuel requested) work expectedUpdates = do
  assertBool
    "consumed fuel is bounded by request"
    (batchMergeFuelConsumed work <= requested)
  assertBool
    "over-fueled merge finishes"
    (batchMergeDone (batchMergeWorkMerger work))
  assertEqual
    "measured work preserves finished denotation"
    expectedUpdates
    (batchToUpdates (finishBatchMerge (batchMergeWorkMerger work)))

batchMergerSuspendFinishDenotesMerge :: IO ()
batchMergerSuspendFinishDenotesMerge = do
  let left =
        testBatch
          [ Update 0 "a" 'x' 1,
            Update 1 "a" 'x' 2,
            Update 2 "b" 'y' 3
          ]
      right =
        testBatch
          [ Update 0 "a" 'x' (-1),
            Update 3 "c" 'z' 4,
            Update 4 "d" 'w' 5
          ]
      suspended =
        workBatchMerge (BatchMergeFuel 1) (beginBatchMerge left right)
  assertBool
    "one fuel unit leaves this six-row merge suspended"
    (not (batchMergeDone suspended))
  assertEqual
    "finishing a suspended merger preserves the binary merge denotation"
    (batchToUpdates (mergeBatch left right))
    (batchToUpdates (finishBatchMerge suspended))

batchMergerSuffixDrainConsolidatesBoundaryDuplicates :: IO ()
batchMergerSuffixDrainConsolidatesBoundaryDuplicates = do
  let left =
        testBatch [Update 1 "a" 'x' 2]
      right =
        testBatch
          [ Update 1 "a" 'x' (-2),
            Update 2 "b" 'y' 4
          ]
  assertEqual
    "suffix drain keeps the DD-style cursor fast path without leaking boundary duplicates"
    [Update 2 "b" 'y' 4]
    (batchToUpdates (finishBatchMerge (beginBatchMerge left right)))

batchMergeBatchesAgreesWithBinaryMerge :: IO ()
batchMergeBatchesAgreesWithBinaryMerge = do
  let batches =
        [ testBatch [Update 0 "a" 'x' 1, Update 1 "a" 'x' 2],
          testBatch [Update 1 "a" 'x' (-2), Update 2 "b" 'y' 4],
          testBatch [Update 3 "b" 'y' (-4), Update 4 "c" 'z' 8]
        ]
  assertEqual
    "bulk batch merge is the same algebra as repeated binary merge"
    (Foldable.foldl' mergeBatch emptyBatch batches)
    (mergeBatches batches)

batchMergeBatchesKWayConsolidatesSharedCells :: IO ()
batchMergeBatchesKWayConsolidatesSharedCells = do
  let batches =
        [ testBatch [Update 0 "a" 'x' 1, Update 1 "b" 'y' 4],
          testBatch [Update 0 "a" 'x' 2, Update 2 "c" 'z' 5],
          testBatch [Update 0 "a" 'x' (-3), Update 3 "d" 'w' 6]
        ]
  assertEqual
    "k-way cursor buckets sum the same current cell across the whole cover"
    [Update 1 "b" 'y' 4, Update 2 "c" 'z' 5, Update 3 "d" 'w' 6]
    (batchToUpdates (mergeBatches batches))

batchMergeBatchesSingletonCoverDenotesReplay :: IO ()
batchMergeBatchesSingletonCoverDenotesReplay = do
  let updates =
        fmap
          ( \index ->
              Update
                { updateTime = index,
                  updateKey = "key-" <> show (index `mod` 8),
                  updateVal = toEnum (fromEnum 'a' + (index `mod` 4)),
                  updateWeight = if even index then 1 else -1
                }
          )
          [0 .. 63]
      singletonBatches =
        fmap singletonBatch updates :: [TestBatch]
  assertEqual
    "bounded singleton batch cover glues to the same consolidated replay"
    (batchToUpdates (fromUpdates updates :: TestBatch))
    (batchToUpdates (mergeBatches singletonBatches))

batchFoldOrderDeterministic :: IO ()
batchFoldOrderDeterministic = do
  let batch =
        testBatch
          [ Update 2 "b" 'x' 1,
            Update 1 "a" 'z' 3,
            Update 0 "a" 'a' 4
          ]
  assertEqual
    "fold observes Map key/value/time order"
    [Update 0 "a" 'a' 4, Update 1 "a" 'z' 3, Update 2 "b" 'x' 1]
    (batchToUpdates batch)

batchFoldRoundTrip :: IO ()
batchFoldRoundTrip = do
  let batch =
        testBatch
          [ Update 2 "b" 'x' 1,
            Update 2 "b" 'x' 4,
            Update 2 "b" 'x' (-5),
            Update 1 "a" 'z' 3,
            Update 0 "a" 'a' 4
          ]
      roundTripped =
        testBatch (batchToUpdates batch)
  assertEqual
    "folded updates reconstruct the same consolidated rows"
    (batchToUpdates batch)
    (batchToUpdates roundTripped)
  assertEqual
    "batch lower frontier records the earliest observed update time"
    [0]
    (frontierPoints (batchLower batch))
  assertEqual
    "batch upper frontier records the latest observed update time even when that cell cancels"
    [2]
    (upperFrontierPoints (batchUpper batch))
  assertEqual
    "batch row count reflects only non-zero finite-support cells"
    2
    (batchRowCount batch)

batchFromUpdatesOwnsConsolidation :: IO ()
batchFromUpdatesOwnsConsolidation = do
  let batch =
        fromUpdates
          [ Update 0 "key" 'a' 2,
            Update 0 "key" 'a' 3,
            Update 1 "key" 'a' 7,
            Update 1 "key" 'a' (-7)
          ] ::
          TestBatch
  assertEqual
    "Batch owns consolidation through Update ingestion"
    [Update 0 "key" 'a' 5]
    (batchToUpdates batch)
  assertBool "nonzero update contribution remains visible" (not (batchNull batch))

traceOverBatchConsolidates :: IO ()
traceOverBatchConsolidates = do
  let left =
        testBatch [Update 0 "a" 'x' 1, Update 0 "a" 'x' 4]
      right =
        testBatch [Update 0 "a" 'x' (-5), Update 1 "b" 'y' 7]
      traceValue =
        traceFromBatch left <> traceFromBatch right
  assertEqual
    "trace batches collapse through the same physical batch algebra"
    [Update 1 "b" 'y' 7]
    (batchToUpdates (snapshotTraceBatch traceValue))

traceAppendAndCompactionLaws :: IO ()
traceAppendAndCompactionLaws = do
  let left =
        testBatch [Update 0 "a" 'x' 1]
      right =
        testBatch [Update 1 "a" 'x' 2, Update 1 "a" 'x' (-2), Update 2 "b" 'z' 4]
      traceValue =
        traceAppendBatch right (traceFromBatch left)
      compacted =
        coalesceTraceBatches (singletonUpperFrontier 1) traceValue
      physicallyCompacted =
        traceCompactPhysicalBefore (singletonUpperFrontier 1) traceValue
  assertEqual
    "appending an empty batch is observationally absent"
    traceValue
    (traceAppendBatch emptyBatch traceValue)
  assertEqual
    "traceFromBatches denotes the batch sum"
    (snapshotTraceBatch traceValue)
    (snapshotTraceBatch (traceFromBatches [left, right]))
  assertEqual
    "row-level trace compaction preserves consolidated denotation"
    (snapshotTraceBatch traceValue)
    (snapshotTraceBatch compacted)
  assertEqual
    "physical trace compaction preserves consolidated denotation"
    (snapshotTraceBatch traceValue)
    (snapshotTraceBatch physicallyCompacted)
  assertEqual
    "physical trace compaction does not advance the logical since frontier"
    (traceSince traceValue)
    (traceSince physicallyCompacted)
  assertEqual
    "trace compaction advances the since frontier"
    [1]
    (upperFrontierPoints (traceSince compacted))
  assertEqual
    "trace compaction keeps the uncompacted suffix in the spine"
    1
    (Seq.length (traceRecentBatches compacted))
  assertEqual
    "trace compaction records only the eligible prefix in the compacted spine"
    left
    (traceSpineCompacted (traceSpine compacted))
  assertEqual
    "trace compaction profile counts compacted physical layers"
    1
    (traceSpineCompactedLayerCount (traceSpine compacted))
  assertEqual
    "trace compaction profile counts recent batches"
    1
    (traceSpineRecentBatchCount (traceSpine compacted))
  assertEqual
    "trace compaction profile counts the physical batch cover"
    2
    (traceSpinePhysicalBatchCount (traceSpine compacted))
  assertEqual
    "trace compaction profile sums physical row payload sizes"
    2
    (traceSpinePhysicalRowCount (traceSpine compacted))
  assertEqual
    "trace compaction profile reports virtual physical effort"
    2
    (traceSpinePhysicalVirtualWeight (traceSpine compacted))

traceSingletonPrefixCompactionDenotesReplay :: IO ()
traceSingletonPrefixCompactionDenotesReplay = do
  let updates =
        fmap
          ( \index ->
              Update
                { updateTime = index,
                  updateKey = "key-" <> show (index `mod` 8),
                  updateVal = toEnum (fromEnum 'a' + (index `mod` 4)),
                  updateWeight = if even index then 1 else -1
                }
          )
          [0 .. 63]
      singletonBatches =
        fmap singletonBatch updates :: [TestBatch]
      compacted =
        coalesceTraceBatches (singletonUpperFrontier 63) (traceFromBatches singletonBatches)
  assertEqual
    "singleton trace prefix compacts to the same consolidated replay"
    (batchToUpdates (fromUpdates updates :: TestBatch))
    (batchToUpdates (snapshotTraceBatch compacted))

traceFueledPhysicalCompactionReportsStats :: IO ()
traceFueledPhysicalCompactionReportsStats = do
  let traceValue =
        traceFromBatches
          [ testBatch [Update 0 "a" 'x' 1],
            testBatch [Update 1 "a" 'x' 2]
          ]
      (stats, compacted) =
        compactTracePhysicalStep (TraceCompactionFuel 0) (singletonUpperFrontier 2) traceValue
      (fueledStats, fueledCompacted) =
        compactTracePhysicalStep (TraceCompactionFuel 1) (singletonUpperFrontier 2) traceValue
      (overStats, overCompacted) =
        compactTracePhysicalStep (TraceCompactionFuel 12) (singletonUpperFrontier 2) traceValue
      eligibleBatches =
        traceRecentBatches traceValue
      eligibleRows =
        Foldable.foldl' (\count batch -> count + batchRowCount batch) 0 eligibleBatches
  assertPhysicalStepLaw traceValue stats compacted
  assertPhysicalStepLaw traceValue fueledStats fueledCompacted
  assertPhysicalStepLaw traceValue overStats overCompacted
  assertEqual
    "physical compaction reports consumed batch sections from eligible cover"
    (Seq.length eligibleBatches)
    (tracePhysicalCompactionBatchesConsumed stats)
  assertEqual
    "physical compaction reports input rows visited from eligible cover"
    eligibleRows
    (tracePhysicalCompactionInputRowsVisited stats)
  assertEqual
    "zero fuel leaves the started merge active"
    1
    (tracePhysicalCompactionActiveMergeCount stats)
  assertEqual
    "zero fuel consumes no merge fuel"
    0
    (tracePhysicalCompactionMergeFuelConsumed stats)
  assertEqual
    "fueled physical compaction reports output layer count"
    (traceSpineCompactedLayerCount (traceSpine compacted))
    (tracePhysicalCompactionOutputLayers stats)
  assertEqual
    "one unit of fuel reports consumed merge fuel"
    1
    (tracePhysicalCompactionMergeFuelConsumed fueledStats)
  assertBool
    "over-fueled physical compaction reports consumed fuel, not requested budget"
    ( tracePhysicalCompactionMergeFuelConsumed overStats > 0
        && tracePhysicalCompactionMergeFuelConsumed overStats < 12
    )
  assertEqual
    "active zero-fuel merge exposes source sections"
    2
    (foldTraceBatches (\count _batch -> count + 1) (0 :: Int) compacted)
  assertEqual
    "active fueled merge exposes source sections"
    2
    (foldTraceBatches (\count _batch -> count + 1) (0 :: Int) fueledCompacted)
  assertEqual
    "active over-fueled merge exposes source sections"
    2
    (foldTraceBatches (\count _batch -> count + 1) (0 :: Int) overCompacted)

assertPhysicalStepLaw ::
  TestTrace ->
  TracePhysicalCompactionStepStats ->
  TestTrace ->
  IO ()
assertPhysicalStepLaw original stats stepped = do
  assertEqual
    "physical step preserves denotation"
    (batchToUpdates (snapshotTraceBatch original))
    (batchToUpdates (snapshotTraceBatch stepped))
  assertEqual
    "physical step does not advance since"
    (traceSince original)
    (traceSince stepped)
  assertEqual
    "physical step stats project output layers"
    (traceSpineCompactedLayerCount (traceSpine stepped))
    (tracePhysicalCompactionOutputLayers stats)

newtype TracePhysicalCompactionSchedule = TracePhysicalCompactionSchedule [(Int, UpperFrontier Int)]
  deriving stock (Eq, Show)

data ScheduledTraceState = ScheduledTraceState
  { scheduledTraceStep :: !Int,
    scheduledTraceValue :: !(Trace Int String Char Int)
  }

data ScheduledTraceOutcome = ScheduledTraceOutcome
  { scheduledTraceUpdates :: ![Update Int String Char Int],
    scheduledTraceSincePoints :: ![Int],
    scheduledTraceUpperPoints :: ![Int]
  }
  deriving stock (Eq, Show)

tracePhysicalCompactionSchedulesPreserveDenotation :: IO ()
tracePhysicalCompactionSchedulesPreserveDenotation = do
  let batches =
        [ testBatch [Update 0 "a" 'x' 1],
          testBatch [Update 1 "a" 'x' 2],
          testBatch [Update 2 "a" 'x' (-1)],
          testBatch [Update 3 "b" 'y' 4]
        ]
      schedules =
        [ TracePhysicalCompactionSchedule [],
          TracePhysicalCompactionSchedule [(1, singletonUpperFrontier 1), (3, singletonUpperFrontier 3)],
          TracePhysicalCompactionSchedule [(4, singletonUpperFrontier 4)]
        ]
      expected =
        scheduledTraceOutcome (traceFromBatches batches)
      outcomes =
        fmap (`applyTracePhysicalCompactionSchedule` batches) schedules
  assertEqual
    "physical compaction schedule is pure merge fuel: denotation and frontiers are invariant"
    (fmap (const expected) schedules)
    outcomes

tracePhysicalReadSnapshotsActiveMergerSources :: IO ()
tracePhysicalReadSnapshotsActiveMergerSources = do
  let positive =
        testBatch [Update 0 "a" 'x' 1]
      negative =
        testBatch [Update 0 "a" 'x' (-1)]
      compactedPositive =
        coalesceTraceBatches (singletonUpperFrontier 1) (traceFromBatch positive)
      activeMerge =
        traceCompactPhysicalBefore (singletonUpperFrontier 1) (traceAppendBatch negative compactedPositive)
  assertEqual
    "active physical merge exposes source sections to snapshot reads"
    (2 :: Int)
    (foldTraceBatches (\count _batch -> count + 1) (0 :: Int) activeMerge)
  assertEqual
    "source snapshot still denotes the finished cancellation"
    []
    (batchToUpdates (snapshotTraceBatch activeMerge))

applyTracePhysicalCompactionSchedule ::
  TracePhysicalCompactionSchedule ->
  [TestBatch] ->
  ScheduledTraceOutcome
applyTracePhysicalCompactionSchedule schedule batches =
  scheduledTraceOutcome
    ( scheduledTraceValue
        (Foldable.foldl' (appendScheduledTraceBatch schedule) emptyScheduledTraceState batches)
    )

emptyScheduledTraceState :: ScheduledTraceState
emptyScheduledTraceState =
  ScheduledTraceState
    { scheduledTraceStep = 0,
      scheduledTraceValue = traceFromBatch emptyBatch
    }

appendScheduledTraceBatch ::
  TracePhysicalCompactionSchedule ->
  ScheduledTraceState ->
  TestBatch ->
  ScheduledTraceState
appendScheduledTraceBatch schedule state batch =
  ScheduledTraceState
    { scheduledTraceStep = nextStep,
      scheduledTraceValue =
        Foldable.foldl'
          (flip traceCompactPhysicalBefore)
          appendedTrace
          (scheduledTraceCompactionFrontiers schedule nextStep)
    }
  where
    nextStep =
      scheduledTraceStep state + 1

    appendedTrace =
      traceAppendBatch batch (scheduledTraceValue state)

scheduledTraceCompactionFrontiers ::
  TracePhysicalCompactionSchedule ->
  Int ->
  [UpperFrontier Int]
scheduledTraceCompactionFrontiers (TracePhysicalCompactionSchedule compactions) step =
  foldMap
    ( \(compactionStep, frontier) ->
        if compactionStep == step
          then [frontier]
          else []
    )
    compactions

scheduledTraceOutcome :: Trace Int String Char Int -> ScheduledTraceOutcome
scheduledTraceOutcome traceValue =
  ScheduledTraceOutcome
    { scheduledTraceUpdates = batchToUpdates (snapshotTraceBatch traceValue),
      scheduledTraceSincePoints = upperFrontierPoints (traceSince traceValue),
      scheduledTraceUpperPoints = upperFrontierPoints (traceUpper traceValue)
    }

tracePhysicalLayerVirtualWeightKeepsEmptyBookkeeping :: IO ()
tracePhysicalLayerVirtualWeightKeepsEmptyBookkeeping = do
  let positive =
        testBatch [Update 0 "a" 'x' 1]
      negative =
        testBatch [Update 0 "a" 'x' (-1)]
      compactedPositive =
        coalesceTraceBatches (singletonUpperFrontier 1) (traceFromBatch positive)
      cancelled =
        traceCompactPhysicalBefore (singletonUpperFrontier 1) (traceAppendBatch negative compactedPositive)
      spine =
        traceSpine cancelled
  assertEqual
    "merged physical layers can be logically empty after cancellation"
    []
    (batchToUpdates (traceSpineCompacted spine))
  assertEqual
    "empty physical bookkeeping keeps the merged layer"
    1
    (traceSpineCompactedLayerCount spine)
  assertEqual
    "empty physical bookkeeping reports physical payload rows without forcing the merge"
    2
    (traceSpinePhysicalRowCount spine)
  assertEqual
    "empty physical bookkeeping preserves virtual merge effort"
    2
    (traceSpinePhysicalVirtualWeight spine)

traceNullRecognizesPhysicallyEmptyCompactedLayers :: IO ()
traceNullRecognizesPhysicallyEmptyCompactedLayers = do
  let positive =
        testBatch [Update 0 "a" 'x' 1]
      negative =
        testBatch [Update 0 "a" 'x' (-1)]
      compactedPositive =
        coalesceTraceBatches (singletonUpperFrontier 1) (traceFromBatch positive)
      cancelled =
        traceCompactPhysicalBefore (singletonUpperFrontier 1) (traceAppendBatch negative compactedPositive)
  assertBool
    "trace null is true for an empty denotation"
    (traceNull cancelled)
  assertEqual
    "test fixture keeps physical payload rows while denotation is empty"
    2
    (traceSpinePhysicalRowCount (traceSpine cancelled))

tracePhysicalCompactionNoOpWhenNothingEligible :: IO ()
tracePhysicalCompactionNoOpWhenNothingEligible = do
  let traceValue =
        traceFromBatch (testBatch [Update 5 "future" 'x' 1])
      compacted =
        traceCompactPhysicalBefore (singletonUpperFrontier 0) traceValue
      spine =
        traceSpine compacted
  assertEqual
    "no-op physical compaction preserves denotation"
    (batchToUpdates (snapshotTraceBatch traceValue))
    (batchToUpdates (snapshotTraceBatch compacted))
  assertEqual
    "no compacted layer is fabricated when nothing is eligible"
    0
    (traceSpineCompactedLayerCount spine)
  assertEqual
    "recent cover remains intact"
    1
    (traceSpineRecentBatchCount spine)

traceSinceAdvanceLaws :: IO ()
traceSinceAdvanceLaws = do
  let left =
        coalesceTraceBatches
          (singletonUpperFrontier 5)
          (traceFromBatch (testBatch [Update 0 "left" 'x' 1]))
      right =
        coalesceTraceBatches
          (singletonUpperFrontier 2)
          (traceFromBatch (testBatch [Update 0 "right" 'y' 1]))
      merged =
        left <> right
  assertEqual
    "merged traces inherit the strongest since frontier"
    [5]
    (upperFrontierPoints (traceSince merged))
  advanced <-
    assertRight
      "monotone since advance"
      (traceAdvanceSince (singletonUpperFrontier 7) merged)
  assertEqual
    "advancing since raises the frontier"
    [7]
    (upperFrontierPoints (traceSince advanced))
  case traceAdvanceSince (singletonUpperFrontier 1) merged of
    Left TraceFrontierRegression {} ->
      pure ()
    Right _ ->
      assertFailure "since regression should be a typed obstruction"

traceAccumUpToReadsPrefix :: IO ()
traceAccumUpToReadsPrefix = do
  let traceValue =
        traceFromUpdates
          ( [ Update 0 "key" 'a' 2,
              Update 1 "key" 'a' 3,
              Update 2 "key" 'a' 5,
              Update 1 "other" 'z' 7
            ] ::
              [TestTraceUpdate]
          )
      expected =
        ZSet.indexedZSetInsert "key" 'a' (5 :: Int) $
          ZSet.indexedZSetInsert "other" 'z' 7 ZSet.indexedZSetEmpty
  assertEqual
    "prefix accumulation erases time only after the cutoff filter"
    expected
    (traceAccumUpTo 1 traceValue)

assertRight :: Show errorValue => String -> Either errorValue value -> IO value
assertRight label eitherValue =
  case eitherValue of
    Left obstruction ->
      assertFailure (label <> " failed with " <> show obstruction)
    Right value ->
      pure value
