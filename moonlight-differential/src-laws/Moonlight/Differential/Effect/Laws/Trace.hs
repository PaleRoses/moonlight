{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Differential.Effect.Laws.Trace
  ( lawBundles,
  )
where

import Data.Word
  ( Word64,
  )
import Moonlight.Differential.Effect.Harness.Trace qualified as Harness
import Moonlight.Differential.Effect.LawNames
  ( LawName (..),
  )
import Moonlight.Differential.Trace.ReadIndex
  ( TimeFrontier (..),
  )
import Moonlight.Differential.Update
  ( Update (..),
  )
import Moonlight.Pale.Test.LawSuite
  ( LawBundle,
    lawBundleQuickCheck,
    quickCheckLawDefinition,
  )
import Test.Tasty.QuickCheck qualified as QC

newtype TestUpdates = TestUpdates
  { unTestUpdates :: [Update Int String Char Int]
  }
  deriving stock (Eq, Show)

newtype TestTraceKey = TestTraceKey
  { unTestTraceKey :: String
  }
  deriving stock (Eq, Show)

data TestTimeIndexEntries = TestTimeIndexEntries
  { ttieReadKey :: !String,
    ttieReadFrontier :: !(TimeFrontier Int Int),
    ttieSinceFrontier :: !(TimeFrontier Int Int),
    ttieUpperFrontier :: !(TimeFrontier Int Int),
    ttieEntries :: ![(String, TimeFrontier Int Int, Int)]
  }
  deriving stock (Eq, Show)

data TestCompactionCell = TestCompactionCell
  { tccPartition :: !Int,
    tccGroup :: !Int,
    tccStamp :: !Word64,
    tccValue :: !Int
  }
  deriving stock (Eq, Show)

newtype TestCompactionEntries = TestCompactionEntries
  { unTestCompactionEntries :: [Harness.LawEntry]
  }
  deriving stock (Eq, Show)

instance QC.Arbitrary TestUpdates where
  arbitrary =
    TestUpdates
      <$> QC.listOf
        ( Update
            <$> QC.chooseInt (0, 12)
            <*> QC.elements ["left", "middle", "right"]
            <*> QC.elements ['a' .. 'f']
            <*> QC.chooseInt (-16, 16)
        )

instance QC.Arbitrary TestTraceKey where
  arbitrary =
    TestTraceKey <$> QC.elements ("missing" : Harness.testTraceKeys)

instance QC.Arbitrary TestTimeIndexEntries where
  arbitrary =
    TestTimeIndexEntries
      <$> QC.elements Harness.testTraceKeys
      <*> arbitraryTimeFrontier
      <*> arbitraryTimeFrontier
      <*> arbitraryTimeFrontier
      <*> QC.listOf
        ((,,) <$> QC.elements Harness.testTraceKeys <*> arbitraryTimeFrontier <*> QC.chooseInt (0, 24))

instance QC.Arbitrary TestCompactionCell where
  arbitrary =
    TestCompactionCell
      <$> QC.chooseInt (0, 4)
      <*> QC.chooseInt (0, 3)
      <*> (fromIntegral <$> QC.chooseInt (0, 5))
      <*> QC.chooseInt (-8, 8)

instance QC.Arbitrary TestCompactionEntries where
  arbitrary = do
    entryCount <- QC.chooseInt (0, 10)
    cells <- QC.vectorOf entryCount QC.arbitrary
    pure (TestCompactionEntries (zipWith compactionCellEntry [0 ..] cells))

arbitraryTimeFrontier :: QC.Gen (TimeFrontier Int Int)
arbitraryTimeFrontier =
  TimeFrontier
    <$> QC.chooseInt (0, 4)
    <*> QC.chooseInt (0, 12)

compactionCellEntry :: Int -> TestCompactionCell -> Harness.LawEntry
compactionCellEntry traceKey cell =
  Harness.compactionCellEntry
    traceKey
    (tccPartition cell)
    (tccGroup cell)
    (tccStamp cell)
    (tccValue cell)

batchRowCountTracksLiveCellsProp :: TestUpdates -> QC.Property
batchRowCountTracksLiveCellsProp (TestUpdates updates) =
  Harness.batchRowCountTracksLiveCells updates

traceAppendDenotesReplayProp :: TestUpdates -> TestUpdates -> QC.Property
traceAppendDenotesReplayProp (TestUpdates initialUpdates) (TestUpdates appendedUpdates) =
  Harness.traceAppendDenotesReplay initialUpdates appendedUpdates

traceSemigroupAppendDenotesReplayProp :: TestUpdates -> TestUpdates -> QC.Property
traceSemigroupAppendDenotesReplayProp (TestUpdates leftUpdates) (TestUpdates rightUpdates) =
  Harness.traceSemigroupAppendDenotesReplay leftUpdates rightUpdates

traceNullMatchesCollapsedDenotationProp :: TestUpdates -> TestUpdates -> QC.Property
traceNullMatchesCollapsedDenotationProp (TestUpdates leftUpdates) (TestUpdates rightUpdates) =
  Harness.traceNullMatchesCollapsedDenotation leftUpdates rightUpdates

traceAccumulationDenotesPrefixReplayProp :: TestUpdates -> QC.NonNegative Int -> QC.Property
traceAccumulationDenotesPrefixReplayProp (TestUpdates updates) (QC.NonNegative cutoff) =
  Harness.traceAccumulationDenotesPrefixReplay updates cutoff

tracePrefixFoldRebuildsPrefixProp :: TestUpdates -> QC.NonNegative Int -> QC.Property
tracePrefixFoldRebuildsPrefixProp (TestUpdates updates) (QC.NonNegative cutoff) =
  Harness.tracePrefixFoldRebuildsPrefix updates cutoff

traceKeyFoldsDenoteFilteredReplayProp :: TestUpdates -> TestTraceKey -> QC.NonNegative Int -> QC.Property
traceKeyFoldsDenoteFilteredReplayProp (TestUpdates updates) (TestTraceKey key) (QC.NonNegative cutoff) =
  Harness.traceKeyFoldsDenoteFilteredReplay updates key cutoff

traceKeyRowFoldsRebuildConsolidatedBatchesProp :: TestUpdates -> QC.Property
traceKeyRowFoldsRebuildConsolidatedBatchesProp (TestUpdates updates) =
  Harness.traceKeyRowFoldsRebuildConsolidatedBatches updates

batchDescriptionProjectsIntervalProp :: TestUpdates -> QC.Property
batchDescriptionProjectsIntervalProp (TestUpdates updates) =
  Harness.batchDescriptionProjectsInterval updates

traceDescriptionProjectsFrontiersProp :: TestUpdates -> QC.Property
traceDescriptionProjectsFrontiersProp (TestUpdates updates) =
  Harness.traceDescriptionProjectsFrontiers updates

traceDescriptionAdvanceMatchesTraceProp :: TestUpdates -> QC.NonNegative Int -> QC.Property
traceDescriptionAdvanceMatchesTraceProp (TestUpdates updates) (QC.NonNegative requestedTime) =
  Harness.traceDescriptionAdvanceMatchesTrace updates requestedTime

traceDescriptionReadAvailabilityMatchesFrontierOracleProp :: TestUpdates -> QC.NonNegative Int -> QC.Property
traceDescriptionReadAvailabilityMatchesFrontierOracleProp (TestUpdates updates) (QC.NonNegative readTime) =
  Harness.traceDescriptionReadAvailabilityMatchesFrontierOracle updates readTime

timeIndexSlicingConsumesReadObligationsProp :: TestTimeIndexEntries -> QC.Property
timeIndexSlicingConsumesReadObligationsProp testCaseValue =
  Harness.timeIndexSlicingConsumesReadObligations
    (ttieReadKey testCaseValue)
    (ttieReadFrontier testCaseValue)
    (ttieSinceFrontier testCaseValue)
    (ttieUpperFrontier testCaseValue)
    (ttieEntries testCaseValue)

batchMergerFuelDenotesBinaryMergeProp :: TestUpdates -> TestUpdates -> QC.Property
batchMergerFuelDenotesBinaryMergeProp (TestUpdates leftUpdates) (TestUpdates rightUpdates) =
  Harness.batchMergerFuelDenotesBinaryMerge leftUpdates rightUpdates

partitionedPrefixCompactionPreservesDenotationProp :: TestCompactionEntries -> QC.Property
partitionedPrefixCompactionPreservesDenotationProp (TestCompactionEntries entries) =
  Harness.partitionedPrefixCompactionPreservesDenotation entries

partitionedPrefixCompactionObeysDescriptionSinceProp :: TestCompactionEntries -> QC.NonNegative Int -> QC.Property
partitionedPrefixCompactionObeysDescriptionSinceProp (TestCompactionEntries entries) (QC.NonNegative rawCutoff) =
  Harness.partitionedPrefixCompactionObeysDescriptionSince entries rawCutoff

partitionedPrefixKeyMismatchTypedProp :: QC.NonNegative Int -> QC.Positive Int -> QC.NonNegative Int -> QC.NonNegative Int -> QC.Property
partitionedPrefixKeyMismatchTypedProp (QC.NonNegative rawKey) (QC.Positive rawOffset) (QC.NonNegative rawPartition) (QC.NonNegative rawGroup) =
  Harness.partitionedPrefixKeyMismatchTyped rawKey rawOffset rawPartition rawGroup

partitionedPrefixSummaryFailureTypedProp :: QC.NonNegative Int -> QC.NonNegative Int -> QC.NonNegative Int -> QC.Property
partitionedPrefixSummaryFailureTypedProp (QC.NonNegative rawKey) (QC.NonNegative rawPartition) (QC.NonNegative rawGroup) =
  Harness.partitionedPrefixSummaryFailureTyped rawKey rawPartition rawGroup

partitionedPrefixOutsideRunSummaryTypedProp :: QC.NonNegative Int -> QC.NonNegative Int -> QC.NonNegative Int -> QC.Property
partitionedPrefixOutsideRunSummaryTypedProp (QC.NonNegative rawKey) (QC.NonNegative rawPartition) (QC.NonNegative rawGroup) =
  Harness.partitionedPrefixOutsideRunSummaryTyped rawKey rawPartition rawGroup

lawBundles :: [LawBundle String]
lawBundles =
  [ lawBundleQuickCheck
      "trace"
      [ quickCheckLawDefinition BatchRowCountTracksLiveCells batchRowCountTracksLiveCellsProp,
        quickCheckLawDefinition BatchDescriptionProjectsInterval batchDescriptionProjectsIntervalProp,
        quickCheckLawDefinition BatchMergerFuelDenotesBinaryMerge batchMergerFuelDenotesBinaryMergeProp,
        quickCheckLawDefinition TraceAppendDenotesReplay traceAppendDenotesReplayProp,
        quickCheckLawDefinition TraceSemigroupAppendDenotesReplay traceSemigroupAppendDenotesReplayProp,
        quickCheckLawDefinition TraceAccumulationDenotesPrefixReplay traceAccumulationDenotesPrefixReplayProp,
        quickCheckLawDefinition TracePrefixFoldRebuildsPrefix tracePrefixFoldRebuildsPrefixProp,
        quickCheckLawDefinition TraceKeyFoldsDenoteFilteredReplay traceKeyFoldsDenoteFilteredReplayProp,
        quickCheckLawDefinition TraceKeyRowFoldsRebuildConsolidatedBatches traceKeyRowFoldsRebuildConsolidatedBatchesProp,
        quickCheckLawDefinition TraceNullMatchesCollapsedDenotation traceNullMatchesCollapsedDenotationProp,
        quickCheckLawDefinition TraceDescriptionProjectsFrontiers traceDescriptionProjectsFrontiersProp,
        quickCheckLawDefinition TraceDescriptionAdvanceMatchesTrace traceDescriptionAdvanceMatchesTraceProp,
        quickCheckLawDefinition TraceDescriptionReadAvailabilityMatchesFrontierOracle traceDescriptionReadAvailabilityMatchesFrontierOracleProp,
        quickCheckLawDefinition TimeIndexSlicingConsumesReadObligations timeIndexSlicingConsumesReadObligationsProp,
        quickCheckLawDefinition PartitionedPrefixCompactionPreservesDenotation partitionedPrefixCompactionPreservesDenotationProp,
        quickCheckLawDefinition PartitionedPrefixCompactionObeysDescriptionSince partitionedPrefixCompactionObeysDescriptionSinceProp,
        quickCheckLawDefinition PartitionedPrefixKeyMismatchTyped partitionedPrefixKeyMismatchTypedProp,
        quickCheckLawDefinition PartitionedPrefixSummaryFailureTyped partitionedPrefixSummaryFailureTypedProp,
        quickCheckLawDefinition PartitionedPrefixOutsideRunSummaryTyped partitionedPrefixOutsideRunSummaryTypedProp
      ]
  ]
