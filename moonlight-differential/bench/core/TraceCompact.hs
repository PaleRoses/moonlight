module TraceCompact where

import Control.DeepSeq (NFData (..))
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Common (eitherShow, weightAt)
import Moonlight.Differential.Frontier
import Moonlight.Differential.Time
import Moonlight.Differential.Trace.Compact

type BenchRuntimeTime = RuntimeTime Int Int Int

data CompactEntry = CompactEntry
  { compactEntryId :: !Int,
    compactEntryPartition :: !Int,
    compactEntryGroup :: !Int,
    compactEntryTime :: !BenchRuntimeTime,
    compactEntryValue :: !Int
  }
  deriving stock (Eq, Show)

newtype PreparedCompactEntries = PreparedCompactEntries (IntMap.IntMap CompactEntry)

instance NFData PreparedCompactEntries where
  rnf (PreparedCompactEntries entries) =
    IntMap.size entries `seq` ()

compactEntries :: Int -> PreparedCompactEntries
compactEntries size =
  PreparedCompactEntries
    ( IntMap.fromAscList
        ( fmap
            (\index -> (index, compactEntryAt index))
            [0 .. size - 1]
        )
    )

compactEntryAt :: Int -> CompactEntry
compactEntryAt index =
  CompactEntry
    { compactEntryId = index,
      compactEntryPartition = index `mod` 32,
      compactEntryGroup = index `div` 8,
      compactEntryTime = compactTime (index `mod` 32) (index `mod` 10),
      compactEntryValue = weightAt index
    }

compactPrefixWeight :: PreparedCompactEntries -> Either String Int
compactPrefixWeight (PreparedCompactEntries entries) =
  compactResultWeight (compactPartitionedPrefixesBefore compactOps compactFrontier entries)

compactPendingBlockedWeight :: PreparedCompactEntries -> Either String Int
compactPendingBlockedWeight (PreparedCompactEntries entries) =
  compactResultWeight (compactPartitionedPrefixesBefore compactPendingOps compactPendingFrontier entries)

compactRetainedIdsWeight :: PreparedCompactEntries -> Either String Int
compactRetainedIdsWeight (PreparedCompactEntries entries) =
  compactResultWeight (compactPartitionedPrefixesBefore compactOps compactRetainedFrontier entries)

compactSummaryOutsideRunWeight :: PreparedCompactEntries -> Either String Int
compactSummaryOutsideRunWeight (PreparedCompactEntries entries) =
  case compactPartitionedPrefixesBefore compactSummaryOutsideRunOps compactFrontier entries of
    Left (PartitionedPrefixCompactionSummaryKeyOutsideRun summaryKey runKeys) ->
      Right (summaryKey + IntSet.size runKeys)
    Left obstruction ->
      Left ("unexpected compaction obstruction: " <> show obstruction)
    Right _ ->
      Left "expected PartitionedPrefixCompactionSummaryKeyOutsideRun"

compactResultWeight ::
  Either
    (PartitionedPrefixCompactionError Int Int Int ())
    (PartitionedPrefixCompactionResult CompactEntry) ->
  Either String Int
compactResultWeight compactionResult =
  fmap
    ( \compactedResult ->
        IntMap.size (ppcrCompacted compactedResult)
          + IntMap.size (ppcrSummaries compactedResult)
          + IntMap.size (ppcrKept compactedResult)
    )
    (eitherShow compactionResult)

compactOps :: PartitionedPrefixCompactionOps Int Int Int CompactEntry Int Int ()
compactOps =
  PartitionedPrefixCompactionOps
    { pcoBatchKey = compactEntryId,
      pcoBatchTime = compactEntryTime,
      pcoPartition = compactEntryPartition,
      pcoPartitionBlockedByPending = \_partition _pending -> False,
      pcoGroup = compactEntryGroup,
      pcoSummarizeRun = summarizeCompactRun
    }

compactPendingOps :: PartitionedPrefixCompactionOps Int Int Int CompactEntry Int Int ()
compactPendingOps =
  compactOps
    { pcoPartitionBlockedByPending =
        \partitionValue pendingTime -> pendingTime == compactTime partitionValue 5
    }

compactSummaryOutsideRunOps :: PartitionedPrefixCompactionOps Int Int Int CompactEntry Int Int ()
compactSummaryOutsideRunOps =
  compactOps
    { pcoSummarizeRun = summarizeOutsideRun
    }

summarizeCompactRun ::
  RuntimeFrontier Int Int Int ->
  Int ->
  Int ->
  NonEmpty CompactEntry ->
  Either () (Maybe CompactEntry)
summarizeCompactRun _frontier partition group entries =
  Right
    ( Just
        CompactEntry
          { compactEntryId = compactEntryId (NonEmpty.head entries),
            compactEntryPartition = partition,
            compactEntryGroup = group,
            compactEntryTime = compactEntryTime (NonEmpty.last entries),
            compactEntryValue = Foldable.foldl' (\acc entry -> acc + compactEntryValue entry) 0 entries
          }
    )

summarizeOutsideRun ::
  RuntimeFrontier Int Int Int ->
  Int ->
  Int ->
  NonEmpty CompactEntry ->
  Either () (Maybe CompactEntry)
summarizeOutsideRun _frontier partition group entries =
  Right
    ( Just
        CompactEntry
          { compactEntryId = compactEntryId (NonEmpty.last entries) + 1000000,
            compactEntryPartition = partition,
            compactEntryGroup = group,
            compactEntryTime = compactEntryTime (NonEmpty.last entries),
            compactEntryValue = Foldable.foldl' (\acc entry -> acc + compactEntryValue entry) 0 entries
          }
    )

compactFrontier :: RuntimeFrontier Int Int Int
compactFrontier =
  Foldable.foldl'
    (\frontier partition -> frontierAdvanceVisibleMin (compactTime partition 10) frontier)
    emptyRuntimeFrontier
    [0 .. 31]

compactPendingFrontier :: RuntimeFrontier Int Int Int
compactPendingFrontier =
  Foldable.foldl'
    (\frontier partition -> frontierInsertPending (compactTime partition 5) frontier)
    compactFrontier
    [0 .. 15]

compactRetainedFrontier :: RuntimeFrontier Int Int Int
compactRetainedFrontier =
  frontierWithTraceRetention
    (Just (traceRetention (IntSet.fromDistinctAscList [0, 7 .. 511]) IntSet.empty IntSet.empty))
    compactFrontier

compactTime :: Int -> Int -> BenchRuntimeTime
compactTime partition stamp =
  runtimeTime partition emptyRuntimeScope 0 0 (frontierStamp (fromIntegral stamp))


compactionSizes :: [Int]
compactionSizes =
  [512, 2048]
