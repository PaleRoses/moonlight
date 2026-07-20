{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Differential.Effect.Harness.Trace
  ( LawEntry,
    TestTraceUpdate,
    batchRowCountTracksLiveCells,
    batchDescriptionProjectsInterval,
    batchMergerFuelDenotesBinaryMerge,
    traceAppendDenotesReplay,
    traceSemigroupAppendDenotesReplay,
    traceAccumulationDenotesPrefixReplay,
    tracePrefixFoldRebuildsPrefix,
    traceKeyFoldsDenoteFilteredReplay,
    traceKeyRowFoldsRebuildConsolidatedBatches,
    traceNullMatchesCollapsedDenotation,
    traceDescriptionProjectsFrontiers,
    traceDescriptionAdvanceMatchesTrace,
    traceDescriptionReadAvailabilityMatchesFrontierOracle,
    timeIndexSlicingConsumesReadObligations,
    partitionedPrefixCompactionPreservesDenotation,
    partitionedPrefixCompactionObeysDescriptionSince,
    partitionedPrefixKeyMismatchTyped,
    partitionedPrefixSummaryFailureTyped,
    partitionedPrefixOutsideRunSummaryTyped,
    compactionCellEntry,
    testTraceKeys,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty
  ( NonEmpty,
  )
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word
  ( Word64,
  )
import Moonlight.Delta.Frontier
  ( UpperFrontier,
    emptyFrontier,
    frontierPoints,
    mkFrontier,
    mkUpperFrontier,
    singletonFrontier,
    singletonUpperFrontier,
    upperFrontierPoints,
  )
import Moonlight.Differential.Algebra.ZSet qualified as ZSet
import Moonlight.Differential.Batch
  ( Batch,
    BatchMergeFuel (..),
    batchDescription,
    batchLower,
    batchNull,
    batchRowCount,
    batchToUpdates,
    batchUpper,
    beginBatchMerge,
    finishBatchMerge,
    fromUpdates,
    mergeBatch,
    workBatchMerge,
  )
import Moonlight.Differential.Frontier
  ( RuntimeFrontier,
    emptyRuntimeFrontier,
    emptyTraceRetention,
    frontierAdvanceVisibleMin,
    frontierWithTraceRetention,
  )
import Moonlight.Differential.Time
  ( RuntimeTime,
    emptyRuntimeScope,
    frontierStamp,
    rtContext,
    runtimeTime,
  )
import Moonlight.Differential.Trace
  ( Trace,
    TraceFrontierAdvanceError (..),
    foldTraceAccumUpTo,
    foldTraceKey,
    foldTraceKeyAfter,
    foldTraceKeyRows,
    foldTraceKeyThrough,
    snapshotTraceBatch,
    traceAccumUpTo,
    traceAdvanceSince,
    traceAdvanceUpper,
    traceAppendBatch,
    traceDescription,
    traceFromUpdates,
    traceNull,
    traceSince,
    traceUpper,
  )
import Moonlight.Differential.Trace.Compact
  ( PartitionedPrefixCompactionError (..),
    PartitionedPrefixCompactionOps (..),
    applyIndexedTraceCompactionPlan,
    compactPartitionedPrefixesBefore,
    planIndexedTraceCompactionBefore,
    planIndexedTraceCompactionBeforeDescription,
    ppcrCompacted,
  )
import Moonlight.Differential.Trace.Description qualified as TraceDescription
import Moonlight.Differential.Trace.Id
  ( TraceId,
    initialTraceId,
    traceIdFromKey,
    traceIdKey,
  )
import Moonlight.Differential.Trace.Indexed
  ( IndexedTrace,
    IndexedTraceError,
    TraceIndexOps (..),
    emptyIndexedTraceWithOps,
    insertIndexedTraceEntry,
    itEntries,
  )
import Moonlight.Differential.Trace.ReadIndex
  ( TimeFrontier (..),
    TimeIndex,
    emptyTimeIndex,
    insertTimeIndex,
    sliceTimeIndexAfter,
    sliceTimeIndexAfterDescription,
  )
import Moonlight.Differential.Update
  ( Update (..),
  )
import Test.Tasty.QuickCheck qualified as QC

type TestBatch = Batch Int String Char Int

type TestTraceUpdate = Update Int String Char Int

type LawTime = RuntimeTime Int Int Int

data LawEntry = LawEntry
  { leId :: !TraceId,
    lePartition :: !Int,
    leGroup :: !Int,
    leTime :: !LawTime,
    leValue :: !Int
  }
  deriving stock (Eq, Ord, Show)

newtype LawIndexes = LawIndexes
  { lawIndexesByPartition :: Map Int IntSet
  }
  deriving stock (Eq, Show)

data LawIndexError =
  LawIndexesMismatch !LawIndexes !LawIndexes
  deriving stock (Eq, Show)

type LawTrace = IndexedTrace LawEntry LawIndexes

testTraceKeys :: [String]
testTraceKeys =
  ["left", "middle", "right"]

batchRowCountTracksLiveCells :: [TestTraceUpdate] -> QC.Property
batchRowCountTracksLiveCells updates =
  let batch =
        fromUpdates updates
   in batchRowCount batch QC.=== length (batchToUpdates batch)

traceAppendDenotesReplay :: [TestTraceUpdate] -> [TestTraceUpdate] -> QC.Property
traceAppendDenotesReplay initialUpdates appendedUpdates =
  batchToUpdates actual QC.=== batchToUpdates expected
  where
    actual =
      snapshotTraceBatch
        ( traceAppendBatch
            (fromUpdates appendedUpdates :: TestBatch)
            (traceFromUpdates initialUpdates)
        )

    expected =
      fromUpdates (initialUpdates <> appendedUpdates) :: TestBatch

traceSemigroupAppendDenotesReplay :: [TestTraceUpdate] -> [TestTraceUpdate] -> QC.Property
traceSemigroupAppendDenotesReplay leftUpdates rightUpdates =
  batchToUpdates actual QC.=== batchToUpdates expected
  where
    actual =
      snapshotTraceBatch (traceFromUpdates leftUpdates <> traceFromUpdates rightUpdates)

    expected =
      fromUpdates (leftUpdates <> rightUpdates) :: TestBatch

traceNullMatchesCollapsedDenotation :: [TestTraceUpdate] -> [TestTraceUpdate] -> QC.Property
traceNullMatchesCollapsedDenotation leftUpdates rightUpdates =
  traceNull traceValue QC.=== batchNull (snapshotTraceBatch traceValue)
  where
    traceValue =
      traceFromUpdates leftUpdates <> traceFromUpdates rightUpdates

traceAccumulationDenotesPrefixReplay :: [TestTraceUpdate] -> Int -> QC.Property
traceAccumulationDenotesPrefixReplay updates cutoff =
  traceAccumUpTo cutoff (traceFromUpdates updates)
    QC.=== traceAccumPrefixOracle cutoff updates

tracePrefixFoldRebuildsPrefix :: [TestTraceUpdate] -> Int -> QC.Property
tracePrefixFoldRebuildsPrefix updates cutoff =
  foldTraceAccumUpTo
    (\accumulation key val weight -> ZSet.indexedZSetInsert key val weight accumulation)
    ZSet.indexedZSetEmpty
    cutoff
    (traceFromUpdates updates)
    QC.=== traceAccumUpTo cutoff (traceFromUpdates updates)

traceKeyFoldsDenoteFilteredReplay :: [TestTraceUpdate] -> String -> Int -> QC.Property
traceKeyFoldsDenoteFilteredReplay updates key cutoff =
  QC.conjoin
    [ QC.counterexample "full key fold" $
        traceKeyFoldMap key updates
          QC.=== traceKeyReplayMap (const True) key updates,
      QC.counterexample "key fold through cutoff" $
        traceKeyFoldThroughMap cutoff key updates
          QC.=== traceKeyReplayMap (<= cutoff) key updates,
      QC.counterexample "key fold after cutoff" $
        traceKeyFoldAfterMap cutoff key updates
          QC.=== traceKeyReplayMap (> cutoff) key updates
    ]

traceKeyRowFoldsRebuildConsolidatedBatches :: [TestTraceUpdate] -> QC.Property
traceKeyRowFoldsRebuildConsolidatedBatches updates =
  batchToUpdates replayed QC.=== batchToUpdates collapsed
  where
    traceValue =
      traceFromUpdates updates

    replayed =
      fromUpdates (foldTraceKeyRows collectTraceKeyRows [] traceValue) :: TestBatch

    collapsed =
      snapshotTraceBatch traceValue

batchDescriptionProjectsInterval :: [TestTraceUpdate] -> QC.Property
batchDescriptionProjectsInterval updates =
  QC.conjoin
    [ TraceDescription.traceDescriptionLower description QC.=== batchLower batch,
      TraceDescription.traceDescriptionUpper description QC.=== batchUpper batch,
      TraceDescription.traceDescriptionSince description QC.=== mkUpperFrontier (frontierPoints (batchLower batch))
    ]
  where
    batch =
      fromUpdates updates :: TestBatch

    description =
      batchDescription batch

traceDescriptionProjectsFrontiers :: [TestTraceUpdate] -> QC.Property
traceDescriptionProjectsFrontiers updates =
  QC.conjoin
    [ TraceDescription.traceDescriptionLower description QC.=== mkFrontier (upperFrontierPoints (traceSince traceValue)),
      TraceDescription.traceDescriptionUpper description QC.=== traceUpper traceValue,
      TraceDescription.traceDescriptionSince description QC.=== traceSince traceValue
    ]
  where
    traceValue =
      traceFromUpdates updates

    description =
      traceDescription traceValue

traceDescriptionAdvanceMatchesTrace :: [TestTraceUpdate] -> Int -> QC.Property
traceDescriptionAdvanceMatchesTrace updates requestedTime =
  QC.conjoin
    [ QC.counterexample "since advance agrees with Trace" $
        advanceSinceAgrees requestedFrontier traceValue description,
      QC.counterexample "upper advance agrees with Trace" $
        advanceUpperAgrees requestedFrontier traceValue description
    ]
  where
    requestedFrontier =
      singletonUpperFrontier requestedTime

    traceValue =
      traceFromUpdates updates

    description =
      traceDescription traceValue

traceDescriptionReadAvailabilityMatchesFrontierOracle :: [TestTraceUpdate] -> Int -> QC.Property
traceDescriptionReadAvailabilityMatchesFrontierOracle updates readTime =
  TraceDescription.traceDescriptionReadAt readTime description
    QC.=== traceDescriptionReadOracle readTime description
  where
    description =
      traceDescription (traceFromUpdates updates)

timeIndexSlicingConsumesReadObligations ::
  String ->
  TimeFrontier Int Int ->
  TimeFrontier Int Int ->
  TimeFrontier Int Int ->
  [(String, TimeFrontier Int Int, Int)] ->
  QC.Property
timeIndexSlicingConsumesReadObligations readKey readFrontier sinceFrontier upperFrontier entries =
  sliceTimeIndexAfterDescription readKey readFrontier description index
    QC.=== expected
  where
    description =
      TraceDescription.traceDescription
        (singletonFrontier sinceFrontier)
        (singletonUpperFrontier upperFrontier)
        (singletonUpperFrontier sinceFrontier)

    index =
      timeIndexFromEntries entries

    expected =
      case TraceDescription.traceDescriptionReadAfter readFrontier description of
        Left obstruction ->
          Left obstruction
        Right () ->
          Right (sliceTimeIndexAfter readKey readFrontier index)

batchMergerFuelDenotesBinaryMerge :: [TestTraceUpdate] -> [TestTraceUpdate] -> QC.Property
batchMergerFuelDenotesBinaryMerge leftUpdates rightUpdates =
  let left =
        fromUpdates leftUpdates :: TestBatch
      right =
        fromUpdates rightUpdates :: TestBatch
      merger =
        workBatchMerge (BatchMergeFuel 2) (beginBatchMerge left right)
   in batchToUpdates (finishBatchMerge merger) QC.=== batchToUpdates (mergeBatch left right)

partitionedPrefixCompactionPreservesDenotation :: [LawEntry] -> QC.Property
partitionedPrefixCompactionPreservesDenotation entries =
  case lawTraceFromEntries entries of
    Left obstruction ->
      QC.counterexample ("fixture construction failed: " <> show obstruction) False
    Right trace0 ->
      case planIndexedTraceCompactionBefore lawPrefixCompactionOps (frontierForEntries entries) trace0 of
        Left obstruction ->
          QC.counterexample ("unexpected compaction planning obstruction: " <> show obstruction) False
        Right prefixPlan ->
          case applyIndexedTraceCompactionPlan lawTraceIndexOps prefixPlan trace0 of
            Left obstruction ->
              QC.counterexample ("unexpected compaction application obstruction: " <> show obstruction) False
            Right compactedTrace ->
              lawTraceDenotation compactedTrace QC.=== lawTraceDenotation trace0

partitionedPrefixCompactionObeysDescriptionSince :: [LawEntry] -> Int -> QC.Property
partitionedPrefixCompactionObeysDescriptionSince entries rawCutoff =
  case lawTraceFromEntries entries of
    Left obstruction ->
      QC.counterexample ("fixture construction failed: " <> show obstruction) False
    Right trace0 ->
      case planIndexedTraceCompactionBeforeDescription lawPrefixCompactionOps (frontierForEntries entries) description trace0 of
        Left obstruction ->
          QC.counterexample ("unexpected compaction planning obstruction: " <> show obstruction) False
        Right prefixPlan ->
          QC.conjoin
            [ QC.counterexample "compacted entries are description-compactable" $
                all (entryAllowedByDescription description) (IntMap.elems (ppcrCompacted prefixPlan))
                  QC.=== True,
              QC.counterexample "description-gated compaction preserves denotation" $
                case applyIndexedTraceCompactionPlan lawTraceIndexOps prefixPlan trace0 of
                  Left obstruction ->
                    QC.counterexample ("unexpected compaction application obstruction: " <> show obstruction) False
                  Right compactedTrace ->
                    lawTraceDenotation compactedTrace QC.=== lawTraceDenotation trace0
            ]
  where
    cutoff =
      fromIntegral (rawCutoff `mod` 8)

    description =
      traceDescriptionForEntriesAt cutoff entries

partitionedPrefixKeyMismatchTyped :: Int -> Int -> Int -> Int -> QC.Property
partitionedPrefixKeyMismatchTyped rawKey rawOffset rawPartition rawGroup =
  compactPartitionedPrefixesBefore lawPrefixCompactionOps (frontierForContexts [partitionValue]) entries
    QC.=== Left (PartitionedPrefixCompactionBatchKeyMismatch storedKey actualKey)
  where
    actualKey =
      rawKey `mod` 24

    storedKey =
      actualKey + 1 + (rawOffset `mod` 8)

    partitionValue =
      rawPartition `mod` 5

    groupValue =
      rawGroup `mod` 4

    entries =
      IntMap.singleton storedKey (lawEntry actualKey partitionValue groupValue 0 1)

partitionedPrefixSummaryFailureTyped :: Int -> Int -> Int -> QC.Property
partitionedPrefixSummaryFailureTyped rawKey rawPartition rawGroup =
  compactPartitionedPrefixesBefore failingOps (frontierForContexts [partitionValue]) entries
    QC.=== Left (PartitionedPrefixCompactionSummaryFailed "generated summary failure")
  where
    actualKey =
      rawKey `mod` 24

    partitionValue =
      rawPartition `mod` 5

    groupValue =
      rawGroup `mod` 4

    entries =
      IntMap.singleton actualKey (lawEntry actualKey partitionValue groupValue 0 1)

    failingOps =
      lawPrefixCompactionOps
        { pcoSummarizeRun = \_frontier _partition _group _entries -> Left "generated summary failure"
        }

partitionedPrefixOutsideRunSummaryTyped :: Int -> Int -> Int -> QC.Property
partitionedPrefixOutsideRunSummaryTyped rawKey rawPartition rawGroup =
  compactPartitionedPrefixesBefore outsideRunOps (frontierForContexts [partitionValue]) entries
    QC.=== Left (PartitionedPrefixCompactionSummaryKeyOutsideRun outsideKey (IntSet.singleton actualKey))
  where
    actualKey =
      rawKey `mod` 24

    outsideKey =
      actualKey + 1

    partitionValue =
      rawPartition `mod` 5

    groupValue =
      rawGroup `mod` 4

    entries =
      IntMap.singleton actualKey (lawEntry actualKey partitionValue groupValue 0 1)

    outsideRunOps :: PartitionedPrefixCompactionOps Int Int Int LawEntry Int Int String
    outsideRunOps =
      lawPrefixCompactionOps
        { pcoSummarizeRun =
            \_frontier partitionValue' groupValue' runEntries ->
              Right
                ( Just
                    LawEntry
                      { leId = lawTraceId outsideKey,
                        lePartition = partitionValue',
                        leGroup = groupValue',
                        leTime = leTime (NonEmpty.head runEntries),
                        leValue = 1
                      }
                )
        }

timeIndexFromEntries :: [(String, TimeFrontier Int Int, Int)] -> TimeIndex String Int Int
timeIndexFromEntries =
  Foldable.foldl'
    ( \indexValue (key, frontier, member) ->
        insertTimeIndex
          key
          (tfEpoch frontier)
          (tfStamp frontier)
          (IntSet.singleton member)
          indexValue
    )
    emptyTimeIndex

advanceSinceAgrees ::
  UpperFrontier Int ->
  Trace Int String Char Int ->
  TraceDescription.TraceDescription Int ->
  QC.Property
advanceSinceAgrees requestedFrontier traceValue description =
  case (traceAdvanceSince requestedFrontier traceValue, TraceDescription.traceDescriptionAdvanceSince requestedFrontier description) of
    (Right advancedTrace, Right advancedDescription) ->
      TraceDescription.traceDescriptionSince advancedDescription QC.=== traceSince advancedTrace
    (Left TraceFrontierRegression {}, Left TraceDescription.TraceDescriptionFrontierRegression {}) ->
      QC.property True
    other ->
      QC.counterexample ("advance mismatch: " <> show other) False

advanceUpperAgrees ::
  UpperFrontier Int ->
  Trace Int String Char Int ->
  TraceDescription.TraceDescription Int ->
  QC.Property
advanceUpperAgrees requestedFrontier traceValue description =
  case (traceAdvanceUpper requestedFrontier traceValue, TraceDescription.traceDescriptionAdvanceUpper requestedFrontier description) of
    (Right advancedTrace, Right advancedDescription) ->
      TraceDescription.traceDescriptionUpper advancedDescription QC.=== traceUpper advancedTrace
    (Left TraceFrontierRegression {}, Left TraceDescription.TraceDescriptionFrontierRegression {}) ->
      QC.property True
    other ->
      QC.counterexample ("advance mismatch: " <> show other) False

traceDescriptionReadOracle ::
  Int ->
  TraceDescription.TraceDescription Int ->
  Either (TraceDescription.TraceDescriptionReadError Int) ()
traceDescriptionReadOracle readTime description
  | intFrontierAtOrBefore (singletonUpperFrontier readTime) (TraceDescription.traceDescriptionSince description) =
      Left (TraceDescription.TraceReadBeforeSince readTime (TraceDescription.traceDescriptionSince description))
  | not (intFrontierAtOrBefore (singletonUpperFrontier readTime) (TraceDescription.traceDescriptionUpper description)) =
      Left (TraceDescription.TraceReadBeyondUpper readTime (TraceDescription.traceDescriptionUpper description))
  | otherwise =
      Right ()

intFrontierAtOrBefore :: UpperFrontier Int -> UpperFrontier Int -> Bool
intFrontierAtOrBefore left right =
  all
    (\leftTime -> any (leftTime <=) (upperFrontierPoints right))
    (upperFrontierPoints left)

traceAccumPrefixOracle :: Int -> [TestTraceUpdate] -> ZSet.IndexedZSet String Char Int
traceAccumPrefixOracle cutoff updates =
  Foldable.foldl' insertPrefixUpdate ZSet.indexedZSetEmpty (batchToUpdates (fromUpdates updates :: TestBatch))
  where
    insertPrefixUpdate rows (Update time key value weight)
      | time <= cutoff =
          ZSet.indexedZSetInsert key value weight rows
      | otherwise =
          rows

type TraceKeyReplayMap = Map (Int, Char) Int

traceKeyFoldMap :: String -> [TestTraceUpdate] -> TraceKeyReplayMap
traceKeyFoldMap key updates =
  foldTraceKey
    key
    collectTraceKeyReplayCell
    Map.empty
    (traceFromUpdates updates)

traceKeyFoldThroughMap :: Int -> String -> [TestTraceUpdate] -> TraceKeyReplayMap
traceKeyFoldThroughMap cutoff key updates =
  foldTraceKeyThrough
    cutoff
    key
    collectTraceKeyReplayCell
    Map.empty
    (traceFromUpdates updates)

traceKeyFoldAfterMap :: Int -> String -> [TestTraceUpdate] -> TraceKeyReplayMap
traceKeyFoldAfterMap cutoff key updates =
  foldTraceKeyAfter
    cutoff
    key
    collectTraceKeyReplayCell
    Map.empty
    (traceFromUpdates updates)

traceKeyReplayMap :: (Int -> Bool) -> String -> [TestTraceUpdate] -> TraceKeyReplayMap
traceKeyReplayMap keepTime key updates =
  Foldable.foldl'
    ( \acc updateValue ->
        if updateKey updateValue == key && keepTime (updateTime updateValue)
          then collectTraceKeyReplayCell acc (updateTime updateValue) (updateVal updateValue) (updateWeight updateValue)
          else acc
    )
    Map.empty
    (batchToUpdates (fromUpdates updates :: TestBatch))

collectTraceKeyReplayCell :: TraceKeyReplayMap -> Int -> Char -> Int -> TraceKeyReplayMap
collectTraceKeyReplayCell cells time value weight =
  Map.insertWith (+) (time, value) weight cells

collectTraceKeyRows ::
  [TestTraceUpdate] ->
  String ->
  ZSet.ZSet (ZSet.Timed Int Char) Int ->
  [TestTraceUpdate]
collectTraceKeyRows updates key =
  ZSet.zsetFold
    ( \rows (ZSet.Timed time value) weight ->
        Update
          { updateTime = time,
            updateKey = key,
            updateVal = value,
            updateWeight = weight
          }
          : rows
    )
    updates

lawTraceIndexOps :: TraceIndexOps LawEntry LawIndexes LawIndexError
lawTraceIndexOps =
  TraceIndexOps
    { tioEntryId = leId,
      tioEmptyIndexes = emptyLawIndexes,
      tioInsertIndexes = insertLawIndex,
      tioDeleteIndexes = deleteLawIndex,
      tioValidateIndexes = validateLawIndexes
    }

emptyLawIndexes :: LawIndexes
emptyLawIndexes =
  LawIndexes Map.empty

insertLawIndex ::
  TraceId ->
  LawEntry ->
  LawIndexes ->
  LawIndexes
insertLawIndex traceId entry indexes =
  indexes
    { lawIndexesByPartition =
        Map.insertWith
          IntSet.union
          (lePartition entry)
          (IntSet.singleton (traceIdKey traceId))
          (lawIndexesByPartition indexes)
    }

deleteLawIndex ::
  TraceId ->
  LawEntry ->
  LawIndexes ->
  LawIndexes
deleteLawIndex traceId entry indexes =
  indexes
    { lawIndexesByPartition =
        Map.update
          (pruneIntSet . IntSet.delete (traceIdKey traceId))
          (lePartition entry)
          (lawIndexesByPartition indexes)
    }

validateLawIndexes ::
  IntMap LawEntry ->
  LawIndexes ->
  [LawIndexError]
validateLawIndexes entries actual =
  let expected =
        IntMap.foldl'
          (\indexes entry -> insertLawIndex (leId entry) entry indexes)
          emptyLawIndexes
          entries
   in if expected == actual
        then []
        else [LawIndexesMismatch expected actual]

pruneIntSet :: IntSet -> Maybe IntSet
pruneIntSet values
  | IntSet.null values =
      Nothing
  | otherwise =
      Just values

lawPrefixCompactionOps ::
  PartitionedPrefixCompactionOps Int Int Int LawEntry Int Int String
lawPrefixCompactionOps =
  PartitionedPrefixCompactionOps
    { pcoBatchKey = traceIdKey . leId,
      pcoBatchTime = leTime,
      pcoPartition = lePartition,
      pcoPartitionBlockedByPending = \partitionValue pendingTime -> partitionValue == rtContext pendingTime,
      pcoGroup = leGroup,
      pcoSummarizeRun = summarizeLawRun
    }

summarizeLawRun ::
  RuntimeFrontier Int Int Int ->
  Int ->
  Int ->
  NonEmpty LawEntry ->
  Either String (Maybe LawEntry)
summarizeLawRun _frontier partitionValue groupValue entries =
  if total == 0
    then Right Nothing
    else
      Right
        ( Just
            LawEntry
              { leId = leId firstEntry,
                lePartition = partitionValue,
                leGroup = groupValue,
                leTime = leTime (NonEmpty.last entries),
                leValue = total
              }
        )
  where
    total =
      Foldable.foldl' (\acc entry -> acc + leValue entry) 0 entries
    firstEntry =
      NonEmpty.head entries

lawTraceFromEntries ::
  [LawEntry] ->
  Either IndexedTraceError LawTrace
lawTraceFromEntries =
  foldM
    ( \traceValue entry ->
        insertIndexedTraceEntry lawTraceIndexOps entry traceValue
    )
    (emptyIndexedTraceWithOps lawTraceIndexOps)

lawTraceDenotation ::
  LawTrace ->
  Map Int Int
lawTraceDenotation =
  lawEntriesDenotation . itEntries

lawEntriesDenotation ::
  IntMap LawEntry ->
  Map Int Int
lawEntriesDenotation =
  Map.filter (/= 0)
    . IntMap.foldl'
      ( \acc entry ->
          Map.insertWith (+) (lePartition entry) (leValue entry) acc
      )
      Map.empty

frontierForContexts ::
  [Int] ->
  RuntimeFrontier Int Int Int
frontierForContexts contexts =
  foldr
    (frontierAdvanceVisibleMin . (`lawTime` 10))
    (frontierWithTraceRetention (Just emptyTraceRetention) emptyRuntimeFrontier)
    contexts

frontierForEntries :: [LawEntry] -> RuntimeFrontier Int Int Int
frontierForEntries =
  frontierForContexts . Set.toList . foldMap (Set.singleton . lePartition)

traceDescriptionForEntriesAt ::
  Word64 ->
  [LawEntry] ->
  TraceDescription.TraceDescription LawTime
traceDescriptionForEntriesAt cutoff entries =
  TraceDescription.traceDescription
    emptyFrontier
    (descriptionFrontierForEntriesAt 10 entries)
    (descriptionFrontierForEntriesAt cutoff entries)

descriptionFrontierForEntriesAt ::
  Word64 ->
  [LawEntry] ->
  UpperFrontier LawTime
descriptionFrontierForEntriesAt cutoff =
  mkUpperFrontier
    . fmap (`lawTime` cutoff)
    . Set.toList
    . foldMap (Set.singleton . lePartition)

entryAllowedByDescription ::
  TraceDescription.TraceDescription LawTime ->
  LawEntry ->
  Bool
entryAllowedByDescription description =
  (`TraceDescription.traceDescriptionTimeCompactable` description) . leTime

compactionCellEntry :: Int -> Int -> Int -> Word64 -> Int -> LawEntry
compactionCellEntry traceKey partitionValue groupValue stamp value =
  lawEntry traceKey partitionValue groupValue stamp value

lawEntry ::
  Int ->
  Int ->
  Int ->
  Word64 ->
  Int ->
  LawEntry
lawEntry traceKey partitionValue groupValue stamp value =
  LawEntry
    { leId = lawTraceId traceKey,
      lePartition = partitionValue,
      leGroup = groupValue,
      leTime = lawTime partitionValue stamp,
      leValue = value
    }

lawTraceId :: Int -> TraceId
lawTraceId =
  either (const initialTraceId) id . traceIdFromKey

lawTime ::
  Int ->
  Word64 ->
  LawTime
lawTime contextValue stamp =
  runtimeTime contextValue emptyRuntimeScope 0 0 (frontierStamp stamp)
