{-# LANGUAGE DerivingStrategies #-}

module TraceCompactionLawSpec
  ( tests,
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
import Data.Word
  ( Word64,
  )
import Moonlight.Delta.Frontier
  ( UpperFrontier,
    singletonUpperFrontier,
    upperFrontierPoints,
  )
import Moonlight.Differential.Batch
  ( Batch,
    batchToUpdates,
    emptyBatch,
    fromUpdates,
  )
import Moonlight.Differential.Frontier
  ( RuntimeFrontier,
    emptyRuntimeFrontier,
    emptyTraceRetention,
    frontierAdvanceVisibleMin,
    frontierInsertPending,
    frontierWithTraceRetention,
    traceRetention,
  )
import Moonlight.Differential.Time
  ( RuntimeTime,
    emptyRuntimeScope,
    frontierStamp,
    rtContext,
    runtimeTime,
  )
import Moonlight.Differential.Trace
  ( TraceFrontierAdvanceError (..),
    coalesceTraceBatches,
    traceAppendBatch,
    traceAdvanceSince,
    traceAdvanceUpper,
    traceFromBatch,
    traceFromBatches,
    traceSince,
    snapshotTraceBatch,
    traceUpper,
  )
import Moonlight.Differential.Trace.Compact
  ( PartitionedPrefixCompactionOps (..),
    PartitionedPrefixCompactionResult (..),
    applyIndexedTraceCompactionPlan,
    planIndexedTraceCompactionBefore,
  )
import Moonlight.Differential.Trace.Id
  ( TraceId,
    initialTraceId,
    traceIdFromKey,
    traceIdKey,
  )
import Moonlight.Differential.Trace.Indexed
  ( IndexedTrace,
    IndexedTraceError (..),
    TraceIndexOps (..),
    applyIndexedTraceRewrite,
    deleteIndexedTraceEntryAt,
    emptyIndexedTraceWithOps,
    insertIndexedTraceEntry,
    itEntries,
    validateIndexedTraceIndexes,
  )
import Moonlight.Differential.Update
  ( Update (..),
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
    (@?=),
  )

tests :: TestTree
tests =
  testGroup
    "trace compaction laws"
    [ testCase "row trace append, advance, and compaction preserve denotation" rowTraceCompactionLaws,
      testCase "indexed trace rewrite preserves indexes and reports typed obstructions" indexedTraceRewriteLaws,
      testCase "partitioned prefix compaction plans and applies by frontier law" partitionedPrefixCompactionLaws
    ]

type RowBatch = Batch Int String Char Int

type RowUpdate = Update Int String Char Int

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

rowTraceCompactionLaws :: Assertion
rowTraceCompactionLaws = do
  let left =
        rowBatch
          [ Update 0 "alpha" 'x' 1,
            Update 0 "alpha" 'x' 2
          ]
      middle =
        rowBatch
          [ Update 1 "alpha" 'x' (-3),
            Update 1 "beta" 'y' 5
          ]
      right =
        rowBatch
          [ Update 2 "beta" 'y' (-2),
            Update 3 "gamma" 'z' 7
          ]
      appended =
        traceAppendBatch right (traceAppendBatch middle (traceFromBatch left))
      fromBatches =
        traceFromBatches [left, middle, right]
      compactedOnce =
        coalesceTraceBatches (singletonUpperFrontier 1) appended
      compactedTwice =
        coalesceTraceBatches (singletonUpperFrontier 3) compactedOnce
      compactedAgain =
        coalesceTraceBatches (singletonUpperFrontier 3) compactedTwice

  assertEqual
    "traceFromBatches and append denote the same consolidated batch"
    (batchToUpdates (snapshotTraceBatch appended))
    (batchToUpdates (snapshotTraceBatch fromBatches))
  assertEqual
    "empty batch append is observationally absent"
    appended
    (traceAppendBatch emptyBatch appended)
  assertEqual
    "first compaction preserves consolidated row denotation"
    (batchToUpdates (snapshotTraceBatch appended))
    (batchToUpdates (snapshotTraceBatch compactedOnce))
  assertEqual
    "repeated compaction preserves consolidated row denotation"
    (batchToUpdates (snapshotTraceBatch appended))
    (batchToUpdates (snapshotTraceBatch compactedTwice))
  assertEqual
    "compaction is idempotent once the eligible prefix has vanished"
    compactedTwice
    compactedAgain

  advancedSince <-
    assertRight
      "monotone since advance"
      (traceAdvanceSince (singletonUpperFrontier 4) compactedTwice)
  assertEqual
    "since frontier advances monotonically"
    [4]
    (upperFrontierPoints (traceSince advancedSince))
  case traceAdvanceSince (singletonUpperFrontier 0) compactedTwice of
    Left TraceFrontierRegression {} ->
      pure ()
    Right _ ->
      assertFailure "since frontier regression should remain a typed obstruction"

  advancedUpper <-
    assertRight
      "monotone upper advance"
      (traceAdvanceUpper (singletonUpperFrontier 4) compactedTwice)
  assertEqual
    "upper frontier advances monotonically"
    [4]
    (upperFrontierPoints (traceUpper advancedUpper))
  case traceAdvanceUpper (singletonUpperFrontier 2) compactedTwice of
    Left TraceFrontierRegression {} ->
      pure ()
    Right _ ->
      assertFailure "upper frontier regression should remain a typed obstruction"

indexedTraceRewriteLaws :: Assertion
indexedTraceRewriteLaws = do
  trace0 <-
    assertRight
      "fixture trace"
      ( lawTraceFromEntries
          [ lawEntry 0 1 10 0 3,
            lawEntry 1 1 10 1 4,
            lawEntry 2 2 20 0 11
          ]
      )
  validateIndexedTraceIndexes lawTraceIndexOps trace0 @?= Right ()

  inserted <-
    assertRight
      "single insert"
      (insertIndexedTraceEntry lawTraceIndexOps (lawEntry 3 3 30 0 17) (emptyIndexedTraceWithOps lawTraceIndexOps))
  removed <-
    assertRight
      "single delete"
      (deleteIndexedTraceEntryAt lawTraceIndexOps 3 inserted)
  validateIndexedTraceIndexes lawTraceIndexOps removed @?= Right ()
  assertEqual "insert/delete round trip returns an empty trace" IntMap.empty (itEntries removed)

  rewritten <-
    assertRight
      "compacted rewrite"
      ( applyIndexedTraceRewrite
          lawTraceIndexOps
          (IntMap.fromList [(0, lawEntry 0 1 10 0 3), (1, lawEntry 1 1 10 1 4)])
          (IntMap.singleton 0 (lawEntry 0 1 10 1 7))
          trace0
      )
  validateIndexedTraceIndexes lawTraceIndexOps rewritten @?= Right ()
  assertEqual
    "indexed rewrite preserves partition denotation"
    (lawTraceDenotation trace0)
    (lawTraceDenotation rewritten)

  applyIndexedTraceRewrite
    lawTraceIndexOps
    (IntMap.singleton 0 (lawEntry 7 1 10 0 3))
    IntMap.empty
    trace0
    @?= Left (IndexedTraceEntryKeyMismatch 0 (lawTraceId 7))

  applyIndexedTraceRewrite
    lawTraceIndexOps
    (IntMap.singleton 9 (lawEntry 9 1 10 0 3))
    IntMap.empty
    trace0
    @?= Left (IndexedTraceEntryMissing 9)

  applyIndexedTraceRewrite
    lawTraceIndexOps
    (IntMap.singleton 0 (lawEntry 0 1 10 0 3))
    (IntMap.singleton 1 (lawEntry 1 1 10 1 7))
    trace0
    @?= Left (IndexedTraceEntryKeyCollision 1)

partitionedPrefixCompactionLaws :: Assertion
partitionedPrefixCompactionLaws = do
  trace0 <-
    assertRight
      "prefix fixture"
      ( lawTraceFromEntries
          [ lawEntry 0 1 10 0 3,
            lawEntry 1 1 10 1 4,
            lawEntry 2 1 20 2 5,
            lawEntry 3 2 10 0 7
          ]
      )
  prefixPlan <-
    assertRight
      "unblocked partition plan"
      (planIndexedTraceCompactionBefore lawPrefixCompactionOps (frontierForContexts [1]) trace0)
  assertEqual
    "eligible partition prefix is selected"
    [0, 1, 2]
    (IntMap.keys (ppcrCompacted prefixPlan))
  assertEqual
    "group changes split summaries"
    [0, 2]
    (IntMap.keys (ppcrSummaries prefixPlan))

  compactedTrace <-
    assertRight
      "apply partition plan"
      (applyIndexedTraceCompactionPlan lawTraceIndexOps prefixPlan trace0)
  validateIndexedTraceIndexes lawTraceIndexOps compactedTrace @?= Right ()
  assertEqual
    "partitioned compaction preserves denotation"
    (lawTraceDenotation trace0)
    (lawTraceDenotation compactedTrace)

  pinnedPlan <-
    assertRight
      "pinned plan"
      (planIndexedTraceCompactionBefore lawPrefixCompactionOps pinnedFrontier trace0)
  assertEqual
    "pinned retained id stops the compactable prefix"
    [0]
    (IntMap.keys (ppcrCompacted pinnedPlan))

  pendingPlan <-
    assertRight
      "pending plan"
      (planIndexedTraceCompactionBefore lawPrefixCompactionOps pendingPartitionFrontier trace0)
  assertEqual
    "pending work blocks only the matching partition"
    [3]
    (IntMap.keys (ppcrCompacted pendingPlan))

  zeroTrace <-
    assertRight
      "zero summary fixture"
      ( lawTraceFromEntries
          [ lawEntry 4 3 30 0 8,
            lawEntry 5 3 30 1 (-8)
          ]
      )
  zeroPlan <-
    assertRight
      "zero summary plan"
      (planIndexedTraceCompactionBefore lawPrefixCompactionOps (frontierForContexts [3]) zeroTrace)
  assertEqual
    "zero-summary run has no summary entries"
    []
    (IntMap.keys (ppcrSummaries zeroPlan))
  zeroCompacted <-
    assertRight
      "apply zero summary plan"
      (applyIndexedTraceCompactionPlan lawTraceIndexOps zeroPlan zeroTrace)
  assertEqual
    "zero-summary deletion preserves pruned denotation"
    (lawTraceDenotation zeroTrace)
    (lawTraceDenotation zeroCompacted)

rowBatch :: [RowUpdate] -> RowBatch
rowBatch =
  fromUpdates

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

pinnedFrontier :: RuntimeFrontier Int Int Int
pinnedFrontier =
  frontierWithTraceRetention
    (Just (traceRetention (IntSet.singleton 1) IntSet.empty IntSet.empty))
    (frontierForContexts [1])

pendingPartitionFrontier :: RuntimeFrontier Int Int Int
pendingPartitionFrontier =
  frontierInsertPending (lawTime 1 5) (frontierForContexts [1, 2])

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
  maybe initialTraceId id . traceIdFromKey

lawTime ::
  Int ->
  Word64 ->
  LawTime
lawTime contextValue stamp =
  runtimeTime contextValue emptyRuntimeScope 0 0 (frontierStamp stamp)

assertRight ::
  Show err =>
  String ->
  Either err value ->
  IO value
assertRight label eitherValue =
  case eitherValue of
    Right value ->
      pure value
    Left err ->
      assertFailure (label <> " failed: " <> show err)
