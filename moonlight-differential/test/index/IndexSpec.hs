{-# LANGUAGE DerivingStrategies #-}

module IndexSpec
  ( tests,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set

import Moonlight.Differential.Batch
  ( fromUpdates,
  )
import Moonlight.Differential.Index.IndexedRows
  ( IndexedRowBindingError (..),
    IndexedRowFormat,
    IndexedRows,
    IndexedRowsInsertError (..),
    IndexedRowsCursorError (..),
    emptyIndexedRows,
    indexedRowFormat,
    indexedRowsInsertFresh,
    indexedRowsInsertWithId,
    indexedRowsNextRowId,
    indexedRowsPayloadMap,
    indexedRowsRebuildValueIndex,
    indexedRowsValueIndex,
    indexedRowsRowUniverse,
    validateIndexedRowsCursor,
  )
import Moonlight.Differential.Index.Registry
  ( RegistryOps (..),
    deleteRegistryRowReturning,
    emptyIndexedRegistry,
    lookupRegistryRow,
    registrySize,
    upsertRegistryRow,
  )
import Moonlight.Differential.Index.Reverse
  ( validateIntReverseIndex,
    validateMapReverseIndex,
  )
import Moonlight.Differential.Index.Reverse.Batch
  ( addMembership,
    dropMapAxis,
    dropMembership,
    insertMapAxis,
  )
import Moonlight.Differential.Index.RowArrangement
  ( IndexedRowArrangement (..),
    indexedRowArrangementFromRows,
    indexedRowArrangementRestrictRowsByPins,
    indexedRowArrangementWithDirtyKeys,
  )
import Moonlight.Differential.Index.RowId
  ( RowIdCursor (..),
    RowIdError (..),
    initialRowId,
    mkRowId,
  )
import Moonlight.Differential.Index.RowIdSet
  ( RowIdSet,
    rowIdSetToIntSet,
  )
import Moonlight.Differential.Index.RowProjection
  ( IndexedRowsProjectionError (..),
    batchToIndexedRows,
    snapshotTraceToIndexedRows,
  )
import Moonlight.Differential.Index.RowSet
  ( rowSetInsert,
    rowSetToIntSet,
    singletonRowSet,
    validateRowSet,
  )
import Moonlight.Differential.Trace
  ( traceFromUpdates,
  )
import Moonlight.Differential.Update
  ( Update (..),
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertEqual,
    assertFailure,
    testCase,
    (@?=),
  )
import Test.Moonlight.Differential.Index.IndexedRows
  ( indexedRowsWithNextRowIdForValidation,
  )

tests :: TestTree
tests =
  testGroup
    "index laws"
    [ testCase "IndexedRegistry returns displaced rows on upsert and delete" indexedRegistryReturnsDisplacedRows,
      testCase "IndexedRows rebuild preserves row payloads and bucket denotation" indexedRowsRebuildPreservesDenotation,
      testCase "bounded row identifiers exhaust without wrapping" boundedRowIdentifiersExhaust,
      testCase "IndexedRowArrangement restricts visible and dirty rows by keys and pins" indexedRowArrangementRestrictsRows,
      testCase "Batch projection to arrangement glues through IndexedRows" batchProjectionArrangementGluesThroughIndexedRows,
      testCase "Trace projects into IndexedRowArrangement through consolidated Batch" traceProjectsIntoIndexedRowArrangement,
      testCase "Rejected physical row projection is typed" rejectedPhysicalRowProjectionReported,
      testCase "Duplicate physical row projection is rejected" duplicatePhysicalRowProjectionRejected
    ]

data RegistryLawRow = RegistryLawRow
  { rlrId :: !Int,
    rlrMapKeys :: !(Set Char),
    rlrIntKeys :: !IntSet.IntSet
  }
  deriving stock (Eq, Show)

data RegistryLawIndexes = RegistryLawIndexes
  { rliByMap :: !(Map.Map Char (Set Int)),
    rliByInt :: !(IntMap (Set Int))
  }
  deriving stock (Eq, Show)

data RegistryLawError
  = RegistryLawStoredUnderWrongId !Int !Int
  | RegistryLawMapMissing !Int !Char
  | RegistryLawMapStale !Int !Char
  | RegistryLawIntMissing !Int !Int
  | RegistryLawIntStale !Int !Int
  deriving stock (Eq, Show)

registryLawOps :: RegistryOps Int RegistryLawRow RegistryLawIndexes RegistryLawError
registryLawOps =
  RegistryOps
    { registryRowId = rlrId,
      registryEmptyIndexes =
        RegistryLawIndexes
          { rliByMap = Map.empty,
            rliByInt = IntMap.empty
          },
      registryInsertIndexes = insertRegistryLawIndexes,
      registryDeleteIndexes = deleteRegistryLawIndexes,
      registryValidateIndexes = validateRegistryLawIndexes
    }

insertRegistryLawIndexes ::
  Int ->
  RegistryLawRow ->
  RegistryLawIndexes ->
  RegistryLawIndexes
insertRegistryLawIndexes rowId row indexes =
  indexes
    { rliByMap = insertMapAxis rowId (rlrMapKeys row) (rliByMap indexes),
      rliByInt = addMembership rowId (rlrIntKeys row) (rliByInt indexes)
    }

deleteRegistryLawIndexes ::
  Int ->
  RegistryLawRow ->
  RegistryLawIndexes ->
  RegistryLawIndexes
deleteRegistryLawIndexes rowId row indexes =
  indexes
    { rliByMap = dropMapAxis rowId (rlrMapKeys row) (rliByMap indexes),
      rliByInt = dropMembership rowId (rlrIntKeys row) (rliByInt indexes)
    }

validateRegistryLawIndexes ::
  Map.Map Int RegistryLawRow ->
  RegistryLawIndexes ->
  [RegistryLawError]
validateRegistryLawIndexes rows indexes =
  validateMapReverseIndex
    (\_ row -> rlrMapKeys row)
    RegistryLawMapMissing
    RegistryLawMapStale
    rows
    (rliByMap indexes)
    <> validateIntReverseIndex
      (\_ row -> rlrIntKeys row)
      RegistryLawIntMissing
      RegistryLawIntStale
      rows
      (rliByInt indexes)

registryLawRow :: Int -> [Char] -> [Int] -> RegistryLawRow
registryLawRow rowId mapKeys intKeys =
  RegistryLawRow
    { rlrId = rowId,
      rlrMapKeys = Set.fromList mapKeys,
      rlrIntKeys = IntSet.fromList intKeys
    }

indexedRegistryReturnsDisplacedRows :: IO ()
indexedRegistryReturnsDisplacedRows = do
  let row0 = registryLawRow 7 "x" [1]
      row1 = registryLawRow 7 "y" [2]
      registry0 = emptyIndexedRegistry registryLawOps
      (old0, registry1) = upsertRegistryRow registryLawOps row0 registry0
      (old1, registry2) = upsertRegistryRow registryLawOps row1 registry1
      (missingDelete, registry3) = deleteRegistryRowReturning registryLawOps 999 registry2
      (deleted, registry4) = deleteRegistryRowReturning registryLawOps 7 registry3
  assertEqual "first upsert has no old row" Nothing old0
  assertEqual "second upsert returns old row" (Just row0) old1
  assertEqual "missing delete has no old row" Nothing missingDelete
  assertEqual "delete returns replaced row" (Just row1) deleted
  assertEqual "lookup misses after delete" Nothing (lookupRegistryRow 7 registry4)
  assertEqual "size after delete" 0 (registrySize registry4)

newtype LawRowKey = LawRowKey
  { unLawRowKey :: [Int]
  }
  deriving stock (Eq, Ord, Show)

type LawIndexedRows payload = IndexedRows [Int] LawRowKey payload

lawLayout :: [Int]
lawLayout =
  [0, 1, 2]

lawLayoutColumnIndex :: [Int] -> IntMap Int
lawLayoutColumnIndex layout =
  IntMap.fromList (zip layout [0 ..])

lawIndexedFormat :: IndexedRowFormat [Int] LawRowKey
lawIndexedFormat =
  indexedRowFormat (length . unLawRowKey) length lawFoldBindings

lawFoldBindings ::
  [Int] ->
  LawRowKey ->
  (Int -> Int -> acc -> acc) ->
  acc ->
  Either (IndexedRowBindingError [Int] LawRowKey) acc
lawFoldBindings layout (LawRowKey values) step initial
  | length layout /= length values =
      Left (IndexedRowWidthMismatch (LawRowKey values) (length layout) (length values))
  | otherwise =
      Right (foldl' foldBinding initial (zip layout values))
  where
    foldBinding acc (slot, value) =
      step slot value acc

emptyLawRows :: LawIndexedRows payload
emptyLawRows =
  emptyIndexedRows lawLayoutColumnIndex lawLayout

insertLawRows ::
  [(LawRowKey, payload)] ->
  IO (LawIndexedRows payload)
insertLawRows =
  foldM insertLawRow emptyLawRows

insertLawRow ::
  LawIndexedRows payload ->
  (LawRowKey, payload) ->
  IO (LawIndexedRows payload)
insertLawRow rows (rowKey, payload) =
  case indexedRowsInsertFresh lawIndexedFormat rowKey payload rows of
    Left obstruction ->
      assertFailure ("failed to insert law row " <> show rowKey <> ": " <> show obstruction)
    Right (_rowId, rows') ->
      pure rows'

normalizeLawBuckets ::
  IntMap (IntMap RowIdSet) ->
  IntMap (IntMap IntSet.IntSet)
normalizeLawBuckets =
  fmap (fmap rowIdSetToIntSet)

lawProjectCell ::
  Int ->
  Int ->
  Int ->
  Int ->
  Maybe (LawRowKey, Int)
lawProjectCell time key val weight =
  Just (LawRowKey [time, key, val], weight)

indexedRowsRebuildPreservesDenotation :: IO ()
indexedRowsRebuildPreservesDenotation = do
  rows <-
    insertLawRows
      ( [ (LawRowKey [1, 10, 100], 11),
          (LawRowKey [2, 20, 200], 22),
          (LawRowKey [1, 30, 300], 33)
        ] ::
          [(LawRowKey, Int)]
      )
  case indexedRowsRebuildValueIndex lawIndexedFormat rows of
    Left obstruction ->
      assertFailure ("unexpected rebuild obstruction: " <> show obstruction)
    Right rebuilt -> do
      assertEqual
        "rebuild preserves row payload authority"
        (indexedRowsPayloadMap rows)
        (indexedRowsPayloadMap rebuilt)
      assertEqual
        "rebuild preserves bucket denotation"
        (normalizeLawBuckets (indexedRowsValueIndex rows))
        (normalizeLawBuckets (indexedRowsValueIndex rebuilt))

boundedRowIdentifiersExhaust :: IO ()
boundedRowIdentifiersExhaust = do
  mkRowId maxBound @?= Left (ReservedRowId maxBound)
  largestId <-
    either
      (\rowIdError -> assertFailure ("largest valid row identifier rejected: " <> show rowIdError))
      pure
      (mkRowId (maxBound - 1))
  exhaustedRows <-
    either
      (\insertError -> assertFailure ("largest valid row insertion failed: " <> show insertError))
      pure
      (indexedRowsInsertWithId lawIndexedFormat largestId (LawRowKey [1, 2, 3]) (1 :: Int) emptyLawRows)
  indexedRowsNextRowId exhaustedRows @?= RowIdsExhausted
  indexedRowsRowUniverse exhaustedRows @?= maxBound
  indexedRowsInsertFresh lawIndexedFormat (LawRowKey [4, 5, 6]) 2 exhaustedRows
    @?= Left IndexedRowsInsertIdsExhausted
  let largestSingleton = singletonRowSet largestId
      sparseExtremePair = rowSetInsert initialRowId largestSingleton
  validateRowSet largestSingleton @?= Right ()
  rowSetToIntSet sparseExtremePair
    @?= IntSet.fromList [0, maxBound - 1]
  validateIndexedRowsCursor
    (indexedRowsWithNextRowIdForValidation maxBound emptyLawRows)
    @?= Left (IndexedRowsCursorInvalid (ReservedRowId maxBound))

indexedRowArrangementRestrictsRows :: IO ()
indexedRowArrangementRestrictsRows = do
  rows <-
    insertLawRows
      ( [ (LawRowKey [1, 10, 100], 11),
          (LawRowKey [2, 20, 200], 22),
          (LawRowKey [1, 30, 300], 33)
        ] ::
          [(LawRowKey, Int)]
      )
  let arrangement =
        indexedRowArrangementFromRows rows
      dirtyArrangement =
        indexedRowArrangementWithDirtyKeys
          (Set.singleton (LawRowKey [2, 20, 200]))
          arrangement
      restricted =
        indexedRowArrangementRestrictRowsByPins
          (IntMap.singleton 0 2)
          dirtyArrangement
  assertEqual
    "dirty keys descend to row ids"
    (IntSet.singleton 1)
    (rowSetToIntSet (indexedRowArrangementDirtyRows dirtyArrangement))
  assertEqual
    "pin restriction cuts visible rows"
    (IntSet.singleton 1)
    (rowSetToIntSet (indexedRowArrangementVisibleRows restricted))
  assertEqual
    "pin restriction cuts dirty rows too"
    (IntSet.singleton 1)
    (rowSetToIntSet (indexedRowArrangementDirtyRows restricted))

batchProjectionArrangementGluesThroughIndexedRows :: IO ()
batchProjectionArrangementGluesThroughIndexedRows = do
  let batch =
        fromUpdates
          [ Update 0 1 10 4,
            Update 0 2 20 8
          ]
      projectedRows =
        batchToIndexedRows lawIndexedFormat lawLayoutColumnIndex lawLayout lawProjectCell batch
      projectedArrangement =
        indexedRowArrangementFromRows
          <$> batchToIndexedRows lawIndexedFormat lawLayoutColumnIndex lawLayout lawProjectCell batch
  case (projectedRows, projectedArrangement) of
    (Right rows, Right arrangement) ->
      assertEqual
        "arrangement projection is exactly gluing the indexed row section"
        (indexedRowArrangementFromRows rows)
        arrangement
    (Left rowObstruction, _) ->
      assertFailure ("unexpected row projection obstruction: " <> show rowObstruction)
    (_, Left arrangementObstruction) ->
      assertFailure ("unexpected arrangement projection obstruction: " <> show arrangementObstruction)

traceProjectsIntoIndexedRowArrangement :: IO ()
traceProjectsIntoIndexedRowArrangement = do
  let traceValue =
        traceFromUpdates
          [ Update 0 1 10 2,
            Update 0 1 10 3,
            Update 0 2 20 5
          ]
  case indexedRowArrangementFromRows <$> snapshotTraceToIndexedRows lawIndexedFormat lawLayoutColumnIndex lawLayout lawProjectCell traceValue of
    Left obstruction ->
      assertFailure ("unexpected trace projection obstruction: " <> show obstruction)
    Right arrangement -> do
      assertEqual
        "trace projection consolidates through Batch before row materialization"
        (Map.fromList [(LawRowKey [0, 1, 10], 5), (LawRowKey [0, 2, 20], 5)])
        (indexedRowsPayloadMap (indexedRowArrangementRows arrangement))
      assertEqual
        "fresh arrangement exposes all projected rows and no dirty rows"
        (IntSet.fromList [0, 1])
        (rowSetToIntSet (indexedRowArrangementVisibleRows arrangement))
      assertEqual
        "fresh arrangement has no dirty rows"
        IntSet.empty
        (rowSetToIntSet (indexedRowArrangementDirtyRows arrangement))

rejectedPhysicalRowProjectionReported :: IO ()
rejectedPhysicalRowProjectionReported = do
  let batch =
        fromUpdates
          [Update 0 1 10 4]
      rejectProject :: Int -> Int -> Int -> Int -> Maybe (LawRowKey, Int)
      rejectProject _time _key _val _weight =
        Nothing
      result =
        batchToIndexedRows
          lawIndexedFormat
          lawLayoutColumnIndex
          lawLayout
          rejectProject
          batch ::
          Either
            (IndexedRowsProjectionError Int Int Int Int LawRowKey [Int])
            (LawIndexedRows Int)
  assertEqual
    "projection rejection preserves the rejected logical cell"
    (Left (IndexedRowsProjectionRejected 0 1 10 4))
    result

duplicatePhysicalRowProjectionRejected :: IO ()
duplicatePhysicalRowProjectionRejected = do
  let batch =
        fromUpdates
          [ Update 0 1 10 2,
            Update 1 1 10 3
          ]
      duplicateProject :: Int -> Int -> Int -> Int -> Maybe (LawRowKey, Int)
      duplicateProject _time key val weight =
        Just (LawRowKey [key, val, 0], weight)
      result =
        batchToIndexedRows
          lawIndexedFormat
          lawLayoutColumnIndex
          lawLayout
          duplicateProject
          batch ::
          Either
            (IndexedRowsProjectionError Int Int Int Int LawRowKey [Int])
            (LawIndexedRows Int)
  assertEqual
    "duplicate physical row keys are a typed obstruction"
    (Left (IndexedRowsProjectionInsertFailed (LawRowKey [1, 10, 0]) (IndexedRowsInsertDuplicateKey (LawRowKey [1, 10, 0]))))
    result
