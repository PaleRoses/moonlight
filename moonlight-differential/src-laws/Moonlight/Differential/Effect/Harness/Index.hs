{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Differential.Effect.Harness.Index
  ( rowSubstrateSetAlgebra,
    rowSubstratesCanonicalizeNegativeRawIds,
    tupleWordConversionRejectsNegativeRepresentatives,
    rowSetDenseInsertDenotesIntSetInsert,
    indexedRowsValueBucketsDenoteBindings,
    indexedRegistryUpsertDeleteLaws,
    indexedRegistryValidationReportsObstructions,
    batchProjectsIntoIndexedRows,
    traceProjectionAccumulatesDuplicatePhysicalRows,
    projectedRowsDeltaMaintainsSnapshot,
    relationAdvanceMaintainsViews,
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
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Data.Vector.Unboxed qualified as VU

import Moonlight.Differential.Batch
  ( fromUpdates,
  )
import Moonlight.Differential.Index.IndexedRows
  ( IndexedRowBindingError (..),
    IndexedRowFormat,
    IndexedRows,
    emptyIndexedRows,
    indexedRowFormat,
    indexedRowsInsertFresh,
    indexedRowsPayloadMap,
    indexedRowsValueIndex,
  )
import Moonlight.Differential.Index.Registry
  ( IndexedRegistry,
    RegistryOps (..),
    deleteRegistryRowReturning,
    emptyIndexedRegistry,
    registryIndexes,
    registryRowsAscList,
    upsertRegistryRow,
    validateIndexedRegistry,
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
    lookupMany,
  )
import Moonlight.Differential.Index.RowArrangement
  ( IndexedRowArrangement (..),
    indexedRowArrangementFromRows,
  )
import Moonlight.Differential.Index.RowId
  ( RowId,
    mkRowId,
  )
import Moonlight.Differential.Index.RowIdSet
  ( RowIdSet,
    rowIdSetFromIntSetCanonical,
    rowIdSetIntersects,
    rowIdSetIntersection,
    rowIdSetToIntSet,
    rowIdSetUnion,
  )
import Moonlight.Differential.Index.RowProjection
  ( IndexedRowsProjectionError (..),
    ProjectedRowsDelta,
    RowChange (..),
    RowChanges (..),
    applyProjectedRowsDelta,
    batchToIndexedRows,
    projectBatchDelta,
    snapshotTraceToIndexedRows,
  )
import Moonlight.Differential.Index.RowSet
  ( rowSetDelete,
    rowSetDifference,
    rowSetFromIntSetCanonical,
    rowSetFromIntSetWithUniverse,
    rowSetInsert,
    rowSetIntersects,
    rowSetIntersection,
    rowSetToIntSet,
    rowSetUnion,
  )
import Moonlight.Differential.Relation
  ( RelationAdvance (..),
    RelationChanges (..),
    RelationPlan (..),
    advanceRelation,
    bootstrapRelation,
    relationRows,
    relationViews,
    validateRelation,
  )
import Moonlight.Differential.Row.Tuple
  ( RepKeyError (..),
    RowTupleKey,
    tupleKeyFromInts,
    tupleKeyToWord64Vector,
    withTupleKeyWord64Slots,
  )
import Moonlight.Differential.Trace
  ( traceFromUpdates,
  )
import Moonlight.Differential.Update
  ( Update (..),
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
  )

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

type RegistryLawRegistry = IndexedRegistry Int RegistryLawRow RegistryLawIndexes

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

indexedRegistryUpsertDeleteLaws :: Assertion
indexedRegistryUpsertDeleteLaws = do
  let row0 = registryLawRow 1 "ab" [10, 20]
      row1 = registryLawRow 1 "bc" [20, 30]
      emptyRegistry = emptyIndexedRegistry registryLawOps
      (old0, registry0) = upsertRegistryRow registryLawOps row0 emptyRegistry
      (old1, registry1) = upsertRegistryRow registryLawOps row1 registry0
      (deleted, registry2) = deleteRegistryRowReturning registryLawOps 1 registry1
  assertEqual "first insert has no displaced row" Nothing old0
  assertEqual "replacement returns displaced row" (Just row0) old1
  assertEqual "delete returns current row" (Just row1) deleted
  assertEqual "replacement prunes old map-only bucket" Set.empty (Map.findWithDefault Set.empty 'a' (rliByMap (registryIndexes registry1)))
  assertEqual "replacement keeps shared int bucket" (Set.singleton 1) (IntMap.findWithDefault Set.empty 20 (rliByInt (registryIndexes registry1)))
  assertEqual "lookup-many unions candidate rows" (Set.singleton 1) (lookupMany (rliByInt (registryIndexes registry1)) (IntSet.fromList [10, 30]))
  assertEqual "delete empties rows" [] (registryRowsAscList registry2)
  assertEqual "delete empties map index" Map.empty (rliByMap (registryIndexes registry2))
  assertEqual "delete empties int index" IntMap.empty (rliByInt (registryIndexes registry2))
  assertEqual "registry validates after insert/delete" (Right ()) (validateIndexedRegistry registryLawOps RegistryLawStoredUnderWrongId registry2)

indexedRegistryValidationReportsObstructions :: Assertion
indexedRegistryValidationReportsObstructions = do
  let row0 = registryLawRow 1 "a" [10]
      row1 = registryLawRow 2 "b" [20]
      registry0 = snd (upsertRegistryRow registryLawOps row0 (emptyIndexedRegistry registryLawOps))
      missingMapOps =
        registryLawOps
          { registryInsertIndexes =
              \rowId row indexes ->
                (insertRegistryLawIndexes rowId row indexes)
                  { rliByMap = Map.delete 'a' (rliByMap indexes)
                  }
          }
      staleIntOps =
        registryLawOps
          { registryInsertIndexes =
              \rowId row indexes ->
                let insertedIndexes = insertRegistryLawIndexes rowId row indexes
                 in insertedIndexes
                      { rliByInt =
                          IntMap.insertWith
                            Set.union
                            99
                            (Set.singleton rowId)
                            (rliByInt insertedIndexes)
                      }
          }
      wrongStoredIdOps =
        registryLawOps
          { registryRowId = const 3
          }
      missingMap =
        snd (upsertRegistryRow missingMapOps row1 registry0)
      staleInt =
        snd (upsertRegistryRow staleIntOps row1 registry0)
      wrongStoredId =
        snd (upsertRegistryRow wrongStoredIdOps row0 (emptyIndexedRegistry registryLawOps))
  assertBool
    "missing map axis is reported"
    (validationContains (RegistryLawMapMissing 1 'a') missingMap)
  assertBool
    "stale int axis is reported"
    (validationContains (RegistryLawIntStale 2 99) staleInt)
  assertBool
    "stored id mismatch is reported"
    (validationContains (RegistryLawStoredUnderWrongId 3 1) wrongStoredId)

validationContains :: RegistryLawError -> RegistryLawRegistry -> Bool
validationContains expected registry =
  case validateIndexedRegistry registryLawOps RegistryLawStoredUnderWrongId registry of
    Left errors -> expected `elem` errors
    Right () -> False

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

rowSubstrateSetAlgebra :: Assertion
rowSubstrateSetAlgebra = do
  let leftSet =
        IntSet.fromList [1, 3, 5, 8, 13, 21]
      rightSet =
        IntSet.fromList [0, 1, 2, 3, 5, 34]
      leftRows =
        rowSetFromIntSetCanonical leftSet
      rightRows =
        rowSetFromIntSetCanonical rightSet
      leftRowIds =
        rowIdSetFromIntSetCanonical leftSet
      rightRowIds =
        rowIdSetFromIntSetCanonical rightSet
  assertEqual
    "RowSet union denotes IntSet union"
    (IntSet.union leftSet rightSet)
    (rowSetToIntSet (rowSetUnion leftRows rightRows))
  assertEqual
    "RowSet difference denotes IntSet difference"
    (IntSet.difference leftSet rightSet)
    (rowSetToIntSet (rowSetDifference leftRows rightRows))
  assertEqual
    "RowSet intersection denotes IntSet intersection"
    (IntSet.intersection leftSet rightSet)
    (rowSetToIntSet (rowSetIntersection leftRows rightRows))
  assertEqual
    "RowSet intersects agrees with IntSet"
    (not (IntSet.null (IntSet.intersection leftSet rightSet)))
    (rowSetIntersects leftRows rightRows)
  assertEqual
    "RowIdSet union denotes IntSet union"
    (IntSet.union leftSet rightSet)
    (rowIdSetToIntSet (rowIdSetUnion leftRowIds rightRowIds))
  assertEqual
    "RowIdSet intersection denotes IntSet intersection"
    (IntSet.intersection leftSet rightSet)
    (rowIdSetToIntSet (rowIdSetIntersection leftRowIds rightRowIds))
  assertEqual
    "RowIdSet intersects agrees with IntSet"
    (not (IntSet.null (IntSet.intersection leftSet rightSet)))
    (rowIdSetIntersects leftRowIds rightRowIds)

rowSubstratesCanonicalizeNegativeRawIds :: Assertion
rowSubstratesCanonicalizeNegativeRawIds = do
  let rawRows =
        IntSet.fromList [-7, -1, 0, 2, 5]
      expectedRows =
        IntSet.fromList [0, 2, 5]
  assertEqual
    "RowIdSet raw construction drops values outside the RowId domain"
    expectedRows
    (rowIdSetToIntSet (rowIdSetFromIntSetCanonical rawRows))
  assertEqual
    "RowSet raw construction drops values outside the RowId domain"
    expectedRows
    (rowSetToIntSet (rowSetFromIntSetCanonical rawRows))
  assertEqual
    "RowSet explicit universe is widened before dense construction can erase live rows"
    (IntSet.fromList [4])
    (rowSetToIntSet (rowSetFromIntSetWithUniverse 1 (IntSet.fromList [-1, 4])))

tupleWordConversionRejectsNegativeRepresentatives :: Assertion
tupleWordConversionRejectsNegativeRepresentatives = do
  assertEqual
    "tuple vector conversion rejects negative representatives"
    (Left (NegativeRepKey (-1)))
    (fmap VU.toList (tupleKeyToWord64Vector (tupleKeyFromInts [-1, 2] :: RowTupleKey)))
  assertEqual
    "slot callback conversion rejects negative representatives"
    (Left (NegativeRepKey (-1)))
    (withTupleKeyWord64Slots (tupleKeyFromInts [-1] :: RowTupleKey) (\width _slotAt -> width))

rowSetDenseInsertDenotesIntSetInsert :: Assertion
rowSetDenseInsertDenotesIntSetInsert = do
  insideRowId <- checkedLawRowId 17
  adjacentRowId <- checkedLawRowId 1024
  rowIdsToDelete <- traverse checkedLawRowId [0 .. 896]
  let denseMissingInside =
        IntSet.delete 17 (IntSet.fromAscList [0 .. 1023])
      denseFull =
        IntSet.fromAscList [0 .. 1023]
      deletedPrefix =
        IntSet.fromAscList [0 .. 896]
  assertEqual
    "dense insert inside existing universe"
    (IntSet.insert 17 denseMissingInside)
    ( rowSetToIntSet
        (rowSetInsert insideRowId (rowSetFromIntSetCanonical denseMissingInside))
    )
  assertEqual
    "dense insert extending universe"
    (IntSet.insert 1024 denseFull)
    ( rowSetToIntSet
        (rowSetInsert adjacentRowId (rowSetFromIntSetCanonical denseFull))
    )
  assertEqual
    "dense delete inside existing universe"
    (IntSet.delete 17 denseFull)
    ( rowSetToIntSet
        (rowSetDelete insideRowId (rowSetFromIntSetCanonical denseFull))
    )
  assertEqual
    "dense repeated delete transitions back to sparse denotation"
    (IntSet.difference denseFull deletedPrefix)
    ( rowSetToIntSet
        (Foldable.foldl' (flip rowSetDelete) (rowSetFromIntSetCanonical denseFull) rowIdsToDelete)
    )

checkedLawRowId :: Int -> IO RowId
checkedLawRowId rawRowId =
  case mkRowId rawRowId of
    Left obstruction ->
      assertFailure ("invalid law row id " <> show rawRowId <> ": " <> show obstruction)
    Right rowId ->
      pure rowId

indexedRowsValueBucketsDenoteBindings :: Assertion
indexedRowsValueBucketsDenoteBindings = do
  rows <-
    insertLawRows
      ( [ (LawRowKey [1, 10, 100], 11),
          (LawRowKey [2, 20, 200], 22),
          (LawRowKey [1, 30, 300], 33)
        ] ::
          [(LawRowKey, Int)]
      )
  assertEqual
    "value buckets point exactly at rows carrying the slot value"
    ( IntMap.fromList
        [ (0, IntMap.fromList [(1, IntSet.fromList [0, 2]), (2, IntSet.singleton 1)]),
          (1, IntMap.fromList [(10, IntSet.singleton 0), (20, IntSet.singleton 1), (30, IntSet.singleton 2)]),
          (2, IntMap.fromList [(100, IntSet.singleton 0), (200, IntSet.singleton 1), (300, IntSet.singleton 2)])
        ]
    )
    (normalizeLawBuckets (indexedRowsValueIndex rows))

batchProjectsIntoIndexedRows :: Assertion
batchProjectsIntoIndexedRows = do
  let batch =
        fromUpdates
          [ Update 0 1 10 4,
            Update 0 2 20 8
          ]
  case batchToIndexedRows lawIndexedFormat lawLayoutColumnIndex lawLayout lawProjectCell batch of
    Left obstruction ->
      assertFailure ("unexpected batch projection obstruction: " <> show obstruction)
    Right rows ->
      assertEqual
        "projected batch cells become indexed rows"
        (Map.fromList [(LawRowKey [0, 1, 10], 4), (LawRowKey [0, 2, 20], 8)])
        (indexedRowsPayloadMap rows)

traceProjectionAccumulatesDuplicatePhysicalRows :: Assertion
traceProjectionAccumulatesDuplicatePhysicalRows = do
  let traceValue =
        traceFromUpdates
          [ Update 0 1 10 2,
            Update 1 1 10 3,
            Update 2 1 10 (-5),
            Update 3 2 20 7
          ]
      snapshotProject :: Int -> Int -> Int -> Int -> Maybe (LawRowKey, Int)
      snapshotProject _time key val weight =
        Just (LawRowKey [0, key, val], weight)
  case indexedRowArrangementFromRows <$> snapshotTraceToIndexedRows lawIndexedFormat lawLayoutColumnIndex lawLayout snapshotProject traceValue of
    Left obstruction ->
      assertFailure ("unexpected trace projection obstruction: " <> show obstruction)
    Right arrangement ->
      assertEqual
        "trace projection accumulates repeated projected row deltas and prunes zero rows"
        (Map.fromList [(LawRowKey [0, 2, 20], 7)])
        (indexedRowsPayloadMap (indexedRowArrangementRows arrangement))

projectedRowsDeltaMaintainsSnapshot :: Assertion
projectedRowsDeltaMaintainsSnapshot = do
  let firstUpdates =
        [ Update 0 1 10 5,
          Update 0 2 20 7
        ]
      secondUpdates =
        [ Update 1 1 10 (-5),
          Update 1 2 20 3,
          Update 1 3 30 11
        ]
      projectedIdentity :: Int -> Int -> Int -> Int -> Maybe (LawRowKey, Int)
      projectedIdentity _time key val weight =
        Just (LawRowKey [0, key, val], weight)
      applyBatch ::
        LawIndexedRows Int ->
        [Update Int Int Int Int] ->
        Either String (RowChanges LawRowKey Int, LawIndexedRows Int)
      applyBatch rows updates =
        case
          projectBatchDelta projectedIdentity (fromUpdates updates) ::
            Either
              (IndexedRowsProjectionError Int Int Int Int LawRowKey [Int])
              (ProjectedRowsDelta LawRowKey Int)
          of
          Left obstruction ->
            Left ("projection: " <> show obstruction)
          Right delta ->
            case applyProjectedRowsDelta lawIndexedFormat delta rows of
              Left obstruction ->
                Left ("advance: " <> show obstruction)
              Right advanced ->
                Right advanced
      expectedSnapshot =
        snapshotTraceToIndexedRows
          lawIndexedFormat
          lawLayoutColumnIndex
          lawLayout
          projectedIdentity
          (traceFromUpdates (firstUpdates <> secondUpdates))
  case applyBatch emptyLawRows firstUpdates >>= \(RowChanges _firstChanges, firstRows) -> applyBatch firstRows secondUpdates of
    Left obstruction ->
      assertFailure ("unexpected incremental projection obstruction: " <> obstruction)
    Right (RowChanges secondChanges, maintainedRows) -> do
      case expectedSnapshot of
        Left obstruction ->
          assertFailure ("unexpected snapshot projection obstruction: " <> show obstruction)
        Right snapshotRows ->
          assertEqual
            "incremental projected rows match snapshot payload denotation"
            (indexedRowsPayloadMap snapshotRows)
            (indexedRowsPayloadMap maintainedRows)
      assertEqual
        "incremental projection reports delete/change/insert transitions"
        [ RowDeleted <$> mkRowId 0 <*> pure (LawRowKey [0, 1, 10]) <*> pure 5,
          RowPayloadChanged <$> mkRowId 1 <*> pure (LawRowKey [0, 2, 20]) <*> pure 7 <*> pure 10,
          RowInserted <$> mkRowId 2 <*> pure (LawRowKey [0, 3, 30]) <*> pure 11
        ]
        (Right <$> secondChanges)

relationAdvanceMaintainsViews :: Assertion
relationAdvanceMaintainsViews = do
  let firstUpdates =
        [ Update 0 1 10 5,
          Update 0 2 20 7
        ]
      secondUpdates =
        [ Update 1 1 10 (-5),
          Update 1 2 20 3,
          Update 1 3 30 11
        ]
      projectedIdentity :: Int -> Int -> Int -> Int -> Maybe (LawRowKey, Int)
      projectedIdentity _time key val weight =
        Just (LawRowKey [0, key, val], weight)
      relationPlan =
        RelationPlan
          { relationIndexedFormat = lawIndexedFormat,
            relationLayoutColumnIndex = lawLayoutColumnIndex,
            relationLayout = lawLayout,
            relationProjectCell = projectedIdentity
          }
      expectedSnapshot =
        snapshotTraceToIndexedRows
          lawIndexedFormat
          lawLayoutColumnIndex
          lawLayout
          projectedIdentity
          (traceFromUpdates (firstUpdates <> secondUpdates))
  case bootstrapRelation relationPlan (traceFromUpdates firstUpdates) of
    Left obstruction ->
      assertFailure ("unexpected relation bootstrap obstruction: " <> show obstruction)
    Right initialRelation ->
      case advanceRelation relationPlan (fromUpdates secondUpdates) initialRelation of
        Left obstruction ->
          assertFailure ("unexpected relation advance obstruction: " <> show obstruction)
        Right advance -> do
          case validateRelation relationPlan (relationNextState advance) of
            Left obstruction ->
              assertFailure ("advanced relation failed validation: " <> show obstruction)
            Right () ->
              pure ()
          case expectedSnapshot of
            Left obstruction ->
              assertFailure ("unexpected relation snapshot obstruction: " <> show obstruction)
            Right snapshotRows ->
              assertEqual
                "relation rows advance with the same batch as the trace"
                (indexedRowsPayloadMap snapshotRows)
                (indexedRowsPayloadMap (relationRows (relationViews (relationNextState advance))))
          assertEqual
            "relation emits the touched projected rows"
            3
            (length (unRowChanges (relationRowChanges (relationChanges advance))))
