{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}

module Moonlight.Differential.Index.IndexedRows
  ( IndexedRowFormat,
    indexedRowFormat,
    indexedRowFormatWidth,
    indexedRowFormatLayoutWidth,
    foldIndexedRowFormatBindings,
    IndexedRowBindingError (..),
    IndexedRowsBuildError (..),
    IndexedRowsInsertError (..),
    IndexedRowsCursorError (..),
    IndexedRowsDeleteError (..),
    IndexedRowsPayloadError (..),
    IndexedRows,
    emptyIndexedRows,
    indexedRowsLayout,
    indexedRowsColumnIndex,
    indexedRowsLiveRows,
    indexedRowsLiveRowSet,
    indexedRowsKeyByRowId,
    indexedRowsIdByKey,
    indexedRowsPayloadMap,
    indexedRowsPayloadByRowId,
    indexedRowsRowById,
    indexedRowsFromPayloadMap,
    indexedRowsFromPayloadMapWithValueIndex,
    indexedRowsValueIndex,
    indexedRowsNextRowId,
    indexedRowsRowUniverse,
    validateIndexedRowsCursor,
    indexedRowsSize,
    indexedRowsLookupId,
    indexedRowsKeyAt,
    indexedRowsLookupPayload,
    indexedRowsPayloadAtRowId,
    indexedRowsRowAt,
    indexedRowsRestrictRowsByPins,
    indexedRowsRestrictLiveRowsByPins,
    indexedRowsInsertFresh,
    indexedRowsInsertWithId,
    indexedRowsDelete,
    indexedRowsSetPayload,
    indexedRowsMapPayloadEither,
    indexedRowsRebuildValueIndex,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Moonlight.Differential.Internal.Index.IndexedRows
  ( IndexedRows (..),
  )
import Moonlight.Differential.Index.RowId
  ( RowId,
    RowIdCursor (..),
    RowIdError,
    initialRowId,
    mkRowId,
    rowIdCursorExclusiveUniverse,
    rowIdCursorFromExclusiveUniverse,
    rowIdInt,
  )
import Moonlight.Differential.Index.RowIdSet
  ( RowIdSet,
    rowIdSetDelete,
    rowIdSetFromIntSetCanonical,
    rowIdSetIntersection,
    rowIdSetIntersectionWithIntSet,
    rowIdSetNull,
    rowIdSetSize,
    rowIdSetUnion,
    singletonRowIdSet,
  )
import Moonlight.Differential.Index.RowSet
  ( RowSet,
    RowSetRestriction (..),
    emptyRowSet,
    rowSetFromIntSetWithUniverse,
    rowSetIntersectionWithRowIdSetChanged,
  )

type IndexedRowFormat :: Type -> Type -> Type
data IndexedRowFormat layout key = IndexedRowFormat
  { irfWidthRaw :: key -> Int,
    irfLayoutWidthRaw :: layout -> Int,
    irfFoldBindings ::
      forall acc.
      layout ->
      key ->
      (Int -> Int -> acc -> acc) ->
      acc ->
      Either (IndexedRowBindingError layout key) acc
  }

type IndexedRowBindingError :: Type -> Type -> Type
data IndexedRowBindingError layout key
  = IndexedRowWidthMismatch !key !Int !Int
  | IndexedRowBindingsRejected !layout !key
  deriving stock (Eq, Ord, Show)

type IndexedRowsBuildError :: Type -> Type -> Type
data IndexedRowsBuildError layout key
  = IndexedRowsBuildBindingFailed !Int !key !(IndexedRowBindingError layout key)
  | IndexedRowsBuildValueIndexMismatch
  | IndexedRowsBuildCursorInvalid !RowIdError
  deriving stock (Eq, Ord, Show)

type IndexedRowsInsertError :: Type -> Type -> Type
data IndexedRowsInsertError layout key
  = IndexedRowsInsertInvalidRowId !RowIdError
  | IndexedRowsInsertIdsExhausted
  | IndexedRowsInsertCursorInvalid !RowIdError
  | IndexedRowsInsertDuplicateRowId !Int
  | IndexedRowsInsertDuplicateKey !key
  | IndexedRowsInsertBindingFailed !Int !key !(IndexedRowBindingError layout key)
  deriving stock (Eq, Ord, Show)

type IndexedRowsCursorError :: Type
data IndexedRowsCursorError
  = IndexedRowsCursorInvalid !RowIdError
  | IndexedRowsCursorNotAfterRow !RowId !Int
  | IndexedRowsStoredRowIdInvalid !Int !RowIdError
  deriving stock (Eq, Ord, Show)

type IndexedRowsDeleteError :: Type -> Type -> Type
data IndexedRowsDeleteError layout key
  = IndexedRowsDeleteMissingKey !key
  | IndexedRowsDeleteInvalidRowId !Int
  | IndexedRowsDeleteBindingFailed !Int !key !(IndexedRowBindingError layout key)
  deriving stock (Eq, Ord, Show)

type IndexedRowsPayloadError :: Type -> Type
data IndexedRowsPayloadError key
  = IndexedRowsPayloadMissingKey !key
  deriving stock (Eq, Ord, Show)

indexedRowFormat ::
  (key -> Int) ->
  (layout -> Int) ->
  (forall acc. layout -> key -> (Int -> Int -> acc -> acc) -> acc -> Either (IndexedRowBindingError layout key) acc) ->
  IndexedRowFormat layout key
indexedRowFormat keyWidth layoutWidth foldBindings =
  IndexedRowFormat
    { irfWidthRaw = keyWidth,
      irfLayoutWidthRaw = layoutWidth,
      irfFoldBindings = foldBindings
    }
{-# INLINE indexedRowFormat #-}

indexedRowFormatWidth :: IndexedRowFormat layout key -> key -> Int
indexedRowFormatWidth format =
  irfWidthRaw format
{-# INLINE indexedRowFormatWidth #-}

indexedRowFormatLayoutWidth :: IndexedRowFormat layout key -> layout -> Int
indexedRowFormatLayoutWidth format =
  irfLayoutWidthRaw format
{-# INLINE indexedRowFormatLayoutWidth #-}

foldIndexedRowFormatBindings ::
  IndexedRowFormat layout key ->
  layout ->
  key ->
  (Int -> Int -> acc -> acc) ->
  acc ->
  Either (IndexedRowBindingError layout key) acc
foldIndexedRowFormatBindings format =
  irfFoldBindings format
{-# INLINE foldIndexedRowFormatBindings #-}

emptyIndexedRows :: (layout -> IntMap Int) -> layout -> IndexedRows layout key payload
emptyIndexedRows layoutColumnIndex schema =
  IndexedRows
    { irLayout = schema,
      irColIndex = layoutColumnIndex schema,
      irLiveRows = IntSet.empty,
      irKeyByRowId = IntMap.empty,
      irIdByKey = Map.empty,
      irPayloadByKey = Map.empty,
      irValueIx = IntMap.empty,
      irNextRowId = RowIdAvailable initialRowId
    }
{-# INLINE emptyIndexedRows #-}

indexedRowsLayout :: IndexedRows layout key payload -> layout
indexedRowsLayout =
  irLayout
{-# INLINE indexedRowsLayout #-}

indexedRowsColumnIndex :: IndexedRows layout key payload -> IntMap Int
indexedRowsColumnIndex =
  irColIndex
{-# INLINE indexedRowsColumnIndex #-}

indexedRowsLiveRows :: IndexedRows layout key payload -> IntSet
indexedRowsLiveRows =
  irLiveRows
{-# INLINE indexedRowsLiveRows #-}

indexedRowsLiveRowSet :: IndexedRows layout key payload -> RowSet
indexedRowsLiveRowSet rows =
  rowSetFromIntSetWithUniverse
    (indexedRowsRowUniverse rows)
    (irLiveRows rows)
{-# INLINE indexedRowsLiveRowSet #-}

indexedRowsKeyByRowId :: IndexedRows layout key payload -> IntMap key
indexedRowsKeyByRowId =
  irKeyByRowId
{-# INLINE indexedRowsKeyByRowId #-}

indexedRowsIdByKey :: IndexedRows layout key payload -> Map key Int
indexedRowsIdByKey =
  irIdByKey
{-# INLINE indexedRowsIdByKey #-}

indexedRowsPayloadMap :: IndexedRows layout key payload -> Map key payload
indexedRowsPayloadMap =
  irPayloadByKey
{-# INLINE indexedRowsPayloadMap #-}

indexedRowsPayloadByRowId ::
  Ord key =>
  IndexedRows layout key payload ->
  IntMap payload
indexedRowsPayloadByRowId rows =
  IntMap.mapMaybe
    (`Map.lookup` irPayloadByKey rows)
    (irKeyByRowId rows)
{-# INLINE indexedRowsPayloadByRowId #-}

indexedRowsRowById ::
  Ord key =>
  IndexedRows layout key payload ->
  IntMap (key, payload)
indexedRowsRowById rows =
  IntMap.mapMaybe withPayload (irKeyByRowId rows)
  where
    withPayload key = do
      payload <- Map.lookup key (irPayloadByKey rows)
      pure (key, payload)
{-# INLINE indexedRowsRowById #-}

indexedRowsFromPayloadMap ::
  IndexedRowFormat layout key ->
  (layout -> IntMap Int) ->
  layout ->
  Map key payload ->
  Either (NonEmpty (IndexedRowsBuildError layout key)) (IndexedRows layout key payload)
indexedRowsFromPayloadMap format layoutColumnIndex schema payloadByKey =
  indexedRowsFromDistinctAscList
    format
    layoutColumnIndex
    schema
    payloadByKey
    (Map.toAscList payloadByKey)
{-# INLINE indexedRowsFromPayloadMap #-}

indexedRowsFromPayloadMapWithValueIndex ::
  IndexedRowFormat layout key ->
  (layout -> IntMap Int) ->
  layout ->
  Map key payload ->
  IntMap (IntMap RowIdSet) ->
  Either (NonEmpty (IndexedRowsBuildError layout key)) (IndexedRows layout key payload)
indexedRowsFromPayloadMapWithValueIndex format layoutColumnIndex schema payloadByKey valueIndex =
  case indexedRowsFromPayloadMap format layoutColumnIndex schema payloadByKey of
    Right rows
      | indexedRowsValueIndex rows == valueIndex ->
          Right rows
      | otherwise ->
          Left (IndexedRowsBuildValueIndexMismatch :| [])
    Left errors ->
      Left errors
{-# INLINE indexedRowsFromPayloadMapWithValueIndex #-}

indexedRowsFromDistinctAscList ::
  IndexedRowFormat layout key ->
  (layout -> IntMap Int) ->
  layout ->
  Map key payload ->
  [(key, payload)] ->
  Either (NonEmpty (IndexedRowsBuildError layout key)) (IndexedRows layout key payload)
indexedRowsFromDistinctAscList format layoutColumnIndex schema payloadByKey rowsAsc =
  do
    valueIndex <- indexedRowsValueIndexFromDistinctAscRows format schema keyedRows
    nextRowId <-
      first
        (NonEmpty.singleton . IndexedRowsBuildCursorInvalid)
        (rowIdCursorFromExclusiveUniverse rowCount)
    Right
      IndexedRows
        { irLayout = schema,
          irColIndex = layoutColumnIndex schema,
          irLiveRows = IntSet.fromDistinctAscList [0 .. rowCount - 1],
          irKeyByRowId = IntMap.fromDistinctAscList (fmap keyByRowId keyedRows),
          irIdByKey = Map.fromDistinctAscList (fmap idByKey keyedRows),
          irPayloadByKey = payloadByKey,
          irValueIx = valueIndex,
          irNextRowId = nextRowId
        }
  where
    keyedRows =
      zip [0 ..] rowsAsc
    rowCount =
      length rowsAsc
    keyByRowId :: (Int, (rowKey, rowPayload)) -> (Int, rowKey)
    keyByRowId (rowId, (key, _payload)) =
      (rowId, key)
    idByKey :: (Int, (rowKey, rowPayload)) -> (rowKey, Int)
    idByKey (rowId, (key, _payload)) =
      (key, rowId)
{-# INLINE indexedRowsFromDistinctAscList #-}

indexedRowsValueIndexFromDistinctAscRows ::
  IndexedRowFormat layout key ->
  layout ->
  [(Int, (key, payload))] ->
  Either (NonEmpty (IndexedRowsBuildError layout key)) (IntMap (IntMap RowIdSet))
indexedRowsValueIndexFromDistinctAscRows format schema =
  finalizeBuildState . foldl' insertRow (Right IntMap.empty)
  where
    insertRow state (rowId, (key, _payload)) =
      let expectedWidth =
            indexedRowFormatLayoutWidth format schema
          actualWidth =
            indexedRowFormatWidth format key
       in if actualWidth /= expectedWidth
            then
              addBuildError
                (IndexedRowsBuildBindingFailed rowId key (IndexedRowWidthMismatch key expectedWidth actualWidth))
                state
            else
              case irfFoldBindings format schema key (insertValueRowIdsBucket rowId) (either (const IntMap.empty) id state) of
                Left bindingError ->
                  addBuildError (IndexedRowsBuildBindingFailed rowId key bindingError) state
                Right valueIndex ->
                  case state of
                    Left errors ->
                      Left errors
                    Right _ ->
                      Right valueIndex

    finalizeBuildState ::
      Either (NonEmpty (IndexedRowsBuildError layout key)) (IntMap (IntMap [Int])) ->
      Either (NonEmpty (IndexedRowsBuildError layout key)) (IntMap (IntMap RowIdSet))
    finalizeBuildState (Left errors) =
      Left errors
    finalizeBuildState (Right valueIndex) =
      Right (finalizeValueIndex valueIndex)
{-# INLINE indexedRowsValueIndexFromDistinctAscRows #-}

addBuildError ::
  IndexedRowsBuildError layout key ->
  Either (NonEmpty (IndexedRowsBuildError layout key)) value ->
  Either (NonEmpty (IndexedRowsBuildError layout key)) value
addBuildError errorValue state =
  Left
    ( case state of
        Left errors ->
          errors <> NonEmpty.singleton errorValue
        Right _ ->
          errorValue :| []
    )
{-# INLINE addBuildError #-}

finalizeValueIndex :: IntMap (IntMap [Int]) -> IntMap (IntMap RowIdSet)
finalizeValueIndex =
  fmap (fmap (rowIdSetFromIntSetCanonical . IntSet.fromAscList . reverse))
{-# INLINE finalizeValueIndex #-}

insertValueRowIdsBucket ::
  Int ->
  Int ->
  Int ->
  IntMap (IntMap [Int]) ->
  IntMap (IntMap [Int])
insertValueRowIdsBucket rowId slotKey repKey =
  IntMap.alter (Just . insertRepBucket . maybe IntMap.empty id) slotKey
  where
    insertRepBucket =
      IntMap.alter (Just . (rowId :) . maybe [] id) repKey
{-# INLINE insertValueRowIdsBucket #-}

indexedRowsValueIndex :: IndexedRows layout key payload -> IntMap (IntMap RowIdSet)
indexedRowsValueIndex =
  irValueIx
{-# INLINE indexedRowsValueIndex #-}

indexedRowsNextRowId :: IndexedRows layout key payload -> RowIdCursor
indexedRowsNextRowId =
  irNextRowId
{-# INLINE indexedRowsNextRowId #-}

indexedRowsRowUniverse :: IndexedRows layout key payload -> Int
indexedRowsRowUniverse =
  rowIdCursorExclusiveUniverse . irNextRowId
{-# INLINE indexedRowsRowUniverse #-}

validateIndexedRowsCursor ::
  IndexedRows layout key payload ->
  Either IndexedRowsCursorError ()
validateIndexedRowsCursor rows =
  case irNextRowId rows of
    RowIdsExhausted ->
      traverse_ validateStoredRowId storedRowIds
    RowIdAvailable rawNextId -> do
      nextId <- first IndexedRowsCursorInvalid (mkRowId (rowIdInt rawNextId))
      traverse_ (ensureBeforeCursor nextId) storedRowIds
  where
    storedRowIds =
      IntMap.keys (irKeyByRowId rows)

    validateStoredRowId rowKey =
      () <$ first (IndexedRowsStoredRowIdInvalid rowKey) (mkRowId rowKey)

    ensureBeforeCursor nextId rowKey = do
      validateStoredRowId rowKey
      if rowKey < rowIdInt nextId
        then Right ()
        else Left (IndexedRowsCursorNotAfterRow nextId rowKey)
{-# INLINE validateIndexedRowsCursor #-}

indexedRowsSize :: IndexedRows layout key payload -> Int
indexedRowsSize =
  IntSet.size . irLiveRows
{-# INLINE indexedRowsSize #-}

indexedRowsLookupId ::
  Ord key =>
  key ->
  IndexedRows layout key payload ->
  Maybe RowId
indexedRowsLookupId key rows =
  Map.lookup key (irIdByKey rows) >>= rowIdFromInt
{-# INLINE indexedRowsLookupId #-}

rowIdFromInt :: Int -> Maybe RowId
rowIdFromInt =
  either (const Nothing) Just . mkRowId
{-# INLINE rowIdFromInt #-}

indexedRowsKeyAt ::
  RowId ->
  IndexedRows layout key payload ->
  Maybe key
indexedRowsKeyAt rowId =
  IntMap.lookup (rowIdInt rowId) . irKeyByRowId
{-# INLINE indexedRowsKeyAt #-}

indexedRowsLookupPayload ::
  Ord key =>
  key ->
  IndexedRows layout key payload ->
  Maybe payload
indexedRowsLookupPayload key =
  Map.lookup key . irPayloadByKey
{-# INLINE indexedRowsLookupPayload #-}

indexedRowsPayloadAtRowId ::
  Ord key =>
  RowId ->
  IndexedRows layout key payload ->
  Maybe payload
indexedRowsPayloadAtRowId rowId rows = do
  key <- indexedRowsKeyAt rowId rows
  indexedRowsLookupPayload key rows
{-# INLINE indexedRowsPayloadAtRowId #-}

indexedRowsRowAt ::
  Ord key =>
  RowId ->
  IndexedRows layout key payload ->
  Maybe (key, payload)
indexedRowsRowAt rowId rows = do
  key <- indexedRowsKeyAt rowId rows
  payload <- indexedRowsLookupPayload key rows
  pure (key, payload)
{-# INLINE indexedRowsRowAt #-}

type PinBucketScan :: Type
data PinBucketScan
  = PinBucketScanNoRelevant
  | PinBucketScanMissing
  | PinBucketScanSome !RowIdSet ![RowIdSet]
  deriving stock (Eq, Show)

indexedRowsRestrictRowsByPins ::
  IndexedRows layout key payload ->
  RowSet ->
  IntMap Int ->
  RowSet
indexedRowsRestrictRowsByPins rows active pins =
  case indexedRowsPinnedBuckets rows pins of
    PinBucketScanNoRelevant ->
      active
    PinBucketScanMissing ->
      emptyRowSet
    PinBucketScanSome firstBucket restBuckets ->
      case rowSetIntersectionWithRowIdSetChanged
          (intersectPinBuckets firstBucket restBuckets)
          active of
        RowSetRestrictionEmpty ->
          emptyRowSet
        RowSetRestrictionUnchanged ->
          active
        RowSetRestrictionChanged restricted ->
          restricted
{-# INLINE indexedRowsRestrictRowsByPins #-}

indexedRowsRestrictLiveRowsByPins ::
  IndexedRows layout key payload ->
  IntMap Int ->
  RowSet
indexedRowsRestrictLiveRowsByPins rows pins =
  case indexedRowsPinnedBuckets rows pins of
    PinBucketScanNoRelevant ->
      indexedRowsLiveRowSet rows
    PinBucketScanMissing ->
      emptyRowSet
    PinBucketScanSome firstBucket restBuckets ->
      rowSetFromIntSetWithUniverse
        (indexedRowsRowUniverse rows)
        ( rowIdSetIntersectionWithIntSet
            (intersectPinBuckets firstBucket restBuckets)
            (irLiveRows rows)
        )
{-# INLINE indexedRowsRestrictLiveRowsByPins #-}

indexedRowsPinnedBuckets ::
  IndexedRows layout key payload ->
  IntMap Int ->
  PinBucketScan
indexedRowsPinnedBuckets rows =
  IntMap.foldlWithKey' step PinBucketScanNoRelevant
  where
    step scan slotKey repKey =
      case scan of
        PinBucketScanMissing ->
          PinBucketScanMissing
        _
          | IntMap.notMember slotKey (irColIndex rows) ->
              scan
          | otherwise ->
              case IntMap.lookup slotKey (irValueIx rows) >>= IntMap.lookup repKey of
                Nothing ->
                  PinBucketScanMissing
                Just bucket ->
                  insertPinBucket bucket scan
{-# INLINE indexedRowsPinnedBuckets #-}

insertPinBucket ::
  RowIdSet ->
  PinBucketScan ->
  PinBucketScan
insertPinBucket bucket scan =
  case scan of
    PinBucketScanNoRelevant ->
      PinBucketScanSome bucket []
    PinBucketScanMissing ->
      PinBucketScanMissing
    PinBucketScanSome best rest
      | rowIdSetSize bucket < rowIdSetSize best ->
          PinBucketScanSome bucket (best : rest)
      | otherwise ->
          PinBucketScanSome best (bucket : rest)
{-# INLINE insertPinBucket #-}

intersectPinBuckets ::
  RowIdSet ->
  [RowIdSet] ->
  RowIdSet
intersectPinBuckets firstBucket =
  foldl' step firstBucket
  where
    step acc bucket
      | rowIdSetNull acc =
          acc
      | otherwise =
          rowIdSetIntersection acc bucket
{-# INLINE intersectPinBuckets #-}

indexedRowsInsertFresh ::
  Ord key =>
  IndexedRowFormat layout key ->
  key ->
  payload ->
  IndexedRows layout key payload ->
  Either (IndexedRowsInsertError layout key) (RowId, IndexedRows layout key payload)
indexedRowsInsertFresh format key payload rows =
  case irNextRowId rows of
    RowIdsExhausted ->
      Left IndexedRowsInsertIdsExhausted
    RowIdAvailable rawRowId -> do
      rowId <- first IndexedRowsInsertCursorInvalid (mkRowId (rowIdInt rawRowId))
      (rowId,) <$> indexedRowsInsertWithId format rowId key payload rows
{-# INLINE indexedRowsInsertFresh #-}

indexedRowsInsertWithId ::
  Ord key =>
  IndexedRowFormat layout key ->
  RowId ->
  key ->
  payload ->
  IndexedRows layout key payload ->
  Either (IndexedRowsInsertError layout key) (IndexedRows layout key payload)
indexedRowsInsertWithId format rowId key payload rows
  | Left idError <- mkRowId rowKey =
      Left (IndexedRowsInsertInvalidRowId idError)
  | indexedRowFormatWidth format key /= indexedRowFormatLayoutWidth format (irLayout rows) =
      Left
        ( IndexedRowsInsertBindingFailed
            rowKey
            key
            ( IndexedRowWidthMismatch
                key
                (indexedRowFormatLayoutWidth format (irLayout rows))
                (indexedRowFormatWidth format key)
            )
        )
  | IntMap.member rowKey (irKeyByRowId rows) =
      Left (IndexedRowsInsertDuplicateRowId rowKey)
  | Map.member key (irIdByKey rows) =
      Left (IndexedRowsInsertDuplicateKey key)
  | otherwise = do
      nextRowId <- advanceIndexedRowsCursor rowId (irNextRowId rows)
      case insertValueBucketsForKey format rowId key rows of
        Left bindingError ->
          Left (IndexedRowsInsertBindingFailed rowKey key bindingError)
        Right valueIx ->
          Right
            rows
              { irLiveRows = IntSet.insert rowKey (irLiveRows rows),
                irKeyByRowId = IntMap.insert rowKey key (irKeyByRowId rows),
                irIdByKey = Map.insert key rowKey (irIdByKey rows),
                irPayloadByKey = Map.insert key payload (irPayloadByKey rows),
                irValueIx = valueIx,
                irNextRowId = nextRowId
              }
  where
    !rowKey =
      rowIdInt rowId
{-# INLINE indexedRowsInsertWithId #-}

advanceIndexedRowsCursor ::
  RowId ->
  RowIdCursor ->
  Either (IndexedRowsInsertError layout key) RowIdCursor
advanceIndexedRowsCursor insertedId cursor =
  case cursor of
    RowIdsExhausted ->
      Right RowIdsExhausted
    RowIdAvailable rawNextId -> do
      nextId <- first IndexedRowsInsertCursorInvalid (mkRowId (rowIdInt rawNextId))
      if rowIdInt insertedId < rowIdInt nextId
        then Right cursor
        else
          if rowIdInt insertedId == maxBound - 1
            then Right RowIdsExhausted
            else
              RowIdAvailable
                <$> first IndexedRowsInsertCursorInvalid (mkRowId (rowIdInt insertedId + 1))
{-# INLINE advanceIndexedRowsCursor #-}

insertValueBucket ::
  RowId ->
  Int ->
  Int ->
  IntMap (IntMap RowIdSet) ->
  IntMap (IntMap RowIdSet)
insertValueBucket rowId slotKey repKey =
  IntMap.insertWith
    (IntMap.unionWith rowIdSetUnion)
    slotKey
    (IntMap.singleton repKey (singletonRowIdSet rowId))
{-# INLINE insertValueBucket #-}

deleteValueBucket ::
  RowId ->
  Int ->
  Int ->
  IntMap (IntMap RowIdSet) ->
  IntMap (IntMap RowIdSet)
deleteValueBucket rowId slotKey repKey =
  IntMap.update
    ( \byValue ->
        let byValue' =
              IntMap.update
                ( \bucket ->
                    let bucket' =
                          rowIdSetDelete rowId bucket
                     in if rowIdSetNull bucket'
                          then Nothing
                          else Just bucket'
                )
                repKey
                byValue
         in if IntMap.null byValue'
              then Nothing
              else Just byValue'
    )
    slotKey
{-# INLINE deleteValueBucket #-}

indexedRowsDelete ::
  Ord key =>
  IndexedRowFormat layout key ->
  key ->
  IndexedRows layout key payload ->
  Either (IndexedRowsDeleteError layout key) (RowId, payload, IndexedRows layout key payload)
indexedRowsDelete format key rows =
  case (Map.lookup key (irIdByKey rows), Map.lookup key (irPayloadByKey rows)) of
    (Just rowKey, Just payload) ->
      case mkRowId rowKey of
        Left _ ->
          Left (IndexedRowsDeleteInvalidRowId rowKey)
        Right rowId ->
          let rowsWithoutKey =
                rows
                  { irLiveRows = IntSet.delete rowKey (irLiveRows rows),
                    irKeyByRowId = IntMap.delete rowKey (irKeyByRowId rows),
                    irIdByKey = Map.delete key (irIdByKey rows),
                    irPayloadByKey = Map.delete key (irPayloadByKey rows)
                  }
           in case deleteValueBucketsForKey format rowId key rows of
                Left bindingError ->
                  Left (IndexedRowsDeleteBindingFailed rowKey key bindingError)
                Right valueIx ->
                  Right (rowId, payload, rowsWithoutKey {irValueIx = valueIx})
    _ ->
      Left (IndexedRowsDeleteMissingKey key)
{-# INLINE indexedRowsDelete #-}

indexedRowsSetPayload ::
  Ord key =>
  key ->
  payload ->
  IndexedRows layout key payload ->
  Either (IndexedRowsPayloadError key) (IndexedRows layout key payload)
indexedRowsSetPayload key payload rows
  | Map.member key (irIdByKey rows) =
      Right rows {irPayloadByKey = Map.insert key payload (irPayloadByKey rows)}
  | otherwise =
      Left (IndexedRowsPayloadMissingKey key)
{-# INLINE indexedRowsSetPayload #-}

indexedRowsMapPayloadEither ::
  (payload -> Either err payload') ->
  IndexedRows layout key payload ->
  Either err (IndexedRows layout key payload')
indexedRowsMapPayloadEither transform rows = do
  payloads <- traverse transform (irPayloadByKey rows)
  pure
    IndexedRows
      { irLayout = irLayout rows,
        irColIndex = irColIndex rows,
        irLiveRows = irLiveRows rows,
        irKeyByRowId = irKeyByRowId rows,
        irIdByKey = irIdByKey rows,
        irPayloadByKey = payloads,
        irValueIx = irValueIx rows,
        irNextRowId = irNextRowId rows
      }
{-# INLINE indexedRowsMapPayloadEither #-}

indexedRowsRebuildValueIndex ::
  Ord key =>
  IndexedRowFormat layout key ->
  IndexedRows layout key payload ->
  Either (NonEmpty (IndexedRowsBuildError layout key)) (IndexedRows layout key payload)
indexedRowsRebuildValueIndex format rows =
  case indexedRowsValueIndexFromDistinctAscRows
    format
    (irLayout rows)
    [ (rowId, (key, payload))
    | (rowId, key) <- IntMap.toAscList (irKeyByRowId rows),
      payload <- maybe [] pure (Map.lookup key (irPayloadByKey rows))
    ] of
    Left errors ->
      Left errors
    Right valueIx ->
      Right rows {irValueIx = valueIx}
{-# INLINE indexedRowsRebuildValueIndex #-}

insertValueBucketsForKey ::
  IndexedRowFormat layout key ->
  RowId ->
  key ->
  IndexedRows layout key payload ->
  Either (IndexedRowBindingError layout key) (IntMap (IntMap RowIdSet))
insertValueBucketsForKey format rowId key rows =
  foldIndexedRowBindings
    format
    rows
    key
    (\slotKey repKey valueIx -> insertValueBucket rowId slotKey repKey valueIx)
    (irValueIx rows)
{-# INLINE insertValueBucketsForKey #-}

deleteValueBucketsForKey ::
  IndexedRowFormat layout key ->
  RowId ->
  key ->
  IndexedRows layout key payload ->
  Either (IndexedRowBindingError layout key) (IntMap (IntMap RowIdSet))
deleteValueBucketsForKey format rowId key rows =
  foldIndexedRowBindings
    format
    rows
    key
    (\slotKey repKey valueIx -> deleteValueBucket rowId slotKey repKey valueIx)
    (irValueIx rows)
{-# INLINE deleteValueBucketsForKey #-}

foldIndexedRowBindings ::
  IndexedRowFormat layout key ->
  IndexedRows layout key payload ->
  key ->
  (Int -> Int -> acc -> acc) ->
  acc ->
  Either (IndexedRowBindingError layout key) acc
foldIndexedRowBindings format rows key step initial =
  irfFoldBindings format (irLayout rows) key step initial
{-# INLINE foldIndexedRowBindings #-}
