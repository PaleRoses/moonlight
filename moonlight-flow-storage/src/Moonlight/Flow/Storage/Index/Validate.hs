{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Moonlight.Flow.Storage.Index.Validate
  ( ValidationPath (..),
    IndexedRowsBucketPath (..),
    SharedBucketError (..),
    indexedRowsBucketErrors,
    indexedRowsActiveBucketErrors,
    validateIndexedRowsBuckets,
    validateIndexedRowsBucketsForRows,
    BucketPath (..),
    IndexValidationError (..),
    validateRelationIndex,
    validateSeparatorIndexForRelation,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Moonlight.Differential.Index.IndexedRows
  ( IndexedRowFormat,
    IndexedRows,
    foldIndexedRowFormatBindings,
    indexedRowsColumnIndex,
    indexedRowsIdByKey,
    indexedRowsKeyByRowId,
    indexedRowsLiveRowSet,
    indexedRowsLiveRows,
    indexedRowsPayloadByRowId,
    indexedRowsLayout,
    indexedRowsValueIndex,
  )
import Moonlight.Flow.Storage.Index.TupleFormat
  ( tupleKeyIndexedFormat,
  )
import Moonlight.Differential.Index.RowId
  ( RowId,
    mkRowId,
    rowIdInt,
  )
import Moonlight.Differential.Index.RowSet
  ( RowSet,
    rowSetToIntSet,
  )
import Moonlight.Differential.Row.Block
  ( RowLayout,
  )
import Moonlight.Flow.Storage.Separator
import Moonlight.Flow.Storage.Relation
import Moonlight.Differential.Row.Tuple
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Differential.Index.RowIdSet
  ( RowIdSet,
    RowIdSetError,
    rowIdSetIntersectionWithIntSet,
    rowIdSetToIntSet,
    validateRowIdSet,
  )
import Moonlight.Delta.Signed
  ( multiplicityValue
  )


type ValidationPath :: Type
data ValidationPath
  = RelationPath
  | DensePath {-# UNPACK #-} !Int
  | FactorPath {-# UNPACK #-} !Int
  | SeparatorPath !SeparatorTupleKey
  deriving stock (Eq, Ord, Show)

type IndexedRowsBucketPath :: Type
data IndexedRowsBucketPath
  = IndexedRowsValueBucket !ValidationPath !SlotId !Int
  deriving stock (Eq, Ord, Show)

type SharedBucketError :: Type
data SharedBucketError
  = SharedRowIdSetBucketInvalid !IndexedRowsBucketPath !RowIdSetError
  | SharedBucketDenotationMismatch !IndexedRowsBucketPath !IntSet !IntSet
  | SharedUnknownIndexedSlot !IndexedRowsBucketPath !Int
  deriving stock (Eq, Show)

type BucketPath :: Type
data BucketPath
  = RelationValueBucket !SlotId !Int
  | SeparatorKeyBucket !SeparatorTupleKey
  deriving stock (Eq, Ord, Show)

type IndexValidationError :: Type
data IndexValidationError
  = RowIdSetBucketInvalid !BucketPath !RowIdSetError
  | BucketDenotationMismatch !BucketPath !IntSet !IntSet
  | UnknownIndexedSlot !BucketPath !Int
  | RelationLiveRowsDoNotMatchMultiplicities !RowSet !IntSet
  | RelationLiveRowMissing {-# UNPACK #-} !Int
  | RelationInverseMissing {-# UNPACK #-} !Int !RowTupleKey
  | RelationReverseInverseMismatch !RowTupleKey !RowId !(Maybe RowTupleKey)
  | RelationRowWidthMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | SeparatorRowToKeyMismatch {-# UNPACK #-} !Int !(Maybe SeparatorTupleKey) !(Maybe SeparatorTupleKey)
  deriving stock (Eq, Show)

finish :: [IndexValidationError] -> Either [IndexValidationError] ()
finish [] = Right ()
finish errors = Left errors
{-# INLINE finish #-}

rowIdFromRaw :: Int -> Maybe RowId
rowIdFromRaw =
  either (const Nothing) Just . mkRowId
{-# INLINE rowIdFromRaw #-}

rowForRawId :: Relation -> Int -> Maybe RowTupleKey
rowForRawId relation rowKey =
  rowIdFromRaw rowKey >>= rowForId relation
{-# INLINE rowForRawId #-}

sharedValidateBucket :: IndexedRowsBucketPath -> RowIdSet -> [SharedBucketError]
sharedValidateBucket path bucket =
  case validateRowIdSet bucket of
    Right () -> []
    Left err -> [SharedRowIdSetBucketInvalid path err]
{-# INLINE sharedValidateBucket #-}

sharedBucketMismatch :: IndexedRowsBucketPath -> IntSet -> IntSet -> [SharedBucketError]
sharedBucketMismatch path actual expected
  | actual == expected = []
  | otherwise = [SharedBucketDenotationMismatch path actual expected]
{-# INLINE sharedBucketMismatch #-}

validateBucket :: BucketPath -> RowIdSet -> [IndexValidationError]
validateBucket path bucket =
  case validateRowIdSet bucket of
    Right () -> []
    Left err -> [RowIdSetBucketInvalid path err]
{-# INLINE validateBucket #-}

bucketMismatch :: BucketPath -> IntSet -> IntSet -> [IndexValidationError]
bucketMismatch path actual expected
  | actual == expected = []
  | otherwise = [BucketDenotationMismatch path actual expected]
{-# INLINE bucketMismatch #-}

finishShared :: [SharedBucketError] -> Either [SharedBucketError] ()
finishShared [] = Right ()
finishShared errors = Left errors
{-# INLINE finishShared #-}

validateIndexedRowsBuckets ::
  IndexedRowFormat RowLayout key ->
  ValidationPath ->
  IndexedRows RowLayout key payload ->
  Either [SharedBucketError] ()
validateIndexedRowsBuckets format path rows =
  finishShared (indexedRowsBucketErrors format path rows)
{-# INLINE validateIndexedRowsBuckets #-}

validateIndexedRowsBucketsForRows ::
  IndexedRowFormat RowLayout key ->
  ValidationPath ->
  IntSet ->
  IndexedRows RowLayout key payload ->
  Either [SharedBucketError] ()
validateIndexedRowsBucketsForRows format path activeRows rows =
  finishShared (indexedRowsActiveBucketErrors format path activeRows rows)
{-# INLINE validateIndexedRowsBucketsForRows #-}

indexedRowsBucketErrors ::
  IndexedRowFormat RowLayout key ->
  ValidationPath ->
  IndexedRows RowLayout key payload ->
  [SharedBucketError]
indexedRowsBucketErrors format path rows =
  indexedRowsBucketErrorsWithActual
    format
    path
    rowIdSetToIntSet
    (indexedRowsLiveRows rows)
    rows
{-# INLINE indexedRowsBucketErrors #-}

indexedRowsActiveBucketErrors ::
  IndexedRowFormat RowLayout key ->
  ValidationPath ->
  IntSet ->
  IndexedRows RowLayout key payload ->
  [SharedBucketError]
indexedRowsActiveBucketErrors format path activeRows rows =
  indexedRowsBucketErrorsWithActual
    format
    path
    (`rowIdSetIntersectionWithIntSet` activeRows)
    activeRows
    rows
{-# INLINE indexedRowsActiveBucketErrors #-}

indexedRowsBucketErrorsWithActual ::
  IndexedRowFormat RowLayout key ->
  ValidationPath ->
  (RowIdSet -> IntSet) ->
  IntSet ->
  IndexedRows RowLayout key payload ->
  [SharedBucketError]
indexedRowsBucketErrorsWithActual format validationPath actualBucketRows activeRows rows =
  concat
    [ case IntMap.lookup slotKey (indexedRowsColumnIndex rows) of
        Nothing ->
          concat
            [ sharedValidateBucket path bucket
                <> [SharedUnknownIndexedSlot path slotKey]
              | (repKey, bucket) <- IntMap.toList byRep,
                let path = IndexedRowsValueBucket validationPath (mkSlotId slotKey) repKey
            ]
        Just _columnIx ->
          concat
            [ let path = IndexedRowsValueBucket validationPath (mkSlotId slotKey) repKey
                  actual = actualBucketRows bucket
                  expected = expectedIndexedRowsBucket format rows activeRows (mkSlotId slotKey) repKey
               in sharedValidateBucket path bucket <> sharedBucketMismatch path actual expected
              | (repKey, bucket) <- IntMap.toList byRep
            ]
      | (slotKey, byRep) <- IntMap.toList (indexedRowsValueIndex rows)
    ]
{-# INLINE indexedRowsBucketErrorsWithActual #-}

expectedIndexedRowsBucket ::
  IndexedRowFormat RowLayout key ->
  IndexedRows RowLayout key payload ->
  IntSet ->
  SlotId ->
  Int ->
  IntSet
expectedIndexedRowsBucket format rows activeRows sid repKey =
  IntSet.foldl'
    (\acc rowKey ->
      case IntMap.lookup rowKey (indexedRowsKeyByRowId rows) of
        Nothing -> acc
        Just row ->
          if indexedRowHasBinding format (indexedRowsLayout rows) sid repKey row
            then IntSet.insert rowKey acc
            else acc
    )
    IntSet.empty
    activeRows
{-# INLINE expectedIndexedRowsBucket #-}

indexedRowHasBinding ::
  IndexedRowFormat RowLayout key ->
  RowLayout ->
  SlotId ->
  Int ->
  key ->
  Bool
indexedRowHasBinding format schema sid repKey row =
  either (const False) id (foldIndexedRowFormatBindings format schema row step False)
  where
    step slotKey value matched =
      matched || (slotKey == slotIdKey sid && value == repKey)
{-# INLINE indexedRowHasBinding #-}

sharedBucketErrorToRelation :: SharedBucketError -> IndexValidationError
sharedBucketErrorToRelation = \case
  SharedRowIdSetBucketInvalid path err ->
    RowIdSetBucketInvalid (relationBucketPath path) err
  SharedBucketDenotationMismatch path actual expected ->
    BucketDenotationMismatch (relationBucketPath path) actual expected
  SharedUnknownIndexedSlot path slotKey ->
    UnknownIndexedSlot (relationBucketPath path) slotKey
{-# INLINE sharedBucketErrorToRelation #-}

relationBucketPath :: IndexedRowsBucketPath -> BucketPath
relationBucketPath (IndexedRowsValueBucket _ sid repKey) =
  RelationValueBucket sid repKey
{-# INLINE relationBucketPath #-}

validateRelationIndex :: Relation -> Either [IndexValidationError] ()
validateRelationIndex pr =
  finish $
    relationLiveMultiplicityErrors
      <> relationRowShapeErrors
      <> relationInverseErrors
      <> relationBucketErrors
  where
    !expectedWidth = Vector.length (indexedRowsLayout (relRows pr))

    liveFromCounts :: IntSet
    liveFromCounts =
      IntMap.keysSet
        ( IntMap.filter
            ((> 0) . multiplicityValue)
            (indexedRowsPayloadByRowId (relRows pr))
        )

    relationLiveMultiplicityErrors :: [IndexValidationError]
    relationLiveMultiplicityErrors =
      [ RelationLiveRowsDoNotMatchMultiplicities relationLiveRows liveFromCounts
        | rowSetToIntSet relationLiveRows /= liveFromCounts
      ]

    relationRowShapeErrors :: [IndexValidationError]
    relationRowShapeErrors =
      IntSet.foldr step [] (rowSetToIntSet relationLiveRows)
      where
        step rowKey acc =
          case rowForRawId pr rowKey of
            Nothing ->
              RelationLiveRowMissing rowKey : acc
            Just row ->
              let !actualWidth = tupleKeyWidth row
               in if actualWidth == expectedWidth
                    then acc
                    else RelationRowWidthMismatch rowKey expectedWidth actualWidth : acc

    relationInverseErrors :: [IndexValidationError]
    relationInverseErrors =
      liveInverseErrors <> reverseInverseErrors
      where
        liveInverseErrors =
          IntSet.foldr step [] (rowSetToIntSet relationLiveRows)
          where
            step rowKey acc =
              case rowForRawId pr rowKey of
                Nothing -> acc
                Just row ->
                  case Map.lookup row relationRowIds of
                    Just rid | rowIdInt rid == rowKey -> acc
                    _ -> RelationInverseMissing rowKey row : acc

        reverseInverseErrors =
          [ RelationReverseInverseMismatch row rid actual
            | (row, rid) <- Map.toList relationRowIds,
              let actual = rowForId pr rid,
              actual /= Just row
          ]

    relationBucketErrors :: [IndexValidationError]
    relationBucketErrors =
      fmap
        sharedBucketErrorToRelation
        (indexedRowsBucketErrors tupleKeyIndexedFormat RelationPath (relRows pr))

    relationLiveRows =
      indexedRowsLiveRowSet (relRows pr)

    relationRowIds =
      Map.mapMaybe rowIdFromRaw (indexedRowsIdByKey (relRows pr))
{-# INLINE validateRelationIndex #-}

validateSeparatorIndexForRelation :: Relation -> SeparatorIndex -> Either [IndexValidationError] ()
validateSeparatorIndexForRelation pr sep =
  finish $
    separatorBucketErrors
      <> separatorRowToKeyErrors
  where
    expectedKeyForRow :: Int -> Maybe SeparatorTupleKey
    expectedKeyForRow rowKey =
      rowIdFromRaw rowKey >>= separatorKeyFromRowId pr (siSlots sep)

    expectedSeparatorBucket :: SeparatorTupleKey -> IntSet
    expectedSeparatorBucket key =
      IntSet.foldl'
        ( \acc rowKey ->
            if expectedKeyForRow rowKey == Just key
              then IntSet.insert rowKey acc
              else acc
        )
        IntSet.empty
        (rowSetToIntSet relationLiveRows)

    separatorBucketErrors :: [IndexValidationError]
    separatorBucketErrors =
      concat
        [ let path = SeparatorKeyBucket key
              actual = rowIdSetToIntSet bucket
              expected = expectedSeparatorBucket key
           in validateBucket path bucket <> bucketMismatch path actual expected
          | (key, bucket) <- Map.toList (siByKey sep)
        ]

    separatorRowToKeyErrors :: [IndexValidationError]
    separatorRowToKeyErrors =
      [ SeparatorRowToKeyMismatch rowKey expected actual
        | rowKey <- IntSet.toAscList rowUniverse,
          let expected = expectedKeyForRow rowKey,
          let actual = IntMap.lookup rowKey (siRowToKey sep),
          expected /= actual
      ]

    rowUniverse :: IntSet
    rowUniverse =
      IntSet.union (rowSetToIntSet relationLiveRows) (IntMap.keysSet (siRowToKey sep))

    relationLiveRows =
      indexedRowsLiveRowSet (relRows pr)
{-# INLINE validateSeparatorIndexForRelation #-}
