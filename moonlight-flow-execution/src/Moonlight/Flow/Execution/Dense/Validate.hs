{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Moonlight.Flow.Execution.Dense.Validate
  ( DenseArrangementIndexValidationError (..),
    validateJoinSourceIndex,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Maybe
  ( isNothing,
  )
import Moonlight.Flow.Execution.Dense.Plan
  ( DenseArrangement,
    DenseArrangementId (..),
    denseArrangementColumnIndex,
    denseArrangementId,
    denseArrangementRows,
    denseArrangementSchema,
    denseArrangementValueAt,
    denseArrangementValueIndex,
  )
import Moonlight.Differential.Row.Tuple (RepKey (..))
import Moonlight.Flow.Plan.Query.Core (SlotId, mkSlotId)
import Moonlight.Flow.Storage.Index.Validate
  ( IndexedRowsBucketPath (..),
    SharedBucketError (..),
    ValidationPath (..),
  )
import Moonlight.Differential.Index.RowIdSet
  ( RowIdSet,
    rowIdSetIntersectionWithIntSet,
    validateRowIdSet,
  )
import Moonlight.Differential.Index.RowSet
  ( RowSet,
    rowSetToIntSet,
  )

type DenseArrangementIndexValidationError :: Type
data DenseArrangementIndexValidationError
  = DenseArrangementBucketInvalid !SharedBucketError
  | DenseArrangementMissingValue !IndexedRowsBucketPath {-# UNPACK #-} !Int !SlotId
  deriving stock (Eq, Show)

finish :: [DenseArrangementIndexValidationError] -> Either [DenseArrangementIndexValidationError] ()
finish [] = Right ()
finish errors = Left errors
{-# INLINE finish #-}

rowSetAscList :: RowSet -> [Int]
rowSetAscList =
  IntSet.toAscList . rowSetToIntSet
{-# INLINE rowSetAscList #-}

validateJoinSourceIndex :: DenseArrangement -> Either [DenseArrangementIndexValidationError] ()
validateJoinSourceIndex cursor =
  finish $
    joinSourceRowValueErrors
      <> fmap DenseArrangementBucketInvalid joinSourceBucketErrors
  where
    path =
      DensePath (unDenseArrangementId (denseArrangementId cursor))

    joinSourceRowValueErrors :: [DenseArrangementIndexValidationError]
    joinSourceRowValueErrors =
      [ DenseArrangementMissingValue bucketPath rowKey sid
        | rowKey <- rowSetAscList (denseArrangementRows cursor),
          sid <- denseArrangementSchema cursor,
          isNothing (denseArrangementValueAt cursor sid rowKey),
          let bucketPath = missingValuePath path sid
      ]

    joinSourceBucketErrors :: [SharedBucketError]
    joinSourceBucketErrors =
      arrangementBucketErrors
        cursor
        path
        (rowSetToIntSet (denseArrangementRows cursor))

    missingValuePath validationPath sid =
      IndexedRowsValueBucket validationPath sid (unRepKey missingRepKey)

    missingRepKey =
      RepKey (-1)
{-# INLINE validateJoinSourceIndex #-}


arrangementBucketErrors ::
  DenseArrangement ->
  ValidationPath ->
  IntSet ->
  [SharedBucketError]
arrangementBucketErrors cursor validationPath activeRows =
  concat
    [ case IntMap.lookup slotKey (denseArrangementColumnIndex cursor) of
        Nothing ->
          concat
            [ validateBucket path bucket
                <> [SharedUnknownIndexedSlot path slotKey]
              | (repKey, bucket) <- IntMap.toList byRep,
                let path = IndexedRowsValueBucket validationPath (mkSlotId slotKey) repKey
            ]
        Just _columnIx ->
          concat
            [ let sid = mkSlotId slotKey
                  path = IndexedRowsValueBucket validationPath sid repKey
                  actual = rowIdSetIntersectionWithIntSet bucket activeRows
                  expected = expectedArrangementBucket cursor activeRows sid repKey
               in validateBucket path bucket <> bucketMismatch path actual expected
              | (repKey, bucket) <- IntMap.toList byRep
            ]
      | (slotKey, byRep) <- IntMap.toList (denseArrangementValueIndex cursor)
    ]
{-# INLINE arrangementBucketErrors #-}

validateBucket :: IndexedRowsBucketPath -> RowIdSet -> [SharedBucketError]
validateBucket path bucket =
  case validateRowIdSet bucket of
    Right () -> []
    Left err -> [SharedRowIdSetBucketInvalid path err]
{-# INLINE validateBucket #-}

bucketMismatch :: IndexedRowsBucketPath -> IntSet -> IntSet -> [SharedBucketError]
bucketMismatch path actual expected
  | actual == expected = []
  | otherwise = [SharedBucketDenotationMismatch path actual expected]
{-# INLINE bucketMismatch #-}

expectedArrangementBucket :: DenseArrangement -> IntSet -> SlotId -> Int -> IntSet
expectedArrangementBucket cursor activeRows sid repKey =
  IntSet.foldl'
    (\matches rowKey ->
      case denseArrangementValueAt cursor sid rowKey of
        Just (RepKey value) | value == repKey ->
          IntSet.insert rowKey matches
        _ ->
          matches
    )
    IntSet.empty
    activeRows
{-# INLINE expectedArrangementBucket #-}
