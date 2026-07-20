{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Execution.Factor.Validate
  ( FactorIndexValidationError (..),
    validateFactorIndex,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Moonlight.Flow.Execution.Factor.Core
  ( Factor,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Types
  ( ProvVal (..),
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Differential.Index.IndexedRows
  ( indexedRowsIdByKey,
    indexedRowsLiveRows,
    indexedRowsPayloadMap,
    indexedRowsRowById,
    indexedRowsLayout,
  )
import Moonlight.Flow.Storage.Index.Validate
  ( SharedBucketError,
    ValidationPath (..),
    indexedRowsBucketErrors,
  )
import Moonlight.Flow.Storage.Index.TupleFormat
  ( tupleKeyIndexedFormat,
  )

type FactorIndexValidationError :: Type
data FactorIndexValidationError
  = FactorBucketInvalid !SharedBucketError
  | FactorLiveRowMissing {-# UNPACK #-} !Int
  | FactorInverseMissing !AssignmentTupleKey {-# UNPACK #-} !Int
  | FactorReverseInverseMismatch !AssignmentTupleKey {-# UNPACK #-} !Int !(Maybe (AssignmentTupleKey, ProvVal))
  | FactorKeyWidthMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | FactorCellMismatch !AssignmentTupleKey !(Maybe ProvVal) !(Maybe ProvVal)
  deriving stock (Eq, Show)

finish :: [FactorIndexValidationError] -> Either [FactorIndexValidationError] ()
finish [] = Right ()
finish errors = Left errors
{-# INLINE finish #-}

validateFactorIndex :: Factor -> Either [FactorIndexValidationError] ()
validateFactorIndex factor =
  finish $
    factorRowShapeErrors
      <> factorInverseErrors
      <> factorCellErrors
      <> factorBucketErrors
  where
    expectedWidth = Vector.length (indexedRowsLayout factor)

    factorRowShapeErrors :: [FactorIndexValidationError]
    factorRowShapeErrors =
      concatMap rowShapeErrors (IntSet.toList (indexedRowsLiveRows factor))
      where
        rowShapeErrors rowId =
          case IntMap.lookup rowId (indexedRowsRowById factor) of
            Nothing ->
              [FactorLiveRowMissing rowId]
            Just (key, _) ->
              let actualWidth = tupleKeyWidth key
               in [ FactorKeyWidthMismatch rowId expectedWidth actualWidth
                    | actualWidth /= expectedWidth
                  ]

    factorInverseErrors :: [FactorIndexValidationError]
    factorInverseErrors =
      keyToIdErrors <> idToKeyErrors
      where
        keyToIdErrors =
          [ FactorInverseMissing key rowId
            | (key, rowId) <- Map.toList (indexedRowsIdByKey factor),
              case IntMap.lookup rowId (indexedRowsRowById factor) of
                Just (indexedKey, _) -> indexedKey /= key
                Nothing -> True
          ]

        idToKeyErrors =
          [ FactorReverseInverseMismatch key rowId actual
            | rowId <- IntSet.toList (indexedRowsLiveRows factor),
              Just (key, _) <- [IntMap.lookup rowId (indexedRowsRowById factor)],
              let expected = Map.lookup key (indexedRowsIdByKey factor),
              expected /= Just rowId,
              let actual = IntMap.lookup rowId (indexedRowsRowById factor)
          ]

    factorCellErrors :: [FactorIndexValidationError]
    factorCellErrors =
      payloadMissingInIndex <> indexRowsMissingInPayload
      where
        payloadMissingInIndex =
          [ FactorCellMismatch key (Just val) actual
            | (key, val) <- Map.toList (indexedRowsPayloadMap factor),
              let actual = do
                    rowId <- Map.lookup key (indexedRowsIdByKey factor)
                    (_, indexedVal) <- IntMap.lookup rowId (indexedRowsRowById factor)
                    pure indexedVal,
              actual /= Just val
          ]

        indexRowsMissingInPayload =
          [ FactorCellMismatch key actual (Just indexedVal)
            | rowId <- IntSet.toList (indexedRowsLiveRows factor),
              Just (key, indexedVal) <- [IntMap.lookup rowId (indexedRowsRowById factor)],
              let actual = Map.lookup key (indexedRowsPayloadMap factor),
              actual /= Just indexedVal
          ]

    factorBucketErrors :: [FactorIndexValidationError]
    factorBucketErrors =
      fmap
        FactorBucketInvalid
        (indexedRowsBucketErrors tupleKeyIndexedFormat (FactorPath 0) factor)
{-# INLINE validateFactorIndex #-}
