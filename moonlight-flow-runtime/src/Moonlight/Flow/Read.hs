{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Read
  ( Rows,
    ReadError (..),
    rowsToList,
    positiveRows,
    rowMultiplicity,
    rowsNull,
    rowsDigest,
    readRows,
    readRowsFold,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Foldable qualified as Foldable
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Word
  ( Word64,
  )
import Moonlight.Flow.Internal.Digest
  ( mix64,
    wordOfInt,
  )
import Moonlight.Delta.Signed
  ( Multiplicity,
    addMultiplicity,
    multiplicityValue,
    zeroMultiplicity
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    tupleKeyFoldlInts',
    tupleKeyWidth,
  )
import Moonlight.Flow.Query qualified as Query
import Moonlight.Flow.Runtime.Types qualified as Runtime
import Moonlight.Flow.Runtime.Visible qualified as Runtime
import Moonlight.Flow.Runtime.Types
  ( RuntimeReadError,
  )
import Moonlight.Flow.Runtime.Spec.Schema
  ( RuntimePlan (..),
    RuntimePlanProjection (..),
  )

newtype Rows = Rows
  { unRows :: Map RowTupleKey Multiplicity
  }
  deriving stock (Eq, Show)

data ReadError ctx prop
  = ReadRuntimeError !(RuntimeReadError ctx prop)
  | ReadQueryError !Query.QueryError
  deriving stock (Eq, Show)

rowsToList :: Rows -> [(RowTupleKey, Multiplicity)]
rowsToList (Rows rows) =
  Map.toAscList rows

positiveRows :: Rows -> [RowTupleKey]
positiveRows (Rows rows) =
  Map.foldrWithKey
    ( \rowValue multiplicity acc ->
        if multiplicityValue multiplicity > 0
          then rowValue : acc
          else acc
    )
    []
    rows

rowMultiplicity :: RowTupleKey -> Rows -> Multiplicity
rowMultiplicity row (Rows rows) =
  Map.findWithDefault zeroMultiplicity row rows

rowsNull :: Rows -> Bool
rowsNull (Rows rows) =
  not (any ((> 0) . multiplicityValue) rows)

rowsDigest :: Rows -> (Word64, Word64)
rowsDigest (Rows rowsValue) =
  Map.foldlWithKey' rowDigestStep headerDigest rowsValue
  where
    headerDigest =
      mixDigestWords
        (0x9e3779b97f4a7c15, 0xcbf29ce484222325)
        [0x726f777344696765, wordOfInt (Map.size rowsValue)]

    rowDigestStep :: (Word64, Word64) -> RowTupleKey -> Multiplicity -> (Word64, Word64)
    rowDigestStep (!high0, !low0) rowValue multiplicity =
      let !withTag =
            mixDigestWord (high0, low0) 0x01
          !withWidth =
            mixDigestWord withTag (wordOfInt (tupleKeyWidth rowValue))
          !withSlots =
            tupleKeyFoldlInts'
              (\digestValue slotValue -> mixDigestWord digestValue (wordOfInt slotValue))
              withWidth
              rowValue
       in mixDigestWord withSlots (fromIntegral (multiplicityValue multiplicity))

mixDigestWords :: (Word64, Word64) -> [Word64] -> (Word64, Word64)
mixDigestWords =
  Foldable.foldl' mixDigestWord

mixDigestWord :: (Word64, Word64) -> Word64 -> (Word64, Word64)
mixDigestWord (!highValue, !lowValue) wordValue =
  (mix64 highValue wordValue, mix64 lowValue wordValue)

readRows ::
  (Ord ctx, Ord prop) =>
  RuntimePlan ctx prop ->
  Runtime.Runtime ctx prop ->
  Either (ReadError ctx prop) Rows
readRows plan runtime =
  Rows . normalizeRowsMap
    <$> readRowsFold
      plan
      runtime
      Map.empty
      ( \rowValue multiplicity !acc ->
          Map.insertWith addMultiplicity rowValue multiplicity acc
      )

readRowsFold ::
  (Ord ctx, Ord prop) =>
  RuntimePlan ctx prop ->
  Runtime.Runtime ctx prop ->
  r ->
  (RowTupleKey -> Multiplicity -> r -> r) ->
  Either (ReadError ctx prop) r
readRowsFold plan runtime initial step
  | runtimePlanProjectionIsIdentity (rpProjection plan) =
      first ReadRuntimeError $
        Runtime.visibleRowsFold plan runtime initial step
  | otherwise =
      flattenProjectedRead $
        Runtime.visibleRowsFold
          plan
          runtime
          (Right initial)
          (projectVisibleRowStep plan step)

projectVisibleRowStep ::
  RuntimePlan ctx prop ->
  (RowTupleKey -> Multiplicity -> r -> r) ->
  RowTupleKey ->
  Multiplicity ->
  Either (ReadError ctx prop) r ->
  Either (ReadError ctx prop) r
projectVisibleRowStep _plan _step _rowValue _multiplicity (Left err) =
  Left err
projectVisibleRowStep plan step rowValue multiplicity (Right acc0) = do
  projectedRow <-
    first ReadQueryError $
      Query.projectRowWithSlots
        (rppFullSchema (rpProjection plan))
        (rppOutputSlots (rpProjection plan))
        rowValue
  let !acc1 =
        step projectedRow multiplicity acc0
  pure acc1

flattenProjectedRead ::
  Either (RuntimeReadError ctx prop) (Either (ReadError ctx prop) r) ->
  Either (ReadError ctx prop) r
flattenProjectedRead =
  either (Left . ReadRuntimeError) id

runtimePlanProjectionIsIdentity :: RuntimePlanProjection -> Bool
runtimePlanProjectionIsIdentity projection =
  rppFullSchema projection == rppOutputSlots projection

normalizeRowsMap ::
  Map RowTupleKey Multiplicity ->
  Map RowTupleKey Multiplicity
normalizeRowsMap =
  Map.filter (/= zeroMultiplicity)
