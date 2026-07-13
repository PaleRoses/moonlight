{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Differential.Row.Delta
  ( RowDelta,
    rowDeltaNull,
    rowDeltaBetween,
    rowDeltaPositivePart,
    rowDeltaNegativePart,
    PositiveMultiplicity,
    positiveMultiplicityValue,
    RowDeltaError (..),

    RowBlockDeltaError (..),
    rowDescToRowTupleKey,
    rowTupleKeyToSlots,
    rowBlockToRowDelta,
    rowDeltaToRowBlock,
    diffRowBlocks,
    applyRowDeltaToRowBlock,
    rowDeltaAffectedClasses,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.IntSet (IntSet)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as VB
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word64)
import Moonlight.Delta.Signed
  ( Multiplicity (..),
    MultiplicityChange (..),
    negateMultiplicityChange,
    positiveMultiplicityChange
  )
import Moonlight.Differential.Row.Block
  ( RowBlock,
    RowBlockIdentity,
    RowBuildError,
    RowDesc,
    RowLayout,
    RowState (Canonical),
    foldRowBlock,
    fromSlotRows,
    rowSlots,
  )
import Moonlight.Differential.Row.Patch
  ( PlainRowPatch,
    composePlainRowPatch,
    normalizePlainRowPatch,
    plainRowPatchFromList,
    plainRowPatchFromMultiplicityMap,
    plainRowPatchChangeMap,
    plainRowPatchNull,
    subtractPlainRowPatch
  )
import Moonlight.Differential.Row.Tuple
  ( RepKey (..),
    RowTupleKey,
    mkRepKey,
    repKeyWord64,
    tupleKeyClassKeys,
    tupleKeyFromRepKeys,
    tupleKeyToRepKeys,
    tupleKeyWidth,
  )


type RowDelta :: Type
type RowDelta =
  PlainRowPatch RowTupleKey

rowDeltaNull :: RowDelta -> Bool
rowDeltaNull =
  plainRowPatchNull
{-# INLINE rowDeltaNull #-}

rowDeltaBetween ::
  Map RowTupleKey Multiplicity ->
  Map RowTupleKey Multiplicity ->
  RowDelta
rowDeltaBetween beforeRows afterRows =
  subtractPlainRowPatch
    (plainRowPatchFromMultiplicityMap afterRows)
    (plainRowPatchFromMultiplicityMap beforeRows)
{-# INLINE rowDeltaBetween #-}

type PositiveMultiplicity :: Type
newtype PositiveMultiplicity = PositiveMultiplicity
  { unPositiveMultiplicity :: Multiplicity
  }
  deriving stock (Eq, Ord, Show)

positiveMultiplicityValue :: PositiveMultiplicity -> Multiplicity
positiveMultiplicityValue =
  unPositiveMultiplicity
{-# INLINE positiveMultiplicityValue #-}

rowDeltaPositivePart :: RowDelta -> Map RowTupleKey PositiveMultiplicity
rowDeltaPositivePart =
  Map.mapMaybe (fmap PositiveMultiplicity . positiveMultiplicityChange)
    . plainRowPatchChangeMap
    . normalizePlainRowPatch
{-# INLINE rowDeltaPositivePart #-}

rowDeltaNegativePart :: RowDelta -> Map RowTupleKey PositiveMultiplicity
rowDeltaNegativePart =
  Map.mapMaybe (fmap PositiveMultiplicity . positiveMultiplicityChange . negateMultiplicityChange)
    . plainRowPatchChangeMap
    . normalizePlainRowPatch
{-# INLINE rowDeltaNegativePart #-}

type RowDeltaError :: Type
data RowDeltaError
  = NonPositiveRemovedMultiplicity !RowTupleKey !Multiplicity
  | NonPositiveInsertedMultiplicity !RowTupleKey !Multiplicity
  deriving stock (Eq, Show)

data RowBlockDeltaError
  = RowBlockWordExceedsInt !Word64
  | RowBlockNegativeRepKey !RepKey
  | RowBlockLengthMismatch !Int !Int !RowTupleKey
  | RowBlockMultiplicityInvalid !RowTupleKey !MultiplicityChange
  | RowBlockBuildFailed !RowBuildError
  deriving stock (Eq, Ord, Show)

rowDescToRowTupleKey ::
  RowBlock state ->
  RowDesc ->
  Either RowBlockDeltaError RowTupleKey
rowDescToRowTupleKey rows desc =
  tupleKeyFromRepKeys
    <$> traverse wordToRowDeltaRepKey (VU.toList (rowSlots rows desc))

rowTupleKeyToSlots ::
  Int ->
  RowTupleKey ->
  Either RowBlockDeltaError (VU.Vector Word64)
rowTupleKeyToSlots expectedWidth rowValue = do
  let actualWidth =
        tupleKeyWidth rowValue
  if actualWidth == expectedWidth
    then pure ()
    else Left (RowBlockLengthMismatch expectedWidth actualWidth rowValue)
  VU.fromList <$> traverse rowDeltaRepKeyToWord (tupleKeyToRepKeys rowValue)

rowBlockToRowDelta ::
  RowBlock 'Canonical ->
  Either RowBlockDeltaError RowDelta
rowBlockToRowDelta rows =
  plainRowPatchFromList
    <$> foldRowBlock collectRow (Right []) rows
  where
    collectRow ::
      Either RowBlockDeltaError [(RowTupleKey, MultiplicityChange)] ->
      RowDesc ->
      Either RowBlockDeltaError [(RowTupleKey, MultiplicityChange)]
    collectRow eitherRows desc = do
      currentRows <- eitherRows
      rowValue <- rowDescToRowTupleKey rows desc
      pure ((rowValue, MultiplicityChange 1) : currentRows)

rowDeltaToRowBlock ::
  RowBlockIdentity ->
  RowLayout ->
  RowDelta ->
  Either RowBlockDeltaError (RowBlock 'Canonical)
rowDeltaToRowBlock identityValue schemaValue rowDelta = do
  packedRows <-
    traverse
      (uncurry (rowEntryToSlots expectedWidth))
      (Map.toAscList (plainRowPatchChangeMap (normalizePlainRowPatch rowDelta)))
  first RowBlockBuildFailed $
    fromSlotRows identityValue schemaValue packedRows
  where
    expectedWidth =
      VB.length schemaValue

rowEntryToSlots ::
  Int ->
  RowTupleKey ->
  MultiplicityChange ->
  Either RowBlockDeltaError (VU.Vector Word64)
rowEntryToSlots expectedWidth rowValue multiplicity =
  if multiplicity == MultiplicityChange 1
    then rowTupleKeyToSlots expectedWidth rowValue
    else Left (RowBlockMultiplicityInvalid rowValue multiplicity)

diffRowBlocks ::
  RowBlock 'Canonical ->
  RowBlock 'Canonical ->
  Either RowBlockDeltaError RowDelta
diffRowBlocks afterRows beforeRows = do
  afterDelta <- rowBlockToRowDelta afterRows
  beforeDelta <- rowBlockToRowDelta beforeRows
  pure (subtractPlainRowPatch afterDelta beforeDelta)

applyRowDeltaToRowBlock ::
  RowBlockIdentity ->
  RowLayout ->
  RowDelta ->
  RowBlock 'Canonical ->
  Either RowBlockDeltaError (RowBlock 'Canonical)
applyRowDeltaToRowBlock identityValue schemaValue rowDelta currentRows = do
  currentDelta <- rowBlockToRowDelta currentRows
  rowDeltaToRowBlock
    identityValue
    schemaValue
    (composePlainRowPatch currentDelta rowDelta)

rowDeltaAffectedClasses ::
  RowDelta ->
  IntSet
rowDeltaAffectedClasses rowDelta =
  foldMap tupleKeyClassKeys $
    Map.keys (plainRowPatchChangeMap (normalizePlainRowPatch rowDelta))
{-# INLINE rowDeltaAffectedClasses #-}

wordToRowDeltaRepKey ::
  Word64 ->
  Either RowBlockDeltaError RepKey
wordToRowDeltaRepKey wordValue =
  if wordValue <= fromIntegral (maxBound :: Int)
    then
      case mkRepKey (fromIntegral wordValue) of
        Left _obstruction ->
          Left (RowBlockNegativeRepKey (RepKey (fromIntegral wordValue)))
        Right keyValue ->
          Right keyValue
    else Left (RowBlockWordExceedsInt wordValue)

rowDeltaRepKeyToWord ::
  RepKey ->
  Either RowBlockDeltaError Word64
rowDeltaRepKeyToWord keyValue =
  case repKeyWord64 keyValue of
    Left _obstruction ->
      Left (RowBlockNegativeRepKey keyValue)
    Right wordValue ->
      Right wordValue
