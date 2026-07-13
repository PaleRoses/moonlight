{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

#include "MachDeps.h"

#if WORD_SIZE_IN_BITS < 64
#error "Moonlight.Differential.Row.Block requires a 64-bit Int."
#endif

module Moonlight.Differential.Row.Block
  ( RowLayout,
    RestrictionMap,

    RowState (Canonical),
    RowBlockIdentity (..),
    RowBlock,
    RowDesc,

    RowBuildError (..),
    RowProgramError (..),
    RowOperationError (..),
    RowRestrictionProgram,
    rowRestrictionProgramTargetLayout,
    rowRestrictionProgramTargetLayoutHash,
    rowRestrictionProgramFingerprint,

    emptyRowBlock,
    rowBlockCount,
    rowBlockLayout,
    rowBlockIdentity,
    rowBlockByteSize,
    layoutHash,
    hashRowFromSlots,

    rowBlockDescAt,
    rowBlockRowIndex,
    rowBlockRowIndices,
    rowSlots,
    foldRowBlock,
    rowDescSupport,
    rowBlockSupport,

    fromSlotRows,
    fromSlotRowsWith,
    compileRowRestriction,
    restrictRows,
    reidentifyRows,

    unionRows,
    differenceRows,
    intersectRows,
    filterRowsWithRemoved,

    containsRow,
    containsProjectedRow,
    containsRestrictedRow,
    withRowBlockIndex,
  )
where

import Control.Monad (void)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor)
import Data.Foldable (traverse_)
import Data.Foldable qualified as Foldable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Primitive.MutVar
  ( MutVar,
    newMutVar,
    readMutVar,
    writeMutVar,
  )
import Data.Primitive.PrimArray
  ( MutablePrimArray,
    PrimArray,
    copyMutablePrimArray,
    copyPrimArray,
    freezePrimArray,
    getSizeofMutablePrimArray,
    indexPrimArray,
    newPrimArray,
    sizeofPrimArray,
    unsafeFreezePrimArray,
    writePrimArray,
  )
import Data.Vector (Vector)
import Data.Vector qualified as VB
import Data.Vector.Algorithms.Intro qualified as Intro
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Data.Vector.Unboxed.Mutable qualified as VUM
import Data.Word (Word64)
import Moonlight.Core
  ( SlotId,
    slotIdKey,
  )

type RowLayout :: Type
type RowLayout = Vector SlotId

type RestrictionMap :: Type
type RestrictionMap = IntMap Int

type RowState :: Type
data RowState
  = Canonical

type RowBlockIdentity :: Type
data RowBlockIdentity = RowBlockIdentity
  { rowBlockBaseRevision :: !Int,
    rowBlockOverlayEpoch :: !Int,
    rowBlockPlanFingerprint :: !Int,
    rowBlockEntityKey :: !Int,
    rowBlockGeneration :: !Int
  }
  deriving stock (Eq, Ord, Show, Read)

type RowDesc :: Type
data RowDesc = RowDesc
  { ardOffsetWords :: !Int,
    ardLengthSlots :: !Int,
    ardHash64 :: !Word64
  }
  deriving stock (Eq, Show)

derivingUnbox
  "RowDesc"
  [t| RowDesc -> (Int, Int, Word64) |]
  [| \(RowDesc offset lengthValue hashValue) -> (offset, lengthValue, hashValue) |]
  [| \(offset, lengthValue, hashValue) -> RowDesc offset lengthValue hashValue |]

type RowBlock :: RowState -> Type
data RowBlock state = RowBlock
  { arIdentity :: !RowBlockIdentity,
    arLayout :: !RowLayout,
    arLayoutHash :: !Word64,
    arPayload :: !(PrimArray Word64),
    arRows :: !(VU.Vector RowDesc),
    arIndex :: !(Maybe RowBlockIndex)
  }

instance Eq (RowBlock state) where
  (==) = sameRowBlock

instance Show (RowBlock state) where
  show rows =
    "RowBlock { rowCount = "
      <> show (rowBlockCount rows)
      <> ", payloadWords = "
      <> show (sizeofPrimArray (arPayload rows))
      <> ", layoutHash = "
      <> show (arLayoutHash rows)
      <> " }"

type RowBlockIndex :: Type
newtype RowBlockIndex = RowBlockIndex
  { ariBuckets :: HashMap Word64 (VU.Vector Int)
  }
  deriving stock (Eq, Show)

type RowBlockBuilder :: Type -> Type
data RowBlockBuilder s = RowBlockBuilder
  { arbIdentity :: !RowBlockIdentity,
    arbLayout :: !RowLayout,
    arbLayoutHash :: !Word64,
    arbPayloadRef :: !(MutVar s (MutablePrimArray s Word64)),
    arbRowsRef :: !(MutVar s (VUM.MVector s RowDesc)),
    arbPayloadLength :: !(MutVar s Int),
    arbRowLength :: !(MutVar s Int)
  }

type RowProgramError :: Type
data RowProgramError
  = NegativeRestrictionKey !Int
  | NegativeRestrictionValue !Int !Int
  deriving stock (Eq, Ord, Show, Read)

type RowBuildError :: Type
data RowBuildError
  = RowWidthMismatch !Int !Int !Int
  | RowNegativeSlotValue !Int
  deriving stock (Eq, Ord, Show, Read)

type SlotRowStats :: Type
data SlotRowStats = SlotRowStats
  { srsNextRowIndex :: !Int,
    srsPayloadCapacity :: !Int,
    srsRowCapacity :: !Int
  }
  deriving stock (Eq, Show)

type DenseRestriction :: Type
data DenseRestriction = DenseRestriction
  { drBaseKey :: !Int,
    drValues :: !(VU.Vector Int)
  }
  deriving stock (Eq, Show)

type RowRestrictionProgram :: Type
data RowRestrictionProgram = RowRestrictionProgram
  { rrpRestrictionMap :: !RestrictionMap,
    rrpDenseRestriction :: !(Maybe DenseRestriction),
    rrpTargetLayout :: !RowLayout,
    rrpTargetLayoutHash :: !Word64,
    rrpFingerprint :: !Word64
  }
  deriving stock (Eq, Show)

type RowOperationError :: Type
data RowOperationError
  = RowLayoutMismatch !Word64 !Word64
  | RowRestrictionWidthMismatch !Int !Int
  deriving stock (Eq, Ord, Show, Read)

data RowMergeTail
  = AppendMergeTail
  | DropMergeTail

data RowMergeAction
  = EmitLeftAdvanceLeft
  | EmitRightAdvanceRight
  | EmitLeftAdvanceBoth
  | AdvanceLeft
  | AdvanceRight
  | AdvanceBoth

emptyRowBlock :: RowBlockIdentity -> RowLayout -> RowBlock 'Canonical
emptyRowBlock identityValue layoutValue =
  RowBlock
    { arIdentity = identityValue,
      arLayout = layoutValue,
      arLayoutHash = layoutHash layoutValue,
      arPayload = runST (unsafeFreezePrimArray =<< newPrimArray 0),
      arRows = VU.empty,
      arIndex = Nothing
    }

rowBlockCount :: RowBlock state -> Int
rowBlockCount = VU.length . arRows

rowBlockLayout :: RowBlock state -> RowLayout
rowBlockLayout = arLayout

rowBlockIdentity :: RowBlock state -> RowBlockIdentity
rowBlockIdentity = arIdentity

rowBlockByteSize :: RowBlock state -> Int
rowBlockByteSize rows =
  sizeofPrimArray (arPayload rows) * word64Bytes
    + VU.length (arRows rows) * atomRowDescBytes

rowBlockDescAt :: RowBlock state -> Int -> Maybe RowDesc
rowBlockDescAt rows indexValue
  | indexValue < 0 = Nothing
  | indexValue >= VU.length (arRows rows) = Nothing
  | otherwise = Just (arRows rows VU.! indexValue)
{-# INLINE rowBlockDescAt #-}

sameRowBlock :: RowBlock left -> RowBlock right -> Bool
sameRowBlock leftRows rightRows =
  arLayoutHash leftRows == arLayoutHash rightRows
    && arLayout leftRows == arLayout rightRows
    && VU.length (arRows leftRows) == VU.length (arRows rightRows)
    && sameRowsAt 0
  where
    rowCount = VU.length (arRows leftRows)
    sameRowsAt indexValue
      | indexValue >= rowCount = True
      | otherwise =
          let leftDesc = arRows leftRows VU.! indexValue
              rightDesc = arRows rightRows VU.! indexValue
           in rowEqualAcross (arPayload leftRows) leftDesc (arPayload rightRows) rightDesc
                && sameRowsAt (indexValue + 1)

layoutHash :: RowLayout -> Word64
layoutHash = VB.foldl' mixSlot initialLayoutHash
  where
    mixSlot hashValue slot = mixWord64 hashValue (slotWord64 slot)

slotWord64 :: SlotId -> Word64
slotWord64 =
  fromIntegral . slotIdKey

rowSlots :: RowBlock state -> RowDesc -> VU.Vector Word64
rowSlots rows desc =
  VU.generate (max 0 (ardLengthSlots desc))
    (\slotIndex -> rowSlotUnchecked rows desc slotIndex)

newRowBlockBuilder :: RowBlockIdentity -> RowLayout -> Int -> Int -> ST s (RowBlockBuilder s)
newRowBlockBuilder identityValue layoutValue requestedPayloadCapacity requestedRowCapacity = do
  let payloadCapacity = max 0 requestedPayloadCapacity
      rowCapacity = max 0 requestedRowCapacity
  payloadArray <- newPrimArray payloadCapacity
  rowVector <- VUM.new rowCapacity
  payloadRef <- newMutVar payloadArray
  rowsRef <- newMutVar rowVector
  payloadLengthRef <- newMutVar 0
  rowLengthRef <- newMutVar 0
  pure
    RowBlockBuilder
      { arbIdentity = identityValue,
        arbLayout = layoutValue,
        arbLayoutHash = layoutHash layoutValue,
        arbPayloadRef = payloadRef,
        arbRowsRef = rowsRef,
        arbPayloadLength = payloadLengthRef,
        arbRowLength = rowLengthRef
      }

appendRowBy :: RowBlockBuilder s -> Int -> (Int -> Word64) -> ST s Bool
appendRowBy builder lengthSlots slotAt
  | lengthSlots < 0 = pure False
  | otherwise = do
      payloadCursor <- readMutVar (arbPayloadLength builder)
      rowCursor <- readMutVar (arbRowLength builder)
      payloadArray <- ensurePayloadCapacity builder (payloadCursor + lengthSlots + 1)
      rowVector <- ensureRowsCapacity builder (rowCursor + 1)
      writePrimArray payloadArray payloadCursor (fromIntegral lengthSlots)
      writeSlots payloadArray payloadCursor 0
      let rowHashValue = hashRowFromSlots (arbLayoutHash builder) lengthSlots slotAt
          rowDesc =
            RowDesc
              { ardOffsetWords = payloadCursor,
                ardLengthSlots = lengthSlots,
                ardHash64 = rowHashValue
              }
      VUM.write rowVector rowCursor rowDesc
      writeMutVar (arbPayloadLength builder) (payloadCursor + lengthSlots + 1)
      writeMutVar (arbRowLength builder) (rowCursor + 1)
      pure True
  where
    writeSlots payloadArray payloadCursor slotIndex
      | slotIndex >= lengthSlots = pure ()
      | otherwise = do
          writePrimArray payloadArray (payloadCursor + 1 + slotIndex) (slotAt slotIndex)
          writeSlots payloadArray payloadCursor (slotIndex + 1)

sealBuilderCanonical :: RowBlockBuilder s -> ST s (RowBlock 'Canonical)
sealBuilderCanonical builder = do
  rowSource <- readMutVar (arbRowsRef builder)
  rowLengthValue <- readMutVar (arbRowLength builder)
  if rowLengthValue <= 0
    then pure (emptyRowBlock (arbIdentity builder) (arbLayout builder))
    else do
      sourcePayload <- freezeBuilderPayloadExact builder
      let sortedRows = VUM.slice 0 rowLengthValue rowSource
      Intro.sortBy (rowCompareInPayload sourcePayload) sortedRows
      canonicalizeSortedRows
        (arbIdentity builder)
        (arbLayout builder)
        sourcePayload
        sortedRows

freezeBuilderCanonicalOrdered :: RowBlockBuilder s -> ST s (RowBlock 'Canonical)
freezeBuilderCanonicalOrdered builder = do
  rowLengthValue <- readMutVar (arbRowLength builder)
  if rowLengthValue <= 0
    then pure (emptyRowBlock (arbIdentity builder) (arbLayout builder))
    else do
      payloadFrozen <- freezeBuilderPayloadExact builder
      rowsFrozen <- freezeBuilderRowsExact builder
      pure
        RowBlock
          { arIdentity = arbIdentity builder,
            arLayout = arbLayout builder,
            arLayoutHash = arbLayoutHash builder,
            arPayload = payloadFrozen,
            arRows = rowsFrozen,
            arIndex = Nothing
          }

-- Consumes the builder payload. The caller must not append after freezing.
freezeBuilderPayloadExact :: RowBlockBuilder s -> ST s (PrimArray Word64)
freezeBuilderPayloadExact builder = do
  payloadSource <- readMutVar (arbPayloadRef builder)
  payloadLengthValue <- readMutVar (arbPayloadLength builder)
  payloadCapacity <- getSizeofMutablePrimArray payloadSource
  if payloadLengthValue == payloadCapacity
    then unsafeFreezePrimArray payloadSource
    else freezePrimArray payloadSource 0 payloadLengthValue

-- Consumes the builder row vector. The caller must not append after freezing.
freezeBuilderRowsExact :: RowBlockBuilder s -> ST s (VU.Vector RowDesc)
freezeBuilderRowsExact builder = do
  rowSource <- readMutVar (arbRowsRef builder)
  rowLengthValue <- readMutVar (arbRowLength builder)
  if rowLengthValue == VUM.length rowSource
    then VU.unsafeFreeze rowSource
    else VU.freeze (VUM.slice 0 rowLengthValue rowSource)

canonicalizeSortedRows ::
  RowBlockIdentity ->
  RowLayout ->
  PrimArray Word64 ->
  VUM.MVector s RowDesc ->
  ST s (RowBlock 'Canonical)
canonicalizeSortedRows identityValue layoutValue sourcePayload sortedRows = do
  (uniqueCount, payloadWords) <- measureUniqueSortedRows sourcePayload sortedRows
  if uniqueCount <= 0
    then pure (emptyRowBlock identityValue layoutValue)
    else do
      payloadOut <- newPrimArray payloadWords
      rowsOut <- VUM.new uniqueCount
      copyUniqueSortedRows sourcePayload sortedRows payloadOut rowsOut
      payloadFrozen <- unsafeFreezePrimArray payloadOut
      rowsFrozen <- VU.unsafeFreeze rowsOut
      pure
        RowBlock
          { arIdentity = identityValue,
            arLayout = layoutValue,
            arLayoutHash = layoutHash layoutValue,
            arPayload = payloadFrozen,
            arRows = rowsFrozen,
            arIndex = Nothing
          }

measureUniqueSortedRows ::
  PrimArray Word64 ->
  VUM.MVector s RowDesc ->
  ST s (Int, Int)
measureUniqueSortedRows sourcePayload sortedRows
  | rowCount <= 0 = pure (0, 0)
  | otherwise = do
      firstDesc <- VUM.read sortedRows 0
      go 1 firstDesc 1 (rowPayloadWords firstDesc)
  where
    rowCount = VUM.length sortedRows

    go !indexValue !previousDesc !uniqueCount !payloadWords
      | indexValue >= rowCount = pure (uniqueCount, payloadWords)
      | otherwise = do
          desc <- VUM.read sortedRows indexValue
          if rowEqualInPayload sourcePayload previousDesc desc
            then go (indexValue + 1) desc uniqueCount payloadWords
            else
              go
                (indexValue + 1)
                desc
                (uniqueCount + 1)
                (payloadWords + rowPayloadWords desc)

copyUniqueSortedRows ::
  PrimArray Word64 ->
  VUM.MVector s RowDesc ->
  MutablePrimArray s Word64 ->
  VUM.MVector s RowDesc ->
  ST s ()
copyUniqueSortedRows sourcePayload sortedRows targetPayload targetRows
  | rowCount <= 0 = pure ()
  | otherwise = do
      firstDesc <- VUM.read sortedRows 0
      copyOne firstDesc 0 0
      go 1 firstDesc 1 (rowPayloadWords firstDesc)
  where
    rowCount = VUM.length sortedRows

    go !sourceIndex !previousDesc !targetIndex !targetOffset
      | sourceIndex >= rowCount = pure ()
      | otherwise = do
          desc <- VUM.read sortedRows sourceIndex
          if rowEqualInPayload sourcePayload previousDesc desc
            then go (sourceIndex + 1) desc targetIndex targetOffset
            else do
              copyOne desc targetIndex targetOffset
              go
                (sourceIndex + 1)
                desc
                (targetIndex + 1)
                (targetOffset + rowPayloadWords desc)

    copyOne desc targetIndex targetOffset = do
      let !wordCount = rowPayloadWords desc
          !targetDesc = desc {ardOffsetWords = targetOffset}
      copyPrimArray targetPayload targetOffset sourcePayload (ardOffsetWords desc) wordCount
      VUM.write targetRows targetIndex targetDesc

fromSlotRows :: RowBlockIdentity -> RowLayout -> [VU.Vector Word64] -> Either RowBuildError (RowBlock 'Canonical)
fromSlotRows identityValue layoutValue =
  fromSlotRowsWith identityValue layoutValue withVectorSlots
  where
    withVectorSlots :: VU.Vector Word64 -> (Int -> (Int -> Word64) -> result) -> result
    withVectorSlots rowValue consume =
      consume (VU.length rowValue) (VU.unsafeIndex rowValue)
{-# INLINE fromSlotRows #-}

fromSlotRowsWith ::
  Foldable rows =>
  RowBlockIdentity ->
  RowLayout ->
  (forall result. row -> (Int -> (Int -> Word64) -> result) -> result) ->
  rows row ->
  Either RowBuildError (RowBlock 'Canonical)
fromSlotRowsWith identityValue layoutValue withSlots slotRows = do
  stats <- slotRowsStats (VB.length layoutValue) withSlots slotRows
  pure $
    runST $ do
      builder <-
        newRowBlockBuilder
          identityValue
          layoutValue
          (srsPayloadCapacity stats)
          (srsRowCapacity stats)
      traverse_ (appendSlotRow builder withSlots) slotRows
      sealBuilderCanonical builder
{-# INLINE fromSlotRowsWith #-}

slotRowsStats ::
  Foldable rows =>
  Int ->
  (forall result. row -> (Int -> (Int -> Word64) -> result) -> result) ->
  rows row ->
  Either RowBuildError SlotRowStats
slotRowsStats expectedWidth withSlots =
  Foldable.foldl' collect (Right emptySlotRowStats)
  where
    collect eitherStats rowValue =
      case eitherStats of
        Left obstruction ->
          Left obstruction
        Right stats ->
          withSlots rowValue $ \actualWidth _slotAt ->
            if actualWidth == expectedWidth
              then
                Right
                  stats
                    { srsNextRowIndex = srsNextRowIndex stats + 1,
                      srsPayloadCapacity = srsPayloadCapacity stats + actualWidth + 1,
                      srsRowCapacity = srsRowCapacity stats + 1
                    }
              else
                Left (RowWidthMismatch (srsNextRowIndex stats) expectedWidth actualWidth)
{-# INLINE slotRowsStats #-}

emptySlotRowStats :: SlotRowStats
emptySlotRowStats =
  SlotRowStats
    { srsNextRowIndex = 0,
      srsPayloadCapacity = 0,
      srsRowCapacity = 0
    }
{-# INLINE emptySlotRowStats #-}

appendSlotRow ::
  RowBlockBuilder s ->
  (forall result. row -> (Int -> (Int -> Word64) -> result) -> result) ->
  row ->
  ST s ()
appendSlotRow builder withSlots rowValue =
  withSlots rowValue $ \width slotAt ->
    void (appendRowBy builder width slotAt)
{-# INLINE appendSlotRow #-}

foldRowBlock :: (acc -> RowDesc -> acc) -> acc -> RowBlock state -> acc
foldRowBlock step initial rows = VU.foldl' step initial (arRows rows)

rowDescSupport :: RowBlock state -> RowDesc -> IntSet
rowDescSupport rows desc = go 0 IntSet.empty
  where
    go slotIndex acc
      | slotIndex >= ardLengthSlots desc = acc
      | otherwise =
          case word64ToInt (rowSlotUnchecked rows desc slotIndex) of
            Nothing -> go (slotIndex + 1) acc
            Just classKey -> go (slotIndex + 1) (IntSet.insert classKey acc)

rowBlockSupport :: RowBlock state -> IntSet
rowBlockSupport rows =
  foldRowBlock
    (\acc desc -> IntSet.union acc (rowDescSupport rows desc))
    IntSet.empty
    rows

compileRowRestriction :: RowLayout -> RestrictionMap -> Either RowProgramError RowRestrictionProgram
compileRowRestriction targetLayout restrictionMapValue = do
  validateRestrictionMap restrictionMapValue
  pure
    RowRestrictionProgram
      { rrpRestrictionMap = restrictionMapValue,
        rrpDenseRestriction = denseRestrictionFrom restrictionMapValue,
        rrpTargetLayout = targetLayout,
        rrpTargetLayoutHash = layoutHash targetLayout,
        rrpFingerprint = restrictionFingerprint targetLayout restrictionMapValue
      }

restrictSlotWord :: RowRestrictionProgram -> Word64 -> Word64
restrictSlotWord program slotWord = case word64ToInt slotWord of
  Nothing -> slotWord
  Just slotKey -> fromIntegral (lookupRestrictedSlot program slotKey)

restrictRows :: RowRestrictionProgram -> RowBlockIdentity -> RowBlock 'Canonical -> Either RowOperationError (RowBlock 'Canonical)
restrictRows program outputIdentity sourceRows = do
  ensureRestrictionWidth program sourceRows
  pure $ runST $ do
    builder <-
      newRowBlockBuilder
        outputIdentity
        (rrpTargetLayout program)
        (sizeofPrimArray (arPayload sourceRows))
        (VU.length (arRows sourceRows))
    copyRestrictedRows builder 0
    sealBuilderCanonical builder
  where
    rowCount = VU.length (arRows sourceRows)
    copyRestrictedRows :: forall s. RowBlockBuilder s -> Int -> ST s ()
    copyRestrictedRows builder indexValue
      | indexValue >= rowCount = pure ()
      | otherwise = do
          let desc = arRows sourceRows VU.! indexValue
          _ <-
            appendRowBy
              builder
              (ardLengthSlots desc)
              (\slotIndex -> restrictSlotWord program (rowSlotUnchecked sourceRows desc slotIndex))
          copyRestrictedRows builder (indexValue + 1)

reidentifyRows :: RowBlockIdentity -> RowBlock state -> RowBlock state
reidentifyRows identityValue rows = rows {arIdentity = identityValue}

rowRestrictionProgramTargetLayout :: RowRestrictionProgram -> RowLayout
rowRestrictionProgramTargetLayout =
  rrpTargetLayout

rowRestrictionProgramTargetLayoutHash :: RowRestrictionProgram -> Word64
rowRestrictionProgramTargetLayoutHash =
  rrpTargetLayoutHash

rowRestrictionProgramFingerprint :: RowRestrictionProgram -> Word64
rowRestrictionProgramFingerprint =
  rrpFingerprint

unionRows ::
  RowBlockIdentity ->
  RowBlock 'Canonical ->
  RowBlock 'Canonical ->
  Either RowOperationError (RowBlock 'Canonical)
unionRows identityValue leftRows rightRows =
  mergeSortedRows
    identityValue
    leftRows
    rightRows
    (sizeofPrimArray (arPayload leftRows) + sizeofPrimArray (arPayload rightRows))
    (VU.length (arRows leftRows) + VU.length (arRows rightRows))
    AppendMergeTail
    AppendMergeTail
    unionAction
  where
    unionAction orderValue =
      case orderValue of
        LT -> EmitLeftAdvanceLeft
        GT -> EmitRightAdvanceRight
        EQ -> EmitLeftAdvanceBoth

differenceRows ::
  RowBlockIdentity ->
  RowBlock 'Canonical ->
  RowBlock 'Canonical ->
  Either RowOperationError (RowBlock 'Canonical)
differenceRows identityValue leftRows rightRows =
  mergeSortedRows
    identityValue
    leftRows
    rightRows
    (sizeofPrimArray (arPayload leftRows))
    (VU.length (arRows leftRows))
    AppendMergeTail
    DropMergeTail
    differenceAction
  where
    differenceAction orderValue =
      case orderValue of
        LT -> EmitLeftAdvanceLeft
        GT -> AdvanceRight
        EQ -> AdvanceBoth

intersectRows ::
  RowBlockIdentity ->
  RowBlock 'Canonical ->
  RowBlock 'Canonical ->
  Either RowOperationError (RowBlock 'Canonical)
intersectRows identityValue leftRows rightRows =
  mergeSortedRows
    identityValue
    leftRows
    rightRows
    (min (sizeofPrimArray (arPayload leftRows)) (sizeofPrimArray (arPayload rightRows)))
    (min (VU.length (arRows leftRows)) (VU.length (arRows rightRows)))
    DropMergeTail
    DropMergeTail
    intersectAction
  where
    intersectAction orderValue =
      case orderValue of
        LT -> AdvanceLeft
        GT -> AdvanceRight
        EQ -> EmitLeftAdvanceBoth

mergeSortedRows ::
  RowBlockIdentity ->
  RowBlock 'Canonical ->
  RowBlock 'Canonical ->
  Int ->
  Int ->
  RowMergeTail ->
  RowMergeTail ->
  (Ordering -> RowMergeAction) ->
  Either RowOperationError (RowBlock 'Canonical)
mergeSortedRows identityValue leftRows rightRows payloadCapacity rowCapacity leftTail rightTail decideAction = do
  ensureSameSchema leftRows rightRows
  pure $ runST $ do
    builder <-
      newRowBlockBuilder
        identityValue
        (arLayout leftRows)
        payloadCapacity
        rowCapacity
    mergeLoop builder 0 0
    freezeBuilderCanonicalOrdered builder
  where
    leftCount = VU.length (arRows leftRows)
    rightCount = VU.length (arRows rightRows)

    mergeLoop :: forall s. RowBlockBuilder s -> Int -> Int -> ST s ()
    mergeLoop builder !leftIndex !rightIndex
      | leftIndex >= leftCount =
          mergeTail builder rightTail rightRows rightIndex
      | rightIndex >= rightCount =
          mergeTail builder leftTail leftRows leftIndex
      | otherwise = do
          let leftDesc = arRows leftRows VU.! leftIndex
              rightDesc = arRows rightRows VU.! rightIndex
              orderValue =
                rowCompareAcross
                  (arPayload leftRows)
                  leftDesc
                  (arPayload rightRows)
                  rightDesc
          case decideAction orderValue of
            EmitLeftAdvanceLeft -> do
              void (appendDescFromSameSchemaPayload builder (arPayload leftRows) leftDesc)
              mergeLoop builder (leftIndex + 1) rightIndex
            EmitRightAdvanceRight -> do
              void (appendDescFromSameSchemaPayload builder (arPayload rightRows) rightDesc)
              mergeLoop builder leftIndex (rightIndex + 1)
            EmitLeftAdvanceBoth -> do
              void (appendDescFromSameSchemaPayload builder (arPayload leftRows) leftDesc)
              mergeLoop builder (leftIndex + 1) (rightIndex + 1)
            AdvanceLeft ->
              mergeLoop builder (leftIndex + 1) rightIndex
            AdvanceRight ->
              mergeLoop builder leftIndex (rightIndex + 1)
            AdvanceBoth ->
              mergeLoop builder (leftIndex + 1) (rightIndex + 1)

    mergeTail :: forall s. RowBlockBuilder s -> RowMergeTail -> RowBlock 'Canonical -> Int -> ST s ()
    mergeTail builder tailAction rows startIndex =
      case tailAction of
        AppendMergeTail ->
          appendRemaining builder rows startIndex
        DropMergeTail ->
          pure ()

filterRowsWithRemoved ::
  RowBlockIdentity ->
  RowBlockIdentity ->
  (RowBlock 'Canonical -> RowDesc -> Bool) ->
  RowBlock 'Canonical ->
  (RowBlock 'Canonical, RowBlock 'Canonical)
filterRowsWithRemoved keepIdentity removedIdentity keepPredicate rows = runST $ do
  keepBuilder <-
    newRowBlockBuilder
      keepIdentity
      (arLayout rows)
      (sizeofPrimArray (arPayload rows))
      (VU.length (arRows rows))
  removedBuilder <-
    newRowBlockBuilder
      removedIdentity
      (arLayout rows)
      (sizeofPrimArray (arPayload rows))
      (VU.length (arRows rows))
  filterLoop keepBuilder removedBuilder 0
  keepRows <- freezeBuilderCanonicalOrdered keepBuilder
  removedRows <- freezeBuilderCanonicalOrdered removedBuilder
  pure (keepRows, removedRows)
  where
    rowCount = VU.length (arRows rows)
    filterLoop :: forall s. RowBlockBuilder s -> RowBlockBuilder s -> Int -> ST s ()
    filterLoop keepBuilder removedBuilder indexValue
      | indexValue >= rowCount = pure ()
      | otherwise = do
          let desc = arRows rows VU.! indexValue
          if keepPredicate rows desc
            then void (appendDescFromSameSchemaPayload keepBuilder (arPayload rows) desc)
            else void (appendDescFromSameSchemaPayload removedBuilder (arPayload rows) desc)
          filterLoop keepBuilder removedBuilder (indexValue + 1)

containsRow :: RowBlock 'Canonical -> RowBlock state -> RowDesc -> Bool
containsRow targetRows candidateRows candidateDesc
  | arLayoutHash targetRows /= arLayoutHash candidateRows = False
  | arLayout targetRows /= arLayout candidateRows = False
  | otherwise =
      case arIndex targetRows of
        Just indexValue -> indexedContains indexValue
        Nothing -> binaryContains 0 (VU.length (arRows targetRows))
  where
    indexedContains (RowBlockIndex buckets) =
      case HashMap.lookup (ardHash64 candidateDesc) buckets of
        Nothing -> False
        Just ordinals -> VU.any ordinalMatches ordinals
    ordinalMatches ordinal
      | ordinal < 0 = False
      | ordinal >= VU.length (arRows targetRows) = False
      | otherwise =
          rowEqualAcross
            (arPayload targetRows)
            (arRows targetRows VU.! ordinal)
            (arPayload candidateRows)
            candidateDesc
    binaryContains low high
      | low >= high = False
      | otherwise =
          let mid = low + ((high - low) `quot` 2)
              midDesc = arRows targetRows VU.! mid
           in case
                rowCompareAcross
                  (arPayload candidateRows)
                  candidateDesc
                  (arPayload targetRows)
                  midDesc
                of
                LT -> binaryContains low mid
                GT -> binaryContains (mid + 1) high
                EQ -> True

rowBlockRowIndex :: RowBlock 'Canonical -> RowBlock state -> RowDesc -> Maybe Int
rowBlockRowIndex targetRows candidateRows candidateDesc
  | arLayoutHash targetRows /= arLayoutHash candidateRows = Nothing
  | arLayout targetRows /= arLayout candidateRows = Nothing
  | otherwise =
      case arIndex targetRows of
        Just indexValue -> indexedIndex indexValue
        Nothing -> binaryIndex 0 (VU.length (arRows targetRows))
  where
    indexedIndex (RowBlockIndex buckets) =
      HashMap.lookup (ardHash64 candidateDesc) buckets >>= VU.foldl' firstMatching Nothing

    firstMatching found ordinal =
      case found of
        Just _ ->
          found
        Nothing ->
          if ordinalMatches ordinal
            then Just ordinal
            else Nothing

    ordinalMatches ordinal =
      ordinal >= 0
        && ordinal < VU.length (arRows targetRows)
        && rowEqualAcross
          (arPayload targetRows)
          (arRows targetRows VU.! ordinal)
          (arPayload candidateRows)
          candidateDesc

    binaryIndex low high
      | low >= high = Nothing
      | otherwise =
          let mid = low + ((high - low) `quot` 2)
              midDesc = arRows targetRows VU.! mid
           in case
                rowCompareAcross
                  (arPayload candidateRows)
                  candidateDesc
                  (arPayload targetRows)
                  midDesc
                of
                LT -> binaryIndex low mid
                GT -> binaryIndex (mid + 1) high
                EQ -> Just mid
{-# INLINE rowBlockRowIndex #-}

rowBlockRowIndices :: RowBlock 'Canonical -> RowBlock state -> IntSet
rowBlockRowIndices targetRows maskRows =
  foldRowBlock
    ( \indices desc ->
        case rowBlockRowIndex indexedTarget maskRows desc of
          Nothing -> indices
          Just indexValue -> IntSet.insert indexValue indices
    )
    IntSet.empty
    maskRows
  where
    indexedTarget =
      withRowBlockIndex targetRows
{-# INLINE rowBlockRowIndices #-}

containsProjectedRow :: RowBlock 'Canonical -> Int -> (Int -> Word64) -> Bool
containsProjectedRow targetRows lengthSlots slotAt
  | lengthSlots < 0 = False
  | otherwise =
      case arIndex targetRows of
        Just indexValue -> indexedContains indexValue
        Nothing -> binaryContains 0 (VU.length (arRows targetRows))
  where
    projectedHash = hashRowFromSlots (arLayoutHash targetRows) lengthSlots slotAt

    indexedContains (RowBlockIndex buckets) =
      case HashMap.lookup projectedHash buckets of
        Nothing -> False
        Just ordinals -> VU.any ordinalMatches ordinals

    ordinalMatches ordinal
      | ordinal < 0 = False
      | ordinal >= VU.length (arRows targetRows) = False
      | otherwise =
          compareProjectedWithDesc
            projectedHash
            lengthSlots
            slotAt
            (arPayload targetRows)
            (arRows targetRows VU.! ordinal)
            == EQ

    binaryContains low high
      | low >= high = False
      | otherwise =
          let mid = low + ((high - low) `quot` 2)
              midDesc = arRows targetRows VU.! mid
           in case compareProjectedWithDesc projectedHash lengthSlots slotAt (arPayload targetRows) midDesc of
                LT -> binaryContains low mid
                GT -> binaryContains (mid + 1) high
                EQ -> True

containsRestrictedRow ::
  RowRestrictionProgram ->
  RowBlock 'Canonical ->
  RowBlock sourceState ->
  RowDesc ->
  Bool
containsRestrictedRow program targetRows sourceRows sourceDesc =
  arLayoutHash targetRows == rrpTargetLayoutHash program
    && arLayout targetRows == rrpTargetLayout program
    && ardLengthSlots sourceDesc == VB.length (rrpTargetLayout program)
    && containsProjectedRow
      targetRows
      (ardLengthSlots sourceDesc)
      (\slotIndex -> restrictSlotWord program (rowSlotUnchecked sourceRows sourceDesc slotIndex))

buildRowBlockIndex :: RowBlock 'Canonical -> RowBlockIndex
buildRowBlockIndex rows =
  RowBlockIndex
    ( HashMap.map
        (VU.fromList . reverse)
        ( VU.ifoldl'
            (\buckets ordinal desc -> HashMap.insertWith (<>) (ardHash64 desc) [ordinal] buckets)
            HashMap.empty
            (arRows rows)
        )
    )

withRowBlockIndex :: RowBlock 'Canonical -> RowBlock 'Canonical
withRowBlockIndex rows = rows {arIndex = Just (buildRowBlockIndex rows)}

ensurePayloadCapacity :: RowBlockBuilder s -> Int -> ST s (MutablePrimArray s Word64)
ensurePayloadCapacity builder neededCapacity = do
  payloadArray <- readMutVar (arbPayloadRef builder)
  currentCapacity <- getSizeofMutablePrimArray payloadArray
  if neededCapacity <= currentCapacity
    then pure payloadArray
    else do
      payloadLengthValue <- readMutVar (arbPayloadLength builder)
      let nextCapacity = growCapacity currentCapacity neededCapacity
      nextArray <- newPrimArray nextCapacity
      copyMutablePrimArray nextArray 0 payloadArray 0 payloadLengthValue
      writeMutVar (arbPayloadRef builder) nextArray
      pure nextArray

ensureRowsCapacity :: RowBlockBuilder s -> Int -> ST s (VUM.MVector s RowDesc)
ensureRowsCapacity builder neededCapacity = do
  rowVector <- readMutVar (arbRowsRef builder)
  let currentCapacity = VUM.length rowVector
  if neededCapacity <= currentCapacity
    then pure rowVector
    else do
      rowLengthValue <- readMutVar (arbRowLength builder)
      let nextCapacity = growCapacity currentCapacity neededCapacity
      nextVector <-
        VUM.unsafeGrow
          (VUM.slice 0 rowLengthValue rowVector)
          (nextCapacity - rowLengthValue)
      writeMutVar (arbRowsRef builder) nextVector
      pure nextVector

growCapacity :: Int -> Int -> Int
growCapacity currentCapacity neededCapacity = go (max 8 currentCapacity)
  where
    go capacity
      | capacity >= neededCapacity = capacity
      | capacity > maxBound `quot` 2 = neededCapacity
      | otherwise = go (capacity * 2)

appendDescFromSameSchemaPayload :: RowBlockBuilder s -> PrimArray Word64 -> RowDesc -> ST s Bool
appendDescFromSameSchemaPayload builder payload desc
  | ardLengthSlots desc < 0 = pure False
  | otherwise = do
      payloadCursor <- readMutVar (arbPayloadLength builder)
      rowCursor <- readMutVar (arbRowLength builder)
      let !wordCount = rowPayloadWords desc
      payloadArray <- ensurePayloadCapacity builder (payloadCursor + wordCount)
      rowVector <- ensureRowsCapacity builder (rowCursor + 1)
      copyPrimArray payloadArray payloadCursor payload (ardOffsetWords desc) wordCount
      VUM.write rowVector rowCursor desc {ardOffsetWords = payloadCursor}
      writeMutVar (arbPayloadLength builder) (payloadCursor + wordCount)
      writeMutVar (arbRowLength builder) (rowCursor + 1)
      pure True

{-# INLINE appendDescFromSameSchemaPayload #-}

rowPayloadWords :: RowDesc -> Int
rowPayloadWords desc =
  ardLengthSlots desc + 1

{-# INLINE rowPayloadWords #-}

appendRemaining :: RowBlockBuilder s -> RowBlock 'Canonical -> Int -> ST s ()
appendRemaining builder rows = go
  where
    rowCount = VU.length (arRows rows)

    go indexValue
      | indexValue >= rowCount = pure ()
      | otherwise = do
          _ <- appendDescFromSameSchemaPayload builder (arPayload rows) (arRows rows VU.! indexValue)
          go (indexValue + 1)

ensureSameSchema :: RowBlock left -> RowBlock right -> Either RowOperationError ()
ensureSameSchema leftRows rightRows =
  if arLayoutHash leftRows == arLayoutHash rightRows
      && arLayout leftRows == arLayout rightRows
    then Right ()
    else Left (RowLayoutMismatch (arLayoutHash leftRows) (arLayoutHash rightRows))

ensureRestrictionWidth :: RowRestrictionProgram -> RowBlock state -> Either RowOperationError ()
ensureRestrictionWidth program sourceRows =
  if sourceWidth == targetWidth
    then Right ()
    else Left (RowRestrictionWidthMismatch sourceWidth targetWidth)
  where
    sourceWidth =
      VB.length (arLayout sourceRows)
    targetWidth =
      VB.length (rrpTargetLayout program)

rowCompareInPayload :: PrimArray Word64 -> RowDesc -> RowDesc -> Ordering
rowCompareInPayload payload leftDesc rightDesc =
  case compare (ardHash64 leftDesc) (ardHash64 rightDesc) of
    EQ ->
      case compare (ardLengthSlots leftDesc) (ardLengthSlots rightDesc) of
        EQ -> compareSlotsInPayload payload leftDesc payload rightDesc
        orderValue -> orderValue
    orderValue -> orderValue

rowCompareAcross :: PrimArray Word64 -> RowDesc -> PrimArray Word64 -> RowDesc -> Ordering
rowCompareAcross leftPayload leftDesc rightPayload rightDesc =
  case compare (ardHash64 leftDesc) (ardHash64 rightDesc) of
    EQ ->
      case compare (ardLengthSlots leftDesc) (ardLengthSlots rightDesc) of
        EQ -> compareSlotsInPayload leftPayload leftDesc rightPayload rightDesc
        orderValue -> orderValue
    orderValue -> orderValue

rowEqualInPayload :: PrimArray Word64 -> RowDesc -> RowDesc -> Bool
rowEqualInPayload payload leftDesc rightDesc =
  ardHash64 leftDesc == ardHash64 rightDesc
    && ardLengthSlots leftDesc == ardLengthSlots rightDesc
    && slotsEqualInPayload payload leftDesc payload rightDesc

rowEqualAcross :: PrimArray Word64 -> RowDesc -> PrimArray Word64 -> RowDesc -> Bool
rowEqualAcross leftPayload leftDesc rightPayload rightDesc =
  ardHash64 leftDesc == ardHash64 rightDesc
    && ardLengthSlots leftDesc == ardLengthSlots rightDesc
    && slotsEqualInPayload leftPayload leftDesc rightPayload rightDesc

compareSlotsInPayload ::
  PrimArray Word64 ->
  RowDesc ->
  PrimArray Word64 ->
  RowDesc ->
  Ordering
compareSlotsInPayload leftPayload leftDesc rightPayload rightDesc = go 0
  where
    lengthSlots = ardLengthSlots leftDesc

    go slotIndex
      | slotIndex >= lengthSlots = EQ
      | otherwise =
          let !leftWord = indexPrimArray leftPayload (ardOffsetWords leftDesc + 1 + slotIndex)
              !rightWord = indexPrimArray rightPayload (ardOffsetWords rightDesc + 1 + slotIndex)
           in case compare leftWord rightWord of
                EQ -> go (slotIndex + 1)
                orderValue -> orderValue

slotsEqualInPayload ::
  PrimArray Word64 ->
  RowDesc ->
  PrimArray Word64 ->
  RowDesc ->
  Bool
slotsEqualInPayload leftPayload leftDesc rightPayload rightDesc = go 0
  where
    lengthSlots = ardLengthSlots leftDesc

    go slotIndex
      | slotIndex >= lengthSlots = True
      | otherwise =
          let !leftWord = indexPrimArray leftPayload (ardOffsetWords leftDesc + 1 + slotIndex)
              !rightWord = indexPrimArray rightPayload (ardOffsetWords rightDesc + 1 + slotIndex)
           in leftWord == rightWord && go (slotIndex + 1)

compareProjectedWithDesc :: Word64 -> Int -> (Int -> Word64) -> PrimArray Word64 -> RowDesc -> Ordering
compareProjectedWithDesc projectedHash lengthSlots slotAt payload desc =
  case compare projectedHash (ardHash64 desc) of
    EQ ->
      case compare lengthSlots (ardLengthSlots desc) of
        EQ -> compareProjectedSlots 0
        orderValue -> orderValue
    orderValue -> orderValue
  where
    compareProjectedSlots slotIndex
      | slotIndex >= lengthSlots = EQ
      | otherwise =
          let !leftWord = slotAt slotIndex
              !rightWord = indexPrimArray payload (ardOffsetWords desc + 1 + slotIndex)
           in case compare leftWord rightWord of
                EQ -> compareProjectedSlots (slotIndex + 1)
                orderValue -> orderValue

rowSlotUnchecked :: RowBlock state -> RowDesc -> Int -> Word64
rowSlotUnchecked rows desc slotIndex =
  indexPrimArray (arPayload rows) (ardOffsetWords desc + 1 + slotIndex)

hashRowFromSlots :: Word64 -> Int -> (Int -> Word64) -> Word64
hashRowFromSlots layoutHashValue lengthSlots slotAt =
  go 0 (mixWord64 layoutHashValue (fromIntegral lengthSlots))
  where
    go slotIndex hashValue
      | slotIndex >= lengthSlots = hashValue
      | otherwise = go (slotIndex + 1) (mixWord64 hashValue (slotAt slotIndex))

validateRestrictionMap :: RestrictionMap -> Either RowProgramError ()
validateRestrictionMap =
  IntMap.foldlWithKey'
    ( \acc sourceKey targetKey ->
        acc
          *> if sourceKey < 0
            then Left (NegativeRestrictionKey sourceKey)
            else
              if targetKey < 0
                then Left (NegativeRestrictionValue sourceKey targetKey)
                else Right ()
    )
    (Right ())

denseRestrictionFrom :: RestrictionMap -> Maybe DenseRestriction
denseRestrictionFrom restrictionMapValue =
  case (IntMap.lookupMin restrictionMapValue, IntMap.lookupMax restrictionMapValue) of
    (Just (minKey, _), Just (maxKey, _)) ->
      let spanWidth = maxKey - minKey + 1
          entryCount = IntMap.size restrictionMapValue
       in if
              spanWidth > 0
                && spanWidth <= maxDenseRestrictionWidth
                && entryCount * 4 >= spanWidth
            then
              Just
                DenseRestriction
                  { drBaseKey = minKey,
                    drValues =
                      VU.generate
                        spanWidth
                        (\offset -> let key = minKey + offset in IntMap.findWithDefault key key restrictionMapValue)
                  }
            else Nothing
    _ -> Nothing

lookupRestrictedSlot :: RowRestrictionProgram -> Int -> Int
lookupRestrictedSlot program slotKey = case rrpDenseRestriction program of
  Just denseRestriction
    | slotKey >= drBaseKey denseRestriction ->
        let offset = slotKey - drBaseKey denseRestriction
         in if offset < VU.length (drValues denseRestriction)
              then drValues denseRestriction VU.! offset
              else sparseLookup
  _ -> sparseLookup
  where
    sparseLookup = IntMap.findWithDefault slotKey slotKey (rrpRestrictionMap program)

restrictionFingerprint :: RowLayout -> RestrictionMap -> Word64
restrictionFingerprint targetLayout = IntMap.foldlWithKey'
    ( \hashValue sourceKey targetKey ->
        mixWord64 (mixWord64 hashValue (fromIntegral sourceKey)) (fromIntegral targetKey)
    )
    (layoutHash targetLayout)

word64ToInt :: Word64 -> Maybe Int
word64ToInt wordValue
  | wordValue <= maxSlotKeyWord64 = Just (fromIntegral wordValue)
  | otherwise = Nothing

{-# INLINE word64ToInt #-}

maxSlotKeyWord64 :: Word64
maxSlotKeyWord64 =
  fromIntegral (maxBound :: Int)

mixWord64 :: Word64 -> Word64 -> Word64
mixWord64 !hashValue !wordValue =
  avalanche64 (hashValue `xor` (wordValue + hashStepSalt))

{-# INLINE mixWord64 #-}

avalanche64 :: Word64 -> Word64
avalanche64 !word0 =
  let !word1 = (word0 `xor` (word0 `shiftR` 33)) * avalancheMul1
      !word2 = (word1 `xor` (word1 `shiftR` 33)) * avalancheMul2
   in word2 `xor` (word2 `shiftR` 33)

{-# INLINE avalanche64 #-}

initialLayoutHash :: Word64
initialLayoutHash = 0x6a09e667f3bcc909

hashStepSalt :: Word64
hashStepSalt = 0x9e3779b97f4a7c15

avalancheMul1 :: Word64
avalancheMul1 = 0xff51afd7ed558ccd

avalancheMul2 :: Word64
avalancheMul2 = 0xc4ceb9fe1a85ec53

word64Bytes :: Int
word64Bytes = 8

atomRowDescBytes :: Int
atomRowDescBytes = 24

maxDenseRestrictionWidth :: Int
maxDenseRestrictionWidth = 1048576
