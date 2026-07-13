{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Derived.Pure.LinAlg.LabeledMatrixSparse
  ( blockedFromLabeledSparseRows
  ) where

import Control.Monad (foldM)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Moonlight.Algebra (IntegralDomain (isZero))
import Moonlight.Core (MoonlightError (..))
import Moonlight.Core (scanMap)
import Moonlight.Derived.Pure.LinAlg.SparseEchelon
  ( SparseRow
  , sparseRowEntries
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( BlockedMat (..)
  , DenseMat (..)
  , GroupedAxis
  , axisMultiplicity
  , fromLabels
  )
import Moonlight.Derived.Pure.Site.Poset (FinObjectId (..))

type AxisPlacement :: Type
data AxisPlacement = AxisPlacement !FinObjectId !Int

type BlockEntryMap :: Type -> Type
type BlockEntryMap a = IntMap (IntMap a)

type SparseBlockMap :: Type -> Type
type SparseBlockMap a = IntMap (IntMap (BlockEntryMap a))

blockedFromLabeledSparseRows ::
  (IntegralDomain a, Num a) =>
  Vector FinObjectId ->
  Vector (FinObjectId, SparseRow a) ->
  Either MoonlightError (BlockedMat a)
blockedFromLabeledSparseRows columnLabels labeledRows = do
  accumulatedBlocks <-
    foldM
      insertSparseRow
      IntMap.empty
      (V.toList (V.indexed labeledRows))
  Right
    BlockedMat
      { bmRows = rowAxis
      , bmCols = columnAxis
      , bmBlocks =
          finishSparseBlocks
            rowAxis
            columnAxis
            accumulatedBlocks
      }
  where
    rowLabels =
      V.map fst labeledRows
    rowAxis =
      fromLabels rowLabels
    columnAxis =
      fromLabels columnLabels
    rowPlacements =
      axisPlacements rowLabels
    columnPlacements =
      axisPlacements columnLabels

    insertSparseRow
      accumulatedBlocks
      (rowIndexValue, (_, rowValue)) = do
        rowPlacement <-
          lookupPlacement
            "blockedFromLabeledSparseRows: row"
            rowIndexValue
            rowPlacements
        IntMap.foldlWithKey'
          (insertSparseEntry rowPlacement)
          (Right accumulatedBlocks)
          (sparseRowEntries rowValue)

    insertSparseEntry
      rowPlacement
      accumulatedEither
      columnIndexValue
      coefficientValue = do
        accumulated <-
          accumulatedEither
        columnPlacement <-
          lookupPlacement
            "blockedFromLabeledSparseRows: column"
            columnIndexValue
            columnPlacements
        Right
          ( insertBlockCoefficient
              rowPlacement
              columnPlacement
              coefficientValue
              accumulated
          )

axisPlacements :: Vector FinObjectId -> Vector AxisPlacement
axisPlacements labels =
  snd (scanMap placeLabel IntMap.empty labels)
  where
    placeLabel localCounts objectValue@(FinObjectId objectKey) =
      let !localIndex =
            IntMap.findWithDefault 0 objectKey localCounts
       in ( IntMap.insert
              objectKey
              (localIndex + 1)
              localCounts
          , AxisPlacement objectValue localIndex
          )

lookupPlacement ::
  String ->
  Int ->
  Vector AxisPlacement ->
  Either MoonlightError AxisPlacement
lookupPlacement context indexValue placements =
  case placements V.!? indexValue of
    Just placementValue ->
      Right placementValue
    Nothing ->
      Left
        ( InvariantViolation
            ( context
                <> ": index "
                <> show indexValue
                <> " is out of bounds for axis cardinality "
                <> show (V.length placements)
            )
        )

insertBlockCoefficient ::
  (IntegralDomain a, Num a) =>
  AxisPlacement ->
  AxisPlacement ->
  a ->
  SparseBlockMap a ->
  SparseBlockMap a
insertBlockCoefficient
  (AxisPlacement (FinObjectId rowKey) localRowIndex)
  (AxisPlacement (FinObjectId columnKey) localColumnIndex)
  coefficientValue
  blockMap
  | isZero coefficientValue =
      blockMap
  | otherwise =
      IntMap.alter updateBlockRow rowKey blockMap
  where
    updateBlockRow Nothing =
      Just
        ( IntMap.singleton
            columnKey
            ( singletonBlockEntry
                localRowIndex
                localColumnIndex
                coefficientValue
            )
        )
    updateBlockRow (Just blockRow) =
      nonEmptyIntMap
        (IntMap.alter updateBlock columnKey blockRow)

    updateBlock Nothing =
      Just
        ( singletonBlockEntry
            localRowIndex
            localColumnIndex
            coefficientValue
        )
    updateBlock (Just blockEntries) =
      nonEmptyIntMap
        ( insertAccumulatedEntry
            localRowIndex
            localColumnIndex
            coefficientValue
            blockEntries
        )

singletonBlockEntry ::
  Int ->
  Int ->
  a ->
  BlockEntryMap a
singletonBlockEntry rowIndexValue columnIndexValue coefficientValue =
  IntMap.singleton
    rowIndexValue
    (IntMap.singleton columnIndexValue coefficientValue)

insertAccumulatedEntry ::
  (IntegralDomain a, Num a) =>
  Int ->
  Int ->
  a ->
  BlockEntryMap a ->
  BlockEntryMap a
insertAccumulatedEntry rowIndexValue columnIndexValue coefficientValue =
  IntMap.alter updateRow rowIndexValue
  where
    updateRow Nothing =
      Just
        (IntMap.singleton columnIndexValue coefficientValue)
    updateRow (Just rowEntries) =
      nonEmptyIntMap
        (IntMap.alter updateColumn columnIndexValue rowEntries)

    updateColumn Nothing =
      Just coefficientValue
    updateColumn (Just oldValue) =
      let !nextValue =
            oldValue + coefficientValue
       in if isZero nextValue
            then Nothing
            else Just nextValue

nonEmptyIntMap :: IntMap value -> Maybe (IntMap value)
nonEmptyIntMap mapValue
  | IntMap.null mapValue =
      Nothing
  | otherwise =
      Just mapValue

finishSparseBlocks ::
  Num a =>
  GroupedAxis ->
  GroupedAxis ->
  SparseBlockMap a ->
  IntMap (IntMap (DenseMat a))
finishSparseBlocks rowAxis columnAxis =
  IntMap.mapMaybeWithKey finishBlockRow
  where
    finishBlockRow rowKey blockRow =
      let rowNode =
            FinObjectId rowKey
          rowCount =
            axisMultiplicity rowAxis rowNode
          finishedRow =
            IntMap.mapMaybeWithKey
              (finishBlock rowCount)
              blockRow
       in if rowCount <= 0 || IntMap.null finishedRow
            then Nothing
            else Just finishedRow

    finishBlock rowCount columnKey blockEntries =
      let columnNode =
            FinObjectId columnKey
          columnCount =
            axisMultiplicity columnAxis columnNode
       in if columnCount <= 0 || IntMap.null blockEntries
            then Nothing
            else
              Just
                DenseMat
                  { dmRows = rowCount
                  , dmCols = columnCount
                  , dmData =
                      V.generate
                        rowCount
                        ( \rowIndexValue ->
                            V.generate
                              columnCount
                              ( \columnIndexValue ->
                                  IntMap.findWithDefault
                                    0
                                    columnIndexValue
                                    ( IntMap.findWithDefault
                                        IntMap.empty
                                        rowIndexValue
                                        blockEntries
                                    )
                              )
                        )
                  }
