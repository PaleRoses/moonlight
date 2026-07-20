{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Derived.Pure.Functor.Tensor.BlockAssembly
  ( BlockAssembly
  , LayoutBlockIndex
  , layoutBlockIndex
  , emptyBlockAssembly
  , finishBlockAssembly
  , contractIsolatedDiagonalAssembly
  , blockAssemblyDiagonalNodes
  , addScaledSparseMatAt
  , addScaledBlockAt
  , blockedBlockCount
  , blockedBlockCellCount
  , blockedBlockNonZeroCount
  ) where

import Control.Monad (foldM)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Moonlight.Core (Field, MoonlightError (..))
import Moonlight.Derived.Pure.LinAlg.Rank (rankSparseDefault)
import Moonlight.Derived.Pure.Functor.Tensor.Layout
  ( DegreeLayout (..)
  , sumVector
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( BlockedMat (..)
  , DenseMat (..)
  , GroupedAxis
  , SparseMat (..)
  , SparseMatrixEntry (..)
  , axisMultiplicity
  , denseToSparseMat
  , fromLabels
  , removeAxisIndices
  , zeroBlocked
  )
import Moonlight.Derived.Pure.Site.Poset
  ( FinObjectId (..)
  )

type BlockCellPlacement :: Type
data BlockCellPlacement = BlockCellPlacement
  { bcpNode :: !FinObjectId
  , bcpIndex :: !Int
  }

type LayoutBlockIndex :: Type
data LayoutBlockIndex = LayoutBlockIndex
  { lbiAxis :: !GroupedAxis
  , lbiPlacements :: !(Vector BlockCellPlacement)
  }

type BlockEntryMap :: Type -> Type
type BlockEntryMap a = IntMap.IntMap (IntMap.IntMap a)

type BlockAssembly :: Type -> Type
data BlockAssembly a = BlockAssembly
  { baRows :: !GroupedAxis
  , baCols :: !GroupedAxis
  , baRowIndex :: !LayoutBlockIndex
  , baColumnIndex :: !LayoutBlockIndex
  , baEntries :: !(IntMap.IntMap (IntMap.IntMap (BlockEntryMap a)))
  , baDiagonalNodes :: !IntSet.IntSet
  }

denseCellCount :: DenseMat a -> Int
denseCellCount DenseMat {dmRows, dmCols} =
  dmRows * dmCols

denseNonZeroCount :: (Eq a, Num a) => DenseMat a -> Int
denseNonZeroCount DenseMat {dmData} =
  sumVector (V.map (V.length . V.filter (/= 0)) dmData)

blockedBlockCount :: BlockedMat a -> Int
blockedBlockCount BlockedMat {bmBlocks} =
  IntMap.foldl' (\rowTotal rowBlocks -> rowTotal + IntMap.size rowBlocks) 0 bmBlocks

blockedBlockCellCount :: BlockedMat a -> Int
blockedBlockCellCount BlockedMat {bmBlocks} =
  IntMap.foldl'
    (\rowTotal rowBlocks -> rowTotal + IntMap.foldl' (\blockTotal denseValue -> blockTotal + denseCellCount denseValue) 0 rowBlocks)
    0
    bmBlocks

blockedBlockNonZeroCount :: (Eq a, Num a) => BlockedMat a -> Int
blockedBlockNonZeroCount BlockedMat {bmBlocks} =
  IntMap.foldl'
    (\rowTotal rowBlocks -> rowTotal + IntMap.foldl' (\blockTotal denseValue -> blockTotal + denseNonZeroCount denseValue) 0 rowBlocks)
    0
    bmBlocks

layoutBlockIndex :: DegreeLayout -> LayoutBlockIndex
layoutBlockIndex DegreeLayout {dlLabels} =
  let (_, reversedPlacements) =
        V.foldl'
          placeCell
          (IntMap.empty, [])
          dlLabels
   in
  LayoutBlockIndex
    { lbiAxis = fromLabels dlLabels
    , lbiPlacements = V.fromList (reverse reversedPlacements)
    }
  where
    placeCell (counts, placements) nodeValue@(FinObjectId nodeKey) =
      let !cellIndex = IntMap.findWithDefault 0 nodeKey counts
       in ( IntMap.insert nodeKey (cellIndex + 1) counts
          , BlockCellPlacement nodeValue cellIndex : placements
          )

emptyBlockAssembly :: LayoutBlockIndex -> LayoutBlockIndex -> BlockAssembly a
emptyBlockAssembly rowIndex columnIndex =
  BlockAssembly
    { baRows = lbiAxis rowIndex
    , baCols = lbiAxis columnIndex
    , baRowIndex = rowIndex
    , baColumnIndex = columnIndex
    , baEntries = IntMap.empty
    , baDiagonalNodes = IntSet.empty
    }

finishBlockAssembly :: Num a => BlockAssembly a -> BlockedMat a
finishBlockAssembly BlockAssembly {baRows, baCols, baEntries} =
  BlockedMat
    { bmRows = baRows
    , bmCols = baCols
    , bmBlocks = IntMap.mapMaybeWithKey finishRow baEntries
    }
  where
    finishRow rowKey rowEntries =
      let rowNode = FinObjectId rowKey
          rowCount = axisMultiplicity baRows rowNode
          blocks =
            IntMap.mapMaybeWithKey
              (finishBlock rowCount)
              rowEntries
       in if rowCount <= 0 || IntMap.null blocks
            then Nothing
            else Just blocks

    finishBlock rowCount columnKey blockEntries =
      let columnNode = FinObjectId columnKey
          columnCount = axisMultiplicity baCols columnNode
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
                                    (IntMap.findWithDefault IntMap.empty rowIndexValue blockEntries)
                              )
                        )
                  }

contractIsolatedDiagonalAssembly ::
  forall a.
  (Eq a, Field a, Num a) =>
  BlockAssembly a ->
  Either MoonlightError (Maybe (BlockedMat a))
contractIsolatedDiagonalAssembly assembly@BlockAssembly {baRows, baCols} =
  case isolatedDiagonalBlocks assembly of
    Nothing ->
      Right Nothing
    Just diagonalBlocks -> do
      diagonalRanks <-
        traverse
          rankDiagonalAssemblyBlock
          diagonalBlocks
      let contractedRows =
            foldl' removeDiagonalRank baRows diagonalRanks
          contractedCols =
            foldl' removeDiagonalRank baCols diagonalRanks
      Right (Just (zeroBlocked contractedRows contractedCols))

type DiagonalAssemblyBlock :: Type -> Type
data DiagonalAssemblyBlock a = DiagonalAssemblyBlock !FinObjectId !Int !Int !(BlockEntryMap a)

type DiagonalRank :: Type
data DiagonalRank = DiagonalRank !FinObjectId !Int

isolatedDiagonalBlocks ::
  BlockAssembly a ->
  Maybe [DiagonalAssemblyBlock a]
isolatedDiagonalBlocks BlockAssembly {baRows, baCols, baEntries} =
  traverse diagonalRow (IntMap.toAscList baEntries)
  where
    diagonalRow (rowKey, rowEntries) =
      let nodeValue =
            FinObjectId rowKey
          rowCount =
            axisMultiplicity baRows nodeValue
          columnCount =
            axisMultiplicity baCols nodeValue
       in
      case IntMap.toAscList rowEntries of
        [] ->
          Just (DiagonalAssemblyBlock nodeValue rowCount columnCount IntMap.empty)
        [(columnKey, blockEntries)]
          | columnKey == rowKey ->
              Just (DiagonalAssemblyBlock nodeValue rowCount columnCount blockEntries)
        _ ->
          Nothing

rankDiagonalAssemblyBlock ::
  forall a.
  (Eq a, Field a, Num a) =>
  DiagonalAssemblyBlock a ->
  Either MoonlightError DiagonalRank
rankDiagonalAssemblyBlock (DiagonalAssemblyBlock nodeValue rowCount columnCount blockEntries) =
  DiagonalRank nodeValue <$> rankBlockEntries rowCount columnCount blockEntries

rankBlockEntries ::
  forall a.
  (Eq a, Field a, Num a) =>
  Int ->
  Int ->
  BlockEntryMap a ->
  Either MoonlightError Int
rankBlockEntries rowCount columnCount blockEntries =
  rankSparseDefault (sparseMatFromBlockEntries rowCount columnCount blockEntries)

sparseMatFromBlockEntries ::
  Int ->
  Int ->
  BlockEntryMap a ->
  SparseMat a
sparseMatFromBlockEntries rowCount columnCount blockEntries =
  SparseMat
    { smRows = rowCount
    , smCols = columnCount
    , smEntries =
        [ SparseMatrixEntry
            { smeRow = rowIndexValue
            , smeColumn = columnIndexValue
            , smeValue = coefficientValue
            }
        | (rowIndexValue, rowEntries) <- IntMap.toAscList blockEntries
        , (columnIndexValue, coefficientValue) <- IntMap.toAscList rowEntries
        ]
    }

removeDiagonalRank :: GroupedAxis -> DiagonalRank -> GroupedAxis
removeDiagonalRank axisValue (DiagonalRank nodeValue rankValue) =
  removeAxisIndices
    nodeValue
    [0 .. min rankValue (axisMultiplicity axisValue nodeValue) - 1]
    axisValue

validateDenseShape :: String -> DenseMat a -> Either MoonlightError ()
validateDenseShape context DenseMat {dmRows, dmCols, dmData}
  | dmRows /= V.length dmData =
      Left
        ( InvariantViolation
            ( context
                <> ": dense row metadata mismatch "
                <> show (dmRows, V.length dmData)
            )
        )
  | not (V.all ((== dmCols) . V.length) dmData) =
      Left (InvariantViolation (context <> ": dense column metadata mismatch"))
  | otherwise =
      Right ()

blockAssemblyDiagonalNodes :: BlockAssembly a -> [FinObjectId]
blockAssemblyDiagonalNodes BlockAssembly {baDiagonalNodes} =
  fmap FinObjectId (IntSet.toAscList baDiagonalNodes)

addScaledBlockAt ::
  (Eq a, Num a) =>
  String ->
  Int ->
  Int ->
  a ->
  DenseMat a ->
  BlockAssembly a ->
  Either MoonlightError (BlockAssembly a)
addScaledBlockAt context rowOffsetValue columnOffsetValue coefficientValue blockValue accumulated =
  validateDenseShape context blockValue
    *> addScaledSparseMatAt
      context
      rowOffsetValue
      columnOffsetValue
      coefficientValue
      (denseToSparseMat blockValue)
      accumulated

addScaledSparseMatAt ::
  (Eq a, Num a) =>
  String ->
  Int ->
  Int ->
  a ->
  SparseMat a ->
  BlockAssembly a ->
  Either MoonlightError (BlockAssembly a)
addScaledSparseMatAt context rowOffsetValue columnOffsetValue coefficientValue sparseValue accumulated
  | rowOffsetValue < 0 || columnOffsetValue < 0 =
      Left
        ( InvariantViolation
            ( context
                <> ": negative embedding offset "
                <> show (rowOffsetValue, columnOffsetValue)
            )
        )
  | otherwise = do
      rowPlacements <-
        placementSlice
          (context <> ": row placement")
          rowOffsetValue
          (smRows sparseValue)
          (baRowIndex accumulated)
      columnPlacements <-
        placementSlice
          (context <> ": column placement")
          columnOffsetValue
          (smCols sparseValue)
          (baColumnIndex accumulated)
      (nextEntries, diagonalNodes) <-
        foldM
          (insertSparseEntry rowPlacements columnPlacements)
          (baEntries accumulated, IntSet.empty)
          (smEntries sparseValue)
      Right
        accumulated
          { baEntries =
              nextEntries
          , baDiagonalNodes =
              IntSet.union (baDiagonalNodes accumulated) diagonalNodes
          }
  where
    insertSparseEntry rowPlacements columnPlacements (entries, diagonalNodes) SparseMatrixEntry {smeRow, smeColumn, smeValue} = do
      rowPlacement <-
        lookupPlacement
          (context <> ": sparse row placement")
          smeRow
          rowPlacements
      columnPlacement <-
        lookupPlacement
          (context <> ": sparse column placement")
          smeColumn
          columnPlacements
      let rawValue = smeValue
          !entryValue = coefficientValue * rawValue
      if entryValue == 0
        then Right (entries, diagonalNodes)
        else
          Right
            ( insertAccumulatedBlockEntry rowPlacement columnPlacement entryValue entries
            , insertDiagonalNode rowPlacement columnPlacement diagonalNodes
            )

lookupPlacement ::
  String ->
  Int ->
  Vector BlockCellPlacement ->
  Either MoonlightError BlockCellPlacement
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
                <> " is out of bounds for placement count "
                <> show (V.length placements)
            )
        )

insertDiagonalNode :: BlockCellPlacement -> BlockCellPlacement -> IntSet.IntSet -> IntSet.IntSet
insertDiagonalNode leftPlacement rightPlacement diagonalNodes
  | bcpNode leftPlacement == bcpNode rightPlacement =
      IntSet.insert (unFinObjectId (bcpNode leftPlacement)) diagonalNodes
  | otherwise =
      diagonalNodes

placementSlice ::
  String ->
  Int ->
  Int ->
  LayoutBlockIndex ->
  Either MoonlightError (Vector BlockCellPlacement)
placementSlice context offsetValue lengthValue LayoutBlockIndex {lbiPlacements}
  | offsetValue < 0 || lengthValue < 0 =
      Left
        ( InvariantViolation
            ( context
                <> ": negative slice "
                <> show (offsetValue, lengthValue)
            )
        )
  | offsetValue + lengthValue > V.length lbiPlacements =
      Left
        ( InvariantViolation
            ( context
                <> ": slice exceeds layout length "
                <> show (offsetValue, lengthValue, V.length lbiPlacements)
            )
        )
  | otherwise =
      Right (V.slice offsetValue lengthValue lbiPlacements)

insertAccumulatedBlockEntry ::
  (Eq a, Num a) =>
  BlockCellPlacement ->
  BlockCellPlacement ->
  a ->
  IntMap.IntMap (IntMap.IntMap (BlockEntryMap a)) ->
  IntMap.IntMap (IntMap.IntMap (BlockEntryMap a))
insertAccumulatedBlockEntry
  BlockCellPlacement {bcpNode = FinObjectId rowKey, bcpIndex = rowIndexValue}
  BlockCellPlacement {bcpNode = FinObjectId columnKey, bcpIndex = columnIndexValue}
  entryValue =
  IntMap.alter updateRow rowKey
  where
    updateRow Nothing =
      Just (IntMap.singleton columnKey (singletonBlockEntry rowIndexValue columnIndexValue entryValue))
    updateRow (Just rowMap) =
      nonEmptyIntMap (IntMap.alter updateBlock columnKey rowMap)

    updateBlock Nothing =
      Just (singletonBlockEntry rowIndexValue columnIndexValue entryValue)
    updateBlock (Just entries) =
      nonEmptyIntMap (insertAccumulatedEntry rowIndexValue columnIndexValue entryValue entries)

singletonBlockEntry :: Int -> Int -> a -> BlockEntryMap a
singletonBlockEntry rowIndexValue columnIndexValue entryValue =
  IntMap.singleton rowIndexValue (IntMap.singleton columnIndexValue entryValue)

insertAccumulatedEntry ::
  (Eq a, Num a) =>
  Int ->
  Int ->
  a ->
  BlockEntryMap a ->
  BlockEntryMap a
insertAccumulatedEntry rowIndexValue columnIndexValue entryValue entries =
  IntMap.alter updateRow rowIndexValue entries
  where
    updateRow Nothing =
      Just (IntMap.singleton columnIndexValue entryValue)
    updateRow (Just rowEntries) =
      nonEmptyIntMap (insertAccumulatedColumnEntry columnIndexValue entryValue rowEntries)

insertAccumulatedColumnEntry ::
  (Eq a, Num a) =>
  Int ->
  a ->
  IntMap.IntMap a ->
  IntMap.IntMap a
insertAccumulatedColumnEntry columnIndexValue entryValue entries =
  case IntMap.lookup columnIndexValue entries of
    Nothing ->
      IntMap.insert columnIndexValue entryValue entries
    Just currentValue ->
      let !nextValue = currentValue + entryValue
       in if nextValue == 0
            then IntMap.delete columnIndexValue entries
            else IntMap.insert columnIndexValue nextValue entries

nonEmptyIntMap :: IntMap.IntMap a -> Maybe (IntMap.IntMap a)
nonEmptyIntMap value
  | IntMap.null value = Nothing
  | otherwise = Just value
