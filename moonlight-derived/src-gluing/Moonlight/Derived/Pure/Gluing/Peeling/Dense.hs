{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Moonlight.Derived.Pure.Gluing.Peeling.Dense
  ( addScaledDenseRow
  , denseIndex
  , denseIsZero
  , denseMul
  , denseRowNonZeros
  , denseSub
  , identityDense
  , indexRange
  , indexRangeFromTo
  , lookupVector
  , replaceVector
  , selectDenseSubmatrix
  , setBlockedBlock
  , survivingIndices
  , swapDenseRows
  , swapVectorEntries
  , scaleDenseRow
  , validateDenseMat
  ) where

import Control.Monad (foldM)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Vector qualified as V
import Moonlight.Algebra (IntegralDomain (isZero))
import Moonlight.Core (MoonlightError (..))
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( BlockedMat (..)
  , DenseMat (..)
  , GroupedAxis
  , axisMultiplicity
  )
import Moonlight.Derived.Pure.Site.Poset (FinObjectId (..))

survivingIndices ::
  GroupedAxis ->
  FinObjectId ->
  V.Vector Int ->
  FinObjectId ->
  [Int]
survivingIndices axisValue pivotNode deletedIndices labelValue =
  if labelValue /= pivotNode
    then indexRange labelMultiplicity
    else
      let deletedSet =
            IntSet.fromList (V.toList deletedIndices)
       in filter
            (`IntSet.notMember` deletedSet)
            (indexRange labelMultiplicity)
  where
    labelMultiplicity =
      axisMultiplicity axisValue labelValue

selectDenseSubmatrix ::
  String ->
  DenseMat a ->
  [Int] ->
  [Int] ->
  Either MoonlightError (DenseMat a)
selectDenseSubmatrix context matrixValue rowIndices columnIndices = do
  rowVectors <-
    traverse
      buildRow
      rowIndices
  Right
    DenseMat
      { dmRows = length rowIndices
      , dmCols = length columnIndices
      , dmData = V.fromList rowVectors
      }
  where
    buildRow rowIndex =
      fmap V.fromList
        ( traverse
            (denseIndex context matrixValue rowIndex)
            columnIndices
        )

denseMul ::
  forall a.
  (Num a, IntegralDomain a) =>
  String ->
  DenseMat a ->
  DenseMat a ->
  Either MoonlightError (DenseMat a)
denseMul context leftMatrix rightMatrix
  | dmCols leftMatrix /= dmRows rightMatrix =
      Left
        ( InvariantViolation
            ( context
                <> ": incompatible dense multiplication shapes "
                <> show (dmRows leftMatrix, dmCols leftMatrix)
                <> " and "
                <> show (dmRows rightMatrix, dmCols rightMatrix)
            )
        )
  | otherwise = do
      validateDenseMat (context <> ": left sparse row") leftMatrix
      validateDenseMat (context <> ": right factor entry") rightMatrix
      let !resultRows =
            forceDenseRows
              ( V.generate
                  (dmRows leftMatrix)
                  buildProductRow
              )
      Right
        DenseMat
          { dmRows = dmRows leftMatrix
          , dmCols = dmCols rightMatrix
          , dmData = resultRows
          }
  where
    !leftRows =
      dmData leftMatrix

    !rightRows =
      dmData rightMatrix

    buildProductRow rowIndex =
      let !leftEntries =
            denseMulSparseEntries (leftRows V.! rowIndex)
       in V.generate
            (dmCols rightMatrix)
            (denseMulCell leftEntries)

    denseMulCell leftEntries columnIndex =
      foldl'
        ( \ !accumulated (innerIndex, leftValue) ->
            let !rightValue =
                  (rightRows V.! innerIndex) V.! columnIndex
                !nextValue =
                  accumulated + leftValue * rightValue
             in nextValue
        )
        0
        leftEntries

    denseMulSparseEntries :: V.Vector a -> [(Int, a)]
    denseMulSparseEntries rowVector =
      reverse
        ( V.ifoldl'
            ( \entries innerIndex leftValue ->
                if isZero leftValue
                  then entries
                  else (innerIndex, leftValue) : entries
            )
            []
            rowVector
        )

    forceDenseRows :: V.Vector (V.Vector a) -> V.Vector (V.Vector a)
    forceDenseRows rowVectors =
      V.foldl'
        (\() rowVector -> forceDenseRow rowVector `seq` ())
        ()
        rowVectors
        `seq` rowVectors

    forceDenseRow :: V.Vector a -> V.Vector a
    forceDenseRow rowVector =
      V.foldl'
        (\() entryValue -> entryValue `seq` ())
        ()
        rowVector
        `seq` rowVector

denseSub ::
  Num a =>
  String ->
  DenseMat a ->
  DenseMat a ->
  Either MoonlightError (DenseMat a)
denseSub context leftMatrix rightMatrix = do
  if dmRows leftMatrix /= dmRows rightMatrix || dmCols leftMatrix /= dmCols rightMatrix
    then
      Left
        ( InvariantViolation
            ( context
                <> ": incompatible dense subtraction shapes "
                <> show (dmRows leftMatrix, dmCols leftMatrix)
                <> " and "
                <> show (dmRows rightMatrix, dmCols rightMatrix)
            )
        )
    else
      Right
        DenseMat
          { dmRows = dmRows leftMatrix
          , dmCols = dmCols leftMatrix
          , dmData =
              V.zipWith
                (V.zipWith (\leftValue rightValue -> leftValue + negate rightValue))
                (dmData leftMatrix)
                (dmData rightMatrix)
          }

denseRowNonZeros ::
  IntegralDomain a =>
  String ->
  DenseMat a ->
  Int ->
  Either MoonlightError [(Int, a)]
denseRowNonZeros context matrixValue rowIndex =
  fmap reverse
    ( foldM
        collectEntry
        []
        (indexRange (dmCols matrixValue))
    )
  where
    collectEntry accumulated columnIndex = do
      entryValue <-
        denseIndex
          context
          matrixValue
          rowIndex
          columnIndex
      Right
        ( if isZero entryValue
            then accumulated
            else (columnIndex, entryValue) : accumulated
        )

denseIsZero ::
  IntegralDomain a =>
  DenseMat a ->
  Bool
denseIsZero DenseMat {dmData} =
  V.all (V.all isZero) dmData

identityDense ::
  Num a =>
  Int ->
  DenseMat a
identityDense matrixSize =
  DenseMat
    { dmRows = matrixSize
    , dmCols = matrixSize
    , dmData =
        V.generate
          matrixSize
          ( \rowIndex ->
              V.generate
                matrixSize
                ( \columnIndex ->
                    if rowIndex == columnIndex
                      then 1
                      else 0
                )
          )
    }

swapDenseRows ::
  String ->
  Int ->
  Int ->
  DenseMat a ->
  Either MoonlightError (DenseMat a)
swapDenseRows context leftRow rightRow matrixValue@DenseMat {dmData} = do
  leftVector <-
    denseRow
      context
      matrixValue
      leftRow
  rightVector <-
    denseRow
      context
      matrixValue
      rightRow
  Right
    ( if leftRow == rightRow
        then matrixValue
        else matrixValue {dmData = dmData V.// [(leftRow, rightVector), (rightRow, leftVector)]}
    )

scaleDenseRow ::
  Num a =>
  String ->
  Int ->
  a ->
  DenseMat a ->
  Either MoonlightError (DenseMat a)
scaleDenseRow context rowIndex coefficientValue matrixValue@DenseMat {dmData} = do
  rowVector <-
    denseRow
      context
      matrixValue
      rowIndex
  let !updatedRow =
        V.map (coefficientValue *) rowVector
  Right
    matrixValue {dmData = dmData V.// [(rowIndex, updatedRow)]}

addScaledDenseRow ::
  Num a =>
  String ->
  Int ->
  a ->
  Int ->
  DenseMat a ->
  Either MoonlightError (DenseMat a)
addScaledDenseRow context targetRow coefficientValue sourceRow matrixValue@DenseMat {dmData} = do
  targetVector <-
    denseRow
      context
      matrixValue
      targetRow
  sourceVector <-
    denseRow
      context
      matrixValue
      sourceRow
  let !updatedTarget =
        V.zipWith
          (\targetValue sourceValue -> targetValue + coefficientValue * sourceValue)
          targetVector
          sourceVector
  Right
    matrixValue {dmData = dmData V.// [(targetRow, updatedTarget)]}

setBlockedBlock ::
  IntegralDomain a =>
  FinObjectId ->
  FinObjectId ->
  DenseMat a ->
  BlockedMat a ->
  BlockedMat a
setBlockedBlock rowLabel columnLabel blockValue blockedMatrix =
  blockedMatrix {bmBlocks = nextBlocks}
  where
    rowKey =
      unFinObjectId rowLabel
    columnKey =
      unFinObjectId columnLabel
    currentRowMap =
      IntMap.findWithDefault IntMap.empty rowKey (bmBlocks blockedMatrix)
    nextRowMap
      | denseIsZero blockValue =
          IntMap.delete columnKey currentRowMap
      | otherwise =
          IntMap.insert columnKey blockValue currentRowMap
    nextBlocks
      | IntMap.null nextRowMap =
          IntMap.delete rowKey (bmBlocks blockedMatrix)
      | otherwise =
          IntMap.insert rowKey nextRowMap (bmBlocks blockedMatrix)

validateDenseMat ::
  String ->
  DenseMat a ->
  Either MoonlightError ()
validateDenseMat context DenseMat {dmRows, dmCols, dmData}
  | dmRows < 0 || dmCols < 0 =
      Left
        ( InvariantViolation
            ( context
                <> ": negative dense matrix shape "
                <> show (dmRows, dmCols)
            )
        )
  | V.length dmData /= dmRows =
      Left
        ( InvariantViolation
            ( context
                <> ": row-vector count does not match declared row count "
                <> show (V.length dmData, dmRows)
            )
        )
  | otherwise =
      foldM
        validateRow
        ()
        (zip (indexRange (V.length dmData)) (V.toList dmData))
  where
    validateRow () (rowIndex, rowVector)
      | V.length rowVector == dmCols =
          Right ()
      | otherwise =
          Left
            ( InvariantViolation
                ( context
                    <> ": dense row "
                    <> show rowIndex
                    <> " has length "
                    <> show (V.length rowVector)
                    <> ", expected "
                    <> show dmCols
                )
            )

denseRow ::
  String ->
  DenseMat a ->
  Int ->
  Either MoonlightError (V.Vector a)
denseRow context DenseMat {dmData} rowIndex =
  case dmData V.!? rowIndex of
    Just rowVector ->
      Right rowVector
    Nothing ->
      Left
        ( InvariantViolation
            ( context
                <> ": dense row index "
                <> show rowIndex
                <> " is out of bounds"
            )
        )

denseIndex ::
  String ->
  DenseMat a ->
  Int ->
  Int ->
  Either MoonlightError a
denseIndex context matrixValue rowIndex columnIndex = do
  rowVector <-
    denseRow context matrixValue rowIndex
  case rowVector V.!? columnIndex of
    Just entryValue ->
      Right entryValue
    Nothing ->
      Left
        ( InvariantViolation
            ( context
                <> ": dense column index "
                <> show columnIndex
                <> " is out of bounds"
            )
        )

lookupVector ::
  String ->
  Int ->
  V.Vector a ->
  Either MoonlightError a
lookupVector context indexValue vectorValue =
  case vectorValue V.!? indexValue of
    Just value ->
      Right value
    Nothing ->
      Left
        ( InvariantViolation
            ( context
                <> ": index "
                <> show indexValue
                <> " is out of bounds"
            )
        )

replaceVector ::
  String ->
  Int ->
  a ->
  V.Vector a ->
  Either MoonlightError (V.Vector a)
replaceVector context indexValue newValue vectorValue
  | indexValue < 0 || indexValue >= V.length vectorValue =
      Left
        ( InvariantViolation
            ( context
                <> ": index "
                <> show indexValue
                <> " is out of bounds"
            )
        )
  | otherwise =
      Right
        (vectorValue V.// [(indexValue, newValue)])

swapVectorEntries ::
  String ->
  Int ->
  Int ->
  V.Vector a ->
  Either MoonlightError (V.Vector a)
swapVectorEntries context leftIndex rightIndex vectorValue = do
  leftValue <-
    lookupVector context leftIndex vectorValue
  rightValue <-
    lookupVector context rightIndex vectorValue
  Right
    ( if leftIndex == rightIndex
        then vectorValue
        else vectorValue V.// [(leftIndex, rightValue), (rightIndex, leftValue)]
    )

indexRange ::
  Int ->
  [Int]
indexRange countValue
  | countValue <= 0 =
      []
  | otherwise =
      [0 .. countValue - 1]

indexRangeFromTo ::
  Int ->
  Int ->
  [Int]
indexRangeFromTo startValue endValue
  | startValue >= endValue =
      []
  | otherwise =
      [startValue .. endValue - 1]
