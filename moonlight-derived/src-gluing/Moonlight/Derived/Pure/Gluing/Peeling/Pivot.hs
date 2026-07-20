{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Derived.Pure.Gluing.Peeling.Pivot
  ( EliminationWitness (..)
  , PivotMinor (..)
  , invertPivotOrExplain
  , pivotMinorFromWitness
  , solveLeftWithEliminationWitness
  , sortedPivotColumns
  , sortedPivotRows
  , trustedEliminationWitness
  ) where

import Control.Monad (foldM)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Vector qualified as V
import Moonlight.Algebra (IntegralDomain (isZero))
import Moonlight.Core
  ( AdditiveGroup (neg)
  , Field
  , MoonlightError (..)
  , requireInvertible
  )
import Moonlight.Derived.Pure.Gluing.Peeling.Dense
  ( denseMul
  , indexRange
  , indexRangeFromTo
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix (DenseMat (..))

type PivotMinor :: Type -> Type
data PivotMinor a = PivotMinor
  { pmRows :: !(V.Vector Int)
  , pmCols :: !(V.Vector Int)
  }

type EliminationWitness :: Type -> Type
data EliminationWitness a = EliminationWitness
  { ewRows :: !(V.Vector Int)
  , ewCols :: !(V.Vector Int)
  , ewRank :: !Int
  , ewInverse :: !(DenseMat a)
  }

type EliminationState :: Type -> Type
data EliminationState a = EliminationState
  { esWork :: !(DenseMat a)
  , esCoefficients :: !(DenseMat a)
  , esRowKeys :: !(V.Vector Int)
  , esActiveRow :: !Int
  , esSelectedRows :: ![Int]
  , esSelectedColumns :: ![Int]
  }

pivotMinorFromWitness :: EliminationWitness a -> Maybe (PivotMinor a)
pivotMinorFromWitness EliminationWitness {ewRows, ewCols, ewRank}
  | ewRank <= 0 =
      Nothing
  | otherwise =
      Just
        PivotMinor
          { pmRows = ewRows
          , pmCols = ewCols
          }

sortedPivotRows :: PivotMinor a -> [Int]
sortedPivotRows PivotMinor {pmRows} =
  IntSet.toAscList (IntSet.fromList (V.toList pmRows))

sortedPivotColumns :: PivotMinor a -> [Int]
sortedPivotColumns PivotMinor {pmCols} =
  IntSet.toAscList (IntSet.fromList (V.toList pmCols))

solveLeftWithEliminationWitness ::
  (Num a, IntegralDomain a) =>
  String ->
  EliminationWitness a ->
  DenseMat a ->
  Either MoonlightError (DenseMat a)
solveLeftWithEliminationWitness context EliminationWitness {ewInverse} leftMatrix =
  denseMul context leftMatrix ewInverse

trustedEliminationWitness ::
  (Field a, Num a, IntegralDomain a) =>
  DenseMat a ->
  Either MoonlightError (EliminationWitness a)
trustedEliminationWitness denseMatrix = do
  initialWork <-
    denseWorkFromDense
      "minimizeComplex: rank profile pivot search"
      denseMatrix
  finalState <-
    foldM
      advanceEliminationColumn
      (initialState initialWork)
      (indexRange columnCount)
  Right
    ( buildEliminationWitness
        (esCoefficients finalState)
        (esSelectedRows finalState)
        (esSelectedColumns finalState)
    )
  where
    rowCount =
      dmRows denseMatrix
    columnCount =
      dmCols denseMatrix
    rankBound =
      max 0 (min rowCount columnCount)

    initialState initialWork =
      EliminationState
        { esWork = initialWork
        , esCoefficients = zeroDenseWork rowCount rankBound
        , esRowKeys = V.fromList (indexRange rowCount)
        , esActiveRow = 0
        , esSelectedRows = []
        , esSelectedColumns = []
        }

advanceEliminationColumn ::
  (Field a, Num a, IntegralDomain a) =>
  EliminationState a ->
  Int ->
  Either MoonlightError (EliminationState a)
advanceEliminationColumn state@EliminationState {esWork, esCoefficients, esRowKeys, esActiveRow, esSelectedRows, esSelectedColumns} columnIndex
  | esActiveRow >= dmRows esWork =
      Right state
  | otherwise =
      case findPivotInWorkColumn esWork esActiveRow columnIndex of
        Nothing ->
          Right state
        Just pivotRow -> do
          let !pivotValue =
                denseWorkIndex
                  esWork
                  pivotRow
                  columnIndex
          pivotInverse <-
            invertPivotOrExplain pivotValue
          let !normalizedPivotRow =
                forceDenseWorkRow
                  ( V.map
                      ( \entryValue ->
                          let !scaledValue =
                                pivotInverse * entryValue
                           in scaledValue
                      )
                      (denseWorkRow esWork pivotRow)
                  )
              !normalizedCoefficientRow =
                forceDenseWorkRow
                  ( V.imap
                      ( \columnCoordinate entryValue ->
                          let !pivotCoordinate =
                                if columnCoordinate == esActiveRow
                                  then 1
                                  else entryValue
                              !scaledValue =
                                pivotInverse * pivotCoordinate
                           in scaledValue
                      )
                      (denseWorkRow esCoefficients pivotRow)
                  )
              !scaledMatrix =
                replaceDenseWorkRowsAfterSwap
                  esActiveRow
                  pivotRow
                  normalizedPivotRow
                  esWork
              !scaledCoefficients =
                replaceDenseWorkRowsAfterSwap
                  esActiveRow
                  pivotRow
                  normalizedCoefficientRow
                  esCoefficients
              !swappedRowKeys =
                swapVectorEntriesTotal
                  esActiveRow
                  pivotRow
                  esRowKeys
              (!eliminatedMatrix, !eliminatedCoefficients) =
                eliminateWorkPivotColumn
                  columnIndex
                  esActiveRow
                  scaledMatrix
                  scaledCoefficients
              !originalPivotRow =
                swappedRowKeys V.! esActiveRow
          Right
            EliminationState
              { esWork = eliminatedMatrix
              , esCoefficients = eliminatedCoefficients
              , esRowKeys = swappedRowKeys
              , esActiveRow = esActiveRow + 1
              , esSelectedRows = originalPivotRow : esSelectedRows
              , esSelectedColumns = columnIndex : esSelectedColumns
              }

buildEliminationWitness ::
  DenseMat a ->
  [Int] ->
  [Int] ->
  EliminationWitness a
buildEliminationWitness !rowCoefficients !selectedRows !selectedColumns =
  let witnessRows =
        V.fromList (reverse selectedRows)
      witnessColumns =
        V.fromList (reverse selectedColumns)
      rankValue =
        V.length witnessRows
      !witnessInverse =
        DenseMat
          { dmRows = rankValue
          , dmCols = rankValue
          , dmData =
              V.generate
                rankValue
                (\rowIndex -> denseWorkRowPrefix rowIndex rankValue rowCoefficients)
          }
   in EliminationWitness
      { ewRows = witnessRows
      , ewCols = witnessColumns
      , ewRank = rankValue
      , ewInverse = witnessInverse
      }

zeroDenseWork ::
  Num a =>
  Int ->
  Int ->
  DenseMat a
zeroDenseWork rowCount columnCount =
  let !zeroRow =
        V.replicate (max 0 columnCount) 0
   in DenseMat
        { dmRows = rowCount
        , dmCols = columnCount
        , dmData = V.replicate (max 0 rowCount) zeroRow
        }

denseWorkFromDense ::
  String ->
  DenseMat a ->
  Either MoonlightError (DenseMat a)
denseWorkFromDense context matrixValue@DenseMat {dmRows, dmCols, dmData} = do
  validateDenseWorkSnapshot context matrixValue
  let !snapshotRows
        | dmRows <= 0 =
            V.empty
        | dmCols <= 0 =
            V.replicate dmRows V.empty
        | otherwise =
            V.generate
              dmRows
              (\rowIndex -> V.take dmCols (dmData V.! rowIndex))
  Right
    matrixValue {dmData = snapshotRows}

validateDenseWorkSnapshot ::
  String ->
  DenseMat a ->
  Either MoonlightError ()
validateDenseWorkSnapshot context DenseMat {dmRows, dmCols, dmData}
  | dmRows <= 0 || dmCols <= 0 =
      Right ()
  | otherwise =
      foldM
        validateRow
        ()
        (indexRange dmRows)
  where
    validateRow () rowIndex =
      case dmData V.!? rowIndex of
        Nothing ->
          Left
            ( InvariantViolation
                ( context
                    <> ": dense row index "
                    <> show rowIndex
                    <> " is out of bounds"
                )
            )
        Just rowVector
          | V.length rowVector < dmCols ->
              Left
                ( InvariantViolation
                    ( context
                        <> ": dense column index "
                        <> show (V.length rowVector)
                        <> " is out of bounds"
                    )
                )
          | otherwise ->
              Right ()

denseWorkIndex ::
  DenseMat a ->
  Int ->
  Int ->
  a
denseWorkIndex DenseMat {dmData} rowIndex columnIndex =
  (dmData V.! rowIndex) V.! columnIndex

denseWorkRowPrefix ::
  Int ->
  Int ->
  DenseMat a ->
  V.Vector a
denseWorkRowPrefix rowIndex columnCount matrixValue =
  forceDenseWorkRow
    (V.force (V.take columnCount (denseWorkRow matrixValue rowIndex)))

denseWorkRow ::
  DenseMat a ->
  Int ->
  V.Vector a
denseWorkRow DenseMat {dmData} rowIndex =
  dmData V.! rowIndex

replaceDenseWorkRowsAfterSwap ::
  Int ->
  Int ->
  V.Vector a ->
  DenseMat a ->
  DenseMat a
replaceDenseWorkRowsAfterSwap activeRow pivotRow !normalizedPivotRow matrixValue@DenseMat {dmData}
  | activeRow == pivotRow =
      let !updatedRows =
            dmData V.// [(activeRow, normalizedPivotRow)]
       in matrixValue {dmData = updatedRows}
  | otherwise =
      let !displacedActiveRow =
            dmData V.! activeRow
          !updatedRows =
            dmData
              V.// [ (activeRow, normalizedPivotRow)
                   , (pivotRow, displacedActiveRow)
                   ]
       in matrixValue {dmData = updatedRows}

eliminateWorkPivotColumn ::
  (Field a, Num a, IntegralDomain a) =>
  Int ->
  Int ->
  DenseMat a ->
  DenseMat a ->
  (DenseMat a, DenseMat a)
eliminateWorkPivotColumn pivotColumn pivotRow matrixValue@DenseMat {dmData = matrixRows} rowCoefficients@DenseMat {dmData = coefficientRows} =
  let !updatedRowPairs =
        forceDenseWorkRowPairs
          ( V.imap
              clearRow
              (V.zip matrixRows coefficientRows)
          )
      (!updatedMatrixRows, !updatedCoefficientRows) =
        V.unzip
          updatedRowPairs
      !updatedMatrix =
        matrixValue {dmData = updatedMatrixRows}
      !updatedCoefficients =
        rowCoefficients {dmData = updatedCoefficientRows}
   in (updatedMatrix, updatedCoefficients)
  where
    !pivotMatrixRow =
      matrixRows V.! pivotRow

    !pivotCoefficientRow =
      coefficientRows V.! pivotRow

    clearRow rowIndex (!matrixRow, !coefficientRow)
      | rowIndex == pivotRow =
          (matrixRow, coefficientRow)
      | otherwise =
          let !entryValue =
                matrixRow V.! pivotColumn
           in if isZero entryValue
                then (matrixRow, coefficientRow)
                else
                  let !coefficientValue =
                        neg entryValue
                      !updatedMatrixRow =
                        addScaledDenseWorkRow
                          coefficientValue
                          pivotMatrixRow
                          matrixRow
                      !updatedCoefficientRow =
                        addScaledDenseWorkRow
                          coefficientValue
                          pivotCoefficientRow
                          coefficientRow
                   in (updatedMatrixRow, updatedCoefficientRow)

addScaledDenseWorkRow ::
  Num a =>
  a ->
  V.Vector a ->
  V.Vector a ->
  V.Vector a
addScaledDenseWorkRow coefficientValue sourceRow targetRow =
  forceDenseWorkRow
    ( V.zipWith
        ( \targetValue sourceValue ->
            let !updatedValue =
                  targetValue + coefficientValue * sourceValue
             in updatedValue
        )
        targetRow
        sourceRow
    )

forceDenseWorkRow ::
  V.Vector a ->
  V.Vector a
forceDenseWorkRow rowVector =
  V.foldl'
    (\() entryValue -> entryValue `seq` ())
    ()
    rowVector
    `seq` rowVector

forceDenseWorkRowPairs ::
  V.Vector (V.Vector a, V.Vector b) ->
  V.Vector (V.Vector a, V.Vector b)
forceDenseWorkRowPairs rowPairs =
  V.foldl'
    ( \() (matrixRow, coefficientRow) ->
        matrixRow `seq` coefficientRow `seq` ()
    )
    ()
    rowPairs
    `seq` rowPairs

swapVectorEntriesTotal ::
  Int ->
  Int ->
  V.Vector a ->
  V.Vector a
swapVectorEntriesTotal leftIndex rightIndex vectorValue
  | leftIndex == rightIndex =
      vectorValue
  | otherwise =
      let !leftValue =
            vectorValue V.! leftIndex
          !rightValue =
            vectorValue V.! rightIndex
          !swappedVector =
            vectorValue V.// [(leftIndex, rightValue), (rightIndex, leftValue)]
       in swappedVector

findPivotInWorkColumn ::
  IntegralDomain a =>
  DenseMat a ->
  Int ->
  Int ->
  Maybe Int
findPivotInWorkColumn matrixValue startRow columnIndex =
  foldl'
    choosePivot
    Nothing
    (indexRangeFromTo startRow (dmRows matrixValue))
  where
    choosePivot foundPivot rowIndex =
      case foundPivot of
        Just _ ->
          foundPivot
        Nothing ->
          let !entryValue =
                denseWorkIndex matrixValue rowIndex columnIndex
           in if isZero entryValue
                then Nothing
                else Just rowIndex

invertPivotOrExplain ::
  (Field a, IntegralDomain a) =>
  a ->
  Either MoonlightError a
invertPivotOrExplain pivotValue
  | isZero pivotValue =
      Left
        (InvariantViolation "minimizeComplex: encountered a zero pivot")
  | otherwise =
      requireInvertible
        (InvariantViolation "minimizeComplex: Field.tryInv failed on a nonzero pivot")
        pivotValue
