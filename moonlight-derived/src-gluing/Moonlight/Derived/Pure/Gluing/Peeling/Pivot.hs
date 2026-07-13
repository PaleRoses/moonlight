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
import Data.IntMap.Strict qualified as IntMap
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
  ( denseIndex
  , denseMul
  , indexRange
  , indexRangeFromTo
  , lookupVector
  , swapVectorEntries
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

type DenseWork :: Type -> Type
data DenseWork a = DenseWork
  { dwRows :: !Int
  , dwCols :: !Int
  , dwData :: !(V.Vector a)
  , dwRowsOverridden :: !(IntMap.IntMap (V.Vector a))
  }

type EliminationState :: Type -> Type
data EliminationState a = EliminationState
  { esWork :: !(DenseWork a)
  , esCoefficients :: !(DenseWork a)
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
  buildEliminationWitness
    (esCoefficients finalState)
    (esSelectedRows finalState)
    (esSelectedColumns finalState)
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
  | esActiveRow >= dwRows esWork =
      Right state
  | otherwise = do
      maybePivotRow <-
        findPivotInWorkColumn
          "minimizeComplex: rank profile pivot search"
          esWork
          esActiveRow
          columnIndex
      case maybePivotRow of
        Nothing ->
          Right state
        Just pivotRow -> do
          swappedMatrix <-
            swapDenseWorkRows
              "minimizeComplex: rank profile row swap"
              esActiveRow
              pivotRow
              esWork
          swappedCoefficients <-
            swapDenseWorkRows
              "minimizeComplex: rank profile coefficient row swap"
              esActiveRow
              pivotRow
              esCoefficients
          swappedRowKeys <-
            swapVectorEntries
              "minimizeComplex: rank profile row-key swap"
              esActiveRow
              pivotRow
              esRowKeys
          pivotValue <-
            denseWorkIndex
              "minimizeComplex: rank profile pivot"
              swappedMatrix
              esActiveRow
              columnIndex
          pivotInverse <-
            invertPivotOrExplain pivotValue
          coefficientWithPivot <-
            setDenseWorkEntry
              "minimizeComplex: rank profile coefficient pivot coordinate"
              esActiveRow
              esActiveRow
              1
              swappedCoefficients
          scaledMatrix <-
            scaleDenseWorkRow
              "minimizeComplex: rank profile pivot normalization"
              esActiveRow
              pivotInverse
              swappedMatrix
          scaledCoefficients <-
            scaleDenseWorkRow
              "minimizeComplex: rank profile coefficient pivot normalization"
              esActiveRow
              pivotInverse
              coefficientWithPivot
          (eliminatedMatrix, eliminatedCoefficients) <-
            eliminateWorkPivotColumn
              columnIndex
              esActiveRow
              scaledMatrix
              scaledCoefficients
          originalPivotRow <-
            lookupVector
              "minimizeComplex: rank profile selected row key"
              esActiveRow
              swappedRowKeys
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
  DenseWork a ->
  [Int] ->
  [Int] ->
  Either MoonlightError (EliminationWitness a)
buildEliminationWitness !rowCoefficients !selectedRows !selectedColumns = do
  let witnessRows =
        V.fromList (reverse selectedRows)
      witnessColumns =
        V.fromList (reverse selectedColumns)
      rankValue =
        V.length witnessRows
  inverseRows <-
    traverse
      ( \rowIndex ->
          denseWorkRowPrefix
            "minimizeComplex: elimination witness inverse row"
            rowIndex
            rankValue
            rowCoefficients
      )
      (indexRange rankValue)
  let witnessInverse =
        DenseMat
          { dmRows = rankValue
          , dmCols = rankValue
          , dmData = V.fromList inverseRows
          }
  Right
    EliminationWitness
      { ewRows = witnessRows
      , ewCols = witnessColumns
      , ewRank = rankValue
      , ewInverse = witnessInverse
      }

zeroDenseWork ::
  Num a =>
  Int ->
  Int ->
  DenseWork a
zeroDenseWork rowCount columnCount =
  DenseWork
    { dwRows = rowCount
    , dwCols = columnCount
    , dwData = V.replicate (denseWorkStorageLength rowCount columnCount) 0
    , dwRowsOverridden = IntMap.empty
    }

denseWorkFromDense ::
  String ->
  DenseMat a ->
  Either MoonlightError (DenseWork a)
denseWorkFromDense context matrixValue@DenseMat {dmRows, dmCols} = do
  entries <-
    traverse
      ( \(rowIndex, columnIndex) ->
          denseIndex
            context
            matrixValue
            rowIndex
            columnIndex
      )
      [ (rowIndex, columnIndex)
      | rowIndex <- indexRange dmRows
      , columnIndex <- indexRange dmCols
      ]
  Right
    DenseWork
      { dwRows = dmRows
      , dwCols = dmCols
      , dwData = V.fromList entries
      , dwRowsOverridden = IntMap.empty
      }

denseWorkStorageLength ::
  Int ->
  Int ->
  Int
denseWorkStorageLength rowCount columnCount =
  max 0 rowCount * max 0 columnCount

denseWorkOffset ::
  DenseWork a ->
  Int ->
  Int ->
  Int
denseWorkOffset DenseWork {dwCols} rowIndex columnIndex =
  rowIndex * dwCols + columnIndex

denseWorkIndex ::
  String ->
  DenseWork a ->
  Int ->
  Int ->
  Either MoonlightError a
denseWorkIndex context matrixValue@DenseWork {dwData, dwRowsOverridden} rowIndex columnIndex = do
  validateDenseWorkRow context matrixValue rowIndex
  validateDenseWorkColumn context matrixValue columnIndex
  case IntMap.lookup rowIndex dwRowsOverridden of
    Just rowVector ->
      denseWorkRowIndex context rowVector columnIndex
    Nothing ->
      let entryOffset =
            denseWorkOffset matrixValue rowIndex columnIndex
       in case dwData V.!? entryOffset of
            Just entryValue ->
              Right entryValue
            Nothing ->
              Left
                ( InvariantViolation
                    ( context
                        <> ": flat dense offset "
                        <> show entryOffset
                        <> " is out of bounds"
                    )
                )

denseWorkRowPrefix ::
  String ->
  Int ->
  Int ->
  DenseWork a ->
  Either MoonlightError (V.Vector a)
denseWorkRowPrefix context rowIndex columnCount matrixValue =
  fmap V.fromList
    ( traverse
        (denseWorkIndex context matrixValue rowIndex)
        (indexRange columnCount)
    )

denseWorkRow ::
  String ->
  DenseWork a ->
  Int ->
  Either MoonlightError (V.Vector a)
denseWorkRow context matrixValue@DenseWork {dwCols, dwRowsOverridden} rowIndex = do
  validateDenseWorkRow context matrixValue rowIndex
  case IntMap.lookup rowIndex dwRowsOverridden of
    Just rowVector ->
      validateDenseWorkRowWidth context matrixValue rowVector *> Right rowVector
    Nothing ->
      fmap V.fromList
        ( traverse
            (denseWorkIndex context matrixValue rowIndex)
            (indexRange dwCols)
        )

denseWorkRowIndex ::
  String ->
  V.Vector a ->
  Int ->
  Either MoonlightError a
denseWorkRowIndex context rowVector columnIndex =
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

validateDenseWorkRow ::
  String ->
  DenseWork a ->
  Int ->
  Either MoonlightError ()
validateDenseWorkRow context DenseWork {dwRows} rowIndex
  | rowIndex < 0 || rowIndex >= dwRows =
      Left
        ( InvariantViolation
            ( context
                <> ": dense row index "
                <> show rowIndex
                <> " is out of bounds"
            )
        )
  | otherwise =
      Right ()

validateDenseWorkColumn ::
  String ->
  DenseWork a ->
  Int ->
  Either MoonlightError ()
validateDenseWorkColumn context DenseWork {dwCols} columnIndex
  | columnIndex < 0 || columnIndex >= dwCols =
      Left
        ( InvariantViolation
            ( context
                <> ": dense column index "
                <> show columnIndex
                <> " is out of bounds"
            )
        )
  | otherwise =
      Right ()

validateDenseWorkRowWidth ::
  String ->
  DenseWork a ->
  V.Vector a ->
  Either MoonlightError ()
validateDenseWorkRowWidth context DenseWork {dwCols} rowVector
  | V.length rowVector == dwCols =
      Right ()
  | otherwise =
      Left
        ( InvariantViolation
            ( context
                <> ": dense row has length "
                <> show (V.length rowVector)
                <> ", expected "
                <> show dwCols
            )
        )

setDenseWorkRow ::
  String ->
  Int ->
  V.Vector a ->
  DenseWork a ->
  Either MoonlightError (DenseWork a)
setDenseWorkRow context rowIndex rowVector matrixValue@DenseWork {dwRowsOverridden} =
  validateDenseWorkRow context matrixValue rowIndex
    *> validateDenseWorkRowWidth context matrixValue rowVector
    *> Right (matrixValue {dwRowsOverridden = IntMap.insert rowIndex rowVector dwRowsOverridden})

updateDenseWorkRow ::
  String ->
  Int ->
  (Int -> a -> a) ->
  DenseWork a ->
  Either MoonlightError (DenseWork a)
updateDenseWorkRow context rowIndex updateEntry matrixValue = do
  rowVector <-
    denseWorkRow context matrixValue rowIndex
  let !updatedRow =
        V.imap updateEntry rowVector
  setDenseWorkRow context rowIndex updatedRow matrixValue

swapDenseWorkRows ::
  String ->
  Int ->
  Int ->
  DenseWork a ->
  Either MoonlightError (DenseWork a)
swapDenseWorkRows context leftRow rightRow matrixValue = do
  leftVector <-
    denseWorkRow context matrixValue leftRow
  rightVector <-
    denseWorkRow context matrixValue rightRow
  if leftRow == rightRow
    then Right matrixValue
    else
      setDenseWorkRow context leftRow rightVector matrixValue
        >>= setDenseWorkRow context rightRow leftVector

scaleDenseWorkRow ::
  Num a =>
  String ->
  Int ->
  a ->
  DenseWork a ->
  Either MoonlightError (DenseWork a)
scaleDenseWorkRow context rowIndex coefficientValue =
  updateDenseWorkRow
    context
    rowIndex
    (\_ entryValue -> coefficientValue * entryValue)

addScaledDenseWorkRow ::
  Num a =>
  String ->
  Int ->
  a ->
  Int ->
  DenseWork a ->
  Either MoonlightError (DenseWork a)
addScaledDenseWorkRow context targetRow coefficientValue sourceRow matrixValue = do
  targetVector <-
    denseWorkRow context matrixValue targetRow
  sourceVector <-
    denseWorkRow context matrixValue sourceRow
  let !updatedRow =
        V.zipWith
          (\targetValue sourceValue -> targetValue + coefficientValue * sourceValue)
          targetVector
          sourceVector
  setDenseWorkRow context targetRow updatedRow matrixValue

setDenseWorkEntry ::
  String ->
  Int ->
  Int ->
  a ->
  DenseWork a ->
  Either MoonlightError (DenseWork a)
setDenseWorkEntry context rowIndex columnIndex entryValue matrixValue = do
  rowVector <-
    denseWorkRow context matrixValue rowIndex
  validateDenseWorkColumn context matrixValue columnIndex
  setDenseWorkRow
    context
    rowIndex
    (rowVector V.// [(columnIndex, entryValue)])
    matrixValue

eliminateWorkPivotColumn ::
  (Field a, Num a, IntegralDomain a) =>
  Int ->
  Int ->
  DenseWork a ->
  DenseWork a ->
  Either MoonlightError (DenseWork a, DenseWork a)
eliminateWorkPivotColumn pivotColumn pivotRow matrixValue rowCoefficients =
  foldM
    clearRow
    (matrixValue, rowCoefficients)
    [ rowIndex
    | rowIndex <- indexRange (dwRows matrixValue)
    , rowIndex /= pivotRow
    ]
  where
    clearRow (!matrixAccumulated, !coefficientsAccumulated) rowIndex = do
      entryValue <-
        denseWorkIndex
          "minimizeComplex: rank profile elimination entry"
          matrixAccumulated
          rowIndex
          pivotColumn
      if isZero entryValue
        then Right (matrixAccumulated, coefficientsAccumulated)
        else do
          let coefficientValue =
                neg entryValue
          nextMatrix <-
            addScaledDenseWorkRow
              "minimizeComplex: rank profile elimination row operation"
              rowIndex
              coefficientValue
              pivotRow
              matrixAccumulated
          nextCoefficients <-
            addScaledDenseWorkRow
              "minimizeComplex: rank profile coefficient row operation"
              rowIndex
              coefficientValue
              pivotRow
              coefficientsAccumulated
          Right (nextMatrix, nextCoefficients)

findPivotInWorkColumn ::
  IntegralDomain a =>
  String ->
  DenseWork a ->
  Int ->
  Int ->
  Either MoonlightError (Maybe Int)
findPivotInWorkColumn context matrixValue startRow columnIndex =
  foldM
    choosePivot
    Nothing
    (indexRangeFromTo startRow (dwRows matrixValue))
  where
    choosePivot foundPivot rowIndex =
      case foundPivot of
        Just _ ->
          Right foundPivot
        Nothing -> do
          entryValue <-
            denseWorkIndex
              context
              matrixValue
              rowIndex
              columnIndex
          Right
            ( if isZero entryValue
                then Nothing
                else Just rowIndex
            )

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
