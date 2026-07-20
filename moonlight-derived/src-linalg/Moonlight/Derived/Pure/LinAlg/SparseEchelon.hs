{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Derived.Pure.LinAlg.SparseEchelon
  ( SparseRow
  , sparseRowEntries
  , SparseSpan
  , TrackedLeftKernel
  , emptySparseRow
  , emptySparseSpan
  , emptyTrackedLeftKernel
  , sparseRowFromEntries
  , appendSparseRowEntries
  , sparseRowsFromDense
  , sparseRowToVector
  , restrictSparseRow
  , spanFromRows
  , admitSparseRow
  , admitTrackedLeftKernelRow
  , prependSparseSpanRows
  , prependStableSparseSpanRows
  , prependTrackedLeftKernelRows
  , prependStableTrackedLeftKernelRows
  , trackedLeftKernelRows
  , trackedLeftKernel
  ) where

import Control.Monad (foldM)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Moonlight.Algebra (IntegralDomain (isZero))
import Moonlight.Core
  ( Field (fieldValueValid)
  , MoonlightError (..)
  , requireInvertible
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix (DenseMat (..))

type SparseRow :: Type -> Type
newtype SparseRow a = SparseRow
  { sparseRowEntries :: IntMap a
  }
  deriving stock (Eq, Show)

type SparseSpan :: Type -> Type
newtype SparseSpan a = SparseSpan (IntMap (SparseRow a))

type TrackedEchelonRow :: Type -> Type
data TrackedEchelonRow a = TrackedEchelonRow
  { terData :: !(SparseRow a)
  , terWitness :: !(SparseRow a)
  }

type TrackedLeftKernel :: Type -> Type
data TrackedLeftKernel a = TrackedLeftKernel
  { tlkSeenRows :: !IntSet
  , tlkBasisRows :: !(IntMap (TrackedEchelonRow a))
  }

emptySparseSpan :: SparseSpan a
emptySparseSpan =
  SparseSpan IntMap.empty

emptySparseRow :: SparseRow a
emptySparseRow =
  SparseRow IntMap.empty

emptyTrackedLeftKernel :: TrackedLeftKernel a
emptyTrackedLeftKernel =
  TrackedLeftKernel
    { tlkSeenRows = IntSet.empty
    , tlkBasisRows = IntMap.empty
    }

sparseRowFromEntries ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  [(Int, a)] ->
  Either MoonlightError (SparseRow a)
sparseRowFromEntries context entries =
  appendSparseRowEntries context entries emptySparseRow

appendSparseRowEntries ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  [(Int, a)] ->
  SparseRow a ->
  Either MoonlightError (SparseRow a)
appendSparseRowEntries context entries rowValue =
  foldM insertEntry rowValue entries
  where
    insertEntry accumulatedRow (indexValue, coefficientValue)
      | indexValue < 0 =
          Left
            ( InvariantViolation
                ( context
                    <> ": negative sparse coordinate "
                    <> show indexValue
                )
            )
      | not (fieldValueValid coefficientValue) =
          Left
            ( InvariantViolation
                ( context
                    <> ": sparse row contains an invalid field value at coordinate "
                    <> show indexValue
                )
            )
      | otherwise =
          Right (insertCoefficient indexValue coefficientValue accumulatedRow)

sparseRowsFromDense ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  DenseMat a ->
  Either MoonlightError [SparseRow a]
sparseRowsFromDense context DenseMat {dmRows, dmCols, dmData}
  | dmRows < 0 || dmCols < 0 =
      Left
        ( InvariantViolation
            ( context
                <> ": negative dense shape "
                <> show (dmRows, dmCols)
            )
        )
  | V.length dmData /= dmRows =
      Left
        ( InvariantViolation
            ( context
                <> ": dense row metadata mismatch "
                <> show (dmRows, V.length dmData)
            )
        )
  | otherwise =
      traverse rowToSparse (zip [0 :: Int ..] (V.toList dmData))
  where
    rowToSparse (rowIndexValue, rowVector)
      | V.length rowVector /= dmCols =
          Left
            ( InvariantViolation
                ( context
                    <> ": dense row "
                    <> show rowIndexValue
                    <> " has width "
                    <> show (V.length rowVector)
                    <> ", expected "
                    <> show dmCols
                )
            )
      | otherwise =
          sparseRowFromEntries
            (context <> ": dense row " <> show rowIndexValue)
            (V.toList (V.indexed rowVector))

sparseRowToVector ::
  Num a =>
  String ->
  Int ->
  SparseRow a ->
  Either MoonlightError (Vector a)
sparseRowToVector context widthValue (SparseRow entries)
  | widthValue < 0 =
      Left
        ( InvariantViolation
            ( context
                <> ": negative vector width "
                <> show widthValue
            )
        )
  | otherwise =
      validateBounds *> Right renderedVector
  where
    validateBounds =
      case IntMap.lookupMax entries of
        Just (maximumIndex, _)
          | maximumIndex >= widthValue ->
              Left
                ( InvariantViolation
                    ( context
                        <> ": sparse coordinate "
                        <> show maximumIndex
                        <> " is outside width "
                        <> show widthValue
                    )
                )
        _ ->
          Right ()

    renderedVector =
      V.generate
        widthValue
        (\indexValue -> IntMap.findWithDefault 0 indexValue entries)

restrictSparseRow :: IntSet -> SparseRow a -> SparseRow a
restrictSparseRow allowedIndices (SparseRow entries) =
  SparseRow
    ( IntMap.filterWithKey
        (\indexValue _ -> IntSet.member indexValue allowedIndices)
        entries
    )

spanFromRows ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  [SparseRow a] ->
  Either MoonlightError (SparseSpan a)
spanFromRows context =
  foldM addRow emptySparseSpan
  where
    addRow spanValue rowValue =
      snd <$> admitSparseRow context rowValue spanValue

admitSparseRow ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  SparseRow a ->
  SparseSpan a ->
  Either MoonlightError (Maybe (SparseRow a), SparseSpan a)
admitSparseRow context candidateRow spanValue@(SparseSpan basisRows) =
  case sparsePivot reducedRow of
    Nothing ->
      Right (Nothing, spanValue)
    Just (pivotIndex, pivotValue) -> do
      normalizedRow <-
        normalizeSparseRow
          (context <> ": normalize independent row")
          pivotValue
          reducedRow
      Right
        ( Just normalizedRow
        , SparseSpan (IntMap.insert pivotIndex normalizedRow basisRows)
        )
  where
    reducedRow =
      reduceAgainstSpan spanValue candidateRow

trackedLeftKernel ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  [(Int, SparseRow a)] ->
  Either MoonlightError [SparseRow a]
trackedLeftKernel context rowsValue =
  snd <$> trackedLeftKernelRows context emptyTrackedLeftKernel rowsValue

trackedLeftKernelRows ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  TrackedLeftKernel a ->
  [(Int, SparseRow a)] ->
  Either MoonlightError (TrackedLeftKernel a, [SparseRow a])
trackedLeftKernelRows context initialKernel rowsValue = do
  (finalKernel, kernelRowsReversed) <-
    foldM
      reduceTrackedInput
      (initialKernel, [])
      rowsValue
  Right (finalKernel, reverse kernelRowsReversed)
  where
    reduceTrackedInput
      (kernelValue, kernelRowsReversed)
      (rowIndexValue, rowValue) = do
        (maybeKernelRow, nextKernel) <-
          admitTrackedLeftKernelRow
            context
            rowIndexValue
            rowValue
            kernelValue
        Right
          ( nextKernel
          , maybe
              kernelRowsReversed
              (: kernelRowsReversed)
              maybeKernelRow
          )

admitTrackedLeftKernelRow ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  Int ->
  SparseRow a ->
  TrackedLeftKernel a ->
  Either MoonlightError (Maybe (SparseRow a), TrackedLeftKernel a)
admitTrackedLeftKernelRow
  context
  rowIndexValue
  rowValue
  kernelValue@TrackedLeftKernel {tlkSeenRows}
    | rowIndexValue < 0 =
        Left
          ( InvariantViolation
              ( context
                  <> ": negative tracked row index "
                  <> show rowIndexValue
              )
          )
    | IntSet.member rowIndexValue tlkSeenRows =
        Left
          ( InvariantViolation
              ( context
                  <> ": duplicate tracked row index "
                  <> show rowIndexValue
              )
          )
    | otherwise = do
        witnessRow <-
          sparseRowFromEntries
            (context <> ": tracked witness")
            [(rowIndexValue, 1)]
        admitPreparedTrackedLeftKernelRow
          context
          rowIndexValue
          rowValue
          witnessRow
          kernelValue

prependStableSparseSpanRows ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  [SparseRow a] ->
  [SparseRow a] ->
  SparseSpan a ->
  Either MoonlightError (Maybe (SparseSpan a))
prependStableSparseSpanRows context prefixRows suffixRows suffixSpan = do
  prefixSpan@(SparseSpan prefixBasis) <-
    spanFromRows
      context
      prefixRows
  let SparseSpan suffixBasis =
        suffixSpan
      prefixPivotColumns =
        sparseSpanPivotColumns prefixSpan
      suffixUnaffected =
        rowsAvoidPivotColumns prefixPivotColumns suffixRows
  if suffixUnaffected
      && disjointIntMapKeys prefixBasis suffixBasis
    then Right (Just (SparseSpan (IntMap.union prefixBasis suffixBasis)))
    else Right Nothing

prependSparseSpanRows ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  [SparseRow a] ->
  [SparseRow a] ->
  SparseSpan a ->
  Either MoonlightError (SparseSpan a)
prependSparseSpanRows context prefixRows suffixRows suffixSpan = do
  prefixSpan@(SparseSpan prefixBasis) <-
    spanFromRows
      context
      prefixRows
  let SparseSpan suffixBasis =
        suffixSpan
      prefixPivotColumns =
        sparseSpanPivotColumns prefixSpan
      suffixUnaffected =
        rowsAvoidPivotColumns prefixPivotColumns suffixRows
  if suffixUnaffected
      && disjointIntMapKeys prefixBasis suffixBasis
    then Right (SparseSpan (IntMap.union prefixBasis suffixBasis))
    else
      spanAfterPrefixReducedRows
        context
        prefixSpan
        (prefixReducedRows prefixSpan prefixPivotColumns suffixRows)

prependStableTrackedLeftKernelRows ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  [(Int, SparseRow a)] ->
  [(Int, SparseRow a)] ->
  TrackedLeftKernel a ->
  [SparseRow a] ->
  Either MoonlightError (Maybe (TrackedLeftKernel a, [SparseRow a]))
prependStableTrackedLeftKernelRows context prefixRows suffixRows suffixKernel suffixKernelRows = do
  (prefixKernel, prefixKernelRows) <-
    trackedLeftKernelRows
      context
      emptyTrackedLeftKernel
      prefixRows
  let prefixPivotColumns =
        trackedKernelPivotColumns prefixKernel
      suffixUnaffected =
        keyedRowsAvoidPivotColumns prefixPivotColumns suffixRows
  if suffixUnaffected
      && disjointIntMapKeys (tlkBasisRows prefixKernel) (tlkBasisRows suffixKernel)
      && IntSet.null (IntSet.intersection (tlkSeenRows prefixKernel) (tlkSeenRows suffixKernel))
    then
      Right
        ( Just
            ( TrackedLeftKernel
                { tlkSeenRows =
                    IntSet.union
                      (tlkSeenRows prefixKernel)
                      (tlkSeenRows suffixKernel)
                , tlkBasisRows =
                    IntMap.union
                      (tlkBasisRows prefixKernel)
                      (tlkBasisRows suffixKernel)
                }
            , prefixKernelRows <> suffixKernelRows
            )
        )
    else Right Nothing

prependTrackedLeftKernelRows ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  [(Int, SparseRow a)] ->
  [(Int, SparseRow a)] ->
  TrackedLeftKernel a ->
  [SparseRow a] ->
  Either MoonlightError (TrackedLeftKernel a, [SparseRow a])
prependTrackedLeftKernelRows context prefixRows suffixRows suffixKernel suffixKernelRows = do
  (prefixKernel, prefixKernelRows) <-
    trackedLeftKernelRows
      context
      emptyTrackedLeftKernel
      prefixRows
  let prefixPivotColumns =
        trackedKernelPivotColumns prefixKernel
      suffixUnaffected =
        keyedRowsAvoidPivotColumns prefixPivotColumns suffixRows
  if suffixUnaffected
      && disjointIntMapKeys (tlkBasisRows prefixKernel) (tlkBasisRows suffixKernel)
      && IntSet.null (IntSet.intersection (tlkSeenRows prefixKernel) (tlkSeenRows suffixKernel))
    then
      Right
        ( TrackedLeftKernel
            { tlkSeenRows =
                IntSet.union
                  (tlkSeenRows prefixKernel)
                  (tlkSeenRows suffixKernel)
            , tlkBasisRows =
                IntMap.union
                  (tlkBasisRows prefixKernel)
                  (tlkBasisRows suffixKernel)
            }
        , prefixKernelRows <> suffixKernelRows
        )
    else do
      (nextKernel, kernelRows) <-
        trackedLeftKernelRowsAfterPrefix
          context
          prefixPivotColumns
          prefixKernel
          prefixKernelRows
          suffixRows
      Right (nextKernel, kernelRows)

trackedLeftKernelRowsAfterPrefix ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  IntSet ->
  TrackedLeftKernel a ->
  [SparseRow a] ->
  [(Int, SparseRow a)] ->
  Either MoonlightError (TrackedLeftKernel a, [SparseRow a])
trackedLeftKernelRowsAfterPrefix context prefixPivotColumns prefixKernel prefixKernelRows suffixRows = do
  (nextKernel, suffixKernelRowsReversed) <-
    foldM
      reduceSuffixInput
      (prefixKernel, [])
      suffixRows
  Right
    ( nextKernel
    , prefixKernelRows <> reverse suffixKernelRowsReversed
    )
  where
    reduceSuffixInput
      (kernelValue, kernelRowsReversed)
      (rowIndexValue, rowValue) = do
        witnessRow <-
          sparseRowFromEntries
            (context <> ": tracked witness")
            [(rowIndexValue, 1)]
        let (prefixReducedData, prefixReducedWitness) =
              if rowAvoidsPivotColumns prefixPivotColumns rowValue
                then (rowValue, witnessRow)
                else
                  reduceTrackedAgainstBasis
                    (tlkBasisRows prefixKernel)
                    rowValue
                    witnessRow
        (maybeKernelRow, nextKernel) <-
          admitPreparedTrackedLeftKernelRow
            context
            rowIndexValue
            prefixReducedData
            prefixReducedWitness
            kernelValue
        Right
          ( nextKernel
          , maybe
              kernelRowsReversed
              (: kernelRowsReversed)
              maybeKernelRow
          )

admitPreparedTrackedLeftKernelRow ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  Int ->
  SparseRow a ->
  SparseRow a ->
  TrackedLeftKernel a ->
  Either MoonlightError (Maybe (SparseRow a), TrackedLeftKernel a)
admitPreparedTrackedLeftKernelRow
  context
  rowIndexValue
  rowValue
  witnessRow
  kernelValue@TrackedLeftKernel {tlkSeenRows, tlkBasisRows}
    | rowIndexValue < 0 =
        Left
          ( InvariantViolation
              ( context
                  <> ": negative tracked row index "
                  <> show rowIndexValue
              )
          )
    | IntSet.member rowIndexValue tlkSeenRows =
        Left
          ( InvariantViolation
              ( context
                  <> ": duplicate tracked row index "
                  <> show rowIndexValue
              )
          )
    | otherwise = do
        let (reducedData, reducedWitness) =
              reduceTrackedAgainstBasis
                tlkBasisRows
                rowValue
                witnessRow
            nextSeenRows =
              IntSet.insert rowIndexValue tlkSeenRows
        case sparsePivot reducedData of
          Nothing ->
            Right
              ( Just reducedWitness
              , kernelValue {tlkSeenRows = nextSeenRows}
              )
          Just (pivotIndex, pivotValue) -> do
            pivotInverse <-
              requireInvertible
                ( InvariantViolation
                    ( context
                        <> ": Field.tryInv failed on a nonzero sparse pivot"
                    )
                )
                pivotValue
            let !normalizedData =
                  scaleSparseRow pivotInverse reducedData
                !normalizedWitness =
                  scaleSparseRow pivotInverse reducedWitness
            Right
              ( Nothing
              , TrackedLeftKernel
                  { tlkSeenRows = nextSeenRows
                  , tlkBasisRows =
                      IntMap.insert
                        pivotIndex
                        TrackedEchelonRow
                          { terData = normalizedData
                          , terWitness = normalizedWitness
                          }
                        tlkBasisRows
                  }
              )

spanAfterPrefixReducedRows ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  SparseSpan a ->
  [SparseRow a] ->
  Either MoonlightError (SparseSpan a)
spanAfterPrefixReducedRows context prefixSpan rowsValue =
  foldM
    addRow
    prefixSpan
    rowsValue
  where
    addRow spanValue rowValue =
      snd
        <$> admitSparseRow
          context
          rowValue
          spanValue

prefixReducedRows ::
  (IntegralDomain a, Num a) =>
  SparseSpan a ->
  IntSet ->
  [SparseRow a] ->
  [SparseRow a]
prefixReducedRows prefixSpan prefixPivotColumns rowsValue
  | IntSet.null prefixPivotColumns =
      rowsValue
  | otherwise =
      fmap reduceIfNecessary rowsValue
  where
    reduceIfNecessary rowValue
      | rowAvoidsPivotColumns prefixPivotColumns rowValue =
          rowValue
      | otherwise =
          reduceAgainstSpan prefixSpan rowValue

rowsAvoidPivotColumns :: IntSet -> [SparseRow a] -> Bool
rowsAvoidPivotColumns pivotColumns rowsValue
  | IntSet.null pivotColumns =
      True
  | otherwise =
      all (rowAvoidsPivotColumns pivotColumns) rowsValue

keyedRowsAvoidPivotColumns :: IntSet -> [(Int, SparseRow a)] -> Bool
keyedRowsAvoidPivotColumns pivotColumns rowsValue
  | IntSet.null pivotColumns =
      True
  | otherwise =
      all (rowAvoidsPivotColumns pivotColumns . snd) rowsValue

rowAvoidsPivotColumns :: IntSet -> SparseRow a -> Bool
rowAvoidsPivotColumns pivotColumns (SparseRow entries)
  | IntSet.null pivotColumns =
      True
  | otherwise =
      IntMap.foldrWithKey
        ( \indexValue _ remainingDisjoint ->
            not (IntSet.member indexValue pivotColumns) && remainingDisjoint
        )
        True
        entries

sparseSpanPivotColumns :: SparseSpan a -> IntSet
sparseSpanPivotColumns (SparseSpan basisRows) =
  IntMap.keysSet basisRows

trackedKernelPivotColumns :: TrackedLeftKernel a -> IntSet
trackedKernelPivotColumns TrackedLeftKernel {tlkBasisRows} =
  IntMap.keysSet tlkBasisRows

reduceAgainstSpan ::
  (IntegralDomain a, Num a) =>
  SparseSpan a ->
  SparseRow a ->
  SparseRow a
reduceAgainstSpan (SparseSpan basisRows) initialRow
  | IntMap.null basisRows =
      initialRow
  | otherwise =
      reduceAtOrAfter 0 initialRow
  where
    reduceAtOrAfter !cursor !rowValue =
      case IntMap.lookupGE cursor (sparseRowEntries rowValue) of
        Nothing ->
          rowValue
        Just (columnIndex, coefficientValue) ->
          let !nextRow =
                case IntMap.lookup columnIndex basisRows of
                  Nothing ->
                    rowValue
                  Just basisRow ->
                    addScaledSparseRow
                      (negate coefficientValue)
                      basisRow
                      rowValue
           in if columnIndex == maxBound
                then nextRow
                else reduceAtOrAfter (columnIndex + 1) nextRow

reduceTrackedAgainstBasis ::
  (IntegralDomain a, Num a) =>
  IntMap (TrackedEchelonRow a) ->
  SparseRow a ->
  SparseRow a ->
  (SparseRow a, SparseRow a)
reduceTrackedAgainstBasis basisRows initialData initialWitness
  | IntMap.null basisRows =
      (initialData, initialWitness)
  | otherwise =
      reduceAtOrAfter 0 (initialData, initialWitness)
  where
    reduceAtOrAfter !cursor !(dataRow, witnessRow) =
      case IntMap.lookupGE cursor (sparseRowEntries dataRow) of
        Nothing ->
          (dataRow, witnessRow)
        Just (columnIndex, coefficientValue) ->
          let !nextRows =
                case IntMap.lookup columnIndex basisRows of
                  Nothing ->
                    (dataRow, witnessRow)
                  Just TrackedEchelonRow {terData, terWitness} ->
                    let !eliminationCoefficient =
                          negate coefficientValue
                     in ( addScaledSparseRow
                            eliminationCoefficient
                            terData
                            dataRow
                        , addScaledSparseRow
                            eliminationCoefficient
                            terWitness
                            witnessRow
                        )
           in if columnIndex == maxBound
                then nextRows
                else reduceAtOrAfter (columnIndex + 1) nextRows

normalizeSparseRow ::
  (Field a, IntegralDomain a, Num a) =>
  String ->
  a ->
  SparseRow a ->
  Either MoonlightError (SparseRow a)
normalizeSparseRow context pivotValue rowValue = do
  pivotInverse <-
    requireInvertible
      ( InvariantViolation
          ( context
              <> ": Field.tryInv failed on a nonzero sparse pivot"
          )
      )
      pivotValue
  Right (scaleSparseRow pivotInverse rowValue)

scaleSparseRow ::
  (IntegralDomain a, Num a) =>
  a ->
  SparseRow a ->
  SparseRow a
scaleSparseRow coefficientValue (SparseRow entries)
  | isZero coefficientValue =
      SparseRow IntMap.empty
  | otherwise =
      SparseRow
        ( IntMap.mapMaybe
            ( \entryValue ->
                let !scaledValue =
                      coefficientValue * entryValue
                 in if isZero scaledValue
                      then Nothing
                      else Just scaledValue
            )
            entries
        )

addScaledSparseRow ::
  (IntegralDomain a, Num a) =>
  a ->
  SparseRow a ->
  SparseRow a ->
  SparseRow a
addScaledSparseRow coefficientValue (SparseRow sourceEntries) targetRow
  | isZero coefficientValue =
      targetRow
  | otherwise =
      IntMap.foldlWithKey'
        addEntry
        targetRow
        sourceEntries
  where
    addEntry accumulatedRow indexValue sourceValue =
      insertCoefficient
        indexValue
        (coefficientValue * sourceValue)
        accumulatedRow

insertCoefficient ::
  (IntegralDomain a, Num a) =>
  Int ->
  a ->
  SparseRow a ->
  SparseRow a
insertCoefficient indexValue coefficientValue rowValue@(SparseRow entries)
  | isZero coefficientValue =
      rowValue
  | otherwise =
      SparseRow (IntMap.alter updateEntry indexValue entries)
  where
    updateEntry Nothing =
      Just coefficientValue
    updateEntry (Just oldValue) =
      let !nextValue =
            oldValue + coefficientValue
       in if isZero nextValue
            then Nothing
            else Just nextValue

sparsePivot :: SparseRow a -> Maybe (Int, a)
sparsePivot (SparseRow entries) =
  IntMap.lookupMin entries

disjointIntMapKeys :: IntMap a -> IntMap b -> Bool
disjointIntMapKeys leftMap rightMap =
  IntMap.null (IntMap.intersection leftMap rightMap)
