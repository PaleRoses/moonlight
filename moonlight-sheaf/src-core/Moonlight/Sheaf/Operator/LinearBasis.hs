{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Sheaf.Operator.LinearBasis
  ( LinearCoordinate,
    linearCoordinateCell,
    linearCoordinateLocalIndex,
    LinearBasis,
    mkLinearBasis,
    mkLinearBasisFromCellDimensions,
    mkLinearBasisByDenseCell,
    linearBasisCardinality,
    linearBasisCells,
    linearBasisCoordinates,
    linearBasisIndexedCoordinates,
    linearBasisSlotAtIndex,
    linearBasisCellOffset,
    linearBasisCellDimension,
    linearBasisCellSlot,
    linearBasisCellSlotByDenseKey,
    linearBasisCellSlotOrError,
  )
where

import Data.List (mapAccumL)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moonlight.Core (DenseKey (encodeDenseKey))
import Moonlight.Sheaf.Kernel.Basis
  ( SheafBasis,
    basisCells,
  )
import Moonlight.Sheaf.Operator.BuildError
  ( OperatorBasisRole,
    SheafOperatorBuildError (..),
  )

data LinearCoordinate cell = LinearCoordinate
  { linearCoordinateCell :: cell,
    linearCoordinateLocalIndex :: Int
  }
  deriving stock (Eq, Ord, Show)

data LinearBasis cell = LinearBasis
  { lbCoordinates :: !(Vector (LinearCoordinate cell)),
    lbSlotsInOrder :: !(Vector (cell, (Int, Int))),
    lbSlotIndexByCell :: !(Map cell Int),
    lbDenseSlotIndexByCell :: !(Maybe (IntMap Int)),
    lbTotalDimension :: !Int
  }
  deriving stock (Show)

instance Eq cell => Eq (LinearBasis cell) where
  leftBasis == rightBasis =
    lbTotalDimension leftBasis == lbTotalDimension rightBasis
      && lbSlotsInOrder leftBasis == lbSlotsInOrder rightBasis

mkLinearBasis ::
  forall cell.
  Ord cell =>
  (cell -> Int) ->
  SheafBasis cell ->
  Either (SheafOperatorBuildError cell) (LinearBasis cell)
mkLinearBasis dimensionOf basis = do
  cellDimensions <-
    traverse (\cell -> pure (cell, dimensionOf cell)) (basisCells basis)
  mkLinearBasisFromCellDimensions cellDimensions

mkLinearBasisFromCellDimensions ::
  Ord cell =>
  [(cell, Int)] ->
  Either (SheafOperatorBuildError cell) (LinearBasis cell)
mkLinearBasisFromCellDimensions cellDimensions = do
  case Map.lookupMin (Map.filter (> 1) occurrenceCounts) of
    Just (duplicateCell, _) ->
      Left (OperatorDuplicateBasisCell duplicateCell)
    Nothing ->
      pure ()
  checkedCellDimensions <- traverse cellDimension cellDimensions
  let (totalDimensionValue, orderedSlots) =
        mapAccumL buildSlot 0 checkedCellDimensions
  pure
    LinearBasis
      { lbCoordinates = Vector.fromList (foldMap coordinatesForSlot orderedSlots),
        lbSlotsInOrder = Vector.fromList orderedSlots,
        lbSlotIndexByCell =
          Map.fromList (zipWith slotIndexEntry [0 ..] orderedSlots),
        lbDenseSlotIndexByCell = Nothing,
        lbTotalDimension = totalDimensionValue
      }
  where
    occurrenceCounts =
      Map.fromListWith (+) (fmap (\(cell, _) -> (cell, 1 :: Int)) cellDimensions)

    cellDimension :: (cell, Int) -> Either (SheafOperatorBuildError cell) (cell, Int)
    cellDimension entry@(cell, dimensionValue) =
      if dimensionValue < 0
        then Left (OperatorNegativeStalkDimension cell dimensionValue)
        else Right entry

    buildSlot :: Int -> (cell, Int) -> (Int, (cell, (Int, Int)))
    buildSlot offsetValue (cell, dimensionValue) =
      ( offsetValue + dimensionValue,
        (cell, (offsetValue, dimensionValue))
      )

    coordinatesForSlot :: (cell, (Int, Int)) -> [LinearCoordinate cell]
    coordinatesForSlot (cell, (_, dimensionValue)) =
      fmap
        (LinearCoordinate cell)
        [0 .. dimensionValue - 1]

    slotIndexEntry :: Int -> (cell, (Int, Int)) -> (cell, Int)
    slotIndexEntry slotIndex (cell, _) =
      (cell, slotIndex)

mkLinearBasisByDenseCell ::
  forall cell.
  DenseKey cell =>
  (cell -> Int) ->
  SheafBasis cell ->
  Either (SheafOperatorBuildError cell) (LinearBasis cell)
mkLinearBasisByDenseCell dimensionOf basis = do
  linearBasis <- mkLinearBasis dimensionOf basis
  pure
    linearBasis
      { lbDenseSlotIndexByCell =
          Just
            ( IntMap.fromList
                (zipWith denseSlotIndexEntry [0 ..] (Vector.toList (lbSlotsInOrder linearBasis)))
            )
      }
  where
    denseSlotIndexEntry :: Int -> (cell, (Int, Int)) -> (Int, Int)
    denseSlotIndexEntry slotIndex (cell, _) =
      (encodeDenseKey cell, slotIndex)

linearBasisCardinality :: LinearBasis cell -> Int
linearBasisCardinality =
  lbTotalDimension

linearBasisCells :: LinearBasis cell -> [cell]
linearBasisCells =
  fmap fst . Vector.toList . lbSlotsInOrder

linearBasisCoordinates :: LinearBasis cell -> [LinearCoordinate cell]
linearBasisCoordinates =
  Vector.toList . lbCoordinates

linearBasisIndexedCoordinates :: LinearBasis cell -> [(Int, LinearCoordinate cell)]
linearBasisIndexedCoordinates basis =
  Vector.toList (Vector.indexed (lbCoordinates basis))

linearBasisSlotAtIndex :: Int -> LinearBasis cell -> Maybe (Int, Int)
linearBasisSlotAtIndex slotIndex basis =
  fmap snd (lbSlotsInOrder basis Vector.!? slotIndex)
{-# INLINE linearBasisSlotAtIndex #-}

linearBasisCellOffset :: Ord cell => cell -> LinearBasis cell -> Maybe Int
linearBasisCellOffset cell =
  fmap fst . linearBasisCellSlot cell

linearBasisCellDimension :: Ord cell => cell -> LinearBasis cell -> Maybe Int
linearBasisCellDimension cell =
  fmap snd . linearBasisCellSlot cell

linearBasisCellSlot :: Ord cell => cell -> LinearBasis cell -> Maybe (Int, Int)
linearBasisCellSlot cell basis = do
  slotIndex <- Map.lookup cell (lbSlotIndexByCell basis)
  linearBasisSlotAtIndex slotIndex basis

linearBasisCellSlotByDenseKey :: DenseKey cell => cell -> LinearBasis cell -> Maybe (Int, Int)
linearBasisCellSlotByDenseKey cell basis =
  maybe
    (linearBasisCellSlot cell basis)
    (\denseIndex -> IntMap.lookup (encodeDenseKey cell) denseIndex >>= (`linearBasisSlotAtIndex` basis))
    (lbDenseSlotIndexByCell basis)

linearBasisCellSlotOrError ::
  Ord cell =>
  OperatorBasisRole ->
  LinearBasis cell ->
  cell ->
  Either (SheafOperatorBuildError cell) (Int, Int)
linearBasisCellSlotOrError role basis cell =
  maybe
    (Left (OperatorCellAbsentFromBasis role cell))
    Right
    (linearBasisCellSlot cell basis)
