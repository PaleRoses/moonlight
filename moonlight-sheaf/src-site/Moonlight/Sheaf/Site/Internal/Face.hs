module Moonlight.Sheaf.Site.Internal.Face
  ( faceMorphismsForCellWith,
    faceOrientation,
    orientationMapForSourceDimension,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Moonlight.Category.Simplicial
  ( NerveSimplex,
    nerveSimplexDimension,
  )
import Numeric.Natural (Natural)

faceMorphismsForCellWith ::
  Ord key =>
  (Natural -> Natural -> NerveSimplex category -> Maybe (NerveSimplex category)) ->
  (cell -> NerveSimplex category) ->
  (NerveSimplex category -> key) ->
  (Natural -> Natural -> faceKind) ->
  (cell -> cell -> faceKind -> Natural -> Int -> face) ->
  Map key cell ->
  cell ->
  [face]
faceMorphismsForCellWith faceAt cellSimplex simplexKey faceKindAt mkFace cellsByKey sourceCell =
  mapMaybe faceFor [0 .. dimensionValue]
  where
    simplexValue = cellSimplex sourceCell
    dimensionValue = nerveSimplexDimension simplexValue

    faceFor faceIndex = do
      targetSimplex <- faceAt dimensionValue faceIndex simplexValue
      targetCell <- Map.lookup (simplexKey targetSimplex) cellsByKey
      pure
        ( mkFace
            sourceCell
            targetCell
            (faceKindAt dimensionValue faceIndex)
            faceIndex
            (faceOrientation faceIndex)
        )

faceOrientation :: Natural -> Int
faceOrientation faceIndex =
  if even faceIndex
    then 1
    else -1

orientationMapForSourceDimension ::
  Ord cell =>
  (face -> cell) ->
  (cell -> Int) ->
  (face -> cell) ->
  (face -> Int) ->
  Int ->
  [face] ->
  Map (cell, cell) Int
orientationMapForSourceDimension sourceCell cellDimension targetCell orientation sourceDimensionValue =
  Map.filter (/= 0)
    . Map.fromListWith (+)
    . fmap (\faceMorphism -> ((sourceCell faceMorphism, targetCell faceMorphism), orientation faceMorphism))
    . filter ((== sourceDimensionValue) . cellDimension . sourceCell)
