module Moonlight.Homology.Pure.Carrier
  ( BasisCellRef (..),
    CellCarrier,
    CellCarrierError (..),
    carrierDegree,
    carrierCells,
    mkCellCarrier,
  )
where

import Data.Kind (Type)
import Data.Function ((&))
import Data.List qualified as List
import Data.Set qualified as Set
import Moonlight.Homology.Pure.Degree (HomologicalDegree (..))

type BasisCellRef :: Type
data BasisCellRef = BasisCellRef
  { cellDegree :: HomologicalDegree,
    cellIndex :: Int
  }
  deriving stock (Eq, Ord, Show)

type CellCarrier :: Type
data CellCarrier = CellCarrier
  { carrierDegree :: HomologicalDegree,
    carrierCells :: [BasisCellRef]
  }
  deriving stock (Eq, Show)

type CellCarrierError :: Type
data CellCarrierError
  = CellCarrierDegreeMismatch HomologicalDegree BasisCellRef
  | CellCarrierCellsNotDistinct [BasisCellRef]
  deriving stock (Eq, Show)

mkCellCarrier :: HomologicalDegree -> [BasisCellRef] -> Either CellCarrierError CellCarrier
mkCellCarrier degreeValue cells =
  case List.find ((/= degreeValue) . cellDegree) cells of
    Just invalidCell ->
      Left (CellCarrierDegreeMismatch degreeValue invalidCell)
    Nothing ->
      case duplicateCells cells of
        [] ->
          Right
            CellCarrier
              { carrierDegree = degreeValue,
                carrierCells = cells
              }
        duplicateCellValues ->
          Left (CellCarrierCellsNotDistinct duplicateCellValues)

duplicateCells :: [BasisCellRef] -> [BasisCellRef]
duplicateCells cells =
  cells
    & List.foldl'
      ( \(seenCells, duplicateCellsValue) cellRefValue ->
          if Set.member cellRefValue seenCells
            then (seenCells, Set.insert cellRefValue duplicateCellsValue)
            else (Set.insert cellRefValue seenCells, duplicateCellsValue)
      )
      (Set.empty, Set.empty)
    & snd
    & Set.toAscList
