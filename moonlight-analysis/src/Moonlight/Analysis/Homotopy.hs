module Moonlight.Analysis.Homotopy
  ( NerveHomotopyProfile (..),
    CellularHomotopyModel (..),
    boundaryFromOrientationMap,
    chainComplexFromModel,
    bettiProfileOfSite,
    homotopyProfileOfSite,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Homology
  ( BoundaryIncidence,
    FiniteChainComplex,
    HomologicalDegree (..),
    HomologyFailure,
    emptyBoundaryIncidence,
    emptyBoundaryIncidenceOf,
    freeBettiVector,
    materializeIncidenceBoundary,
    mkFiniteChainComplexChecked,
  )
import Moonlight.Pale.Diagnostic.Site.Homotopy (NerveHomotopyProfile (..))

type CellularHomotopyModel :: Type -> Type -> Type
data CellularHomotopyModel site cell = CellularHomotopyModel
  { chmMaxDimension :: site -> Int,
    chmCellsAtDimension :: Int -> site -> [cell],
    chmBoundaryOf :: Int -> site -> cell -> [(Int, cell)]
  }

boundaryFromOrientationMap ::
  Eq cell =>
  Map (cell, cell) Int ->
  cell ->
  [(Int, cell)]
boundaryFromOrientationMap orientationByPair sourceCell =
  fmap
    (\((_, targetCell), orientationValue) -> (orientationValue, targetCell))
    (filter (\((candidateSource, _), _) -> candidateSource == sourceCell) (Map.toList orientationByPair))

chainComplexFromModel ::
  Ord cell =>
  CellularHomotopyModel site cell ->
  site ->
  Either HomologyFailure (FiniteChainComplex Int)
chainComplexFromModel homotopyModel siteValue =
  let maxDimensionValue = chmMaxDimension homotopyModel siteValue
      positiveDimensions = [1 .. maxDimensionValue]
   in do
        incidenceByDimension <-
          Map.fromList <$> traverse (dimensionIncidence homotopyModel siteValue) positiveDimensions
        mkFiniteChainComplexChecked
          (HomologicalDegree maxDimensionValue)
          (\(HomologicalDegree dimensionValue) ->
              if dimensionValue <= 0
                then zeroBoundary homotopyModel siteValue
                else Map.findWithDefault emptyBoundaryIncidence dimensionValue incidenceByDimension
          )

bettiProfileOfSite ::
  Ord cell =>
  CellularHomotopyModel site cell ->
  site ->
  Either HomologyFailure [Int]
bettiProfileOfSite homotopyModel siteValue =
  freeBettiVector <$> chainComplexFromModel homotopyModel siteValue

homotopyProfileOfSite ::
  Ord cell =>
  Int ->
  CellularHomotopyModel site cell ->
  site ->
  Either HomologyFailure NerveHomotopyProfile
homotopyProfileOfSite connectedComponentCount homotopyModel siteValue =
  fmap
    (\bettiVectorValue ->
        NerveHomotopyProfile
          { nhpConnectedComponents = connectedComponentCount,
            nhpBettiVector = bettiVectorValue
          }
    )
    (bettiProfileOfSite homotopyModel siteValue)

dimensionIncidence ::
  Ord cell =>
  CellularHomotopyModel site cell ->
  site ->
  Int ->
  Either HomologyFailure (Int, BoundaryIncidence Int)
dimensionIncidence homotopyModel siteValue sourceDimensionValue =
  fmap
    (\incidenceValue -> (sourceDimensionValue, incidenceValue))
    (incidenceAtDimension homotopyModel siteValue sourceDimensionValue)

incidenceAtDimension ::
  Ord cell =>
  CellularHomotopyModel site cell ->
  site ->
  Int ->
  Either HomologyFailure (BoundaryIncidence Int)
incidenceAtDimension homotopyModel siteValue sourceDimensionValue =
  if sourceDimensionValue <= 0
    then Right emptyBoundaryIncidence
    else
      materializeIncidenceBoundary
        (chmBoundaryOf homotopyModel sourceDimensionValue siteValue)
        (chmCellsAtDimension homotopyModel sourceDimensionValue siteValue)
        (chmCellsAtDimension homotopyModel (sourceDimensionValue - 1) siteValue)

zeroBoundary ::
  CellularHomotopyModel site cell ->
  site ->
  BoundaryIncidence Int
zeroBoundary homotopyModel siteValue =
  emptyBoundaryIncidenceOf
    (fromIntegral (length (chmCellsAtDimension homotopyModel 0 siteValue)))
    0
