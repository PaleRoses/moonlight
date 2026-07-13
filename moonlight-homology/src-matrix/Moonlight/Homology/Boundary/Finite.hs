module Moonlight.Homology.Boundary.Finite
  ( FiniteChainComplex,
    mkFiniteChainComplex,
    mkFiniteChainComplexChecked,
    maxHomologicalDegree,
    incidenceMatrixAt,
    degreeCardinality,
    basisCellNodeId,
    finiteChainBasisRefsAtDegree,
    basisIndexCellMapAtDegree,
    inverseBasisRefMap,
    validateFiniteChainComplexShape,
    restrictComplex,
  )
where

import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Moonlight.Core (Semiring)
import Moonlight.Homology.Boundary.LinAlg
  ( BoundaryIncidence,
    boundaryCoefficient,
    boundaryEntries,
    composeBoundaryIncidence,
    emptyBoundaryIncidence,
    emptyBoundaryIncidenceOf,
    materializeIncidenceBoundary,
    sourceCardinality,
    sourceIndex,
    targetCardinality,
    targetIndex,
  )
import Moonlight.Homology.Pure.Degree (HomologicalDegree (..), decrementDegree)
import Moonlight.Homology.Pure.Failure (HomologyFailure (..))
import Moonlight.Homology.Pure.Carrier (BasisCellRef (..))

type FiniteChainComplex :: Type -> Type
data FiniteChainComplex r = FiniteChainComplex
  { maxDimension :: Int,
    incidenceMatrix :: Int -> BoundaryIncidence r
  }

mkFiniteChainComplex :: HomologicalDegree -> (HomologicalDegree -> BoundaryIncidence r) -> FiniteChainComplex r
mkFiniteChainComplex (HomologicalDegree maxDimensionValue) incidenceLookup =
  FiniteChainComplex
    { maxDimension = maxDimensionValue,
      incidenceMatrix = incidenceLookup . HomologicalDegree
    }

mkFiniteChainComplexChecked ::
  (Eq r, Num r, Semiring r) =>
  HomologicalDegree ->
  (HomologicalDegree -> BoundaryIncidence r) ->
  Either HomologyFailure (FiniteChainComplex r)
mkFiniteChainComplexChecked maxDegree incidenceLookup = do
  let finite = mkFiniteChainComplex maxDegree incidenceLookup
  validateFiniteChainComplexShape finite
  traverse_ (adjacentNilpotenceAt finite) [0 .. maxDimension finite - 1]
  pure finite

adjacentNilpotenceAt ::
  (Eq r, Num r, Semiring r) =>
  FiniteChainComplex r ->
  Int ->
  Either HomologyFailure ()
adjacentNilpotenceAt finite degreeIndex =
  composeBoundaryIncidence
    (incidenceMatrixAt finite (HomologicalDegree degreeIndex))
    (incidenceMatrixAt finite (HomologicalDegree (degreeIndex + 1)))
    & either
      (Left . InvalidBoundaryIncidence . show)
      ( \composed ->
          if null (boundaryEntries composed)
            then Right ()
            else Left (ChainComplexNilpotenceViolation degreeIndex)
      )

maxHomologicalDegree :: FiniteChainComplex r -> HomologicalDegree
maxHomologicalDegree =
  HomologicalDegree . maxDimension

incidenceMatrixAt :: FiniteChainComplex r -> HomologicalDegree -> BoundaryIncidence r
incidenceMatrixAt finite =
  incidenceMatrix finite . unHomologicalDegree

degreeCardinality :: FiniteChainComplex r -> HomologicalDegree -> Int
degreeCardinality finite degreeValue@(HomologicalDegree degreeIndex) =
  case maxHomologicalDegree finite of
    HomologicalDegree maxDegreeValue
      | degreeIndex < 0 -> 0
      | degreeIndex > maxDegreeValue -> 0
      | otherwise -> sourceCardinality (incidenceMatrixAt finite degreeValue)

basisCellNodeId :: FiniteChainComplex r -> BasisCellRef -> Int
basisCellNodeId finite basisCellRef =
  case cellDegree basisCellRef of
    HomologicalDegree degreeValue ->
      sum
        (fmap (degreeCardinality finite . HomologicalDegree) [0 .. degreeValue - 1])
        + cellIndex basisCellRef

finiteChainBasisRefsAtDegree :: FiniteChainComplex r -> HomologicalDegree -> [BasisCellRef]
finiteChainBasisRefsAtDegree finite degreeValue =
  fmap
    (\cellIndexValue -> BasisCellRef {cellDegree = degreeValue, cellIndex = cellIndexValue})
    [0 .. degreeCardinality finite degreeValue - 1]

basisIndexCellMapAtDegree ::
  HomologicalDegree ->
  Map.Map cell BasisCellRef ->
  Map.Map Int cell
basisIndexCellMapAtDegree degreeValue =
  Map.foldrWithKey
    ( \cellValue basisCellRef ->
        if cellDegree basisCellRef == degreeValue
          then Map.insert (cellIndex basisCellRef) cellValue
          else id
    )
    Map.empty

inverseBasisRefMap :: Map.Map cell BasisCellRef -> Map.Map BasisCellRef cell
inverseBasisRefMap =
  Map.fromList . fmap (\(cellValue, basisRef) -> (basisRef, cellValue)) . Map.toList

validateFiniteChainComplexShape :: FiniteChainComplex r -> Either HomologyFailure ()
validateFiniteChainComplexShape finite =
  dimensionsOf finite
    & mapMaybe (shapeViolation finite)
    & safeHead
    & maybe (Right ()) Left

dimensionsOf :: FiniteChainComplex r -> [HomologicalDegree]
dimensionsOf finite =
  fmap HomologicalDegree [0 .. maxDimension finite]

shapeViolation :: FiniteChainComplex r -> HomologicalDegree -> Maybe HomologyFailure
shapeViolation finite degreeValue@(HomologicalDegree degreeIndex) =
  let incidence = incidenceMatrixAt finite degreeValue
      expectedTarget =
        if degreeIndex <= 0
          then 0
          else sourceCardinality (incidenceMatrixAt finite (decrementDegree degreeValue))
   in if targetCardinality incidence == expectedTarget
        then Nothing
        else Just (ChainComplexShapeMismatch degreeIndex expectedTarget (targetCardinality incidence))

restrictComplex :: Set BasisCellRef -> FiniteChainComplex Int -> Either HomologyFailure (FiniteChainComplex Int)
restrictComplex activeCells finiteComplex =
  let retainedByDegree = groupByDegree (Set.toAscList activeCells)
      maxDimensionValue = maximum (0 : Map.keys retainedByDegree)
   in mkFiniteChainComplexChecked
        (HomologicalDegree maxDimensionValue)
        (\(HomologicalDegree degreeValue) -> restrictedBoundary activeCells retainedByDegree finiteComplex degreeValue)

restrictedBoundary ::
  Set BasisCellRef ->
  Map.Map Int [BasisCellRef] ->
  FiniteChainComplex Int ->
  Int ->
  BoundaryIncidence Int
restrictedBoundary activeCells retainedByDegree finiteComplex dimensionValue
  | dimensionValue <= 0 =
      emptyBoundaryIncidenceOf
        (fromIntegral (length (Map.findWithDefault [] 0 retainedByDegree)))
        0
  | otherwise =
      either
        (const emptyBoundaryIncidence)
        id
        ( materializeIncidenceBoundary
            (restrictedBoundaryOf activeCells finiteComplex)
            (Map.findWithDefault [] dimensionValue retainedByDegree)
            (Map.findWithDefault [] (dimensionValue - 1) retainedByDegree)
        )

restrictedBoundaryOf ::
  Set BasisCellRef ->
  FiniteChainComplex Int ->
  BasisCellRef ->
  [(Int, BasisCellRef)]
restrictedBoundaryOf activeCells finiteComplex sourceCell =
  let dimensionValue = unHomologicalDegree (cellDegree sourceCell)
      incidence = incidenceMatrixAt finiteComplex (HomologicalDegree dimensionValue)
   in boundaryEntries incidence
        & filter (\entryValue -> sourceIndex entryValue == cellIndex sourceCell)
        & mapMaybe
          ( \entryValue ->
              let targetCell =
                    BasisCellRef
                      { cellDegree = HomologicalDegree (dimensionValue - 1),
                        cellIndex = targetIndex entryValue
                      }
               in if Set.member targetCell activeCells
                    then Just (boundaryCoefficient entryValue, targetCell)
                    else Nothing
          )

groupByDegree :: [BasisCellRef] -> Map.Map Int [BasisCellRef]
groupByDegree =
  foldr
    (\cellRef -> Map.insertWith (<>) (unHomologicalDegree (cellDegree cellRef)) [cellRef])
    Map.empty

safeHead :: [a] -> Maybe a
safeHead values =
  case values of
    firstValue : _ -> Just firstValue
    [] -> Nothing
