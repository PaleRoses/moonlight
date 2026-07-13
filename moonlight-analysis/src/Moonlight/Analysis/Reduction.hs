module Moonlight.Analysis.Reduction
  ( MorseReduction (..),
    ReductionScaffold (..),
    buildReduction,
    reducedComplex,
    criticalCells,
    collapsedDerivations,
    potentialField,
  )
where

import Data.Function ((&))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Homology
  ( AcyclicMatching (..),
    AcyclicPair (..),
    BasisCellRef (..),
    CellCarrier,
    CollapseObstruction,
    FiniteChainComplex,
    Graph1Skeleton (..),
    HomologicalDegree (..),
    HomologyFailure (..),
    MacroScaffoldIR (..),
    MorseComplex (..),
    MorseReebNode,
    MorseReebScaffold (..),
    PotentialNormalization (..),
    ScalarPotentialField,
    graph1SkeletonFromComplex,
    graphMacroScaffold,
    mkCellCarrier,
    mkScalarPotentialFieldFromSamples,
  )

type MorseReduction :: Type -> Type -> Type -> Type
data MorseReduction site cell witness = MorseReduction
  { mrSite :: site,
    mrOriginalComplex :: FiniteChainComplex Int,
    mrReducedComplex :: FiniteChainComplex Int,
    mrPotential :: ScalarPotentialField,
    mrSkeleton :: Graph1Skeleton,
    mrCriticalNodes :: [MorseReebNode],
    mrCriticalCells :: [cell],
    mrCollapsedPairs :: [(cell, cell)],
    mrCollapsedDerivations :: [(cell, cell, witness)],
    mrRetainedCells :: [cell],
    mrMatching :: AcyclicMatching,
    mrObstructions :: [CollapseObstruction],
    mrMorseComplex :: MorseComplex Int
  }

type ReductionScaffold :: Type -> Type -> Type -> Type
data ReductionScaffold site cell witness = ReductionScaffold
  { rsSite :: site,
    rsOriginalComplex :: FiniteChainComplex Int,
    rsMorseComplex :: MorseComplex Int,
    rsBasisRefs :: Map cell BasisCellRef,
    rsZeroCells :: [cell],
    rsIncidentUpperCells :: cell -> [cell],
    rsCellWeight :: cell -> Double,
    rsUpperWitnessAtCell :: cell -> witness
  }

buildReduction ::
  ReductionScaffold site cell witness ->
  Either HomologyFailure (MorseReduction site cell witness)
buildReduction reductionScaffold = do
  let originalComplex = rsOriginalComplex reductionScaffold
      morseValue = rsMorseComplex reductionScaffold
      matchingValue = mcMatching morseValue
      skeletonValue = reductionSkeleton (rsZeroCells reductionScaffold) originalComplex
  potentialValue <- potentialField reductionScaffold
  scaffoldValue <- graphMacroScaffold potentialValue skeletonValue
  criticalCellValues <- translateCriticalCells reductionScaffold morseValue
  collapsedPairValues <- translateCollapsedPairs reductionScaffold matchingValue
  let collapsedDerivationValues =
        fmap
          (\(lowerCellValue, upperCellValue) -> (lowerCellValue, upperCellValue, rsUpperWitnessAtCell reductionScaffold upperCellValue))
          collapsedPairValues
  pure
    MorseReduction
      { mrSite = rsSite reductionScaffold,
        mrOriginalComplex = originalComplex,
        mrReducedComplex = mcReducedComplex morseValue,
        mrPotential = potentialValue,
        mrSkeleton = skeletonValue,
        mrCriticalNodes = morseReebNodes (macroScaffoldReeb scaffoldValue),
        mrCriticalCells = criticalCellValues,
        mrCollapsedPairs = collapsedPairValues,
        mrCollapsedDerivations = collapsedDerivationValues,
        mrRetainedCells = criticalCellValues,
        mrMatching = matchingValue,
        mrObstructions = amObstructions matchingValue,
        mrMorseComplex = morseValue
      }

reducedComplex :: MorseReduction site cell witness -> FiniteChainComplex Int
reducedComplex = mrReducedComplex

criticalCells ::
  ReductionScaffold site cell witness ->
  Either HomologyFailure [cell]
criticalCells reductionScaffold =
  mrCriticalCells <$> buildReduction reductionScaffold

collapsedDerivations ::
  MorseReduction site cell witness ->
  [(cell, cell, witness)]
collapsedDerivations = mrCollapsedDerivations

potentialField ::
  ReductionScaffold site cell witness ->
  Either HomologyFailure ScalarPotentialField
potentialField reductionScaffold = do
  carrierValue <- zeroCellCarrier (rsZeroCells reductionScaffold)
  mkScalarPotentialFieldFromSamples
    carrierValue
    NativePotentialScale
    (vertexSamples reductionScaffold (rsZeroCells reductionScaffold))
    & either
      (Left . InvalidTopologyInput . ("reduction potential construction failed: " <>) . show)
      Right

zeroCellCarrier :: [cell] -> Either HomologyFailure CellCarrier
zeroCellCarrier zeroCells =
  mkCellCarrier
    (HomologicalDegree 0)
    (fmap zeroBasisCellRef (enumerateFromZeroLocal (length zeroCells)))
    & either
      (Left . InvalidTopologyInput . ("zero-cell carrier construction failed: " <>) . show)
      Right

zeroBasisCellRef :: Int -> BasisCellRef
zeroBasisCellRef cellIndexValue =
  BasisCellRef
    { cellDegree = HomologicalDegree 0,
      cellIndex = cellIndexValue
    }

vertexSamples ::
  ReductionScaffold site cell witness ->
  [cell] ->
  Map BasisCellRef Double
vertexSamples reductionScaffold zeroCells =
  Map.fromList
    ( fmap
        (\(vertexIndexValue, cellValue) -> (zeroBasisCellRef vertexIndexValue, cellPotential reductionScaffold cellValue))
        (zip [0 :: Int ..] zeroCells)
    )

cellPotential :: ReductionScaffold site cell witness -> cell -> Double
cellPotential reductionScaffold zeroCell =
  let incidentUpperCells = rsIncidentUpperCells reductionScaffold zeroCell
      weightedCells =
        case incidentUpperCells of
          [] -> [zeroCell]
          _ -> incidentUpperCells
   in sum (fmap (rsCellWeight reductionScaffold) weightedCells)

reductionSkeleton :: [cell] -> FiniteChainComplex Int -> Graph1Skeleton
reductionSkeleton zeroCells chainComplex =
  either
    (const (emptySkeleton (length zeroCells)))
    id
    (graph1SkeletonFromComplex chainComplex)

emptySkeleton :: Int -> Graph1Skeleton
emptySkeleton vertexCountValue =
  Graph1Skeleton
    { graphVertexCount = vertexCountValue,
      graphEdges = [],
      graphEdgeAdjacency =
        Map.fromList
          (fmap (\vertexValue -> (vertexValue, [])) (enumerateFromZeroLocal vertexCountValue))
    }

translateCriticalCells ::
  ReductionScaffold site cell witness ->
  MorseComplex Int ->
  Either HomologyFailure [cell]
translateCriticalCells reductionScaffold morseValue =
  let cellByBasisRef = inverseBasisRefMapLocal (rsBasisRefs reductionScaffold)
   in traverse
        (\(_, originalBasisRef) -> lookupTranslatedCell cellByBasisRef originalBasisRef)
        (Map.toAscList (mcCriticalBasis morseValue))

translateCollapsedPairs ::
  ReductionScaffold site cell witness ->
  AcyclicMatching ->
  Either HomologyFailure [(cell, cell)]
translateCollapsedPairs reductionScaffold matchingValue =
  let cellByBasisRef = inverseBasisRefMapLocal (rsBasisRefs reductionScaffold)
   in traverse
        (\pairValue ->
            (,) <$> lookupTranslatedCell cellByBasisRef (apLowerCell pairValue)
                <*> lookupTranslatedCell cellByBasisRef (apUpperCell pairValue)
        )
        (amPairs matchingValue)

lookupTranslatedCell ::
  Map BasisCellRef cell ->
  BasisCellRef ->
  Either HomologyFailure cell
lookupTranslatedCell cellByBasisRef basisCellRef =
  maybe
    (Left (InvalidTopologyInput ("missing cell for basis ref " <> show basisCellRef)))
    Right
    (Map.lookup basisCellRef cellByBasisRef)

inverseBasisRefMapLocal :: Map cell BasisCellRef -> Map BasisCellRef cell
inverseBasisRefMapLocal =
  Map.fromList . fmap (\(cellValue, basisCellRef) -> (basisCellRef, cellValue)) . Map.toList

enumerateFromZeroLocal :: Int -> [Int]
enumerateFromZeroLocal upperBound
  | upperBound <= 0 = []
  | otherwise = [0 .. upperBound - 1]
