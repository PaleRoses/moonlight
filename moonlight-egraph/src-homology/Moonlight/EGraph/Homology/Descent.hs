module Moonlight.EGraph.Homology.Descent
  ( DescentPage (..),
    computeDescentPage,
    cohomologyBasisByDegree,
    touchedCodomainKeys,
    boundaryAtOrZero,
  )
where

import Data.Function ((&))
import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.EGraph.Homology.Gerbe
  ( automorphismChainComplex,
    effectiveAutomorphismCount,
  )
import Moonlight.EGraph.Homology.Representative
  ( representativeAbsoluteWeightMap,
    representativeKey,
    representativeSupportIndices,
  )
import Moonlight.Homology
  ( BasisCellRef (..),
    degreeCardinality,
    boundaryCoefficient,
    sourceIndex,
    targetIndex,
    BoundaryIncidence,
    boundaryEntries,
    Bidegree,
    FiniteChainComplex,
    FormalMap (..),
    HomologicalDegree (..),
    HomologyFailure,
    RepresentativeCocycle,
    SpectralPage,
    cohomologyBasisAt,
    computeRationalSpectralPages,
    directSumBoundaryIncidence,
    emptyBoundaryIncidenceOf,
    incidenceMatrixAt,
    maxHomologicalDegree,
    mkBidegree,
    mkFiniteChainComplexChecked,
    filteredReducedFiltration,
    filteredRefinedMorseComplex,
    frmcRefinedMorseComplex,
    rmcReducedComplex,
  )
import Moonlight.Homology.Matrix
  ( matrixRows,
    validatedMatrixFromColumns,
  )

type DescentPage :: Type -> Type
data DescentPage r = DescentPage
  { dpRow0 :: !(IntMap [RepresentativeCocycle r Int]),
    dpRow1 :: !(IntMap [RepresentativeCocycle r Int]),
    dpDifferentialD2 :: !(Map Bidegree (FormalMap r)),
    dpShadowPages :: ![SpectralPage Rational]
  }

computeDescentPage :: FiniteChainComplex Int -> IntMap Int -> Either HomologyFailure (DescentPage Rational)
computeDescentPage finite automorphismCounts = do
  automorphismFinite <- automorphismChainComplex finite automorphismCounts
  shadowComplex <- shadowCombinedComplex finite automorphismFinite
  let row0Basis = cohomologyBasisByDegree finite
      row1Basis = cohomologyBasisByDegree automorphismFinite
  differentialD2 <- d2Maps finite automorphismCounts row0Basis row1Basis
  shadowPages <-
    computeReducedShadowPages
      shadowComplex
      (shadowFiltration finite automorphismFinite)
  pure
    DescentPage
      { dpRow0 = row0Basis,
        dpRow1 = row1Basis,
        dpDifferentialD2 = differentialD2,
        dpShadowPages = shadowPages
      }

computeReducedShadowPages ::
  FiniteChainComplex Int ->
  (BasisCellRef -> Int) ->
  Either HomologyFailure [SpectralPage Rational]
computeReducedShadowPages finite originalFiltration = do
  filteredMorseValue <- filteredRefinedMorseComplex finite originalFiltration (const 0)
  let refinedMorseValue = frmcRefinedMorseComplex filteredMorseValue
  computeRationalSpectralPages
    (rmcReducedComplex refinedMorseValue)
    (filteredReducedFiltration filteredMorseValue)

cohomologyBasisByDegree :: FiniteChainComplex Int -> IntMap [RepresentativeCocycle Rational Int]
cohomologyBasisByDegree finite =
  [0 .. unHomologicalDegree (maxHomologicalDegree finite)]
    & fmap
      ( \degreeValue ->
          ( degreeValue,
            cohomologyBasisAt finite (HomologicalDegree degreeValue)
          )
      )
    & IntMap.fromList

touchedCodomainKeys :: DescentPage Rational -> Set String
touchedCodomainKeys descentPage =
  dpDifferentialD2 descentPage
    & Map.elems
    & foldMap
      ( \formalMapValue ->
          zip (formalCodomainBasis formalMapValue) (formalMatrix formalMapValue)
            & foldMap
              ( \(codomainRepresentative, matrixRow) ->
                  if any (/= 0) matrixRow
                    then Set.singleton (representativeKey codomainRepresentative)
                    else Set.empty
              )
      )

d2Maps ::
  FiniteChainComplex Int ->
  IntMap Int ->
  IntMap [RepresentativeCocycle Rational Int] ->
  IntMap [RepresentativeCocycle Rational Int] ->
  Either HomologyFailure (Map Bidegree (FormalMap Rational))
d2Maps finite automorphismCounts row0Basis row1Basis =
  Map.fromList . mapMaybe id
    <$> traverse
      ( \(degreeValue, domainBasis) ->
          let targetDegreeValue = degreeValue + 2
              codomainBasis = IntMap.findWithDefault [] targetDegreeValue row0Basis
           in if null domainBasis || null codomainBasis
                then Right Nothing
                else do
                  let columns =
                        fmap
                          (d2Column finite automorphismCounts degreeValue codomainBasis)
                          domainBasis
                  formalMatrixValue <-
                    matrixRows
                      <$> validatedMatrixFromColumns (length codomainBasis) columns
                  pure
                    ( Just
                        ( mkBidegree degreeValue 1,
                          FormalMap
                            { formalMatrix = formalMatrixValue,
                              formalDomainBasis = domainBasis,
                              formalCodomainBasis = codomainBasis
                            }
                        )
                    )
      )
      (IntMap.toAscList row1Basis)

d2Column ::
  FiniteChainComplex Int ->
  IntMap Int ->
  Int ->
  [RepresentativeCocycle Rational Int] ->
  RepresentativeCocycle Rational Int ->
  [Rational]
d2Column finite automorphismCounts degreeValue codomainBasis domainRepresentative =
  let liftedSupport = twoStepTransferWeights finite automorphismCounts degreeValue domainRepresentative
   in codomainBasis
        & fmap
          ( \codomainRepresentative ->
              representativeSupportIndices codomainRepresentative
                & IntSet.toAscList
                & fmap (codomainWeight liftedSupport)
                & sum
          )

codomainWeight :: Map Int Rational -> Int -> Rational
codomainWeight liftedSupport basisIndexValue =
  Map.findWithDefault 0 basisIndexValue liftedSupport

twoStepTransferWeights ::
  FiniteChainComplex Int ->
  IntMap Int ->
  Int ->
  RepresentativeCocycle Rational Int ->
  Map Int Rational
twoStepTransferWeights finite automorphismCounts degreeValue representativeValue =
  let supportWeights = representativeAbsoluteWeightMap representativeValue
      firstBoundary = incidenceMatrixAt finite (HomologicalDegree (degreeValue + 1))
      secondBoundary = incidenceMatrixAt finite (HomologicalDegree (degreeValue + 2))
      firstLift =
        boundaryEntries firstBoundary
          & foldr
            ( \entryValue ->
                case Map.lookup (targetIndex entryValue) supportWeights of
                  Nothing -> id
                  Just coefficientValue ->
                    let sourceBasisRef = BasisCellRef (HomologicalDegree (degreeValue + 1)) (sourceIndex entryValue)
                        sourceWeight = effectiveAutomorphismCount finite automorphismCounts sourceBasisRef
                        liftedWeight = coefficientValue * fromIntegral (abs (boundaryCoefficient entryValue) * max 0 sourceWeight)
                     in if liftedWeight == 0
                          then id
                          else Map.insertWith (+) (sourceIndex entryValue) liftedWeight
            )
            Map.empty
   in boundaryEntries secondBoundary
        & foldr
          ( \entryValue ->
              case Map.lookup (targetIndex entryValue) firstLift of
                Nothing -> id
                Just coefficientValue ->
                  let sourceBasisRef = BasisCellRef (HomologicalDegree (degreeValue + 2)) (sourceIndex entryValue)
                      sourceWeight = effectiveAutomorphismCount finite automorphismCounts sourceBasisRef
                      liftedWeight = coefficientValue * fromIntegral (abs (boundaryCoefficient entryValue) * max 0 sourceWeight)
                   in if liftedWeight == 0
                        then id
                        else Map.insertWith (+) (sourceIndex entryValue) liftedWeight
          )
          Map.empty

shadowCombinedComplex :: FiniteChainComplex Int -> FiniteChainComplex Int -> Either HomologyFailure (FiniteChainComplex Int)
shadowCombinedComplex row0Finite row1Finite =
  let combinedMaxDegree =
        max
          (unHomologicalDegree (maxHomologicalDegree row0Finite))
          (unHomologicalDegree (maxHomologicalDegree row1Finite) + 1)
   in mkFiniteChainComplexChecked
        (HomologicalDegree combinedMaxDegree)
        (shadowBoundaryAt row0Finite row1Finite)

shadowBoundaryAt :: FiniteChainComplex Int -> FiniteChainComplex Int -> HomologicalDegree -> BoundaryIncidence Int
shadowBoundaryAt row0Finite row1Finite degreeValue@(HomologicalDegree degreeIndex)
  | degreeIndex <= 0 =
      emptyBoundaryIncidenceOf
        (fromIntegral (degreeCardinality row0Finite degreeValue))
        0
  | otherwise =
      directSumBoundaryIncidence
        (boundaryAtOrZero row0Finite degreeValue)
        (boundaryAtOrZero row1Finite (HomologicalDegree (degreeIndex - 1)))

shadowFiltration :: FiniteChainComplex Int -> FiniteChainComplex Int -> BasisCellRef -> Int
shadowFiltration row0Finite row1Finite basisRef =
  let row0Count = degreeCardinality row0Finite (cellDegree basisRef)
      row1Count = degreeCardinality row1Finite (HomologicalDegree (unHomologicalDegree (cellDegree basisRef) - 1))
   in if cellIndex basisRef < row0Count
        then 0
        else
          if cellIndex basisRef < row0Count + row1Count
            then 1
            else 0

boundaryAtOrZero :: FiniteChainComplex Int -> HomologicalDegree -> BoundaryIncidence Int
boundaryAtOrZero finite degreeValue@(HomologicalDegree degreeIndex)
  | degreeIndex < 0 =
      emptyBoundaryIncidenceOf 0 0
  | degreeIndex > unHomologicalDegree (maxHomologicalDegree finite) =
      emptyBoundaryIncidenceOf 0 (fromIntegral (degreeCardinality finite (HomologicalDegree (degreeIndex - 1))))
  | otherwise =
      incidenceMatrixAt finite degreeValue
