module Moonlight.EGraph.Homology.Gerbe
  ( automorphismChainComplex,
    effectiveAutomorphismCount,
    gerbeCharacteristicClass,
    isGerbeTrivial,
  )
where

import Data.Function ((&))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.EGraph.Homology.Representative (normalizedRepresentativeTerms)
import Moonlight.Homology
  ( BasisCellRef (..),
    basisCellNodeId,
    degreeCardinality,
    boundaryCoefficient,
    sourceIndex,
    targetIndex,
    BoundaryIncidence,
    FiniteChainComplex,
    HomologicalDegree (..),
    HomologyFailure,
    RepresentativeCocycle,
    cohomologyBasisAt,
    emptyBoundaryIncidenceOf,
    incidenceMatrixAt,
    maxHomologicalDegree,
    mkFiniteChainComplexChecked,
    reindexBoundaryIncidenceWith,
  )

automorphismChainComplex :: FiniteChainComplex Int -> IntMap Int -> Either HomologyFailure (FiniteChainComplex Int)
automorphismChainComplex finite automorphismCounts =
  mkFiniteChainComplexChecked
    (maxHomologicalDegree finite)
    (weightedBoundaryAt finite automorphismCounts)

gerbeCharacteristicClass :: FiniteChainComplex Int -> IntMap Int -> Either HomologyFailure (Maybe (RepresentativeCocycle Rational Int))
gerbeCharacteristicClass finite automorphismCounts =
  automorphismChainComplex finite automorphismCounts
    & fmap
      ( \automorphismFinite ->
          case cohomologyBasisAt automorphismFinite (HomologicalDegree 2) of
            representativeValue : _ -> Just representativeValue
            [] -> Nothing
      )

isGerbeTrivial :: FiniteChainComplex Int -> IntMap Int -> Either HomologyFailure Bool
isGerbeTrivial finite automorphismCounts =
  gerbeCharacteristicClass finite automorphismCounts
    & fmap
      ( \characteristicClass ->
          case characteristicClass of
            Nothing -> True
            Just representativeValue -> null (normalizedRepresentativeTerms representativeValue)
      )

weightedBoundaryAt :: FiniteChainComplex Int -> IntMap Int -> HomologicalDegree -> BoundaryIncidence Int
weightedBoundaryAt finite automorphismCounts degreeValue@(HomologicalDegree degreeIndex)
  | degreeIndex <= 0 =
      emptyBoundaryIncidenceOf
        (fromIntegral (length (activeBasisAtDegree finite automorphismCounts degreeValue)))
        0
  | otherwise =
      let sourceBasis = activeBasisAtDegree finite automorphismCounts degreeValue
          targetDegreeValue = HomologicalDegree (degreeIndex - 1)
          targetBasis = activeBasisAtDegree finite automorphismCounts targetDegreeValue
          sourceIndexMap = reindexedByOriginalIndex sourceBasis
          targetIndexMap = reindexedByOriginalIndex targetBasis
          baseBoundary = incidenceMatrixAt finite degreeValue
       in reindexBoundaryIncidenceWith
            (fmap fromIntegral . (`Map.lookup` sourceIndexMap))
            (fmap fromIntegral . (`Map.lookup` targetIndexMap))
            ( \entryValue ->
                let sourceBasisRef = BasisCellRef degreeValue (sourceIndex entryValue)
                    targetBasisRef = BasisCellRef targetDegreeValue (targetIndex entryValue)
                    sourceWeight = effectiveAutomorphismCount finite automorphismCounts sourceBasisRef
                    targetWeight = effectiveAutomorphismCount finite automorphismCounts targetBasisRef
                 in if sourceWeight <= 0 || targetWeight <= 0
                      then Nothing
                      else Just (boundaryCoefficient entryValue * sourceWeight)
            )
            baseBoundary

activeBasisAtDegree :: FiniteChainComplex Int -> IntMap Int -> HomologicalDegree -> [BasisCellRef]
activeBasisAtDegree finite automorphismCounts degreeValue =
  [0 .. degreeCardinality finite degreeValue - 1]
    & fmap (BasisCellRef degreeValue)
    & filter ((> 0) . effectiveAutomorphismCount finite automorphismCounts)

reindexedByOriginalIndex :: [BasisCellRef] -> Map Int Int
reindexedByOriginalIndex =
  Map.fromList
    . fmap (\(newIndexValue, basisRefValue) -> (cellIndex basisRefValue, newIndexValue))
    . zip [0 :: Int ..]

effectiveAutomorphismCount :: FiniteChainComplex Int -> IntMap Int -> BasisCellRef -> Int
effectiveAutomorphismCount finite automorphismCounts basisRef =
  max 0
    ( IntMap.findWithDefault 1 (basisCellNodeId finite basisRef) automorphismCounts
        - 1
    )
