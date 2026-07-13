module Moonlight.EGraph.Homology.Representative
  ( normalizedRepresentativeCoefficientMap,
    normalizedRepresentativeTerms,
    representativeAbsoluteWeightMap,
    representativeKey,
    representativeSupportIndices,
  )
where

import Data.Function ((&))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Homology
  ( RepresentativeCocycle,
    representativeTerms,
  )

normalizedRepresentativeCoefficientMap :: RepresentativeCocycle Rational Int -> Map Int Rational
normalizedRepresentativeCoefficientMap representativeValue =
  representativeTerms representativeValue
    & fmap (\(coefficientValue, basisIndexValue) -> (basisIndexValue, coefficientValue))
    & Map.fromListWith (+)
    & Map.filter (/= 0)

normalizedRepresentativeTerms :: RepresentativeCocycle Rational Int -> [(Rational, Int)]
normalizedRepresentativeTerms representativeValue =
  normalizedRepresentativeCoefficientMap representativeValue
    & Map.toAscList
    & fmap (\(basisIndexValue, coefficientValue) -> (coefficientValue, basisIndexValue))

representativeAbsoluteWeightMap :: RepresentativeCocycle Rational Int -> Map Int Rational
representativeAbsoluteWeightMap representativeValue =
  normalizedRepresentativeCoefficientMap representativeValue
    & fmap abs

representativeKey :: RepresentativeCocycle Rational Int -> String
representativeKey = show . normalizedRepresentativeTerms

representativeSupportIndices :: RepresentativeCocycle Rational Int -> IntSet
representativeSupportIndices =
  IntSet.fromList
    . Map.keys
    . normalizedRepresentativeCoefficientMap
