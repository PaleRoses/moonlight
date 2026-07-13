module Moonlight.Homology.Pure.Matrix.Shape
  ( cellCountAtDegree,
    dimensionsOf,
  )
where

import Data.Function ((&))
import Moonlight.Homology.Boundary.Finite
  ( FiniteChainComplex,
    incidenceMatrixAt,
    maxHomologicalDegree,
  )
import Moonlight.Homology.Boundary.LinAlg (sourceCardinality)
import Moonlight.Homology.Pure.Degree (HomologicalDegree (..))
import Moonlight.Homology.Pure.Filtration (enumerateFromZero)

cellCountAtDegree :: FiniteChainComplex r -> HomologicalDegree -> Int
cellCountAtDegree finite (HomologicalDegree degreeValue)
  | degreeValue < 0 = 0
  | degreeValue > unHomologicalDegree (maxHomologicalDegree finite) = 0
  | otherwise = sourceCardinality (incidenceMatrixAt finite (HomologicalDegree degreeValue))

dimensionsOf :: FiniteChainComplex r -> [HomologicalDegree]
dimensionsOf finite =
  enumerateFromZero (unHomologicalDegree (maxHomologicalDegree finite) + 1)
    & fmap HomologicalDegree
