module Moonlight.Analysis.Solve.Linearize
  ( boundaryIncidenceApply,
    boundaryIncidenceApplySemiring,
    transposeBoundaryIncidence,
    composeBoundaryIncidence,
    boundaryIncidenceDiagonal,
    boundaryToSparseCOO,
    identityBoundaryIncidence,
    scaleBoundaryIncidence,
  )
where

import Data.Function ((&))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Moonlight.Algebra (AdditiveMonoid (..), MultiplicativeMonoid (..), Semiring)
import Moonlight.Core (MoonlightError)
import Moonlight.Homology
  ( boundaryCoefficient, sourceIndex, targetIndex,
    BoundaryIncidence, boundaryEntries, sourceCardinality, targetCardinality,
    boundaryIncidenceApply,
    boundaryIncidenceDiagonal,
    composeBoundaryIncidence,
    identityBoundaryIncidenceOf,
    mapBoundaryCoefficients,
    transposeBoundaryIncidence,
  )
import Moonlight.LinAlg (SparseCOO, mkSparseCOO)

boundaryIncidenceApplySemiring :: Semiring r => BoundaryIncidence r -> Map Int r -> Map Int r
boundaryIncidenceApplySemiring incidence vectorValues =
  boundaryEntries incidence
    & fmap
      ( \entry ->
          ( targetIndex entry,
            mul
              (boundaryCoefficient entry)
              (Map.findWithDefault zero (sourceIndex entry) vectorValues)
          )
      )
    & Map.fromListWith add

boundaryToSparseCOO :: BoundaryIncidence r -> Either MoonlightError (SparseCOO r)
boundaryToSparseCOO incidence =
  mkSparseCOO
    (targetCardinality incidence)
    (sourceCardinality incidence)
    ( boundaryEntries incidence
        & fmap
          ( \entry ->
              ( targetIndex entry,
                sourceIndex entry,
                boundaryCoefficient entry
              )
          )
    )

identityBoundaryIncidence :: Int -> BoundaryIncidence Double
identityBoundaryIncidence dimension =
  identityBoundaryIncidenceOf (fromIntegral (max 0 dimension))

scaleBoundaryIncidence ::
  Double ->
  BoundaryIncidence Double ->
  BoundaryIncidence Double
scaleBoundaryIncidence scaleValue =
  mapBoundaryCoefficients (scaleValue *)
