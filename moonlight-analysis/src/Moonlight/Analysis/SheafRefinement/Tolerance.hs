module Moonlight.Analysis.SheafRefinement.Tolerance
  ( sheafRelativeTolerance,
    relClose,
    vecApproxEq,
    averageDouble,
  )
where

import Moonlight.Analysis.Convergence (Tolerance (..), withinTolerance)
import Moonlight.Core (mkAbsTol, mkRelTol)
import Moonlight.LinAlg.Geometry (Vec3 (..))
import Prelude

sheafRelativeTolerance :: Tolerance
sheafRelativeTolerance =
  case (mkAbsTol 1.0e-6, mkRelTol 1.0e-6) of
    (Right absoluteTolerance, Right relativeTolerance) ->
      DisjunctiveTol (AbsTolBound absoluteTolerance) (RelTolBound relativeTolerance)
    _ -> AbsTolBound zeroFallback
  where
    zeroFallback = case mkAbsTol 0.0 of
      Right value -> value
      Left _ -> error "unreachable: mkAbsTol 0.0"

relClose :: Double -> Double -> Bool
relClose = withinTolerance sheafRelativeTolerance

vecApproxEq :: Vec3 -> Vec3 -> Bool
vecApproxEq (Vec3 leftX leftY leftZ) (Vec3 rightX rightY rightZ) =
  relClose leftX rightX
    && relClose leftY rightY
    && relClose leftZ rightZ

averageDouble :: Double -> Double -> Double
averageDouble leftValue rightValue =
  0.5 * (leftValue + rightValue)
