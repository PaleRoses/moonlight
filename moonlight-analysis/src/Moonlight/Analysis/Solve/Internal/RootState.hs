module Moonlight.Analysis.Solve.Internal.RootState
  ( RootApprox (..),
    normalizeRootTolerance,
    normalizeRootConfig,
    mapRootTermination,
  )
where

import Data.Kind (Type)
import Moonlight.Analysis.Convergence
  ( ConvergenceConfig (..),
    ConvergenceMetric (..),
    Termination (..),
    Tolerance,
    normalizeTolerance,
  )
import Moonlight.Core
  ( AdditiveGroup,
    AdditiveMonoid (..),
  )
import Prelude

type RootApprox :: Type -> Type
data RootApprox a = RootApprox
  { rootPoint :: !a,
    rootResidual :: !a
  }
  deriving stock (Eq, Show)

instance (ConvergenceMetric a, AdditiveGroup a) => ConvergenceMetric (RootApprox a) where
  convergenceDistance (RootApprox previousPoint _) (RootApprox currentPoint currentResidual) =
    max
      (convergenceDistance previousPoint currentPoint)
      (convergenceDistance currentResidual zero)
  convergenceScale (RootApprox previousPoint _) (RootApprox currentPoint _) =
    convergenceScale previousPoint currentPoint
  convergenceWithinUlp toleranceValue (RootApprox previousPoint _) (RootApprox currentPoint currentResidual) =
    convergenceWithinUlp toleranceValue previousPoint currentPoint
      && convergenceWithinUlp toleranceValue currentResidual zero

normalizeRootTolerance :: Tolerance -> Tolerance
normalizeRootTolerance = normalizeTolerance

normalizeRootConfig :: ConvergenceConfig -> ConvergenceConfig
normalizeRootConfig config =
  config
    { tolerance = normalizeTolerance (tolerance config)
    }

mapRootTermination :: Termination (RootApprox a) -> Termination a
mapRootTermination terminationResult =
  case terminationResult of
    Converged approximation -> Converged (rootPoint approximation)
    IterationLimitReached approximation iterationCount ->
      IterationLimitReached (rootPoint approximation) iterationCount
    Diverged -> Diverged
