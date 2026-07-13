module Moonlight.Analysis.Solve.Internal.RootEngine
  ( RootMethod (..),
    runRootMethod,
    runRootMethodSimple,
  )
where

import Data.Kind (Type)
import Moonlight.Analysis.Convergence
  ( ConvergenceConfig,
    ConvergenceMetric,
    IterationLimit,
    Termination (..),
    Tolerance,
    evaluateStream,
    evaluateStreamWithConfig,
  )
import Moonlight.Analysis.Solve.Internal.RootState
  ( RootApprox,
    mapRootTermination,
    normalizeRootConfig,
    normalizeRootTolerance,
  )
import Moonlight.Core (AdditiveGroup)
import Prelude

type RootMethod :: Type -> Type
data RootMethod a = RootMethod
  { rmApproximations :: [RootApprox a],
    rmBracketCheck :: Maybe Bool
  }

runRootMethod :: (ConvergenceMetric a, AdditiveGroup a) => ConvergenceConfig -> RootMethod a -> Termination a
runRootMethod config rootMethod =
  case rmBracketCheck rootMethod of
    Just False -> Diverged
    _ ->
      mapRootTermination
        ( evaluateStreamWithConfig
            (normalizeRootConfig config)
            (rmApproximations rootMethod)
        )

runRootMethodSimple :: (ConvergenceMetric a, AdditiveGroup a) => Tolerance -> IterationLimit -> RootMethod a -> Termination a
runRootMethodSimple toleranceValue iterationLimitValue =
  runRootMethodWithEvaluation
    (evaluateStream (normalizeRootTolerance toleranceValue) iterationLimitValue)

runRootMethodWithEvaluation :: ([RootApprox a] -> Termination (RootApprox a)) -> RootMethod a -> Termination a
runRootMethodWithEvaluation evaluateMethod rootMethod =
  case rmBracketCheck rootMethod of
    Just False -> Diverged
    _ -> mapRootTermination (evaluateMethod (rmApproximations rootMethod))
