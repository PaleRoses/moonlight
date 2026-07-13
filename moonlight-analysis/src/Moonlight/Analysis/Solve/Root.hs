
module Moonlight.Analysis.Solve.Root
  ( Bracket (..),
    mkBracket,
    newtonSteps,
    bisectionSteps,
    findRootNewton,
    findRootNewtonWithConfig,
    findRootBisection,
    findRootBisectionWithConfig,
  )
where

import Data.Kind (Type)
import Moonlight.Analysis.Convergence
  ( ConvergenceConfig,
    ConvergenceMetric,
    IterationLimit,
    Termination,
    Tolerance,
  )
import Moonlight.Analysis.Dual (Dual (..), diff)
import Moonlight.Analysis.Solve.Internal.BracketOps
  ( bracketMidpoint,
    mkBracketState,
    updateBracketState,
  )
import Moonlight.Analysis.Solve.Internal.BracketSign (bracketContainsRootFromValues)
import Moonlight.Analysis.Solve.Internal.RootEngine
  ( RootMethod (..),
    runRootMethod,
    runRootMethodSimple,
  )
import Moonlight.Analysis.Solve.Internal.RootEvaluator
  ( ScalarRootFunction (..),
    evaluateScalarRootFunction,
  )
import Moonlight.Analysis.Solve.Internal.RootState (RootApprox (..))
import Moonlight.Core
  ( AdditiveGroup (..),
    Field (..),
    MoonlightError (..),
  )
import Prelude

type Bracket :: Type -> Type
data Bracket a = Bracket
  { lowerBound :: !a,
    upperBound :: !a
  }
  deriving stock (Eq, Show)

mkBracket :: Ord a => a -> a -> Either MoonlightError (Bracket a)
mkBracket lower upper
  | lower >= upper = Left (InvariantViolation "root bracket must satisfy lower < upper")
  | otherwise = Right (Bracket lower upper)

newtonSteps :: Field a => (forall s. Dual s a -> Dual s a) -> a -> [a]
newtonSteps function initialGuess = map rootPoint (newtonApproximations function initialGuess)

findRootNewton :: (ConvergenceMetric a, Field a) =>
  Tolerance ->
  IterationLimit ->
  (forall s. Dual s a -> Dual s a) ->
  a ->
  Termination a
findRootNewton toleranceValue iterationLimitValue function initialGuess =
  runRootMethodSimple toleranceValue iterationLimitValue
    RootMethod
      { rmApproximations = newtonApproximations function initialGuess,
        rmBracketCheck = Nothing
      }

findRootNewtonWithConfig :: (ConvergenceMetric a, Field a) =>
  ConvergenceConfig ->
  (forall s. Dual s a -> Dual s a) ->
  a ->
  Termination a
findRootNewtonWithConfig config function initialGuess =
  runRootMethod config
    RootMethod
      { rmApproximations = newtonApproximations function initialGuess,
        rmBracketCheck = Nothing
      }

bisectionSteps :: (Field a, Ord a) => (a -> a) -> Bracket a -> [a]
bisectionSteps function bracketValue = map rootPoint (bisectionApproximations function bracketValue)

findRootBisection :: (ConvergenceMetric a, Field a, Ord a) =>
  Tolerance ->
  IterationLimit ->
  (a -> a) ->
  Bracket a ->
  Termination a
findRootBisection toleranceValue iterationLimitValue function bracketValue =
  runRootMethodSimple toleranceValue iterationLimitValue
    RootMethod
      { rmApproximations = bisectionApproximations function bracketValue,
        rmBracketCheck = Just (bracketContainsRoot function bracketValue)
      }

findRootBisectionWithConfig :: (ConvergenceMetric a, Field a, Ord a) =>
  ConvergenceConfig ->
  (a -> a) ->
  Bracket a ->
  Termination a
findRootBisectionWithConfig config function bracketValue =
  runRootMethod config
    RootMethod
      { rmApproximations = bisectionApproximations function bracketValue,
        rmBracketCheck = Just (bracketContainsRoot function bracketValue)
      }

newtonApproximations :: Field a => (forall s. Dual s a -> Dual s a) -> a -> [RootApprox a]
newtonApproximations function initialGuess = unfold initialGuess
  where
    unfold currentValue =
      let (functionValue, derivativeValue) = diff function currentValue
          nextValue =
            case tryDiv functionValue derivativeValue of
              Just stepValue -> sub currentValue stepValue
              Nothing -> currentValue
       in RootApprox currentValue functionValue : unfold nextValue

bisectionApproximations :: (Field a, Ord a) => (a -> a) -> Bracket a -> [RootApprox a]
bisectionApproximations function bracketValue =
  unfold initialState
  where
    scalarFunction = ScalarRootFunction function
    lower = lowerBound bracketValue
    upper = upperBound bracketValue
    lowerValue = evaluateScalarRootFunction scalarFunction lower
    upperValue = evaluateScalarRootFunction scalarFunction upper

    initialState = mkBracketState lower lowerValue upper upperValue

    unfold bracketState =
      case bracketMidpoint bracketState of
        Nothing -> []
        Just midpointValue ->
          let midpointFunctionValue = evaluateScalarRootFunction scalarFunction midpointValue
              nextState = updateBracketState midpointValue midpointFunctionValue bracketState
           in RootApprox midpointValue midpointFunctionValue : unfold nextState

bracketContainsRoot :: (Ord a, AdditiveGroup a) => (a -> a) -> Bracket a -> Bool
bracketContainsRoot function bracketValue =
  bracketContainsRootFromValues
    (function (lowerBound bracketValue))
    (function (upperBound bracketValue))
