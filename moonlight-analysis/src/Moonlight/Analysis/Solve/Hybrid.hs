
module Moonlight.Analysis.Solve.Hybrid
  ( hybridSteps,
    findRootHybrid,
    findRootHybridWithConfig,
  )
where

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
    clampToBracketState,
    mkBracketState,
    updateBracketState,
    withinBracketState,
  )
import Moonlight.Analysis.Solve.Internal.BracketSign (bracketContainsRootFromValues)
import Moonlight.Analysis.Solve.Internal.RootEngine
  ( RootMethod (..),
    runRootMethod,
    runRootMethodSimple,
  )
import Moonlight.Analysis.Solve.Internal.RootEvaluator
  ( DifferentiableRootFunction (..),
    evaluateDifferentiableRootFunction,
  )
import Moonlight.Analysis.Solve.Internal.RootState (RootApprox (..))
import Moonlight.Analysis.Solve.Root (Bracket (..))
import Moonlight.Core
  ( AdditiveGroup (..),
    Field (..),
  )
import Prelude

hybridSteps :: (Field a, Ord a) =>
  (forall s. Dual s a -> Dual s a) ->
  Bracket a ->
  a ->
  [a]
hybridSteps function bracketValue initialGuess =
  map rootPoint (hybridApproximations function bracketValue initialGuess)

findRootHybrid :: (ConvergenceMetric a, Field a, Ord a) =>
  Tolerance ->
  IterationLimit ->
  (forall s. Dual s a -> Dual s a) ->
  Bracket a ->
  a ->
  Termination a
findRootHybrid toleranceValue iterationLimitValue function bracketValue initialGuess =
  runRootMethodSimple toleranceValue iterationLimitValue
    RootMethod
      { rmApproximations = hybridApproximations function bracketValue initialGuess,
        rmBracketCheck = Just (bracketContainsRoot function bracketValue)
      }

findRootHybridWithConfig :: (ConvergenceMetric a, Field a, Ord a) =>
  ConvergenceConfig ->
  (forall s. Dual s a -> Dual s a) ->
  Bracket a ->
  a ->
  Termination a
findRootHybridWithConfig config function bracketValue initialGuess =
  runRootMethod config
    RootMethod
      { rmApproximations = hybridApproximations function bracketValue initialGuess,
        rmBracketCheck = Just (bracketContainsRoot function bracketValue)
      }

hybridApproximations :: (Field a, Ord a) =>
  (forall s. Dual s a -> Dual s a) ->
  Bracket a ->
  a ->
  [RootApprox a]
hybridApproximations function bracketValue initialGuess =
  iterateState initialState
  where
    differentiableFunction = DifferentiableRootFunction function
    lower = lowerBound bracketValue
    upper = upperBound bracketValue

    lowerValue = evaluateDifferentiableRootFunction differentiableFunction lower
    upperValue = evaluateDifferentiableRootFunction differentiableFunction upper

    initialBracketState = mkBracketState lower lowerValue upper upperValue
    clampedInitial = clampToBracketState initialBracketState initialGuess
    initialState = (initialBracketState, clampedInitial)

    iterateState (bracketState, currentValue) =
      case bracketMidpoint bracketState of
        Nothing -> []
        Just midpointValue ->
          let (functionValue, derivativeValue) = diff function currentValue
              newtonCandidate =
                case tryDiv functionValue derivativeValue of
                  Just stepValue -> sub currentValue stepValue
                  Nothing -> midpointValue
              candidate =
                if withinBracketState bracketState newtonCandidate
                  then newtonCandidate
                  else midpointValue
              candidateValue = evaluateDifferentiableRootFunction differentiableFunction candidate
              nextBracketState = updateBracketState candidate candidateValue bracketState
              nextState = (nextBracketState, candidate)
           in RootApprox candidate candidateValue : iterateState nextState

bracketContainsRoot :: (Ord a, AdditiveGroup a) => (forall s. Dual s a -> Dual s a) -> Bracket a -> Bool
bracketContainsRoot function bracketValue =
  let differentiableFunction = DifferentiableRootFunction function
   in bracketContainsRootFromValues
        (evaluateDifferentiableRootFunction differentiableFunction (lowerBound bracketValue))
        (evaluateDifferentiableRootFunction differentiableFunction (upperBound bracketValue))
