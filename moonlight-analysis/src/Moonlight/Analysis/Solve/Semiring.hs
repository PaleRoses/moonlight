module Moonlight.Analysis.Solve.Semiring
  ( semiringSolve,
    semiringSolveFromDirty,
    semiringSolveExact,
  )
where

import Data.Function ((&))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Moonlight.Algebra (AdditiveMonoid (..), Semiring)
import Moonlight.Analysis.Solve.Types
  ( SemiringSolverConfig (..),
    SolverFailure (..),
    SolverFamily (..),
    Result (..),
    SolverStats (..),
  )

semiringSolveExact ::
  (Ord index, Semiring carrier, Eq carrier) =>
  SemiringSolverConfig ->
  (Map index carrier -> Map index carrier) ->
  Map index carrier ->
  Either SolverFailure (Result index (Map index carrier))
semiringSolveExact config propagate seed =
  semiringSolve config exactDistance propagate seed
  where
    exactDistance :: Eq carrier => carrier -> carrier -> Double
    exactDistance left right = if left == right then 0.0 else 1.0

semiringSolve ::
  (Ord index, Semiring carrier) =>
  SemiringSolverConfig ->
  (carrier -> carrier -> Double) ->
  (Map index carrier -> Map index carrier) ->
  Map index carrier ->
  Either SolverFailure (Result index (Map index carrier))
semiringSolve config distance propagate seed =
  semiringSolveFromDirty
    config
    distance
    propagate
    (Map.keysSet seed)
    seed

semiringSolveFromDirty ::
  (Ord index, Semiring carrier) =>
  SemiringSolverConfig ->
  (carrier -> carrier -> Double) ->
  (Map index carrier -> Map index carrier) ->
  Set index ->
  Map index carrier ->
  Either SolverFailure (Result index (Map index carrier))
semiringSolveFromDirty config distance propagate dirty seed =
  go 0 seed dirty Set.empty
  where
    go iterationIndex current frontier accumulatedChanged
      | iterationIndex >= semiringMaxIterations config =
          Left (SolverIterationBudgetExceeded SemiringFamily (semiringMaxIterations config))
      | Set.null changedIndices =
          Right
            Result
              { solverResultState = next,
                solverResultChanged = Set.union accumulatedChanged changedIndices,
                solverResultStats =
                  SolverStats
                    { solverIterations = iterationIndex + 1,
                      solverResidual = maxDelta,
                      solverConverged = True
                    }
              }
      | otherwise =
          go
            (iterationIndex + 1)
            next
            changedIndices
            (Set.union accumulatedChanged changedIndices)
      where
        activeState =
          if Set.null frontier
            then Map.empty
            else Map.restrictKeys current frontier
        propagated = propagate activeState
        next = Map.unionWith add current propagated
        indexUniverse = Map.keysSet current `Set.union` Map.keysSet next
        changeMagnitudeAt indexValue =
          distance
            (Map.findWithDefault zero indexValue current)
            (Map.findWithDefault zero indexValue next)
        changedIndices =
          Set.filter
            (\indexValue -> changeMagnitudeAt indexValue > semiringConvergenceTolerance config)
            indexUniverse
        maxDelta =
          indexUniverse
            & Set.toList
            & fmap changeMagnitudeAt
            & maximumOr 0.0

maximumOr :: Ord value => value -> [value] -> value
maximumOr fallback values =
  foldr max fallback values
