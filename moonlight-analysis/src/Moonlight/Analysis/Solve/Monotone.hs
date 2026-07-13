module Moonlight.Analysis.Solve.Monotone
  ( monotoneSolve,
  )
where

import qualified Data.Set as Set
import Moonlight.Analysis.Solve.Types
  ( MonotoneSolverConfig (..),
    SolverFailure (..),
    SolverFamily (..),
    Result (..),
    SolverStats (..),
  )

monotoneSolve ::
  Ord cell =>
  MonotoneSolverConfig ->
  (Set.Set cell -> state -> (state, Set.Set cell)) ->
  Set.Set cell ->
  state ->
  Either SolverFailure (Result cell state)
monotoneSolve config step frontier0 state0 =
  go 0 Set.empty frontier0 state0
  where
    go iterationIndex accumulatedChanged frontier stateValue
      | Set.null frontier =
          Right
            Result
              { solverResultState = stateValue,
                solverResultChanged = accumulatedChanged,
                solverResultStats =
                  SolverStats
                    { solverIterations = iterationIndex,
                      solverResidual = 0.0,
                      solverConverged = True
                    }
              }
      | iterationIndex >= monotoneMaxIterations config =
          Left (SolverIterationBudgetExceeded MonotoneFamily (monotoneMaxIterations config))
      | otherwise =
          let (nextState, nextFrontier) = step frontier stateValue
              nextAccumulated = Set.union accumulatedChanged nextFrontier
           in if Set.null nextFrontier
                then
                  Right
                    Result
                      { solverResultState = nextState,
                        solverResultChanged = nextAccumulated,
                        solverResultStats =
                          SolverStats
                            { solverIterations = iterationIndex + 1,
                              solverResidual = 0.0,
                              solverConverged = True
                            }
                      }
                else go (iterationIndex + 1) nextAccumulated nextFrontier nextState
