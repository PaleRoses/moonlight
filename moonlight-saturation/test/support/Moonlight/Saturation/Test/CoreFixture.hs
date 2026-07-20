{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Saturation.Test.CoreFixture
  ( ToyState (..),
    ToyRound (..),
    initialToy,
    toyKernel,
    unobservedToyKernel,
    fixedPointToyKernel,
    convergedToyKernel,
    idleKernel,
    monotone,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Saturation.Core
  ( ApplyOutcome (..),
    RebuildOutcome (..),
    RoundPlan (..),
    SaturationKernel (..),
    TerminationGoal (..),
  )

data ToyState = ToyState
  { tsIteration :: !Int,
    tsFacts :: !Int,
    tsTarget :: !Int,
    tsObserved :: ![Int]
  }
  deriving stock (Eq, Show)

data ToyRound = ToyRound
  { trInput :: !ToyState,
    trMatches :: ![Int]
  }
  deriving stock (Eq, Show)

initialToy :: Int -> ToyState
initialToy target =
  ToyState
    { tsIteration = 0,
      tsFacts = 0,
      tsTarget = max 0 target,
      tsObserved = [0]
    }

toyKernel :: SaturationKernel ToyState ToyRound Int Int String
toyKernel =
  SaturationKernel
    { skIterationOf = tsIteration,
      skNodeCountOf = tsFacts,
      skGoal = TerminationGoal (\state -> tsFacts state >= tsTarget state),
      skPlanRound = \state ->
        let roundValue =
              ToyRound
                { trInput = state,
                  trMatches = [tsFacts state + 1 | tsFacts state < tsTarget state]
                }
         in case NonEmpty.nonEmpty (trMatches roundValue) of
              Just matches -> Right (ApplyRound roundValue (trInput roundValue) matches)
              Nothing -> Right (StopRound (trInput roundValue)),
      skApply = \matches state ->
        Right
          ApplyOutcome
            { aoEffect = NonEmpty.length matches,
              aoState = state {tsFacts = tsFacts state + NonEmpty.length matches}
            },
      skRebuild = \roundValue _applied state ->
        Right
          RebuildOutcome
            { roRound = roundValue,
              roState = state {tsObserved = tsFacts state : tsObserved state}
            },
      skCommit = \_roundValue _applied state -> advance state,
      skConverged = \_roundValue _state -> False
    }

fixedPointToyKernel :: SaturationKernel ToyState ToyRound Int Int String
fixedPointToyKernel =
  unobservedToyKernel {skGoal = mempty}

unobservedToyKernel :: SaturationKernel ToyState ToyRound Int Int String
unobservedToyKernel =
  toyKernel
    { skRebuild = \roundValue _applied state ->
        Right
          RebuildOutcome
            { roRound = roundValue,
              roState = state
            }
    }

convergedToyKernel :: SaturationKernel ToyState ToyRound Int Int String
convergedToyKernel =
  fixedPointToyKernel
    { skConverged = \_roundValue state ->
        tsFacts state >= tsTarget state
    }

idleKernel :: SaturationKernel ToyState ToyRound Int Int String
idleKernel =
  toyKernel
    { skGoal = mempty,
      skPlanRound = \state -> Right (AdvanceRound (advance state))
    }

advance :: ToyState -> ToyState
advance state =
  state {tsIteration = tsIteration state + 1}

monotone :: [Int] -> Bool
monotone values =
  case values of
    [] -> True
    [_] -> True
    left : right : rest -> left <= right && monotone (right : rest)
