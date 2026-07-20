{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Saturation.Core.Engine
  ( SaturationEffects (..),
    runSaturation,
    runSaturationWith,
    runSaturationSteps,
  )
where

import Data.Functor.Identity
  ( Identity (..),
  )
import Data.Kind
  ( Type,
  )
import Data.List.NonEmpty (NonEmpty (..))
import Moonlight.Saturation.Core.Kernel
  ( SaturationKernel (..),
  )
import Moonlight.Saturation.Core.Outcome
  ( ApplyOutcome (..),
    RebuildOutcome (..),
  )
import Moonlight.Saturation.Core.Round
  ( RoundPlan (..),
  )
import Moonlight.Saturation.Core.Run
  ( SaturationRun (..),
  )
import Moonlight.Saturation.Core.Termination
  ( SaturationBudget (..),
    SaturationTermination (..),
    checkTerminationGoal,
  )

type SaturationEffects :: (Type -> Type) -> Type -> Type -> Type -> Type -> Type -> Type
data SaturationEffects m state round match effect err = SaturationEffects
  { sePlanRound :: !(state -> m (Either err (RoundPlan round state match))),
    seApply :: !(NonEmpty match -> state -> m (Either err (ApplyOutcome effect state))),
    seRebuild :: !(round -> effect -> state -> m (Either err (RebuildOutcome round state))),
    seCommit :: !(round -> effect -> state -> m state)
  }

saturationDone ::
  SaturationTermination ->
  state ->
  Either err (SaturationRun state)
saturationDone !termination !state =
  Right
    SaturationRun
      { srTermination = termination,
        srFinalState = state
      }

runSaturation ::
  SaturationBudget ->
  SaturationKernel state round match effect err ->
  state ->
  Either err (SaturationRun state)
runSaturation !budget !kernel =
  runIdentity
    . runSaturationWith
      budget
      kernel
      (pureSaturationEffects kernel)
{-# INLINE runSaturation #-}

pureSaturationEffects ::
  SaturationKernel state round match effect err ->
  SaturationEffects Identity state round match effect err
pureSaturationEffects kernel =
  SaturationEffects
    { sePlanRound =
        pure . skPlanRound kernel,
      seApply =
        \matches state -> pure (skApply kernel matches state),
      seRebuild =
        \roundValue effect state -> pure (skRebuild kernel roundValue effect state),
      seCommit =
        \roundValue effect state -> pure (skCommit kernel roundValue effect state)
    }
{-# INLINE pureSaturationEffects #-}

runSaturationWith ::
  Monad m =>
  SaturationBudget ->
  SaturationKernel state round match effect err ->
  SaturationEffects m state round match effect err ->
  state ->
  m (Either err (SaturationRun state))
runSaturationWith !budget !kernel !effects =
  go
  where
    reachedGoal !state =
      checkTerminationGoal (skGoal kernel) state

    hitIterationLimit !state =
      skIterationOf kernel state >= sbMaxIterations budget

    hitNodeLimit !state =
      skNodeCountOf kernel state > sbMaxNodes budget

    go !state
      | reachedGoal state =
          pure (saturationDone ReachedGoal state)
      | hitIterationLimit state =
          pure (saturationDone HitIterationLimit state)
      | hitNodeLimit state =
          pure (saturationDone HitNodeLimit state)
      | otherwise = do
          roundPlanResult <- sePlanRound effects state
          case roundPlanResult of
            Left err ->
              pure (Left err)
            Right roundPlan ->
              case roundPlan of
                StopRound terminalState ->
                  pure (saturationDone ReachedFixedPoint terminalState)
                AdvanceRound nextState ->
                  go nextState
                ApplyRound roundValue applyState matches -> do
                  applyResult <-
                    seApply effects matches applyState
                  case applyResult of
                    Left err ->
                      pure (Left err)
                    Right
                      ApplyOutcome
                        { aoState = appliedState,
                          aoEffect = appliedEffect
                        } -> do
                        rebuildResult <-
                          seRebuild effects roundValue appliedEffect appliedState
                        case rebuildResult of
                          Left err ->
                            pure (Left err)
                          Right
                            RebuildOutcome
                              { roRound = rebuiltRound,
                                roState = rebuiltState
                              } -> do
                              committedState <-
                                seCommit effects rebuiltRound appliedEffect rebuiltState
                              if hitNodeLimit committedState
                                then pure (saturationDone HitNodeLimit committedState)
                                else
                                  if skConverged kernel rebuiltRound committedState
                                    then pure (saturationDone ReachedFixedPoint committedState)
                                    else go committedState
{-# INLINE runSaturationWith #-}

runSaturationSteps ::
  SaturationBudget ->
  (state -> Int) ->
  (state -> Int) ->
  (state -> Bool) ->
  (state -> Either err state) ->
  state ->
  Either err (SaturationRun state)
runSaturationSteps !budget !iterationOf !nodeCountOf !converged !step =
  runSaturation budget stepKernel
  where
    stepKernel =
      SaturationKernel
        { skIterationOf = iterationOf,
          skNodeCountOf = nodeCountOf,
          skGoal = mempty,
          skPlanRound =
            \state ->
              Right (ApplyRound () state (() :| [])),
          skApply =
            \_matches state ->
              fmap
                ( \nextState ->
                    ApplyOutcome
                      { aoState = nextState,
                        aoEffect = ()
                      }
                )
                (step state),
          skRebuild =
            \roundValue _effect state ->
              Right
                RebuildOutcome
                  { roRound = roundValue,
                    roState = state
                  },
          skCommit =
            \_roundValue _effect state -> state,
          skConverged =
            \_roundValue state -> converged state
        }
{-# INLINE runSaturationSteps #-}
