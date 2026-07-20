{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Saturation.Core.Kernel
  ( SaturationKernel (..),
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Moonlight.Saturation.Core.Outcome
  ( ApplyOutcome,
    RebuildOutcome,
  )
import Moonlight.Saturation.Core.Round
  ( RoundPlan,
  )
import Moonlight.Saturation.Core.Termination
  ( TerminationGoal,
  )

type SaturationKernel :: Type -> Type -> Type -> Type -> Type -> Type
data SaturationKernel state round match effect err = SaturationKernel
  { skIterationOf :: state -> Int,
    skNodeCountOf :: state -> Int,
    skGoal :: !(TerminationGoal state),
    skPlanRound ::
      state ->
      Either err (RoundPlan round state match),
    skApply ::
      NonEmpty match ->
      state ->
      Either err (ApplyOutcome effect state),
    skRebuild ::
      round ->
      effect ->
      state ->
      Either err (RebuildOutcome round state),
    skCommit ::
      round ->
      effect ->
      state ->
      state,
    skConverged ::
      round ->
      state ->
      Bool
  }
