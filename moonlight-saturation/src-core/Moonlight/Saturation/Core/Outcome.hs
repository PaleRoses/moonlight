{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Saturation.Core.Outcome
  ( ApplyOutcome (..),
    RebuildOutcome (..),
    reportedApply,
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)

type ApplyOutcome :: Type -> Type -> Type
data ApplyOutcome effect state = ApplyOutcome
  { aoState :: !state,
    aoEffect :: !effect
  }
  deriving stock (Functor)

type RebuildOutcome :: Type -> Type -> Type
data RebuildOutcome round state = RebuildOutcome
  { roRound :: !round,
    roState :: !state
  }
  deriving stock (Functor)

reportedApply ::
  (NonEmpty match -> effect) ->
  (NonEmpty match -> input -> Either err output) ->
  NonEmpty match ->
  input ->
  Either err (ApplyOutcome effect output)
reportedApply resultOf apply matches input =
  fmap
    ( \output ->
        ApplyOutcome
          { aoState = output,
            aoEffect = resultOf matches
          }
    )
    (apply matches input)
{-# INLINE reportedApply #-}
