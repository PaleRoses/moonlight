{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Saturation.Core.Round
  ( RoundPlan (..),
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)

type RoundPlan :: Type -> Type -> Type -> Type
data RoundPlan round state match
  = StopRound !state
  | AdvanceRound !state
  | ApplyRound !round !state !(NonEmpty match)
  deriving stock (Eq, Ord, Show, Read)
