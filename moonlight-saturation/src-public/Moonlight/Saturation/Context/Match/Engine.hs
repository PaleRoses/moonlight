{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Direction and planner-error vocabulary for cross-context restriction scheduling.
module Moonlight.Saturation.Context.Match.Engine
  ( RestrictionPlannerError (..),
    ProjectionDirection (..),
  )
where

import Data.Kind (Type)

type RestrictionPlannerError :: Type -> Type -> Type
data RestrictionPlannerError ctx query
  = RestrictionPlannerContextUnavailable !ctx
  deriving stock (Eq, Show)

type ProjectionDirection :: Type
data ProjectionDirection
  = TowardChild
  | TowardParent
  deriving stock (Eq, Ord, Show)
