{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Model.Phase
  ( RelationalPhase (..),
  )
where

import Data.Kind
  ( Type,
  )
import Moonlight.Core
  ( PartialOrder (..),
    totalOrderLeq,
  )

type RelationalPhase :: Type
data RelationalPhase
  = PhaseJoin
  | PhaseProject
  | PhaseSubsumption
  | PhaseRestrict
  | PhaseAmalgamate
  | PhaseIndex
  | PhaseVisible
  | PhaseObstruction
  deriving stock (Eq, Ord, Show, Read)

instance PartialOrder RelationalPhase where
  leq =
    totalOrderLeq
