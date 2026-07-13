{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Runtime.Error
  ( RuntimeInvalidCapabilityAdvance (..),
    RuntimeIllegalCapabilityTransport (..),
    RuntimeSettleBudgetExhausted (..),
  )
where

import Data.Kind
  ( Type,
  )
import Moonlight.Differential.Frontier
  ( RuntimeInvalidCapabilityAdvance (..),
  )
import Moonlight.Differential.Time
  ( RuntimeTime,
  )

type RuntimeIllegalCapabilityTransport :: Type -> Type -> Type -> Type -> Type
data RuntimeIllegalCapabilityTransport ctx epoch phase witness = RuntimeIllegalCapabilityTransport
  { rictWitness :: !witness,
    rictSourceTime :: !(RuntimeTime ctx epoch phase),
    rictTargetTime :: !(RuntimeTime ctx epoch phase)
  }
  deriving stock (Eq, Ord, Show)

type RuntimeSettleBudgetExhausted :: Type -> Type
data RuntimeSettleBudgetExhausted residual = RuntimeSettleBudgetExhausted
  { rsbeIterationLimit :: !Int,
    rsbeResidual :: !residual
  }
  deriving stock (Eq, Ord, Show, Read)
