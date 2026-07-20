{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Runtime.Round.Input
  ( RoundInput (..),
  )
where

import Data.Kind
  ( Type,
  )
import Moonlight.Saturation.Context.Runtime.State
  ( RuntimeState,
  )
import Moonlight.Saturation.Substrate

type RoundInput :: Type -> Type -> Type -> Type
data RoundInput u carrier schedulerGroup = RoundInput
  { riState :: !(RuntimeState u carrier schedulerGroup),
    riGraph :: !(SatGraph u),
    riBaseContext :: !(SatContext u),
    riBaseGraph :: !(SatBaseGraph u),
    riBaseFacts :: !(SatFactStore u),
    riBaseFactDerivations :: !(SatFactIndex u),
    riRewriteContext :: !(SatRewriteContext u),
    riCapabilityResolver :: !(SatCapabilityResolver u)
  }
