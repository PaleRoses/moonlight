{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Program.View
  ( SaturationRoundView (..),
  )
where

import Data.Kind (Type)
import Moonlight.Saturation.Substrate

type SaturationRoundView :: Type -> Type
data SaturationRoundView u = SaturationRoundView
  { srvIteration :: !Int,
    srvGraph :: !(SatGraph u),
    srvBaseGraph :: !(SatBaseGraph u),
    srvFacts :: !(SatFactStore u),
    srvFactDerivations :: !(SatFactIndex u),
    srvFactsChanged :: !Bool,
    srvFactRoundCount :: !Int,
    srvBaseEligibleMatchCount :: !Int,
    srvContextEligibleMatchCount :: !Int,
    srvAggregatedEligibleMatchCount :: !Int,
    srvContextRevision :: !Int
  }
