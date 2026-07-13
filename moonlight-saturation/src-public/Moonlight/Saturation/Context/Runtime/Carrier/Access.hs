{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Runtime.Carrier.Access
  ( plainCarrierAccess,
  )
where

import Moonlight.Saturation.Context.Runtime.Policy.Internal
  ( CarrierAccess (..),
  )
import Moonlight.Saturation.Substrate

plainCarrierAccess :: CarrierAccess u (SatGraph u)
plainCarrierAccess =
  CarrierAccess
    { caGraph = id,
      caSetGraph = const
    }
{-# INLINE plainCarrierAccess #-}
