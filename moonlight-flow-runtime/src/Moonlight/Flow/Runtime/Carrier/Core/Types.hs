{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Moonlight.Flow.Runtime.Carrier.Core.Types
  ( CarrierCommitTrace (..),
  )
where

import Data.Set
  ( Set,
  )
import GHC.Generics
  ( Generic,
    Generically (..),
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )

data CarrierCommitTrace ctx prop = CarrierCommitTrace
  { cctTouchedContexts :: !(Set ctx),
    cctTouchedCarriers :: !(Set (CarrierAddr ctx Carrier prop))
  }
  deriving stock (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Generically (CarrierCommitTrace ctx prop))
