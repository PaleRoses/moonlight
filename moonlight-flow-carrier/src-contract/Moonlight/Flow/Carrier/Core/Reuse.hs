{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Carrier.Core.Reuse
  ( CarrierReuseKeyPayload (..),
    CarrierReuseId (..),
    carrierReuseIdDigest,
  )
where

import Data.Kind
  ( Type,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
    SubsumptionWitnessDigest,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )

type CarrierReuseKeyPayload :: Type -> Type -> Type
data CarrierReuseKeyPayload ctx prop = CarrierReuseKeyPayload
  { crkpSource :: !(CarrierAddr ctx Carrier prop),
    crkpWitnessTarget :: !(CarrierAddr ctx Carrier prop),
    crkpExpectedTarget :: !(Maybe (CarrierAddr ctx Carrier prop)),
    crkpWitnessDigest :: !SubsumptionWitnessDigest,
    crkpSourceShapeDigest :: !StableDigest128,
    crkpTargetShapeDigest :: !StableDigest128,
    crkpTargetBoundaryDigest :: !StableDigest128,
    crkpTargetViewDigest :: !(Maybe StableDigest128),
    crkpCoverageRuleDigest :: !StableDigest128
  }
  deriving stock (Eq, Ord, Show)

type CarrierReuseId :: Type -> Type -> Type
data CarrierReuseId ctx prop = CarrierReuseId
  { cridPayload :: !(CarrierReuseKeyPayload ctx prop),
    cridDigest :: !StableDigest128
  }
  deriving stock (Eq, Ord, Show)

carrierReuseIdDigest :: CarrierReuseId ctx prop -> StableDigest128
carrierReuseIdDigest =
  cridDigest
{-# INLINE carrierReuseIdDigest #-}
