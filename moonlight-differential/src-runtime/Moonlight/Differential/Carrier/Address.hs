{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Carrier.Address
  ( CarrierProp,
    CarrierAddr,
    caContext,
    caProp,
    caCarrier,
    RestrictKey,
    rkSource,
    rkTarget,
    carrierAddr,
    restrictKey,
  )
where

import Data.Kind
  ( Type,
  )
import Moonlight.Differential.Proposition
  ( PropositionKey,
  )

type CarrierProp :: Type -> Type
type CarrierProp prop =
  PropositionKey prop

type CarrierAddr :: Type -> Type -> Type -> Type
data CarrierAddr ctx carrier prop = CarrierAddr
  { caContext :: !ctx,
    caProp :: !(CarrierProp prop),
    caCarrier :: !carrier
  }
  deriving stock (Eq, Ord, Show, Read)

type RestrictKey :: Type -> Type -> Type -> Type
data RestrictKey ctx carrier prop = RestrictKey
  { rkSource :: !(CarrierAddr ctx carrier prop),
    rkTarget :: !(CarrierAddr ctx carrier prop)
  }
  deriving stock (Eq, Ord, Show, Read)

carrierAddr ::
  ctx ->
  CarrierProp prop ->
  carrier ->
  CarrierAddr ctx carrier prop
carrierAddr contextValue propValue carrierValue =
  CarrierAddr
    { caContext = contextValue,
      caProp = propValue,
      caCarrier = carrierValue
    }
{-# INLINE carrierAddr #-}

restrictKey ::
  CarrierAddr ctx carrier prop ->
  CarrierAddr ctx carrier prop ->
  RestrictKey ctx carrier prop
restrictKey =
  RestrictKey
{-# INLINE restrictKey #-}
