{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Runtime.Engine.Capability
  ( RelationalCapabilityTransport (..),
    RelationalCapabilityTransportMissing (..),
    RelationalDrainEmission (..),
    relationalDrainEmissionForOp,
    validateRelationalCapabilityTransport,
  )
where

import Data.Kind
  ( Type,
  )
import Data.Set qualified as Set
import Moonlight.Differential.Frontier
  ( RuntimeCapability,
    runtimeCapabilityTime,
  )
import Moonlight.Differential.Runtime.Error
  ( RuntimeIllegalCapabilityTransport (..),
  )
import Moonlight.Differential.Time
  ( RuntimeTime,
    rtContext,
    rtEpoch,
    rtFrontier,
    rtScope,
  )
import Moonlight.Differential.Carrier.Address
  ( caContext,
    rkSource,
    rkTarget,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    RelationalRuntimeEpoch,
  )
import Moonlight.Differential.Carrier.Topology
  ( carrierCoverMembers,
    carrierCoverTarget,
    carrierFamilyCover,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase,
  )
import Moonlight.Flow.Runtime.Execution.IR
  ( RelationalCapabilityTransport (..),
    RuntimeDataflowOp,
    RuntimeDataflowOpKey,
    runtimeDataflowOpContext,
    runtimeDataflowOpKey,
    runtimeDataflowOpTransport,
  )

type RelationalCapabilityTransportMissing :: Type -> Type -> Type
data RelationalCapabilityTransportMissing ctx prop = RelationalCapabilityTransportMissing
  { rctmSourceTime :: !(RelationalCarrierTime ctx),
    rctmTargetTime :: !(RelationalCarrierTime ctx),
    rctmOpContext :: !ctx,
    rctmOpKey :: !(Maybe (RuntimeDataflowOpKey ctx prop))
  }
  deriving stock (Eq, Show)

type RelationalDrainEmission :: Type -> Type -> Type
data RelationalDrainEmission ctx prop
  = EmitDowngrade !(RelationalCarrierTime ctx)
  | EmitTransport !(RelationalCapabilityTransport ctx prop) !(RelationalCarrierTime ctx)
  deriving stock (Eq, Show)

relationalDrainEmissionForOp ::
  Eq ctx =>
  RuntimeCapability ctx RelationalRuntimeEpoch RelationalPhase ->
  RelationalCarrierTime ctx ->
  RuntimeDataflowOp ctx prop boundary evidence ->
  Either (RelationalCapabilityTransportMissing ctx prop) (RelationalDrainEmission ctx prop)
relationalDrainEmissionForOp parentCapability targetTime op
  | rtContext parentTime == rtContext targetTime =
      Right (EmitDowngrade targetTime)
  | otherwise =
      case relationalCapabilityTransportForOp op of
        Just transport ->
          Right (EmitTransport transport targetTime)
        Nothing ->
          Left
            RelationalCapabilityTransportMissing
              { rctmSourceTime = parentTime,
                rctmTargetTime = targetTime,
                rctmOpContext = runtimeDataflowOpContext op,
                rctmOpKey = runtimeDataflowOpKey op
              }
  where
    parentTime =
      runtimeCapabilityTime parentCapability
{-# INLINE relationalDrainEmissionForOp #-}

validateRelationalCapabilityTransport ::
  Ord ctx =>
  RuntimeCapability ctx RelationalRuntimeEpoch RelationalPhase ->
  RelationalCapabilityTransport ctx prop ->
  RelationalCarrierTime ctx ->
  Either
    ( RuntimeIllegalCapabilityTransport
        ctx
        RelationalRuntimeEpoch
        RelationalPhase
        (RelationalCapabilityTransport ctx prop)
    )
    ()
validateRelationalCapabilityTransport parentCapability transport targetTime
  | transportSourceMatches parentTime transport
      && transportTargetMatches targetTime transport
      && transportTimeCompatible parentTime targetTime =
      Right ()
  | otherwise =
      Left
        RuntimeIllegalCapabilityTransport
          { rictWitness = transport,
            rictSourceTime = parentTime,
            rictTargetTime = targetTime
          }
  where
    parentTime =
      runtimeCapabilityTime parentCapability
{-# INLINE validateRelationalCapabilityTransport #-}

relationalCapabilityTransportForOp ::
  RuntimeDataflowOp ctx prop boundary evidence ->
  Maybe (RelationalCapabilityTransport ctx prop)
relationalCapabilityTransportForOp =
  runtimeDataflowOpTransport
{-# INLINE relationalCapabilityTransportForOp #-}

transportSourceMatches ::
  Ord ctx =>
  RelationalCarrierTime ctx ->
  RelationalCapabilityTransport ctx prop ->
  Bool
transportSourceMatches sourceTime transport =
  case transport of
    TransportViaRestriction key ->
      rtContext sourceTime == caContext (rkSource key)
    TransportViaAmalgamation family ->
      Set.member
        (rtContext sourceTime)
        (carrierCoverMembers (carrierFamilyCover family))
    TransportViaSubsumption _reuseId source _target ->
      rtContext sourceTime == caContext source
{-# INLINE transportSourceMatches #-}

transportTargetMatches ::
  Eq ctx =>
  RelationalCarrierTime ctx ->
  RelationalCapabilityTransport ctx prop ->
  Bool
transportTargetMatches targetTime transport =
  rtContext targetTime == transportTargetContext transport
{-# INLINE transportTargetMatches #-}

transportTargetContext :: RelationalCapabilityTransport ctx prop -> ctx
transportTargetContext transport =
  case transport of
    TransportViaRestriction key ->
      caContext (rkTarget key)
    TransportViaAmalgamation family ->
      carrierCoverTarget (carrierFamilyCover family)
    TransportViaSubsumption _reuseId _source target ->
      caContext target
{-# INLINE transportTargetContext #-}

transportTimeCompatible ::
  Eq epoch =>
  RuntimeTime ctx epoch phase ->
  RuntimeTime ctx epoch phase ->
  Bool
transportTimeCompatible sourceTime targetTime =
  rtScope sourceTime == rtScope targetTime
    && rtEpoch sourceTime == rtEpoch targetTime
    && rtFrontier sourceTime <= rtFrontier targetTime
{-# INLINE transportTimeCompatible #-}
