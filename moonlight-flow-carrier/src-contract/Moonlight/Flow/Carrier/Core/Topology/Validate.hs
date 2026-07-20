{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Carrier.Core.Topology.Validate
  ( CarrierTopologyValidationError (..),
    validateCarrierTopologyIntrinsic,
  )
where

import Data.Kind
  ( Type,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caCarrier,
    rkSource,
  )
import Moonlight.Flow.Carrier.Core.Reuse
  ( CarrierReuseId,
  )
import Moonlight.Flow.Carrier.Core.Topology
  ( CarrierEdge (..),
    CarrierTopology,
    carrierTopologyDerivedOwners,
    carrierTopologyEdges,
    carrierTopologyTouchedAddresses,
  )
import Moonlight.Differential.Carrier.Topology
  ( carrierFamilyMembers,
  )

type CarrierTopologyValidationError :: Type -> Type -> Type
data CarrierTopologyValidationError ctx prop
  = CarrierTopologyEdgeAnchorMismatch
      !(CarrierAddr ctx Carrier prop)
      !(CarrierEdge ctx Carrier prop)
  | CarrierTopologyUnownedDerivedTouchRoot
      !(CarrierAddr ctx Carrier prop)
  | CarrierTopologyDuplicateDerivedCarrierOwners
      !(CarrierAddr ctx Carrier prop)
      !(Set (CarrierReuseId ctx prop, CarrierAddr ctx Carrier prop))
  deriving stock (Eq, Show)

validateCarrierTopologyIntrinsic ::
  (Ord ctx, Ord prop) =>
  CarrierTopology ctx Carrier prop ->
  Either [CarrierTopologyValidationError ctx prop] ()
validateCarrierTopologyIntrinsic topology =
  finishErrors
    ( edgeAnchorErrors topology
        <> derivedOwnerErrors topology
        <> derivedTouchRootErrors topology
    )
{-# INLINE validateCarrierTopologyIntrinsic #-}

finishErrors :: [err] -> Either [err] ()
finishErrors [] =
  Right ()
finishErrors errors =
  Left errors
{-# INLINE finishErrors #-}

edgeAnchorErrors ::
  (Ord ctx, Ord prop) =>
  CarrierTopology ctx Carrier prop ->
  [CarrierTopologyValidationError ctx prop]
edgeAnchorErrors topology =
  [ CarrierTopologyEdgeAnchorMismatch anchor edge
  | (anchor, edge) <- carrierTopologyEdges topology,
    not (edgeAnchoredAt anchor edge)
  ]
{-# INLINE edgeAnchorErrors #-}

edgeAnchoredAt ::
  Ord (CarrierAddr ctx Carrier prop) =>
  CarrierAddr ctx Carrier prop ->
  CarrierEdge ctx Carrier prop ->
  Bool
edgeAnchoredAt anchor edge =
  case edge of
    EdgeRestriction key ->
      anchor == rkSource key
    EdgeSubsumption _reuseId source _target ->
      anchor == source
    EdgeAmalgamation family ->
      Set.member anchor (carrierFamilyMembers family)
{-# INLINE edgeAnchoredAt #-}

derivedOwnerErrors ::
  (Ord ctx, Ord prop) =>
  CarrierTopology ctx Carrier prop ->
  [CarrierTopologyValidationError ctx prop]
derivedOwnerErrors topology =
  [ CarrierTopologyDuplicateDerivedCarrierOwners addr owners
  | (addr, owners) <- Map.toAscList (carrierTopologyDerivedOwners topology),
    Set.size owners /= 1
  ]
{-# INLINE derivedOwnerErrors #-}

derivedTouchRootErrors ::
  (Ord ctx, Ord prop) =>
  CarrierTopology ctx Carrier prop ->
  [CarrierTopologyValidationError ctx prop]
derivedTouchRootErrors topology =
  [ CarrierTopologyUnownedDerivedTouchRoot addr
  | addr <- Set.toAscList touched,
    DerivedCarrier _derivedId <- [caCarrier addr],
    not (Map.member addr owners)
  ]
  where
    owners =
      carrierTopologyDerivedOwners topology

    touched =
      carrierTopologyTouchedAddresses topology
{-# INLINE derivedTouchRootErrors #-}
