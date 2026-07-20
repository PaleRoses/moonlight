{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Runtime.Topology.Validate
  ( RuntimeTopologyBindingError (..),
    RuntimeTopologyValidationError (..),
    validateRuntimeTopologyBindings,
    validateRuntimeTopology,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Kind
  ( Type,
  )
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Core
  ( QueryId,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    QueryCarrierNode (..),
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caCarrier,
    RestrictKey,
    rkSource,
    rkTarget,
  )
import Moonlight.Flow.Carrier.Core.Topology
  ( CarrierEdge (..),
    CarrierTopology,
    carrierTopologyAddresses,
    carrierTopologyEdges,
  )
import Moonlight.Differential.Carrier.Topology
  ( carrierFamilyMembers,
    carrierFamilyTargets,
  )
import Moonlight.Flow.Carrier.Core.Topology.Validate
  ( CarrierTopologyValidationError,
    validateCarrierTopologyIntrinsic,
  )
import Moonlight.Flow.Carrier.Morphism.Core.Program
  ( CarrierMorphismRuntime,
    hasCarrierMorphismRestriction,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (PhaseRestrict),
  )
import Moonlight.Flow.Runtime.Topology.Routing
  ( RuntimeRouting,
    Shard (..),
    routeCarrierShard,
    routeIndexShard,
  )

type RuntimeTopologyBindingError :: Type -> Type -> Type
data RuntimeTopologyBindingError ctx prop
  = RuntimeTopologyUnregisteredFactorCarrier
      !(CarrierAddr ctx Carrier prop)
      !QueryId
  | RuntimeTopologyMissingIndexRoute
      !(CarrierAddr ctx Carrier prop)
  | RuntimeTopologyMissingIndexShard
      !(CarrierAddr ctx Carrier prop)
      !Shard
  | RuntimeTopologyMissingRestrictRoute
      !(CarrierAddr ctx Carrier prop)
  | RuntimeTopologyMissingRestrictShard
      !(CarrierAddr ctx Carrier prop)
      !Shard
  | RuntimeTopologyMissingRestrictionProgram
      !(RestrictKey ctx Carrier prop)
      !Shard
  deriving stock (Eq, Show)

type RuntimeTopologyValidationError :: Type -> Type -> Type
data RuntimeTopologyValidationError ctx prop
  = RuntimeTopologyIntrinsicInvalid !(CarrierTopologyValidationError ctx prop)
  | RuntimeTopologyBindingInvalid !(RuntimeTopologyBindingError ctx prop)
  deriving stock (Eq, Show)

validateRuntimeTopology ::
  (Ord ctx, Ord prop) =>
  RuntimeRouting ctx prop ->
  Set QueryId ->
  IntMap (CarrierMorphismRuntime ctx Carrier prop boundary evidence) ->
  IntMap indexState ->
  CarrierTopology ctx Carrier prop ->
  Either [RuntimeTopologyValidationError ctx prop] ()
validateRuntimeTopology routing registeredFactorQueries restrictStates indexStates topology =
  finishErrors
    ( intrinsicErrors topology
        <> bindingErrors routing registeredFactorQueries restrictStates indexStates topology
    )
{-# INLINE validateRuntimeTopology #-}

validateRuntimeTopologyBindings ::
  (Ord ctx, Ord prop) =>
  RuntimeRouting ctx prop ->
  Set QueryId ->
  IntMap (CarrierMorphismRuntime ctx Carrier prop boundary evidence) ->
  IntMap indexState ->
  CarrierTopology ctx Carrier prop ->
  Either [RuntimeTopologyBindingError ctx prop] ()
validateRuntimeTopologyBindings routing registeredFactorQueries restrictStates indexStates topology =
  finishErrors
    ( factorQueryErrors registeredFactorQueries topology
        <> indexRouteErrors routing indexStates topology
        <> restrictionProgramErrors routing restrictStates topology
    )
{-# INLINE validateRuntimeTopologyBindings #-}

intrinsicErrors ::
  (Ord ctx, Ord prop) =>
  CarrierTopology ctx Carrier prop ->
  [RuntimeTopologyValidationError ctx prop]
intrinsicErrors =
  either (fmap RuntimeTopologyIntrinsicInvalid) (const [])
    . validateCarrierTopologyIntrinsic
{-# INLINE intrinsicErrors #-}

bindingErrors ::
  (Ord ctx, Ord prop) =>
  RuntimeRouting ctx prop ->
  Set QueryId ->
  IntMap (CarrierMorphismRuntime ctx Carrier prop boundary evidence) ->
  IntMap indexState ->
  CarrierTopology ctx Carrier prop ->
  [RuntimeTopologyValidationError ctx prop]
bindingErrors routing registeredFactorQueries restrictStates indexStates topology =
  either (fmap RuntimeTopologyBindingInvalid) (const []) $
    validateRuntimeTopologyBindings
      routing
      registeredFactorQueries
      restrictStates
      indexStates
      topology
{-# INLINE bindingErrors #-}

finishErrors :: [err] -> Either [err] ()
finishErrors [] =
  Right ()
finishErrors errors =
  Left errors
{-# INLINE finishErrors #-}

factorQueryErrors ::
  (Ord ctx, Ord prop) =>
  Set QueryId ->
  CarrierTopology ctx Carrier prop ->
  [RuntimeTopologyBindingError ctx prop]
factorQueryErrors registeredFactorQueries topology =
  [ RuntimeTopologyUnregisteredFactorCarrier addr queryId
  | addr <- Set.toAscList (carrierTopologyAddresses topology),
    QueryCarrier queryId (QueryFactor _node) <- [caCarrier addr],
    not (Set.member queryId registeredFactorQueries)
  ]
{-# INLINE factorQueryErrors #-}

indexRouteErrors ::
  (Ord ctx, Ord prop) =>
  RuntimeRouting ctx prop ->
  IntMap indexState ->
  CarrierTopology ctx Carrier prop ->
  [RuntimeTopologyBindingError ctx prop]
indexRouteErrors routing indexStates topology =
  concatMap
    (validateIndexAddress routing indexStates)
    (Set.toAscList (indexAddressSet topology))
{-# INLINE indexRouteErrors #-}

indexAddressSet ::
  (Ord ctx, Ord prop) =>
  CarrierTopology ctx Carrier prop ->
  Set (CarrierAddr ctx Carrier prop)
indexAddressSet topology =
  Set.unions
    [ carrierTopologyAddresses topology,
      Set.unions
        [ topologyEdgeReads anchor edge
            <> topologyEdgeWrites anchor edge
        | (anchor, edge) <- carrierTopologyEdges topology
        ]
    ]
{-# INLINE indexAddressSet #-}

topologyEdgeReads ::
  CarrierAddr ctx Carrier prop ->
  CarrierEdge ctx Carrier prop ->
  Set (CarrierAddr ctx Carrier prop)
topologyEdgeReads _anchor edge =
  case edge of
    EdgeRestriction restrictKey ->
      Set.singleton (rkSource restrictKey)
    EdgeSubsumption _reuseId source _target ->
      Set.singleton source
    EdgeAmalgamation family ->
      carrierFamilyMembers family
{-# INLINE topologyEdgeReads #-}

topologyEdgeWrites ::
  (Ord ctx, Ord prop) =>
  CarrierAddr ctx Carrier prop ->
  CarrierEdge ctx Carrier prop ->
  Set (CarrierAddr ctx Carrier prop)
topologyEdgeWrites _anchor edge =
  case edge of
    EdgeRestriction restrictKey ->
      Set.singleton (rkTarget restrictKey)
    EdgeSubsumption _reuseId _source target ->
      Set.singleton target
    EdgeAmalgamation family ->
      carrierFamilyTargets family
{-# INLINE topologyEdgeWrites #-}

validateIndexAddress ::
  (Ord ctx, Ord prop) =>
  RuntimeRouting ctx prop ->
  IntMap indexState ->
  CarrierAddr ctx Carrier prop ->
  [RuntimeTopologyBindingError ctx prop]
validateIndexAddress routing indexStates addr =
  case routeIndexShard addr routing of
    Nothing ->
      [RuntimeTopologyMissingIndexRoute addr]
    Just shard
      | IntMap.member (shardKey shard) indexStates ->
          []
      | otherwise ->
          [RuntimeTopologyMissingIndexShard addr shard]
{-# INLINE validateIndexAddress #-}

restrictionProgramErrors ::
  (Ord ctx, Ord prop) =>
  RuntimeRouting ctx prop ->
  IntMap (CarrierMorphismRuntime ctx Carrier prop boundary evidence) ->
  CarrierTopology ctx Carrier prop ->
  [RuntimeTopologyBindingError ctx prop]
restrictionProgramErrors routing restrictStates topology =
  concatMap
    (validateRestrictionEdge routing restrictStates)
    (restrictionEdges topology)
{-# INLINE restrictionProgramErrors #-}

restrictionEdges ::
  (Ord ctx, Ord prop) =>
  CarrierTopology ctx Carrier prop ->
  [RestrictKey ctx Carrier prop]
restrictionEdges topology =
  Set.toAscList $
    Set.fromList
      [ key
      | (_anchor, EdgeRestriction key) <- carrierTopologyEdges topology
      ]
{-# INLINE restrictionEdges #-}

validateRestrictionEdge ::
  (Ord ctx, Ord prop) =>
  RuntimeRouting ctx prop ->
  IntMap (CarrierMorphismRuntime ctx Carrier prop boundary evidence) ->
  RestrictKey ctx Carrier prop ->
  [RuntimeTopologyBindingError ctx prop]
validateRestrictionEdge routing restrictStates restrictKey =
  case routeCarrierShard PhaseRestrict sourceAddr routing of
    Nothing ->
      [RuntimeTopologyMissingRestrictRoute sourceAddr]
    Just shard ->
      case IntMap.lookup (shardKey shard) restrictStates of
        Nothing ->
          [RuntimeTopologyMissingRestrictShard sourceAddr shard]
        Just restrictState
          | hasCarrierMorphismRestriction restrictKey restrictState ->
              []
          | otherwise ->
              [RuntimeTopologyMissingRestrictionProgram restrictKey shard]
  where
    sourceAddr =
      rkSource restrictKey
{-# INLINE validateRestrictionEdge #-}
