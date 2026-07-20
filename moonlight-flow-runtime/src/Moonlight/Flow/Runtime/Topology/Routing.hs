{-# LANGUAGE DerivingStrategies #-}
module Moonlight.Flow.Runtime.Topology.Routing
  ( Shard (..),
    RuntimeRouting,
    RuntimeRoutingError (..),
    runtimeRoutingAtomSubscribers,
    routeContextOfQuery,
    routeQueryShard,
    routeCarrierShard,
    routeIndexShard,
  )
where
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( AtomId,
    QueryId,
    queryIdKey,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
    caProp,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (..),
  )
import Moonlight.Flow.Runtime.Topology.Routing.Internal
  ( RuntimeRouting,
    RuntimeRoutingError (..),
    Shard (..),
    rrAtomSubscribers,
    rrContextOfQuery,
    rrIndexShardOfCarrier,
    rrIndexShardOfContextProp,
    rrProjectShardOfQuery,
    rrRestrictShardOfCarrier,
  )
runtimeRoutingAtomSubscribers ::
  RuntimeRouting ctx prop ->
  IntMap [(QueryId, AtomId)]
runtimeRoutingAtomSubscribers =
  rrAtomSubscribers
{-# INLINE runtimeRoutingAtomSubscribers #-}
routeContextOfQuery ::
  QueryId ->
  RuntimeRouting ctx prop ->
  Maybe ctx
routeContextOfQuery queryId =
  IntMap.lookup (queryIdKey queryId) . rrContextOfQuery
{-# INLINE routeContextOfQuery #-}
routeQueryShard ::
  RelationalPhase ->
  QueryId ->
  RuntimeRouting ctx prop ->
  Maybe Shard
routeQueryShard phase queryId routing =
  case phase of
    PhaseJoin ->
      Nothing
    PhaseProject ->
      IntMap.lookup key (rrProjectShardOfQuery routing)
    PhaseSubsumption ->
      Nothing
    PhaseRestrict ->
      Nothing
    PhaseAmalgamate ->
      Nothing
    PhaseIndex ->
      Nothing
    PhaseVisible ->
      Nothing
    PhaseObstruction ->
      Nothing
  where
    key =
      queryIdKey queryId
{-# INLINE routeQueryShard #-}
routeCarrierShard ::
  (Ord ctx, Ord prop) =>
  RelationalPhase ->
  CarrierAddr ctx Carrier prop ->
  RuntimeRouting ctx prop ->
  Maybe Shard
routeCarrierShard phase addr routing =
  case phase of
    PhaseJoin ->
      Nothing
    PhaseProject ->
      Nothing
    PhaseSubsumption ->
      Nothing
    PhaseRestrict ->
      Map.lookup addr (rrRestrictShardOfCarrier routing)
    PhaseAmalgamate ->
      Nothing
    PhaseIndex ->
      routeIndexShard addr routing
    PhaseVisible ->
      Nothing
    PhaseObstruction ->
      Nothing
{-# INLINE routeCarrierShard #-}
routeIndexShard ::
  (Ord ctx, Ord prop) =>
  CarrierAddr ctx Carrier prop ->
  RuntimeRouting ctx prop ->
  Maybe Shard
routeIndexShard addr routing =
  case Map.lookup addr (rrIndexShardOfCarrier routing) of
    Just shard ->
      Just shard
    Nothing ->
      Map.lookup (caContext addr, caProp addr) (rrIndexShardOfContextProp routing)
{-# INLINE routeIndexShard #-}
