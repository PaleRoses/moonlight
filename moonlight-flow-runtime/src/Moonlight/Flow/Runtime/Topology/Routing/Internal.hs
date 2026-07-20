{-# LANGUAGE DerivingStrategies #-}
module Moonlight.Flow.Runtime.Topology.Routing.Internal
  ( Shard (..),
    RuntimeRoutingError (..),
    RuntimeRouting
      ( RuntimeRouting,
        rrAtomSubscribers,
        rrContextOfQuery,
        rrProjectShardOfQuery,
        rrRestrictShardOfCarrier,
        rrIndexShardOfCarrier,
        rrIndexShardOfContextProp
      ),
    mkRuntimeRouting,
  )
where
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Core
  ( firstDuplicate,
  )
import Moonlight.Core
  ( AtomId,
    QueryId,
    queryIdKey,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierProp,
    CarrierAddr,
    caContext,
    caProp,
    rkSource,
  )
import Moonlight.Flow.Carrier.Core.Topology
  ( CarrierEdge (..),
    CarrierTopology,
    carrierTopologyAddresses,
    carrierTopologyEdges,
  )
import Moonlight.Flow.Runtime.Execution.Shard
  ( Shard (..),
  )
type RuntimeRouting :: Type -> Type -> Type
data RuntimeRouting ctx prop = RuntimeRouting
  { rrAtomSubscribers :: !(IntMap [(QueryId, AtomId)]),
    rrContextOfQuery :: !(IntMap ctx),
    rrProjectShardOfQuery :: !(IntMap Shard),
    rrRestrictShardOfCarrier :: !(Map (CarrierAddr ctx Carrier prop) Shard),
    rrIndexShardOfCarrier :: !(Map (CarrierAddr ctx Carrier prop) Shard),
    rrIndexShardOfContextProp :: !(Map (ctx, CarrierProp prop) Shard)
  }
  deriving stock (Eq, Show)
type RuntimeRoutingError :: Type -> Type -> Type
data RuntimeRoutingError ctx prop
  = RuntimeRoutingNegativeProjectShard !QueryId !Shard
  | RuntimeRoutingNegativeRestrictShard !(CarrierAddr ctx Carrier prop) !Shard
  | RuntimeRoutingNegativeIndexShard !(CarrierAddr ctx Carrier prop) !Shard
  | RuntimeRoutingNegativeContextPropIndexShard !ctx !(CarrierProp prop) !Shard
  | RuntimeRoutingMissingQueryContext !QueryId
  | RuntimeRoutingMissingProjectShardForQuery !QueryId
  | RuntimeRoutingMissingRestrictShardForCarrier !(CarrierAddr ctx Carrier prop)
  | RuntimeRoutingMissingIndexShardForCarrier !(CarrierAddr ctx Carrier prop)
  | RuntimeRoutingQueryContextCollision !QueryId !ctx !ctx
  | RuntimeRoutingQueryProjectShardCollision !QueryId !Shard !Shard
  | RuntimeRoutingContextPropIndexShardCollision !ctx !(CarrierProp prop) !Shard !Shard
  | RuntimeRoutingDuplicateAtomSubscriber !Int !(QueryId, AtomId)
  deriving stock (Eq, Ord, Show)
mkRuntimeRouting ::
  (Ord ctx, Ord prop) =>
  IntMap [(QueryId, AtomId)] ->
  CarrierTopology ctx Carrier prop ->
  IntMap ctx ->
  IntMap Shard ->
  Map (CarrierAddr ctx Carrier prop) Shard ->
  Map (CarrierAddr ctx Carrier prop) Shard ->
  Map (ctx, CarrierProp prop) Shard ->
  Either (RuntimeRoutingError ctx prop) (RuntimeRouting ctx prop)
mkRuntimeRouting
  atomSubscribers
  graph
  contextByQuery
  projectByQuery
  restrictByCarrier
  indexByCarrier
  indexByContextProp = do
  validateAtomSubscribers atomSubscribers
  Foldable.traverse_ validateQueryRoutes subscribedQueries
  validateCarrierShardMap RuntimeRoutingNegativeRestrictShard restrictByCarrier
  validateCarrierShardMap RuntimeRoutingNegativeIndexShard indexByCarrier
  validateContextPropIndexShards indexByContextProp
  Foldable.traverse_ validateCarrierRoutes indexCarriers
  Foldable.traverse_ validateRestrictionRoute restrictCarrierSources
  pure routing
  where
    routing =
      RuntimeRouting
        { rrAtomSubscribers = atomSubscribers,
          rrContextOfQuery = contextByQuery,
          rrProjectShardOfQuery = projectByQuery,
          rrRestrictShardOfCarrier = restrictByCarrier,
          rrIndexShardOfCarrier = indexByCarrier,
          rrIndexShardOfContextProp = indexByContextProp
        }
    subscribedQueries :: Set QueryId
    subscribedQueries =
      Set.fromList
        [ queryId
        | pairs <- IntMap.elems atomSubscribers,
          (queryId, _atomId) <- pairs
        ]
    indexCarriers =
      carrierTopologyAddresses graph
    restrictCarrierSources =
      Set.fromList
        [ rkSource restrictKey
        | (_source, EdgeRestriction restrictKey) <- carrierTopologyEdges graph
        ]
    validateQueryRoutes queryId = do
      case IntMap.lookup (queryIdKey queryId) contextByQuery of
        Nothing ->
          Left (RuntimeRoutingMissingQueryContext queryId)
        Just _ ->
          Right ()
      case IntMap.lookup (queryIdKey queryId) projectByQuery of
        Nothing ->
          Left (RuntimeRoutingMissingProjectShardForQuery queryId)
        Just shard ->
          validateShard (RuntimeRoutingNegativeProjectShard queryId) shard
    validateCarrierRoutes addr =
      case routeIndexShardInternal addr routing of
        Nothing ->
          Left (RuntimeRoutingMissingIndexShardForCarrier addr)
        Just _ ->
          Right ()
    validateRestrictionRoute addr =
      case Map.lookup addr restrictByCarrier of
        Nothing ->
          Left (RuntimeRoutingMissingRestrictShardForCarrier addr)
        Just shard ->
          validateShard (RuntimeRoutingNegativeRestrictShard addr) shard
{-# INLINE mkRuntimeRouting #-}
routeIndexShardInternal ::
  (Ord ctx, Ord prop) =>
  CarrierAddr ctx Carrier prop ->
  RuntimeRouting ctx prop ->
  Maybe Shard
routeIndexShardInternal addr routing =
  case Map.lookup addr (rrIndexShardOfCarrier routing) of
    Just shard ->
      Just shard
    Nothing ->
      Map.lookup (caContext addr, caProp addr) (rrIndexShardOfContextProp routing)
{-# INLINE routeIndexShardInternal #-}
validateCarrierShardMap ::
  (CarrierAddr ctx Carrier prop -> Shard -> RuntimeRoutingError ctx prop) ->
  Map (CarrierAddr ctx Carrier prop) Shard ->
  Either (RuntimeRoutingError ctx prop) ()
validateCarrierShardMap mkError =
  Map.foldlWithKey'
    ( \eitherUnit addr shard -> do
        eitherUnit
        validateShard (mkError addr) shard
    )
    (Right ())
{-# INLINE validateCarrierShardMap #-}
validateContextPropIndexShards ::
  Map (ctx, CarrierProp prop) Shard ->
  Either (RuntimeRoutingError ctx prop) ()
validateContextPropIndexShards =
  Map.foldlWithKey'
    ( \eitherUnit (contextValue, propKey) shard -> do
        eitherUnit
        validateShard
          (RuntimeRoutingNegativeContextPropIndexShard contextValue propKey)
          shard
    )
    (Right ())
{-# INLINE validateContextPropIndexShards #-}
validateShard ::
  (Shard -> RuntimeRoutingError ctx prop) ->
  Shard ->
  Either (RuntimeRoutingError ctx prop) ()
validateShard mkError shard =
  if shardKey shard < 0
    then Left (mkError shard)
    else Right ()
{-# INLINE validateShard #-}
validateAtomSubscribers ::
  IntMap [(QueryId, AtomId)] ->
  Either (RuntimeRoutingError ctx prop) ()
validateAtomSubscribers =
  Foldable.traverse_
    (uncurry rejectDuplicates)
    . IntMap.toAscList
{-# INLINE validateAtomSubscribers #-}
rejectDuplicates ::
  Int ->
  [(QueryId, AtomId)] ->
  Either (RuntimeRoutingError ctx prop) ()
rejectDuplicates atomKey subscribers =
  case firstDuplicate subscribers of
    Nothing ->
      Right ()
    Just duplicateSubscriber ->
      Left (RuntimeRoutingDuplicateAtomSubscriber atomKey duplicateSubscriber)
{-# INLINE rejectDuplicates #-}
