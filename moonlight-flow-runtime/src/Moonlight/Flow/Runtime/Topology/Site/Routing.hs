{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Moonlight.Flow.Runtime.Topology.Site.Routing
  ( RoutingDelta (..),
    compileRouting,
    diffRouting,
  )
where
import Control.Monad
  ( foldM,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Moonlight.Core
  ( QueryId,
    queryIdKey,
  )
import Moonlight.Differential.Proposition
  ( PropositionKey,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Runtime.Topology.Routing
  ( RuntimeRouting,
    RuntimeRoutingError (..),
  )
import Moonlight.Flow.Runtime.Topology.Routing.Internal
  ( mkRuntimeRouting,
  )
import Moonlight.Flow.Runtime.Topology.Site.Topology
  ( compileGeneratedCarrierTopology,
  )
import Moonlight.Flow.Runtime.Topology.Site.Patch
  ( GeneratedSiteTransition (..),
    SiteEffects (..),
  )
import Moonlight.Flow.Runtime.Topology.Site.Types
data RoutingDelta ctx prop = RoutingDelta
  { rdBefore :: !(RuntimeRouting ctx prop),
    rdAfter :: !(RuntimeRouting ctx prop),
    rdCarrierMoves :: !(CarrierMoves (CarrierAddr ctx Carrier prop)),
    rdDropContexts :: !(Set ctx)
  }
  deriving stock (Eq, Show)
data CompiledQueryRoutes ctx = CompiledQueryRoutes
  { cqrContextOfQuery :: !(IntMap.IntMap ctx),
    cqrProjectShardOfQuery :: !(IntMap.IntMap Shard)
  }
emptyCompiledQueryRoutes :: CompiledQueryRoutes ctx
emptyCompiledQueryRoutes =
  CompiledQueryRoutes
    { cqrContextOfQuery = IntMap.empty,
      cqrProjectShardOfQuery = IntMap.empty
    }
{-# INLINE emptyCompiledQueryRoutes #-}
compileRouting ::
  (Ord ctx, Ord prop) =>
  GeneratedSiteState ctx prop ->
  Either (RuntimeRoutingError ctx prop) (RuntimeRouting ctx prop)
compileRouting site = do
  queryRoutes <-
    compileGeneratedQueryRoutes site
  indexByContextProp <-
    compileContextPropIndexRoutes site
  mkRuntimeRouting
    (grsAtomSubscribers source)
    (compileGeneratedCarrierTopology site)
    (cqrContextOfQuery queryRoutes)
    (cqrProjectShardOfQuery queryRoutes)
    (grsRestrictShardsByCarrier source)
    (grsIndexShardsByCarrier source)
    indexByContextProp
  where
    source =
      gssRouteSource site
{-# INLINE compileRouting #-}
diffRouting ::
  (Ord ctx, Ord prop) =>
  GeneratedSiteTransition ctx prop ->
  Either (RuntimeRoutingError ctx prop) (RoutingDelta ctx prop)
diffRouting transition = do
  beforeRouting <-
    compileRouting (gstBefore transition)
  afterRouting <-
    compileRouting (gstAfter transition)
  let effects =
        gstEffects transition
  pure
    RoutingDelta
      { rdBefore = beforeRouting,
        rdAfter = afterRouting,
        rdCarrierMoves = seCarrierMoves effects,
        rdDropContexts = seDropContexts effects
      }
{-# INLINE diffRouting #-}
compileGeneratedQueryRoutes ::
  forall ctx prop.
  Eq ctx =>
  GeneratedSiteState ctx prop ->
  Either (RuntimeRoutingError ctx prop) (CompiledQueryRoutes ctx)
compileGeneratedQueryRoutes =
  foldM insertBinding emptyCompiledQueryRoutes . generatedSiteBindings
  where
    insertBinding ::
      CompiledQueryRoutes ctx ->
      (ctx, QueryId, GeneratedQueryBinding prop) ->
      Either (RuntimeRoutingError ctx prop) (CompiledQueryRoutes ctx)
    insertBinding routes (contextValue, queryId, binding) = do
      contextRoutes <-
        insertQueryRoute
          RuntimeRoutingQueryContextCollision
          queryId
          contextValue
          (cqrContextOfQuery routes)
      projectRoutes <-
        insertQueryRoute
          RuntimeRoutingQueryProjectShardCollision
          queryId
          (gqbProjectShard binding)
          (cqrProjectShardOfQuery routes)
      pure
        routes
          { cqrContextOfQuery = contextRoutes,
            cqrProjectShardOfQuery = projectRoutes
          }
{-# INLINE compileGeneratedQueryRoutes #-}
generatedSiteBindings ::
  GeneratedSiteState ctx prop ->
  [(ctx, QueryId, GeneratedQueryBinding prop)]
generatedSiteBindings site =
  [ (contextValue, queryId, binding)
  | (contextValue, shape) <- Map.toAscList (gssContexts site),
    (queryId, binding) <- Map.toAscList (gcsQueryBindings shape)
  ]
{-# INLINE generatedSiteBindings #-}
insertQueryRoute ::
  Eq value =>
  (QueryId -> value -> value -> RuntimeRoutingError ctx prop) ->
  QueryId ->
  value ->
  IntMap.IntMap value ->
  Either (RuntimeRoutingError ctx prop) (IntMap.IntMap value)
insertQueryRoute mkCollision queryId newValue routes =
  case IntMap.lookup key routes of
    Nothing ->
      Right (IntMap.insert key newValue routes)
    Just oldValue
      | oldValue == newValue ->
          Right routes
      | otherwise ->
          Left (mkCollision queryId oldValue newValue)
  where
    key =
      queryIdKey queryId
{-# INLINE insertQueryRoute #-}
compileContextPropIndexRoutes ::
  forall ctx prop.
  (Ord ctx, Ord prop) =>
  GeneratedSiteState ctx prop ->
  Either
    (RuntimeRoutingError ctx prop)
    (Map (ctx, PropositionKey prop) Shard)
compileContextPropIndexRoutes site =
  foldM insertShapeRoutes Map.empty (Map.toAscList (gssContexts site))
  where
    insertShapeRoutes ::
      Map (ctx, PropositionKey prop) Shard ->
      (ctx, GeneratedContextShape prop) ->
      Either
        (RuntimeRoutingError ctx prop)
        (Map (ctx, PropositionKey prop) Shard)
    insertShapeRoutes acc (contextValue, shape) =
      foldM
        (insertContextPropRoute contextValue)
        acc
        (Map.toAscList (gcsIndexShardsByProp shape))
    insertContextPropRoute ::
      ctx ->
      Map (ctx, PropositionKey prop) Shard ->
      (PropositionKey prop, Shard) ->
      Either
        (RuntimeRoutingError ctx prop)
        (Map (ctx, PropositionKey prop) Shard)
    insertContextPropRoute contextValue acc (propKey, newShard) =
      case Map.lookup (contextValue, propKey) acc of
        Nothing ->
          Right (Map.insert (contextValue, propKey) newShard acc)
        Just oldShard
          | oldShard == newShard ->
              Right acc
          | otherwise ->
              Left
                ( RuntimeRoutingContextPropIndexShardCollision
                    contextValue
                    propKey
                    oldShard
                    newShard
                )
{-# INLINE compileContextPropIndexRoutes #-}
