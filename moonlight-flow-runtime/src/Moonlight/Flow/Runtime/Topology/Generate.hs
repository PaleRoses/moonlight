{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Flow.Runtime.Topology.Generate
  ( deriveGeneratedSite,
    validateGeneratedSite,
  )
where

import Data.Foldable qualified as Foldable
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core
  ( QueryId,
    mkAtomId,
    queryIdKey,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    QueryCarrierNode (..),
    queryAtomCarrier,
    queryCarrier,
    queryFactorCarrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    carrierAddr,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128 (..),
  )
import Moonlight.Flow.Plan.Query.Core
  ( FactorNode (..),
  )
import Moonlight.Flow.Runtime.Carrier.Emit.Factor
  ( factorNodeCarrierVisible,
  )
import Moonlight.Flow.Runtime.Spec.FactorProgram
  ( ErasedQueryPlanShape (..),
    FactorProgramSpec (..),
  )
import Moonlight.Flow.Runtime.Factor.State.Types
  ( RuntimeQueryBinding,
  )
import Moonlight.Flow.Runtime.Spec.Schema
  ( RuntimeAtomSchema (..),
    RuntimeContextSchema (..),
    RuntimePlan (..),
    RuntimeSchema (..),
    runtimePlanAtomSourcePairs,
    runtimePlanAtomKeys,
    runtimePlanFactorNodes,
    runtimePlanQueryId,
  )
import Moonlight.Flow.Runtime.Topology.Subscription
  ( QueryAtomSubscription (..),
  )
import Moonlight.Flow.Runtime.Topology.Subscription.Builder
  ( CarrierAddressing (..),
    CarrierSubscriptionBuildError,
    SubscriptionBuildInput (..),
    buildGeneratedRoutingSource,
  )
import Moonlight.Flow.Runtime.Topology.Site.Types
  ( GeneratedContextShape (..),
    GeneratedQueryBinding (..),
    GeneratedRoutingSource,
    GeneratedSiteState (..),
    GeneratedSiteValidationError,
    Shard (..),
    emptyContextEGraph,
    emptyGeneratedSiteState,
    generatedContextShapeDigest,
    insertContextClass,
    refreshGeneratedSiteDigest,
    validateGeneratedContextRouting,
    validateGeneratedContextShapeWithPrograms,
  )

deriveGeneratedSite ::
  forall ctx prop.
  (Ord ctx, Ord prop) =>
  IntMap RuntimeAtomSchema ->
  RuntimeSchema ctx prop ->
  [RuntimePlan ctx prop] ->
  Either (CarrierSubscriptionBuildError ctx prop) (GeneratedSiteState ctx prop)
deriveGeneratedSite atomSchemas schema plans = do
  routeSource <-
    deriveGeneratedRoutingSource atomSchemas plans
  Right
    ( refreshGeneratedSiteDigest
        emptyGeneratedSiteState
          { gssContexts = contextShapes,
            gssContextClasses = contextClasses,
            gssRouteSource = routeSource,
            gssPlanObjects = planObjects
          }
    )
  where
    plansByContext =
      Map.fromListWith
        (<>)
        [ (rpContext plan, [plan])
        | plan <- plans
        ]

    contextShapes =
      Map.mapWithKey
        ( \contextValue contextSchema ->
            generatedContextShape
              contextSchema
              (Map.findWithDefault [] contextValue plansByContext)
        )
        (rscContexts schema)

    contextClasses =
      Foldable.foldl'
        ( \classes contextValue ->
            insertContextClass contextValue classes
        )
        emptyContextEGraph
        (Map.keys (rscContexts schema))

    planObjects =
      Map.fromList
        [ ( eqpsPlanClassDigest (fpsQueryPlan (rpProgram plan)),
            rpContext plan
          )
        | plan <- plans
        ]

generatedContextShape ::
  Ord prop =>
  RuntimeContextSchema prop ->
  [RuntimePlan ctx prop] ->
  GeneratedContextShape prop
generatedContextShape contextSchema plans =
  shape0
    { gcsShapeDigest = generatedContextShapeDigest shape0
    }
  where
    shard0 =
      Shard 0

    queryBindings =
      Map.fromList
        [ ( runtimePlanQueryId plan,
            GeneratedQueryBinding
              { gqbProp = rpProp plan,
                gqbProjectShard = shard0
              }
          )
        | plan <- plans
        ]

    routedProps =
      rcsPropositions contextSchema
        <> Set.fromList (fmap rpProp plans)

    shape0 =
      GeneratedContextShape
        { gcsShapeDigest = StableDigest128 0 0,
          gcsQueryBindings = queryBindings,
          gcsIndexShardsByProp =
            Map.fromSet (const shard0) routedProps
        }
{-# INLINE generatedContextShape #-}

deriveGeneratedRoutingSource ::
  forall ctx prop.
  (Ord ctx, Ord prop) =>
  IntMap RuntimeAtomSchema ->
  [RuntimePlan ctx prop] ->
  Either (CarrierSubscriptionBuildError ctx prop) (GeneratedRoutingSource ctx prop)
deriveGeneratedRoutingSource atomSchemas plans =
  buildGeneratedRoutingSource
    SubscriptionBuildInput
      { sbiAtomSubscriptions = atomSubscriptions,
        sbiQueryDecompositions = queryDecompositions,
        sbiAddressing = planAddressing plansByQuery,
        sbiAtomTouchDeps = IntMap.map rasTouchDeps atomSchemas,
        sbiAtomTouchTopo = IntMap.map rasTouchTopo atomSchemas,
        sbiCarrierDeps = Map.empty,
        sbiCarrierTopo = Map.empty
      }
    Map.empty
    indexShardsByCarrier
  where
    shard0 =
      Shard 0

    plansByQuery =
      IntMap.fromList
        [ (queryIdKey (runtimePlanQueryId plan), plan)
        | plan <- plans
        ]

    queryDecompositions =
      Map.fromList
        [ (runtimePlanQueryId plan, fpsDecompPlan (rpProgram plan))
        | plan <- plans
        ]

    atomSubscriptions =
      [ QueryAtomSubscription
          { qasSourceAtomId = sourceAtomId,
            qasQueryId = queryId,
            qasQueryAtomId = queryAtomId
          }
      | plan <- plans,
        let queryId = runtimePlanQueryId plan,
        (queryAtomId, sourceAtomId) <- runtimePlanAtomSourcePairs plan
      ]

    indexShardsByCarrier =
      Map.fromSet (const shard0) indexCarriers

    indexCarriers =
      Set.fromList
        (concatMap planIndexCarriers plans)
{-# INLINE deriveGeneratedRoutingSource #-}

planAddressing ::
  IntMap (RuntimePlan ctx prop) ->
  CarrierAddressing ctx prop
planAddressing plansByQuery =
  CarrierAddressing
    { caaAtom =
        \queryId atomId ->
          planCarrierForNode
            plansByQuery
            queryId
            (QueryAtom atomId),
      caaBag =
        \queryId bagId ->
          planCarrierForNode
            plansByQuery
            queryId
            (QueryFactor (FactorNodeBag bagId)),
      caaSeparator =
        \queryId child parent ->
          planCarrierForNode
            plansByQuery
            queryId
            (QueryFactor (FactorNodeSeparator child parent)),
      caaRoot =
        \queryId ->
          planCarrierForNode
            plansByQuery
            queryId
            (QueryFactor FactorNodeRoot)
    }
{-# INLINE planAddressing #-}

planCarrierForNode ::
  IntMap (RuntimePlan ctx prop) ->
  QueryId ->
  QueryCarrierNode ->
  Maybe (CarrierAddr ctx Carrier prop)
planCarrierForNode plansByQuery queryId node = do
  plan <- IntMap.lookup (queryIdKey queryId) plansByQuery
  pure (carrierAddr (rpContext plan) (rpProp plan) (queryCarrier queryId node))
{-# INLINE planCarrierForNode #-}

planIndexCarriers ::
  RuntimePlan ctx prop ->
  [CarrierAddr ctx Carrier prop]
planIndexCarriers plan =
  planAtomCarriers plan <> planFactorCarriers plan
{-# INLINE planIndexCarriers #-}

planAtomCarriers ::
  RuntimePlan ctx prop ->
  [CarrierAddr ctx Carrier prop]
planAtomCarriers plan =
  [ carrierAddr
      (rpContext plan)
      (rpProp plan)
      (queryAtomCarrier (runtimePlanQueryId plan) (mkAtomId atomKey))
  | atomKey <- IntSet.toAscList (runtimePlanAtomKeys plan)
  ]
{-# INLINE planAtomCarriers #-}

planFactorCarriers ::
  RuntimePlan ctx prop ->
  [CarrierAddr ctx Carrier prop]
planFactorCarriers plan =
  [ carrierAddr
      (rpContext plan)
      (rpProp plan)
      (queryFactorCarrier (runtimePlanQueryId plan) node)
  | node <- runtimePlanFactorNodes plan,
    factorNodeCarrierVisible node
  ]
{-# INLINE planFactorCarriers #-}

validateGeneratedSite ::
  (Ord ctx, Ord prop) =>
  Map QueryId RuntimeQueryBinding ->
  GeneratedSiteState ctx prop ->
  Either [GeneratedSiteValidationError ctx prop] ()
validateGeneratedSite queryBindings site =
  case concat errors of
    [] ->
      Right ()
    generatedErrors ->
      Left generatedErrors
  where
    errors =
      [ either id (const []) $
          validateGeneratedContextShapeWithPrograms knownPrograms contextValue shape
            *> validateGeneratedContextRouting site contextValue shape
      | (contextValue, shape) <- Map.toAscList (gssContexts site)
      ]

    knownPrograms =
      Map.map (const ()) queryBindings
{-# INLINE validateGeneratedSite #-}
