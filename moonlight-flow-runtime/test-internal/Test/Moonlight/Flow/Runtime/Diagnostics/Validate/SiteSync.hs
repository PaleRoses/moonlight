module Test.Moonlight.Flow.Runtime.Diagnostics.Validate.SiteSync
  ( GeneratedSiteRuntimeSyncError (..),
    validateGeneratedSiteRuntimeSync,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Core
  ( QueryId,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
    caCarrier,
    RestrictKey,
    rkSource,
    rkTarget,
  )
import Moonlight.Flow.Carrier.Store
  ( carrierCurrentAddresses,
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
import Moonlight.Flow.Carrier.View.Cache
  ( VisibleContextKey (..),
    VisibleSectionCache (..),
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (PhaseProject),
  )
import Moonlight.Flow.Carrier.Reuse
  ( InstalledReuseMaterialization (..),
    lookupCarrierReuse,
    planReuseCarrierReuses,
    planReuseInstalledMaterializations,
    planReuseRegisteredCarriers,
    validatePlanReuseState,
  )
import Moonlight.Flow.Carrier.Core.Topology
  ( CarrierTopology,
    carrierTopologyAddresses,
    carrierTopologyDerivedOwners,
    carrierTopologyHasFamilyMember,
    carrierTopologyRestrictionEdges,
    carrierTopologySubsumptionEdges,
  )
import Moonlight.Differential.Carrier.Topology
  ( CarrierFamily,
    carrierFamilyMembers,
    carrierFamilyTargetContext,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
    rsCarrierTopology,
    rsGeneratedSite,
    rsPlanReuse,
    rsRouting,
  )
import Moonlight.Flow.Runtime.Carrier.State
  ( runtimeIndexOps,
    runtimeVisibleCache,
  )
import Moonlight.Flow.Runtime.Factor.State
  ( runtimeQueryBindings,
  )
import Moonlight.Flow.Runtime.Topology.Routing
  ( RuntimeRouting,
    RuntimeRoutingError,
    routeContextOfQuery,
    routeIndexShard,
    routeQueryShard,
  )
import Moonlight.Flow.Runtime.Topology.Site.Types
  ( GeneratedContextShape (..),
    GeneratedSiteState (..),
    canonicalContextOf,
  )
import Moonlight.Flow.Runtime.Topology.Site.Routing
  ( compileRouting,
  )
import Moonlight.Flow.Runtime.Topology.ReuseEdges
  ( insertPlanReuseTopologyEdges,
  )
import Moonlight.Flow.Runtime.Topology.Site.Topology
  ( compileGeneratedCarrierTopology,
  )

data GeneratedSiteRuntimeSyncError ctx prop
  = GeneratedSiteRuntimeRoutingCompileFailed !(RuntimeRoutingError ctx prop)
  | GeneratedSiteRuntimeRoutingMismatch
      !(RuntimeRouting ctx prop)
      !(RuntimeRouting ctx prop)
  | GeneratedSiteRuntimeTopologyMismatch
      !(CarrierTopology ctx Carrier prop)
      !(CarrierTopology ctx Carrier prop)
  | GeneratedSiteRuntimeBindingUnrouted !ctx !QueryId
  | GeneratedSiteRuntimeBindingWrongContext !ctx !QueryId !ctx
  | GeneratedSiteRuntimeBindingMissingProjectShard !ctx !QueryId
  | GeneratedSiteRuntimeMissingFactorProgram !ctx !QueryId
  | GeneratedSiteRuntimePlanReuseCarrierUnrouted !(CarrierAddr ctx Carrier prop)
  | GeneratedSiteRuntimeSubscriptionCarrierUnrouted !(CarrierAddr ctx Carrier prop)
  | GeneratedSiteRuntimeReuseSourceUnrouted !(CarrierReuseId ctx prop) !(CarrierAddr ctx Carrier prop)
  | GeneratedSiteRuntimeReuseTargetUnrouted !(CarrierReuseId ctx prop) !(CarrierAddr ctx Carrier prop)
  | GeneratedSiteRuntimeGraphReuseMissing
      !(CarrierReuseId ctx prop)
      !(CarrierAddr ctx Carrier prop)
      !(CarrierAddr ctx Carrier prop)
  | GeneratedSiteRuntimeGraphReuseSourceMismatch
      !(CarrierReuseId ctx prop)
      !(CarrierAddr ctx Carrier prop)
      !(CarrierAddr ctx Carrier prop)
  | GeneratedSiteRuntimeGraphReuseTargetMismatch
      !(CarrierReuseId ctx prop)
      !(CarrierAddr ctx Carrier prop)
      !(CarrierAddr ctx Carrier prop)
  | GeneratedSiteRuntimeMaterializationMissingReuse !(CarrierReuseId ctx prop)
  | GeneratedSiteRuntimeMaterializationTargetMismatch
      !(CarrierReuseId ctx prop)
      !(CarrierAddr ctx Carrier prop)
      !(CarrierAddr ctx Carrier prop)
  | GeneratedSiteRuntimeDerivedCarrierNoOwner !(CarrierAddr ctx Carrier prop)
  | GeneratedSiteRuntimeDerivedCarrierMultipleOwners
      !(CarrierAddr ctx Carrier prop)
      !(Set (CarrierReuseId ctx prop))
  | GeneratedSiteRuntimeDerivedCarrierCurrentWithoutOwner !(CarrierAddr ctx Carrier prop)
  | GeneratedSiteRuntimeReuseIndexMismatch
  | GeneratedSiteRuntimeReuseDepIndexMismatch
      !(IntMap.IntMap (Set (CarrierReuseId ctx prop)))
      !(IntMap.IntMap (Set (CarrierReuseId ctx prop)))
  | GeneratedSiteRuntimeReuseTopoIndexMismatch
      !(IntMap.IntMap (Set (CarrierReuseId ctx prop)))
      !(IntMap.IntMap (Set (CarrierReuseId ctx prop)))
  | GeneratedSiteRuntimeRestrictionSourceUnrouted !(RestrictKey ctx Carrier prop)
  | GeneratedSiteRuntimeRestrictionTargetUnrouted !(RestrictKey ctx Carrier prop)
  | GeneratedSiteRuntimeCoverMemberUnrouted
      !(CarrierFamily ctx Carrier prop)
      !(CarrierAddr ctx Carrier prop)
  | GeneratedSiteRuntimeCoverMemberUnsubscribed
      !(CarrierFamily ctx Carrier prop)
      !(CarrierAddr ctx Carrier prop)
  | GeneratedSiteRuntimeVisibleRemovedContext !ctx
  | GeneratedSiteRuntimeNonCanonicalOwner !ctx !ctx
  deriving stock (Eq, Show)

validateGeneratedSiteRuntimeSync ::
  (Ord ctx, Ord prop) =>
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either [GeneratedSiteRuntimeSyncError ctx prop] ()
validateGeneratedSiteRuntimeSync runtime =
  case compileRouting site of
    Left routingError ->
      Left [GeneratedSiteRuntimeRoutingCompileFailed routingError]
    Right expectedRouting ->
      finishErrors
        ( [ GeneratedSiteRuntimeRoutingMismatch expectedRouting routing
          | expectedRouting /= routing
          ]
            <> [ GeneratedSiteRuntimeTopologyMismatch expectedGraph graph
               | expectedGraph /= graph
               ]
            <> bindingErrors
            <> programErrors
            <> planReuseRouteErrors
            <> subscriptionRouteErrors
            <> planReuseCarrierReuseRouteErrors
            <> subscriptionCarrierReuseRouteErrors
            <> graphReuseMetadataErrors
            <> installedMaterializationOwnershipErrors
            <> derivedCarrierOwnershipErrors
            <> reuseIndexErrors
            <> restrictionRouteErrors
            <> coverRouteErrors
            <> coverSubscriptionErrors
            <> visibleRemovedContextErrors
            <> nonCanonicalOwnerErrors
        )
  where
    site =
      rsGeneratedSite (rdrState runtime)
    routing =
      rsRouting (rdrState runtime)
    expectedGraph =
      insertPlanReuseTopologyEdges
        (rsPlanReuse (rdrState runtime))
        (compileGeneratedCarrierTopology site)
    generatedBindings =
      [ (contextValue, queryId, binding)
      | (contextValue, shape) <- Map.toAscList (gssContexts site),
        (queryId, binding) <- Map.toAscList (gcsQueryBindings shape)
      ]
    bindingErrors =
      concatMap bindingRouteErrors generatedBindings
    bindingRouteErrors (contextValue, queryId, _binding) =
      case routeContextOfQuery queryId routing of
        Nothing ->
          [GeneratedSiteRuntimeBindingUnrouted contextValue queryId]
        Just routedContext
          | routedContext /= contextValue ->
              [GeneratedSiteRuntimeBindingWrongContext contextValue queryId routedContext]
          | otherwise ->
              [ GeneratedSiteRuntimeBindingMissingProjectShard contextValue queryId
              | routeQueryShard PhaseProject queryId routing == Nothing
              ]
    programErrors =
      [ GeneratedSiteRuntimeMissingFactorProgram contextValue queryId
      | (contextValue, queryId, _binding) <- generatedBindings,
        not (Map.member queryId (runtimeQueryBindings runtime))
      ]
    planReuseCarriers =
      planReuseRegisteredCarriers (rsPlanReuse (rdrState runtime))
    planReuseRouteErrors =
      [ GeneratedSiteRuntimePlanReuseCarrierUnrouted addr
      | addr <- Set.toAscList planReuseCarriers,
        routeIndexShard addr routing == Nothing
      ]
    graph =
      rsCarrierTopology (rdrState runtime)
    subscribedCarriers =
      carrierTopologyAddresses graph
    planReuse =
      rsPlanReuse (rdrState runtime)
    subscriptionRouteErrors =
      [ GeneratedSiteRuntimeSubscriptionCarrierUnrouted addr
      | addr <- Set.toAscList subscribedCarriers,
        routeIndexShard addr routing == Nothing
      ]
    planReuseCarrierReuseRouteErrors =
      concatMap
        reuseRouteErrors
        (planReuseCarrierReuses (rsPlanReuse (rdrState runtime)))
    subscriptionCarrierReuseRouteErrors =
      concatMap
        reuseEdgeRouteErrors
        (carrierTopologySubsumptionEdges graph)
    graphReuseMetadataErrors =
      concatMap graphReuseMetadataError
        (carrierTopologySubsumptionEdges graph)
    graphReuseMetadataError (reuseId, source, target) =
      case lookupCarrierReuse reuseId planReuse of
        Nothing ->
          [GeneratedSiteRuntimeGraphReuseMissing reuseId source target]
        Just reuse ->
          let expectedSource =
                rwSourceCarrier (cruWitness reuse)
              expectedTarget =
                carrierReuseExpectedTarget reuse
           in [ GeneratedSiteRuntimeGraphReuseSourceMismatch reuseId expectedSource source
              | expectedSource /= source
              ]
                <> [ GeneratedSiteRuntimeGraphReuseTargetMismatch reuseId expected target
                   | Just expected <- [expectedTarget],
                     expected /= target
                   ]
    installedMaterializationOwnershipErrors =
      concat
        [ case lookupCarrierReuse reuseId planReuse of
            Nothing ->
              [GeneratedSiteRuntimeMaterializationMissingReuse reuseId]
            Just reuse ->
              case carrierReuseExpectedTarget reuse of
                Nothing ->
                  []
                Just expectedTarget ->
                  [ GeneratedSiteRuntimeMaterializationTargetMismatch reuseId expectedTarget (irmTarget installed)
                  | expectedTarget /= irmTarget installed
                  ]
        | (reuseId, installed) <- planReuseInstalledMaterializations (rsPlanReuse (rdrState runtime))
        ]
    derivedOwners =
      carrierTopologyDerivedOwners graph
    currentDerivedCarriers =
      Set.filter isDerivedCarrierAddr (currentCarrierAddresses runtime)
    derivedCarrierOwnershipErrors =
      [ GeneratedSiteRuntimeDerivedCarrierMultipleOwners target (Set.map fst owners)
      | (target, owners) <- Map.toAscList derivedOwners,
        Set.size owners /= 1
      ]
        <> [ GeneratedSiteRuntimeDerivedCarrierCurrentWithoutOwner target
           | target <- Set.toAscList currentDerivedCarriers,
             Map.notMember target derivedOwners
           ]
    reuseIndexErrors =
      case validatePlanReuseState planReuse of
        Right () ->
          []
        Left _errors ->
          [GeneratedSiteRuntimeReuseIndexMismatch]
    reuseRouteErrors (reuseId, reuse) =
      case carrierReuseExpectedTarget reuse of
        Nothing ->
          []
        Just target ->
          reuseEdgeRouteErrors
            ( reuseId,
              rwSourceCarrier (cruWitness reuse),
              target
            )
    reuseEdgeRouteErrors (reuseId, source, target) =
      [ GeneratedSiteRuntimeReuseSourceUnrouted reuseId source
      | routeIndexShard source routing == Nothing
      ]
        <> [ GeneratedSiteRuntimeReuseTargetUnrouted reuseId target
           | routeIndexShard target routing == Nothing
           ]
    restrictionEdges =
      carrierTopologyRestrictionEdges graph
    restrictionRouteErrors =
      concatMap restrictRouteErrors restrictionEdges
    restrictRouteErrors restrictKey =
      [ GeneratedSiteRuntimeRestrictionSourceUnrouted restrictKey
      | routeIndexShard (rkSource restrictKey) routing == Nothing
      ]
        <> [ GeneratedSiteRuntimeRestrictionTargetUnrouted restrictKey
           | routeIndexShard (rkTarget restrictKey) routing == Nothing
           ]
    coverRouteErrors =
      [ GeneratedSiteRuntimeCoverMemberUnrouted family addr
      | (family, _generatedCover) <- Map.toAscList (gssCovers site),
        addr <- Set.toAscList (carrierFamilyMembers family),
        routeIndexShard addr routing == Nothing
      ]
    coverSubscriptionErrors =
      [ GeneratedSiteRuntimeCoverMemberUnsubscribed family addr
      | (family, _generatedCover) <- Map.toAscList (gssCovers site),
        addr <- Set.toAscList (carrierFamilyMembers family),
        not (coverMemberSubscribed family addr graph)
      ]
    visibleRemovedContextErrors =
      [ GeneratedSiteRuntimeVisibleRemovedContext contextValue
      | keyValue <- Map.keys (vscEntries (runtimeVisibleCache (rdrState runtime))),
        let contextValue = vckContext keyValue,
        not (Map.member contextValue (gssContexts site))
      ]
    ownedContexts =
      Set.unions
        [ Map.keysSet (gssContexts site),
          Set.fromList (Map.elems (gssPlanObjects site)),
          Set.fromList [carrierFamilyTargetContext family | family <- Map.keys (gssCovers site)],
          Set.fromList [caContext addr | addr <- Set.toAscList subscribedCarriers],
          Set.fromList [caContext addr | addr <- Set.toAscList planReuseCarriers]
        ]
    nonCanonicalOwnerErrors =
      [ GeneratedSiteRuntimeNonCanonicalOwner contextValue canonical
      | contextValue <- Set.toAscList ownedContexts,
        let canonical = canonicalContextOf contextValue (gssContextClasses site),
        canonical /= contextValue
      ]
{-# INLINE validateGeneratedSiteRuntimeSync #-}

currentCarrierAddresses ::
  (Ord ctx, Ord prop) =>
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Set (CarrierAddr ctx Carrier prop)
currentCarrierAddresses runtime =
  Set.unions
    [ carrierCurrentAddresses store
    | store <- IntMap.elems (runtimeIndexOps (rdrState runtime))
    ]
{-# INLINE currentCarrierAddresses #-}

isDerivedCarrierAddr :: CarrierAddr ctx Carrier prop -> Bool
isDerivedCarrierAddr addr =
  case caCarrier addr of
    DerivedCarrier {} ->
      True
    QueryCarrier {} ->
      False
{-# INLINE isDerivedCarrierAddr #-}


coverMemberSubscribed ::
  (Ord ctx, Ord prop) =>
  CarrierFamily ctx Carrier prop ->
  CarrierAddr ctx Carrier prop ->
  CarrierTopology ctx Carrier prop ->
  Bool
coverMemberSubscribed family addr graph =
  carrierTopologyHasFamilyMember family addr graph
{-# INLINE coverMemberSubscribed #-}

finishErrors :: [err] -> Either [err] ()
finishErrors errors =
  case errors of
    [] -> Right ()
    _ -> Left errors
{-# INLINE finishErrors #-}
