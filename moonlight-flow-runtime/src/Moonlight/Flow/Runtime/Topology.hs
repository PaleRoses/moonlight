{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Runtime.Topology
  ( RuntimeTopology,
    RuntimeTopologySource (..),
    RuntimeTopologyView,
    RuntimeTopologyStamp (..),
    RuntimeTopologyError (..),
    RuntimeTopologyTransitionError (..),
    RuntimeTopologyTransition (..),
    compileRuntimeTopology,
    applyRuntimeTopologyPatch,
    compileRuntimeTopologyTransition,
    updateRuntimeGeneratedSite,
    updateRuntimePlanReuse,
    mapRuntimePlanReuse,
    planReuseTopologyDigest,
    runtimeTopologySource,
    runtimeGeneratedSite,
    runtimePlanReuse,
    runtimeTopologyView,
    runtimeTopologyViewStamp,
    runtimeRouting,
    runtimeCarrierTopology,
    runtimeTopologyStamp,
    validateRuntimeTopology,
    validateRuntimeTopologyBindings,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Kind
  ( Type,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Flow.Carrier.Core.Topology
  ( CarrierTopology,
  )
import Moonlight.Flow.Carrier.Reuse
  ( PlanReuseState,
    planReuseTopologyDigest,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.Flow.Runtime.Topology.ReuseEdges
  ( insertPlanReuseTopologyEdges,
  )
import Moonlight.Flow.Runtime.Topology.Routing
  ( RuntimeRouting,
    RuntimeRoutingError,
  )
import Moonlight.Flow.Runtime.Topology.Site.Patch
  ( GeneratedSitePatch,
    GeneratedSiteTransition (..),
    SiteEffects,
    applyGeneratedSitePatchState,
  )
import Moonlight.Flow.Runtime.Topology.Site.Routing
  ( RoutingDelta (..),
    compileRouting,
    diffRouting,
  )
import Moonlight.Flow.Runtime.Topology.Site.Topology
  ( compileGeneratedCarrierTopology,
  )
import Moonlight.Flow.Runtime.Topology.Site.Types
  ( GeneratedSitePatchError,
    GeneratedSiteState (..),
  )
import Moonlight.Flow.Runtime.Topology.Validate
  ( validateRuntimeTopology,
    validateRuntimeTopologyBindings,
  )

type RuntimeTopologySource :: Type -> Type -> Type
data RuntimeTopologySource ctx prop = RuntimeTopologySource
  { rtsGeneratedSite :: !(GeneratedSiteState ctx prop),
    rtsPlanReuse :: !(PlanReuseState ctx prop)
  }
  deriving stock (Eq, Show)

type RuntimeTopologyStamp :: Type
data RuntimeTopologyStamp = RuntimeTopologyStamp
  { rtsGeneratedSiteDigest :: !StableDigest128,
    rtsPlanReuseTopologyDigest :: !StableDigest128
  }
  deriving stock (Eq, Ord, Show)

type RuntimeTopologyView :: Type -> Type -> Type
data RuntimeTopologyView ctx prop = RuntimeTopologyView
  { rtvStamp :: !RuntimeTopologyStamp,
    rtvRouting :: !(RuntimeRouting ctx prop),
    rtvCarrierTopology :: !(CarrierTopology ctx Carrier prop)
  }
  deriving stock (Eq, Show)

type RuntimeTopology :: Type -> Type -> Type
data RuntimeTopology ctx prop = RuntimeTopology
  { rtSource :: !(RuntimeTopologySource ctx prop),
    rtView :: !(RuntimeTopologyView ctx prop)
  }
  deriving stock (Eq, Show)

type RuntimeTopologyError :: Type -> Type -> Type
data RuntimeTopologyError ctx prop
  = RuntimeTopologyRoutingError !(RuntimeRoutingError ctx prop)
  deriving stock (Eq, Show)

type RuntimeTopologyTransitionError :: Type -> Type -> Type
data RuntimeTopologyTransitionError ctx prop
  = RuntimeTopologyTransitionPatchError !(GeneratedSitePatchError ctx prop)
  | RuntimeTopologyTransitionRoutingError !(RuntimeRoutingError ctx prop)
  deriving stock (Eq, Show)

type RuntimeTopologyTransition :: Type -> Type -> Type
data RuntimeTopologyTransition ctx prop = RuntimeTopologyTransition
  { rttPatch :: !(GeneratedSitePatch ctx prop),
    rttSiteBefore :: !(GeneratedSiteState ctx prop),
    rttSiteAfter :: !(GeneratedSiteState ctx prop),
    rttSiteEffects :: !(SiteEffects ctx prop),
    rttRoutingDelta :: !(RoutingDelta ctx prop),
    rttTopology :: !(RuntimeTopology ctx prop)
  }

compileRuntimeTopology ::
  (Ord ctx, Ord prop) =>
  RuntimeTopologySource ctx prop ->
  Either (RuntimeTopologyError ctx prop) (RuntimeTopology ctx prop)
compileRuntimeTopology source = do
  routing <-
    first RuntimeTopologyRoutingError $
      compileRouting (rtsGeneratedSite source)
  pure (runtimeTopologyFromRouting source routing)

applyRuntimeTopologyPatch ::
  (Ord ctx, Ord prop) =>
  GeneratedSitePatch ctx prop ->
  RuntimeTopology ctx prop ->
  Either
    (RuntimeTopologyTransitionError ctx prop)
    (RuntimeTopologyTransition ctx prop)
applyRuntimeTopologyPatch patch topology = do
  siteTransition <-
    first RuntimeTopologyTransitionPatchError $
      applyGeneratedSitePatchState patch (runtimeGeneratedSite topology)
  compileRuntimeTopologyTransition
    (runtimePlanReuse topology)
    patch
    siteTransition

compileRuntimeTopologyTransition ::
  (Ord ctx, Ord prop) =>
  PlanReuseState ctx prop ->
  GeneratedSitePatch ctx prop ->
  GeneratedSiteTransition ctx prop ->
  Either
    (RuntimeTopologyTransitionError ctx prop)
    (RuntimeTopologyTransition ctx prop)
compileRuntimeTopologyTransition planReuse patch siteTransition = do
  routingDelta <-
    first RuntimeTopologyTransitionRoutingError $
      diffRouting siteTransition
  let source =
        RuntimeTopologySource
          { rtsGeneratedSite = gstAfter siteTransition,
            rtsPlanReuse = planReuse
          }
      topology =
        runtimeTopologyFromRouting source (rdAfter routingDelta)
  pure
    RuntimeTopologyTransition
      { rttPatch = patch,
        rttSiteBefore = gstBefore siteTransition,
        rttSiteAfter = gstAfter siteTransition,
        rttSiteEffects = gstEffects siteTransition,
        rttRoutingDelta = routingDelta,
        rttTopology = topology
      }

compileRuntimeCarrierTopology ::
  (Ord ctx, Ord prop) =>
  GeneratedSiteState ctx prop ->
  PlanReuseState ctx prop ->
  CarrierTopology ctx Carrier prop
compileRuntimeCarrierTopology site planReuse =
  insertPlanReuseTopologyEdges
    planReuse
    (compileGeneratedCarrierTopology site)

runtimeTopologyFromRouting ::
  (Ord ctx, Ord prop) =>
  RuntimeTopologySource ctx prop ->
  RuntimeRouting ctx prop ->
  RuntimeTopology ctx prop
runtimeTopologyFromRouting source routing =
  RuntimeTopology
    { rtSource = source,
      rtView =
        RuntimeTopologyView
          { rtvStamp = runtimeTopologyStamp source,
            rtvRouting = routing,
            rtvCarrierTopology = carrierTopology
          }
    }
  where
    carrierTopology =
      compileRuntimeCarrierTopology
        (rtsGeneratedSite source)
        (rtsPlanReuse source)

updateRuntimeGeneratedSite ::
  (Ord ctx, Ord prop) =>
  GeneratedSiteState ctx prop ->
  RuntimeTopology ctx prop ->
  Either (RuntimeTopologyError ctx prop) (RuntimeTopology ctx prop)
updateRuntimeGeneratedSite generatedSite topology =
  compileRuntimeTopology
    RuntimeTopologySource
      { rtsGeneratedSite = generatedSite,
        rtsPlanReuse = runtimePlanReuse topology
      }

updateRuntimePlanReuse ::
  (Ord ctx, Ord prop) =>
  PlanReuseState ctx prop ->
  RuntimeTopology ctx prop ->
  RuntimeTopology ctx prop
updateRuntimePlanReuse planReuse topology =
  runtimeTopologyFromRouting
    RuntimeTopologySource
      { rtsGeneratedSite = runtimeGeneratedSite topology,
        rtsPlanReuse = planReuse
      }
    (runtimeRouting topology)

mapRuntimePlanReuse ::
  (Ord ctx, Ord prop) =>
  (PlanReuseState ctx prop -> PlanReuseState ctx prop) ->
  RuntimeTopology ctx prop ->
  RuntimeTopology ctx prop
mapRuntimePlanReuse transform topology =
  updateRuntimePlanReuse (transform (runtimePlanReuse topology)) topology

runtimeTopologySource :: RuntimeTopology ctx prop -> RuntimeTopologySource ctx prop
runtimeTopologySource =
  rtSource

runtimeGeneratedSite :: RuntimeTopology ctx prop -> GeneratedSiteState ctx prop
runtimeGeneratedSite =
  rtsGeneratedSite . rtSource

runtimePlanReuse :: RuntimeTopology ctx prop -> PlanReuseState ctx prop
runtimePlanReuse =
  rtsPlanReuse . rtSource

runtimeTopologyView :: RuntimeTopology ctx prop -> RuntimeTopologyView ctx prop
runtimeTopologyView =
  rtView

runtimeTopologyViewStamp :: RuntimeTopology ctx prop -> RuntimeTopologyStamp
runtimeTopologyViewStamp =
  rtvStamp . rtView

runtimeRouting :: RuntimeTopology ctx prop -> RuntimeRouting ctx prop
runtimeRouting =
  rtvRouting . rtView

runtimeCarrierTopology :: RuntimeTopology ctx prop -> CarrierTopology ctx Carrier prop
runtimeCarrierTopology =
  rtvCarrierTopology . rtView

runtimeTopologyStamp ::
  RuntimeTopologySource ctx prop ->
  RuntimeTopologyStamp
runtimeTopologyStamp source =
  RuntimeTopologyStamp
    { rtsGeneratedSiteDigest = gssDigest (rtsGeneratedSite source),
      rtsPlanReuseTopologyDigest = planReuseTopologyDigest (rtsPlanReuse source)
    }
