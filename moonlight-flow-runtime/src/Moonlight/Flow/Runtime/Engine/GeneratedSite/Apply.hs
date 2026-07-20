module Moonlight.Flow.Runtime.Engine.GeneratedSite.Apply
  ( applyGeneratedSitePatchRuntime,
    applyGeneratedSiteTransitionRuntime,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Set qualified as Set
import Moonlight.Flow.Carrier.View.Cache
  ( dropVisibleContext,
  )
import Moonlight.Flow.Runtime.Carrier.Reuse.State
  ( dropRuntimeCarrierReuseStates,
    invalidateRuntimePlanReuseByScope,
  )
import Moonlight.Flow.Runtime.Carrier.State
  ( mapRuntimeVisibleCache,
  )
import Moonlight.Flow.Runtime.Core.State qualified as Core
import Moonlight.Flow.Runtime.Engine.GeneratedSite.Materialize
  ( materializeCarrierMoves,
  )
import Moonlight.Flow.Runtime.Engine.GeneratedSite.Validation
  ( runtimeTopologyTransitionError,
    validateGeneratedSiteCandidateRuntime,
  )
import Moonlight.Flow.Runtime.Engine.Input
  ( RuntimeGeneratedSitePatch (..),
  )
import Moonlight.Flow.Runtime.Engine.Schedule.Enqueue
  ( scheduleRuntimeDataflowOps,
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Factor.State
  ( factorQueryRepresentativeQueryId,
    factorQueryRepairKey,
    factorRepairKeyIsCold,
    installFactorQueryBindings,
    installFactorPrograms,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
  )
import Moonlight.Flow.Runtime.Topology
  ( RuntimeTopologyTransition (..),
    applyRuntimeTopologyPatch,
  )
import Moonlight.Flow.Runtime.Topology.Lowering.GeneratedSite
  ( lowerGeneratedSiteTransition,
  )
import Moonlight.Flow.Runtime.Topology.Lowering.Types
  ( RuntimeRepairRoute (..),
    RuntimeRepairRouting (..),
  )
import Moonlight.Flow.Runtime.Topology.Site.Patch
  ( GeneratedSiteTransition (..),
    SiteEffects (..),
  )
import Moonlight.Flow.Runtime.Topology.Site.Routing
  ( RoutingDelta (..),
  )
import Moonlight.Flow.Runtime.Topology.Site.Types

applyGeneratedSitePatchRuntime ::
  (Ord ctx, Ord prop) =>
  RuntimeGeneratedSitePatch ctx prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
applyGeneratedSitePatchRuntime runtimePatch runtime0 = do
  topologyTransition <-
    first runtimeTopologyTransitionError $
      applyRuntimeTopologyPatch
        (rgspSitePatch runtimePatch)
        (Core.rsTopology (rdrState runtime0))
  applyGeneratedSiteTransitionRuntime runtimePatch topologyTransition runtime0

applyGeneratedSiteTransitionRuntime ::
  (Ord ctx, Ord prop) =>
  RuntimeGeneratedSitePatch ctx prop ->
  RuntimeTopologyTransition ctx prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
applyGeneratedSiteTransitionRuntime runtimePatch topologyTransition runtime0 = do
  let effects =
        gstEffects transition
      state0 =
        rdrState runtime0
      runtimeWithSite =
        runtime0
          { rdrState =
              Core.mapRuntimeTopologySection
                (const topologyWithSite)
                state0
          }
      runtimePrograms =
        installFactorQueryBindings
          (rgspQueryBindings runtimePatch)
          ( installFactorPrograms
              (rgspFactorPrograms runtimePatch)
              runtimeWithSite
          )
      reuseInvalidationCarriers =
        Set.toAscList $
          cmEvict (rdCarrierMoves routingDelta)
            <> foldMap
              (\(source, target) -> Set.fromList [source, target])
              (carrierMovesRetargetPairs (rdCarrierMoves routingDelta))
      runtimeReuse =
        invalidateRuntimePlanReuseByScope
          (seRelationalScope effects)
          (dropRuntimeCarrierReuseStates reuseInvalidationCarriers runtimePrograms)
  runtimeChecked <-
    validateGeneratedSiteCandidateRuntime runtimeReuse
  let runtimeVisible =
        Set.foldl'
          ( \runtime contextValue ->
              runtime
                { rdrState =
                    mapRuntimeVisibleCache
                      (dropVisibleContext contextValue)
                      (rdrState runtime)
                }
          )
          runtimeChecked
          (rdDropContexts routingDelta)
  runtimeCarriers <-
    materializeCarrierMoves routingDelta runtimeVisible
  scheduleRuntimeDataflowOps
    ( lowerGeneratedSiteTransition
        (repairRoutingForRuntime runtimeCarriers)
        patch
        transition
    )
    runtimeCarriers
  where
    patch =
      rttPatch topologyTransition

    transition =
      GeneratedSiteTransition
        { gstBefore = rttSiteBefore topologyTransition,
          gstAfter = rttSiteAfter topologyTransition,
          gstEffects = rttSiteEffects topologyTransition
        }

    routingDelta =
      rttRoutingDelta topologyTransition

    topologyWithSite =
      rttTopology topologyTransition

repairRoutingForRuntime ::
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RuntimeRepairRouting
repairRoutingForRuntime runtime =
  RuntimeRepairRouting
    { rrRepairRouteOfQuery = repairRouteOfQuery,
      rrRepairIsCold = factorRepairKeyIsCold runtime
    }
  where
    repairRouteOfQuery queryId = do
      repairKey <- factorQueryRepairKey runtime queryId
      representativeQueryId <- factorQueryRepresentativeQueryId runtime queryId
      pure
        RuntimeRepairRoute
          { rrtRepairKey = repairKey,
            rrtRepresentativeQueryId = representativeQueryId
          }
