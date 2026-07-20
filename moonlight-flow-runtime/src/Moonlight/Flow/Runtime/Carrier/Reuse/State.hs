{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Runtime.Carrier.Reuse.State
  ( runtimePlanReuseState,
    replaceRuntimePlanReuse,
    transformRuntimePlanReuse,
    transformRuntimePlanReuseStats,
    selectStaleCarrierReusesRuntime,
    selectStaleInstalledReuseMaterializationsRuntime,
    dropSelectedCarrierReusesRuntime,
    invalidateRuntimePlanReuseByPatch,
    invalidateRuntimePlanReuseByScope,
    dropRuntimeCarrierReuseState,
    dropRuntimeCarrierReuseStates,
    lookupRuntimeCarrierReuse,
    runtimePlanReuseTargetReuseIds,
    recordRuntimeReuseProjectionRejection,
    currentCarrierLookupE,
  )
where

import Data.Set
  ( Set,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
import Moonlight.Flow.Carrier.Reuse
  ( InstalledReuseMaterialization,
    PlanReuseState,
    PlanReuseStats,
    StaleCarrierReuse,
    dropPlanReuseCarrierReuseState,
    dropSelectedCarrierReuses,
    invalidatePlanReuseByPatch,
    invalidatePlanReuseState,
    lookupCarrierReuse,
    mapPlanReuseStats,
    planReuseTargetReuseIds,
    recordBoundaryRejected,
    recordObstructedProjection,
    selectStaleCarrierReuses,
    selectStaleInstalledReuseMaterializations,
  )
import Moonlight.Flow.Model.Delta
  ( QuotientPatch
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope,
    scopeDeps,
    scopeTopo,
  )
import Moonlight.Flow.Runtime.Carrier.Store
  ( currentCarrierMaybe,
  )
import Moonlight.Flow.Runtime.Carrier.Reuse.CoverMaterialization
  ( CoverMaterializationError (..),
    CurrentCarrierLookupE (..),
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
    rsPlanReuse,
  )
import Moonlight.Flow.Runtime.Core.State qualified as Core
import Moonlight.Flow.Runtime.Topology
  ( updateRuntimePlanReuse,
  )

runtimePlanReuseState ::
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  PlanReuseState ctx prop
runtimePlanReuseState =
  rsPlanReuse . rdrState
{-# INLINE runtimePlanReuseState #-}

replaceRuntimePlanReuse ::
  (Ord ctx, Ord prop) =>
  PlanReuseState ctx prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
replaceRuntimePlanReuse planReuse runtime =
  runtime
    { rdrState =
        Core.mapRuntimeTopologySection
          (updateRuntimePlanReuse planReuse)
          (rdrState runtime)
    }
{-# INLINE replaceRuntimePlanReuse #-}

transformRuntimePlanReuse ::
  (Ord ctx, Ord prop) =>
  (PlanReuseState ctx prop -> PlanReuseState ctx prop) ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
transformRuntimePlanReuse updatePlanReuse runtime =
  replaceRuntimePlanReuse
    (updatePlanReuse (runtimePlanReuseState runtime))
    runtime
{-# INLINE transformRuntimePlanReuse #-}

transformRuntimePlanReuseStats ::
  (Ord ctx, Ord prop) =>
  (PlanReuseStats -> PlanReuseStats) ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
transformRuntimePlanReuseStats updateStats =
  transformRuntimePlanReuse (mapPlanReuseStats updateStats)
{-# INLINE transformRuntimePlanReuseStats #-}

selectStaleCarrierReusesRuntime ::
  (Ord ctx, Ord prop) =>
  RelationalScope ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  [StaleCarrierReuse ctx prop]
selectStaleCarrierReusesRuntime dirtyScope runtime =
  selectStaleCarrierReuses dirtyScope (runtimePlanReuseState runtime)
{-# INLINE selectStaleCarrierReusesRuntime #-}

selectStaleInstalledReuseMaterializationsRuntime ::
  (Ord ctx, Ord prop) =>
  RelationalScope ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  [(CarrierReuseId ctx prop, InstalledReuseMaterialization ctx prop)]
selectStaleInstalledReuseMaterializationsRuntime dirtyScope runtime =
  selectStaleInstalledReuseMaterializations dirtyScope (runtimePlanReuseState runtime)
{-# INLINE selectStaleInstalledReuseMaterializationsRuntime #-}

dropSelectedCarrierReusesRuntime ::
  (Ord ctx, Ord prop, Foldable f) =>
  f (CarrierReuseId ctx prop) ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
dropSelectedCarrierReusesRuntime reuseIds =
  transformRuntimePlanReuse (dropSelectedCarrierReuses reuseIds)
{-# INLINE dropSelectedCarrierReusesRuntime #-}

invalidateRuntimePlanReuseByPatch ::
  (Ord ctx, Ord prop) =>
  QuotientPatch ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
invalidateRuntimePlanReuseByPatch patch =
  transformRuntimePlanReuse (invalidatePlanReuseByPatch patch)
{-# INLINE invalidateRuntimePlanReuseByPatch #-}

invalidateRuntimePlanReuseByScope ::
  (Ord ctx, Ord prop) =>
  RelationalScope ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
invalidateRuntimePlanReuseByScope dirtyScope =
  transformRuntimePlanReuse
    (invalidatePlanReuseState (scopeDeps dirtyScope) (scopeTopo dirtyScope))
{-# INLINE invalidateRuntimePlanReuseByScope #-}

dropRuntimeCarrierReuseState ::
  (Ord ctx, Ord prop) =>
  CarrierAddr ctx Carrier prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
dropRuntimeCarrierReuseState addr =
  transformRuntimePlanReuse (dropPlanReuseCarrierReuseState addr)
{-# INLINE dropRuntimeCarrierReuseState #-}

dropRuntimeCarrierReuseStates ::
  (Ord ctx, Ord prop, Foldable f) =>
  f (CarrierAddr ctx Carrier prop) ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
dropRuntimeCarrierReuseStates addrs =
  transformRuntimePlanReuse (\state -> foldr dropPlanReuseCarrierReuseState state addrs)
{-# INLINE dropRuntimeCarrierReuseStates #-}

lookupRuntimeCarrierReuse ::
  (Ord ctx, Ord prop) =>
  CarrierReuseId ctx prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Maybe (CarrierReuse ctx prop)
lookupRuntimeCarrierReuse reuseId runtime =
  lookupCarrierReuse reuseId (runtimePlanReuseState runtime)
{-# INLINE lookupRuntimeCarrierReuse #-}

runtimePlanReuseTargetReuseIds ::
  (Ord ctx, Ord prop) =>
  CarrierAddr ctx Carrier prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Set (CarrierReuseId ctx prop)
runtimePlanReuseTargetReuseIds target runtime =
  planReuseTargetReuseIds target (runtimePlanReuseState runtime)
{-# INLINE runtimePlanReuseTargetReuseIds #-}

recordRuntimeReuseProjectionRejection ::
  (Ord ctx, Ord prop) =>
  CarrierReuseError ctx prop evidence ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
recordRuntimeReuseProjectionRejection projectionError =
  transformRuntimePlanReuseStats $
    case projectionError of
      CarrierReuseBoundaryFailed {} ->
        recordBoundaryRejected 1
      CarrierReuseBoundaryMismatch {} ->
        recordBoundaryRejected 1
      CarrierReuseObstructed {} ->
        recordObstructedProjection 1
      CarrierReuseRowsFailed {} ->
        id
      CarrierReuseAddressPolicyFailed {} ->
        id
      CarrierReuseSourceCoverageNotExact {} ->
        id
      CarrierReuseExactProjectionNotPreserved {} ->
        recordBoundaryRejected 1
      CarrierReuseSupportProjectionFailed {} ->
        recordBoundaryRejected 1
      CarrierReuseEvidenceProjectionFailed {} ->
        recordBoundaryRejected 1
{-# INLINE recordRuntimeReuseProjectionRejection #-}

currentCarrierLookupE ::
  (Ord ctx, Ord prop) =>
  RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr ->
  CurrentCarrierLookupE ctx prop evidence
currentCarrierLookupE runtime =
  CurrentCarrierLookupE $ \addr ->
    case currentCarrierMaybe addr runtime of
      Left _runtimeError ->
        Left (CoverRuntimeLookupFailed addr)
      Right maybeSnapshot ->
        Right maybeSnapshot
{-# INLINE currentCarrierLookupE #-}
