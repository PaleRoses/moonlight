module Moonlight.Flow.Carrier.Reuse
  ( PlanReuseState,
    emptyPlanReuseState,
    ReuseConfig (..),
    ReuseMode (..),
    defaultReuseConfig,
    PlanReuseRegistrationEntry (..),
    PlanReuseRegistration (..),
    PlanReuseRequest (..),
    CarrierReuseStrategy (..),
    CarrierReuseCandidateGroup (..),
    PlanReuseMiss (..),
    PlanReuseError (..),
    PlanReuseDiagnostics (..),
    PlanReuseInvariantError (..),
    InstalledReuseMaterialization (..),
    ReuseValidityRequest (..),
    PlanReuseStats (..),
    RequestedFactorShape (..),
    SubsumptionEntry (..),
    SubsumptionRegistrationError (..),
    ReuseValidity (..),
    StaleCarrierReuse (..),
    CarrierReuseRegistryInvariantError (..),
    SubsumptionIndexInvariantError (..),
    PlanReuseInvalidationPostconditionError (..),
    recordExactReuseEmits,
    recordContainmentReuseEmits,
    recordBoundaryRejected,
    recordObstructedProjection,
    recordStaleRejected,
    recordResidualRejected,
    registerReusableCarriers,
    registerFactorCarrierShapes,
    registerSubsumptionEntry,
    carrierReuseStrategiesForMode,
    planCarrierReuse,
    planCarrierReuseStrategy,
    normalizeRequestedFactorShape,
    lookupReusableCarrierEntry,
    reuseExactValidityMatchesRequest,
    registerCarrierReuse,
    registerCarrierReuses,
    lookupCarrierReuse,
    selectStaleCarrierReuses,
    dropSelectedCarrierReuses,
    reuseValidityRequestFromTime,
    installCarrierReuse,
    installPlanReuseMaterialization,
    removePlanReuseInstalledMaterialization,
    invalidatePlanReuseByPatch,
    invalidatePlanReuseState,
    dropPlanReuseCarrierReuseState,
    invalidateCarrierReusesByDepsTopo,
    validatePlanReuseInvalidationPostcondition,
    planReuseStats,
    mapPlanReuseStats,
    planReuseCarrierReuses,
    planReuseTopologyDigest,
    planReuseRegisteredCarriers,
    planReuseInstalledMaterializations,
    selectStaleInstalledReuseMaterializations,
    planReuseTargetReuseIds,
    validatePlanReuseState,
    planReuseDiagnostics,
  )
where

import Moonlight.Flow.Carrier.Reuse.Config
  ( ReuseConfig (..),
    ReuseMode (..),
    defaultReuseConfig,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Invalidation
  ( PlanReuseInvalidationPostconditionError (..),
    dropPlanReuseCarrierReuseState,
    invalidateCarrierReusesByDepsTopo,
    invalidatePlanReuseByPatch,
    invalidatePlanReuseState,
    validatePlanReuseInvalidationPostcondition,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Index.Shape
  ( SubsumptionIndexInvariantError (..),
    lookupSubsumptionEntryByCarrier,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Index.Reuse
  ( CarrierReuseRegistryInvariantError (..),
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Registry
  ( StaleCarrierReuse (..),
    dropSelectedCarrierReuses,
    lookupCarrierReuse,
    registerCarrierReuse,
    registerCarrierReuses,
    selectStaleCarrierReuses,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Resolve
  ( carrierReuseStrategiesForMode,
    planCarrierReuse,
    planCarrierReuseStrategy,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Shape
  ( RequestedFactorShape (..),
    SubsumptionEntry (..),
    SubsumptionRegistrationError (..),
  )
import Moonlight.Flow.Carrier.Reuse.Internal.State.Diagnostics
  ( planReuseDiagnostics,
    planReuseCarrierReuses,
    planReuseRegisteredCarriers,
    planReuseTopologyDigest,
    planReuseTargetReuseIds,
    validatePlanReuseState,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.State.Materialization
  ( installCarrierReuse,
    installPlanReuseMaterialization,
    planReuseInstalledMaterializations,
    removePlanReuseInstalledMaterialization,
    selectStaleInstalledReuseMaterializations,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.State.Normalize
  ( normalizeRequestedFactorShape,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.State.Register
  ( registerFactorCarrierShapes,
    registerReusableCarriers,
    registerSubsumptionEntry,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.State.Types
  ( PlanReuseState (..),
    emptyPlanReuseState,
    mapPlanReuseStats,
    planReuseStats,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Stats
  ( recordBoundaryRejected,
    recordContainmentReuseEmits,
    recordExactReuseEmits,
    recordObstructedProjection,
    recordResidualRejected,
    recordStaleRejected,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Validity
  ( ReuseValidity (..),
    reuseExactValidityMatchesRequest,
    reuseValidityRequestFromTime,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Reuse.Types
  ( CarrierReuseCandidateGroup (..),
    CarrierReuseStrategy (..),
    InstalledReuseMaterialization (..),
    PlanReuseDiagnostics (..),
    PlanReuseError (..),
    PlanReuseInvariantError (..),
    PlanReuseMiss (..),
    PlanReuseRegistrationEntry (..),
    PlanReuseRegistration (..),
    PlanReuseRequest (..),
    PlanReuseStats (..),
    ReuseValidityRequest (..),
  )

lookupReusableCarrierEntry ::
  (Ord ctx, Ord prop) =>
  CarrierAddr ctx Carrier prop ->
  PlanReuseState ctx prop ->
  Maybe (SubsumptionEntry ctx prop)
lookupReusableCarrierEntry addr state =
  lookupSubsumptionEntryByCarrier addr (prsSubsumptionIndex state)
{-# INLINE lookupReusableCarrierEntry #-}
