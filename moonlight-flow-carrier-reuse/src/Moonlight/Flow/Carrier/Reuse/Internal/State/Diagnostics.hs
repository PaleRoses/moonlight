module Moonlight.Flow.Carrier.Reuse.Internal.State.Diagnostics
  ( planReuseCarrierReuses,
    planReuseTopologyDigest,
    planReuseRegisteredCarriers,
    planReuseTargetReuseIds,
    validateSubsumptionOwnership,
    validatePlanReuseState,
    planReuseDiagnostics,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Word
  ( Word64,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
  ( CarrierReuse,
    CarrierReuseId,
    carrierReuseId,
    carrierReuseIdDigest,
    cruWitnessDeps,
    cruWitnessTopo,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Index.Materialization
  ( rmiInstalledByReuse,
    validateReuseMaterializationIndex,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Index.Reuse
  ( carrierReuseRegistryEntries,
    carrierReuseRegistryIdsForTarget,
    carrierReuseRegistrySize,
    crrReuses,
    validateCarrierReuseRegistry,
  )
import Moonlight.Differential.Index.Reverse
  ( finishInvariantErrors,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Index.Shape
  ( SubsumptionIndexInvariantError,
    siByCarrier,
    subsumptionIndexSize,
    validateSubsumptionIndex,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.State.Types
  ( PlanReuseState (..),
  )
import Moonlight.Flow.Carrier.Reuse.Types
  ( PlanReuseDiagnostics (..),
    PlanReuseInvariantError (..),
  )
import Moonlight.Flow.Internal.Digest
  ( wordOfInt,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
    stableDigest128,
    stableDigestWords,
  )
import Moonlight.Flow.Model.Schema.Digest.Words
  ( digestIntSetWords,
  )

planReuseCarrierReuses ::
  PlanReuseState ctx prop ->
  [(CarrierReuseId ctx prop, CarrierReuse ctx prop)]
planReuseCarrierReuses =
  carrierReuseRegistryEntries . prsReuseRegistry

planReuseTopologyDigest ::
  PlanReuseState ctx prop ->
  StableDigest128
planReuseTopologyDigest state =
  let reuses =
        crrReuses (prsReuseRegistry state)
   in stableDigest128
        ( [0x707273746f706f, wordOfInt (Map.size reuses)]
            <> foldMap reuseWords (Map.toAscList reuses)
        )
  where
    reuseWords ::
      (CarrierReuseId ctx prop, CarrierReuse ctx prop) ->
      [Word64]
    reuseWords (reuseId, reuse) =
      [0x01]
        <> stableDigestWords (carrierReuseIdDigest reuseId)
        <> stableDigestWords (carrierReuseIdDigest (carrierReuseId reuse))
        <> digestIntSetWords 0x02 (cruWitnessDeps reuse)
        <> digestIntSetWords 0x03 (cruWitnessTopo reuse)

planReuseRegisteredCarriers ::
  PlanReuseState ctx prop ->
  Set (CarrierAddr ctx Carrier prop)
planReuseRegisteredCarriers =
  Map.keysSet . siByCarrier . prsSubsumptionIndex

planReuseTargetReuseIds ::
  (Ord ctx, Ord prop) =>
  CarrierAddr ctx Carrier prop ->
  PlanReuseState ctx prop ->
  Set (CarrierReuseId ctx prop)
planReuseTargetReuseIds target =
  carrierReuseRegistryIdsForTarget target . prsReuseRegistry

validateSubsumptionOwnership ::
  Ord ctx =>
  Ord prop =>
  PlanReuseState ctx prop ->
  Either [SubsumptionIndexInvariantError ctx prop] ()
validateSubsumptionOwnership =
  validateSubsumptionIndex . prsSubsumptionIndex

validatePlanReuseState ::
  (Ord ctx, Ord prop) =>
  PlanReuseState ctx prop ->
  Either [PlanReuseInvariantError ctx prop] ()
validatePlanReuseState state =
  finishInvariantErrors $
    reuseRegistryErrors <> subsumptionErrors <> materializationErrors
  where
    reuseRegistryErrors =
      case validateCarrierReuseRegistry (prsReuseRegistry state) of
        Right () ->
          []
        Left errors ->
          fmap PlanReuseReuseRegistryInvariant errors
    subsumptionErrors =
      case validateSubsumptionOwnership state of
        Right () ->
          []
        Left errors ->
          fmap PlanReuseSubsumptionInvariant errors
    materializationErrors =
      case validateReuseMaterializationIndex (prsMaterializations state) of
        Right () ->
          []
        Left errors ->
          fmap PlanReuseMaterializationInvariant errors

planReuseDiagnostics :: PlanReuseState ctx prop -> PlanReuseDiagnostics
planReuseDiagnostics state =
  PlanReuseDiagnostics
    { prdStats = prsStats state,
      prdRegisteredShapes = subsumptionIndexSize (prsSubsumptionIndex state),
      prdRegisteredReuses = carrierReuseRegistrySize (prsReuseRegistry state),
      prdInstalledMaterializations = Map.size (rmiInstalledByReuse (prsMaterializations state))
    }
