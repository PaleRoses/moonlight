module Moonlight.Flow.Carrier.Reuse.Internal.State.Materialization
  ( installCarrierReuse,
    installPlanReuseMaterialization,
    planReuseInstalledMaterializations,
    selectStaleInstalledReuseMaterializations,
    removePlanReuseInstalledMaterialization,
  )
where

import Data.Map.Strict qualified as Map
import Moonlight.Flow.Carrier.Morphism.Subsumption
  ( CarrierReuseId,
    carrierReuseExpectedTarget,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Index.Materialization
  ( InstalledReuseMaterialization (..),
    removeInstalledReuseMaterialization,
    rmiInstalledByReuse,
    upsertInstalledReuseMaterialization,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Index.Materialization qualified as MaterializationIndex
import Moonlight.Flow.Carrier.Reuse.Internal.Index.Reuse
  ( lookupCarrierReuseRegistry,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.State.Types
  ( PlanReuseState (..),
  )
import Moonlight.Flow.Carrier.Reuse.Types
  ( PlanReuseError (..),
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope,
  )

installCarrierReuse ::
  (Ord ctx, Ord prop) =>
  InstalledReuseMaterialization ctx prop ->
  PlanReuseState ctx prop ->
  Either (PlanReuseError ctx prop) (RowDelta, PlanReuseState ctx prop)
installCarrierReuse installed state =
  case lookupCarrierReuseRegistry (irmReuseId installed) (prsReuseRegistry state) of
    Nothing ->
      Left (ReuseInstallUnknownReuse (irmReuseId installed))
    Just reuse ->
      case carrierReuseExpectedTarget reuse of
        Nothing ->
          Left (ReuseInstallObstructedReuse (irmReuseId installed))
        Just expectedTarget
          | expectedTarget /= irmTarget installed ->
              Left
                ( ReuseInstallTargetMismatch
                    (irmReuseId installed)
                    expectedTarget
                    (irmTarget installed)
                )
          | otherwise ->
              let (deltaRows, materializations') =
                    upsertInstalledReuseMaterialization
                      installed
                      (prsMaterializations state)
               in Right
                    ( deltaRows,
                      state {prsMaterializations = materializations'}
                    )

installPlanReuseMaterialization ::
  (Ord ctx, Ord prop) =>
  InstalledReuseMaterialization ctx prop ->
  PlanReuseState ctx prop ->
  (RowDelta, PlanReuseState ctx prop)
installPlanReuseMaterialization installed state =
  let (deltaRows, materializations') =
        upsertInstalledReuseMaterialization
          installed
          (prsMaterializations state)
   in ( deltaRows,
        state {prsMaterializations = materializations'}
      )

planReuseInstalledMaterializations ::
  PlanReuseState ctx prop ->
  [(CarrierReuseId ctx prop, InstalledReuseMaterialization ctx prop)]
planReuseInstalledMaterializations =
  Map.toAscList . rmiInstalledByReuse . prsMaterializations

selectStaleInstalledReuseMaterializations ::
  (Ord ctx, Ord prop) =>
  RelationalScope ->
  PlanReuseState ctx prop ->
  [(CarrierReuseId ctx prop, InstalledReuseMaterialization ctx prop)]
selectStaleInstalledReuseMaterializations dirty =
  MaterializationIndex.selectStaleInstalledReuseMaterializations dirty . prsMaterializations

removePlanReuseInstalledMaterialization ::
  (Ord ctx, Ord prop) =>
  CarrierReuseId ctx prop ->
  PlanReuseState ctx prop ->
  (Maybe (InstalledReuseMaterialization ctx prop), PlanReuseState ctx prop)
removePlanReuseInstalledMaterialization reuseId state =
  let (installed, materializations') =
        removeInstalledReuseMaterialization reuseId (prsMaterializations state)
   in (installed, state {prsMaterializations = materializations'})
