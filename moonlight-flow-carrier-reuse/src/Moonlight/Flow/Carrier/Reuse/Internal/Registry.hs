{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.Reuse.Internal.Registry
  ( StaleCarrierReuse (..),
    lookupCarrierReuse,
    lookupCarrierReuseIdsForCarrier,
    lookupCarrierReusesForCarrier,
    registerCarrierReuse,
    registerCarrierReuses,
    dropCarrierReuse,
    dropCarrierReusesForCarrier,
    selectStaleCarrierReuses,
    dropSelectedCarrierReuses,
    carrierReuseStale,
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
  ( CarrierReuse (..),
    CarrierReuseId,
    ReuseWitness (..),
    carrierReuseExpectedTarget,
    carrierReuseId,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Index.Materialization
  ( dropInstalledReuseMaterialization,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Index.Reuse
  ( carrierReuseRegistryEntries,
    carrierReuseRegistryIdsForCarrier,
    carrierReuseRegistryStaleEntries,
    carrierReuseStale,
    deleteCarrierReuseRegistry,
    insertCarrierReuseRegistry,
    lookupCarrierReuseRegistry,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.State.Types
  ( PlanReuseState (..),
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope,
    scopeDeps,
    scopeTopo,
  )

data StaleCarrierReuse ctx prop = StaleCarrierReuse
  { scrReuseId :: !(CarrierReuseId ctx prop),
    scrReuse :: !(CarrierReuse ctx prop),
    scrSource :: !(CarrierAddr ctx Carrier prop),
    scrExpectedTarget :: !(Maybe (CarrierAddr ctx Carrier prop))
  }
  deriving stock (Eq, Show)

lookupCarrierReuse ::
  (Ord ctx, Ord prop) =>
  CarrierReuseId ctx prop ->
  PlanReuseState ctx prop ->
  Maybe (CarrierReuse ctx prop)
lookupCarrierReuse reuseId =
  lookupCarrierReuseRegistry reuseId . prsReuseRegistry
{-# INLINE lookupCarrierReuse #-}

lookupCarrierReuseIdsForCarrier ::
  (Ord ctx, Ord prop) =>
  CarrierAddr ctx Carrier prop ->
  PlanReuseState ctx prop ->
  Set (CarrierReuseId ctx prop)
lookupCarrierReuseIdsForCarrier addr =
  carrierReuseRegistryIdsForCarrier addr . prsReuseRegistry
{-# INLINE lookupCarrierReuseIdsForCarrier #-}

lookupCarrierReusesForCarrier ::
  (Ord ctx, Ord prop) =>
  CarrierAddr ctx Carrier prop ->
  PlanReuseState ctx prop ->
  [(CarrierReuseId ctx prop, CarrierReuse ctx prop)]
lookupCarrierReusesForCarrier addr state =
  [ (reuseId, reuse)
  | (reuseId, reuse) <- carrierReuseRegistryEntries (prsReuseRegistry state),
    Set.member reuseId (lookupCarrierReuseIdsForCarrier addr state)
  ]
{-# INLINE lookupCarrierReusesForCarrier #-}

registerCarrierReuse ::
  (Ord ctx, Ord prop) =>
  CarrierReuse ctx prop ->
  PlanReuseState ctx prop ->
  PlanReuseState ctx prop
registerCarrierReuse reuse state0 =
  let reuseId =
        carrierReuseId reuse
      state1 =
        dropCarrierReuse reuseId state0
   in state1
        { prsReuseRegistry =
            insertCarrierReuseRegistry reuse (prsReuseRegistry state1)
        }
{-# INLINE registerCarrierReuse #-}

registerCarrierReuses ::
  (Ord ctx, Ord prop, Foldable f) =>
  f (CarrierReuse ctx prop) ->
  PlanReuseState ctx prop ->
  PlanReuseState ctx prop
registerCarrierReuses reuses state0 =
  foldl'
    ( \state reuse ->
        registerCarrierReuse reuse state
    )
    state0
    reuses
{-# INLINE registerCarrierReuses #-}

dropCarrierReuse ::
  (Ord ctx, Ord prop) =>
  CarrierReuseId ctx prop ->
  PlanReuseState ctx prop ->
  PlanReuseState ctx prop
dropCarrierReuse reuseId state =
  let (maybeReuse, registry') =
        deleteCarrierReuseRegistry reuseId (prsReuseRegistry state)
   in case maybeReuse of
        Nothing ->
          state
        Just _reuse ->
          state
            { prsReuseRegistry = registry',
              prsMaterializations =
                dropInstalledReuseMaterialization reuseId (prsMaterializations state)
            }
{-# INLINE dropCarrierReuse #-}

dropCarrierReusesForCarrier ::
  (Ord ctx, Ord prop) =>
  CarrierAddr ctx Carrier prop ->
  PlanReuseState ctx prop ->
  PlanReuseState ctx prop
dropCarrierReusesForCarrier addr state =
  Set.foldl'
    (flip dropCarrierReuse)
    state
    (lookupCarrierReuseIdsForCarrier addr state)
{-# INLINE dropCarrierReusesForCarrier #-}

selectStaleCarrierReuses ::
  (Ord ctx, Ord prop) =>
  RelationalScope ->
  PlanReuseState ctx prop ->
  [StaleCarrierReuse ctx prop]
selectStaleCarrierReuses dirty state =
  [ StaleCarrierReuse
      { scrReuseId = reuseId,
        scrReuse = reuse,
        scrSource = cruSourceCarrier reuse,
        scrExpectedTarget = carrierReuseExpectedTarget reuse
      }
  | (reuseId, reuse) <-
      carrierReuseRegistryStaleEntries
        (scopeDeps dirty)
        (scopeTopo dirty)
        (prsReuseRegistry state)
  ]
{-# INLINE selectStaleCarrierReuses #-}

dropSelectedCarrierReuses ::
  (Ord ctx, Ord prop, Foldable f) =>
  f (CarrierReuseId ctx prop) ->
  PlanReuseState ctx prop ->
  PlanReuseState ctx prop
dropSelectedCarrierReuses reuseIds state0 =
  foldl'
    ( \state reuseId ->
        dropCarrierReuse reuseId state
    )
    state0
    reuseIds
{-# INLINE dropSelectedCarrierReuses #-}

cruSourceCarrier ::
  CarrierReuse ctx prop ->
  CarrierAddr ctx Carrier prop
cruSourceCarrier =
  rwSourceCarrier . cruWitness
{-# INLINE cruSourceCarrier #-}
