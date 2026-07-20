{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.Reuse.Internal.Invalidation
  ( PlanReuseInvalidationPostconditionError (..),
    dropPlanReuseCarrierReuseState,
    invalidateCarrierReusesByDepsTopo,
    invalidatePlanReuseState,
    invalidatePlanReuseByPatch,
    validatePlanReuseInvalidationPostcondition,
  )
where

import Data.IntSet
  ( IntSet,
  )
import Data.Set qualified as Set
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
  ( CarrierReuseId,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Index.Materialization
  ( dropInstalledReuseMaterializationsForCarrier,
    dropInstalledReuseMaterializationsForReuses,
    selectStaleInstalledReuseMaterializations,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Registry
  ( dropCarrierReuse,
    dropCarrierReusesForCarrier,
    scrReuseId,
    selectStaleCarrierReuses,
  )
import Moonlight.Flow.Carrier.Reuse.Internal.State.Types
  ( PlanReuseState (..),
  )
import Moonlight.Flow.Carrier.Reuse.Internal.Stats
  ( recordStaleRejected,
  )
import Moonlight.Flow.Model.Delta
  ( QuotientPatch (..)
  )
import Moonlight.Flow.Model.Scope
  ( DepsDelta (..),
    RelationalScope (..),
    TopoDelta (..),
    scopeDeps,
    scopeTopo,
  )

data PlanReuseInvalidationPostconditionError ctx prop
  = PlanReuseInvalidationDirtyReuseSurvived !(CarrierReuseId ctx prop)
  | PlanReuseInvalidationDirtyMaterializationSurvived !(CarrierReuseId ctx prop)
  deriving stock (Eq, Show)

dropPlanReuseCarrierReuseState ::
  (Ord ctx, Ord prop) =>
  CarrierAddr ctx Carrier prop ->
  PlanReuseState ctx prop ->
  PlanReuseState ctx prop
dropPlanReuseCarrierReuseState addr state0 =
  let state1 =
        dropCarrierReusesForCarrier addr state0
   in state1
        { prsMaterializations =
            dropInstalledReuseMaterializationsForCarrier addr (prsMaterializations state1)
        }
{-# INLINE dropPlanReuseCarrierReuseState #-}

invalidateCarrierReusesByDepsTopo ::
  (Ord ctx, Ord prop) =>
  IntSet ->
  IntSet ->
  PlanReuseState ctx prop ->
  (PlanReuseState ctx prop, [CarrierReuseId ctx prop])
invalidateCarrierReusesByDepsTopo dirtyDeps dirtyTopo state =
  let dirty =
        mempty {rsDeps = DepsDelta dirtyDeps, rsTopo = TopoDelta dirtyTopo}
      stale =
        selectStaleCarrierReuses dirty state
      staleKeys =
        Set.fromList (fmap scrReuseId stale)
      stateAfterReuses =
        Set.foldl'
          (flip dropCarrierReuse)
          state
          staleKeys
   in ( stateAfterReuses
          { prsStats =
              recordStaleRejected (Set.size staleKeys) (prsStats stateAfterReuses)
          },
        Set.toAscList staleKeys
      )
{-# INLINE invalidateCarrierReusesByDepsTopo #-}

invalidatePlanReuseByPatch ::
  (Ord ctx, Ord prop) =>
  QuotientPatch ->
  PlanReuseState ctx prop ->
  PlanReuseState ctx prop
invalidatePlanReuseByPatch patch =
  invalidatePlanReuseState
    (scopeDeps (qpScope patch))
    (scopeTopo (qpScope patch))
{-# INLINE invalidatePlanReuseByPatch #-}

invalidatePlanReuseState ::
  (Ord ctx, Ord prop) =>
  IntSet ->
  IntSet ->
  PlanReuseState ctx prop ->
  PlanReuseState ctx prop
invalidatePlanReuseState dirtyDeps dirtyTopo state =
  let dirty =
        mempty {rsDeps = DepsDelta dirtyDeps, rsTopo = TopoDelta dirtyTopo}
      (stateAfterReuses, _staleReuseKeys) =
        invalidateCarrierReusesByDepsTopo
          dirtyDeps
          dirtyTopo
          state
      staleInstalledKeys =
        Set.fromList
          [ reuseId
          | (reuseId, _installed) <-
              selectStaleInstalledReuseMaterializations
                dirty
                (prsMaterializations stateAfterReuses)
          ]
      stateAfterInstalled =
        stateAfterReuses
          { prsMaterializations =
              dropInstalledReuseMaterializationsForReuses
                staleInstalledKeys
                (prsMaterializations stateAfterReuses)
          }
   in stateAfterInstalled
        { prsStats =
            recordStaleRejected (Set.size staleInstalledKeys) (prsStats stateAfterInstalled)
        }
{-# INLINE invalidatePlanReuseState #-}

validatePlanReuseInvalidationPostcondition ::
  (Ord ctx, Ord prop) =>
  IntSet ->
  IntSet ->
  PlanReuseState ctx prop ->
  Either [PlanReuseInvalidationPostconditionError ctx prop] ()
validatePlanReuseInvalidationPostcondition dirtyDeps dirtyTopo state =
  let dirty =
        mempty {rsDeps = DepsDelta dirtyDeps, rsTopo = TopoDelta dirtyTopo}
      reuseErrors =
        [ PlanReuseInvalidationDirtyReuseSurvived (scrReuseId stale)
        | stale <- selectStaleCarrierReuses dirty state
        ]
      materializationErrors =
        [ PlanReuseInvalidationDirtyMaterializationSurvived reuseId
        | (reuseId, _installed) <-
            selectStaleInstalledReuseMaterializations dirty (prsMaterializations state)
        ]
      errors =
        reuseErrors <> materializationErrors
   in case errors of
        [] ->
          Right ()
        _ ->
          Left errors
{-# INLINE validatePlanReuseInvalidationPostcondition #-}
