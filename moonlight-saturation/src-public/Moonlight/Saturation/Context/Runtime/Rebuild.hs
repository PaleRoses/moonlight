{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Runtime.Rebuild
  ( rebuildRuntimeCore,
    rebuildRuntimeState,
  )
where

import Moonlight.Saturation.Context.Runtime.Policy.Internal
  ( CarrierAccess (..),
  )
import Moonlight.Saturation.Context.Runtime.State
  ( RuntimeCore (..),
    RuntimeState (..),
    runtimeCoreFactDerivationsAt,
    runtimeCoreFactInputsAt,
    runtimeCoreFactsAt,
  )
import Moonlight.Saturation.Substrate

rebuildRuntimeCore ::
  forall u schedulerGroup.
  (RebuildSystem u, Ord (SatContext u)) =>
  SatGraph u ->
  RuntimeCore u schedulerGroup ->
  Either (SatObstruction u) (RuntimeCore u schedulerGroup, SatGraph u, SatRebuild u)
rebuildRuntimeCore graph core = do
  let baseContext =
        graphBaseContext @u graph
      baseFacts =
        unionFactStores @u
          (runtimeCoreFactsAt @u baseContext core)
          (runtimeCoreFactInputsAt @u baseContext core)
      baseFactDerivations =
        runtimeCoreFactDerivationsAt @u baseContext core

  (rebuiltGraph, rebuildReport) <-
    rebuildGraph @u
      graph
      baseFacts
      baseFactDerivations

  pure
    ( core
        { rcContextRevision = rebuildEpoch @u rebuildReport
        },
      rebuiltGraph,
      rebuildReport
    )

rebuildRuntimeState ::
  forall u carrier schedulerState.
  (RebuildSystem u, Ord (SatContext u)) =>
  CarrierAccess u carrier ->
  RuntimeState u carrier schedulerState ->
  Either
    (SatObstruction u)
    (RuntimeState u carrier schedulerState, SatRebuild u)
rebuildRuntimeState carrierOps state = do
  let carrier =
        rsCarrier state
      graph =
        caGraph carrierOps carrier

  (rebuiltCore, rebuiltGraph, rebuildReport) <-
    rebuildRuntimeCore @u graph (rsCore state)

  let carrierWithGraph =
        caSetGraph carrierOps rebuiltGraph carrier

  pure
    ( state
        { rsCore = rebuiltCore,
          rsCarrier = carrierWithGraph
        },
      rebuildReport
    )
