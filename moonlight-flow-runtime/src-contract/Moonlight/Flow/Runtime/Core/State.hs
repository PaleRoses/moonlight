{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Moonlight.Flow.Runtime.Core.State
  ( RuntimeClockState (..),
    RuntimeSeedState (..),
    RuntimeState (..),
    initialRuntimeClockState,
    emptyRuntimeSeedState,
    runtimeSeedStateFromPatch,
    runtimeSeedStateSettled,
    rsQuotientEpoch,
    rsLiveEpoch,
    rsNextFrontierStamp,
    setRuntimeClockState,
    mapRuntimeClockState,
    setRuntimeSeedState,
    mapRuntimeTopologySection,
    mapRuntimeEngineSection,
    mapRuntimeCarrierSection,
    mapRuntimeFactorSection,
  )
where

import Data.Kind
  ( Type,
  )
import Moonlight.Core
  ( LiveEpoch,
    QuotientEpoch,
    initialLiveEpoch,
    initialQuotientEpoch,
  )
import Moonlight.Differential.Time
  ( FrontierStamp,
    initialFrontierStamp,
  )
import Moonlight.Flow.Runtime.Core.Patch.Internal
  ( Patch,
    patchNull,
  )

type RuntimeClockState :: Type
data RuntimeClockState = RuntimeClockState
  { rcsQuotientEpoch :: !QuotientEpoch,
    rcsLiveEpoch :: !LiveEpoch,
    rcsNextFrontierStamp :: !FrontierStamp
  }
  deriving stock (Eq, Show)

initialRuntimeClockState :: RuntimeClockState
initialRuntimeClockState =
  RuntimeClockState
    { rcsQuotientEpoch = initialQuotientEpoch,
      rcsLiveEpoch = initialLiveEpoch,
      rcsNextFrontierStamp = initialFrontierStamp
    }
{-# INLINE initialRuntimeClockState #-}

data RuntimeSeedState
  = RuntimeSeedSettled
  | RuntimeSeedPending !Patch
  deriving stock (Eq, Show)

emptyRuntimeSeedState :: RuntimeSeedState
emptyRuntimeSeedState =
  RuntimeSeedSettled
{-# INLINE emptyRuntimeSeedState #-}

runtimeSeedStateFromPatch :: Patch -> RuntimeSeedState
runtimeSeedStateFromPatch patch
  | patchNull patch =
      RuntimeSeedSettled
  | otherwise =
      RuntimeSeedPending patch
{-# INLINE runtimeSeedStateFromPatch #-}

runtimeSeedStateSettled :: RuntimeSeedState -> Bool
runtimeSeedStateSettled seedState =
  case seedState of
    RuntimeSeedSettled ->
      True
    RuntimeSeedPending _ ->
      False
{-# INLINE runtimeSeedStateSettled #-}

type RuntimeState :: Type -> Type -> Type -> Type -> Type
data RuntimeState topology engine carrier factor = RuntimeState
  { rsClock :: !RuntimeClockState,
    rsSeedState :: !RuntimeSeedState,
    rsTopology :: !topology,
    rsEngine :: !engine,
    rsCarrier :: !carrier,
    rsFactor :: !factor
  }

rsQuotientEpoch :: RuntimeState topology engine carrier factor -> QuotientEpoch
rsQuotientEpoch =
  rcsQuotientEpoch . rsClock
{-# INLINE rsQuotientEpoch #-}

rsLiveEpoch :: RuntimeState topology engine carrier factor -> LiveEpoch
rsLiveEpoch =
  rcsLiveEpoch . rsClock
{-# INLINE rsLiveEpoch #-}

rsNextFrontierStamp :: RuntimeState topology engine carrier factor -> FrontierStamp
rsNextFrontierStamp =
  rcsNextFrontierStamp . rsClock
{-# INLINE rsNextFrontierStamp #-}

setRuntimeClockState ::
  RuntimeClockState ->
  RuntimeState topology engine carrier factor ->
  RuntimeState topology engine carrier factor
setRuntimeClockState clockState state =
  state {rsClock = clockState}
{-# INLINE setRuntimeClockState #-}

mapRuntimeClockState ::
  (RuntimeClockState -> RuntimeClockState) ->
  RuntimeState topology engine carrier factor ->
  RuntimeState topology engine carrier factor
mapRuntimeClockState update state =
  state {rsClock = update (rsClock state)}
{-# INLINE mapRuntimeClockState #-}

setRuntimeSeedState ::
  RuntimeSeedState ->
  RuntimeState topology engine carrier factor ->
  RuntimeState topology engine carrier factor
setRuntimeSeedState seedState state =
  state {rsSeedState = seedState}
{-# INLINE setRuntimeSeedState #-}

mapRuntimeTopologySection ::
  (topology -> topology') ->
  RuntimeState topology engine carrier factor ->
  RuntimeState topology' engine carrier factor
mapRuntimeTopologySection update state =
  state {rsTopology = update (rsTopology state)}
{-# INLINE mapRuntimeTopologySection #-}

mapRuntimeEngineSection ::
  (engine -> engine') ->
  RuntimeState topology engine carrier factor ->
  RuntimeState topology engine' carrier factor
mapRuntimeEngineSection update state =
  state {rsEngine = update (rsEngine state)}
{-# INLINE mapRuntimeEngineSection #-}

mapRuntimeCarrierSection ::
  (carrier -> carrier') ->
  RuntimeState topology engine carrier factor ->
  RuntimeState topology engine carrier' factor
mapRuntimeCarrierSection update state =
  state {rsCarrier = update (rsCarrier state)}
{-# INLINE mapRuntimeCarrierSection #-}

mapRuntimeFactorSection ::
  (factor -> factor') ->
  RuntimeState topology engine carrier factor ->
  RuntimeState topology engine carrier factor'
mapRuntimeFactorSection update state =
  state {rsFactor = update (rsFactor state)}
{-# INLINE mapRuntimeFactorSection #-}
