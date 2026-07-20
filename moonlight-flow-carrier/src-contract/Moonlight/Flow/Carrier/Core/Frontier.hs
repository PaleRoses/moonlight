{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.Core.Frontier
  ( RelDiffFrontier,
    RelDiffAntichain,
    RelDiffCapability,
    emptyRelDiffFrontier,
    relDiffCapabilityFrontier,
    downgradeRelDiffCapability,
    relDiffFrontierVisibleAntichain,
    relDiffFrontierVisibleAntichainForContext,
    relDiffFrontierVisibleAntichainForScope,
    relDiffFrontierCutoffForScope,
    relDiffFrontierPendingAntichain,
    relDiffFrontierReferencedTraceKeys,
    relDiffFrontierPendingBeforeVisibleMin,
    relDiffFrontierCompactsTime,
  )
where

import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.Set
  ( Set,
  )
import Moonlight.Core
  ( PartialOrder,
  )
import Moonlight.Differential.Frontier
  ( RuntimeFrontier,
    RuntimeAntichain,
    RuntimeCapability,
    RuntimeInvalidCapabilityAdvance,
    downgradeRuntimeCapability,
    emptyRuntimeFrontier,
    emptyTraceRetention,
    frontierCutoffForScope,
    frontierPendingAntichain,
    frontierPendingBeforeVisibleMinimum,
    frontierTraceRetention,
    frontierTimeCompactable,
    frontierWithTraceRetention,
    frontierVisibleAntichain,
    frontierVisibleAntichainForContext,
    frontierVisibleAntichainForScope,
    runtimeCapabilityFrontier,
    traceRetentionReferencedKeys,
  )
import Moonlight.Differential.Time
  ( RuntimeScope,
    RuntimeTime,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalRuntimeEpoch,
  )

type RelDiffFrontier ctx phase =
  RuntimeFrontier ctx RelationalRuntimeEpoch phase

type RelDiffAntichain ctx phase =
  RuntimeAntichain ctx RelationalRuntimeEpoch phase

type RelDiffCapability ctx phase =
  RuntimeCapability ctx RelationalRuntimeEpoch phase

emptyRelDiffFrontier :: RelDiffFrontier ctx phase
emptyRelDiffFrontier =
  frontierWithTraceRetention (Just emptyTraceRetention) emptyRuntimeFrontier
{-# INLINE emptyRelDiffFrontier #-}

relDiffCapabilityFrontier ::
  RelDiffCapability ctx phase ->
  RelDiffAntichain ctx phase
relDiffCapabilityFrontier =
  runtimeCapabilityFrontier
{-# INLINE relDiffCapabilityFrontier #-}

downgradeRelDiffCapability ::
  (Eq ctx, PartialOrder phase) =>
  RuntimeTime ctx RelationalRuntimeEpoch phase ->
  RelDiffCapability ctx phase ->
  Either (RuntimeInvalidCapabilityAdvance ctx RelationalRuntimeEpoch phase) (RelDiffCapability ctx phase)
downgradeRelDiffCapability =
  downgradeRuntimeCapability
{-# INLINE downgradeRelDiffCapability #-}

relDiffFrontierVisibleAntichain ::
  (Ord ctx, Ord phase, PartialOrder phase) =>
  RelDiffFrontier ctx phase ->
  RelDiffAntichain ctx phase
relDiffFrontierVisibleAntichain =
  frontierVisibleAntichain
{-# INLINE relDiffFrontierVisibleAntichain #-}

relDiffFrontierVisibleAntichainForContext ::
  Ord ctx =>
  ctx ->
  RelDiffFrontier ctx phase ->
  RelDiffAntichain ctx phase
relDiffFrontierVisibleAntichainForContext =
  frontierVisibleAntichainForContext
{-# INLINE relDiffFrontierVisibleAntichainForContext #-}

relDiffFrontierVisibleAntichainForScope ::
  Ord ctx =>
  ctx ->
  RuntimeScope ->
  RelDiffFrontier ctx phase ->
  RelDiffAntichain ctx phase
relDiffFrontierVisibleAntichainForScope =
  frontierVisibleAntichainForScope
{-# INLINE relDiffFrontierVisibleAntichainForScope #-}

relDiffFrontierCutoffForScope ::
  Ord ctx =>
  ctx ->
  RuntimeScope ->
  RelDiffFrontier ctx phase ->
  RelDiffAntichain ctx phase
relDiffFrontierCutoffForScope =
  frontierCutoffForScope
{-# INLINE relDiffFrontierCutoffForScope #-}

relDiffFrontierPendingAntichain ::
  (Ord ctx, Ord phase, PartialOrder phase) =>
  RelDiffFrontier ctx phase ->
  RelDiffAntichain ctx phase
relDiffFrontierPendingAntichain =
  frontierPendingAntichain
{-# INLINE relDiffFrontierPendingAntichain #-}

relDiffFrontierReferencedTraceKeys ::
  RelDiffFrontier ctx phase ->
  IntSet
relDiffFrontierReferencedTraceKeys frontier =
  case frontierTraceRetention frontier of
    Nothing ->
      IntSet.empty
    Just retention ->
      traceRetentionReferencedKeys retention
{-# INLINE relDiffFrontierReferencedTraceKeys #-}

relDiffFrontierPendingBeforeVisibleMin ::
  (Ord ctx, PartialOrder phase) =>
  RelDiffFrontier ctx phase ->
  Set (RuntimeTime ctx RelationalRuntimeEpoch phase)
relDiffFrontierPendingBeforeVisibleMin =
  frontierPendingBeforeVisibleMinimum
{-# INLINE relDiffFrontierPendingBeforeVisibleMin #-}

relDiffFrontierCompactsTime ::
  (Ord ctx, PartialOrder phase) =>
  RelDiffFrontier ctx phase ->
  RuntimeTime ctx RelationalRuntimeEpoch phase ->
  Bool
relDiffFrontierCompactsTime =
  frontierTimeCompactable
{-# INLINE relDiffFrontierCompactsTime #-}
