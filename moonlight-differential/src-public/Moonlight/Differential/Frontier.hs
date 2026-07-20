{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Frontier
  ( TraceRetention,
    traceRetention,
    emptyTraceRetention,
    traceRetentionPinnedTraceIds,
    traceRetentionExactEvidenceTraceIds,
    traceRetentionProvenanceTraceIds,
    traceRetentionReferencedKeys,
    RuntimeFrontierKey,
    runtimeFrontierKey,
    runtimeFrontierKeyContext,
    runtimeFrontierKeyScope,
    runtimeFrontierKeyForTime,
    RuntimeFrontier,
    RuntimeAntichain,
    RuntimeCapability,
    RuntimeInvalidCapabilityAdvance (..),
    RuntimeFrontierError (..),
    runtimeCapabilityTime,
    mintRootRuntimeCapability,
    runtimeCapabilityFrontier,
    downgradeRuntimeCapability,
    emptyRuntimeFrontier,
    frontierCutoffForContext,
    frontierCutoffForScope,
    frontierCutoffForTime,
    frontierVisibleAntichain,
    frontierVisibleAntichainForContext,
    frontierVisibleAntichainForScope,
    frontierVisibleMinimums,
    frontierPendingCounts,
    frontierWithPendingCounts,
    frontierTraceRetention,
    frontierWithTraceRetention,
    frontierPendingPointstamps,
    frontierPendingInScope,
    frontierPendingAntichain,
    frontierPendingCount,
    frontierInsertPending,
    frontierDeletePending,
    frontierCompletePending,
    frontierAdvanceVisibleMin,
    frontierHasPendingAtOrBefore,
    frontierPendingBeforeVisibleMinimum,
    frontierTimeCompactable,
  )
where

import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Delta.Frontier
  ( UpperFrontier,
    emptyUpperFrontier,
    mkUpperFrontier,
    singletonUpperFrontier,
    upperFrontierPoints,
  )
import Moonlight.Differential.Time
  ( RuntimeScope,
    RuntimeTime,
    emptyRuntimeScope,
    isDescendantOf,
    rtContext,
    rtScope,
    runtimeTimeSameScopeLeq,
    runtimeTimeSameScopeLt,
  )
import Moonlight.Core
  ( PartialOrder (..),
  )

type TraceRetention :: Type
data TraceRetention = TraceRetention
  { trPinnedTraceIds :: !IntSet,
    trExactEvidenceTraceIds :: !IntSet,
    trProvenanceTraceIds :: !IntSet
  }
  deriving stock (Eq, Ord, Show)

traceRetention :: IntSet -> IntSet -> IntSet -> TraceRetention
traceRetention pinnedTraceIds exactEvidenceTraceIds provenanceTraceIds =
  TraceRetention
    { trPinnedTraceIds = pinnedTraceIds,
      trExactEvidenceTraceIds = exactEvidenceTraceIds,
      trProvenanceTraceIds = provenanceTraceIds
    }
{-# INLINE traceRetention #-}

emptyTraceRetention :: TraceRetention
emptyTraceRetention =
  TraceRetention
    { trPinnedTraceIds = IntSet.empty,
      trExactEvidenceTraceIds = IntSet.empty,
      trProvenanceTraceIds = IntSet.empty
    }
{-# INLINE emptyTraceRetention #-}

traceRetentionReferencedKeys :: TraceRetention -> IntSet
traceRetentionReferencedKeys retention =
  IntSet.unions
    [ trPinnedTraceIds retention,
      trExactEvidenceTraceIds retention,
      trProvenanceTraceIds retention
    ]
{-# INLINE traceRetentionReferencedKeys #-}

traceRetentionPinnedTraceIds :: TraceRetention -> IntSet
traceRetentionPinnedTraceIds =
  trPinnedTraceIds
{-# INLINE traceRetentionPinnedTraceIds #-}

traceRetentionExactEvidenceTraceIds :: TraceRetention -> IntSet
traceRetentionExactEvidenceTraceIds =
  trExactEvidenceTraceIds
{-# INLINE traceRetentionExactEvidenceTraceIds #-}

traceRetentionProvenanceTraceIds :: TraceRetention -> IntSet
traceRetentionProvenanceTraceIds =
  trProvenanceTraceIds
{-# INLINE traceRetentionProvenanceTraceIds #-}

type RuntimeFrontierKey :: Type -> Type
data RuntimeFrontierKey ctx = RuntimeFrontierKey
  { rfkContext :: !ctx,
    rfkScope :: !RuntimeScope
  }
  deriving stock (Eq, Ord, Show)

runtimeFrontierKey :: ctx -> RuntimeScope -> RuntimeFrontierKey ctx
runtimeFrontierKey contextValue scopeValue =
  RuntimeFrontierKey
    { rfkContext = contextValue,
      rfkScope = scopeValue
    }
{-# INLINE runtimeFrontierKey #-}

runtimeFrontierKeyContext :: RuntimeFrontierKey ctx -> ctx
runtimeFrontierKeyContext =
  rfkContext
{-# INLINE runtimeFrontierKeyContext #-}

runtimeFrontierKeyScope :: RuntimeFrontierKey ctx -> RuntimeScope
runtimeFrontierKeyScope =
  rfkScope
{-# INLINE runtimeFrontierKeyScope #-}

runtimeFrontierKeyForTime ::
  RuntimeTime ctx epoch phase ->
  RuntimeFrontierKey ctx
runtimeFrontierKeyForTime timeValue =
  runtimeFrontierKey (rtContext timeValue) (rtScope timeValue)
{-# INLINE runtimeFrontierKeyForTime #-}

type RuntimeFrontier :: Type -> Type -> Type -> Type
data RuntimeFrontier ctx epoch phase = RuntimeFrontier
  { rfVisibleMin :: !(Map (RuntimeFrontierKey ctx) (RuntimeAntichain ctx epoch phase)),
    rfPending :: !(Map (RuntimeTime ctx epoch phase) Int),
    rfTraceRetention :: !(Maybe TraceRetention)
  }
  deriving stock (Eq, Show)

type RuntimeAntichain :: Type -> Type -> Type -> Type
type RuntimeAntichain ctx epoch phase =
  UpperFrontier (RuntimeTime ctx epoch phase)

type RuntimeCapability :: Type -> Type -> Type -> Type
newtype RuntimeCapability ctx epoch phase = RuntimeCapability
  { runtimeCapabilityTimeRaw :: RuntimeTime ctx epoch phase
  }
  deriving stock (Eq, Ord, Show)

runtimeCapabilityTime ::
  RuntimeCapability ctx epoch phase ->
  RuntimeTime ctx epoch phase
runtimeCapabilityTime =
  runtimeCapabilityTimeRaw
{-# INLINE runtimeCapabilityTime #-}

type RuntimeInvalidCapabilityAdvance :: Type -> Type -> Type -> Type
data RuntimeInvalidCapabilityAdvance ctx epoch phase = RuntimeInvalidCapabilityAdvance
  { ricaSourceTime :: !(RuntimeTime ctx epoch phase),
    ricaTargetTime :: !(RuntimeTime ctx epoch phase)
  }
  deriving stock (Eq, Ord, Show)

type RuntimeFrontierError :: Type -> Type -> Type -> Type
data RuntimeFrontierError ctx epoch phase
  = RuntimeFrontierMissingPendingComplete !(RuntimeTime ctx epoch phase)
  deriving stock (Eq, Ord, Show)

mintRootRuntimeCapability ::
  RuntimeTime ctx epoch phase ->
  RuntimeCapability ctx epoch phase
mintRootRuntimeCapability =
  RuntimeCapability
{-# INLINE mintRootRuntimeCapability #-}

runtimeCapabilityFrontier ::
  RuntimeCapability ctx epoch phase ->
  RuntimeAntichain ctx epoch phase
runtimeCapabilityFrontier =
  singletonUpperFrontier . runtimeCapabilityTime
{-# INLINE runtimeCapabilityFrontier #-}

downgradeRuntimeCapability ::
  (Eq ctx, PartialOrder epoch, PartialOrder phase) =>
  RuntimeTime ctx epoch phase ->
  RuntimeCapability ctx epoch phase ->
  Either (RuntimeInvalidCapabilityAdvance ctx epoch phase) (RuntimeCapability ctx epoch phase)
downgradeRuntimeCapability nextTime capability
  | runtimeCapabilityTime capability `runtimeTimeSameScopeLeq` nextTime =
      Right (RuntimeCapability nextTime)
  | otherwise =
      Left
        RuntimeInvalidCapabilityAdvance
          { ricaSourceTime = runtimeCapabilityTime capability,
            ricaTargetTime = nextTime
          }
{-# INLINE downgradeRuntimeCapability #-}

emptyRuntimeFrontier :: RuntimeFrontier ctx epoch phase
emptyRuntimeFrontier =
  RuntimeFrontier
    { rfVisibleMin = Map.empty,
      rfPending = Map.empty,
      rfTraceRetention = Nothing
    }
{-# INLINE emptyRuntimeFrontier #-}

frontierCutoffForContext ::
  Ord ctx =>
  ctx ->
  RuntimeFrontier ctx epoch phase ->
  RuntimeAntichain ctx epoch phase
frontierCutoffForContext contextValue =
  Map.findWithDefault emptyUpperFrontier (runtimeFrontierKey contextValue emptyRuntimeScope) . rfVisibleMin
{-# INLINE frontierCutoffForContext #-}

frontierCutoffForScope ::
  Ord ctx =>
  ctx ->
  RuntimeScope ->
  RuntimeFrontier ctx epoch phase ->
  RuntimeAntichain ctx epoch phase
frontierCutoffForScope contextValue scopeValue =
  Map.findWithDefault emptyUpperFrontier (runtimeFrontierKey contextValue scopeValue) . rfVisibleMin
{-# INLINE frontierCutoffForScope #-}

frontierCutoffForTime ::
  Ord ctx =>
  RuntimeTime ctx epoch phase ->
  RuntimeFrontier ctx epoch phase ->
  RuntimeAntichain ctx epoch phase
frontierCutoffForTime timeValue =
  Map.findWithDefault emptyUpperFrontier (runtimeFrontierKeyForTime timeValue) . rfVisibleMin
{-# INLINE frontierCutoffForTime #-}

frontierVisibleAntichain ::
  (Ord ctx, Ord epoch, Ord phase, PartialOrder epoch, PartialOrder phase) =>
  RuntimeFrontier ctx epoch phase ->
  RuntimeAntichain ctx epoch phase
frontierVisibleAntichain =
  mkUpperFrontier . concatMap upperFrontierPoints . Map.elems . rfVisibleMin
{-# INLINE frontierVisibleAntichain #-}

frontierVisibleAntichainForContext ::
  Ord ctx =>
  ctx ->
  RuntimeFrontier ctx epoch phase ->
  RuntimeAntichain ctx epoch phase
frontierVisibleAntichainForContext =
  frontierCutoffForContext
{-# INLINE frontierVisibleAntichainForContext #-}

frontierVisibleAntichainForScope ::
  Ord ctx =>
  ctx ->
  RuntimeScope ->
  RuntimeFrontier ctx epoch phase ->
  RuntimeAntichain ctx epoch phase
frontierVisibleAntichainForScope =
  frontierCutoffForScope
{-# INLINE frontierVisibleAntichainForScope #-}

frontierVisibleMinimums ::
  RuntimeFrontier ctx epoch phase ->
  Map (RuntimeFrontierKey ctx) (RuntimeAntichain ctx epoch phase)
frontierVisibleMinimums =
  rfVisibleMin
{-# INLINE frontierVisibleMinimums #-}

frontierPendingCounts ::
  RuntimeFrontier ctx epoch phase ->
  Map (RuntimeTime ctx epoch phase) Int
frontierPendingCounts =
  rfPending
{-# INLINE frontierPendingCounts #-}

frontierWithPendingCounts ::
  Map (RuntimeTime ctx epoch phase) Int ->
  RuntimeFrontier ctx epoch phase ->
  RuntimeFrontier ctx epoch phase
frontierWithPendingCounts pendingCounts frontier =
  frontier
    { rfPending =
        Map.filter (> 0) pendingCounts
    }
{-# INLINE frontierWithPendingCounts #-}

frontierTraceRetention :: RuntimeFrontier ctx epoch phase -> Maybe TraceRetention
frontierTraceRetention =
  rfTraceRetention
{-# INLINE frontierTraceRetention #-}

frontierWithTraceRetention ::
  Maybe TraceRetention ->
  RuntimeFrontier ctx epoch phase ->
  RuntimeFrontier ctx epoch phase
frontierWithTraceRetention retention frontier =
  frontier {rfTraceRetention = retention}
{-# INLINE frontierWithTraceRetention #-}

frontierPendingPointstamps ::
  RuntimeFrontier ctx epoch phase ->
  Set (RuntimeTime ctx epoch phase)
frontierPendingPointstamps =
  Map.keysSet . Map.filter (> 0) . rfPending
{-# INLINE frontierPendingPointstamps #-}

frontierPendingInScope ::
  RuntimeScope ->
  RuntimeFrontier ctx epoch phase ->
  Set (RuntimeTime ctx epoch phase)
frontierPendingInScope scopeValue =
  Set.filter
    (isDescendantOf scopeValue . rtScope)
    . frontierPendingPointstamps
{-# INLINE frontierPendingInScope #-}

frontierPendingAntichain ::
  (Ord ctx, Ord epoch, Ord phase, PartialOrder epoch, PartialOrder phase) =>
  RuntimeFrontier ctx epoch phase ->
  RuntimeAntichain ctx epoch phase
frontierPendingAntichain =
  mkUpperFrontier . Set.toList . frontierPendingPointstamps
{-# INLINE frontierPendingAntichain #-}

frontierPendingCount ::
  (Ord ctx, Ord epoch, Ord phase) =>
  RuntimeTime ctx epoch phase ->
  RuntimeFrontier ctx epoch phase ->
  Int
frontierPendingCount timeValue =
  Map.findWithDefault 0 timeValue . rfPending
{-# INLINE frontierPendingCount #-}

frontierInsertPending ::
  (Ord ctx, Ord epoch, Ord phase) =>
  RuntimeTime ctx epoch phase ->
  RuntimeFrontier ctx epoch phase ->
  RuntimeFrontier ctx epoch phase
frontierInsertPending timeValue frontier =
  frontier
    { rfPending =
        Map.insertWith (+) timeValue 1 (rfPending frontier)
    }
{-# INLINE frontierInsertPending #-}

frontierDeletePending ::
  (Ord ctx, Ord epoch, Ord phase) =>
  RuntimeTime ctx epoch phase ->
  RuntimeFrontier ctx epoch phase ->
  Either (RuntimeFrontierError ctx epoch phase) (RuntimeFrontier ctx epoch phase)
frontierDeletePending timeValue frontier =
  case Map.lookup timeValue (rfPending frontier) of
    Nothing ->
      Left (RuntimeFrontierMissingPendingComplete timeValue)
    Just count
      | count <= 0 ->
          Left (RuntimeFrontierMissingPendingComplete timeValue)
      | otherwise ->
          Right
            frontier
              { rfPending =
                  Map.update decrement timeValue (rfPending frontier)
              }
  where
    decrement :: Int -> Maybe Int
    decrement count
      | count <= 1 =
          Nothing
      | otherwise =
          Just (count - 1)
{-# INLINE frontierDeletePending #-}

frontierCompletePending ::
  (Ord ctx, Ord epoch, Ord phase, PartialOrder epoch, PartialOrder phase) =>
  RuntimeTime ctx epoch phase ->
  RuntimeFrontier ctx epoch phase ->
  Either (RuntimeFrontierError ctx epoch phase) (RuntimeFrontier ctx epoch phase)
frontierCompletePending timeValue frontier =
  frontierAdvanceVisibleMin timeValue <$> frontierDeletePending timeValue frontier
{-# INLINE frontierCompletePending #-}

frontierAdvanceVisibleMin ::
  (Ord ctx, Ord epoch, Ord phase, PartialOrder epoch, PartialOrder phase) =>
  RuntimeTime ctx epoch phase ->
  RuntimeFrontier ctx epoch phase ->
  RuntimeFrontier ctx epoch phase
frontierAdvanceVisibleMin timeValue frontier =
  frontier
    { rfVisibleMin =
        Map.insertWith
          mergeUpperFrontiers
          (runtimeFrontierKeyForTime timeValue)
          (singletonUpperFrontier timeValue)
          (rfVisibleMin frontier)
    }
{-# INLINE frontierAdvanceVisibleMin #-}

frontierHasPendingAtOrBefore ::
  (Eq ctx, PartialOrder epoch, PartialOrder phase) =>
  RuntimeFrontier ctx epoch phase ->
  RuntimeAntichain ctx epoch phase ->
  Bool
frontierHasPendingAtOrBefore frontier cutoffs =
  Map.foldrWithKey
    ( \pendingTime count found ->
        found || (count > 0 && anyRuntimeCutoff (pendingTime `runtimeTimeSameScopeLeq`) cutoffs)
    )
    False
    (rfPending frontier)
{-# INLINE frontierHasPendingAtOrBefore #-}

frontierPendingBeforeVisibleMinimum ::
  (Ord ctx, PartialOrder epoch, PartialOrder phase) =>
  RuntimeFrontier ctx epoch phase ->
  Set (RuntimeTime ctx epoch phase)
frontierPendingBeforeVisibleMinimum frontier =
  Set.filter pendingBlocksCompaction (frontierPendingPointstamps frontier)
  where
    pendingBlocksCompaction pendingTime =
      anyRuntimeCutoff
        (pendingTime `runtimeTimeSameScopeLeq`)
        (frontierCutoffForTime pendingTime frontier)
{-# INLINE frontierPendingBeforeVisibleMinimum #-}

frontierTimeCompactable ::
  (Ord ctx, PartialOrder epoch, PartialOrder phase) =>
  RuntimeFrontier ctx epoch phase ->
  RuntimeTime ctx epoch phase ->
  Bool
frontierTimeCompactable frontier timeValue =
  let cutoffs =
        frontierCutoffForTime timeValue frontier
   in anyRuntimeCutoff (timeValue `runtimeTimeSameScopeLt`) cutoffs
        && not (frontierHasPendingAtOrBefore frontier cutoffs)
{-# INLINE frontierTimeCompactable #-}

mergeUpperFrontiers ::
  (Ord time, PartialOrder time) =>
  UpperFrontier time ->
  UpperFrontier time ->
  UpperFrontier time
mergeUpperFrontiers left right =
  mkUpperFrontier (upperFrontierPoints left <> upperFrontierPoints right)
{-# INLINE mergeUpperFrontiers #-}

anyRuntimeCutoff ::
  (RuntimeTime ctx epoch phase -> Bool) ->
  RuntimeAntichain ctx epoch phase ->
  Bool
anyRuntimeCutoff predicate =
  any predicate . upperFrontierPoints
{-# INLINE anyRuntimeCutoff #-}
