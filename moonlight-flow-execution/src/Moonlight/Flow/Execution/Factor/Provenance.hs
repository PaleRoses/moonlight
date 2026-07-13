module Moonlight.Flow.Execution.Factor.Provenance
  ( sealFactorCacheEpoch,
    clearFactorEntryDelta,
    maybeCollectFactorCache,
    compactFactorCache,
    factorProvRoots,
    factorDeltaProvRoots,
    factorCacheProvRoots,
    snapshotFactorCacheProvTelemetry,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Moonlight.Delta.Patch qualified as CorePatch
import Moonlight.Flow.Execution.Factor.Core
  ( Factor,
    remapFactorProvIds,
  )
import Moonlight.Flow.Execution.Factor.Delta
  ( FactorDelta,
    remapFactorDeltaProvIds,
  )
import Moonlight.Flow.Execution.Factor.Contribution
  ( factorContributionIndexProvRoots,
    remapFactorContributionIndexProvIds,
  )
import Moonlight.Flow.Execution.Factor.Types
import Moonlight.Flow.Execution.Observe.Provenance.Types
  ( ProvenanceObstruction (..),
    ProvVal (..),
    ProvArena,
    paNext,
    paNodes
  )
import Moonlight.Flow.Execution.Observe.Provenance.Support
  ( pruneProvSupportMemo,
    remapProvSupportMemo
  )
import Moonlight.Flow.Execution.Observe.Provenance.GC
  ( ProvGCMode (..),
    ProvGCConfig (..),
    ProvGCStats (..),
    collectProvArena,
    shouldRunMinorGC,
    shouldRunMajorGC,
    shouldCompactArena,
    compactProvArena
  )
import Moonlight.Flow.Execution.Observe.Telemetry
  ( ProvTelemetry,
    snapshotProvTelemetry,
  )
import Moonlight.Differential.Row.Patch
  ( ShapedPatch (..),
    emptyShapedPatch,
  )
import Moonlight.Flow.Plan.Query.Core
  ( FactorNode,
  )
import Moonlight.Differential.Index.IndexedRows
  ( indexedRowsPayloadMap,
    indexedRowsLayout,
  )

sealFactorCacheEpoch :: Set FactorNode -> FactorCache -> FactorCache
sealFactorCacheEpoch deltaNodes cache =
  cache
    { fcFactors =
        Set.foldl'
          (\factors node -> Map.adjust clearFactorEntryDelta node factors)
          (fcFactors cache)
          deltaNodes
    }
{-# INLINE sealFactorCacheEpoch #-}

clearFactorEntryDelta :: FactorEntry -> FactorEntry
clearFactorEntryDelta entry =
  entry
    { feDelta = emptyShapedPatch (Vector.toList (indexedRowsLayout (feFactor entry)))
    }
{-# INLINE clearFactorEntryDelta #-}

factorProvRoots :: Factor -> [ProvVal]
factorProvRoots =
  Map.elems . indexedRowsPayloadMap
{-# INLINE factorProvRoots #-}

factorDeltaProvRoots :: FactorDelta -> [ProvVal]
factorDeltaProvRoots delta =
  CorePatch.foldWithKey
    (\_key roots -> roots)
    (\_key newValue roots -> newValue : roots)
    (\_key oldValue roots -> oldValue : roots)
    (\_key oldValue newValue roots -> oldValue : newValue : roots)
    []
    (spdDelta delta)
{-# INLINE factorDeltaProvRoots #-}

factorCacheProvRoots :: FactorCache -> [ProvVal]
factorCacheProvRoots cache =
  foldMap entryRoots (fcFactors cache)
  where
    entryRoots entry =
      factorProvRoots (feFactor entry)
        <> factorDeltaProvRoots (feDelta entry)
        <> factorContributionIndexProvRoots (feContributions entry)
{-# INLINE factorCacheProvRoots #-}

snapshotFactorCacheProvTelemetry :: Maybe ProvGCStats -> FactorCache -> ProvTelemetry
snapshotFactorCacheProvTelemetry lastGc cache =
  snapshotProvTelemetry lastGc (Just (factorCacheProvRoots cache)) (fcArena cache)
{-# INLINE snapshotFactorCacheProvTelemetry #-}

maybeCollectFactorCache ::
  ProvGCConfig ->
  FactorCache ->
  Either ProvenanceObstruction (FactorCache, Maybe ProvGCStats)
maybeCollectFactorCache cfg cache0 =
  let arena0 = fcArena cache0
   in if paNext arena0 < pgcCompactionMinPaNext cfg
        && not (shouldRunMinorGC cfg arena0)
        && IntMap.size (paNodes arena0) < pgcMajorNodeLimit cfg
      then pure (cache0, Nothing)
      else do
        let roots = factorCacheProvRoots cache0
        compact <- shouldCompactArena cfg roots arena0
        if compact
          then do
            (cache1, stats) <- compactFactorCache cache0
            pure (cache1, Just stats)
          else do
            major <- shouldRunMajorGC cfg roots arena0
            if major
              then do
                (arena1, stats) <- collectProvArena cfg MajorGC roots arena0
                pure (cacheWithCollectedArena roots arena1, Just stats)
              else
                if shouldRunMinorGC cfg arena0
                  then do
                    (arena1, stats) <- collectProvArena cfg MinorGC roots arena0
                    pure (cacheWithCollectedArena roots arena1, Just stats)
                  else pure (cache0, Nothing)
  where
    cacheWithCollectedArena ::
      [ProvVal] ->
      ProvArena ->
      FactorCache
    cacheWithCollectedArena roots arena =
      cache0
        { fcArena = arena,
          fcSupportMemo =
            pruneProvSupportMemo roots arena (fcSupportMemo cache0)
        }
{-# INLINE maybeCollectFactorCache #-}

compactFactorCache :: FactorCache -> Either ProvenanceObstruction (FactorCache, ProvGCStats)
compactFactorCache cache =
  let roots = factorCacheProvRoots cache
   in do
        (arena1, remap, stats) <- compactProvArena roots (fcArena cache)
        factors <- traverse (remapFactorEntry remap) (fcFactors cache)
        supportMemo0 <-
          remapProvSupportMemo
            (fcArena cache)
            arena1
            remap
            (fcSupportMemo cache)
        let cache1 =
              cache
                { fcArena = arena1,
                  fcFactors = factors,
                  fcSupportMemo = supportMemo0
                }
            supportMemo1 =
              pruneProvSupportMemo
                (factorCacheProvRoots cache1)
                arena1
                supportMemo0
        pure (cache1 {fcSupportMemo = supportMemo1}, stats)
  where
    remapFactorEntry remap entry = do
      factor <- remapFactorProvIds remap (feFactor entry)
      delta <- remapFactorDeltaProvIds remap (feDelta entry)
      contributions <- remapFactorContributionIndexProvIds remap (feContributions entry)
      pure
        FactorEntry
          { feFactor = factor,
            feDelta = delta,
            feContributions = contributions
          }
{-# INLINE compactFactorCache #-}
