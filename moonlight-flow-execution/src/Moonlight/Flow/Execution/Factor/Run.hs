{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}

module Moonlight.Flow.Execution.Factor.Run
  ( FactorRunError (..),
    runFactor,
    factorRunTelemetry,
  )
where

import Data.HashSet qualified as HashSet
import Data.Bifunctor (first)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Moonlight.Flow.Execution.Factor.Dirty
  ( dropFrameDirtyNodes,
    recordFrameSupportEvalStats,
    refreshFactorFrame,
  )
import Moonlight.Flow.Execution.Dense.Plan (DenseJoinPlanError)
import Moonlight.Flow.Execution.Factor.NodePlan
  ( ensureAllBagBeliefs,
    ensureRootFactor,
  )
import Moonlight.Flow.Execution.Factor.Provenance
  ( maybeCollectFactorCache,
    sealFactorCacheEpoch,
  )
import Moonlight.Flow.Execution.Factor.Types
import Moonlight.Flow.Execution.Observe.Provenance.Types
  ( ProvenanceObstruction (..),
    ProvVal (..)
  )
import Moonlight.Flow.Execution.Observe.Provenance.Support
  ( ProvSupport,
    provSupportMemoNodeCount,
    evalProvSupportWithMemo
  )
import Moonlight.Flow.Execution.Observe.Telemetry
  ( FactorCacheTelemetry,
    snapshotFactorCacheTelemetry,
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Differential.Index.IndexedRows
  ( indexedRowsPayloadMap,
  )
import Moonlight.Differential.Index.RowId
  ( rowIdInt,
  )
import Moonlight.Flow.Storage.Relation
import Moonlight.Flow.Storage.Store
import Moonlight.Flow.Storage.View
import Moonlight.Differential.Index.RowSet
  ( emptyRowSet,
    rowSetFromIntSetCanonical,
    rowSetMember,
  )

data FactorRunError
  = FactorRunDenseJoinPlanError !DenseJoinPlanError
  | FactorRunProvenanceError !ProvenanceObstruction
  deriving stock (Eq, Ord, Show)

runFactor :: FactorRunSpec support -> Either FactorRunError (FactorRunResult support)
runFactor spec =
  let decomp = frsDecomp spec
      frame0 = refreshFactorFrame (frsRepairTelemetry spec) decomp (frsInput spec) (frsCache spec)
   in case frsDemand spec of
        FactorDemandMaintenance ->
          do
            (frame1, _rootFactor, _rootDelta) <- first FactorRunDenseJoinPlanError (ensureRootFactor decomp frame0)
            first FactorRunProvenanceError (finishFactorRun spec () frame1)

        FactorDemandRows ->
          do
            frame1 <- first FactorRunDenseJoinPlanError (ensureAllBagBeliefs decomp (dpRoot decomp) frame0)
            first FactorRunProvenanceError (finishFactorRun spec () frame1)

        FactorDemandSupport ->
          runFactorSupportDemand spec frame0
{-# INLINE runFactor #-}

runFactorSupportDemand ::
  FactorRunSpec SupportIds ->
  FactorFrame ->
  Either FactorRunError (FactorRunResult SupportIds)
runFactorSupportDemand spec frame0 = do
  let decomp =
        frsDecomp spec
  (frame1, rootFactor, _rootDelta) <- first FactorRunDenseJoinPlanError (ensureRootFactor decomp frame0)
  let rootVal =
        Map.findWithDefault PVZero emptyTupleKey (indexedRowsPayloadMap rootFactor)
  (supportByRow, supportMemo, stats) <-
    first FactorRunProvenanceError $
      evalProvSupportWithMemo
        (fcArena (ffCache frame1))
        rootVal
        (fcSupportMemo (ffCache frame1))
  let !support =
        resolveSupportAgainstView (ffInput frame1) supportByRow
      !frame2 =
        recordFrameSupportEvalStats
          stats
          frame1
            { ffCache =
                (ffCache frame1)
                  { fcSupportMemo = supportMemo
                  }
            }
  first FactorRunProvenanceError (finishFactorRun spec support frame2)
{-# INLINE runFactorSupportDemand #-}

finishFactorRun ::
  FactorRunSpec support ->
  support ->
  FactorFrame ->
  Either ProvenanceObstruction (FactorRunResult support)
finishFactorRun spec support frame0 =
  let frame1 = dropFrameDirtyNodes frame0
      preSealCache = ffCache frame1
      sealedCache = sealFactorCacheEpoch (ffDeltaNodes frame1) preSealCache
   in do
        (postGcCache, stats) <- maybeCollectFactorCache (frsGc spec) sealedCache
        pure
          FactorRunResult
            { frrSupport = support,
              frrPreSealCache = preSealCache,
              frrCache = postGcCache,
              frrMetrics = ffMetrics frame1,
              frrGcStats = stats
            }
{-# INLINE finishFactorRun #-}

factorRunTelemetry :: FactorRunResult support -> FactorCacheTelemetry
factorRunTelemetry result =
  snapshotFactorCacheTelemetry
    (frrGcStats result)
    (frrMetrics result)
    (fcFactors cache)
    (provSupportMemoNodeCount (fcSupportMemo cache))
    Nothing
    Nothing
    (fcArena cache)
  where
    cache =
      frrCache result
{-# INLINE factorRunTelemetry #-}

resolveSupportAgainstView :: FactorInput -> ProvSupport -> SupportIds
resolveSupportAgainstView inputValue supportByRow =
  let store = fiStore inputValue
      view = fiView inputValue
      prepared = storeRelations store
   in IntMap.mapWithKey
        ( \atomKey rows ->
            case IntMap.lookup atomKey prepared of
              Nothing -> emptyRowSet
              Just pr ->
                let activeRows = viewRows store view atomKey
                 in rowSetFromIntSetCanonical $
                     IntSet.fromList
                        [ rowIdInt rid
                          | row <- HashSet.toList rows,
                            Just rid <- [rowIdForRow pr row],
                            rowSetMember rid activeRows
                        ]
        )
        supportByRow
{-# INLINE resolveSupportAgainstView #-}
