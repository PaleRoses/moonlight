{-# LANGUAGE BangPatterns #-}

module Moonlight.Flow.Execution.Factor.Dirty
  ( refreshFactorFrame,
    dropFrameDirtyNodes,
    recordFrameSupportEvalStats,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( mapMaybe,
  )
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Flow.Execution.Factor.Incremental
  ( normalizeAtomDeltas,
  )
import Moonlight.Flow.Execution.Factor.Types
  ( FactorCache (..),
    FactorFrame (..),
    FactorInput (..),
    factorInputSignature,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Support
  ( ProvSupportEvalStats (..),
  )
import Moonlight.Flow.Execution.Observe.Telemetry
  ( RepairTelemetryConfig,
    recordInputDeltaRows,
    recordSupportEvaluations,
    recordSupportMemoHits,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Differential.Row.Patch
  ( normalizePlainRowPatch,
    plainRowPatchChangeMap,
  )
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Flow.Storage.View
  ( ViewSignature,
  )

sourceDirtyNodesForBags :: IntSet -> Set FactorNode
sourceDirtyNodesForBags =
  Set.fromDistinctAscList
    . fmap (FactorNodeBag . BagId)
    . IntSet.toAscList
{-# INLINE sourceDirtyNodesForBags #-}

beliefDirtyNodesForBags :: IntSet -> Set FactorNode
beliefDirtyNodesForBags =
  Set.fromDistinctAscList
    . fmap (FactorNodeBagBelief . BagId)
    . IntSet.toAscList
{-# INLINE beliefDirtyNodesForBags #-}

bagAncestorClosure :: DecompPlan -> IntSet -> IntSet
bagAncestorClosure decomp =
  IntSet.foldl' (insertBagAncestors decomp) IntSet.empty
{-# INLINE bagAncestorClosure #-}

insertBagAncestors :: DecompPlan -> IntSet -> Int -> IntSet
insertBagAncestors decomp ancestors bagKey
  | IntSet.member bagKey ancestors =
      ancestors
  | otherwise =
      let ancestorsWithBag =
            IntSet.insert bagKey ancestors
       in maybe
            ancestorsWithBag
            (insertBagAncestors decomp ancestorsWithBag . unBagId)
            (IntMap.lookup bagKey (dpParent decomp))
{-# INLINE insertBagAncestors #-}

dirtyFactorNodesForBags :: DecompPlan -> IntSet -> Set FactorNode
dirtyFactorNodesForBags decomp sourceBags
  | IntSet.null sourceBags =
      Set.empty
  | otherwise =
      Set.insert FactorNodeRoot $
        Set.union
          (sourceDirtyNodesForBags sourceBags)
          (beliefDirtyNodesForBags (bagAncestorClosure decomp sourceBags))
{-# INLINE dirtyFactorNodesForBags #-}

bagsForAtoms :: DecompPlan -> IntSet -> IntSet
bagsForAtoms decomp =
  IntSet.fromList . mapMaybe ownerBagKey . IntSet.toList
  where
    ownerBagKey atomKey =
      unBagId <$> IntMap.lookup atomKey (dpAtomOwner decomp)
{-# INLINE bagsForAtoms #-}

refreshFactorFrame :: RepairTelemetryConfig -> DecompPlan -> FactorInput -> FactorCache -> FactorFrame
refreshFactorFrame repairTelemetryConfig decomp input0 cache0 =
  let !atomDeltas = normalizeAtomDeltas (fiAtomDeltas input0)
      !sig = factorInputSignature input0
      !viewChanged = fcViewSignature cache0 /= Just sig
      !cache1 =
        if viewChanged && IntMap.null atomDeltas
          then invalidateFactorCacheForView sig cache0
          else cache0 {fcViewSignature = Just sig}
      !dirtyNodes =
        dirtyFactorNodesForBags
          decomp
          (bagsForAtoms decomp (IntMap.keysSet atomDeltas))
      !metrics = recordInputDeltaRows (atomDeltasRowCount atomDeltas) mempty
   in FactorFrame
        { ffInput = input0 {fiAtomDeltas = atomDeltas},
          ffCache = cache1,
          ffDirtyNodes = dirtyNodes,
          ffDeltaNodes = Set.empty,
          ffMetrics = metrics,
          ffRepairTelemetry = repairTelemetryConfig
        }
{-# INLINE refreshFactorFrame #-}

invalidateFactorCacheForView :: ViewSignature -> FactorCache -> FactorCache
invalidateFactorCacheForView sig cache =
  cache
    { fcViewSignature = Just sig,
      fcFactors = Map.empty,
      fcParentSepIndexes = Map.empty
    }
{-# INLINE invalidateFactorCacheForView #-}

atomInputDeltaRowCount :: RowDelta -> Int
atomInputDeltaRowCount =
  Map.size . plainRowPatchChangeMap . normalizePlainRowPatch
{-# INLINE atomInputDeltaRowCount #-}

atomDeltasRowCount :: IntMap RowDelta -> Int
atomDeltasRowCount =
  sum . fmap atomInputDeltaRowCount . IntMap.elems
{-# INLINE atomDeltasRowCount #-}

dropFrameDirtyNodes :: FactorFrame -> FactorFrame
dropFrameDirtyNodes frame
  | Set.null (ffDirtyNodes frame) =
      frame
  | otherwise =
      frame
        { ffCache =
            Set.foldr
              dropDirtyFactorNode
              (ffCache frame)
              (ffDirtyNodes frame),
          ffDirtyNodes = Set.empty
        }
{-# INLINE dropFrameDirtyNodes #-}

dropDirtyFactorNode :: FactorNode -> FactorCache -> FactorCache
dropDirtyFactorNode node cache =
  case node of
    FactorNodeBagBelief bag ->
      cache
        { fcFactors = Map.delete node (fcFactors cache),
          fcParentSepIndexes = Map.delete bag (fcParentSepIndexes cache)
        }
    _ ->
      cache
        { fcFactors = Map.delete node (fcFactors cache)
        }
{-# INLINE dropDirtyFactorNode #-}

recordFrameSupportEvalStats ::
  ProvSupportEvalStats ->
  FactorFrame ->
  FactorFrame
recordFrameSupportEvalStats stats frame =
  frame
    { ffMetrics =
        recordSupportMemoHits (pseMemoHits stats) $
          recordSupportEvaluations
            (pseNodesEvaluated stats)
            (ffMetrics frame)
    }
{-# INLINE recordFrameSupportEvalStats #-}
