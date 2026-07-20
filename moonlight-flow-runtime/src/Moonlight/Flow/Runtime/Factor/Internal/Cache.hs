{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Runtime.Factor.Internal.Cache
  ( FactorNodeCache (..),
    FactorAtomReadStamp (..),
    FactorPreparedInputCache (..),
    FactorCacheReadiness (..),
    FactorCacheState (..),
    emptyFactorCacheState,
    clearFactorCacheState,
    factorAtomReadsAt,
    factorCacheReadiness,
    factorCacheStateFromCacheAt,
    factorCacheStateFromCacheAtWithInput,
    factorCacheStateToTransientCache,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Moonlight.Flow.Carrier.Store
  ( CarrierReadFrontier,
  )
import Moonlight.Flow.Execution.Factor.Types
  ( FactorCache (..),
    FactorEntry (..),
    emptyFactorCache,
    factorCacheEntries,
  )
import Moonlight.Flow.Execution.Factor.Core
  ( Factor,
  )
import Moonlight.Flow.Execution.Factor.Delta
  ( FactorDelta,
  )
import Moonlight.Flow.Execution.Factor.Contribution
  ( FactorContributionIndex,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Types
  ( ProvArena,
    emptyProvArena
  )
import Moonlight.Flow.Plan.Query.Core
  ( FactorNode,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.Flow.Storage.Relation
  ( Relation,
  )
import Moonlight.Flow.Storage.Store
  ( Store,
  )
import Moonlight.Flow.Storage.View
  ( ViewSignature,
  )

data FactorNodeCache = FactorNodeCache
  { fncFactor :: !Factor,
    fncDelta :: !FactorDelta,
    fncContributions :: !FactorContributionIndex
  }
  deriving stock (Eq, Show)

data FactorAtomReadStamp = FactorAtomReadStamp
  { farsFrontier :: !CarrierReadFrontier,
    farsBoundaryDigest :: !StableDigest128
  }
  deriving stock (Eq, Show)

data FactorPreparedInputCache = FactorPreparedInputCache
  { fpicRelations :: !(IntMap Relation),
    fpicStore :: !Store,
    fpicBoundaryDigests :: !(IntMap StableDigest128),
    fpicViewSignature :: !ViewSignature
  }
  deriving stock (Eq, Show)

data FactorCacheReadiness
  = FactorCacheCold
  | FactorCacheReady !CarrierReadFrontier
  | FactorCacheIncoherent
  deriving stock (Eq, Show)

data FactorCacheState = FactorCacheState
  { fcsNodes :: !(Map FactorNode FactorNodeCache),
    fcsProv :: !ProvArena,
    fcsNodeReads :: !(Map FactorNode CarrierReadFrontier),
    fcsAtomReads :: !(IntMap FactorAtomReadStamp),
    fcsPreparedInput :: !(Maybe FactorPreparedInputCache)
  }
  deriving stock (Eq, Show)

emptyFactorCacheState :: FactorCacheState
emptyFactorCacheState =
  FactorCacheState
    { fcsNodes = Map.empty,
      fcsProv = emptyProvArena,
      fcsNodeReads = Map.empty,
      fcsAtomReads = IntMap.empty,
      fcsPreparedInput = Nothing
    }
{-# INLINE emptyFactorCacheState #-}

clearFactorCacheState :: FactorCacheState -> FactorCacheState
clearFactorCacheState _ =
  emptyFactorCacheState
{-# INLINE clearFactorCacheState #-}

factorAtomReadsAt ::
  CarrierReadFrontier ->
  IntMap StableDigest128 ->
  IntMap FactorAtomReadStamp
factorAtomReadsAt frontier =
  IntMap.map
    ( \boundaryDigestValue ->
        FactorAtomReadStamp
          { farsFrontier = frontier,
            farsBoundaryDigest = boundaryDigestValue
          }
    )
{-# INLINE factorAtomReadsAt #-}

factorCacheReadiness ::
  IntSet ->
  FactorCacheState ->
  FactorCacheReadiness
factorCacheReadiness atomKeys state
  | Map.null (fcsNodes state) =
      FactorCacheCold
  | otherwise =
      case factorCacheUniformNodeFrontier state of
        Nothing ->
          FactorCacheIncoherent
        Just frontier
          | factorAtomReadsMatch atomKeys frontier state ->
              FactorCacheReady frontier
          | otherwise ->
              FactorCacheIncoherent
{-# INLINE factorCacheReadiness #-}

factorCacheUniformNodeFrontier ::
  FactorCacheState ->
  Maybe CarrierReadFrontier
factorCacheUniformNodeFrontier state =
  case Map.lookupMin (fcsNodes state) of
    Nothing ->
      Nothing
    Just (firstNode, _) -> do
      firstFrontier <- Map.lookup firstNode (fcsNodeReads state)
      let nodeReads =
            Map.restrictKeys (fcsNodeReads state) (Map.keysSet (fcsNodes state))
      if Map.size nodeReads == Map.size (fcsNodes state)
        && all (== firstFrontier) (Map.elems nodeReads)
        then Just firstFrontier
        else Nothing
{-# INLINE factorCacheUniformNodeFrontier #-}

factorAtomReadsMatch ::
  IntSet ->
  CarrierReadFrontier ->
  FactorCacheState ->
  Bool
factorAtomReadsMatch atomKeys frontier state =
  IntMap.keysSet (fcsAtomReads state) == atomKeys
    && all
      ((== frontier) . farsFrontier)
      (IntMap.elems (fcsAtomReads state))
{-# INLINE factorAtomReadsMatch #-}

factorCacheStateFromCacheAt ::
  IntMap FactorAtomReadStamp ->
  CarrierReadFrontier ->
  FactorCache ->
  FactorCacheState
factorCacheStateFromCacheAt =
  factorCacheStateFromCacheAtWithInput Nothing
{-# INLINE factorCacheStateFromCacheAt #-}

factorCacheStateFromCacheAtWithInput ::
  Maybe FactorPreparedInputCache ->
  IntMap FactorAtomReadStamp ->
  CarrierReadFrontier ->
  FactorCache ->
  FactorCacheState
factorCacheStateFromCacheAtWithInput maybeInput atomReads frontier cache =
  let nodeCaches =
        Map.fromAscList
          [ ( node,
              FactorNodeCache
                { fncFactor = feFactor entry,
                  fncDelta = feDelta entry,
                  fncContributions = feContributions entry
                }
            )
          | (node, entry) <- factorCacheEntries cache
          ]
   in FactorCacheState
        { fcsNodes = nodeCaches,
          fcsProv = fcArena cache,
          fcsNodeReads = Map.map (const frontier) nodeCaches,
          fcsAtomReads = atomReads,
          fcsPreparedInput = maybeInput
        }
{-# INLINE factorCacheStateFromCacheAtWithInput #-}

factorCacheStateToTransientCache ::
  FactorCacheState ->
  FactorCache
factorCacheStateToTransientCache state =
  emptyFactorCache
    { fcArena = fcsProv state,
      fcViewSignature = fpicViewSignature <$> fcsPreparedInput state,
      fcFactors =
        Map.map
          ( \cache ->
              FactorEntry
                { feFactor = fncFactor cache,
                  feDelta = fncDelta cache,
                  feContributions = fncContributions cache
                }
          )
          (fcsNodes state)
    }
{-# INLINE factorCacheStateToTransientCache #-}
