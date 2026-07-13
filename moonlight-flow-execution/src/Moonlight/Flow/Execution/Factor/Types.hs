{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Execution.Factor.Types
  ( FactorInput (..),
    factorInputFromStoreView,
    factorInputSignature,
    FactorDemand (..),
    FactorRunSpec (..),
    FactorRunResult (..),
    FactorCache (..),
    FactorEntry (..),
    FactorFrame (..),
    ParentSepIndex (..),
    emptyFactorCache,
    factorCacheEntries,
    factorCacheLookup,
    factorCacheFactorAt,
    factorCacheInsert,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
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
import Moonlight.Flow.Execution.Observe.Provenance.Support
  ( ProvSupportMemo,
    emptyProvSupportMemo
  )
import Moonlight.Flow.Execution.Observe.Provenance.GC
  ( ProvGCConfig (..),
    ProvGCStats (..)
  )
import Moonlight.Flow.Execution.Observe.Telemetry
  ( MaintenanceMetrics,
    RepairTelemetryConfig,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Differential.Row.Tuple
  ( AssignmentTupleKey,
  )
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Flow.Storage.Store
  ( Store,
  )
import Moonlight.Flow.Storage.View
  ( SupportIds,
    View,
    ViewSignature,
    viewSignature,
  )

type FactorInput :: Type
data FactorInput = FactorInput
  { fiStore :: !Store,
    fiView :: !View,
    fiAtomDeltas :: !(IntMap RowDelta)
  }
  deriving stock (Eq, Show)

factorInputFromStoreView ::
  Store ->
  View ->
  IntMap RowDelta ->
  FactorInput
factorInputFromStoreView store view atomDeltas =
  FactorInput
    { fiStore = store,
      fiView = view,
      fiAtomDeltas = atomDeltas
    }
{-# INLINE factorInputFromStoreView #-}

factorInputSignature :: FactorInput -> ViewSignature
factorInputSignature inputValue =
  viewSignature (fiStore inputValue) (fiView inputValue)
{-# INLINE factorInputSignature #-}

type FactorDemand :: Type -> Type
data FactorDemand support where
  FactorDemandMaintenance :: FactorDemand ()
  FactorDemandRows :: FactorDemand ()
  FactorDemandSupport :: FactorDemand SupportIds

type FactorRunSpec :: Type -> Type
data FactorRunSpec support = FactorRunSpec
  { frsDecomp :: !DecompPlan,
    frsInput :: !FactorInput,
    frsCache :: !FactorCache,
    frsGc :: !ProvGCConfig,
    frsRepairTelemetry :: !RepairTelemetryConfig,
    frsDemand :: !(FactorDemand support)
  }

type FactorRunResult :: Type -> Type
data FactorRunResult support = FactorRunResult
  { frrSupport :: !support,
    frrPreSealCache :: !FactorCache,
    frrCache :: !FactorCache,
    frrMetrics :: !MaintenanceMetrics,
    frrGcStats :: !(Maybe ProvGCStats)
  }

type FactorEntry :: Type
data FactorEntry = FactorEntry
  { feFactor :: !Factor,
    feDelta :: !FactorDelta,
    feContributions :: !FactorContributionIndex
  }
  deriving stock (Eq, Show)

type ParentSepIndex :: Type
data ParentSepIndex = ParentSepIndex
  { psiSeparator :: ![SlotId],
    psiRowsBySeparator :: !(Map AssignmentTupleKey (Set AssignmentTupleKey))
  }
  deriving stock (Eq, Show)

type FactorCache :: Type
data FactorCache = FactorCache
  { fcArena :: !ProvArena,
    fcViewSignature :: !(Maybe ViewSignature),
    fcFactors :: !(Map FactorNode FactorEntry),
    fcParentSepIndexes :: !(Map BagId ParentSepIndex),
    fcSupportMemo :: !ProvSupportMemo
  }
  deriving stock (Eq, Show)

type FactorFrame :: Type
data FactorFrame = FactorFrame
  { ffInput :: !FactorInput,
    ffCache :: !FactorCache,
    ffDirtyNodes :: !(Set FactorNode),
    ffDeltaNodes :: !(Set FactorNode),
    ffMetrics :: !MaintenanceMetrics,
    ffRepairTelemetry :: !RepairTelemetryConfig
  }
  deriving stock (Eq, Show)

emptyFactorCache :: FactorCache
emptyFactorCache =
  FactorCache
    { fcArena = emptyProvArena,
      fcViewSignature = Nothing,
      fcFactors = Map.empty,
      fcParentSepIndexes = Map.empty,
      fcSupportMemo = emptyProvSupportMemo
    }

factorCacheEntries :: FactorCache -> [(FactorNode, FactorEntry)]
factorCacheEntries =
  Map.toAscList . fcFactors
{-# INLINE factorCacheEntries #-}

factorCacheLookup :: FactorNode -> FactorCache -> Maybe FactorEntry
factorCacheLookup node =
  Map.lookup node . fcFactors
{-# INLINE factorCacheLookup #-}

factorCacheFactorAt :: FactorNode -> FactorCache -> Maybe Factor
factorCacheFactorAt node cache =
  feFactor <$> factorCacheLookup node cache
{-# INLINE factorCacheFactorAt #-}

factorCacheInsert :: FactorNode -> FactorEntry -> FactorCache -> FactorCache
factorCacheInsert node entry cache =
  cache
    { fcFactors = Map.insert node entry (fcFactors cache)
    }
{-# INLINE factorCacheInsert #-}
