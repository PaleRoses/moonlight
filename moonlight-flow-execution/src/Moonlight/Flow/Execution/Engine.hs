{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Flow.Execution.Engine
  ( RelationalEngineLimits (..),
    RelationalEngineState (..),
    RelationalRequest (..),
    RelationalResult (..),
    RelationalRunObstruction (..),
    FactorCacheKey (..),
    FactorCacheEntry (..),
    EngineTelemetry (..),
    emptyRelationalEngineState,
    runRelational,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict (IntMap)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Moonlight.Flow.Execution.Prepared.Cache
  ( emptyJoinCacheState,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Flow.Execution.Factor.Types
  ( FactorCache,
    emptyFactorCache,
  )
import Moonlight.Flow.Storage.Restriction
  ( Restriction,
    restrictionDigest,
  )
import Moonlight.Flow.Storage.View
  ( unrestrictedView,
  )
import Moonlight.Flow.Execution.Observe.Provenance.Types
  ( ProvenanceObstruction (..),
  )
import Moonlight.Flow.Execution.Observe.Telemetry
  ( DispatchTelemetry,
  )
import Moonlight.Flow.Execution.Prepared.Backend
  ( PreparedBackend (..),
    PreparedJoinCacheState,
    PreparedPlan,
    preparedScopeStore,
  )
import Moonlight.Flow.Execution.Prepared.Run
  ( PreparedOp (..),
    PreparedResult (..),
    PreparedRunMode (..),
    PreparedRunSpec (..),
    preparedOpRestriction,
    runPrepared,
  )
import Moonlight.Flow.Execution.Prepared.Request
  ( PreparedExecutionKey (..),
    PreparedRequestView,
    ensurePreparedForRequestWith,
    preparedExecutionKey,
  )
import Moonlight.Flow.Execution.Prepared.Topology
  ( PreparedTopologyStamp,
    preparedTopologyStamp,
  )
import Moonlight.Flow.Plan.Query.Core

data RelationalEngineLimits = RelationalEngineLimits
  { relMaxFactorCaches :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)

data RelationalEngineState c backend = RelationalEngineState
  { resJoinCache :: !(PreparedJoinCacheState c backend),
    resFactorCaches :: !(Map (FactorCacheKey c) FactorCacheEntry),
    resTick :: {-# UNPACK #-} !Word64,
    resLimits :: !RelationalEngineLimits
  }

data RelationalRequest c backend projection a = RelationalRequest
  { relRequestPlan :: !(PreparedPlan backend),
    relRequestView :: !(PreparedRequestView c backend projection),
    relRequestRestriction :: !Restriction,
    relRequestAtomDeltas :: !(IntMap RowDelta),
    relRequestOp :: !(PreparedOp a)
  }

data RelationalResult c a = RelationalResult
  { relResultValue :: !a,
    relResultTelemetry :: !(EngineTelemetry c)
  }
  deriving stock (Eq, Show)

data RelationalRunObstruction backend
  = RelationalPreparedObstruction !(PreparedObstruction backend)
  | RelationalProvenanceObstruction !ProvenanceObstruction

deriving stock instance Eq (PreparedObstruction backend) => Eq (RelationalRunObstruction backend)

deriving stock instance Show (PreparedObstruction backend) => Show (RelationalRunObstruction backend)

data FactorCacheKey c = FactorCacheKey
  { fckPreparedExecutionKey :: !(PreparedExecutionKey c),
    fckRestrictionHigh :: {-# UNPACK #-} !Word64,
    fckRestrictionLow :: {-# UNPACK #-} !Word64,
    fckTopologyStamp :: !PreparedTopologyStamp
  }
  deriving stock (Eq, Ord, Show)

data FactorCacheEntry = FactorCacheEntry
  { fceCache :: !FactorCache,
    fceTouchedAt :: {-# UNPACK #-} !Word64
  }
  deriving stock (Eq, Show)

data EngineTelemetry c = EngineTelemetry
  { etPreparedExecutionKey :: !(PreparedExecutionKey c),
    etTopologyStamp :: !PreparedTopologyStamp,
    etDispatch :: !DispatchTelemetry
  }
  deriving stock (Eq, Show)

emptyRelationalEngineState ::
  RelationalEngineState c backend
emptyRelationalEngineState =
  RelationalEngineState
    { resJoinCache = emptyJoinCacheState,
      resFactorCaches = Map.empty,
      resTick = 0,
      resLimits =
        RelationalEngineLimits
          { relMaxFactorCaches = 128
          }
    }

runRelational ::
  (Ord c, PreparedBackend backend) =>
  backend ->
  RelationalRequest c backend projection a ->
  RelationalEngineState c backend ->
  Either
    (RelationalRunObstruction backend)
    ( RelationalEngineState c backend,
      RelationalResult c a
    )
runRelational backend request st0 =
  let plan =
        relRequestPlan request
      requestView =
        relRequestView request
      restriction =
        relRequestRestriction request
      atomDeltas =
        relRequestAtomDeltas request
      op =
        relRequestOp request
      planKey =
        queryPlanCacheKey plan
      execKey =
        preparedExecutionKey plan requestView
   in do
      (joinCache1, preparedScope) <-
        first RelationalPreparedObstruction $
          ensurePreparedForRequestWith
            backend
            plan
            requestView
            (resJoinCache st0)
      let
        store =
          preparedScopeStore backend preparedScope
        view =
          unrestrictedView
        topology =
          preparedTopologyStamp planKey store

        preparedSpec mode =
          PreparedRunSpec
            { prsPlan = plan,
              prsRestriction = restriction,
              prsStore = store,
              prsView = view,
              prsAtomDeltas = atomDeltas,
              prsStructuralSources = Nothing,
              prsOp = op,
              prsMode = mode
            }

        finish st result =
          Right
            ( st {resJoinCache = joinCache1},
              RelationalResult
                { relResultValue = prValue result,
                  relResultTelemetry =
                    EngineTelemetry
                      { etPreparedExecutionKey = execKey,
                        etTopologyStamp = topology,
                        etDispatch = prTelemetry result
                      }
                }
            )
      if structuralPlanUsesFactorCache plan
        then do
          let factorKey =
                factorCacheKey execKey topology restriction op
              cache0 =
                maybe
                  emptyFactorCache
                  fceCache
                  (Map.lookup factorKey (resFactorCaches st0))
          result <-
            firstRelationalProvenance (runPrepared (preparedSpec (PreparedMeasuredCached cache0)))
          let st1 =
                maybe
                  st0
                  (\cache1 -> insertFactorCache factorKey cache1 st0)
                  (prFactorCache result)
          finish st1 result
        else do
          result <-
            firstRelationalProvenance (runPrepared (preparedSpec PreparedMeasuredFresh))
          finish st0 result

firstRelationalProvenance ::
  Either ProvenanceObstruction a ->
  Either (RelationalRunObstruction backend) a
firstRelationalProvenance =
  first RelationalProvenanceObstruction
{-# INLINE firstRelationalProvenance #-}

structuralPlanUsesFactorCache ::
  QueryPlan compiled output guard tag tuple key ->
  Bool
structuralPlanUsesFactorCache plan =
  case qpDomain plan of
    StructuralQueryPlan ->
      True
    RootDomainQueryPlan ->
      False
{-# INLINE structuralPlanUsesFactorCache #-}

factorCacheKey ::
  PreparedExecutionKey c ->
  PreparedTopologyStamp ->
  Restriction ->
  PreparedOp a ->
  FactorCacheKey c
factorCacheKey execKey topology restriction op =
  let effectiveRestriction =
        restriction <> preparedOpRestriction op
      (restrictionHigh, restrictionLow) =
        restrictionDigest effectiveRestriction
   in FactorCacheKey
        { fckPreparedExecutionKey = execKey,
          fckRestrictionHigh = restrictionHigh,
          fckRestrictionLow = restrictionLow,
          fckTopologyStamp = topology
        }

insertFactorCache ::
  Ord c =>
  FactorCacheKey c ->
  FactorCache ->
  RelationalEngineState c backend ->
  RelationalEngineState c backend
insertFactorCache key cache st =
  let !tick = resTick st + 1
      entry =
        FactorCacheEntry
          { fceCache = cache,
            fceTouchedAt = tick
          }
   in pruneFactorCaches
        st
          { resTick = tick,
            resFactorCaches = Map.insert key entry (resFactorCaches st)
          }

pruneFactorCaches ::
  Ord c =>
  RelationalEngineState c backend ->
  RelationalEngineState c backend
pruneFactorCaches st
  | Map.size (resFactorCaches st) <= relMaxFactorCaches (resLimits st) =
      st
  | otherwise =
      let victimCount =
            Map.size (resFactorCaches st) - relMaxFactorCaches (resLimits st)
          victims =
            take
              victimCount
              ( fmap fst
                  ( List.sortOn
                      (\(key, entry) -> (fceTouchedAt entry, key))
                      (Map.toList (resFactorCaches st))
                  )
              )
       in st
            { resFactorCaches =
                foldl'
                  (flip Map.delete)
                  (resFactorCaches st)
                  victims
            }
