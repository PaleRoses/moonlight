module Moonlight.Flow.Execution.Prepared.Request
  ( PreparedExecutionKey (..),
    preparedExecutionKey,
    PreparedRequestView (..),
    frontierRestriction,
    ensurePreparedForRequestWith,
    lookupPreparedForRequestWith,
  )
where

import Data.IntSet (IntSet)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( QuerySnapshot,
    footprint,
    liveEpoch,
    liveRelations,
    queryId,
  )
import Moonlight.Flow.Execution.Prepared.Cache
  ( PreparedCacheEntry (BasePreparedEntry, ContextPreparedEntry),
    PreparedCacheKey (BasePreparedKey, ContextPreparedKey),
    ensureBasePreparedWith,
    ensureContextPrepared,
    jcsPrepared,
  )
import Moonlight.Flow.Execution.Prepared.Backend
  ( PreparedBackend (..),
    PreparedJoinCacheState,
    PreparedPlan,
    PreparedScope (BasePreparedScope, ContextPreparedScope),
  )
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Flow.Storage.Restriction
  ( Restriction,
    emptyRestriction,
    restrictRootSlot,
  )

type PreparedExecutionKey :: Type -> Type
data PreparedExecutionKey c = PreparedExecutionKey
  { pekPlanKey :: !PlanCacheKey,
    pekPreparedScope :: !(Maybe (c, QueryId, Int))
  }
  deriving stock (Eq, Ord, Show)

type PreparedRequestView :: Type -> Type -> Type -> Type
data PreparedRequestView c backend projection = PreparedRequestView
  { prvHost :: !(PreparedHost backend),
    prvContext :: !(Maybe (c, QuerySnapshot projection (PreparedRelation backend)))
  }

preparedExecutionKey ::
  PreparedPlan backend ->
  PreparedRequestView c backend projection ->
  PreparedExecutionKey c
preparedExecutionKey plan requestView =
  PreparedExecutionKey
    { pekPlanKey = queryPlanCacheKey plan,
      pekPreparedScope =
        fmap
          ( \(ctx, snapshot) ->
              (ctx, queryId snapshot, liveEpoch snapshot)
          )
          (prvContext requestView)
    }

frontierRestriction ::
  QueryPlan compiled output guard tag tuple key ->
  Maybe IntSet ->
  Restriction
frontierRestriction plan maybeFrontierKeys =
  maybe emptyRestriction (restrictRootSlot (qpRootSlot plan)) maybeFrontierKeys
{-# INLINE frontierRestriction #-}

ensurePreparedForRequestWith ::
  (Ord c, PreparedBackend backend) =>
  backend ->
  PreparedPlan backend ->
  PreparedRequestView c backend projection ->
  PreparedJoinCacheState c backend ->
  Either
    (PreparedObstruction backend)
    (PreparedJoinCacheState c backend, PreparedScope backend)
ensurePreparedForRequestWith backend plan requestView st =
  case prvContext requestView of
    Nothing ->
      do
        (st1, basePrepared) <-
          ensureBasePreparedWith
            queryPlanCacheKey
            (\planValue hostValue -> pbBuildBase backend planValue hostValue)
            plan
            (prvHost requestView)
            st
        Right (st1, BasePreparedScope basePrepared)
    Just (ctx, snapshot) ->
      do
        (st1, contextPrepared) <-
          ensureContextPrepared
            (pbPrepareContext backend)
            ctx
            (queryId snapshot)
            (liveEpoch snapshot)
            (liveRelations snapshot)
            (footprint snapshot)
            st
        Right (st1, ContextPreparedScope contextPrepared)
{-# INLINE ensurePreparedForRequestWith #-}

lookupPreparedForRequestWith ::
  Ord c =>
  PreparedPlan backend ->
  PreparedRequestView c backend projection ->
  PreparedJoinCacheState c backend ->
  Maybe (PreparedScope backend)
lookupPreparedForRequestWith plan requestView st =
  case prvContext requestView of
    Nothing ->
      case Map.lookup (BasePreparedKey (queryPlanCacheKey plan)) (jcsPrepared st) of
        Just (BasePreparedEntry prepared _) ->
          Just (BasePreparedScope prepared)
        _ ->
          Nothing
    Just (ctx, snapshot) ->
      case
        Map.lookup
          (ContextPreparedKey ctx (queryId snapshot) (liveEpoch snapshot))
          (jcsPrepared st)
      of
        Just (ContextPreparedEntry prepared _ _) ->
          Just (ContextPreparedScope prepared)
        _ ->
          Nothing
{-# INLINE lookupPreparedForRequestWith #-}
