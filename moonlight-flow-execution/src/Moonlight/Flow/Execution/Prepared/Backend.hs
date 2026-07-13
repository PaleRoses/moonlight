{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Execution.Prepared.Backend
  ( PreparedBackend (..),
    PreparedPlan,
    PreparedJoinCacheState,
    PreparedScope (..),
    PreparedScopeView (..),
    preparedScopeView,
    preparedScopeFibers,
    preparedScopeStore,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.Kind (Constraint, Type)
import Moonlight.Flow.Execution.Prepared.Cache
  ( JoinCacheState,
  )
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Flow.Storage.Store
  ( Store,
  )

type PreparedScopeView :: Type -> Type
data PreparedScopeView backend = PreparedScopeView
  { psvFibers :: !(IntMap (PreparedFiber backend)),
    psvStore :: !Store
  }

type PreparedBackend :: Type -> Constraint
class PreparedBackend backend where
  type PreparedCompiled backend :: Type
  type PreparedOutput backend :: Type
  type PreparedGuard backend :: Type
  type PreparedTag backend :: Type
  type PreparedTuple backend :: Type
  type PreparedKey backend :: Type
  type PreparedHost backend :: Type
  type PreparedRepair backend :: Type
  type PreparedRelation backend :: Type
  type PreparedBase backend :: Type
  type PreparedContext backend :: Type
  type PreparedFiber backend :: Type
  type PreparedPatch backend :: Type
  type PreparedObstruction backend :: Type

  pbBuildBase ::
    backend ->
    PreparedPlan backend ->
    PreparedHost backend ->
    Either (PreparedObstruction backend) (PreparedBase backend)

  pbPatchBase ::
    backend ->
    PreparedHost backend ->
    PreparedRepair backend ->
    IntSet ->
    PreparedBase backend ->
    Either (PreparedObstruction backend) (PreparedBase backend, PreparedPatch backend)

  pbPrepareContext ::
    backend ->
    IntMap (PreparedRelation backend) ->
    Either (PreparedObstruction backend) (PreparedContext backend)

  pbBaseScopeView ::
    backend ->
    PreparedBase backend ->
    PreparedScopeView backend

  pbContextScopeView ::
    backend ->
    PreparedContext backend ->
    PreparedScopeView backend

type PreparedPlan :: Type -> Type
type PreparedPlan backend =
  QueryPlan
    (PreparedCompiled backend)
    (PreparedOutput backend)
    (PreparedGuard backend)
    (PreparedTag backend)
    (PreparedTuple backend)
    (PreparedKey backend)

type PreparedJoinCacheState :: Type -> Type -> Type
type PreparedJoinCacheState c backend =
  JoinCacheState
    c
    (PreparedPlan backend)
    (PreparedBase backend)
    (PreparedContext backend)
    (PreparedRepair backend)

type PreparedScope :: Type -> Type
data PreparedScope backend
  = BasePreparedScope !(PreparedBase backend)
  | ContextPreparedScope !(PreparedContext backend)

preparedScopeView ::
  PreparedBackend backend =>
  backend ->
  PreparedScope backend ->
  PreparedScopeView backend
preparedScopeView backend prepared =
  case prepared of
    BasePreparedScope basePrepared ->
      pbBaseScopeView backend basePrepared
    ContextPreparedScope contextPrepared ->
      pbContextScopeView backend contextPrepared
{-# INLINE preparedScopeView #-}

preparedScopeFibers ::
  PreparedBackend backend =>
  backend ->
  PreparedScope backend ->
  IntMap (PreparedFiber backend)
preparedScopeFibers backend prepared =
  psvFibers (preparedScopeView backend prepared)
{-# INLINE preparedScopeFibers #-}

preparedScopeStore ::
  PreparedBackend backend =>
  backend ->
  PreparedScope backend ->
  Store
preparedScopeStore backend prepared =
  psvStore (preparedScopeView backend prepared)
{-# INLINE preparedScopeStore #-}
