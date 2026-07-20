{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Moonlight.Flow.Execution.Prepared.Matching
  ( CachedRequestOps (..),
    CachedJoinObstruction (..),
    runCachedJoinQueryBatchWith,
    cachedJoinMatchingAlgebraWith,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.Bifunctor
  ( first,
  )
import Data.Containers.ListUtils
  ( nubOrd,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Moonlight.Delta.Scope
  ( Scoped,
    scopeKeys,
    scopedDeltaSupport,
  )
import Moonlight.Flow.Execution.Prepared.Cache
  ( PreparedCacheKey,
    advanceJoinCacheStateWith,
    ensurePlanWith,
  )
import Moonlight.Flow.Execution.Factor.Run
  ( FactorRunError (..),
  )
import Moonlight.Flow.Execution.Prepared.Backend
  ( PreparedBackend (..),
    PreparedJoinCacheState,
    PreparedPlan,
    PreparedScope,
    preparedScopeStore,
  )
import Moonlight.Flow.Execution.Prepared.Run
  ( PreparedOp (..),
    PreparedResult (..),
    PreparedRunMode (..),
    PreparedRunSpec (..),
    runPrepared,
  )
import Moonlight.Flow.Execution.Prepared.Request
  ( PreparedExecutionKey,
    PreparedRequestView,
    ensurePreparedForRequestWith,
    frontierRestriction,
    preparedExecutionKey,
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Saturation.Matching
  ( MatchingAlgebra (..),
    MatchingQuery (..),
  )
import Moonlight.Flow.Storage.View
  ( unrestrictedView,
  )
import Moonlight.Flow.Model.Scope

type CachedRequestOps :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data CachedRequestOps request c backend projection query match = CachedRequestOps
  { croQuery :: request -> query,
    croView :: request -> PreparedRequestView c backend projection,
    croFilterMatches :: request -> PreparedPlan backend -> [PreparedOutput backend] -> [match]
  }

data CachedJoinObstruction preparedObstruction compileObstruction
  = CachedJoinCompileObstruction !compileObstruction
  | CachedJoinPreparedObstruction !preparedObstruction
  | CachedJoinPreparedRunError !FactorRunError
  | CachedJoinOutputProjectionObstruction !OutputProjectionObstruction
  deriving stock (Eq, Show)

type RequestGroup :: Type -> Type -> Type
data RequestGroup key request = RequestGroup
  { rgKey :: !key,
    rgRequests :: ![(Int, request)]
  }

collectRequestGroups ::
  Ord key =>
  [(Int, key, request)] ->
  [RequestGroup key request]
collectRequestGroups entries =
  [ RequestGroup
      { rgKey = key,
        rgRequests = Map.findWithDefault [] key grouped
      }
    | key <- nubOrd [key | (_requestIndex, key, _requestValue) <- entries]
  ]
  where
    grouped =
      Map.fromListWith
        (flip (<>))
        [ (key, [(requestIndex, requestValue)])
          | (requestIndex, key, requestValue) <- entries
        ]
{-# INLINE collectRequestGroups #-}

type CachedExecutionGroupKey :: Type -> Type -> Type -> Type
data CachedExecutionGroupKey c backend projection = CachedExecutionGroupKey
  { cegExecutionKey :: !(PreparedExecutionKey c),
    cegPlan :: !(PreparedPlan backend),
    cegView :: !(PreparedRequestView c backend projection)
  }

instance Eq c => Eq (CachedExecutionGroupKey c backend projection) where
  left == right =
    cegExecutionKey left == cegExecutionKey right
  {-# INLINE (==) #-}

instance Ord c => Ord (CachedExecutionGroupKey c backend projection) where
  compare left right =
    compare (cegExecutionKey left) (cegExecutionKey right)
  {-# INLINE compare #-}

collectCachedRequestGroups ::
  Ord c =>
  (query -> Either compileObstruction (PreparedPlan backend)) ->
  CachedRequestOps request c backend projection query match ->
  [request] ->
  PreparedJoinCacheState c backend ->
  ( PreparedJoinCacheState c backend,
    Either compileObstruction [RequestGroup (CachedExecutionGroupKey c backend projection) request]
  )
collectCachedRequestGroups compilePlan requestOps requests st0 =
  case foldM compileRequest (st0, []) (zip [0 :: Int ..] requests) of
    Left (stateAfterCompile, obstruction) ->
      (stateAfterCompile, Left obstruction)
    Right (stateAfterCompile, entriesRev) ->
      (stateAfterCompile, Right (collectRequestGroups (reverse entriesRev)))
  where
    compileRequest (st, entriesRev) (requestIndex, requestValue) =
      case
        ensurePlanWith
          compilePlan
          queryPlanCacheKey
          (croQuery requestOps requestValue)
          st
      of
        Left obstruction ->
          Left (st, obstruction)
        Right (stateWithPlan, plan) ->
          let view =
                croView requestOps requestValue
              groupKey =
                CachedExecutionGroupKey
                  { cegExecutionKey = preparedExecutionKey plan view,
                    cegPlan = plan,
                    cegView = view
                  }
           in Right (stateWithPlan, (requestIndex, groupKey, requestValue) : entriesRev)
{-# INLINE collectCachedRequestGroups #-}

runCachedJoinQueryBatchWith ::
  ( Ord c,
    PreparedBackend backend,
    QueryOutput (PreparedOutput backend) (PreparedKey backend)
  ) =>
  (query -> Either compileObstruction (PreparedPlan backend)) ->
  backend ->
  CachedRequestOps request c backend projection query match ->
  Maybe IntSet ->
  [request] ->
  PreparedJoinCacheState c backend ->
  ( PreparedJoinCacheState c backend,
    Either (CachedJoinObstruction (PreparedObstruction backend) compileObstruction) [[match]]
  )
runCachedJoinQueryBatchWith compilePlan backend requestOps wantedRoots requests st0 =
  case collectCachedRequestGroups compilePlan requestOps requests st0 of
    (stAfterCompile, Left obstruction) ->
      (stAfterCompile, Left (CachedJoinCompileObstruction obstruction))
    (stAfterCompile, Right requestGroups) ->
      case foldM runRequestGroup (stAfterCompile, IntMap.empty) requestGroups of
        Left (stateAfterGroups, obstruction) ->
          (stateAfterGroups, Left obstruction)
        Right (stateAfterGroups, indexedResults) ->
          (stateAfterGroups, Right (fmap snd (IntMap.toAscList indexedResults)))
  where
    runRequestGroup (currentState, resultMap) group =
      let groupKey =
            rgKey group
          plan =
            cegPlan groupKey
       in case ensurePreparedForRequestWith backend plan (cegView groupKey) currentState of
            Left obstruction ->
              Left (currentState, CachedJoinPreparedObstruction obstruction)
            Right (stateWithPrepared, prepared) ->
              case first CachedJoinPreparedRunError $
                runPreparedRowsWith backend plan wantedRoots prepared
              of
                Left obstruction ->
                  Left (stateWithPrepared, obstruction)
                Right rows ->
                  case
                    first
                      CachedJoinOutputProjectionObstruction
                      (projectQueryPlanOutputs plan rows)
                  of
                    Left obstruction ->
                      Left (stateWithPrepared, obstruction)
                    Right structuralMatches ->
                      let batchResults =
                            IntMap.fromList
                              [ ( requestIndex,
                                  croFilterMatches requestOps requestValue plan structuralMatches
                                )
                                | (requestIndex, requestValue) <- rgRequests group
                              ]
                       in Right (stateWithPrepared, IntMap.union batchResults resultMap)

cachedJoinMatchingAlgebraWith ::
  forall environment c backend projection query match request advance payload world compileObstruction.
  ( Ord c,
    PreparedBackend backend,
    QueryOutput (PreparedOutput backend) (PreparedKey backend)
  ) =>
  environment ->
  PreparedJoinCacheState c backend ->
  (query -> Either compileObstruction (PreparedPlan backend)) ->
  backend ->
  (forall token. CachedRequestOps (request token) c backend projection query match) ->
  (Maybe payload -> PreparedJoinCacheState c backend -> Set (PreparedCacheKey c)) ->
  (Scoped IntSet payload -> Scoped RelationalScope payload) ->
  (forall token. advance token -> PreparedHost backend) ->
  (forall token. advance token -> Maybe (PreparedRepair backend)) ->
  MatchingAlgebra environment (PreparedJoinCacheState c backend) IntSet payload world request advance (CachedJoinObstruction (PreparedObstruction backend) compileObstruction) match
cachedJoinMatchingAlgebraWith environment initialState compilePlan backend requestOps affectedPreparedKeys cacheDelta extractAdvanceHost extractAdvanceRepair =
  MatchingAlgebra
    { maInitialState = initialState,
      maEnvironment = environment,
      maPrepareQueries = \state delta _world requests ->
        (state, [MatchingQuery (scopedDeltaSupport delta) req | req <- requests]),
      maRunQueries = \cacheState _world queries ->
        let requestGroups =
              collectRequestGroups
                [ ( requestIndex,
                    scopeKeys (mqScope queryValue),
                    mqRequest queryValue
                  )
                  | (requestIndex, queryValue) <- zip [0 :: Int ..] queries
                ]
            runScopeGroup (currentState, resultMap) group =
              let (nextState, groupResults) =
                    runCachedJoinQueryBatchWith
                      compilePlan
                      backend
                      requestOps
                      (rgKey group)
                      (fmap snd (rgRequests group))
                      currentState
               in case groupResults of
                    Left obstruction ->
                      Left (nextState, obstruction)
                    Right groupMatches ->
                      let indexedGroupMatches =
                            IntMap.fromList
                              [ (requestIndex, requestMatches)
                                | ((requestIndex, _requestValue), requestMatches) <-
                                    zip (rgRequests group) groupMatches
                              ]
                       in Right (nextState, IntMap.union indexedGroupMatches resultMap)
         in case foldM runScopeGroup (cacheState, IntMap.empty) requestGroups of
              Left (stateAfterGroups, obstruction) ->
                (stateAfterGroups, Left obstruction)
              Right (stateAfterGroups, indexedResults) ->
                ( stateAfterGroups,
                  Right (fmap snd (IntMap.toAscList indexedResults))
                ),
      maPreviewQuery = \_ _ _ -> Nothing,
      maAdvanceState =
        \matchingDelta advance cacheState ->
          fst
            ( advanceJoinCacheStateWith
                affectedPreparedKeys
                (pbPatchBase backend)
                (cacheDelta matchingDelta)
                (extractAdvanceHost advance)
                (extractAdvanceRepair advance)
                cacheState
            ),
      maReplayDiagnostics = const Nothing
    }

runPreparedRowsWith ::
  PreparedBackend backend =>
  backend ->
  PreparedPlan backend ->
  Maybe IntSet ->
  PreparedScope backend ->
  Either FactorRunError [RowTupleKey]
runPreparedRowsWith backend plan wantedRoots prepared =
  let restriction =
        frontierRestriction plan wantedRoots
      store =
        preparedScopeStore backend prepared
      view =
        unrestrictedView
      spec =
        PreparedRunSpec
          { prsPlan = plan,
            prsRestriction = restriction,
            prsStore = store,
            prsView = view,
            prsAtomDeltas = IntMap.empty,
            prsStructuralSources = Nothing,
            prsOp = PreparedRows Nothing,
            prsMode = PreparedValueOnly
          }
   in prValue <$> runPrepared spec
