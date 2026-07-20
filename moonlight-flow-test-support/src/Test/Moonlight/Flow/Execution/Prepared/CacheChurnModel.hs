{-# LANGUAGE BangPatterns #-}

module Test.Moonlight.Flow.Execution.Prepared.CacheChurnModel
  ( ChurnOp (..),
    ChurnState,
    footprintWidth,
    cacheStateWithLimit,
    preparedKeyFor,
    churnOpAt,
    applyChurnOp,
    applyChurnOps,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Void
  ( Void,
    absurd,
  )
import Moonlight.Core
  ( MatchFootprint (..),
  )
import Moonlight.Core
  ( mkQueryId,
  )
import Moonlight.Flow.Execution.Prepared.Cache
  ( JoinCacheLimits (..),
    JoinCacheState (..),
    PreparedCacheKey (..),
    emptyJoinCacheState,
    ensureContextPrepared,
    evictPreparedKeys,
  )

type ChurnState =
  JoinCacheState Int () () Int ()

data ChurnOp
  = ChurnSubscribe !Int !Int !Int
  | ChurnUnsubscribe !Int !Int !Int
  deriving stock (Eq, Show)

footprintWidth :: Int
footprintWidth =
  4

cacheStateWithLimit :: Int -> ChurnState
cacheStateWithLimit rawLimit =
  emptyJoinCacheState
    { jcsLimits =
        JoinCacheLimits
          { jclMaxPreparedEntries = max 0 rawLimit
          }
    }

preparedKeyFor :: Int -> Int -> Int -> PreparedCacheKey Int
preparedKeyFor ctx queryKey liveEpoch =
  ContextPreparedKey ctx (mkQueryId queryKey) liveEpoch
{-# INLINE preparedKeyFor #-}

churnOpAt :: Int -> Int -> ChurnOp
churnOpAt rawWorkingSet rawIteration =
  let !workingSet =
        max 1 rawWorkingSet
      !iteration =
        max 0 rawIteration
      !contextKey =
        iteration `rem` workingSet
      !queryKey =
        (iteration * 17 + 11) `rem` (workingSet + 5)
      !oldEpoch =
        iteration `rem` (workingSet + 3)
      !newEpoch =
        (iteration + 1) `rem` (workingSet + 3)
   in case iteration `rem` 3 of
        0 ->
          ChurnSubscribe contextKey queryKey oldEpoch
        1 ->
          ChurnUnsubscribe contextKey queryKey oldEpoch
        _ ->
          ChurnSubscribe contextKey queryKey newEpoch
{-# INLINE churnOpAt #-}

applyChurnOp :: ChurnOp -> ChurnState -> ChurnState
applyChurnOp op st =
  case op of
    ChurnSubscribe ctx queryKey liveEpoch ->
      let footprint =
            footprintFor ctx queryKey liveEpoch
          eitherPrepared =
            ensureContextPrepared
              prepareContextStubEither
              ctx
              (mkQueryId queryKey)
              liveEpoch
              IntMap.empty
              footprint
              st
       in case eitherPrepared of
            Left impossible ->
              absurd impossible
            Right (st', !prepared) ->
              prepared `seq` st'
    ChurnUnsubscribe ctx queryKey liveEpoch ->
      evictPreparedKeys
        (Set.singleton (preparedKeyFor ctx queryKey liveEpoch))
        st
{-# INLINE applyChurnOp #-}

applyChurnOps :: [ChurnOp] -> ChurnState -> ChurnState
applyChurnOps =
  flip (List.foldl' (flip applyChurnOp))
{-# INLINE applyChurnOps #-}

prepareContextStub :: IntMap relation -> Int
prepareContextStub _relations =
  1
{-# INLINE prepareContextStub #-}

prepareContextStubEither :: IntMap relation -> Either Void Int
prepareContextStubEither =
  Right . prepareContextStub
{-# INLINE prepareContextStubEither #-}

footprintFor :: Int -> Int -> Int -> MatchFootprint
footprintFor ctx queryKey liveEpoch =
  let !base =
        ctx * 4099 + queryKey * 97 + liveEpoch * 13
      keys offset =
        IntSet.fromList
          [ base + offset,
            base + offset + 1,
            base + offset + 2,
            base + offset + 3
          ]
   in MatchFootprint
        { mfRoots = keys 0,
          mfDeps = keys 11,
          mfTopo = keys 23,
          mfResults = keys 37
        }
{-# INLINE footprintFor #-}
