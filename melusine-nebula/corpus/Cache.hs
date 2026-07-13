{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Execution.Prepared.Cache
  ( PreparedCacheKey (..)
  , PreparedCacheEntry (..)
  , JoinCacheLimits (..)
  , JoinCacheState (..)
  , JoinCacheMetrics (..)
  , emptyJoinCacheState
  , defaultJoinCacheLimits
  , advanceJoinCacheStateWith
  , ensurePlanWith
  , ensureBasePreparedWith
  , ensureContextPrepared
  , touchPreparedEntry
  , insertPreparedEntry
  , evictPreparedKeys
  , prunePreparedLRU
  , lookupAffected
  , contextKeysOnly
  , joinCacheMetrics
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Moonlight.Delta.Scope
  ( ScopedDelta (..)
  , foldDeltaScopeWith
  )
import Moonlight.Core
  ( MatchFootprint
  , mfDeps
  , mfResults
  , mfRoots
  , mfTopo
  )
import Moonlight.Differential.Index.Reverse.Batch
  ( addMembership
  , dropMembership
  , lookupMany
  )
import Moonlight.Flow.Model.Scope
import Moonlight.Flow.Plan.Query.Core

type PreparedCacheKey :: Type -> Type
data PreparedCacheKey c
  = BasePreparedKey !PlanCacheKey
  | ContextPreparedKey !c {-# UNPACK #-} !QueryId {-# UNPACK #-} !Int
  deriving stock (Eq, Ord, Show)

type PreparedCacheEntry :: Type -> Type -> Type
data PreparedCacheEntry basePrepared contextPrepared
  = BasePreparedEntry !basePrepared {-# UNPACK #-} !Word64
  | ContextPreparedEntry !contextPrepared !MatchFootprint {-# UNPACK #-} !Word64

type JoinCacheLimits :: Type
data JoinCacheLimits = JoinCacheLimits
  { jclMaxPreparedEntries :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)

defaultJoinCacheLimits :: JoinCacheLimits
defaultJoinCacheLimits =
  JoinCacheLimits
    { jclMaxPreparedEntries = 256
    }

type JoinCacheState :: Type -> Type -> Type -> Type -> Type -> Type
data JoinCacheState c plan basePrepared contextPrepared repair = JoinCacheState
  { jcsTick :: {-# UNPACK #-} !Word64,
    jcsPlanCache :: !(Map PlanCacheKey plan),
    jcsPrepared :: !(Map (PreparedCacheKey c) (PreparedCacheEntry basePrepared contextPrepared)),
    jcsByDep :: !(IntMap (Set (PreparedCacheKey c))),
    jcsByTopo :: !(IntMap (Set (PreparedCacheKey c))),
    jcsByRoot :: !(IntMap (Set (PreparedCacheKey c))),
    jcsByResult :: !(IntMap (Set (PreparedCacheKey c))),
    jcsLimits :: !JoinCacheLimits
  }

type JoinCacheMetrics :: Type
data JoinCacheMetrics = JoinCacheMetrics
  { jcmPreparedEntries :: {-# UNPACK #-} !Int
  , jcmPlanEntries :: {-# UNPACK #-} !Int
  , jcmDepIndexKeys :: {-# UNPACK #-} !Int
  , jcmDepIndexMembers :: {-# UNPACK #-} !Int
  , jcmTopoIndexKeys :: {-# UNPACK #-} !Int
  , jcmTopoIndexMembers :: {-# UNPACK #-} !Int
  , jcmRootIndexKeys :: {-# UNPACK #-} !Int
  , jcmRootIndexMembers :: {-# UNPACK #-} !Int
  , jcmResultIndexKeys :: {-# UNPACK #-} !Int
  , jcmResultIndexMembers :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)

joinCacheMetrics ::
  JoinCacheState c plan basePrepared contextPrepared repair ->
  JoinCacheMetrics
joinCacheMetrics st =
  JoinCacheMetrics
    { jcmPreparedEntries = Map.size (jcsPrepared st)
    , jcmPlanEntries = Map.size (jcsPlanCache st)
    , jcmDepIndexKeys = IntMap.size (jcsByDep st)
    , jcmDepIndexMembers = countIndexMembers (jcsByDep st)
    , jcmTopoIndexKeys = IntMap.size (jcsByTopo st)
    , jcmTopoIndexMembers = countIndexMembers (jcsByTopo st)
    , jcmRootIndexKeys = IntMap.size (jcsByRoot st)
    , jcmRootIndexMembers = countIndexMembers (jcsByRoot st)
    , jcmResultIndexKeys = IntMap.size (jcsByResult st)
    , jcmResultIndexMembers = countIndexMembers (jcsByResult st)
    }
{-# INLINE joinCacheMetrics #-}

countIndexMembers :: IntMap (Set member) -> Int
countIndexMembers =
  sum . fmap Set.size . IntMap.elems
{-# INLINE countIndexMembers #-}

emptyJoinCacheState :: JoinCacheState c plan basePrepared contextPrepared repair
emptyJoinCacheState =
  emptyJoinCacheStateWith defaultJoinCacheLimits

emptyJoinCacheStateWith :: JoinCacheLimits -> JoinCacheState c plan basePrepared contextPrepared repair
emptyJoinCacheStateWith limits =
  JoinCacheState
    { jcsTick = 0
    , jcsPlanCache = Map.empty
    , jcsPrepared = Map.empty
    , jcsByDep = IntMap.empty
    , jcsByTopo = IntMap.empty
    , jcsByRoot = IntMap.empty
    , jcsByResult = IntMap.empty
    , jcsLimits = limits
    }

advanceJoinCacheStateWith ::
  Ord c =>
  (Maybe payload -> JoinCacheState c plan basePrepared contextPrepared repair -> Set (PreparedCacheKey c)) ->
  (host -> repair -> IntSet -> basePrepared -> Either obstruction (basePrepared, patch)) ->
  ScopedDelta RelationalScope payload ->
  host ->
  Maybe repair ->
  JoinCacheState c plan basePrepared contextPrepared repair ->
  (JoinCacheState c plan basePrepared contextPrepared repair, Map (PreparedCacheKey c) patch)
advanceJoinCacheStateWith affectedPreparedKeys patchBasePrepared delta0 host maybeRepair st =
  let support =
        sdScope delta0
      payload =
        sdPayload delta0
   in foldDeltaScopeWith
        relationalScopeNull
        ( case payload of
            Nothing ->
              (st, Map.empty)
            Just _ ->
              let stEvicted =
                    evictPreparedKeys
                      (affectedPreparedKeys payload st)
                      st
               in (prunePreparedLRU stEvicted, Map.empty)
        )
        ( \dirty ->
          if not (IntSet.null (scopeImpacted dirty))
            then (prunePreparedLRU (clearPreparedCaches st), Map.empty)
            else
              let dirtyResults =
                    scopeResults dirty
                  (stPatched, patches) =
                    patchBaseEntries maybeRepair dirtyResults st
                  stEvicted =
                    evictPreparedKeys
                      (affectedPreparedKeys payload stPatched)
                      stPatched
               in (prunePreparedLRU stEvicted, patches)
        )
        (prunePreparedLRU (clearPreparedCaches st), Map.empty)
        support
  where
    patchBaseEntries Nothing _dirtyResults st' =
      (st', Map.empty)
    patchBaseEntries (Just repair) dirtyResults st'
      | IntSet.null dirtyResults =
          (st', Map.empty)
      | otherwise =
          let (prepared', patches) =
                Map.foldrWithKey
                  patchEntry
                  (Map.empty, Map.empty)
                  (jcsPrepared st')
           in
            ( st' {jcsPrepared = prepared'},
              patches
            )
      where
        patchEntry key entry (preparedAcc, patchesAcc) =
          case entry of
            BasePreparedEntry baseDb touchedAt ->
              case patchBasePrepared host repair dirtyResults baseDb of
                Left _obstruction ->
                  (preparedAcc, patchesAcc)
                Right (baseDb', patch) ->
                  ( Map.insert key (BasePreparedEntry baseDb' touchedAt) preparedAcc,
                    Map.insert key patch patchesAcc
                  )
            ContextPreparedEntry {} ->
              (Map.insert key entry preparedAcc, patchesAcc)

contextKeysOnly :: Set (PreparedCacheKey c) -> Set (PreparedCacheKey c)
contextKeysOnly =
  Set.filter $ \case
    ContextPreparedKey {} -> True
    _ -> False

lookupAffected :: Ord c => IntMap (Set (PreparedCacheKey c)) -> IntSet -> Set (PreparedCacheKey c)
lookupAffected =
  lookupMany
{-# INLINE lookupAffected #-}

clearPreparedCaches ::
  JoinCacheState c plan basePrepared contextPrepared repair ->
  JoinCacheState c plan basePrepared contextPrepared repair
clearPreparedCaches st =
  st
    { jcsPrepared = Map.empty
    , jcsByDep = IntMap.empty
    , jcsByTopo = IntMap.empty
    , jcsByRoot = IntMap.empty
    , jcsByResult = IntMap.empty
    }

nextTick :: JoinCacheState c plan basePrepared contextPrepared repair -> (Word64, JoinCacheState c plan basePrepared contextPrepared repair)
nextTick st =
  let t = jcsTick st + 1
   in (t, st {jcsTick = t})
{-# INLINE nextTick #-}

touchPreparedEntry ::
  Ord c =>
  PreparedCacheKey c ->
  JoinCacheState c plan basePrepared contextPrepared repair ->
  JoinCacheState c plan basePrepared contextPrepared repair

type PreparedEntry :: Type -> Type -> Type -> Type -> Type -> Type
type PreparedEntry c plan basePrepared contextPrepared repair =
  PreparedCacheEntry basePrepared contextPrepared

entryTouchedAt :: PreparedEntry c plan basePrepared contextPrepared repair -> Word64
entryTouchedAt (BasePreparedEntry _ t) = t
entryTouchedAt (ContextPreparedEntry _ _ t) = t

entryFootprint :: PreparedEntry c plan basePrepared contextPrepared repair -> Maybe MatchFootprint
entryFootprint (BasePreparedEntry _ _) = Nothing
entryFootprint (ContextPreparedEntry _ fp _) = Just fp

touchEntry :: Word64 -> PreparedEntry c plan basePrepared contextPrepared repair -> PreparedEntry c plan basePrepared contextPrepared repair
touchEntry t (BasePreparedEntry db _) = BasePreparedEntry db t
touchEntry t (ContextPreparedEntry db fp _) = ContextPreparedEntry db fp t

touchPreparedEntry key st =
  case Map.lookup key (jcsPrepared st) of
    Nothing -> st
    Just entry ->
      let (t, st1) = nextTick st
       in st1
            { jcsPrepared =
                Map.insert key (touchEntry t entry) (jcsPrepared st1)
            }
{-# INLINE touchPreparedEntry #-}

removeEntryIndexes ::
  Ord c =>
  PreparedCacheKey c ->
  PreparedEntry c plan basePrepared contextPrepared repair ->
  JoinCacheState c plan basePrepared contextPrepared repair ->
  JoinCacheState c plan basePrepared contextPrepared repair
removeEntryIndexes key entry st =
  case entryFootprint entry of
    Nothing -> st
    Just fp ->
      deleteIx mfDeps jcsByDep setByDep key fp $
        deleteIx mfTopo jcsByTopo setByTopo key fp $
          deleteIx mfRoots jcsByRoot setByRoot key fp $
            deleteIx mfResults jcsByResult setByResult key fp $
              st

insertEntryIndexes ::
  Ord c =>
  PreparedCacheKey c ->
  PreparedEntry c plan basePrepared contextPrepared repair ->
  JoinCacheState c plan basePrepared contextPrepared repair ->
  JoinCacheState c plan basePrepared contextPrepared repair
insertEntryIndexes key entry st =
  case entryFootprint entry of
    Nothing -> st
    Just fp ->
      addIx mfDeps jcsByDep setByDep key fp $
        addIx mfTopo jcsByTopo setByTopo key fp $
          addIx mfRoots jcsByRoot setByRoot key fp $
            addIx mfResults jcsByResult setByResult key fp $
              st

insertPreparedEntry ::
  Ord c =>
  PreparedCacheKey c ->
  PreparedEntry c plan basePrepared contextPrepared repair ->
  JoinCacheState c plan basePrepared contextPrepared repair ->
  JoinCacheState c plan basePrepared contextPrepared repair
insertPreparedEntry key entry st =
  let st1 =
        case Map.lookup key (jcsPrepared st) of
          Nothing -> st
          Just existing -> removeEntryIndexes key existing st
      st2 =
        st1
          { jcsPrepared =
              Map.insert key entry (jcsPrepared st1)
          }
   in insertEntryIndexes key entry st2
{-# INLINE insertPreparedEntry #-}

evictPreparedKeys ::
  Ord c =>
  Set (PreparedCacheKey c) ->
  JoinCacheState c plan basePrepared contextPrepared repair ->
  JoinCacheState c plan basePrepared contextPrepared repair
evictPreparedKeys keys st =
  Set.foldr
    evictPreparedKey
    st
    keys
  where
    evictPreparedKey ::
      Ord c =>
      PreparedCacheKey c ->
      JoinCacheState c plan basePrepared contextPrepared repair ->
      JoinCacheState c plan basePrepared contextPrepared repair
    evictPreparedKey key st' =
      case Map.lookup key (jcsPrepared st') of
        Nothing -> st'
        Just entry ->
          removeEntryIndexes key entry $
            st' {jcsPrepared = Map.delete key (jcsPrepared st')}
{-# INLINE evictPreparedKeys #-}

prunePreparedLRU ::
  Ord c =>
  JoinCacheState c plan basePrepared contextPrepared repair ->
  JoinCacheState c plan basePrepared contextPrepared repair
prunePreparedLRU st
  | Map.size (jcsPrepared st) <= jclMaxPreparedEntries (jcsLimits st) = st
  | otherwise =
      let victims =
            take
              (Map.size (jcsPrepared st) - jclMaxPreparedEntries (jcsLimits st))
              ( fmap fst
                  ( List.sortOn
                      (\(key, entry) -> (entryTouchedAt entry, key))
                      (Map.toList (jcsPrepared st))
                  )
              )
       in evictPreparedKeys (Set.fromList victims) st

deleteIx ::
  Ord c =>
  (MatchFootprint -> IntSet) ->
  (JoinCacheState c plan basePrepared contextPrepared repair -> IntMap (Set (PreparedCacheKey c))) ->
  (IntMap (Set (PreparedCacheKey c)) -> JoinCacheState c plan basePrepared contextPrepared repair -> JoinCacheState c plan basePrepared contextPrepared repair) ->
  PreparedCacheKey c ->
  MatchFootprint ->
  JoinCacheState c plan basePrepared contextPrepared repair ->
  JoinCacheState c plan basePrepared contextPrepared repair
deleteIx project getIx setIx key fp st =
  setIx
    (dropMembership key (project fp) (getIx st))
    st

addIx ::
  Ord c =>
  (MatchFootprint -> IntSet) ->
  (JoinCacheState c plan basePrepared contextPrepared repair -> IntMap (Set (PreparedCacheKey c))) ->
  (IntMap (Set (PreparedCacheKey c)) -> JoinCacheState c plan basePrepared contextPrepared repair -> JoinCacheState c plan basePrepared contextPrepared repair) ->
  PreparedCacheKey c ->
  MatchFootprint ->
  JoinCacheState c plan basePrepared contextPrepared repair ->
  JoinCacheState c plan basePrepared contextPrepared repair
addIx project getIx setIx key fp st =
  setIx
    (addMembership key (project fp) (getIx st))
    st

setByDep :: IntMap (Set (PreparedCacheKey c)) -> JoinCacheState c plan basePrepared contextPrepared repair -> JoinCacheState c plan basePrepared contextPrepared repair
setByDep ix st = st {jcsByDep = ix}

setByTopo :: IntMap (Set (PreparedCacheKey c)) -> JoinCacheState c plan basePrepared contextPrepared repair -> JoinCacheState c plan basePrepared contextPrepared repair
setByTopo ix st = st {jcsByTopo = ix}

setByRoot :: IntMap (Set (PreparedCacheKey c)) -> JoinCacheState c plan basePrepared contextPrepared repair -> JoinCacheState c plan basePrepared contextPrepared repair
setByRoot ix st = st {jcsByRoot = ix}

setByResult :: IntMap (Set (PreparedCacheKey c)) -> JoinCacheState c plan basePrepared contextPrepared repair -> JoinCacheState c plan basePrepared contextPrepared repair
setByResult ix st = st {jcsByResult = ix}

ensurePlanWith ::
  (query -> Either obstruction plan) ->
  (plan -> PlanCacheKey) ->
  query ->
  JoinCacheState c plan basePrepared contextPrepared repair ->
  Either obstruction (JoinCacheState c plan basePrepared contextPrepared repair, plan)
ensurePlanWith compilePlan planCacheKey queryValue st = do
  plan <- compilePlan queryValue
  let key = planCacheKey plan
  Right $
    case Map.lookup key (jcsPlanCache st) of
      Just cachedPlan ->
        (st, cachedPlan)
      Nothing ->
        ( st
            { jcsPlanCache =
                Map.insert key plan (jcsPlanCache st)
            }
        , plan
        )

ensureBasePreparedWith ::
  Ord c =>
  (plan -> PlanCacheKey) ->
  (plan -> host -> Either obstruction basePrepared) ->
  plan ->
  host ->
  JoinCacheState c plan basePrepared contextPrepared repair ->
  Either obstruction (JoinCacheState c plan basePrepared contextPrepared repair, basePrepared)
ensureBasePreparedWith planCacheKey buildBasePrepared plan host st =
  let key = BasePreparedKey (planCacheKey plan)
   in case Map.lookup key (jcsPrepared st) of
        Just (BasePreparedEntry baseDb _) ->
          Right (touchPreparedEntry key st, baseDb)
        _ ->
          do
            baseDb <- buildBasePrepared plan host
            let (t, st1) = nextTick st
                entry = BasePreparedEntry baseDb t
                st2 = insertPreparedEntry key entry st1
            Right (prunePreparedLRU st2, baseDb)

ensureContextPrepared ::
  Ord c =>
  (IntMap relation -> contextPrepared) ->
  c ->
  QueryId ->
  Int ->
  IntMap relation ->
  MatchFootprint ->
  JoinCacheState c plan basePrepared contextPrepared repair ->
  (JoinCacheState c plan basePrepared contextPrepared repair, contextPrepared)
ensureContextPrepared prepareContext ctx qid liveEpoch rels fp st =
  let key = ContextPreparedKey ctx qid liveEpoch
   in case Map.lookup key (jcsPrepared st) of
        Just (ContextPreparedEntry prepared _ _) ->
          (touchPreparedEntry key st, prepared)
        _ ->
          let prepared = prepareContext rels
              (t, st1) = nextTick st
              entry = ContextPreparedEntry prepared fp t
              st2 = insertPreparedEntry key entry st1
           in (prunePreparedLRU st2, prepared)
