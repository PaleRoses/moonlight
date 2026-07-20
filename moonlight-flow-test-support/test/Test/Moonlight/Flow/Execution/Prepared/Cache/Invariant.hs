module Test.Moonlight.Flow.Execution.Prepared.Cache.Invariant
  ( PreparedCacheInvariantError (..)
  , validatePreparedCacheInvariants
  ) where

import Data.Foldable
  ( for_
  )
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Moonlight.Core
  ( MatchFootprint
  , mfDeps
  , mfResults
  , mfRoots
  , mfTopo
  )
import Moonlight.Flow.Execution.Prepared.Cache
  ( JoinCacheState (..)
  , PreparedCacheEntry (..)
  , PreparedCacheKey (..)
  )
import Moonlight.Differential.Index.Reverse.Batch
  ( validateIntAxisFromMap
  )

type PreparedCacheInvariantError :: Type -> Type
data PreparedCacheInvariantError c
  = PreparedCacheDepIndexMismatch
      !(IntMap (Set (PreparedCacheKey c)))
      !(IntMap (Set (PreparedCacheKey c)))
  | PreparedCacheTopoIndexMismatch
      !(IntMap (Set (PreparedCacheKey c)))
      !(IntMap (Set (PreparedCacheKey c)))
  | PreparedCacheRootIndexMismatch
      !(IntMap (Set (PreparedCacheKey c)))
      !(IntMap (Set (PreparedCacheKey c)))
  | PreparedCacheResultIndexMismatch
      !(IntMap (Set (PreparedCacheKey c)))
      !(IntMap (Set (PreparedCacheKey c)))
  | PreparedCacheIndexedKeyMissing !(PreparedCacheKey c)
  | PreparedCacheBaseKeyIndexed !(PreparedCacheKey c)
  deriving stock (Eq, Show)

validatePreparedCacheInvariants ::
  Ord c =>
  JoinCacheState c plan basePrepared contextPrepared repair ->
  Either (PreparedCacheInvariantError c) ()
validatePreparedCacheInvariants st =
  checkAxis (jcsPrepared st) PreparedCacheDepIndexMismatch mfDeps (jcsByDep st)
    *> checkAxis (jcsPrepared st) PreparedCacheTopoIndexMismatch mfTopo (jcsByTopo st)
    *> checkAxis (jcsPrepared st) PreparedCacheRootIndexMismatch mfRoots (jcsByRoot st)
    *> checkAxis (jcsPrepared st) PreparedCacheResultIndexMismatch mfResults (jcsByResult st)
    *> checkAllIndexedKeys st

checkAxis ::
  Ord c =>
  Map (PreparedCacheKey c) (PreparedCacheEntry basePrepared contextPrepared) ->
  ( IntMap (Set (PreparedCacheKey c)) ->
    IntMap (Set (PreparedCacheKey c)) ->
    PreparedCacheInvariantError c
  ) ->
  (MatchFootprint -> IntSet) ->
  IntMap (Set (PreparedCacheKey c)) ->
  Either (PreparedCacheInvariantError c) ()
checkAxis prepared mkMismatch project =
  validateIntAxisFromMap
    mkMismatch
    (entryAxis project)
    prepared
{-# INLINE checkAxis #-}

entryAxis ::
  (MatchFootprint -> IntSet) ->
  PreparedCacheKey c ->
  PreparedCacheEntry basePrepared contextPrepared ->
  IntSet
entryAxis _ _ (BasePreparedEntry _ _) =
  IntSet.empty
entryAxis project _ (ContextPreparedEntry _ footprint _) =
  project footprint
{-# INLINE entryAxis #-}

checkAllIndexedKeys ::
  Ord c =>
  JoinCacheState c plan basePrepared contextPrepared repair ->
  Either (PreparedCacheInvariantError c) ()
checkAllIndexedKeys st =
  for_ axes $ \axisIndex ->
    for_ axisIndex $ \members ->
      for_ members checkOne
  where
    axes =
      [ jcsByDep st
      , jcsByTopo st
      , jcsByRoot st
      , jcsByResult st
      ]
    checkOne key =
      case Map.lookup key (jcsPrepared st) of
        Nothing ->
          Left (PreparedCacheIndexedKeyMissing key)
        Just (BasePreparedEntry _ _) ->
          Left (PreparedCacheBaseKeyIndexed key)
        Just (ContextPreparedEntry {}) ->
          Right ()
{-# INLINE checkAllIndexedKeys #-}
