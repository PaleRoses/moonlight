{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Saturation.Context.Runtime.Match.Batch
  ( MatchBatch (..),
    emptyMatchBatch,
    singletonMatchBatch,
    matchBatchFromList,
    matchBatchFromVector,
    matchBatchToList,
    matchBatchToVector,
    matchBatchNull,
    matchBatchLength,
    matchBatchNonEmpty,
    matchBatchAppend,
    matchBatchFilter,
    matchBatchFoldl',
    countMatchBatchBy,
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as Vector

type MatchBatch :: Type -> Type
newtype MatchBatch match = MatchBatch
  { unMatchBatch :: Vector match
  }
  deriving stock (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

emptyMatchBatch :: MatchBatch match
emptyMatchBatch =
  MatchBatch Vector.empty
{-# INLINE emptyMatchBatch #-}

singletonMatchBatch :: match -> MatchBatch match
singletonMatchBatch =
  MatchBatch . Vector.singleton
{-# INLINE singletonMatchBatch #-}

matchBatchFromList :: [match] -> MatchBatch match
matchBatchFromList =
  MatchBatch . Vector.fromList
{-# INLINE matchBatchFromList #-}

matchBatchFromVector :: Vector match -> MatchBatch match
matchBatchFromVector =
  MatchBatch
{-# INLINE matchBatchFromVector #-}

matchBatchToList :: MatchBatch match -> [match]
matchBatchToList =
  Vector.toList . unMatchBatch
{-# INLINE matchBatchToList #-}

matchBatchToVector :: MatchBatch match -> Vector match
matchBatchToVector =
  unMatchBatch
{-# INLINE matchBatchToVector #-}

matchBatchNull :: MatchBatch match -> Bool
matchBatchNull =
  Vector.null . unMatchBatch
{-# INLINE matchBatchNull #-}

matchBatchLength :: MatchBatch match -> Int
matchBatchLength =
  Vector.length . unMatchBatch
{-# INLINE matchBatchLength #-}

matchBatchNonEmpty :: MatchBatch match -> Maybe (NonEmpty match)
matchBatchNonEmpty batch =
  case Vector.uncons (unMatchBatch batch) of
    Nothing ->
      Nothing
    Just (headMatch, tailMatches) ->
      Just (headMatch :| Vector.toList tailMatches)
{-# INLINE matchBatchNonEmpty #-}

matchBatchAppend ::
  MatchBatch match ->
  MatchBatch match ->
  MatchBatch match
matchBatchAppend leftBatch rightBatch =
  MatchBatch
    (unMatchBatch leftBatch Vector.++ unMatchBatch rightBatch)
{-# INLINE matchBatchAppend #-}

matchBatchFilter ::
  (match -> Bool) ->
  MatchBatch match ->
  MatchBatch match
matchBatchFilter predicate =
  MatchBatch . Vector.filter predicate . unMatchBatch
{-# INLINE matchBatchFilter #-}

matchBatchFoldl' ::
  (acc -> match -> acc) ->
  acc ->
  MatchBatch match ->
  acc
matchBatchFoldl' step initial =
  Vector.foldl' step initial . unMatchBatch
{-# INLINE matchBatchFoldl' #-}

countMatchBatchBy ::
  Ord group =>
  (match -> group) ->
  MatchBatch match ->
  Map group Int
countMatchBatchBy groupOf =
  matchBatchFoldl'
    ( \counts match ->
        Map.insertWith (+) (groupOf match) 1 counts
    )
    Map.empty
{-# INLINE countMatchBatchBy #-}
