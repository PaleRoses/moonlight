{-# LANGUAGE LambdaCase #-}

module Moonlight.Rewrite.Kernel.Subst
  ( TermSubst (..),
    emptyTermSubst,
    nullTermSubst,
    termSubstFromList,
    termSubstToAscList,
    termSubstDomain,
    lookupTermSubst,
    insertTermSubst,
    applyTermSubst,
    resolveTermSubst,
    resolvedTermSubst,
    composeTermSubst,
    restrictTermSubst,
  )
where

import Data.IntMap.Lazy qualified as IntMapLazy
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.Kind (Type)
import Moonlight.Core
  ( Pattern (..),
    PatternVar,
    patternVarKey,
  )
import Moonlight.Core qualified as EGraph

type TermSubst :: (Type -> Type) -> Type
newtype TermSubst f = TermSubst
  { unTermSubst :: IntMap (Pattern f)
  }

deriving stock instance Eq (Pattern f) => Eq (TermSubst f)

deriving stock instance Ord (Pattern f) => Ord (TermSubst f)

deriving stock instance Show (Pattern f) => Show (TermSubst f)

instance Functor f => Semigroup (TermSubst f) where
  outer <> inner =
    composeTermSubst outer inner

instance Functor f => Monoid (TermSubst f) where
  mempty =
    emptyTermSubst

emptyTermSubst :: TermSubst f
emptyTermSubst =
  TermSubst IntMap.empty

nullTermSubst :: TermSubst f -> Bool
nullTermSubst =
  IntMap.null . unTermSubst

termSubstFromList :: [(PatternVar, Pattern f)] -> TermSubst f
termSubstFromList bindings =
  TermSubst
    (IntMap.fromList
       [ (patternVarKey patternVar, patternValue)
         | (patternVar, patternValue) <- bindings
       ])

termSubstToAscList :: TermSubst f -> [(PatternVar, Pattern f)]
termSubstToAscList (TermSubst bindings) =
  [ (EGraph.mkPatternVar varKey, patternValue)
    | (varKey, patternValue) <- IntMap.toAscList bindings
  ]

termSubstDomain :: TermSubst f -> IntSet
termSubstDomain =
  IntMap.keysSet . unTermSubst

lookupTermSubst :: PatternVar -> TermSubst f -> Maybe (Pattern f)
lookupTermSubst patternVar =
  IntMap.lookup (patternVarKey patternVar) . unTermSubst

insertTermSubst :: PatternVar -> Pattern f -> TermSubst f -> TermSubst f
insertTermSubst patternVar patternValue =
  TermSubst . IntMap.insert (patternVarKey patternVar) patternValue . unTermSubst

applyTermSubst :: Functor f => TermSubst f -> Pattern f -> Pattern f
applyTermSubst (TermSubst bindings)
  | IntMap.null bindings =
      id
  | otherwise =
      go
  where
    go =
      \case
        PatternVar patternVar ->
          IntMap.findWithDefault (PatternVar patternVar) (patternVarKey patternVar) bindings
        PatternNode patternNode ->
          PatternNode (fmap go patternNode)

resolveTermSubst :: Functor f => TermSubst f -> Pattern f -> Pattern f
resolveTermSubst (TermSubst bindings)
  | IntMap.null bindings =
      id
  | otherwise =
      go
  where
    resolved =
      IntMapLazy.map go bindings

    go =
      \case
        PatternVar patternVar ->
          IntMapLazy.findWithDefault (PatternVar patternVar) (patternVarKey patternVar) resolved
        PatternNode patternNode ->
          PatternNode (fmap go patternNode)

resolvedTermSubst :: Functor f => TermSubst f -> TermSubst f
resolvedTermSubst termSubst =
  TermSubst (IntMapLazy.map (resolveTermSubst termSubst) (unTermSubst termSubst))

composeTermSubst :: Functor f => TermSubst f -> TermSubst f -> TermSubst f
composeTermSubst outer (TermSubst inner) =
  TermSubst
    (IntMap.union
       (IntMap.map (applyTermSubst outer) inner)
       (unTermSubst outer))

restrictTermSubst :: IntSet -> TermSubst f -> TermSubst f
restrictTermSubst keys =
  TermSubst . flip IntMap.restrictKeys keys . unTermSubst
