{-# LANGUAGE LambdaCase #-}

-- | Post-match substitution algebra for binder-sensitive RHS construction.
-- It owns sequential binder substitutions and their variable-dependency set;
-- actual binder semantics are supplied by 'BinderSubstAlgebra' at the runtime boundary.
module Moonlight.Rewrite.Runtime.PostMatch
  ( BinderSubstAlgebra (..),
    PostMatchTerm (..),
    PostMatchSubst (..),
    applyPostMatchSubst,
    postMatchSubstVariables,
  )
where

import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core (BinderId, Pattern (..), PatternVar, patternVariables)
import Moonlight.Core (note)

type BinderSubstAlgebra :: (Type -> Type) -> Type
data BinderSubstAlgebra f = BinderSubstAlgebra
  { bsaSubstituteBinder :: BinderId -> Pattern f -> Pattern f -> Pattern f
  }

type PostMatchTerm :: (Type -> Type) -> Type
data PostMatchTerm f
  = PostMatchVar !PatternVar
  | PostMatchPattern !(Pattern f)

deriving stock instance Eq (Pattern f) => Eq (PostMatchTerm f)
deriving stock instance Ord (Pattern f) => Ord (PostMatchTerm f)
deriving stock instance Show (Pattern f) => Show (PostMatchTerm f)

type PostMatchSubst :: (Type -> Type) -> Type
data PostMatchSubst f
  = SubstBinder !BinderId !(PostMatchTerm f)
  | SequentialPostMatchSubst !(PostMatchSubst f) !(PostMatchSubst f)

deriving stock instance Eq (Pattern f) => Eq (PostMatchSubst f)
deriving stock instance Ord (Pattern f) => Ord (PostMatchSubst f)
deriving stock instance Show (Pattern f) => Show (PostMatchSubst f)

applyPostMatchSubst ::
  Traversable f =>
  BinderSubstAlgebra f ->
  Map.Map PatternVar (Pattern f) ->
  PostMatchSubst f ->
  Pattern f ->
  Either PatternVar (Pattern f)
applyPostMatchSubst binderSubstAlgebra substitution postMatchSubst patternValue =
  case postMatchSubst of
    SubstBinder targetBinderId argumentTerm -> do
      resolvedArgument <- resolvePostMatchTerm substitution argumentTerm
      pure (bsaSubstituteBinder binderSubstAlgebra targetBinderId resolvedArgument patternValue)
    SequentialPostMatchSubst leftSubst rightSubst ->
      applyPostMatchSubst binderSubstAlgebra substitution leftSubst patternValue
        >>= applyPostMatchSubst binderSubstAlgebra substitution rightSubst

resolvePostMatchTerm ::
  Traversable f =>
  Map.Map PatternVar (Pattern f) ->
  PostMatchTerm f ->
  Either PatternVar (Pattern f)
resolvePostMatchTerm substitution =
  \case
    PostMatchVar patternVar ->
      note patternVar (Map.lookup patternVar substitution)
    PostMatchPattern patternValue ->
      resolvePattern substitution patternValue

resolvePattern ::
  Traversable f =>
  Map.Map PatternVar (Pattern f) ->
  Pattern f ->
  Either PatternVar (Pattern f)
resolvePattern substitution =
  \case
    PatternVar patternVar ->
      note patternVar (Map.lookup patternVar substitution)
    PatternNode patternNode ->
      PatternNode <$> traverse (resolvePattern substitution) patternNode

postMatchSubstVariables :: Foldable f => PostMatchSubst f -> Set PatternVar
postMatchSubstVariables =
  \case
    SubstBinder _ argumentTerm ->
      postMatchTermVariables argumentTerm
    SequentialPostMatchSubst leftSubst rightSubst ->
      postMatchSubstVariables leftSubst <> postMatchSubstVariables rightSubst

postMatchTermVariables :: Foldable f => PostMatchTerm f -> Set PatternVar
postMatchTermVariables =
  \case
    PostMatchVar patternVar ->
      Set.singleton patternVar
    PostMatchPattern patternValue ->
      patternVariables patternValue
