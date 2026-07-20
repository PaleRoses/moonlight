{-# LANGUAGE TypeFamilies #-}

-- | Categorical stratum for pattern rewrites as morphisms between patterns.
-- It owns the unbounded 'Category' instance and the composition witness wrapper,
-- while boundary compatibility and decoration transport remain in the kernel.
module Moonlight.Rewrite.Algebra.Category.Core
  ( RewriteCategory (..),
    RewriteOb (..),
    RewriteMor (..),
    RewriteTwoMor (..),
    RewriteCompositor (..),
    composeRewriteMorWithError,
  )
where

import Data.Kind (Type)
import Moonlight.Category (Category (..))
import Moonlight.Core
  ( HasConstructorTag,
    Pattern,
    ZipMatch,
  )
import Moonlight.Rewrite.Kernel.Compose
  ( CompositionError,
    CompositionResult (..),
    composePatternRewrites,
  )
import Moonlight.Rewrite.Kernel.Rewrite
  ( PatternRewrite,
    identityPatternRewrite,
    prLeft,
    prRight,
  )
import Moonlight.Rewrite.Kernel.Decoration
  ( RewriteDecoration (..),
  )
import Moonlight.Rewrite.Kernel.Unify
  ( PatternUnifier,
  )

type RewriteCategory :: Type -> ((Type -> Type) -> Type) -> (Type -> Type) -> Type
data RewriteCategory atom dec f = RewriteCategory

type RewriteOb :: Type -> ((Type -> Type) -> Type) -> (Type -> Type) -> Type
newtype RewriteOb atom dec f = RewriteOb
  { unRewriteOb :: Pattern f
  }

deriving stock instance Eq (Pattern f) => Eq (RewriteOb atom dec f)
deriving stock instance Ord (Pattern f) => Ord (RewriteOb atom dec f)
deriving stock instance Show (Pattern f) => Show (RewriteOb atom dec f)

type RewriteMor :: Type -> ((Type -> Type) -> Type) -> (Type -> Type) -> Type
newtype RewriteMor atom dec f = RewriteMor
  { rmRewrite :: PatternRewrite atom dec f
  }

deriving stock instance Eq (PatternRewrite atom dec f) => Eq (RewriteMor atom dec f)
deriving stock instance Ord (PatternRewrite atom dec f) => Ord (RewriteMor atom dec f)
deriving stock instance Show (PatternRewrite atom dec f) => Show (RewriteMor atom dec f)

type RewriteTwoMor :: Type -> ((Type -> Type) -> Type) -> (Type -> Type) -> Type
newtype RewriteTwoMor atom dec f = RewriteTwoMor ()

type RewriteCompositor :: Type -> ((Type -> Type) -> Type) -> (Type -> Type) -> Type
newtype RewriteCompositor atom dec f = RewriteCompositor (PatternUnifier f)

composeRewriteMorWithError ::
  ( HasConstructorTag f,
    ZipMatch f,
    RewriteDecoration dec,
    DecorationConstraint dec f
  ) =>
  RewriteMor atom dec f ->
  RewriteMor atom dec f ->
  Either (CompositionError dec f) (RewriteMor atom dec f, RewriteCompositor atom dec f)
composeRewriteMorWithError (RewriteMor rewriteG) (RewriteMor rewriteF) =
  fmap
    ( \compositionResult ->
        ( RewriteMor (crRewrite compositionResult),
          RewriteCompositor (crOverlap compositionResult)
        )
    )
    (composePatternRewrites rewriteF rewriteG)

instance
  ( HasConstructorTag f,
    ZipMatch f,
    RewriteDecoration dec,
    DecorationConstraint dec f
  ) =>
  Category (RewriteCategory atom dec f)
  where
  type Ob (RewriteCategory atom dec f) = RewriteOb atom dec f
  type Mor (RewriteCategory atom dec f) = RewriteMor atom dec f
  type TwoMor (RewriteCategory atom dec f) = RewriteTwoMor atom dec f
  type Compositor (RewriteCategory atom dec f) = RewriteCompositor atom dec f
  type CategoryError (RewriteCategory atom dec f) = CompositionError dec f

  identity _ (RewriteOb patternValue) =
    Right (RewriteMor (identityPatternRewrite patternValue))

  compose _ rewriteG rewriteF =
    composeRewriteMorWithError rewriteG rewriteF

  source _ (RewriteMor rewriteValue) =
    Right (RewriteOb (prLeft rewriteValue))

  target _ (RewriteMor rewriteValue) =
    Right (RewriteOb (prRight rewriteValue))
