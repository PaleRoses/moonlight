{-# LANGUAGE TypeFamilies #-}

-- | Finite category view over a chosen rewrite corpus.
-- It owns enumeration of explicit rewrites plus identity morphisms, deriving
-- objects only from rewrite endpoints so nerves can consume a bounded category.
module Moonlight.Rewrite.Algebra.Category.Finite
  ( FiniteRewriteCategory (..),
    FinRewriteOb (..),
    FinRewriteMor (..),
    FinRewriteTwoMor (..),
    FinRewriteCompositor (..),
    composeFiniteRewriteMorWithError,
    finiteRewriteCategory,
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.Kind (Type)
import Moonlight.Category (Category (..), FiniteComposableCategory (..))
import Moonlight.Core
  ( HasConstructorTag,
    Pattern,
    ZipMatch,
  )
import Moonlight.Rewrite.Algebra.Category.Core
  ( RewriteCompositor (..),
    RewriteMor (..),
    RewriteOb (..),
    composeRewriteMorWithError,
  )
import Moonlight.Rewrite.Kernel.Compose
  ( CompositionError,
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

type FiniteRewriteCategory :: Type -> ((Type -> Type) -> Type) -> (Type -> Type) -> Type
data FiniteRewriteCategory atom dec f = FiniteRewriteCategory
  { frcRewrites :: [PatternRewrite atom dec f],
    frcObjects :: [Pattern f]
  }

type FinRewriteOb :: Type -> ((Type -> Type) -> Type) -> (Type -> Type) -> Type
newtype FinRewriteOb atom dec f = FinRewriteOb
  { unFinRewriteOb :: RewriteOb atom dec f
  }

deriving stock instance Eq (RewriteOb atom dec f) => Eq (FinRewriteOb atom dec f)
deriving stock instance Ord (RewriteOb atom dec f) => Ord (FinRewriteOb atom dec f)

type FinRewriteMor :: Type -> ((Type -> Type) -> Type) -> (Type -> Type) -> Type
newtype FinRewriteMor atom dec f = FinRewriteMor
  { unFinRewriteMor :: RewriteMor atom dec f
  }

deriving stock instance Eq (RewriteMor atom dec f) => Eq (FinRewriteMor atom dec f)
deriving stock instance Ord (RewriteMor atom dec f) => Ord (FinRewriteMor atom dec f)

type FinRewriteTwoMor :: Type -> ((Type -> Type) -> Type) -> (Type -> Type) -> Type
newtype FinRewriteTwoMor atom dec f = FinRewriteTwoMor ()

type FinRewriteCompositor :: Type -> ((Type -> Type) -> Type) -> (Type -> Type) -> Type
newtype FinRewriteCompositor atom dec f = FinRewriteCompositor (PatternUnifier f)

composeFiniteRewriteMorWithError ::
  ( HasConstructorTag f,
    ZipMatch f,
    RewriteDecoration dec,
    DecorationConstraint dec f
  ) =>
  FinRewriteMor atom dec f ->
  FinRewriteMor atom dec f ->
  Either (CompositionError dec f) (FinRewriteMor atom dec f, FinRewriteCompositor atom dec f)
composeFiniteRewriteMorWithError
  (FinRewriteMor rewriteG)
  (FinRewriteMor rewriteF) =
    fmap
      ( \(composedMor, RewriteCompositor overlapWitness) ->
          (FinRewriteMor composedMor, FinRewriteCompositor overlapWitness)
      )
      (composeRewriteMorWithError rewriteG rewriteF)

finiteRewriteCategory :: Ord (Pattern f) => [PatternRewrite atom dec f] -> FiniteRewriteCategory atom dec f
finiteRewriteCategory rewriteValues =
  let objectPatterns = foldr (\rewriteValue objects -> prLeft rewriteValue : prRight rewriteValue : objects) [] rewriteValues
   in FiniteRewriteCategory
        { frcRewrites = rewriteValues,
          frcObjects = nubOrd objectPatterns
        }

instance
  ( HasConstructorTag f,
    ZipMatch f,
    RewriteDecoration dec,
    DecorationConstraint dec f
  ) =>
  Category (FiniteRewriteCategory atom dec f)
  where
  type Ob (FiniteRewriteCategory atom dec f) = FinRewriteOb atom dec f
  type Mor (FiniteRewriteCategory atom dec f) = FinRewriteMor atom dec f
  type TwoMor (FiniteRewriteCategory atom dec f) = FinRewriteTwoMor atom dec f
  type Compositor (FiniteRewriteCategory atom dec f) = FinRewriteCompositor atom dec f
  type CategoryError (FiniteRewriteCategory atom dec f) = CompositionError dec f

  identity _ (FinRewriteOb (RewriteOb patternValue)) =
    Right (FinRewriteMor (RewriteMor (identityPatternRewrite patternValue)))

  compose _ rewriteG rewriteF =
    composeFiniteRewriteMorWithError rewriteG rewriteF

  source _ (FinRewriteMor (RewriteMor rewriteValue)) =
    Right (FinRewriteOb (RewriteOb (prLeft rewriteValue)))

  target _ (FinRewriteMor (RewriteMor rewriteValue)) =
    Right (FinRewriteOb (RewriteOb (prRight rewriteValue)))

instance
  ( HasConstructorTag f,
    ZipMatch f,
    RewriteDecoration dec,
    DecorationConstraint dec f,
    Ord (Pattern f)
  ) =>
  FiniteComposableCategory (FiniteRewriteCategory atom dec f)
  where
  enumerateObjects =
    fmap (FinRewriteOb . RewriteOb) . frcObjects

  enumerateMorphisms finiteCategory =
    fmap (FinRewriteMor . RewriteMor) (frcRewrites finiteCategory)
      <> foldMap
        (either (const []) pure . identity finiteCategory . FinRewriteOb . RewriteOb)
        (frcObjects finiteCategory)

  enumerateMorphismsFrom finiteCategory sourceObject@(FinRewriteOb (RewriteOb sourcePattern)) =
    fmap
      (FinRewriteMor . RewriteMor)
      (filter ((== sourcePattern) . prLeft) (frcRewrites finiteCategory))
      <> either (const []) pure (identity finiteCategory sourceObject)
