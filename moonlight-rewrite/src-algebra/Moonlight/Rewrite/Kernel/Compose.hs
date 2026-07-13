{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Descent and gluing stratum for sequential pattern rewrites.
-- It owns right-rewrite freshening, boundary unification, interface intersection,
-- decoration projection and composition, and origin composition; every failure is
-- a typed composition obstruction rather than a guessed partial composite.
module Moonlight.Rewrite.Kernel.Compose
  ( CompositionError (..),
    CompositionResult (..),
    composePatternRewrites,
    composePatternRewriteChain,
  )
where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Moonlight.Core
  ( HasConstructorTag,
    Pattern,
    PatternVar,
    ZipMatch,
    patternVarKey,
    patternVariables,
  )
import Moonlight.Rewrite.Kernel.Rewrite
  ( PatternRewrite,
    PatternRewriteError,
    RewriteOrigin (..),
    allPatternRewriteVariables,
    mkPatternRewrite,
    patternInterfaceVariables,
    prDecoration,
    prInterface,
    prLeft,
    prOrigin,
    prRight,
    renamePatternRewrite,
  )
import Moonlight.Rewrite.Kernel.Decoration
  ( DecorationError,
    DecorationObstruction,
    PatternRenaming,
    RewriteDecoration (..),
    offsetPatternRenaming,
    projectDecoration,
  )
import Moonlight.Rewrite.Kernel.SpanModel
  ( ComposedInterface (..),
    PatternSpanModel,
    PatternSpanModelError,
    ProjectedInterface (..),
    SpanOverlap (..),
    patternInterfaceLeg,
    projectedInterface,
  )
import Moonlight.Rewrite.Kernel.Unify
  ( PatternUnifier,
  )

type CompositionError :: ((Type -> Type) -> Type) -> (Type -> Type) -> Type
data CompositionError dec f
  = IncompatibleBoundary !(Pattern f) !(Pattern f) !(SpanOverlapError (PatternSpanModel f))
  | EmptyRewriteChain
  | InvalidComposedInterface !PatternSpanModelError
  | InvalidComposedDecoration !(DecorationError (DecorationObstruction dec f) f)
  | InvalidComposedRewrite !(PatternRewriteError dec f)

deriving stock instance
  (Eq (Pattern f), Eq (DecorationObstruction dec f)) =>
  Eq (CompositionError dec f)

deriving stock instance
  (Show (Pattern f), Show (DecorationObstruction dec f)) =>
  Show (CompositionError dec f)

type CompositionResult :: Type -> ((Type -> Type) -> Type) -> (Type -> Type) -> Type
data CompositionResult atom dec f = CompositionResult
  { crRewrite :: !(PatternRewrite atom dec f),
    crOverlap :: !(PatternUnifier f)
  }

deriving stock instance
  (Show (PatternRewrite atom dec f), Show (PatternUnifier f)) =>
  Show (CompositionResult atom dec f)

composePatternRewrites ::
  forall atom dec f.
  ( HasConstructorTag f,
    ZipMatch f,
    RewriteDecoration dec,
    DecorationConstraint dec f
  ) =>
  PatternRewrite atom dec f ->
  PatternRewrite atom dec f ->
  Either (CompositionError dec f) (CompositionResult atom dec f)
composePatternRewrites leftRewrite rightRewrite =
  let model =
        Proxy @(PatternSpanModel f)

      rightRenaming =
        freshRightRenaming leftRewrite rightRewrite

      renamedRightRewrite =
        renamePatternRewriteIfNeeded rightRenaming rightRewrite

      forbiddenApexVars =
        compositionApexForbidden leftRewrite renamedRightRewrite
   in case
        spanOverlapFreshFrom
          model
          forbiddenApexVars
          (prRight leftRewrite)
          (prLeft renamedRightRewrite)
      of
        Left overlapError ->
          Left (IncompatibleBoundary (prRight leftRewrite) (prLeft rightRewrite) overlapError)
        Right patternOverlap -> do
          let leftProjection =
                spanOverlapLeftProjection model patternOverlap

              rightProjection =
                spanOverlapRightProjection model patternOverlap

              leftProjected =
                projectedInterface
                  model
                  leftProjection
                  (prLeft leftRewrite)
                  (prInterface leftRewrite)
                  patternInterfaceLeg

              rightProjected =
                projectedInterface
                  model
                  rightProjection
                  (prRight renamedRightRewrite)
                  (prInterface renamedRightRewrite)
                  patternInterfaceLeg

              composedOrigin =
                RewriteComposite (prOrigin leftRewrite) (prOrigin rightRewrite)

          composedInterface <-
            first InvalidComposedInterface
              (spanComposeInterfaces model patternOverlap leftProjected rightProjected)

          projectedLeftDecoration <-
            first InvalidComposedDecoration
              (projectDecoration leftProjection (prDecoration leftRewrite))

          projectedRightDecoration <-
            first InvalidComposedDecoration
              (projectDecoration rightProjection (prDecoration renamedRightRewrite))

          composedDecoration <-
            first InvalidComposedDecoration
              (composeDecoration projectedLeftDecoration projectedRightDecoration)

          composedRewrite <-
            first InvalidComposedRewrite
              ( mkPatternRewrite
                  composedOrigin
                  (piObject leftProjected)
                  (patternInterfaceVariables (ciInterface composedInterface))
                  (piObject rightProjected)
                  composedDecoration
              )

          Right
            CompositionResult
              { crRewrite = composedRewrite,
                crOverlap = patternOverlap
              }

renamePatternRewriteIfNeeded ::
  (Functor f, Foldable f, RewriteDecoration dec, DecorationConstraint dec f) =>
  PatternRenaming ->
  PatternRewrite atom dec f ->
  PatternRewrite atom dec f
renamePatternRewriteIfNeeded renaming rewriteValue
  | renaming == mempty =
      rewriteValue
  | otherwise =
      renamePatternRewrite renaming rewriteValue

compositionApexForbidden ::
  (Foldable f, RewriteDecoration dec, DecorationConstraint dec f) =>
  PatternRewrite atom dec f ->
  PatternRewrite atom dec f ->
  Set.Set PatternVar
compositionApexForbidden leftRewrite rightRewrite =
  let leftBoundaryVars =
        patternVariables (prRight leftRewrite)

      rightBoundaryVars =
        patternVariables (prLeft rightRewrite)

      leftAmbientOnly =
        Set.difference (allPatternRewriteVariables leftRewrite) leftBoundaryVars

      rightAmbientOnly =
        Set.difference (allPatternRewriteVariables rightRewrite) rightBoundaryVars
   in leftAmbientOnly <> rightAmbientOnly

composePatternRewriteChain ::
  ( HasConstructorTag f,
    ZipMatch f,
    RewriteDecoration dec,
    DecorationConstraint dec f
  ) =>
  [PatternRewrite atom dec f] ->
  Either (CompositionError dec f) (PatternRewrite atom dec f)
composePatternRewriteChain rewriteValues =
  case rewriteValues of
    [] ->
      Left EmptyRewriteChain
    firstRewrite : remainingRewrites ->
      foldM
        (\currentRewrite nextRewrite -> crRewrite <$> composePatternRewrites currentRewrite nextRewrite)
        firstRewrite
        remainingRewrites

freshRightRenaming ::
  (Foldable f, RewriteDecoration dec, DecorationConstraint dec f) =>
  PatternRewrite atom dec f ->
  PatternRewrite atom dec f ->
  PatternRenaming
freshRightRenaming leftRewrite rightRewrite =
  let leftVars = allPatternRewriteVariables leftRewrite
      rightVars = allPatternRewriteVariables rightRewrite
   in if Set.disjoint leftVars rightVars
        then mempty
        else offsetPatternRenaming (freshOffset leftVars rightVars) rightVars

freshOffset :: Set.Set PatternVar -> Set.Set PatternVar -> Int
freshOffset leftVars rightVars =
  foldr
    (max . ((+ 1) . patternVarKey))
    0
    (Set.toAscList (leftVars <> rightVars))
