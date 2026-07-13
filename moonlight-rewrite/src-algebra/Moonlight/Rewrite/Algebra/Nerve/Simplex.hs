-- | Read-only semantics for rewrite-category simplices.
-- It owns extraction of edge rewrites, endpoint patterns, intermediate targets,
-- and the composed path rewrite; composition failure stays a kernel obstruction.
module Moonlight.Rewrite.Algebra.Nerve.Simplex
  ( simplexRewrites,
    simplexSourcePattern,
    simplexTargetPattern,
    simplexIntermediatePatterns,
    simplexComposedRewrite,
  )
where

import Moonlight.Category (chainMorphisms, chainStartObject)
import Moonlight.Core
  ( HasConstructorTag,
    Pattern,
    ZipMatch,
  )
import Moonlight.Rewrite.Algebra.Category.Core (RewriteOb (..), rmRewrite)
import Moonlight.Rewrite.Algebra.Category.Finite
  ( FinRewriteMor (..),
    FinRewriteOb (..),
    FiniteRewriteCategory,
  )
import Moonlight.Rewrite.Kernel.Compose (CompositionError, composePatternRewriteChain)
import Moonlight.Rewrite.Kernel.Rewrite (PatternRewrite, prRight)
import Moonlight.Rewrite.Kernel.Decoration
  ( RewriteDecoration (..),
  )
import Moonlight.Category.Simplicial (NerveSimplex, nerveSimplexChain)

simplexRewrites :: NerveSimplex (FiniteRewriteCategory atom dec f) -> [PatternRewrite atom dec f]
simplexRewrites =
  fmap (rmRewrite . unFinRewriteMor) . chainMorphisms . nerveSimplexChain

simplexSourcePattern :: NerveSimplex (FiniteRewriteCategory atom dec f) -> Pattern f
simplexSourcePattern simplexValue =
  case chainStartObject (nerveSimplexChain simplexValue) of
    FinRewriteOb (RewriteOb patternValue) -> patternValue

simplexTargetPattern :: NerveSimplex (FiniteRewriteCategory atom dec f) -> Pattern f
simplexTargetPattern simplexValue =
  case reverse (simplexRewrites simplexValue) of
    rewriteValue : _ -> prRight rewriteValue
    [] -> simplexSourcePattern simplexValue

simplexIntermediatePatterns :: NerveSimplex (FiniteRewriteCategory atom dec f) -> [Pattern f]
simplexIntermediatePatterns simplexValue =
  fmap prRight (dropLast (simplexRewrites simplexValue))

simplexComposedRewrite ::
  ( HasConstructorTag f,
    ZipMatch f,
    RewriteDecoration dec,
    DecorationConstraint dec f
  ) =>
  NerveSimplex (FiniteRewriteCategory atom dec f) ->
  Either (CompositionError dec f) (PatternRewrite atom dec f)
simplexComposedRewrite simplexValue =
  composePatternRewriteChain (simplexRewrites simplexValue)

dropLast :: [a] -> [a]
dropLast values =
  zipWith const values (drop 1 values)
