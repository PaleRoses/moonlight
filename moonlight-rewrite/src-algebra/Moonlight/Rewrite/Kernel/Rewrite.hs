-- | Public surface for kernel rewrite values and their invariants.
-- It exposes the internal owner where interfaces must occur on both sides,
-- decorations validate against left variables, and canonicalization only renames variables.
module Moonlight.Rewrite.Kernel.Rewrite
  ( RewriteOrigin (..),
    rewriteOriginAtoms,
    rewriteOriginFoldMap,
    PatternInterface,
    patternInterfaceVariables,
    mkPatternInterface,
    PatternRewrite,
    prOrigin,
    prLeft,
    prInterface,
    prRight,
    prDecoration,
    PatternRewriteError (..),
    mkPatternRewrite,
    identityPatternRewrite,
    unitPatternRewriteWithCommonInterface,
    renamePatternRewrite,
    canonicalizePatternRewrite,
    samePatternRewriteShape,
    patternRewriteLeftVars,
    patternRewriteRightVars,
    patternRewriteDeletedVars,
    patternRewriteCreatedVars,
    allPatternRewriteVariables,
    isInvertiblePatternRewrite,
    isLeftLinearPatternRewrite,
  )
where

import Moonlight.Rewrite.Kernel.Rewrite.Internal
