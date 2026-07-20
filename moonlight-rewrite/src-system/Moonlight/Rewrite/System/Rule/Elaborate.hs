{-# LANGUAGE GHC2024 #-}

-- | Elaboration from checked raw rules into kernel pattern rewrites.
-- Owns origin attachment, interface-variable selection, and
-- 'LogicalDecoration' assembly.
-- Contracts: the interface is the LHS/RHS variable intersection, and origin
-- length mismatch is distinct from kernel rewrite failure.
module Moonlight.Rewrite.System.Rule.Elaborate
  ( elaborateCheckedRewrite,
    CheckedRewriteElaborationError (..),
    elaborateCheckedRewrites,
  )
where

import Data.Bifunctor (first)
import Data.Set qualified as Set
import Moonlight.Rewrite.Algebra
  ( PatternRewrite,
    PatternRewriteError,
    RewriteOrigin (..),
    mkPatternRewrite,
  )
import Moonlight.Rewrite.System.Rule.Check
  ( CheckedRawRewriteRule,
    chrApplicationCondition,
    chrCondition,
    chrLhsPattern,
    chrPostSubst,
    chrRhsPattern,
  )
import Moonlight.Rewrite.System.Logic.Decoration
  ( LogicalDecoration,
    logicalDecorationWithApplicationCondition,
  )
import Moonlight.Rewrite.System.Logic.Guard
  ( CompiledGuard,
  )
import Moonlight.Core
  ( Language,
    patternVariables,
  )

data CheckedRewriteElaborationError capability f
  = CheckedRewriteElaborationFailure !(PatternRewriteError (LogicalDecoration capability) f)
  | CheckedRewriteElaborationLengthMismatch !Int !Int

deriving stock instance Eq (CheckedRewriteElaborationError capability f)
deriving stock instance Show (CheckedRewriteElaborationError capability f)

elaborateCheckedRewrite ::
  (Language f, Ord capability) =>
  atom ->
  CheckedRawRewriteRule (CompiledGuard capability f) f ->
  Either (PatternRewriteError (LogicalDecoration capability) f) (PatternRewrite atom (LogicalDecoration capability) f)
elaborateCheckedRewrite originAtom checkedRule =
  mkPatternRewrite
    (RewriteAtomic originAtom)
    (chrLhsPattern checkedRule)
    ( Set.intersection
        (patternVariables (chrLhsPattern checkedRule))
        (patternVariables (chrRhsPattern checkedRule))
    )
    (chrRhsPattern checkedRule)
    ( logicalDecorationWithApplicationCondition
        (chrCondition checkedRule)
        (chrApplicationCondition checkedRule)
        (chrPostSubst checkedRule)
    )

elaborateCheckedRewrites ::
  (Language f, Ord capability) =>
  [atom] ->
  [CheckedRawRewriteRule (CompiledGuard capability f) f] ->
  Either (CheckedRewriteElaborationError capability f) [PatternRewrite atom (LogicalDecoration capability) f]
elaborateCheckedRewrites originAtoms checkedRules =
  if originCount == ruleCount
    then
      traverse
        (first CheckedRewriteElaborationFailure . uncurry elaborateCheckedRewrite)
        (zip originAtoms checkedRules)
    else
      Left (CheckedRewriteElaborationLengthMismatch originCount ruleCount)
  where
    originCount =
      length originAtoms
    ruleCount =
      length checkedRules
