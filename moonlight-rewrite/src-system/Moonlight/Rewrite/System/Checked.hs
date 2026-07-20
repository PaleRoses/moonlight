{-# LANGUAGE GHC2024 #-}

module Moonlight.Rewrite.System.Checked
  ( CheckedRewrite,
    checkedRewriteId,
    checkedRewriteName,
    checkedRewriteAlgebra,
    checkedRewriteLhs,
    checkedRewriteRhs,
    checkedRewriteInterface,
    checkedRewriteOrigin,
    checkedRewriteCondition,
    checkedRewriteApplicationCondition,
    checkedRewritePostSubst,
    checkedRewriteVariables,
    CheckedSystem,
    CheckedSystemError (..),
    checkedSystemFromRewrites,
    checkedRuleNames,
    checkedRewrites,
    lookupCheckedRewrite,
  )
where

import Moonlight.Core
  ( Pattern,
  )
import Moonlight.Rewrite.Algebra
  ( CompiledApplicationCondition,
    PatternInterface,
    RewriteOrigin,
    prDecoration,
    prInterface,
    prLeft,
    prOrigin,
    prRight,
  )
import Moonlight.Rewrite.Runtime
  ( PostMatchSubst,
  )
import Moonlight.Rewrite.System.Logic.Decoration
  ( ldApplicationCondition,
    ldCondition,
    ldPostSubst,
  )
import Moonlight.Rewrite.System.Logic.Guard
  ( CompiledGuard,
  )
import Moonlight.Rewrite.System.Origin
  ( RuleOrigin,
  )
import Moonlight.Rewrite.System.RuleName
  ( RuleName,
  )
import Moonlight.Rewrite.System.Checked.Internal
  ( CheckedRewrite,
    CheckedSystem,
    CheckedSystemError (..),
    checkedRewriteAlgebra,
    checkedRewriteId,
    checkedRewriteName,
    checkedRewriteVariables,
    checkedRewrites,
    checkedSystemFromRewrites,
    lookupCheckedRewrite,
  )

checkedRewriteLhs :: CheckedRewrite capability f -> Pattern f
checkedRewriteLhs =
  prLeft . checkedRewriteAlgebra

checkedRewriteRhs :: CheckedRewrite capability f -> Pattern f
checkedRewriteRhs =
  prRight . checkedRewriteAlgebra

checkedRewriteInterface :: CheckedRewrite capability f -> PatternInterface
checkedRewriteInterface =
  prInterface . checkedRewriteAlgebra

checkedRewriteOrigin :: CheckedRewrite capability f -> RewriteOrigin RuleOrigin
checkedRewriteOrigin =
  prOrigin . checkedRewriteAlgebra

checkedRewriteCondition :: CheckedRewrite capability f -> Maybe (CompiledGuard capability f)
checkedRewriteCondition =
  ldCondition . prDecoration . checkedRewriteAlgebra

checkedRewriteApplicationCondition ::
  CheckedRewrite capability f ->
  Maybe (CompiledApplicationCondition (CompiledGuard capability f) f)
checkedRewriteApplicationCondition =
  ldApplicationCondition . prDecoration . checkedRewriteAlgebra

checkedRewritePostSubst :: CheckedRewrite capability f -> Maybe (PostMatchSubst f)
checkedRewritePostSubst =
  ldPostSubst . prDecoration . checkedRewriteAlgebra

checkedRuleNames :: CheckedSystem capability f -> [RuleName]
checkedRuleNames =
  fmap checkedRewriteName . checkedRewrites
