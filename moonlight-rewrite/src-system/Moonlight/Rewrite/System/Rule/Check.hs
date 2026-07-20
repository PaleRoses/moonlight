{-# LANGUAGE GHC2024 #-}

module Moonlight.Rewrite.System.Rule.Check
  ( RawRewriteRule (..),
    CheckedRawRewriteRule,
    chrId,
    chrLhsPattern,
    chrRhsPattern,
    chrCondition,
    chrApplicationCondition,
    chrPostSubst,
    RewriteCompileError (..),
    checkRawRewrite,
    checkRawRewrites,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Semigroup (sconcat)
import Data.Set qualified as Set
import Moonlight.Core
  ( Language,
    Pattern,
    PatternVar,
    RewriteRuleId,
    patternVariables,
  )
import Moonlight.Rewrite.Algebra
  ( ApplicationCondition,
    ApplicationConditionCompileError,
    CompiledApplicationCondition,
    compileApplicationCondition,
  )
import Moonlight.Rewrite.Runtime
  ( PostMatchSubst,
    postMatchSubstVariables,
  )

type RawRewriteRule :: Type -> (Type -> Type) -> Type
data RawRewriteRule guard f = RawRewriteRule
  { rrId :: !RewriteRuleId,
    rrLhs :: !(Pattern f),
    rrRhs :: !(Pattern f),
    rrCondition :: !(Maybe guard),
    rrApplicationCondition :: !(Maybe (ApplicationCondition guard f)),
    rrPostSubst :: !(Maybe (PostMatchSubst f))
  }

type CheckedRawRewriteRule :: Type -> (Type -> Type) -> Type
data CheckedRawRewriteRule compiledGuard f = CheckedRawRewriteRule
  !RewriteRuleId
  !(Pattern f)
  !(Pattern f)
  !(Maybe compiledGuard)
  !(Maybe (CompiledApplicationCondition compiledGuard f))
  !(Maybe (PostMatchSubst f))

chrId :: CheckedRawRewriteRule compiledGuard f -> RewriteRuleId
chrId (CheckedRawRewriteRule rewriteRuleId _lhs _rhs _condition _applicationCondition _postSubstitution) =
  rewriteRuleId

chrLhsPattern :: CheckedRawRewriteRule compiledGuard f -> Pattern f
chrLhsPattern (CheckedRawRewriteRule _rewriteRuleId lhs _rhs _condition _applicationCondition _postSubstitution) =
  lhs

chrRhsPattern :: CheckedRawRewriteRule compiledGuard f -> Pattern f
chrRhsPattern (CheckedRawRewriteRule _rewriteRuleId _lhs rhs _condition _applicationCondition _postSubstitution) =
  rhs

chrCondition :: CheckedRawRewriteRule compiledGuard f -> Maybe compiledGuard
chrCondition (CheckedRawRewriteRule _rewriteRuleId _lhs _rhs condition _applicationCondition _postSubstitution) =
  condition

chrApplicationCondition ::
  CheckedRawRewriteRule compiledGuard f ->
  Maybe (CompiledApplicationCondition compiledGuard f)
chrApplicationCondition (CheckedRawRewriteRule _rewriteRuleId _lhs _rhs _condition applicationCondition _postSubstitution) =
  applicationCondition

chrPostSubst :: CheckedRawRewriteRule compiledGuard f -> Maybe (PostMatchSubst f)
chrPostSubst (CheckedRawRewriteRule _rewriteRuleId _lhs _rhs _condition _applicationCondition postSubstitution) =
  postSubstitution

type RewriteCompileError :: Type
data RewriteCompileError
  = RewriteRhsIntroducesUnboundVars !RewriteRuleId ![PatternVar]
  | RewriteGuardIntroducesUnboundVars !RewriteRuleId ![PatternVar]
  | RewritePostSubstIntroducesUnboundVars !RewriteRuleId ![PatternVar]
  | RewriteApplicationConditionFailure !RewriteRuleId !ApplicationConditionCompileError
  deriving stock (Eq, Show)

checkRawRewrite ::
  (Language f, Semigroup compiledGuard) =>
  (Set.Set PatternVar -> guard -> Either [PatternVar] compiledGuard) ->
  RawRewriteRule guard f ->
  Either RewriteCompileError (CheckedRawRewriteRule compiledGuard f)
checkRawRewrite compileGuard rawRule =
  let lhsVariables =
        patternVariables (rrLhs rawRule)

      unboundVariables =
        Set.toAscList . (`Set.difference` lhsVariables)

      unboundRhsVariables =
        unboundVariables (patternVariables (rrRhs rawRule))

      unboundPostSubstVariables =
        maybe
          []
          (unboundVariables . postMatchSubstVariables)
          (rrPostSubst rawRule)
   in case (unboundRhsVariables, unboundPostSubstVariables) of
        (_ : _, _) ->
          Left (RewriteRhsIntroducesUnboundVars (rrId rawRule) unboundRhsVariables)

        (_, _ : _) ->
          Left (RewritePostSubstIntroducesUnboundVars (rrId rawRule) unboundPostSubstVariables)

        ([], []) -> do
          compiledGuard <-
            traverse
              ( first (RewriteGuardIntroducesUnboundVars (rrId rawRule))
                  . compileGuard lhsVariables
              )
              (rrCondition rawRule)

          compiledApplicationCondition <-
            traverse
              ( first (RewriteApplicationConditionFailure (rrId rawRule))
                  . compileApplicationCondition
                    combineCompiledGuards
                    compileGuard
                    lhsVariables
              )
              (rrApplicationCondition rawRule)

          Right
            ( CheckedRawRewriteRule
                (rrId rawRule)
                (rrLhs rawRule)
                (rrRhs rawRule)
                compiledGuard
                compiledApplicationCondition
                (rrPostSubst rawRule)
            )

checkRawRewrites ::
  (Language f, Semigroup compiledGuard) =>
  (Set.Set PatternVar -> guard -> Either [PatternVar] compiledGuard) ->
  [RawRewriteRule guard f] ->
  Either RewriteCompileError [CheckedRawRewriteRule compiledGuard f]
checkRawRewrites =
  traverse . checkRawRewrite

combineCompiledGuards :: Semigroup compiledGuard => [compiledGuard] -> Maybe compiledGuard
combineCompiledGuards =
  fmap sconcat . NonEmpty.nonEmpty
