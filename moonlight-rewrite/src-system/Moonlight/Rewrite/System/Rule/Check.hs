{-# LANGUAGE GHC2024 #-}

-- | Raw-rule well-formedness checker before algebra elaboration.
-- Owns variable-scope checks for RHS terms, guards, post substitutions, and
-- application-condition compilation.
-- Contracts: only LHS-bound variables may be introduced, and
-- application-condition guard compilation combines semigroup guards.
module Moonlight.Rewrite.System.Rule.Check
  ( RawRewriteRule (..),
    CheckedRawRewriteRule (..),
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
  { chrId :: !RewriteRuleId,
    chrLhsPattern :: !(Pattern f),
    chrRhsPattern :: !(Pattern f),
    chrCondition :: !(Maybe compiledGuard),
    chrApplicationCondition :: !(Maybe (CompiledApplicationCondition compiledGuard f)),
    chrPostSubst :: !(Maybe (PostMatchSubst f))
  }

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
            CheckedRawRewriteRule
              { chrId = rrId rawRule,
                chrLhsPattern = rrLhs rawRule,
                chrRhsPattern = rrRhs rawRule,
                chrCondition = compiledGuard,
                chrApplicationCondition = compiledApplicationCondition,
                chrPostSubst = rrPostSubst rawRule
              }

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
