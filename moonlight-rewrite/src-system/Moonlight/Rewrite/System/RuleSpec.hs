{-# LANGUAGE GHC2024 #-}

-- | Authoring AST for system rewrite rules and rule sets.
-- Owns optional explicit ids, names, LHS/RHS payloads, logical guards,
-- application conditions, and post-match substitutions.
-- Contracts: modifiers append by conjunction or sequencing; capability sugar
-- lowers to guard references without checking variables here.
module Moonlight.Rewrite.System.RuleSpec
  ( RuleSpec (..),
    rule,
    ruleWithId,
    ruleNameOf,
    when_,
    withApplicationCondition,
    requires_,
    forbids_,
    post,
    capability,
    RuleSet (..),
    ruleSet,
    ruleSetRuleNames,
    (|>),
  )
where

import Data.Kind (Type)
import Moonlight.Core
  ( Pattern,
    RewriteRuleId,
  )
import Moonlight.Rewrite.Algebra
  ( ApplicationCondition,
    PatternExtension,
    andApplicationConditions,
    forbidsExtension,
    requiresExtension,
  )
import Moonlight.Rewrite.Runtime
  ( PostMatchSubst (..),
  )
import Moonlight.Rewrite.System.Logic.Guard
  ( GuardRef,
    RewriteCondition (..),
    guardHasCapability,
  )
import Moonlight.Rewrite.System.RuleName
  ( RuleName,
  )

infixl 1 |>

(|>) :: a -> (a -> b) -> b
value |> transform =
  transform value

type RuleSpec :: Type -> (Type -> Type) -> Type
data RuleSpec capability f = RuleSpec
  { rsId :: !(Maybe RewriteRuleId),
    rsName :: !RuleName,
    rsLhs :: !(Pattern f),
    rsRhs :: !(Pattern f),
    rsCondition :: !(Maybe (RewriteCondition capability f)),
    rsApplicationCondition :: !(Maybe (ApplicationCondition (RewriteCondition capability f) f)),
    rsPostSubst :: !(Maybe (PostMatchSubst f))
  }

rule :: RuleName -> Pattern f -> Pattern f -> RuleSpec capability f
rule =
  ruleWithMaybeId Nothing

ruleWithId :: RewriteRuleId -> RuleName -> Pattern f -> Pattern f -> RuleSpec capability f
ruleWithId =
  ruleWithMaybeId . Just

ruleWithMaybeId :: Maybe RewriteRuleId -> RuleName -> Pattern f -> Pattern f -> RuleSpec capability f
ruleWithMaybeId maybeRewriteRuleId name leftPattern rightPattern =
  RuleSpec
    { rsId = maybeRewriteRuleId,
      rsName = name,
      rsLhs = leftPattern,
      rsRhs = rightPattern,
      rsCondition = Nothing,
      rsApplicationCondition = Nothing,
      rsPostSubst = Nothing
    }

ruleNameOf :: RuleSpec capability f -> RuleName
ruleNameOf =
  rsName

when_ ::
  (Ord capability, forall a. Ord a => Ord (f a)) =>
  RewriteCondition capability f ->
  RuleSpec capability f ->
  RuleSpec capability f
when_ rewriteCondition ruleValue =
  ruleValue
    { rsCondition =
        appendOptional (<>) rewriteCondition (rsCondition ruleValue)
    }

withApplicationCondition ::
  ApplicationCondition (RewriteCondition capability f) f ->
  RuleSpec capability f ->
  RuleSpec capability f
withApplicationCondition applicationCondition ruleValue =
  ruleValue
    { rsApplicationCondition =
        appendOptional
          (\existing new -> andApplicationConditions [existing, new])
          applicationCondition
          (rsApplicationCondition ruleValue)
    }

requires_ ::
  PatternExtension (RewriteCondition capability f) f ->
  RuleSpec capability f ->
  RuleSpec capability f
requires_ =
  withApplicationCondition . requiresExtension

forbids_ ::
  PatternExtension (RewriteCondition capability f) f ->
  RuleSpec capability f ->
  RuleSpec capability f
forbids_ =
  withApplicationCondition . forbidsExtension

post :: PostMatchSubst f -> RuleSpec capability f -> RuleSpec capability f
post postMatchSubst ruleValue =
  ruleValue
    { rsPostSubst =
        appendOptional SequentialPostMatchSubst postMatchSubst (rsPostSubst ruleValue)
    }

appendOptional :: (value -> value -> value) -> value -> Maybe value -> Maybe value
appendOptional combine newValue =
  Just . maybe newValue (`combine` newValue)

capability :: capability -> [GuardRef] -> RewriteCondition capability f
capability guardCapability refs =
  RewriteCondition (guardHasCapability guardCapability refs)

type RuleSet :: Type -> (Type -> Type) -> Type
newtype RuleSet capability f = RuleSet
  { ruleSetRules :: [RuleSpec capability f]
  }

ruleSet :: [RuleSpec capability f] -> RuleSet capability f
ruleSet =
  RuleSet

ruleSetRuleNames :: RuleSet capability f -> [RuleName]
ruleSetRuleNames =
  fmap ruleNameOf . ruleSetRules
