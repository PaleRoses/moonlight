{-# LANGUAGE GHC2024 #-}

-- | Relational plan compiler for system rule plans.
-- Owns atomizing rule queries into Flow 'QueryPlan's keyed by 'RuleName' and
-- collecting them into a 'RelationalPlanSet'.
-- Contract: compile failures are attributed to the rule name, and the output
-- payload is the root/binding 'RelationalRewriteMatch'.
module Moonlight.Rewrite.Relational.Compile
  ( RewritePlan,
    RelationalPlanSet (..),
    RelationalRuleCompileError (..),
    compileRelationalRulePlan,
    compileRelationalRulePlans,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( DenseKey,
  )
import Moonlight.Flow.Plan.Compile.Atomize
  ( PatternAtomizeHost,
    PatternAtomizeObstruction,
    atomizePatternQueryWith,
  )
import Moonlight.Flow.Plan.Query.Core
  ( QueryPlan,
  )
import Moonlight.Rewrite.Relational.Output
  ( RelationalRewriteMatch,
  )
import Moonlight.Rewrite.System
  ( RuleName,
  )

type RewritePlan :: Type -> Type -> Type -> Type -> Type -> Type -> Type
type RewritePlan compiled var key guard tag tuple =
  QueryPlan compiled (RelationalRewriteMatch var key) guard tag tuple key

type RelationalPlanSet :: Type -> Type -> Type -> Type -> Type -> Type -> Type
newtype RelationalPlanSet compiled var key guard tag tuple =
  RelationalPlanSet
    { rpsPlans :: Map RuleName (RewritePlan compiled var key guard tag tuple)
    }

type RelationalRuleCompileError :: Type
data RelationalRuleCompileError
  = RelationalRuleCompileError !RuleName !PatternAtomizeObstruction
  deriving stock (Eq, Ord, Show)

compileRelationalRulePlan ::
  (Ord var, DenseKey key) =>
  PatternAtomizeHost compiled pattern var guard tag tuple key (RelationalRewriteMatch var key) ->
  RuleName ->
  compiled ->
  Either RelationalRuleCompileError (RewritePlan compiled var key guard tag tuple)
compileRelationalRulePlan host ruleNameValue compiled =
  first
    (RelationalRuleCompileError ruleNameValue)
    (atomizePatternQueryWith host compiled)

compileRelationalRulePlans ::
  (Ord var, DenseKey key) =>
  PatternAtomizeHost compiled pattern var guard tag tuple key (RelationalRewriteMatch var key) ->
  Map RuleName compiled ->
  Either
    RelationalRuleCompileError
    (RelationalPlanSet compiled var key guard tag tuple)
compileRelationalRulePlans host compiledRules =
  RelationalPlanSet
    <$> Map.traverseWithKey
      (compileRelationalRulePlan host)
      compiledRules
