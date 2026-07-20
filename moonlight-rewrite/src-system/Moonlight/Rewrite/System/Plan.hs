{-# LANGUAGE GHC2024 #-}

-- | Runtime planning boundary for checked systems.
-- Owns 'RulePlanSet' construction from checked rewrites by compiling LHS
-- queries, RHS instantiation specs, guards, application conditions, and post
-- substitutions.
-- Contract: plans are derived views whose names and order follow
-- 'CheckedSystem' without revalidating rule semantics.
module Moonlight.Rewrite.System.Plan
  ( RulePlan,
    rpId,
    rpQuery,
    rpRhs,
    rpApplicationCondition,
    RulePlanError (..),
    rulePlanPrimaryPattern,
    rulePlanCondition,
    rulePlanRhsPattern,
    RulePlanSet,
    rulePlanNames,
    rulePlans,
    orderedRulePlans,
    lookupRulePlan,
    traverseRulePlanSet,
    planCheckedRewrite,
    planRuleSet,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( Language,
  )
import Moonlight.Rewrite.Algebra
  ( compiledSinglePatternQuery,
  )
import Moonlight.Rewrite.Runtime
  ( RulePlan,
    RulePlanError (..),
    certifyRulePlan,
    rpApplicationCondition,
    rpId,
    rpQuery,
    rpRhs,
    rulePlanCondition,
    rulePlanPrimaryPattern,
    rulePlanRhsPattern,
    rhsInstantiationSpec,
  )
import Moonlight.Rewrite.System.Checked
  ( CheckedRewrite,
    CheckedSystem,
    checkedRewriteApplicationCondition,
    checkedRewriteCondition,
    checkedRewriteId,
    checkedRewriteLhs,
    checkedRewriteName,
    checkedRewritePostSubst,
    checkedRewriteRhs,
    checkedRewrites,
  )
import Moonlight.Rewrite.System.Logic.Guard
  ( CompiledGuard,
  )
import Moonlight.Rewrite.System.RuleName
  ( RuleName,
  )

type RulePlanSet :: Type -> (Type -> Type) -> Type
data RulePlanSet capability f = RulePlanSet
  { rpsPlansByName :: !(Map RuleName (RulePlan (CompiledGuard capability f) f)),
    rpsOrderedPlans :: ![(RuleName, RulePlan (CompiledGuard capability f) f)]
  }

rulePlanNames :: RulePlanSet capability f -> [RuleName]
rulePlanNames =
  fmap fst . rpsOrderedPlans

rulePlans :: RulePlanSet capability f -> [RulePlan (CompiledGuard capability f) f]
rulePlans =
  fmap snd . rpsOrderedPlans

orderedRulePlans ::
  RulePlanSet capability f ->
  [(RuleName, RulePlan (CompiledGuard capability f) f)]
orderedRulePlans =
  rpsOrderedPlans

lookupRulePlan :: RuleName -> RulePlanSet capability f -> Maybe (RulePlan (CompiledGuard capability f) f)
lookupRulePlan ruleNameValue =
  Map.lookup ruleNameValue . rpsPlansByName

traverseRulePlanSet ::
  Applicative effect =>
  (RulePlan (CompiledGuard capability f) f -> effect (RulePlan (CompiledGuard mappedCapability mappedF) mappedF)) ->
  RulePlanSet capability f ->
  effect (RulePlanSet mappedCapability mappedF)
traverseRulePlanSet transformPlan =
  fmap rulePlanSetFromOrderedPlans
    . traverse
      ( \(ruleNameValue, rulePlan) ->
          fmap (\mappedPlan -> (ruleNameValue, mappedPlan)) (transformPlan rulePlan)
      )
    . rpsOrderedPlans

rulePlanSetFromOrderedPlans ::
  [(RuleName, RulePlan (CompiledGuard capability f) f)] ->
  RulePlanSet capability f
rulePlanSetFromOrderedPlans orderedPlans =
  RulePlanSet
    { rpsPlansByName = Map.fromList orderedPlans,
      rpsOrderedPlans = orderedPlans
    }

planRuleSet ::
  Language f =>
  CheckedSystem capability f ->
  Either RulePlanError (RulePlanSet capability f)
planRuleSet checkedSystem =
  rulePlanSetFromOrderedPlans
    <$> traverse
      ( \rewriteValue ->
          fmap
            ((,) (checkedRewriteName rewriteValue))
            (planCheckedRewrite rewriteValue)
      )
      (checkedRewrites checkedSystem)

planCheckedRewrite ::
  Language f =>
  CheckedRewrite capability f ->
  Either RulePlanError (RulePlan (CompiledGuard capability f) f)
planCheckedRewrite rewriteValue =
  let leftPattern =
        checkedRewriteLhs rewriteValue

      rightPattern =
        checkedRewriteRhs rewriteValue

      rewriteCondition =
        checkedRewriteCondition rewriteValue
   in certifyRulePlan
        (checkedRewriteId rewriteValue)
        (compiledSinglePatternQuery leftPattern rewriteCondition)
        (rhsInstantiationSpec (checkedRewritePostSubst rewriteValue) rightPattern)
        (checkedRewriteApplicationCondition rewriteValue)
