{-# LANGUAGE GHC2024 #-}

-- | Runtime planning boundary for checked systems.
-- Owns 'RulePlanSet' construction from checked rewrites by compiling LHS
-- queries, RHS instantiation specs, guards, application conditions, and post
-- substitutions.
-- Contract: plans are derived views whose names and order follow
-- 'CheckedSystem' without revalidating rule semantics.
module Moonlight.Rewrite.System.Plan
  ( RulePlan (..),
    rulePlanPrimaryPattern,
    rulePlanCondition,
    rulePlanRhsPattern,
    RulePlanSet,
    rulePlanNames,
    rulePlans,
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
  ( rhsInstantiationSpec,
  )
import Moonlight.Rewrite.Runtime
  ( RulePlan (..),
    rulePlanCondition,
    rulePlanPrimaryPattern,
    rulePlanRhsPattern,
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
  RulePlanSet capability f
planRuleSet checkedSystem =
  rulePlanSetFromOrderedPlans
    ( fmap
        (\rewriteValue -> (checkedRewriteName rewriteValue, planCheckedRewrite rewriteValue))
        (checkedRewrites checkedSystem)
    )

planCheckedRewrite ::
  Language f =>
  CheckedRewrite capability f ->
  RulePlan (CompiledGuard capability f) f
planCheckedRewrite rewriteValue =
  let leftPattern =
        checkedRewriteLhs rewriteValue

      rightPattern =
        checkedRewriteRhs rewriteValue

      rewriteCondition =
        checkedRewriteCondition rewriteValue
   in RulePlan
        { rpId = checkedRewriteId rewriteValue,
          rpQuery = compiledSinglePatternQuery leftPattern rewriteCondition,
          rpRhs = rhsInstantiationSpec (checkedRewritePostSubst rewriteValue) rightPattern,
          rpApplicationCondition = checkedRewriteApplicationCondition rewriteValue
        }
