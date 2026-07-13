{-# LANGUAGE GHC2024 #-}

-- | Validation boundary from author rule specs to checked rewrite systems.
-- Owns duplicate name/id detection, synthetic id allocation, guard
-- compilation, raw-rule checking, and algebra elaboration error translation.
-- Contracts: RHS, post substitutions, and guards may mention only LHS-bound
-- variables; lookup and validation failures stay typed as 'RewriteError'.
module Moonlight.Rewrite.System.Check
  ( RewriteError (..),
    rewriteErrorMessage,
    checkRuleSet,
    checkRawRewriteSystem,
    checkedRewriteFromCheckedRawRule,
    rewriteByRuleName,
    checkedSystemErrorToRewriteError,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Traversable (mapAccumL)
import Moonlight.Core
  ( Language,
    Pattern,
    RewriteRuleId,
    rewriteRuleIdKey,
  )
import Moonlight.Core
  ( firstDuplicate,
  )
import Moonlight.Rewrite.Algebra
  ( CompositionError (..),
  )
import Moonlight.Rewrite.Algebra
  ( PatternRewriteError (..),
  )
import Moonlight.Rewrite.System.Checked
  ( CheckedRewrite (..),
    CheckedSystem,
    CheckedSystemError (..),
    checkedSystemFromRewrites,
    firstFreeRuleId,
    lookupCheckedRewrite,
  )
import Moonlight.Rewrite.System.Logic.Guard
  ( CompiledGuard,
    RewriteCondition,
    compileGuard,
  )
import Moonlight.Rewrite.System.Logic.Decoration
  ( LogicalDecoration,
  )
import Moonlight.Rewrite.System.Origin
  ( RuleOrigin (..),
  )
import Moonlight.Rewrite.System.Rule.Check qualified as RuleCheck
import Moonlight.Rewrite.System.Rule.Elaborate qualified as RuleElaborate
import Moonlight.Rewrite.System.RuleName
  ( RuleName,
    RuleNameError,
    mkRuleName,
  )
import Moonlight.Rewrite.System.RuleSpec
  ( RuleSet (..),
    RuleSpec (..),
  )

type RewriteError :: Type -> (Type -> Type) -> Type
data RewriteError capability f
  = RewriteDuplicateRuleName !RuleName
  | RewriteDuplicateRuleId !RewriteRuleId
  | RewriteInvalidSyntheticRuleName !String !RuleNameError
  | RewriteUnknownRule !RuleName
  | RewriteCompileFailure !RuleCheck.RewriteCompileError
  | RewriteAlgebraFailure !(PatternRewriteError (LogicalDecoration capability) f)
  | RewriteCompositionFailure !(CompositionError (LogicalDecoration capability) f)

deriving stock instance Eq (Pattern f) => Eq (RewriteError capability f)
deriving stock instance Show (Pattern f) => Show (RewriteError capability f)

rewriteErrorMessage :: RewriteError capability f -> String
rewriteErrorMessage rewriteError =
  case rewriteError of
    RewriteDuplicateRuleName ruleName ->
      "duplicate rewrite rule name: " <> show ruleName
    RewriteDuplicateRuleId ruleId ->
      "duplicate rewrite rule id: " <> show ruleId
    RewriteInvalidSyntheticRuleName rawName ruleNameError ->
      "invalid synthetic rewrite rule name " <> show rawName <> ": " <> show ruleNameError
    RewriteUnknownRule ruleName ->
      "unknown rewrite rule: " <> show ruleName
    RewriteCompileFailure compileError ->
      "rewrite rule compile failure: " <> show compileError
    RewriteAlgebraFailure algebraError ->
      rewriteAlgebraErrorMessage algebraError
    RewriteCompositionFailure compositionError ->
      rewriteCompositionErrorMessage compositionError

rewriteAlgebraErrorMessage :: PatternRewriteError (LogicalDecoration capability) f -> String
rewriteAlgebraErrorMessage rewriteError =
  case rewriteError of
    RewriteInterfaceNotInLeft missingVariables ->
      "rewrite interface missing from left pattern: " <> show missingVariables
    RewriteInterfaceNotInRight missingVariables ->
      "rewrite interface missing from right pattern: " <> show missingVariables
    RewriteInterfaceNotInBoth leftMissingVariables rightMissingVariables ->
      "rewrite interface missing from both patterns: " <> show (leftMissingVariables, rightMissingVariables)
    RewriteInvalidDecoration _ ->
      "rewrite decoration is invalid"

rewriteCompositionErrorMessage :: CompositionError (LogicalDecoration capability) f -> String
rewriteCompositionErrorMessage compositionError =
  case compositionError of
    IncompatibleBoundary _ _ _ ->
      "rewrite composition has an incompatible boundary"
    EmptyRewriteChain ->
      "rewrite composition chain is empty"
    InvalidComposedInterface _ ->
      "composed rewrite interface is invalid"
    InvalidComposedDecoration _ ->
      "composed rewrite decoration is invalid"
    InvalidComposedRewrite rewriteError ->
      rewriteAlgebraErrorMessage rewriteError

checkRuleSet ::
  (Language f, Ord capability) =>
  RuleSet capability f ->
  Either (RewriteError capability f) (CheckedSystem capability f)
checkRuleSet ruleSetValue = do
  validateRuleNames rules
  (nextId, assignedRules) <-
    assignRuleIds rules
  rewriteValues <-
    traverse checkAssignedRule assignedRules
  first checkedSystemErrorToRewriteError
    (checkedSystemFromRewrites nextId rewriteValues)
  where
    rules =
      ruleSetRules ruleSetValue

checkRawRewriteSystem ::
  (Language f, Ord capability) =>
  [RuleCheck.RawRewriteRule (RewriteCondition capability f) f] ->
  Either (RewriteError capability f) (CheckedSystem capability f)
checkRawRewriteSystem rawRules = do
  validateRawRuleIds rawRules
  checkedRawRules <-
    first RewriteCompileFailure
      (RuleCheck.checkRawRewrites compileGuard rawRules)
  checkedRules <-
    traverse checkedRewriteFromCheckedRawRule checkedRawRules
  first checkedSystemErrorToRewriteError
    (checkedSystemFromRewrites 0 checkedRules)

checkAssignedRule ::
  (Language f, Ord capability) =>
  (RewriteRuleId, RuleSpec capability f) ->
  Either (RewriteError capability f) (CheckedRewrite capability f)
checkAssignedRule (rewriteRuleId, ruleValue) = do
  checkedRawRule <-
    first RewriteCompileFailure
      ( RuleCheck.checkRawRewrite
          compileGuard
          (rawRuleFromRule rewriteRuleId ruleValue)
      )
  elaborateNamedCheckedRewrite (rsName ruleValue) checkedRawRule

rawRuleFromRule ::
  RewriteRuleId ->
  RuleSpec capability f ->
  RuleCheck.RawRewriteRule (RewriteCondition capability f) f
rawRuleFromRule rewriteRuleId ruleValue =
  RuleCheck.RawRewriteRule
    { RuleCheck.rrId = rewriteRuleId,
      RuleCheck.rrLhs = rsLhs ruleValue,
      RuleCheck.rrRhs = rsRhs ruleValue,
      RuleCheck.rrCondition = rsCondition ruleValue,
      RuleCheck.rrApplicationCondition = rsApplicationCondition ruleValue,
      RuleCheck.rrPostSubst = rsPostSubst ruleValue
    }

checkedRewriteFromCheckedRawRule ::
  (Language f, Ord capability) =>
  RuleCheck.CheckedRawRewriteRule (CompiledGuard capability f) f ->
  Either (RewriteError capability f) (CheckedRewrite capability f)
checkedRewriteFromCheckedRawRule checkedRawRule = do
  let rawName =
        "raw-" <> show (rewriteRuleIdKey (RuleCheck.chrId checkedRawRule))
  syntheticName <-
    first (RewriteInvalidSyntheticRuleName rawName)
      (mkRuleName rawName)
  elaborateNamedCheckedRewrite syntheticName checkedRawRule

elaborateNamedCheckedRewrite ::
  (Language f, Ord capability) =>
  RuleName ->
  RuleCheck.CheckedRawRewriteRule (CompiledGuard capability f) f ->
  Either (RewriteError capability f) (CheckedRewrite capability f)
elaborateNamedCheckedRewrite name checkedRawRule = do
  let rewriteRuleId =
        RuleCheck.chrId checkedRawRule
  algebraicRewrite <-
    first RewriteAlgebraFailure
      ( RuleElaborate.elaborateCheckedRewrite
          RuleOrigin
            { roRuleId = rewriteRuleId,
              roRuleName = name
            }
          checkedRawRule
      )

  Right
    CheckedRewrite
      { checkedRewriteId = rewriteRuleId,
        checkedRewriteName = name,
        checkedRewriteAlgebra = algebraicRewrite
      }

validateRuleNames :: [RuleSpec capability f] -> Either (RewriteError capability f) ()
validateRuleNames rules =
  case firstDuplicate (fmap rsName rules) of
    Just duplicateName ->
      Left (RewriteDuplicateRuleName duplicateName)
    Nothing ->
      Right ()

validateRawRuleIds ::
  [RuleCheck.RawRewriteRule guard f] ->
  Either (RewriteError capability f) ()
validateRawRuleIds rawRules =
  case firstDuplicate (fmap RuleCheck.rrId rawRules) of
    Just duplicateId ->
      Left (RewriteDuplicateRuleId duplicateId)

    Nothing ->
      Right ()

assignRuleIds ::
  [RuleSpec capability f] ->
  Either (RewriteError capability f) (Int, [(RewriteRuleId, RuleSpec capability f)])
assignRuleIds rules =
  case firstDuplicate explicitIds of
    Just duplicateId ->
      Left (RewriteDuplicateRuleId duplicateId)

    Nothing ->
      let ((_, nextCandidate), assignedRules) =
            mapAccumL assignRuleId (explicitIdSet, 0) rules
       in Right (nextCandidate, assignedRules)
  where
    explicitIds =
      mapMaybe rsId rules

    explicitIdSet =
      Set.fromList explicitIds

    assignRuleId ::
      (Set.Set RewriteRuleId, Int) ->
      RuleSpec capability f ->
      ((Set.Set RewriteRuleId, Int), (RewriteRuleId, RuleSpec capability f))
    assignRuleId (usedIds, nextCandidate) nextRule =
      let (rewriteRuleId, nextCandidate') =
            case rsId nextRule of
              Just explicitId ->
                (explicitId, nextCandidate)
              Nothing ->
                firstFreeRuleId usedIds nextCandidate
       in ( (Set.insert rewriteRuleId usedIds, nextCandidate'),
            (rewriteRuleId, nextRule)
          )

rewriteByRuleName :: RuleName -> CheckedSystem capability f -> Either (RewriteError capability f) (CheckedRewrite capability f)
rewriteByRuleName name checkedSystem =
  case lookupCheckedRewrite name checkedSystem of
    Nothing ->
      Left (RewriteUnknownRule name)

    Just rewriteValue ->
      Right rewriteValue

checkedSystemErrorToRewriteError :: CheckedSystemError -> RewriteError capability f
checkedSystemErrorToRewriteError checkedSystemError =
  case checkedSystemError of
    CheckedSystemDuplicateRuleName ruleName ->
      RewriteDuplicateRuleName ruleName
    CheckedSystemDuplicateRuleId ruleId ->
      RewriteDuplicateRuleId ruleId
