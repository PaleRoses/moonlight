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
import Data.Set qualified as Set
import Moonlight.Core
  ( Language,
    Pattern,
    PatternVar,
    RewriteRuleId,
    firstDuplicate,
    patternVariables,
    rewriteRuleIdKey,
  )
import Moonlight.Rewrite.Algebra
  ( ApplicationCondition (..),
    CompositionError (..),
    PatternExtension (..),
    PatternRewriteError (..),
    patternQueryConditions,
    patternQueryVariables,
  )
import Moonlight.Rewrite.System.Checked
  ( CheckedRewrite,
    CheckedSystem,
    CheckedSystemError (..),
    checkedSystemFromRewrites,
    lookupCheckedRewrite,
  )
import Moonlight.Rewrite.System.Checked.Internal
  ( assignRewriteRuleIds,
    checkedRewriteFromAlgebra,
  )
import Moonlight.Rewrite.System.Logic.Guard
  ( CompiledGuard,
    RewriteCondition (..),
    compileGuard,
    guardAtomVariables,
  )
import Moonlight.Rewrite.Runtime (postMatchSubstVariables)
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
import Moonlight.Rewrite.System.Variable
  ( RuleVariableMetadataError,
    RuleVariables,
    ruleVariableKeys,
    untypedRuleVariables,
  )

type RewriteError :: Type -> (Type -> Type) -> Type
data RewriteError capability f
  = RewriteDuplicateRuleName !RuleName
  | RewriteDuplicateRuleId !RewriteRuleId
  | RewriteInvalidRuleId !RewriteRuleId
  | RewriteRuleIdExhausted
  | RewriteInvalidRuleVariableMetadata !RuleName !(Set.Set PatternVar) !(Set.Set PatternVar)
  | RewriteVariableMetadataFailure !RuleVariableMetadataError
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
    RewriteInvalidRuleId ruleId ->
      "invalid negative rewrite rule id: " <> show ruleId
    RewriteRuleIdExhausted ->
      "rewrite rule id space is exhausted"
    RewriteInvalidRuleVariableMetadata ruleName missingVariables unexpectedVariables ->
      "rewrite rule variable metadata does not match the rule semantics for "
        <> show ruleName
        <> ": missing "
        <> show missingVariables
        <> ", unexpected "
        <> show unexpectedVariables
    RewriteVariableMetadataFailure metadataError ->
      "rewrite variable metadata composition failed: " <> show metadataError
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
    IncompatibleBoundary {} ->
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
  assignedIds <-
    first checkedSystemErrorToRewriteError
      (assignRewriteRuleIds (fmap rsId rules))
  rewriteValues <-
    traverse checkAssignedRule (zip assignedIds rules)
  first checkedSystemErrorToRewriteError
    (checkedSystemFromRewrites rewriteValues)
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
    (checkedSystemFromRewrites checkedRules)

checkAssignedRule ::
  (Language f, Ord capability) =>
  (RewriteRuleId, RuleSpec capability f) ->
  Either (RewriteError capability f) (CheckedRewrite capability f)
checkAssignedRule (rewriteRuleId, ruleValue) = do
  validateRuleVariables ruleValue
  checkedRawRule <-
    first RewriteCompileFailure
      ( RuleCheck.checkRawRewrite
          compileGuard
          (rawRuleFromRule rewriteRuleId ruleValue)
      )
  elaborateNamedCheckedRewrite (rsName ruleValue) (rsVariables ruleValue) checkedRawRule

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
  elaborateNamedCheckedRewrite
    syntheticName
    (untypedRuleVariables (patternVariables (RuleCheck.chrLhsPattern checkedRawRule)))
    checkedRawRule

elaborateNamedCheckedRewrite ::
  (Language f, Ord capability) =>
  RuleName ->
  RuleVariables ->
  RuleCheck.CheckedRawRewriteRule (CompiledGuard capability f) f ->
  Either (RewriteError capability f) (CheckedRewrite capability f)
elaborateNamedCheckedRewrite name variables checkedRawRule = do
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

  Right (checkedRewriteFromAlgebra rewriteRuleId name algebraicRewrite variables)

validateRuleVariables ::
  Foldable f =>
  RuleSpec capability f ->
  Either (RewriteError capability f) ()
validateRuleVariables ruleValue =
  let semanticVariables =
        ruleSpecVariables ruleValue
      metadataVariables =
        ruleVariableKeys (rsVariables ruleValue)
      missingVariables =
        Set.difference semanticVariables metadataVariables
      unexpectedVariables =
        Set.difference metadataVariables semanticVariables
   in if Set.null missingVariables && Set.null unexpectedVariables
        then Right ()
        else
          Left
            ( RewriteInvalidRuleVariableMetadata
                (rsName ruleValue)
                missingVariables
                unexpectedVariables
            )

ruleSpecVariables ::
  Foldable f =>
  RuleSpec capability f ->
  Set.Set PatternVar
ruleSpecVariables ruleValue =
  patternVariables (rsLhs ruleValue)
    <> patternVariables (rsRhs ruleValue)
    <> foldMap rewriteConditionVariables (rsCondition ruleValue)
    <> foldMap postMatchSubstVariables (rsPostSubst ruleValue)
    <> foldMap applicationConditionVariables (rsApplicationCondition ruleValue)

applicationConditionVariables ::
  Foldable f =>
  ApplicationCondition (RewriteCondition capability f) f ->
  Set.Set PatternVar
applicationConditionVariables =
  foldMap patternExtensionVariables . unApplicationCondition

patternExtensionVariables ::
  Foldable f =>
  PatternExtension (RewriteCondition capability f) f ->
  Set.Set PatternVar
patternExtensionVariables extension =
  let query = peQuery extension
   in patternQueryVariables query
        <> foldMap
          rewriteConditionVariables
          (patternQueryConditions query)

rewriteConditionVariables ::
  Foldable f =>
  RewriteCondition capability f ->
  Set.Set PatternVar
rewriteConditionVariables =
  foldMap guardAtomVariables . rewriteGuardExpr

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
  ()
    <$ first checkedSystemErrorToRewriteError
      (assignRewriteRuleIds (fmap (Just . RuleCheck.rrId) rawRules))

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
    CheckedSystemInvalidRuleId ruleId ->
      RewriteInvalidRuleId ruleId
    CheckedSystemRuleIdExhausted ->
      RewriteRuleIdExhausted
