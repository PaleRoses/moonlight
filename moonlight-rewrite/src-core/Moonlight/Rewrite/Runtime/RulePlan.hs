{-# LANGUAGE GHC2024 #-}

module Moonlight.Rewrite.Runtime.RulePlan
  ( RulePlan,
    rpId,
    rpQuery,
    rpRhs,
    rpApplicationCondition,
    RulePlanError (..),
    certifyRulePlan,
    rulePlanPrimaryPattern,
    rulePlanRhsPattern,
    rulePlanCondition,
    rulePlanPostSubst,
    module Moonlight.Rewrite.Runtime.Rhs,
    RewriteApplicationError (..),
  )
where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.Set qualified as Set
import Moonlight.Core
  ( ClassId,
    Pattern,
    PatternVar,
    RewriteRuleId,
    UnionFindAllocationError,
    patternVariables,
  )
import Moonlight.Rewrite.Algebra
  ( ApplicationConditionCompileError,
    CompiledApplicationCondition,
    CompiledPatternQuery,
    compiledApplicationConditionExtensions,
    cpqCondition,
    cpqPrimaryPattern,
    cpqQuery,
    patternQueryVariables,
    validateCompiledPatternExtensionAnchors,
  )
import Moonlight.Rewrite.Runtime.PostMatch
  ( PostMatchSubst,
    postMatchSubstVariables,
  )
import Moonlight.Rewrite.Runtime.Rhs
import Moonlight.Rewrite.Runtime.Rhs.Internal qualified as RhsInternal

type RulePlan :: Type -> (Type -> Type) -> Type
data RulePlan compiledGuard f = RulePlan
  !RewriteRuleId
  !(CompiledPatternQuery compiledGuard f)
  !(RhsInstantiationSpec f)
  !(Maybe (CompiledApplicationCondition compiledGuard f))

deriving stock instance
  ( Eq (CompiledPatternQuery compiledGuard f),
    Eq (RhsInstantiationSpec f),
    Eq (CompiledApplicationCondition compiledGuard f)
  ) =>
  Eq (RulePlan compiledGuard f)

rpId :: RulePlan compiledGuard f -> RewriteRuleId
rpId (RulePlan rewriteRuleId _query _rhs _applicationCondition) =
  rewriteRuleId

rpQuery :: RulePlan compiledGuard f -> CompiledPatternQuery compiledGuard f
rpQuery (RulePlan _rewriteRuleId query _rhs _applicationCondition) =
  query

rpRhs :: RulePlan compiledGuard f -> RhsInstantiationSpec f
rpRhs (RulePlan _rewriteRuleId _query rhs _applicationCondition) =
  rhs

rpApplicationCondition ::
  RulePlan compiledGuard f ->
  Maybe (CompiledApplicationCondition compiledGuard f)
rpApplicationCondition (RulePlan _rewriteRuleId _query _rhs applicationCondition) =
  applicationCondition

type RulePlanError :: Type
data RulePlanError
  = RulePlanRhsIntroducesUnboundVariables !RewriteRuleId ![PatternVar]
  | RulePlanPostSubstitutionIntroducesUnboundVariables !RewriteRuleId ![PatternVar]
  | RulePlanApplicationConditionInvalid !RewriteRuleId !ApplicationConditionCompileError
  deriving stock (Eq, Ord, Show)

certifyRulePlan ::
  Foldable f =>
  RewriteRuleId ->
  CompiledPatternQuery compiledGuard f ->
  RhsInstantiationSpec f ->
  Maybe (CompiledApplicationCondition compiledGuard f) ->
  Either RulePlanError (RulePlan compiledGuard f)
certifyRulePlan rewriteRuleId compiledQuery rhsSpec applicationCondition = do
  let boundVariables = patternQueryVariables (cpqQuery compiledQuery)
      unboundRhsVariables =
        Set.toAscList
          (Set.difference (patternVariables (RhsInternal.rhsInstantiationSpecPattern rhsSpec)) boundVariables)
      unboundPostSubstitutionVariables =
        Set.toAscList
          ( Set.difference
              (foldMap postMatchSubstVariables (rhsInstantiationPostSubst rhsSpec))
              boundVariables
          )
  case (unboundRhsVariables, unboundPostSubstitutionVariables) of
    (_ : _, _) ->
      Left (RulePlanRhsIntroducesUnboundVariables rewriteRuleId unboundRhsVariables)
    (_, _ : _) ->
      Left (RulePlanPostSubstitutionIntroducesUnboundVariables rewriteRuleId unboundPostSubstitutionVariables)
    ([], []) -> do
      traverse_
        ( first
            (RulePlanApplicationConditionInvalid rewriteRuleId)
            . validateCompiledPatternExtensionAnchors boundVariables
        )
        (foldMap compiledApplicationConditionExtensions applicationCondition)
      Right (RulePlan rewriteRuleId compiledQuery rhsSpec applicationCondition)

rhsInstantiationPostSubst :: RhsInstantiationSpec f -> Maybe (PostMatchSubst f)
rhsInstantiationPostSubst rhsSpec =
  case rhsSpec of
    RhsInternal.StaticRhs _ _ ->
      Nothing
    RhsInternal.PostMatchRhs postSubstitution _ ->
      Just postSubstitution

rulePlanPrimaryPattern :: RulePlan compiledGuard f -> Pattern f
rulePlanPrimaryPattern =
  cpqPrimaryPattern . rpQuery

rulePlanRhsPattern :: RulePlan compiledGuard f -> Pattern f
rulePlanRhsPattern =
  RhsInternal.rhsInstantiationSpecPattern . rpRhs

rulePlanCondition :: RulePlan compiledGuard f -> Maybe compiledGuard
rulePlanCondition =
  cpqCondition . rpQuery

rulePlanPostSubst :: RulePlan compiledGuard f -> Maybe (PostMatchSubst f)
rulePlanPostSubst =
  rhsInstantiationPostSubst . rpRhs

type RewriteApplicationError :: Type
data RewriteApplicationError
  = RewriteMissingBinding !PatternVar
  | RewriteMissingEClass !ClassId
  | RewriteMissingInstantiatedNode
  | RewriteDuplicateInstantiationRef !Int
  | RewriteInstantiationInputUnavailable !Int
  | RewriteMissingBinderSubstAlgebra !RewriteRuleId
  | RewriteConditionRejected !RewriteRuleId
  | RewriteUnloweredBinderScope
  | RewriteClassSortMismatch !ClassId !ClassId
  | RewriteNodeChildSortMismatch !ClassId !String !String
  | RewriteProgramReadAfterMerge
  | RewriteClassIdAllocationFailed !UnionFindAllocationError
  deriving stock (Eq, Ord, Show, Read)
