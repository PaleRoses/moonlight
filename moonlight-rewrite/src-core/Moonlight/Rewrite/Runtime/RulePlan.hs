{-# LANGUAGE GHC2024 #-}

-- | Runtime rule-plan record connecting compiled queries to RHS instantiation.
-- It owns rule identity, query and RHS projections, optional application
-- conditions, and the closed execution-error vocabulary shared with the executor.
module Moonlight.Rewrite.Runtime.RulePlan
  ( RulePlan (..),
    rulePlanPrimaryPattern,
    rulePlanRhsPattern,
    rulePlanCondition,
    rulePlanPostSubst,
    module Moonlight.Rewrite.Runtime.Rhs,
    RewriteApplicationError (..),
  )
where

import Data.Kind (Type)
import Moonlight.Core
  ( ClassId,
    Pattern,
    PatternVar,
    RewriteRuleId,
    UnionFindAllocationError,
  )
import Moonlight.Rewrite.Algebra
  ( CompiledApplicationCondition,
  )
import Moonlight.Rewrite.Algebra
  ( CompiledPatternQuery,
    cpqCondition,
    cpqPrimaryPattern,
  )
import Moonlight.Rewrite.Runtime.PostMatch
  ( PostMatchSubst,
  )
import Moonlight.Rewrite.Runtime.Rhs
import Moonlight.Rewrite.Runtime.Rhs.Internal qualified as RhsInternal

type RulePlan :: Type -> (Type -> Type) -> Type
data RulePlan compiledGuard f = RulePlan
  { rpId :: !RewriteRuleId,
    rpQuery :: !(CompiledPatternQuery compiledGuard f),
    rpRhs :: !(RhsInstantiationSpec f),
    rpApplicationCondition :: !(Maybe (CompiledApplicationCondition compiledGuard f))
  }

deriving stock instance
  ( Eq (CompiledPatternQuery compiledGuard f),
    Eq (RhsInstantiationSpec f),
    Eq (CompiledApplicationCondition compiledGuard f)
  ) =>
  Eq (RulePlan compiledGuard f)

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
rulePlanPostSubst rulePlan =
  case rpRhs rulePlan of
    RhsInternal.StaticRhs _ _ ->
      Nothing

    RhsInternal.PostMatchRhs postSubst _ ->
      Just postSubst

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
