{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Saturation.Context.Error
  ( SaturationProgramSite (..),
    SaturationBudgetError (..),
    GateCompatibilityError (..),
    PlanSpecViolation (..),
    PlanCompileError (..),
    RuleKind (..),
    ProgramRelation (..),
    ProgramViolation (..),
    SaturationSupportError (..),
    SaturationCompileError (..),
    RuntimeResumeError (..),
    SaturationRunError (..),
    SaturationError (..),
    validateSaturationBudget
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Moonlight.Core (RewriteRuleId)
import Moonlight.Saturation.Core (SaturationBudget (..))
import Moonlight.Control.Gate
  ( GateCompatibilityError (..),
  )
import Moonlight.Saturation.Substrate
import Moonlight.Sheaf.Twist.Compile
  ( TwistCompileMismatch,
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSupportError,
  )

type SaturationProgramSite :: Type -> Type
data SaturationProgramSite context
  = BaseProgramSite
  | WholeProgramSite
  | ContextProgramSite !context
  deriving stock (Eq, Ord, Show, Read)

type SaturationBudgetError :: Type
data SaturationBudgetError
  = NegativeHitIterationLimit !Int
  | NegativeHitNodeLimit !Int
  deriving stock (Eq, Ord, Show, Read)

type PlanCompileError :: Type -> Type
newtype PlanCompileError schedulerKey = PlanCompileError
  { unPlanCompileError :: NonEmpty (PlanSpecViolation schedulerKey)
  }

type PlanSpecViolation :: Type -> Type
data PlanSpecViolation schedulerKey
  = PlanSaturationBudgetViolation !SaturationBudgetError
  | PlanGuidanceCompatibilityViolation !(GateCompatibilityError schedulerKey)

deriving stock instance Eq schedulerKey => Eq (PlanCompileError schedulerKey)

deriving stock instance Show schedulerKey => Show (PlanCompileError schedulerKey)

deriving stock instance Eq schedulerKey => Eq (PlanSpecViolation schedulerKey)

deriving stock instance Show schedulerKey => Show (PlanSpecViolation schedulerKey)

type RuleKind :: Type
data RuleKind
  = RewriteRuleKind
  | FactRuleKind
  deriving stock (Eq, Ord, Show, Read)

type ProgramRelation :: Type
data ProgramRelation
  = DuplicateRuleId
  | DuplicateSupportDeclaration
  | UnknownActivatedRule
  | UnknownSupportRule
  deriving stock (Eq, Ord, Show, Read)

type ProgramViolation :: Type -> Type
data ProgramViolation context = ProgramViolation
  { pvSite :: !(SaturationProgramSite context),
    pvRuleKind :: !RuleKind,
    pvRelation :: !ProgramRelation,
    pvRuleIds :: !(Set RewriteRuleId)
  }
  deriving stock (Eq, Ord, Show, Read)

type SaturationSupportError :: Type -> Type
newtype SaturationSupportError u = SaturationSupportError
  { unSaturationSupportError :: NonEmpty (ProgramViolation (SatContext u))
  }

deriving stock instance Eq (SatContext u) => Eq (SaturationSupportError u)

deriving stock instance Show (SatContext u) => Show (SaturationSupportError u)

type SaturationCompileError :: Type -> Type -> Type
data SaturationCompileError u schedulerKey
  = SaturationRewriteRulesFailed
      !(SaturationProgramSite (SatContext u))
      !(SatRuleCompileError u)
  | SaturationFactRulesFailed
      !(SaturationProgramSite (SatContext u))
      !(SatFactCompileError u)
  | SaturationRuleBookMismatch
      !(TwistCompileMismatch (SatContext u) RewriteRuleId)
  | SaturationSupportContextLookupFailed
      !(PreparedContextSupportError (SatContext u))
  | SaturationSupportProgramInvalid !(SaturationSupportError u)
  | SaturationPlanInvalid !(PlanCompileError schedulerKey)

deriving stock instance
  ( Eq (SatContext u),
    Eq (SatRuleCompileError u),
    Eq (SatFactCompileError u),
    Eq schedulerKey
  ) =>
  Eq (SaturationCompileError u schedulerKey)

deriving stock instance
  ( Show (SatContext u),
    Show (SatRuleCompileError u),
    Show (SatFactCompileError u),
    Show schedulerKey
  ) =>
  Show (SaturationCompileError u schedulerKey)

type RuntimeResumeError :: Type -> Type
data RuntimeResumeError u
  = RuntimeResumeMissingPlanIdentity
  | RuntimeResumePlanChanged
  deriving stock (Eq, Show)

type SaturationRunError :: Type -> Type
data SaturationRunError u
  = SaturationRunSectionObstructed !(SatObstruction u)
  | SaturationRunApplyFailed !(SatApplicationError u)
  | SaturationRunSupportContextLookupFailed !(PreparedContextSupportError (SatContext u))
  | SaturationRunResumeIncompatible !(RuntimeResumeError u)

deriving stock instance
  ( Eq (SatObstruction u),
    Eq (SatApplicationError u),
    Eq (SatContext u)
  ) =>
  Eq (SaturationRunError u)

deriving stock instance
  ( Show (SatObstruction u),
    Show (SatApplicationError u),
    Show (SatContext u)
  ) =>
  Show (SaturationRunError u)

type SaturationError :: Type -> Type -> Type
data SaturationError u schedulerKey
  = SaturationCompileFailure !(SaturationCompileError u schedulerKey)
  | SaturationRunFailure !(SaturationRunError u)

deriving stock instance
  ( Eq (SatContext u),
    Eq (SatRuleCompileError u),
    Eq (SatFactCompileError u),
    Eq (SatObstruction u),
    Eq (SatApplicationError u),
    Eq schedulerKey
  ) =>
  Eq (SaturationError u schedulerKey)

deriving stock instance
  ( Show (SatContext u),
    Show (SatRuleCompileError u),
    Show (SatFactCompileError u),
    Show (SatObstruction u),
    Show (SatApplicationError u),
    Show schedulerKey
  ) =>
  Show (SaturationError u schedulerKey)

validateSaturationBudget :: SaturationBudget -> Either SaturationBudgetError ()
validateSaturationBudget budget
  | sbMaxIterations budget < 0 =
      Left (NegativeHitIterationLimit (sbMaxIterations budget))
  | sbMaxNodes budget < 0 =
      Left (NegativeHitNodeLimit (sbMaxNodes budget))
  | otherwise =
      Right ()
