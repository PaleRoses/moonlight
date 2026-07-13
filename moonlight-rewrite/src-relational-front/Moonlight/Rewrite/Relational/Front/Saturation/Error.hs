{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Rewrite.Relational.Front.Saturation.Error
  ( RelationalSaturationContext (..),
    RelationalSaturationPlanError (..),
    RelationalSaturationObstruction (..),
    relationalSaturationResumeError,
    prettyRelationalSaturationPlanError,
    prettyRelationalSaturationObstruction,
  )
where

import Data.Kind
  ( Type,
  )
import GHC.TypeLits
  ( Symbol,
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSupportError,
  )
import Moonlight.Rewrite.DSL
  ( ContextName,
  )
import Moonlight.Rewrite.Relational
  ( RewriteRunError,
  )
import Moonlight.Rewrite.Runtime
  ( RewriteApplicationError,
  )
import Moonlight.Rewrite.System
  ( RuleName,
  )
import Moonlight.FiniteLattice
  ( ContextLatticeCompileError,
    ContextLatticeLookupError
  )
import Moonlight.Saturation.Context.Error
  ( RuntimeResumeError (..),
  )

type RewriteSigKind :: Type
type RewriteSigKind = Symbol -> (Symbol -> Type) -> Type

data RelationalSaturationContext
  = RelationalBaseContext
  | RelationalNamedContext !ContextName
  deriving stock (Eq, Ord, Show)

data RelationalSaturationPlanError
  = RelationalSaturationContextLatticeCompileError !(ContextLatticeCompileError RelationalSaturationContext)
  | RelationalSaturationMissingRulePlan !RuleName
  | RelationalSaturationResumeMissingPlanIdentity
  | RelationalSaturationResumePlanChanged
  deriving stock (Eq, Show)

type RelationalSaturationObstruction :: RewriteSigKind -> Type
data RelationalSaturationObstruction sig
  = RelationalSaturationLatticeLookupFailed !(ContextLatticeLookupError RelationalSaturationContext)
  | RelationalSaturationPreparedSupportFailed !(PreparedContextSupportError RelationalSaturationContext)
  | RelationalSaturationRunFailed !(RewriteRunError ContextName)
  | RelationalSaturationHostRebuildFailed !RewriteApplicationError
  | RelationalSaturationUnsupportedMatchingRequest
  deriving stock (Show)

relationalSaturationResumeError ::
  RuntimeResumeError u ->
  RelationalSaturationPlanError
relationalSaturationResumeError =
  \case
    RuntimeResumeMissingPlanIdentity ->
      RelationalSaturationResumeMissingPlanIdentity

    RuntimeResumePlanChanged ->
      RelationalSaturationResumePlanChanged

prettyRelationalSaturationPlanError :: RelationalSaturationPlanError -> String
prettyRelationalSaturationPlanError errorValue =
  case errorValue of
    RelationalSaturationContextLatticeCompileError latticeError ->
      "relational saturation context lattice failed to compile: " <> show latticeError

    RelationalSaturationMissingRulePlan ruleNameValue ->
      "relational saturation rule has no compiled relational plan: " <> show ruleNameValue

    RelationalSaturationResumeMissingPlanIdentity ->
      "relational saturation cannot resume without the prior plan identity"

    RelationalSaturationResumePlanChanged ->
      "relational saturation cannot resume after the plan identity changed"

prettyRelationalSaturationObstruction :: RelationalSaturationObstruction sig -> String
prettyRelationalSaturationObstruction obstruction =
  case obstruction of
    RelationalSaturationLatticeLookupFailed latticeError ->
      "relational saturation lattice lookup failed: " <> show latticeError

    RelationalSaturationPreparedSupportFailed supportError ->
      "relational saturation prepared support failed: " <> show supportError

    RelationalSaturationRunFailed runError ->
      "relational saturation match run failed: " <> show runError

    RelationalSaturationHostRebuildFailed rebuildError ->
      "relational saturation host rebuild barrier failed: " <> show rebuildError

    RelationalSaturationUnsupportedMatchingRequest ->
      "relational saturation matching request is unsupported by the front substrate"
