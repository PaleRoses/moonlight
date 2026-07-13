{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Program.Plan
  ( ProgramStage (..),
    SourceProgram,
    CompiledProgram,
    Program,
    ProgramFactRule,
    ProgramRewriteRule,
    SaturationGuidanceView (..),
    baseProgram,
    Plan,
    mkPlan,
    planPlanSpec,
    planSaturationBudget,
    planSchedulerConfig,
    planMatchingStrategy,
    planRewriteContextSnapshot,
    planRewriteContext,
    planGuidance,
    planProgram,
  )
where

import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core (RewriteRuleId)
import Moonlight.Core
  ( MatchActivationIndex (..),
    SiteIndex (..),
    SiteProgram (..),
  )
import Moonlight.Saturation.Context.Program.Spec
  ( PlanSpec,
    RewriteContextSnapshot,
    SaturationGuidanceView (..),
    planSpecSaturationBudget,
    planSpecGuidance,
    planSpecMatchingStrategy,
    planSpecRewriteContextSnapshot,
    planSpecRewriteContext,
    planSpecSchedulerConfig,
  )
import Moonlight.Saturation.Core
  ( SaturationBudget,
  )
import Moonlight.Control.Gate
  ( Gate,
    GuideRoundTrace,
  )
import Moonlight.Control.Schedule
  ( SchedulerConfig,
  )
import Moonlight.Saturation.Substrate

type ProgramStage :: Type
data ProgramStage
  = SourceProgramStage
  | CompiledProgramStage
  deriving stock (Eq, Ord, Show, Read)

type ProgramRewriteRule :: ProgramStage -> Type -> Type
type family ProgramRewriteRule stage u where
  ProgramRewriteRule 'SourceProgramStage u = SatRuleSource u
  ProgramRewriteRule 'CompiledProgramStage u = SatRule u

type ProgramFactRule :: ProgramStage -> Type -> Type
type family ProgramFactRule stage u where
  ProgramFactRule 'SourceProgramStage u = SatFactSource u
  ProgramFactRule 'CompiledProgramStage u = SatFactRule u

type Program :: ProgramStage -> Type -> Type
type Program stage u =
  SiteProgram
    (SatContext u)
    (ProgramRewriteRule stage u)
    (ProgramFactRule stage u)
    RewriteRuleId
    (SupportBasis (SatContext u))

type SourceProgram :: Type -> Type
type SourceProgram u =
  Program 'SourceProgramStage u

type CompiledProgram :: Type -> Type
type CompiledProgram u =
  Program 'CompiledProgramStage u

baseProgram ::
  (ProgramRewriteRule stage u -> RewriteRuleId) ->
  [ProgramRewriteRule stage u] ->
  [ProgramFactRule stage u] ->
  Program stage u
baseProgram rewriteRuleIdOf rewriteRules factRules =
  SiteProgram
    { spFactRules =
        SiteIndex
          { siBase = factRules,
            siContexts = Map.empty
          },
      spRewriteRules =
        SiteIndex
          { siBase = rewriteRules,
            siContexts = Map.empty
          },
      spSupportedFactRules = [],
      spSupportedRewriteRules = Map.empty,
      spRewriteActivation =
        MatchActivationIndex
          { maiBase = Set.fromList (fmap rewriteRuleIdOf rewriteRules),
            maiContexts = Map.empty
          },
      spBaseRewriteSupport = Map.empty
    }
{-# INLINE baseProgram #-}

type Plan :: Type -> Type -> Type -> Type
data Plan u carrier schedulerGroup = Plan
  { pSpec :: !(PlanSpec u carrier schedulerGroup),
    pProgram :: !(Program 'CompiledProgramStage u)
  }

mkPlan ::
  PlanSpec u carrier schedulerGroup ->
  Program 'CompiledProgramStage u ->
  Plan u carrier schedulerGroup
mkPlan =
  Plan
{-# INLINE mkPlan #-}

planPlanSpec ::
  Plan u carrier schedulerGroup ->
  PlanSpec u carrier schedulerGroup
planPlanSpec =
  pSpec
{-# INLINE planPlanSpec #-}

planSaturationBudget :: Plan u carrier schedulerGroup -> SaturationBudget
planSaturationBudget =
  planSpecSaturationBudget . pSpec
{-# INLINE planSaturationBudget #-}

planSchedulerConfig ::
  Plan u carrier schedulerGroup ->
  SchedulerConfig schedulerGroup
planSchedulerConfig =
  planSpecSchedulerConfig . pSpec
{-# INLINE planSchedulerConfig #-}

planMatchingStrategy :: Plan u carrier schedulerGroup -> SatMatchStrategy u
planMatchingStrategy =
  planSpecMatchingStrategy . pSpec
{-# INLINE planMatchingStrategy #-}

planRewriteContextSnapshot ::
  Plan u carrier schedulerGroup ->
  carrier ->
  RewriteContextSnapshot u
planRewriteContextSnapshot =
  planSpecRewriteContextSnapshot . pSpec
{-# INLINE planRewriteContextSnapshot #-}

planRewriteContext ::
  Plan u carrier schedulerGroup ->
  carrier ->
  SatRewriteContext u
planRewriteContext =
  planSpecRewriteContext . pSpec
{-# INLINE planRewriteContext #-}

planGuidance ::
  Plan u carrier schedulerGroup ->
  Gate
    (SaturationGuidanceView u)
    ()
    (SatSupportedMatch u)
    GuideRoundTrace
    schedulerGroup
planGuidance =
  planSpecGuidance . pSpec
{-# INLINE planGuidance #-}

planProgram :: Plan u carrier schedulerGroup -> Program 'CompiledProgramStage u
planProgram =
  pProgram
{-# INLINE planProgram #-}
