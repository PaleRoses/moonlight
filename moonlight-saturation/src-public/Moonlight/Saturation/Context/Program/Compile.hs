{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Program.Compile
  ( compileSourceProgram,
    planFromCompiledProgram,
    compileBase,
  )
where

import Data.Bifunctor (first)
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( SiteProgram (..),
  )
import Moonlight.Saturation.Context.Error
  ( SaturationCompileError (..),
  )
import Moonlight.Saturation.Context.Program.Plan
  ( Plan,
    Program,
    ProgramStage (..),
    baseProgram,
    mkPlan,
  )
import Moonlight.Saturation.Context.Program.Spec
  ( PlanSpec,
    canonicalPlanSpec,
    validatePlanSpec,
  )
import Moonlight.Saturation.Context.Program.Internal.SiteIndex
  ( compileSiteIndex,
  )
import Moonlight.Saturation.Context.Program.Internal.Validate
  ( validateProgram,
    validateSourceProgram,
  )
import Moonlight.Saturation.Substrate

compileSourceProgram ::
  forall u carrier schedulerGroup.
  (RewriteSystem u, FactSystem u) =>
  PlanSpec u carrier schedulerGroup ->
  Program 'SourceProgramStage u ->
  Either
    (SaturationCompileError u schedulerGroup)
    (Plan u carrier schedulerGroup)
compileSourceProgram spec sourceProgram = do
  first SaturationSupportProgramInvalid $
    validateSourceProgram @u sourceProgram

  compiledRewriteRules <-
    compileSiteIndex
      (compileRewriteRules @u)
      SaturationRewriteRulesFailed
      (spRewriteRules sourceProgram)

  compiledFactRules <-
    compileSiteIndex
      (compileFactRules @u)
      SaturationFactRulesFailed
      (spFactRules sourceProgram)

  let compiledProgram :: Program 'CompiledProgramStage u
      compiledProgram =
        SiteProgram
          { spFactRules = compiledFactRules,
            spRewriteRules = compiledRewriteRules,
            spSupportedFactRules = [],
            spSupportedRewriteRules = Map.empty,
            spRewriteActivation = spRewriteActivation sourceProgram,
            spBaseRewriteSupport = spBaseRewriteSupport sourceProgram
          }

  planFromCompiledProgram @u spec compiledProgram

planFromCompiledProgram ::
  forall u carrier schedulerGroup.
  (RewriteSystem u, FactSystem u) =>
  PlanSpec u carrier schedulerGroup ->
  Program 'CompiledProgramStage u ->
  Either
    (SaturationCompileError u schedulerGroup)
    (Plan u carrier schedulerGroup)
planFromCompiledProgram spec compiledProgram = do
  first SaturationPlanInvalid $
    validatePlanSpec spec

  let canonicalSpec =
        canonicalPlanSpec spec

  first SaturationSupportProgramInvalid $
    validateProgram @u compiledProgram

  pure (mkPlan canonicalSpec compiledProgram)

compileBase ::
  forall u carrier schedulerGroup.
  (RewriteSystem u, FactSystem u) =>
  PlanSpec u carrier schedulerGroup ->
  [SatRuleSource u] ->
  [SatFactSource u] ->
  Either
    (SaturationCompileError u schedulerGroup)
    (Plan u carrier schedulerGroup)
compileBase spec rewriteRuleSources factRuleSources =
  compileSourceProgram @u
    spec
    ( baseProgram
        @'SourceProgramStage
        @u
        (rewriteRuleSourceId @u)
        rewriteRuleSources
        factRuleSources
    )
