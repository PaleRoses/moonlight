{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Support.Compile
  ( compileSupportProgram,
    buildSupportProgram,
    compileSupportedRuleBook,
    compileSupportedFactBook,
  )
where

import Data.Bifunctor
  ( first,
  )
import Moonlight.Core
  ( RewriteRuleId,
    SupportIndexedRule,
  )
import Moonlight.Saturation.Context.Error
  ( SaturationCompileError (..),
    SaturationProgramSite (..),
  )
import Moonlight.Saturation.Context.Program.Plan
  ( Program,
    ProgramStage (CompiledProgramStage),
  )
import Moonlight.Saturation.Support.Core (SupportScheduleGroup)
import Moonlight.Saturation.Substrate
import Moonlight.Sheaf.Twist.FactClosure qualified as SheafTwist
import Moonlight.Sheaf.Twist.SupportedRuleSpec qualified as SheafTwist
import Moonlight.Sheaf.Twist.SiteProgram qualified as SheafTwist
import Moonlight.Sheaf.Twist.Compile qualified as TwistEngine
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    unionPreparedSupport,
  )

compileSupportProgram ::
  forall u.
  (RewriteSystem u, FactSystem u, Ord (SatContext u)) =>
  PreparedContextSite (SatContext u) ->
  SheafTwist.SupportedRuleBook (SatContext u) (SatRuleSource u) ->
  SheafTwist.SupportedFactBook (SatContext u) (SatFactSource u) ->
  Either
    (SaturationCompileError u (SupportScheduleGroup u))
    (Program 'CompiledProgramStage u)
compileSupportProgram site supportedRuleBook supportedFactBook = do
  compiledRules <-
    compileSupportedRuleBook @u supportedRuleBook
  compiledFactRules <-
    compileSupportedFactBook @u supportedFactBook
  buildSupportProgram @u site compiledFactRules compiledRules

buildSupportProgram ::
  forall u.
  (RewriteSystem u, Ord (SatContext u)) =>
  PreparedContextSite (SatContext u) ->
  [SheafTwist.CompiledSupportedFactRule (SupportBasis (SatContext u)) (SatFactRule u)] ->
  [SupportIndexedRule (SupportBasis (SatContext u)) (SatRule u)] ->
  Either
    (SaturationCompileError u (SupportScheduleGroup u))
    (Program 'CompiledProgramStage u)
buildSupportProgram site compiledFactRules compiledRules =
  first SaturationSupportContextLookupFailed $
    SheafTwist.buildSupportSiteProgram
      (unionPreparedSupport site)
      compiledFactRules
      compiledRules
      (rewriteRuleId @u)

compileSupportedRuleBook ::
  forall u.
  RewriteSystem u =>
  SheafTwist.SupportedRuleBook (SatContext u) (SatRuleSource u) ->
  Either
    (SaturationCompileError u (SupportScheduleGroup u))
    [SupportIndexedRule (SupportBasis (SatContext u)) (SatRule u)]
compileSupportedRuleBook supportedRuleBook =
  first
    (twistCompileFailure (SaturationRewriteRulesFailed WholeProgramSite))
    ( TwistEngine.compileSupportedRuleBookWith
        (rewriteRuleSourceId @u)
        (compileRewriteRules @u)
        (rewriteRuleId @u)
        supportedRuleBook
    )

compileSupportedFactBook ::
  forall u.
  FactSystem u =>
  SheafTwist.SupportedFactBook (SatContext u) (SatFactSource u) ->
  Either
    (SaturationCompileError u (SupportScheduleGroup u))
    [SheafTwist.CompiledSupportedFactRule (SupportBasis (SatContext u)) (SatFactRule u)]
compileSupportedFactBook supportedFactBook =
  first
    (twistCompileFailure (SaturationFactRulesFailed WholeProgramSite))
    ( TwistEngine.compileSupportedFactBookWith
        (factSourceId @u)
        (compileFactRules @u)
        (factRuleId @u)
        supportedFactBook
    )

twistCompileFailure ::
  (err -> SaturationCompileError u schedulerKey) ->
  TwistEngine.TwistCompileError (SatContext u) RewriteRuleId err ->
  SaturationCompileError u schedulerKey
twistCompileFailure sourceFailure compileError =
  case compileError of
    TwistEngine.TwistCompileSourceError sourceError ->
      sourceFailure sourceError
    TwistEngine.TwistCompileMismatch mismatchValue ->
      SaturationRuleBookMismatch mismatchValue
