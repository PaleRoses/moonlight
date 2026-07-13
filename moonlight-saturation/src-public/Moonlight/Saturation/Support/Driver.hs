{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Support.Driver
  ( prepareSupportPlan,
    runSupportPlan,
    runSupportPlanObserved,
    resumeSupportPlan,
  )
where

import Data.Bifunctor (first)
import Moonlight.Saturation.Context.Driver
  ( ContextExecutionSpec,
    ContextRunResult,
    ResumableRuntimeState,
    resumeContextPlan,
    runContextPlan,
    runContextPlanObserved,
  )
import Moonlight.Saturation.Context.Error (SaturationError (..))
import Moonlight.Saturation.Context.Program.Compile (planFromCompiledProgram)
import Moonlight.Saturation.Context.Program.Plan
  ( Plan,
    Program,
    ProgramStage (CompiledProgramStage),
  )
import Moonlight.Saturation.Context.Program.Spec (PlanSpec)
import Moonlight.Saturation.Context.Runtime.Engine
  ( RuntimeObservedResult,
    mapRuntimeObservedResult,
  )
import Moonlight.Saturation.Substrate
import Moonlight.Saturation.Support.Core (SupportScheduleGroup)

prepareSupportPlan ::
  forall u carrier.
  (RewriteSystem u, FactSystem u) =>
  PlanSpec u carrier (SupportScheduleGroup u) ->
  Program 'CompiledProgramStage u ->
  Either
    (SaturationError u (SupportScheduleGroup u))
    (Plan u carrier (SupportScheduleGroup u))
prepareSupportPlan planSpecValue =
  first SaturationCompileFailure
    . planFromCompiledProgram @u planSpecValue

runSupportPlan ::
  forall u carrier result.
  ( RebuildSystem u,
    ApplicationResultSystem u,
    Ord (SatRuleKey u),
    Ord (SatContext u),
    Ord (SatClassId u),
    Eq (SatFactStore u),
    Semigroup (SatFactIndex u)
  ) =>
  ContextExecutionSpec u carrier (SupportScheduleGroup u) result ->
  Plan u carrier (SupportScheduleGroup u) ->
  carrier ->
  Either
    (SaturationError u (SupportScheduleGroup u))
    (ContextRunResult u carrier (SupportScheduleGroup u) result)
runSupportPlan executionSpec planValue =
  first SaturationRunFailure
    . runContextPlan @u executionSpec planValue

runSupportPlanObserved ::
  forall u carrier result.
  ( RebuildSystem u,
    ApplicationResultSystem u,
    Ord (SatRuleKey u),
    Ord (SatContext u),
    Ord (SatClassId u),
    Eq (SatFactStore u),
    Semigroup (SatFactIndex u)
  ) =>
  ContextExecutionSpec u carrier (SupportScheduleGroup u) result ->
  Plan u carrier (SupportScheduleGroup u) ->
  carrier ->
  IO
    ( RuntimeObservedResult
        (SaturationError u (SupportScheduleGroup u))
        (ContextRunResult u carrier (SupportScheduleGroup u) result)
    )
runSupportPlanObserved executionSpec planValue carrier =
  fmap
    (mapRuntimeObservedResult (first SaturationRunFailure))
    (runContextPlanObserved @u executionSpec planValue carrier)

resumeSupportPlan ::
  forall u carrier result.
  ( RebuildSystem u,
    ApplicationResultSystem u,
    Eq (SatMatchStrategy u),
    Eq (SatFactRuleIdentity u),
    Eq (SatRewriteRuleIdentity u),
    Eq (SupportScheduleGroup u),
    Ord (SatRuleKey u),
    Ord (SatContext u),
    Ord (SatClassId u),
    Eq (SatFactStore u),
    Semigroup (SatFactIndex u)
  ) =>
  ContextExecutionSpec u carrier (SupportScheduleGroup u) result ->
  Plan u carrier (SupportScheduleGroup u) ->
  ResumableRuntimeState u carrier (SupportScheduleGroup u) ->
  Either
    (SaturationError u (SupportScheduleGroup u))
    (ContextRunResult u carrier (SupportScheduleGroup u) result)
resumeSupportPlan executionSpec planValue =
  first SaturationRunFailure
    . resumeContextPlan @u executionSpec planValue
