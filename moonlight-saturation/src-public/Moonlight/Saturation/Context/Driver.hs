{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Driver
  ( ContextScheduler,
    ContextExecutionSpec (..),
    ContextRunSpec (..),
    ResumableRuntimeState,
    resumableRuntimeState,
    ContextRunResult (..),
    contextExecutionSpec,
    contextRunSpec,
    carrierGoal,
    compileContextProgram,
    runContextPlan,
    runContextPlanObserved,
    resumeContextPlan,
    runContextCompiledProgram,
    runContextFragment,
    runContextProgram,
    plainContextRunSpec,
    proofContextRunSpec,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Functor.Identity
  ( Identity,
  )
import Data.Kind
  ( Type,
  )
import Data.Proxy
  ( Proxy,
  )
import Moonlight.Control.Candidate
  ( CandidateSpace,
  )
import Moonlight.Saturation.Context.Error
  ( SaturationCompileError,
    SaturationError (..),
    SaturationRunError,
  )
import Moonlight.Saturation.Context.Program.Compile
  ( planFromCompiledProgram,
  )
import Moonlight.Saturation.Context.Program.Plan
  ( Plan,
    Program,
    ProgramStage (..),
  )
import Moonlight.Saturation.Context.Program.Source
  ( ProgramFragment,
    ProgramM,
    compileFragment,
    compileProgram,
  )
import Moonlight.Saturation.Context.Program.Spec
  ( PlanSpec,
  )
import Moonlight.Saturation.Context.Program.View
  ( SaturationRoundView,
  )
import Moonlight.Saturation.Context.Runtime.Carrier.Plain
  ( plainRuntimePolicy,
  )
import Moonlight.Saturation.Context.Runtime.Carrier.Proof
  ( proofRuntimePolicy,
  )
import Moonlight.Saturation.Context.Runtime.Engine
  ( RuntimeObservedResult (..),
    mapRuntimeObservedResult,
    resumePlanWithPolicyAndGoal,
    runPlanWithPolicyAndGoal,
    runPlanWithPolicyAndGoalObserved,
  )
import Moonlight.Saturation.Context.Runtime.Policy
  ( RuntimePolicy,
  )
import Moonlight.Saturation.Context.Runtime.Schedule.Decision
  ( RuntimeScheduleDecision,
  )
import Moonlight.Saturation.Context.Runtime.Report
  ( ProofSaturationReport,
    SaturationReport,
  )
import Moonlight.Saturation.Context.Runtime.State
  ( RuntimeState,
    rsCarrier,
  )
import Moonlight.Saturation.Core
  ( TerminationGoal,
    contramapGoal,
  )
import Moonlight.Control.Schedule
  ( SchedulerConfig,
  )
import Moonlight.Saturation.Substrate

type ContextScheduler :: Type -> Type -> Type -> Type
type ContextScheduler u carrier schedulerGroup =
  SchedulerConfig schedulerGroup ->
  SatRewriteContext u ->
  SaturationRoundView u ->
  CandidateSpace Identity schedulerGroup () (SatSupportedMatch u) ->
  RuntimeState u carrier schedulerGroup ->
  RuntimeScheduleDecision schedulerGroup (SatSupportedMatch u)

type ContextExecutionSpec :: Type -> Type -> Type -> Type -> Type
data ContextExecutionSpec u carrier schedulerGroup result = ContextExecutionSpec
  { cesPolicy :: !(RuntimePolicy u carrier schedulerGroup result),
    cesGoal :: !(TerminationGoal (RuntimeState u carrier schedulerGroup))
  }

type ContextRunSpec :: Type -> Type -> Type -> Type -> Type
data ContextRunSpec u carrier schedulerGroup result = ContextRunSpec
  { crsPlanSpec :: !(PlanSpec u carrier schedulerGroup),
    crsExecution :: !(ContextExecutionSpec u carrier schedulerGroup result)
  }

type ResumableRuntimeState :: Type -> Type -> Type -> Type
newtype ResumableRuntimeState u carrier schedulerGroup = ResumableRuntimeState
  { resumableRuntimeState :: RuntimeState u carrier schedulerGroup
  }

type ContextRunResult :: Type -> Type -> Type -> Type -> Type
data ContextRunResult u carrier schedulerGroup result = ContextRunResult
  { crrState :: !(ResumableRuntimeState u carrier schedulerGroup),
    crrResult :: !result
  }

contextExecutionSpec ::
  RuntimePolicy u carrier schedulerGroup result ->
  TerminationGoal (RuntimeState u carrier schedulerGroup) ->
  ContextExecutionSpec u carrier schedulerGroup result
contextExecutionSpec =
  ContextExecutionSpec

contextRunSpec ::
  PlanSpec u carrier schedulerGroup ->
  RuntimePolicy u carrier schedulerGroup result ->
  TerminationGoal (RuntimeState u carrier schedulerGroup) ->
  ContextRunSpec u carrier schedulerGroup result
contextRunSpec planSpecValue policyValue goalValue =
  ContextRunSpec
    { crsPlanSpec = planSpecValue,
      crsExecution = contextExecutionSpec policyValue goalValue
    }

carrierGoal ::
  TerminationGoal carrier ->
  TerminationGoal (RuntimeState u carrier schedulerGroup)
carrierGoal =
  contramapGoal rsCarrier

compileContextProgram ::
  forall u carrier schedulerGroup.
  (RewriteSystem u, FactSystem u, Ord (SatContext u)) =>
  PlanSpec u carrier schedulerGroup ->
  ProgramM u () ->
  Either
    (SaturationCompileError u schedulerGroup)
    (Plan u carrier schedulerGroup)
compileContextProgram =
  compileProgram @u

runContextPlan ::
  forall u carrier schedulerGroup result.
  ( RebuildSystem u,
    ApplicationResultSystem u,
    Ord (SatRuleKey u),
    Ord (SatContext u),
    Ord (SatClassId u),
    Eq (SatFactStore u),
    Semigroup (SatFactIndex u)
  ) =>
  ContextExecutionSpec u carrier schedulerGroup result ->
  Plan u carrier schedulerGroup ->
  carrier ->
  Either
    (SaturationRunError u)
    (ContextRunResult u carrier schedulerGroup result)
runContextPlan executionSpec planValue carrierValue =
  fmap
    contextRunResultFromPair
    ( runPlanWithPolicyAndGoal @u
        (cesPolicy executionSpec)
        planValue
        (cesGoal executionSpec)
        carrierValue
    )

runContextPlanObserved ::
  forall u carrier schedulerGroup result.
  ( RebuildSystem u,
    ApplicationResultSystem u,
    Ord (SatRuleKey u),
    Ord (SatContext u),
    Ord (SatClassId u),
    Eq (SatFactStore u),
    Semigroup (SatFactIndex u)
  ) =>
  ContextExecutionSpec u carrier schedulerGroup result ->
  Plan u carrier schedulerGroup ->
  carrier ->
  IO
    ( RuntimeObservedResult
        (SaturationRunError u)
        (ContextRunResult u carrier schedulerGroup result)
    )
runContextPlanObserved executionSpec planValue carrierValue =
  fmap
    (mapRuntimeObservedResult (fmap contextRunResultFromPair))
    ( runPlanWithPolicyAndGoalObserved @u
        (cesPolicy executionSpec)
        planValue
        (cesGoal executionSpec)
        carrierValue
    )

resumeContextPlan ::
  forall u carrier schedulerGroup result.
  ( RebuildSystem u,
    ApplicationResultSystem u,
    Eq (SatMatchStrategy u),
    Eq (SatFactRuleIdentity u),
    Eq (SatRewriteRuleIdentity u),
    Eq schedulerGroup,
    Ord (SatRuleKey u),
    Ord (SatContext u),
    Ord (SatClassId u),
    Eq (SatFactStore u),
    Semigroup (SatFactIndex u)
  ) =>
  ContextExecutionSpec u carrier schedulerGroup result ->
  Plan u carrier schedulerGroup ->
  ResumableRuntimeState u carrier schedulerGroup ->
  Either
    (SaturationRunError u)
    (ContextRunResult u carrier schedulerGroup result)
resumeContextPlan executionSpec planValue stateValue =
  fmap
    contextRunResultFromPair
    ( resumePlanWithPolicyAndGoal @u
        (cesPolicy executionSpec)
        planValue
        (cesGoal executionSpec)
        (resumableRuntimeState stateValue)
    )

runContextCompiledProgram ::
  forall u carrier schedulerGroup result.
  ( RebuildSystem u,
    ApplicationResultSystem u,
    Ord (SatRuleKey u),
    Ord (SatContext u),
    Ord (SatClassId u),
    Eq (SatFactStore u),
    Semigroup (SatFactIndex u)
  ) =>
  ContextRunSpec u carrier schedulerGroup result ->
  Program 'CompiledProgramStage u ->
  carrier ->
  Either
    (SaturationError u schedulerGroup)
    (ContextRunResult u carrier schedulerGroup result)
runContextCompiledProgram spec compiledProgram carrierValue = do
  planValue <-
    first SaturationCompileFailure $
      planFromCompiledProgram @u
        (crsPlanSpec spec)
        compiledProgram

  first SaturationRunFailure $
    runContextPlan
      (crsExecution spec)
      planValue
      carrierValue

runContextFragment ::
  forall u carrier schedulerGroup result.
  ( RebuildSystem u,
    ApplicationResultSystem u,
    Ord (SatContext u),
    Ord (SatRuleKey u),
    Ord (SatClassId u),
    Eq (SatFactStore u),
    Semigroup (SatFactIndex u)
  ) =>
  ContextRunSpec u carrier schedulerGroup result ->
  ProgramFragment u ->
  carrier ->
  Either
    (SaturationError u schedulerGroup)
    (ContextRunResult u carrier schedulerGroup result)
runContextFragment spec fragment carrierValue = do
  planValue <-
    first SaturationCompileFailure $
      compileFragment @u
        (crsPlanSpec spec)
        fragment

  first SaturationRunFailure $
    runContextPlan
      (crsExecution spec)
      planValue
      carrierValue

runContextProgram ::
  forall u carrier schedulerGroup result.
  ( RebuildSystem u,
    ApplicationResultSystem u,
    Ord (SatContext u),
    Ord (SatRuleKey u),
    Ord (SatClassId u),
    Eq (SatFactStore u),
    Semigroup (SatFactIndex u)
  ) =>
  ContextRunSpec u carrier schedulerGroup result ->
  ProgramM u () ->
  carrier ->
  Either
    (SaturationError u schedulerGroup)
    (ContextRunResult u carrier schedulerGroup result)
runContextProgram spec programValue carrierValue = do
  planValue <-
    first SaturationCompileFailure $
      compileProgram @u
        (crsPlanSpec spec)
        programValue

  first SaturationRunFailure $
    runContextPlan
      (crsExecution spec)
      planValue
      carrierValue

plainContextRunSpec ::
  forall u.
  ( RebuildSystem u,
    GraphApply u,
    Ord (SatRuleKey u),
    Ord (SatContext u),
    Ord (SatClassId u)
  ) =>
  PlanSpec u (SatGraph u) (SatRuleKey u) ->
  TerminationGoal (SatGraph u) ->
  ContextRunSpec
    u
    (SatGraph u)
    (SatRuleKey u)
    (SaturationReport u)
plainContextRunSpec planSpecValue graphGoal =
  contextRunSpec
    planSpecValue
    (plainRuntimePolicy @u)
    (carrierGoal graphGoal)

proofContextRunSpec ::
  forall u p.
  ( ProofCarrier u p,
    Ord (SatRuleKey u),
    Ord (SatContext u),
    Ord (SatClassId u)
  ) =>
  Proxy p ->
  SatProofBuilder u p ->
  Maybe (SatContext u) ->
  PlanSpec u (SatProofGraph u p) (SatRuleKey u) ->
  TerminationGoal (SatProofGraph u p) ->
  ContextRunSpec
    u
    (SatProofGraph u p)
    (SatRuleKey u)
    (ProofSaturationReport u (SatProofGraph u p))
proofContextRunSpec _ proofBuilder activeContext planSpecValue proofGraphGoal =
  contextRunSpec
    planSpecValue
    (proofRuntimePolicy @u @p proofBuilder activeContext)
    (carrierGoal proofGraphGoal)

contextRunResultFromPair ::
  (RuntimeState u carrier schedulerGroup, result) ->
  ContextRunResult u carrier schedulerGroup result
contextRunResultFromPair (finalState, resultValue) =
  ContextRunResult
    { crrState = ResumableRuntimeState finalState,
      crrResult = resultValue
    }
