{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Runtime.Engine
  ( RuntimePhaseResult (..),
    RuntimeIOTiming (..),
    RuntimeObservedResult (..),
    emptyRuntimeIOTiming,
    mapRuntimeObservedResult,
    runRuntimePhase,
    resumeRuntimePhase,
    runRuntime,
    runRuntimeObserved,
    resumeRuntime,
    runPlanWithPolicy,
    runPlanWithPolicyAndGoalObserved,
    resumePlanWithPolicy,
    runPlanWithPolicyAndGoal,
    runPlanWithPolicyAndGoalWithApplyIO,
    runPlanWithPolicyAndGoalWithApplyIOObserved,
    resumePlanWithPolicyAndGoal,
    runtimeStateFromCarrier,
  )
where

import Control.Exception (evaluate)
import Control.Monad.Trans.Class
  ( lift,
  )
import Control.Monad.Trans.State.Strict
  ( StateT,
    modify',
    runStateT,
  )
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty)
import Data.Kind (Constraint, Type)
import GHC.Clock (getMonotonicTimeNSec)
import Numeric.Natural (Natural)
import Moonlight.Saturation.Context.Error
  ( SaturationRunError (..),
  )
import Moonlight.Saturation.Context.Program.Plan
  ( Plan,
    planMatchingStrategy,
    planRewriteContext,
    planSaturationBudget,
  )
import Moonlight.Saturation.Context.Runtime.PlanIdentity
  ( ensureRuntimeResumeCompatible,
  )
import Moonlight.Saturation.Context.Runtime.Policy.Internal
  ( RuntimePolicy (..),
  )
import Moonlight.Saturation.Context.Runtime.Round
  ( RuntimeEnv (..),
    prepareRuntimeInitialState,
    refreshRuntimeStateFactViewCapabilityGeneration,
    runtimeKernel,
  )
import Moonlight.Saturation.Context.Runtime.State
  ( RuntimeReportWindow (..),
    RuntimeState (..),
    initialRuntimeState,
  )
import Moonlight.Saturation.Core
  ( ApplyOutcome (..),
    SaturationBudget,
    SaturationEffects (..),
    SaturationKernel (..),
    SaturationRun (..),
    SaturationTermination (..),
    TerminationGoal,
    runSaturation,
    runSaturationWith,
  )
import Moonlight.Control.Schedule.Round
  ( emptySchedulerState,
  )
import Moonlight.Saturation.Substrate

type RuntimePhaseResult :: Type -> Type -> Type -> Type
data RuntimePhaseResult u carrier schedulerGroup = RuntimePhaseResult
  { rprTermination :: !SaturationTermination,
    rprReportWindow :: !(RuntimeReportWindow u carrier schedulerGroup)
  }

type RuntimePlanResult :: Type -> Type -> Type -> Type -> Type
type RuntimePlanResult u carrier schedulerGroup result =
  (RuntimeState u carrier schedulerGroup, result)

type RuntimeApplyIO :: Type -> Type -> Type -> Type
type RuntimeApplyIO u carrier schedulerGroup =
  SatRewriteContext u ->
  NonEmpty (SatSupportedMatch u) ->
  RuntimeState u carrier schedulerGroup ->
  IO
    ( Either
        (SaturationRunError u)
        (ApplyOutcome (SatApplicationResult u) (RuntimeState u carrier schedulerGroup))
    )

type RuntimeRunConstraints :: Type -> Constraint
type RuntimeRunConstraints u =
  ( RebuildSystem u,
    ApplicationResultSystem u,
    Ord (SatRuleKey u),
    Ord (SatContext u),
    Ord (SatClassId u),
    Eq (SatFactStore u),
    Semigroup (SatFactIndex u)
  )

type RuntimeResumeConstraints :: Type -> Type -> Constraint
type RuntimeResumeConstraints u schedulerGroup =
  ( RuntimeRunConstraints u,
    Eq (SatMatchStrategy u),
    Eq (SatFactRuleIdentity u),
    Eq (SatRewriteRuleIdentity u),
    Eq schedulerGroup
  )

type RuntimeIOTiming :: Type
data RuntimeIOTiming = RuntimeIOTiming
  { ritRoundBuildNanoseconds :: !Natural,
    ritApplyNanoseconds :: !Natural,
    ritRebuildNanoseconds :: !Natural,
    ritCommitNanoseconds :: !Natural
  }
  deriving stock (Eq, Ord, Show, Read)

type RuntimeObservedResult :: Type -> Type -> Type
data RuntimeObservedResult err result = RuntimeObservedResult
  { rorTiming :: !RuntimeIOTiming,
    rorResult :: !(Either err result)
  }
  deriving stock (Eq, Ord, Show, Read)

type RuntimeTimingMode :: Type
data RuntimeTimingMode
  = RuntimeTimingDisabled
  | RuntimeTimingObserved
  deriving stock (Eq, Ord, Show, Read)

emptyRuntimeIOTiming :: RuntimeIOTiming
emptyRuntimeIOTiming =
  RuntimeIOTiming
    { ritRoundBuildNanoseconds = 0,
      ritApplyNanoseconds = 0,
      ritRebuildNanoseconds = 0,
      ritCommitNanoseconds = 0
    }
{-# INLINE emptyRuntimeIOTiming #-}

addRoundBuildTime :: Natural -> RuntimeIOTiming -> RuntimeIOTiming
addRoundBuildTime duration timing =
  timing {ritRoundBuildNanoseconds = ritRoundBuildNanoseconds timing + duration}
{-# INLINE addRoundBuildTime #-}

addApplyTime :: Natural -> RuntimeIOTiming -> RuntimeIOTiming
addApplyTime duration timing =
  timing {ritApplyNanoseconds = ritApplyNanoseconds timing + duration}
{-# INLINE addApplyTime #-}

addRebuildTime :: Natural -> RuntimeIOTiming -> RuntimeIOTiming
addRebuildTime duration timing =
  timing {ritRebuildNanoseconds = ritRebuildNanoseconds timing + duration}
{-# INLINE addRebuildTime #-}

addCommitTime :: Natural -> RuntimeIOTiming -> RuntimeIOTiming
addCommitTime duration timing =
  timing {ritCommitNanoseconds = ritCommitNanoseconds timing + duration}
{-# INLINE addCommitTime #-}

measureRuntimeDuration :: IO result -> IO (Natural, result)
measureRuntimeDuration action = do
  start <- getMonotonicTimeNSec
  result <- action
  end <- getMonotonicTimeNSec
  pure (fromIntegral (end - start), result)
{-# INLINE measureRuntimeDuration #-}

measureRuntimePhase :: RuntimeTimingMode -> IO result -> IO (Natural, result)
measureRuntimePhase timingMode action =
  case timingMode of
    RuntimeTimingDisabled ->
      fmap ((,) 0) action
    RuntimeTimingObserved ->
      measureRuntimeDuration action
{-# INLINE measureRuntimePhase #-}

mapRuntimeObservedResult ::
  (Either err result -> Either err' result') ->
  RuntimeObservedResult err result ->
  RuntimeObservedResult err' result'
mapRuntimeObservedResult transform observed =
  RuntimeObservedResult
    { rorTiming = rorTiming observed,
      rorResult = transform (rorResult observed)
    }
{-# INLINE mapRuntimeObservedResult #-}

runPreparedRuntimePhase ::
  forall u carrier schedulerGroup result.
  RuntimeRunConstraints u =>
  RuntimePolicy u carrier schedulerGroup result ->
  Plan u carrier schedulerGroup ->
  TerminationGoal (RuntimeState u carrier schedulerGroup) ->
  RuntimeState u carrier schedulerGroup ->
  Either
    (SaturationRunError u)
    (RuntimePhaseResult u carrier schedulerGroup)
runPreparedRuntimePhase policy plan terminationGoal reportInitialState = do
  run <-
    runSaturation
      (planSaturationBudget plan)
      ( runtimeKernel
          RuntimeEnv
            { rePolicy = policy,
              rePlan = plan,
              reGoal = terminationGoal
            }
          reportInitialState
      )
      reportInitialState

  pure
    RuntimePhaseResult
      { rprTermination = srTermination run,
        rprReportWindow =
          RuntimeReportWindow
            { rrwInitialState = reportInitialState,
              rrwFinalState = srFinalState run
            }
      }

runRuntimePhase ::
  forall u carrier schedulerGroup result.
  RuntimeRunConstraints u =>
  RuntimePolicy u carrier schedulerGroup result ->
  Plan u carrier schedulerGroup ->
  TerminationGoal (RuntimeState u carrier schedulerGroup) ->
  RuntimeState u carrier schedulerGroup ->
  Either
    (SaturationRunError u)
    (RuntimePhaseResult u carrier schedulerGroup)
runRuntimePhase policy plan terminationGoal initialState = do
  bootstrappedState <-
    prepareRuntimeInitialState
      policy
      plan
      initialState

  runPreparedRuntimePhase
    policy
    plan
    terminationGoal
    bootstrappedState
{-# INLINE runRuntimePhase #-}

resumeRuntimePhase ::
  forall u carrier schedulerGroup result.
  RuntimeResumeConstraints u schedulerGroup =>
  RuntimePolicy u carrier schedulerGroup result ->
  Plan u carrier schedulerGroup ->
  TerminationGoal (RuntimeState u carrier schedulerGroup) ->
  RuntimeState u carrier schedulerGroup ->
  Either
    (SaturationRunError u)
    (RuntimePhaseResult u carrier schedulerGroup)
resumeRuntimePhase policy plan terminationGoal state = do
  ensureRuntimeResumeCompatible @u plan state

  runPreparedRuntimePhase
    policy
    plan
    terminationGoal
    (refreshRuntimeStateFactViewCapabilityGeneration plan state)
{-# INLINE resumeRuntimePhase #-}

runRuntime ::
  forall u carrier schedulerGroup result.
  RuntimeRunConstraints u =>
  RuntimePolicy u carrier schedulerGroup result ->
  Plan u carrier schedulerGroup ->
  TerminationGoal (RuntimeState u carrier schedulerGroup) ->
  RuntimeState u carrier schedulerGroup ->
  Either
    (SaturationRunError u)
    (RuntimePlanResult u carrier schedulerGroup result)
runRuntime policy plan terminationGoal initialState = do
  phaseResult <-
    runRuntimePhase
      policy
      plan
      terminationGoal
      initialState
  let window =
        rprReportWindow phaseResult
  report <-
    first
      SaturationRunSectionObstructed
      (rpReport policy (rprTermination phaseResult) window)
  pure (rrwFinalState window, report)
{-# INLINE runRuntime #-}

runRuntimeObserved ::
  forall u carrier schedulerGroup result.
  RuntimeRunConstraints u =>
  RuntimePolicy u carrier schedulerGroup result ->
  Plan u carrier schedulerGroup ->
  TerminationGoal (RuntimeState u carrier schedulerGroup) ->
  RuntimeState u carrier schedulerGroup ->
  IO
    ( RuntimeObservedResult
        (SaturationRunError u)
        (RuntimePlanResult u carrier schedulerGroup result)
    )
runRuntimeObserved policy plan terminationGoal initialState =
  runRuntimeWithApplyIOMode
    RuntimeTimingObserved
    policy
    plan
    terminationGoal
    applyPure
    initialState
  where
    env =
      RuntimeEnv
        { rePolicy = policy,
          rePlan = plan,
          reGoal = terminationGoal
        }

    kernel =
      runtimeKernel env initialState

    applyPure _rewriteContext matches state =
      evaluate (skApply kernel matches state)
{-# INLINE runRuntimeObserved #-}

resumeRuntime ::
  forall u carrier schedulerGroup result.
  RuntimeResumeConstraints u schedulerGroup =>
  RuntimePolicy u carrier schedulerGroup result ->
  Plan u carrier schedulerGroup ->
  TerminationGoal (RuntimeState u carrier schedulerGroup) ->
  RuntimeState u carrier schedulerGroup ->
  Either
    (SaturationRunError u)
    (RuntimePlanResult u carrier schedulerGroup result)
resumeRuntime policy plan terminationGoal state = do
  phaseResult <-
    resumeRuntimePhase
      policy
      plan
      terminationGoal
      state
  let window =
        rprReportWindow phaseResult
  report <-
    first
      SaturationRunSectionObstructed
      (rpReport policy (rprTermination phaseResult) window)
  pure (rrwFinalState window, report)
{-# INLINE resumeRuntime #-}

runtimeStateFromCarrier ::
  forall u carrier schedulerGroup.
  (MatchingBackend u, Monoid (SatChangeSummary u)) =>
  Plan u carrier schedulerGroup ->
  carrier ->
  RuntimeState u carrier schedulerGroup
runtimeStateFromCarrier plan carrier =
  let matchState =
        initialMatchState @u
          (planMatchingStrategy plan)
          (planRewriteContext plan carrier)
   in initialRuntimeState @u matchState emptySchedulerState carrier
{-# INLINE runtimeStateFromCarrier #-}

runPlanWithPolicy ::
  forall u carrier schedulerGroup result.
  RuntimeRunConstraints u =>
  RuntimePolicy u carrier schedulerGroup result ->
  Plan u carrier schedulerGroup ->
  carrier ->
  Either
    (SaturationRunError u)
    (RuntimePlanResult u carrier schedulerGroup result)
runPlanWithPolicy policy plan =
  runPlanWithPolicyAndGoal
    policy
    plan
    mempty
{-# INLINE runPlanWithPolicy #-}

resumePlanWithPolicy ::
  forall u carrier schedulerGroup result.
  RuntimeResumeConstraints u schedulerGroup =>
  RuntimePolicy u carrier schedulerGroup result ->
  Plan u carrier schedulerGroup ->
  RuntimeState u carrier schedulerGroup ->
  Either
    (SaturationRunError u)
    (RuntimePlanResult u carrier schedulerGroup result)
resumePlanWithPolicy policy plan =
  resumePlanWithPolicyAndGoal
    policy
    plan
    mempty
{-# INLINE resumePlanWithPolicy #-}

runPlanWithPolicyAndGoal ::
  forall u carrier schedulerGroup result.
  RuntimeRunConstraints u =>
  RuntimePolicy u carrier schedulerGroup result ->
  Plan u carrier schedulerGroup ->
  TerminationGoal (RuntimeState u carrier schedulerGroup) ->
  carrier ->
  Either
    (SaturationRunError u)
    (RuntimePlanResult u carrier schedulerGroup result)
runPlanWithPolicyAndGoal policy plan terminationGoal carrier =
  runRuntime
    policy
    plan
    terminationGoal
    (runtimeStateFromCarrier @u plan carrier)
{-# INLINE runPlanWithPolicyAndGoal #-}

runPlanWithPolicyAndGoalObserved ::
  forall u carrier schedulerGroup result.
  RuntimeRunConstraints u =>
  RuntimePolicy u carrier schedulerGroup result ->
  Plan u carrier schedulerGroup ->
  TerminationGoal (RuntimeState u carrier schedulerGroup) ->
  carrier ->
  IO
    ( RuntimeObservedResult
        (SaturationRunError u)
        (RuntimePlanResult u carrier schedulerGroup result)
    )
runPlanWithPolicyAndGoalObserved policy plan terminationGoal carrier =
  runRuntimeObserved
    policy
    plan
    terminationGoal
    (runtimeStateFromCarrier @u plan carrier)
{-# INLINE runPlanWithPolicyAndGoalObserved #-}

runPlanWithPolicyAndGoalWithApplyIO ::
  forall u carrier schedulerGroup result.
  RuntimeRunConstraints u =>
  RuntimePolicy u carrier schedulerGroup result ->
  Plan u carrier schedulerGroup ->
  TerminationGoal (RuntimeState u carrier schedulerGroup) ->
  RuntimeApplyIO u carrier schedulerGroup ->
  carrier ->
  IO (Either (SaturationRunError u) (RuntimePlanResult u carrier schedulerGroup result))
runPlanWithPolicyAndGoalWithApplyIO policy plan terminationGoal applyMatches carrier =
  runRuntimeWithApplyIO
    policy
    plan
    terminationGoal
    applyMatches
    (runtimeStateFromCarrier @u plan carrier)
{-# INLINE runPlanWithPolicyAndGoalWithApplyIO #-}

runPlanWithPolicyAndGoalWithApplyIOObserved ::
  forall u carrier schedulerGroup result.
  RuntimeRunConstraints u =>
  RuntimePolicy u carrier schedulerGroup result ->
  Plan u carrier schedulerGroup ->
  TerminationGoal (RuntimeState u carrier schedulerGroup) ->
  RuntimeApplyIO u carrier schedulerGroup ->
  carrier ->
  IO
    ( RuntimeObservedResult
        (SaturationRunError u)
        (RuntimePlanResult u carrier schedulerGroup result)
    )
runPlanWithPolicyAndGoalWithApplyIOObserved policy plan terminationGoal applyMatches carrier =
  runRuntimeWithApplyIOObserved
    policy
    plan
    terminationGoal
    applyMatches
    (runtimeStateFromCarrier @u plan carrier)
{-# INLINE runPlanWithPolicyAndGoalWithApplyIOObserved #-}

runRuntimeWithApplyIO ::
  forall u carrier schedulerGroup result.
  RuntimeRunConstraints u =>
  RuntimePolicy u carrier schedulerGroup result ->
  Plan u carrier schedulerGroup ->
  TerminationGoal (RuntimeState u carrier schedulerGroup) ->
  RuntimeApplyIO u carrier schedulerGroup ->
  RuntimeState u carrier schedulerGroup ->
  IO (Either (SaturationRunError u) (RuntimePlanResult u carrier schedulerGroup result))
runRuntimeWithApplyIO policy plan terminationGoal applyMatches initialState =
  fmap
    rorResult
    ( runRuntimeWithApplyIOMode
        RuntimeTimingDisabled
        policy
        plan
        terminationGoal
        applyMatches
        initialState
    )

runRuntimeWithApplyIOObserved ::
  forall u carrier schedulerGroup result.
  RuntimeRunConstraints u =>
  RuntimePolicy u carrier schedulerGroup result ->
  Plan u carrier schedulerGroup ->
  TerminationGoal (RuntimeState u carrier schedulerGroup) ->
  RuntimeApplyIO u carrier schedulerGroup ->
  RuntimeState u carrier schedulerGroup ->
  IO
    ( RuntimeObservedResult
        (SaturationRunError u)
        (RuntimePlanResult u carrier schedulerGroup result)
    )
runRuntimeWithApplyIOObserved policy plan terminationGoal applyMatches initialState =
  runRuntimeWithApplyIOMode
    RuntimeTimingObserved
    policy
    plan
    terminationGoal
    applyMatches
    initialState

runRuntimeWithApplyIOMode ::
  forall u carrier schedulerGroup result.
  RuntimeRunConstraints u =>
  RuntimeTimingMode ->
  RuntimePolicy u carrier schedulerGroup result ->
  Plan u carrier schedulerGroup ->
  TerminationGoal (RuntimeState u carrier schedulerGroup) ->
  RuntimeApplyIO u carrier schedulerGroup ->
  RuntimeState u carrier schedulerGroup ->
  IO
    ( RuntimeObservedResult
        (SaturationRunError u)
        (RuntimePlanResult u carrier schedulerGroup result)
    )
runRuntimeWithApplyIOMode timingMode policy plan terminationGoal applyMatches initialState =
  case prepareRuntimeInitialState policy plan initialState of
    Left runError ->
      pure
        RuntimeObservedResult
          { rorTiming = emptyRuntimeIOTiming,
            rorResult = Left runError
          }
    Right bootstrappedState ->
      fmap
        ( mapRuntimeObservedResult
            (>>= runtimePlanResultFromRun policy bootstrappedState)
        )
        ( runSaturationWithApplyIOTimed
            timingMode
            (planSaturationBudget plan)
            ( runtimeKernel
                RuntimeEnv
                  { rePolicy = policy,
                    rePlan = plan,
                    reGoal = terminationGoal
                  }
                bootstrappedState
            )
            applyWithRewriteContext
            bootstrappedState
        )
  where
    applyWithRewriteContext matches state =
      applyMatches
        (planRewriteContext plan (rsCarrier state))
        matches
        state

runtimePlanResultFromRun ::
  RuntimePolicy u carrier schedulerGroup result ->
  RuntimeState u carrier schedulerGroup ->
  SaturationRun (RuntimeState u carrier schedulerGroup) ->
  Either
    (SaturationRunError u)
    (RuntimePlanResult u carrier schedulerGroup result)
runtimePlanResultFromRun policy reportInitialState runResult =
  let window =
        RuntimeReportWindow
          { rrwInitialState = reportInitialState,
            rrwFinalState = srFinalState runResult
          }
   in fmap
        ((,) (srFinalState runResult))
        ( first
            SaturationRunSectionObstructed
            (rpReport policy (srTermination runResult) window)
        )

runSaturationWithApplyIOTimed ::
  RuntimeTimingMode ->
  SaturationBudget ->
  SaturationKernel state round match effect err ->
  (NonEmpty match -> state -> IO (Either err (ApplyOutcome effect state))) ->
  state ->
  IO (RuntimeObservedResult err (SaturationRun state))
runSaturationWithApplyIOTimed !timingMode !budget !kernel applyMatches state =
  fmap
    runtimeObservedResultFromTimedRun
    ( runStateT
        ( runSaturationWith
            budget
            kernel
            (timedRuntimeEffects timingMode kernel applyMatches)
            state
        )
        emptyRuntimeIOTiming
    )

runtimeObservedResultFromTimedRun ::
  (Either err (SaturationRun state), RuntimeIOTiming) ->
  RuntimeObservedResult err (SaturationRun state)
runtimeObservedResultFromTimedRun (result, timing) =
  RuntimeObservedResult
    { rorTiming = timing,
      rorResult = result
    }

timedRuntimeEffects ::
  RuntimeTimingMode ->
  SaturationKernel state round match effect err ->
  (NonEmpty match -> state -> IO (Either err (ApplyOutcome effect state))) ->
  SaturationEffects (StateT RuntimeIOTiming IO) state round match effect err
timedRuntimeEffects timingMode kernel applyMatches =
  SaturationEffects
    { sePlanRound =
        \state ->
          measureTimedRuntimePhase
            timingMode
            addRoundBuildTime
            (evaluate (skPlanRound kernel state)),
      seApply =
        \matches state ->
          measureTimedRuntimePhase
            timingMode
            addApplyTime
            (applyMatches matches state),
      seRebuild =
        \roundValue effect state ->
          measureTimedRuntimePhase
            timingMode
            addRebuildTime
            (evaluate (skRebuild kernel roundValue effect state)),
      seCommit =
        \roundValue effect state ->
          measureTimedRuntimePhase
            timingMode
            addCommitTime
            (evaluate (skCommit kernel roundValue effect state))
    }

measureTimedRuntimePhase ::
  RuntimeTimingMode ->
  (Natural -> RuntimeIOTiming -> RuntimeIOTiming) ->
  IO result ->
  StateT RuntimeIOTiming IO result
measureTimedRuntimePhase timingMode recordDuration action = do
  (duration, result) <-
    lift (measureRuntimePhase timingMode action)
  modify' (recordDuration duration)
  pure result

resumePlanWithPolicyAndGoal ::
  forall u carrier schedulerGroup result.
  RuntimeResumeConstraints u schedulerGroup =>
  RuntimePolicy u carrier schedulerGroup result ->
  Plan u carrier schedulerGroup ->
  TerminationGoal (RuntimeState u carrier schedulerGroup) ->
  RuntimeState u carrier schedulerGroup ->
  Either
    (SaturationRunError u)
    (RuntimePlanResult u carrier schedulerGroup result)
resumePlanWithPolicyAndGoal =
  resumeRuntime
{-# INLINE resumePlanWithPolicyAndGoal #-}
