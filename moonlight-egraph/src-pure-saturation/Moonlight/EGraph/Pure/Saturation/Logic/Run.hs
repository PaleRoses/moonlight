{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Pure.Saturation.Logic.Run
  ( EGraphLogicConstraints,
    EGraphLogic (..),
    EGraphLogicM,
    logic,
    source,
    sourceFragment,
    seedFacts,
    observe,
    observeRun,
    EGraphLogicError (..),
    EGraphLogicReport (..),
    EGraphLogicObservedReport (..),
    compileEGraphLogic,
    runEGraphLogic,
    runCompiledEGraphLogic,
    runCompiledEGraphLogicObserved,
  )
where

import Control.Monad (ap)
import Data.Bifunctor (first)
import Data.Kind (Constraint, Type)
import Moonlight.Algebra (JoinSemilattice)
import Moonlight.Core
  ( ConstructorTag,
    HasConstructorTag,
    Language,
    RewriteRuleId,
  )
import Moonlight.EGraph.Pure.Saturation.Logic.Observation
  ( SomeStableObservation (..),
    SomeStableObservationResult,
    StableObservation,
    StableObservationError,
    runSomeStableObservations,
  )
import Moonlight.EGraph.Pure.Saturation.Logic.RunObservation
  ( RunObservation,
    SomeRunObservation (..),
    SomeRunObservationResult,
    runSomeRunObservations,
  )
import Moonlight.EGraph.Pure.Saturation.Logic.Seed
  ( SeedFacts,
    appendSeedFacts,
    emptySeedFacts,
    resolveSeedFacts,
    singletonSeedFacts,
  )
import Moonlight.EGraph.Pure.Saturation.Substrate (EGraphU)
import Moonlight.EGraph.Saturation.Context.State (SaturatingContextEGraph)
import Moonlight.Saturation.Context.Driver
  ( ContextExecutionSpec (..),
    ContextRunSpec (..),
  )
import Moonlight.Saturation.Context.Error
  ( SaturationCompileError,
    SaturationRunError,
  )
import Moonlight.Saturation.Context.Program.Plan (Plan)
import Moonlight.Saturation.Context.Program.Source
  ( ProgramFragment,
    ProgramM,
    appendProgramFragments,
    compileFragment,
    emptyProgramFragment,
    program,
  )
import Moonlight.Saturation.Context.Program.Spec (PlanSpec)
import Moonlight.Saturation.Context.Runtime.Engine
  ( RuntimeObservedResult (..),
    RuntimeIOTiming,
    runRuntime,
    runRuntimeObserved,
    runtimeStateFromCarrier,
  )
import Moonlight.Saturation.Context.Runtime.Report
  ( SaturationReport,
    srCarrier,
  )
import Moonlight.Saturation.Context.Runtime.State
  ( RuntimeState,
    seedRuntimeStateFacts,
  )
import Moonlight.Saturation.Matching (MatchSite)
import Moonlight.Saturation.Substrate (SatFactStore)

-- | The concrete constraints required by the existing 'EGraphU' instances.
type EGraphLogicConstraints :: Type -> (Type -> Type) -> Type -> Type -> Constraint
type EGraphLogicConstraints capability f a c =
  ( Language f,
    HasConstructorTag f,
    Show (ConstructorTag f),
    Show (f ()),
    Eq a,
    Ord a,
    JoinSemilattice a,
    Ord capability,
    Show capability,
    Ord c
  )

data EGraphLogic capability f a c = EGraphLogic
  { eglSource :: !(ProgramFragment (EGraphU capability f a c)),
    eglSeedFacts :: !(SeedFacts (EGraphU capability f a c)),
    eglObservations :: ![SomeStableObservation f a c],
    eglRunObservations :: ![SomeRunObservation c]
  }

emptyEGraphLogic :: EGraphLogic capability f a c
emptyEGraphLogic =
  EGraphLogic
    { eglSource = emptyProgramFragment,
      eglSeedFacts = emptySeedFacts,
      eglObservations = [],
      eglRunObservations = []
    }
{-# INLINE emptyEGraphLogic #-}

appendEGraphLogic :: EGraphLogic capability f a c -> EGraphLogic capability f a c -> EGraphLogic capability f a c
appendEGraphLogic leftLogic rightLogic =
  EGraphLogic
    { eglSource = appendProgramFragments (eglSource leftLogic) (eglSource rightLogic),
      eglSeedFacts = appendSeedFacts (eglSeedFacts leftLogic) (eglSeedFacts rightLogic),
      eglObservations = eglObservations leftLogic <> eglObservations rightLogic,
      eglRunObservations = eglRunObservations leftLogic <> eglRunObservations rightLogic
    }
{-# INLINE appendEGraphLogic #-}

newtype EGraphLogicM capability f a c result = EGraphLogicM
  { runEGraphLogicM :: (result, EGraphLogic capability f a c)
  }

instance Functor (EGraphLogicM capability f a c) where
  fmap transform action =
    let (value, logicValue) = runEGraphLogicM action
     in EGraphLogicM (transform value, logicValue)

instance Applicative (EGraphLogicM capability f a c) where
  pure value =
    EGraphLogicM (value, emptyEGraphLogic)

  (<*>) = ap

instance Monad (EGraphLogicM capability f a c) where
  action >>= continue =
    let (value, leftLogic) = runEGraphLogicM action
        (resultValue, rightLogic) = runEGraphLogicM (continue value)
     in EGraphLogicM (resultValue, appendEGraphLogic leftLogic rightLogic)

logic :: EGraphLogicM capability f a c () -> EGraphLogic capability f a c
logic action =
  snd (runEGraphLogicM action)
{-# INLINE logic #-}

source :: ProgramM (EGraphU capability f a c) () -> EGraphLogicM capability f a c ()
source =
  sourceFragment . program
{-# INLINE source #-}

sourceFragment :: ProgramFragment (EGraphU capability f a c) -> EGraphLogicM capability f a c ()
sourceFragment fragment =
  EGraphLogicM
    ( (),
      emptyEGraphLogic
        { eglSource = fragment
        }
    )
{-# INLINE sourceFragment #-}

seedFacts ::
  MatchSite c ->
  SatFactStore (EGraphU capability f a c) ->
  EGraphLogicM capability f a c ()
seedFacts site facts =
  EGraphLogicM
    ( (),
      emptyEGraphLogic
        { eglSeedFacts = singletonSeedFacts site facts
        }
    )
{-# INLINE seedFacts #-}

observe :: StableObservation f a c result -> EGraphLogicM capability f a c ()
observe observation =
  EGraphLogicM
    ( (),
      emptyEGraphLogic
        { eglObservations = [SomeStableObservation observation]
        }
    )
{-# INLINE observe #-}

observeRun :: RunObservation c result -> EGraphLogicM capability f a c ()
observeRun observation =
  EGraphLogicM
    ( (),
      emptyEGraphLogic
        { eglRunObservations = [SomeRunObservation observation]
        }
    )
{-# INLINE observeRun #-}

data EGraphLogicError capability f a c
  = EGraphLogicCompileError !(SaturationCompileError (EGraphU capability f a c) RewriteRuleId)
  | EGraphLogicRunError !(SaturationRunError (EGraphU capability f a c))
  | EGraphLogicObservationError !(StableObservationError c)

data EGraphLogicReport capability f a c = EGraphLogicReport
  { elrRuntimeState :: !(RuntimeState (EGraphU capability f a c) (SaturatingContextEGraph capability f a c) RewriteRuleId),
    elrSaturation :: !(SaturationReport (EGraphU capability f a c)),
    elrObservations :: ![SomeStableObservationResult f],
    elrRunObservations :: ![SomeRunObservationResult c]
  }

data EGraphLogicObservedReport capability f a c = EGraphLogicObservedReport
  { elorTiming :: !RuntimeIOTiming,
    elorReport :: !(EGraphLogicReport capability f a c)
  }

compileEGraphLogic ::
  forall capability f a c.
  EGraphLogicConstraints capability f a c =>
  PlanSpec (EGraphU capability f a c) (SaturatingContextEGraph capability f a c) RewriteRuleId ->
  EGraphLogic capability f a c ->
  Either
    (SaturationCompileError (EGraphU capability f a c) RewriteRuleId)
    (Plan (EGraphU capability f a c) (SaturatingContextEGraph capability f a c) RewriteRuleId)
compileEGraphLogic planSpec logicValue =
  compileFragment @(EGraphU capability f a c)
    planSpec
    (eglSource logicValue)
{-# INLINE compileEGraphLogic #-}

runEGraphLogic ::
  forall capability f a c.
  EGraphLogicConstraints capability f a c =>
  ContextRunSpec
    (EGraphU capability f a c)
    (SaturatingContextEGraph capability f a c)
    RewriteRuleId
    (SaturationReport (EGraphU capability f a c)) ->
  EGraphLogic capability f a c ->
  SaturatingContextEGraph capability f a c ->
  Either (EGraphLogicError capability f a c) (EGraphLogicReport capability f a c)
runEGraphLogic runSpec logicValue graph = do
  plan <-
    first EGraphLogicCompileError $
      compileEGraphLogic @capability @f @a @c
        (crsPlanSpec runSpec)
        logicValue

  runCompiledEGraphLogic
    (crsExecution runSpec)
    plan
    logicValue
    graph

runCompiledEGraphLogic ::
  forall capability f a c.
  EGraphLogicConstraints capability f a c =>
  ContextExecutionSpec
    (EGraphU capability f a c)
    (SaturatingContextEGraph capability f a c)
    RewriteRuleId
    (SaturationReport (EGraphU capability f a c)) ->
  Plan (EGraphU capability f a c) (SaturatingContextEGraph capability f a c) RewriteRuleId ->
  EGraphLogic capability f a c ->
  SaturatingContextEGraph capability f a c ->
  Either (EGraphLogicError capability f a c) (EGraphLogicReport capability f a c)
runCompiledEGraphLogic executionSpec plan logicValue graph = do
  let seedFactsByContext =
        resolveSeedFacts @(EGraphU capability f a c)
          graph
          (eglSeedFacts logicValue)
      seededState =
        seedRuntimeStateFacts @(EGraphU capability f a c)
          seedFactsByContext
          (runtimeStateFromCarrier @(EGraphU capability f a c) plan graph)

  (finalState, saturationReport) <-
    first EGraphLogicRunError $
      runRuntime @(EGraphU capability f a c)
        (cesPolicy executionSpec)
        plan
        (cesGoal executionSpec)
        seededState

  observationResults <-
    first EGraphLogicObservationError $
      runSomeStableObservations
        (srCarrier saturationReport)
        (eglObservations logicValue)

  let runObservationResults =
        runSomeRunObservations
          graph
          saturationReport
          (eglRunObservations logicValue)

  pure
    EGraphLogicReport
      { elrRuntimeState = finalState,
        elrSaturation = saturationReport,
        elrObservations = observationResults,
        elrRunObservations = runObservationResults
      }

runCompiledEGraphLogicObserved ::
  forall capability f a c.
  EGraphLogicConstraints capability f a c =>
  ContextExecutionSpec
    (EGraphU capability f a c)
    (SaturatingContextEGraph capability f a c)
    RewriteRuleId
    (SaturationReport (EGraphU capability f a c)) ->
  Plan (EGraphU capability f a c) (SaturatingContextEGraph capability f a c) RewriteRuleId ->
  EGraphLogic capability f a c ->
  SaturatingContextEGraph capability f a c ->
  IO (Either (EGraphLogicError capability f a c) (EGraphLogicObservedReport capability f a c))
runCompiledEGraphLogicObserved executionSpec plan logicValue graph = do
  let seedFactsByContext =
        resolveSeedFacts @(EGraphU capability f a c)
          graph
          (eglSeedFacts logicValue)
      seededState =
        seedRuntimeStateFacts @(EGraphU capability f a c)
          seedFactsByContext
          (runtimeStateFromCarrier @(EGraphU capability f a c) plan graph)

  observedRuntime <-
    runRuntimeObserved @(EGraphU capability f a c)
      (cesPolicy executionSpec)
      plan
      (cesGoal executionSpec)
      seededState

  pure $ do
    (finalState, saturationReport) <-
      first EGraphLogicRunError (rorResult observedRuntime)

    observationResults <-
      first EGraphLogicObservationError $
        runSomeStableObservations
          (srCarrier saturationReport)
          (eglObservations logicValue)

    let runObservationResults =
          runSomeRunObservations
            graph
            saturationReport
            (eglRunObservations logicValue)

    pure
      EGraphLogicObservedReport
        { elorTiming = rorTiming observedRuntime,
          elorReport =
            EGraphLogicReport
              { elrRuntimeState = finalState,
                elrSaturation = saturationReport,
                elrObservations = observationResults,
                elrRunObservations = runObservationResults
              }
        }
