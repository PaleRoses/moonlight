module Moonlight.Control.EngineRunSpec
  ( tests,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.List (delete)
import Data.List.NonEmpty (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

import Moonlight.Control.Candidate
  ( ScheduledMatch (..),
    finiteCandidateSpace,
    scheduledBatchMatchesWithGroups,
  )
import Moonlight.Control.Class (phase)
import Moonlight.Control.Count (workCountMayBePositive)
import Moonlight.Control.Engine.Plan
  ( EngineProgram,
    Plan,
    PhaseDecl,
    canonicalRoundBudget,
    phaseDecl,
  )
import Moonlight.Control.Engine.Report
  ( EngineReport (..),
    EngineRound (..),
    Observation (..),
    StopReason (..),
  )
import Moonlight.Control.Engine.Run
  ( EngineFailure (..),
    EngineRoundResult (..),
    EngineRuntime (..),
    initialEngineRuntime,
    runEngine,
    runEngineRound,
  )
import Moonlight.Control.Engine.Spec
  ( EngineSpec,
    EngineSpecError (..),
    Raw,
    TracePolicySpec (..),
    Validated,
    compilePlan,
    compilePlanWithProgram,
    defaultEngineSpec,
    rawEngineSpec,
    setMaxRounds,
    setRoundBudget,
    setTracePolicySpec,
    validateEngineSpec,
  )
import Moonlight.Control.Engine.Work
  ( WorkSource (..),
    applyResult,
  )
import Moonlight.Control.Gate
  ( Gate (..),
    GateCompatibilityError (..),
    GateValidation (..),
    noSelector,
  )
import Moonlight.Control.Modality (gated, weighted)
import Moonlight.Control.Weight
  ( singletonPriorityProfile,
    structuralPriorityEvidence,
  )

tests :: TestTree
tests =
  testGroup
    "Moonlight.Control.Engine.Run"
    [ testCase "spec validation accumulates every error" testSpecValidationAccumulates,
      testCase "engine drains all work and stops on no candidate work" testEngineDrains,
      testCase "plan round budget limits selection and defers the rest" testRoundBudgetLimits,
      testCase "phase budget overrides the plan default" testPhaseBudgetOverrides,
      testCase "budget one drains three matches in exactly four rounds" testBudgetOneRoundCount,
      testCase "trace-last policy bounds retained trace entries across rounds" testTraceRetention,
      testCase "public single-round runner advances the runtime" testRunEngineRound,
      testCase "rejecting gate surfaces as EngineGateIncompatible" testGateValidationFailure,
      testCase "scoped weight reorders group scheduling" testWeightedScopeReorders
    ]

data DrainState = DrainState
  { dsPending :: !(Map Int [Int]),
    dsBatches :: ![[ScheduledMatch Int Int]]
  }
  deriving stock (Eq, Show)

drainState :: [(Int, [Int])] -> DrainState
drainState pending =
  DrainState
    { dsPending = Map.filter (not . null) (Map.fromList pending),
      dsBatches = []
    }

drainSource :: WorkSource Identity DrainState () Int Int Natural String
drainSource =
  WorkSource
    { wsView = const (),
      wsCandidateSpace = pure . finiteSpace,
      wsApplyScheduled = \batch state ->
        let scheduled = scheduledBatchMatchesWithGroups batch
            nextPending =
              Map.filter
                (not . null)
                (foldl' removeScheduled (dsPending state) scheduled)
            nextState =
              DrainState
                { dsPending = nextPending,
                  dsBatches = dsBatches state <> [scheduled]
                }
         in pure
              ( Right
                  ( applyResult
                      nextState
                      (fromIntegral (length scheduled))
                      (length scheduled)
                  )
              ),
      wsProgressed = (> 0)
    }
  where
    finiteSpace state =
      finiteCandidateSpace (Map.toList (dsPending state))
    removeScheduled pending scheduledMatch =
      Map.adjust (delete (smMatch scheduledMatch)) (smGroup scheduledMatch) pending

validatedSpec :: (EngineSpec Raw -> EngineSpec Raw) -> EngineSpec Validated
validatedSpec refine =
  case validateEngineSpec (defaultEngineSpec (refine rawEngineSpec)) of
    Right spec -> spec
    Left errors -> error ("validatedSpec: " <> show errors)

drainPhase :: PhaseDecl
drainPhase = phaseDecl "drain" Nothing

runDrain ::
  Plan () Int Int () Natural ->
  [(Int, [Int])] ->
  Either (EngineFailure String Int) (EngineReport DrainState Int () Natural)
runDrain plan pending =
  runIdentity (runEngine plan drainSource (drainState pending))

reportOrFail ::
  Either (EngineFailure String Int) (EngineReport DrainState Int () Natural) ->
  IO (EngineReport DrainState Int () Natural)
reportOrFail outcome =
  case outcome of
    Left failure -> assertFailure ("engine failed: " <> show failure) >> error "unreachable"
    Right report -> pure report

testSpecValidationAccumulates :: Assertion
testSpecValidationAccumulates =
  case validateEngineSpec (defaultEngineSpec (setRoundBudget 0 (setMaxRounds (-1) rawEngineSpec))) of
    Right _spec ->
      assertFailure "invalid spec must be rejected"
    Left errors -> do
      assertBool
        "reports non-positive max rounds"
        (SpecMaxRoundsNonPositive (-1) `elem` toList errors)
      assertBool
        "reports non-positive round budget"
        (SpecRoundBudgetNonPositive 0 `elem` toList errors)

testEngineDrains :: Assertion
testEngineDrains = do
  report <-
    reportOrFail
      (runDrain (compilePlan (validatedSpec id) drainPhase) [(1, [10, 11]), (2, [20])])
  assertEqual "all pending work consumed" Map.empty (dsPending (erFinalState report))
  assertEqual "stops on missing candidates" NoCandidateWork (erStopReason report)

testRoundBudgetLimits :: Assertion
testRoundBudgetLimits = do
  report <-
    reportOrFail
      (runDrain (compilePlan (validatedSpec (setRoundBudget 4)) drainPhase) [(1, [1 .. 10])])
  firstRound <- firstRoundOf report
  let observation = roundObservation firstRound
  assertEqual "budget bounds the scheduled count" 4 (obScheduledCount observation)
  assertBool
    "remainder is deferred by budget"
    (workCountMayBePositive (obDeferredByBudgetCount observation))

testPhaseBudgetOverrides :: Assertion
testPhaseBudgetOverrides = do
  let program :: EngineProgram () Int Int ()
      program = phase (phaseDecl "tight" (Just (canonicalRoundBudget 2)))
  report <-
    reportOrFail
      (runDrain (compilePlanWithProgram (validatedSpec (setRoundBudget 4)) program) [(1, [1 .. 10])])
  firstRound <- firstRoundOf report
  assertEqual
    "phase budget wins over plan default"
    2
    (obScheduledCount (roundObservation firstRound))
  assertEqual "single phase completes the program" ProgramCompleted (erStopReason report)

testBudgetOneRoundCount :: Assertion
testBudgetOneRoundCount = do
  report <-
    reportOrFail
      (runDrain (compilePlan (validatedSpec (setRoundBudget 1)) drainPhase) [(1, [1, 2, 3])])
  assertEqual
    "three progressing rounds plus one terminal round"
    4
    (length (erRounds report))
  assertEqual "ends with no candidate work" NoCandidateWork (erStopReason report)

testTraceRetention :: Assertion
testTraceRetention = do
  report <-
    reportOrFail
      ( runDrain
          ( compilePlan
              (validatedSpec (setTracePolicySpec (TraceLastSpec 1) . setRoundBudget 1))
              drainPhase
          )
          [(1, [1, 2, 3])]
      )
  let retainedEntries roundValue =
        length (obGateTrace (roundObservation roundValue))
          + length (obScheduleTrace (roundObservation roundValue))
  assertBool
    "log retains at most one trace entry in total"
    (sum (fmap retainedEntries (erRounds report)) <= 1)

testRunEngineRound :: Assertion
testRunEngineRound = do
  let plan = compilePlan (validatedSpec (setRoundBudget 4)) drainPhase
  case runIdentity
    ( runEngineRound
        plan
        mempty
        drainPhase
        drainSource
        initialEngineRuntime
        (drainState [(1, [1 .. 6])])
    ) of
    Left failure ->
      assertFailure ("single round failed: " <> show failure)
    Right result -> do
      assertEqual
        "round schedules up to the budget"
        4
        (obScheduledCount (roundObservation (rrRound result)))
      assertEqual "runtime advances one round" 1 (ertRoundIndex (rrRuntime result))
      assertEqual
        "scheduled matches were applied"
        [4]
        (fmap length (dsBatches (rrState result)))

testGateValidationFailure :: Assertion
testGateValidationFailure = do
  let rejectingGate :: Gate () Int Int () Int
      rejectingGate =
        Gate
          { gateSelector = noSelector,
            gateValidation = GateValidation (Left . GateRejectedScheduler)
          }
      plan =
        compilePlanWithProgram
          (validatedSpec id)
          (gated rejectingGate (phase drainPhase))
  case runDrain plan [(1, [1])] of
    Left (EngineGateIncompatible _rejected) ->
      pure ()
    Left other ->
      assertFailure ("expected gate incompatibility, got: " <> show other)
    Right _report ->
      assertFailure "rejecting gate must abort the run"

testWeightedScopeReorders :: Assertion
testWeightedScopeReorders = do
  let pending = [(1, [10, 11]), (2, [20, 21])]
      unweightedPlan = compilePlan (validatedSpec id) drainPhase
      weightedProgram :: EngineProgram () Int Int ()
      weightedProgram =
        weighted
          (singletonPriorityProfile 2 (structuralPriorityEvidence 5))
          (phase drainPhase)
      weightedPlan = compilePlanWithProgram (validatedSpec id) weightedProgram
  unweightedReport <- reportOrFail (runDrain unweightedPlan pending)
  weightedReport <- reportOrFail (runDrain weightedPlan pending)
  assertEqual
    "equal evidence schedules ascending group order"
    (Just 1)
    (firstScheduledGroup unweightedReport)
  assertEqual
    "scoped weight promotes the boosted group"
    (Just 2)
    (firstScheduledGroup weightedReport)
  where
    firstScheduledGroup report =
      case concat (dsBatches (erFinalState report)) of
        [] -> Nothing
        scheduledMatch : _rest -> Just (smGroup scheduledMatch)

firstRoundOf ::
  EngineReport DrainState Int () Natural ->
  IO (EngineRound Int () Natural)
firstRoundOf report =
  case erRounds report of
    [] -> assertFailure "expected at least one retained round" >> error "unreachable"
    firstRound : _rest -> pure firstRound
