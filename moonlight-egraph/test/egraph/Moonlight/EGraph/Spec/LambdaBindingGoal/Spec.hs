{-# LANGUAGE RankNTypes #-}

module Moonlight.EGraph.Spec.LambdaBindingGoal.Spec
  ( lambdaBindingGoalTests,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Moonlight.Core (Language)
import Moonlight.EGraph.Pure.Context (ContextEGraph)
import Moonlight.Sheaf.Context.Algebra
  ( ContextClassLookupFailure,
    contextEquivalentAt,
    restrictionMap,
  )
import Moonlight.Sheaf.Descent.Context
  ( descentAt,
  )
import Moonlight.Sheaf.Verdict
  ( SearchVerdict (..),
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId,
    classIdKey,
  )
import Moonlight.EGraph.Spec.LambdaBindingGoal.Harness
  ( LambdaBindingHarness (..),
    LambdaGoalCore (..),
    LambdaGoalReport (..),
    LambdaGoalRun (..),
    LambdaGoalScenario (..),
    lookupMetric,
    lookupNamedClass,
    lookupNamedContext,
    lookupNamedNormalForm,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )

lambdaBindingGoalTests ::
  (Language f, Ord ctx) =>
  LambdaBindingHarness ctx f a ->
  TestTree
lambdaBindingGoalTests harness =
  testGroup
    "LambdaF binding via context-sensitive equality saturation"
    [ testCase
        "alpha-equivalence is glued by restriction"
        (test_alphaEquivalenceAsSheafCondition harness),
      testCase
        "beta reduction survives dynamic scope extension"
        (test_dynamicBetaAndLatticeGrowth harness),
      testCase
        "capture-avoiding substitution reports the typed binding obstruction"
        (test_captureAvoidanceViaBindingObstruction harness),
      testCase
        "eta contracts only in the side-condition-safe case"
        (test_etaPositiveAndNegative harness),
      testCase
        "let-float crosses binders only when scope-safe"
        (test_letFloatPositiveAndNegative harness),
      testCase
        "lattice growth preserves prior equalities and restriction composition"
        (test_latticeGrowthPreservesEqualities harness),
      testCase
        "deep nesting stays within the declared context and iteration budget"
        (test_deepNestingContextGrowth harness),
      testCase
        "profiled scope-sensitive workload stays bounded under prepared context repair"
        (test_profiledScopeSensitiveWorkload harness)
    ]

requireRight :: String -> Either String x -> IO x
requireRight prefix =
  either
    (\err -> assertFailure (prefix <> ": " <> err) >> fail err)
    pure

withScenario ::
  LambdaBindingHarness ctx f a ->
  LambdaGoalScenario ->
  (forall owner. LambdaGoalRun owner ctx f a -> Assertion) ->
  Assertion
withScenario harness scenario useRun =
  either
    (assertFailure . (("scenario " <> show scenario <> ": ") <>))
    id
    (lbhRunScenario harness scenario useRun)

requireContext :: String -> LambdaGoalRun owner ctx f a -> IO ctx
requireContext label runValue =
  requireRight ("context " <> show label) (lookupNamedContext label runValue)

requireClass :: String -> String -> LambdaGoalRun owner ctx f a -> IO ClassId
requireClass contextLabel termLabel runValue =
  requireRight
    ("class " <> show (contextLabel, termLabel))
    (lookupNamedClass contextLabel termLabel runValue)

requireNormalForm :: String -> String -> LambdaGoalRun owner ctx f a -> IO String
requireNormalForm contextLabel termLabel runValue =
  requireRight
    ("normal form " <> show (contextLabel, termLabel))
    (lookupNamedNormalForm contextLabel termLabel runValue)

requireMetricIO :: String -> LambdaGoalRun owner ctx f a -> IO Int
requireMetricIO key runValue =
  requireRight ("metric " <> show key) (lookupMetric key runValue)

assertAtLeast :: String -> Int -> Int -> Assertion
assertAtLeast label minimumValue actualValue =
  assertBool
    (label <> " underflow; expected at least " <> show minimumValue <> ", saw " <> show actualValue)
    (actualValue >= minimumValue)

assertAtMost :: String -> Int -> Int -> Assertion
assertAtMost label maximumValue actualValue =
  assertBool
    (label <> " overflow; max " <> show maximumValue <> ", saw " <> show actualValue)
    (actualValue <= maximumValue)

contextCount :: (Language f, Ord ctx) => LambdaGoalRun owner ctx f a -> Int
contextCount =
  Map.size . lgrNamedContexts

runCore :: LambdaGoalRun owner ctx f a -> LambdaGoalCore owner ctx f a
runCore =
  lgrCore

runGraph :: LambdaGoalRun owner ctx f a -> ContextEGraph owner f a ctx
runGraph =
  lgcGraph . lgrCore

equivalentAt ::
  (Language f, Ord ctx) =>
  LambdaGoalRun owner ctx f a ->
  String ->
  String ->
  String ->
  IO (Either (ContextClassLookupFailure ctx ClassId) Bool)
equivalentAt runValue contextLabel leftLabel rightLabel = do
  ctx <- requireContext contextLabel runValue
  lhs <- requireClass contextLabel leftLabel runValue
  rhs <- requireClass contextLabel rightLabel runValue
  pure (contextEquivalentAt ctx lhs rhs (runGraph runValue))

assertEquivalentAt ::
  (Language f, Ord ctx) =>
  LambdaGoalRun owner ctx f a ->
  String ->
  String ->
  String ->
  Assertion
assertEquivalentAt runValue contextLabel leftLabel rightLabel = do
  result <- equivalentAt runValue contextLabel leftLabel rightLabel
  case result of
    Left _ ->
      assertFailure
        ( "context class lookup failed at "
            <> show contextLabel
            <> " between "
            <> show leftLabel
            <> " and "
            <> show rightLabel
        )
    Right ok ->
      assertBool
        ( "expected equivalence at context "
            <> show contextLabel
            <> " between "
            <> show leftLabel
            <> " and "
            <> show rightLabel
        )
        ok

assertNotEquivalentAt ::
  (Language f, Ord ctx) =>
  LambdaGoalRun owner ctx f a ->
  String ->
  String ->
  String ->
  Assertion
assertNotEquivalentAt runValue contextLabel leftLabel rightLabel = do
  result <- equivalentAt runValue contextLabel leftLabel rightLabel
  case result of
    Left _ ->
      pure ()
    Right ok ->
      assertBool
        ( "unexpected equivalence at context "
            <> show contextLabel
            <> " between "
            <> show leftLabel
            <> " and "
            <> show rightLabel
        )
        (not ok)

restrictedClassTo ::
  (Language f, Ord ctx) =>
  LambdaGoalRun owner ctx f a ->
  String ->
  String ->
  String ->
  IO ClassId
restrictedClassTo runValue sourceLabel targetLabel termLabel = do
  sourceCtx <- requireContext sourceLabel runValue
  targetCtx <- requireContext targetLabel runValue
  sourceClass <- requireClass sourceLabel termLabel runValue
  let graph = runGraph runValue
  case restrictionMap sourceCtx targetCtx graph of
    Left _restrictionFailure ->
      assertFailure
        ( "no restriction map from "
            <> show sourceLabel
            <> " to "
            <> show targetLabel
        )
        >> fail "missing restriction map"
    Right mp ->
      pure (IntMap.findWithDefault sourceClass (classIdKey sourceClass) mp)

assertRestrictionCompositionOn ::
  (Language f, Ord ctx) =>
  LambdaGoalRun owner ctx f a ->
  String ->
  String ->
  String ->
  Assertion
assertRestrictionCompositionOn runValue sourceLabel middleLabel targetLabel = do
  sourceCtx <- requireContext sourceLabel runValue
  middleCtx <- requireContext middleLabel runValue
  targetCtx <- requireContext targetLabel runValue
  let graph = runGraph runValue
  case
    ( restrictionMap sourceCtx middleCtx graph,
      restrictionMap middleCtx targetCtx graph,
      restrictionMap sourceCtx targetCtx graph
    ) of
    (Right sToM, Right mToT, Right sToT) ->
      let composed =
            IntMap.map
              (\cid -> IntMap.findWithDefault cid (classIdKey cid) mToT)
              sToM
       in assertEqual
            ( "restriction composition failed for chain "
                <> show (sourceLabel, middleLabel, targetLabel)
            )
            sToT
            composed
    _ ->
      assertFailure
        ( "missing restriction map(s) for chain "
            <> show (sourceLabel, middleLabel, targetLabel)
        )

assertDescentSatisfiedAt ::
  (Language f, Ord ctx) =>
  LambdaGoalRun owner ctx f a ->
  String ->
  Assertion
assertDescentSatisfiedAt runValue contextLabel = do
  ctx <- requireContext contextLabel runValue
  case descentAt ctx (runGraph runValue) of
    SearchAccepted ->
      pure ()
    SearchRejected obs ->
      assertFailure
        ( "expected descent satisfaction at "
            <> show contextLabel
            <> ", got obstruction count "
            <> show (NonEmpty.length obs)
        )
    SearchUndecided refusals _ ->
      assertFailure
        ( "expected decided descent satisfaction at "
            <> show contextLabel
            <> ", got refusal count "
            <> show (NonEmpty.length refusals)
        )

test_alphaEquivalenceAsSheafCondition ::
  (Language f, Ord ctx) =>
  LambdaBindingHarness ctx f a ->
  Assertion
test_alphaEquivalenceAsSheafCondition harness = withScenario harness AlphaEquivalenceScenario $ \runValue -> do

  assertEquivalentAt runValue "global" "alphaLeft" "alphaRight"

  leftRestricted <-
    restrictedClassTo runValue "left/body" "global" "leftBound"
  rightRestricted <-
    restrictedClassTo runValue "right/body" "global" "rightBound"

  assertEqual
    "alpha-equivalent binders should restrict to the same coarse/global section"
    leftRestricted
    rightRestricted

  assertRestrictionCompositionOn runValue "left/body" "left/binder" "global"
  assertRestrictionCompositionOn runValue "right/body" "right/binder" "global"

test_dynamicBetaAndLatticeGrowth ::
  (Language f, Ord ctx) =>
  LambdaBindingHarness ctx f a ->
  Assertion
test_dynamicBetaAndLatticeGrowth harness = withScenario harness DynamicBetaScenario $ \runValue -> do

  initialContextCount <- requireMetricIO "initialContextCount" runValue
  initialRevision <- requireMetricIO "initialContextRevision" runValue
  _binderContext <- requireContext "beta/binder" runValue

  assertEquivalentAt runValue "global" "betaInput" "betaExpected"

  assertBool
    ( "dynamic beta should extend the scope lattice; initial contexts="
        <> show initialContextCount
        <> ", final contexts="
        <> show (contextCount runValue)
    )
    (contextCount runValue > initialContextCount)

  assertBool
    ( "dynamic beta should advance the context revision; initial="
        <> show initialRevision
        <> ", final="
        <> show (lgcContextRevision (runCore runValue))
    )
    (lgcContextRevision (runCore runValue) > initialRevision)

  assertBool
    "dynamic beta should apply at least one match"
    (lgrMatchesApplied (lgrReport runValue) > 0)

test_captureAvoidanceViaBindingObstruction ::
  (Language f, Ord ctx) =>
  LambdaBindingHarness ctx f a ->
  Assertion
test_captureAvoidanceViaBindingObstruction harness = withScenario harness CaptureAvoidanceScenario $ \runValue -> do
  captureObstructionCount <- requireMetricIO "captureObstructionCount" runValue

  assertEquivalentAt runValue "global" "captureInput" "captureSafe"
  assertNotEquivalentAt runValue "global" "captureInput" "captureUnsafe"
  assertAtLeast "capture obstructions" 1 captureObstructionCount

test_etaPositiveAndNegative ::
  (Language f, Ord ctx) =>
  LambdaBindingHarness ctx f a ->
  Assertion
test_etaPositiveAndNegative harness = withScenario harness EtaScenario $ \runValue -> do

  assertEquivalentAt runValue "global" "etaSafeInput" "etaSafeResult"
  assertNotEquivalentAt runValue "global" "etaUnsafeInput" "etaUnsafeCandidate"

  etaSafeNormal <- requireNormalForm "global" "etaSafeInput" runValue
  etaSafeExpected <- requireNormalForm "global" "etaSafeResult" runValue
  etaUnsafeNormal <- requireNormalForm "global" "etaUnsafeInput" runValue
  etaUnsafeCandidate <- requireNormalForm "global" "etaUnsafeCandidate" runValue

  assertEqual
    "safe eta should normalize to the eta-reduced form"
    etaSafeExpected
    etaSafeNormal

  assertBool
    "unsafe eta case should not normalize to the forbidden candidate"
    (etaUnsafeNormal /= etaUnsafeCandidate)

test_letFloatPositiveAndNegative ::
  (Language f, Ord ctx) =>
  LambdaBindingHarness ctx f a ->
  Assertion
test_letFloatPositiveAndNegative harness = withScenario harness LetFloatScenario $ \runValue -> do

  assertEquivalentAt runValue "global" "floatSafeInput" "floatSafeResult"
  assertNotEquivalentAt runValue "global" "floatUnsafeInput" "floatUnsafeCandidate"

  floatSafeNormal <- requireNormalForm "global" "floatSafeInput" runValue
  floatSafeExpected <- requireNormalForm "global" "floatSafeResult" runValue
  floatUnsafeNormal <- requireNormalForm "global" "floatUnsafeInput" runValue
  floatUnsafeCandidate <- requireNormalForm "global" "floatUnsafeCandidate" runValue

  assertEqual
    "scope-safe let-float should normalize to the floated form"
    floatSafeExpected
    floatSafeNormal

  assertBool
    "scope-unsafe let-float should not normalize to the forbidden floated form"
    (floatUnsafeNormal /= floatUnsafeCandidate)

test_latticeGrowthPreservesEqualities ::
  (Language f, Ord ctx) =>
  LambdaBindingHarness ctx f a ->
  Assertion
test_latticeGrowthPreservesEqualities harness = withScenario harness LatticeGrowthScenario $ \runValue -> do

  initialContextCount <- requireMetricIO "initialContextCount" runValue
  initialRevision <- requireMetricIO "initialContextRevision" runValue

  assertEquivalentAt runValue "global" "preExistingLeft" "preExistingRight"
  assertRestrictionCompositionOn runValue "growth/grandchild" "growth/child" "global"
  assertDescentSatisfiedAt runValue "growth/child"

  assertBool
    ( "lattice-growth scenario should materialize more contexts; initial="
        <> show initialContextCount
        <> ", final="
        <> show (contextCount runValue)
    )
    (contextCount runValue > initialContextCount)

  assertBool
    ( "lattice-growth scenario should increase context revision; initial="
        <> show initialRevision
        <> ", final="
        <> show (lgcContextRevision (runCore runValue))
    )
    (lgcContextRevision (runCore runValue) > initialRevision)

test_deepNestingContextGrowth ::
  (Language f, Ord ctx) =>
  LambdaBindingHarness ctx f a ->
  Assertion
test_deepNestingContextGrowth harness =
  let depth = 32
   in withScenario harness (DeepNestingScenario depth) $ \runValue -> do
        minContexts <- requireMetricIO "expectedMinimumContextCount" runValue
        maxContexts <- requireMetricIO "maxAllowedContextCount" runValue
        maxIterations <- requireMetricIO "maxAllowedIterations" runValue

        assertEquivalentAt runValue "global" "deepInput" "deepExpected"

        assertAtLeast "deep nesting contexts" minContexts (contextCount runValue)
        assertAtMost "deep nesting contexts" maxContexts (contextCount runValue)
        assertAtMost "deep nesting iterations" maxIterations (lgrIterations (lgrReport runValue))

test_profiledScopeSensitiveWorkload ::
  (Language f, Ord ctx) =>
  LambdaBindingHarness ctx f a ->
  Assertion
test_profiledScopeSensitiveWorkload harness =
  -- Performance calibration, 2026-06-09 local dev run: this canary sat around
  -- 230-260ms. Acceptable for now, but not ignorable; if it climbs, inspect
  -- scoped ingestion/materialization and prepared context repair before raising
  -- these semantic bounds. The bloody number is a symptom, not the law.
  let binderCount = 24
      middleScope = "profile/scope/" <> show (binderCount `div` 2)
      deepestScope = "profile/scope/" <> show binderCount
   in withScenario harness (ProfiledScopeSensitiveScenario binderCount) $ \runValue -> do
        observedBinderCount <- requireMetricIO "binderCount" runValue
        scopeDepth <- requireMetricIO "scopeDepth" runValue
        minContexts <- requireMetricIO "expectedMinimumContextCount" runValue
        maxContexts <- requireMetricIO "maxAllowedContextCount" runValue
        maxIterations <- requireMetricIO "maxAllowedIterations" runValue
        maxMatches <- requireMetricIO "maxAllowedMatches" runValue

        assertEqual "requested binder count" binderCount observedBinderCount
        assertEqual "positive scope depth" binderCount scopeDepth
        assertEquivalentAt runValue "global" "profileInput" "profileExpected"
        assertRestrictionCompositionOn runValue deepestScope middleScope "global"
        assertDescentSatisfiedAt runValue middleScope

        assertAtLeast "profile contexts" minContexts (contextCount runValue)
        assertAtMost "profile contexts" maxContexts (contextCount runValue)
        assertAtMost "profile iterations" maxIterations (lgrIterations (lgrReport runValue))
        assertAtMost "profile matches" maxMatches (lgrMatchesApplied (lgrReport runValue))
