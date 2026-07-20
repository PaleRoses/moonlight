{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Moonlight.EGraph.Binding.SlottedUpstreamCoverageSpec
  ( tests,
    upstreamSlottedCoverageRows,
    moonlightOnlyRows,
    renderUpstreamCoverageCsv,
    renderMoonlightOnlyCsv,
  )
where

import Data.List (intercalate)
import Data.Set qualified as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

newtype UpstreamTestName = UpstreamTestName
  { unUpstreamTestName :: String
  }
  deriving stock (Eq, Ord, Show)

data UpstreamSuite
  = SlottedLibSuite
  | SlottedEntrySuite
  deriving stock (Eq, Ord, Show)

data SlottedIgnoreReason
  = TooSlow
  | TodoFails
  deriving stock (Eq, Ord, Show)

data SlottedFailureReason
  = NodeLimitFailure
  deriving stock (Eq, Ord, Show)

data SlottedOutcome
  = SlottedPassed
  | SlottedIgnored !SlottedIgnoreReason
  deriving stock (Eq, Ord, Show)

data ForcedIgnoredOutcome
  = NotForcedIgnored
  | ForcedIgnoredFailed !SlottedFailureReason
  deriving stock (Eq, Ord, Show)

data MoonlightCoverage
  = MoonlightFamilyCovered !MoonlightEvidence
  | ComparableNotYetPorted !PortGap
  | UpstreamInternalOnly !InternalSurface
  deriving stock (Eq, Ord, Show)

data MoonlightEvidence
  = LambdaBindingGoalFamily
  deriving stock (Eq, Ord, Show)

data PortGap
  = PlainArithmeticReachability
  | AnalysisAndExtractionFixture
  | SlotPermutationWitness
  | RiseRewriteCorpus
  | SdqlBinderRewrite
  deriving stock (Eq, Ord, Show)

data InternalSurface
  = SlottedCoreInvariant
  | SlottedGroupInvariant
  | SlottedSlotMapInvariant
  | DeterministicHasherInvariant
  deriving stock (Eq, Ord, Show)

data MoonlightOnlyCapability
  = ContextLatticeGrowth
  | TypedBindingObstruction
  | ScopeSafeLetFloat
  | DescentRestrictionGluing
  | BranchLocalEquality
  | ExhaustiveCaseLift
  deriving stock (Eq, Ord, Show)

data UpstreamCoverageRow = UpstreamCoverageRow
  { ucrSuite :: !UpstreamSuite,
    ucrName :: !UpstreamTestName,
    ucrDefaultOutcome :: !SlottedOutcome,
    ucrAllFeaturesOutcome :: !SlottedOutcome,
    ucrForcedIgnoredOutcome :: !ForcedIgnoredOutcome,
    ucrMoonlightCoverage :: !MoonlightCoverage
  }
  deriving stock (Eq, Ord, Show)

data MoonlightOnlyRow = MoonlightOnlyRow
  { morCapability :: !MoonlightOnlyCapability,
    morEvidence :: !String,
    morSlottedReason :: !String
  }
  deriving stock (Eq, Ord, Show)

tests :: TestTree
tests =
  testGroup
    "slotted-upstream-coverage-table"
    [ testCase "classifies every slotted-egraphs 0.0.36 upstream test" $ do
        assertEqual "upstream test count" 75 (length upstreamSlottedCoverageRows)
        assertEqual "unique upstream test count" 75 (Set.size (Set.fromList (fmap ucrName upstreamSlottedCoverageRows))),
      testCase "records the upstream ignored-row landmine" $ do
        assertEqual
          "default ignored upstream tests"
          [UpstreamTestName "rise::tst::binomial"]
          (ucrName <$> filterByDefaultIgnored upstreamSlottedCoverageRows)
        assertEqual
          "forced ignored failure"
          [UpstreamTestName "rise::tst::binomial"]
          (ucrName <$> filterByForcedNodeLimit upstreamSlottedCoverageRows),
      testCase "keeps the comparison honest instead of pretending all rows are exact ports" $ do
        assertEqual "Moonlight family-covered rows" 42 (coverageCount isFamilyCovered)
        assertEqual "comparable but not exact-ported rows" 27 (coverageCount isComparableDebt)
        assertEqual "upstream-internal rows" 6 (coverageCount isUpstreamInternal)
        assertBool "Moonlight-only gap rows must exist" (not (null moonlightOnlyRows))
    ]

upstreamSlottedCoverageRows :: [UpstreamCoverageRow]
upstreamSlottedCoverageRows =
  concat
    [ slottedLibRows,
      arithRows,
      fghRows,
      lambdaRows,
      miscRows,
      riseRows,
      sdqlRows,
      varRows
    ]

moonlightOnlyRows :: [MoonlightOnlyRow]
moonlightOnlyRows =
  [ moonlightOnly ContextLatticeGrowth "LambdaBindingGoal.DynamicBetaScenario; LambdaBindingGoal.LatticeGrowthScenario" "slotted e-graphs do not expose a context lattice or revision-growth surface",
    moonlightOnly TypedBindingObstruction "LambdaBindingGoal.CaptureAvoidanceScenario" "slotted can reject capture by slot side conditions but does not emit our typed obstruction/descent reason",
    moonlightOnly ScopeSafeLetFloat "LambdaBindingGoal.LetFloatScenario" "upstream slotted lambda tests push let/substitution but do not model our outward scope-safe let-float rule",
    moonlightOnly DescentRestrictionGluing "Context.DescentSpec; Context.PowersetTwinSpec; Boundary.ObstructionSpec" "no restriction-map descent or sibling gluing operation in the upstream slotted test surface",
    moonlightOnly BranchLocalEquality "bench-caselift sound/better artifacts" "no branch-assumption lattice indexed by carriers",
    moonlightOnly ExhaustiveCaseLift "bench-caselift q_lifted.txt; CaseLiftSpec" "no operation glues equalities proven in every exhaustive case alternative into the parent context"
  ]

renderUpstreamCoverageCsv :: String
renderUpstreamCoverageCsv =
  unlines (upstreamCoverageHeader : fmap renderUpstreamCoverageRow upstreamSlottedCoverageRows)

renderMoonlightOnlyCsv :: String
renderMoonlightOnlyCsv =
  unlines (moonlightOnlyHeader : fmap renderMoonlightOnlyRow moonlightOnlyRows)

slottedLibRows :: [UpstreamCoverageRow]
slottedLibRows =
  [ row SlottedLibSuite "egraph::cartesian1" SlottedPassed SlottedPassed NotForcedIgnored (UpstreamInternalOnly SlottedCoreInvariant),
    row SlottedLibSuite "group::tst::group_test1" SlottedPassed SlottedPassed NotForcedIgnored (UpstreamInternalOnly SlottedGroupInvariant),
    row SlottedLibSuite "group::tst::group_test2" SlottedPassed SlottedPassed NotForcedIgnored (UpstreamInternalOnly SlottedGroupInvariant),
    row SlottedLibSuite "group::tst::group_test3" SlottedPassed SlottedPassed NotForcedIgnored (UpstreamInternalOnly SlottedGroupInvariant),
    row SlottedLibSuite "slotmap::test_slotmap" SlottedPassed SlottedPassed NotForcedIgnored (UpstreamInternalOnly SlottedSlotMapInvariant)
  ]

arithRows :: [UpstreamCoverageRow]
arithRows =
  fmap
    (\name -> comparableRow name PlainArithmeticReachability)
    [ "arith::tst::t1",
      "arith::tst::t2",
      "arith::tst::t3",
      "arith::tst::t4",
      "arith::tst::t6"
    ]
    <> [ comparableRow "arith::const_prop::const_prop" AnalysisAndExtractionFixture,
         comparableRow "arith::const_prop::const_prop_union" AnalysisAndExtractionFixture,
         row SlottedEntrySuite "arith::tst::t5" SlottedPassed (SlottedIgnored TodoFails) NotForcedIgnored (ComparableNotYetPorted PlainArithmeticReachability)
       ]

fghRows :: [UpstreamCoverageRow]
fghRows =
  [comparableRow "fgh::transitive_symmetry" SlotPermutationWitness]

lambdaRows :: [UpstreamCoverageRow]
lambdaRows =
  fmap lambdaFamilyRow lambdaAllModesPass
    <> fmap lambdaAllFeaturesTooSlowRow lambdaAllFeaturesTooSlow

miscRows :: [UpstreamCoverageRow]
miscRows =
  [row SlottedEntrySuite "misc::is_deterministic_hasher" SlottedPassed SlottedPassed NotForcedIgnored (UpstreamInternalOnly DeterministicHasherInvariant)]

riseRows :: [UpstreamCoverageRow]
riseRows =
  fmap
    (\name -> comparableRow name RiseRewriteCorpus)
    [ "rise::tst::small10",
      "rise::tst::small11",
      "rise::tst::small12",
      "rise::tst::small13",
      "rise::tst::small14",
      "rise::tst::small15",
      "rise::tst::small2",
      "rise::tst::small3",
      "rise::tst::small5",
      "rise::tst::small6",
      "rise::tst::small7",
      "rise::tst::small8",
      "rise::tst::small9"
    ]
    <> fmap
      (\name -> row SlottedEntrySuite name SlottedPassed (SlottedIgnored TooSlow) NotForcedIgnored (ComparableNotYetPorted RiseRewriteCorpus))
      [ "rise::tst::fission",
        "rise::tst::reduction"
      ]
    <> [ row SlottedEntrySuite "rise::tst::binomial" (SlottedIgnored TooSlow) (SlottedIgnored TooSlow) (ForcedIgnoredFailed NodeLimitFailure) (ComparableNotYetPorted RiseRewriteCorpus)
       ]

sdqlRows :: [UpstreamCoverageRow]
sdqlRows =
  [comparableRow "sdql::rewrite::t1" SdqlBinderRewrite]

varRows :: [UpstreamCoverageRow]
varRows =
  [comparableRow "var::xy_eq_yz_causes_redundancy" SlotPermutationWitness]

lambdaAllModesPass :: [String]
lambdaAllModesPass =
  foldMap
    lambdaModeTests
    [ "lambda::lambda_small_step",
      "lambda::let_small_step",
      "lambda::native"
    ]
  where
    lambdaModeTests modeName =
      fmap
        (\testName -> modeName <> "::" <> testName)
        [ "add_y_step",
          "cannot_simplify",
          "inf_loop",
          "nested_identity1",
          "nested_identity2",
          "nested_identity3",
          "redundant_slot",
          "redundant_slot2",
          "self_rec",
          "simple_beta",
          "t_shift"
        ]

lambdaAllFeaturesTooSlow :: [String]
lambdaAllFeaturesTooSlow =
  foldMap
    lambdaModeSlowTests
    [ "lambda::lambda_small_step",
      "lambda::let_small_step",
      "lambda::native"
    ]
  where
    lambdaModeSlowTests modeName =
      fmap
        (\testName -> modeName <> "::" <> testName)
        [ "add00",
          "add01",
          "y_identity"
        ]

lambdaFamilyRow :: String -> UpstreamCoverageRow
lambdaFamilyRow name =
  row SlottedEntrySuite name SlottedPassed SlottedPassed NotForcedIgnored (MoonlightFamilyCovered LambdaBindingGoalFamily)

lambdaAllFeaturesTooSlowRow :: String -> UpstreamCoverageRow
lambdaAllFeaturesTooSlowRow name =
  row SlottedEntrySuite name SlottedPassed (SlottedIgnored TooSlow) NotForcedIgnored (MoonlightFamilyCovered LambdaBindingGoalFamily)

comparableRow :: String -> PortGap -> UpstreamCoverageRow
comparableRow name gap =
  row SlottedEntrySuite name SlottedPassed SlottedPassed NotForcedIgnored (ComparableNotYetPorted gap)

row :: UpstreamSuite -> String -> SlottedOutcome -> SlottedOutcome -> ForcedIgnoredOutcome -> MoonlightCoverage -> UpstreamCoverageRow
row suiteValue name defaultOutcome allFeaturesOutcome forcedOutcome coverage =
  UpstreamCoverageRow
    { ucrSuite = suiteValue,
      ucrName = UpstreamTestName name,
      ucrDefaultOutcome = defaultOutcome,
      ucrAllFeaturesOutcome = allFeaturesOutcome,
      ucrForcedIgnoredOutcome = forcedOutcome,
      ucrMoonlightCoverage = coverage
    }

moonlightOnly :: MoonlightOnlyCapability -> String -> String -> MoonlightOnlyRow
moonlightOnly capability evidence slottedReason =
  MoonlightOnlyRow
    { morCapability = capability,
      morEvidence = evidence,
      morSlottedReason = slottedReason
    }

filterByDefaultIgnored :: [UpstreamCoverageRow] -> [UpstreamCoverageRow]
filterByDefaultIgnored =
  filter (isIgnored . ucrDefaultOutcome)

filterByForcedNodeLimit :: [UpstreamCoverageRow] -> [UpstreamCoverageRow]
filterByForcedNodeLimit =
  filter
    ( \rowValue ->
        case ucrForcedIgnoredOutcome rowValue of
          ForcedIgnoredFailed NodeLimitFailure -> True
          NotForcedIgnored -> False
    )

coverageCount :: (MoonlightCoverage -> Bool) -> Int
coverageCount predicate =
  length (filter (predicate . ucrMoonlightCoverage) upstreamSlottedCoverageRows)

isFamilyCovered :: MoonlightCoverage -> Bool
isFamilyCovered =
  \case
    MoonlightFamilyCovered _ -> True
    ComparableNotYetPorted _ -> False
    UpstreamInternalOnly _ -> False

isComparableDebt :: MoonlightCoverage -> Bool
isComparableDebt =
  \case
    MoonlightFamilyCovered _ -> False
    ComparableNotYetPorted _ -> True
    UpstreamInternalOnly _ -> False

isUpstreamInternal :: MoonlightCoverage -> Bool
isUpstreamInternal =
  \case
    MoonlightFamilyCovered _ -> False
    ComparableNotYetPorted _ -> False
    UpstreamInternalOnly _ -> True

isIgnored :: SlottedOutcome -> Bool
isIgnored =
  \case
    SlottedPassed -> False
    SlottedIgnored _ -> True

upstreamCoverageHeader :: String
upstreamCoverageHeader =
  "suite,test,theirs_default,theirs_all_features,forced_ignored,moonlight_status,moonlight_reason"

moonlightOnlyHeader :: String
moonlightOnlyHeader =
  "capability,moonlight_evidence,why_slotted_cannot_cover"

renderUpstreamCoverageRow :: UpstreamCoverageRow -> String
renderUpstreamCoverageRow rowValue =
  intercalate
    ","
    [ renderSuite (ucrSuite rowValue),
      unUpstreamTestName (ucrName rowValue),
      renderSlottedOutcome (ucrDefaultOutcome rowValue),
      renderSlottedOutcome (ucrAllFeaturesOutcome rowValue),
      renderForcedIgnoredOutcome (ucrForcedIgnoredOutcome rowValue),
      renderCoverageStatus (ucrMoonlightCoverage rowValue),
      renderCoverageReason (ucrMoonlightCoverage rowValue)
    ]

renderMoonlightOnlyRow :: MoonlightOnlyRow -> String
renderMoonlightOnlyRow rowValue =
  intercalate
    ","
    [ renderMoonlightOnlyCapability (morCapability rowValue),
      morEvidence rowValue,
      morSlottedReason rowValue
    ]

renderSuite :: UpstreamSuite -> String
renderSuite =
  \case
    SlottedLibSuite -> "lib"
    SlottedEntrySuite -> "entry"

renderSlottedOutcome :: SlottedOutcome -> String
renderSlottedOutcome =
  \case
    SlottedPassed -> "PASS"
    SlottedIgnored reason -> "IGNORED_" <> renderSlottedIgnoreReason reason

renderSlottedIgnoreReason :: SlottedIgnoreReason -> String
renderSlottedIgnoreReason =
  \case
    TooSlow -> "TOO_SLOW"
    TodoFails -> "TODO_FAILS"

renderForcedIgnoredOutcome :: ForcedIgnoredOutcome -> String
renderForcedIgnoredOutcome =
  \case
    NotForcedIgnored -> "NOT_FORCED"
    ForcedIgnoredFailed reason -> "FAIL_" <> renderSlottedFailureReason reason

renderSlottedFailureReason :: SlottedFailureReason -> String
renderSlottedFailureReason =
  \case
    NodeLimitFailure -> "NODE_LIMIT"

renderCoverageStatus :: MoonlightCoverage -> String
renderCoverageStatus =
  \case
    MoonlightFamilyCovered _ -> "MOONLIGHT_FAMILY_COVERED"
    ComparableNotYetPorted _ -> "COMPARABLE_NOT_YET_EXACT_PORTED"
    UpstreamInternalOnly _ -> "UPSTREAM_INTERNAL_ONLY"

renderCoverageReason :: MoonlightCoverage -> String
renderCoverageReason =
  \case
    MoonlightFamilyCovered evidence -> renderMoonlightEvidence evidence
    ComparableNotYetPorted gap -> renderPortGap gap
    UpstreamInternalOnly surface -> renderInternalSurface surface

renderMoonlightEvidence :: MoonlightEvidence -> String
renderMoonlightEvidence =
  \case
    LambdaBindingGoalFamily -> "LambdaBindingGoal plus slotted interop binding family"

renderPortGap :: PortGap -> String
renderPortGap =
  \case
    PlainArithmeticReachability -> "plain arithmetic reachability exact row not yet ported"
    AnalysisAndExtractionFixture -> "analysis extraction exact row not yet ported"
    SlotPermutationWitness -> "slot permutation exact row not yet ported"
    RiseRewriteCorpus -> "RISE rewrite exact row not yet ported"
    SdqlBinderRewrite -> "SDQL binder rewrite exact row not yet ported"

renderInternalSurface :: InternalSurface -> String
renderInternalSurface =
  \case
    SlottedCoreInvariant -> "slotted crate core invariant"
    SlottedGroupInvariant -> "slotted crate group invariant"
    SlottedSlotMapInvariant -> "slotted crate slot map invariant"
    DeterministicHasherInvariant -> "slotted crate deterministic hasher invariant"

renderMoonlightOnlyCapability :: MoonlightOnlyCapability -> String
renderMoonlightOnlyCapability =
  \case
    ContextLatticeGrowth -> "CONTEXT_LATTICE_GROWTH"
    TypedBindingObstruction -> "TYPED_BINDING_OBSTRUCTION"
    ScopeSafeLetFloat -> "SCOPE_SAFE_LET_FLOAT"
    DescentRestrictionGluing -> "DESCENT_RESTRICTION_GLUING"
    BranchLocalEquality -> "BRANCH_LOCAL_EQUALITY"
    ExhaustiveCaseLift -> "EXHAUSTIVE_CASE_LIFT"
