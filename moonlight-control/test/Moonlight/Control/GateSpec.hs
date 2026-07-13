module Moonlight.Control.GateSpec
  ( tests,
  )
where

import Data.Functor.Identity (Identity (..), runIdentity)
import Numeric.Natural (Natural)

import Moonlight.Control.Candidate
  ( CandidateSpace,
    finiteCandidateSpace,
    scheduledBatchCount,
    scheduledBatchMatches,
  )
import Moonlight.Control.Count
  ( WorkCount,
    workCountExact,
    workCountZero,
  )
import Moonlight.Control.Gate
  ( Gate (..),
    GateCompatibilityError (..),
    GatePullTrace (..),
    GateValidation (..),
    MatchSelector (..),
    MatchSelectorResult (..),
    composeSelectors,
    filterSelector,
    gateCandidateSpace,
    gateName,
    noGate,
    noSelector,
    validateGateScheduler,
  )
import Moonlight.Control.Modality
  ( gateIsUnit,
  )
import Moonlight.Control.Schedule
  ( SchedulerConfig,
    defaultSchedulerConfig,
  )
import Moonlight.Control.Schedule.Round
  ( ScheduleOutcome (..),
    emptySchedulerState,
    scheduleCandidateSpace,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "Gate / MatchSelector semantics"
    [ testCase "noSelector is identity for match list" testNoSelectorIsIdentity,
      testCase "noSelector has empty name" testNoSelectorHasEmptyName,
      testCase "filterSelector retains accepted matches in order" testFilterSelectorRetainsOrder,
      testCase "filterSelector counts rejected matches" testFilterSelectorCounts,
      testCase "composeSelectors pipes left output into right" testComposeSelectorsSequential,
      testCase "composeSelectors sums rejected counts" testComposeSelectorsRejectedCounts,
      testCase "noSelector is left identity under composeSelectors" testNoSelectorLeftIdentity,
      testCase "noSelector is right identity under composeSelectors" testNoSelectorRightIdentity,
      testCase "composeSelectors is associative (result)" testComposeSelectorAssociative,
      testCase "noGate passes gate pull trace with zero rejected" testNoGatePullTrace,
      testCase "filterSelector gate: gptRejectedCount counts rejected" testFilterGateRejectedCount,
      testCase "filterSelector gate: gptRawPulledCount equals total pulled" testFilterGateRawPulledCount,
      testCase "filterSelector gate: gptAcceptedCount equals scheduled count" testFilterGateAcceptedCount,
      testCase "gateName returns selector name" testGateName,
      testCase "gateIsUnit is True for noGate" testGateIsUnitNoGate,
      testCase "gateIsUnit is False for named filter gate" testGateIsUnitFilterGate,
      testCase "validateGateScheduler rejects when GateValidation rejects" testValidationRejects,
      testCase "validateGateScheduler accepts when GateValidation accepts" testValidationAccepts,
      testCase "filterSelector bounds sparse pulls by raw demand" testFilterGateSparseRawDemand,
      QC.testProperty "noSelector preserves match list exactly" prop_noSelectorPreservesMatches,
      QC.testProperty "composeSelectors associativity on small match lists" prop_composeSelectorAssociativity
    ]

newtype SmallMatchList = SmallMatchList
  { smallMatchList :: [Int]
  }
  deriving stock (Eq, Show)

instance QC.Arbitrary SmallMatchList where
  arbitrary =
    SmallMatchList <$> (QC.chooseInt (0, 12) >>= \n -> QC.vectorOf n (QC.chooseInt (-20, 20)))
  shrink _ = []

candidateSpace :: [(String, [Int])] -> CandidateSpace Identity String () Int
candidateSpace = finiteCandidateSpace

runSchedule ::
  Monoid meta =>
  SchedulerConfig String ->
  Natural ->
  CandidateSpace Identity String meta Int ->
  ScheduleOutcome String meta Int
runSchedule config budget space =
  runIdentity (scheduleCandidateSpace config budget 0 space emptySchedulerState)

testNoSelectorIsIdentity :: Assertion
testNoSelectorIsIdentity = do
  let matches = [1 :: Int, 2, 3, 4, 5]
      result = runMatchSelector noSelector () () matches
  msrAcceptedMatches result @?= matches
  msrRejectedCount result @?= 0

testNoSelectorHasEmptyName :: Assertion
testNoSelectorHasEmptyName =
  matchSelectorName noSelector @?= ""

testFilterSelectorRetainsOrder :: Assertion
testFilterSelectorRetainsOrder = do
  let matches = [1 :: Int, 2, 3, 4, 5, 6]
      sel = filterSelector "even" (\() m -> even m)
      result = runMatchSelector sel () () matches
  msrAcceptedMatches result @?= [2, 4, 6]

testFilterSelectorCounts :: Assertion
testFilterSelectorCounts = do
  let matches = [1 :: Int, 2, 3, 4, 5]
      sel = filterSelector "even" (\() m -> even m)
      result = runMatchSelector sel () () matches
  msrRejectedCount result @?= 3

testComposeSelectorsSequential :: Assertion
testComposeSelectorsSequential = do
  let matches = [1 :: Int, 2, 3, 4, 5, 6, 7, 8]
      selEven = filterSelector "even" (\() m -> even m)
      selGt4 = filterSelector "gt4" (\() m -> m > 4)
      composed = composeSelectors selEven selGt4
      result = runMatchSelector composed () () matches
  msrAcceptedMatches result @?= [6, 8]

testComposeSelectorsRejectedCounts :: Assertion
testComposeSelectorsRejectedCounts = do
  let matches = [1 :: Int, 2, 3, 4, 5, 6, 7, 8]
      selEven = filterSelector "even" (\() m -> even m)
      selGt4 = filterSelector "gt4" (\() m -> m > 4)
      composed = composeSelectors selEven selGt4
      result = runMatchSelector composed () () matches
  msrRejectedCount result @?= 6

testNoSelectorLeftIdentity :: Assertion
testNoSelectorLeftIdentity = do
  let matches = [1 :: Int, 2, 3, 4, 5]
      sel = filterSelector "even" (\() m -> even m)
      composed = composeSelectors noSelector sel
      expected = runMatchSelector sel () () matches
      actual = runMatchSelector composed () () matches
  msrAcceptedMatches actual @?= msrAcceptedMatches expected
  msrRejectedCount actual @?= msrRejectedCount expected

testNoSelectorRightIdentity :: Assertion
testNoSelectorRightIdentity = do
  let matches = [1 :: Int, 2, 3, 4, 5]
      sel = filterSelector "even" (\() m -> even m)
      composed = composeSelectors sel noSelector
      expected = runMatchSelector sel () () matches
      actual = runMatchSelector composed () () matches
  msrAcceptedMatches actual @?= msrAcceptedMatches expected
  msrRejectedCount actual @?= msrRejectedCount expected

testComposeSelectorAssociative :: Assertion
testComposeSelectorAssociative = do
  let matches = [1 :: Int, 2, 3, 4, 5, 6, 7, 8, 9, 10]
      a = filterSelector "even" (\() m -> even m)
      b = filterSelector "gt4" (\() m -> m > 4)
      c = filterSelector "lt9" (\() m -> m < 9)
      left = runMatchSelector (composeSelectors (composeSelectors a b) c) () () matches
      right = runMatchSelector (composeSelectors a (composeSelectors b c)) () () matches
  msrAcceptedMatches left @?= msrAcceptedMatches right
  msrRejectedCount left @?= msrRejectedCount right

testNoGatePullTrace :: Assertion
testNoGatePullTrace = do
  let guided = gateCandidateSpace noGate () (candidateSpace [("g", [1 :: Int, 2, 3])])
      outcome = runSchedule defaultSchedulerConfig 10 guided
      pt = soPullMeta outcome
  gptRejectedCount pt @?= workCountZero
  gptAcceptedCount pt @?= 3
  gptRawPulledCount pt @?= 3

testFilterGateRejectedCount :: Assertion
testFilterGateRejectedCount = do
  let gate = Gate { gateSelector = filterSelector "even" (\() m -> even m), gateValidation = mempty }
      guided = gateCandidateSpace gate () (candidateSpace [("n", [1 :: Int, 2, 3, 4, 5])])
      outcome = runSchedule defaultSchedulerConfig 10 guided
      pt = soPullMeta outcome
  gptRejectedCount pt @?= workCountExact 3

testFilterGateRawPulledCount :: Assertion
testFilterGateRawPulledCount = do
  let gate = Gate { gateSelector = filterSelector "even" (\() m -> even m), gateValidation = mempty }
      guided = gateCandidateSpace gate () (candidateSpace [("n", [1 :: Int, 2, 3, 4, 5])])
      outcome = runSchedule defaultSchedulerConfig 10 guided
      pt = soPullMeta outcome
  gptRawPulledCount pt @?= 5

testFilterGateAcceptedCount :: Assertion
testFilterGateAcceptedCount = do
  let gate = Gate { gateSelector = filterSelector "even" (\() m -> even m), gateValidation = mempty }
      guided = gateCandidateSpace gate () (candidateSpace [("n", [1 :: Int, 2, 3, 4, 5])])
      outcome = runSchedule defaultSchedulerConfig 10 guided
      pt = soPullMeta outcome
  gptAcceptedCount pt @?= 2
  scheduledBatchMatches (soScheduledBatch outcome) @?= [2, 4]

testGateName :: Assertion
testGateName = do
  let gate = Gate { gateSelector = filterSelector "my-filter" (\() (_ :: Int) -> True), gateValidation = mempty }
  gateName gate @?= "my-filter"

testGateIsUnitNoGate :: Assertion
testGateIsUnitNoGate =
  assertBool "noGate should satisfy gateIsUnit" (gateIsUnit noGate)

testGateIsUnitFilterGate :: Assertion
testGateIsUnitFilterGate = do
  let gate = Gate { gateSelector = filterSelector "f" (\() (_ :: Int) -> True), gateValidation = mempty }
  assertBool "named filter gate should not satisfy gateIsUnit" (not (gateIsUnit gate))

testValidationRejects :: Assertion
testValidationRejects = do
  let rejectingGate :: Gate () () Int () String
      rejectingGate =
        Gate
          { gateSelector = noSelector,
            gateValidation = GateValidation (\config -> Left (GateRejectedScheduler config))
          }
  case validateGateScheduler rejectingGate defaultSchedulerConfig of
    Left (GateRejectedScheduler _) -> pure ()
    Right () -> fail "expected rejection but got success"

testValidationAccepts :: Assertion
testValidationAccepts =
  case validateGateScheduler noGate (defaultSchedulerConfig :: SchedulerConfig String) of
    Right () -> pure ()
    Left err -> fail ("unexpected rejection: " <> show err)

testFilterGateSparseRawDemand :: Assertion
testFilterGateSparseRawDemand = do
  let gate = Gate { gateSelector = filterSelector "even" (\() m -> even m), gateValidation = mempty }
      guided = gateCandidateSpace gate () (candidateSpace [("n", [1 :: Int, 2, 3])])
      outcome = runSchedule defaultSchedulerConfig 1 guided
      pt = soPullMeta outcome
  scheduledBatchMatches (soScheduledBatch outcome) @?= []
  gptRawPulledCount pt @?= 1
  gptRejectedCount pt @?= workCountExact 1

prop_noSelectorPreservesMatches :: SmallMatchList -> QC.Property
prop_noSelectorPreservesMatches (SmallMatchList matches) =
  let result = runMatchSelector (noSelector :: MatchSelector () () Int ()) () () matches
   in msrAcceptedMatches result QC.=== matches

prop_composeSelectorAssociativity :: SmallMatchList -> QC.Property
prop_composeSelectorAssociativity (SmallMatchList matches) =
  let a = filterSelector "pos" (\() m -> m > 0)
      b = filterSelector "lt10" (\() m -> m < 10)
      c = filterSelector "even" (\() m -> even m)
      leftAssoc = runMatchSelector (composeSelectors (composeSelectors a b) c) () () matches
      rightAssoc = runMatchSelector (composeSelectors a (composeSelectors b c)) () () matches
   in QC.conjoin
        [ msrAcceptedMatches leftAssoc QC.=== msrAcceptedMatches rightAssoc,
          msrRejectedCount leftAssoc QC.=== msrRejectedCount rightAssoc
        ]
