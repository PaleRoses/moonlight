module Moonlight.Control.WeightCountSpec
  ( tests,
  )
where

import Moonlight.Control.Count
  ( SuppressionCounts,
    WorkCount (..),
    WorkCoverage (..),
    anyCooldownSuppressed,
    anySuppressed,
    cooldownSuppressedRoundCount,
    emptySuppressionCounts,
    observedRoundCount,
    singletonSuppressionCounts,
    workCountAtLeast,
    workCountExact,
    workCountMayBePositive,
    workCountUnknown,
    workCountZero,
    workCoverageFromRemaining,
  )
import Moonlight.Control.Engine.Evidence
  ( EvidencePolicy (..),
    PriorityUpdateMode (..),
    applyEvidencePolicies,
    noEvidencePolicy,
  )
import Moonlight.Control.Weight
  ( CriticalityRank,
    EvidenceCount (..),
    PriorityEvidence (..),
    PriorityProfile,
    comparePriorityEvidence,
    criticalPriorityRank,
    emptyPriorityProfile,
    lookupPriorityEvidence,
    nonCriticalPriorityRank,
    priorityEvidence,
    priorityEvidenceKey,
    priorityProfileFromList,
    singletonPriorityProfile,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "WorkCount / WorkCoverage / SuppressionCounts / PriorityProfile / PriorityEvidence"
    [ testCase "workCountAtLeast 0 is conservative" testWorkCountAtLeastZeroIsConservative,
      testCase "cooldown suppression deduplicates rounds by index" testCooldownSuppressedRoundDedup,
      testCase "zero-candidate suppression entry does not count as cooldown-suppressed" testZeroCandidateSuppression,
      testCase "priorityEvidence clamps negative counts to zero" testPriorityEvidenceClampsNegative,
      testCase "PriorityEvidence monoid accumulates counts and joins rank" testPriorityEvidenceMonoid,
      testCase "comparePriorityEvidence agrees with priorityEvidenceKey ordering" testCompareAgreesWithKey,
      testCase "comparePriorityEvidence rank dominates evidence counts" testCriticalityRankDominates,
      testCase "emptyPriorityProfile is identity under join" testEmptyProfileIdentity,
      testCase "singletonPriorityProfile with mempty evidence yields empty profile" testSingletonMempytIsEmpty,
      testCase "priorityProfileFromList merges duplicate keys by monoid" testProfileFromListMergesDuplicates,
      testCase "applyEvidencePolicies accumulate mode adds to current profile" testAccumulateAddsToProfile,
      testCase "applyEvidencePolicies replace mode discards stale profile" testReplaceDiscardsStale,
      testCase "applyEvidencePolicies replace wins over accumulate within round" testReplaceWinsOverAccumulate,
      testCase "noEvidencePolicy leaves profile unchanged" testNoEvidencePolicyUnchanged,
      testCase "SuppressionCounts monoid left identity" testSuppressionCountsLeftIdentity,
      testCase "SuppressionCounts monoid right identity" testSuppressionCountsRightIdentity,
      testCase "SuppressionCounts monoid associativity" testSuppressionCountsAssociativity,
      testCase "anySuppressed reflects suppressed count" testAnySuppressed,
      testCase "anyCooldownSuppressed reflects cooldown rounds" testAnyCooldownSuppressed,
      QC.testProperty "WorkCount (<>) is associative" prop_workCountAssociative,
      QC.testProperty "WorkCount zero is left identity" prop_workCountZeroLeftIdentity,
      QC.testProperty "WorkCount zero is right identity" prop_workCountZeroRightIdentity,
      QC.testProperty "WorkCoverage (<>) is associative" prop_workCoverageAssociative,
      QC.testProperty "WorkCoverage (<>) is idempotent" prop_workCoverageIdempotent,
      QC.testProperty "WorkCoverageComplete is identity for WorkCoverage" prop_workCoverageCompleteIdentity,
      QC.testProperty "PriorityProfile (<>) is associative" prop_priorityProfileAssociative,
      QC.testProperty "PriorityProfile empty is left identity" prop_priorityProfileLeftIdentity,
      QC.testProperty "PriorityProfile empty is right identity" prop_priorityProfileRightIdentity,
      QC.testProperty "PriorityProfile (<>) is commutative" prop_priorityProfileCommutative,
      QC.testProperty "comparePriorityEvidence is a total order (reflexive)" prop_compareReflexive,
      QC.testProperty "comparePriorityEvidence agrees with key comparison" prop_compareAgreesWithKeyProp
    ]

newtype AWorkCount = AWorkCount
  { aWorkCount :: WorkCount
  }
  deriving stock (Eq, Show)

newtype AWorkCoverage = AWorkCoverage
  { aWorkCoverage :: WorkCoverage
  }
  deriving stock (Eq, Show)

newtype APriorityProfile = APriorityProfile
  { aPriorityProfile :: PriorityProfile Int
  }
  deriving stock (Eq, Show)

newtype APriorityEvidence = APriorityEvidence
  { aPriorityEvidence :: PriorityEvidence
  }
  deriving stock (Eq, Show)

instance QC.Arbitrary AWorkCount where
  arbitrary =
    AWorkCount <$> do
      tag <- QC.chooseInt (0, 2)
      case tag of
        0 -> workCountExact . fromIntegral <$> QC.chooseInt (0, 32)
        1 -> workCountAtLeast . fromIntegral <$> QC.chooseInt (0, 32)
        _ -> pure workCountUnknown
  shrink _ = []

instance QC.Arbitrary AWorkCoverage where
  arbitrary =
    AWorkCoverage
      <$> QC.elements [WorkCoverageComplete, WorkCoveragePartial, WorkCoverageUnknown]
  shrink _ = []

instance QC.Arbitrary APriorityEvidence where
  arbitrary =
    APriorityEvidence
      <$> ( priorityEvidence
              <$> QC.chooseInt (0, 20)
              <*> QC.chooseInt (0, 20)
              <*> QC.chooseInt (0, 20)
              <*> QC.elements [nonCriticalPriorityRank, criticalPriorityRank]
          )
  shrink _ = []

instance QC.Arbitrary APriorityProfile where
  arbitrary = do
    entries <- QC.chooseInt (0, 6) >>= \n ->
      QC.vectorOf n ((,) <$> QC.chooseInt (-4, 4) <*> (aPriorityEvidence <$> QC.arbitrary))
    pure (APriorityProfile (priorityProfileFromList entries))
  shrink _ = []

testWorkCountAtLeastZeroIsConservative :: Assertion
testWorkCountAtLeastZeroIsConservative = do
  workCountAtLeast 0 @?= workCountUnknown
  workCountAtLeast 1 @?= WorkCountAtLeast 1
  workCountMayBePositive (WorkCountAtLeast 0) @?= True
  workCoverageFromRemaining (WorkCountAtLeast 0) @?= WorkCoverageUnknown

testCooldownSuppressedRoundDedup :: Assertion
testCooldownSuppressedRoundDedup = do
  let first = singletonSuppressionCounts 7 (workCountExact 2) 0 (workCountExact 2) True
      second = singletonSuppressionCounts 7 (workCountExact 1) 0 (workCountExact 1) True
  cooldownSuppressedRoundCount (first <> second) @?= 1

testZeroCandidateSuppression :: Assertion
testZeroCandidateSuppression = do
  let entry = singletonSuppressionCounts 8 (workCountExact 1) 0 workCountZero True
  assertBool "zero dropped count must not count as cooldown-suppressed" (not (anyCooldownSuppressed entry))

testPriorityEvidenceClampsNegative :: Assertion
testPriorityEvidenceClampsNegative =
  let left = priorityEvidence (-1) 2 3 criticalPriorityRank
      right = priorityEvidence 4 0 (-8) criticalPriorityRank
   in left <> right @?= priorityEvidence 4 2 3 criticalPriorityRank

testPriorityEvidenceMonoid :: Assertion
testPriorityEvidenceMonoid = do
  let a = priorityEvidence 1 0 2 nonCriticalPriorityRank
      b = priorityEvidence 0 3 1 criticalPriorityRank
      c = a <> b
  peStructuralInfluence c @?= EvidenceCount 1
  peObservedTransitionCount c @?= EvidenceCount 3
  peObservedScheduledCount c @?= EvidenceCount 3
  peCriticalityRank c @?= criticalPriorityRank

testCompareAgreesWithKey :: Assertion
testCompareAgreesWithKey = do
  let a = priorityEvidence 0 5 0 nonCriticalPriorityRank
      b = priorityEvidence 0 3 0 nonCriticalPriorityRank
  comparePriorityEvidence a b @?= compare (priorityEvidenceKey a) (priorityEvidenceKey b)

testCriticalityRankDominates :: Assertion
testCriticalityRankDominates = do
  let highCount = priorityEvidence 100 100 100 nonCriticalPriorityRank
      lowCountCritical = priorityEvidence 0 0 1 criticalPriorityRank
  comparePriorityEvidence lowCountCritical highCount @?= LT

testEmptyProfileIdentity :: Assertion
testEmptyProfileIdentity = do
  let p = singletonPriorityProfile (1 :: Int) (priorityEvidence 2 3 4 nonCriticalPriorityRank)
  emptyPriorityProfile <> p @?= p
  p <> emptyPriorityProfile @?= p

testSingletonMempytIsEmpty :: Assertion
testSingletonMempytIsEmpty = do
  let p = singletonPriorityProfile (1 :: Int) mempty
  p @?= emptyPriorityProfile

testProfileFromListMergesDuplicates :: Assertion
testProfileFromListMergesDuplicates = do
  let p =
        priorityProfileFromList
          [ (1 :: Int, priorityEvidence 1 0 0 nonCriticalPriorityRank),
            (1, priorityEvidence 0 2 0 nonCriticalPriorityRank)
          ]
  lookupPriorityEvidence 1 p @?= priorityEvidence 1 2 0 nonCriticalPriorityRank

testAccumulateAddsToProfile :: Assertion
testAccumulateAddsToProfile = do
  let current = singletonPriorityProfile "a" (priorityEvidence 0 0 5 nonCriticalPriorityRank)
      policy =
        EvidencePolicy
          { epObserve = const (singletonPriorityProfile "b" (priorityEvidence 0 0 3 nonCriticalPriorityRank)),
            epUpdateMode = AccumulateDynamicPriority,
            epNeedsScheduleTrace = False
          }
      result = applyEvidencePolicies [policy] () current
  lookupPriorityEvidence "a" result @?= priorityEvidence 0 0 5 nonCriticalPriorityRank
  lookupPriorityEvidence "b" result @?= priorityEvidence 0 0 3 nonCriticalPriorityRank

testReplaceDiscardsStale :: Assertion
testReplaceDiscardsStale = do
  let current = singletonPriorityProfile "stale" (priorityEvidence 0 0 7 criticalPriorityRank)
      policy =
        EvidencePolicy
          { epObserve = const (singletonPriorityProfile "fresh" (priorityEvidence 1 0 0 criticalPriorityRank)),
            epUpdateMode = ReplaceDynamicPriority,
            epNeedsScheduleTrace = False
          }
      result = applyEvidencePolicies [policy] () current
  lookupPriorityEvidence "stale" result @?= mempty
  lookupPriorityEvidence "fresh" result @?= priorityEvidence 1 0 0 criticalPriorityRank

testReplaceWinsOverAccumulate :: Assertion
testReplaceWinsOverAccumulate = do
  let current = singletonPriorityProfile "stale" (priorityEvidence 0 0 7 criticalPriorityRank)
      result =
        applyEvidencePolicies
          [ EvidencePolicy
              { epObserve = const (singletonPriorityProfile "fresh" (priorityEvidence 1 0 0 criticalPriorityRank)),
                epUpdateMode = ReplaceDynamicPriority,
                epNeedsScheduleTrace = False
              },
            EvidencePolicy
              { epObserve = const (singletonPriorityProfile "delta" (priorityEvidence 0 1 0 criticalPriorityRank)),
                epUpdateMode = AccumulateDynamicPriority,
                epNeedsScheduleTrace = False
              }
          ]
          ()
          current
  lookupPriorityEvidence "stale" result @?= mempty
  lookupPriorityEvidence "fresh" result @?= priorityEvidence 1 0 0 criticalPriorityRank
  lookupPriorityEvidence "delta" result @?= priorityEvidence 0 1 0 criticalPriorityRank

testNoEvidencePolicyUnchanged :: Assertion
testNoEvidencePolicyUnchanged = do
  let current = singletonPriorityProfile "x" (priorityEvidence 0 3 0 nonCriticalPriorityRank)
      result = applyEvidencePolicies [noEvidencePolicy] () current
  result @?= current

testSuppressionCountsLeftIdentity :: Assertion
testSuppressionCountsLeftIdentity = do
  let s = singletonSuppressionCounts 1 (workCountExact 3) 2 (workCountExact 1) False
  emptySuppressionCounts <> s @?= s

testSuppressionCountsRightIdentity :: Assertion
testSuppressionCountsRightIdentity = do
  let s = singletonSuppressionCounts 1 (workCountExact 3) 2 (workCountExact 1) False
  s <> emptySuppressionCounts @?= s

testSuppressionCountsAssociativity :: Assertion
testSuppressionCountsAssociativity = do
  let a = singletonSuppressionCounts 1 (workCountExact 2) 1 (workCountExact 1) True
      b = singletonSuppressionCounts 2 (workCountExact 3) 2 (workCountExact 2) False
      c = singletonSuppressionCounts 3 (workCountExact 1) 0 workCountZero True
  (a <> b) <> c @?= a <> (b <> c)

testAnySuppressed :: Assertion
testAnySuppressed = do
  let suppressed = singletonSuppressionCounts 1 (workCountExact 2) 0 (workCountExact 2) False
      notSuppressed = singletonSuppressionCounts 1 (workCountExact 2) 2 workCountZero False
  assertBool "anySuppressed should be True when suppressed count is positive" (anySuppressed suppressed)
  assertBool "anySuppressed should be False when suppressed count is zero" (not (anySuppressed notSuppressed))

testAnyCooldownSuppressed :: Assertion
testAnyCooldownSuppressed = do
  let entry = singletonSuppressionCounts 5 (workCountExact 3) 0 (workCountExact 3) True
  assertBool "anyCooldownSuppressed should be True" (anyCooldownSuppressed entry)
  observedRoundCount entry @?= 1
  cooldownSuppressedRoundCount entry @?= 1

prop_workCountAssociative :: AWorkCount -> AWorkCount -> AWorkCount -> QC.Property
prop_workCountAssociative (AWorkCount a) (AWorkCount b) (AWorkCount c) =
  (a <> b) <> c QC.=== a <> (b <> c)

prop_workCountZeroLeftIdentity :: AWorkCount -> QC.Property
prop_workCountZeroLeftIdentity (AWorkCount c) =
  workCountZero <> c QC.=== c

prop_workCountZeroRightIdentity :: AWorkCount -> QC.Property
prop_workCountZeroRightIdentity (AWorkCount c) =
  c <> workCountZero QC.=== c

prop_workCoverageAssociative :: AWorkCoverage -> AWorkCoverage -> AWorkCoverage -> QC.Property
prop_workCoverageAssociative (AWorkCoverage a) (AWorkCoverage b) (AWorkCoverage c) =
  (a <> b) <> c QC.=== a <> (b <> c)

prop_workCoverageIdempotent :: AWorkCoverage -> QC.Property
prop_workCoverageIdempotent (AWorkCoverage c) =
  c <> c QC.=== c

prop_workCoverageCompleteIdentity :: AWorkCoverage -> QC.Property
prop_workCoverageCompleteIdentity (AWorkCoverage c) =
  QC.conjoin
    [ WorkCoverageComplete <> c QC.=== c,
      c <> WorkCoverageComplete QC.=== c
    ]

prop_priorityProfileAssociative :: APriorityProfile -> APriorityProfile -> APriorityProfile -> QC.Property
prop_priorityProfileAssociative (APriorityProfile a) (APriorityProfile b) (APriorityProfile c) =
  (a <> b) <> c QC.=== a <> (b <> c)

prop_priorityProfileLeftIdentity :: APriorityProfile -> QC.Property
prop_priorityProfileLeftIdentity (APriorityProfile p) =
  emptyPriorityProfile <> p QC.=== p

prop_priorityProfileRightIdentity :: APriorityProfile -> QC.Property
prop_priorityProfileRightIdentity (APriorityProfile p) =
  p <> emptyPriorityProfile QC.=== p

prop_priorityProfileCommutative :: APriorityProfile -> APriorityProfile -> QC.Property
prop_priorityProfileCommutative (APriorityProfile a) (APriorityProfile b) =
  a <> b QC.=== b <> a

prop_compareReflexive :: APriorityEvidence -> QC.Property
prop_compareReflexive (APriorityEvidence e) =
  comparePriorityEvidence e e QC.=== EQ

prop_compareAgreesWithKeyProp :: APriorityEvidence -> APriorityEvidence -> QC.Property
prop_compareAgreesWithKeyProp (APriorityEvidence a) (APriorityEvidence b) =
  comparePriorityEvidence a b QC.=== compare (priorityEvidenceKey a) (priorityEvidenceKey b)
