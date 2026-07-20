{-# LANGUAGE TypeFamilies #-}

module Moonlight.Cosheaf.Core.ComputedExampleSpec
  ( tests,
  )
where

import Data.Foldable (traverse_)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sort)
import Data.Set qualified as Set
import Moonlight.Cosheaf
import Moonlight.Cosheaf.Test.Support
  ( compileFullTropicalCostTable,
    fullFiniteCosheafColimit,
  )
import Moonlight.Cosheaf.Test.Fixture
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
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

tests :: TestTree
tests =
  testGroup
    "computed finite cosheaf examples"
    [ testCase "H0 classes are checked by membership and equivalence" testColimitMembershipAndEquivalence,
      testCase "class lookup round-trips every representative" testClassLookupRoundTrips,
      testCase "colimit members partition all representatives" testColimitMembersPartitionRepresentatives,
      testCase "factorization accepts class-constant maps and rejects nonconstant maps" testFactorization,
      testCase "cover quotient is bijective with target costalk" testCoverQuotientBijection,
      testCase "tropical planning chooses least propagated representative per class" testTropicalPlanning,
      testCase "min-plus algebra uses infinity as zero and finite zero as one" testMinPlusAlgebra,
      testCase "cover verification rejects missing pullback" testMissingPullback,
      testCase "cover verification rejects missing compiled cover corestriction" testMissingCompiledCoverCorestriction,
      testCase "cover verification rejects overlap class target conflict" testOverlapClassTargetConflict,
      testCase "cover verification rejects target non-surjectivity" testTargetNonSurjectivity,
      testCase "cover verification rejects target non-injectivity" testTargetNonInjectivity,
      testCase "tropical compilation rejects missing representative cost" testMissingRepresentativeCost,
      testCase "tropical compilation rejects missing transition cost" testMissingTransitionCost,
      testCase "tropical planning rejects negative-cycle unbounded costs" testNegativeCycleUnboundedPlan
    ]

testColimitMembershipAndEquivalence :: Assertion
testColimitMembershipAndEquivalence = do
  colimit <- goodColimit
  class0 <- expectRight (cosheafColimitClassOf (coverRep CoverRoot 100) colimit)
  class1 <- expectRight (cosheafColimitClassOf (coverRep CoverRoot 101) colimit)
  members0 <- expectRight (cosheafColimitMembers class0 colimit)
  members1 <- expectRight (cosheafColimitMembers class1 colimit)
  assertEqual "class-0 members" expectedClass0 (Set.fromList members0)
  assertEqual "class-1 members" expectedClass1 (Set.fromList members1)
  root100Key <- expectRight (cosectionRepresentativeKeyOf (coverRep CoverRoot 100) colimit)
  left10Key <- expectRight (cosectionRepresentativeKeyOf (coverRep CoverLeft 10) colimit)
  root101Key <- expectRight (cosectionRepresentativeKeyOf (coverRep CoverRoot 101) colimit)
  assertBool "root 100 equivalent to left 10" (cosheafColimitEquivalent root100Key left10Key colimit)
  assertBool "root 100 not equivalent to root 101" (not (cosheafColimitEquivalent root100Key root101Key colimit))

testClassLookupRoundTrips :: Assertion
testClassLookupRoundTrips = do
  colimit <- goodColimit
  traverse_
    (assertRoundTrip colimit)
    (cosheafColimitRepresentatives colimit)
  where
    assertRoundTrip :: CosheafColimit CoverSite Int -> CosectionRepresentative CoverObject Int -> Assertion
    assertRoundTrip colimit representativeValue = do
      classKey <- expectRight (cosheafColimitClassOf representativeValue colimit)
      members <- expectRight (cosheafColimitMembers classKey colimit)
      assertBool
        ("representative round-trips through class members: " <> show representativeValue)
        (Set.member representativeValue (Set.fromList members))

testColimitMembersPartitionRepresentatives :: Assertion
testColimitMembersPartitionRepresentatives = do
  colimit <- goodColimit
  classMembers <- expectRight (traverse (`cosheafColimitMembers` colimit) (cosheafColimitClassKeys colimit))
  let representativeSet =
        Set.fromList (cosheafColimitRepresentatives colimit)
      partitionMembers =
        concat classMembers
      partitionSet =
        Set.fromList partitionMembers
  assertEqual "partition covers all representatives" representativeSet partitionSet
  assertEqual "partition contains no duplicate representative" (Set.size representativeSet) (length partitionMembers)

testFactorization :: Assertion
testFactorization = do
  colimit <- goodColimit
  factors <- expectRight (factorCosheafColimit coverClassLabel colimit)
  assertEqual "class-constant factor targets" [0, 1] (sort (fmap ccfTarget factors))
  case factorCosheafColimit nonConstantTarget colimit of
    Left (CosheafColimitFactorIncompatible _ _ _ _ _) -> pure ()
    Left otherFailure -> assertFailure ("unexpected factor failure: " <> show otherFailure)
    Right _ -> assertFailure "expected nonconstant factor rejection"
  where
    nonConstantTarget representativeValue =
      case cosectionRepObject representativeValue of
        CoverRoot -> 99
        _ -> coverClassLabel representativeValue

testCoverQuotientBijection :: Assertion
testCoverQuotientBijection = do
  let coverValue = coverFamily
  cosheaf <- goodCosheaf
  coequalizerValue <- expectRight (coverCosheafCoequalizer coverValue cosheaf)
  assertEqual
    "cover quotient targets exactly the target costalk keys"
    (Set.fromList [CostalkKey 0, CostalkKey 1])
    (Set.fromList (IntMap.elems (cccClassTargets coequalizerValue)))
  assertEqual "two quotient classes" 2 (IntMap.size (cccClassTargets coequalizerValue))

testTropicalPlanning :: Assertion
testTropicalPlanning = do
  colimit <- goodColimit
  costTable <- expectRight (compileFullTropicalCostTable colimit coverTropicalCostModel)
  tropicalPlan <- expectRight (planTropicalCosections costTable)
  let choices =
        fmap choiceSummary (IntMap.elems (tcpClassChoices tropicalPlan))
  assertEqual
    "least propagated representative per class"
    [(0, CoverLeft, 10, MinPlusFinite 1), (1, CoverLeft, 11, MinPlusFinite 2)]
    (sort choices)
  where
    choiceSummary choice =
      ( coverClassLabel (tccRepresentative choice),
        cosectionRepObject (tccRepresentative choice),
        cosectionRepValue (tccRepresentative choice),
        tccCost choice
      )

testMinPlusAlgebra :: Assertion
testMinPlusAlgebra = do
  assertEqual "additive zero is infinity" (MinPlusFinite 3) (minPlusAdd minPlusZero (MinPlusFinite 3))
  assertEqual "multiplicative one is finite zero" (MinPlusFinite 3) (minPlusMul minPlusOne (MinPlusFinite 3))
  assertEqual "infinity annihilates multiplication" MinPlusInfinity (minPlusMul MinPlusInfinity (MinPlusFinite 3))
  assertEqual "finite multiplication is rational addition" (MinPlusFinite 5) (minPlusMul (MinPlusFinite 2) (MinPlusFinite 3))
  assertEqual "sum chooses least finite weight" (MinPlusFinite 1) (minPlusSum [MinPlusFinite 4, MinPlusInfinity, MinPlusFinite 1])

testMissingPullback :: Assertion
testMissingPullback = do
  let coverValue = coverFamily
  cosheaf <- expectRight (coverCosheaf (CoverSite CoverMissingPullbackSite) CoverGoodAlgebra coverRawCostalks)
  case coverCosheafCoequalizer coverValue cosheaf of
    Left (CoverCosheafEffectiveCoverInvalid _) -> pure ()
    Left otherFailure -> assertFailure ("unexpected cover failure: " <> show otherFailure)
    Right _ -> assertFailure "expected missing pullback failure"

testMissingCompiledCoverCorestriction :: Assertion
testMissingCompiledCoverCorestriction = do
  let coverValue = coverFamilyWithSyntheticLeft
  cosheaf <- expectRight (coverCosheaf (CoverSite CoverMissingCompiledCoverCorestrictionSite) CoverGoodAlgebra coverRawCostalks)
  case coverCosheafCoequalizer coverValue cosheaf of
    Left (CoverCosheafCorestrictionMissing morphismValue) ->
      assertEqual "missing synthetic cover arrow" coverSyntheticLeftToRoot morphismValue
    Left otherFailure -> assertFailure ("unexpected cover failure: " <> show otherFailure)
    Right _ -> assertFailure "expected missing compiled cover corestriction"

testOverlapClassTargetConflict :: Assertion
testOverlapClassTargetConflict = do
  let coverValue = coverFamily
  cosheaf <- expectRight (coverCosheaf (CoverSite CoverGoodSite) CoverConflictAlgebra coverRawCostalks)
  case coverCosheafCoequalizer coverValue cosheaf of
    Left (CoverCosheafClassTargetConflict _ (CostalkKey 0) (CostalkKey 1)) -> pure ()
    Left (CoverCosheafClassTargetConflict _ (CostalkKey 1) (CostalkKey 0)) -> pure ()
    Left otherFailure -> assertFailure ("unexpected cover failure: " <> show otherFailure)
    Right _ -> assertFailure "expected overlap class target conflict"

testTargetNonSurjectivity :: Assertion
testTargetNonSurjectivity = do
  let coverValue = coverFamily
  cosheaf <- expectRight (coverCosheaf (CoverSite CoverGoodSite) CoverNonSurjectiveAlgebra coverRawCostalks)
  case coverCosheafCoequalizer coverValue cosheaf of
    Left (CoverCosheafTargetNotSurjective [CostalkKey 1]) -> pure ()
    Left otherFailure -> assertFailure ("unexpected cover failure: " <> show otherFailure)
    Right _ -> assertFailure "expected target non-surjectivity"

testTargetNonInjectivity :: Assertion
testTargetNonInjectivity = do
  let coverValue = coverFamily
  cosheaf <- expectRight (coverCosheaf (CoverSite CoverGoodSite) CoverNonInjectiveAlgebra coverRawCostalksWithSingletonRoot)
  case coverCosheafCoequalizer coverValue cosheaf of
    Left (CoverCosheafTargetNotInjective (CostalkKey 0) classKeys) ->
      assertBool "at least two classes collide" (length classKeys >= 2)
    Left otherFailure -> assertFailure ("unexpected cover failure: " <> show otherFailure)
    Right _ -> assertFailure "expected target non-injectivity"

testMissingRepresentativeCost :: Assertion
testMissingRepresentativeCost = do
  colimit <- goodColimit
  case compileFullTropicalCostTable colimit missingRepresentativeModel of
    Left (TropicalRepresentativeCostMissing representativeValue) ->
      assertEqual "missing representative" (coverRep CoverLeft 10) representativeValue
    Left otherFailure -> assertFailure ("unexpected tropical failure: " <> show otherFailure)
    Right _ -> assertFailure "expected missing representative cost"
  where
    missingRepresentativeModel =
      coverTropicalCostModel
        { tcmRepresentativeCost = \representativeValue ->
            if representativeValue == coverRep CoverLeft 10
              then Left (TropicalRepresentativeCostMissing representativeValue)
              else Right (coverRepresentativeCost representativeValue)
        }

testMissingTransitionCost :: Assertion
testMissingTransitionCost = do
  colimit <- goodColimit
  case compileFullTropicalCostTable colimit missingTransitionModel of
    Left (TropicalTransitionCostMissing transitionValue) ->
      assertEqual "missing transition morphism" CoverLeftToRoot (cmWitness (tropicalTransitionMorphism transitionValue))
    Left otherFailure -> assertFailure ("unexpected tropical failure: " <> show otherFailure)
    Right _ -> assertFailure "expected missing transition cost"
  where
    missingTransitionModel =
      coverTropicalCostModel
        { tcmTransitionCost = \transitionValue ->
            case cmWitness (tropicalTransitionMorphism transitionValue) of
              CoverLeftToRoot -> Left (TropicalTransitionCostMissing transitionValue)
              _ -> Right (coverTransitionCost transitionValue)
        }

testNegativeCycleUnboundedPlan :: Assertion
testNegativeCycleUnboundedPlan = do
  cosheaf <- expectRight twoWayCosheaf
  colimit <- expectRight (fullFiniteCosheafColimit cosheaf)
  costTable <- expectRight (compileFullTropicalCostTable colimit twoWayNegativeCostModel)
  case planTropicalCosections costTable of
    Left (TropicalUnboundedCost _) -> pure ()
    Left otherFailure -> assertFailure ("unexpected tropical failure: " <> show otherFailure)
    Right _ -> assertFailure "expected negative-cycle unbounded plan"

goodCosheaf :: IO (FiniteCosheaf CoverSite Int)
goodCosheaf =
  expectRight (coverCosheaf (CoverSite CoverGoodSite) CoverGoodAlgebra coverRawCostalks)

goodColimit :: IO (CosheafColimit CoverSite Int)
goodColimit = do
  cosheaf <- goodCosheaf
  expectRight (fullFiniteCosheafColimit cosheaf)

coverRep :: CoverObject -> Int -> CosectionRepresentative CoverObject Int
coverRep objectValue value =
  CosectionRepresentative
    { cosectionRepObject = objectValue,
      cosectionRepValue = value
    }

expectedClass0 :: Set.Set (CosectionRepresentative CoverObject Int)
expectedClass0 =
  Set.fromList
    [ coverRep CoverRoot 100,
      coverRep CoverLeft 10,
      coverRep CoverRight 20,
      coverRep CoverOverlap 0
    ]

expectedClass1 :: Set.Set (CosectionRepresentative CoverObject Int)
expectedClass1 =
  Set.fromList
    [ coverRep CoverRoot 101,
      coverRep CoverLeft 11,
      coverRep CoverRight 21,
      coverRep CoverOverlap 1
    ]
