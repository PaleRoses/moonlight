module Moonlight.Sheaf.Core.StalkLawSpec
  ( tests,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Sheaf.TestFixture.Branch
  ( BranchContext (..),
    BranchMismatch (..),
    BranchStalk,
    branchStalk,
    branchStalkAlgebra,
    branchStalkEntries,
  )
import Moonlight.Sheaf.TestFixture.SheafClassLaws
  ( StalkGluingSample (..),
    StalkMergeLawsFixture (..),
    stalkMergeLawTests,
  )
import Moonlight.Sheaf.Section.Stalk
  ( MergeObstruction (..),
    StalkAlgebra,
    mergeStalks,
    stalkMismatches,
  )
import Moonlight.Sheaf.Section.Stalk.Discrete
  ( DiscreteMismatch (..),
    DiscreteRepairObstruction,
    discreteStalkAlgebra,
  )
import Moonlight.Sheaf.Section.Stalk.Geometric
  ( GeometricMismatch (..),
    GeometricRepairObstruction,
    GeometricRestriction,
    GeometricStalk (..),
    geometricStalkAlgebra,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    arbitrary,
    elements,
    forAll,
    listOf,
    sublistOf,
    testProperty,
    (.&&.),
    (===),
  )

tests :: TestTree
tests =
  testGroup
    "stalk-laws"
    [ stalkMergeLawTests discreteMergeLawsFixture,
      testProperty "discrete mismatches witness exactly the discrete merge verdict" propDiscreteMergeCoherent,
      stalkMergeLawTests branchMergeLawsFixture,
      testProperty "branch merge obstructions are exactly the overlap conflicts" propBranchMergeObstructionsAreOverlapConflicts,
      stalkMergeLawTests geometricMergeLawsFixture,
      testProperty "geometric merge is the product of component merges" propGeometricMergeComponentwise,
      testProperty "geometric mismatches decompose componentwise without cross-talk" propGeometricMismatchesDecompose
    ]

discreteIntAlgebra :: StalkAlgebra () Int (DiscreteMismatch Int) (DiscreteRepairObstruction Int)
discreteIntAlgebra =
  discreteStalkAlgebra

geometricIntAlgebra ::
  StalkAlgebra
    (GeometricRestriction () ())
    (GeometricStalk Int Int)
    (GeometricMismatch (DiscreteMismatch Int) (DiscreteMismatch Int))
    (GeometricRepairObstruction (DiscreteRepairObstruction Int) (DiscreteRepairObstruction Int))
geometricIntAlgebra =
  geometricStalkAlgebra discreteStalkAlgebra discreteStalkAlgebra

discreteMergeLawsFixture ::
  StalkMergeLawsFixture
    ()
    Int
    (DiscreteMismatch Int)
    (DiscreteRepairObstruction Int)
discreteMergeLawsFixture =
  StalkMergeLawsFixture
    { smlfName = "discrete merge laws",
      smlfStalkAlgebra = discreteIntAlgebra,
      smlfGenStalk = arbitrary,
      smlfGenCompatiblePair = (\value -> (value, value)) <$> arbitrary,
      smlfGenGluingSample = (\value -> StalkGluingSample value value value value) <$> arbitrary,
      smlfLeq = (==)
    }

branchMergeLawsFixture ::
  StalkMergeLawsFixture
    ()
    BranchStalk
    BranchMismatch
    ()
branchMergeLawsFixture =
  StalkMergeLawsFixture
    { smlfName = "branch merge laws",
      smlfStalkAlgebra = branchStalkAlgebra,
      smlfGenStalk = branchStalk <$> genBranchEntries,
      smlfGenCompatiblePair = genCompatibleBranchPair,
      smlfGenGluingSample = genBranchGluingSample,
      smlfLeq = branchStalkLeq
    }

geometricMergeLawsFixture ::
  StalkMergeLawsFixture
    (GeometricRestriction () ())
    (GeometricStalk Int Int)
    (GeometricMismatch (DiscreteMismatch Int) (DiscreteMismatch Int))
    (GeometricRepairObstruction (DiscreteRepairObstruction Int) (DiscreteRepairObstruction Int))
geometricMergeLawsFixture =
  StalkMergeLawsFixture
    { smlfName = "geometric merge laws",
      smlfStalkAlgebra = geometricIntAlgebra,
      smlfGenStalk = genGeometricStalk,
      smlfGenCompatiblePair = (\value -> (value, value)) <$> genGeometricStalk,
      smlfGenGluingSample = (\value -> StalkGluingSample value value value value) <$> genGeometricStalk,
      smlfLeq = (==)
    }

mergeValue :: Either obstruction stalk -> Maybe stalk
mergeValue =
  either (const Nothing) Just

propDiscreteMergeCoherent :: Int -> Int -> Property
propDiscreteMergeCoherent leftValue rightValue =
  null (stalkMismatches discreteIntAlgebra leftValue rightValue)
    === either (const False) (const True) (mergeStalks discreteIntAlgebra leftValue rightValue)

genBranchEntries :: Gen [(BranchContext, Int)]
genBranchEntries =
  listOf ((,) <$> elements [minBound .. maxBound] <*> arbitrary)

genGlobalSection :: Gen (Map BranchContext Int)
genGlobalSection =
  Map.fromList <$> traverse (\context -> (,) context <$> arbitrary) [minBound .. maxBound]

restrictedEntries :: Map BranchContext Int -> [BranchContext] -> [(BranchContext, Int)]
restrictedEntries globalSection domain =
  Map.toList (Map.restrictKeys globalSection (Set.fromList domain))

genCompatibleBranchPair :: Gen (BranchStalk, BranchStalk)
genCompatibleBranchPair =
  compatibleBranchPairFromDomains
    <$> genGlobalSection
    <*> sublistOf [minBound .. maxBound]
    <*> sublistOf [minBound .. maxBound]

compatibleBranchPairFromDomains ::
  Map BranchContext Int ->
  [BranchContext] ->
  [BranchContext] ->
  (BranchStalk, BranchStalk)
compatibleBranchPairFromDomains globalSection leftDomain rightDomain =
  ( branchStalk (restrictedEntries globalSection leftDomain),
    branchStalk (restrictedEntries globalSection rightDomain)
  )

genBranchGluingSample :: Gen (StalkGluingSample BranchStalk)
genBranchGluingSample =
  branchGluingSample
    <$> genGlobalSection
    <*> sublistOf [minBound .. maxBound]
    <*> sublistOf [minBound .. maxBound]
    <*> sublistOf [minBound .. maxBound]

branchGluingSample ::
  Map BranchContext Int ->
  [BranchContext] ->
  [BranchContext] ->
  [BranchContext] ->
  StalkGluingSample BranchStalk
branchGluingSample globalSection firstDomain secondDomain thirdDomain =
  StalkGluingSample
    { sgsFirstStalk = branchStalk (restrictedEntries globalSection firstDomain),
      sgsSecondStalk = branchStalk (restrictedEntries globalSection secondDomain),
      sgsThirdStalk = branchStalk (restrictedEntries globalSection thirdDomain),
      sgsExpectedGluedStalk =
        branchStalk
          ( Map.toList
              (Map.restrictKeys globalSection (Set.fromList (firstDomain <> secondDomain <> thirdDomain)))
          )
    }

branchStalkLeq :: BranchStalk -> BranchStalk -> Bool
branchStalkLeq leftStalk rightStalk =
  Map.isSubmapOf (branchStalkEntries leftStalk) (branchStalkEntries rightStalk)

propBranchMergeObstructionsAreOverlapConflicts :: Property
propBranchMergeObstructionsAreOverlapConflicts =
  forAll genBranchEntries $ \leftEntries ->
    forAll genBranchEntries $ \rightEntries ->
      let leftStalk = branchStalk leftEntries
          rightStalk = branchStalk rightEntries
          overlapConflicts =
            [ mismatch
              | mismatch@BranchCoordinateConflict {} <-
                  stalkMismatches branchStalkAlgebra leftStalk rightStalk
            ]
       in case mergeStalks branchStalkAlgebra leftStalk rightStalk of
            Left (MergeMismatchObstruction obstructions) ->
              NonEmpty.toList obstructions === overlapConflicts
            Right merged ->
              overlapConflicts === []
                .&&. branchStalkEntries merged
                  === Map.union (branchStalkEntries leftStalk) (branchStalkEntries rightStalk)

genGeometricStalk :: Gen (GeometricStalk Int Int)
genGeometricStalk =
  GeometricStalk <$> arbitrary <*> arbitrary

propGeometricMergeComponentwise :: Int -> Int -> Int -> Int -> Property
propGeometricMergeComponentwise chartLeft metricLeft chartRight metricRight =
  mergeValue
    ( mergeStalks
        geometricIntAlgebra
        (GeometricStalk chartLeft metricLeft)
        (GeometricStalk chartRight metricRight)
    )
    === ( GeometricStalk
            <$> mergeValue (mergeStalks discreteIntAlgebra chartLeft chartRight)
            <*> mergeValue (mergeStalks discreteIntAlgebra metricLeft metricRight)
        )

propGeometricMismatchesDecompose :: Int -> Int -> Int -> Int -> Property
propGeometricMismatchesDecompose chartLeft metricLeft chartRight metricRight =
  stalkMismatches
    geometricIntAlgebra
    (GeometricStalk chartLeft metricLeft)
    (GeometricStalk chartRight metricRight)
    === fmap GeometricChartMismatch (stalkMismatches discreteIntAlgebra chartLeft chartRight)
      <> fmap GeometricMetricMismatch (stalkMismatches discreteIntAlgebra metricLeft metricRight)
