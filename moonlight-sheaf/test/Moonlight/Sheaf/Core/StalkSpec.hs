module Moonlight.Sheaf.Core.StalkSpec
  ( tests,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Moonlight.Sheaf.Section.Stalk
  ( MergeObstruction (..),
    RepairInput (..),
    StalkAlgebra,
    mergeStalks,
    normalizeStalk,
    restrictStalk,
    saRepair,
    stalkApproxEq,
    stalkMismatches,
  )
import Moonlight.Sheaf.Section.Stalk.Discrete
  ( DiscreteMismatch (..),
    DiscreteRepairObstruction (..),
    discreteStalkAlgebra,
  )
import Moonlight.Sheaf.Section.Stalk.Geometric
  ( GeometricMismatch (..),
    GeometricRepairObstruction (..),
    GeometricRestriction (..),
    GeometricStalk (..),
    geometricStalkAlgebra,
  )
import Moonlight.Sheaf.Section.Stalk.Groupoid
  ( interfaceStalkAutomorphismCounts,
    maxInterfaceStalkAutomorphismCount,
    mkInterfaceStalkGroupoid,
    orbitsWithSize,
  )
import Moonlight.Sheaf.TestFixture.Branch
  ( BranchContext (..),
    BranchMismatch (..),
    branchLeftCompatibleStalk,
    branchRightCompatibleStalk,
    branchRightIncompatibleStalk,
    branchStalk,
    branchStalkAlgebra,
    branchStalkEntries,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "stalk"
    [ testCase "stalkMismatches reports exact overlap disagreements" testStalkMismatchesReportOverlap,
      testCase "mergeStalks preserves compatible branch data without inventing overlap values" testMergeStalksPreservesCompatibleUnion,
      testCase "stalkApproxEq rejects missing coordinates as typed mismatch" testMissingCoordinateMismatch,
      testCase "discreteStalkAlgebra is the lawful boring base case" testDiscreteStalkAlgebra,
      testCase "geometricStalkAlgebra delegates repair to component algebras" testGeometricStalkRepair,
      testCase "interface stalk orbits are mutual reachability components" testInterfaceStalkOrbitComponents
    ]

testStalkMismatchesReportOverlap :: Assertion
testStalkMismatchesReportOverlap =
  stalkMismatches branchStalkAlgebra branchLeftCompatibleStalk branchRightIncompatibleStalk
    @?=
      [ BranchMissingCoordinate BranchLeft (Just 10) Nothing,
        BranchMissingCoordinate BranchRight Nothing (Just 20),
        BranchCoordinateConflict BranchApex 7 8
      ]

testMergeStalksPreservesCompatibleUnion :: Assertion
testMergeStalksPreservesCompatibleUnion =
  fmap branchStalkEntries (mergeStalks branchStalkAlgebra branchLeftCompatibleStalk branchRightCompatibleStalk)
    @?=
      Right
        ( Map.fromList
            [ (BranchLeft, 10),
              (BranchRight, 20),
              (BranchApex, 7)
            ]
        )

testMissingCoordinateMismatch :: Assertion
testMissingCoordinateMismatch =
  stalkApproxEq
    branchStalkAlgebra
    (branchStalk [(BranchApex, 7)])
    (branchStalk [(BranchLeft, 10), (BranchApex, 7)])
    @?= False

testDiscreteStalkAlgebra :: Assertion
testDiscreteStalkAlgebra = do
  let algebra =
        discreteStalkAlgebra ::
          StalkAlgebra () Int (DiscreteMismatch Int) (DiscreteRepairObstruction Int)
  restrictStalk algebra () 7 @?= 7
  stalkMismatches algebra 7 7 @?= []
  stalkMismatches algebra 7 9 @?= [DiscreteMismatch 7 9]
  mergeStalks algebra 7 7 @?= Right 7
  mergeStalks algebra 7 9
    @?= Left (MergeMismatchObstruction (DiscreteMismatch 7 9 :| []))
  saRepair algebra (RepairMergeInput (7 :| [7]) (DiscreteMismatch 7 9 :| []))
    @?= Right 7
  saRepair algebra (RepairMergeInput (7 :| [9]) (DiscreteMismatch 7 9 :| []))
    @?= Left (DiscreteMergeConflict (7 :| [9]))
  saRepair algebra (RepairRestrictionInput () 7 7 (DiscreteMismatch 7 9 :| []))
    @?= Right 7
  saRepair algebra (RepairRestrictionInput () 7 9 (DiscreteMismatch 7 9 :| []))
    @?= Left (DiscreteRestrictionConflict 7 9)
  normalizeStalk algebra (normalizeStalk algebra 7) @?= normalizeStalk algebra 7

testGeometricStalkRepair :: Assertion
testGeometricStalkRepair = do
  let algebra =
        geometricStalkAlgebra discreteStalkAlgebra discreteStalkAlgebra ::
          StalkAlgebra
            (GeometricRestriction () ())
            (GeometricStalk Int Int)
            (GeometricMismatch (DiscreteMismatch Int) (DiscreteMismatch Int))
            (GeometricRepairObstruction (DiscreteRepairObstruction Int) (DiscreteRepairObstruction Int))
  saRepair
    algebra
    ( RepairMergeInput
        (GeometricStalk 7 3 :| [GeometricStalk 7 3])
        (GeometricChartMismatch (DiscreteMismatch 7 9) :| [])
    )
    @?= Right (GeometricStalk 7 3)
  saRepair
    algebra
    ( RepairRestrictionInput
        (GeometricRestriction () ())
        (GeometricStalk 1 3)
        (GeometricStalk 2 3)
        (GeometricChartMismatch (DiscreteMismatch 1 2) :| [])
    )
    @?= Left (GeometricChartRepairObstruction (DiscreteRestrictionConflict 1 2))

testInterfaceStalkOrbitComponents :: Assertion
testInterfaceStalkOrbitComponents = do
  let groupoidValue =
        mkInterfaceStalkGroupoid
          (IntSet.fromList [0, 1, 2, 3])
          (IntMap.fromList [(1, 3)])
          ( IntMap.fromList
              [ (0, [(1, 1)]),
                (1, [(0, 1), (2, 1)]),
                (2, [(3, 0)]),
                (3, [(2, 1)])
              ]
          )

  orbitsWithSize groupoidValue @?= [(0, 2), (2, 1), (3, 1)]
  interfaceStalkAutomorphismCounts groupoidValue
    @?= IntMap.fromList [(0, 1), (1, 3), (2, 1), (3, 1)]
  maxInterfaceStalkAutomorphismCount groupoidValue @?= 3
