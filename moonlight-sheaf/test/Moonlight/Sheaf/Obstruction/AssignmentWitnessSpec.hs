module Moonlight.Sheaf.Obstruction.AssignmentWitnessSpec
  ( tests,
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Sheaf.Descent.Assignment qualified as AssignmentDescent
import Moonlight.Sheaf.Descent.Core
  ( DescentOutcome (..),
    DescentReport (..),
  )
import Moonlight.Sheaf.Descent.Kernel
  ( CoverSearchBudget (..),
    CoverSearchCost (..),
    CoverSearchRefusal (..),
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Algebra
  ( assignmentWitnessCoverCohomologyReport,
    nerveFromSupport,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Types
  ( AssignmentWitnessBasis (..),
    AnalysisCompleteness (..),
    AnalysisTruncationCause (..),
    C1LocalConflict (..),
    CoverCohomologyReport (..),
    witnessCoefficients,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    testCase,
  )

data BranchContext
  = BranchBase
  | BranchLeft
  | BranchRight
  | BranchApex
  deriving stock (Eq, Ord, Show)

type TestCoordinate :: Type
data TestCoordinate
  = CoordA
  | CoordB
  deriving stock (Eq, Ord, Show)

type BucketValue :: Type
newtype BucketValue = BucketValue Int
  deriving stock (Eq, Ord, Show)

type CompletenessObstruction =
  AssignmentDescent.AssignmentDescentObstruction BranchContext TestCoordinate BucketValue () ()

type CompletenessReport =
  DescentReport BranchContext (CoverSearchRefusal BranchContext) CompletenessObstruction

tests :: TestTree
tests =
  testGroup
    "assignment-witness"
    [ testCase "assignment witness preserves coordinate identity" testAssignmentWitnessPreservesCoordinateIdentity,
      testCase "analysis completeness follows descent refusals" testAnalysisCompletenessFollowsDescentRefusals
    ]

testAssignmentWitnessPreservesCoordinateIdentity :: Assertion
testAssignmentWitnessPreservesCoordinateIdentity =
  let sectionAt contextValue =
        case contextValue of
          BranchBase -> Map.fromList [(CoordA, BucketValue 0), (CoordB, BucketValue 0)]
          BranchLeft -> Map.fromList [(CoordA, BucketValue 1), (CoordB, BucketValue 2)]
          BranchRight -> Map.fromList [(CoordA, BucketValue 2), (CoordB, BucketValue 1)]
          BranchApex -> Map.fromList [(CoordA, BucketValue 2), (CoordB, BucketValue 2)]
      kernel :: AssignmentDescent.DescentKernel BranchContext (Map TestCoordinate BucketValue) TestCoordinate BucketValue () ()
      kernel =
        AssignmentDescent.DescentKernel
          { AssignmentDescent.dkCoverOf =
              \case
                BranchBase -> [BranchLeft, BranchRight]
                _ -> [],
            AssignmentDescent.dkMaterializedContexts = [BranchBase, BranchLeft, BranchRight],
            AssignmentDescent.dkSectionAt = sectionAt,
            AssignmentDescent.dkAssignmentOf = id,
            AssignmentDescent.dkAdmissibility = AssignmentDescent.trivialAdmissibility
          }
      report = AssignmentDescent.fullDescentCheck (CoverSearchBudget Nothing) kernel
      nerve =
        nerveFromSupport
          [BranchBase, BranchLeft, BranchRight]
          supportAt
      cohomologyReport = assignmentWitnessCoverCohomologyReport nerve report
      localWitness =
        case corLocalC1Conflicts cohomologyReport of
          conflictValue : _ -> c1cWitness conflictValue
          [] -> mempty
      witnessCoordinates =
        Set.fromList (fmap awbCoordinate (Map.keys (witnessCoefficients localWitness)))
   in do
        assertBool
          "assignment descent should report an obstruction"
          (not (AssignmentDescent.drSatisfied report))
        assertEqual
          "assignment witness should retain both coordinates instead of colliding them into one bucket"
          (Set.fromList [CoordA, CoordB])
          witnessCoordinates
  where
    supportAt contextValue =
      case contextValue of
        BranchBase -> Set.fromList [0 :: Int, 1]
        BranchLeft -> Set.fromList [0 :: Int]
        BranchRight -> Set.fromList [1 :: Int]
        BranchApex -> Set.fromList [0 :: Int, 1]

testAnalysisCompletenessFollowsDescentRefusals :: Assertion
testAnalysisCompletenessFollowsDescentRefusals =
  let budget =
        CoverSearchBudget (Just 1)
      searchCost =
        CoverSearchCost
          { cscCoordinates = [BranchLeft],
            cscDomainSizes = Map.singleton BranchLeft 2,
            cscAssignmentUpperBound = 2
          }
      expectedCause =
        TruncatedByDescentSearchRefusal budget 2
      refusal =
        CoverSearchBudgetExceeded budget searchCost
      truncatedReport =
        completenessReport
          DescentUndecided
          False
          [refusal]
      completeReport =
        completenessReport
          DescentSatisfied
          True
          []
      nerve =
        nerveFromSupport
          [BranchBase, BranchLeft]
          (const (Set.empty :: Set.Set Int))
   in do
        assertEqual
          "refused descent search should mark cohomology analysis truncated"
          (AnalysisTruncated (expectedCause :| []))
          (corCompleteness (assignmentWitnessCoverCohomologyReport nerve truncatedReport))
        assertEqual
          "refusal-free descent search should mark cohomology analysis complete"
          AnalysisComplete
          (corCompleteness (assignmentWitnessCoverCohomologyReport nerve completeReport))

completenessReport ::
  DescentOutcome ->
  Bool ->
  [CoverSearchRefusal BranchContext] ->
  CompletenessReport
completenessReport outcomeValue satisfiedValue refusals =
  DescentReport
    { drContextCount = 2,
      drObstructionCount = 0,
      drOutcome = outcomeValue,
      drSatisfied = satisfiedValue,
      drRefusals = refusals,
      drObstructions = []
    }
