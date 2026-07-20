{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Moonlight.Sheaf.Core.Section.RepairSpec
  ( tests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Sheaf.Section.Certified
  ( SectionCertification (..),
    certifySectionCompatibility,
  )
import Moonlight.Sheaf.Kernel.Basis
  ( SheafBasis,
    mkSheafBasis,
  )
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
    withPreparedSheafModel,
  )
import Moonlight.Sheaf.Section.Morphism
  ( RestrictionKind (..),
    RestrictionParts (..),
    RestrictionPresentation,
    unitIncidenceRestriction,
  )
import Moonlight.Sheaf.Kernel.Basis (basisCells)
import Moonlight.Sheaf.Section.ObjectIndex
  ( SheafModelVersion (..),
    mkObjectIndex,
  )
import Moonlight.Sheaf.Section.Repair
  ( PresheafAssignment (..),
    RepairDiagnostics (..),
    RepairObstruction (..),
    RepairPartialSectionResult (..),
    RepairStatus (..),
    repairDiagnosticsAreEmpty,
    repairPresheafAssignment,
  )
import Moonlight.Sheaf.Section.Stalk (MergeObstruction (..), RepairInput (..), StalkAlgebra (..), StalkRestrictionKernel (..))
import Moonlight.Sheaf.Section.Store.State
import Moonlight.Sheaf.TestFixture.Mini
  ( MiniCell (..),
    MiniStalk (..),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "repair"
    [ testCase "repair preserves direct assignments" testRepairPreservesDirectAssignments,
      testCase "repair fills stalks from incoming restrictions" testRepairFillsFromIncomingRestrictions,
      testCase "repair obstructs incompatible merges" testRepairObstructsDisagreement,
      testCase "repair calls restriction repair before aggregate repair" testRepairUsesRestrictionRepair,
      testCase "repair falls back to aggregate repair when restriction repair fails" testRepairFallsBackToAggregateRepair,
      testCase "settled sections are repair fixpoints" testRepairFixpointOnSettledSection,
      testCase "residual diagnostics agree with total-section certification" testResidualAgreesWithCertification,
      testCase "self-loop restrictions never displace direct assignments" testSelfLoopKeepsDirectAssignment
    ]

data MiniRestrictionSpec = MiniRestrictionSpec
  { mrsKind :: !RestrictionKind,
    mrsSource :: !MiniCell,
    mrsTarget :: !MiniCell,
    mrsScale :: !Double
  }
  deriving stock (Eq, Show)

fullMiniBasis :: SheafBasis MiniCell
fullMiniBasis =
  mkSheafBasis [Ghost, Cell0, Cell1]

sectionRestrictionSpecs :: [MiniRestrictionSpec]
sectionRestrictionSpecs =
  [ MiniRestrictionSpec
      { mrsKind = unitIncidenceRestriction,
        mrsSource = Ghost,
        mrsTarget = Cell0,
        mrsScale = 3.0
      },
    MiniRestrictionSpec
      { mrsKind = unitIncidenceRestriction,
        mrsSource = Cell0,
        mrsTarget = Cell1,
        mrsScale = 2.0
      },
    MiniRestrictionSpec
      { mrsKind = PortalRestriction,
        mrsSource = Ghost,
        mrsTarget = Cell1,
        mrsScale = 5.0
      }
  ]

testRepairPreservesDirectAssignments :: Assertion
testRepairPreservesDirectAssignments =
  withSectionModel $ \model -> do
    assignment <- sectionAssignment model (Map.fromList [(Cell0, MiniStalk 2.0)])
    case repairPresheafAssignment assignment of
      Left obstruction ->
        assertFailure ("expected repair success, received " <> show obstruction)
      Right repairResult -> do
        let repairedSection = repairedPartialSection repairResult
            diagnostics = repairPartialDiagnostics repairResult
        assertEqual
          "repaired entries"
          (Map.fromList [(Cell0, MiniStalk 2.0), (Cell1, MiniStalk 4.0)])
          (partialSectionEntries repairedSection)
        assertEqual "repair settled" RepairSettled (repairPartialStatus repairResult)
        assertBool "diagnostics are empty" (repairDiagnosticsAreEmpty diagnostics)

testRepairFillsFromIncomingRestrictions :: Assertion
testRepairFillsFromIncomingRestrictions =
  withSectionModel $ \model -> do
    assignment <- sectionAssignment model (Map.fromList [(Ghost, MiniStalk 1.0)])
    case repairPresheafAssignment assignment of
      Left obstruction ->
        assertFailure ("expected repair success, received " <> show obstruction)
      Right repairResult -> do
        let repairedSection = repairedPartialSection repairResult
            diagnostics = repairPartialDiagnostics repairResult
        assertEqual
          "repaired entries"
          (Map.fromList [(Ghost, MiniStalk 1.0), (Cell0, MiniStalk 3.0), (Cell1, MiniStalk 5.0)])
          (partialSectionEntries repairedSection)
        assertEqual "repair reports residual cross-restriction mismatch" RepairResidual (repairPartialStatus repairResult)
        assertBool "diagnostics record residual mismatch" (not (repairDiagnosticsAreEmpty diagnostics))

testRepairObstructsDisagreement :: Assertion
testRepairObstructsDisagreement =
  withSectionModel $ \model -> do
    assignment <-
      sectionAssignment
        model
        ( Map.fromList
            [ (Ghost, MiniStalk 1.0),
              (Cell0, MiniStalk 4.0)
            ]
        )
    case repairPresheafAssignment assignment of
      Left (RepairDomainObstruction Cell0 ()) ->
        pure ()
      Left obstruction ->
        assertFailure ("expected Cell0 repair obstruction, received " <> show obstruction)
      Right _ ->
        assertFailure "expected incompatible repair to obstruct"

testRepairUsesRestrictionRepair :: Assertion
testRepairUsesRestrictionRepair =
  withSimpleSectionModel $ \model -> do
    assignment <-
      sectionAssignmentWith
        targetedRestrictionRepairAlgebra
        model
        ( Map.fromList
            [ (Ghost, MiniStalk 1.0),
              (Cell0, MiniStalk 4.0)
            ]
        )
    case repairPresheafAssignment assignment of
      Left obstruction ->
        assertFailure ("expected targeted repair success, received " <> show obstruction)
      Right repairResult -> do
        let repairedSection = repairedPartialSection repairResult
            diagnostics = repairPartialDiagnostics repairResult
        assertEqual
          "repaired entries"
          (Map.fromList [(Ghost, MiniStalk 1.0), (Cell0, MiniStalk 3.0)])
          (partialSectionEntries repairedSection)
        assertEqual "restriction repair settled" RepairSettled (repairPartialStatus repairResult)
        assertBool "diagnostics record original mismatch" (not (repairDiagnosticsAreEmpty diagnostics))

testRepairFallsBackToAggregateRepair :: Assertion
testRepairFallsBackToAggregateRepair =
  withSimpleSectionModel $ \model -> do
    assignment <-
      sectionAssignmentWith
        aggregateFallbackRepairAlgebra
        model
        ( Map.fromList
            [ (Ghost, MiniStalk 1.0),
              (Cell0, MiniStalk 4.0)
            ]
        )
    case repairPresheafAssignment assignment of
      Left obstruction ->
        assertFailure ("expected aggregate fallback repair success, received " <> show obstruction)
      Right repairResult -> do
        let repairedSection = repairedPartialSection repairResult
        assertEqual
          "repaired entries"
          (Map.fromList [(Ghost, MiniStalk 1.0), (Cell0, MiniStalk 99.0)])
          (partialSectionEntries repairedSection)
        assertEqual "bad aggregate repair reports residual" RepairResidual (repairPartialStatus repairResult)

testRepairFixpointOnSettledSection :: Assertion
testRepairFixpointOnSettledSection =
  withSectionModel $ \model -> do
    let settledEntries =
          Map.fromList [(Ghost, MiniStalk 0.0), (Cell0, MiniStalk 0.0), (Cell1, MiniStalk 0.0)]
    assignment <- sectionAssignment model settledEntries
    case repairPresheafAssignment assignment of
      Left obstruction ->
        assertFailure ("expected settled repair success, received " <> show obstruction)
      Right repairResult -> do
        assertEqual
          "settled section is a repair fixpoint"
          settledEntries
          (partialSectionEntries (repairedPartialSection repairResult))
        assertEqual "settled status" RepairSettled (repairPartialStatus repairResult)
        assertBool "settled diagnostics are empty" (repairDiagnosticsAreEmpty (repairPartialDiagnostics repairResult))
        rerun <- sectionAssignment model (partialSectionEntries (repairedPartialSection repairResult))
        case repairPresheafAssignment rerun of
          Left obstruction ->
            assertFailure ("expected idempotent repair success, received " <> show obstruction)
          Right secondResult ->
            assertEqual
              "repair is idempotent on its settled output"
              (partialSectionEntries (repairedPartialSection repairResult))
              (partialSectionEntries (repairedPartialSection secondResult))

testResidualAgreesWithCertification :: Assertion
testResidualAgreesWithCertification =
  withSectionModel $ \model -> do
    assignment <- sectionAssignment model (Map.fromList [(Ghost, MiniStalk 1.0)])
    case repairPresheafAssignment assignment of
      Left obstruction ->
        assertFailure ("expected repair success, received " <> show obstruction)
      Right repairResult -> do
        assertEqual "residual status" RepairResidual (repairPartialStatus repairResult)
        totalStore <-
          case mkTotalSectionStore model (partialSectionEntries (repairedPartialSection repairResult)) of
            Left storeError ->
              assertFailure ("expected repaired output to be a total section: " <> show storeError)
            Right store ->
              pure store
        case certifySectionCompatibility model miniRestrictionSpecAlgebra totalStore of
          Right (SectionRejected mismatches) ->
            assertEqual
              "certification rejects exactly the residual restriction targets"
              (Set.map snd (Map.keysSet (repairDiagnosticRestrictionMismatches (repairPartialDiagnostics repairResult))))
              (Map.keysSet mismatches)
          certification ->
            assertFailure ("expected semantic rejection mirroring residual diagnostics, received " <> show certification)

testSelfLoopKeepsDirectAssignment :: Assertion
testSelfLoopKeepsDirectAssignment =
  withSelfLoopSectionModel $ \model -> do
      assignment <- sectionAssignment model (Map.fromList [(Cell0, MiniStalk 2.0)])
      case repairPresheafAssignment assignment of
        Left obstruction ->
          assertFailure ("expected self-loop repair success, received " <> show obstruction)
        Right repairResult -> do
          assertEqual
            "direct assignment survives the self-loop restriction"
            (Map.fromList [(Cell0, MiniStalk 2.0)])
            (partialSectionEntries (repairedPartialSection repairResult))
          assertEqual "violated self-loop is reported as residual" RepairResidual (repairPartialStatus repairResult)

sectionAssignment ::
  SheafModel owner MiniCell MiniRestrictionSpec ->
  Map.Map MiniCell MiniStalk ->
  AssertionWithAssignment owner
sectionAssignment =
  sectionAssignmentWith miniRestrictionSpecAlgebra

sectionAssignmentWith ::
  StalkAlgebra MiniRestrictionSpec MiniStalk () () ->
  SheafModel owner MiniCell MiniRestrictionSpec ->
  Map.Map MiniCell MiniStalk ->
  AssertionWithAssignment owner
sectionAssignmentWith stalkAlgebra model entries =
  case mkPartialSectionStore model entries of
    Left storeError ->
      assertFailure ("expected partial section construction to succeed: " <> show storeError)
    Right assignment ->
      pure
        PresheafAssignment
          { paModel = model,
            paStalkAlgebra = stalkAlgebra,
            paAssignment = assignment
          }

type AssertionWithAssignment owner = IO (PresheafAssignment owner MiniCell MiniStalk () MiniRestrictionSpec ())

withSectionModel ::
  (forall owner. SheafModel owner MiniCell MiniRestrictionSpec -> Assertion) ->
  Assertion
withSectionModel useModel =
  case
      withPreparedSheafModel
        (SheafModelVersion 0)
        (mkObjectIndex (basisCells fullMiniBasis))
        presentMiniRestrictionSpec
        sectionRestrictionSpecs
        useModel
    of
      Left modelError ->
        assertFailure ("expected sheaf model construction to succeed: " <> show modelError)
      Right assertion ->
        assertion

withSimpleSectionModel ::
  (forall owner. SheafModel owner MiniCell MiniRestrictionSpec -> Assertion) ->
  Assertion
withSimpleSectionModel useModel =
  case
      withPreparedSheafModel
        (SheafModelVersion 0)
        (mkObjectIndex (basisCells simpleMiniBasis))
        presentMiniRestrictionSpec
        simpleRestrictionSpecs
        useModel
    of
      Left modelError ->
        assertFailure ("expected simple sheaf model construction to succeed: " <> show modelError)
      Right assertion ->
        assertion

presentMiniRestrictionSpec :: RestrictionPresentation MiniRestrictionSpec MiniCell MiniRestrictionSpec
presentMiniRestrictionSpec restrictionSpec =
  RestrictionParts
    { partKind = mrsKind restrictionSpec,
      partSource = mrsSource restrictionSpec,
      partTarget = mrsTarget restrictionSpec,
      partWitness = restrictionSpec
    }

simpleMiniBasis :: SheafBasis MiniCell
simpleMiniBasis =
  mkSheafBasis [Ghost, Cell0]

simpleRestrictionSpecs :: [MiniRestrictionSpec]
simpleRestrictionSpecs =
  [ MiniRestrictionSpec
      { mrsKind = unitIncidenceRestriction,
        mrsSource = Ghost,
        mrsTarget = Cell0,
        mrsScale = 3.0
      }
  ]

withSelfLoopSectionModel ::
  (forall owner. SheafModel owner MiniCell MiniRestrictionSpec -> Assertion) ->
  Assertion
withSelfLoopSectionModel useModel =
  case
      withPreparedSheafModel
        (SheafModelVersion 0)
        (mkObjectIndex (basisCells (mkSheafBasis [Cell0])))
        presentMiniRestrictionSpec
        [ MiniRestrictionSpec
            { mrsKind = unitIncidenceRestriction,
              mrsSource = Cell0,
              mrsTarget = Cell0,
              mrsScale = 7.0
            }
        ]
        useModel
    of
      Left modelError ->
        assertFailure ("expected self-loop sheaf model construction to succeed: " <> show modelError)
      Right assertion ->
        assertion

miniRestrictionSpecAlgebra :: StalkAlgebra MiniRestrictionSpec MiniStalk () ()
miniRestrictionSpecAlgebra =
  StalkAlgebra
    { saRestrictionKernel = \restrictionSpec -> StalkRestrictionMap (scaleMiniStalk (mrsScale restrictionSpec)),
      saMismatches =
        \left right ->
          ([() | left /= right]),
      saMerge =
        \left right ->
          if left == right
            then Right left
            else Left (MergeMismatchObstruction (() :| [])),
      saRepair = const (Left ()),
      saNormalize = id
    }

targetedRestrictionRepairAlgebra :: StalkAlgebra MiniRestrictionSpec MiniStalk () ()
targetedRestrictionRepairAlgebra =
  miniRestrictionSpecAlgebra
    { saRepair =
        \case
          RepairRestrictionInput _restrictionSpec restrictedValue _targetValue _mismatches ->
            Right restrictedValue
          RepairMergeInput _candidates _mismatches ->
            Left ()
    }

aggregateFallbackRepairAlgebra :: StalkAlgebra MiniRestrictionSpec MiniStalk () ()
aggregateFallbackRepairAlgebra =
  miniRestrictionSpecAlgebra
    { saRepair =
        \case
          RepairRestrictionInput {} ->
            Left ()
          RepairMergeInput {} ->
            Right (MiniStalk 99.0)
    }

scaleMiniStalk :: Double -> MiniStalk -> MiniStalk
scaleMiniStalk factor (MiniStalk value) =
  MiniStalk (factor * value)
