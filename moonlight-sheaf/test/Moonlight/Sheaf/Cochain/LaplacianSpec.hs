{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Cochain.LaplacianSpec
  ( tests,
  )
where

import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Sheaf.Cochain.Laplacian
  ( buildSemiringLaplacian,
    buildSemiringLaplacianFromPlan,
    buildTarskiLaplacian,
    buildTarskiLaplacianFromPlan,
    laplacianSupportCells,
    prepareRestrictionGraphPlan,
    RestrictionGraphPlan (..),
    RestrictionGraphPlanKind (..),
  )
import Moonlight.Sheaf.Section.Model
  ( SheafModel,
    SheafModelVersion (..),
    prepareSheafModel,
  )
import Moonlight.Sheaf.Kernel.Basis (basisCells)
import Moonlight.Sheaf.Section.ObjectIndex
  ( mkObjectIndex,
  )
import Moonlight.Sheaf.Section.Morphism
  ( RestrictionParts (..),
    unitIncidenceRestriction,
  )
import Moonlight.Sheaf.Section.Store.Descent.Execute
import Moonlight.Sheaf.Section.Store.Descent.Prepare
import Moonlight.Sheaf.Section.Store.State
import Moonlight.Sheaf.Section.Store.Types
import Moonlight.Sheaf.TestFixture.Mini
  ( MiniCell (..),
    MiniStalk (..),
    miniBasis,
  )
import Moonlight.Sheaf.TestFixture.Assertions (assertRight)
import Test.Tasty (TestTree, testGroup)
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
    "laplacian"
    [ testCase "buildTarskiLaplacianFromPlan uses the model-derived plan" testBuildTarskiLaplacianFromPlan,
      testCase "buildSemiringLaplacianFromPlan uses the model-derived plan" testBuildSemiringLaplianFromPlan,
    testCase "projection-driven alignment delta applies without runtime fallback" testProjectionAlignmentDeltaApplies,
      testCase "projection plan rejects irrelevant frontier cells" testProjectionPlanSkipsIrrelevantFrontier
    ]

type MiniMorphism :: Type
data MiniMorphism = MiniMorphism
  { mmSource :: MiniCell,
    mmTarget :: MiniCell
  }
  deriving stock (Eq, Show)

type MiniResolutionReport :: Type
data MiniResolutionReport = MiniResolutionReport
  { mrrChangedCells :: !(Set MiniCell),
    mrrSettled :: !Bool
  }
  deriving stock (Eq, Show)

lineModel ::
  Either
    String
    (SheafModel MiniCell MiniMorphism)
lineModel =
  case
    prepareSheafModel
      (SheafModelVersion 1)
      (mkObjectIndex (basisCells miniBasis))
      ( \morphism ->
          RestrictionParts
            { partKind = unitIncidenceRestriction,
              partSource = mmSource morphism,
              partTarget = mmTarget morphism,
              partWitness = morphism
            }
      )
      [MiniMorphism Cell0 Cell1]
    of
    Left modelError -> Left (show modelError)
    Right model -> Right model

testBuildTarskiLaplacianFromPlan :: Assertion
testBuildTarskiLaplacianFromPlan = do
  model <-
    assertRight
      "line sheaf model"
      lineModel
  plan <-
    assertRight
      "tarski restriction graph plan"
      (prepareRestrictionGraphPlan TarskiRestrictionGraphPlan model)
  planLaplacian <-
    assertRight
      "planned tarski laplacian"
      (buildTarskiLaplacianFromPlan plan)
  derivedLaplacian <-
    assertRight
      "derived tarski laplacian"
      (buildTarskiLaplacian model)
  let plannedSupport =
        laplacianSupportCells planLaplacian
      derivedSupport =
        laplacianSupportCells derivedLaplacian
  assertEqual
    "plan kind"
    TarskiRestrictionGraphPlan
    (rgpKind plan)
  assertEqual
    "planned tarski support should only include model cells"
    (Set.fromList [Cell0, Cell1])
    plannedSupport
  assertEqual
    "derived tarski support should only include registry cells"
    (Set.fromList [Cell0, Cell1])
    derivedSupport

testBuildSemiringLaplianFromPlan :: Assertion
testBuildSemiringLaplianFromPlan = do
  model <-
    assertRight
      "line sheaf model"
      lineModel
  plan <-
    assertRight
      "semiring restriction graph plan"
      (prepareRestrictionGraphPlan SemiringRestrictionGraphPlan model)
  planLaplacian <-
    assertRight
      "planned semiring laplacian"
      (buildSemiringLaplacianFromPlan plan)
  derivedLaplacian <-
    assertRight
      "derived semiring laplacian"
      (buildSemiringLaplacian model)
  let plannedSupport =
        laplacianSupportCells planLaplacian
      derivedSupport =
        laplacianSupportCells derivedLaplacian
  assertEqual
    "plan kind"
    SemiringRestrictionGraphPlan
    (rgpKind plan)
  assertEqual
    "planned semiring support should only include model cells"
    (Set.fromList [Cell0, Cell1])
    plannedSupport
  assertEqual
    "derived semiring support should only include registry cells"
    (Set.fromList [Cell0, Cell1])
    derivedSupport

testProjectionAlignmentDeltaApplies :: Assertion
testProjectionAlignmentDeltaApplies = do
  model <-
    assertRight
      "line sheaf model"
      lineModel
  sectionValue <-
    assertRight
      "initial section"
      (initialSection model)
  case applyAlignmentDelta model (Set.singleton Cell0) sectionValue of
    Left failure ->
      assertFailure ("expected projection alignment to succeed, received " <> failure)
    Right (finalSection, report) ->
      do
        assertEqual
          "resolution should align the target cell with the source cell"
          (Right (MiniStalk 1.0))
          (totalStalkAt model Cell1 finalSection)
        assertBool
          "expected the projection alignment to settle"
          (mrrSettled report)
        assertEqual
          "expected changed target to be reported"
          (Set.singleton Cell1)
          (mrrChangedCells report)

testProjectionPlanSkipsIrrelevantFrontier :: Assertion
testProjectionPlanSkipsIrrelevantFrontier = do
  assertBool
    "irrelevant frontier should not match the plan"
    (Set.disjoint (Set.singleton Cell1) (Set.singleton Cell0))
  assertBool
    "source frontier should match the plan"
    (not (Set.disjoint (Set.singleton Cell0) (Set.singleton Cell0)))

initialSection ::
  SheafModel MiniCell MiniMorphism ->
  Either
    (SectionConstructionError MiniCell)
    (TotalSectionStore MiniCell MiniStalk)
initialSection model =
  mkTotalSectionStore
    model
    (Map.fromList [(Cell0, MiniStalk 1.0), (Cell1, MiniStalk 0.0)])

applyAlignmentDelta ::
  SheafModel MiniCell MiniMorphism ->
  Set MiniCell ->
  TotalSectionStore MiniCell MiniStalk ->
  Either String (TotalSectionStore MiniCell MiniStalk, MiniResolutionReport)
applyAlignmentDelta model dirtyCells sectionValue = do
  deltaValue <- alignmentDelta model dirtyCells sectionValue
  (resolvedSection, changedCells) <- alignmentApply model deltaValue (sectionValue, Set.empty)
  Right
    ( resolvedSection,
      MiniResolutionReport
        { mrrChangedCells = changedCells,
          mrrSettled = True
        }
    )

alignmentDelta ::
  SheafModel MiniCell MiniMorphism ->
  Set MiniCell ->
  TotalSectionStore MiniCell MiniStalk ->
  Either String (Maybe MiniStalk)
alignmentDelta model dirtyCells sectionValue =
  if not (Set.disjoint dirtyCells (Set.singleton Cell0))
    then
      case totalStalkAt model Cell0 sectionValue of
        Left lookupError ->
          Left (show lookupError)
        Right sourceStalk ->
          Right (Just sourceStalk)
    else Right Nothing

alignmentApply ::
  SheafModel MiniCell MiniMorphism ->
  Maybe MiniStalk ->
  (TotalSectionStore MiniCell MiniStalk, Set MiniCell) ->
  Either String (TotalSectionStore MiniCell MiniStalk, Set MiniCell)
alignmentApply model maybeSourceStalk (sectionValue, changedCells) =
  case maybeSourceStalk of
    Nothing ->
      Right (sectionValue, changedCells)
    Just sourceStalk ->
      fmap
        (\nextSection -> (nextSection, Set.singleton Cell1))
        (firstShow (updateStalkAtChecked model Cell1 (const sourceStalk) sectionValue))

firstShow :: Show left => Either left right -> Either String right
firstShow =
  either (Left . show) Right
