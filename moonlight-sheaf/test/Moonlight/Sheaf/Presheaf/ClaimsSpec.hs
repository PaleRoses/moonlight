module Moonlight.Sheaf.Presheaf.ClaimsSpec
  ( tests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moonlight.Sheaf.Presheaf.Core (restrictAlong)
import Moonlight.Sheaf.Section.Stalk (stalkMismatches)
import Moonlight.Sheaf.Sheaf.Gluing
  ( GluingAlgebra (..),
    GluingFailure (..),
    MatchingFamily,
    MatchingFamilyConstructionError,
    MatchingFailure (..),
    amalgamatedStalk,
    amalgamateMatchingFamilyWith,
    mkMatchingFamily,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    CoveringFamily,
    PullbackSquare (..),
    Site (..),
  )
import Moonlight.Sheaf.Site.Plan
  ( EffectiveCoverPlanFailure,
    prepareEffectiveCoverPlan,
  )
import Moonlight.Sheaf.TestFixture.Branch
  ( BranchContext (..),
    BranchMismatch (..),
    branchCompatibleAmalgamatedStalk,
    branchLeftCompatibleStalk,
    branchRightCompatibleStalk,
    branchRightIncompatibleStalk,
    branchStalkAlgebra,
  )
import Moonlight.Sheaf.TestFixture.Branch.Presheaf
  ( branchCompiledStalkAlgebra,
    branchGluingAlgebra,
  )
import Moonlight.Sheaf.TestFixture.Branch.Site
  ( branchArrow,
    branchRootCover,
    branchSite,
  )
import Moonlight.Sheaf.TestFixture.Assertions (expectJust, expectRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "semantic-claims"
    [ testCase "compatible local sections glue into a global section that restricts back" testCompatibleLocalsGlueAndRestrictBack,
      testCase "overlap disagreement blocks gluing instead of laundering a global section" testOverlapDisagreementBlocksGluing,
      testCase "stalk evidence preserves missing-coordinate and conflicting-overlap failures" testStalkEvidencePreservesFailureShape
    ]

testCompatibleLocalsGlueAndRestrictBack :: Assertion
testCompatibleLocalsGlueAndRestrictBack = do
  coverValue <- expectRight branchRootCover
  matchingFamily <-
    expectRight
      ( mkMatchingFamilyForCover branchSite
          coverValue
          (Vector.fromList [branchLeftCompatibleStalk, branchRightCompatibleStalk])
      )
  leftArrow <- expectJust (branchArrow BranchLeft BranchBase)
  rightArrow <- expectJust (branchArrow BranchRight BranchBase)
  case amalgamateMatchingFamilyWith branchCompiledStalkAlgebra (gaAmalgamate branchGluingAlgebra) branchSite matchingFamily of
    Right amalgamation -> do
      let gluedStalk = amalgamatedStalk amalgamation
      gluedStalk @?= branchCompatibleAmalgamatedStalk
      restrictAlong branchSite leftArrow gluedStalk @?= branchLeftCompatibleStalk
      restrictAlong branchSite rightArrow gluedStalk @?= branchRightCompatibleStalk
    Left failure ->
      assertFailure ("expected semantic gluing claim to hold, received " <> show failure)

testOverlapDisagreementBlocksGluing :: Assertion
testOverlapDisagreementBlocksGluing = do
  coverValue <- expectRight branchRootCover
  matchingFamily <-
    expectRight
      ( mkMatchingFamilyForCover branchSite
          coverValue
          (Vector.fromList [branchLeftCompatibleStalk, branchRightIncompatibleStalk])
      )
  case amalgamateMatchingFamilyWith branchCompiledStalkAlgebra (gaAmalgamate branchGluingAlgebra) branchSite matchingFamily of
    Left
      ( IncompatibleMatchingFamily
          (PullbackDisagreement square [BranchCoordinateConflict BranchApex 7 8] :| [])
        ) -> do
        cmSource (psToLeft square) @?= BranchApex
        cmSource (psToRight square) @?= BranchApex
    Left failure ->
      assertFailure ("expected overlap disagreement, received " <> show failure)
    Right _ ->
      assertFailure "expected overlap disagreement, received successful amalgamation"

mkMatchingFamilyForCover ::
  Site site =>
  site ->
  CoveringFamily (SiteObject site) (SiteMorphism site) ->
  Vector stalk ->
  Either
    (Either (EffectiveCoverPlanFailure (SiteObject site) (SiteMorphism site)) MatchingFamilyConstructionError)
    (MatchingFamily site stalk)
mkMatchingFamilyForCover site coverValue sections = do
  effectiveCover <-
    case prepareEffectiveCoverPlan site coverValue of
      Left failure ->
        Left (Left failure)
      Right planValue ->
        Right planValue
  case mkMatchingFamily effectiveCover sections of
    Left failure ->
      Left (Right failure)
    Right matchingFamily ->
      Right matchingFamily

testStalkEvidencePreservesFailureShape :: Assertion
testStalkEvidencePreservesFailureShape =
  stalkMismatches branchStalkAlgebra branchLeftCompatibleStalk branchRightIncompatibleStalk
    @?=
      [ BranchMissingCoordinate BranchLeft (Just 10) Nothing,
        BranchMissingCoordinate BranchRight Nothing (Just 20),
        BranchCoordinateConflict BranchApex 7 8
      ]
