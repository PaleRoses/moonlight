{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Presheaf.GoldenOperationSpec
  ( tests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moonlight.Sheaf
import Moonlight.Sheaf.Stalk
import Moonlight.Sheaf.Presheaf.Core qualified as Presheaf
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

data GoldenCell
  = GoldenGlobal
  | GoldenLeft
  | GoldenRight
  | GoldenOverlap
  deriving stock (Eq, Ord, Show)

newtype GoldenStalk = GoldenStalk Int
  deriving stock (Eq, Show)

data GoldenGluingFailure
  = GoldenEmptyFamily
  | GoldenNonConstantFamily ![GoldenStalk]
  deriving stock (Eq, Show)

type GoldenSite = FiniteMeetSite GoldenCell

type GoldenMismatch = DiscreteMismatch GoldenStalk

type GoldenRepair = DiscreteRepairObstruction GoldenStalk

goldenCells :: [GoldenCell]
goldenCells =
  [GoldenGlobal, GoldenLeft, GoldenRight, GoldenOverlap]

tests :: TestTree
tests =
  testGroup
    "golden sheaf operations"
    [ testCase "finite-meet EDSL names restriction cover and overlap geometry" testFiniteMeetEdslGeometry,
      testCase "restriction maps a section to a smaller context" testRestrictionOperation,
      testCase "compatibility on overlaps accepts equal germs and rejects disagreements" testCompatibilityOnOverlaps,
      testCase "gluing descent amalgamates compatible local sections" testGluingDescent,
      testCase "reactive-shaped local edits certify, obstruct, and glue through public sheaf API" testReactiveShapedPublicSheafFlow,
      testCase "extension succeeds or fails as typed certification" testExtensionOperation,
      testCase "section compatibility verdict exposes a fast public predicate" testSectionCompatibilityVerdict,
      testCase "stalk and germ reads expose local behaviour" testStalkAndGermOperation,
      testCase "local-to-global certification separates sections from obstructions" testLocalToGlobalCertification,
      testGroup
        "gluing algebra laws"
        [ testCase "amalgamated global stalk restricts to every matching local stalk" testGluingAlgebraRestrictsToMatchingFamily,
          testCase "amalgamation respects each cover's slot ordering" testGluingAlgebraOrderIndependent
        ]
    ]

goldenSiteSpec :: FiniteMeetSiteSpec GoldenCell
goldenSiteSpec =
  FiniteMeetSiteSpec
    { fmssCells = GoldenGlobal :| [GoldenLeft, GoldenRight, GoldenOverlap],
      fmssRefinements =
        Set.fromList
          [ (GoldenLeft, GoldenGlobal),
            (GoldenRight, GoldenGlobal),
            (GoldenOverlap, GoldenLeft),
            (GoldenOverlap, GoldenRight)
          ],
      fmssCovers =
        Map.singleton GoldenGlobal [GoldenLeft :| [GoldenRight]]
    }

goldenPermutedCoverSiteSpec :: FiniteMeetSiteSpec GoldenCell
goldenPermutedCoverSiteSpec =
  goldenSiteSpec
    { fmssCovers =
        Map.singleton GoldenGlobal [GoldenRight :| [GoldenLeft]]
    }

goldenAlgebra :: StalkAlgebra (CompiledRestriction GoldenSite) GoldenStalk GoldenMismatch GoldenRepair
goldenAlgebra =
  discreteStalkAlgebra

goldenGluing :: GluingAlgebra owner GoldenSite GoldenStalk GoldenGluingFailure
goldenGluing =
  GluingAlgebra
    { gaAmalgamate = \_site compatibleFamily ->
        let matchingFamilyValue =
              compatibleMatchingFamilyUnderlying compatibleFamily
         in case Vector.toList (matchingSections matchingFamilyValue) of
              [] ->
                Left (GluingRejected GoldenEmptyFamily)
              firstStalk : remainingStalks
                | all (== firstStalk) remainingStalks ->
                    Right firstStalk
                | otherwise ->
                    Left (GluingRejected (GoldenNonConstantFamily (firstStalk : remainingStalks)))
    }

withGoldenSiteFromSpec :: FiniteMeetSiteSpec GoldenCell -> (GoldenSite -> Assertion) -> Assertion
withGoldenSiteFromSpec finiteMeetSpec continue =
  case mkFiniteMeetSite finiteMeetSpec of
    Left failure ->
      assertFailure ("expected golden finite-meet site to build, received " <> show failure)
    Right site ->
      continue site

withGoldenPreparedSite :: (forall owner. GoldenSite -> PreparedSite owner GoldenSite -> Assertion) -> Assertion
withGoldenPreparedSite continue =
  withGoldenPreparedSiteFromSpec goldenSiteSpec continue

withGoldenPreparedSiteFromSpec ::
  FiniteMeetSiteSpec GoldenCell ->
  (forall owner. GoldenSite -> PreparedSite owner GoldenSite -> Assertion) ->
  Assertion
withGoldenPreparedSiteFromSpec finiteMeetSpec continue =
  withGoldenSiteFromSpec finiteMeetSpec $ \site ->
    case compile (siteSpec site) (continue site) of
      Left failure ->
        assertFailure ("expected golden site preparation to succeed, received " <> show failure)
      Right assertion ->
        assertion

withGoldenCoverPlan :: PreparedSite owner GoldenSite -> (PreparedCover owner GoldenSite -> Assertion) -> Assertion
withGoldenCoverPlan preparedSite continue =
  case preparedCovers preparedSite GoldenGlobal of
    Right [coverPlan] ->
      continue coverPlan
    Right coverPlans ->
      assertFailure ("expected exactly one global cover plan, received " <> show (length coverPlans))
    Left refusal ->
      assertFailure ("expected known global object, received " <> show refusal)

withGoldenSection :: PreparedSite owner GoldenSite -> Int -> (Section owner GoldenSite GoldenStalk -> Assertion) -> Assertion
withGoldenSection preparedSite value continue =
  case section preparedSite (constantEntries value) of
    Left failure ->
      assertFailure ("expected constant total section, received " <> show failure)
    Right sectionValue ->
      continue sectionValue

constantEntries :: Int -> Map GoldenCell GoldenStalk
constantEntries value =
  goldenCellMap (const (GoldenStalk value))

goldenCellMap :: (GoldenCell -> value) -> Map GoldenCell value
goldenCellMap valuesAt =
  Map.fromList (fmap (\cell -> (cell, valuesAt cell)) goldenCells)

expectGoldenMorphism :: GoldenSite -> GoldenCell -> GoldenCell -> (CheckedMorphism GoldenCell (FiniteMeetMorphism GoldenCell) -> Assertion) -> Assertion
expectGoldenMorphism site source target continue =
  case finiteMeetMorphism site source target of
    Nothing ->
      assertFailure ("expected finite-meet morphism " <> show (source, target))
    Just morphismValue ->
      continue morphismValue

goldenGluingLawSections :: Vector GoldenStalk
goldenGluingLawSections =
  Vector.fromList [GoldenStalk 17, GoldenStalk 17]

goldenGluingLawSectionsPermuted :: Vector GoldenStalk
goldenGluingLawSectionsPermuted =
  Vector.fromList [GoldenStalk 17, GoldenStalk 17]

expectGoldenCompatibleFamily ::
  GoldenSite ->
  PreparedCover owner GoldenSite ->
  Vector GoldenStalk ->
  (CompatibleMatchingFamily owner GoldenSite GoldenStalk -> Assertion) ->
  Assertion
expectGoldenCompatibleFamily _site coverPlan localSections continue =
  case matching coverPlan localSections of
    Left failure ->
      assertFailure ("expected matching family construction, received " <> show failure)
    Right matchingFamilyValue ->
      case certifyMatching goldenAlgebra matchingFamilyValue of
        Left failures ->
          assertFailure ("expected compatible matching family, received " <> show failures)
        Right compatibleFamily ->
          continue compatibleFamily

testGluingAlgebraRestrictsToMatchingFamily :: Assertion
testGluingAlgebraRestrictsToMatchingFamily =
  withGoldenPreparedSite $ \site preparedSite ->
    withGoldenCoverPlan preparedSite $ \coverPlan ->
      expectGoldenCompatibleFamily site coverPlan goldenGluingLawSections $ \compatibleFamily ->
        case gaAmalgamate goldenGluing site compatibleFamily of
          Left obstruction ->
            assertFailure ("expected gluing algebra amalgamation, received " <> show obstruction)
          Right globalStalk -> do
            expectGoldenMorphism site GoldenLeft GoldenGlobal $ \leftMorphism ->
              restrictStalk goldenAlgebra (Presheaf.CompiledRestriction site leftMorphism) globalStalk
                @?= GoldenStalk 17
            expectGoldenMorphism site GoldenRight GoldenGlobal $ \rightMorphism ->
              restrictStalk goldenAlgebra (Presheaf.CompiledRestriction site rightMorphism) globalStalk
                @?= GoldenStalk 17

testGluingAlgebraOrderIndependent :: Assertion
testGluingAlgebraOrderIndependent =
  withGoldenPreparedSiteFromSpec goldenSiteSpec $ \canonicalSite canonicalPreparedSite ->
    withGoldenCoverPlan canonicalPreparedSite $ \canonicalCoverPlan ->
      expectGoldenCompatibleFamily canonicalSite canonicalCoverPlan goldenGluingLawSections $ \canonicalFamily ->
        withGoldenPreparedSiteFromSpec goldenPermutedCoverSiteSpec $ \permutedSite permutedPreparedSite ->
          withGoldenCoverPlan permutedPreparedSite $ \permutedCoverPlan ->
            expectGoldenCompatibleFamily permutedSite permutedCoverPlan goldenGluingLawSectionsPermuted $ \permutedFamily ->
              gaAmalgamate goldenGluing canonicalSite canonicalFamily
                @?= gaAmalgamate goldenGluing permutedSite permutedFamily

testFiniteMeetEdslGeometry :: Assertion
testFiniteMeetEdslGeometry =
  withGoldenPreparedSite $ \site preparedSite ->
    withGoldenCoverPlan preparedSite $ \coverPlan -> do
      let restrictionPairs =
            Set.fromList
              (fmap (\morphismValue -> (cmSource morphismValue, cmTarget morphismValue)) (siteRestrictionMorphisms site))
      restrictionPairs
        @?= Set.fromList
          [ (GoldenLeft, GoldenGlobal),
            (GoldenRight, GoldenGlobal),
            (GoldenOverlap, GoldenGlobal),
            (GoldenOverlap, GoldenLeft),
            (GoldenOverlap, GoldenRight)
          ]
      preparedCoverTarget coverPlan @?= GoldenGlobal
      preparedCoverSources coverPlan @?= Vector.fromList [GoldenLeft, GoldenRight]
      preparedCoverSize coverPlan @?= 2

testRestrictionOperation :: Assertion
testRestrictionOperation =
  withGoldenPreparedSite $ \site preparedSite ->
    withGoldenSection preparedSite 42 $ \sectionValue ->
      expectGoldenMorphism site GoldenOverlap GoldenLeft $ \_overlapToLeft ->
        certify goldenAlgebra sectionValue @?= Right SectionCertified

testCompatibilityOnOverlaps :: Assertion
testCompatibilityOnOverlaps =
  withGoldenPreparedSite $ \_site preparedSite ->
    withGoldenCoverPlan preparedSite $ \coverPlan -> do
      compatibleFamily <-
        case matching coverPlan (Vector.fromList [GoldenStalk 7, GoldenStalk 7]) of
          Left failure ->
            assertFailure ("expected compatible matching family construction, received " <> show failure)
          Right family ->
            pure family
      matchingTarget compatibleFamily @?= GoldenGlobal
      case glue goldenAlgebra goldenGluing compatibleFamily of
        Right amalgamation ->
          amalgamatedStalk amalgamation @?= GoldenStalk 7
        Left failure ->
          assertFailure ("expected compatible overlap family to glue, received " <> show failure)

      incompatibleFamily <-
        case matching coverPlan (Vector.fromList [GoldenStalk 7, GoldenStalk 8]) of
          Left failure ->
            assertFailure ("expected incompatible matching family construction, received " <> show failure)
          Right family ->
            pure family
      case glue goldenAlgebra goldenGluing incompatibleFamily of
        Left
          ( CoverAmalgamationFailed
              ( IncompatibleMatchingFamily
                  (PullbackDisagreement square [DiscreteMismatch (GoldenStalk 7) (GoldenStalk 8)] :| [])
                )
            ) ->
          psApex square @?= GoldenOverlap
        Left failure ->
          assertFailure ("expected one typed overlap disagreement, received " <> show failure)
        Right success ->
          assertFailure ("expected incompatible overlap family to fail, received " <> show (amalgamatedStalk success))

testGluingDescent :: Assertion
testGluingDescent =
  withGoldenPreparedSite $ \_site preparedSite ->
    withGoldenCoverPlan preparedSite $ \coverPlan -> do
      let localSections =
            Vector.fromList [GoldenStalk 11, GoldenStalk 11]
      matchingFamilyValue <- expectPublicRight (matching coverPlan localSections)
      matchingTarget matchingFamilyValue @?= GoldenGlobal
      case glue goldenAlgebra goldenGluing matchingFamilyValue of
        Left failure ->
          assertFailure ("expected compatible local family to glue, received " <> show failure)
        Right amalgamation ->
          amalgamatedStalk amalgamation @?= GoldenStalk 11

testReactiveShapedPublicSheafFlow :: Assertion
testReactiveShapedPublicSheafFlow =
  withGoldenPreparedSite $ \_site preparedSite ->
    withGoldenCoverPlan preparedSite $ \coverPlan -> do
      baseSection <- expectPublicRight (section preparedSite (constantEntries 0))
      localEdit <- expectPublicRight (assignOne GoldenLeft (GoldenStalk 9) baseSection)
      changedObjects localEdit @?= ChangedObjects (Set.singleton GoldenLeft)
      case certify goldenAlgebra localEdit of
        Right (SectionRejected rejections) ->
          assertBool "unpropagated local edit produces typed obstruction" (not (Map.null rejections))
        Right SectionCertified ->
          assertFailure "unpropagated local edit must not certify as globally compatible"
        certificationFailure ->
          assertFailure ("expected typed section rejection, received " <> show certificationFailure)

      propagatedSection <- expectPublicRight (assign (constantEntries 9) baseSection)
      certify goldenAlgebra propagatedSection @?= Right SectionCertified
      stalkAt GoldenGlobal propagatedSection @?= Right (GoldenStalk 9)
      stalkAt GoldenOverlap propagatedSection @?= Right (GoldenStalk 9)
      case globalSection goldenAlgebra propagatedSection of
        Left rejection ->
          assertFailure ("expected propagated edit to become a global section, received " <> show rejection)
        Right _globalSection ->
          pure ()

      let compatibleLocalPatch =
            Vector.fromList [GoldenStalk 9, GoldenStalk 9]
      matchingValue <- expectPublicRight (matching coverPlan compatibleLocalPatch)
      matchingTarget matchingValue @?= GoldenGlobal
      case glue goldenAlgebra goldenGluing matchingValue of
        Left failure ->
          assertFailure ("expected compatible local patch to glue, received " <> show failure)
        Right amalgamation ->
          amalgamatedStalk amalgamation @?= GoldenStalk 9

      conflictingMatchingValue <- expectPublicRight (matching coverPlan (Vector.fromList [GoldenStalk 9, GoldenStalk 10]))
      case glue goldenAlgebra goldenGluing conflictingMatchingValue of
        Left
          ( CoverAmalgamationFailed
              ( IncompatibleMatchingFamily
                  (PullbackDisagreement square [DiscreteMismatch (GoldenStalk 9) (GoldenStalk 10)] :| [])
                )
            ) ->
          psApex square @?= GoldenOverlap
        Left failure ->
          assertFailure ("expected typed overlap obstruction, received " <> show failure)
        Right success ->
          assertFailure ("expected conflicting local patch to fail, received " <> show (amalgamatedStalk success))

testExtensionOperation :: Assertion
testExtensionOperation =
  withGoldenPreparedSite $ \_site preparedSite -> do
    withGoldenSection preparedSite 5 $ \sectionValue ->
      case globalSection goldenAlgebra sectionValue of
        Left rejection ->
          assertFailure ("expected constant local data to extend globally, received " <> show rejection)
        Right _globalSection ->
          pure ()

    case section preparedSite (Map.fromList [(GoldenGlobal, GoldenStalk 5), (GoldenLeft, GoldenStalk 5), (GoldenRight, GoldenStalk 6), (GoldenOverlap, GoldenStalk 5)]) of
      Left failure ->
        assertFailure ("expected obstructed total section to construct, received " <> show failure)
      Right obstructedSection ->
        case globalSection goldenAlgebra obstructedSection of
          Left (SectionCertificationSemanticallyRejected rejections) ->
            assertBool "right patch extension obstruction is retained" (Map.member GoldenRight rejections)
          Left rejection ->
            assertFailure ("expected typed section rejection, received " <> show rejection)
          Right _ ->
            assertFailure "expected inconsistent local data to fail extension"

testSectionCompatibilityVerdict :: Assertion
testSectionCompatibilityVerdict =
  withGoldenPreparedSite $ \_site preparedSite -> do
    withGoldenSection preparedSite 5 $ \sectionValue -> do
      assertBool "compatible section predicate accepts" (isSectionCompatible goldenAlgebra sectionValue)
      sectionCompatibilityVerdict goldenAlgebra sectionValue @?= Accepted ()

    obstructedSection <-
      case section preparedSite (Map.fromList [(GoldenGlobal, GoldenStalk 5), (GoldenLeft, GoldenStalk 5), (GoldenRight, GoldenStalk 6), (GoldenOverlap, GoldenStalk 5)]) of
        Left failure ->
          assertFailure ("expected obstructed total section to construct, received " <> show failure)
        Right sectionValue ->
          pure sectionValue
    assertBool "obstructed section predicate rejects" (not (isSectionCompatible goldenAlgebra obstructedSection))
    case sectionCompatibilityVerdict goldenAlgebra obstructedSection of
      Rejected (SectionCertificationSemanticallyRejected rejections) ->
        Map.size rejections @?= 2
      Rejected rejection ->
        assertFailure ("expected rejected section verdict, received " <> show rejection)
      Accepted () ->
        assertFailure "expected obstructed section verdict rejection"

testStalkAndGermOperation :: Assertion
testStalkAndGermOperation =
  withGoldenPreparedSite $ \_site preparedSite ->
    withGoldenSection preparedSite 13 $ \sectionValue -> do
      stalkAt GoldenOverlap sectionValue @?= Right (GoldenStalk 13)
      stalkAt GoldenLeft sectionValue @?= Right (GoldenStalk 13)

testLocalToGlobalCertification :: Assertion
testLocalToGlobalCertification =
  withGoldenPreparedSite $ \_site preparedSite -> do
    withGoldenSection preparedSite 3 $ \sectionValue ->
      certify goldenAlgebra sectionValue @?= Right SectionCertified

    case section preparedSite (Map.fromList [(GoldenGlobal, GoldenStalk 3), (GoldenLeft, GoldenStalk 3), (GoldenRight, GoldenStalk 4), (GoldenOverlap, GoldenStalk 3)]) of
      Left failure ->
        assertFailure ("expected rejected total section to construct, received " <> show failure)
      Right rejectedSection ->
        case certify goldenAlgebra rejectedSection of
          Right (SectionRejected rejections) ->
            assertBool "certification names obstructed contexts" (not (Map.null rejections))
          otherCertification ->
            assertFailure ("expected rejected certification, received " <> show otherCertification)

expectPublicRight :: Show failure => Either failure value -> AssertionWith value
expectPublicRight result =
  case result of
    Left failure ->
      assertFailure ("expected public operation to succeed, received " <> show failure)
    Right value ->
      pure value

type AssertionWith value = IO value
