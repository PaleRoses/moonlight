{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Presheaf.GoldenConstructionSpec
  ( tests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Sheaf.Image.ContextGalois
  ( extendFiniteContextPresheaf,
    mkContextGaloisMap,
  )
import Moonlight.Sheaf.Image.Restrict
  ( pullbackFinitePresheaf,
  )
import Moonlight.Sheaf.Presheaf.Finite
  ( FinitePresheaf (..),
    finiteFiberAt,
    finiteFiberValues,
    mkFinitePresheaf,
  )
import Moonlight.Sheaf.Presheaf.Morphism
  ( finitePresheafMorphismComponents,
    mkFinitePresheafMorphism,
  )
import Moonlight.Sheaf.Presheaf.Enumeration
  ( FiniteEnumerationBudget (..),
  )
import Moonlight.Sheaf.Sheafification.Finite
  ( FinitePlusUnitEvidence (..),
    SheafConditionReport (..),
    SheafificationUnitEvidence (..),
    UnitSurjectivityFailure (..),
    associatedSheafificationReport,
    sheafificationUnitEvidence,
    sheafConditionReportAccepted,
    sheafifyFinitePresheaf,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism,
    Site (..),
  )
import Moonlight.Sheaf.Site.CoverBasis.Finite
  ( mkFiniteCoverBasis,
  )
import Moonlight.Sheaf.Site.Construction.FiniteMeet
  ( FiniteMeetMorphism,
    FiniteMeetSite,
    FiniteMeetSiteSpec (..),
    finiteMeetMorphism,
    mkFiniteMeetSite,
  )
import Moonlight.Sheaf.Site.Construction.Map
  ( mkFiniteSiteMap,
  )
import Moonlight.Sheaf.TestFixture.Assertions (expectRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import Moonlight.FiniteLattice
  ( ContextLattice,
    compileContextLattice,
    contextOrderDecl
  )

data GoldenContext
  = GoldenCoarse
  | GoldenFine
  deriving stock (Eq, Ord, Show, Read)

data UnitContext
  = UnitOnly
  deriving stock (Eq, Ord, Show, Read)

data VoidRestrictionFailure
  deriving stock (Eq, Show)

tests :: TestTree
tests =
  testGroup
    "golden sheaf constructions"
    [ testCase "pullback inverse image reindexes fibers and restrictions along a site map" testPullbackInverseImage,
      testCase "pushforward direct image extends fibers along the context upper adjoint" testPushforwardDirectImage,
      testCase "sheafification replaces a non-effective presheaf by an accepted associated sheaf" testSheafificationAssociatedSheaf,
      testCase "finite presheaf morphism validates the natural-transformation Hom surface" testFinitePresheafMorphismHomSurface
    ]

goldenSiteSpec :: FiniteMeetSiteSpec GoldenContext
goldenSiteSpec =
  FiniteMeetSiteSpec
    { fmssCells = GoldenCoarse :| [GoldenFine],
      fmssRefinements = Set.singleton (GoldenFine, GoldenCoarse),
      fmssCovers = Map.singleton GoldenCoarse [GoldenFine :| []]
    }

unitSiteSpec :: FiniteMeetSiteSpec UnitContext
unitSiteSpec =
  FiniteMeetSiteSpec
    { fmssCells = UnitOnly :| [],
      fmssRefinements = Set.empty,
      fmssCovers = Map.empty
    }

goldenLattice :: Either String (ContextLattice GoldenContext)
goldenLattice =
  mapLeft show $
    compileContextLattice
      (Set.fromList [GoldenCoarse, GoldenFine])
      (contextOrderDecl GoldenFine GoldenCoarse [(GoldenCoarse, GoldenFine)])

withGoldenSite :: (FiniteMeetSite GoldenContext -> Assertion) -> Assertion
withGoldenSite continue =
  expectRight (mapLeft show (mkFiniteMeetSite goldenSiteSpec)) >>= continue

withUnitSite :: (FiniteMeetSite UnitContext -> Assertion) -> Assertion
withUnitSite continue =
  expectRight (mapLeft show (mkFiniteMeetSite unitSiteSpec)) >>= continue

mkIntPresheaf ::
  (Show ctx, Site site, SiteObject site ~ ctx, SiteMorphism site ~ FiniteMeetMorphism ctx) =>
  site ->
  Map ctx [Int] ->
  Either String (FinitePresheaf site Int () VoidRestrictionFailure)
mkIntPresheaf site fibers =
  mapLeft show $
    mkFinitePresheaf
      site
      (\_morphism value -> Right value)
      (\_object leftValue rightValue -> [() | leftValue /= rightValue])
      (\_object value -> value)
      fibers

testPullbackInverseImage :: Assertion
testPullbackInverseImage =
  withGoldenSite $ \sourceSite ->
    withUnitSite $ \targetSite -> do
      let targetIdentity = identityAt targetSite UnitOnly
          objectImages = Map.fromList [(GoldenCoarse, UnitOnly), (GoldenFine, UnitOnly)]
          morphismImages = Map.fromList [(morphismValue, targetIdentity) | morphismValue <- siteMorphisms sourceSite]
      siteMapValue <- expectRight (mkFiniteSiteMap sourceSite targetSite objectImages morphismImages)
      targetPresheaf <- expectRight (mkIntPresheaf targetSite (Map.singleton UnitOnly [0, 1]))
      pulledPresheaf <- expectRight (pullbackFinitePresheaf siteMapValue targetPresheaf)
      fiberValuesAt GoldenCoarse pulledPresheaf @?= Just [0, 1]
      fiberValuesAt GoldenFine pulledPresheaf @?= Just [0, 1]
      fineToCoarse <- expectGoldenMorphism sourceSite GoldenFine GoldenCoarse
      fpRestrict pulledPresheaf fineToCoarse 1 @?= Right 1

testPushforwardDirectImage :: Assertion
testPushforwardDirectImage =
  withGoldenSite $ \site -> do
    lattice <- expectRight goldenLattice
    galois <-
      expectRight
        ( mkContextGaloisMap
            lattice
            lattice
            site
            site
            (const GoldenCoarse)
            (const GoldenFine)
        )
    sourcePresheaf <-
      expectRight
        ( mkIntPresheaf
            site
            (Map.fromList [(GoldenCoarse, [10, 11]), (GoldenFine, [10, 11])])
        )
    extendedPresheaf <- expectRight (extendFiniteContextPresheaf galois sourcePresheaf)
    fiberValuesAt GoldenCoarse extendedPresheaf @?= Just [10, 11]
    fiberValuesAt GoldenFine extendedPresheaf @?= Just [10, 11]

testSheafificationAssociatedSheaf :: Assertion
testSheafificationAssociatedSheaf =
  withGoldenSite $ \site -> do
    basis <- expectRight (mkFiniteCoverBasis site)
    presheaf <-
      expectRight
        ( mkIntPresheaf
            site
            (Map.fromList [(GoldenCoarse, []), (GoldenFine, [7])])
        )
    sheafification <- expectRight (sheafifyFinitePresheaf (FiniteEnumerationBudget Nothing) basis presheaf)
    unitEvidence <- expectRight (sheafificationUnitEvidence basis sheafification)
    case scrSurjectivityFailures (finitePlusUnitReport (sheafificationFirstUnit unitEvidence)) of
      [failure] ->
        usfObject failure @?= GoldenCoarse
      failures ->
        assertFailure ("expected one first-unit surjectivity failure, received " <> show failures)
    associatedReport <- expectRight (associatedSheafificationReport (FiniteEnumerationBudget Nothing) basis sheafification)
    assertBool
      "associated presheaf should satisfy the finite sheaf condition after plus-plus"
      (sheafConditionReportAccepted associatedReport)

testFinitePresheafMorphismHomSurface :: Assertion
testFinitePresheafMorphismHomSurface =
  withGoldenSite $ \site -> do
    sourcePresheaf <-
      expectRight
        ( mkIntPresheaf
            site
            (Map.fromList [(GoldenCoarse, [0, 1]), (GoldenFine, [0, 1])])
        )
    targetPresheaf <-
      expectRight
        ( mkIntPresheaf
            site
            (Map.fromList [(GoldenCoarse, [0, 1]), (GoldenFine, [0, 1])])
        )
    naturalTransformation <- expectRight (mkFinitePresheafMorphism sourcePresheaf targetPresheaf identityFiniteComponent)
    finitePresheafMorphismComponents naturalTransformation
      @?= Map.fromList
        [ (GoldenCoarse, [(0, 0), (1, 1)]),
          (GoldenFine, [(0, 0), (1, 1)])
        ]

identityFiniteComponent :: GoldenContext -> Int -> Either () Int
identityFiniteComponent _object value =
  Right value

fiberValuesAt ::
  Site site =>
  SiteObject site ->
  FinitePresheaf site value mismatch restrictionFailure ->
  Maybe [value]
fiberValuesAt objectValue presheaf =
  finiteFiberValues <$> finiteFiberAt objectValue presheaf

expectGoldenMorphism ::
  FiniteMeetSite GoldenContext ->
  GoldenContext ->
  GoldenContext ->
  AssertionWith (CheckedMorphism GoldenContext (FiniteMeetMorphism GoldenContext))
expectGoldenMorphism site source target =
  maybe
    (assertFailure ("expected finite-meet morphism " <> show (source, target)))
    pure
    (finiteMeetMorphism site source target)

type AssertionWith value = IO value

mapLeft :: (left -> left') -> Either left right -> Either left' right
mapLeft transform result =
  case result of
    Left failure ->
      Left (transform failure)
    Right value ->
      Right value
