{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Sheaf.Presheaf.Image.AdjunctionSpec
  ( tests,
  )
where

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Sheaf.Image.Adjunction
  ( FiniteImageAdjunction (..),
    FiniteImageAdjunctionFailure (..),
    FiniteSiteMapLeftTriangleFailure (..),
    FiniteSiteMapRightTriangleFailure (..),
    finiteImageAdjunctionFromEvidence,
    finiteImageAdjunctionSatisfied,
    finiteSiteMapImageAdjunction,
    finiteSiteMapLeftTriangleFailures,
    finiteSiteMapRightTriangleFailures,
  )
import Moonlight.Sheaf.Image.ContextGalois
  ( checkContextImageAdjunction,
    mkContextGaloisMap,
  )
import Moonlight.Sheaf.Image.Direct
  ( DirectImageCone,
    DirectImageIndexObject (..),
    directImageConeAssignments,
    directImageConeTarget,
    directImageConeValueAt,
    mkDirectImageCone,
    pushforwardFinitePresheaf,
  )
import Moonlight.Sheaf.Presheaf.Enumeration
  ( FiniteEnumerationBudget (..),
  )
import Moonlight.Sheaf.Presheaf.Finite
  ( FinitePresheaf (..),
    mkFinitePresheaf,
  )
import Moonlight.Sheaf.Presheaf.Morphism
  ( FinitePresheafMorphismFailure (..),
    finitePresheafMorphismComponentAt,
    mkFinitePresheafMorphism,
  )
import Moonlight.Sheaf.Site.Class
  ( Site (..),
    siteMorphismUniverse,
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
  ( ContinuousSiteMap,
    FiniteSiteMap,
    mkContinuousSiteMap,
    mkFiniteSiteMap,
  )
import Moonlight.Sheaf.TestFixture.Assertions (expectRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Moonlight.FiniteLattice
  ( ContextLattice,
    compileContextLattice,
    contextOrderDecl
  )

tests :: TestTree
tests =
  testGroup
    "finite image adjunction"
    [ testCase "identity site map gives identity-shaped image adjunction evidence" testIdentitySiteMapAdjunction,
      testCase "context Galois image adjunction carries unit, counit, and triangle evidence" testContextGaloisAdjunction,
      testCase "image adjunction evidence rejects non-natural unit morphisms" testRejectsNonNaturalUnitEvidence,
      testCase "collapse site map satisfies both computed triangle identities" testCollapseSiteMapAdjunctionSatisfied,
      testCase "perturbed target restriction trips the left triangle identity" testPerturbedLeftTriangleRejected,
      testCase "perturbed pushforward restriction trips the right triangle identity" testPerturbedRightTriangleRejected
    ]

data ChainCell = ChainCoarse | ChainFine
  deriving stock (Eq, Ord, Show, Read)

data PointCell = PointCell
  deriving stock (Eq, Ord, Show)

data VoidRestrictionFailure
  deriving stock (Eq, Show)

type ChainSite = FiniteMeetSite ChainCell

unboundedBudget :: FiniteEnumerationBudget
unboundedBudget =
  FiniteEnumerationBudget Nothing

testIdentitySiteMapAdjunction :: Assertion
testIdentitySiteMapAdjunction = do
  site <- expectRight chainSite
  continuous <- expectRight (identityContinuousSiteMap site)
  presheaf <- expectRight (chainPresheaf site)
  adjunction <- expectRight (finiteSiteMapImageAdjunction unboundedBudget continuous presheaf presheaf)
  finiteImageAdjunctionSatisfied adjunction @?= True
  coarseIdentityIndex <- finiteMeetIdentityIndex site ChainCoarse
  unitCone <-
    maybe
      (assertFailure "expected unit component at ChainCoarse / 10")
      pure
      (finitePresheafMorphismComponentAt ChainCoarse 10 (finiteImageAdjunctionUnit adjunction))
  directImageConeValueAt coarseIdentityIndex unitCone @?= Just 10
  finitePresheafMorphismComponentAt ChainCoarse unitCone (finiteImageAdjunctionCounit adjunction) @?= Just 10

testContextGaloisAdjunction :: Assertion
testContextGaloisAdjunction = do
  lattice <- expectRight chainLattice
  site <- expectRight chainSite
  galois <-
    expectRight
      ( mkContextGaloisMap
          lattice
          lattice
          site
          site
          (const ChainCoarse)
          (const ChainFine)
      )
  presheaf <- expectRight (chainPresheaf site)
  adjunction <- expectRight (checkContextImageAdjunction galois presheaf presheaf)
  finiteImageAdjunctionSatisfied adjunction @?= True
  finitePresheafMorphismComponentAt ChainCoarse 11 (finiteImageAdjunctionUnit adjunction) @?= Just 11
  finitePresheafMorphismComponentAt ChainFine 11 (finiteImageAdjunctionCounit adjunction) @?= Just 11

testRejectsNonNaturalUnitEvidence :: Assertion
testRejectsNonNaturalUnitEvidence = do
  site <- expectRight chainSite
  presheaf <- expectRight (chainPresheaf site)
  let unitResult =
        mkFinitePresheafMorphism presheaf presheaf unnaturalChainComponent
      counitResult =
        mkFinitePresheafMorphism presheaf presheaf identityChainComponent
  case finiteImageAdjunctionFromEvidence unitResult counitResult [] [] of
    Left (FiniteImageAdjunctionUnitInvalid FinitePresheafMorphismNaturalityMismatch {}) ->
      pure ()
    Left failure ->
      assertFailure ("expected naturality obstruction, received " <> show failure)
    Right _adjunction ->
      assertFailure "expected non-natural unit evidence to be rejected"

testCollapseSiteMapAdjunctionSatisfied :: Assertion
testCollapseSiteMapAdjunctionSatisfied = do
  site <- expectRight chainSite
  point <- expectRight pointSite
  continuous <- expectRight (collapseContinuousSiteMap site point)
  targetPresheaf <- expectRight (pointPresheaf point)
  sourcePresheaf <- expectRight (chainPresheaf site)
  adjunction <- expectRight (finiteSiteMapImageAdjunction unboundedBudget continuous targetPresheaf sourcePresheaf)
  finiteImageAdjunctionSatisfied adjunction @?= True
  finiteImageAdjunctionLeftTriangleFailures adjunction @?= []
  finiteImageAdjunctionRightTriangleFailures adjunction @?= []

testPerturbedLeftTriangleRejected :: Assertion
testPerturbedLeftTriangleRejected = do
  site <- expectRight chainSite
  siteMapValue <- expectRight (identityFiniteSiteMap site)
  honest <- expectRight (chainPresheaf site)
  let perturbed =
        honest {fpRestrict = \_morphism value -> Right (flipChainValue value)} ::
          FinitePresheaf ChainSite Int () VoidRestrictionFailure
      failures =
        finiteSiteMapLeftTriangleFailures siteMapValue perturbed honest
  case [failure | failure@FiniteSiteMapLeftTriangleMismatch {} <- failures] of
    [] -> assertFailure ("expected left triangle mismatch, received " <> show failures)
    _ -> pure ()

testPerturbedRightTriangleRejected :: Assertion
testPerturbedRightTriangleRejected = do
  site <- expectRight chainSite
  continuous <- expectRight (identityContinuousSiteMap site)
  siteMapValue <- expectRight (identityFiniteSiteMap site)
  sourcePresheaf <- expectRight (chainPresheaf site)
  pushed <- expectRight (first show (pushforwardFinitePresheaf unboundedBudget continuous sourcePresheaf))
  let flipCone ::
        DirectImageCone ChainCell ChainCell (FiniteMeetMorphism ChainCell) Int ->
        DirectImageCone ChainCell ChainCell (FiniteMeetMorphism ChainCell) Int
      flipCone coneValue =
        mkDirectImageCone
          (directImageConeTarget coneValue)
          (fmap flipChainValue (directImageConeAssignments coneValue))
      perturbed =
        pushed {fpRestrict = \morphismValue coneValue -> fmap flipCone (fpRestrict pushed morphismValue coneValue)}
      failures =
        finiteSiteMapRightTriangleFailures siteMapValue perturbed
  case [failure | failure@FiniteSiteMapRightTriangleMismatch {} <- failures] of
    [] -> assertFailure ("expected right triangle mismatch, received " <> show failures)
    _ -> pure ()

chainSite :: Either String ChainSite
chainSite =
  first show $
    mkFiniteMeetSite
      FiniteMeetSiteSpec
        { fmssCells = ChainCoarse :| [ChainFine],
          fmssRefinements = Set.singleton (ChainFine, ChainCoarse),
          fmssCovers = Map.empty
        }

chainLattice :: Either String (ContextLattice ChainCell)
chainLattice =
  first show $
    compileContextLattice
      (Set.fromList [ChainCoarse, ChainFine])
      (contextOrderDecl ChainFine ChainCoarse [(ChainCoarse, ChainFine)])

chainPresheaf :: ChainSite -> Either String (FinitePresheaf ChainSite Int () VoidRestrictionFailure)
chainPresheaf site =
  first show $
    mkFinitePresheaf
      site
      (\_morphism value -> Right value)
      (\_object leftValue rightValue -> [() | leftValue /= rightValue])
      (\_object value -> value)
      (Map.fromList [(ChainCoarse, [10, 11]), (ChainFine, [10, 11])])

identityContinuousSiteMap ::
  ( Site site,
    Ord (SiteMorphism site),
    Show (SiteObject site),
    Show (SiteMorphism site)
  ) =>
  site ->
  Either String (ContinuousSiteMap site site)
identityContinuousSiteMap site = do
  let objectImages =
        Map.fromList (fmap (\objectValue -> (objectValue, objectValue)) (siteObjects site))
      morphismImages =
        Map.fromList (fmap (\morphismValue -> (morphismValue, morphismValue)) (siteMorphismUniverse site))
  siteMapValue <- first show (mkFiniteSiteMap site site objectImages morphismImages)
  basis <- first show (mkFiniteCoverBasis site)
  first show (mkContinuousSiteMap basis basis siteMapValue)

finiteMeetIdentityIndex ::
  (Ord cell, Show cell) =>
  FiniteMeetSite cell ->
  cell ->
  AssertionWith (DirectImageIndexObject cell cell (FiniteMeetMorphism cell))
finiteMeetIdentityIndex site objectValue = do
  morphismValue <-
    maybe
      (assertFailure ("expected finite-meet identity morphism at " <> show objectValue))
      pure
      (finiteMeetMorphism site objectValue objectValue)
  pure
    DirectImageIndexObject
      { directImageIndexSourceObject = objectValue,
        directImageIndexTargetMorphism = morphismValue
      }

pointSite :: Either String (FiniteMeetSite PointCell)
pointSite =
  first show $
    mkFiniteMeetSite
      FiniteMeetSiteSpec
        { fmssCells = PointCell :| [],
          fmssRefinements = Set.empty,
          fmssCovers = Map.empty
        }

pointPresheaf ::
  FiniteMeetSite PointCell ->
  Either String (FinitePresheaf (FiniteMeetSite PointCell) Int () VoidRestrictionFailure)
pointPresheaf site =
  first show $
    mkFinitePresheaf
      site
      (\_morphism value -> Right value)
      (\_object leftValue rightValue -> [() | leftValue /= rightValue])
      (\_object value -> value)
      (Map.fromList [(PointCell, [10, 11])])

collapseContinuousSiteMap ::
  ChainSite ->
  FiniteMeetSite PointCell ->
  Either String (ContinuousSiteMap ChainSite (FiniteMeetSite PointCell))
collapseContinuousSiteMap chain point = do
  pointIdentity <-
    case siteMorphismUniverse point of
      [morphismValue] -> Right morphismValue
      universe -> Left ("expected a single point morphism, received " <> show (length universe))
  let objectImages =
        Map.fromList (fmap (\objectValue -> (objectValue, PointCell)) (siteObjects chain))
      morphismImages =
        Map.fromList (fmap (\morphismValue -> (morphismValue, pointIdentity)) (siteMorphismUniverse chain))
  siteMapValue <- first show (mkFiniteSiteMap chain point objectImages morphismImages)
  chainBasis <- first show (mkFiniteCoverBasis chain)
  pointBasis <- first show (mkFiniteCoverBasis point)
  first show (mkContinuousSiteMap chainBasis pointBasis siteMapValue)

identityFiniteSiteMap ::
  ( Site site,
    Ord (SiteMorphism site),
    Show (SiteObject site),
    Show (SiteMorphism site)
  ) =>
  site ->
  Either String (FiniteSiteMap site site)
identityFiniteSiteMap site =
  let objectImages =
        Map.fromList (fmap (\objectValue -> (objectValue, objectValue)) (siteObjects site))
      morphismImages =
        Map.fromList (fmap (\morphismValue -> (morphismValue, morphismValue)) (siteMorphismUniverse site))
   in first show (mkFiniteSiteMap site site objectImages morphismImages)

flipChainValue :: Int -> Int
flipChainValue value =
  if value == 10 then 11 else 10

identityChainComponent :: ChainCell -> Int -> Either () Int
identityChainComponent _object value =
  Right value

unnaturalChainComponent :: ChainCell -> Int -> Either () Int
unnaturalChainComponent objectValue value =
  case objectValue of
    ChainCoarse ->
      Right value
    ChainFine ->
      Right (if value == 10 then 11 else 10)

type AssertionWith value = IO value
