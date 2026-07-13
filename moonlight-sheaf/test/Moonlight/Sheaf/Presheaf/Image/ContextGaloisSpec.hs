{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Sheaf.Presheaf.Image.ContextGaloisSpec
  ( tests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Sheaf.Image.ContextGalois
  ( ContextGaloisMapFailure (..),
    ContextExtendPresheafFailure (..),
    ContextRestrictPresheafFailure (..),
    checkContextImageAdjunction,
    extendFiniteContextPresheaf,
    mkContextGaloisMap,
    restrictFiniteContextPresheaf,
  )
import Moonlight.Sheaf.Image.Adjunction
  ( finiteImageAdjunctionSatisfied,
  )
import Moonlight.Sheaf.Presheaf.Finite
  ( FinitePresheaf,
    mkFinitePresheaf,
  )
import Moonlight.Sheaf.Site.Class (CheckedMorphism)
import Moonlight.Sheaf.Site.Construction.FiniteMeet
  ( FiniteMeetMorphism,
    FiniteMeetSite,
    FiniteMeetSiteSpec (..),
    mkFiniteMeetSite,
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
    "context Galois image"
    [ testCase "validates the identity finite-context Galois map" testIdentityContextGalois,
      testCase "validates a non-identity finite-context Galois map" testNonIdentityContextGalois,
      testCase "rejects a non-adjoint lower/upper pair" testRejectsInvalidAdjunction,
      testCase "restricts and extends ordinary finite presheaves" testRestrictExtendFinitePresheaves,
      testCase "guards against presheaves over a different target site" testRestrictTargetSiteMismatch,
      testCase "guards against presheaves over a different source site" testExtendSourceSiteMismatch,
      testCase "reports empty adjunction laws for the identity context image pair" testIdentityAdjunctionReport
    ]

testIdentityContextGalois :: Assertion
testIdentityContextGalois = do
  lattice <- expectRight tinyLattice
  site <- expectRight tinySite
  case mkContextGaloisMap lattice lattice site site id id of
    Right _ ->
      pure ()
    Left failure ->
      assertFailure ("expected identity context Galois map to validate, received " <> show failure)

testNonIdentityContextGalois :: Assertion
testNonIdentityContextGalois = do
  lattice <- expectRight tinyLattice
  site <- expectRight tinySite
  galois <-
    expectRight
      ( mkContextGaloisMap
          lattice
          lattice
          site
          site
          (const TinyCoarse)
          (const TinyFine)
      )
  presheaf <- expectRight (tinyPresheaf site)
  _restricted <- expectRight (restrictFiniteContextPresheaf galois presheaf)
  _extended <- expectRight (extendFiniteContextPresheaf galois presheaf)
  adjunction <- expectRight (checkContextImageAdjunction galois presheaf presheaf)
  finiteImageAdjunctionSatisfied adjunction @?= True

testRejectsInvalidAdjunction :: Assertion
testRejectsInvalidAdjunction = do
  lattice <- expectRight tinyLattice
  site <- expectRight tinySite
  case mkContextGaloisMap lattice lattice site site (const TinyCoarse) id of
    Left ContextGaloisAdjunctionFailed {} ->
      pure ()
    Left failure ->
      assertFailure ("expected adjunction failure, received " <> show failure)
    Right _ ->
      assertFailure "expected adjunction failure, received valid map"

testRestrictExtendFinitePresheaves :: Assertion
testRestrictExtendFinitePresheaves = do
  lattice <- expectRight tinyLattice
  site <- expectRight tinySite
  galois <- expectRight (mkContextGaloisMap lattice lattice site site id id)
  presheaf <- expectRight (tinyPresheaf site)
  _restricted <- expectRight (restrictFiniteContextPresheaf galois presheaf)
  _extended <- expectRight (extendFiniteContextPresheaf galois presheaf)
  pure ()

testRestrictTargetSiteMismatch :: Assertion
testRestrictTargetSiteMismatch = do
  lattice <- expectRight tinyLattice
  site <- expectRight tinySite
  singletonSite <- expectRight tinyCoarseOnlySite
  galois <- expectRight (mkContextGaloisMap lattice lattice site site id id)
  wrongPresheaf <- expectRight (tinyPresheafWithFibers singletonSite (Map.singleton TinyCoarse [0]))
  case restrictFiniteContextPresheaf galois wrongPresheaf of
    Left (ContextRestrictTargetSiteMismatch expectedObjects actualObjects) -> do
      expectedObjects @?= [TinyCoarse, TinyFine]
      actualObjects @?= [TinyCoarse]
    Left failure ->
      assertFailure ("expected target site mismatch, received " <> show failure)
    Right _ ->
      assertFailure "expected target site mismatch, received restricted presheaf"

testExtendSourceSiteMismatch :: Assertion
testExtendSourceSiteMismatch = do
  lattice <- expectRight tinyLattice
  site <- expectRight tinySite
  singletonSite <- expectRight tinyCoarseOnlySite
  galois <- expectRight (mkContextGaloisMap lattice lattice site site id id)
  wrongPresheaf <- expectRight (tinyPresheafWithFibers singletonSite (Map.singleton TinyCoarse [0]))
  case extendFiniteContextPresheaf galois wrongPresheaf of
    Left (ContextExtendSourceSiteMismatch expectedObjects actualObjects) -> do
      expectedObjects @?= [TinyCoarse, TinyFine]
      actualObjects @?= [TinyCoarse]
    Left failure ->
      assertFailure ("expected source site mismatch, received " <> show failure)
    Right _ ->
      assertFailure "expected source site mismatch, received extended presheaf"

testIdentityAdjunctionReport :: Assertion
testIdentityAdjunctionReport = do
  lattice <- expectRight tinyLattice
  site <- expectRight tinySite
  galois <- expectRight (mkContextGaloisMap lattice lattice site site id id)
  presheaf <- expectRight (tinyPresheaf site)
  adjunction <- expectRight (checkContextImageAdjunction galois presheaf presheaf)
  finiteImageAdjunctionSatisfied adjunction @?= True

data TinyContext
  = TinyCoarse
  | TinyFine
  deriving stock (Eq, Ord, Show, Read)

tinyLattice :: Either String (ContextLattice TinyContext)
tinyLattice =
  mapLeft show $
    compileContextLattice
      (Set.fromList [TinyCoarse, TinyFine])
      (contextOrderDecl TinyFine TinyCoarse [(TinyCoarse, TinyFine)])

tinySite :: Either String (FiniteMeetSite TinyContext)
tinySite =
  mapLeft show $
    mkFiniteMeetSite
      FiniteMeetSiteSpec
        { fmssCells = TinyCoarse :| [TinyFine],
          fmssRefinements =
            Set.fromList
              [ (TinyFine, TinyCoarse)
              ],
          fmssCovers = Map.empty
        }

tinyCoarseOnlySite :: Either String (FiniteMeetSite TinyContext)
tinyCoarseOnlySite =
  mapLeft show $
    mkFiniteMeetSite
      FiniteMeetSiteSpec
        { fmssCells = TinyCoarse :| [],
          fmssRefinements = Set.empty,
          fmssCovers = Map.empty
        }

tinyPresheaf ::
  FiniteMeetSite TinyContext ->
  Either String (FinitePresheaf (FiniteMeetSite TinyContext) Int () VoidRestrictionFailure)
tinyPresheaf site =
  tinyPresheafWithFibers
    site
    ( Map.fromList
        [ (TinyCoarse, [0, 1]),
          (TinyFine, [0, 1])
        ]
    )

tinyPresheafWithFibers ::
  FiniteMeetSite TinyContext ->
  Map TinyContext [Int] ->
  Either String (FinitePresheaf (FiniteMeetSite TinyContext) Int () VoidRestrictionFailure)
tinyPresheafWithFibers site fibers =
  mapLeft show $
    mkFinitePresheaf site restrictAction mismatchAt normalizeAt fibers
  where
    restrictAction ::
      CheckedMorphism TinyContext (FiniteMeetMorphism TinyContext) ->
      Int ->
      Either VoidRestrictionFailure Int
    restrictAction _morphism value =
      Right value

    mismatchAt :: TinyContext -> Int -> Int -> [()]
    mismatchAt _object leftValue rightValue =
      [() | leftValue /= rightValue]

    normalizeAt :: TinyContext -> Int -> Int
    normalizeAt _object value =
      value

data VoidRestrictionFailure
  deriving stock (Eq, Show)

mapLeft :: (left -> left') -> Either left right -> Either left' right
mapLeft transform result =
  case result of
    Left failure ->
      Left (transform failure)
    Right value ->
      Right value
