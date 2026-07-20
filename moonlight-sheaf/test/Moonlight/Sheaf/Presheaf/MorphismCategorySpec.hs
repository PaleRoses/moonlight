{-# LANGUAGE DerivingStrategies #-}
module Moonlight.Sheaf.Presheaf.MorphismCategorySpec
  ( identityLawTests,
    associativityLawTests,
    compositionBoundaryTests,
  )
where

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Void (Void)
import Moonlight.Sheaf.Presheaf.Finite
  ( FinitePresheaf (..),
    finiteFiberAt,
    finiteFiberValues,
    mkFinitePresheaf,
  )
import Moonlight.Sheaf.Presheaf.Morphism
  ( FinitePresheafMorphism,
    FinitePresheafMorphismCompositionFailure (..),
    composeFinitePresheafMorphisms,
    finitePresheafMorphismComponentMap,
    finitePresheafMorphismSource,
    finitePresheafMorphismTarget,
    identityFinitePresheafMorphism,
    mkFinitePresheafMorphism,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    Site (..),
  )
import Moonlight.Sheaf.Site.Construction.FiniteMeet
  ( FiniteMeetMorphism,
    FiniteMeetSite,
    FiniteMeetSiteSpec (..),
    finiteMeetMorphism,
    mkFiniteMeetSite,
  )
import Moonlight.Sheaf.TestFixture.Assertions (expectJust, expectRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

data FixtureObject
  = FixtureCoarse
  | FixtureFine
  deriving stock (Eq, Ord, Show)

type FixturePresheaf = FinitePresheaf (FiniteMeetSite FixtureObject) Int () Void

type FixtureMorphism =
  FinitePresheafMorphism
    (FiniteMeetSite FixtureObject)
    Int
    Int
    ()
    ()
    Void
    Void

data CategoryFixture = CategoryFixture
  { categoryFixtureFirst :: !FixtureMorphism,
    categoryFixtureSecond :: !FixtureMorphism,
    categoryFixtureThird :: !FixtureMorphism
  }

identityLawTests :: TestTree
identityLawTests =
  testGroup
    "finite presheaf morphism identity"
    [ testCase "left identity preserves a finite presheaf morphism" testLeftIdentity,
      testCase "right identity preserves a finite presheaf morphism" testRightIdentity
    ]

associativityLawTests :: TestTree
associativityLawTests =
  testCase "finite presheaf morphism composition is associative" testAssociativity

compositionBoundaryTests :: TestTree
compositionBoundaryTests =
  testGroup
    "finite presheaf morphism composition boundary"
    [ testCase "category fixture has nontrivial components and restrictions" testCategoryFixtureIsNontrivial,
      testCase
        "composition rejects genuinely different middle presheaves"
        testCompositionRejectsDifferentMiddlePresheaves
    ]

testLeftIdentity :: Assertion
testLeftIdentity =
  withCategoryFixture $ \fixtureValue -> do
    let firstMorphism = categoryFixtureFirst fixtureValue
    composed <-
      expectRight
        ( composeFinitePresheafMorphisms
            (identityFinitePresheafMorphism (finitePresheafMorphismTarget firstMorphism))
            firstMorphism
        )
    assertObservedMorphismEqual firstMorphism composed

testRightIdentity :: Assertion
testRightIdentity =
  withCategoryFixture $ \fixtureValue -> do
    let firstMorphism = categoryFixtureFirst fixtureValue
    composed <-
      expectRight
        ( composeFinitePresheafMorphisms
            firstMorphism
            (identityFinitePresheafMorphism (finitePresheafMorphismSource firstMorphism))
        )
    assertObservedMorphismEqual firstMorphism composed

testAssociativity :: Assertion
testAssociativity =
  withCategoryFixture $ \fixtureValue -> do
    let firstMorphism = categoryFixtureFirst fixtureValue
    secondAfterFirst <-
      expectRight
        ( composeFinitePresheafMorphisms
            (categoryFixtureSecond fixtureValue)
            firstMorphism
        )
    leftAssociated <-
      expectRight
        ( composeFinitePresheafMorphisms
            (categoryFixtureThird fixtureValue)
            secondAfterFirst
        )
    thirdAfterSecond <-
      expectRight
        ( composeFinitePresheafMorphisms
            (categoryFixtureThird fixtureValue)
            (categoryFixtureSecond fixtureValue)
        )
    rightAssociated <-
      expectRight
        ( composeFinitePresheafMorphisms
            thirdAfterSecond
            firstMorphism
        )
    assertObservedMorphismEqual leftAssociated rightAssociated

testCategoryFixtureIsNontrivial :: Assertion
testCategoryFixtureIsNontrivial =
  withCategoryFixture $ \fixtureValue -> do
    let firstMorphism = categoryFixtureFirst fixtureValue
        sourcePresheaf = finitePresheafMorphismSource firstMorphism
    Map.lookup FixtureCoarse (finitePresheafMorphismComponentMap firstMorphism)
      @?= Just (Map.fromList [(0, 0), (1, 0), (2, 1)])
    restrictionMorphism <-
      expectJust
        ( finiteMeetMorphism
            (fpSite sourcePresheaf)
            FixtureFine
            FixtureCoarse
        )
    fpRestrict sourcePresheaf restrictionMorphism 2 @?= Right 1

testCompositionRejectsDifferentMiddlePresheaves :: Assertion
testCompositionRejectsDifferentMiddlePresheaves =
  withCategoryFixture $ \fixtureValue -> do
    let innerMiddlePresheaf =
          finitePresheafMorphismSource (categoryFixtureFirst fixtureValue)
        outerMiddlePresheaf =
          finitePresheafMorphismTarget (categoryFixtureSecond fixtureValue)
    case
        composeFinitePresheafMorphisms
          (identityFinitePresheafMorphism outerMiddlePresheaf)
          (identityFinitePresheafMorphism innerMiddlePresheaf)
      of
        Left
          ( FinitePresheafMorphismCompositionMiddleRestrictionMismatch
              morphismValue
              middleValue
              innerValue
              outerValue
            ) -> do
          cmSource morphismValue @?= FixtureFine
          cmTarget morphismValue @?= FixtureCoarse
          middleValue @?= 0
          innerValue @?= 0
          outerValue @?= 1
        Left otherFailure ->
          assertFailure ("expected middle-restriction mismatch, received " <> show otherFailure)
        Right _ ->
          assertFailure "expected middle-restriction mismatch, received composed morphism"

withCategoryFixture :: (CategoryFixture -> Assertion) -> Assertion
withCategoryFixture continue =
  expectRight categoryFixture >>= continue

categoryFixture :: Either String CategoryFixture
categoryFixture = do
  siteValue <- first show (mkFiniteMeetSite fixtureSiteSpec)
  sourcePresheaf <-
    mkFixturePresheaf
      siteValue
      restrictSource
      (Map.fromList [(FixtureCoarse, [0, 1, 2]), (FixtureFine, [0, 1])])
  firstMiddlePresheaf <-
    mkFixturePresheaf
      siteValue
      restrictFirstMiddle
      (Map.fromList [(FixtureCoarse, [0, 1]), (FixtureFine, [0])])
  secondMiddlePresheaf <-
    mkFixturePresheaf
      siteValue
      restrictSecondMiddle
      (Map.fromList [(FixtureCoarse, [0, 1, 2]), (FixtureFine, [0, 1])])
  targetPresheaf <-
    mkFixturePresheaf
      siteValue
      restrictTarget
      (Map.fromList [(FixtureCoarse, [0, 1]), (FixtureFine, [0])])
  firstMorphism <-
    first show (mkFinitePresheafMorphism sourcePresheaf firstMiddlePresheaf firstComponent)
  secondMorphism <-
    first show (mkFinitePresheafMorphism firstMiddlePresheaf secondMiddlePresheaf secondComponent)
  thirdMorphism <-
    first show (mkFinitePresheafMorphism secondMiddlePresheaf targetPresheaf thirdComponent)
  pure
    CategoryFixture
      { categoryFixtureFirst = firstMorphism,
        categoryFixtureSecond = secondMorphism,
        categoryFixtureThird = thirdMorphism
      }

fixtureSiteSpec :: FiniteMeetSiteSpec FixtureObject
fixtureSiteSpec =
  FiniteMeetSiteSpec
    { fmssCells = FixtureCoarse :| [FixtureFine],
      fmssRefinements = Set.singleton (FixtureFine, FixtureCoarse),
      fmssCovers = Map.empty
    }

mkFixturePresheaf ::
  FiniteMeetSite FixtureObject ->
  (CheckedMorphism FixtureObject (FiniteMeetMorphism FixtureObject) -> Int -> Either Void Int) ->
  Map FixtureObject [Int] ->
  Either String FixturePresheaf
mkFixturePresheaf siteValue restrictValue fibers =
  first show $
    mkFinitePresheaf
      siteValue
      restrictValue
      (\_objectValue leftValue rightValue -> [() | leftValue /= rightValue])
      (\_objectValue value -> value)
      fibers

restrictSource ::
  CheckedMorphism FixtureObject (FiniteMeetMorphism FixtureObject) ->
  Int ->
  Either Void Int
restrictSource morphismValue value
  | isNonIdentityRestriction morphismValue = Right (min 1 value)
  | otherwise = Right value

restrictFirstMiddle ::
  CheckedMorphism FixtureObject (FiniteMeetMorphism FixtureObject) ->
  Int ->
  Either Void Int
restrictFirstMiddle morphismValue value
  | isNonIdentityRestriction morphismValue = Right 0
  | otherwise = Right value

restrictSecondMiddle ::
  CheckedMorphism FixtureObject (FiniteMeetMorphism FixtureObject) ->
  Int ->
  Either Void Int
restrictSecondMiddle morphismValue value
  | isNonIdentityRestriction morphismValue = Right 1
  | otherwise = Right value

restrictTarget ::
  CheckedMorphism FixtureObject (FiniteMeetMorphism FixtureObject) ->
  Int ->
  Either Void Int
restrictTarget morphismValue value
  | isNonIdentityRestriction morphismValue = Right 0
  | otherwise = Right value

isNonIdentityRestriction ::
  CheckedMorphism FixtureObject (FiniteMeetMorphism FixtureObject) ->
  Bool
isNonIdentityRestriction morphismValue =
  cmSource morphismValue == FixtureFine
    && cmTarget morphismValue == FixtureCoarse

firstComponent :: FixtureObject -> Int -> Either Void Int
firstComponent objectValue value =
  Right $
    case objectValue of
      FixtureCoarse -> value `div` 2
      FixtureFine -> 0

secondComponent :: FixtureObject -> Int -> Either Void Int
secondComponent objectValue value =
  Right $
    case objectValue of
      FixtureCoarse -> value * 2
      FixtureFine -> 1

thirdComponent :: FixtureObject -> Int -> Either Void Int
thirdComponent objectValue value =
  Right $
    case objectValue of
      FixtureCoarse -> value `div` 2
      FixtureFine -> 0

assertObservedMorphismEqual :: FixtureMorphism -> FixtureMorphism -> Assertion
assertObservedMorphismEqual expectedMorphism actualMorphism = do
  finitePresheafMorphismComponentMap actualMorphism
    @?= finitePresheafMorphismComponentMap expectedMorphism
  assertObservedPresheafEqual
    (finitePresheafMorphismSource expectedMorphism)
    (finitePresheafMorphismSource actualMorphism)
  assertObservedPresheafEqual
    (finitePresheafMorphismTarget expectedMorphism)
    (finitePresheafMorphismTarget actualMorphism)

assertObservedPresheafEqual :: FixturePresheaf -> FixturePresheaf -> Assertion
assertObservedPresheafEqual expectedPresheaf actualPresheaf = do
  observedObjects actualPresheaf @?= observedObjects expectedPresheaf
  observedFiberValues actualPresheaf @?= observedFiberValues expectedPresheaf

observedObjects :: FixturePresheaf -> [FixtureObject]
observedObjects =
  siteObjects . fpSite

observedFiberValues :: FixturePresheaf -> [(FixtureObject, Maybe [Int])]
observedFiberValues presheafValue =
  fmap
    (\objectValue -> (objectValue, finiteFiberValues <$> finiteFiberAt objectValue presheafValue))
    (observedObjects presheafValue)
