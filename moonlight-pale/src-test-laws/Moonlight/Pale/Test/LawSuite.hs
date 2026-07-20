{-# LANGUAGE ExistentialQuantification #-}

module Moonlight.Pale.Test.LawSuite
  ( LawSuite,
    LawBundle,
    QuickCheckLaw,
    LawSuiteBundle,
    QuickCheckLawDefinition,
    SomeQuickCheckLawDefinition,
    HedgehogLawDefinition,
    SomeHedgehogLawDefinition,
    QuickCheckLawBundle,
    quickCheckLawBundle,
    renderQuickCheckLawBundle,
    quickCheckLawBundleGroup,
    lawBundleQuickCheck,
    lawBundleHedgehog,
    hedgehogLawDefinition,
    suffixedHedgehogLawDefinition,
    hedgehogLawDefinitions,
    quickCheckLawDefinition,
    suffixedQuickCheckLawDefinition,
    quickCheckLawDefinitions,
    quickCheckLawGroupDefinitions,
    renderedLawBundle,
    renderLawBundle,
    renderLawBundles,
    quickCheckLaw,
    namedQuickCheckLaw,
    qualifyLawName,
    suffixedQuickCheckLaw,
    namedQuickCheckLawWithSuffix,
    hedgehogLaw,
    namedHedgehogLaw,
    suffixedHedgehogLaw,
    namedHedgehogLawWithSuffix,
    hUnitLaw,
    testTreeLaw,
    lawGroup,
    lawSuiteBundle,
    bundleLawGroup,
    lawSuiteGroup,
    lawSuiteBundleGroup,
    renderLawSuite,
    renderLawSuiteBundle,
    quickCheckLawGroup,
  )
where

import qualified Hedgehog as HH
import Data.Kind (Type)
import Moonlight.Core (IsLawName (..))
import Moonlight.Pale.Test.Section.Property (etaHedgehog)
import Prelude (Bool, Show, String, map, (.), (<>))
import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.Hedgehog as TH
import Test.Tasty.HUnit (Assertion, testCase)
import qualified Test.Tasty.QuickCheck as QC

type LawSuite :: Type
data LawSuite
  = QuickCheckLawCase String QC.Property
  | HedgehogLawCase String HH.Property
  | HUnitLawCase String Assertion
  | EmbeddedTestTree TestTree
  | LawGroup String [LawSuite]

type LawSuiteBundle :: Type -> Type
data LawSuiteBundle label = LawSuiteBundle label [LawSuite]
type QuickCheckLawBundle :: Type -> Type -> Type
data QuickCheckLawBundle label lawName = QuickCheckLawBundle label [QuickCheckLawDefinition lawName]
type LawBundle :: Type -> Type
data LawBundle label
  = forall lawName. IsLawName lawName => QuickCheckBundle label [QuickCheckLawDefinition lawName]
  | forall lawName. IsLawName lawName => HedgehogBundle label [HedgehogLawDefinition lawName]
  | RenderedBundle label [LawSuite]

type QuickCheckLaw :: Type
type QuickCheckLaw = LawSuite

type SomeQuickCheckLawDefinition :: Type -> Type
data SomeQuickCheckLawDefinition lawName
  = forall prop. QC.Testable prop => QuickCheckLawDefinition lawName prop
  | forall prop. QC.Testable prop => SuffixedQuickCheckLawDefinition lawName String prop

type QuickCheckLawDefinition :: Type -> Type
type QuickCheckLawDefinition lawName = SomeQuickCheckLawDefinition lawName

type SomeHedgehogLawDefinition :: Type -> Type
data SomeHedgehogLawDefinition lawName
  = forall a. Show a => HedgehogLawDefinition lawName (HH.Gen a) (a -> Bool)
  | forall a. Show a => SuffixedHedgehogLawDefinition lawName String (HH.Gen a) (a -> Bool)

type HedgehogLawDefinition :: Type -> Type
type HedgehogLawDefinition lawName = SomeHedgehogLawDefinition lawName

quickCheckLawBundle :: label -> [QuickCheckLawDefinition lawName] -> QuickCheckLawBundle label lawName
quickCheckLawBundle = QuickCheckLawBundle

renderedLawBundle :: label -> [LawSuite] -> LawBundle label
renderedLawBundle = RenderedBundle

lawBundleQuickCheck :: IsLawName lawName => label -> [QuickCheckLawDefinition lawName] -> LawBundle label
lawBundleQuickCheck = QuickCheckBundle

lawBundleHedgehog :: IsLawName lawName => label -> [HedgehogLawDefinition lawName] -> LawBundle label
lawBundleHedgehog = HedgehogBundle

quickCheckLawDefinition :: QC.Testable prop => lawName -> prop -> QuickCheckLawDefinition lawName
quickCheckLawDefinition lawName =
  QuickCheckLawDefinition lawName

suffixedQuickCheckLawDefinition :: QC.Testable prop => lawName -> String -> prop -> QuickCheckLawDefinition lawName
suffixedQuickCheckLawDefinition lawName suffix =
  SuffixedQuickCheckLawDefinition lawName suffix

quickCheckLawDefinitions :: IsLawName lawName => [QuickCheckLawDefinition lawName] -> [LawSuite]
quickCheckLawDefinitions =
  map renderQuickCheckLawDefinition

hedgehogLawDefinition :: Show a => lawName -> HH.Gen a -> (a -> Bool) -> HedgehogLawDefinition lawName
hedgehogLawDefinition lawName generator =
  HedgehogLawDefinition lawName generator

suffixedHedgehogLawDefinition :: Show a => lawName -> String -> HH.Gen a -> (a -> Bool) -> HedgehogLawDefinition lawName
suffixedHedgehogLawDefinition lawName suffix generator =
  SuffixedHedgehogLawDefinition lawName suffix generator

hedgehogLawDefinitions :: IsLawName lawName => [HedgehogLawDefinition lawName] -> [LawSuite]
hedgehogLawDefinitions =
  map renderHedgehogLawDefinition

renderQuickCheckLawBundle :: IsLawName lawName => (label -> String) -> QuickCheckLawBundle label lawName -> LawSuite
renderQuickCheckLawBundle renderLabel (QuickCheckLawBundle label definitions) =
  lawGroup (renderLabel label) (quickCheckLawDefinitions definitions)

quickCheckLawBundleGroup :: IsLawName lawName => String -> (label -> String) -> [QuickCheckLawBundle label lawName] -> LawSuite
quickCheckLawBundleGroup groupLabel renderLabel =
  lawGroup groupLabel . map (renderQuickCheckLawBundle renderLabel)

renderQuickCheckLawDefinition :: IsLawName lawName => QuickCheckLawDefinition lawName -> LawSuite
renderQuickCheckLawDefinition lawDefinition =
  case lawDefinition of
    QuickCheckLawDefinition lawName propertyValue ->
      namedQuickCheckLaw lawName propertyValue
    SuffixedQuickCheckLawDefinition lawName suffix propertyValue ->
      namedQuickCheckLawWithSuffix lawName suffix propertyValue

quickCheckLawGroupDefinitions :: IsLawName lawName => String -> [QuickCheckLawDefinition lawName] -> LawSuite
quickCheckLawGroupDefinitions groupLabel =
  lawGroup groupLabel . quickCheckLawDefinitions

renderHedgehogLawDefinition :: IsLawName lawName => HedgehogLawDefinition lawName -> LawSuite
renderHedgehogLawDefinition lawDefinition =
  case lawDefinition of
    HedgehogLawDefinition lawName generator predicate ->
      namedHedgehogLaw lawName generator predicate
    SuffixedHedgehogLawDefinition lawName suffix generator predicate ->
      namedHedgehogLawWithSuffix lawName suffix generator predicate

renderLawBundle :: (label -> String) -> LawBundle label -> LawSuite
renderLawBundle renderLabel lawBundleValue =
  case lawBundleValue of
    QuickCheckBundle label definitions ->
      lawGroup (renderLabel label) (quickCheckLawDefinitions definitions)
    HedgehogBundle label definitions ->
      lawGroup (renderLabel label) (hedgehogLawDefinitions definitions)
    RenderedBundle label nestedLaws ->
      lawGroup (renderLabel label) nestedLaws

renderLawBundles :: (label -> String) -> [LawBundle label] -> [LawSuite]
renderLawBundles renderLabel =
  map (renderLawBundle renderLabel)

quickCheckLaw :: QC.Testable prop => String -> prop -> LawSuite
quickCheckLaw lawLabel lawPredicate =
  QuickCheckLawCase lawLabel (QC.property lawPredicate)

namedQuickCheckLaw :: (IsLawName lawName, QC.Testable prop) => lawName -> prop -> LawSuite
namedQuickCheckLaw lawName =
  quickCheckLaw (lawNameText lawName)

qualifyLawName :: String -> String -> String
qualifyLawName lawLabel suffix =
  lawLabel <> "_" <> suffix

suffixedQuickCheckLaw :: QC.Testable prop => String -> String -> prop -> LawSuite
suffixedQuickCheckLaw lawLabel suffix =
  quickCheckLaw (qualifyLawName lawLabel suffix)

namedQuickCheckLawWithSuffix ::
  (IsLawName lawName, QC.Testable prop) =>
  lawName ->
  String ->
  prop ->
  LawSuite
namedQuickCheckLawWithSuffix lawName suffix =
  suffixedQuickCheckLaw (lawNameText lawName) suffix

hedgehogLaw :: Show a => String -> HH.Gen a -> (a -> Bool) -> LawSuite
hedgehogLaw lawLabel generator predicate =
  HedgehogLawCase lawLabel (etaHedgehog generator predicate)

namedHedgehogLaw :: (IsLawName lawName, Show a) => lawName -> HH.Gen a -> (a -> Bool) -> LawSuite
namedHedgehogLaw lawName =
  hedgehogLaw (lawNameText lawName)

suffixedHedgehogLaw :: Show a => String -> String -> HH.Gen a -> (a -> Bool) -> LawSuite
suffixedHedgehogLaw lawLabel suffix =
  hedgehogLaw (qualifyLawName lawLabel suffix)

namedHedgehogLawWithSuffix ::
  (IsLawName lawName, Show a) =>
  lawName ->
  String ->
  HH.Gen a ->
  (a -> Bool) ->
  LawSuite
namedHedgehogLawWithSuffix lawName suffix =
  suffixedHedgehogLaw (lawNameText lawName) suffix

hUnitLaw :: String -> Assertion -> LawSuite
hUnitLaw = HUnitLawCase

testTreeLaw :: TestTree -> LawSuite
testTreeLaw = EmbeddedTestTree

lawGroup :: String -> [LawSuite] -> LawSuite
lawGroup = LawGroup

lawSuiteBundle :: label -> [LawSuite] -> LawSuiteBundle label
lawSuiteBundle = LawSuiteBundle

bundleLawGroup :: (label -> String) -> LawSuiteBundle label -> LawSuite
bundleLawGroup renderLabel (LawSuiteBundle label nestedLaws) =
  lawGroup (renderLabel label) nestedLaws

renderLawSuite :: LawSuite -> TestTree
renderLawSuite lawSuiteValue =
  case lawSuiteValue of
    QuickCheckLawCase lawLabel propertyValue ->
      QC.testProperty lawLabel propertyValue
    HedgehogLawCase lawLabel propertyValue ->
      TH.testProperty lawLabel propertyValue
    HUnitLawCase lawLabel assertion ->
      testCase lawLabel assertion
    EmbeddedTestTree testTreeValue ->
      testTreeValue
    LawGroup groupLabel nestedLaws ->
      testGroup groupLabel (map renderLawSuite nestedLaws)

renderLawSuiteBundle :: (label -> String) -> LawSuiteBundle label -> TestTree
renderLawSuiteBundle renderLabel =
  renderLawSuite . bundleLawGroup renderLabel

lawSuiteGroup :: String -> [LawSuite] -> TestTree
lawSuiteGroup groupLabel =
  renderLawSuite . lawGroup groupLabel

lawSuiteBundleGroup :: String -> (label -> String) -> [LawSuiteBundle label] -> TestTree
lawSuiteBundleGroup groupLabel renderLabel =
  lawSuiteGroup groupLabel . map (bundleLawGroup renderLabel)

quickCheckLawGroup :: String -> [QuickCheckLaw] -> TestTree
quickCheckLawGroup = lawSuiteGroup
