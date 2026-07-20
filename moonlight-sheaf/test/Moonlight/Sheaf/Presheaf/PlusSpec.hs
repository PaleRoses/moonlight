{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Presheaf.PlusSpec
  ( tests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Moonlight.Sheaf.Presheaf.Finite
  ( FinitePresheaf (..),
    FinitePresheafFailure (..),
    finiteFiberAt,
    finiteFiberValues,
    mkFinitePresheaf,
    validateFinitePresheafLaws,
  )
import Moonlight.Sheaf.Presheaf.Morphism
  ( FinitePresheafMorphism,
    FinitePresheafMorphismCompositionFailure (..),
    FinitePresheafMorphismFailure (..),
    composeFinitePresheafMorphisms,
    finitePresheafMorphismComponentAt,
    finitePresheafMorphismComponents,
    mkFinitePresheafMorphism,
  )
import Moonlight.Sheaf.Presheaf.Enumeration
  ( FiniteEnumerationBudget (..),
  )
import Moonlight.Sheaf.Presheaf.Plus
  ( PlusConstructionFailure (..),
    PlusEnumerationCost (..),
    plusAsFinitePresheaf,
    plusConstruction,
    plusUnitClass,
  )
import Moonlight.Sheaf.Sheafification.Finite
  ( FinitePlusUnitEvidence (..),
    SheafConditionReport (..),
    Sheafification (..),
    SheafificationUnitEvidence (..),
    UnitInjectivityFailure (..),
    UnitSurjectivityFailure (..),
    associatedSheafificationReport,
    checkFiniteSheafCondition,
    finiteSheafWitnessPresheaf,
    finiteSheafWitnessReport,
    finiteSheafificationAssociated,
    finiteSheafificationAssociatedWitness,
    finiteSheafificationReflectorResult,
    finiteSheafificationUnit,
    sheafificationUnitEvidence,
    sheafConditionReportAccepted,
    sheafifyFinitePresheaf,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
    CoverConstructionError,
    CoveringFamily,
    PullbackSquare (..),
    Site (..),
    mkCoveringFamily,
  )
import Moonlight.Sheaf.Site.CoverBasis.Finite
  ( finiteCanonicalCoverPlan,
    finiteIdentityCoverAt,
    finitePullbackCoverPlan,
    mkFiniteCoverBasis,
  )
import Moonlight.Sheaf.Site.Plan
  ( canonicalizePulledCoverPlan,
    pcpPulledCover,
    preparePullbackCoverPlan,
  )
import Moonlight.Sheaf.TestFixture.Assertions (expectRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "plus construction"
    [ testCase "identity-only basis makes F+ a finite presheaf with base classes" testIdentityOnlyPlusPresheaf,
      testCase "plus-class restriction pulls representatives through indexed pullbacks" testPlusRestrictionPullsBackRepresentative,
      testCase "finite cover basis caches pullback plans" testFiniteCoverBasisCachesPullbackPlans,
      testCase "prepared plus presheaves pass the cold finite law audit" testPreparedPlusPresheavesPassColdLawAudit,
      testCase "F+ is separated before it is necessarily effective" testPlusPresheafIsSeparated,
      testCase "non-separated presheaf reports unit injectivity failure" testNonSeparatedReport,
      testCase "separated non-effective presheaf reports unit surjectivity failure" testSeparatedNonEffectiveReport,
      testCase "finite sheaf reports no sheaf-condition failures" testSheafConditionSatisfied,
      testCase "finite presheaf morphism rejects non-natural component families" testFinitePresheafMorphismRejectsNonNaturalComponents,
      testCase "finite sheafification proves associated presheaf is a sheaf" testSheafificationAssociatedReportAccepted,
      testCase "finite sheafification carries first-unit morphism evidence" testSheafificationCarriesFirstUnitMorphism,
      testCase "finite sheafification reflector composes first and second unit morphisms" testReflectorCompositeUnitEqualsUnitChain,
      testCase "finite sheafification reflector carries associated sheaf witness" testReflectorCarriesAssociatedWitness,
      testCase "finite sheafification reflector accepts non-sheaf input" testReflectorAcceptsNonSheafInput,
      testCase "non-separated sheafification records first-unit obstruction" testSheafificationRecordsNonSeparatedFirstUnit,
      testCase "non-effective sheafification records first-unit surjectivity" testSheafificationRecordsNonEffectiveFirstUnit,
      testCase "sheafification field names distinguish separated and associated" testSheafificationFieldNamesAreHonest,
      testCase "finite presheaf morphism composition rejects incompatible middle presheaves" testFinitePresheafMorphismCompositionRejectsIncompatibleMiddle,
      testCase "plus enumeration budget rejects raw candidate explosion" testBudgetRejectsRawCandidateExplosion
    ]

testIdentityOnlyPlusPresheaf :: Assertion
testIdentityOnlyPlusPresheaf = do
  let siteValue = SimpleSite IdentityOnlyTopology
  basis <- expectRight (mkFiniteCoverBasis siteValue)
  presheaf <- expectRight (mkSimplePresheaf siteValue [RootSection 0] [LeafSection 0])
  plusValue <- expectRight (plusConstruction unboundedBudget basis presheaf)
  plusPresheaf <- expectRight (plusAsFinitePresheaf plusValue)
  rootUnit <- expectRight (plusUnitClass plusValue Root (RootSection 0))
  leafUnit <- expectRight (plusUnitClass plusValue Leaf (LeafSection 0))
  rootPlusValues <- expectFiberValuesAt Root plusPresheaf
  leafPlusValues <- expectFiberValuesAt Leaf plusPresheaf
  rootPlusValues @?= [rootUnit]
  leafPlusValues @?= [leafUnit]

testPlusRestrictionPullsBackRepresentative :: Assertion
testPlusRestrictionPullsBackRepresentative = do
  basis <- expectRight (mkFiniteCoverBasis coveredSimpleSite)
  presheaf <- expectRight (mkSimplePresheaf coveredSimpleSite [RootSection 0] [LeafSection 0])
  plusValue <- expectRight (plusConstruction unboundedBudget basis presheaf)
  plusPresheaf <- expectRight (plusAsFinitePresheaf plusValue)
  rootUnit <- expectRight (plusUnitClass plusValue Root (RootSection 0))
  leafUnit <- expectRight (plusUnitClass plusValue Leaf (LeafSection 0))
  restrictedClass <- expectRight (fpRestrict plusPresheaf leafToRoot rootUnit)
  restrictedClass @?= leafUnit

testFiniteCoverBasisCachesPullbackPlans :: Assertion
testFiniteCoverBasisCachesPullbackPlans = do
  basis <- expectRight (mkFiniteCoverBasis coveredSimpleSite)
  rootIdentity <- expectRight (finiteIdentityCoverAt basis Root)
  cachedPlan <- expectRight (finitePullbackCoverPlan basis leafToRoot rootIdentity)
  preparedPlan <- expectRight (preparePullbackCoverPlan coveredSimpleSite leafToRoot rootIdentity)
  canonicalPulledCover <- expectRight (finiteCanonicalCoverPlan basis (pcpPulledCover preparedPlan))
  canonicalPreparedPlan <- expectRight (canonicalizePulledCoverPlan canonicalPulledCover preparedPlan)
  cachedPlan @?= canonicalPreparedPlan

testPreparedPlusPresheavesPassColdLawAudit :: Assertion
testPreparedPlusPresheavesPassColdLawAudit = do
  basis <- expectRight (mkFiniteCoverBasis coveredSimpleSite)
  presheaf <- expectRight (mkSimplePresheaf coveredSimpleSite [RootSection 0] [LeafSection 0])
  sheafification <- expectRight (sheafifyFinitePresheaf unboundedBudget basis presheaf)
  expectRight (validateFinitePresheafLaws (sheafificationSeparated sheafification))
  expectRight (validateFinitePresheafLaws (sheafificationAssociated sheafification))

testPlusPresheafIsSeparated :: Assertion
testPlusPresheafIsSeparated = do
  basis <- expectRight (mkFiniteCoverBasis coveredSimpleSite)
  presheaf <- expectRight (mkSimplePresheaf coveredSimpleSite [RootSection 0, RootSection 1] [LeafSection 0])
  plusValue <- expectRight (plusConstruction unboundedBudget basis presheaf)
  plusPresheaf <- expectRight (plusAsFinitePresheaf plusValue)
  reportValue <- expectRight (checkFiniteSheafCondition unboundedBudget basis plusPresheaf)
  scrInjectivityFailures reportValue @?= []
  scrSeparationFailures reportValue @?= []

testNonSeparatedReport :: Assertion
testNonSeparatedReport = do
  reportValue <-
    sheafConditionReportFor
      coveredSimpleSite
      [RootSection 0, RootSection 1]
      [LeafSection 0]
  case scrInjectivityFailures reportValue of
    [failure] -> do
      uifObject failure @?= Root
      assertBool "expected global mismatch evidence" (not (null (uifGlobalMismatches failure)))
    otherFailures ->
      assertFailure ("expected one injectivity failure, received " <> show otherFailures)
  scrSurjectivityFailures reportValue @?= []

testSeparatedNonEffectiveReport :: Assertion
testSeparatedNonEffectiveReport = do
  reportValue <-
    sheafConditionReportFor
      coveredSimpleSite
      []
      [LeafSection 0]
  case scrSurjectivityFailures reportValue of
    [failure] ->
      usfObject failure @?= Root
    otherFailures ->
      assertFailure ("expected one surjectivity failure, received " <> show otherFailures)
  scrInjectivityFailures reportValue @?= []
  scrSeparationFailures reportValue @?= []

testSheafConditionSatisfied :: Assertion
testSheafConditionSatisfied = do
  reportValue <-
    sheafConditionReportFor
      coveredSimpleSite
      [RootSection 0]
      [LeafSection 0]
  scrInjectivityFailures reportValue @?= []
  scrSurjectivityFailures reportValue @?= []
  scrSeparationFailures reportValue @?= []

testFinitePresheafMorphismRejectsNonNaturalComponents :: Assertion
testFinitePresheafMorphismRejectsNonNaturalComponents = do
  sourcePresheaf <- expectRight (mkSimplePresheaf coveredSimpleSite [RootSection 0] [LeafSection 0])
  targetPresheaf <- expectRight (mkSimplePresheaf coveredSimpleSite [RootSection 0] [LeafSection 0, LeafSection 1])
  case mkFinitePresheafMorphism sourcePresheaf targetPresheaf unnaturalSimpleComponent of
    Left
      ( FinitePresheafMorphismNaturalityMismatch
          morphismValue
          sourceValueAtTarget
          sourceRestricted
          targetAfterSourceRestriction
          targetRestricted
          mismatches
        ) -> do
        morphismValue @?= leafToRoot
        sourceValueAtTarget @?= RootSection 0
        sourceRestricted @?= LeafSection 0
        targetAfterSourceRestriction @?= LeafSection 1
        targetRestricted @?= LeafSection 0
        assertBool "expected target mismatch evidence" (not (null mismatches))
    Left otherFailure ->
      assertFailure ("expected naturality failure, received " <> show otherFailure)
    Right _ ->
      assertFailure "expected naturality failure, received finite presheaf morphism"

testSheafificationAssociatedReportAccepted :: Assertion
testSheafificationAssociatedReportAccepted = do
  basis <- expectRight (mkFiniteCoverBasis coveredSimpleSite)
  presheaf <- expectRight (mkSimplePresheaf coveredSimpleSite [RootSection 0] [LeafSection 0])
  sheafification <- expectRight (sheafifyFinitePresheaf unboundedBudget basis presheaf)
  associatedReport <- expectRight (associatedSheafificationReport unboundedBudget basis sheafification)
  assertReportAccepted associatedReport

testSheafificationCarriesFirstUnitMorphism :: Assertion
testSheafificationCarriesFirstUnitMorphism = do
  basis <- expectRight (mkFiniteCoverBasis coveredSimpleSite)
  presheaf <- expectRight (mkSimplePresheaf coveredSimpleSite [RootSection 0] [LeafSection 0])
  sheafification <- expectRight (sheafifyFinitePresheaf unboundedBudget basis presheaf)
  unitEvidence <- expectRight (sheafificationUnitEvidence basis sheafification)
  rootUnit <- expectRight (plusUnitClass (sheafificationFirstPlusConstruction sheafification) Root (RootSection 0))
  leafUnit <- expectRight (plusUnitClass (sheafificationFirstPlusConstruction sheafification) Leaf (LeafSection 0))
  let unitMorphism =
        finitePlusUnitMorphism (sheafificationFirstUnit unitEvidence)
      unitComponents =
        finitePresheafMorphismComponents unitMorphism
  finitePresheafMorphismComponentAt Root (RootSection 0) unitMorphism @?= Just rootUnit
  finitePresheafMorphismComponentAt Leaf (LeafSection 0) unitMorphism @?= Just leafUnit
  Map.lookup Root unitComponents @?= Just [(RootSection 0, rootUnit)]
  Map.lookup Leaf unitComponents @?= Just [(LeafSection 0, leafUnit)]

testReflectorCompositeUnitEqualsUnitChain :: Assertion
testReflectorCompositeUnitEqualsUnitChain = do
  basis <- expectRight (mkFiniteCoverBasis coveredSimpleSite)
  presheaf <- expectRight (mkSimplePresheaf coveredSimpleSite [RootSection 0] [LeafSection 0])
  sheafification <- expectRight (sheafifyFinitePresheaf unboundedBudget basis presheaf)
  unitEvidence <- expectRight (sheafificationUnitEvidence basis sheafification)
  reflectorResult <- expectRight (finiteSheafificationReflectorResult unboundedBudget basis presheaf)
  assertCompositeUnitAt
    Root
    (RootSection 0)
    (finitePlusUnitMorphism (sheafificationFirstUnit unitEvidence))
    (finitePlusUnitMorphism (sheafificationSecondUnit unitEvidence))
    (finiteSheafificationUnit reflectorResult)
  assertCompositeUnitAt
    Leaf
    (LeafSection 0)
    (finitePlusUnitMorphism (sheafificationFirstUnit unitEvidence))
    (finitePlusUnitMorphism (sheafificationSecondUnit unitEvidence))
    (finiteSheafificationUnit reflectorResult)

testReflectorCarriesAssociatedWitness :: Assertion
testReflectorCarriesAssociatedWitness = do
  basis <- expectRight (mkFiniteCoverBasis coveredSimpleSite)
  presheaf <- expectRight (mkSimplePresheaf coveredSimpleSite [RootSection 0] [LeafSection 0])
  reflectorResult <- expectRight (finiteSheafificationReflectorResult unboundedBudget basis presheaf)
  let associatedWitness =
        finiteSheafificationAssociatedWitness reflectorResult
  assertReportAccepted (finiteSheafWitnessReport associatedWitness)
  assertFiberValuesEqual Root (finiteSheafWitnessPresheaf associatedWitness) (finiteSheafificationAssociated reflectorResult)
  assertFiberValuesEqual Leaf (finiteSheafWitnessPresheaf associatedWitness) (finiteSheafificationAssociated reflectorResult)

testReflectorAcceptsNonSheafInput :: Assertion
testReflectorAcceptsNonSheafInput = do
  basis <- expectRight (mkFiniteCoverBasis coveredSimpleSite)
  nonSheafPresheaf <- expectRight (mkSimplePresheaf coveredSimpleSite [RootSection 0, RootSection 1] [LeafSection 0])
  reflectorResult <- expectRight (finiteSheafificationReflectorResult unboundedBudget basis nonSheafPresheaf)
  assertReportAccepted (finiteSheafWitnessReport (finiteSheafificationAssociatedWitness reflectorResult))

testSheafificationRecordsNonSeparatedFirstUnit :: Assertion
testSheafificationRecordsNonSeparatedFirstUnit = do
  basis <- expectRight (mkFiniteCoverBasis coveredSimpleSite)
  presheaf <- expectRight (mkSimplePresheaf coveredSimpleSite [RootSection 0, RootSection 1] [LeafSection 0])
  sheafification <- expectRight (sheafifyFinitePresheaf unboundedBudget basis presheaf)
  unitEvidence <- expectRight (sheafificationUnitEvidence basis sheafification)
  case scrInjectivityFailures (finitePlusUnitReport (sheafificationFirstUnit unitEvidence)) of
    [failure] -> do
      uifObject failure @?= Root
      assertBool "expected global mismatch evidence" (not (null (uifGlobalMismatches failure)))
    otherFailures ->
      assertFailure ("expected one first-unit injectivity failure, received " <> show otherFailures)
  associatedReport <- expectRight (associatedSheafificationReport unboundedBudget basis sheafification)
  assertReportAccepted associatedReport

testSheafificationRecordsNonEffectiveFirstUnit :: Assertion
testSheafificationRecordsNonEffectiveFirstUnit = do
  basis <- expectRight (mkFiniteCoverBasis coveredSimpleSite)
  presheaf <- expectRight (mkSimplePresheaf coveredSimpleSite [] [LeafSection 0])
  sheafification <- expectRight (sheafifyFinitePresheaf unboundedBudget basis presheaf)
  unitEvidence <- expectRight (sheafificationUnitEvidence basis sheafification)
  case scrSurjectivityFailures (finitePlusUnitReport (sheafificationFirstUnit unitEvidence)) of
    [failure] ->
      usfObject failure @?= Root
    otherFailures ->
      assertFailure ("expected one first-unit surjectivity failure, received " <> show otherFailures)
  assertReportAccepted (finitePlusUnitReport (sheafificationSecondUnit unitEvidence))
  associatedReport <- expectRight (associatedSheafificationReport unboundedBudget basis sheafification)
  assertReportAccepted associatedReport

testSheafificationFieldNamesAreHonest :: Assertion
testSheafificationFieldNamesAreHonest = do
  basis <- expectRight (mkFiniteCoverBasis coveredSimpleSite)
  presheaf <- expectRight (mkSimplePresheaf coveredSimpleSite [RootSection 0] [LeafSection 0])
  sheafification <- expectRight (sheafifyFinitePresheaf unboundedBudget basis presheaf)
  separatedFromFirstPlus <-
    expectRight (plusAsFinitePresheaf (sheafificationFirstPlusConstruction sheafification))
  associatedFromSecondPlus <-
    expectRight (plusAsFinitePresheaf (sheafificationSecondPlusConstruction sheafification))
  assertFiberValuesEqual Root separatedFromFirstPlus (sheafificationSeparated sheafification)
  assertFiberValuesEqual Leaf separatedFromFirstPlus (sheafificationSeparated sheafification)
  assertFiberValuesEqual Root associatedFromSecondPlus (sheafificationAssociated sheafification)
  assertFiberValuesEqual Leaf associatedFromSecondPlus (sheafificationAssociated sheafification)

testFinitePresheafMorphismCompositionRejectsIncompatibleMiddle :: Assertion
testFinitePresheafMorphismCompositionRejectsIncompatibleMiddle = do
  innerMiddle <- expectRight (mkSimplePresheaf coveredSimpleSite [RootSection 0] [LeafSection 0])
  outerMiddle <- expectRight (mkSimplePresheaf coveredSimpleSite [RootSection 0] [LeafSection 0, LeafSection 1])
  innerMorphism <- expectRight (mkFinitePresheafMorphism innerMiddle innerMiddle identitySimpleComponent)
  outerMorphism <- expectRight (mkFinitePresheafMorphism outerMiddle outerMiddle identitySimpleComponent)
  case composeFinitePresheafMorphisms outerMorphism innerMorphism of
    Left (FinitePresheafMorphismCompositionMiddleFiberMismatch objectValue innerValues outerValues) -> do
      objectValue @?= Leaf
      innerValues @?= [LeafSection 0]
      outerValues @?= [LeafSection 0, LeafSection 1]
    Left otherFailure ->
      assertFailure ("expected middle-fiber mismatch, received " <> show otherFailure)
    Right _ ->
      assertFailure "expected middle-fiber mismatch, received composed morphism"

testBudgetRejectsRawCandidateExplosion :: Assertion
testBudgetRejectsRawCandidateExplosion = do
  let siteValue = SimpleSite DuplicateLeafCoverTopology
      budget = FiniteEnumerationBudget (Just 4)
  basis <- expectRight (mkFiniteCoverBasis siteValue)
  presheaf <- expectRight (mkSimplePresheaf siteValue [RootSection 0] [LeafSection 0, LeafSection 1])
  case plusConstruction budget basis presheaf of
    Left (PlusEnumerationBudgetExceeded Leaf cost) -> do
      pecCoverCount cost @?= 2
      pecAssignmentUpperBound cost @?= 6
    Left otherFailure ->
      assertFailure ("expected raw candidate budget failure, received " <> show otherFailure)
    Right _ ->
      assertFailure "expected raw candidate budget failure, received plus construction"

assertReportAccepted ::
  SheafConditionReport obj value mismatch ->
  Assertion
assertReportAccepted reportValue =
  assertBool
    ("expected accepted sheaf-condition report, received " <> showReportShape reportValue)
    (sheafConditionReportAccepted reportValue)
  where
    showReportShape :: SheafConditionReport obj value mismatch -> String
    showReportShape reportShape =
      show
        ( length (scrInjectivityFailures reportShape),
          length (scrSurjectivityFailures reportShape),
          length (scrSeparationFailures reportShape)
        )

assertCompositeUnitAt ::
  (Eq targetValue, Show targetValue, Site site, Ord sourceValue, Ord middleValue) =>
  SiteObject site ->
  sourceValue ->
  FinitePresheafMorphism site sourceValue middleValue sourceMismatch middleMismatch sourceRestrictionFailure middleRestrictionFailure ->
  FinitePresheafMorphism site middleValue targetValue middleMismatch targetMismatch middleRestrictionFailure targetRestrictionFailure ->
  FinitePresheafMorphism site sourceValue targetValue sourceMismatch targetMismatch sourceRestrictionFailure targetRestrictionFailure ->
  Assertion
assertCompositeUnitAt objectValue sourceValue firstUnit secondUnit compositeUnit = do
  middleValue <- expectComponentAt "first unit" objectValue sourceValue firstUnit
  expectedTarget <- expectComponentAt "second unit" objectValue middleValue secondUnit
  finitePresheafMorphismComponentAt objectValue sourceValue compositeUnit @?= Just expectedTarget

expectComponentAt ::
  (Site site, Ord sourceValue) =>
  String ->
  SiteObject site ->
  sourceValue ->
  FinitePresheafMorphism site sourceValue targetValue sourceMismatch targetMismatch sourceRestrictionFailure targetRestrictionFailure ->
  IO targetValue
expectComponentAt label objectValue sourceValue morphismValue =
  maybe
    (assertFailure ("expected " <> label <> " component"))
    pure
    (finitePresheafMorphismComponentAt objectValue sourceValue morphismValue)

assertFiberValuesEqual ::
  (Eq value, Show value) =>
  SimpleObject ->
  FinitePresheaf SimpleSite value mismatchLeft restrictionFailureLeft ->
  FinitePresheaf SimpleSite value mismatchRight restrictionFailureRight ->
  Assertion
assertFiberValuesEqual objectValue leftPresheaf rightPresheaf = do
  leftValues <- expectFiberValuesAt objectValue leftPresheaf
  rightValues <- expectFiberValuesAt objectValue rightPresheaf
  leftValues @?= rightValues

sheafConditionReportFor ::
  SimpleSite ->
  [SimpleSection] ->
  [SimpleSection] ->
  IO (SheafConditionReport SimpleObject SimpleSection SimpleMismatch)
sheafConditionReportFor siteValue rootValues leafValues = do
  basis <- expectRight (mkFiniteCoverBasis siteValue)
  presheaf <- expectRight (mkSimplePresheaf siteValue rootValues leafValues)
  expectRight (checkFiniteSheafCondition unboundedBudget basis presheaf)

expectFiberValuesAt ::
  SimpleObject ->
  FinitePresheaf SimpleSite value mismatch restrictionFailure ->
  IO [value]
expectFiberValuesAt objectValue presheaf =
  maybe
    (assertFailure ("expected finite fiber at " <> show objectValue))
    (pure . finiteFiberValues)
    (finiteFiberAt objectValue presheaf)

unboundedBudget :: FiniteEnumerationBudget
unboundedBudget =
  FiniteEnumerationBudget Nothing

data SimpleTopology
  = IdentityOnlyTopology
  | LeafCoverTopology
  | DuplicateLeafCoverTopology
  deriving stock (Eq, Ord, Show)

newtype SimpleSite = SimpleSite SimpleTopology
  deriving stock (Eq, Ord, Show)

data SimpleObject
  = Leaf
  | Root
  deriving stock (Eq, Ord, Show)

data SimpleMorphism
  = SimpleIdentity SimpleObject
  | LeafToRoot
  deriving stock (Eq, Ord, Show)

data SimpleSection
  = LeafSection Int
  | RootSection Int
  deriving stock (Eq, Ord, Show)

data SimpleMismatch = SimpleMismatch !SimpleObject !SimpleSection !SimpleSection
  deriving stock (Eq, Show)

data SimpleRestrictionFailure = SimpleRestrictionFailure !SimpleMorphism !SimpleSection
  deriving stock (Eq, Show)

data SimpleComponentFailure = SimpleComponentFailure !SimpleObject !SimpleSection
  deriving stock (Eq, Show)

coveredSimpleSite :: SimpleSite
coveredSimpleSite =
  SimpleSite LeafCoverTopology

instance Site SimpleSite where
  type SiteObject SimpleSite = SimpleObject
  type SiteMorphism SimpleSite = SimpleMorphism

  siteObjects _ =
    [Leaf, Root]

  siteMorphisms _ =
    [identityMorphism Leaf, identityMorphism Root, leafToRoot]

  identityAt _ =
    identityMorphism

  coversAt (SimpleSite topology) objectValue =
    case (topology, objectValue) of
      (LeafCoverTopology, Root) ->
        [coverValue | Right coverValue <- [leafCover]]
      (DuplicateLeafCoverTopology, Root) ->
        [coverValue | Right coverValue <- [duplicateLeafCover]]
      _ ->
        []

  composeChecked _ outerMorphism innerMorphism
    | cmSource outerMorphism /= cmTarget innerMorphism =
        Nothing
    | isIdentityMorphism outerMorphism =
        Just innerMorphism
    | isIdentityMorphism innerMorphism =
        Just outerMorphism
    | otherwise =
        Nothing

  pullbackPair _ leftMorphism rightMorphism
    | cmTarget leftMorphism /= cmTarget rightMorphism =
        Nothing
    | otherwise =
        let apexObject = simpleMeet (cmSource leftMorphism) (cmSource rightMorphism)
         in Just
              PullbackSquare
                { psLeftBase = leftMorphism,
                  psRightBase = rightMorphism,
                  psApex = apexObject,
                  psToLeft = arrowFromApex apexObject (cmSource leftMorphism),
                  psToRight = arrowFromApex apexObject (cmSource rightMorphism)
                }

mkSimplePresheaf ::
  SimpleSite ->
  [SimpleSection] ->
  [SimpleSection] ->
  Either
    (FinitePresheafFailure SimpleObject SimpleMorphism SimpleSection SimpleMismatch SimpleRestrictionFailure)
    (FinitePresheaf SimpleSite SimpleSection SimpleMismatch SimpleRestrictionFailure)
mkSimplePresheaf siteValue rootValues leafValues =
  mkFinitePresheaf
    siteValue
    simpleRestrict
    simpleMismatches
    (\_objectValue sectionValue -> sectionValue)
    ( Map.fromList
        [ (Leaf, leafValues),
          (Root, rootValues)
        ]
    )

simpleRestrict ::
  CheckedMorphism SimpleObject SimpleMorphism ->
  SimpleSection ->
  Either SimpleRestrictionFailure SimpleSection
simpleRestrict morphismValue sectionValue =
  case (cmSource morphismValue, cmTarget morphismValue, sectionValue) of
    (Leaf, Leaf, LeafSection _) ->
      Right sectionValue
    (Root, Root, RootSection _) ->
      Right sectionValue
    (Leaf, Root, RootSection _) ->
      Right (LeafSection 0)
    _ ->
      Left (SimpleRestrictionFailure (cmWitness morphismValue) sectionValue)

simpleMismatches ::
  SimpleObject ->
  SimpleSection ->
  SimpleSection ->
  [SimpleMismatch]
simpleMismatches objectValue leftValue rightValue =
  [SimpleMismatch objectValue leftValue rightValue | leftValue /= rightValue]

unnaturalSimpleComponent ::
  SimpleObject ->
  SimpleSection ->
  Either SimpleComponentFailure SimpleSection
unnaturalSimpleComponent objectValue sectionValue =
  case (objectValue, sectionValue) of
    (Root, RootSection 0) ->
      Right (RootSection 0)
    (Leaf, LeafSection 0) ->
      Right (LeafSection 1)
    _ ->
      Left (SimpleComponentFailure objectValue sectionValue)

identitySimpleComponent ::
  SimpleObject ->
  SimpleSection ->
  Either SimpleComponentFailure SimpleSection
identitySimpleComponent _objectValue sectionValue =
  Right sectionValue

identityMorphism :: SimpleObject -> CheckedMorphism SimpleObject SimpleMorphism
identityMorphism objectValue =
  CheckedMorphism
    { cmSource = objectValue,
      cmTarget = objectValue,
      cmWitness = SimpleIdentity objectValue
    }

leafToRoot :: CheckedMorphism SimpleObject SimpleMorphism
leafToRoot =
  CheckedMorphism
    { cmSource = Leaf,
      cmTarget = Root,
      cmWitness = LeafToRoot
    }

leafCover :: Either (CoverConstructionError SimpleObject) (CoveringFamily SimpleObject SimpleMorphism)
leafCover =
  mkCoveringFamily Root (leafToRoot :| [])

duplicateLeafCover :: Either (CoverConstructionError SimpleObject) (CoveringFamily SimpleObject SimpleMorphism)
duplicateLeafCover =
  mkCoveringFamily Root (leafToRoot :| [leafToRoot])

isIdentityMorphism :: CheckedMorphism SimpleObject SimpleMorphism -> Bool
isIdentityMorphism morphismValue =
  case cmWitness morphismValue of
    SimpleIdentity _ ->
      True
    LeafToRoot ->
      False

simpleMeet :: SimpleObject -> SimpleObject -> SimpleObject
simpleMeet leftObject rightObject =
  if leftObject == Leaf || rightObject == Leaf
    then Leaf
    else Root

arrowFromApex :: SimpleObject -> SimpleObject -> CheckedMorphism SimpleObject SimpleMorphism
arrowFromApex sourceObject targetObject =
  case (sourceObject, targetObject) of
    (Leaf, Root) ->
      leafToRoot
    _ ->
      identityMorphism sourceObject
