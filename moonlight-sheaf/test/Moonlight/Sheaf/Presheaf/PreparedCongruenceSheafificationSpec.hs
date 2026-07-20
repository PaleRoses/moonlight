{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Presheaf.PreparedCongruenceSheafificationSpec
  ( tests,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core (DenseKey (..))
import Moonlight.Sheaf.Presheaf.Congruence
  ( CongruenceFinitePresheaf,
    CongruencePresheafBuildFailure (..),
    PreparedCongruenceSiteModel,
    finiteCongruencePresheafFromRelations,
    finiteCongruencePresheafFromStalks,
    prepareCongruenceSiteModelWith,
  )
import Moonlight.Sheaf.Presheaf.Enumeration
  ( FiniteEnumerationBudget (..),
  )
import Moonlight.Sheaf.Presheaf.Finite
  ( FinitePresheaf (..),
    finiteFiberAt,
    finiteFiberValues,
  )
import Moonlight.Sheaf.Section.Congruence.Equivalence.Relation
import Moonlight.Sheaf.Section.Stalk
  ( stalkMismatches,
  )
import Moonlight.Sheaf.Section.Stalk.Congruence.Carrier
import Moonlight.Sheaf.Section.Stalk.Congruence.Model
import Moonlight.Sheaf.Sheafification.Finite
  ( associatedSheafificationReport,
    sheafConditionReportAccepted,
    sheafifyFinitePresheaf,
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
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
import Moonlight.Sheaf.TestFixture.Assertions
  ( expectJust,
    expectRight,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

data CongruenceContext
  = CongruenceCoarse
  | CongruenceFine
  | CongruenceGhost
  deriving stock (Eq, Ord, Show, Read)

tests :: TestTree
tests =
  testGroup
    "prepared congruence finite presheaf"
    [ testCase "constructs finite congruence presheaf over one fixed carrier" testConstructsOverFixedCarrier,
      testCase "raw stalk adapter rejects carrier mismatch" testRejectsRawCarrierMismatch,
      testCase "typed construction failures report unknown cells, visible support, and invalid relations" testTypedConstructionFailures,
      testCase "restriction preserves prepared relation image" testRestrictionPreservesRelationImage,
      testCase "sheafification accepts congruence-valued finite presheaf" testSheafificationAcceptsCongruencePresheaf
    ]

testConstructsOverFixedCarrier :: Assertion
testConstructsOverFixedCarrier =
  withPreparedSiteModel $ \_site _carrier model -> do
    presheaf <- samplePresheaf model
    fmap (length . finiteFiberValues) (finiteFiberAt CongruenceCoarse presheaf) @?= Just 1
    fmap (length . finiteFiberValues) (finiteFiberAt CongruenceFine presheaf) @?= Just 1

testRejectsRawCarrierMismatch :: Assertion
testRejectsRawCarrierMismatch =
  withPreparedSiteModel $ \_site expectedCarrier model -> do
    actualCarrier <- expectRight (mkGlobalCarrier (CarrierId 1) ["w" :: String, "x", "y", "z"])
    wrongStalk <- expectRight (mkCongruenceStalkFromPairs actualCarrier [key 0, key 1] [(key 0, key 1)])
    expectLeft "carrier mismatch" (finiteCongruencePresheafFromStalks model (Map.singleton CongruenceCoarse [wrongStalk])) $ \case
      CongruencePresheafCarrierMismatch (CarrierId 0) (CarrierId 1) expectedIndexedValues actualIndexedValues -> do
        expectedIndexedValues @?= carrierIndexedValues expectedCarrier
        actualIndexedValues @?= carrierIndexedValues actualCarrier
      otherFailure ->
        assertFailure ("expected carrier mismatch, received " <> show otherFailure)

testTypedConstructionFailures :: Assertion
testTypedConstructionFailures =
  withCongruenceSite $ \site -> do
    carrier <- sampleCarrier
    expectLeftValue
      "unknown visible-support cell"
      (PreparedCongruenceVisibleSupportUnknownObject CongruenceGhost)
      (prepareCongruenceSiteModelWith site carrier (Map.insert CongruenceGhost [key 0] sampleVisibleSupport) carrierMapFor (const ()))
    expectLeftValue
      "invalid visible support"
      ( PreparedCongruenceVisibleSupportInvalid
          CongruenceCoarse
          (CongruenceVisibleKeyOutsideCarrier CongruenceStalkVisible 99)
      )
      (prepareCongruenceSiteModelWith site carrier (Map.insert CongruenceCoarse [key 99] sampleVisibleSupport) carrierMapFor (const ()))
    withPreparedSiteModelAt site carrier $ \model -> do
      rawCoarse <- expectRight (mkCongruenceStalkFromPairs carrier [key 0, key 1] [(key 0, key 1)])
      expectLeft "raw unknown cell" (finiteCongruencePresheafFromStalks model (Map.singleton CongruenceGhost [rawCoarse])) $ \case
        CongruencePresheafUnknownCell CongruenceGhost -> pure ()
        otherFailure -> assertFailure ("expected raw unknown cell, received " <> show otherFailure)
      expectLeft "raw visible mismatch" (finiteCongruencePresheafFromStalks model (Map.singleton CongruenceFine [rawCoarse])) $ \case
        CongruencePresheafVisibleMismatch CongruenceFine expectedVisible actualVisible -> do
          expectedVisible @?= IntSet.fromList [2, 3]
          actualVisible @?= IntSet.fromList [0, 1]
        otherFailure -> assertFailure ("expected raw visible mismatch, received " <> show otherFailure)
      badCoarse <- relationOver (IntSet.fromList [0, 1]) [(0, 1)]
      fine <- sampleFineRelation
      expectLeft "invalid relation" (finiteCongruencePresheafFromRelations model (relationFibers badCoarse fine)) $ \case
        CongruencePresheafStalkInvalid (PreparedCongruenceStalkRelationInvalid CongruenceCoarse (EquivalenceDomainMismatch expectedDomain actualDomain)) -> do
          expectedDomain @?= sampleCarrierDomain
          actualDomain @?= IntSet.fromList [0, 1]
        otherFailure -> assertFailure ("expected invalid relation, received " <> show otherFailure)

testRestrictionPreservesRelationImage :: Assertion
testRestrictionPreservesRelationImage =
  withPreparedSiteModel $ \site _carrier model -> do
    presheaf <- samplePresheaf model
    fineToCoarse <- expectJust (finiteMeetMorphism site CongruenceFine CongruenceCoarse)
    coarseValue <- singleFiberValueAt CongruenceCoarse presheaf
    fineValue <- singleFiberValueAt CongruenceFine presheaf
    restrictedValue <- expectRight (fpRestrict presheaf fineToCoarse coarseValue)
    stalkMismatches preparedCongruenceStalkAlgebra restrictedValue fineValue @?= []

testSheafificationAcceptsCongruencePresheaf :: Assertion
testSheafificationAcceptsCongruencePresheaf =
  withPreparedSiteModel $ \site _carrier model -> do
    basis <- expectRight (mkFiniteCoverBasis site)
    presheaf <- samplePresheaf model
    sheafification <- expectRight (sheafifyFinitePresheaf (FiniteEnumerationBudget Nothing) basis presheaf)
    reportValue <- expectRight (associatedSheafificationReport (FiniteEnumerationBudget Nothing) basis sheafification)
    assertBool
      "associated congruence-valued presheaf should satisfy the finite sheaf condition"
      (sheafConditionReportAccepted reportValue)

withCongruenceSite ::
  (FiniteMeetSite CongruenceContext -> Assertion) ->
  Assertion
withCongruenceSite continue =
  expectRight (mkFiniteMeetSite sampleSiteSpec) >>= continue

withPreparedSiteModel ::
  ( forall carrier owner.
    FiniteMeetSite CongruenceContext ->
    GlobalCarrier (CarrierKey String) String ->
    PreparedCongruenceSiteModel carrier owner (FiniteMeetSite CongruenceContext) (CarrierKey String) String ->
    Assertion
  ) ->
  Assertion
withPreparedSiteModel continue =
  withCongruenceSite $ \site -> do
    carrier <- sampleCarrier
    withPreparedSiteModelAt site carrier (continue site carrier)

withPreparedSiteModelAt ::
  FiniteMeetSite CongruenceContext ->
  GlobalCarrier (CarrierKey String) String ->
  ( forall carrier owner.
    PreparedCongruenceSiteModel carrier owner (FiniteMeetSite CongruenceContext) (CarrierKey String) String ->
    Assertion
  ) ->
  Assertion
withPreparedSiteModelAt site carrier continue =
  case prepareCongruenceSiteModelWith site carrier sampleVisibleSupport carrierMapFor continue of
    Left failureValue ->
      assertFailure ("expected prepared congruence site model, received " <> show failureValue)
    Right assertion ->
      assertion

samplePresheaf ::
  PreparedCongruenceSiteModel carrier owner (FiniteMeetSite CongruenceContext) (CarrierKey String) String ->
  IO (CongruenceFinitePresheaf (FiniteMeetSite CongruenceContext) carrier (CarrierKey String) String)
samplePresheaf model = do
  fibers <- relationFibers <$> sampleCoarseRelation <*> sampleFineRelation
  expectRight (finiteCongruencePresheafFromRelations model fibers)

singleFiberValueAt ::
  (Site site, Show (SiteObject site)) =>
  SiteObject site ->
  FinitePresheaf site value mismatch restrictionFailure ->
  IO value
singleFiberValueAt objectValue presheaf =
  case finiteFiberValues <$> finiteFiberAt objectValue presheaf of
    Just [value] -> pure value
    actualValues -> assertFailure ("expected one fiber value at " <> show objectValue <> ", received " <> show (fmap length actualValues))

sampleSiteSpec :: FiniteMeetSiteSpec CongruenceContext
sampleSiteSpec =
  FiniteMeetSiteSpec
    { fmssCells = CongruenceCoarse :| [CongruenceFine],
      fmssRefinements = Set.singleton (CongruenceFine, CongruenceCoarse),
      fmssCovers = Map.singleton CongruenceCoarse [CongruenceFine :| []]
    }

sampleCarrier :: IO (GlobalCarrier (CarrierKey String) String)
sampleCarrier =
  expectRight (mkGlobalCarrier (CarrierId 0) ["a" :: String, "b", "c", "d"])

sampleVisibleSupport :: Map CongruenceContext [CarrierKey String]
sampleVisibleSupport =
  Map.fromList
    [ (CongruenceCoarse, [key 0, key 1]),
      (CongruenceFine, [key 2, key 3])
    ]

carrierMapFor ::
  CheckedMorphism CongruenceContext (FiniteMeetMorphism CongruenceContext) ->
  IntMap.IntMap (CarrierKey String)
carrierMapFor morphism
  | cmSource morphism == CongruenceFine && cmTarget morphism == CongruenceCoarse =
      carrierMap [(0, 2), (1, 3), (2, 2), (3, 3)]
  | otherwise =
      carrierMap [(0, 0), (1, 1), (2, 2), (3, 3)]

carrierMap :: [(Int, Int)] -> IntMap.IntMap (CarrierKey String)
carrierMap =
  IntMap.fromList . fmap (\(sourceKey, targetKey) -> (sourceKey, key targetKey))

relationFibers ::
  EquivalenceRelation (CarrierKey String) ->
  EquivalenceRelation (CarrierKey String) ->
  Map CongruenceContext [EquivalenceRelation (CarrierKey String)]
relationFibers coarseRelation fineRelation =
  Map.fromList [(CongruenceCoarse, [coarseRelation]), (CongruenceFine, [fineRelation])]

sampleCoarseRelation :: IO (EquivalenceRelation (CarrierKey String))
sampleCoarseRelation =
  relationOver sampleCarrierDomain [(0, 1)]

sampleFineRelation :: IO (EquivalenceRelation (CarrierKey String))
sampleFineRelation =
  relationOver sampleCarrierDomain [(2, 3)]

relationOver ::
  IntSet.IntSet ->
  [(Int, Int)] ->
  IO (EquivalenceRelation (CarrierKey String))
relationOver domain pairs =
  expectRight $
    equivalenceFromPairs
      domain
      (fmap (\(leftKey, rightKey) -> (key leftKey, key rightKey)) pairs)

sampleCarrierDomain :: IntSet.IntSet
sampleCarrierDomain =
  IntSet.fromList [0, 1, 2, 3]

key :: Int -> CarrierKey atom
key =
  decodeDenseKey

expectLeft ::
  String ->
  Either failure value ->
  (failure -> Assertion) ->
  Assertion
expectLeft label result checkFailure =
  case result of
    Left failure -> checkFailure failure
    Right _ -> assertFailure (label <> ": expected Left")

expectLeftValue ::
  (Eq failure, Show failure) =>
  String ->
  failure ->
  Either failure value ->
  Assertion
expectLeftValue label expected result =
  expectLeft label result (\actual -> actual @?= expected)
