module Moonlight.Cosheaf.Core.SupportSpec
  ( tests,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.List (find)
import Data.Map.Strict qualified as Map
import Moonlight.Cosheaf
  ( CosectionRepresentative (..),
    CosheafChainFailure (..),
    CosheafSupportFailure (..),
    CosheafSupportPlan,
    FiniteCosheafAlgebra (..),
    FiniteCosheafFailure (..),
    FiniteCostalk,
    FiniteCosheaf,
    TropicalCosectionFailure (..),
    TropicalCostModel (..),
    cosheafColimitClassKeys,
    cosheafColimitRepresentatives,
    cosheafSupportPlanFromKeys,
    cspFootprintMeasures,
    cspObjects,
    fcSiteIndex,
    finiteCostalkAtObjectKey,
    finiteCostalkKeys,
    finiteCosheafColimitFromSupportPlan,
    finiteCosheafCorestrictions,
    fullFiniteCosheafChainSupportPlan,
    minPlusOne,
    mkFiniteCosheaf,
    planTropicalCosections,
    prepareFiniteCosheafChainFromSupportPlan,
    scHasAny,
    supportCarrierCount,
    tctTransitions,
    tcpClassChoices,
    compileTropicalCostTableFromSupportPlan,
    ccMorphism,
    ccMorphismKey,
    ccSourceObjectKey,
  )
import Moonlight.Cosheaf.Chain
  ( cosheafBoundaryIncidenceAt,
  )
import Moonlight.Cosheaf.SiteIndex
  ( CosheafMorphismKey (..),
    cosheafSiteObjectIndex,
  )
import Moonlight.Cosheaf.Test.Fixture
  ( ChainMorphism (..),
    ChainObject (..),
    ChainSite (..),
    ChainSiteMode (..),
    ChainCoreFailure (..),
    chainCorestrictValue,
    chainCosheaf,
    chainGoodAlgebra,
    chainRawCostalks,
  )
import Moonlight.Homology
  ( HomologicalDegree (..),
    sourceCardinality,
  )
import Moonlight.Sheaf.Footprint
  ( FootprintMeasure (..),
    FootprintMeasureUnit (..),
  )
import Moonlight.Sheaf.Index.Dense (denseIndexKeyOf)
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectKey (..),
  )
import Moonlight.Sheaf.Site.Class
  ( CheckedMorphism (..),
  )
import Numeric.Natural (Natural)
import Test.Tasty
  ( TestTree,
    testGroup,
  )
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
    "cosheaf support pruning"
    [ testCase "restricted support exposes exact retained/pruned footprint" testRestrictedSupportFootprint,
      testCase "support closure failure is typed and does not fall back" testInvalidSupportClosureFails,
      testCase "unknown retained morphism key is a typed obstruction" testUnknownMorphismKeyFails,
      testCase "supported chain materializes only retained carrier" testSupportedChainSkipsPrunedCarrier,
      testCase "colimit and tropical planning operate on restricted support" testRestrictedColimitAndTropical,
      testCase "restricted tropical planning skips pruned representative costs" testRestrictedTropicalSkipsPrunedRepresentativeCosts,
      testCase "finite cosheaf construction still requires pruned source payloads" testFiniteCosheafConstructionStillRequiresPrunedSourcePayloads
    ]

testRestrictedSupportFootprint :: Assertion
testRestrictedSupportFootprint = do
  cosheaf <- expectRight (chainCosheaf (ChainSite ChainGoodSite) chainGoodAlgebra)
  supportPlan <- chainAOnlySupportPlan cosheaf
  assertBool "retained object carrier reports non-empty without projection" (scHasAny (cspObjects supportPlan))
  assertEqual "one site object retained" 1 (supportCarrierCount (cspObjects supportPlan))
  assertFootprint ContextOrdinalUnit (Just 3) (Just 1) (Just 2) (cspFootprintMeasures supportPlan)
  assertFootprint CoboundaryRestrictionUnit (Just 6) (Just 0) (Just 6) (cspFootprintMeasures supportPlan)
  assertFootprint SupportCellUnit (Just 6) (Just 2) (Just 4) (cspFootprintMeasures supportPlan)

testInvalidSupportClosureFails :: Assertion
testInvalidSupportClosureFails = do
  cosheaf <- expectRight (chainCosheaf (ChainSite ChainGoodSite) chainGoodAlgebra)
  chainABCorestriction <-
    maybe
      (assertFailure "ChainAB corestriction missing")
      pure
      (find ((== ChainAB) . cmWitness . ccMorphism) (finiteCosheafCorestrictions cosheaf))
  sourceCostalk <-
    costalkAtOrFail cosheaf (ccSourceObjectKey chainABCorestriction)
  let retainedSourceKeys =
        fmap
          (\costalkKey -> (ccSourceObjectKey chainABCorestriction, costalkKey))
          (finiteCostalkKeys sourceCostalk)
      result =
        cosheafSupportPlanFromKeys
          1
          [ccSourceObjectKey chainABCorestriction]
          [ccMorphismKey chainABCorestriction]
          retainedSourceKeys
          cosheaf
  case result of
    Left (CosheafSupportMorphismEndpointPruned morphismValue) ->
      assertEqual "typed obstruction names the exiting morphism" ChainAB (cmWitness morphismValue)
    Left otherFailure ->
      assertFailure ("unexpected support failure: " <> show otherFailure)
    Right _ ->
      assertFailure "invalid closure should not silently expand or fall back"

testUnknownMorphismKeyFails :: Assertion
testUnknownMorphismKeyFails = do
  cosheaf <- expectRight (chainCosheaf (ChainSite ChainGoodSite) chainGoodAlgebra)
  let unknownMorphismKey =
        CosheafMorphismKey 9999
      result =
        cosheafSupportPlanFromKeys
          1
          []
          [unknownMorphismKey]
          []
          cosheaf
  case result of
    Left (CosheafSupportMorphismUnknown morphismKey) ->
      assertEqual "unknown retained morphism is not ignored" unknownMorphismKey morphismKey
    Left otherFailure ->
      assertFailure ("unexpected support failure: " <> show otherFailure)
    Right _ ->
      assertFailure "unknown retained morphism should not be silently discarded"

testSupportedChainSkipsPrunedCarrier :: Assertion
testSupportedChainSkipsPrunedCarrier = do
  cosheaf <- expectRight (chainCosheaf (ChainSite ChainGoodSite) chainGoodAlgebra)
  supportPlan <- chainAOnlySupportPlan cosheaf
  chainPlan <- expectRight (prepareFiniteCosheafChainFromSupportPlan supportPlan cosheaf)
  assertEqual
    "C0 contains only ChainA costalk coordinates"
    2
    (sourceCardinality (cosheafBoundaryIncidenceAt (HomologicalDegree 0) chainPlan))
  assertEqual
    "C1 is empty because no retained morphism survives"
    0
    (sourceCardinality (cosheafBoundaryIncidenceAt (HomologicalDegree 1) chainPlan))

testRestrictedColimitAndTropical :: Assertion
testRestrictedColimitAndTropical = do
  cosheaf <- expectRight (chainCosheaf (ChainSite ChainGoodSite) chainGoodAlgebra)
  fullSupportPlan <- expectRight (fullFiniteCosheafChainSupportPlan 1 cosheaf)
  restrictedSupportPlan <- chainAOnlySupportPlan cosheaf
  fullColimit <- expectRight (finiteCosheafColimitFromSupportPlan fullSupportPlan cosheaf)
  restrictedColimit <- expectRight (finiteCosheafColimitFromSupportPlan restrictedSupportPlan cosheaf)
  fullCostTable <- expectRight (compileTropicalCostTableFromSupportPlan fullSupportPlan fullColimit unitTropicalCostModel)
  restrictedCostTable <- expectRight (compileTropicalCostTableFromSupportPlan restrictedSupportPlan restrictedColimit unitTropicalCostModel)
  fullPlan <- expectRight (planTropicalCosections fullCostTable)
  restrictedPlan <- expectRight (planTropicalCosections restrictedCostTable)
  assertEqual
    "ChainA-only support preserves the target H0/colimit class count"
    (length (cosheafColimitClassKeys fullColimit))
    (length (cosheafColimitClassKeys restrictedColimit))
  assertBool
    "full tropical table materializes transition payloads"
    (not (null (tctTransitions fullCostTable)))
  assertEqual
    "restricted tropical table materializes no pruned transitions"
    0
    (length (tctTransitions restrictedCostTable))
  assertEqual
    "restricted planner still chooses one representative per class"
    (IntMap.size (tcpClassChoices fullPlan))
    (IntMap.size (tcpClassChoices restrictedPlan))

testRestrictedTropicalSkipsPrunedRepresentativeCosts :: Assertion
testRestrictedTropicalSkipsPrunedRepresentativeCosts = do
  cosheaf <- expectRight (chainCosheaf (ChainSite ChainGoodSite) chainGoodAlgebra)
  fullSupportPlan <- expectRight (fullFiniteCosheafChainSupportPlan 1 cosheaf)
  restrictedSupportPlan <- chainAOnlySupportPlan cosheaf
  fullColimit <- expectRight (finiteCosheafColimitFromSupportPlan fullSupportPlan cosheaf)
  restrictedColimit <- expectRight (finiteCosheafColimitFromSupportPlan restrictedSupportPlan cosheaf)
  assertEqual
    "full colimit surfaces every costalk representative payload"
    6
    (length (cosheafColimitRepresentatives fullColimit))
  assertEqual
    "restricted colimit surfaces only retained representative payloads"
    2
    (length (cosheafColimitRepresentatives restrictedColimit))
  restrictedCostTable <-
    expectRight
      (compileTropicalCostTableFromSupportPlan restrictedSupportPlan restrictedColimit retainedOnlyTropicalCostModel)
  assertEqual
    "restricted support has no transition payloads to price"
    0
    (length (tctTransitions restrictedCostTable))
  case compileTropicalCostTableFromSupportPlan fullSupportPlan fullColimit retainedOnlyTropicalCostModel of
    Left (TropicalRepresentativeCostMissing representativeValue) ->
      assertBool
        "full support tries to price a representative outside the retained ChainA carrier"
        (cosectionRepObject representativeValue /= ChainA)
    Left otherFailure ->
      assertFailure ("unexpected full-support tropical failure: " <> show otherFailure)
    Right _ ->
      assertFailure "full support should request a pruned representative cost"

testFiniteCosheafConstructionStillRequiresPrunedSourcePayloads :: Assertion
testFiniteCosheafConstructionStillRequiresPrunedSourcePayloads = do
  case mkFiniteCosheaf (ChainSite ChainGoodSite) chainGoodAlgebra (Map.singleton ChainA [0, 1]) of
    Left (FiniteCostalkMissing ChainB) ->
      pure ()
    Left otherFailure ->
      assertFailure ("unexpected retained-only construction failure: " <> show otherFailure)
    Right _ ->
      assertFailure "current FiniteCosheaf construction should require pruned ChainB costalk payloads"
  case mkFiniteCosheaf (ChainSite ChainGoodSite) preSupportSentinelAlgebra chainRawCostalks of
    Left (FiniteCorestrictionFailed morphismValue _ ChainCoreFailure) ->
      assertEqual
        "current FiniteCosheaf construction still compiles pruned ChainB -> ChainC corestrictions"
        ChainBC
        (cmWitness morphismValue)
    Left otherFailure ->
      assertFailure ("unexpected pre-support sentinel failure: " <> show otherFailure)
    Right _ ->
      assertFailure "current FiniteCosheaf construction should force pruned corestriction compilation"

chainAOnlySupportPlan ::
  FiniteCosheaf ChainSite Int ->
  IO (CosheafSupportPlan)
chainAOnlySupportPlan =
  chainAOnlySupportPlanAtDegree 1

chainAOnlySupportPlanAtDegree ::
  Natural ->
  FiniteCosheaf ChainSite Int ->
  IO (CosheafSupportPlan)
chainAOnlySupportPlanAtDegree maxDegreeValue cosheaf = do
  objectKey <-
    maybe
      (assertFailure "ChainA object key missing")
      pure
      (denseIndexKeyOf ChainA (cosheafSiteObjectIndex (fcSiteIndex cosheaf)))
  costalkValue <-
    costalkAtOrFail cosheaf objectKey
  expectRight
    ( cosheafSupportPlanFromKeys
        maxDegreeValue
        [objectKey]
        []
        (fmap (\costalkKey -> (objectKey, costalkKey)) (finiteCostalkKeys costalkValue))
        cosheaf
    )

costalkAtOrFail ::
  FiniteCosheaf ChainSite Int ->
  ObjectKey ->
  IO (FiniteCostalk ChainObject Int)
costalkAtOrFail cosheaf objectKey =
  maybe
    (assertFailure ("costalk missing at " <> show objectKey))
    pure
    (finiteCostalkAtObjectKey objectKey cosheaf)

unitTropicalCostModel :: TropicalCostModel ChainSite Int
unitTropicalCostModel =
  TropicalCostModel
    { tcmRepresentativeCost = const (Right minPlusOne),
      tcmTransitionCost = const (Right minPlusOne)
    }

retainedOnlyTropicalCostModel :: TropicalCostModel ChainSite Int
retainedOnlyTropicalCostModel =
  TropicalCostModel
    { tcmRepresentativeCost = \representativeValue ->
        if cosectionRepObject representativeValue == ChainA
          then Right minPlusOne
          else Left (TropicalRepresentativeCostMissing representativeValue),
      tcmTransitionCost = \transitionValue ->
        Left (TropicalTransitionCostMissing transitionValue)
    }

preSupportSentinelAlgebra :: FiniteCosheafAlgebra ChainSite Int () ChainCoreFailure
preSupportSentinelAlgebra =
  chainGoodAlgebra
    { fcaCorestrict = \morphismValue sourceValue ->
        case (cmWitness morphismValue, sourceValue) of
          (ChainBC, 10) -> Left ChainCoreFailure
          _ -> chainCorestrictValue morphismValue sourceValue
    }

assertFootprint ::
  FootprintMeasureUnit ->
  Maybe Natural ->
  Maybe Natural ->
  Maybe Natural ->
  [FootprintMeasure Natural] ->
  Assertion
assertFootprint unitValue expectedTotal expectedRetained expectedPruned measures =
  case find ((== unitValue) . fmUnit) measures of
    Nothing ->
      assertFailure ("missing footprint measure for " <> show unitValue)
    Just measure ->
      assertEqual
        ("footprint for " <> show unitValue)
        (expectedTotal, expectedRetained, expectedPruned)
        (fmTotal measure, fmRetained measure, fmPruned measure)

expectRight :: (Show failure) => Either failure value -> IO value
expectRight result =
  case result of
    Right value -> pure value
    Left failureValue -> assertFailure ("unexpected failure: " <> show failureValue)
