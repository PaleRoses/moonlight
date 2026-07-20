{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Cochain.SiteCohomologySpec
  ( tests,
  )
where

import Data.Map.Strict qualified as Map
import Data.Monoid (Any (..))
import Data.Set qualified as Set
import Data.Vector.Unboxed qualified as Unboxed
import Data.Bifunctor (first)
import Moonlight.Homology
  ( BasisCellRef (..),
    BoundaryEntry,
    BoundaryIncidence,
    FiniteChainComplex,
    HomologyBackend (RationalRankBackend),
    HomologyGroup (freeRank),
    HomologicalDegree (..),
    boundaryEntries,
    boundaryIncidenceApply,
    computeRationalSpectralPages,
    emptyBoundaryIncidenceOf,
    filteredReducedFiltration,
    filteredRefinedMorseComplex,
    frmcRefinedMorseComplex,
    incidenceMatrixAt,
    mapBoundaryCoefficients,
    maxHomologicalDegree,
    mcReducedComplex,
    mkBoundaryEntry,
    mkBoundaryIncidence,
    mkFiniteChainComplexChecked,
    pageDifferentialMap,
    pageEntryMap,
    pageIndex,
    rmcReducedComplex,
    runHomologyBackend,
    sourceCardinality,
    targetCardinality,
    transposeBoundaryIncidence,
  )
import Moonlight.Category
  ( FinCat,
    FinGeneratorId (..),
    FinMor,
    FinMorphismId (..),
    FinObjectId (..),
    FinObj,
    allMorphisms,
    chainMorphisms,
    chainStartObject,
    mkFinCat,
    sampleFinCat,
  )
import Moonlight.Sheaf.Kernel.Basis (basisCells)
import Moonlight.Sheaf.Cochain.Coboundary
  ( checkCoboundaryNilpotence,
  )
import Moonlight.Sheaf.Cochain.Laplacian
  ( buildHodgeLaplacian0,
    buildHodgeLaplacian1,
    LaplacianKind (HodgeLaplacian),
    SheafLaplacian,
    laplacianDomainCardinality,
    slIncidence,
  )
import Moonlight.Sheaf.Cochain.Cohomology
  ( SiteCoboundaryRealization (..),
    SiteCochainInput (..),
    buildGrothendieckCochainArtifact,
    buildNerveCochainArtifact,
    cochainSupportWindow,
  )
import Moonlight.Sheaf.Cochain.PreparedDenseNerve
  ( PreparedDenseNerveCochainError (..),
    applyPreparedDenseNerveRankOneCoboundaryDense,
    materializePreparedDenseNerveCoboundaryComplex,
    materializePreparedDenseNerveRankOneCoboundaryComplexWith,
    prepareDenseNerveCochainPlan,
    preparedDenseNerveComplexScaffold,
    projectPreparedDenseNerveSite,
  )
import Moonlight.Sheaf.Operator.GradedComplex
import Moonlight.Sheaf.Operator.BuildError
  ( SheafOperatorBuildError,
  )
import Moonlight.Sheaf.Cochain.Preparation
  ( morseReducedIterationValue,
    mrscReduction,
    prepareNerveCochainReduced,
    prepareNerveCochainSpectralWith,
    prepareRawNerveCochain,
    rawIterationValue,
    rsciCochainComplex,
    spectralReadyIterationValue,
    srscFilteredMorse,
    srscOriginalFiltration,
    srscSiteComplex,
    srscSpectralPages,
  )
import Moonlight.Sheaf.Site.Stalk.Interface.Linearization
  ( InterfaceStalkBasisAtom (..),
    interfaceStalkBasisAtoms,
    interfaceStalkBasisLinearization,
    linearizedRestrictionComparableRestrictions,
    linearizedRestrictionStalkDimensions,
  )
import Moonlight.Sheaf.Site.Stalk.Interface
  ( CompositionWitness (..),
    FaceStalkProjectionError (..),
    InterfaceDomain (..),
    InterfaceStalk (..),
    WitnessClass (..),
    interfaceStalkExactEq,
    interfaceStalkSignature,
  )
import Moonlight.Sheaf.Site.Analysis.Scaffold
  ( SiteComplexScaffold,
    mkNerveComplexScaffold,
    scsChainComplex,
    smrCriticalCellByReducedBasis,
    smrMorseComplex,
    smrRefinedCriticalCellByReducedBasis,
    smrRefinedMorseComplex,
  )
import Moonlight.Sheaf.Site.Grothendieck
  ( GrothendieckCell,
    GrothendieckFaceMorphism,
    GrothendieckSite,
    grothendieckCellDimension,
    grothendieckFaceMorphismFaceIndex,
    grothendieckFaceMorphismOrientation,
    grothendieckFaceMorphismSource,
    grothendieckFaceMorphismTarget,
    grothendieckSiteCellsAtDimension,
    grothendieckSiteDepth,
    grothendieckSiteFaceMorphisms,
    grothendieckSiteSourceNerve,
    mkGrothendieckSite,
    mkGrothendieckSiteWindow,
  )
import Moonlight.Sheaf.Site.Interface.Types
  ( InterfaceMeasure (..),
    InterfaceName,
    interfaceNameFromString,
  )
import Moonlight.Sheaf.Site.Construction.Nerve
  ( CellKey (..),
    FaceKind,
    FaceMorphism,
    NerveCell,
    NerveSite,
    NerveSiteAlgebra (..),
    faceMorphismFaceIndex,
    faceMorphismKind,
    faceMorphismOrientation,
    faceMorphismSource,
    faceMorphismTarget,
    mkNerveSite,
    mkNerveSiteDenseWindow,
    mkNerveSiteWCOJWindow,
    mkNerveSiteWindow,
    nerveCellKey,
    nerveCellSimplex,
    nerveSiteBasis,
    nerveSiteCells,
    nerveSiteDepth,
    nerveSiteSourceNerve,
    restrictNerveSiteToCellKeys,
    siteCellsAtDimension,
    siteFaceMorphisms,
  )
import Moonlight.Sheaf.Section.Morphism (Restriction)
import Moonlight.Sheaf.Site.Stalk.Restriction
  ( SiteRestrictionBuildError (..),
    SiteRestrictionWitness,
    buildNerveRestrictions,
  )
import Moonlight.Sheaf.TestFixture.Site
  ( constantRestrictionModel,
    featureRestrictionModel,
    SampleSiteTag,
    SampleSystem (..),
    sampleGrothendieckSite,
    sampleNerveSite,
    sourceInterfaceStalk,
  )
import Moonlight.Category.Simplicial (NerveSimplex, nerve, nerveSimplexChain)
import Moonlight.Category.Simplicial (TruncatedNormalizedSSet, simplicesAtDimension)
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    assertEqual,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "site cohomology"
    [ testCase "interface stalk basis atoms expose named features and guardedness" testInterfaceStalkBasisAtoms,
      testCase "interface stalk signature names quotient witness semantics" testInterfaceStalkSignatureNamesQuotientSemantics,
      testCase "interface stalk exact equality distinguishes witness payloads" testInterfaceStalkExactEqualityDistinguishesWitnessPayloads,
      testCase "interface stalk feature linearization projects the shared stalk basis" testInterfaceStalkFeatureLinearization,
      testCase "constant restriction linearization preserves a one-dimensional stalk" testConstantRestrictionLinearization,
      testCase "nerve restriction projection reports failed inner-face composition" testNerveRestrictionProjectionReportsFailedInnerFaceComposition,
      testCase "nerve skeleton preserves simplex counts and face rows" testNerveSkeletonPreservesSimplexCountsAndFaceRows,
      testCase "nerve cochain window avoids high-dimensional skeleton materialization" testNerveCochainWindowAvoidsHighDimensionalMaterialization,
      testCase "WCOJ nerve row source matches simplicial cochain-window rows" testWCOJNerveRowsMatchSimplicialWindowRows,
      testCase "dense ordinal nerve row source matches simplicial cochain-window rows" testDenseOrdinalNerveRowsMatchSimplicialWindowRows,
      testCase "prepared dense nerve plan projects the dense cochain window" testPreparedDenseNervePlanProjectsDenseWindow,
      testCase "prepared dense nerve scaffold matches dense-window scaffold incidence" testPreparedDenseNerveScaffoldMatchesDenseWindow,
      testCase "prepared dense nerve explicit cochain matches dense-window cochain incidence" testPreparedDenseNerveExplicitCochainMatchesDenseWindow,
      testCase "prepared dense nerve rank-one cochain and dense apply match materialized incidence" testPreparedDenseNerveRankOneCochainAndDenseApply,
      testCase "prepared dense nerve plan reports failed adjacent interface composition" testPreparedDenseNerveReportsFailedAdjacentComposition,
      testCase "nerve site restriction recomputes derived caches" testNerveSiteRestrictionRecomputesDerivedCaches,
      testCase "Grothendieck skeleton preserves simplex counts and face rows" testGrothendieckSkeletonPreservesSimplexCountsAndFaceRows,
      testCase "Grothendieck cochain window avoids high-dimensional skeleton materialization" testGrothendieckCochainWindowAvoidsHighDimensionalMaterialization,
      testCase "nerve cohomology builders succeed on a generic finite site" testNerveCohomologyBuilders,
      testCase "rank-one nerve cohomology builder uses scalar topology realization" testRankOneNerveCohomologyBuilder,
      testCase "rank-one constant sheaf reports S1 and tree Betti anchors" testRankOneConstantSheafBettiAnchors,
      testCase "rank-one constant sheaf harmonic kernels realize the S1 and tree Hodge anchors" testRankOneConstantSheafHarmonicAnchors,
      testCase "default nerve cohomology remains explicit interface-block realization" testDefaultNerveCohomologyRemainsExplicitBlock,
      testCase "scaffold-backed nerve cohomology matches site-backed cohomology" testScaffoldBackedNerveCohomologyMatchesSiteBacked,
      testCase "raw nerve preparation preserves the raw coboundary complex" testRawNervePreparationPreservesCochain,
      testCase "reduced nerve preparation maps reduced basis cells back to original cells" testReducedNervePreparationCoversCriticalCells,
      testCase "spectral-ready nerve preparation computes pages on the reduced complex" testSpectralReadyNervePreparationUsesReducedComplex,
      testCase "single-context grothendieck builders agree with the generic nerve fixture" testGrothendieckCohomologyBuilders
    ]

testInterfaceName :: String -> InterfaceName tag
testInterfaceName =
  interfaceNameFromString

data FailingComposeTag

data FailingComposeError
  = FailingInnerFaceComposition
  deriving stock (Eq, Show)

instance NerveSiteAlgebra FailingComposeTag where
  type NerveCategory FailingComposeTag = FinCat
  type NerveSource FailingComposeTag = FinObj
  type NerveMorphism FailingComposeTag = FinMor

  buildSiteNerve :: FinCat -> Natural -> TruncatedNormalizedSSet (NerveSimplex FinCat)
  buildSiteNerve =
    nerve

  simplexSourceValue =
    chainStartObject . nerveSimplexChain

  simplexMorphismChain =
    chainMorphisms . nerveSimplexChain

instance InterfaceDomain FailingComposeTag where
  type InterfaceObject FailingComposeTag = FinObj
  type InterfaceMorphism FailingComposeTag = FinMor
  type InterfaceComposeError FailingComposeTag = FailingComposeError

  measureObject _ =
    mempty

  measureMorphism _ =
    InterfaceMeasure
      { imBoundNames = mempty,
        imDeletedNames = mempty,
        imCreatedNames = mempty,
        imGuarded = Any False
      }

  composeMorphismChain _ =
    Left FailingInnerFaceComposition

testInterfaceStalkBasisAtoms :: Assertion
testInterfaceStalkBasisAtoms =
  assertEqual
    "basis atoms should reflect the interface stalk feature set"
    [ BoundNameAtom (testInterfaceName "x"),
      BoundNameAtom (testInterfaceName "y"),
      DeletedNameAtom (testInterfaceName "z"),
      GuardedAtom,
      WitnessAtom WitnessTerminal
    ]
    (interfaceStalkBasisAtoms sourceInterfaceStalk)

testInterfaceStalkSignatureNamesQuotientSemantics :: Assertion
testInterfaceStalkSignatureNamesQuotientSemantics =
  case allMorphisms sampleFinCat of
    firstMorphism : secondMorphism : _ -> do
      let leftStalk = sourceInterfaceStalk {rsWitness = ComposedWitness firstMorphism}
          rightStalk = sourceInterfaceStalk {rsWitness = ComposedWitness secondMorphism}
      assertEqual
        "signature equality intentionally quotients composed witnesses by witness class"
        (interfaceStalkSignature leftStalk)
        (interfaceStalkSignature rightStalk)
    _ ->
      assertFailure "sample category must expose at least two morphisms for witness quotient testing"

testInterfaceStalkExactEqualityDistinguishesWitnessPayloads :: Assertion
testInterfaceStalkExactEqualityDistinguishesWitnessPayloads =
  case allMorphisms sampleFinCat of
    firstMorphism : secondMorphism : _ ->
      assertBool
        "exact equality must preserve the witness payload instead of collapsing to witness class"
        ( not
            ( interfaceStalkExactEq
                (sourceInterfaceStalk {rsWitness = ComposedWitness firstMorphism})
                (sourceInterfaceStalk {rsWitness = ComposedWitness secondMorphism})
            )
        )
    _ ->
      assertFailure "sample category must expose at least two morphisms for exact witness testing"

testInterfaceStalkFeatureLinearization :: Assertion
testInterfaceStalkFeatureLinearization =
  case Map.lookup ("source", "target") (linearizedRestrictionComparableRestrictions featureRestrictionModel) of
    Nothing ->
      assertFailure "expected a comparable restriction from source to target"
    Just incidenceValue -> do
      assertEqual "expected the source stalk basis dimension" (Just 5) (Map.lookup "source" (linearizedRestrictionStalkDimensions featureRestrictionModel))
      assertEqual "expected the target stalk basis dimension" (Just 4) (Map.lookup "target" (linearizedRestrictionStalkDimensions featureRestrictionModel))
      case
        mkBoundaryIncidence
          5
          4
          [ mkBoundaryEntry 1 0 1,
            mkBoundaryEntry 3 2 1,
            mkBoundaryEntry 4 3 1
          ]
        of
        Left failure ->
          assertFailure (show failure)
        Right expectedIncidence ->
          assertEqual "expected the shared feature incidence" expectedIncidence incidenceValue

testConstantRestrictionLinearization :: Assertion
testConstantRestrictionLinearization =
  case mkBoundaryIncidence 1 1 [mkBoundaryEntry 0 0 1] of
    Left failure ->
      assertFailure (show failure)
    Right expectedIncidence ->
      assertEqual
        "constant restriction linearization should preserve a one-dimensional stalk"
        (Just expectedIncidence)
        (Map.lookup ("upper", "lower") (linearizedRestrictionComparableRestrictions constantRestrictionModel))

testNerveRestrictionProjectionReportsFailedInnerFaceComposition :: Assertion
testNerveRestrictionProjectionReportsFailedInnerFaceComposition =
  case buildNerveRestrictions (mkNerveSite @FailingComposeTag sampleFinCat 2) of
    Left
      ( SiteRestrictionProjectionBuildError
          (FaceStalkProjectionAdjacentCompositionFailed _ FailingInnerFaceComposition)
        ) ->
        pure ()
    Left otherFailure ->
      assertFailure ("expected inner-face composition projection failure, received " <> show otherFailure)
    Right _ ->
      assertFailure "expected failed inner-face composition to reject nerve restriction construction"

testNerveSkeletonPreservesSimplexCountsAndFaceRows :: Assertion
testNerveSkeletonPreservesSimplexCountsAndFaceRows = do
  assertEqual
    "nerve cells by dimension must match simplices by dimension"
    (fmap (nerveSimplexCountAt sampleNerveSite) (dimensionsThrough (nerveSiteDepth sampleNerveSite)))
    (fmap (nerveCellCountAt sampleNerveSite) (dimensionsThrough (nerveSiteDepth sampleNerveSite)))
  assertBool
    "nerve face rows must lower dimension by one and preserve alternating orientation"
    (all nerveFaceIsOrientedBoundary (siteFaceMorphisms sampleNerveSite))

testNerveCochainWindowAvoidsHighDimensionalMaterialization :: Assertion
testNerveCochainWindowAvoidsHighDimensionalMaterialization =
  case linearOrderCategory 4 of
    Left categoryError ->
      assertFailure ("expected the linear-order fixture category to build, received " <> categoryError)
    Right categoryValue -> do
      let fullDepthTwoSite = mkNerveSite @SampleSiteTag categoryValue 2
          fullDepthThreeSite = mkNerveSite @SampleSiteTag categoryValue 3
          windowSite = mkNerveSiteWindow @SampleSiteTag categoryValue (cochainSupportWindow 1)
          lowDimensions = [0, 1, 2]
      assertBool
        "fixture sanity check: full depth-3 site must actually contain degree-3 cells"
        (not (null (siteCellsAtDimension fullDepthThreeSite 3)))
      assertEqual
        "cochain window depth should be exactly the d0/d1 support, not the caller's whole cathedral"
        2
        (nerveSiteDepth windowSite)
      assertEqual
        "cochain window must preserve the same low-dimensional cells as the full depth-2 site"
        (fmap (siteCellsAtDimension fullDepthTwoSite) lowDimensions)
        (fmap (siteCellsAtDimension windowSite) lowDimensions)
      assertEqual
        "cochain window must not materialize irrelevant degree-3 cells"
        []
        (siteCellsAtDimension windowSite 3)
      assertEqual
        "cochain window must preserve exactly the d0/d1 face morphisms"
        (siteFaceMorphisms fullDepthTwoSite)
        (siteFaceMorphisms windowSite)
      assertEqual
        "category-window explicit cochain must agree with full depth-2 site materialization"
        (explicitNerveCochain fullDepthTwoSite)
        (explicitNerveCochain windowSite)
      assertEqual
        "category-window rank-one cochain must agree with full depth-2 site materialization"
        (rankOneNerveCochain (\_ _ _ -> 1) fullDepthTwoSite)
        (rankOneNerveCochain (\_ _ _ -> 1) windowSite)

testWCOJNerveRowsMatchSimplicialWindowRows :: Assertion
testWCOJNerveRowsMatchSimplicialWindowRows =
  case linearOrderCategory 4 of
    Left categoryError ->
      assertFailure ("expected the linear-order WCOJ fixture category to build, received " <> categoryError)
    Right categoryValue -> do
      let simplicialWindowSite = mkNerveSiteWindow @SampleSiteTag categoryValue (cochainSupportWindow 1)
          lowDimensions = [0, 1, 2]
      wcojWindowSite <-
        assertRight
          "expected WCOJ-window site construction to succeed"
          (mkNerveSiteWCOJWindow @SampleSiteTag categoryValue (cochainSupportWindow 1))
      assertEqual
        "WCOJ source must preserve cochain-window depth"
        (nerveSiteDepth simplicialWindowSite)
        (nerveSiteDepth wcojWindowSite)
      assertEqual
        "WCOJ source must produce the same low-dimensional simplex rows as the simplicial source"
        (fmap (nerveCellSignatureSetAt simplicialWindowSite) lowDimensions)
        (fmap (nerveCellSignatureSetAt wcojWindowSite) lowDimensions)
      assertEqual
        "WCOJ source must produce the same face rows as the simplicial source"
        (nerveFaceSignatureSet simplicialWindowSite)
        (nerveFaceSignatureSet wcojWindowSite)
      case explicitNerveCochain wcojWindowSite of
        Left cochainError ->
          assertFailure ("expected WCOJ-window cochain construction to succeed, received " <> show cochainError)
        Right cochainValue ->
          assertBool
            "WCOJ-window cochain must preserve nilpotence"
            (checkCoboundaryNilpotence cochainValue)
      case rankOneNerveCochain (\_ _ _ -> 1) wcojWindowSite of
        Left cochainError ->
          assertFailure ("expected WCOJ-window rank-one cochain construction to succeed, received " <> show cochainError)
        Right cochainValue ->
          assertBool
            "WCOJ-window rank-one cochain must preserve nilpotence"
            (checkCoboundaryNilpotence cochainValue)

testDenseOrdinalNerveRowsMatchSimplicialWindowRows :: Assertion
testDenseOrdinalNerveRowsMatchSimplicialWindowRows =
  case linearOrderCategory 4 of
    Left categoryError ->
      assertFailure ("expected the linear-order dense fixture category to build, received " <> categoryError)
    Right categoryValue -> do
      let simplicialWindowSite = mkNerveSiteWindow @SampleSiteTag categoryValue (cochainSupportWindow 1)
          denseWindowSiteResult = mkNerveSiteDenseWindow @SampleSiteTag categoryValue (cochainSupportWindow 1)
          lowDimensions = [0, 1, 2]
      denseWindowSite <-
        either
          (\arrangementError -> assertFailure ("expected dense-window site construction to succeed, received " <> show arrangementError))
          pure
          denseWindowSiteResult
      assertEqual
        "dense source must preserve cochain-window depth"
        (nerveSiteDepth simplicialWindowSite)
        (nerveSiteDepth denseWindowSite)
      assertEqual
        "dense source must produce the same low-dimensional simplex rows as the simplicial source"
        (fmap (nerveCellSignatureSetAt simplicialWindowSite) lowDimensions)
        (fmap (nerveCellSignatureSetAt denseWindowSite) lowDimensions)
      assertEqual
        "dense source must produce the same face rows as the simplicial source"
        (nerveFaceSignatureSet simplicialWindowSite)
        (nerveFaceSignatureSet denseWindowSite)
      case explicitNerveCochain denseWindowSite of
        Left cochainError ->
          assertFailure ("expected dense-window cochain construction to succeed, received " <> show cochainError)
        Right cochainValue ->
          assertBool
            "dense-window cochain must preserve nilpotence"
            (checkCoboundaryNilpotence cochainValue)
      case rankOneNerveCochain (\_ _ _ -> 1) denseWindowSite of
        Left cochainError ->
          assertFailure ("expected dense-window rank-one cochain construction to succeed, received " <> show cochainError)
        Right cochainValue ->
          assertBool
            "dense-window rank-one cochain must preserve nilpotence"
            (checkCoboundaryNilpotence cochainValue)

testPreparedDenseNervePlanProjectsDenseWindow :: Assertion
testPreparedDenseNervePlanProjectsDenseWindow = do
  categoryValue <- linearOrderCategoryFixture "prepared-dense projection"
  denseWindowSite <- assertRight "expected dense-window site construction to succeed" (mkNerveSiteDenseWindow @SampleSiteTag categoryValue (cochainSupportWindow 1))
  preparedPlan <- assertRight "expected prepared dense plan to succeed" (prepareDenseNerveCochainPlan @SampleSiteTag categoryValue 1)
  projectedSite <- assertRight "expected prepared dense projection to succeed" (projectPreparedDenseNerveSite preparedPlan)
  let lowDimensions = [0, 1, 2]
  assertEqual
    "prepared dense projection must preserve low-dimensional rows"
    (fmap (nerveCellSignatureSetAt denseWindowSite) lowDimensions)
    (fmap (nerveCellSignatureSetAt projectedSite) lowDimensions)
  assertEqual
    "prepared dense projection must preserve face rows"
    (nerveFaceSignatureSet denseWindowSite)
    (nerveFaceSignatureSet projectedSite)

testPreparedDenseNerveScaffoldMatchesDenseWindow :: Assertion
testPreparedDenseNerveScaffoldMatchesDenseWindow = do
  categoryValue <- linearOrderCategoryFixture "prepared scaffold"
  denseWindowSite <- assertRight "expected dense-window site construction to succeed" (mkNerveSiteDenseWindow @SampleSiteTag categoryValue (cochainSupportWindow 1))
  richScaffold <- assertRight "expected dense-window scaffold construction to succeed" (mkNerveComplexScaffold denseWindowSite)
  preparedPlan <- assertRight "expected prepared dense plan to succeed" (prepareDenseNerveCochainPlan @SampleSiteTag categoryValue 1)
  preparedScaffold <- assertRight "expected prepared dense scaffold construction to succeed" (preparedDenseNerveComplexScaffold preparedPlan)
  assertEqual
    "prepared dense scaffold must preserve topological boundary incidence by degree"
    (finiteComplexIncidenceSignatures (scsChainComplex richScaffold))
    (finiteComplexIncidenceSignatures (scsChainComplex preparedScaffold))

testPreparedDenseNerveExplicitCochainMatchesDenseWindow :: Assertion
testPreparedDenseNerveExplicitCochainMatchesDenseWindow = do
  categoryValue <- linearOrderCategoryFixture "prepared explicit"
  denseWindowSite <- assertRight "expected dense-window site construction to succeed" (mkNerveSiteDenseWindow @SampleSiteTag categoryValue (cochainSupportWindow 1))
  richComplex <- assertRight "expected dense-window cochain construction to succeed" (explicitNerveCochain denseWindowSite)
  preparedPlan <- assertRight "expected prepared dense plan to succeed" (prepareDenseNerveCochainPlan @SampleSiteTag categoryValue 1)
  preparedComplex <- assertRight "expected prepared explicit cochain construction to succeed" (materializePreparedDenseNerveCoboundaryComplex interfaceStalkBasisLinearization preparedPlan)
  assertEqual
    "prepared explicit cochain must preserve dense-window sparse incidence by degree"
    (cochainIncidenceSignatures richComplex)
    (cochainIncidenceSignatures preparedComplex)

testPreparedDenseNerveRankOneCochainAndDenseApply :: Assertion
testPreparedDenseNerveRankOneCochainAndDenseApply = do
  categoryValue <- linearOrderCategoryFixture "prepared rank-one"
  denseWindowSite <- assertRight "expected dense-window site construction to succeed" (mkNerveSiteDenseWindow @SampleSiteTag categoryValue (cochainSupportWindow 1))
  richComplex <- assertRight "expected dense-window rank-one cochain construction to succeed" (rankOneNerveCochain (\_ _ _ -> 1) denseWindowSite)
  preparedPlan <- assertRight "expected prepared dense plan to succeed" (prepareDenseNerveCochainPlan @SampleSiteTag categoryValue 1)
  preparedComplex <- assertRight "expected prepared rank-one cochain construction to succeed" (materializePreparedDenseNerveRankOneCoboundaryComplexWith (\_ _ _ -> 1) preparedPlan)
  assertEqual
    "prepared rank-one cochain must preserve dense-window rank-one sparse incidence by degree"
    (cochainIncidenceSignatures richComplex)
    (cochainIncidenceSignatures preparedComplex)
  case Map.lookup (HomologicalDegree 0) (gradedOperatorsByDegree preparedComplex) of
    Nothing ->
      assertFailure "expected a degree-0 prepared rank-one differential"
    Just differentialValue -> do
      let incidenceValue = gradedOperatorIncidence differentialValue
          sourceVector = Unboxed.fromList [1 .. sourceCardinality incidenceValue]
          expectedVector = denseVectorFromSparseApply incidenceValue sourceVector
      denseResult <- assertRight "expected prepared rank-one dense apply to succeed" (applyPreparedDenseNerveRankOneCoboundaryDense (\_ _ _ -> 1) preparedPlan (HomologicalDegree 0) sourceVector)
      assertEqual
        "prepared rank-one dense apply must agree with materialized sparse incidence"
        expectedVector
        denseResult

testPreparedDenseNerveReportsFailedAdjacentComposition :: Assertion
testPreparedDenseNerveReportsFailedAdjacentComposition =
  case prepareDenseNerveCochainPlan @FailingComposeTag sampleFinCat 1 of
    Left (PreparedDenseNerveAdjacentCompositionFailed {}) ->
      pure ()
    Left otherFailure ->
      assertFailure ("expected prepared dense adjacent composition failure, received " <> show otherFailure)
    Right _ ->
      assertFailure "expected prepared dense plan construction to reject failed adjacent interface composition"

linearOrderCategoryFixture :: String -> IO FinCat
linearOrderCategoryFixture fixtureName =
  assertRight
    ("expected the linear-order " <> fixtureName <> " fixture category to build")
    (linearOrderCategory 4)

assertRight :: Show errorValue => String -> Either errorValue value -> IO value
assertRight successMessage =
  either
    (\errorValue -> assertFailure (successMessage <> ", received " <> show errorValue))
    pure

type SampleNerveCochain = GradedComplex (NerveCell SampleSiteTag) Int

type SampleNerveInput = SiteCochainInput (NerveSite SampleSiteTag) (NerveCell SampleSiteTag)

type SampleNerveLaplacian = SheafLaplacian HodgeLaplacian (NerveCell SampleSiteTag)

type SampleNerveWitness = SiteRestrictionWitness (FaceMorphism SampleSiteTag) (InterfaceStalk SampleSiteTag)

type SampleNerveRestriction = Restriction (NerveCell SampleSiteTag) SampleNerveWitness

type SampleNerveArtifactBuilder artifact = SampleNerveCochain -> Either (SheafOperatorBuildError (NerveCell SampleSiteTag)) artifact

type SampleGrothendieckCochain = GradedComplex (GrothendieckCell SampleSystem) Int

type SampleGrothendieckInput = SiteCochainInput (GrothendieckSite SampleSystem) (GrothendieckCell SampleSystem)

type SampleGrothendieckLaplacian = SheafLaplacian HodgeLaplacian (GrothendieckCell SampleSystem)

type SampleGrothendieckArtifactBuilder artifact = SampleGrothendieckCochain -> Either (SheafOperatorBuildError (GrothendieckCell SampleSystem)) artifact

showCochainFailure :: Show errorValue => Either errorValue artifact -> Either String artifact
showCochainFailure =
  first show

explicitNerveArtifact ::
  SampleNerveArtifactBuilder artifact -> SampleNerveInput -> Either String artifact
explicitNerveArtifact buildArtifact =
  showCochainFailure . buildNerveCochainArtifact (ExplicitSiteCoboundary interfaceStalkBasisLinearization) buildArtifact

explicitNerveCochain :: NerveSite SampleSiteTag -> Either String SampleNerveCochain
explicitNerveCochain =
  explicitNerveArtifact Right . MaterializedSite

rankOneNerveArtifact ::
  (SampleNerveRestriction -> InterfaceStalk SampleSiteTag -> InterfaceStalk SampleSiteTag -> Int) ->
  SampleNerveInput ->
  Either String SampleNerveCochain
rankOneNerveArtifact scalarCoefficient =
  showCochainFailure . buildNerveCochainArtifact (RankOneSiteCoboundary scalarCoefficient) Right

rankOneNerveCochain ::
  (SampleNerveRestriction -> InterfaceStalk SampleSiteTag -> InterfaceStalk SampleSiteTag -> Int) ->
  NerveSite SampleSiteTag ->
  Either String SampleNerveCochain
rankOneNerveCochain scalarCoefficient =
  rankOneNerveArtifact scalarCoefficient . MaterializedSite

scaffoldedNerveCochain ::
  SiteComplexScaffold (NerveSite SampleSiteTag) (NerveCell SampleSiteTag) ->
  Either String SampleNerveCochain
scaffoldedNerveCochain =
  explicitNerveArtifact Right . ScaffoldedSite

nerveHodgeLaplacian0From :: NerveSite SampleSiteTag -> Either String SampleNerveLaplacian
nerveHodgeLaplacian0From =
  explicitNerveArtifact buildHodgeLaplacian0 . MaterializedSite

nerveHodgeLaplacian1From :: NerveSite SampleSiteTag -> Either String SampleNerveLaplacian
nerveHodgeLaplacian1From =
  explicitNerveArtifact buildHodgeLaplacian1 . MaterializedSite

rankOneNerveLaplacian ::
  SampleNerveArtifactBuilder SampleNerveLaplacian ->
  NerveSite SampleSiteTag ->
  Either String SampleNerveLaplacian
rankOneNerveLaplacian buildArtifact =
  showCochainFailure
    . buildNerveCochainArtifact (RankOneSiteCoboundary (\_ _ _ -> 1)) buildArtifact
    . MaterializedSite

explicitGrothendieckArtifact ::
  SampleGrothendieckArtifactBuilder artifact -> SampleGrothendieckInput -> Either String artifact
explicitGrothendieckArtifact buildArtifact =
  showCochainFailure . buildGrothendieckCochainArtifact (ExplicitSiteCoboundary interfaceStalkBasisLinearization) buildArtifact

explicitGrothendieckCochain :: GrothendieckSite SampleSystem -> Either String SampleGrothendieckCochain
explicitGrothendieckCochain =
  explicitGrothendieckArtifact Right . MaterializedSite

grothendieckHodgeLaplacian0From :: GrothendieckSite SampleSystem -> Either String SampleGrothendieckLaplacian
grothendieckHodgeLaplacian0From =
  explicitGrothendieckArtifact buildHodgeLaplacian0 . MaterializedSite

grothendieckHodgeLaplacian1From :: GrothendieckSite SampleSystem -> Either String SampleGrothendieckLaplacian
grothendieckHodgeLaplacian1From =
  explicitGrothendieckArtifact buildHodgeLaplacian1 . MaterializedSite

testNerveSiteRestrictionRecomputesDerivedCaches :: Assertion
testNerveSiteRestrictionRecomputesDerivedCaches = do
  let keptCells = filter ((<= 1) . nerveCellDimension) (nerveSiteCells sampleNerveSite)
      keptCellSet = Set.fromList keptCells
      restrictedSite =
        restrictNerveSiteToCellKeys
          (Set.map nerveCellKey keptCellSet)
          sampleNerveSite
      expectedFaces =
        filter
          ( \faceValue ->
              Set.member (faceMorphismSource faceValue) keptCellSet
                && Set.member (faceMorphismTarget faceValue) keptCellSet
          )
          (siteFaceMorphisms sampleNerveSite)
  assertEqual
    "restricted site cells must match the retained key cover"
    keptCells
    (nerveSiteCells restrictedSite)
  assertEqual
    "restricted site cells-by-dimension cache must be rebuilt from retained cells"
    (fmap (filter ((<= 1) . nerveCellDimension) . siteCellsAtDimension sampleNerveSite) (dimensionsThrough (nerveSiteDepth sampleNerveSite)))
    (fmap (siteCellsAtDimension restrictedSite) (dimensionsThrough (nerveSiteDepth restrictedSite)))
  assertEqual
    "restricted site basis must be rebuilt from retained cells"
    keptCells
    (basisCells (nerveSiteBasis restrictedSite))
  assertEqual
    "restricted site faces must keep only faces whose endpoints survived"
    expectedFaces
    (siteFaceMorphisms restrictedSite)

testGrothendieckSkeletonPreservesSimplexCountsAndFaceRows :: Assertion
testGrothendieckSkeletonPreservesSimplexCountsAndFaceRows = do
  assertEqual
    "Grothendieck cells by dimension must match simplices by dimension"
    (fmap (grothendieckSimplexCountAt sampleGrothendieckSite) (dimensionsThrough (grothendieckSiteDepth sampleGrothendieckSite)))
    (fmap (grothendieckCellCountAt sampleGrothendieckSite) (dimensionsThrough (grothendieckSiteDepth sampleGrothendieckSite)))
  assertBool
    "Grothendieck face rows must lower dimension by one and preserve alternating orientation"
    (all grothendieckFaceIsOrientedBoundary (grothendieckSiteFaceMorphisms sampleGrothendieckSite))

testGrothendieckCochainWindowAvoidsHighDimensionalMaterialization :: Assertion
testGrothendieckCochainWindowAvoidsHighDimensionalMaterialization =
  case linearOrderCategory 4 of
    Left categoryError ->
      assertFailure ("expected the linear-order Grothendieck fixture category to build, received " <> categoryError)
    Right categoryValue -> do
      let systemValue = SampleSystem categoryValue
          fullDepthTwoSite = mkGrothendieckSite systemValue 2
          fullDepthThreeSite = mkGrothendieckSite systemValue 3
          windowSite =
            mkGrothendieckSiteWindow
              systemValue
              (cochainSupportWindow 1)
          lowDimensions = [0, 1, 2]
      assertBool
        "fixture sanity check: full Grothendieck depth-3 site must actually contain degree-3 cells"
        (not (null (grothendieckSiteCellsAtDimension fullDepthThreeSite 3)))
      assertEqual
        "Grothendieck cochain window depth should be exactly the d0/d1 support"
        2
        (grothendieckSiteDepth windowSite)
      assertEqual
        "Grothendieck cochain window must preserve low-dimensional cells"
        (fmap (grothendieckSiteCellsAtDimension fullDepthTwoSite) lowDimensions)
        (fmap (grothendieckSiteCellsAtDimension windowSite) lowDimensions)
      assertEqual
        "Grothendieck cochain window must not materialize irrelevant degree-3 cells"
        []
        (grothendieckSiteCellsAtDimension windowSite 3)
      assertEqual
        "Grothendieck cochain window must preserve exactly the d0/d1 face morphisms"
        (grothendieckSiteFaceMorphisms fullDepthTwoSite)
        (grothendieckSiteFaceMorphisms windowSite)
      assertEqual
        "system-window Grothendieck cochain must agree with full depth-2 site materialization"
        (explicitGrothendieckCochain fullDepthTwoSite)
        (explicitGrothendieckCochain windowSite)

testNerveCohomologyBuilders :: Assertion
testNerveCohomologyBuilders =
  case explicitNerveCochain sampleNerveSite of
    Left shapeError ->
      assertFailure ("expected generic nerve coboundary materialization to succeed, received " <> show shapeError)
    Right coboundaryCacheValue -> do
      assertBool
        "expected the generic nerve coboundary to be nilpotent"
        (checkCoboundaryNilpotence coboundaryCacheValue)
      case (nerveHodgeLaplacian0From sampleNerveSite, nerveHodgeLaplacian1From sampleNerveSite) of
        (Left shapeError, _) ->
          assertFailure ("expected the degree-0 Hodge Laplacian to materialize, received " <> show shapeError)
        (_, Left shapeError) ->
          assertFailure ("expected the degree-1 Hodge Laplacian to materialize, received " <> show shapeError)
        (Right laplacian0, Right laplacian1) -> do
          assertBool
            "expected the degree-0 Hodge Laplacian to have positive support"
            (laplacianDomainCardinality laplacian0 > 0)
          assertBool
            "expected the degree-1 Hodge Laplacian to have positive support"
            (laplacianDomainCardinality laplacian1 > 0)

testRankOneNerveCohomologyBuilder :: Assertion
testRankOneNerveCohomologyBuilder =
  case rankOneNerveCochain (\_ _ _ -> 1) sampleNerveSite of
    Left shapeError ->
      assertFailure ("expected rank-one nerve coboundary materialization to succeed, received " <> show shapeError)
    Right rankOneCache -> do
      assertBool
        "expected rank-one nerve coboundary to preserve nilpotence"
        (checkCoboundaryNilpotence rankOneCache)
      assertEqual
        "rank-one source space should have exactly one coordinate per degree-0 cell"
        (Just (length (siteCellsAtDimension sampleNerveSite 0)))
        (zeroDifferentialSourceCardinality rankOneCache)

testRankOneConstantSheafBettiAnchors :: Assertion
testRankOneConstantSheafBettiAnchors = do
  circleSite <- assertRight "expected triangulated circle site construction to succeed" triangulatedCircleSite
  treeSite <- assertRight "expected contractible tree site construction to succeed" contractibleTreeSite
  assertEqual
    "triangulated circle fixture must expose three vertices, three cyclic overlaps, and no filled two-cell"
    [3, 3, 0]
    (siteCellCountsThroughTwo circleSite)
  assertEqual
    "contractible tree fixture must expose three vertices, two overlaps, and no filled two-cell"
    [3, 2, 0]
    (siteCellCountsThroughTwo treeSite)
  circleCochain <- assertRight "expected triangulated circle rank-one cochain construction to succeed" (rankOneNerveCochain (\_ _ _ -> 1) circleSite)
  treeCochain <- assertRight "expected contractible tree rank-one cochain construction to succeed" (rankOneNerveCochain (\_ _ _ -> 1) treeSite)
  assertBool
    "triangulated circle constant-sheaf coboundary must remain nilpotent"
    (checkCoboundaryNilpotence circleCochain)
  assertBool
    "contractible tree constant-sheaf coboundary must remain nilpotent"
    (checkCoboundaryNilpotence treeCochain)
  circleBettiRanks <- assertRight "expected triangulated circle Betti rank report to succeed" (cochainBettiRanks circleCochain)
  treeBettiRanks <- assertRight "expected contractible tree Betti rank report to succeed" (cochainBettiRanks treeCochain)
  assertEqual
    "triangulated circle constant sheaf must have Betti ranks H0=1, H1=1"
    [1, 1, 0]
    circleBettiRanks
  assertEqual
    "contractible tree constant sheaf must have Betti ranks H0=1, H1=0"
    [1, 0, 0]
    treeBettiRanks

testRankOneConstantSheafHarmonicAnchors :: Assertion
testRankOneConstantSheafHarmonicAnchors = do
  circleSite <- assertRight "expected triangulated circle site construction to succeed" triangulatedCircleSite
  treeSite <- assertRight "expected contractible tree site construction to succeed" contractibleTreeSite
  circleHarmonicRanks <-
    traverse
      (laplacianHarmonicRankFrom circleSite)
      [buildHodgeLaplacian0, buildHodgeLaplacian1]
  treeHarmonicRanks <-
    traverse
      (laplacianHarmonicRankFrom treeSite)
      [buildHodgeLaplacian0, buildHodgeLaplacian1]
  assertEqual
    "triangulated circle harmonic kernels must realize the Hodge isomorphism with Betti ranks H0=1, H1=1"
    [1, 1]
    circleHarmonicRanks
  assertEqual
    "contractible tree harmonic kernels must realize the Hodge isomorphism with Betti ranks H0=1, H1=0"
    [1, 0]
    treeHarmonicRanks

laplacianHarmonicRankFrom ::
  NerveSite SampleSiteTag ->
  SampleNerveArtifactBuilder SampleNerveLaplacian ->
  IO Int
laplacianHarmonicRankFrom siteValue buildArtifact = do
  laplacianValue <-
    assertRight
      "expected the rank-one Hodge Laplacian to materialize"
      (rankOneNerveLaplacian buildArtifact siteValue)
  assertRight
    "expected the Hodge Laplacian kernel rank to compute over the rationals"
    (laplacianHarmonicRank laplacianValue)

testDefaultNerveCohomologyRemainsExplicitBlock :: Assertion
testDefaultNerveCohomologyRemainsExplicitBlock =
  case (explicitNerveCochain sampleNerveSite, rankOneNerveCochain (\_ _ _ -> 1) sampleNerveSite) of
    (Left shapeError, _) ->
      assertFailure ("expected explicit nerve coboundary materialization to succeed, received " <> show shapeError)
    (_, Left shapeError) ->
      assertFailure ("expected rank-one nerve coboundary materialization to succeed, received " <> show shapeError)
    (Right explicitCache, Right rankOneCache) ->
      assertBool
        "default interface-feature cohomology must remain the explicit block path, not a rank-one shortcut"
        (zeroDifferentialSourceCardinality explicitCache > zeroDifferentialSourceCardinality rankOneCache)

testScaffoldBackedNerveCohomologyMatchesSiteBacked :: Assertion
testScaffoldBackedNerveCohomologyMatchesSiteBacked =
  case mkNerveComplexScaffold sampleNerveSite of
    Left scaffoldError ->
      assertFailure ("expected scaffold construction to succeed, received " <> show scaffoldError)
    Right scaffoldValue ->
      assertEqual
        "scaffold-backed cochain preparation must preserve the site-backed coboundary result"
        (explicitNerveCochain sampleNerveSite)
        (scaffoldedNerveCochain scaffoldValue)

testRawNervePreparationPreservesCochain :: Assertion
testRawNervePreparationPreservesCochain =
  case (prepareRawNerveCochain sampleNerveSite, explicitNerveCochain sampleNerveSite) of
    (Left preparationError, _) ->
      assertFailure ("expected raw nerve preparation to succeed, received " <> show preparationError)
    (_, Left cochainError) ->
      assertFailure ("expected raw nerve coboundary construction to succeed, received " <> show cochainError)
    (Right rawIteration, Right cochainValue) ->
      assertEqual
        "raw preparation must preserve the raw coboundary complex instead of secretly reducing it"
        cochainValue
        (rsciCochainComplex (rawIterationValue rawIteration))

testReducedNervePreparationCoversCriticalCells :: Assertion
testReducedNervePreparationCoversCriticalCells =
  case prepareNerveCochainReduced sampleNerveSite of
    Left preparationError ->
      assertFailure ("expected reduced nerve preparation to succeed, received " <> show preparationError)
    Right reducedIteration -> do
      let reducedValue = morseReducedIterationValue reducedIteration
      let morseValue = smrMorseComplex (mrscReduction reducedValue)
          criticalCellsByReducedBasis = smrCriticalCellByReducedBasis (mrscReduction reducedValue)
          reducedBasisRefs = basisRefsOfComplex (mcReducedComplex morseValue)
          refinedMorseValue = smrRefinedMorseComplex (mrscReduction reducedValue)
          refinedCriticalCellsByReducedBasis = smrRefinedCriticalCellByReducedBasis (mrscReduction reducedValue)
          refinedReducedBasisRefs = basisRefsOfComplex (rmcReducedComplex refinedMorseValue)
      assertEqual
        "every reduced basis ref must retain original sheaf-cell provenance"
        reducedBasisRefs
        (Map.keys criticalCellsByReducedBasis)
      assertEqual
        "every refined reduced basis ref must retain original sheaf-cell provenance"
        refinedReducedBasisRefs
        (Map.keys refinedCriticalCellsByReducedBasis)

testSpectralReadyNervePreparationUsesReducedComplex :: Assertion
testSpectralReadyNervePreparationUsesReducedComplex =
  case prepareNerveCochainSpectralWith (\_ _ -> 0) (const 0) sampleNerveSite of
    Left preparationError ->
      assertFailure ("expected spectral-ready nerve preparation to succeed, received " <> show preparationError)
    Right spectralIteration ->
      let spectralValue = spectralReadyIterationValue spectralIteration
       in
      let sourceComplex = scsChainComplex (srscSiteComplex spectralValue)
          preparedRefinedMorseValue = frmcRefinedMorseComplex (srscFilteredMorse spectralValue)
          sourceBasisCount = length (basisRefsOfComplex sourceComplex)
          preparedReducedBasisCount = length (basisRefsOfComplex (rmcReducedComplex preparedRefinedMorseValue))
       in case filteredRefinedMorseComplex sourceComplex (srscOriginalFiltration spectralValue) (const 0) of
            Left filteredError ->
              assertFailure ("expected filtered reduced spectral-page preparation to succeed, received " <> show filteredError)
            Right filteredMorseValue ->
              let refinedMorseValue = frmcRefinedMorseComplex filteredMorseValue
               in case computeRationalSpectralPages (rmcReducedComplex refinedMorseValue) (filteredReducedFiltration filteredMorseValue) of
                    Left spectralError ->
                      assertFailure ("expected direct filtered reduced spectral-page computation to succeed, received " <> show spectralError)
                    Right expectedPages -> do
                      assertBool
                        ( "spectral-ready sheaf preparation must feed pages from a strictly smaller filtered Morse complex; raw="
                            <> show sourceBasisCount
                            <> " reduced="
                            <> show preparedReducedBasisCount
                        )
                        (preparedReducedBasisCount < sourceBasisCount)
                      assertEqual
                        "prepared spectral pages must use the filtered reduced complex"
                        (fmap pageIndex expectedPages)
                        (fmap pageIndex (srscSpectralPages spectralValue))
                      assertEqual
                        "prepared spectral entries must match direct filtered reduced computation"
                        (fmap pageEntryMap expectedPages)
                        (fmap pageEntryMap (srscSpectralPages spectralValue))
                      assertEqual
                        "prepared spectral differentials must match direct filtered reduced computation"
                        (fmap pageDifferentialMap expectedPages)
                        (fmap pageDifferentialMap (srscSpectralPages spectralValue))

testGrothendieckCohomologyBuilders :: Assertion
testGrothendieckCohomologyBuilders =
  case (explicitGrothendieckCochain sampleGrothendieckSite, explicitNerveCochain sampleNerveSite) of
    (Left shapeError, _) ->
      assertFailure ("expected generic Grothendieck coboundary materialization to succeed, received " <> show shapeError)
    (_, Left shapeError) ->
      assertFailure ("expected generic nerve coboundary materialization to succeed, received " <> show shapeError)
    (Right grothendieckCache, Right nerveCache) -> do
      assertEqual
        "expected the single-context Grothendieck coboundary to agree with the flat generic nerve"
        (checkCoboundaryNilpotence nerveCache)
        (checkCoboundaryNilpotence grothendieckCache)
      case
        ( nerveHodgeLaplacian0From sampleNerveSite,
          grothendieckHodgeLaplacian0From sampleGrothendieckSite,
          nerveHodgeLaplacian1From sampleNerveSite,
          grothendieckHodgeLaplacian1From sampleGrothendieckSite
        )
        of
        (Left shapeError, _, _, _) ->
          assertFailure ("expected the nerve degree-0 Hodge Laplacian to materialize, received " <> show shapeError)
        (_, Left shapeError, _, _) ->
          assertFailure ("expected the Grothendieck degree-0 Hodge Laplacian to materialize, received " <> show shapeError)
        (_, _, Left shapeError, _) ->
          assertFailure ("expected the nerve degree-1 Hodge Laplacian to materialize, received " <> show shapeError)
        (_, _, _, Left shapeError) ->
          assertFailure ("expected the Grothendieck degree-1 Hodge Laplacian to materialize, received " <> show shapeError)
        (Right nerveLaplacian0, Right grothendieckLaplacian0, Right nerveLaplacian1, Right grothendieckLaplacian1) -> do
          assertEqual
            "expected the degree-0 Hodge domain cardinality to agree across the single-context presentation"
            (laplacianDomainCardinality nerveLaplacian0)
            (laplacianDomainCardinality grothendieckLaplacian0)
          assertEqual
            "expected the degree-1 Hodge domain cardinality to agree across the single-context presentation"
            (laplacianDomainCardinality nerveLaplacian1)
            (laplacianDomainCardinality grothendieckLaplacian1)

dimensionsThrough :: Natural -> [Natural]
dimensionsThrough depthValue = [0 .. depthValue]

nerveSimplexCountAt :: NerveSite tag -> Natural -> Int
nerveSimplexCountAt siteValue dimensionValue =
  length (simplicesAtDimension (nerveSiteSourceNerve siteValue) dimensionValue)

nerveCellCountAt :: NerveSite tag -> Natural -> Int
nerveCellCountAt siteValue dimensionValue =
  length (siteCellsAtDimension siteValue dimensionValue)

grothendieckSimplexCountAt :: GrothendieckSite system -> Natural -> Int
grothendieckSimplexCountAt siteValue dimensionValue =
  length (simplicesAtDimension (grothendieckSiteSourceNerve siteValue) dimensionValue)

grothendieckCellCountAt :: GrothendieckSite system -> Natural -> Int
grothendieckCellCountAt siteValue dimensionValue =
  length (grothendieckSiteCellsAtDimension siteValue dimensionValue)

type NerveSimplexSignature = (FinObj, [FinMor])

type NerveCellSignature = (Natural, NerveSimplexSignature)

type NerveFaceSignature = (NerveCellSignature, NerveCellSignature, FaceKind, Natural, Int)

nerveCellSignatureSetAt :: NerveSite SampleSiteTag -> Natural -> Set.Set NerveCellSignature
nerveCellSignatureSetAt siteValue dimensionValue =
  Set.fromList (fmap nerveCellSignature (siteCellsAtDimension siteValue dimensionValue))

nerveCellSignature :: NerveCell SampleSiteTag -> NerveCellSignature
nerveCellSignature cellValue =
  (nerveCellDimension cellValue, nerveSimplexSignature (nerveCellSimplex cellValue))

nerveSimplexSignature :: NerveSimplex FinCat -> NerveSimplexSignature
nerveSimplexSignature simplexValue =
  let chainValue = nerveSimplexChain simplexValue
   in (chainStartObject chainValue, chainMorphisms chainValue)

nerveFaceSignatureSet :: NerveSite SampleSiteTag -> Set.Set NerveFaceSignature
nerveFaceSignatureSet =
  Set.fromList . fmap nerveFaceSignature . siteFaceMorphisms

nerveFaceSignature :: FaceMorphism SampleSiteTag -> NerveFaceSignature
nerveFaceSignature faceValue =
  ( nerveCellSignature (faceMorphismSource faceValue),
    nerveCellSignature (faceMorphismTarget faceValue),
    faceMorphismKind faceValue,
    faceMorphismFaceIndex faceValue,
    faceMorphismOrientation faceValue
  )

nerveFaceIsOrientedBoundary :: FaceMorphism tag -> Bool
nerveFaceIsOrientedBoundary faceValue =
  let sourceDimension = nerveCellDimension (faceMorphismSource faceValue)
      targetDimension = nerveCellDimension (faceMorphismTarget faceValue)
   in sourceDimension > 0
        && targetDimension + 1 == sourceDimension
        && faceMorphismOrientation faceValue == alternatingFaceOrientation (faceMorphismFaceIndex faceValue)

nerveCellDimension :: NerveCell tag -> Natural
nerveCellDimension =
  ckDimension . nerveCellKey

grothendieckFaceIsOrientedBoundary :: GrothendieckFaceMorphism system -> Bool
grothendieckFaceIsOrientedBoundary faceValue =
  let sourceDimension = grothendieckCellDimension (grothendieckFaceMorphismSource faceValue)
      targetDimension = grothendieckCellDimension (grothendieckFaceMorphismTarget faceValue)
   in sourceDimension > 0
        && targetDimension == sourceDimension - 1
        && grothendieckFaceMorphismOrientation faceValue == alternatingFaceOrientation (grothendieckFaceMorphismFaceIndex faceValue)

alternatingFaceOrientation :: Natural -> Int
alternatingFaceOrientation faceIndex =
  if even faceIndex
    then 1
    else -1

zeroDifferentialSourceCardinality :: GradedComplex cell Int -> Maybe Int
zeroDifferentialSourceCardinality complex =
  sourceCardinality . gradedOperatorIncidence
    <$> Map.lookup (HomologicalDegree 0) (gradedOperatorsByDegree complex)

triangulatedCircleSite :: Either String (NerveSite SampleSiteTag)
triangulatedCircleSite =
  mkNerveSite @SampleSiteTag <$> linearOrderCategory 3 <*> pure 1

contractibleTreeSite :: Either String (NerveSite SampleSiteTag)
contractibleTreeSite =
  mkNerveSite @SampleSiteTag <$> contractibleTreeCategory <*> pure 1

contractibleTreeCategory :: Either String FinCat
contractibleTreeCategory =
  case mkFinCat (Set.fromList (linearOrderObjectIds 3)) contractibleTreeMorphismMap Map.empty of
    Left categoryError ->
      Left (show categoryError)
    Right categoryValue ->
      Right categoryValue

contractibleTreeMorphismMap :: Map.Map (FinObjectId, FinObjectId) [FinMorphismId]
contractibleTreeMorphismMap =
  Map.fromList
    [ ((FinObjectId 0, FinObjectId 1), [contractibleTreeMorphismId 1]),
      ((FinObjectId 0, FinObjectId 2), [contractibleTreeMorphismId 2])
    ]

contractibleTreeMorphismId :: Int -> FinMorphismId
contractibleTreeMorphismId targetKey =
  FinGeneratorMorphismId (FinGeneratorId (20 + targetKey))

siteCellCountsThroughTwo :: NerveSite SampleSiteTag -> [Int]
siteCellCountsThroughTwo siteValue =
  fmap (length . siteCellsAtDimension siteValue) [0, 1, 2]

cochainBettiRanks :: SampleNerveCochain -> Either String [Int]
cochainBettiRanks cochainComplex =
  fmap (fmap freeRank) $
    cochainDualFiniteChainComplex cochainComplex
      >>= first show . runHomologyBackend RationalRankBackend

cochainDualFiniteChainComplex :: SampleNerveCochain -> Either String (FiniteChainComplex Rational)
cochainDualFiniteChainComplex cochainComplex =
  Map.fromAscList
    <$> traverse (cochainDualBoundaryAt cochainComplex) [0, 1, 2]
    >>= \boundariesByDimension ->
      first show $
        mkFiniteChainComplexChecked
          (HomologicalDegree 2)
          ( \(HomologicalDegree dimensionValue) ->
              Map.findWithDefault (emptyBoundaryIncidenceOf 0 0) dimensionValue boundariesByDimension
          )

cochainDualBoundaryAt :: SampleNerveCochain -> Int -> Either String (Int, BoundaryIncidence Rational)
cochainDualBoundaryAt cochainComplex dimensionValue =
  fmap
    ((,) dimensionValue)
    ( case dimensionValue of
        0 ->
          fmap
            ( \zeroDifferential ->
                emptyBoundaryIncidenceOf
                  (fromIntegral (sourceCardinality zeroDifferential))
                  0
            )
            (cochainDifferentialIncidenceAt (HomologicalDegree 0) cochainComplex)
        1 ->
          fmap
            cochainBoundaryFromCoboundary
            (cochainDifferentialIncidenceAt (HomologicalDegree 0) cochainComplex)
        2 ->
          fmap
            cochainBoundaryFromCoboundary
            (cochainDifferentialIncidenceAt (HomologicalDegree 1) cochainComplex)
        _ ->
          Right (emptyBoundaryIncidenceOf 0 0)
    )

cochainDifferentialIncidenceAt :: HomologicalDegree -> SampleNerveCochain -> Either String (BoundaryIncidence Int)
cochainDifferentialIncidenceAt degreeValue cochainComplex =
  maybe
    (Left ("missing cochain differential at " <> show degreeValue))
    (Right . gradedOperatorIncidence)
    (Map.lookup degreeValue (gradedOperatorsByDegree cochainComplex))

cochainBoundaryFromCoboundary :: BoundaryIncidence Int -> BoundaryIncidence Rational
cochainBoundaryFromCoboundary =
  mapBoundaryCoefficients fromIntegral . transposeBoundaryIncidence

laplacianHarmonicRank :: SampleNerveLaplacian -> Either String Int
laplacianHarmonicRank laplacianValue =
  laplacianRankComplex laplacianValue
    >>= first show . runHomologyBackend RationalRankBackend
    >>= harmonicKernelRank

-- toRational is exact on finite Doubles, so the rational rank below is the rank
-- of the shipped operator, not an approximation.
laplacianRankComplex :: SampleNerveLaplacian -> Either String (FiniteChainComplex Rational)
laplacianRankComplex laplacianValue =
  let incidenceValue = mapBoundaryCoefficients toRational (slIncidence laplacianValue)
   in first show
        ( mkFiniteChainComplexChecked
            (HomologicalDegree 1)
            ( \(HomologicalDegree dimensionValue) ->
                case dimensionValue of
                  1 -> incidenceValue
                  0 -> emptyBoundaryIncidenceOf (fromIntegral (targetCardinality incidenceValue)) 0
                  _ -> emptyBoundaryIncidenceOf 0 0
            )
        )

harmonicKernelRank :: [HomologyGroup coefficient] -> Either String Int
harmonicKernelRank homologyGroups =
  case fmap freeRank homologyGroups of
    [_, kernelRank] -> Right kernelRank
    unexpectedRanks -> Left ("expected homology ranks at degrees 0 and 1, received " <> show unexpectedRanks)

type IncidenceSignature = (Int, Int, [BoundaryEntry Int])

incidenceSignature :: BoundaryIncidence Int -> IncidenceSignature
incidenceSignature incidenceValue =
  ( sourceCardinality incidenceValue,
    targetCardinality incidenceValue,
    boundaryEntries incidenceValue
  )

cochainIncidenceSignatures :: GradedComplex cell Int -> Map.Map HomologicalDegree IncidenceSignature
cochainIncidenceSignatures =
  Map.map (incidenceSignature . gradedOperatorIncidence) . gradedOperatorsByDegree

finiteComplexIncidenceSignatures :: FiniteChainComplex Int -> Map.Map HomologicalDegree IncidenceSignature
finiteComplexIncidenceSignatures finiteComplex =
  let HomologicalDegree maxDegreeValue = maxHomologicalDegree finiteComplex
   in Map.fromList
        [ (degreeValue, incidenceSignature (incidenceMatrixAt finiteComplex degreeValue))
        | degreeValue <- fmap HomologicalDegree (enumerateBetween 0 maxDegreeValue)
        ]

denseVectorFromSparseApply :: BoundaryIncidence Int -> Unboxed.Vector Int -> Unboxed.Vector Int
denseVectorFromSparseApply incidenceValue sourceVector =
  let sourceMap =
        Map.fromList
          (zip (enumerateFromZero (Unboxed.length sourceVector)) (Unboxed.toList sourceVector))
      targetMap =
        boundaryIncidenceApply incidenceValue sourceMap
   in Unboxed.fromList
        [ Map.findWithDefault 0 targetIndexValue targetMap
        | targetIndexValue <- enumerateFromZero (targetCardinality incidenceValue)
        ]

basisRefsOfComplex :: FiniteChainComplex r -> [BasisCellRef]
basisRefsOfComplex finiteComplex =
  let HomologicalDegree maxDegreeValue = maxHomologicalDegree finiteComplex
   in foldMap
        ( \degreeValue ->
            let homologicalDegreeValue = HomologicalDegree degreeValue
                cardinality = sourceCardinality (incidenceMatrixAt finiteComplex homologicalDegreeValue)
             in fmap
                  (BasisCellRef homologicalDegreeValue)
                  (enumerateFromZero cardinality)
        )
        (enumerateBetween 0 maxDegreeValue)

enumerateFromZero :: Int -> [Int]
enumerateFromZero upperBound
  | upperBound <= 0 = []
  | otherwise = [0 .. upperBound - 1]

enumerateBetween :: Int -> Int -> [Int]
enumerateBetween lowerBound upperBound =
  if upperBound < lowerBound
    then []
    else [lowerBound .. upperBound]

linearOrderCategory :: Int -> Either String FinCat
linearOrderCategory objectCount =
  if objectCount <= 0
    then Left ("linear-order object count must be positive: " <> show objectCount)
    else
      case
        mkFinCat
          (Set.fromList (linearOrderObjectIds objectCount))
          (linearOrderMorphismMap objectCount)
          (linearOrderCompositionMap objectCount)
        of
        Left categoryError ->
          Left (show categoryError)
        Right categoryValue ->
          Right categoryValue

linearOrderMorphismMap :: Int -> Map.Map (FinObjectId, FinObjectId) [FinMorphismId]
linearOrderMorphismMap objectCount =
  Map.fromList
    [ ((sourceId, targetId), [linearOrderMorphismId objectCount sourceId targetId])
    | (sourceId, targetId) <- linearOrderPairs objectCount
    ]

linearOrderCompositionMap :: Int -> Map.Map (FinMorphismId, FinMorphismId) FinMorphismId
linearOrderCompositionMap objectCount =
  Map.fromList
    [ ( ( linearOrderMorphismId objectCount middleId targetId,
          linearOrderMorphismId objectCount sourceId middleId
        ),
        linearOrderMorphismId objectCount sourceId targetId
      )
    | sourceId <- linearOrderObjectIds objectCount,
      middleId <- linearOrderObjectIds objectCount,
      sourceId < middleId,
      targetId <- linearOrderObjectIds objectCount,
      middleId < targetId
    ]

linearOrderPairs :: Int -> [(FinObjectId, FinObjectId)]
linearOrderPairs objectCount =
  [ (sourceId, targetId)
  | sourceId <- linearOrderObjectIds objectCount,
    targetId <- linearOrderObjectIds objectCount,
    sourceId < targetId
  ]

linearOrderObjectIds :: Int -> [FinObjectId]
linearOrderObjectIds objectCount =
  FinObjectId <$> [0 .. objectCount - 1]

linearOrderMorphismId :: Int -> FinObjectId -> FinObjectId -> FinMorphismId
linearOrderMorphismId objectCount (FinObjectId sourceId) (FinObjectId targetId) =
  FinGeneratorMorphismId (FinGeneratorId (sourceId * objectCount + targetId))
