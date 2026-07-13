module Moonlight.Sheaf.Cochain.CoboundarySpec
  ( tests,
  )
where

import Data.Map.Strict qualified as Map
import Data.Vector.Unboxed qualified as Unboxed
import Moonlight.Homology
  ( BoundaryIncidence,
    BoundaryIncidenceShapeError (..),
    boundaryEntries,
    emptyBoundaryIncidenceOf,
    mkBoundaryEntry,
    mkBoundaryIncidence,
  )
import Moonlight.Sheaf.Cochain.Coboundary
  ( CoboundaryBlockKernel (..),
    CoboundaryContribution (..),
    CoboundaryMatrix (..),
    CoboundarySpec (..),
    applyRankOneCoboundaryPlan,
    applyRankOneCoboundaryPlanDense,
    applyCoboundaryAssemblyPlanWithKernel,
    applyCoboundary,
    applyCoboundaryIncidencePlan,
    buildCoboundary,
    buildCoboundaryComplex,
    buildRankOneCoboundaryComplex,
    materializeRankOneCoboundaryDifferential,
    materializeRankOneCoboundaryIncidence,
    materializeCoboundaryAssemblyPlan,
    materializeCoboundaryIncidence,
    materializeCoboundaryIncidenceWithKernel,
    materializeCoboundaryIncidencePlan,
    mkCoboundaryEntry,
    prepareCoboundaryAssemblyPlan,
    prepareCoboundaryIncidencePlan,
    prepareCoboundaryIncidencePlanWithKernel,
    prepareRankOneCoboundaryPlan,
  )
import Moonlight.Sheaf.Operator.BuildError
  ( OperatorBasisRole (..),
    SheafOperatorBuildError (..),
  )
import Moonlight.Sheaf.Operator.GradedComplex
import Moonlight.Homology
  ( HomologicalDegree (..),
  )
import Moonlight.Sheaf.Operator.LinearBasis
  ( mkLinearBasis,
  )
import Moonlight.Sheaf.Kernel.Basis (mkSheafBasis)
import Moonlight.Sheaf.Section.Linearize (identityBoundaryIncidence)
import Moonlight.Sheaf.Section.Morphism
  ( Restriction (..),
    RestrictionId (..),
    RestrictionParts (..),
    mkIncidenceRestriction,
    unitIncidenceRestriction,
  )
import Moonlight.Sheaf.Kernel.Basis (basisCells)
import Moonlight.Sheaf.Section.ObjectIndex (mkObjectIndex)
import Moonlight.Sheaf.Section.Store.State (mkTotalSectionStore)
import Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndex,
    RestrictionIndexError (..),
    buildRestrictionIndex,
    emptyRestrictionIndex,
  )
import Moonlight.Sheaf.Section.Stalk (StalkAlgebra (..), StalkRestrictionKernel (..))
import Moonlight.Sheaf.TestFixture.Assertions (assertRight)
import Moonlight.Sheaf.TestFixture.Mini
  ( MiniCell (..),
    MiniStalk (..),
    miniBasis,
    miniCell0Basis,
    miniCell1Basis,
    miniGhostBasis,
    miniSheafModel,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "coboundary"
    [ testCase "materializeCoboundaryIncidence rejects incompatible local block shapes" testRejectsIncompatibleLocalBlockShapes,
      testCase "constant unit materialization matches generic unit-block materialization" testConstantUnitMatchesGeneric,
      testCase "unit block kernel matches generic unit materialization" testUnitBlockKernelMatchesGeneric,
      testCase "unit block kernel rejects non-unit stalk dimensions" testUnitBlockKernelRejectsNonUnitStalkDimensions,
      testCase "prepared coboundary assembly plan matches direct materialization" testPreparedAssemblyPlanMatchesDirectMaterialization,
      testCase "prepared coboundary incidence plan matches direct materialization" testPreparedIncidencePlanMatchesDirectMaterialization,
      testCase "prepared coboundary incidence plan keeps the reusable hot path explicit" testPreparedIncidencePlanReusableHotPath,
      testCase "unit block kernel applies without explicit sparse materialization" testUnitBlockKernelMatrixFreeApply,
      testCase "rank-one incidence matches explicit unit block materialization" testRankOneIncidenceMatchesUnitBlock,
      testCase "rank-one scalar coefficients preserve restriction orientation" testRankOneScalarPreservesOrientation,
      testCase "rank-one zero scalar vanishes without corrupting shape" testRankOneZeroScalarVanishes,
      testCase "rank-one duplicate entries canonicalize like sparse incidence" testRankOneDuplicateEntriesCanonicalize,
      testCase "rank-one dense apply matches sparse apply without map churn" testRankOneDenseApplyMatchesSparseApply,
      testCase "rank-one dense apply rejects source length mismatch" testRankOneDenseApplyRejectsSourceLengthMismatch,
      testCase "rank-one empty source and target plans produce shaped empty incidences" testRankOneEmptyEndpointPlans,
      testCase "rank-one preparation skips half-present restriction endpoints like the general path" testRankOneSkipsHalfPresentEndpoint,
      testCase "rank-one differential and complex match explicit unit block results" testRankOneDifferentialAndComplexMatchExplicit,
      testCase "rank-one complex rejects non-nilpotent adjacent differentials" testRankOneComplexRejectsNonNilpotence,
      testCase "adjacent cache rejects reordered middle bases" testRejectsReorderedMiddleBasis,
      testCase "coboundary complex preparation rejects reordered middle bases" testBuildComplexRejectsReorderedMiddleBasis,
      testCase "validated boundary constructors reject out-of-bounds local entries" testRejectsOutOfBoundsLocalEntries,
      testCase "constant unit materialization projects away entries outside the selected basis" testConstantUnitProjectsAwayOutOfBasisEntries,
      testCase "constant unit materialization preserves the orientation incidence of an identity generic coboundary" testConstantUnitPreservesOrientationIncidence,
      testCase "general matrix-free apply canonicalizes canceled zero entries" testGeneralMatrixFreeApplyCanonicalizesCanceledZeros,
      testCase "section coboundary apply interprets stored witnesses at the algebra boundary" testApplyCoboundaryInterpretsStoredWitness,
      testCase "cochain complex constructors preserve rational coefficient authority" testCochainComplexConstructorsAcceptRationalCoefficients,
      testCase "restriction index rejects zero incidence coefficients" testRejectsZeroRestrictionCoefficient
    ]

testRejectsIncompatibleLocalBlockShapes :: Assertion
testRejectsIncompatibleLocalBlockShapes = do
  matrix <- assertRight "valid singleton coboundary matrix" singletonCoboundaryMatrix
  let incidence =
        materializeCoboundaryIncidence
          unitStalkAtCell
          unitStalkDimension
          (\_ _ -> emptyBoundaryIncidenceOf 2 1)
          matrix
  assertEqual
    "expected block shape mismatch to be reported explicitly"
    (Left (OperatorBoundaryShapeError (BoundaryIncidenceBlockShapeMismatch 1 1 2 1)))
    incidence

testConstantUnitMatchesGeneric :: Assertion
testConstantUnitMatchesGeneric = do
  restrictions <-
    assertRight
      "singleton unit restriction index"
      singletonUnitRestrictionIndex
  genericMatrix <-
    assertRight
      "valid generic unit matrix"
      singletonCoboundaryMatrix
  let genericIncidence = materializeUnitIncidence genericMatrix
      specBuiltIncidence =
        materializeBuiltUnitIncidence
          singletonCoboundarySpec
          restrictions
  assertEqual
    "expected the spec-built unit coboundary to agree with generic unit-block materialization"
    genericIncidence
    specBuiltIncidence

testUnitBlockKernelMatchesGeneric :: Assertion
testUnitBlockKernelMatchesGeneric = do
  matrix <- assertRight "valid singleton coboundary matrix" singletonCoboundaryMatrix
  assertEqual
    "expected the unit block kernel to preserve generic unit incidence semantics"
    (materializeUnitIncidence matrix)
    ( materializeCoboundaryIncidenceWithKernel
        unitStalkAtCell
        unitStalkDimension
        UnitCoboundaryBlock
        matrix
    )

testUnitBlockKernelRejectsNonUnitStalkDimensions :: Assertion
testUnitBlockKernelRejectsNonUnitStalkDimensions = do
  matrix <- assertRight "valid stress coboundary matrix" (unitStressCoboundaryMatrix 1)
  assertEqual
    "expected the unit block kernel to reject non-rank-one stalk blocks"
    (Left (OperatorBoundaryShapeError (BoundaryIncidenceBlockShapeMismatch 2 2 1 1)))
    ( materializeCoboundaryIncidenceWithKernel
        (const ())
        (const 2)
        UnitCoboundaryBlock
        matrix
    )

testPreparedAssemblyPlanMatchesDirectMaterialization :: Assertion
testPreparedAssemblyPlanMatchesDirectMaterialization = do
  matrix <- assertRight "valid singleton coboundary matrix" singletonCoboundaryMatrix
  assemblyPlan <-
    assertRight
      "valid prepared coboundary assembly plan"
      ( prepareCoboundaryAssemblyPlan
          unitStalkAtCell
          unitStalkDimension
          matrix
      )
  differentialValue <-
    assertRight
      "valid materialized differential from prepared assembly plan"
      (materializeCoboundaryAssemblyPlan unitCoboundaryBlock assemblyPlan)
  assertEqual
    "expected prepared assembly to agree with direct materialization"
    (materializeUnitIncidence matrix)
    (Right (gradedOperatorIncidence differentialValue))

testPreparedIncidencePlanMatchesDirectMaterialization :: Assertion
testPreparedIncidencePlanMatchesDirectMaterialization = do
  matrix <- assertRight "valid singleton coboundary matrix" singletonCoboundaryMatrix
  assemblyPlan <-
    assertRight
      "valid prepared coboundary assembly plan"
      ( prepareCoboundaryAssemblyPlan
          unitStalkAtCell
          unitStalkDimension
          matrix
      )
  incidencePlan <-
    assertRight
      "valid prepared coboundary incidence plan"
      (prepareCoboundaryIncidencePlan unitCoboundaryBlock assemblyPlan)
  differentialValue <-
    assertRight
      "valid materialized differential from prepared incidence plan"
      (materializeCoboundaryIncidencePlan incidencePlan)
  assertEqual
    "expected prepared incidence to agree with direct materialization"
    (materializeUnitIncidence matrix)
    (Right (gradedOperatorIncidence differentialValue))

testPreparedIncidencePlanReusableHotPath :: Assertion
testPreparedIncidencePlanReusableHotPath = do
  matrix <- assertRight "valid reusable stress matrix" (unitStressCoboundaryMatrix 256)
  assemblyPlan <-
    assertRight
      "valid reusable assembly plan"
      ( prepareCoboundaryAssemblyPlan
          (const ())
          (const 1)
          matrix
      )
  incidencePlan <-
    assertRight
      "valid reusable incidence plan"
      (prepareCoboundaryIncidencePlanWithKernel UnitCoboundaryBlock assemblyPlan)
  differentialValue <-
    assertRight
      "valid materialized differential from cached incidence"
      (materializeCoboundaryIncidencePlan incidencePlan)
  assertEqual
    "expected one cached global entry per unit restriction"
    256
    (length (boundaryEntries (gradedOperatorIncidence differentialValue)))

testUnitBlockKernelMatrixFreeApply :: Assertion
testUnitBlockKernelMatrixFreeApply = do
  matrix <- assertRight "valid reusable stress matrix" (unitStressCoboundaryMatrix 3)
  assemblyPlan <-
    assertRight
      "valid unit assembly plan"
      ( prepareCoboundaryAssemblyPlan
          (const ())
          (const 1)
          matrix
      )
  incidencePlan <-
    assertRight
      "valid unit incidence plan"
      (prepareCoboundaryIncidencePlanWithKernel UnitCoboundaryBlock assemblyPlan)
  let sourceVector =
        Map.fromList [(0, 5), (1, 7), (2, 11)]
      expectedTargetVector =
        Map.fromList [(0, 5), (1, 7), (2, 11)]
  matrixFreeTargetVector <-
    assertRight
      "valid matrix-free unit application"
      (applyCoboundaryAssemblyPlanWithKernel UnitCoboundaryBlock assemblyPlan sourceVector)
  assertEqual
    "expected matrix-free application to match cached incidence application"
    expectedTargetVector
    (applyCoboundaryIncidencePlan incidencePlan sourceVector)
  assertEqual
    "expected unit kernel to apply directly without requiring explicit sparse construction"
    expectedTargetVector
    matrixFreeTargetVector

testRankOneIncidenceMatchesUnitBlock :: Assertion
testRankOneIncidenceMatchesUnitBlock = do
  restrictions <-
    assertRight
      "singleton unit restriction index"
      singletonUnitRestrictionIndex
  rankOnePlan <-
    assertRight
      "valid rank-one coboundary plan"
      (prepareRankOneCoboundaryPlan unitStalkAtCell unitScalarCoefficient singletonCoboundarySpec restrictions)
  assertEqual
    "rank-one materialization must agree with the explicit unit block path"
    (materializeBuiltUnitIncidence singletonCoboundarySpec restrictions)
    (Right (materializeRankOneCoboundaryIncidence rankOnePlan))

testRankOneScalarPreservesOrientation :: Assertion
testRankOneScalarPreservesOrientation = do
  restrictions <-
    assertRight
      "oriented singleton unit restriction index"
      (singletonUnitRestrictionIndexWithCoefficient (-1))
  expectedIncidence <-
    assertRight
      "valid oriented scalar rank-one incidence"
      (mkBoundaryIncidence 1 1 [mkBoundaryEntry 0 0 (-2 :: Int)])
  rankOnePlan <-
    assertRight
      "valid oriented rank-one coboundary plan"
      (prepareRankOneCoboundaryPlan unitStalkAtCell (const (const (const 2))) singletonCoboundarySpec restrictions)
  assertEqual
    "restriction orientation must multiply the rank-one scalar transport"
    expectedIncidence
    (materializeRankOneCoboundaryIncidence rankOnePlan)

testRankOneZeroScalarVanishes :: Assertion
testRankOneZeroScalarVanishes = do
  restrictions <-
    assertRight
      "singleton unit restriction index"
      singletonUnitRestrictionIndex
  rankOnePlan <-
    assertRight
      "valid zero rank-one coboundary plan"
      (prepareRankOneCoboundaryPlan unitStalkAtCell (const (const (const 0))) singletonCoboundarySpec restrictions)
  assertEqual
    "zero scalar entries must vanish while preserving source and target shape"
    (emptyBoundaryIncidenceOf 1 1)
    (materializeRankOneCoboundaryIncidence rankOnePlan)

testRankOneDuplicateEntriesCanonicalize :: Assertion
testRankOneDuplicateEntriesCanonicalize = do
  restrictions <-
    assertRight
      "duplicate unit restriction index"
      duplicateUnitRestrictionIndex
  expectedIncidence <-
    assertRight
      "valid duplicate-canonicalized rank-one incidence"
      (mkBoundaryIncidence 1 1 [mkBoundaryEntry 0 0 (2 :: Int)])
  rankOnePlan <-
    assertRight
      "valid duplicate rank-one coboundary plan"
      (prepareRankOneCoboundaryPlan unitStalkAtCell unitScalarCoefficient singletonCoboundarySpec restrictions)
  assertEqual
    "rank-one materialization must preserve sparse duplicate canonicalization"
    expectedIncidence
    (materializeRankOneCoboundaryIncidence rankOnePlan)

testRankOneDenseApplyMatchesSparseApply :: Assertion
testRankOneDenseApplyMatchesSparseApply = do
  restrictions <-
    assertRight
      "duplicate unit restriction index"
      duplicateUnitRestrictionIndex
  rankOnePlan <-
    assertRight
      "valid duplicate rank-one coboundary plan"
      (prepareRankOneCoboundaryPlan unitStalkAtCell unitScalarCoefficient singletonCoboundarySpec restrictions)
  denseTarget <-
    assertRight
      "valid rank-one dense application"
      (applyRankOneCoboundaryPlanDense rankOnePlan (Unboxed.singleton 7))
  assertEqual
    "dense rank-one application must multiply through the canonicalized coefficient"
    (Unboxed.singleton 14)
    denseTarget
  assertEqual
    "dense rank-one application must agree with the sparse map application"
    (Map.fromList [(0, 14)])
    (applyRankOneCoboundaryPlan rankOnePlan (Map.fromList [(0, 7)]))

testRankOneDenseApplyRejectsSourceLengthMismatch :: Assertion
testRankOneDenseApplyRejectsSourceLengthMismatch = do
  restrictions <-
    assertRight
      "singleton unit restriction index"
      singletonUnitRestrictionIndex
  rankOnePlan <-
    assertRight
      "valid rank-one coboundary plan"
      (prepareRankOneCoboundaryPlan unitStalkAtCell unitScalarCoefficient singletonCoboundarySpec restrictions)
  assertEqual
    "dense rank-one application must reject mismatched source vector length at the typed boundary"
    (Left (OperatorVectorLengthMismatch OperatorSourceBasis 1 0))
    (applyRankOneCoboundaryPlanDense rankOnePlan Unboxed.empty)

testRankOneEmptyEndpointPlans :: Assertion
testRankOneEmptyEndpointPlans = do
  restrictions <-
    assertRight
      "singleton unit restriction index"
      singletonUnitRestrictionIndex
  emptyTargetPlan <-
    assertRight
      "valid empty-target rank-one plan"
      ( prepareRankOneCoboundaryPlan
          unitStalkAtCell
          unitScalarCoefficient
          singletonCoboundarySpec {csTargetBasis = mkSheafBasis []}
          restrictions
      )
  emptySourcePlan <-
    assertRight
      "valid empty-source rank-one plan"
      ( prepareRankOneCoboundaryPlan
          unitStalkAtCell
          unitScalarCoefficient
          singletonCoboundarySpec {csSourceBasis = mkSheafBasis []}
          restrictions
      )
  assertEqual
    "empty target plan must preserve source cardinality with no entries"
    (emptyBoundaryIncidenceOf 1 0)
    (materializeRankOneCoboundaryIncidence emptyTargetPlan)
  assertEqual
    "empty source plan must preserve target cardinality with no entries"
    (emptyBoundaryIncidenceOf 0 1)
    (materializeRankOneCoboundaryIncidence emptySourcePlan)

testRankOneSkipsHalfPresentEndpoint :: Assertion
testRankOneSkipsHalfPresentEndpoint = do
  onlyCoboundaryTargetRestrictions <-
    assertRight
      "restriction index with only the selected coboundary target endpoint"
      restrictionIndexWithOnlyCoboundaryTargetEndpoint
  onlyCoboundarySourceRestrictions <-
    assertRight
      "restriction index with only the selected coboundary source endpoint"
      restrictionIndexWithOnlyCoboundarySourceEndpoint
  assertEqual
    "rank-one preparation must skip a restriction with only its coboundary target endpoint selected"
    (materializeBuiltUnitIncidence singletonCoboundarySpec onlyCoboundaryTargetRestrictions)
    ( fmap
        materializeRankOneCoboundaryIncidence
        (prepareRankOneCoboundaryPlan unitStalkAtCell unitScalarCoefficient singletonCoboundarySpec onlyCoboundaryTargetRestrictions)
    )
  assertEqual
    "rank-one preparation must skip a restriction with only its coboundary source endpoint selected"
    (materializeBuiltUnitIncidence singletonCoboundarySpec onlyCoboundarySourceRestrictions)
    ( fmap
        materializeRankOneCoboundaryIncidence
        (prepareRankOneCoboundaryPlan unitStalkAtCell unitScalarCoefficient singletonCoboundarySpec onlyCoboundarySourceRestrictions)
    )

testRankOneDifferentialAndComplexMatchExplicit :: Assertion
testRankOneDifferentialAndComplexMatchExplicit = do
  restrictions <-
    assertRight
      "singleton unit restriction index"
      singletonUnitRestrictionIndex
  rankOnePlan <-
    assertRight
      "valid rank-one coboundary plan"
      (prepareRankOneCoboundaryPlan unitStalkAtCell unitScalarCoefficient singletonCoboundarySpec restrictions)
  rankOneDifferential <-
    assertRight
      "valid rank-one differential"
      (materializeRankOneCoboundaryDifferential rankOnePlan)
  assertEqual
    "rank-one differential incidence must agree with explicit unit block materialization"
    (materializeBuiltUnitIncidence singletonCoboundarySpec restrictions)
    (Right (gradedOperatorIncidence rankOneDifferential))
  assertEqual
    "rank-one complex must agree with the explicit unit block complex"
    ( explicitUnitComplex
        singletonCoboundarySpec
        singletonSecondCoboundarySpec
        restrictions
    )
    ( buildRankOneCoboundaryComplex
        unitStalkAtCell
        unitScalarCoefficient
        singletonCoboundarySpec
        singletonSecondCoboundarySpec
        restrictions
    )

testRankOneComplexRejectsNonNilpotence :: Assertion
testRankOneComplexRejectsNonNilpotence = do
  restrictions <-
    assertRight
      "non-nilpotent rank-one restriction index"
      nonNilpotentRestrictionIndex
  assertEqual
    "rank-one complex must reject nonzero adjacent composition"
    (Left (OperatorNonNilpotent (HomologicalDegree 0) (HomologicalDegree 1) 0 0))
    ( buildRankOneCoboundaryComplex
        unitStalkAtCell
        unitScalarCoefficient
        singletonCoboundarySpec
        singletonSecondCoboundarySpec
        restrictions
    )

testRejectsReorderedMiddleBasis :: Assertion
testRejectsReorderedMiddleBasis = do
  let sourceSheafBasis = miniCell0Basis
      middleSheafBasis = miniBasis
      reorderedMiddleSheafBasis = mkSheafBasis [Cell1, Cell0]
      targetSheafBasis = mkSheafBasis [Ghost]
      unitDimension :: cell -> Int
      unitDimension = const (1 :: Int)
  sourceBasis <-
    assertRight
      "valid linear basis for source"
      (mkLinearBasis unitDimension sourceSheafBasis)
  middleBasis <-
    assertRight
      "valid linear basis for middle"
      (mkLinearBasis unitDimension middleSheafBasis)
  reorderedMiddleBasis <-
    assertRight
      "valid linear basis for reordered-middle"
      (mkLinearBasis unitDimension reorderedMiddleSheafBasis)
  targetBasis <-
    assertRight
      "valid linear basis for target"
      (mkLinearBasis unitDimension targetSheafBasis)
  rightDifferential <-
    assertRight
      "valid right differential"
      (mkGradedOperator (HomologicalDegree 0) sourceBasis middleBasis (emptyBoundaryIncidenceOf 1 2 :: BoundaryIncidence Int))
  leftDifferential <-
    assertRight
      "valid left differential"
      (mkGradedOperator (HomologicalDegree 1) reorderedMiddleBasis targetBasis (emptyBoundaryIncidenceOf 2 1 :: BoundaryIncidence Int))
  case mkGradedComplexFromList DegreeIncreasing [rightDifferential, leftDifferential] of
    Left constructionError ->
      assertEqual
        "expected exact middle-basis equality rather than mere cardinality agreement"
        (OperatorIntermediateBasisMismatch (HomologicalDegree 0) (HomologicalDegree 1))
        constructionError
    Right _ ->
      assertFailure
        "expected exact middle-basis equality rather than mere cardinality agreement"

testBuildComplexRejectsReorderedMiddleBasis :: Assertion
testBuildComplexRejectsReorderedMiddleBasis =
  case
    buildCoboundaryComplex
      unitStalkAtCell
      unitStalkDimension
      unitCoboundaryBlock
      CoboundarySpec
        { csDimension = (HomologicalDegree 0),
          csSourceBasis = miniCell0Basis,
          csTargetBasis = miniBasis
        }
      CoboundarySpec
        { csDimension = (HomologicalDegree 1),
          csSourceBasis = mkSheafBasis [Cell1, Cell0],
          csTargetBasis = miniGhostBasis
        }
      emptyRestrictionIndex
  of
    Left constructionError ->
      assertEqual
        "expected shared-basis preparation to preserve exact middle-basis equality"
        (OperatorIntermediateBasisMismatch (HomologicalDegree 0) (HomologicalDegree 1))
        constructionError
    Right _ ->
      assertFailure
        "expected shared-basis preparation to reject reordered middle basis"

testCochainComplexConstructorsAcceptRationalCoefficients :: Assertion
testCochainComplexConstructorsAcceptRationalCoefficients = do
  let unitDimension :: cell -> Int
      unitDimension = const (1 :: Int)
  sourceBasis <-
    assertRight
      "valid rational source basis"
      (mkLinearBasis unitDimension miniCell0Basis)
  middleBasis <-
    assertRight
      "valid rational middle basis"
      (mkLinearBasis unitDimension miniBasis)
  targetBasis <-
    assertRight
      "valid rational target basis"
      (mkLinearBasis unitDimension miniGhostBasis)
  rightDifferential <-
    assertRight
      "valid rational right differential"
      (mkGradedOperator (HomologicalDegree 0) sourceBasis middleBasis (emptyBoundaryIncidenceOf 1 2 :: BoundaryIncidence Rational))
  leftDifferential <-
    assertRight
      "valid rational left differential"
      (mkGradedOperator (HomologicalDegree 1) middleBasis targetBasis (emptyBoundaryIncidenceOf 2 3 :: BoundaryIncidence Rational))
  case mkGradedComplexFromList DegreeIncreasing [rightDifferential, leftDifferential] of
    Left constructionError ->
      assertFailure
        ("expected rational cochain constructor to preserve coefficient authority, but got: " <> show constructionError)
    Right _ ->
      pure ()

testRejectsOutOfBoundsLocalEntries :: Assertion
testRejectsOutOfBoundsLocalEntries =
  assertEqual
    "expected out-of-bounds local entries to be rejected at construction time"
    (Left (BoundaryIncidenceEntryOutOfBounds 1 0 1 1))
    (mkBoundaryIncidence 1 1 [mkBoundaryEntry 1 0 (1 :: Int)])

testConstantUnitProjectsAwayOutOfBasisEntries :: Assertion
testConstantUnitProjectsAwayOutOfBasisEntries = do
  restrictions <-
    assertRight
      "ghost-filtered restriction index"
      restrictionIndexWithOnlyCoboundaryTargetEndpoint
  assertEqual
    "expected spec-built unit materialization to discard entries whose cells are absent from the selected basis"
    (Right (emptyBoundaryIncidenceOf 1 1))
    ( materializeBuiltUnitIncidence
        ghostFilteredCoboundarySpec
        restrictions
    )

testConstantUnitPreservesOrientationIncidence :: Assertion
testConstantUnitPreservesOrientationIncidence = do
  expectedIncidence <-
    assertRight
      "valid expected oriented incidence"
      (mkBoundaryIncidence 1 1 [mkBoundaryEntry 0 0 (-1 :: Int)])
  matrix <-
    assertRight
      "valid oriented singleton matrix"
      (singletonCoboundaryMatrixWithOrientation (-1))
  assertEqual
    "expected unit-block materialization to preserve coboundary entry orientation"
    (Right expectedIncidence)
    (materializeUnitIncidence matrix)

testGeneralMatrixFreeApplyCanonicalizesCanceledZeros :: Assertion
testGeneralMatrixFreeApplyCanonicalizesCanceledZeros = do
  matrix <-
    assertRight
      "valid canceling coboundary matrix"
      cancelingCoboundaryMatrix
  assemblyPlan <-
    assertRight
      "valid canceling prepared coboundary assembly plan"
      ( prepareCoboundaryAssemblyPlan
          unitStalkAtCell
          unitStalkDimension
          matrix
      )
  assertEqual
    "canceled sparse coordinates must be absent from the canonical matrix-free result"
    (Right Map.empty)
    ( applyCoboundaryAssemblyPlanWithKernel
        (GeneralCoboundaryBlock unitCoboundaryBlock)
        assemblyPlan
        (Map.singleton 0 7)
    )

testApplyCoboundaryInterpretsStoredWitness :: Assertion
testApplyCoboundaryInterpretsStoredWitness = do
  model <-
    assertRight
      "valid mini sheaf model"
      miniSheafModel
  matrix <-
    assertRight
      "valid witness-bearing coboundary matrix"
      ( coboundaryMatrixFromEntries
          singletonCoboundarySpec
          [(0, Cell0, Cell1, 3, 5 :: Int)]
      )
  section <-
    assertRight
      "valid source section"
      (mkTotalSectionStore model (Map.fromList [(Cell0, MiniStalk 2.0), (Cell1, MiniStalk 0.0)]))
  contributions <-
    assertRight
      "valid applied coboundary"
      (applyCoboundary offsetMiniStalkAlgebra matrix model section)
  assertEqual
    "expected applyCoboundary to interpret the stored witness through the supplied algebra"
    (Map.fromList [(Cell1, [CoboundaryContribution {contributionOrientation = 3, contributionValue = MiniStalk 7.0}])])
    contributions

testRejectsZeroRestrictionCoefficient :: Assertion
testRejectsZeroRestrictionCoefficient =
  assertEqual
    "expected zero orientation coefficients to be unconstructable"
    Nothing
    (mkIncidenceRestriction 0)

singletonCoboundaryMatrix :: Either (SheafOperatorBuildError MiniCell) (CoboundaryMatrix MiniCell ())
singletonCoboundaryMatrix =
  singletonCoboundaryMatrixWithOrientation 1

singletonCoboundaryMatrixWithOrientation :: Int -> Either (SheafOperatorBuildError MiniCell) (CoboundaryMatrix MiniCell ())
singletonCoboundaryMatrixWithOrientation orientation =
  coboundaryMatrixFromEntries
    singletonCoboundarySpec
    [(0, Cell0, Cell1, orientation, ())]

cancelingCoboundaryMatrix :: Either (SheafOperatorBuildError MiniCell) (CoboundaryMatrix MiniCell ())
cancelingCoboundaryMatrix =
  coboundaryMatrixFromEntries
    singletonCoboundarySpec
    [ (0, Cell0, Cell1, 1, ()),
      (1, Cell0, Cell1, -1, ())
    ]

singletonCoboundarySpec :: CoboundarySpec MiniCell
singletonCoboundarySpec =
  CoboundarySpec
    { csDimension = (HomologicalDegree 0),
      csSourceBasis = miniCell0Basis,
      csTargetBasis = miniCell1Basis
    }

newtype StressCell = StressCell Int
  deriving stock (Eq, Ord, Show)

unitStressCoboundaryMatrix :: Int -> Either (SheafOperatorBuildError StressCell) (CoboundaryMatrix StressCell ())
unitStressCoboundaryMatrix entryCount =
  coboundaryMatrixFromEntries
    CoboundarySpec
      { csDimension = (HomologicalDegree 0),
        csSourceBasis = mkSheafBasis cells,
        csTargetBasis = mkSheafBasis cells
      }
    (fmap (\cellValue@(StressCell cellOrdinal) -> (cellOrdinal, cellValue, cellValue, 1, ())) cells)
  where
    cells =
      fmap StressCell [0 .. entryCount - 1]

coboundaryMatrixFromEntries ::
  CoboundarySpec cell ->
  [(Int, cell, cell, Int, witness)] ->
  Either (SheafOperatorBuildError cell) (CoboundaryMatrix cell witness)
coboundaryMatrixFromEntries spec entries =
  (\coboundaryEntries ->
     CoboundaryMatrix
       { cmDimension = csDimension spec,
         cmEntries = coboundaryEntries,
         cmSourceBasis = csSourceBasis spec,
         cmTargetBasis = csTargetBasis spec
       }
  )
    <$> traverse
      ( \(entryId, coboundarySource, coboundaryTarget, orientation, witness) ->
          case mkIncidenceRestriction orientation of
            Just restrictionKind ->
              mkCoboundaryEntry
                Restriction
                  { rId = RestrictionId entryId,
                    rKind = restrictionKind,
                    rSource = coboundaryTarget,
                    rTarget = coboundarySource,
                    rWitness = witness
                  }
            Nothing ->
              Left (OperatorZeroIncidenceCoefficient coboundaryTarget coboundarySource)
      )
      entries

ghostFilteredCoboundarySpec :: CoboundarySpec MiniCell
ghostFilteredCoboundarySpec =
  CoboundarySpec
    { csDimension = (HomologicalDegree 0),
      csSourceBasis = miniCell0Basis,
      csTargetBasis = miniCell1Basis
    }

singletonUnitRestrictionIndex ::
  Either
    (RestrictionIndexError MiniCell)
    (RestrictionIndex MiniCell (MiniCell, MiniCell, Int))
singletonUnitRestrictionIndex =
  singletonUnitRestrictionIndexWithCoefficient 1

singletonUnitRestrictionIndexWithCoefficient ::
  Int ->
  Either
    (RestrictionIndexError MiniCell)
    (RestrictionIndex MiniCell (MiniCell, MiniCell, Int))
singletonUnitRestrictionIndexWithCoefficient coefficient =
  case mkIncidenceRestriction coefficient of
    Nothing ->
      Left (RestrictionZeroIncidenceCoefficient Cell1 Cell0)
    Just restrictionKind ->
      buildRestrictionIndex
        (mkObjectIndex (basisCells miniGhostBasis))
        ( \(sourceCell, targetCell, kindValue) ->
            RestrictionParts
              { partKind = kindValue,
                partSource = sourceCell,
                partTarget = targetCell,
                partWitness = (sourceCell, targetCell, coefficient)
              }
        )
        [(Cell1, Cell0, restrictionKind)]

duplicateUnitRestrictionIndex ::
  Either
    (RestrictionIndexError MiniCell)
    (RestrictionIndex MiniCell (MiniCell, MiniCell, Int))
duplicateUnitRestrictionIndex =
  buildRestrictionIndex
    (mkObjectIndex (basisCells miniGhostBasis))
    presentUnitRestrictionTriple
    [(Cell1, Cell0, 1 :: Int), (Cell1, Cell0, 1 :: Int)]

nonNilpotentRestrictionIndex ::
  Either
    (RestrictionIndexError MiniCell)
    (RestrictionIndex MiniCell (MiniCell, MiniCell, Int))
nonNilpotentRestrictionIndex =
  buildRestrictionIndex
    (mkObjectIndex (basisCells miniGhostBasis))
    presentUnitRestrictionTriple
    [(Cell1, Cell0, 1 :: Int), (Ghost, Cell1, 1 :: Int)]

restrictionIndexWithOnlyCoboundaryTargetEndpoint ::
  Either
    (RestrictionIndexError MiniCell)
    (RestrictionIndex MiniCell (MiniCell, MiniCell, Int))
restrictionIndexWithOnlyCoboundaryTargetEndpoint =
  buildRestrictionIndex
    (mkObjectIndex (basisCells miniGhostBasis))
    presentUnitRestrictionTriple
    [(Cell1, Ghost, 1 :: Int)]

restrictionIndexWithOnlyCoboundarySourceEndpoint ::
  Either
    (RestrictionIndexError MiniCell)
    (RestrictionIndex MiniCell (MiniCell, MiniCell, Int))
restrictionIndexWithOnlyCoboundarySourceEndpoint =
  buildRestrictionIndex
    (mkObjectIndex (basisCells miniGhostBasis))
    presentUnitRestrictionTriple
    [(Ghost, Cell0, 1 :: Int)]

presentUnitRestrictionTriple :: (MiniCell, MiniCell, Int) -> RestrictionParts MiniCell (MiniCell, MiniCell, Int)
presentUnitRestrictionTriple triple@(sourceCell, targetCell, _) =
  RestrictionParts
    { partKind = unitIncidenceRestriction,
      partSource = sourceCell,
      partTarget = targetCell,
      partWitness = triple
    }

offsetMiniStalkAlgebra :: StalkAlgebra Int MiniStalk () ()
offsetMiniStalkAlgebra =
  StalkAlgebra
    { saRestrictionKernel = \offset -> StalkRestrictionMap (\(MiniStalk value) -> MiniStalk (value + fromIntegral offset)),
      saMismatches = \_ _ -> [],
      saMerge = \left _ -> Right left,
      saRepair = const (Left ()),
      saNormalize = id
    }

unitStalkAtCell :: MiniCell -> MiniStalk
unitStalkAtCell =
  const (MiniStalk 0.0)

unitStalkDimension :: MiniStalk -> Int
unitStalkDimension =
  const 1

unitCoboundaryBlock :: MiniStalk -> MiniStalk -> BoundaryIncidence Int
unitCoboundaryBlock _ _ =
  identityBoundaryIncidence 1

unitScalarCoefficient ::
  restriction ->
  MiniStalk ->
  MiniStalk ->
  Int
unitScalarCoefficient _ _ _ =
  1

materializeUnitIncidence ::
  CoboundaryMatrix MiniCell witness ->
  Either (SheafOperatorBuildError MiniCell) (BoundaryIncidence Int)
materializeUnitIncidence =
  materializeCoboundaryIncidence
    unitStalkAtCell
    unitStalkDimension
    unitCoboundaryBlock

materializeBuiltUnitIncidence ::
  CoboundarySpec MiniCell ->
  RestrictionIndex MiniCell (MiniCell, MiniCell, Int) ->
  Either (SheafOperatorBuildError MiniCell) (BoundaryIncidence Int)
materializeBuiltUnitIncidence spec restrictions =
  buildCoboundary spec restrictions >>= materializeUnitIncidence

singletonSecondCoboundarySpec :: CoboundarySpec MiniCell
singletonSecondCoboundarySpec =
  CoboundarySpec
    { csDimension = (HomologicalDegree 1),
      csSourceBasis = miniCell1Basis,
      csTargetBasis = mkSheafBasis [Ghost]
    }

explicitUnitComplex ::
  CoboundarySpec MiniCell ->
  CoboundarySpec MiniCell ->
  RestrictionIndex MiniCell (MiniCell, MiniCell, Int) ->
  Either (SheafOperatorBuildError MiniCell) (GradedComplex MiniCell Int)
explicitUnitComplex =
  buildCoboundaryComplex
    unitStalkAtCell
    unitStalkDimension
    unitCoboundaryBlock
