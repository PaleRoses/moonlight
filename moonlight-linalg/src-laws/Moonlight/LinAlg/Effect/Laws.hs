module Moonlight.LinAlg.Effect.Laws
  ( linalgLawBundles,
    registeredLawNames,
    tests,
  )
where

import Data.List ((\\))
import Moonlight.LinAlg.Effect.Harness qualified as Harness
import Moonlight.LinAlg.Effect.LawNames (LawName (..), allLawNames)
import Moonlight.Pale.Test.LawSuite
  ( LawBundle,
    hUnitLaw,
    lawBundleQuickCheck,
    lawSuiteGroup,
    quickCheckLawDefinition,
    renderedLawBundle,
    renderLawBundles,
  )
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool)

tests :: TestTree
tests =
  lawSuiteGroup "moonlight-linalg laws" (renderLawBundles id linalgLawBundles)

linalgLawBundles :: [LawBundle String]
linalgLawBundles =
  [ renderedLawBundle
      "manifest"
      [hUnitLaw "linalg_law_manifest_totality" manifestTotalityAssertion],
    lawBundleQuickCheck
      "dense algebra"
      [ quickCheckLawDefinition DenseAddAssociative Harness.denseAddAssociativeLaw,
        quickCheckLawDefinition DenseAddCommutative Harness.denseAddCommutativeLaw,
        quickCheckLawDefinition DenseMultiplyAssociative Harness.denseMultiplyAssociativeLaw,
        quickCheckLawDefinition DenseLeftDistributive Harness.denseLeftDistributiveLaw,
        quickCheckLawDefinition DenseRightDistributive Harness.denseRightDistributiveLaw,
        quickCheckLawDefinition DenseTransposeInvolution Harness.denseTransposeInvolutionLaw,
        quickCheckLawDefinition DenseTransposeProductReversal Harness.denseTransposeProductReversalLaw,
        quickCheckLawDefinition DenseMapComposition Harness.denseMapCompositionLaw
      ],
    lawBundleQuickCheck
      "decompositions"
      [ quickCheckLawDefinition QRReconstructsInput Harness.qrReconstructsInputLaw,
        quickCheckLawDefinition QROrthonormalColumns Harness.qrOrthonormalColumnsLaw,
        quickCheckLawDefinition CholeskyReconstructsSPD Harness.choleskyReconstructsSpdLaw,
        quickCheckLawDefinition SymmetricEigenReconstructs Harness.symmetricEigenReconstructsLaw,
        quickCheckLawDefinition SymmetricEigenOrthonormal Harness.symmetricEigenOrthonormalLaw,
        quickCheckLawDefinition SymmetricEigenUncheckedPassesCertification Harness.symmetricEigenUncheckedPassesCertificationLaw,
        quickCheckLawDefinition ThinSVDReconstructs Harness.thinSvdReconstructsLaw,
        quickCheckLawDefinition ThinSVDOrthonormalFactors Harness.thinSvdOrthonormalFactorsLaw,
        quickCheckLawDefinition ThinSVDSingularValuesOrderedNonnegative Harness.thinSvdSingularValuesOrderedNonnegativeLaw
      ],
    lawBundleQuickCheck
      "field and gf2"
      [ quickCheckLawDefinition PLUReconstructsInput Harness.pluReconstructsInputLaw,
        quickCheckLawDefinition RankKernelNullity Harness.rankKernelNullityLaw,
        quickCheckLawDefinition KernelVectorsAnnihilated Harness.kernelVectorsAnnihilatedLaw,
        quickCheckLawDefinition PackedLinearMapIdentity Harness.packedLinearMapIdentityLaw,
        quickCheckLawDefinition PackedLinearMapComposition Harness.packedLinearMapCompositionLaw,
        quickCheckLawDefinition GF2PackedInverseTwoSided Harness.gf2PackedInverseTwoSidedLaw
      ],
    lawBundleQuickCheck
      "domain"
      [ quickCheckLawDefinition SmithDiagonalReconstructsInput Harness.smithDiagonalReconstructsInputLaw,
        quickCheckLawDefinition SmithDivisibilityChain Harness.smithDivisibilityChainLaw,
        quickCheckLawDefinition SmithWitnessesUnimodular Harness.smithWitnessesUnimodularLaw,
        quickCheckLawDefinition SmithDiagonalOnlyAgreesWithFull Harness.smithDiagonalOnlyAgreesWithFullLaw,
        quickCheckLawDefinition BareissRankAgreesWithFieldRank Harness.bareissRankAgreesWithFieldRankLaw,
        quickCheckLawDefinition BareissDeterminantAgreesWithExterior Harness.bareissDeterminantAgreesWithExteriorLaw
      ],
    lawBundleQuickCheck
      "sparse storage"
      [ quickCheckLawDefinition COOCSRRoundTrip Harness.cooCsrRoundTripLaw,
        quickCheckLawDefinition COOCSCRoundTrip Harness.cooCscRoundTripLaw,
        quickCheckLawDefinition CSRCSCTransposeAgreement Harness.csrCscTransposeAgreementLaw,
        quickCheckLawDefinition CSRMatVecAgreesWithDense Harness.csrMatVecAgreesWithDenseLaw,
        quickCheckLawDefinition CanonicalCSRCombinesDuplicates Harness.canonicalCsrCombinesDuplicatesLaw,
        quickCheckLawDefinition GraphLaplacianSymmetricRowSumsZero Harness.graphLaplacianSymmetricRowSumsZeroLaw,
        quickCheckLawDefinition SelfAdjointCSRRejectsAsymmetry Harness.selfAdjointCsrRejectsAsymmetryLaw
      ],
    lawBundleQuickCheck
      "operator"
      [ quickCheckLawDefinition ScaledOperatorAction Harness.scaledOperatorActionLaw,
        quickCheckLawDefinition ShiftedIdentityAction Harness.shiftedIdentityActionLaw
      ],
    lawBundleQuickCheck
      "preconditioner"
      [ quickCheckLawDefinition IC0FactorSolveRoundTrip Harness.ic0FactorSolveRoundTripLaw,
        quickCheckLawDefinition IC0RejectsNonpositivePivot Harness.ic0RejectsNonpositivePivotLaw,
        quickCheckLawDefinition PreconditionedCGConvergesOnSPD Harness.preconditionedCgConvergesOnSpdLaw
      ],
    lawBundleQuickCheck
      "krylov spectral"
      [ quickCheckLawDefinition ArnoldiRelationHolds Harness.arnoldiRelationHoldsLaw,
        quickCheckLawDefinition ArnoldiBasisOrthonormal Harness.arnoldiBasisOrthonormalLaw,
        quickCheckLawDefinition LanczosProjectionTridiagonal Harness.lanczosProjectionTridiagonalLaw,
        quickCheckLawDefinition LanczosBasisOrthonormal Harness.lanczosBasisOrthonormalLaw,
        quickCheckLawDefinition ThickRestartLockedPairsResidualBounded Harness.thickRestartLockedPairsResidualBoundedLaw,
        quickCheckLawDefinition SelectedPairsResidualBounded Harness.selectedPairsResidualBoundedLaw,
        quickCheckLawDefinition SelectedPairsClusterOrthonormal Harness.selectedPairsClusterOrthonormalLaw,
        quickCheckLawDefinition TridiagonalSelectedValuesAgreeWithAllPairs Harness.tridiagonalSelectedValuesAgreeWithAllPairsLaw,
        quickCheckLawDefinition DiagonalSpectralValuesExact Harness.diagonalSpectralValuesExactLaw,
        quickCheckLawDefinition PathLaplacianSpectralValuesClosedForm Harness.pathLaplacianSpectralValuesClosedFormLaw,
        quickCheckLawDefinition EigenRequestRejectsOversubscription Harness.eigenRequestRejectsOversubscriptionLaw
      ],
    lawBundleQuickCheck
      "geometry"
      [ quickCheckLawDefinition Vec3AddCommutativeAssociative Harness.vec3AddCommutativeAssociativeLaw,
        quickCheckLawDefinition Vec3DotSymmetric Harness.vec3DotSymmetricLaw,
        quickCheckLawDefinition Vec3NormalizeUnit Harness.vec3NormalizeUnitLaw,
        quickCheckLawDefinition AABBUnionCommutativeAssociative Harness.aabbUnionCommutativeAssociativeLaw,
        quickCheckLawDefinition AABBUnionContainsOperands Harness.aabbUnionContainsOperandsLaw,
        quickCheckLawDefinition AABBIntersectionCommutative Harness.aabbIntersectionCommutativeLaw,
        quickCheckLawDefinition SymmetricOuterApplyAgreement Harness.symmetricOuterApplyAgreementLaw,
        quickCheckLawDefinition GeometrySymmetricEigenReconstructs Harness.geometrySymmetricEigenReconstructsLaw,
        quickCheckLawDefinition GeometrySymmetricEigenOrthonormal Harness.geometrySymmetricEigenOrthonormalLaw
      ],
    lawBundleQuickCheck
      "statics"
      [ quickCheckLawDefinition NetworkDeclarationOrderInvariant Harness.networkDeclarationOrderInvariantLaw,
        quickCheckLawDefinition RepeatedLoadsAccumulate Harness.repeatedLoadsAccumulateLaw,
        quickCheckLawDefinition EquilibriumAssemblyCanonicalOrdering Harness.equilibriumAssemblyCanonicalOrderingLaw,
        quickCheckLawDefinition EquilibriumSolutionResidualBounded Harness.equilibriumSolutionResidualBoundedLaw,
        quickCheckLawDefinition UnsupportedLoadProducesResidualViolation Harness.unsupportedLoadProducesResidualViolationLaw
      ]
  ]

registeredLawNames :: [LawName]
registeredLawNames =
  [ DenseAddAssociative,
    DenseAddCommutative,
    DenseMultiplyAssociative,
    DenseLeftDistributive,
    DenseRightDistributive,
    DenseTransposeInvolution,
    DenseTransposeProductReversal,
    DenseMapComposition,
    QRReconstructsInput,
    QROrthonormalColumns,
    CholeskyReconstructsSPD,
    SymmetricEigenReconstructs,
    SymmetricEigenOrthonormal,
    SymmetricEigenUncheckedPassesCertification,
    ThinSVDReconstructs,
    ThinSVDOrthonormalFactors,
    ThinSVDSingularValuesOrderedNonnegative,
    PLUReconstructsInput,
    RankKernelNullity,
    KernelVectorsAnnihilated,
    PackedLinearMapIdentity,
    PackedLinearMapComposition,
    GF2PackedInverseTwoSided,
    SmithDiagonalReconstructsInput,
    SmithDivisibilityChain,
    SmithWitnessesUnimodular,
    SmithDiagonalOnlyAgreesWithFull,
    BareissRankAgreesWithFieldRank,
    BareissDeterminantAgreesWithExterior,
    COOCSRRoundTrip,
    COOCSCRoundTrip,
    CSRCSCTransposeAgreement,
    CSRMatVecAgreesWithDense,
    CanonicalCSRCombinesDuplicates,
    GraphLaplacianSymmetricRowSumsZero,
    SelfAdjointCSRRejectsAsymmetry,
    ScaledOperatorAction,
    ShiftedIdentityAction,
    IC0FactorSolveRoundTrip,
    IC0RejectsNonpositivePivot,
    PreconditionedCGConvergesOnSPD,
    ArnoldiRelationHolds,
    ArnoldiBasisOrthonormal,
    LanczosProjectionTridiagonal,
    LanczosBasisOrthonormal,
    ThickRestartLockedPairsResidualBounded,
    SelectedPairsResidualBounded,
    SelectedPairsClusterOrthonormal,
    TridiagonalSelectedValuesAgreeWithAllPairs,
    DiagonalSpectralValuesExact,
    PathLaplacianSpectralValuesClosedForm,
    EigenRequestRejectsOversubscription,
    Vec3AddCommutativeAssociative,
    Vec3DotSymmetric,
    Vec3NormalizeUnit,
    AABBUnionCommutativeAssociative,
    AABBUnionContainsOperands,
    AABBIntersectionCommutative,
    SymmetricOuterApplyAgreement,
    GeometrySymmetricEigenReconstructs,
    GeometrySymmetricEigenOrthonormal,
    NetworkDeclarationOrderInvariant,
    RepeatedLoadsAccumulate,
    EquilibriumAssemblyCanonicalOrdering,
    EquilibriumSolutionResidualBounded,
    UnsupportedLoadProducesResidualViolation
  ]

manifestTotalityAssertion :: IO ()
manifestTotalityAssertion = do
  let missing = allLawNames \\ registeredLawNames
      unrecognized = registeredLawNames \\ allLawNames
      duplicateCount = length registeredLawNames - length (dedupe registeredLawNames)
  assertBool ("missing law registrations: " <> show missing) (null missing)
  assertBool ("unrecognized law registrations: " <> show unrecognized) (null unrecognized)
  assertBool ("duplicate law registrations: " <> show duplicateCount) (duplicateCount == 0)

dedupe :: Eq value => [value] -> [value]
dedupe =
  foldr (\value accumulated -> if value `elem` accumulated then accumulated else value : accumulated) []
