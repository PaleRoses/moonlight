module Moonlight.Derived.Effect.Laws
  ( derivedLawBundles
  , tests
  ) where

import qualified Moonlight.Derived.Effect.Harness as Harness
import Moonlight.Derived.Effect.LawNames (LawName (..))
import Moonlight.Pale.Test.LawSuite (LawBundle, lawBundleQuickCheck, lawSuiteGroup, quickCheckLawDefinition, renderLawBundles)
import Test.Tasty (TestTree)

tests :: TestTree
tests =
  lawSuiteGroup "moonlight-derived laws" (renderLawBundles id derivedLawBundles)

derivedLawBundles :: [LawBundle String]
derivedLawBundles =
  [ lawBundleQuickCheck
      "poset kernel"
      [ quickCheckLawDefinition PosetReflexive Harness.posetReflexiveLaw
      , quickCheckLawDefinition PosetAntisymmetric Harness.posetAntisymmetricLaw
      , quickCheckLawDefinition PosetTransitive Harness.posetTransitiveLaw
      , quickCheckLawDefinition PosetUpperLowerDual Harness.posetUpperLowerDualLaw
      , quickCheckLawDefinition PosetTopoRespectsEdges Harness.posetTopoRespectsEdgesLaw
      ]
  , lawBundleQuickCheck
      "matrix block"
      [ quickCheckLawDefinition MatrixIdentity Harness.matrixIdentityLaw
      , quickCheckLawDefinition MatrixTransposeInvolution Harness.matrixTransposeInvolutionLaw
      , quickCheckLawDefinition MatrixRestrictIdempotent Harness.matrixRestrictIdempotentLaw
      , quickCheckLawDefinition MatrixBlockedSparseRepresentationAgreement Harness.matrixBlockedSparseRepresentationAgreementLaw
      ]
  , lawBundleQuickCheck
      "complex"
      [ quickCheckLawDefinition ComplexDifferentialSquaresZero Harness.complexDifferentialSquaresZeroLaw
      , quickCheckLawDefinition ComplexNormalizationIdempotent Harness.complexNormalizationIdempotentLaw
      , quickCheckLawDefinition ComplexMinimizationHypercohomologyInvariant Harness.complexMinimizationHypercohomologyInvariantLaw
      , quickCheckLawDefinition ComplexMinimizationMicrosupportInvariant Harness.complexMinimizationMicrosupportInvariantLaw
      , quickCheckLawDefinition ComplexMinimizationDegreeWindowStable Harness.complexMinimizationDegreeWindowStableLaw
      ]
  , lawBundleQuickCheck
      "triangulated"
      [ quickCheckLawDefinition ShiftReindexesHypercohomology Harness.shiftReindexesHypercohomologyLaw
      , quickCheckLawDefinition MapSquaresCommute Harness.mapSquaresCommuteLaw
      , quickCheckLawDefinition ConeEulerAdditive Harness.coneEulerAdditiveLaw
      , quickCheckLawDefinition TriangleRotationInvariant Harness.triangleRotationInvariantLaw
      , quickCheckLawDefinition QuasiIsoConeAcyclic Harness.quasiIsoConeAcyclicLaw
      , quickCheckLawDefinition VerdierInvolutionInvariants Harness.verdierInvolutionInvariantsLaw
      , quickCheckLawDefinition RHomTensorAdjunctionDims Harness.rHomTensorAdjunctionDimsLaw
      , quickCheckLawDefinition TruncationTriangleExact Harness.truncationTriangleExactLaw
      ]
  , lawBundleQuickCheck
      "functor"
      [ quickCheckLawDefinition FunctorQuillenARejectsBadFiber Harness.quillenARejectsBadFiberLaw
      ]
  , lawBundleQuickCheck
      "morse"
      [ quickCheckLawDefinition MorseSparseDigestCacheCoherence Harness.morseSparseDigestCacheCoherenceLaw
      ]
  , lawBundleQuickCheck
      "determinism"
      [ quickCheckLawDefinition DerivedDeterministicFixture Harness.deterministicFixtureLaw
      ]
  ]
