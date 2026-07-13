module Moonlight.Geometry.Gluing.LawsSpec (tests) where

import Moonlight.Algebra (JoinSemilattice (join))
import Moonlight.Geometry.Gluing.Laws
import Moonlight.Geometry.Section.Analysis (SpatialSupport (..))
import Moonlight.Geometry.Site.Semantics
  ( BoundEnvelope (..),
    Certification (..),
    DirectionalLipschitz (..),
    DistanceCertificate (..),
    DistanceSemantics (..),
    FarFieldLowerBound (..),
    LipschitzUpperBound (..),
    PrecisionFloor (..),
    TraceSafety (..),
    TraceStepScale (..),
    exactCertificate,
  )
import Moonlight.LinAlg.Geometry (mkAabb)
import Moonlight.LinAlg.Geometry (Vec3 (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, (@?=), testCase)
import qualified Test.Tasty.QuickCheck as QC

tests :: TestTree
tests =
  testGroup
    "Laws"
    [ testCase "hard booleans do not preserve exactness by default" $ do
        assertBool "expected conservative degradation" propHardBooleanNotExact,
      testCase "zero set lawfulness is complete for hard booleans" $ do
        assertBool "expected full zero-set algebra" propZeroSetFullAlgebra,
      testCase "metric lawfulness stays conservative" $ do
        assertBool "expected metric degeneration" propMetricDegenerateOnly,
      testCase "smooth parents require Lipschitz proxies" $ do
        assertBool "expected proxy admissibility split" propProxyAdmissibility,
      testCase "certificate join is an explicit semilattice operation" $ do
        dcSemantics (join exactCertificate exactCertificate) @?= dcSemantics exactCertificate,
      testCase "support join uses empty as the neutral element" $ do
        let boundedSupport =
              maybe
                (error "expected valid AABB fixture")
                BoundedSupport
                (mkAabb (Vec3 (-1.0) (-1.0) (-1.0)) (Vec3 1.0 1.0 1.0))
        join EmptySupport boundedSupport @?= boundedSupport,
      testCase "unknown certification is explicit" $ do
        dcPrecisionLowerBound (join exactCertificate exactCertificate) @?= Certified (PrecisionFloor 1.0),
      QC.testProperty "onion preserves the child certificate" $
        QC.forAll (QC.choose (-1.0, 1.0)) $ \thickness ->
          propOnionPreservesCertificate thickness exactCertificate,
      QC.testProperty "onion preserves arbitrary nontrivial certificates" $
        QC.checkCoverage $
          QC.forAll genNontrivialDistanceCertificate $ \certificate ->
            QC.forAll (QC.choose (-1.0, 1.0)) $ \thickness ->
              coverCertificateSurface certificate $
                propOnionPreservesCertificate thickness certificate,
      QC.testProperty "hard booleans stay within the lower-bound regime" $
        propHardBooleanLowerBoundClosure exactCertificate exactCertificate
    ]

genNontrivialDistanceCertificate :: QC.Gen DistanceCertificate
genNontrivialDistanceCertificate =
  QC.suchThat genDistanceCertificate (/= exactCertificate)

genDistanceCertificate :: QC.Gen DistanceCertificate
genDistanceCertificate =
  DistanceCertificate
    <$> genDistanceSemantics
    <*> genTraceSafety
    <*> genCertification genPrecisionFloor
    <*> QC.arbitrary
    <*> genBoundEnvelope

genDistanceSemantics :: QC.Gen DistanceSemantics
genDistanceSemantics =
  QC.elements [ExactDist, ConservativeDist, PseudoField, Occupancy]

genTraceSafety :: QC.Gen TraceSafety
genTraceSafety =
  QC.oneof
    [ pure SphereTraceExact,
      SphereTraceConservative <$> genCertification genTraceStepScale,
      pure RequiresCertifiedStepper,
      pure UnsafeForSphereTracing
    ]

genBoundEnvelope :: QC.Gen BoundEnvelope
genBoundEnvelope =
  BoundEnvelope
    <$> genCertification genLipschitzUpperBound
    <*> genCertification genDirectionalLipschitz
    <*> genCertification genFarFieldLowerBound

genDirectionalLipschitz :: QC.Gen DirectionalLipschitz
genDirectionalLipschitz =
  DirectionalLipschitz
    <$> genNonNegativeVec3
    <*> genCertification genSignedPartialEnvelope

genSignedPartialEnvelope :: QC.Gen (Vec3, Vec3)
genSignedPartialEnvelope =
  orderedVec3Pair <$> genVec3 <*> genVec3

genCertification :: QC.Gen a -> QC.Gen (Certification a)
genCertification generator =
  QC.frequency
    [ (1, pure Unknown),
      (3, Certified <$> generator)
    ]

genTraceStepScale :: QC.Gen TraceStepScale
genTraceStepScale =
  TraceStepScale <$> QC.choose (0.0, 4.0)

genLipschitzUpperBound :: QC.Gen LipschitzUpperBound
genLipschitzUpperBound =
  LipschitzUpperBound <$> QC.choose (0.0, 8.0)

genFarFieldLowerBound :: QC.Gen FarFieldLowerBound
genFarFieldLowerBound =
  FarFieldLowerBound <$> QC.choose (-8.0, 8.0)

genPrecisionFloor :: QC.Gen PrecisionFloor
genPrecisionFloor =
  PrecisionFloor <$> QC.choose (0.0, 2.0)

genVec3 :: QC.Gen Vec3
genVec3 =
  Vec3
    <$> QC.choose (-4.0, 4.0)
    <*> QC.choose (-4.0, 4.0)
    <*> QC.choose (-4.0, 4.0)

genNonNegativeVec3 :: QC.Gen Vec3
genNonNegativeVec3 =
  Vec3
    <$> QC.choose (0.0, 4.0)
    <*> QC.choose (0.0, 4.0)
    <*> QC.choose (0.0, 4.0)

orderedVec3Pair :: Vec3 -> Vec3 -> (Vec3, Vec3)
orderedVec3Pair (Vec3 leftX leftY leftZ) (Vec3 rightX rightY rightZ) =
  ( Vec3 (min leftX rightX) (min leftY rightY) (min leftZ rightZ),
    Vec3 (max leftX rightX) (max leftY rightY) (max leftZ rightZ)
  )

coverCertificateSurface :: QC.Testable prop => DistanceCertificate -> prop -> QC.Property
coverCertificateSurface certificate =
  QC.cover 10 (dcSemantics certificate == ExactDist) "semantics:ExactDist"
    . QC.cover 10 (dcSemantics certificate == ConservativeDist) "semantics:ConservativeDist"
    . QC.cover 10 (dcSemantics certificate == PseudoField) "semantics:PseudoField"
    . QC.cover 10 (dcSemantics certificate == Occupancy) "semantics:Occupancy"
    . QC.cover 10 (dcTraceSafety certificate == SphereTraceExact) "trace:SphereTraceExact"
    . QC.cover 10 (isSphereTraceConservative (dcTraceSafety certificate)) "trace:SphereTraceConservative"
    . QC.cover 10 (dcTraceSafety certificate == RequiresCertifiedStepper) "trace:RequiresCertifiedStepper"
    . QC.cover 10 (dcTraceSafety certificate == UnsafeForSphereTracing) "trace:UnsafeForSphereTracing"
    . QC.cover 5 (dcSemantics certificate == ExactDist && dcTraceSafety certificate == SphereTraceExact) "pair:ExactDist×SphereTraceExact"
    . QC.cover 5 (dcSemantics certificate == ConservativeDist && isSphereTraceConservative (dcTraceSafety certificate)) "pair:ConservativeDist×SphereTraceConservative"
    . QC.cover 5 (dcSemantics certificate == PseudoField && dcTraceSafety certificate == RequiresCertifiedStepper) "pair:PseudoField×RequiresCertifiedStepper"

isSphereTraceConservative :: TraceSafety -> Bool
isSphereTraceConservative traceSafety =
  case traceSafety of
    SphereTraceConservative _ -> True
    _ -> False
