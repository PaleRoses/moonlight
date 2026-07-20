module Moonlight.Geometry.Site.Semantics
  ( Certification (..),
    TraceStepScale (..),
    LipschitzUpperBound (..),
    FarFieldLowerBound (..),
    PrecisionFloor (..),
    GradientLipschitzUpperBound (..),
    CurvatureUpperBound (..),
    DistanceSemantics (..),
    TraceSafety (..),
    DirectionalLipschitz (..),
    BoundEnvelope (..),
    DistanceCertificate (..),
    InterfaceOperator (..),
    emptyBoundEnvelope,
    exactCertificate,
    conservativeCertificate,
    joinDistanceSemantics,
    semanticsJoin,
    weakenToConservative,
    weakenToPseudo,
    attachGlobalLipschitz,
    clearDifferentialBounds,
    supportsSphereCarving,
  )
where

import Data.Kind (Type)
import Moonlight.Algebra (JoinSemilattice (..))
import Moonlight.LinAlg.Geometry (Vec3 (..), maxVec3, minVec3)

type Certification :: Type -> Type
data Certification a
  = Unknown
  | Certified a
  deriving stock (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

type TraceStepScale :: Type
newtype TraceStepScale = TraceStepScale
  { getTraceStepScale :: Double
  }
  deriving stock (Eq, Ord, Show, Read)

type LipschitzUpperBound :: Type
newtype LipschitzUpperBound = LipschitzUpperBound
  { getLipschitzUpperBound :: Double
  }
  deriving stock (Eq, Ord, Show, Read)

type FarFieldLowerBound :: Type
newtype FarFieldLowerBound = FarFieldLowerBound
  { getFarFieldLowerBound :: Double
  }
  deriving stock (Eq, Ord, Show, Read)

type PrecisionFloor :: Type
newtype PrecisionFloor = PrecisionFloor
  { getPrecisionFloor :: Double
  }
  deriving stock (Eq, Ord, Show, Read)

type GradientLipschitzUpperBound :: Type
newtype GradientLipschitzUpperBound = GradientLipschitzUpperBound
  { getGradientLipschitzUpperBound :: Double
  }
  deriving stock (Eq, Ord, Show, Read)

type CurvatureUpperBound :: Type
newtype CurvatureUpperBound = CurvatureUpperBound
  { getCurvatureUpperBound :: Double
  }
  deriving stock (Eq, Ord, Show, Read)

type DistanceSemantics :: Type
data DistanceSemantics
  = ExactDist
  | ConservativeDist
  | PseudoField
  | Occupancy
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

type TraceSafety :: Type
data TraceSafety
  = SphereTraceExact
  | SphereTraceConservative !(Certification TraceStepScale)
  | RequiresCertifiedStepper
  | UnsafeForSphereTracing
  deriving stock (Eq, Ord, Show, Read)

type DirectionalLipschitz :: Type
data DirectionalLipschitz = DirectionalLipschitz
  { dlAxisUpper :: !Vec3,
    dlSignedPartials :: !(Certification (Vec3, Vec3))
  }
  deriving stock (Eq, Ord, Show)

type BoundEnvelope :: Type
data BoundEnvelope = BoundEnvelope
  { beGlobalLipschitz :: !(Certification LipschitzUpperBound),
    beDirectional :: !(Certification DirectionalLipschitz),
    beFarFieldLowerBound :: !(Certification FarFieldLowerBound)
  }
  deriving stock (Eq, Ord, Show)

type DistanceCertificate :: Type
data DistanceCertificate = DistanceCertificate
  { dcSemantics :: !DistanceSemantics,
    dcTraceSafety :: !TraceSafety,
    dcPrecisionLowerBound :: !(Certification PrecisionFloor),
    dcSignConsistentAwayFromZero :: !Bool,
    dcBounds :: !BoundEnvelope
  }
  deriving stock (Eq, Ord, Show)

type InterfaceOperator :: Type
data InterfaceOperator
  = InterfaceSmoothUnion
  | InterfaceSmoothSubtract
  | InterfaceSmoothIntersect
  | InterfaceHardUnion
  | InterfaceHardSubtract
  | InterfaceHardIntersect
  | InterfaceChamfer
  | InterfaceRound
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

instance JoinSemilattice DistanceSemantics where
  join leftSemantics rightSemantics =
    if distanceSemanticsRank leftSemantics >= distanceSemanticsRank rightSemantics
      then leftSemantics
      else rightSemantics

instance JoinSemilattice TraceStepScale where
  join (TraceStepScale leftBound) (TraceStepScale rightBound) =
    TraceStepScale (min leftBound rightBound)

instance JoinSemilattice LipschitzUpperBound where
  join (LipschitzUpperBound leftBound) (LipschitzUpperBound rightBound) =
    LipschitzUpperBound (max leftBound rightBound)

instance JoinSemilattice FarFieldLowerBound where
  join (FarFieldLowerBound leftBound) (FarFieldLowerBound rightBound) =
    FarFieldLowerBound (min leftBound rightBound)

instance JoinSemilattice PrecisionFloor where
  join (PrecisionFloor leftBound) (PrecisionFloor rightBound) =
    PrecisionFloor (min leftBound rightBound)

instance JoinSemilattice GradientLipschitzUpperBound where
  join (GradientLipschitzUpperBound leftBound) (GradientLipschitzUpperBound rightBound) =
    GradientLipschitzUpperBound (max leftBound rightBound)

instance JoinSemilattice CurvatureUpperBound where
  join (CurvatureUpperBound leftBound) (CurvatureUpperBound rightBound) =
    CurvatureUpperBound (max leftBound rightBound)

instance JoinSemilattice a => JoinSemilattice (Certification a) where
  join leftCertification rightCertification =
    case (leftCertification, rightCertification) of
      (Certified leftValue, Certified rightValue) -> Certified (join leftValue rightValue)
      _ -> Unknown

instance JoinSemilattice TraceSafety where
  join leftSafety rightSafety =
    case (leftSafety, rightSafety) of
      (UnsafeForSphereTracing, _) -> UnsafeForSphereTracing
      (_, UnsafeForSphereTracing) -> UnsafeForSphereTracing
      (RequiresCertifiedStepper, _) -> RequiresCertifiedStepper
      (_, RequiresCertifiedStepper) -> RequiresCertifiedStepper
      (SphereTraceExact, safety) -> safety
      (safety, SphereTraceExact) -> safety
      (SphereTraceConservative leftQuality, SphereTraceConservative rightQuality) ->
        SphereTraceConservative (join leftQuality rightQuality)

instance JoinSemilattice DirectionalLipschitz where
  join leftDirectional rightDirectional =
    DirectionalLipschitz
      { dlAxisUpper = maxVec3 (dlAxisUpper leftDirectional) (dlAxisUpper rightDirectional),
        dlSignedPartials = joinCertificationWith joinSignedPartials (dlSignedPartials leftDirectional) (dlSignedPartials rightDirectional)
      }

instance JoinSemilattice BoundEnvelope where
  join leftEnvelope rightEnvelope =
    BoundEnvelope
      { beGlobalLipschitz = join (beGlobalLipschitz leftEnvelope) (beGlobalLipschitz rightEnvelope),
        beDirectional = join (beDirectional leftEnvelope) (beDirectional rightEnvelope),
        beFarFieldLowerBound = join (beFarFieldLowerBound leftEnvelope) (beFarFieldLowerBound rightEnvelope)
      }

instance JoinSemilattice DistanceCertificate where
  join leftCertificate rightCertificate =
    DistanceCertificate
      { dcSemantics = join (dcSemantics leftCertificate) (dcSemantics rightCertificate),
        dcTraceSafety = join (dcTraceSafety leftCertificate) (dcTraceSafety rightCertificate),
        dcPrecisionLowerBound = join (dcPrecisionLowerBound leftCertificate) (dcPrecisionLowerBound rightCertificate),
        dcSignConsistentAwayFromZero =
          dcSignConsistentAwayFromZero leftCertificate && dcSignConsistentAwayFromZero rightCertificate,
        dcBounds = join (dcBounds leftCertificate) (dcBounds rightCertificate)
      }

emptyBoundEnvelope :: BoundEnvelope
emptyBoundEnvelope =
  BoundEnvelope
    { beGlobalLipschitz = Unknown,
      beDirectional = Unknown,
      beFarFieldLowerBound = Unknown
    }

exactCertificate :: DistanceCertificate
exactCertificate =
  DistanceCertificate
    { dcSemantics = ExactDist,
      dcTraceSafety = SphereTraceExact,
      dcPrecisionLowerBound = Certified (PrecisionFloor 1.0),
      dcSignConsistentAwayFromZero = True,
      dcBounds = emptyBoundEnvelope
    }

conservativeCertificate :: DistanceCertificate
conservativeCertificate =
  DistanceCertificate
    { dcSemantics = ConservativeDist,
      dcTraceSafety = SphereTraceConservative Unknown,
      dcPrecisionLowerBound = Unknown,
      dcSignConsistentAwayFromZero = True,
      dcBounds = emptyBoundEnvelope
    }

joinCertificationWith :: (a -> a -> a) -> Certification a -> Certification a -> Certification a
joinCertificationWith combine leftCertification rightCertification =
  case (leftCertification, rightCertification) of
    (Certified leftValue, Certified rightValue) -> Certified (combine leftValue rightValue)
    _ -> Unknown

joinDistanceSemantics :: DistanceSemantics -> DistanceSemantics -> DistanceSemantics
joinDistanceSemantics = join

semanticsJoin :: DistanceCertificate -> DistanceCertificate -> DistanceCertificate
semanticsJoin = join

weakenToConservative :: DistanceCertificate -> DistanceCertificate
weakenToConservative certificate =
  certificate
    { dcSemantics = join ConservativeDist (dcSemantics certificate),
      dcTraceSafety =
        case dcTraceSafety certificate of
          SphereTraceExact ->
            SphereTraceConservative (Certified (TraceStepScale 1.0))
          existingSafety ->
            join existingSafety (SphereTraceConservative Unknown),
      dcPrecisionLowerBound = dcPrecisionLowerBound certificate
    }

weakenToPseudo :: DistanceCertificate -> DistanceCertificate
weakenToPseudo certificate =
  certificate
    { dcSemantics = join PseudoField (dcSemantics certificate),
      dcTraceSafety = join (dcTraceSafety certificate) RequiresCertifiedStepper,
      dcPrecisionLowerBound = Unknown
    }

attachGlobalLipschitz :: Certification LipschitzUpperBound -> DistanceCertificate -> DistanceCertificate
attachGlobalLipschitz certification certificate =
  certificate
    { dcBounds =
        (dcBounds certificate)
          { beGlobalLipschitz =
              case certification of
                Unknown -> beGlobalLipschitz (dcBounds certificate)
                Certified boundValue -> Certified boundValue
          }
    }

clearDifferentialBounds :: DistanceCertificate -> DistanceCertificate
clearDifferentialBounds certificate =
  certificate
    { dcBounds =
        (dcBounds certificate)
          { beGlobalLipschitz = Unknown,
            beDirectional = Unknown
          }
    }

supportsSphereCarving :: DistanceCertificate -> Bool
supportsSphereCarving certificate =
  distanceSemanticsRank (dcSemantics certificate) <= distanceSemanticsRank ConservativeDist

joinSignedPartials :: (Vec3, Vec3) -> (Vec3, Vec3) -> (Vec3, Vec3)
joinSignedPartials (leftLower, leftUpper) (rightLower, rightUpper) =
  ( minVec3 leftLower rightLower,
    maxVec3 leftUpper rightUpper
  )

distanceSemanticsRank :: DistanceSemantics -> Int
distanceSemanticsRank = \case
  ExactDist -> 0
  ConservativeDist -> 1
  PseudoField -> 2
  Occupancy -> 3
