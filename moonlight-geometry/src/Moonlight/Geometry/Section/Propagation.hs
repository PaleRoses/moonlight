module Moonlight.Geometry.Section.Propagation
  ( BooleanMetricWitness (..),
    hardBooleanCertificate,
    sdfTokenDistanceCertificate,
  )
where

import Data.Kind (Type)
import Moonlight.Geometry.Site.Parameters
import Moonlight.Geometry.Site.Semantics
import Moonlight.Geometry.Site.Token
import Moonlight.LinAlg.Geometry

type BooleanMetricWitness :: Type
data BooleanMetricWitness = BooleanMetricWitness
  { bmwSemantics :: !DistanceSemantics,
    bmwTraceSafety :: !TraceSafety,
    bmwPrecisionLowerBound :: !(Certification PrecisionFloor)
  }
  deriving stock (Eq, Show)

hardBooleanCertificate ::
  Maybe BooleanMetricWitness ->
  DistanceCertificate ->
  DistanceCertificate ->
  DistanceCertificate
hardBooleanCertificate maybeWitness leftCertificate rightCertificate =
  case maybeWitness of
    Just witness ->
      let joinedCertificate = semanticsJoin leftCertificate rightCertificate
       in joinedCertificate
            { dcSemantics = bmwSemantics witness,
              dcTraceSafety = bmwTraceSafety witness,
              dcPrecisionLowerBound = bmwPrecisionLowerBound witness
            }
    Nothing -> weakenToConservative (semanticsJoin leftCertificate rightCertificate)

sdfTokenDistanceCertificate :: SDFTokenF DistanceCertificate -> DistanceCertificate
sdfTokenDistanceCertificate = \case
  Prim _ -> exactCertificate
  HardUnion leftCertificate rightCertificate -> hardBooleanCertificate Nothing leftCertificate rightCertificate
  HardSubtract leftCertificate rightCertificate -> hardBooleanCertificate Nothing leftCertificate rightCertificate
  HardIntersect leftCertificate rightCertificate -> hardBooleanCertificate Nothing leftCertificate rightCertificate
  SmoothUnion _ leftCertificate rightCertificate -> weakenToPseudo (semanticsJoin leftCertificate rightCertificate)
  SmoothSubtract _ leftCertificate rightCertificate -> weakenToPseudo (semanticsJoin leftCertificate rightCertificate)
  SmoothIntersect _ leftCertificate rightCertificate -> weakenToPseudo (semanticsJoin leftCertificate rightCertificate)
  Chamfer radius leftCertificate rightCertificate
    | radius <= 0.0 -> hardBooleanCertificate Nothing leftCertificate rightCertificate
    | otherwise -> weakenToConservative (semanticsJoin leftCertificate rightCertificate)
  Round radius childCertificate
    | radius <= 0.0 -> childCertificate
    | otherwise -> weakenToConservative childCertificate
  NoisePerturbation noiseParams childCertificate
    | isNeutralNoiseParams noiseParams -> childCertificate
    | noiseKernelRegularity (npKernel noiseParams) >= LipschitzRegular -> weakenToPseudo childCertificate
    | otherwise -> clearDifferentialBounds (weakenToPseudo childCertificate)
  DomainWarp noiseParams childCertificate
    | isNeutralNoiseParams noiseParams -> childCertificate
    | noiseKernelRegularity (npKernel noiseParams) >= LipschitzRegular -> weakenToPseudo childCertificate
    | otherwise -> clearDifferentialBounds (weakenToPseudo childCertificate)
  Twist rate childCertificate
    | rate == 0.0 -> childCertificate
    | otherwise -> weakenToConservative childCertificate
  Bend rate childCertificate
    | rate == 0.0 -> childCertificate
    | otherwise -> weakenToConservative childCertificate
  Onion thickness childCertificate
    | thickness <= 0.0 -> childCertificate
    | otherwise -> childCertificate
  Transform affineTransform childCertificate ->
    case affineMetricEffect affineTransform of
      MetricIsometry -> childCertificate
      UniformMetricScale _ -> weakenToConservative childCertificate
      AnisotropicMetricDistortion -> weakenToConservative childCertificate
  Scale scaleVector childCertificate
    | isIdentityScale scaleVector -> childCertificate
    | otherwise -> weakenToConservative childCertificate
  Repeat repeatParams childCertificate ->
    case repeatExtent repeatParams of
      RepeatSingleton -> childCertificate
      _ -> weakenToConservative childCertificate
  SDFEmpty -> exactCertificate
