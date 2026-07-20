module Moonlight.Geometry.Section.Analysis
  ( SpatialSupport (..),
    supportBounds,
    SDFAnalysis (..),
    sdfAnalysisSpec,
    aabbFromPrimitive,
    tokenSupport,
    tokenLipschitz,
  )
where

import Data.Kind (Type)
import Moonlight.Algebra (JoinSemilattice (..))
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec, semilatticeAnalysis)
import Moonlight.LinAlg.Geometry
  ( AABB,
    expandAabb,
    intersectAabbMaybe,
    scaleAabb,
    symmetricAabb,
    transformAabb,
    translateAabb,
  )
import Moonlight.Geometry.Section.Propagation (sdfTokenDistanceCertificate)
import Moonlight.Geometry.Site.Parameters
import Moonlight.Geometry.Site.Primitive
import Moonlight.Geometry.Site.Semantics
  ( Certification (..),
    DistanceCertificate,
    LipschitzUpperBound (..),
    attachGlobalLipschitz,
  )
import Moonlight.Geometry.Site.Token (SDFTokenF (..))
import Moonlight.LinAlg.Geometry (AffineTransform, affineMaxScale)
import Moonlight.LinAlg.Geometry (Vec3 (..), maxAbsComponentVec3, scaleVec3)

type SpatialSupport :: Type
data SpatialSupport
  = EmptySupport
  | BoundedSupport !AABB
  | UnboundedSupport
  deriving stock (Eq, Ord, Show)

type SDFAnalysis :: Type
data SDFAnalysis = SDFAnalysis
  { saSupport :: !SpatialSupport,
    saDistanceCertificate :: !DistanceCertificate,
    saLipschitzBound :: !(Certification LipschitzUpperBound),
    saNodeCount :: !Int
  }
  deriving stock (Eq, Ord, Show)

instance JoinSemilattice SpatialSupport where
  join leftSupport rightSupport =
    case (leftSupport, rightSupport) of
      (UnboundedSupport, _) -> UnboundedSupport
      (_, UnboundedSupport) -> UnboundedSupport
      (EmptySupport, supportValue) -> supportValue
      (supportValue, EmptySupport) -> supportValue
      (BoundedSupport leftAabb, BoundedSupport rightAabb) -> BoundedSupport (join leftAabb rightAabb)

instance JoinSemilattice SDFAnalysis where
  join leftAnalysis rightAnalysis =
    SDFAnalysis
      { saSupport = join (saSupport leftAnalysis) (saSupport rightAnalysis),
        saDistanceCertificate = join (saDistanceCertificate leftAnalysis) (saDistanceCertificate rightAnalysis),
        saLipschitzBound = join (saLipschitzBound leftAnalysis) (saLipschitzBound rightAnalysis),
        saNodeCount = max (saNodeCount leftAnalysis) (saNodeCount rightAnalysis)
      }

supportBounds :: SpatialSupport -> Maybe AABB
supportBounds = \case
  EmptySupport -> Nothing
  BoundedSupport aabb -> Just aabb
  UnboundedSupport -> Nothing

sdfAnalysisSpec :: AnalysisSpec SDFTokenF SDFAnalysis
sdfAnalysisSpec = semilatticeAnalysis makeSdfAnalysis

makeSdfAnalysis :: SDFTokenF SDFAnalysis -> SDFAnalysis
makeSdfAnalysis token =
  let supportValue = tokenSupport token
      lipschitzBound = tokenLipschitz token
      certificate = attachGlobalLipschitz lipschitzBound (sdfTokenDistanceCertificate (fmap saDistanceCertificate token))
   in SDFAnalysis
        { saSupport = supportValue,
          saDistanceCertificate = certificate,
          saLipschitzBound = lipschitzBound,
          saNodeCount = 1 + foldr ((+) . saNodeCount) 0 token
        }

aabbFromPrimitive :: SDFPrimitive -> Maybe AABB
aabbFromPrimitive = \case
  Sphere radius -> symmetricAabb radius radius radius
  Box (Vec3 halfX halfY halfZ) -> symmetricAabb halfX halfY halfZ
  Capsule radius height -> symmetricAabb radius (radius + height * 0.5) radius
  Cylinder radius height -> symmetricAabb radius (height * 0.5) radius
  RoundedBox (Vec3 halfX halfY halfZ) radius -> symmetricAabb (halfX + radius) (halfY + radius) (halfZ + radius)
  Torus majorRadius minorRadius -> symmetricAabb (majorRadius + minorRadius) minorRadius (majorRadius + minorRadius)
  Superquadric (Vec3 halfX halfY halfZ) _ _ -> symmetricAabb halfX halfY halfZ
  VoronoiCell (Vec3 halfX halfY halfZ) _ -> symmetricAabb halfX halfY halfZ
  Cone radius height -> symmetricAabb radius (height * 0.5) radius
  Prism _ radius height -> symmetricAabb radius (height * 0.5) radius

tokenSupport :: SDFTokenF SDFAnalysis -> SpatialSupport
tokenSupport = \case
  Prim primitive -> maybe UnboundedSupport BoundedSupport (aabbFromPrimitive primitive)
  SmoothUnion _ leftAnalysis rightAnalysis -> join (saSupport leftAnalysis) (saSupport rightAnalysis)
  SmoothSubtract _ leftAnalysis rightAnalysis -> join (saSupport leftAnalysis) (saSupport rightAnalysis)
  SmoothIntersect _ leftAnalysis rightAnalysis -> intersectSupport (saSupport leftAnalysis) (saSupport rightAnalysis)
  HardUnion leftAnalysis rightAnalysis -> join (saSupport leftAnalysis) (saSupport rightAnalysis)
  HardSubtract leftAnalysis rightAnalysis -> join (saSupport leftAnalysis) (saSupport rightAnalysis)
  HardIntersect leftAnalysis rightAnalysis -> intersectSupport (saSupport leftAnalysis) (saSupport rightAnalysis)
  Chamfer radius leftAnalysis rightAnalysis -> expandSupport (abs radius) (join (saSupport leftAnalysis) (saSupport rightAnalysis))
  Round radius childAnalysis -> expandSupport (abs radius) (saSupport childAnalysis)
  NoisePerturbation noiseParams childAnalysis -> expandSupport (noiseDisplacementRadius noiseParams) (saSupport childAnalysis)
  DomainWarp noiseParams childAnalysis -> expandSupport (noiseDisplacementRadius noiseParams) (saSupport childAnalysis)
  Twist _ childAnalysis -> saSupport childAnalysis
  Bend _ childAnalysis -> saSupport childAnalysis
  Onion thickness childAnalysis -> expandSupport (abs thickness) (saSupport childAnalysis)
  Transform affineTransform childAnalysis -> transformSupport affineTransform (saSupport childAnalysis)
  Scale scaleVector childAnalysis -> scaleSupport scaleVector (saSupport childAnalysis)
  Repeat repeatParams childAnalysis -> repeatSupport repeatParams (saSupport childAnalysis)
  SDFEmpty -> EmptySupport

tokenLipschitz :: SDFTokenF SDFAnalysis -> Certification LipschitzUpperBound
tokenLipschitz = \case
  Prim _ -> Certified (LipschitzUpperBound 1.0)
  SmoothUnion _ leftAnalysis rightAnalysis -> join (saLipschitzBound leftAnalysis) (saLipschitzBound rightAnalysis)
  SmoothSubtract _ leftAnalysis rightAnalysis -> join (saLipschitzBound leftAnalysis) (saLipschitzBound rightAnalysis)
  SmoothIntersect _ leftAnalysis rightAnalysis -> join (saLipschitzBound leftAnalysis) (saLipschitzBound rightAnalysis)
  HardUnion leftAnalysis rightAnalysis -> join (saLipschitzBound leftAnalysis) (saLipschitzBound rightAnalysis)
  HardSubtract leftAnalysis rightAnalysis -> join (saLipschitzBound leftAnalysis) (saLipschitzBound rightAnalysis)
  HardIntersect leftAnalysis rightAnalysis -> join (saLipschitzBound leftAnalysis) (saLipschitzBound rightAnalysis)
  Chamfer _ leftAnalysis rightAnalysis -> join (saLipschitzBound leftAnalysis) (saLipschitzBound rightAnalysis)
  Round _ childAnalysis -> saLipschitzBound childAnalysis
  NoisePerturbation noiseParams childAnalysis
    | isNeutralNoiseParams noiseParams -> saLipschitzBound childAnalysis
    | noiseKernelRegularity (npKernel noiseParams) < LipschitzRegular -> Unknown
    | otherwise ->
        fmap
          ( \(LipschitzUpperBound boundValue) ->
              LipschitzUpperBound (boundValue + noiseDisplacementRadius noiseParams * npFrequency noiseParams)
          )
          (saLipschitzBound childAnalysis)
  DomainWarp noiseParams childAnalysis
    | isNeutralNoiseParams noiseParams -> saLipschitzBound childAnalysis
    | noiseKernelRegularity (npKernel noiseParams) < LipschitzRegular -> Unknown
    | otherwise ->
        fmap
          ( \(LipschitzUpperBound boundValue) ->
              LipschitzUpperBound (boundValue + noiseDisplacementRadius noiseParams * npFrequency noiseParams)
          )
          (saLipschitzBound childAnalysis)
  Twist twistRate childAnalysis ->
    fmap
      (\(LipschitzUpperBound boundValue) -> LipschitzUpperBound (boundValue * max 1.0 (abs twistRate)))
      (saLipschitzBound childAnalysis)
  Bend bendRate childAnalysis ->
    fmap
      (\(LipschitzUpperBound boundValue) -> LipschitzUpperBound (boundValue * max 1.0 (abs bendRate)))
      (saLipschitzBound childAnalysis)
  Onion _ childAnalysis -> saLipschitzBound childAnalysis
  Transform affineTransform childAnalysis ->
    fmap
      (\(LipschitzUpperBound boundValue) -> LipschitzUpperBound (boundValue * affineMaxScale affineTransform))
      (saLipschitzBound childAnalysis)
  Scale scaleVector childAnalysis ->
    fmap
      (\(LipschitzUpperBound boundValue) -> LipschitzUpperBound (boundValue * maxAbsComponentVec3 scaleVector))
      (saLipschitzBound childAnalysis)
  Repeat repeatParams childAnalysis ->
    case repeatExtent repeatParams of
      RepeatEmpty -> Certified (LipschitzUpperBound 0.0)
      _ -> saLipschitzBound childAnalysis
  SDFEmpty -> Certified (LipschitzUpperBound 0.0)

intersectSupport :: SpatialSupport -> SpatialSupport -> SpatialSupport
intersectSupport leftSupport rightSupport =
  case (leftSupport, rightSupport) of
    (EmptySupport, _) -> EmptySupport
    (_, EmptySupport) -> EmptySupport
    (UnboundedSupport, supportValue) -> supportValue
    (supportValue, UnboundedSupport) -> supportValue
    (BoundedSupport leftAabb, BoundedSupport rightAabb) -> maybe EmptySupport BoundedSupport (intersectAabbMaybe leftAabb rightAabb)

expandSupport :: Double -> SpatialSupport -> SpatialSupport
expandSupport radius supportValue =
  case supportValue of
    EmptySupport -> EmptySupport
    UnboundedSupport -> UnboundedSupport
    BoundedSupport aabbValue -> maybe UnboundedSupport BoundedSupport (expandAabb radius aabbValue)

transformSupport :: AffineTransform -> SpatialSupport -> SpatialSupport
transformSupport affineTransform = \case
  EmptySupport -> EmptySupport
  UnboundedSupport -> UnboundedSupport
  BoundedSupport aabbValue -> BoundedSupport (transformAabb affineTransform aabbValue)

scaleSupport :: Vec3 -> SpatialSupport -> SpatialSupport
scaleSupport scaleVector = \case
  EmptySupport -> EmptySupport
  UnboundedSupport -> UnboundedSupport
  BoundedSupport aabbValue -> BoundedSupport (scaleAabb scaleVector aabbValue)

repeatSupport :: RepeatParams -> SpatialSupport -> SpatialSupport
repeatSupport repeatParams supportValue =
  case supportValue of
    EmptySupport -> EmptySupport
    UnboundedSupport -> UnboundedSupport
    BoundedSupport aabbValue ->
      case repeatExtent repeatParams of
        RepeatInfinite -> UnboundedSupport
        RepeatEmpty -> EmptySupport
        RepeatSingleton -> BoundedSupport aabbValue
        RepeatFinite countValue ->
              let repetitionSpan = scaleVec3 (fromIntegral (countValue - 1)) (rpStride repeatParams)
                  translatedAabb = translateAabb repetitionSpan aabbValue
               in BoundedSupport (join aabbValue translatedAabb)
