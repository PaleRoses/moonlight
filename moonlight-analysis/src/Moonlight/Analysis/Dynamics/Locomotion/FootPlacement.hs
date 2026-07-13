module Moonlight.Analysis.Dynamics.Locomotion.FootPlacement
  ( ContactType (..),
    SurfaceHit (..),
    TerrainOracle (..),
    FootPlacementSpec (..),
    defaultFootPlacementSpec,
    searchFoothold,
    sampleConeCandidates,
  )
where

import Data.Kind (Type)
import Moonlight.LinAlg.Geometry (Vec3 (..), addVec3, dotVec3, magnitudeVec3, normalizeVec3Safe, vec3Zero)

type ContactType :: Type
data ContactType
  = FootContact
  | ClawContact
  | SuckerContact
  | SurfaceAttachContact
  deriving stock (Eq, Show, Read)

type SurfaceHit :: Type
data SurfaceHit = SurfaceHit
  { surfaceHitPosition :: Vec3,
    surfaceHitNormal :: Vec3
  }
  deriving stock (Eq, Show, Read)

type TerrainOracle :: Type
data TerrainOracle = TerrainOracle
  { terrainRaycastDown :: Vec3 -> Maybe SurfaceHit,
    terrainSignedDistance :: Vec3 -> Double,
    terrainSurfaceNormal :: Vec3 -> Vec3
  }

type FootPlacementSpec :: Type
data FootPlacementSpec = FootPlacementSpec
  { footPlacementContactType :: ContactType,
    footPlacementSearchConeAngle :: Double,
    footPlacementMaxSearchRadius :: Double,
    footPlacementSurfaceThreshold :: Double
  }
  deriving stock (Eq, Show, Read)

defaultFootPlacementSpec :: ContactType -> FootPlacementSpec
defaultFootPlacementSpec contactType =
  FootPlacementSpec
    { footPlacementContactType = contactType,
      footPlacementSearchConeAngle = pi / 6.0,
      footPlacementMaxSearchRadius = 1.0,
      footPlacementSurfaceThreshold = 0.15
    }

searchFoothold :: FootPlacementSpec -> TerrainOracle -> Vec3 -> Maybe SurfaceHit
searchFoothold spec oracle kinematicTarget =
  case terrainRaycastDown oracle kinematicTarget of
    Just surfaceHit
      | surfaceNormalAcceptable (footPlacementContactType spec) (surfaceHitNormal surfaceHit) ->
          Just surfaceHit
    _ ->
      searchConeFoothold spec oracle kinematicTarget

sampleConeCandidates :: FootPlacementSpec -> Vec3 -> [Vec3]
sampleConeCandidates spec apex =
  fmap (candidatePosition apex (footPlacementSearchConeAngle spec))
    (innerRingOffsets <> outerRingOffsets)
  where
    radius = footPlacementMaxSearchRadius spec
    innerRadius = 0.5 * radius
    innerRingOffsets =
      fmap
        (scaleOffset innerRadius)
        [ Vec3 1.0 0.0 0.0,
          Vec3 0.0 0.0 1.0,
          Vec3 (-1.0) 0.0 0.0,
          Vec3 0.0 0.0 (-1.0)
        ]
    outerRingOffsets =
      fmap
        (scaleOffset radius)
        [ Vec3 reciprocalSqrt2 0.0 reciprocalSqrt2,
          Vec3 (-reciprocalSqrt2) 0.0 reciprocalSqrt2,
          Vec3 (-reciprocalSqrt2) 0.0 (-reciprocalSqrt2),
          Vec3 reciprocalSqrt2 0.0 (-reciprocalSqrt2)
        ]

searchConeFoothold :: FootPlacementSpec -> TerrainOracle -> Vec3 -> Maybe SurfaceHit
searchConeFoothold spec oracle kinematicTarget =
  fmap materializeCandidate
    (bestCandidateWithinThreshold spec oracle (sampleConeCandidates spec kinematicTarget))
  where
    materializeCandidate candidatePositionValue =
      SurfaceHit
        { surfaceHitPosition = candidatePositionValue,
          surfaceHitNormal = terrainSurfaceNormal oracle candidatePositionValue
        }

bestCandidateWithinThreshold :: FootPlacementSpec -> TerrainOracle -> [Vec3] -> Maybe Vec3
bestCandidateWithinThreshold spec oracle candidates =
  case bestCandidate oracle candidates of
    Just candidatePositionValue
      | abs (terrainSignedDistance oracle candidatePositionValue) <= footPlacementSurfaceThreshold spec
          && surfaceNormalAcceptable (footPlacementContactType spec) (terrainSurfaceNormal oracle candidatePositionValue) ->
            Just candidatePositionValue
    _ ->
      Nothing

bestCandidate :: TerrainOracle -> [Vec3] -> Maybe Vec3
bestCandidate oracle candidates =
  foldr preferCloser Nothing candidates
  where
    preferCloser candidateValue accumulated =
      case accumulated of
        Nothing ->
          Just candidateValue
        Just incumbent ->
          if abs (terrainSignedDistance oracle candidateValue) < abs (terrainSignedDistance oracle incumbent)
            then Just candidateValue
            else Just incumbent

candidatePosition :: Vec3 -> Double -> Vec3 -> Vec3
candidatePosition apex coneAngle lateralOffset =
  let lateralMagnitude = magnitudeVec3 lateralOffset
      depthOffset = lateralMagnitude / max 1.0e-6 (tan coneAngle)
   in addVec3 apex (Vec3 (vecX lateralOffset) (-depthOffset) (vecZ lateralOffset))

surfaceNormalAcceptable :: ContactType -> Vec3 -> Bool
surfaceNormalAcceptable contactType surfaceNormal =
  let normalizedNormal = normalizeNormalForSlope surfaceNormal
      slopeCosine = dotVec3 normalizedNormal upDirection
      slopeAngle = acos (clamp (-1.0) 1.0 slopeCosine)
   in slopeAngle <= gripAngle contactType

gripAngle :: ContactType -> Double
gripAngle contactType =
  case contactType of
    FootContact -> pi / 4.0
    ClawContact -> 70.0 * pi / 180.0
    SuckerContact -> 80.0 * pi / 180.0
    SurfaceAttachContact -> pi

scaleOffset :: Double -> Vec3 -> Vec3
scaleOffset scaleValue offset =
  Vec3
    (scaleValue * vecX offset)
    0.0
    (scaleValue * vecZ offset)

normalizeNormalForSlope :: Vec3 -> Vec3
normalizeNormalForSlope v =
  let n = normalizeVec3Safe v
   in if n == vec3Zero then upDirection else n

clamp :: Double -> Double -> Double -> Double
clamp lowerBound upperBound value =
  max lowerBound (min upperBound value)

upDirection :: Vec3
upDirection = Vec3 0.0 1.0 0.0

reciprocalSqrt2 :: Double
reciprocalSqrt2 = recip (sqrt 2.0)
