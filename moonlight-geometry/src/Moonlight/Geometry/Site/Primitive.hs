module Moonlight.Geometry.Site.Primitive
  ( SDFPrimitive (..),
    primitiveOrderingKey,
  )
where

import Data.Kind (Type)
import Moonlight.LinAlg.Geometry (Vec3 (..), vec3ToList)

type SDFPrimitive :: Type
data SDFPrimitive
  = Sphere Double
  | Box Vec3
  | Capsule Double Double
  | Cylinder Double Double
  | RoundedBox Vec3 Double
  | Torus Double Double
  | Superquadric Vec3 Double Double
  | VoronoiCell Vec3 Int
  | Cone Double Double
  | Prism Int Double Double
  deriving stock (Eq, Show, Read)

instance Ord SDFPrimitive where
  compare leftPrimitive rightPrimitive =
    compare (primitiveOrderingKey leftPrimitive) (primitiveOrderingKey rightPrimitive)

primitiveOrderingKey :: SDFPrimitive -> (Int, [Double], [Int])
primitiveOrderingKey = \case
  Sphere radius -> (0, [radius], [])
  Box size -> (1, vec3ToList size, [])
  Capsule radius height -> (2, [radius, height], [])
  Cylinder radius height -> (3, [radius, height], [])
  RoundedBox size radius -> (4, vec3ToList size <> [radius], [])
  Torus majorRadius minorRadius -> (5, [majorRadius, minorRadius], [])
  Superquadric axes exponent1 exponent2 -> (6, vec3ToList axes <> [exponent1, exponent2], [])
  VoronoiCell boundingBox seedCount -> (7, vec3ToList boundingBox, [seedCount])
  Cone radius height -> (8, [radius, height], [])
  Prism sideCount radius height -> (9, [radius, height], [sideCount])
