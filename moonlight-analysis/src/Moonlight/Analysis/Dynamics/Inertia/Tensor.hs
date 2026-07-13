module Moonlight.Analysis.Dynamics.Inertia.Tensor
  ( PointMass (..),
    parallelAxisCorrection,
    translatePointMasses,
    centerOfMass,
    inertiaTensorAboutOrigin,
    inertiaTensorAboutCenterOfMass,
    principalInertia,
  )
where

import Data.Kind (Type)
import Moonlight.Core (MoonlightError)
import Moonlight.LinAlg
  ( DiagonalizedSymmetric3,
    OrthonormalFrame,
    Symmetric3 (..),
    eigendecomposeSymmetric3OrthonormalFrame,
  )
import Moonlight.LinAlg.Geometry (Vec3 (..), addVec3, negateVec3, scaleVec3, vec3Zero)

type PointMass :: Type
data PointMass = PointMass
  { pointMassValue :: Double,
    pointMassPosition :: Vec3
  }
  deriving stock (Eq, Show, Read)

translatePointMasses :: Vec3 -> [PointMass] -> [PointMass]
translatePointMasses shiftVector =
  map
    ( \pointMass ->
        pointMass
          { pointMassPosition =
              addVec3 shiftVector (pointMassPosition pointMass)
          }
    )

centerOfMass :: [PointMass] -> Maybe Vec3
centerOfMass pointMasses =
  let totalMass = sum (map pointMassValue pointMasses)
   in if abs totalMass <= 1.0e-12
        then Nothing
        else
          Just
            ( scaleVec3
                (1.0 / totalMass)
                ( foldr
                    (\pointMass accumulator -> addVec3 accumulator (scaleVec3 (pointMassValue pointMass) (pointMassPosition pointMass)))
                    vec3Zero
                    pointMasses
                )
            )

inertiaTensorAboutOrigin :: [PointMass] -> Symmetric3 Double
inertiaTensorAboutOrigin =
  foldMap
    ( \pointMass ->
        parallelAxisCorrection
          (pointMassValue pointMass)
          (pointMassPosition pointMass)
    )

inertiaTensorAboutCenterOfMass :: [PointMass] -> Maybe (Symmetric3 Double)
inertiaTensorAboutCenterOfMass pointMasses =
  fmap
    ( \massCenter ->
        inertiaTensorAboutOrigin
          (translatePointMasses (negateVec3 massCenter) pointMasses)
    )
    (centerOfMass pointMasses)

principalInertia ::
  Symmetric3 Double ->
  Either MoonlightError (DiagonalizedSymmetric3 OrthonormalFrame Double)
principalInertia = eigendecomposeSymmetric3OrthonormalFrame

parallelAxisCorrection :: Double -> Vec3 -> Symmetric3 Double
parallelAxisCorrection massValue displacement =
  let Vec3 xValue yValue zValue = displacement
      radiusSquared = xValue * xValue + yValue * yValue + zValue * zValue
   in Symmetric3
        { sym3XX = massValue * (radiusSquared - xValue * xValue),
          sym3XY = -massValue * xValue * yValue,
          sym3XZ = -massValue * xValue * zValue,
          sym3YY = massValue * (radiusSquared - yValue * yValue),
          sym3YZ = -massValue * yValue * zValue,
          sym3ZZ = massValue * (radiusSquared - zValue * zValue)
        }
