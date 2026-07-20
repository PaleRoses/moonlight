{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Geometry.Discrete.Connection
  ( Rotation2,
    rotation2Angle,
    UnitQuaternion (..),
    Rotation3,
    rotation3Quaternion,
    SO,
    DiscreteConnection,
    SpecialOrthogonal (..),
    mkRotation2,
    mkRotation3,
    rotation3FromAxisAngle,
    rotation3AxisAngle,
    composeConnection,
    inverseConnection,
    composeConnectionPath,
    parallelTransport,
    leviCivitaConnectionFromTangents,
    pullbackMetricTensor2,
    pullbackMetricTensor3,
    metricCompatibleConnection,
  )
where

import Data.Kind (Constraint, Type)
import Moonlight.Algebra
  ( AbelianGroup,
    Action (..),
    Group (..),
    InvertibleAction,
  )
import Moonlight.Geometry.Discrete.Metric
  ( Dim (..),
    DiscreteMetricTensor,
    VecN,
    epsilon,
    metricTensor2RelApproxEq,
    metricTensor3RelApproxEq,
  )
import Moonlight.LinAlg.Geometry
  ( Symmetric2 (..),
    Symmetric3 (..),
    applySymmetric2,
    applySymmetric3,
  )
import Moonlight.LinAlg.Geometry
  ( Vec2 (..),
    crossZVec2,
    dotVec2,
    magnitudeVec2,
    normalizeVec2Safe,
  )
import Moonlight.LinAlg.Geometry
  ( Vec3 (..),
    addVec3,
    crossVec3,
    dotVec3,
    magnitudeVec3,
    normalizeVec3Safe,
    scaleVec3,
  )
import Prelude

type SO :: Dim -> Type
type family SO (d :: Dim) = r | r -> d where
  SO 'D2 = Rotation2
  SO 'D3 = Rotation3

type DiscreteConnection :: Dim -> Type
type DiscreteConnection d = SO d

type Rotation2 :: Type
newtype Rotation2 = Rotation2
  { rotation2Angle :: Double
  }
  deriving stock (Eq, Ord, Show, Read)

type UnitQuaternion :: Type
data UnitQuaternion = UnitQuaternion
  { quatW :: !Double,
    quatX :: !Double,
    quatY :: !Double,
    quatZ :: !Double
  }
  deriving stock (Eq, Ord, Show, Read)

type Rotation3 :: Type
newtype Rotation3 = Rotation3
  { rotation3Quaternion :: UnitQuaternion
  }
  deriving stock (Eq, Ord, Show, Read)

type SpecialOrthogonal :: Dim -> Constraint
class SpecialOrthogonal (d :: Dim) where
  identitySO :: SO d
  composeSO :: SO d -> SO d -> SO d
  inverseSO :: SO d -> SO d
  applySO :: SO d -> VecN d -> VecN d
  leviCivitaFromTangents :: VecN d -> VecN d -> Maybe (SO d)
  preservesMetricSO ::
    DiscreteMetricTensor d ->
    DiscreteMetricTensor d ->
    SO d ->
    Bool

mkRotation2 :: Double -> Rotation2
mkRotation2 =
  Rotation2 . normalizeAngle

mkRotation3 :: UnitQuaternion -> Rotation3
mkRotation3 =
  Rotation3 . normalizeUnitQuaternion

rotation3FromAxisAngle :: Vec3 -> Double -> Rotation3
rotation3FromAxisAngle axisValue angleValue =
  let axisUnit = normalizeVec3Safe axisValue
      halfAngle = 0.5 * angleValue
      sinHalfAngle = sin halfAngle
   in if magnitudeVec3 axisUnit <= epsilon || abs angleValue <= epsilon
        then mkRotation3 identityQuaternion
        else
          mkRotation3
            ( UnitQuaternion
                (cos halfAngle)
                (sinHalfAngle * vecX axisUnit)
                (sinHalfAngle * vecY axisUnit)
                (sinHalfAngle * vecZ axisUnit)
            )

rotation3AxisAngle :: Rotation3 -> (Vec3, Double)
rotation3AxisAngle (Rotation3 quaternionValue) =
  let UnitQuaternion wValue xValue yValue zValue =
        normalizeUnitQuaternion quaternionValue
      clampedW = clampToUnit wValue
      angleValue = 2.0 * acos clampedW
      sinHalfAngle = sqrt (max 0.0 (1.0 - clampedW * clampedW))
   in if sinHalfAngle <= epsilon
        then (Vec3 1.0 0.0 0.0, 0.0)
        else
          ( Vec3
              (xValue / sinHalfAngle)
              (yValue / sinHalfAngle)
              (zValue / sinHalfAngle),
            angleValue
          )

composeConnection ::

  SpecialOrthogonal d =>
  DiscreteConnection d ->
  DiscreteConnection d ->
  DiscreteConnection d
composeConnection leftConnection rightConnection =
  composeSO leftConnection rightConnection

inverseConnection ::

  SpecialOrthogonal d =>
  DiscreteConnection d ->
  DiscreteConnection d
inverseConnection connectionValue =
  inverseSO connectionValue

composeConnectionPath ::

  SpecialOrthogonal d =>
  [DiscreteConnection d] ->
  DiscreteConnection d
composeConnectionPath =
  foldl'
    (\composedConnection nextConnection -> composeSO nextConnection composedConnection)
    (identitySO)

parallelTransport ::

  SpecialOrthogonal d =>
  DiscreteConnection d ->
  VecN d ->
  VecN d
parallelTransport connectionValue vectorValue =
  applySO connectionValue vectorValue

leviCivitaConnectionFromTangents ::

  SpecialOrthogonal d =>
  VecN d ->
  VecN d ->
  Maybe (DiscreteConnection d)
leviCivitaConnectionFromTangents sourceTangent targetTangent =
  leviCivitaFromTangents sourceTangent targetTangent

metricCompatibleConnection ::

  SpecialOrthogonal d =>
  DiscreteMetricTensor d ->
  DiscreteMetricTensor d ->
  DiscreteConnection d ->
  Bool
metricCompatibleConnection sourceMetric targetMetric connectionValue =
  preservesMetricSO sourceMetric targetMetric connectionValue

instance SpecialOrthogonal 'D2 where
  identitySO = mkRotation2 0.0

  composeSO leftRotation rightRotation =
    mkRotation2
      (rotation2Angle leftRotation + rotation2Angle rightRotation)

  inverseSO rotationValue =
    mkRotation2 (negate (rotation2Angle rotationValue))

  applySO rotationValue (Vec2 xValue yValue) =
    let angleValue = rotation2Angle rotationValue
        cosineValue = cos angleValue
        sineValue = sin angleValue
     in Vec2
          (cosineValue * xValue - sineValue * yValue)
          (sineValue * xValue + cosineValue * yValue)

  leviCivitaFromTangents sourceTangent targetTangent
    | magnitudeVec2 sourceTangent <= epsilon = Nothing
    | magnitudeVec2 targetTangent <= epsilon = Nothing
    | otherwise =
        let sourceUnit = normalizeVec2Safe sourceTangent
            targetUnit = normalizeVec2Safe targetTangent
            angleValue =
              atan2
                (crossZVec2 sourceUnit targetUnit)
                (dotVec2 sourceUnit targetUnit)
         in Just (mkRotation2 angleValue)

  preservesMetricSO sourceMetric targetMetric connectionValue =
    metricTensor2RelApproxEq
      sourceMetric
      (pullbackMetricTensor2 targetMetric connectionValue)

instance SpecialOrthogonal 'D3 where
  identitySO = mkRotation3 identityQuaternion

  composeSO leftRotation rightRotation =
    mkRotation3
      ( unitQuaternionMultiply
          (rotation3Quaternion leftRotation)
          (rotation3Quaternion rightRotation)
      )

  inverseSO rotationValue =
    mkRotation3
      (unitQuaternionConjugate (rotation3Quaternion rotationValue))

  applySO rotationValue =
    rotateVec3ByQuaternion (rotation3Quaternion rotationValue)

  leviCivitaFromTangents sourceTangent targetTangent
    | magnitudeVec3 sourceTangent <= epsilon = Nothing
    | magnitudeVec3 targetTangent <= epsilon = Nothing
    | otherwise =
        let sourceUnit = normalizeVec3Safe sourceTangent
            targetUnit = normalizeVec3Safe targetTangent
            cosineValue = clampToUnit (dotVec3 sourceUnit targetUnit)
         in if cosineValue >= 1.0 - epsilon
              then Just (mkRotation3 identityQuaternion)
              else
                if cosineValue <= -1.0 + epsilon
                  then
                    Just
                      (rotation3FromAxisAngle (orthogonalAxisFor sourceUnit) pi)
                  else
                    let axisValue =
                          normalizeVec3Safe (crossVec3 sourceUnit targetUnit)
                        angleValue = acos cosineValue
                     in Just (rotation3FromAxisAngle axisValue angleValue)

  preservesMetricSO sourceMetric targetMetric connectionValue =
    metricTensor3RelApproxEq
      sourceMetric
      (pullbackMetricTensor3 targetMetric connectionValue)

instance Semigroup Rotation2 where
  (<>) = composeSO 


instance Monoid Rotation2 where
  mempty = identitySO 

instance Group Rotation2 where
  groupInverse = inverseSO 

instance AbelianGroup Rotation2

instance Action Rotation2 Vec2 where
  act = applySO 

instance InvertibleAction Rotation2 Vec2

instance Semigroup Rotation3 where
  (<>) = composeSO 


instance Monoid Rotation3 where
  mempty = identitySO 

instance Group Rotation3 where
  groupInverse = inverseSO 

instance Action Rotation3 Vec3 where
  act = applySO 

instance InvertibleAction Rotation3 Vec3

pullbackMetricTensor2 :: Symmetric2 Double -> Rotation2 -> Symmetric2 Double
pullbackMetricTensor2 targetMetric rotationValue =
  let basisX = Vec2 1.0 0.0
      basisY = Vec2 0.0 1.0
      rotatedBasisX = applySO  rotationValue basisX
      rotatedBasisY = applySO  rotationValue basisY
   in Symmetric2
        { sym2XX = dotVec2 rotatedBasisX (applySymmetric2 targetMetric rotatedBasisX),
          sym2XY = dotVec2 rotatedBasisX (applySymmetric2 targetMetric rotatedBasisY),
          sym2YY = dotVec2 rotatedBasisY (applySymmetric2 targetMetric rotatedBasisY)
        }

pullbackMetricTensor3 :: Symmetric3 Double -> Rotation3 -> Symmetric3 Double
pullbackMetricTensor3 targetMetric rotationValue =
  let basisX = Vec3 1.0 0.0 0.0
      basisY = Vec3 0.0 1.0 0.0
      basisZ = Vec3 0.0 0.0 1.0
      rotatedBasisX = applySO  rotationValue basisX
      rotatedBasisY = applySO  rotationValue basisY
      rotatedBasisZ = applySO  rotationValue basisZ
   in Symmetric3
        { sym3XX = dotVec3 rotatedBasisX (applySymmetric3 targetMetric rotatedBasisX),
          sym3XY = dotVec3 rotatedBasisX (applySymmetric3 targetMetric rotatedBasisY),
          sym3XZ = dotVec3 rotatedBasisX (applySymmetric3 targetMetric rotatedBasisZ),
          sym3YY = dotVec3 rotatedBasisY (applySymmetric3 targetMetric rotatedBasisY),
          sym3YZ = dotVec3 rotatedBasisY (applySymmetric3 targetMetric rotatedBasisZ),
          sym3ZZ = dotVec3 rotatedBasisZ (applySymmetric3 targetMetric rotatedBasisZ)
        }


normalizeAngle :: Double -> Double
normalizeAngle angleValue =
  let fullTurn = 2.0 * pi
      shiftedTurns = fromIntegral (floor ((angleValue + pi) / fullTurn) :: Int)
      wrappedValue = angleValue - fullTurn * shiftedTurns
   in if wrappedValue > pi
        then wrappedValue - fullTurn
        else
          if wrappedValue <= (-pi)
            then wrappedValue + fullTurn
            else wrappedValue

identityQuaternion :: UnitQuaternion
identityQuaternion =
  UnitQuaternion 1.0 0.0 0.0 0.0

normalizeUnitQuaternion :: UnitQuaternion -> UnitQuaternion
normalizeUnitQuaternion quaternionValue =
  let normSquared =
        quatW quaternionValue * quatW quaternionValue
          + quatX quaternionValue * quatX quaternionValue
          + quatY quaternionValue * quatY quaternionValue
          + quatZ quaternionValue * quatZ quaternionValue
   in if normSquared <= epsilon
        then identityQuaternion
        else
          let reciprocalNorm = 1.0 / sqrt normSquared
              normalizedQuaternion =
                UnitQuaternion
                  (reciprocalNorm * quatW quaternionValue)
                  (reciprocalNorm * quatX quaternionValue)
                  (reciprocalNorm * quatY quaternionValue)
                  (reciprocalNorm * quatZ quaternionValue)
           in canonicalizeQuaternionSign normalizedQuaternion

canonicalizeQuaternionSign :: UnitQuaternion -> UnitQuaternion
canonicalizeQuaternionSign quaternionValue
  | shouldFlipQuaternion quaternionValue = negateQuaternion quaternionValue
  | otherwise = quaternionValue

shouldFlipQuaternion :: UnitQuaternion -> Bool
shouldFlipQuaternion quaternionValue =
  let wValue = quatW quaternionValue
      xValue = quatX quaternionValue
      yValue = quatY quaternionValue
      zValue = quatZ quaternionValue
   in wValue < 0.0
        || (wValue == 0.0 && xValue < 0.0)
        || (wValue == 0.0 && xValue == 0.0 && yValue < 0.0)
        || (wValue == 0.0 && xValue == 0.0 && yValue == 0.0 && zValue < 0.0)

negateQuaternion :: UnitQuaternion -> UnitQuaternion
negateQuaternion quaternionValue =
  UnitQuaternion
    (negate (quatW quaternionValue))
    (negate (quatX quaternionValue))
    (negate (quatY quaternionValue))
    (negate (quatZ quaternionValue))

unitQuaternionConjugate :: UnitQuaternion -> UnitQuaternion
unitQuaternionConjugate quaternionValue =
  UnitQuaternion
    (quatW quaternionValue)
    (negate (quatX quaternionValue))
    (negate (quatY quaternionValue))
    (negate (quatZ quaternionValue))

unitQuaternionMultiply :: UnitQuaternion -> UnitQuaternion -> UnitQuaternion
unitQuaternionMultiply leftQuaternion rightQuaternion =
  let w1 = quatW leftQuaternion
      x1 = quatX leftQuaternion
      y1 = quatY leftQuaternion
      z1 = quatZ leftQuaternion
      w2 = quatW rightQuaternion
      x2 = quatX rightQuaternion
      y2 = quatY rightQuaternion
      z2 = quatZ rightQuaternion
   in UnitQuaternion
        (w1 * w2 - x1 * x2 - y1 * y2 - z1 * z2)
        (w1 * x2 + x1 * w2 + y1 * z2 - z1 * y2)
        (w1 * y2 - x1 * z2 + y1 * w2 + z1 * x2)
        (w1 * z2 + x1 * y2 - y1 * x2 + z1 * w2)

rotateVec3ByQuaternion :: UnitQuaternion -> Vec3 -> Vec3
rotateVec3ByQuaternion quaternionValue vectorValue =
  let UnitQuaternion wValue xValue yValue zValue =
        normalizeUnitQuaternion quaternionValue
      quaternionVector = Vec3 xValue yValue zValue
      tValue = scaleVec3 2.0 (crossVec3 quaternionVector vectorValue)
   in addVec3
        vectorValue
        ( addVec3
            (scaleVec3 wValue tValue)
            (crossVec3 quaternionVector tValue)
        )

orthogonalAxisFor :: Vec3 -> Vec3
orthogonalAxisFor vectorValue =
  let xAxis = Vec3 1.0 0.0 0.0
      yAxis = Vec3 0.0 1.0 0.0
      candidateAxisX = crossVec3 vectorValue xAxis
   in if magnitudeVec3 candidateAxisX > epsilon
        then normalizeVec3Safe candidateAxisX
        else normalizeVec3Safe (crossVec3 vectorValue yAxis)

clampToUnit :: Double -> Double
clampToUnit value =
  max (-1.0) (min 1.0 value)
