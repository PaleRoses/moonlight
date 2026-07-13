{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Geometry.Discrete.Metric
  ( Dim (..),
    VecN,
    MetricTensor2,
    MetricTensor3,
    DiscreteMetricTensor,
    SPD (..),
    PrincipalDecomposition3,
    metricTensor2Determinant,
    metricTensor2InnerProduct,
    metricTensor2NormSquared,
    metricTensor3InnerProduct,
    metricTensor3NormSquared,
    metricTensor2ApproxEq,
    metricTensor3ApproxEq,
    metricTensor2RelApproxEq,
    metricTensor3RelApproxEq,
    isSPDMetricTensor2,
    isSPDMetricTensor3,
    mkSPDMetricTensor2,
    mkSPDMetricTensor3,
    spectralMapMetricTensor2,
    spectralMapMetricTensor3,
    logEuclideanMeanMetricTensor2,
    logEuclideanMeanMetricTensor3,
    principalMetricTensor3,
    metricTensor3FromPrincipal,
    outerProductMetricTensor2,
    outerProductMetricTensor3,
    Eigensystem2Axes (..),
    eigensystemMetricTensor2,
    relClose,
    epsilon,
  )
where

import Data.List (foldl')
import Data.Kind (Type)
import Moonlight.Core (MoonlightError (..))
import Moonlight.LinAlg.Geometry
  ( OrthonormalFrame,
    orthonormalFrameColumns,
  )
import Moonlight.LinAlg.Geometry
  ( DiagonalizedSymmetric2 (..),
    DiagonalizedSymmetric3 (..),
    Symmetric2 (..),
    Symmetric3 (..),
    diagonalSymmetric2,
    diagonalSymmetric3,
    eigendecomposeSymmetric2With,
    eigendecomposeSymmetric3OrthonormalFrame,
    applySymmetric2,
    applySymmetric3,
    outerSymmetric2,
    outerSymmetric3,
    scaleSymmetric2,
    scaleSymmetric3,
    zipSymmetric2With,
    zipSymmetric3With,
  )
import Moonlight.LinAlg.Geometry (Vec2 (..), dotVec2)
import Moonlight.LinAlg.Geometry (Vec3 (..), dotVec3)
import Prelude
  ( Bool (..),
    Double,
    Either (..),
    Eq,
    Maybe (..),
    Ord,
    Read,
    Show,
    abs,
    exp,
    fromIntegral,
    length,
    log,
    map,
    max,
    otherwise,
    pure,
    reverse,
    (*),
    (+),
    (-),
    (/),
    (<=),
    (>),
    (&&),
    (.),
  )

type Dim :: Type
data Dim
  = D2
  | D3
  deriving stock (Eq, Ord, Show, Read)

type VecN :: Dim -> Type
type family VecN (d :: Dim) where
  VecN 'D2 = Vec2
  VecN 'D3 = Vec3

type MetricTensor2 :: Type
type MetricTensor2 = Symmetric2 Double

type MetricTensor3 :: Type
type MetricTensor3 = Symmetric3 Double

type DiscreteMetricTensor :: Dim -> Type
type family DiscreteMetricTensor (d :: Dim) where
  DiscreteMetricTensor 'D2 = MetricTensor2
  DiscreteMetricTensor 'D3 = MetricTensor3

type SPD :: Type -> Type
newtype SPD a = SPD {unSPD :: a}
  deriving stock (Eq, Ord, Show, Read)

type PrincipalDecomposition3 :: Type
type PrincipalDecomposition3 = DiagonalizedSymmetric3 OrthonormalFrame Double

metricTensor2Determinant :: MetricTensor2 -> Double
metricTensor2Determinant tensorValue =
  sym2XX tensorValue * sym2YY tensorValue
    - sym2XY tensorValue * sym2XY tensorValue

metricTensor2InnerProduct :: MetricTensor2 -> Vec2 -> Vec2 -> Double
metricTensor2InnerProduct tensorValue leftVector rightVector =
  dotVec2 leftVector (applySymmetric2 tensorValue rightVector)

metricTensor2NormSquared :: MetricTensor2 -> Vec2 -> Double
metricTensor2NormSquared tensorValue vectorValue =
  metricTensor2InnerProduct tensorValue vectorValue vectorValue

metricTensor3InnerProduct :: MetricTensor3 -> Vec3 -> Vec3 -> Double
metricTensor3InnerProduct tensorValue leftVector rightVector =
  dotVec3 leftVector (applySymmetric3 tensorValue rightVector)

metricTensor3NormSquared :: MetricTensor3 -> Vec3 -> Double
metricTensor3NormSquared tensorValue vectorValue =
  metricTensor3InnerProduct tensorValue vectorValue vectorValue

metricTensor2ApproxEq :: Double -> MetricTensor2 -> MetricTensor2 -> Bool
metricTensor2ApproxEq toleranceValue leftTensor rightTensor =
  let differenceXX = abs (sym2XX leftTensor - sym2XX rightTensor)
      differenceXY = abs (sym2XY leftTensor - sym2XY rightTensor)
      differenceYY = abs (sym2YY leftTensor - sym2YY rightTensor)
   in differenceXX <= toleranceValue
        && differenceXY <= toleranceValue
        && differenceYY <= toleranceValue

metricTensor3ApproxEq :: Double -> MetricTensor3 -> MetricTensor3 -> Bool
metricTensor3ApproxEq toleranceValue leftTensor rightTensor =
  let differenceXX = abs (sym3XX leftTensor - sym3XX rightTensor)
      differenceXY = abs (sym3XY leftTensor - sym3XY rightTensor)
      differenceXZ = abs (sym3XZ leftTensor - sym3XZ rightTensor)
      differenceYY = abs (sym3YY leftTensor - sym3YY rightTensor)
      differenceYZ = abs (sym3YZ leftTensor - sym3YZ rightTensor)
      differenceZZ = abs (sym3ZZ leftTensor - sym3ZZ rightTensor)
   in differenceXX <= toleranceValue
        && differenceXY <= toleranceValue
        && differenceXZ <= toleranceValue
        && differenceYY <= toleranceValue
        && differenceYZ <= toleranceValue
        && differenceZZ <= toleranceValue

metricTensor2RelApproxEq :: MetricTensor2 -> MetricTensor2 -> Bool
metricTensor2RelApproxEq leftTensor rightTensor =
  relClose (sym2XX leftTensor) (sym2XX rightTensor)
    && relClose (sym2XY leftTensor) (sym2XY rightTensor)
    && relClose (sym2YY leftTensor) (sym2YY rightTensor)

metricTensor3RelApproxEq :: MetricTensor3 -> MetricTensor3 -> Bool
metricTensor3RelApproxEq leftTensor rightTensor =
  relClose (sym3XX leftTensor) (sym3XX rightTensor)
    && relClose (sym3XY leftTensor) (sym3XY rightTensor)
    && relClose (sym3XZ leftTensor) (sym3XZ rightTensor)
    && relClose (sym3YY leftTensor) (sym3YY rightTensor)
    && relClose (sym3YZ leftTensor) (sym3YZ rightTensor)
    && relClose (sym3ZZ leftTensor) (sym3ZZ rightTensor)

isSPDMetricTensor2 :: MetricTensor2 -> Bool
isSPDMetricTensor2 tensorValue =
  sym2XX tensorValue > 0.0
    && metricTensor2Determinant tensorValue > 0.0

isSPDMetricTensor3 :: MetricTensor3 -> Bool
isSPDMetricTensor3 tensorValue =
  let leadingMinor1 = sym3XX tensorValue
      leadingMinor2 =
        sym3XX tensorValue * sym3YY tensorValue
          - sym3XY tensorValue * sym3XY tensorValue
      leadingMinor3 =
        sym3XX tensorValue
          * (sym3YY tensorValue * sym3ZZ tensorValue - sym3YZ tensorValue * sym3YZ tensorValue)
          - sym3XY tensorValue
          * (sym3XY tensorValue * sym3ZZ tensorValue - sym3YZ tensorValue * sym3XZ tensorValue)
          + sym3XZ tensorValue
          * (sym3XY tensorValue * sym3YZ tensorValue - sym3YY tensorValue * sym3XZ tensorValue)
   in leadingMinor1 > 0.0
        && leadingMinor2 > 0.0
        && leadingMinor3 > 0.0

mkSPDMetricTensor2 :: MetricTensor2 -> Maybe (SPD MetricTensor2)
mkSPDMetricTensor2 tensorValue
  | isSPDMetricTensor2 tensorValue = Just (SPD tensorValue)
  | otherwise = Nothing

mkSPDMetricTensor3 :: MetricTensor3 -> Maybe (SPD MetricTensor3)
mkSPDMetricTensor3 tensorValue
  | isSPDMetricTensor3 tensorValue = Just (SPD tensorValue)
  | otherwise = Nothing

spectralMapMetricTensor2 ::
  (Double -> Double) ->
  MetricTensor2 ->
  Either MoonlightError MetricTensor2
spectralMapMetricTensor2 spectralFunction tensorValue = do
  decomposition <- eigensystemMetricTensor2 tensorValue
  let mappedLambda1 = spectralFunction (diag2XX decomposition)
      mappedLambda2 = spectralFunction (diag2YY decomposition)
      axesValue = diag2Axes decomposition
      cosAngle = diag2AxesCos axesValue
      sinAngle = diag2AxesSin axesValue
      cos2 = cosAngle * cosAngle
      sin2 = sinAngle * sinAngle
      cosSin = cosAngle * sinAngle
   in pure
        Symmetric2
          { sym2XX = mappedLambda1 * cos2 + mappedLambda2 * sin2,
            sym2XY = (mappedLambda1 - mappedLambda2) * cosSin,
            sym2YY = mappedLambda1 * sin2 + mappedLambda2 * cos2
          }

spectralMapMetricTensor3 ::
  (Double -> Double) ->
  MetricTensor3 ->
  Either MoonlightError MetricTensor3
spectralMapMetricTensor3 spectralFunction tensorValue = do
  decomposition <- eigendecomposeSymmetric3OrthonormalFrame tensorValue
  let mappedEigenvalues =
        Vec3
          (spectralFunction (diag3XX decomposition))
          (spectralFunction (diag3YY decomposition))
          (spectralFunction (diag3ZZ decomposition))
   in pure (metricTensor3FromPrincipal mappedEigenvalues (diag3Axes decomposition))

logEuclideanMeanMetricTensor2 ::
  [SPD MetricTensor2] ->
  Either MoonlightError MetricTensor2
logEuclideanMeanMetricTensor2 spdTensors =
  case spdTensors of
    [] -> Left (InvariantViolation "log-Euclidean mean requires at least one tensor")
    _ ->
      let tensorCount = length spdTensors
          logTensors = map (spectralMapMetricTensor2 log . unSPD) spdTensors
       in case sequenceResults logTensors of
            Left errorValue -> Left errorValue
            Right loggedTensors ->
              let summedTensor =
                    foldl'
                      (zipSymmetric2With (+))
                      (diagonalSymmetric2 0.0 0.0)
                      loggedTensors
                  scaleFactor = 1.0 / fromIntegral tensorCount
                  meanLogTensor = scaleSymmetric2 scaleFactor summedTensor
               in spectralMapMetricTensor2 exp meanLogTensor

logEuclideanMeanMetricTensor3 ::
  [SPD MetricTensor3] ->
  Either MoonlightError MetricTensor3
logEuclideanMeanMetricTensor3 spdTensors =
  case spdTensors of
    [] -> Left (InvariantViolation "log-Euclidean mean requires at least one tensor")
    _ ->
      let tensorCount = length spdTensors
          logTensors = map (spectralMapMetricTensor3 log . unSPD) spdTensors
       in case sequenceResults logTensors of
            Left errorValue -> Left errorValue
            Right loggedTensors ->
              let summedTensor =
                    foldl'
                      (zipSymmetric3With (+))
                      (diagonalSymmetric3 0.0 0.0 0.0)
                      loggedTensors
                  scaleFactor = 1.0 / fromIntegral tensorCount
                  meanLogTensor = scaleSymmetric3 scaleFactor summedTensor
               in spectralMapMetricTensor3 exp meanLogTensor

principalMetricTensor3 ::
  MetricTensor3 ->
  Either MoonlightError PrincipalDecomposition3
principalMetricTensor3 = eigendecomposeSymmetric3OrthonormalFrame

metricTensor3FromPrincipal :: Vec3 -> OrthonormalFrame -> MetricTensor3
metricTensor3FromPrincipal (Vec3 lambda1 lambda2 lambda3) frameValue =
  let (axis1, axis2, axis3) = orthonormalFrameColumns frameValue
      outerProduct1 = scaleSymmetric3 lambda1 (outerSymmetric3 1.0 axis1)
      outerProduct2 = scaleSymmetric3 lambda2 (outerSymmetric3 1.0 axis2)
      outerProduct3 = scaleSymmetric3 lambda3 (outerSymmetric3 1.0 axis3)
   in zipSymmetric3With (+) outerProduct1 (zipSymmetric3With (+) outerProduct2 outerProduct3)

outerProductMetricTensor2 :: Double -> Vec2 -> MetricTensor2
outerProductMetricTensor2 = outerSymmetric2

outerProductMetricTensor3 :: Double -> Vec3 -> MetricTensor3
outerProductMetricTensor3 = outerSymmetric3

type Eigensystem2Axes :: Type
data Eigensystem2Axes = Eigensystem2Axes
  { diag2AxesCos :: !Double,
    diag2AxesSin :: !Double
  }
  deriving stock (Eq, Ord, Show, Read)

eigensystemMetricTensor2 ::
  MetricTensor2 ->
  Either MoonlightError (DiagonalizedSymmetric2 Eigensystem2Axes Double)
eigensystemMetricTensor2 tensorValue =
  let decodeRotation entries =
        case entries of
          cosTheta : _ : sinTheta : _ ->
            Just
              Eigensystem2Axes
                { diag2AxesCos = cosTheta,
                  diag2AxesSin = sinTheta
                }
          _ -> Nothing
      fallbackRotation =
        Eigensystem2Axes
          { diag2AxesCos = 1.0,
            diag2AxesSin = 0.0
          }
   in eigendecomposeSymmetric2With decodeRotation fallbackRotation tensorValue

relClose :: Double -> Double -> Bool
relClose leftValue rightValue =
  abs (leftValue - rightValue)
    <= 1.0e-9 * max 1.0 (max (abs leftValue) (abs rightValue))

epsilon :: Double
epsilon = 1.0e-12

sequenceResults :: [Either MoonlightError a] -> Either MoonlightError [a]
sequenceResults = go []
  where
    go :: [a] -> [Either MoonlightError a] -> Either MoonlightError [a]
    go accumulated remainingValues =
      case remainingValues of
        [] -> Right (reverse accumulated)
        Left errorValue : _ -> Left errorValue
        Right successValue : restValues -> go (successValue : accumulated) restValues
