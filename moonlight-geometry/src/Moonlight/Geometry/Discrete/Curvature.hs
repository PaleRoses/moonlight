{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Geometry.Discrete.Curvature
  ( GaussianCurvature,
    AngleDefect,
    GaussBonnetReport (..),
    faceGaussianCurvature2,
    faceAngleDefect2,
    faceGaussianCurvatures2,
    totalGaussianCurvature2,
    verifyGaussBonnet2,
    verifyGeometryGaussBonnet2,
    verifyConnectionGaussBonnet2,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Geometry.Discrete
  ( CellComplex (..),
    DiscreteGeometry (..),
    DiscreteHolonomy,
    eulerCharacteristic2,
  )
import Moonlight.Geometry.Discrete.Connection (DiscreteConnection)
import Moonlight.Geometry.Discrete.Holonomy
  ( HolonomyError,
    allFaceHolonomies,
    gaussianCurvatureFromHolonomy2,
  )
import Moonlight.Geometry.Discrete.Metric (Dim (..))
import Prelude

type GaussianCurvature :: Type
type GaussianCurvature = Double

type AngleDefect :: Type
type AngleDefect = Double

type GaussBonnetReport :: Type -> Type
data GaussBonnetReport face = GaussBonnetReport
  { gaussBonnetByFace :: !(Map face GaussianCurvature),
    gaussBonnetTotalCurvature :: !GaussianCurvature,
    gaussBonnetEulerCharacteristic :: !Int,
    gaussBonnetExpectedCurvature :: !GaussianCurvature,
    gaussBonnetResidual :: !Double,
    gaussBonnetSatisfied :: !Bool
  }

faceGaussianCurvature2 :: DiscreteHolonomy 'D2 -> GaussianCurvature
faceGaussianCurvature2 = gaussianCurvatureFromHolonomy2

faceAngleDefect2 :: DiscreteHolonomy 'D2 -> AngleDefect
faceAngleDefect2 = faceGaussianCurvature2

faceGaussianCurvatures2 :: Map face (DiscreteHolonomy 'D2) -> Map face GaussianCurvature
faceGaussianCurvatures2 = Map.map faceGaussianCurvature2

totalGaussianCurvature2 :: Map face (DiscreteHolonomy 'D2) -> GaussianCurvature
totalGaussianCurvature2 =
  sum . Map.elems . faceGaussianCurvatures2

verifyGaussBonnet2 ::
  CellComplex c =>
  Double ->
  c ->
  Map (Face c) (DiscreteHolonomy 'D2) ->
  GaussBonnetReport (Face c)
verifyGaussBonnet2 toleranceValue complexValue faceHolonomyMap =
  let byFace = faceGaussianCurvatures2 faceHolonomyMap
      totalCurvature = sum (Map.elems byFace)
      eulerValue = eulerCharacteristic2 complexValue
      expectedCurvature = 2.0 * pi * fromIntegral eulerValue
      residualValue = totalCurvature - expectedCurvature
   in GaussBonnetReport
        { gaussBonnetByFace = byFace,
          gaussBonnetTotalCurvature = totalCurvature,
          gaussBonnetEulerCharacteristic = eulerValue,
          gaussBonnetExpectedCurvature = expectedCurvature,
          gaussBonnetResidual = residualValue,
          gaussBonnetSatisfied = abs residualValue <= toleranceValue
        }

verifyGeometryGaussBonnet2 ::
  CellComplex c =>
  Double ->
  DiscreteGeometry c 'D2 ->
  GaussBonnetReport (Face c)
verifyGeometryGaussBonnet2 toleranceValue geometryValue =
  verifyGaussBonnet2
    toleranceValue
    (dgComplex geometryValue)
    (dgFaceHolonomies geometryValue)

verifyConnectionGaussBonnet2 ::
  (CellComplex c, Eq (Vertex c), Ord (Edge c), Ord (Face c)) =>
  Double ->
  c ->
  Map (Edge c) (DiscreteConnection 'D2) ->
  Either (HolonomyError (Edge c) (Face c)) (GaussBonnetReport (Face c))
verifyConnectionGaussBonnet2 toleranceValue complexValue edgeConnections =
  fmap
    (verifyGaussBonnet2 toleranceValue complexValue)
    (allFaceHolonomies complexValue edgeConnections)
