{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Moonlight.Geometry.Discrete.Holonomy
  ( HolonomyError (..),
    boundaryHolonomy,
    faceHolonomyFromLookup,
    faceHolonomy,
    allFaceHolonomies,
    holonomyAngle2,
    gaussianCurvatureFromHolonomy2,
    holonomyAxisAngle3,
    isFlatHolonomy2,
    isFlatHolonomy3,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Geometry.Discrete
import Moonlight.LinAlg.Geometry (Vec3)

type HolonomyError :: Type -> Type -> Type
data HolonomyError edge face
  = MissingEdgeConnection !edge
  | FaceBoundaryDisconnected !face
  | FaceBoundaryNotClosed !face
  deriving stock (Eq, Show)

boundaryHolonomy ::
  SpecialOrthogonal d =>
  [SO d] ->
  SO d
boundaryHolonomy = composeConnectionPath

faceHolonomyFromLookup ::
  (CellComplex c, Eq (Vertex c), SpecialOrthogonal d) =>
  c ->
  (Edge c -> Maybe (SO d)) ->
  Face c ->
  Either (HolonomyError (Edge c) (Face c)) (SO d)
faceHolonomyFromLookup complexValue lookupConnection faceValue = do
  validateBoundary complexValue faceValue
  adjustedBoundaryConnections <-
    traverse
      ( \(edgeValue, orientationValue) ->
          case lookupConnection edgeValue of
            Just connectionValue ->
              Right (orientConnection orientationValue connectionValue)
            Nothing ->
              Left (MissingEdgeConnection edgeValue)
      )
      (faceBoundary complexValue faceValue)
  pure (boundaryHolonomy adjustedBoundaryConnections)

faceHolonomy ::
  (CellComplex c, Eq (Vertex c), Ord (Edge c), SpecialOrthogonal d) =>
  c ->
  Map (Edge c) (SO d) ->
  Face c ->
  Either (HolonomyError (Edge c) (Face c)) (SO d)
faceHolonomy complexValue edgeConnections =
  faceHolonomyFromLookup complexValue (`Map.lookup` edgeConnections)

allFaceHolonomies ::
  (CellComplex c, Eq (Vertex c), Ord (Edge c), Ord (Face c), SpecialOrthogonal d) =>
  c ->
  Map (Edge c) (SO d) ->
  Either (HolonomyError (Edge c) (Face c)) (Map (Face c) (SO d))
allFaceHolonomies complexValue edgeConnections =
  fmap Map.fromList
    ( traverse
        ( \faceValue ->
            fmap
              (\holonomyValue -> (faceValue, holonomyValue))
              (faceHolonomy complexValue edgeConnections faceValue)
        )
        (faces complexValue)
    )

holonomyAngle2 :: SO 'D2 -> Double
holonomyAngle2 = rotation2Angle

gaussianCurvatureFromHolonomy2 :: SO 'D2 -> Double
gaussianCurvatureFromHolonomy2 = holonomyAngle2

holonomyAxisAngle3 :: SO 'D3 -> (Vec3, Double)
holonomyAxisAngle3 = rotation3AxisAngle

isFlatHolonomy2 :: Double -> SO 'D2 -> Bool
isFlatHolonomy2 toleranceValue holonomyValue =
  abs (holonomyAngle2 holonomyValue) <= toleranceValue

isFlatHolonomy3 :: Double -> SO 'D3 -> Bool
isFlatHolonomy3 toleranceValue holonomyValue =
  let (_, angleValue) = holonomyAxisAngle3 holonomyValue
   in angleValue <= toleranceValue

orientConnection ::
  SpecialOrthogonal d =>
  Orientation ->
  SO d ->
  SO d
orientConnection orientationValue connectionValue =
  case orientationValue of
    Positive -> connectionValue
    Negative -> inverseConnection connectionValue

validateBoundary ::
  (CellComplex c, Eq (Vertex c)) =>
  c ->
  Face c ->
  Either (HolonomyError (Edge c) (Face c)) ()
validateBoundary complexValue faceValue =
  case fmap (\(edgeValue, orientationValue) -> edgeBoundaryWithOrientation complexValue orientationValue edgeValue) (faceBoundary complexValue faceValue) of
    [] ->
      Right ()
    (startVertex, nextVertex) : remainingSegments ->
      go startVertex nextVertex remainingSegments
  where
    go firstVertex currentVertex remainingSegments =
      case remainingSegments of
        [] ->
          if currentVertex == firstVertex
            then Right ()
            else Left (FaceBoundaryNotClosed faceValue)
        (segmentStart, segmentEnd) : restSegments ->
          if segmentStart == currentVertex
            then go firstVertex segmentEnd restSegments
            else Left (FaceBoundaryDisconnected faceValue)
