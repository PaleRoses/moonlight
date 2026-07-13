{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Geometry.Discrete.Surface
  ( FaceCorner (..),
    orientedEdgeEndpoints,
    otherEndpointOnEdge,
    faceVertexCycle,
    faceCorners,
    faceCornersAtVertex,
  )
where

import Moonlight.Algebra (Orientation (..))
import Moonlight.Core (MoonlightError (..))
import Data.Kind (Type)
import Moonlight.Homology.Topology
  ( CellComplex2D (..),
    CellTypes (..),
    OrientedEdge (..),
  )

type FaceCorner :: Type -> Type -> Type
data FaceCorner vertex face = FaceCorner
  { faceCornerFace :: !face,
    faceCornerVertex :: !vertex,
    faceCornerPrevVertex :: !vertex,
    faceCornerNextVertex :: !vertex
  }
  deriving stock (Eq, Show)

orientedEdgeEndpoints :: CellComplex2D c => c -> OrientedEdge (Edge c) -> (Vertex c, Vertex c)
orientedEdgeEndpoints complex orientedBoundaryEdge =
  case edgeOrientation orientedBoundaryEdge of
    Positive -> edgeBoundary complex (orientedEdge orientedBoundaryEdge)
    Negative ->
      let (sourceVertex, targetVertex) = edgeBoundary complex (orientedEdge orientedBoundaryEdge)
       in (targetVertex, sourceVertex)

otherEndpointOnEdge :: CellComplex2D c => c -> Vertex c -> Edge c -> Maybe (Vertex c)
otherEndpointOnEdge complex vertexValue edgeValue =
  let (sourceVertex, targetVertex) = edgeBoundary complex edgeValue
   in if vertexValue == sourceVertex
        then Just targetVertex
        else
          if vertexValue == targetVertex
            then Just sourceVertex
            else Nothing

rotateLeft :: [a] -> [a]
rotateLeft [] = []
rotateLeft (firstValue : restValues) = restValues ++ [firstValue]

rotateRight :: [a] -> [a]
rotateRight [] = []
rotateRight [value] = [value]
rotateRight values =
  case reverse values of
    [] -> []
    lastValue : reversedInit -> lastValue : reverse reversedInit

faceVertexCycle :: CellComplex2D c => c -> Face c -> Either MoonlightError [Vertex c]
faceVertexCycle complex faceValue =
  case fmap (orientedEdgeEndpoints complex) (faceBoundary complex faceValue) of
    [] ->
      Left (InvariantViolation "discrete surface face boundary must be non-empty")
    endpointPairs ->
      let sourceVertices = fmap fst endpointPairs
          targetVertices = fmap snd endpointPairs
          expectedSources = rotateLeft sourceVertices
       in if and (zipWith (==) targetVertices expectedSources)
            then Right sourceVertices
            else Left (InvariantViolation "discrete surface face boundary is not a coherent cyclic walk")

faceCorners :: CellComplex2D c => c -> Face c -> Either MoonlightError [FaceCorner (Vertex c) (Face c)]
faceCorners complex faceValue = do
  vertexCycle <- faceVertexCycle complex faceValue
  case vertexCycle of
    [] ->
      Right []
    _ ->
      let previousVertices = rotateRight vertexCycle
          nextVertices = rotateLeft vertexCycle
       in Right
            ( zipWith3
                ( \vertexValue previousVertex nextVertex ->
                    FaceCorner
                      { faceCornerFace = faceValue,
                        faceCornerVertex = vertexValue,
                        faceCornerPrevVertex = previousVertex,
                        faceCornerNextVertex = nextVertex
                      }
                )
                vertexCycle
                previousVertices
                nextVertices
            )

faceCornersAtVertex :: CellComplex2D c => c -> Vertex c -> Either MoonlightError [FaceCorner (Vertex c) (Face c)]
faceCornersAtVertex complex vertexValue =
  fmap
    (filter (\cornerValue -> faceCornerVertex cornerValue == vertexValue))
    (traverse (faceCorners complex) (faces complex) >>= pure . concat)
