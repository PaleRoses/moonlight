{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Geometry.Discrete
  ( module MetricX,
    module ConnectionX,
    Orientation (..),
    flipOrientation,
    CellComplex (..),
    VertexField,
    EdgeField,
    FaceField,
    DiscreteHolonomy,
    DiscreteGeometry (..),
    DiscreteGeometry2,
    DiscreteGeometry3,
    lookupVertexMetric,
    lookupEdgeLength,
    lookupEdgeConnection,
    lookupFaceHolonomy,
    edgeBoundaryWithOrientation,
    faceBoundaryEdges,
    faceVertices,
    faceBoundaryClosed,
    faceContainsVertex,
    incidentFaces,
    eulerCharacteristic2,
    vertexValence,
  )
where

import Data.Kind (Constraint, Type)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Void (Void)
import Moonlight.Algebra (Orientation (..), flipOrientation)
import Moonlight.Geometry.Discrete.Connection as ConnectionX
import Moonlight.Geometry.Discrete.Metric as MetricX
import Moonlight.Homology.Topology
  ( Graph1Skeleton (..),
    GraphEdge (..),
    RawCellData (..),
  )

type CellComplex :: Type -> Constraint
class CellComplex c where
  type Vertex c
  type Edge c
  type Face c

  vertices :: c -> [Vertex c]
  edges :: c -> [Edge c]
  faces :: c -> [Face c]

  edgeBoundary :: c -> Edge c -> (Vertex c, Vertex c)

  faceBoundary :: c -> Face c -> [(Edge c, Orientation)]

  starOf :: c -> Vertex c -> [Edge c]

type VertexField :: Type -> Type -> Type
type VertexField c a = Map (Vertex c) a

type EdgeField :: Type -> Type -> Type
type EdgeField c a = Map (Edge c) a

type FaceField :: Type -> Type -> Type
type FaceField c a = Map (Face c) a

type DiscreteHolonomy :: Dim -> Type
type DiscreteHolonomy d = DiscreteConnection d

type DiscreteGeometry :: Type -> Dim -> Type
data DiscreteGeometry c (d :: Dim) = DiscreteGeometry
  { dgComplex :: !c,
    dgVertexMetrics :: !(VertexField c (DiscreteMetricTensor d)),
    dgEdgeLengths :: !(EdgeField c Double),
    dgEdgeConnections :: !(EdgeField c (DiscreteConnection d)),
    dgFaceHolonomies :: !(FaceField c (DiscreteHolonomy d))
  }

type DiscreteGeometry2 :: Type -> Type
type DiscreteGeometry2 c = DiscreteGeometry c 'D2

type DiscreteGeometry3 :: Type -> Type
type DiscreteGeometry3 c = DiscreteGeometry c 'D3

lookupVertexMetric ::
  Ord (Vertex c) =>
  Vertex c ->
  DiscreteGeometry c d ->
  Maybe (DiscreteMetricTensor d)
lookupVertexMetric vertexValue geometryValue =
  Map.lookup vertexValue (dgVertexMetrics geometryValue)

lookupEdgeLength ::
  Ord (Edge c) =>
  Edge c ->
  DiscreteGeometry c d ->
  Maybe Double
lookupEdgeLength edgeValue geometryValue =
  Map.lookup edgeValue (dgEdgeLengths geometryValue)

lookupEdgeConnection ::
  Ord (Edge c) =>
  Edge c ->
  DiscreteGeometry c d ->
  Maybe (DiscreteConnection d)
lookupEdgeConnection edgeValue geometryValue =
  Map.lookup edgeValue (dgEdgeConnections geometryValue)

lookupFaceHolonomy ::
  Ord (Face c) =>
  Face c ->
  DiscreteGeometry c d ->
  Maybe (DiscreteHolonomy d)
lookupFaceHolonomy faceValue geometryValue =
  Map.lookup faceValue (dgFaceHolonomies geometryValue)

edgeBoundaryWithOrientation ::
  CellComplex c =>
  c ->
  Orientation ->
  Edge c ->
  (Vertex c, Vertex c)
edgeBoundaryWithOrientation complexValue orientationValue edgeValue =
  case orientationValue of
    Positive ->
      edgeBoundary complexValue edgeValue
    Negative ->
      let (sourceVertex, targetVertex) = edgeBoundary complexValue edgeValue
       in (targetVertex, sourceVertex)

faceBoundaryEdges ::
  CellComplex c =>
  c ->
  Face c ->
  [Edge c]
faceBoundaryEdges complexValue faceValue =
  fmap fst (faceBoundary complexValue faceValue)

faceVertices ::
  (CellComplex c, Eq (Vertex c)) =>
  c ->
  Face c ->
  Maybe [Vertex c]
faceVertices complexValue faceValue =
  case fmap (\(edgeValue, orientationValue) -> edgeBoundaryWithOrientation complexValue orientationValue edgeValue) (faceBoundary complexValue faceValue) of
    [] ->
      Just []
    (startVertex, nextVertex) : remainingSegments ->
      go startVertex nextVertex [startVertex] remainingSegments
  where
    go :: Eq vertex => vertex -> vertex -> [vertex] -> [(vertex, vertex)] -> Maybe [vertex]
    go firstVertex currentVertex accumulatedVertices remainingSegments =
      case remainingSegments of
        [] ->
          if currentVertex == firstVertex
            then Just accumulatedVertices
            else Nothing
        (segmentStart, segmentEnd) : restSegments ->
          if segmentStart == currentVertex
            then go firstVertex segmentEnd (accumulatedVertices <> [currentVertex]) restSegments
            else Nothing

faceBoundaryClosed ::
  (CellComplex c, Eq (Vertex c)) =>
  c ->
  Face c ->
  Bool
faceBoundaryClosed complexValue faceValue =
  case faceVertices complexValue faceValue of
    Just _ -> True
    Nothing -> False

faceContainsVertex ::
  (CellComplex c, Eq (Vertex c)) =>
  c ->
  Vertex c ->
  Face c ->
  Bool
faceContainsVertex complexValue vertexValue faceValue =
  any
    ( \edgeValue ->
        let (sourceVertex, targetVertex) = edgeBoundary complexValue edgeValue
         in sourceVertex == vertexValue || targetVertex == vertexValue
    )
    (faceBoundaryEdges complexValue faceValue)

incidentFaces ::
  (CellComplex c, Eq (Vertex c)) =>
  c ->
  Vertex c ->
  [Face c]
incidentFaces complexValue vertexValue =
  filter (faceContainsVertex complexValue vertexValue) (faces complexValue)

eulerCharacteristic2 :: CellComplex c => c -> Int
eulerCharacteristic2 complexValue =
  length (vertices complexValue)
    - length (edges complexValue)
    + length (faces complexValue)

vertexValence :: CellComplex c => c -> Vertex c -> Int
vertexValence complexValue =
  length . starOf complexValue

instance CellComplex Graph1Skeleton where
  type Vertex Graph1Skeleton = Int
  type Edge Graph1Skeleton = Int
  type Face Graph1Skeleton = Void

  vertices skeletonValue =
    [0 .. graphVertexCount skeletonValue - 1]

  edges skeletonValue =
    fmap graphEdgeIndex (graphEdges skeletonValue)

  faces _ = []

  edgeBoundary skeletonValue edgeValue =
    case find ((== edgeValue) . graphEdgeIndex) (graphEdges skeletonValue) of
      Just graphEdgeValue ->
        (graphEdgeSource graphEdgeValue, graphEdgeTarget graphEdgeValue)
      Nothing ->
        error ("Graph1Skeleton.edgeBoundary: unknown edge id " <> show edgeValue)

  faceBoundary _ faceValue =
    case faceValue of {}

  starOf skeletonValue vertexValue =
    fmap graphEdgeIndex
      ( filter
          (\edgeValue -> graphEdgeSource edgeValue == vertexValue || graphEdgeTarget edgeValue == vertexValue)
          (graphEdges skeletonValue)
      )

instance CellComplex RawCellData where
  type Vertex RawCellData = Int
  type Edge RawCellData = Int
  type Face RawCellData = Int

  vertices = rawVertices

  edges rawCellData =
    fmap (\(edgeValue, _, _) -> edgeValue) (rawEdges rawCellData)

  faces rawCellData =
    fmap fst (rawFaces rawCellData)

  edgeBoundary rawCellData edgeValue =
    case find (\(candidateEdge, _, _) -> candidateEdge == edgeValue) (rawEdges rawCellData) of
      Just (_, sourceVertex, targetVertex) ->
        (sourceVertex, targetVertex)
      Nothing ->
        error ("RawCellData.edgeBoundary: unknown edge id " <> show edgeValue)

  faceBoundary rawCellData faceValue =
    case lookup faceValue (rawFaces rawCellData) of
      Just boundaryValue ->
        boundaryValue
      Nothing ->
        error ("RawCellData.faceBoundary: unknown face id " <> show faceValue)

  starOf rawCellData vertexValue =
    fmap
      (\(edgeValue, _, _) -> edgeValue)
      ( filter
          (\(_, sourceVertex, targetVertex) -> sourceVertex == vertexValue || targetVertex == vertexValue)
          (rawEdges rawCellData)
      )
