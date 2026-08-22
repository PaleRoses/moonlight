{-# LANGUAGE EmptyDataDeriving #-}

-- | The admitted Moonlight triangulation cell section as a generic
-- 'CellComplex2D'. The 'ExactCellSet' remains the semantic owner: this module
-- only supplies the incidence interpretation required by downstream topology.
module Moonlight.Triangulation.CellComplex
  ( DCELComplex,
    DCELError,
    fromExactCellSet,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Moonlight.Algebra.Pure.Orientation (Orientation (..))
import Moonlight.Homology.Pure.Topology.CellComplex
  ( CellComplex2D (..),
    CellTypes (..),
    OrientedEdge (..),
    ValidateComplex2D (..),
  )
import Moonlight.Triangulation.Dcel qualified as Dcel
import Moonlight.Triangulation.Handles.HandleDefs
  ( FaceId (..),
    UndirectedEdgeId (..),
    VertexId (..),
    asUndirected,
    directedPair,
    isNormalized,
  )
import Moonlight.Triangulation.Internal.CellSet (ExactCellSet (..))
import Moonlight.Triangulation.Types (Triangulation)

-- | An incidence view of one already-validated, downward-closed exact cell
-- selection. It deliberately has no independent cell inventory.
newtype DCELComplex = DCELComplex ExactCellSet

-- | 'ExactCellSet' construction discharges every closure and handle
-- obligation before this view exists, so there are no residual validation
-- failures for the adapter to manufacture.
data DCELError
  deriving stock (Eq, Show)

fromExactCellSet :: ExactCellSet -> DCELComplex
fromExactCellSet = DCELComplex

instance CellTypes DCELComplex where
  type Vertex DCELComplex = VertexId
  type Edge DCELComplex = UndirectedEdgeId
  type Face DCELComplex = FaceId

instance CellComplex2D DCELComplex where
  vertices (DCELComplex (ExactCellSet _ selectedVertices _ _)) =
    fmap (VertexId . fromIntegral) (IntMap.keys selectedVertices)

  edges (DCELComplex (ExactCellSet _ _ selectedEdges _)) =
    fmap (UndirectedEdgeId . fromIntegral) (IntSet.toAscList selectedEdges)

  faces (DCELComplex (ExactCellSet _ _ _ selectedFaces)) =
    fmap (FaceId . fromIntegral) (IntSet.toAscList selectedFaces)

  edgeBoundary (DCELComplex (ExactCellSet triangulation _ _ _)) =
    Dcel.undirectedEndpoints triangulation

  faceBoundary (DCELComplex (ExactCellSet triangulation _ _ _)) face =
    fmap orientedBoundaryEdge (Dcel.faceDirectedEdges triangulation face)
    where
      orientedBoundaryEdge directedEdge =
        OrientedEdge
          { orientedEdge = asUndirected directedEdge,
            edgeOrientation =
              if isNormalized directedEdge
                then Positive
                else Negative
          }

  edgesAtVertex complexValue@(DCELComplex (ExactCellSet triangulation _ _ _)) vertex =
    filter (edgeContainsVertex triangulation vertex) (edges complexValue)

  facesAtEdge (DCELComplex (ExactCellSet triangulation _ _ selectedFaces)) edge =
    let (forward, backward) = directedPair edge
        selectedIncidentFace directedEdge =
          let face@(FaceId rawFace) = Dcel.incidentFace triangulation directedEdge
           in if IntSet.member (fromIntegral rawFace) selectedFaces
                then Just face
                else Nothing
     in (selectedIncidentFace forward, selectedIncidentFace backward)

instance ValidateComplex2D DCELComplex where
  type ValidationIssue DCELComplex = DCELError
  validateComplex _ = []

edgeContainsVertex ::
  Triangulation mode vertex directed undirected face ->
  VertexId ->
  UndirectedEdgeId ->
  Bool
edgeContainsVertex triangulation vertex edge =
  let (sourceVertex, targetVertex) = Dcel.undirectedEndpoints triangulation edge
   in vertex == sourceVertex || vertex == targetVertex
