{-# LANGUAGE BangPatterns #-}

-- | Discharge: the invariants the constructors guarantee, checkable on a value
-- built by any route.
module Moonlight.Triangulation.Validation
  ( validateTopology
  , validateDelaunay
  , validateTriangulation
  , triangulationIsValid
  , faceArea
  , faceMinimumAngleDegrees
  ) where

import Data.List (nub)
import qualified Data.IntSet as IntSet
import Moonlight.Triangulation.Internal.BoxedPaged (boxedPagedLength)
import Moonlight.Triangulation.Internal.BoundaryCycle (orderedPair)
import Moonlight.Triangulation.Internal.Paged (pagedFoldl', pagedLength, pagedUnsafeIndex)
import Moonlight.Triangulation.Dcel
import Moonlight.Triangulation.Handles.HandleDefs
import Moonlight.Triangulation.Handles.Iterators.FixedIterators (allFaces, directedEdges, undirectedEdges, vertices)
import Moonlight.Triangulation.Internal.PackedIndex (noIndex)
import Moonlight.Triangulation.Math
import Moonlight.Triangulation.Internal.Representation
import Moonlight.Triangulation.Internal.Types

-- | Every structural invariant violated, not the first.
validateTopology :: Triangulation mode vertex directed undirected face -> [InvariantViolation]
validateTopology triangulation =
  structuralViolations ++ orientationViolations
 where
  verticesCount = numVertices triangulation
  halfCount = numDirectedEdges triangulation
  edgeCount = numUndirectedEdges triangulation
  facesCount = numFaces triangulation

  -- Geometry descends only after the finite DCEL has glued structurally.
  -- Reading triangle coordinates through malformed links would turn a typed
  -- validation failure into an indexing crash.
  structuralViolations =
    cardinalityViolations
      ++ rangeViolations
      ++ edgeViolations
      ++ faceViolations
      ++ vertexViolations
      ++ eulerViolations

  orientationViolations
    | not (null structuralViolations) = []
    | otherwise =
        [ InnerFaceNotCounterClockwise face
        | face <- allFaces triangulation
        , face /= outerFace
        , Just (first, second, third) <- [innerFaceVertices triangulation face]
        , orient2d
            (vertexPoint triangulation first)
            (vertexPoint triangulation second)
            (vertexPoint triangulation third)
            /= GT
        ]

  cardinalityViolations =
    [ CoordinatePlaneLengthMismatch pointXCount pointYCount
    | pointXCount /= pointYCount
    ]
      ++ [VertexOutgoingLengthMismatch vertexOutCount verticesCount | vertexOutCount /= verticesCount]
      ++ [VertexPayloadLengthMismatch vertexPayloadCount verticesCount | vertexPayloadCount /= verticesCount]
      ++ [TopologyArenaLengthMismatch topologyLength (4 * halfCount) | not halfArraysEqual]
      ++ [DirectedPayloadLengthMismatch directedPayloadCount halfCount | directedPayloadCount /= halfCount]
      ++ [UndirectedPayloadLengthMismatch undirectedPayloadCount edgeCount | undirectedPayloadCount /= edgeCount]
      ++ [DirectedEdgeCountOdd halfCount | odd halfCount]
      ++ [ConstraintLengthMismatch constraintLength edgeCount | constraintLength /= edgeCount]
      ++ [ NonCanonicalConstraintFlag (UndirectedEdgeId (fromIntegral index)) flag
         | index <- [0 .. pagedLength (triConstraint triangulation) - 1]
         , let flag = pagedUnsafeIndex (triConstraint triangulation) index
         , flag /= 0 && flag /= 1
         ]
      ++ [CachedConstraintCountMismatch (triConstraintCount triangulation) actualConstraintCount | triConstraintCount triangulation /= actualConstraintCount]
      ++ [CachedConstraintIndexMismatch | triConstraintEdges triangulation /= indexedConstraintEdges]
      ++ [MissingOuterFace | facesCount == 0]
      ++ [FacePayloadLengthMismatch facePayloadCount facesCount | facePayloadCount /= facesCount]

  pointXCount = pagedLength (triPointX triangulation)
  pointYCount = pagedLength (triPointY triangulation)
  vertexOutCount = pagedLength (triVertexOut triangulation)
  vertexPayloadCount = boxedPagedLength (triVertexData triangulation)
  topologyLength = pagedLength (triHalfTopology triangulation)
  directedPayloadCount = boxedPagedLength (triDirectedData triangulation)
  undirectedPayloadCount = boxedPagedLength (triUndirectedData triangulation)
  constraintLength = pagedLength (triConstraint triangulation)
  facePayloadCount = boxedPagedLength (triFaceData triangulation)
  actualConstraintCount = pagedFoldl' (\count flag -> if flag == 1 then count + 1 else count) 0 (triConstraint triangulation)
  halfArraysEqual = topologyLength == 4 * halfCount

  indexedConstraintEdges =
    IntSet.fromAscList
      [ index
      | index <- [0 .. edgeCount - 1]
      , pagedUnsafeIndex (triConstraint triangulation) index == 1
      ]

  rangeViolations =
    [ EdgeOriginOutOfRange (DirectedEdgeId (fromIntegral index)) (VertexId value) verticesCount
    | index <- [0 .. halfCount - 1]
    , let value = pagedUnsafeIndex (triHalfTopology triangulation) (4 * index)
    , fromIntegral value >= verticesCount
    ]
      ++ [ EdgeNextOutOfRange (DirectedEdgeId (fromIntegral index)) (DirectedEdgeId value) halfCount
         | index <- [0 .. halfCount - 1]
         , let value = pagedUnsafeIndex (triHalfTopology triangulation) (4 * index + 1)
         , fromIntegral value >= halfCount
         ]
      ++ [ EdgePreviousOutOfRange (DirectedEdgeId (fromIntegral index)) (DirectedEdgeId value) halfCount
         | index <- [0 .. halfCount - 1]
         , let value = pagedUnsafeIndex (triHalfTopology triangulation) (4 * index + 2)
         , fromIntegral value >= halfCount
         ]
      ++ [ EdgeFaceOutOfRange (DirectedEdgeId (fromIntegral index)) (FaceId value) facesCount
         | index <- [0 .. halfCount - 1]
         , let value = pagedUnsafeIndex (triHalfTopology triangulation) (4 * index + 3)
         , fromIntegral value >= facesCount
         ]
      ++ [ VertexOutgoingOutOfRange (VertexId (fromIntegral index)) (DirectedEdgeId value) halfCount
         | index <- [0 .. pagedLength (triVertexOut triangulation) - 1]
         , let value = pagedUnsafeIndex (triVertexOut triangulation) index
         , value /= noIndex
         , fromIntegral value >= halfCount
         ]
      ++ [ FaceAdjacentOutOfRange (FaceId (fromIntegral index)) (DirectedEdgeId value) halfCount
         | index <- [0 .. facesCount - 1]
         , let value = pagedUnsafeIndex (triFaceEdge triangulation) index
         , value /= noIndex
         , fromIntegral value >= halfCount
         ]

  edgeViolations
    | not halfArraysEqual || odd halfCount = []
    | otherwise = concatMap validateEdge (directedEdges triangulation)

  validateEdge edge@(DirectedEdgeId raw) =
    let index = fromIntegral raw
        nextEdge = next triangulation edge
        previousEdge = previous triangulation edge
        twinEdge = reverseEdge edge
        local =
          [ EdgeNextPreviousMismatch edge nextEdge
          | validEdge nextEdge && previous triangulation nextEdge /= edge
          ]
            ++ [ EdgePreviousNextMismatch edge previousEdge
               | validEdge previousEdge && next triangulation previousEdge /= edge
               ]
            ++ [EdgeDoubleReversalMismatch edge | reverseEdge twinEdge /= edge]
            ++ [EdgeSelfLinkedNext edge | nextEdge == edge && halfCount > 2]
            ++ [EdgeSelfLinkedPrevious edge | previousEdge == edge && halfCount > 2]
        innerCycle =
          if incidentFace triangulation edge /= outerFace && validEdge nextEdge && validEdge previousEdge
            then
              [ InnerFaceNotTriangularAtEdge edge
              | next triangulation (next triangulation nextEdge) /= edge
              ]
            else []
     in if index < halfCount then local ++ innerCycle else []

  validEdge (DirectedEdgeId value) = fromIntegral value < halfCount

  faceViolations = concatMap validateFace (allFaces triangulation)
  validateFace face@(FaceId _) =
    case adjacentEdge triangulation face of
      Nothing
        | face == outerFace && halfCount == 0 -> []
        | otherwise -> [FaceMissingAdjacentEdge face]
      Just edge ->
        [ FaceRepresentativeMismatch face edge representedFace
        | let representedFace = incidentFace triangulation edge
        , representedFace /= face
        ]
          ++ [ InnerFaceVertexCardinalityMismatch face (length faceVertexIds) (length (nub faceVertexIds))
             | face /= outerFace
             , let faceVertexIds = faceVertices triangulation face
             , length faceVertexIds /= 3 || length (nub faceVertexIds) /= 3
             ]

  vertexViolations = concatMap validateVertex (vertices triangulation)
  validateVertex vertex = case vertexOutEdge triangulation vertex of
    Nothing
      | verticesCount <= 1 -> []
      | otherwise -> [ConnectedVertexMissingOutgoing vertex]
    Just edge ->
      [ VertexOutgoingOriginMismatch vertex edge actualOrigin
      | let actualOrigin = origin triangulation edge
      , actualOrigin /= vertex
      ]


  eulerViolations
    | not (null cardinalityViolations) || verticesCount < 2 = []
    | numInnerFaces triangulation == 0 =
        [ CollinearEdgeCountMismatch (verticesCount - 1) edgeCount
        | edgeCount /= verticesCount - 1
        ]
    | otherwise =
        [ EulerCharacteristicMismatch eulerCharacteristic
        | eulerCharacteristic /= 2
        ]
   where
    eulerCharacteristic = verticesCount - edgeCount + facesCount

-- | Every edge whose circumcircle is not empty.
validateDelaunay :: Triangulation mode vertex directed undirected face -> [InvariantViolation]
validateDelaunay triangulation = concatMap validateEdge (undirectedEdges triangulation)
 where
  validateEdge edge
    | isConstraintEdge triangulation edge = []
    | isBoundaryEdge triangulation edge = []
    | otherwise =
        let directed = normalizedDirected edge
            twin = reverseEdge directed
         in case (innerFaceDirectedEdges triangulation (incidentFace triangulation directed), innerFaceDirectedEdges triangulation (incidentFace triangulation twin)) of
              (Just _, Just _) ->
                let a = vertexPoint triangulation (origin triangulation directed)
                    b = vertexPoint triangulation (destination triangulation directed)
                    c = vertexPoint triangulation (origin triangulation (previous triangulation directed))
                    d = vertexPoint triangulation (origin triangulation (previous triangulation twin))
                    convex = orient2d c d b == GT && orient2d d c a == GT
                    circle = inCircle a b c d
                    illegal = convex && (circle == GT || (circle == EQ && orderedPair c d < orderedPair a b))
                 in [LocallyIllegalDelaunayEdge edge | illegal]
              _ -> [DelaunayIncidentFaceNotTriangular edge]

-- | Topology first; the Delaunay property only if the topology holds.
validateTriangulation
  :: Triangulation mode vertex directed undirected face
  -> [InvariantViolation]
validateTriangulation triangulation =
  let topology = validateTopology triangulation
   in if null topology
        then validateDelaunay triangulation
        else topology

-- | Whether 'validateTriangulation' is empty.
triangulationIsValid
  :: Triangulation mode vertex directed undirected face
  -> Bool
triangulationIsValid = null . validateTriangulation

-- | Signed area, or 'Nothing' where the face is not a triangle.
faceArea :: Triangulation mode vertex directed undirected face -> FaceId -> Maybe Double
faceArea triangulation face = do
  (v0, v1, v2) <- innerFaceVertices triangulation face
  pure (triangleArea (vertexPoint triangulation v0) (vertexPoint triangulation v1) (vertexPoint triangulation v2))

-- | Smallest interior angle, in degrees.
faceMinimumAngleDegrees :: Triangulation mode vertex directed undirected face -> FaceId -> Maybe Double
faceMinimumAngleDegrees triangulation face = do
  (v0, v1, v2) <- innerFaceVertices triangulation face
  let p0 = vertexPoint triangulation v0
      p1 = vertexPoint triangulation v1
      p2 = vertexPoint triangulation v2
      a = sqrt (squaredDistance p1 p2)
      b = sqrt (squaredDistance p2 p0)
      c = sqrt (squaredDistance p0 p1)
  if min a (min b c) <= 0
    then Nothing
    else Just (minimum [angle b c a, angle c a b, angle a b c])
 where
  angle left right opposite = acos (clamp ((left * left + right * right - opposite * opposite) / (2 * left * right))) * 180 / pi
  clamp = max (-1) . min 1
