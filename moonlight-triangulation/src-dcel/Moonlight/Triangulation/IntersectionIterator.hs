{-# LANGUAGE BangPatterns #-}

-- | Ordered traversal of the mesh features met by a line segment.
module Moonlight.Triangulation.IntersectionIterator
  ( Intersection (..)
  , lineIntersections
  , lineIntersectionsBetweenVertices
  , foldCorridorBetweenPoints
  , foldCorridorBetweenVertices
  , conflictingEdges
  , segmentIntersectsNonCollinear
  ) where

import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Void (Void, absurd)
import Moonlight.Triangulation.Dcel
import Moonlight.Triangulation.Handles.HandleDefs
import Moonlight.Triangulation.Handles.Iterators.FixedIterators (undirectedEdges, vertices)
import Moonlight.Triangulation.Math
import Moonlight.Triangulation.PointLocation
import Moonlight.Triangulation.Types

-- | One crossing: an edge cut, a vertex hit, or a collinear overlap.
data Intersection
  = EdgeIntersection !DirectedEdgeId
  | VertexIntersection !VertexId
  | EdgeOverlap !DirectedEdgeId
  deriving stock (Eq, Ord, Show)

-- | Every crossing between two points, ordered along the segment.
lineIntersections
  :: Triangulation mode vertex directed undirected face
  -> QueryPoint
  -> QueryPoint
  -> [Intersection]
lineIntersections triangulation queryFrom queryTo =
  let !from = queryPointValue queryFrom
      !to = queryPointValue queryTo
   in case firstIntersection triangulation queryFrom queryTo of
        Nothing -> []
        Just first -> walkIntersections triangulation from to first

-- | 'lineIntersections' between two existing vertices.
lineIntersectionsBetweenVertices :: Triangulation mode vertex directed undirected face -> VertexId -> VertexId -> [Intersection]
lineIntersectionsBetweenVertices triangulation fromVertex toVertex =
  let from = vertexPoint triangulation fromVertex
      to = vertexPoint triangulation toVertex
   in walkIntersections triangulation from to (VertexIntersection fromVertex)

-- | The directed edges a crossing list cuts.
conflictingEdges :: [Intersection] -> [DirectedEdgeId]
conflictingEdges = mapMaybe asConflict
 where
  asConflict (EdgeIntersection edge) = Just edge
  asConflict _ = Nothing

-- | Fold the corridor one crossing at a time, stopping the instant the step
-- function answers.
--
-- The list-producing walks cannot stop early: their step budget is only known
-- to have been respected once the walk ends, so the whole corridor is
-- materialized before the first event is visible. A caller whose answer is
-- settled by a prefix — anything asking whether some crossing exists — should
-- not pay for the suffix. 'Nothing' reports a walk that outran its budget and
-- is the caller's signal to fall back to the materialized walk, which
-- substitutes the exact scan.
foldCorridorBetweenVertices
  :: Triangulation mode vertex directed undirected face
  -> VertexId
  -> VertexId
  -> (state -> Intersection -> Either answer state)
  -> state
  -> Maybe (Either answer state)
foldCorridorBetweenVertices triangulation fromVertex toVertex =
  foldCorridor
    triangulation
    (vertexPoint triangulation fromVertex)
    (vertexPoint triangulation toVertex)
    (VertexIntersection fromVertex)

-- | As 'foldCorridorBetweenVertices', for a corridor given by its endpoints.
-- A segment that meets nothing at all folds to the initial state.
foldCorridorBetweenPoints
  :: Triangulation mode vertex directed undirected face
  -> QueryPoint
  -> QueryPoint
  -> (state -> Intersection -> Either answer state)
  -> state
  -> Maybe (Either answer state)
foldCorridorBetweenPoints triangulation queryFrom queryTo step state =
  case firstIntersection triangulation queryFrom queryTo of
    Nothing -> Just (Right state)
    Just first ->
      foldCorridor
        triangulation
        (queryPointValue queryFrom)
        (queryPointValue queryTo)
        first
        step
        state

foldCorridor
  :: Triangulation mode vertex directed undirected face
  -> Point
  -> Point
  -> Intersection
  -> (state -> Intersection -> Either answer state)
  -> state
  -> Maybe (Either answer state)
foldCorridor triangulation from to first step =
  go (2 * numDirectedEdges triangulation + numVertices triangulation + 8) first
 where
  go !remaining !current !state
    | remaining <= 0 = Nothing
    | otherwise = case step state current of
        Left answer -> Just (Left answer)
        Right advanced ->
          case nextIntersection triangulation from to current of
            Nothing -> Just (Right advanced)
            Just following -> go (remaining - 1) following advanced
-- The early-answer 'Either' exists to let a caller stop at the first event it
-- cares about; it should not survive to runtime. Inlining the non-recursive
-- wrapper puts the worker at each call site with 'step' statically known, so
-- the constructor is matched where it is built — and at 'walkIntersections',
-- where the answer is 'Void', the left branch is erased outright.
{-# INLINE foldCorridor #-}

walkIntersections :: Triangulation mode vertex directed undirected face -> Point -> Point -> Intersection -> [Intersection]
walkIntersections triangulation from to first =
  case foldCorridor triangulation from to first collect [] of
    Nothing -> exactIntersectionScan triangulation from to
    Just (Left impossible) -> absurd impossible
    Just (Right events) -> reverse events
 where
  collect :: [Intersection] -> Intersection -> Either Void [Intersection]
  collect accumulated event = Right (event : accumulated)

nextIntersection :: Triangulation mode vertex directed undirected face -> Point -> Point -> Intersection -> Maybe Intersection
nextIntersection triangulation lineFrom lineTo current = case current of
  EdgeIntersection edge -> case traceDirectionOutOfEdge triangulation edge lineFrom lineTo of
    EdgeOutHull -> Nothing
    EdgeOutVertex vertex -> Just (VertexIntersection vertex)
    EdgeOutEdge nextEdge -> Just (EdgeIntersection nextEdge)
    EdgeOutNone -> Nothing
  VertexIntersection vertex
    | vertexPoint triangulation vertex == lineTo -> Nothing
    | otherwise -> case traceDirectionOutOfVertex triangulation vertex lineTo of
        VertexOutHull -> Nothing
        VertexOutOverlap edge -> Just (EdgeOverlap edge)
        VertexOutEdge edge ->
          let from = vertexPoint triangulation (origin triangulation edge)
              to = vertexPoint triangulation (destination triangulation edge)
           in if orient2d from to lineTo == LT then Nothing else Just (EdgeIntersection edge)
  EdgeOverlap edge
    | lineFrom == lineTo -> Nothing
    | onClosedSegment lineFrom lineTo (vertexPoint triangulation (destination triangulation edge)) ->
        Just (VertexIntersection (destination triangulation edge))
    | otherwise -> Nothing

firstIntersection :: Triangulation mode vertex directed undirected face -> QueryPoint -> QueryPoint -> Maybe Intersection
firstIntersection triangulation queryFrom queryTo =
  case locatePoint triangulation queryFrom of
    EmptyTriangulation -> singleVertexHit
    OnVertex vertex -> Just (VertexIntersection vertex)
    OnEdge edge -> Just (classifyStartingEdge edge)
    InFace face -> firstFromFace face
    OutsideConvexHull entry -> firstFromOutside entry
 where
  !lineFrom = queryPointValue queryFrom
  !lineTo = queryPointValue queryTo
  singleVertexHit = case vertices triangulation of
    [vertex]
      | onClosedSegment lineFrom lineTo (vertexPoint triangulation vertex) -> Just (VertexIntersection vertex)
    _ -> Nothing

  classifyStartingEdge edge =
    let a = vertexPoint triangulation (origin triangulation edge)
        b = vertexPoint triangulation (destination triangulation edge)
     in if orient2d lineFrom lineTo a == EQ && orient2d lineFrom lineTo b == EQ
          then EdgeOverlap (orientAlongLine edge)
          else EdgeIntersection (orientTowardTarget edge)

  firstFromFace face = firstEdgeFromRing (faceDirectedEdges triangulation face)

  firstEdgeFromRing [] = Nothing
  firstEdgeFromRing (edge : remaining) =
    let a = vertexPoint triangulation (origin triangulation edge)
        b = vertexPoint triangulation (destination triangulation edge)
     in if segmentIntersectsNonCollinear lineFrom lineTo a b
          then
            if orient2d lineFrom lineTo a == EQ
              then Just (VertexIntersection (origin triangulation edge))
              else
                if orient2d lineFrom lineTo b == EQ
                  then Just (VertexIntersection (destination triangulation edge))
                  else Just (EdgeIntersection (reverseEdge edge))
          else firstEdgeFromRing remaining

  -- Outside the region the segment's first contact with it lies on the ring
  -- the locator's edge sits on, so only that ring can carry the earliest
  -- event. A ring that outran its budget, an absent locator edge, and
  -- endpoints that leave the parameter comparison without a total order all
  -- keep the exact scan.
  firstFromOutside (Just edge)
    | finiteEndpoint lineFrom && finiteEndpoint lineTo =
        case ringEntryEvent triangulation lineFrom lineTo edge of
          Just entry -> eventValue <$> entry
          Nothing -> firstFromScan
  firstFromOutside _ = firstFromScan

  firstFromScan = case exactIntersectionScan triangulation lineFrom lineTo of
    event : _ -> Just event
    [] -> Nothing

  orientAlongLine edge =
    let a = vertexPoint triangulation (origin triangulation edge)
        b = vertexPoint triangulation (destination triangulation edge)
     in if projectionFactor lineFrom lineTo a <= projectionFactor lineFrom lineTo b then edge else reverseEdge edge

  orientTowardTarget edge =
    let a = vertexPoint triangulation (origin triangulation edge)
        b = vertexPoint triangulation (destination triangulation edge)
     in if orient2d a b lineTo == LT then reverseEdge edge else edge

data VertexOut
  = VertexOutHull
  | VertexOutOverlap !DirectedEdgeId
  | VertexOutEdge !DirectedEdgeId

data EdgeOut
  = EdgeOutHull
  | EdgeOutVertex !VertexId
  | EdgeOutEdge !DirectedEdgeId
  | EdgeOutNone

traceDirectionOutOfVertex :: Triangulation mode vertex directed undirected face -> VertexId -> Point -> VertexOut
traceDirectionOutOfVertex triangulation vertex target =
  case vertexOutEdge triangulation vertex of
    Nothing -> VertexOutHull
    Just start ->
      let !startSide = sideOf start
          !rotateCounterClockwise = startSide == GT
       in go rotateCounterClockwise (numDirectedEdges triangulation + 1) start startSide
 where
  go !rotateCounterClockwise !remaining !current !currentSide
    | remaining <= 0 = VertexOutHull
    | currentSide == EQ && projectionFactor currentPoint (edgeTarget current) target >= 0 =
        VertexOutOverlap current
    | otherwise =
        let following = if rotateCounterClockwise then counterClockwise triangulation current else clockwise triangulation current
            followingSide = sideOf following
         in if followingSide == EQ && projectionFactor currentPoint (edgeTarget following) target >= 0
              then VertexOutOverlap following
              else
                let faceBetween = if rotateCounterClockwise then incidentFace triangulation current else incidentFace triangulation following
                 in if faceBetween == outerFace
                      then VertexOutHull
                      else
                        if rotateCounterClockwise == (followingSide == LT)
                          then
                            let segment = if rotateCounterClockwise then next triangulation current else previous triangulation (reverseEdge current)
                             in VertexOutEdge (reverseEdge segment)
                          else go rotateCounterClockwise (remaining - 1) following followingSide

  currentPoint = vertexPoint triangulation vertex
  edgeTarget edge = vertexPoint triangulation (destination triangulation edge)
  sideOf edge = orient2d currentPoint (edgeTarget edge) target

traceDirectionOutOfEdge :: Triangulation mode vertex directed undirected face -> DirectedEdgeId -> Point -> Point -> EdgeOut
traceDirectionOutOfEdge triangulation edge lineFrom lineTo
  | incidentFace triangulation edge == outerFace = EdgeOutHull
  | otherwise =
      case (previousIntersects, nextIntersects) of
        (True, False) -> EdgeOutEdge (reverseEdge edgePrevious)
        (False, True) -> EdgeOutEdge (reverseEdge edgeNext)
        (True, True) -> EdgeOutVertex (origin triangulation edgePrevious)
        (False, False) -> EdgeOutNone
 where
  edgePrevious = previous triangulation edge
  edgeNext = next triangulation edge

  -- The face runs @edge@ A->B, @edgeNext@ B->C, @edgePrevious@ C->A, so the
  -- two candidates share C and each vertex's side of the line is read once
  -- rather than once per candidate. Whether the segment reaches a candidate is
  -- then asked only of one the line already separates, and a walk that entered
  -- across A->B leaves that true for exactly one of the two.
  pointA = vertexPoint triangulation (origin triangulation edge)
  pointB = vertexPoint triangulation (origin triangulation edgeNext)
  pointC = vertexPoint triangulation (origin triangulation edgePrevious)

  sideA = orient2d lineFrom lineTo pointA
  sideB = orient2d lineFrom lineTo pointB
  sideC = orient2d lineFrom lineTo pointC

  previousIntersects = sideC /= sideA && reaches pointC pointA
  nextIntersects = sideB /= sideC && reaches pointB pointC

  reaches from to = orient2d from to lineFrom /= orient2d from to lineTo

-- | Whether two segments properly cross; collinear touching does not.
segmentIntersectsNonCollinear :: Point -> Point -> Point -> Point -> Bool
segmentIntersectsNonCollinear p0 p1 q0 q1 =
  -- Equality admits an endpoint on the opposite segment; four equal sides
  -- reject the collinear case without recomputing either orientation pair.
  p0Side /= p1Side
    && q0Side /= q1Side
 where
  !p0Side = orient2d q0 q1 p0
  !p1Side = orient2d q0 q1 p1
  !q0Side = orient2d p0 p1 q0
  !q1Side = orient2d p0 p1 q1
{-# INLINE segmentIntersectsNonCollinear #-}

exactIntersectionScan :: Triangulation mode vertex directed undirected face -> Point -> Point -> [Intersection]
exactIntersectionScan triangulation lineFrom lineTo =
  map eventValue . sortBy compareEvent $ vertexEvents ++ edgeEvents
 where
  vertexEvents = mapMaybe (vertexEvent triangulation lineFrom lineTo) (vertices triangulation)
  edgeEvents = mapMaybe (edgeEvent triangulation lineFrom lineTo) (undirectedEdges triangulation)

data Event = Event
  { eventParameter :: !Double
  , eventPriority :: {-# UNPACK #-} !Int
  , eventValue :: !Intersection
  }

compareEvent :: Event -> Event -> Ordering
compareEvent = comparing (\event -> (eventParameter event, eventPriority event, eventValue event))

-- Ties in this order are the same 'Intersection', so a traversal that reaches
-- the minimum by a different route still reports the value the sorted scan's
-- head reports.
vertexEvent
  :: Triangulation mode vertex directed undirected face
  -> Point
  -> Point
  -> VertexId
  -> Maybe (Event)
vertexEvent triangulation lineFrom lineTo vertex
  | onClosedSegment lineFrom lineTo point = Just (Event (projectionFactor lineFrom lineTo point) 0 (VertexIntersection vertex))
  | otherwise = Nothing
 where
  point = vertexPoint triangulation vertex

edgeEvent
  :: Triangulation mode vertex directed undirected face
  -> Point
  -> Point
  -> UndirectedEdgeId
  -> Maybe (Event)
edgeEvent triangulation lineFrom lineTo edge
  | oa == EQ && ob == EQ =
      if low < high
        then Just (Event low 1 (EdgeOverlap (if projectedA <= projectedB then directed else reverseEdge directed)))
        else Nothing
  | segmentsIntersect lineFrom lineTo a b && oa /= EQ && ob /= EQ =
      Just (Event (segmentIntersectionParameter lineFrom lineTo a b) 2 (EdgeIntersection oriented))
  | otherwise = Nothing
 where
  directed = normalizedDirected edge
  a = vertexPoint triangulation (origin triangulation directed)
  b = vertexPoint triangulation (destination triangulation directed)
  oa = orient2d lineFrom lineTo a
  ob = orient2d lineFrom lineTo b
  projectedA = projectionFactor lineFrom lineTo a
  projectedB = projectionFactor lineFrom lineTo b
  low = max 0 (min projectedA projectedB)
  high = min 1 (max projectedA projectedB)
  oriented = if orient2d a b lineTo == LT then reverseEdge directed else directed

-- | The earliest event carried by the outer-face ring the given edge sits on,
-- walked from that edge. 'Nothing' reports a ring that outran its budget.
ringEntryEvent
  :: Triangulation mode vertex directed undirected face
  -> Point
  -> Point
  -> DirectedEdgeId
  -> Maybe (Maybe (Event))
ringEntryEvent triangulation lineFrom lineTo entry =
  go (numDirectedEdges triangulation + 1) entry False Nothing
 where
  go !remaining !edge !departed !earliest
    | remaining <= 0 = Nothing
    | departed && edge == entry = Just earliest
    | otherwise =
        let !stepped =
              keepEarliest (edgeEvent triangulation lineFrom lineTo (asUndirected edge)) $
                keepEarliest (vertexEvent triangulation lineFrom lineTo (origin triangulation edge)) earliest
         in go (remaining - 1) (next triangulation edge) True stepped

keepEarliest :: Maybe (Event) -> Maybe (Event) -> Maybe (Event)
keepEarliest Nothing held = held
keepEarliest candidate Nothing = candidate
keepEarliest candidate@(Just proposed) held@(Just incumbent)
  | compareEvent proposed incumbent == LT = candidate
  | otherwise = held

finiteEndpoint :: Point -> Bool
finiteEndpoint point = isFinite (pointX point) && isFinite (pointY point)

segmentIntersectionParameter :: Point -> Point -> Point -> Point -> Double
segmentIntersectionParameter (Point ax ay) (Point bx by) (Point cx cy) (Point dx dy)
  | denominator == 0 = 0
  | otherwise = ((cx - ax) * (dy - cy) - (cy - ay) * (dx - cx)) / denominator
 where
  denominator = (bx - ax) * (dy - cy) - (by - ay) * (dx - cx)
