{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Geometry.Discrete.Geodesic
  ( GeodesicStep (..),
    DiscreteGeodesic (..),
    GeodesicError (..),
    LocalGeodesicStep (..),
    GeodesicLiftError (..),
    geodesicDistancesBy,
    geodesicDistances,
    solveDiscreteGeodesicBy,
    solveDiscreteGeodesic,
    parallelTransportAlongGeodesic,
    parallelTransportAlongGeometryGeodesic,
    chartTransitionAlongGeodesic,
    liftGeodesicToAtlas,
  )
where

import Control.Monad (foldM)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Moonlight.Algebra (Orientation (..))
import Moonlight.Geometry.Discrete
  ( CellComplex (..),
    DiscreteGeometry (..),
  )
import Moonlight.Geometry.Discrete.Chart
  ( Atlas,
    ChartAmbient (..),
    ChartTransition,
    atlasTransitionPath,
    chartCenter,
    chartCoordinate,
    identityChartTransition,
    lookupAtlasChart,
  )
import Moonlight.Geometry.Discrete.Connection
  ( DiscreteConnection,
    SpecialOrthogonal (..),
    composeConnection,
    inverseConnection,
  )
import Moonlight.Geometry.Discrete.Metric
  ( Dim (..),
    VecN,
  )
import Prelude

type GeodesicStep :: Type -> Type -> Type
data GeodesicStep vertex edge = GeodesicStep
  { geodesicStepEdge :: !edge,
    geodesicStepSource :: !vertex,
    geodesicStepTarget :: !vertex,
    geodesicStepOrientation :: !Orientation,
    geodesicStepLength :: !Double
  }

type DiscreteGeodesic :: Type -> Type -> Type
data DiscreteGeodesic vertex edge = DiscreteGeodesic
  { geodesicStart :: !vertex,
    geodesicGoal :: !vertex,
    geodesicVertices :: ![vertex],
    geodesicSteps :: ![GeodesicStep vertex edge],
    geodesicLength :: !Double
  }

type GeodesicError :: Type -> Type -> Type
data GeodesicError vertex edge
  = MissingGeodesicVertex !vertex
  | MissingGeodesicEdgeLength !edge
  | InvalidGeodesicEdgeLength !edge !Double
  | NegativeGeodesicEdgeLength !edge !Double
  | MissingGeodesicEdgeConnection !edge
  | DisconnectedGeodesic !vertex !vertex
  deriving stock (Eq, Show)

type LocalGeodesicStep :: Dim -> Type -> Type -> Type
data LocalGeodesicStep (d :: Dim) vertex edge = LocalGeodesicStep
  { localGeodesicChartCenter :: !vertex,
    localGeodesicSourceCoordinate :: !(VecN d),
    localGeodesicTargetCoordinate :: !(VecN d),
    localGeodesicDirection :: !(VecN d),
    localGeodesicStepData :: !(GeodesicStep vertex edge)
  }

type GeodesicLiftError :: Type -> Type
data GeodesicLiftError vertex
  = MissingLocalChart !vertex
  | MissingLocalCoordinate !vertex !vertex
  deriving stock (Eq, Show)

geodesicDistancesBy ::
  (CellComplex c, Ord (Vertex c)) =>
  c ->
  (Edge c -> Maybe Double) ->
  Vertex c ->
  Either (GeodesicError (Vertex c) (Edge c)) (Map (Vertex c) Double)
geodesicDistancesBy complexValue edgeLengthOf startVertex = do
  (distanceMap, _) <- dijkstraPredecessorsBy complexValue edgeLengthOf startVertex
  pure distanceMap

geodesicDistances ::
  (CellComplex c, Ord (Vertex c), Ord (Edge c)) =>
  DiscreteGeometry c d ->
  Vertex c ->
  Either (GeodesicError (Vertex c) (Edge c)) (Map (Vertex c) Double)
geodesicDistances geometryValue =
  geodesicDistancesBy
    (dgComplex geometryValue)
    (\edgeValue -> Map.lookup edgeValue (dgEdgeLengths geometryValue))

solveDiscreteGeodesicBy ::
  (CellComplex c, Ord (Vertex c)) =>
  c ->
  (Edge c -> Maybe Double) ->
  Vertex c ->
  Vertex c ->
  Either (GeodesicError (Vertex c) (Edge c)) (DiscreteGeodesic (Vertex c) (Edge c))
solveDiscreteGeodesicBy complexValue edgeLengthOf startVertex goalVertex = do
  ensureVertexPresent complexValue startVertex
  ensureVertexPresent complexValue goalVertex
  (distanceMap, predecessorMap) <-
    dijkstraPredecessorsBy complexValue edgeLengthOf startVertex
  reconstructGeodesic startVertex goalVertex distanceMap predecessorMap

solveDiscreteGeodesic ::
  (CellComplex c, Ord (Vertex c), Ord (Edge c)) =>
  DiscreteGeometry c d ->
  Vertex c ->
  Vertex c ->
  Either (GeodesicError (Vertex c) (Edge c)) (DiscreteGeodesic (Vertex c) (Edge c))
solveDiscreteGeodesic geometryValue =
  solveDiscreteGeodesicBy
    (dgComplex geometryValue)
    (\edgeValue -> Map.lookup edgeValue (dgEdgeLengths geometryValue))

parallelTransportAlongGeodesic ::

  SpecialOrthogonal d =>
  (edge -> Maybe (DiscreteConnection d)) ->
  DiscreteGeodesic vertex edge ->
  Either (GeodesicError vertex edge) (DiscreteConnection d)
parallelTransportAlongGeodesic lookupConnection geodesicValue =
  foldM stepTransport (identitySO) (geodesicSteps geodesicValue)
  where
    stepTransport accumulatedConnection stepValue = do
      edgeConnection <-
        case lookupConnection (geodesicStepEdge stepValue) of
          Just value -> Right value
          Nothing -> Left (MissingGeodesicEdgeConnection (geodesicStepEdge stepValue))
      pure
        ( composeConnection
            (orientConnection (geodesicStepOrientation stepValue) edgeConnection)
            accumulatedConnection
        )

parallelTransportAlongGeometryGeodesic ::

  (Ord (Edge c), SpecialOrthogonal d) =>
  DiscreteGeometry c d ->
  DiscreteGeodesic (Vertex c) (Edge c) ->
  Either (GeodesicError (Vertex c) (Edge c)) (DiscreteConnection d)
parallelTransportAlongGeometryGeodesic geometryValue =
  parallelTransportAlongGeodesic
    (\edgeValue -> Map.lookup edgeValue (dgEdgeConnections geometryValue))

chartTransitionAlongGeodesic ::

  (Eq vertex, Ord edge, ChartAmbient d, SpecialOrthogonal d) =>
  Atlas edge d vertex ->
  DiscreteGeodesic vertex edge ->
  Maybe (ChartTransition d vertex)
chartTransitionAlongGeodesic atlasValue geodesicValue =
  case geodesicSteps geodesicValue of
    [] ->
      Just (identityChartTransition (geodesicStart geodesicValue))
    stepValues ->
      atlasTransitionPath
        atlasValue
        ( fmap
            (\stepValue -> (geodesicStepEdge stepValue, geodesicStepOrientation stepValue))
            stepValues
        )

liftGeodesicToAtlas ::
  forall edge d vertex.
  (Ord vertex, ChartAmbient d) =>
  Atlas edge d vertex ->
  DiscreteGeodesic vertex edge ->
  Either (GeodesicLiftError vertex) [LocalGeodesicStep d vertex edge]
liftGeodesicToAtlas atlasValue geodesicValue =
  traverse liftStep (geodesicSteps geodesicValue)
  where
    liftStep stepValue = do
      sourceChart <-
        case lookupAtlasChart (geodesicStepSource stepValue) atlasValue of
          Just value -> Right value
          Nothing -> Left (MissingLocalChart (geodesicStepSource stepValue))
      sourceCoordinate <-
        case chartCoordinate (geodesicStepSource stepValue) sourceChart of
          Just value -> Right value
          Nothing ->
            Left
              ( MissingLocalCoordinate
                  (chartCenter sourceChart)
                  (geodesicStepSource stepValue)
              )
      targetCoordinate <-
        case chartCoordinate (geodesicStepTarget stepValue) sourceChart of
          Just value -> Right value
          Nothing ->
            Left
              ( MissingLocalCoordinate
                  (chartCenter sourceChart)
                  (geodesicStepTarget stepValue)
              )
      pure
        LocalGeodesicStep
          { localGeodesicChartCenter = chartCenter sourceChart,
            localGeodesicSourceCoordinate = sourceCoordinate,
            localGeodesicTargetCoordinate = targetCoordinate,
            localGeodesicDirection =
              normalizeChartVec @d (subChartVec @d targetCoordinate sourceCoordinate),
            localGeodesicStepData = stepValue
          }

type GeodesicPredecessor :: Type -> Type -> Type
data GeodesicPredecessor vertex edge = GeodesicPredecessor
  { geodesicPredecessorStep :: !(GeodesicStep vertex edge)
  }

dijkstraPredecessorsBy ::
  (CellComplex c, Ord (Vertex c)) =>
  c ->
  (Edge c -> Maybe Double) ->
  Vertex c ->
  Either
    (GeodesicError (Vertex c) (Edge c))
    ( Map (Vertex c) Double,
      Map (Vertex c) (GeodesicPredecessor (Vertex c) (Edge c))
    )
dijkstraPredecessorsBy complexValue edgeLengthOf startVertex = do
  ensureVertexPresent complexValue startVertex
  go (Set.singleton (0.0, startVertex)) (Map.singleton startVertex 0.0) Map.empty
  where
    go frontierValue distanceMap predecessorMap =
      case Set.minView frontierValue of
        Nothing ->
          Right (distanceMap, predecessorMap)
        Just ((currentDistance, currentVertex), remainingFrontier)
          | isStaleDistance currentVertex currentDistance distanceMap ->
              go remainingFrontier distanceMap predecessorMap
          | otherwise -> do
              (nextFrontier, nextDistanceMap, nextPredecessorMap) <-
                foldM
                  (relaxNeighbor currentVertex currentDistance)
                  (remainingFrontier, distanceMap, predecessorMap)
                  (incidentNeighborSteps complexValue currentVertex)
              go nextFrontier nextDistanceMap nextPredecessorMap

    relaxNeighbor
      currentVertex
      currentDistance
      (frontierValue, distanceMap, predecessorMap)
      (edgeValue, orientationValue, nextVertex) = do
        edgeLength <-
          case edgeLengthOf edgeValue of
            Just value -> Right value
            Nothing -> Left (MissingGeodesicEdgeLength edgeValue)
        if isNaN edgeLength || isInfinite edgeLength
          then Left (InvalidGeodesicEdgeLength edgeValue edgeLength)
          else
            if edgeLength < 0.0
              then Left (NegativeGeodesicEdgeLength edgeValue edgeLength)
              else
                let candidateDistance = currentDistance + edgeLength
                 in if improvesDistance nextVertex candidateDistance distanceMap
                      then
                        Right
                          ( Set.insert (candidateDistance, nextVertex) frontierValue,
                            Map.insert nextVertex candidateDistance distanceMap,
                            Map.insert
                              nextVertex
                              ( GeodesicPredecessor
                                  GeodesicStep
                                    { geodesicStepEdge = edgeValue,
                                      geodesicStepSource = currentVertex,
                                      geodesicStepTarget = nextVertex,
                                      geodesicStepOrientation = orientationValue,
                                      geodesicStepLength = edgeLength
                                    }
                              )
                              predecessorMap
                          )
                      else Right (frontierValue, distanceMap, predecessorMap)

reconstructGeodesic ::
  Ord vertex =>
  vertex ->
  vertex ->
  Map vertex Double ->
  Map vertex (GeodesicPredecessor vertex edge) ->
  Either (GeodesicError vertex edge) (DiscreteGeodesic vertex edge)
reconstructGeodesic startVertex goalVertex distanceMap predecessorMap =
  case Map.lookup goalVertex distanceMap of
    Nothing ->
      Left (DisconnectedGeodesic startVertex goalVertex)
    Just totalLength ->
      if startVertex == goalVertex
        then
          Right
            DiscreteGeodesic
              { geodesicStart = startVertex,
                geodesicGoal = goalVertex,
                geodesicVertices = [startVertex],
                geodesicSteps = [],
                geodesicLength = totalLength
              }
        else buildPath totalLength goalVertex [goalVertex] []
  where
    buildPath totalLength currentVertex accumulatedVertices accumulatedSteps
      | currentVertex == startVertex =
          Right
            DiscreteGeodesic
              { geodesicStart = startVertex,
                geodesicGoal = goalVertex,
                geodesicVertices = accumulatedVertices,
                geodesicSteps = accumulatedSteps,
                geodesicLength = totalLength
              }
      | otherwise =
          case Map.lookup currentVertex predecessorMap of
            Nothing ->
              Left (DisconnectedGeodesic startVertex goalVertex)
            Just predecessorValue ->
              let stepValue = geodesicPredecessorStep predecessorValue
               in buildPath
                    totalLength
                    (geodesicStepSource stepValue)
                    (geodesicStepSource stepValue : accumulatedVertices)
                    (stepValue : accumulatedSteps)

incidentNeighborSteps ::
  (CellComplex c, Ord (Vertex c)) =>
  c ->
  Vertex c ->
  [(Edge c, Orientation, Vertex c)]
incidentNeighborSteps complexValue vertexValue =
  mapMaybe
    (orientIncidentEdge vertexValue)
    (starOf complexValue vertexValue)
  where
    orientIncidentEdge currentVertex edgeValue =
      let (sourceVertex, targetVertex) = edgeBoundary complexValue edgeValue
       in if sourceVertex == currentVertex
            then Just (edgeValue, Positive, targetVertex)
            else
              if targetVertex == currentVertex
                then Just (edgeValue, Negative, sourceVertex)
                else Nothing

ensureVertexPresent ::
  (CellComplex c, Ord (Vertex c)) =>
  c ->
  Vertex c ->
  Either (GeodesicError (Vertex c) edge) ()
ensureVertexPresent complexValue vertexValue =
  if vertexValue `elem` vertices complexValue
    then Right ()
    else Left (MissingGeodesicVertex vertexValue)

improvesDistance :: Ord vertex => vertex -> Double -> Map vertex Double -> Bool
improvesDistance vertexValue candidateDistance distanceMap =
  case Map.lookup vertexValue distanceMap of
    Nothing -> True
    Just currentDistance -> candidateDistance + epsilon < currentDistance

isStaleDistance :: Ord vertex => vertex -> Double -> Map vertex Double -> Bool
isStaleDistance vertexValue candidateDistance distanceMap =
  case Map.lookup vertexValue distanceMap of
    Nothing -> True
    Just currentDistance -> candidateDistance > currentDistance + epsilon

orientConnection ::

  SpecialOrthogonal d =>
  Orientation ->
  DiscreteConnection d ->
  DiscreteConnection d
orientConnection orientationValue connectionValue =
  case orientationValue of
    Positive -> connectionValue
    Negative -> inverseConnection connectionValue

epsilon :: Double
epsilon = 1.0e-12
