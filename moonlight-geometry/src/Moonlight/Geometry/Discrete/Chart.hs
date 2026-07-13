{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Geometry.Discrete.Chart
  ( CoordinateChart (..),
    ChartTransition (..),
    Atlas (..),
    ChartAmbient (..),
    ChartError (..),
    AtlasError (..),
    mkCoordinateChart,
    chartCoordinate,
    chartVertices,
    chartContains,
    chartNeighbors,
    chartOverlap,
    deriveChartTransition,
    orientChartTransition,
    identityChartTransition,
    inverseChartTransition,
    composeChartTransitions,
    applyChartTransition,
    chartTransitionResiduals,
    chartTransitionMaxResidual,
    chartTransitionRMSError,
    lookupAtlasChart,
    lookupAtlasTransition,
    atlasFromComplex,
    atlasTransitionPath,
  )
where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Kind (Constraint, Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Algebra (Orientation (..))
import Moonlight.Geometry.Discrete
  ( CellComplex (..),
  )
import Moonlight.Geometry.Discrete.Connection
  ( DiscreteConnection,
    SpecialOrthogonal (..),
    composeConnection,
    inverseConnection,
    parallelTransport,
  )
import Moonlight.Geometry.Discrete.Metric
  ( Dim (..),
    VecN,
  )
import Moonlight.LinAlg.Geometry
  ( addVec2,
    magnitudeVec2,
    negateVec2,
    normalizeVec2Safe,
    subVec2,
    vec2Zero,
  )
import Moonlight.LinAlg.Geometry
  ( addVec3,
    distanceVec3,
    magnitudeVec3,
    negateVec3,
    normalizeVec3Safe,
    subVec3,
    vec3Zero,
  )
import Prelude

type ChartAmbient :: Dim -> Constraint
class ChartAmbient (d :: Dim) where
  zeroChartVec :: VecN d
  addChartVec :: VecN d -> VecN d -> VecN d
  subChartVec :: VecN d -> VecN d -> VecN d
  negateChartVec :: VecN d -> VecN d
  distanceChartVec :: VecN d -> VecN d -> Double
  magnitudeChartVec :: VecN d -> Double
  normalizeChartVec :: VecN d -> VecN d

instance ChartAmbient 'D2 where
  zeroChartVec = vec2Zero
  addChartVec = addVec2
  subChartVec = subVec2
  negateChartVec = negateVec2
  distanceChartVec leftVector rightVector =
    magnitudeVec2 (subVec2 leftVector rightVector)
  magnitudeChartVec = magnitudeVec2
  normalizeChartVec = normalizeVec2Safe

instance ChartAmbient 'D3 where
  zeroChartVec = vec3Zero
  addChartVec = addVec3
  subChartVec = subVec3
  negateChartVec = negateVec3
  distanceChartVec = distanceVec3
  magnitudeChartVec = magnitudeVec3
  normalizeChartVec = normalizeVec3Safe

type CoordinateChart :: Dim -> Type -> Type
data CoordinateChart (d :: Dim) vertex = CoordinateChart
  { chartCenter :: !vertex,
    chartCoordinates :: !(Map vertex (VecN d))
  }

type ChartTransition :: Dim -> Type -> Type
data ChartTransition (d :: Dim) vertex = ChartTransition
  { chartTransitionSource :: !vertex,
    chartTransitionTarget :: !vertex,
    chartTransitionLinear :: !(DiscreteConnection d),
    chartTransitionTranslation :: !(VecN d)
  }

type Atlas :: Type -> Dim -> Type -> Type
data Atlas edge (d :: Dim) vertex = Atlas
  { atlasCharts :: !(Map vertex (CoordinateChart d vertex)),
    atlasTransitions :: !(Map edge (ChartTransition d vertex))
  }

type ChartError :: Type -> Type
data ChartError vertex
  = MissingChartCoordinate !vertex !vertex
  | TransitionAnchorMismatch !vertex !vertex !Double
  deriving stock (Eq, Show)

type AtlasError :: Type -> Type -> Type
data AtlasError vertex edge
  = MissingAtlasChart !vertex
  | MissingAtlasEdgeConnection !edge
  | AtlasChartError !edge !(ChartError vertex)
  deriving stock (Eq, Show)

mkCoordinateChart ::
  forall d vertex.
  (Ord vertex, ChartAmbient d) =>
  vertex ->
  [(vertex, VecN d)] ->
  CoordinateChart d vertex
mkCoordinateChart centerVertex entries =
  CoordinateChart
    { chartCenter = centerVertex,
      chartCoordinates =
        Map.insert centerVertex (zeroChartVec @d) (Map.fromList entries)
    }

chartCoordinate :: Ord vertex => vertex -> CoordinateChart d vertex -> Maybe (VecN d)
chartCoordinate vertexValue =
  Map.lookup vertexValue . chartCoordinates

chartVertices :: CoordinateChart d vertex -> [vertex]
chartVertices =
  Map.keys . chartCoordinates

chartContains :: Ord vertex => vertex -> CoordinateChart d vertex -> Bool
chartContains vertexValue =
  Map.member vertexValue . chartCoordinates

chartNeighbors :: Eq vertex => CoordinateChart d vertex -> [vertex]
chartNeighbors chartValue =
  filter (/= chartCenter chartValue) (chartVertices chartValue)

chartOverlap :: Ord vertex => CoordinateChart d vertex -> CoordinateChart d vertex -> [vertex]
chartOverlap leftChart rightChart =
  Map.keys (Map.intersection (chartCoordinates leftChart) (chartCoordinates rightChart))

deriveChartTransition ::
  forall d vertex.
  (Ord vertex, ChartAmbient d, SpecialOrthogonal d) =>
  CoordinateChart d vertex ->
  CoordinateChart d vertex ->
  DiscreteConnection d ->
  Either (ChartError vertex) (ChartTransition d vertex)
deriveChartTransition sourceChart targetChart connectionValue = do
  targetCenterInSource <-
    requireChartCoordinate (chartCenter targetChart) sourceChart
  sourceCenterInTarget <-
    requireChartCoordinate (chartCenter sourceChart) targetChart
  let derivedTranslation =
        negateChartVec @d (parallelTransport @d connectionValue targetCenterInSource)
      anchorResidual =
        distanceChartVec @d sourceCenterInTarget derivedTranslation
  if anchorResidual <= epsilon
    then
      pure
        ChartTransition
          { chartTransitionSource = chartCenter sourceChart,
            chartTransitionTarget = chartCenter targetChart,
            chartTransitionLinear = connectionValue,
            chartTransitionTranslation = sourceCenterInTarget
          }
    else
      Left
        ( TransitionAnchorMismatch
            (chartCenter sourceChart)
            (chartCenter targetChart)
            anchorResidual
        )

orientChartTransition ::
  forall d vertex.
  (ChartAmbient d, SpecialOrthogonal d) =>
  Orientation ->
  ChartTransition d vertex ->
  ChartTransition d vertex
orientChartTransition orientationValue transitionValue =
  case orientationValue of
    Positive -> transitionValue
    Negative -> inverseChartTransition @d transitionValue

identityChartTransition ::
  forall d vertex.
  (ChartAmbient d, SpecialOrthogonal d) =>
  vertex ->
  ChartTransition d vertex
identityChartTransition vertexValue =
  ChartTransition
    { chartTransitionSource = vertexValue,
      chartTransitionTarget = vertexValue,
      chartTransitionLinear = identitySO @d,
      chartTransitionTranslation = zeroChartVec @d
    }

inverseChartTransition ::
  forall d vertex.
  (ChartAmbient d, SpecialOrthogonal d) =>
  ChartTransition d vertex ->
  ChartTransition d vertex
inverseChartTransition transitionValue =
  let inverseLinear = inverseConnection @d (chartTransitionLinear transitionValue)
   in ChartTransition
        { chartTransitionSource = chartTransitionTarget transitionValue,
          chartTransitionTarget = chartTransitionSource transitionValue,
          chartTransitionLinear = inverseLinear,
          chartTransitionTranslation =
            negateChartVec @d
              ( parallelTransport @d
                  inverseLinear
                  (chartTransitionTranslation transitionValue)
              )
        }

composeChartTransitions ::
  forall d vertex.
  (Eq vertex, ChartAmbient d, SpecialOrthogonal d) =>
  ChartTransition d vertex ->
  ChartTransition d vertex ->
  Maybe (ChartTransition d vertex)
composeChartTransitions afterTransition beforeTransition
  | chartTransitionTarget beforeTransition /= chartTransitionSource afterTransition =
      Nothing
  | otherwise =
      Just
        ChartTransition
          { chartTransitionSource = chartTransitionSource beforeTransition,
            chartTransitionTarget = chartTransitionTarget afterTransition,
            chartTransitionLinear =
              composeConnection @d
                (chartTransitionLinear afterTransition)
                (chartTransitionLinear beforeTransition),
            chartTransitionTranslation =
              addChartVec @d
                ( parallelTransport @d
                    (chartTransitionLinear afterTransition)
                    (chartTransitionTranslation beforeTransition)
                )
                (chartTransitionTranslation afterTransition)
          }

applyChartTransition ::
  forall d vertex.
  (ChartAmbient d, SpecialOrthogonal d) =>
  ChartTransition d vertex ->
  VecN d ->
  VecN d
applyChartTransition transitionValue vectorValue =
  addChartVec @d
    (parallelTransport @d (chartTransitionLinear transitionValue) vectorValue)
    (chartTransitionTranslation transitionValue)

chartTransitionResiduals ::
  forall d vertex.
  (Ord vertex, ChartAmbient d, SpecialOrthogonal d) =>
  CoordinateChart d vertex ->
  CoordinateChart d vertex ->
  ChartTransition d vertex ->
  [(vertex, Double)]
chartTransitionResiduals sourceChart targetChart transitionValue =
  [ (vertexValue, distanceChartVec @d transformedSourceCoordinate targetCoordinate)
    | vertexValue <- chartOverlap sourceChart targetChart,
      Just sourceCoordinate <- [chartCoordinate vertexValue sourceChart],
      Just targetCoordinate <- [chartCoordinate vertexValue targetChart],
      let transformedSourceCoordinate =
            applyChartTransition @d transitionValue sourceCoordinate
  ]

chartTransitionMaxResidual ::
  forall d vertex.
  (Ord vertex, ChartAmbient d, SpecialOrthogonal d) =>
  CoordinateChart d vertex ->
  CoordinateChart d vertex ->
  ChartTransition d vertex ->
  Double
chartTransitionMaxResidual sourceChart targetChart transitionValue =
  case fmap snd (chartTransitionResiduals @d sourceChart targetChart transitionValue) of
    [] -> 0.0
    residualValues -> maximum residualValues

chartTransitionRMSError ::
  forall d vertex.
  (Ord vertex, ChartAmbient d, SpecialOrthogonal d) =>
  CoordinateChart d vertex ->
  CoordinateChart d vertex ->
  ChartTransition d vertex ->
  Double
chartTransitionRMSError sourceChart targetChart transitionValue =
  case fmap snd (chartTransitionResiduals @d sourceChart targetChart transitionValue) of
    [] -> 0.0
    residualValues ->
      sqrt
        ( sum (fmap (\residualValue -> residualValue * residualValue) residualValues)
            / fromIntegral (length residualValues)
        )

lookupAtlasChart :: Ord vertex => vertex -> Atlas edge d vertex -> Maybe (CoordinateChart d vertex)
lookupAtlasChart vertexValue atlasValue =
  Map.lookup vertexValue (atlasCharts atlasValue)

lookupAtlasTransition :: Ord edge => edge -> Atlas edge d vertex -> Maybe (ChartTransition d vertex)
lookupAtlasTransition edgeValue atlasValue =
  Map.lookup edgeValue (atlasTransitions atlasValue)

atlasFromComplex ::
  forall c d.
  (CellComplex c, Ord (Vertex c), Ord (Edge c), ChartAmbient d, SpecialOrthogonal d) =>
  c ->
  Map (Vertex c) (CoordinateChart d (Vertex c)) ->
  Map (Edge c) (DiscreteConnection d) ->
  Either (AtlasError (Vertex c) (Edge c)) (Atlas (Edge c) d (Vertex c))
atlasFromComplex complexValue chartMap edgeConnectionMap = do
  transitionEntries <- traverse buildTransitionEntry (edges complexValue)
  pure
    Atlas
      { atlasCharts = chartMap,
        atlasTransitions = Map.fromList transitionEntries
      }
  where
    buildTransitionEntry edgeValue = do
      connectionValue <-
        case Map.lookup edgeValue edgeConnectionMap of
          Just value -> Right value
          Nothing -> Left (MissingAtlasEdgeConnection edgeValue)
      let (sourceVertex, targetVertex) = edgeBoundary complexValue edgeValue
      sourceChart <-
        case Map.lookup sourceVertex chartMap of
          Just value -> Right value
          Nothing -> Left (MissingAtlasChart sourceVertex)
      targetChart <-
        case Map.lookup targetVertex chartMap of
          Just value -> Right value
          Nothing -> Left (MissingAtlasChart targetVertex)
      transitionValue <-
        first (AtlasChartError edgeValue)
          (deriveChartTransition @d sourceChart targetChart connectionValue)
      pure (edgeValue, transitionValue)

atlasTransitionPath ::
  forall d vertex edge.
  (Eq vertex, Ord edge, ChartAmbient d, SpecialOrthogonal d) =>
  Atlas edge d vertex ->
  [(edge, Orientation)] ->
  Maybe (ChartTransition d vertex)
atlasTransitionPath atlasValue stepsValue =
  case stepsValue of
    [] ->
      Nothing
    (firstEdge, firstOrientation) : remainingSteps -> do
      firstTransition <-
        orientChartTransition @d firstOrientation
          <$> lookupAtlasTransition firstEdge atlasValue
      foldM extendTransition firstTransition remainingSteps
  where
    extendTransition accumulatedTransition (edgeValue, orientationValue) = do
      nextTransition <-
        orientChartTransition @d orientationValue
          <$> lookupAtlasTransition edgeValue atlasValue
      composeChartTransitions @d nextTransition accumulatedTransition

requireChartCoordinate ::
  Ord vertex =>
  vertex ->
  CoordinateChart d vertex ->
  Either (ChartError vertex) (VecN d)
requireChartCoordinate vertexValue chartValue =
  case chartCoordinate vertexValue chartValue of
    Just coordinateValue ->
      Right coordinateValue
    Nothing ->
      Left (MissingChartCoordinate (chartCenter chartValue) vertexValue)

epsilon :: Double
epsilon = 1.0e-9
