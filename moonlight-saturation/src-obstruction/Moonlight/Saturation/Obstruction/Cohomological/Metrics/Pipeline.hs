module Moonlight.Saturation.Obstruction.Cohomological.Metrics.Pipeline
  ( PipelineMetrics,
    pipelineStageCounts,
    emptyPipelineMetrics,
    singletonPipelineMetric,
    pipelineMetricsFromList,
    pipelineMetricsFromNonNegativeIntList,
    pipelineMetricCount,
    insertPipelineMetric,
    incrementPipelineMetric,
    pipelineCountFromInt,
  )
where

import Data.Bifunctor (second)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Numeric.Natural (Natural)

type PipelineMetrics :: Type -> Type
newtype PipelineMetrics stage = PipelineMetrics
  { pipelineStageCounts :: Map stage Natural
  }
  deriving stock (Eq, Ord, Show, Read)

instance Ord stage => Semigroup (PipelineMetrics stage) where
  PipelineMetrics leftCounts <> PipelineMetrics rightCounts =
    PipelineMetrics (Map.unionWith (+) leftCounts rightCounts)

instance Ord stage => Monoid (PipelineMetrics stage) where
  mempty =
    emptyPipelineMetrics

emptyPipelineMetrics :: PipelineMetrics stage
emptyPipelineMetrics =
  PipelineMetrics Map.empty

singletonPipelineMetric ::
  Ord stage =>
  stage ->
  Natural ->
  PipelineMetrics stage
singletonPipelineMetric stage countValue =
  insertPipelineMetric stage countValue emptyPipelineMetrics

pipelineMetricsFromList ::
  Ord stage =>
  [(stage, Natural)] ->
  PipelineMetrics stage
pipelineMetricsFromList =
  PipelineMetrics
    . Map.filter (> 0)
    . Map.fromListWith (+)

pipelineMetricsFromNonNegativeIntList ::
  Ord stage =>
  [(stage, Int)] ->
  PipelineMetrics stage
pipelineMetricsFromNonNegativeIntList =
  pipelineMetricsFromList
    . fmap (second pipelineCountFromInt)

pipelineMetricCount ::
  Ord stage =>
  stage ->
  PipelineMetrics stage ->
  Natural
pipelineMetricCount stage =
  Map.findWithDefault 0 stage . pipelineStageCounts

insertPipelineMetric ::
  Ord stage =>
  stage ->
  Natural ->
  PipelineMetrics stage ->
  PipelineMetrics stage
insertPipelineMetric stage countValue metrics =
  PipelineMetrics $
    if countValue == 0
      then Map.delete stage (pipelineStageCounts metrics)
      else Map.insert stage countValue (pipelineStageCounts metrics)

incrementPipelineMetric ::
  Ord stage =>
  stage ->
  Natural ->
  PipelineMetrics stage ->
  PipelineMetrics stage
incrementPipelineMetric stage delta metrics =
  insertPipelineMetric
    stage
    (pipelineMetricCount stage metrics + delta)
    metrics

pipelineCountFromInt :: Int -> Natural
pipelineCountFromInt countValue
  | countValue <= 0 =
      0
  | otherwise =
      fromIntegral countValue
