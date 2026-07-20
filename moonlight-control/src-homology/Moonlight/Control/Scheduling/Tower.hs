module Moonlight.Control.Scheduling.Tower
  ( spectralSchedulingPriorityObservation,
    towerWarningClusters,
  )
where

import Moonlight.Homology (HomologicalDegree (..))
import Moonlight.Control.Weight
  ( PriorityObservation,
    PriorityProfile,
    nonCriticalPriorityRank,
    priorityEvidence,
    singletonPriorityProfile,
  )
import Moonlight.Control.Scheduling.Successor
  ( GradedObstructionCluster (..),
    InfluenceComplex (..),
  )

spectralSchedulingPriorityObservation ::
  Ord key =>
  (runtimeRule -> Maybe ruleId) ->
  (ruleId -> key) ->
  PriorityObservation
    (InfluenceComplex key context rule runtimeRule composite compositionObstruction)
    key
spectralSchedulingPriorityObservation ruleIdOf ruleKeyOf influenceComplex =
  foldMap
    (clusterPriorityProfile ruleIdOf ruleKeyOf maximumDegree)
    (ricGradedObstructionClusters influenceComplex)
  where
    clusters = ricGradedObstructionClusters influenceComplex
    maximumDegree = foldr (max . clusterDegreeIndex) 1 clusters

towerWarningClusters :: Rational -> InfluenceComplex key context rule runtimeRule composite compositionObstruction -> [GradedObstructionCluster runtimeRule]
towerWarningClusters warningThreshold =
  filter ((>= warningThreshold) . gocCocycleNorm) . ricGradedObstructionClusters

clusterPriorityProfile ::
  Ord key =>
  (runtimeRule -> Maybe ruleId) ->
  (ruleId -> key) ->
  Int ->
  GradedObstructionCluster runtimeRule ->
  PriorityProfile key
clusterPriorityProfile ruleIdOf ruleKeyOf maximumDegree clusterValue =
  foldMap
    ( \runtimeRule ->
        maybe
          mempty
          ( \ruleId ->
              singletonPriorityProfile (ruleKeyOf ruleId) rulePriority
          )
          (ruleIdOf runtimeRule)
    )
    (gocRules clusterValue)
  where
    rulePriority =
      priorityEvidence
        clusterWidth
        passRank
        normRank
        nonCriticalPriorityRank
    passRank =
      max 1 (maximumDegree - clusterDegreeIndex clusterValue + 1)
    normRank =
      ceiling (gocCocycleNorm clusterValue)
    clusterWidth =
      max 0 (length (gocRules clusterValue) - 1)

clusterDegreeIndex :: GradedObstructionCluster runtimeRule -> Int
clusterDegreeIndex clusterValue =
  case gocDegree clusterValue of
    HomologicalDegree degreeIndex -> degreeIndex
