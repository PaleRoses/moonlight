{-# LANGUAGE TupleSections #-}

module Moonlight.Control.Scheduling.Successor
  ( SuccessorAlgebra (..),
    ObstructionOverlay (..),
    SuccessorNode (..),
    SuccessorNodeKey,
    SuccessorEdge (..),
    SuccessorEdgeKey,
    SuccessorCompositionObstruction (..),
    SuccessorComplex (..),
    BackoffInfluenceEnvelope (..),
    SchedulerInfluence (..),
    SpectralSchedulingEvidence (..),
    GradedObstructionCluster (..),
    InfluenceComplex (..),
    buildSuccessorComplex,
    buildInfluenceComplex,
    influenceFromSuccessorComplex,
    findSuccessorEdge,
    findSuccessorNode,
    runtimeRulesForRule,
    spectralSchedulingEvidence,
    successorAdjacencyMap,
    spectralSchedulingOverlay,
    successorInfluencePriorityObservation,
    schedulerInfluenceWeightRatio,
    schedulerInfluenceEdgeCount,
    successorCompositionObstructionCount,
    successorEdgeCount,
    successorNodeCount,
  )
where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as AdjacencyMap
import Data.Either (partitionEithers)
import Data.Function ((&))
import Data.Containers.ListUtils (nubOrdOn)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))
import Numeric.Natural (Natural)
import Moonlight.Control.Weight
  ( PriorityObservation,
    priorityProfileFromList,
    structuralPriorityEvidence,
  )
import Moonlight.Control.Schedule
  ( bcCooldownRounds,
    bcMatchLimit,
    ScheduleOrder (..),
    SchedulerConfig (..),
    canonicalBackoffConfig,
  )
import Moonlight.Homology
  ( HomologicalDegree,
    Bidegree,
    SpectralPage (..),
    bidegreeCoordinates,
    bidegreeTotalDegree,
    convergenceDepth,
    freeRank,
  )
import Moonlight.Homology.Topology (Graph1Skeleton, graphFromEdgeSupports)

data SuccessorAlgebra system context rule runtimeRule composite compositionObstruction = SuccessorAlgebra
  { saContexts :: system -> [context],
    saContextLeq :: system -> context -> context -> Bool,
    saRulesInContext :: system -> context -> [rule],
    saCandidateTargetRules :: system -> context -> [rule] -> rule -> [rule],
    saRestrictRule :: system -> context -> context -> rule -> Maybe rule,
    saComposeRules :: system -> context -> rule -> rule -> Either compositionObstruction composite,
    saRuntimeRule :: system -> rule -> runtimeRule
  }

data SuccessorNode context rule runtimeRule = SuccessorNode
  { snContext :: !context,
    snRule :: !rule,
    snRuntimeRuleIdentity :: !runtimeRule
  }

instance (Eq context, Eq rule) => Eq (SuccessorNode context rule runtimeRule) where
  leftNode == rightNode =
    (snContext leftNode, snRule leftNode)
      ==
    (snContext rightNode, snRule rightNode)

instance (Show context, Show rule) => Show (SuccessorNode context rule runtimeRule) where
  show nodeValue =
    show (snContext nodeValue, snRule nodeValue)

data SuccessorEdge context rule runtimeRule composite = SuccessorEdge
  { seSource :: !(SuccessorNode context rule runtimeRule),
    seTarget :: !(SuccessorNode context rule runtimeRule),
    seComposite :: !composite
  }

instance (Eq context, Eq rule) => Eq (SuccessorEdge context rule runtimeRule composite) where
  leftEdge == rightEdge =
    (seSource leftEdge, seTarget leftEdge)
      ==
    (seSource rightEdge, seTarget rightEdge)

instance (Show context, Show rule) => Show (SuccessorEdge context rule runtimeRule composite) where
  show edgeValue =
    show (seSource edgeValue, seTarget edgeValue)

data SuccessorCompositionObstruction context rule compositionObstruction = SuccessorCompositionObstruction
  { scoSourceContext :: !context,
    scoTargetContext :: !context,
    scoSourceRule :: !rule,
    scoRestrictedSourceRule :: !rule,
    scoTargetRule :: !rule,
    scoCompositionObstruction :: !compositionObstruction
  }
  deriving stock (Eq, Show)

data SuccessorComplex context rule runtimeRule composite compositionObstruction = SuccessorComplex
  { rscNodes :: ![SuccessorNode context rule runtimeRule],
    rscEdges :: ![SuccessorEdge context rule runtimeRule composite],
    rscCompositionObstructions :: ![SuccessorCompositionObstruction context rule compositionObstruction],
    rscNodeOrdinals :: !(Map.Map (SuccessorNodeKey context rule) Int),
    rscNodeIndex :: !(Map.Map (SuccessorNodeKey context rule) (SuccessorNode context rule runtimeRule)),
    rscEdgeIndex :: !(Map.Map (SuccessorEdgeKey context rule) (SuccessorEdge context rule runtimeRule composite)),
    rscOutgoingEdgeCounts :: !(Map.Map (SuccessorNodeKey context rule) Int),
    rscUndirectedSkeleton :: !Graph1Skeleton
  }

data BackoffInfluenceEnvelope = BackoffInfluenceEnvelope
  { bieMatchLimit :: !Natural,
    bieCooldownRounds :: !Int,
    bieSharedOutgoingEdges :: !Int
  }
  deriving stock (Eq, Ord, Show, Read)

data SchedulerInfluence
  = DeterministicInfluence
  | BackoffInfluence !BackoffInfluenceEnvelope
  deriving stock (Eq, Ord, Show, Read)

data GradedObstructionCluster runtimeRule = GradedObstructionCluster
  { gocDegree :: !HomologicalDegree,
    gocRules :: ![runtimeRule],
    gocCocycleNorm :: !Rational
  }
  deriving stock (Eq, Show)

data SpectralSchedulingEvidence runtimeRule = SpectralSchedulingEvidence
  { sseStablePageIndex :: !Int,
    sseRuleObstructionWeights :: ![(runtimeRule, Rational)]
  }
  deriving stock (Eq, Show)

data InfluenceComplex key context rule runtimeRule composite compositionObstruction = InfluenceComplex
  { ricSuccessorComplex :: !(SuccessorComplex context rule runtimeRule composite compositionObstruction),
    ricSchedulerConfig :: !(SchedulerConfig key),
    ricEdgeInfluences :: ![(SuccessorEdge context rule runtimeRule composite, SchedulerInfluence)],
    ricGradedObstructionClusters :: ![GradedObstructionCluster runtimeRule]
  }

data ObstructionOverlay key obstruction cell context rule runtimeRule composite compositionObstruction = ObstructionOverlay
  { ooDegree :: obstruction -> HomologicalDegree,
    ooSupportingCells :: obstruction -> [cell],
    ooNorm :: obstruction -> Rational,
    ooRulesForCell :: InfluenceComplex key context rule runtimeRule composite compositionObstruction -> cell -> [runtimeRule]
  }

type SuccessorNodeKey context rule = (context, rule)

type SuccessorEdgeKey context rule =
  (SuccessorNodeKey context rule, SuccessorNodeKey context rule)

type SuccessorCompositionObstructionKey context rule =
  (context, context, rule, rule, rule)

type SuccessorContextRuleRows context rule = [(context, [rule])]

data SuccessorEdgeArtifacts context rule runtimeRule composite compositionObstruction = SuccessorEdgeArtifacts
  { seaEdges :: ![SuccessorEdge context rule runtimeRule composite],
    seaCompositionObstructions :: ![SuccessorCompositionObstruction context rule compositionObstruction]
  }

buildSuccessorComplex ::
  (Ord context, Ord rule) =>
  SuccessorAlgebra system context rule runtimeRule composite compositionObstruction ->
  system ->
  SuccessorComplex context rule runtimeRule composite compositionObstruction
buildSuccessorComplex algebra systemValue =
  let contextRuleRows = successorContextRuleRows algebra systemValue
      nodes = successorNodesFromContextRuleRows algebra systemValue contextRuleRows
      edgeArtifacts = successorEdgeArtifactsFromContextRuleRows algebra systemValue contextRuleRows
   in successorComplexFromNodesAndEdges
        nodes
        (seaEdges edgeArtifacts)
        (seaCompositionObstructions edgeArtifacts)

successorComplexFromNodesAndEdges ::
  (Ord context, Ord rule) =>
  [SuccessorNode context rule runtimeRule] ->
  [SuccessorEdge context rule runtimeRule composite] ->
  [SuccessorCompositionObstruction context rule compositionObstruction] ->
  SuccessorComplex context rule runtimeRule composite compositionObstruction
successorComplexFromNodesAndEdges nodes edges compositionObstructions =
  let nodeOrdinals = successorNodeOrdinals nodes
      edgeSupports =
        edges
          & mapMaybe (successorEdgeSupport nodeOrdinals)
   in SuccessorComplex
        { rscNodes = nodes,
          rscEdges = edges,
          rscCompositionObstructions = compositionObstructions,
          rscNodeOrdinals = nodeOrdinals,
          rscNodeIndex = successorNodeIndex nodes,
          rscEdgeIndex = successorEdgeIndex edges,
          rscOutgoingEdgeCounts = successorOutgoingCounts edges,
          rscUndirectedSkeleton = graphFromEdgeSupports (length nodes) edgeSupports
        }

buildInfluenceComplex ::
  (Ord context, Ord rule) =>
  SchedulerConfig key ->
  SuccessorAlgebra system context rule runtimeRule composite compositionObstruction ->
  system ->
  InfluenceComplex key context rule runtimeRule composite compositionObstruction
buildInfluenceComplex schedulerConfig algebra systemValue =
  influenceFromSuccessorComplex
    schedulerConfig
    (buildSuccessorComplex algebra systemValue)

influenceFromSuccessorComplex ::
  (Ord context, Ord rule) =>
  SchedulerConfig key ->
  SuccessorComplex context rule runtimeRule composite compositionObstruction ->
  InfluenceComplex key context rule runtimeRule composite compositionObstruction
influenceFromSuccessorComplex schedulerConfig successorComplex =
  let outgoingEdgeCount =
        successorOutgoingEdgeCount successorComplex
      influencedEdge edgeValue =
        ( edgeValue,
          schedulerInfluenceFor schedulerConfig (outgoingEdgeCount (seSource edgeValue))
        )
   in InfluenceComplex
        { ricSuccessorComplex = successorComplex,
          ricSchedulerConfig = schedulerConfig,
          ricEdgeInfluences = fmap influencedEdge (rscEdges successorComplex),
          ricGradedObstructionClusters = []
        }

successorNodeCount :: SuccessorComplex context rule runtimeRule composite compositionObstruction -> Int
successorNodeCount =
  length . rscNodes

successorEdgeCount :: SuccessorComplex context rule runtimeRule composite compositionObstruction -> Int
successorEdgeCount =
  length . rscEdges

successorCompositionObstructionCount :: SuccessorComplex context rule runtimeRule composite compositionObstruction -> Int
successorCompositionObstructionCount =
  length . rscCompositionObstructions

schedulerInfluenceEdgeCount :: InfluenceComplex key context rule runtimeRule composite compositionObstruction -> Int
schedulerInfluenceEdgeCount =
  length . ricEdgeInfluences

successorInfluencePriorityObservation ::
  Ord key =>
  (runtimeRule -> Maybe ruleId) ->
  (ruleId -> key) ->
  PriorityObservation
    (InfluenceComplex key context rule runtimeRule composite compositionObstruction)
    key
successorInfluencePriorityObservation runtimeRuleIdOf ruleKeyOf influenceComplex =
  priorityProfileFromList
    [ (ruleKeyOf ruleId, structuralPriorityEvidence (schedulerInfluenceStructuralWeight influenceValue))
    | (edgeValue, influenceValue) <- ricEdgeInfluences influenceComplex,
      Just ruleId <- [runtimeRuleIdOf (snRuntimeRuleIdentity (seSource edgeValue))]
    ]

-- Structural evidence is the positive ceiling of scheduler influence in thousandths.
schedulerInfluenceStructuralWeight :: SchedulerInfluence -> Int
schedulerInfluenceStructuralWeight =
  ceiling . (* 1000) . schedulerInfluenceWeightRatio

schedulerInfluenceWeightRatio :: SchedulerInfluence -> Rational
schedulerInfluenceWeightRatio influenceValue =
  case influenceValue of
    DeterministicInfluence ->
      1
    BackoffInfluence envelope ->
      let sharedOutgoingEdges =
            max 1 (bieSharedOutgoingEdges envelope)
          matchLimit =
            max (1 :: Natural) (bieMatchLimit envelope)
          denominator =
            toInteger sharedOutgoingEdges
              * toInteger (max 1 (bieCooldownRounds envelope + 1))
       in min 1 (toInteger matchLimit % denominator)

findSuccessorNode ::
  (Ord context, Ord rule) =>
  SuccessorComplex context rule runtimeRule composite compositionObstruction ->
  context ->
  rule ->
  Maybe (SuccessorNode context rule runtimeRule)
findSuccessorNode successorComplex contextValue ruleValue =
  Map.lookup
    (contextValue, ruleValue)
    (rscNodeIndex successorComplex)

findSuccessorEdge ::
  (Ord context, Ord rule) =>
  SuccessorComplex context rule runtimeRule composite compositionObstruction ->
  SuccessorNode context rule runtimeRule ->
  SuccessorNode context rule runtimeRule ->
  Maybe (SuccessorEdge context rule runtimeRule composite)
findSuccessorEdge successorComplex sourceNode targetNode =
  Map.lookup
    (successorNodeKey sourceNode, successorNodeKey targetNode)
    (rscEdgeIndex successorComplex)

successorAdjacencyMap ::
  (Ord context, Ord rule) =>
  SuccessorComplex context rule runtimeRule composite compositionObstruction ->
  AdjacencyMap (SuccessorNodeKey context rule)
successorAdjacencyMap successorComplex =
  AdjacencyMap.overlay
    (AdjacencyMap.vertices (fmap successorNodeKey (rscNodes successorComplex)))
    (AdjacencyMap.edges (fmap successorEdgeKey (rscEdges successorComplex)))

runtimeRulesForRule ::
  Eq rule =>
  InfluenceComplex key context rule runtimeRule composite compositionObstruction ->
  rule ->
  [runtimeRule]
runtimeRulesForRule influenceComplex ruleValue =
  rscNodes (ricSuccessorComplex influenceComplex)
    & filter ((== ruleValue) . snRule)
    & fmap snRuntimeRuleIdentity

spectralSchedulingOverlay ::
  Eq runtimeRule =>
  ObstructionOverlay key obstruction cell context rule runtimeRule composite compositionObstruction ->
  [SpectralPage Rational] ->
  [[obstruction]] ->
  InfluenceComplex key context rule runtimeRule composite compositionObstruction ->
  InfluenceComplex key context rule runtimeRule composite compositionObstruction
spectralSchedulingOverlay obstructionOverlay spectralPages obstructionLayers influenceComplex =
  influenceComplex
    { ricGradedObstructionClusters =
        foldMap
          (mapMaybe (obstructionCluster obstructionOverlay spectralPages influenceComplex))
          obstructionLayers
    }

spectralSchedulingEvidence ::
  Ord runtimeRule =>
  [SpectralPage Rational] ->
  [GradedObstructionCluster runtimeRule] ->
  SpectralSchedulingEvidence runtimeRule
spectralSchedulingEvidence spectralPages clusters =
  SpectralSchedulingEvidence
    { sseStablePageIndex = convergenceDepth spectralPages,
      sseRuleObstructionWeights = orderedClusterWeights clusters
    }

obstructionCluster ::
  Eq runtimeRule =>
  ObstructionOverlay key obstruction cell context rule runtimeRule composite compositionObstruction ->
  [SpectralPage Rational] ->
  InfluenceComplex key context rule runtimeRule composite compositionObstruction ->
  obstruction ->
  Maybe (GradedObstructionCluster runtimeRule)
obstructionCluster obstructionOverlay spectralPages influenceComplex obstructionValue =
  let runtimeRules =
        ooSupportingCells obstructionOverlay obstructionValue
          & foldMap (ooRulesForCell obstructionOverlay influenceComplex)
          & nub
   in if null runtimeRules
        then Nothing
        else
          Just
            GradedObstructionCluster
              { gocDegree = ooDegree obstructionOverlay obstructionValue,
                gocRules = runtimeRules,
                gocCocycleNorm =
                  ooNorm obstructionOverlay obstructionValue
                    * spectralDegreeWeight spectralPages (ooDegree obstructionOverlay obstructionValue)
              }

orderedClusterWeights ::
  Ord runtimeRule =>
  [GradedObstructionCluster runtimeRule] ->
  [(runtimeRule, Rational)]
orderedClusterWeights clusters =
  let weightsByRule =
        Map.fromListWith
          (+)
          [ (runtimeRuleValue, gocCocycleNorm cluster)
          | cluster <- clusters,
            runtimeRuleValue <- gocRules cluster
          ]
   in [ (runtimeRuleValue, weightValue)
      | runtimeRuleValue <- nubOrdOn id (foldMap gocRules clusters),
        Just weightValue <- [Map.lookup runtimeRuleValue weightsByRule]
      ]

spectralDegreeWeight :: [SpectralPage Rational] -> HomologicalDegree -> Rational
spectralDegreeWeight spectralPages degreeValue =
  maybe
    1
    (max 1 . pageDegreeRank degreeValue)
    (activeSpectralPage spectralPages)

activeSpectralPage :: [SpectralPage Rational] -> Maybe (SpectralPage Rational)
activeSpectralPage spectralPages =
  case reverse spectralPages of
    activePage : _ -> Just activePage
    [] -> Nothing

pageDegreeRank :: HomologicalDegree -> SpectralPage Rational -> Rational
pageDegreeRank degreeValue spectralPage =
  spectralPage
    & pageEntryMap
    & Map.keys
    & filter ((== degreeValue) . bidegreeTotalDegree)
    & fmap (pageBidegreeRank spectralPage)
    & sum

pageBidegreeRank :: SpectralPage Rational -> Bidegree -> Rational
pageBidegreeRank spectralPage bidegreeValue =
  let (filtrationDegreeValue, complementaryDegreeValue) = bidegreeCoordinates bidegreeValue
   in fromIntegral (freeRank (groupAt spectralPage filtrationDegreeValue complementaryDegreeValue))

successorNodesFromContextRuleRows ::
  (Ord context, Ord rule) =>
  SuccessorAlgebra system context rule runtimeRule composite compositionObstruction ->
  system ->
  SuccessorContextRuleRows context rule ->
  [SuccessorNode context rule runtimeRule]
successorNodesFromContextRuleRows algebra systemValue contextRuleRows =
  nubOrdOn
    successorNodeKey
    ( contextRuleRows
        >>= \(contextValue, ruleValues) ->
          ruleValues
            & fmap (mkSuccessorNode algebra systemValue contextValue)
    )

successorEdgeArtifactsFromContextRuleRows ::
  (Ord context, Ord rule) =>
  SuccessorAlgebra system context rule runtimeRule composite compositionObstruction ->
  system ->
  SuccessorContextRuleRows context rule ->
  SuccessorEdgeArtifacts context rule runtimeRule composite compositionObstruction
successorEdgeArtifactsFromContextRuleRows algebra systemValue contextRuleRows =
  let (compositionObstructions, edges) =
        partitionEithers (successorEdgeOutcomesFromContextRuleRows algebra systemValue contextRuleRows)
   in SuccessorEdgeArtifacts
        { seaEdges = nubOrdOn successorEdgeKey edges,
          seaCompositionObstructions =
            nubOrdOn successorCompositionObstructionKey compositionObstructions
        }

successorEdgeOutcomesFromContextRuleRows ::
  Eq context =>
  SuccessorAlgebra system context rule runtimeRule composite compositionObstruction ->
  system ->
  SuccessorContextRuleRows context rule ->
  [Either (SuccessorCompositionObstruction context rule compositionObstruction) (SuccessorEdge context rule runtimeRule composite)]
successorEdgeOutcomesFromContextRuleRows algebra systemValue contextRuleRows =
  contextRuleRows
    >>= \(sourceContext, sourceRules) ->
      contextRuleRows
        & filter (\(targetContext, _) -> saContextLeq algebra systemValue targetContext sourceContext)
        >>= \(targetContext, targetRules) ->
          sourceRules
            >>= \sourceRule ->
              maybe
                []
                ( \restrictedRule ->
                    saCandidateTargetRules algebra systemValue targetContext targetRules restrictedRule
                      >>= groundedSuccessor sourceContext targetContext sourceRule restrictedRule
                )
                (restrictInto algebra systemValue sourceContext targetContext sourceRule)
  where
    groundedSuccessor sourceContext targetContext sourceRule restrictedRule targetRule =
      pure
        ( case saComposeRules algebra systemValue targetContext targetRule restrictedRule of
            Left compositionObstruction ->
              Left
                SuccessorCompositionObstruction
                  { scoSourceContext = sourceContext,
                    scoTargetContext = targetContext,
                    scoSourceRule = sourceRule,
                    scoRestrictedSourceRule = restrictedRule,
                    scoTargetRule = targetRule,
                    scoCompositionObstruction = compositionObstruction
                  }
            Right composedRule ->
              Right
                SuccessorEdge
                  { seSource = mkSuccessorNode algebra systemValue sourceContext sourceRule,
                    seTarget = mkSuccessorNode algebra systemValue targetContext targetRule,
                    seComposite = composedRule
                  }
        )

mkSuccessorNode ::
  SuccessorAlgebra system context rule runtimeRule composite compositionObstruction ->
  system ->
  context ->
  rule ->
  SuccessorNode context rule runtimeRule
mkSuccessorNode algebra systemValue contextValue ruleValue =
  SuccessorNode
    { snContext = contextValue,
      snRule = ruleValue,
      snRuntimeRuleIdentity = saRuntimeRule algebra systemValue ruleValue
    }

successorNodeKey :: SuccessorNode context rule runtimeRule -> SuccessorNodeKey context rule
successorNodeKey nodeValue =
  (snContext nodeValue, snRule nodeValue)

successorEdgeKey :: SuccessorEdge context rule runtimeRule composite -> SuccessorEdgeKey context rule
successorEdgeKey edgeValue =
  (successorNodeKey (seSource edgeValue), successorNodeKey (seTarget edgeValue))

successorCompositionObstructionKey ::
  SuccessorCompositionObstruction context rule compositionObstruction ->
  SuccessorCompositionObstructionKey context rule
successorCompositionObstructionKey obstructionValue =
  ( scoSourceContext obstructionValue,
    scoTargetContext obstructionValue,
    scoSourceRule obstructionValue,
    scoRestrictedSourceRule obstructionValue,
    scoTargetRule obstructionValue
  )

restrictInto ::
  Eq context =>
  SuccessorAlgebra system context rule runtimeRule composite compositionObstruction ->
  system ->
  context ->
  context ->
  rule ->
  Maybe rule
restrictInto algebra systemValue sourceContext targetContext sourceRule
  | sourceContext == targetContext = Just sourceRule
  | otherwise = saRestrictRule algebra systemValue sourceContext targetContext sourceRule

successorContextRuleRows ::
  SuccessorAlgebra system context rule runtimeRule composite compositionObstruction ->
  system ->
  SuccessorContextRuleRows context rule
successorContextRuleRows algebra systemValue =
  saContexts algebra systemValue
    & fmap
      ( \contextValue ->
          (contextValue, saRulesInContext algebra systemValue contextValue)
      )

successorNodeOrdinals ::
  (Ord context, Ord rule) =>
  [SuccessorNode context rule runtimeRule] ->
  Map.Map (SuccessorNodeKey context rule) Int
successorNodeOrdinals nodes =
  zip [0 :: Int ..] nodes
    & fmap (\(nodeOrdinal, nodeValue) -> (successorNodeKey nodeValue, nodeOrdinal))
    & Map.fromListWith min

successorNodeIndex ::
  (Ord context, Ord rule) =>
  [SuccessorNode context rule runtimeRule] ->
  Map.Map (SuccessorNodeKey context rule) (SuccessorNode context rule runtimeRule)
successorNodeIndex =
  Map.fromListWith keepFirst . fmap (\nodeValue -> (successorNodeKey nodeValue, nodeValue))

successorEdgeIndex ::
  (Ord context, Ord rule) =>
  [SuccessorEdge context rule runtimeRule composite] ->
  Map.Map (SuccessorEdgeKey context rule) (SuccessorEdge context rule runtimeRule composite)
successorEdgeIndex =
  Map.fromListWith keepFirst . fmap (\edgeValue -> (successorEdgeKey edgeValue, edgeValue))

keepFirst :: value -> value -> value
keepFirst _newValue existingValue =
  existingValue

successorEdgeSupport ::
  (Ord context, Ord rule) =>
  Map.Map (SuccessorNodeKey context rule) Int ->
  SuccessorEdge context rule runtimeRule composite ->
  Maybe (Int, Int)
successorEdgeSupport nodeIndex edgeValue =
  (,)
    <$> Map.lookup (successorNodeKey (seSource edgeValue)) nodeIndex
    <*> Map.lookup (successorNodeKey (seTarget edgeValue)) nodeIndex

successorOutgoingCounts ::
  (Ord context, Ord rule) =>
  [SuccessorEdge context rule runtimeRule composite] ->
  Map.Map (SuccessorNodeKey context rule) Int
successorOutgoingCounts =
  Map.fromListWith (+)
    . fmap (\edgeValue -> (successorNodeKey (seSource edgeValue), 1 :: Int))

successorOutgoingEdgeCount ::
  (Ord context, Ord rule) =>
  SuccessorComplex context rule runtimeRule composite compositionObstruction ->
  SuccessorNode context rule runtimeRule ->
  Int
successorOutgoingEdgeCount successorComplex sourceNode =
  Map.findWithDefault
    0
    (successorNodeKey sourceNode)
    (rscOutgoingEdgeCounts successorComplex)

schedulerInfluenceFor :: SchedulerConfig key -> Int -> SchedulerInfluence
schedulerInfluenceFor schedulerConfig outgoingEdgeCount =
  case scOrder schedulerConfig of
    ByRuleIdThenSubstitution ->
      DeterministicInfluence
    DeficitRoundRobin {} ->
      DeterministicInfluence
    BackoffByGroup rawBackoffConfig ->
      let canonicalBackoffPolicy =
            canonicalBackoffConfig rawBackoffConfig
       in BackoffInfluence
            BackoffInfluenceEnvelope
              { bieMatchLimit = bcMatchLimit canonicalBackoffPolicy,
                bieCooldownRounds = bcCooldownRounds canonicalBackoffPolicy,
                bieSharedOutgoingEdges = outgoingEdgeCount
              }
