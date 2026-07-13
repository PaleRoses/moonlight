{-# LANGUAGE TupleSections #-}

module Moonlight.Control.Scheduling.Successor.Runtime
  ( RuleRuntimeProjection (..),
    RuntimeInfluenceEvidence (..),
    RuntimeWeightedEdge (..),
    RuntimeSuccessorAnnotation (..),
    RuntimeAnnotatedSuccessorComplex (..),
    runtimeAnnotatedSuccessorComplexWithProjection,
    runtimeObservedNodeCount,
    runtimeObservedNodes,
    runtimeWeightedEdgeCount,
    runtimeWeightedEdges,
    runtimeTransitionPriorityObservation,
    unobservedStructuralEdgeCount,
    unobservedStructuralEdges,
  )
where

import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Moonlight.Control.Weight
  ( PriorityObservation,
    observedTransitionPriorityEvidence,
    priorityProfileFromList,
  )

newtype RuleRuntimeProjection node rule = RuleRuntimeProjection
  { projectRuntimeRuleId :: node -> Maybe rule
  }

data RuntimeInfluenceEvidence transition outcome = RuntimeInfluenceEvidence
  { rieTransitions :: !transition,
    rieSourceOutcomes :: !(Maybe outcome),
    rieTargetOutcomes :: !(Maybe outcome)
  }
  deriving stock (Eq, Show)

data RuntimeWeightedEdge edge transition outcome = RuntimeWeightedEdge
  { rweStructuralEdge :: !edge,
    rweEvidence :: !(RuntimeInfluenceEvidence transition outcome)
  }
  deriving stock (Eq, Show)

data RuntimeSuccessorAnnotation outcomeSummary transitionSummary outcome transition = RuntimeSuccessorAnnotation
  { rsaOutcomeSummary :: !outcomeSummary,
    rsaTransitionSummary :: !transitionSummary,
    rsaStructuralNodeCount :: !Int,
    rsaStructuralEdgeCount :: !Int,
    rsaObservedNodeIndices :: !IntSet,
    rsaEdgeEvidence :: !(IntMap (RuntimeInfluenceEvidence transition outcome))
  }
  deriving stock (Eq, Show)

data RuntimeAnnotatedSuccessorComplex base outcomeSummary transitionSummary outcome transition = RuntimeAnnotatedSuccessorComplex
  { rascBase :: !base,
    rascRuntime :: !(RuntimeSuccessorAnnotation outcomeSummary transitionSummary outcome transition)
  }
  deriving stock (Eq, Show)

runtimeAnnotatedSuccessorComplexWithProjection ::
  Ord rule =>
  RuleRuntimeProjection node rule ->
  (outcomeSummary -> [outcome]) ->
  (outcome -> rule) ->
  (transitionSummary -> [transition]) ->
  (transition -> rule) ->
  (transition -> rule) ->
  (base -> [node]) ->
  (base -> [edge]) ->
  (edge -> node) ->
  (edge -> node) ->
  outcomeSummary ->
  transitionSummary ->
  base ->
  RuntimeAnnotatedSuccessorComplex base outcomeSummary transitionSummary (NonEmpty outcome) (NonEmpty transition)
runtimeAnnotatedSuccessorComplexWithProjection projectionValue outcomesOf outcomeRule transitionsOf transitionSourceRule transitionTargetRule nodesOf edgesOf edgeSource edgeTarget outcomeSummary transitionSummary base =
  let nodes =
        nodesOf base
      edges =
        edgesOf base
      outcomeByRule =
        outcomeMap outcomesOf outcomeRule outcomeSummary
      transitionByRulePair =
        transitionMap transitionsOf transitionSourceRule transitionTargetRule transitionSummary
      observedNodeIndices =
        IntSet.fromAscList
          [ nodeIndex
          | (nodeIndex, nodeValue) <- indexed nodes,
            isObservedNode projectionValue outcomeByRule nodeValue
          ]
      edgeEvidence =
        edges
          & indexed
          & mapMaybe
            ( \(edgeIndex, edgeValue) ->
                fmap
                  (edgeIndex,)
                  ( weightedEdgeEvidence
                      projectionValue
                      outcomeByRule
                      transitionByRulePair
                      edgeSource
                      edgeTarget
                      edgeValue
                  )
            )
          & IntMap.fromAscList
   in RuntimeAnnotatedSuccessorComplex
        { rascBase = base,
          rascRuntime =
            RuntimeSuccessorAnnotation
              { rsaOutcomeSummary = outcomeSummary,
                rsaTransitionSummary = transitionSummary,
                rsaStructuralNodeCount = length nodes,
                rsaStructuralEdgeCount = length edges,
                rsaObservedNodeIndices = observedNodeIndices,
                rsaEdgeEvidence = edgeEvidence
              }
        }

runtimeObservedNodeCount ::
  RuntimeAnnotatedSuccessorComplex base outcomeSummary transitionSummary outcome transition ->
  Int
runtimeObservedNodeCount =
  IntSet.size . rsaObservedNodeIndices . rascRuntime

runtimeObservedNodes ::
  (base -> [node]) ->
  RuntimeAnnotatedSuccessorComplex base outcomeSummary transitionSummary outcome transition ->
  [node]
runtimeObservedNodes nodesOf annotatedComplex =
  let observedNodeIndices =
        rsaObservedNodeIndices (rascRuntime annotatedComplex)
   in [ nodeValue
        | (nodeIndex, nodeValue) <- indexed (nodesOf (rascBase annotatedComplex)),
          IntSet.member nodeIndex observedNodeIndices
      ]

runtimeWeightedEdgeCount ::
  RuntimeAnnotatedSuccessorComplex base outcomeSummary transitionSummary outcome transition ->
  Int
runtimeWeightedEdgeCount =
  IntMap.size . rsaEdgeEvidence . rascRuntime

runtimeWeightedEdges ::
  (base -> [edge]) ->
  RuntimeAnnotatedSuccessorComplex base outcomeSummary transitionSummary outcome transition ->
  [RuntimeWeightedEdge edge transition outcome]
runtimeWeightedEdges edgesOf annotatedComplex =
  let edgeEvidence =
        rsaEdgeEvidence (rascRuntime annotatedComplex)
   in mapMaybe
        ( \(edgeIndex, edgeValue) ->
            fmap
              ( \evidenceValue ->
                  RuntimeWeightedEdge
                    { rweStructuralEdge = edgeValue,
                      rweEvidence = evidenceValue
                    }
              )
              (IntMap.lookup edgeIndex edgeEvidence)
        )
        (indexed (edgesOf (rascBase annotatedComplex)))

runtimeTransitionPriorityObservation ::
  Ord key =>
  (node -> Maybe key) ->
  (transition -> Int) ->
  (base -> [edge]) ->
  (edge -> node) ->
  PriorityObservation
    (RuntimeAnnotatedSuccessorComplex base outcomeSummary transitionSummary outcome transition)
    key
runtimeTransitionPriorityObservation priorityKeyOf transitionCount edgesOf edgeSource annotatedComplex =
  priorityProfileFromList
    [ ( priorityKey,
        observedTransitionPriorityEvidence
          (runtimeInfluenceTransitionCount transitionCount (rweEvidence weightedEdge))
      )
    | weightedEdge <- runtimeWeightedEdges edgesOf annotatedComplex,
      Just priorityKey <- [priorityKeyOf (edgeSource (rweStructuralEdge weightedEdge))]
    ]

unobservedStructuralEdgeCount ::
  RuntimeAnnotatedSuccessorComplex base outcomeSummary transitionSummary outcome transition ->
  Int
unobservedStructuralEdgeCount annotatedComplex =
  let runtimeAnnotation =
        rascRuntime annotatedComplex
   in max
        0
        (rsaStructuralEdgeCount runtimeAnnotation - IntMap.size (rsaEdgeEvidence runtimeAnnotation))

unobservedStructuralEdges ::
  (base -> [edge]) ->
  RuntimeAnnotatedSuccessorComplex base outcomeSummary transitionSummary outcome transition ->
  [edge]
unobservedStructuralEdges edgesOf annotatedComplex =
  let edgeEvidence =
        rsaEdgeEvidence (rascRuntime annotatedComplex)
   in [ edgeValue
        | (edgeIndex, edgeValue) <- indexed (edgesOf (rascBase annotatedComplex)),
          IntMap.notMember edgeIndex edgeEvidence
      ]

isObservedNode ::
  Ord rule =>
  RuleRuntimeProjection node rule ->
  Map rule (NonEmpty outcome) ->
  node ->
  Bool
isObservedNode projectionValue outcomeByRule nodeValue =
  maybe False (`Map.member` outcomeByRule) (projectRuntimeRuleId projectionValue nodeValue)

weightedEdgeEvidence ::
  Ord rule =>
  RuleRuntimeProjection node rule ->
  Map rule (NonEmpty outcome) ->
  Map (rule, rule) (NonEmpty transition) ->
  (edge -> node) ->
  (edge -> node) ->
  edge ->
  Maybe (RuntimeInfluenceEvidence (NonEmpty transition) (NonEmpty outcome))
weightedEdgeEvidence projectionValue outcomeByRule transitionByRulePair edgeSource edgeTarget structuralEdge = do
  transitionValues <-
    observedTransition
      projectionValue
      transitionByRulePair
      edgeSource
      edgeTarget
      structuralEdge
  pure
    RuntimeInfluenceEvidence
      { rieTransitions = transitionValues,
        rieSourceOutcomes = projectedOutcomes edgeSource,
        rieTargetOutcomes = projectedOutcomes edgeTarget
      }
  where
    projectedOutcomes selectNode =
      projectRuntimeRuleId projectionValue (selectNode structuralEdge)
        >>= (`Map.lookup` outcomeByRule)

runtimeInfluenceTransitionCount ::
  (transition -> Int) ->
  RuntimeInfluenceEvidence transition outcome ->
  Int
runtimeInfluenceTransitionCount transitionCount =
  transitionCount . rieTransitions

observedTransition ::
  Ord rule =>
  RuleRuntimeProjection node rule ->
  Map (rule, rule) (NonEmpty transition) ->
  (edge -> node) ->
  (edge -> node) ->
  edge ->
  Maybe (NonEmpty transition)
observedTransition projectionValue transitionByRulePair edgeSource edgeTarget structuralEdge = do
  sourceRule <- projectRuntimeRuleId projectionValue (edgeSource structuralEdge)
  targetRule <- projectRuntimeRuleId projectionValue (edgeTarget structuralEdge)
  Map.lookup (sourceRule, targetRule) transitionByRulePair

outcomeMap ::
  Ord rule =>
  (outcomeSummary -> [outcome]) ->
  (outcome -> rule) ->
  outcomeSummary ->
  Map rule (NonEmpty outcome)
outcomeMap outcomesOf outcomeRule =
  nonEmptyMapBy outcomeRule
    . outcomesOf

transitionMap ::
  Ord rule =>
  (transitionSummary -> [transition]) ->
  (transition -> rule) ->
  (transition -> rule) ->
  transitionSummary ->
  Map (rule, rule) (NonEmpty transition)
transitionMap transitionsOf transitionSourceRule transitionTargetRule =
  nonEmptyMapBy
    ( \transitionValue ->
        ( transitionSourceRule transitionValue,
          transitionTargetRule transitionValue
        )
    )
    . transitionsOf

nonEmptyMapBy ::
  Ord key =>
  (value -> key) ->
  [value] ->
  Map key (NonEmpty value)
nonEmptyMapBy keyOf =
  Map.fromListWith appendInputOrder
    . fmap (\value -> (keyOf value, value :| []))

appendInputOrder ::
  NonEmpty value ->
  NonEmpty value ->
  NonEmpty value
appendInputOrder newValues existingValues =
  existingValues <> newValues

indexed :: [value] -> [(Int, value)]
indexed =
  zip [0 :: Int ..]
