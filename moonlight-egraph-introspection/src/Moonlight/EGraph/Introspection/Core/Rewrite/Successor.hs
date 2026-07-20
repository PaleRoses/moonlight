{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Introspection.Core.Rewrite.Successor
  ( InfluenceComplex,
    RewriteInfluenceComplex,
    RewriteCompositionObstruction,
    RewriteRuntimeProjection,
    RewriteRuntimeWeightedSuccessorComplex,
    RewriteSuccessorComplex,
    RewriteSuccessorEdge,
    RewriteSuccessorNode,
    RuleRuntimeProjection,
    RuntimeInfluenceEvidence,
    RuntimeWeightedEdge,
    RuntimeWeightedSuccessorComplex (..),
    SuccessorComplex,
    SuccessorEdge,
    SuccessorNode,
    buildInfluenceComplex,
    buildSuccessorComplex,
    defaultRuntimeProjection,
    findSuccessorNode,
    rewriteInfluenceComplex,
    rewriteSupportRuntimeOverlayFromTrace,
    rewriteSuccessorComplex,
    runtimeWeightedEdgeCount,
    runtimeWeightedSuccessorComplexFromBase,
    runtimeWeightedSuccessorComplexFromTrace,
    runtimeWeightedSuccessorComplexWithProjection,
    schedulerInfluenceEdgeCount,
    successorEdgeCount,
    successorNodeCount,
    unobservedStructuralEdgeCount,
  )
where

import Data.Kind (Type)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (mapMaybe)
import Numeric.Natural (Natural)
import Moonlight.Core (ZipMatch (..), HasConstructorTag (..), Pattern (..), RewriteRuleId)
import Moonlight.EGraph.Introspection.Core.Rewrite
  ( RewriteContext,
    RewriteSystem,
    RuntimeRuleIdentity (..),
    resolveRuntimeRuleIdentity,
    rewriteMorphismLeft,
    rewriteMorphismRight,
  )
import Moonlight.Sheaf.Site (AnalyzableSystem (..))
import Moonlight.Rewrite.ProofContext (SupportBasis)
import Moonlight.EGraph.Introspection.Core.Rewrite (RewriteMorphism)
import Moonlight.Sheaf.Twist.Report (SupportTraceEntry (..))
import Moonlight.Pale.Diagnostic.Derived.Rewrite
  ( RewriteOutcomeSummary (..),
    RewriteTransitionSummary (..),
    summarizeRewriteTransitions,
    summarizeSaturationTrace,
  )
import Moonlight.Pale.Diagnostic.Section.Rewrite
  ( RewriteOutcomeStat (..),
    RewriteTransitionStat (..),
  )
import Moonlight.Pale.Diagnostic.Section.Saturation
  ( SaturationTrace,
  )
import Moonlight.Control.Schedule (SchedulerConfig)
import Moonlight.Control.Count
  ( workCountFromInt,
  )
import Moonlight.Control.Scheduling.Successor
  ( SuccessorAlgebra (..),
    findSuccessorNode,
    schedulerInfluenceEdgeCount,
    successorEdgeCount,
    successorNodeCount,
  )
import Moonlight.Control.Scheduling.Successor qualified as Successor
import Moonlight.Control.Scheduling.Successor.Runtime qualified as Runtime
import Moonlight.Control.Scheduling.Support qualified as Support
import Moonlight.EGraph.Introspection.Core.Rewrite (CompositionError)

type SuccessorNode :: (Type -> Type) -> Type
type SuccessorNode f =
  Successor.SuccessorNode
    (RewriteContext f)
    (RewriteMorphism f)
    RuntimeRuleIdentity

type SuccessorEdge :: (Type -> Type) -> Type
type SuccessorEdge f =
  Successor.SuccessorEdge
    (RewriteContext f)
    (RewriteMorphism f)
    RuntimeRuleIdentity
    (RewriteMorphism f)

type RewriteCompositionObstruction :: (Type -> Type) -> Type
type RewriteCompositionObstruction f = CompositionError f

type SuccessorComplex :: (Type -> Type) -> Type
type SuccessorComplex f =
  Successor.SuccessorComplex
    (RewriteContext f)
    (RewriteMorphism f)
    RuntimeRuleIdentity
    (RewriteMorphism f)
    (RewriteCompositionObstruction f)

type InfluenceComplex :: (Type -> Type) -> Type
type InfluenceComplex f =
  Successor.InfluenceComplex
    RewriteRuleId
    (RewriteContext f)
    (RewriteMorphism f)
    RuntimeRuleIdentity
    (RewriteMorphism f)
    (RewriteCompositionObstruction f)

type RewriteSuccessorNode :: (Type -> Type) -> Type
type RewriteSuccessorNode f = SuccessorNode f

type RewriteSuccessorEdge :: (Type -> Type) -> Type
type RewriteSuccessorEdge f = SuccessorEdge f

type RewriteSuccessorComplex :: (Type -> Type) -> Type
type RewriteSuccessorComplex f = SuccessorComplex f

type RewriteInfluenceComplex :: (Type -> Type) -> Type
type RewriteInfluenceComplex f = InfluenceComplex f

type RuleRuntimeProjection :: (Type -> Type) -> Type
type RuleRuntimeProjection f =
  Runtime.RuleRuntimeProjection (SuccessorNode f) RewriteRuleId

type RewriteRuntimeProjection :: (Type -> Type) -> Type
type RewriteRuntimeProjection f = RuleRuntimeProjection f

type RuntimeInfluenceEvidence :: Type -> Type -> Type
type RuntimeInfluenceEvidence =
  Runtime.RuntimeInfluenceEvidence

type RuntimeWeightedEdge :: Type -> Type -> Type -> Type
type RuntimeWeightedEdge edge transition outcome =
  Runtime.RuntimeWeightedEdge edge transition outcome

type RuntimeWeightedSuccessorComplex :: (Type -> Type) -> Type
data RuntimeWeightedSuccessorComplex f = RuntimeWeightedSuccessorComplex
  { rwscBase :: SuccessorComplex f,
    rwscOutcomeSummary :: RewriteOutcomeSummary RewriteRuleId,
    rwscTransitionSummary :: RewriteTransitionSummary RewriteRuleId,
    rwscObservedNodes :: [SuccessorNode f],
    rwscWeightedEdges :: [RuntimeWeightedEdge (SuccessorEdge f) (NonEmpty (RewriteTransitionStat RewriteRuleId)) (NonEmpty (RewriteOutcomeStat RewriteRuleId))],
    rwscUnobservedStructuralEdges :: [SuccessorEdge f],
    rwscUnmappedNodes :: [SuccessorNode f],
    rwscAmbiguousNodes :: [(SuccessorNode f, NonEmpty RewriteRuleId)]
  }

type RewriteRuntimeWeightedSuccessorComplex :: (Type -> Type) -> Type
type RewriteRuntimeWeightedSuccessorComplex f = RuntimeWeightedSuccessorComplex f

rewriteSuccessorComplex ::
  (HasConstructorTag f, ZipMatch f, Eq (RewriteMorphism f)) =>
  RewriteSystem f ->
  SuccessorComplex f
rewriteSuccessorComplex =
  Successor.buildSuccessorComplex rewriteSuccessorAlgebra

buildSuccessorComplex ::
  (HasConstructorTag f, ZipMatch f, Eq (RewriteMorphism f)) =>
  RewriteSystem f ->
  SuccessorComplex f
buildSuccessorComplex =
  rewriteSuccessorComplex

rewriteInfluenceComplex ::
  (HasConstructorTag f, ZipMatch f, Eq (RewriteMorphism f)) =>
  SchedulerConfig RewriteRuleId ->
  RewriteSystem f ->
  InfluenceComplex f
rewriteInfluenceComplex schedulerConfig =
  Successor.buildInfluenceComplex schedulerConfig rewriteSuccessorAlgebra

buildInfluenceComplex ::
  (HasConstructorTag f, ZipMatch f, Eq (RewriteMorphism f)) =>
  SchedulerConfig RewriteRuleId ->
  RewriteSystem f ->
  InfluenceComplex f
buildInfluenceComplex =
  rewriteInfluenceComplex

rewriteSupportRuntimeOverlayFromTrace ::
  Ord c =>
  [SupportTraceEntry (SupportBasis c) RewriteRuleId] ->
  Support.SupportRuntimeOverlay (SupportBasis c) RewriteRuleId
rewriteSupportRuntimeOverlayFromTrace =
  Support.supportRuntimeOverlayFromTrace
    Support.SupportTraceView
      { Support.stvRound = steRound,
        Support.stvSupport = Just . steSupport,
        Support.stvRuleId = steRuleId,
        Support.stvMatchedCount = workCountFromInt . steMatchedCount,
        Support.stvScheduledCount = naturalFromInt . steScheduledCount,
        Support.stvSuppressedCount = workCountFromInt . steSuppressedCount,
        Support.stvSuppressedByCooldown = steSuppressedByCooldown
      }

naturalFromInt :: Int -> Natural
naturalFromInt =
  fromIntegral . max 0
{-# INLINE naturalFromInt #-}

runtimeWeightedSuccessorComplexFromTrace ::
  (HasConstructorTag f, ZipMatch f, Eq (RewriteMorphism f)) =>
  SaturationTrace RewriteRuleId ->
  RewriteSystem f ->
  RuntimeWeightedSuccessorComplex f
runtimeWeightedSuccessorComplexFromTrace =
  runtimeWeightedSuccessorComplexWithProjection defaultRuntimeProjection

runtimeWeightedSuccessorComplexWithProjection ::
  (HasConstructorTag f, ZipMatch f, Eq (RewriteMorphism f)) =>
  RuleRuntimeProjection f ->
  SaturationTrace RewriteRuleId ->
  RewriteSystem f ->
  RuntimeWeightedSuccessorComplex f
runtimeWeightedSuccessorComplexWithProjection projectionValue saturationTrace rewriteSystem =
  runtimeWeightedSuccessorComplexFromBase
    projectionValue
    saturationTrace
    (rewriteSuccessorComplex rewriteSystem)

runtimeWeightedSuccessorComplexFromBase ::
  RuleRuntimeProjection f ->
  SaturationTrace RewriteRuleId ->
  SuccessorComplex f ->
  RuntimeWeightedSuccessorComplex f
runtimeWeightedSuccessorComplexFromBase projectionValue saturationTrace base =
  let outcomeSummary = summarizeSaturationTrace saturationTrace
      transitionSummary = summarizeRewriteTransitions saturationTrace
      runtimeOverlay =
        Runtime.runtimeAnnotatedSuccessorComplexWithProjection
          projectionValue
          rosRuleStats
          rosRuleId
          rtrsTransitions
          rtsFromRule
          rtsToRule
          Successor.rscNodes
          Successor.rscEdges
          Successor.seSource
          Successor.seTarget
          outcomeSummary
          transitionSummary
          base
   in RuntimeWeightedSuccessorComplex
        { rwscBase = base,
          rwscOutcomeSummary = outcomeSummary,
          rwscTransitionSummary = transitionSummary,
          rwscObservedNodes = Runtime.runtimeObservedNodes Successor.rscNodes runtimeOverlay,
          rwscWeightedEdges = Runtime.runtimeWeightedEdges Successor.rscEdges runtimeOverlay,
          rwscUnobservedStructuralEdges = Runtime.unobservedStructuralEdges Successor.rscEdges runtimeOverlay,
          rwscUnmappedNodes = filter (isUnmappedIdentity . Successor.snRuntimeRuleIdentity) (Successor.rscNodes base),
          rwscAmbiguousNodes = mapMaybe ambiguousIdentityEntry (Successor.rscNodes base)
        }

runtimeWeightedEdgeCount :: RuntimeWeightedSuccessorComplex f -> Int
runtimeWeightedEdgeCount =
  length . rwscWeightedEdges

unobservedStructuralEdgeCount :: RuntimeWeightedSuccessorComplex f -> Int
unobservedStructuralEdgeCount =
  length . rwscUnobservedStructuralEdges

rewriteSuccessorAlgebra ::
  (HasConstructorTag f, ZipMatch f, Eq (RewriteMorphism f)) =>
  SuccessorAlgebra
    (RewriteSystem f)
    (RewriteContext f)
    (RewriteMorphism f)
    RuntimeRuleIdentity
    (RewriteMorphism f)
    (RewriteCompositionObstruction f)
rewriteSuccessorAlgebra =
  SuccessorAlgebra
    { saContexts = allContexts,
      saContextLeq = contextLeq,
      saRulesInContext = systemMorphismsInContext,
      saCandidateTargetRules = candidateMorphismsInContext,
      saRestrictRule = restrictMorphism,
      saComposeRules = composeMorphisms,
      saRuntimeRule = resolveRuntimeRuleIdentity
    }

candidateMorphismsInContext ::
  HasConstructorTag f =>
  RewriteSystem f ->
  RewriteContext f ->
  [RewriteMorphism f] ->
  RewriteMorphism f ->
  [RewriteMorphism f]
candidateMorphismsInContext _ _ targetMorphisms restrictedSourceMorphism =
  targetMorphisms
    & filter
      ( rewriteBoundariesMayUnify (rewriteMorphismRight restrictedSourceMorphism)
          . rewriteMorphismLeft
      )

rewriteBoundariesMayUnify :: HasConstructorTag f => Pattern f -> Pattern f -> Bool
rewriteBoundariesMayUnify sourceRightBoundary targetLeftBoundary =
  case (sourceRightBoundary, targetLeftBoundary) of
    (PatternNode sourceNode, PatternNode targetNode) ->
      constructorTag sourceNode == constructorTag targetNode
    _ ->
      True

defaultRuntimeProjection :: RuleRuntimeProjection f
defaultRuntimeProjection =
  Runtime.RuleRuntimeProjection projectUniqueRuleId

projectUniqueRuleId :: SuccessorNode f -> Maybe RewriteRuleId
projectUniqueRuleId nodeValue =
  case Successor.snRuntimeRuleIdentity nodeValue of
    UniqueRuntimeRuleIdentity ruleId -> Just ruleId
    _ -> Nothing

isUnmappedIdentity :: RuntimeRuleIdentity -> Bool
isUnmappedIdentity runtimeRuleIdentity =
  case runtimeRuleIdentity of
    NoRuntimeRuleIdentity -> True
    _ -> False

ambiguousIdentityEntry ::
  SuccessorNode f ->
  Maybe (SuccessorNode f, NonEmpty RewriteRuleId)
ambiguousIdentityEntry nodeValue =
  case Successor.snRuntimeRuleIdentity nodeValue of
    AmbiguousRuntimeRuleIdentity ruleIds -> Just (nodeValue, ruleIds)
    _ -> Nothing
