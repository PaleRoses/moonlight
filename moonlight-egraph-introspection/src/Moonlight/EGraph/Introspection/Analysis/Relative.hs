{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Introspection.Analysis.Relative
  ( AbsoluteDiagnostics,
    ChainGrounding,
    GroundingKind (..),
    MorphismGrounding,
    RelativeDiagnostics,
    SupportRuntimeCounts (..),
    RuntimeRelativeDiagnostics,
    absoluteDiagnostics,
    groundGrothendieckMorphism,
    relativeDiagnostics,
    supportRuntimeRelativeDiagnostics,
    runtimeRelativeDiagnostics,
    runtimeRelativeDiagnosticsWithProjection,
    rdAbsolute,
    rdGroundingKind,
    rdGroundedMorphismCount,
    rdGroundedNodeCoverage,
    rdGroundableChainCount,
    rdGroundedChainCount,
    rdVerticalLoss,
    rdStructuralCompressionGap,
    rdMorphismGroundings,
    rdChainGroundings,
    mgStaticMorphism,
    mgGroundedNode,
    cgStaticLeft,
    cgStaticRight,
    cgGroundedLeft,
    cgGroundedRight,
    cgGroundedEdge,
    rrdBase,
    rrdObservedGroundedMorphismCount,
    rrdObservedGroundedNodeCoverage,
    rrdObservedGroundedChainCount,
    rrdUnobservedGroundedChainCount,
    rrdUnmappedGroundedNodeCount,
    rrdAmbiguousGroundedNodeCount,
    rrdSupportRuntimeCounts,
    adStructuralSummary,
    adGrothendieckSummary,
  )
where

import Data.Function ((&))
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Moonlight.Analysis.Relative qualified as Generic
import Moonlight.Analysis.Relative
  ( GroundingKind (..),
    SupportRuntimeCounts (..),
    RelativeGroundingModel (..),
    RuntimeGroundingOverlay (..),
    rdAbsolute,
    rdGroundingKind,
    rdGroundedMorphismCount,
    rdGroundedNodeCoverage,
    rdGroundableChainCount,
    rdGroundedChainCount,
    rdVerticalLoss,
    rdStructuralCompressionGap,
    rdMorphismGroundings,
    rdChainGroundings,
    mgStaticMorphism,
    mgGroundedNode,
    cgStaticLeft,
    cgStaticRight,
    cgGroundedLeft,
    cgGroundedRight,
    cgGroundedEdge,
    rrdBase,
    rrdObservedGroundedMorphismCount,
    rrdObservedGroundedNodeCoverage,
    rrdObservedGroundedChainCount,
    rrdUnobservedGroundedChainCount,
    rrdUnmappedGroundedNodeCount,
    rrdAmbiguousGroundedNodeCount,
    rrdSupportRuntimeCounts,
    adStructuralSummary,
  )
import Moonlight.Category (chainMorphisms)
import Moonlight.Core (ZipMatch (..), HasConstructorTag, Pattern, RewriteRuleId)
import Moonlight.Sheaf.Site (ContextPresentationSystem (systemContextPresentation))
import Moonlight.Sheaf.Site
  ( GrothendieckCategory,
    GrothendieckMor (..),
    grothendieckMorphisms,
    grothendieckNerve,
  )
import Moonlight.Sheaf.Site
  ( GrothendieckStructuralSummary (..),
    summarizeGrothendieckSystem,
  )
import Moonlight.EGraph.Introspection.Core.Rewrite
  ( RewriteContext,
    RewriteMorphism,
    RewriteSystem,
    rcObjects,
    sameRuntimeRewriteMorphism,
  )
import Moonlight.EGraph.Introspection.Core.Rewrite.Successor
  ( RewriteRuntimeProjection,
    RewriteRuntimeWeightedSuccessorComplex,
    RewriteSuccessorComplex,
    RewriteSuccessorEdge,
    RewriteSuccessorNode,
    RuntimeWeightedSuccessorComplex (..),
    buildSuccessorComplex,
    defaultRuntimeProjection,
    rewriteSupportRuntimeOverlayFromTrace,
    runtimeWeightedSuccessorComplexFromBase,
  )
import Moonlight.Control.Scheduling.Successor
  ( findSuccessorEdge,
  )
import Moonlight.Control.Scheduling.Successor qualified as Successor
import Moonlight.Control.Scheduling.Successor.Runtime (RuntimeWeightedEdge (..))
import Moonlight.Control.Scheduling.Support
  ( SupportRuntimeOverlay,
    supportRuntimeCooldownRuleCount,
    supportRuntimeObservedRuleCount,
    supportRuntimeSuppressedRuleCount,
  )
import Moonlight.Rewrite.ProofContext (SupportBasis)
import Moonlight.Pale.Diagnostic.Section.Saturation (SaturationTrace)
import Moonlight.Homology (HomologyFailure)
import Moonlight.Sheaf.Twist.Report (SupportTraceEntry)
import Moonlight.Category.Simplicial (NerveSimplex, nerveSimplexChain)
import Moonlight.Category.Simplicial (simplicesAtDimension)
import Numeric.Natural (Natural)

type AbsoluteDiagnostics :: Type
type AbsoluteDiagnostics =
  Generic.AbsoluteDiagnostics GrothendieckStructuralSummary

type MorphismGrounding :: (Type -> Type) -> Type
type MorphismGrounding f =
  Generic.MorphismGrounding
    (GrothendieckMor (RewriteSystem f))
    (RewriteSuccessorNode f)

type ChainGrounding :: (Type -> Type) -> Type
type ChainGrounding f =
  Generic.ChainGrounding
    (GrothendieckMor (RewriteSystem f))
    (RewriteSuccessorNode f)
    (RewriteSuccessorEdge f)

type RelativeDiagnostics :: (Type -> Type) -> Type
type RelativeDiagnostics f =
  Generic.RelativeDiagnostics
    GrothendieckStructuralSummary
    (GrothendieckMor (RewriteSystem f))
    (RewriteSuccessorNode f)
    (RewriteSuccessorEdge f)

type RuntimeRelativeDiagnostics :: (Type -> Type) -> Type
type RuntimeRelativeDiagnostics f =
  Generic.RuntimeRelativeDiagnostics
    GrothendieckStructuralSummary
    (GrothendieckMor (RewriteSystem f))
    (RewriteSuccessorNode f)
    (RewriteSuccessorEdge f)

absoluteDiagnostics ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f)) =>
  RewriteSystem f ->
  Natural ->
  Either HomologyFailure AbsoluteDiagnostics
absoluteDiagnostics rewriteSystem =
  fmap Generic.AbsoluteDiagnostics
    . summarizeGrothendieckSystem rewriteSystem

groundGrothendieckMorphism ::
  (Ord (Pattern f), Eq (RewriteMorphism f)) =>
  RewriteSuccessorComplex f ->
  GrothendieckMor (RewriteSystem f) ->
  Maybe (RewriteSuccessorNode f)
groundGrothendieckMorphism successorComplex morphismValue =
  gmTargetMorphism morphismValue
    >>= findStructuralSuccessorNode successorComplex (gmTargetContext morphismValue)

findStructuralSuccessorNode ::
  (Ord (Pattern f), Eq (RewriteMorphism f)) =>
  RewriteSuccessorComplex f ->
  RewriteContext f ->
  RewriteMorphism f ->
  Maybe (RewriteSuccessorNode f)
findStructuralSuccessorNode successorComplex contextValue morphismValue =
  find
    ( \nodeValue ->
        rcObjects (Successor.snContext nodeValue) == rcObjects contextValue
          && sameRuntimeRewriteMorphism (Successor.snRule nodeValue) morphismValue
    )
    (Successor.rscNodes successorComplex)

relativeDiagnostics ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f), Eq (RewriteMorphism f)) =>
  RewriteSystem f ->
  Natural ->
  Either HomologyFailure (RelativeDiagnostics f)
relativeDiagnostics rewriteSystem depthValue =
  relativeDiagnosticsFromSuccessorComplex
    rewriteSystem
    depthValue
    (buildSuccessorComplex rewriteSystem)

relativeDiagnosticsFromSuccessorComplex ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f), Eq (RewriteMorphism f)) =>
  RewriteSystem f ->
  Natural ->
  RewriteSuccessorComplex f ->
  Either HomologyFailure (RelativeDiagnostics f)
relativeDiagnosticsFromSuccessorComplex rewriteSystem depthValue successorComplex = do
  summaryValue <- summarizeGrothendieckSystem rewriteSystem depthValue
  let model = rewriteGroundingModel summaryValue successorComplex
  pure (Generic.relativeDiagnostics model rewriteSystem)

runtimeRelativeDiagnostics ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f), Eq (RewriteMorphism f)) =>
  SaturationTrace RewriteRuleId ->
  RewriteSystem f ->
  Natural ->
  Either HomologyFailure (RuntimeRelativeDiagnostics f)
runtimeRelativeDiagnostics saturationTrace rewriteSystem depthValue =
  let successorComplex = buildSuccessorComplex rewriteSystem
      runtimeOverlay =
        runtimeOverlayFromWeightedComplex
          (runtimeWeightedSuccessorComplexFromBase defaultRuntimeProjection saturationTrace successorComplex)
          successorComplex
   in fmap
        (Generic.runtimeRelativeDiagnostics runtimeOverlay)
        (relativeDiagnosticsFromSuccessorComplex rewriteSystem depthValue successorComplex)

runtimeRelativeDiagnosticsWithProjection ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f), Eq (RewriteMorphism f)) =>
  RewriteRuntimeProjection f ->
  SaturationTrace RewriteRuleId ->
  RewriteSystem f ->
  Natural ->
  Either HomologyFailure (RuntimeRelativeDiagnostics f)
runtimeRelativeDiagnosticsWithProjection projectionValue saturationTrace rewriteSystem depthValue =
  let successorComplex = buildSuccessorComplex rewriteSystem
      runtimeOverlay =
        runtimeOverlayFromWeightedComplex
          (runtimeWeightedSuccessorComplexFromBase projectionValue saturationTrace successorComplex)
          successorComplex
   in fmap
        (Generic.runtimeRelativeDiagnostics runtimeOverlay)
        (relativeDiagnosticsFromSuccessorComplex rewriteSystem depthValue successorComplex)

supportRuntimeRelativeDiagnostics ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f), Eq (RewriteMorphism f), Ord c) =>
  [SupportTraceEntry (SupportBasis c) RewriteRuleId] ->
  RewriteSystem f ->
  Natural ->
  Either HomologyFailure (RuntimeRelativeDiagnostics f)
supportRuntimeRelativeDiagnostics supportTrace rewriteSystem depthValue =
  fmap
    (attachSupportRuntimeOverlay (rewriteSupportRuntimeOverlayFromTrace supportTrace))
    (relativeDiagnostics rewriteSystem depthValue >>= pure . emptyRuntimeRelative)
  where
    emptyRuntimeRelative ::
      RelativeDiagnostics f ->
      RuntimeRelativeDiagnostics f
    emptyRuntimeRelative relativeValue =
      Generic.RuntimeRelativeDiagnostics
        { rrdBase = relativeValue,
          rrdObservedGroundedMorphismCount = 0,
          rrdObservedGroundedNodeCoverage = 0,
          rrdObservedGroundedChainCount = 0,
          rrdUnobservedGroundedChainCount = 0,
          rrdUnmappedGroundedNodeCount = 0,
          rrdAmbiguousGroundedNodeCount = 0,
          rrdSupportRuntimeCounts = Nothing
        }

rewriteGroundingModel ::
  (HasConstructorTag f, ZipMatch f, Ord (Pattern f), Eq (RewriteMorphism f)) =>
  GrothendieckStructuralSummary ->
  RewriteSuccessorComplex f ->
  RelativeGroundingModel
    (RewriteSystem f)
    GrothendieckStructuralSummary
    (GrothendieckMor (RewriteSystem f))
    (RewriteSuccessorNode f)
    (RewriteSuccessorEdge f)
rewriteGroundingModel summaryValue successorComplex =
  RelativeGroundingModel
    { rgmSummaryOf = const summaryValue,
      rgmVerticalLoss = gssVerticalMorphismCount,
      rgmMorphismsOf = \system ->
        grothendieckMorphisms (systemContextPresentation system),
      rgmComposablePairsOf = \system ->
        simplicesAtDimension (grothendieckNerve (systemContextPresentation system) 2) 2
          & mapMaybe extractChainPair,
      rgmGroundMorphism =
        groundGrothendieckMorphism successorComplex,
      rgmFindEdge = findSuccessorEdge successorComplex,
      rgmGroundingKind = GroundingKind "RuleSuccessorGrounding"
    }

extractChainPair ::
  NerveSimplex (GrothendieckCategory (RewriteSystem f)) ->
  Maybe (GrothendieckMor (RewriteSystem f), GrothendieckMor (RewriteSystem f))
extractChainPair simplexValue =
  case chainMorphisms (nerveSimplexChain simplexValue) of
    [leftMorphism, rightMorphism] ->
      Just (leftMorphism, rightMorphism)
    _ -> Nothing

runtimeOverlayFromWeightedComplex ::
  RewriteRuntimeWeightedSuccessorComplex f ->
  RewriteSuccessorComplex f ->
  RuntimeGroundingOverlay (RewriteSuccessorNode f) (RewriteSuccessorEdge f)
runtimeOverlayFromWeightedComplex runtimeWeighted _baseComplex =
  RuntimeGroundingOverlay
    { rgoObservedNodes = rwscObservedNodes runtimeWeighted,
      rgoObservedEdges = fmap rweStructuralEdge (rwscWeightedEdges runtimeWeighted),
      rgoUnobservedEdges = rwscUnobservedStructuralEdges runtimeWeighted,
      rgoUnmappedNodes = rwscUnmappedNodes runtimeWeighted,
      rgoAmbiguousNodes = fmap fst (rwscAmbiguousNodes runtimeWeighted),
      rgoSupportCounts = Nothing
    }

attachSupportRuntimeOverlay ::
  SupportRuntimeOverlay c RewriteRuleId ->
  RuntimeRelativeDiagnostics f ->
  RuntimeRelativeDiagnostics f
attachSupportRuntimeOverlay supportOverlay runtimeRelativeValue =
  runtimeRelativeValue
    { rrdSupportRuntimeCounts = Just (supportRuntimeCounts supportOverlay)
    }

supportRuntimeCounts :: SupportRuntimeOverlay c RewriteRuleId -> SupportRuntimeCounts
supportRuntimeCounts supportRuntimeOverlay =
  SupportRuntimeCounts
    { srcObservedRuleCount = supportRuntimeObservedRuleCount supportRuntimeOverlay,
      srcSuppressedRuleCount = supportRuntimeSuppressedRuleCount supportRuntimeOverlay,
      srcCooldownRuleCount = supportRuntimeCooldownRuleCount supportRuntimeOverlay
    }

adGrothendieckSummary :: AbsoluteDiagnostics -> GrothendieckStructuralSummary
adGrothendieckSummary = adStructuralSummary
