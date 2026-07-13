{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Moonlight.EGraph.Pure.Saturation.Guidance
  ( GuidanceRound (..),
    applyGuidance,
    applyGuidanceWithScope,
    applyGuidanceWithRuntimeCapabilities,
    egraphSupportGuidance,
  )
where

import Data.Kind (Type)
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Control.Monad (guard)
import Moonlight.Algebra
  ( JoinSemilattice,
  )
import Moonlight.EGraph.Effect.CoveringSurface
  ( SurfaceKind,
  )
import Moonlight.Core
  ( HasConstructorTag,
    Language,
    Pattern,
  )
import Moonlight.Core qualified as UnionFind
import Moonlight.EGraph.Pure.Change (EGraphMutationResult (..))
import Moonlight.EGraph.Pure.Query.RootFilter
  ( RootClassFilter (RestrictedRootClasses),
  )
import Moonlight.EGraph.Pure.Rebuild (drainPendingEditDelta)
import Moonlight.EGraph.Pure.Relational
  ( wcojMatchCompiledWithRootFilter,
  )
import Moonlight.EGraph.Pure.Rewrite.Env
  ( EGraphRewriteEnv (..),
  )
import Moonlight.EGraph.Pure.Rewrite.Program
  ( runExecutableRewriteMatchEGraphCommitted,
  )
import Moonlight.EGraph.Pure.Saturation.Substrate
  ( EGraphU,
  )
import Moonlight.EGraph.Pure.Types (ClassId, EGraph, classIdKey, eGraphUnionFind)
import Moonlight.Rewrite.ProofContext (SupportedRewriteMatch (..))
import Moonlight.Rewrite.Runtime (ExecutableRewriteMatch (..), ExecutedRewrite (..))
import Moonlight.Rewrite.Runtime (BinderSubstAlgebra)
import Moonlight.Rewrite.Runtime
  ( RewriteRuntimeCapabilities,
    emptyRewriteRuntimeCapabilities,
    withRuntimeBinderSubstAlgebra,
  )
import Moonlight.Rewrite.System
  ( CompiledGuard,
    GuardCapabilityResolver,
    GuardEvidence,
    combineCompiledGuards,
    compileGuard,
  )
import Moonlight.Rewrite.System (FactStore)
import Moonlight.Rewrite.Algebra
  ( CompiledPatternQuery,
    compilePatternQuery,
    singlePatternQuery,
  )
import Moonlight.Saturation.Context.Program.View
  ( srvBaseGraph,
    srvFacts,
    srvIteration,
  )
import Moonlight.Saturation.Context.Program.Spec
  ( SaturationGuidanceView (..),
  )
import Moonlight.Control.Gate
  ( Gate (..),
    GuidanceConfig (..),
    GuideCheckpoint (..),
    GuideCheckpointHit (..),
    GuideEvidence (..),
    GuideMode (..),
    GuideRoundTrace (..),
    GuideSelection (..),
    MatchSelectorResult (..),
    MatchSelector (..),
    noGate,
  )
import Moonlight.Control.Candidate
  ( lengthNatural,
  )
import Moonlight.Saturation.Substrate
  ( matchKey,
    setSupportedMatchInner,
    supportedMatchInner,
  )

type GuidanceRound :: Type -> (Type -> Type) -> Type
data GuidanceRound capability f = GuidanceRound
  { grMatches :: [ExecutableRewriteMatch (CompiledGuard capability f) GuardEvidence (GuideEvidence ClassId) f],
    grTrace :: GuideRoundTrace
  }

applyGuidance ::
  (Language f, Show (f ())) =>
  GuidanceConfig (Pattern f) ->
  Int ->
  FactStore ->
  EGraph f a ->
  [ExecutableRewriteMatch (CompiledGuard capability f) GuardEvidence (GuideEvidence ClassId) f] ->
  GuidanceRound capability f
applyGuidance =
  applyGuidanceWithRuntimeCapabilities emptyRewriteRuntimeCapabilities

applyGuidanceWithScope ::
  (Language f, Show (f ())) =>
  Maybe (BinderSubstAlgebra f) ->
  GuidanceConfig (Pattern f) ->
  Int ->
  FactStore ->
  EGraph f a ->
  [ExecutableRewriteMatch (CompiledGuard capability f) GuardEvidence (GuideEvidence ClassId) f] ->
  GuidanceRound capability f
applyGuidanceWithScope maybeBinderSubstAlgebra =
  applyGuidanceWithRuntimeCapabilities
    ( maybe
        emptyRewriteRuntimeCapabilities
        (`withRuntimeBinderSubstAlgebra` emptyRewriteRuntimeCapabilities)
        maybeBinderSubstAlgebra
    )

applyGuidanceWithRuntimeCapabilities ::
  (Language f, Show (f ())) =>
  RewriteRuntimeCapabilities (GuardCapabilityResolver capability) f ->
  GuidanceConfig (Pattern f) ->
  Int ->
  FactStore ->
  EGraph f a ->
  [ExecutableRewriteMatch (CompiledGuard capability f) GuardEvidence (GuideEvidence ClassId) f] ->
  GuidanceRound capability f
applyGuidanceWithRuntimeCapabilities runtimeCapabilities guidanceConfig iterationIndex factStore graph rewriteMatches =
  let checkpoints = gcCheckpoints guidanceConfig
      compiledCheckpoints = mapMaybe compileGuideCheckpoint checkpoints
      guidedMatches = fmap (attachGuideEvidence compiledCheckpoints) rewriteMatches
      matchesWithEvidence = filter (hasGuideEvidence . ermGuideEvidence) guidedMatches
      requiredMatches = filter (hasRequiredHit . ermGuideEvidence) guidedMatches
      (retainedMatches, guideSelection) =
        if any ((== GuideRequire) . gcMode) checkpoints
          then (requiredMatches, GuideRequired)
          else
            if null matchesWithEvidence
              then (guidedMatches, GuidePassThrough)
              else (matchesWithEvidence, GuidePreferred)
      matchedCheckpointCount =
        sum (fmap (length . geCheckpointHits) (mapMaybe ermGuideEvidence guidedMatches))
   in GuidanceRound
        { grMatches = retainedMatches,
          grTrace =
            GuideRoundTrace
              { grtIteration = iterationIndex,
                grtEligibleCount = length rewriteMatches,
                grtRetainedCount = length retainedMatches,
                grtGuidedCount = length matchesWithEvidence,
                grtMatchedCheckpointCount = matchedCheckpointCount,
                grtSelection = guideSelection
              }
        }
  where
    rewriteEnv =
      EGraphRewriteEnv
        { ereFactStore = factStore,
          ereRuntimeCapabilities = runtimeCapabilities
        }

    attachGuideEvidence compiledCheckpoints rewriteMatch =
      rewriteMatch
        { ermGuideEvidence =
            previewGuideEvidence compiledCheckpoints rewriteMatch
        }

    previewGuideEvidence compiledCheckpoints rewriteMatch = do
      rewriteCommit <-
        either
          (const Nothing)
          Just
          (runExecutableRewriteMatchEGraphCommitted rewriteEnv rewriteMatch graph)
      let previewGraphUnrebuilt =
            emrGraph rewriteCommit
          previewLhsClass =
            erwLhsClass (emrResult rewriteCommit)
      let previewGraph = drainPendingEditDelta previewGraphUnrebuilt
          previewClass =
            fst
              ( UnionFind.find
                  previewLhsClass
                  (eGraphUnionFind previewGraph)
              )
          checkpointHits =
            mapMaybe
              (guideCheckpointHit previewClass previewGraph)
              compiledCheckpoints
      guard (not (null checkpointHits))
      pure (GuideEvidence checkpointHits)

hasGuideEvidence :: Maybe (GuideEvidence ClassId) -> Bool
hasGuideEvidence =
  maybe False (not . null . geCheckpointHits)

hasRequiredHit :: Maybe (GuideEvidence ClassId) -> Bool
hasRequiredHit =
  maybe
    False
    (any ((== GuideRequire) . gchMode) . geCheckpointHits)

type CompiledGuideCheckpoint :: (Type -> Type) -> Type
type CompiledGuideCheckpoint f =
  (GuideCheckpoint (Pattern f), CompiledPatternQuery (CompiledGuard () f) f)

compileGuideCheckpoint ::
  Language f =>
  GuideCheckpoint (Pattern f) ->
  Maybe (CompiledGuideCheckpoint f)
compileGuideCheckpoint guideCheckpoint =
  either
    (const Nothing)
    (Just . (,) guideCheckpoint)
    (compilePatternQuery combineCompiledGuards compileGuard (singlePatternQuery (gcTarget guideCheckpoint)))

guideCheckpointHit ::
  (Language f, Show (f ())) =>
  ClassId ->
  EGraph f a ->
  CompiledGuideCheckpoint f ->
  Maybe (GuideCheckpointHit ClassId)
guideCheckpointHit previewClass previewGraph (guideCheckpoint, compiledCheckpoint) =
  do
    checkpointMatches <-
      either
        (const Nothing)
        Just
        ( wcojMatchCompiledWithRootFilter
            (RestrictedRootClasses (IntSet.singleton (classIdKey previewClass)))
            compiledCheckpoint
            previewGraph
        )
    guard (not (null checkpointMatches))
    pure
      GuideCheckpointHit
        { gchCheckpointName = gcName guideCheckpoint,
          gchMode = gcMode guideCheckpoint,
          gchPreviewClass = previewClass
        }

egraphSupportGuidance ::
  forall capability f a c schedulerKey.
  (HasConstructorTag f, Language f, Show (f ()), Ord capability, Show capability, Ord a, JoinSemilattice a, Ord c) =>
  RewriteRuntimeCapabilities (GuardCapabilityResolver capability) f ->
  Maybe (GuidanceConfig (Pattern f)) ->
  Gate
    (SaturationGuidanceView (EGraphU capability f a c))
    ()
    (SupportedRewriteMatch c capability f)
    GuideRoundTrace
    schedulerKey
egraphSupportGuidance runtimeCapabilities =
  maybe
    noGate
    ( \guidanceConfig ->
        Gate
          { gateSelector =
              MatchSelector
                { matchSelectorName = "egraph-support-guide",
                  matchSelectorPreservesCount = False,
                  runMatchSelector =
                    \guidanceView () supportedMatches ->
                      let roundView =
                            sgvRoundView guidanceView
                          guidanceRound =
                            applyGuidanceWithRuntimeCapabilities
                              runtimeCapabilities
                              guidanceConfig
                              (srvIteration roundView)
                              (srvFacts roundView)
                              (srvBaseGraph roundView)
                              (fmap srmMatch supportedMatches)
                          retainedMatches =
                            projectSupportedByInner
                              (grMatches guidanceRound)
                              supportedMatches
                       in MatchSelectorResult
                            { msrAcceptedMatches = retainedMatches,
                              msrTrace = [grTrace guidanceRound],
                              msrRejectedCount =
                                lengthNatural supportedMatches
                                  - lengthNatural retainedMatches
                            }
                },
            gateValidation = mempty
          }
    )
  where
    projectSupportedByInner rawMatches supportedMatches =
      let retainedMatches = Map.fromList [(matchKey @(EGraphU capability f a c) match, match) | match <- rawMatches]
       in mapMaybe
            ( \sm ->
                fmap
                  (\m -> setSupportedMatchInner @(EGraphU capability f a c) m sm)
                  (Map.lookup (matchKey @(EGraphU capability f a c) (supportedMatchInner @(EGraphU capability f a c) sm)) retainedMatches)
            )
            supportedMatches
