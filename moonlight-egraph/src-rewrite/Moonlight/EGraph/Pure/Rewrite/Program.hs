{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.EGraph.Pure.Rewrite.Program
  ( RewriteProgramPreview (..),
    rewriteProgramPreviewEffect,
    rewriteProgramPreviewTouchedKeys,
    commitRewriteProgramPreview,
    runRewriteProgramEGraphPreview,
    runRewriteProgramEGraphCommitted,
    runRewriteRhsEGraphPreview,
    runRewriteRhsEGraphPreviewWithResolver,
    runExecutableRewriteMatchEGraphPreview,
    runExecutableRewriteMatchEGraphCommitted,
    runExecutableRewriteMatchesEGraphPreview,
    runExecutableRewriteMatchesEGraphCommitted,
  )
where

import Control.Monad (foldM)
import Control.Monad.Trans.State.Strict (StateT (..), runStateT)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.Kind (Type)
import Moonlight.Core
  ( Language,
    Pattern,
    PatternVar,
  )
import Moonlight.Core.EGraph.Program
  ( EGraphProgram,
    EGraphProgramEffect,
    EGraphProgramOp (..),
    foldEGraphProgram,
    repeatEGraphProgramEffect,
    requiredClassMergeEffect,
  )
import Moonlight.Core qualified as UnionFind
import Moonlight.EGraph.Pure.Analysis (asMake)
import Moonlight.EGraph.Pure.Change
  ( EGraphMutationResult (..),
    EGraphMutationTrace,
    ObservedClassUnions,
    appendEGraphMutationTrace,
    eGraphMutationTraceEffect,
    emptyEGraphMutationTrace,
    emtTouchedClassKeys,
    observedClassUnionCount,
    observedClassUnionKeys,
    observedClassUnionPairs,
    observedClassUnions,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (insertENodeTracked)
import Moonlight.EGraph.Pure.Rebuild
  ( equateClassPairsTracked,
  )
import Moonlight.EGraph.Pure.Rewrite.Env
  ( EGraphRewriteEnv (..),
    rewriteRuntimeGuardCapabilityResolver,
  )
import Moonlight.EGraph.Pure.Rewrite.Guard
  ( acceptRewriteCondition,
  )
import Moonlight.EGraph.Pure.Rewrite.Instantiate
  ( bindingPatternResolverMaybe,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    EGraph,
    ENode (..),
    canonicalizeClassId,
    classIdKey,
    eGraphAnalysis,
    eGraphAnalysisSpec,
    eGraphUnionFind,
  )
import Moonlight.Rewrite.Runtime
  ( RewriteRuntimeCapabilities,
    runtimeBinderSubstAlgebra,
  )
import Moonlight.Rewrite.System
  ( FactStore,
    canonicalizeFactStore,
    factStoreClassKeys,
  )
import Moonlight.Rewrite.Runtime
  ( ExecutableRewriteMatch (..),
    ExecutedRewrite,
  )
import Moonlight.Rewrite.Runtime qualified as RewriteExec
import Moonlight.Rewrite.Runtime (RewriteApplicationError (..))
import Moonlight.Rewrite.System
  ( CompiledGuard,
    GuardCapabilityResolver,
    GuardEvidence,
  )

type RewriteProgramPreview :: (Type -> Type) -> Type -> Type -> Type
data RewriteProgramPreview f a resultValue = RewriteProgramPreview
  { rppResult :: resultValue,
    rppInsertionTrace :: !(EGraphMutationTrace f),
    rppPlannedClassUnions :: !ObservedClassUnions,
    rppPreviewGraph :: !(EGraph f a)
  }

rewriteProgramPreviewEffect :: RewriteProgramPreview f a resultValue -> EGraphProgramEffect
rewriteProgramPreviewEffect previewValue =
  eGraphMutationTraceEffect (rppInsertionTrace previewValue)
    <> repeatEGraphProgramEffect
      (observedClassUnionCount (rppPlannedClassUnions previewValue))
      requiredClassMergeEffect
{-# INLINE rewriteProgramPreviewEffect #-}

rewriteProgramPreviewTouchedKeys :: RewriteProgramPreview f a resultValue -> IntSet
rewriteProgramPreviewTouchedKeys previewValue =
  emtTouchedClassKeys (rppInsertionTrace previewValue)
    <> observedClassUnionKeys (rppPlannedClassUnions previewValue)
{-# INLINE rewriteProgramPreviewTouchedKeys #-}

commitRewriteProgramPreview ::
  RewriteProgramPreview f a resultValue ->
  EGraphMutationResult f a resultValue
commitRewriteProgramPreview previewValue =
  let EGraphMutationResult
        { emrTrace = unionTrace,
          emrGraph = nextGraph
        } =
        equateClassPairsTracked
          (observedClassUnionPairs (rppPlannedClassUnions previewValue))
          (rppPreviewGraph previewValue)
   in EGraphMutationResult
        { emrResult = rppResult previewValue,
          emrTrace = appendEGraphMutationTrace (rppInsertionTrace previewValue) unionTrace,
          emrGraph = nextGraph
        }
{-# INLINE commitRewriteProgramPreview #-}

runRewriteProgramEGraphPreview ::
  Language f =>
  EGraphProgram RewriteApplicationError (f ClassId) resultValue ->
  EGraph f a ->
  Either RewriteApplicationError (RewriteProgramPreview f a resultValue)
runRewriteProgramEGraphPreview =
  runPreview

runRewriteProgramEGraphCommitted ::
  Language f =>
  EGraphProgram RewriteApplicationError (f ClassId) resultValue ->
  EGraph f a ->
  Either RewriteApplicationError (EGraphMutationResult f a resultValue)
runRewriteProgramEGraphCommitted rewriteProgram graph =
  commitRewriteProgramPreview <$> runRewriteProgramEGraphPreview rewriteProgram graph

runRewriteRhsEGraphPreview ::
  Language f =>
  RewriteRuntimeCapabilities (GuardCapabilityResolver capability) f ->
  ExecutableRewriteMatch compiledGuard guardEvidence guideEvidence f ->
  EGraph f a ->
  Either RewriteApplicationError (RewriteProgramPreview f a ClassId)
runRewriteRhsEGraphPreview runtimeCapabilities rewriteMatch graph =
  runRewriteRhsEGraphPreviewWithResolver
    (bindingPatternResolverMaybe (runtimeBinderSubstAlgebra runtimeCapabilities) rewriteMatch graph)
    runtimeCapabilities
    rewriteMatch
    graph

-- | RHS preview with a caller-supplied binder witness resolver, so batch
-- drivers can route binder witnesses through shared extraction choices
-- instead of a per-match whole-graph extraction.
runRewriteRhsEGraphPreviewWithResolver ::
  Language f =>
  Maybe (PatternVar -> Either RewriteApplicationError (Pattern f)) ->
  RewriteRuntimeCapabilities (GuardCapabilityResolver capability) f ->
  ExecutableRewriteMatch compiledGuard guardEvidence guideEvidence f ->
  EGraph f a ->
  Either RewriteApplicationError (RewriteProgramPreview f a ClassId)
runRewriteRhsEGraphPreviewWithResolver bindingResolver runtimeCapabilities rewriteMatch graph = do
  rewriteProgram <-
    RewriteExec.compileRewriteRhs
      bindingResolver
      maybeBinderSubstAlgebra
      (ermRule rewriteMatch)
      (ermSubstitution rewriteMatch)
  runRewriteProgramEGraphPreview rewriteProgram graph
  where
    maybeBinderSubstAlgebra =
      runtimeBinderSubstAlgebra runtimeCapabilities

runExecutableRewriteMatchEGraphPreview ::
  Language f =>
  EGraphRewriteEnv capability f ->
  ExecutableRewriteMatch (CompiledGuard capability f) GuardEvidence guideEvidence f ->
  EGraph f a ->
  Either RewriteApplicationError (RewriteProgramPreview f a ExecutedRewrite)
runExecutableRewriteMatchEGraphPreview rewriteEnv rewriteMatch graph =
  runExecutableRewriteMatchWithFactStoreEGraphPreview
    rewriteEnv
    (cfsFactStore (canonicalFactStoreFor (ereFactStore rewriteEnv) graph))
    rewriteMatch
    graph

runExecutableRewriteMatchWithFactStoreEGraphPreview ::
  Language f =>
  EGraphRewriteEnv capability f ->
  FactStore ->
  ExecutableRewriteMatch (CompiledGuard capability f) GuardEvidence guideEvidence f ->
  EGraph f a ->
  Either RewriteApplicationError (RewriteProgramPreview f a ExecutedRewrite)
runExecutableRewriteMatchWithFactStoreEGraphPreview rewriteEnv factStore rewriteMatch graph = do
  guardEvidence <-
    acceptRewriteCondition
      factStore
      (rewriteRuntimeGuardCapabilityResolver (ereRuntimeCapabilities rewriteEnv))
      rewriteMatch
      graph
  let acceptedRewriteMatch =
        rewriteMatch {ermGuardEvidence = guardEvidence}
      runtimeCapabilities =
        ereRuntimeCapabilities rewriteEnv
      maybeBinderSubstAlgebra =
        runtimeBinderSubstAlgebra runtimeCapabilities
  rewriteProgram <-
    RewriteExec.compileExecutableRewriteMatch
      (bindingPatternResolverMaybe maybeBinderSubstAlgebra acceptedRewriteMatch graph)
      maybeBinderSubstAlgebra
      acceptedRewriteMatch
  runRewriteProgramEGraphPreview rewriteProgram graph

runExecutableRewriteMatchEGraphCommitted ::
  Language f =>
  EGraphRewriteEnv capability f ->
  ExecutableRewriteMatch (CompiledGuard capability f) GuardEvidence guideEvidence f ->
  EGraph f a ->
  Either RewriteApplicationError (EGraphMutationResult f a ExecutedRewrite)
runExecutableRewriteMatchEGraphCommitted rewriteEnv rewriteMatch graph =
  commitRewriteProgramPreview <$> runExecutableRewriteMatchEGraphPreview rewriteEnv rewriteMatch graph

runExecutableRewriteMatchesEGraphPreview ::
  Language f =>
  EGraphRewriteEnv capability f ->
  [ExecutableRewriteMatch (CompiledGuard capability f) GuardEvidence guideEvidence f] ->
  EGraph f a ->
  Either RewriteApplicationError (RewriteProgramPreview f a [ExecutedRewrite])
runExecutableRewriteMatchesEGraphPreview rewriteEnv rewriteMatches initialGraph =
  previewFromBatch
    <$> foldM runOne initialBatch rewriteMatches
  where
    rawFactStore =
      ereFactStore rewriteEnv

    initialBatch =
      RewriteProgramBatch
        { rpbGraph = initialGraph,
          rpbInsertionTrace = emptyEGraphMutationTrace initialGraph,
          rpbPlannedClassUnions = mempty,
          rpbCanonicalFactStore = canonicalFactStoreFor rawFactStore initialGraph,
          rpbResultsRev = []
        }

    runOne rewriteBatch rewriteMatch = do
      let canonicalFactStore =
            refreshCanonicalFactStore rawFactStore (rpbGraph rewriteBatch) (rpbCanonicalFactStore rewriteBatch)
      rewriteProgramPreview <-
        runExecutableRewriteMatchWithFactStoreEGraphPreview
          rewriteEnv
          (cfsFactStore canonicalFactStore)
          rewriteMatch
          (rpbGraph rewriteBatch)
      pure
        RewriteProgramBatch
          { rpbGraph = rppPreviewGraph rewriteProgramPreview,
            rpbInsertionTrace =
              appendEGraphMutationTrace
                (rpbInsertionTrace rewriteBatch)
                (rppInsertionTrace rewriteProgramPreview),
            rpbPlannedClassUnions =
              rpbPlannedClassUnions rewriteBatch <> rppPlannedClassUnions rewriteProgramPreview,
            rpbCanonicalFactStore = canonicalFactStore,
            rpbResultsRev = rppResult rewriteProgramPreview : rpbResultsRev rewriteBatch
          }

    previewFromBatch :: RewriteProgramBatch f a -> RewriteProgramPreview f a [ExecutedRewrite]
    previewFromBatch rewriteBatch =
      RewriteProgramPreview
        { rppResult = reverse (rpbResultsRev rewriteBatch),
          rppInsertionTrace = rpbInsertionTrace rewriteBatch,
          rppPlannedClassUnions = rpbPlannedClassUnions rewriteBatch,
          rppPreviewGraph = rpbGraph rewriteBatch
        }

runExecutableRewriteMatchesEGraphCommitted ::
  Language f =>
  EGraphRewriteEnv capability f ->
  [ExecutableRewriteMatch (CompiledGuard capability f) GuardEvidence guideEvidence f] ->
  EGraph f a ->
  Either RewriteApplicationError (EGraphMutationResult f a [ExecutedRewrite])
runExecutableRewriteMatchesEGraphCommitted rewriteEnv rewriteMatches graph =
  commitRewriteProgramPreview <$> runExecutableRewriteMatchesEGraphPreview rewriteEnv rewriteMatches graph

type RewriteProgramBatch :: (Type -> Type) -> Type -> Type
data RewriteProgramBatch f a = RewriteProgramBatch
  { rpbGraph :: !(EGraph f a),
    rpbInsertionTrace :: !(EGraphMutationTrace f),
    rpbPlannedClassUnions :: !ObservedClassUnions,
    rpbCanonicalFactStore :: !CanonicalFactStore,
    rpbResultsRev :: ![ExecutedRewrite]
  }

type CanonicalFactStore :: Type
data CanonicalFactStore = CanonicalFactStore
  { cfsSupportKeys :: !IntSet,
    cfsCanonicalClassBySupportKey :: !(IntMap.IntMap ClassId),
    cfsFactStore :: !FactStore
  }

canonicalFactStoreFor ::
  FactStore ->
  EGraph f a ->
  CanonicalFactStore
canonicalFactStoreFor factStore graph =
  let supportKeys =
        factStoreClassKeys factStore
      canonicalSupport =
        canonicalFactSupport graph supportKeys
   in CanonicalFactStore
        { cfsSupportKeys = supportKeys,
          cfsCanonicalClassBySupportKey = canonicalSupport,
          cfsFactStore = canonicalizeFactStore (canonicalizeClassId graph) factStore
        }

refreshCanonicalFactStore ::
  FactStore ->
  EGraph f a ->
  CanonicalFactStore ->
  CanonicalFactStore
refreshCanonicalFactStore factStore graph canonicalFactStore
  | cfsCanonicalClassBySupportKey canonicalFactStore == canonicalSupport =
      canonicalFactStore
  | otherwise =
      canonicalFactStoreFor factStore graph
  where
    canonicalSupport =
      canonicalFactSupport graph (cfsSupportKeys canonicalFactStore)

canonicalFactSupport ::
  EGraph f a ->
  IntSet ->
  IntMap.IntMap ClassId
canonicalFactSupport graph =
  IntMap.fromSet (canonicalizeClassId graph . ClassId)

data PreviewProgramState f a = PreviewProgramState
  { ppsInsertionTrace :: !(EGraphMutationTrace f),
    ppsPlannedClassUnions :: !ObservedClassUnions,
    ppsPhase :: !PreviewProgramPhase,
    ppsGraph :: !(EGraph f a)
  }

data PreviewProgramPhase
  = ConstructionOpen
  | MergesStarted
  deriving stock (Eq, Ord, Show)

runPreview ::
  forall f a resultValue.
  Language f =>
  EGraphProgram RewriteApplicationError (f ClassId) resultValue ->
  EGraph f a ->
  Either RewriteApplicationError (RewriteProgramPreview f a resultValue)
runPreview rewriteProgram initialGraph = do
  (resultValue, finalState) <-
    runStateT
      (foldEGraphProgram interpretProgramOp rewriteProgram)
      PreviewProgramState
        { ppsInsertionTrace = emptyEGraphMutationTrace initialGraph,
          ppsPlannedClassUnions = mempty,
          ppsPhase = ConstructionOpen,
          ppsGraph = initialGraph
        }
  pure
    RewriteProgramPreview
      { rppResult = resultValue,
        rppInsertionTrace = ppsInsertionTrace finalState,
        rppPlannedClassUnions = ppsPlannedClassUnions finalState,
        rppPreviewGraph = ppsGraph finalState
      }
  where
    interpretProgramOp ::
      EGraphProgramOp RewriteApplicationError (f ClassId) next ->
      StateT (PreviewProgramState f a) (Either RewriteApplicationError) next
    interpretProgramOp programOp =
      StateT $ \state ->
        case programOp of
          AbortProgram programError ->
            Left programError

          CanonicalizeClass classId continue ->
            case ppsPhase state of
              ConstructionOpen ->
                pure (continue (canonicalizeClassId (ppsGraph state) classId), state)
              MergesStarted ->
                Left RewriteProgramReadAfterMerge

          AddNode node continue -> do
            case ppsPhase state of
              ConstructionOpen -> do
                let graph =
                      ppsGraph state
                    canonicalChildren =
                      fmap (canonicalizeClassId graph) node
                childAnalyses <-
                  traverse
                    ( \childClassId ->
                        maybe
                          (Left (RewriteMissingEClass childClassId))
                          Right
                          (IntMap.lookup (classIdKey childClassId) (eGraphAnalysis graph))
                    )
                    canonicalChildren
                let nodeAnalysis =
                      asMake (eGraphAnalysisSpec graph) childAnalyses
                EGraphMutationResult
                  { emrResult = resultClassId,
                    emrTrace = traceValue,
                    emrGraph = nextGraph
                  } <-
                  case insertENodeTracked (ENode canonicalChildren) nodeAnalysis graph of
                    Left allocationError ->
                      Left (RewriteClassIdAllocationFailed allocationError)
                    Right mutationResult ->
                      Right mutationResult
                pure
                  ( continue resultClassId,
                    state
                      { ppsInsertionTrace = appendEGraphMutationTrace (ppsInsertionTrace state) traceValue,
                        ppsGraph = nextGraph
                      }
                  )
              MergesStarted ->
                Left RewriteProgramReadAfterMerge

          MergeClasses leftClassId rightClassId continue -> do
            let graph =
                  ppsGraph state
                canonicalLeft =
                  canonicalizeClassId graph leftClassId
                canonicalRight =
                  canonicalizeClassId graph rightClassId
                requiredNewMerge =
                  not (UnionFind.equivalent canonicalLeft canonicalRight (eGraphUnionFind graph))
                plannedUnions =
                  if requiredNewMerge
                    then observedClassUnions [(canonicalLeft, canonicalRight)]
                    else mempty
            pure
              ( continue canonicalLeft,
                state
                  { ppsPlannedClassUnions = ppsPlannedClassUnions state <> plannedUnions,
                    ppsPhase = MergesStarted
                  }
              )
