{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Introspection.Core.HsExpr.Spans
  ( etaReductionSpanFor,
    compositionSpanFor,
    betaReductionSpanFor,
    letReductionSpanFor,
    HsExprInsertionMetrics (..),
    HsExprInsertionError (..),
    HsExprContextLatticeError (..),
    HsExprSiteRuleError (..),
    InsertionSeeding (..),
    SpanClassRow (..),
    identityInsertionSeeding,
    HsExprSupportRuleMetrics (..),
    convertedModuleContextLattice,
    hsExprScopeGuardCapabilityResolver,
    hsExprCapabilityGenerationForContextGraph,
    hsExprRuntimeCapabilitiesForContextGraph,
    insertScopedExprWithSupport,
    insertConvertedModuleWithMetrics,
    insertConvertedModule,
    hsExprSupportRuleMetrics,
    HsExprSiteRuleKind (..),
    hsExprSupportedLawRules,
    hsExprSupportedRules,
    matchesHsExprSpanLhs,
    applyHsExprSpanAtRoot,
    hsExprDiagnosticSpans,
  )
where

import Data.Kind (Type)
import Data.Bifunctor (first)
import Data.Foldable (fold)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Control.Monad (foldM)
import GHC.Types.Name.Occurrence (mkVarOcc)
import GHC.Types.Name.Reader (RdrName, mkRdrUnqual)
import Numeric.Natural (Natural)
import Moonlight.Algebra (JoinSemilattice)
import Moonlight.Core (BinderId, Pattern (..), RewriteRuleId (..), binderIdKey, zipSameNodeShape)
import Moonlight.Core qualified as EGraph
import Moonlight.EGraph.Introspection.Core.HsExpr.BinderAlgebra (hsExprBinderSubstAlgebra)
import Moonlight.EGraph.Pure.Analysis (asMake)
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    ContextDeltaError,
    ContextRebaseBatch,
    beginContextRebaseBatch,
    commitContextRebaseBatch,
    contextRebaseBatchBaseGraph,
    contextRebaseBatchDirtyContexts,
    contextRebaseBatchSite,
    stageENodeWithSupport,
  )
import Moonlight.EGraph.Pure.Context
  ( cegBase,
    cegSite,
    cegClassSupportIndex,
    cegContextAnalysisDeltas,
    cegRuntimeState,
    ContextRuntimeState (..),
  )
import Moonlight.EGraph.Pure.Context.AnnotatedDelta
  ( AnnotatedDeltaMetrics (..),
    annotatedDeltaMetrics,
    contextAnnotatedDeltaBuckets,
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    classSupportIndexCarrierGeneratorCount,
    classSupportIndexGeneratorBucketCount,
    classSupportIndexSupportEntryCount,
    contextFragmentRestrictionPairs,
    contextObjectKeyFor,
    preparedContextFragment,
    preparedRegionTable,
    preparedSupportObjects,
    supportCarrierContainsKey,
    supportCarrierFromSupport,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId,
    ENode (..),
    EClass (..),
    EGraphRevision (..),
    canonicalizeClassId,
    classIdKey,
    eGraphAnalysis,
    eGraphAnalysisSpec,
    eGraphClassCount,
    eGraphNodeCount,
    eGraphRevision,
    lookupEClass,
  )
import Moonlight.EGraph.Introspection.Core.HsExpr.FreeScope
  ( FreeScopeWitness (..),
    HasFreeScopeWitness (..),
    freeScopeWitnessScopes,
  )
import Moonlight.Rewrite.System
  ( GuardCapabilityResolver (..),
    RewriteCondition (..),
    guardHasCapability,
    data GuardVar,
  )
import Moonlight.Rewrite.System
  ( RawRewriteRule (..)
  )
import Moonlight.Rewrite.Runtime
  ( PostMatchSubst (..),
    PostMatchTerm (..),
    applyPostMatchSubst,
  )
import Moonlight.Rewrite.System
  ( ldPostSubst,
  )
import Moonlight.Rewrite.Runtime
  ( RewriteRuntimeCapabilities,
    emptyRewriteRuntimeCapabilities,
    withRuntimeBinderSubstAlgebra,
    withRuntimeGuardCapabilityResolver,
  )
import Moonlight.EGraph.Introspection.Core.Rewrite
  ( PatternRewriteError,
    RewriteMorphism,
    rewriteMorphism,
  )
import Moonlight.Rewrite.Algebra (prDecoration, prLeft, prRight)
import Moonlight.Sheaf.Twist.SupportedRuleSpec qualified as SheafTwist
import Moonlight.Sheaf.Context.Core qualified as SheafTwist
import Moonlight.Pale.Ghc.Expr
import Moonlight.FiniteLattice
  ( ContextLattice,
    ContextLatticeCompileError,
    compileContextLattice,
    contextOrderDecl
  )
import Moonlight.FiniteLattice
  ( SupportBasis,
    principalSupport
  )

type HsExprInsertionMetrics :: Type
data HsExprInsertionMetrics = HsExprInsertionMetrics
  { himBindingCount :: !Int,
    himScopedExprCount :: !Int,
    himObservedContextCount :: !Int,
    himTotalSupportContextCount :: !Int,
    himMaxSupportContextCount :: !Int,
    himRebaseCount :: !Int,
    himRebaseDirtyContextCount :: !Int,
    himFinalActiveContextCount :: !Int,
    himFinalRestrictionCount :: !Int,
    himFinalChangedContextCount :: !Int,
    himFinalClassSupportEntryCount :: !Int,
    himFinalStoredSupportContextCount :: !Int,
    himFinalRegionalParentEdgeCount :: !Int,
    himFinalRegionalParentRegionCubeCount :: !Int,
    himFinalRegionalVariantRowCount :: !Int,
    himFinalRegionalAbsorbedRowCount :: !Int,
    himFinalContextAnalysisDeltaCount :: !Int,
    himBaseNodeCountBefore :: !Int,
    himBaseNodeCountAfter :: !Int,
    himBaseClassCountBefore :: !Int,
    himBaseClassCountAfter :: !Int
  }
  deriving stock (Eq, Ord, Show)

type ClosedExpressionSupport :: Type
data ClosedExpressionSupport
  -- Closed nested expressions remain root-supported so ordinary saturation
  -- keeps its global sharing.  The outer binding seed is occurrence-supported
  -- so a plan can glue that binding without lawfully dirtying the whole cover.
  = ClosedAtRoot
  | ClosedAtOccurrence

type HsExprInsertionError :: Type
data HsExprInsertionError
  = HsExprContextDeltaError !(ContextDeltaError HsExprF ScopeCtx)
  | HsExprSpanLockstepError !(Maybe SourceRegion)
  deriving stock (Show)

type HsExprContextLatticeError :: Type
data HsExprContextLatticeError
  = HsExprContextScopeLookupFailure !ScopeLookupFailure
  | HsExprContextLatticeCompileFailure !(ContextLatticeCompileError ScopeCtx)
  deriving stock (Show)

type HsExprSiteRuleError :: Type
data HsExprSiteRuleError
  = HsExprSiteRuleScopeLookupFailure !ScopeLookupFailure
  | HsExprSiteRuleRewriteFailure !(PatternRewriteError HsExprF)
  deriving stock (Show)

type InsertionSeeding :: Type -> Type
newtype InsertionSeeding a = InsertionSeeding
  { applySeed :: Maybe SourceRegion -> a -> a
  }

identityInsertionSeeding :: InsertionSeeding a
identityInsertionSeeding =
  InsertionSeeding (\_ analysisValue -> analysisValue)

type SpanClassRow :: Type
data SpanClassRow = SpanClassRow
  { scrRegion :: !SourceRegion,
    scrClass :: !ClassId
  }
  deriving stock (Eq, Ord, Show)

type ContextStructureSnapshot :: Type
data ContextStructureSnapshot = ContextStructureSnapshot
  { cssActiveContextCount :: !Int,
    cssRestrictionCount :: !Int,
    cssChangedContextCount :: !Int,
    cssClassSupportEntryCount :: !Int,
    cssSupportCarrierGeneratorCount :: !Int,
    cssSupportGeneratorBucketCount :: !Int,
    cssRegionalParentEdgeCount :: !Int,
    cssRegionalParentRegionCubeCount :: !Int,
    cssRegionalVariantRowCount :: !Int,
    cssRegionalAbsorbedRowCount :: !Int,
    cssContextAnalysisDeltaCount :: !Int
  }
  deriving stock (Eq, Ord, Show)

contextStructureSnapshot :: ContextEGraph owner f a c -> ContextStructureSnapshot
contextStructureSnapshot contextGraph =
  let contextAnalysisDeltas =
        cegContextAnalysisDeltas contextGraph
      regionalMetrics =
        annotatedDeltaMetrics
          (preparedRegionTable (cegSite contextGraph))
          (contextAnnotatedDeltaBuckets contextGraph)
   in ContextStructureSnapshot
        { cssActiveContextCount = Map.size contextAnalysisDeltas,
          cssRestrictionCount =
            length
              (contextFragmentRestrictionPairs (preparedContextFragment (cegSite contextGraph))),
          cssChangedContextCount =
            length (contextGraphPropagationChangedContexts contextGraph),
          cssClassSupportEntryCount = classSupportIndexSupportEntryCount (cegClassSupportIndex contextGraph),
          cssSupportCarrierGeneratorCount =
            classSupportIndexCarrierGeneratorCount (cegClassSupportIndex contextGraph),
          cssSupportGeneratorBucketCount =
            classSupportIndexGeneratorBucketCount (cegClassSupportIndex contextGraph),
          cssRegionalParentEdgeCount = annotatedDeltaParentEdgeCount regionalMetrics,
          cssRegionalParentRegionCubeCount = annotatedDeltaParentRegionCubeCount regionalMetrics,
          cssRegionalVariantRowCount = annotatedDeltaVariantRowCount regionalMetrics,
          cssRegionalAbsorbedRowCount = annotatedDeltaAbsorbedRowCount regionalMetrics,
          cssContextAnalysisDeltaCount = sum (fmap IntMap.size (Map.elems contextAnalysisDeltas))
        }

contextGraphPropagationChangedContexts ::
  ContextEGraph owner f a c ->
  [c]
contextGraphPropagationChangedContexts contextGraph =
  maybe
    []
    SheafTwist.contextPropagationChangedContexts
    (crsLastRepair (cegRuntimeState contextGraph))

type AccumEither :: Type -> Type -> Type -> Type
newtype AccumEither error state value = AccumEither
  { runAccumEither :: state -> Either error (state, value)
  }

instance Functor (AccumEither error state) where
  fmap transform (AccumEither runValue) =
    AccumEither (fmap (fmap transform) . runValue)

instance Applicative (AccumEither error state) where
  pure value =
    AccumEither (\stateValue -> Right (stateValue, value))

  AccumEither runTransform <*> AccumEither runValue =
    AccumEither $ \state0 -> do
      (state1, transform) <- runTransform state0
      (state2, value) <- runValue state1
      Right (state2, transform value)

traverseAccumEither ::
  Traversable t =>
  (state -> input -> Either error (state, output)) ->
  state ->
  t input ->
  Either error (state, t output)
traverseAccumEither step initialState =
  flip runAccumEither initialState
    . traverse (AccumEither . flip step)

type HsExprSupportRuleMetrics :: Type
data HsExprSupportRuleMetrics = HsExprSupportRuleMetrics
  { hsrmLambdaSiteCount :: !Int,
    hsrmLetSiteCount :: !Int,
    hsrmEtaRuleCount :: !Int,
    hsrmCompositionRuleCount :: !Int,
    hsrmBetaRuleCount :: !Int,
    hsrmLetRuleCount :: !Int,
    hsrmTotalRuleCount :: !Int,
    hsrmDiagnosticSpanCount :: !Int
  }
  deriving stock (Eq, Ord, Show)

type HsExprSiteRuleKind :: Type
data HsExprSiteRuleKind
  = HsExprEtaRule
  | HsExprCompositionRule
  | HsExprBetaRule
  | HsExprLetRule
  deriving stock (Eq, Ord, Show)

type RewriteRule :: (Type -> Type) -> Type
type RewriteRule f = RawRewriteRule (RewriteCondition ScopeCtx f) f

etaReductionSpanFor :: BinderAnn -> Either (PatternRewriteError HsExprF) (RewriteMorphism HsExprF)
etaReductionSpanFor binderAnn =
  let functionVar :: Pattern HsExprF
      functionVar = PatternVar (EGraph.mkPatternVar 0)
      leftPattern =
        PatternNode
          ( LamF
              binderAnn
              ( PatternNode
                  ( AppF
                      functionVar
                      (PatternNode (VarF (LocalName binderAnn)))
                  )
              )
          )
   in rewriteMorphism ("eta/" <> show (baId binderAnn)) leftPattern functionVar Nothing Nothing

compositionSpanFor :: BinderAnn -> Either (PatternRewriteError HsExprF) (RewriteMorphism HsExprF)
compositionSpanFor binderAnn =
  let functionVar :: Pattern HsExprF
      functionVar = PatternVar (EGraph.mkPatternVar 0)
      argumentVar :: Pattern HsExprF
      argumentVar = PatternVar (EGraph.mkPatternVar 1)
      leftPattern =
        PatternNode
          ( LamF
              binderAnn
              ( PatternNode
                  ( AppF
                      functionVar
                      ( PatternNode
                          ( ParF
                              ( PatternNode
                                  ( AppF
                                      argumentVar
                                      (PatternNode (VarF (LocalName binderAnn)))
                                  )
                              )
                          )
                      )
                  )
              )
          )
      rightPattern =
        PatternNode
          ( OpAppF
              functionVar
              (PatternNode (VarF (GlobalName compositionOperatorName)))
              argumentVar
          )
   in rewriteMorphism ("composition/" <> show (baId binderAnn)) leftPattern rightPattern Nothing Nothing

betaReductionSpanFor :: BinderAnn -> Either (PatternRewriteError HsExprF) (RewriteMorphism HsExprF)
betaReductionSpanFor binderAnn =
  let bodyVar = EGraph.mkPatternVar 0
      argumentVar = EGraph.mkPatternVar 1
      leftPattern =
        PatternNode
          ( AppF
              (PatternNode (ParF (PatternNode (LamF binderAnn (PatternVar bodyVar)))))
              (PatternVar argumentVar)
          )
      rightPattern :: Pattern HsExprF
      rightPattern = PatternVar bodyVar
      postSubst =
        SubstBinder (baId binderAnn) (PostMatchVar argumentVar)
   in rewriteMorphism ("beta/" <> show (baId binderAnn)) leftPattern rightPattern Nothing (Just postSubst)

letReductionSpanFor :: BinderAnn -> LetProvenance -> Either (PatternRewriteError HsExprF) (RewriteMorphism HsExprF)
letReductionSpanFor binderAnn provenanceValue =
  let bodyVar = EGraph.mkPatternVar 0
      argumentVar = EGraph.mkPatternVar 1
      leftPattern =
        PatternNode
          ( LetF
              (LetMode NonRecursiveBinds provenanceValue)
              [(PVarP binderAnn, PatternVar argumentVar)]
              (PatternVar bodyVar)
          )
      rightPattern :: Pattern HsExprF
      rightPattern = PatternVar bodyVar
      postSubst =
        SubstBinder (baId binderAnn) (PostMatchVar argumentVar)
   in rewriteMorphism ("let-reduce/" <> show (baId binderAnn)) leftPattern rightPattern Nothing (Just postSubst)

convertedModuleContextLattice :: ConvertedModule -> Either HsExprContextLatticeError (ContextLattice ScopeCtx)
convertedModuleContextLattice convertedModule =
  let scopeIndex = cmScopeIndex convertedModule
   in do
        topCtx <- first HsExprContextScopeLookupFailure (scopeTopCtx scopeIndex)
        observedContexts <- first HsExprContextScopeLookupFailure (scopeObservedContexts scopeIndex)
        relationPairs <- first HsExprContextScopeLookupFailure (scopeLeqPairs scopeIndex observedContexts)
        first HsExprContextLatticeCompileFailure $
          compileContextLattice
            (Set.fromList observedContexts)
            (contextOrderDecl topCtx (scopeBottomCtx scopeIndex) relationPairs)

scopeLeqPairs :: ScopeIndex -> [ScopeCtx] -> Either ScopeLookupFailure [(ScopeCtx, ScopeCtx)]
scopeLeqPairs scopeIndex contexts =
  fold <$> traverse relationRows contexts
  where
    relationRows leftCtx =
      fold <$> traverse (relationEntry leftCtx) contexts

    relationEntry leftCtx rightCtx =
      (\isLeq -> [(leftCtx, rightCtx) | isLeq]) <$> scopeCtxLeq scopeIndex leftCtx rightCtx

hsExprScopeGuardCapabilityResolver ::
  HasFreeScopeWitness a =>
  ContextEGraph owner HsExprF a ScopeCtx ->
  GuardCapabilityResolver ScopeCtx
hsExprScopeGuardCapabilityResolver contextGraph =
  let site =
        cegSite contextGraph
      baseGraph =
        cegBase contextGraph
      classWitness classId =
        maybe FreeScopeUnknown freeScopeWitness
          (IntMap.lookup (classIdKey (canonicalizeClassId baseGraph classId)) (eGraphAnalysis baseGraph))
      scopeLegalAt requiredKey legal scopeId =
        if not legal
          then pure False
          else do
            scopeCarrier <- supportCarrierFromSupport site (principalSupport (ActualScope scopeId))
            pure (supportCarrierContainsKey site scopeCarrier requiredKey)
   in GuardCapabilityResolver
        ( \requiredCtx classIds ->
            either
              (const False)
              id
              ( do
                  requiredKey <- contextObjectKeyFor site requiredCtx
                  foldM
                    ( \visible classId ->
                        if not visible
                          then pure False
                          else case freeScopeWitnessScopes (classWitness classId) of
                            Nothing -> pure False
                            Just freeScopes -> foldM (scopeLegalAt requiredKey) True freeScopes
                    )
                    True
                    classIds
              )
        )

hsExprRuntimeCapabilitiesForContextGraph ::
  HasFreeScopeWitness a =>
  ContextEGraph owner HsExprF a ScopeCtx ->
  RewriteRuntimeCapabilities (GuardCapabilityResolver ScopeCtx) HsExprF
hsExprRuntimeCapabilitiesForContextGraph contextGraph =
  withRuntimeGuardCapabilityResolver
    (hsExprScopeGuardCapabilityResolver contextGraph)
    ( withRuntimeBinderSubstAlgebra
        hsExprBinderSubstAlgebra
        emptyRewriteRuntimeCapabilities
    )

hsExprCapabilityGenerationForContextGraph ::
  ContextEGraph owner HsExprF a ScopeCtx ->
  Natural
hsExprCapabilityGenerationForContextGraph =
  fromIntegral . eGraphRevisionValue . eGraphRevision . cegBase

insertScopedExprWithSupport ::
  (Ord a, JoinSemilattice a) =>
  ScopeIndex ->
  ScopedExpr ->
  ContextEGraph owner HsExprF a ScopeCtx ->
  Either
    (ContextDeltaError HsExprF ScopeCtx)
    (ClassId, a, ContextEGraph owner HsExprF a ScopeCtx)
insertScopedExprWithSupport scopeIndex scopedExpr contextGraph0 = do
  (classId, classAnalysis, _, contextGraph1) <-
    insertScopedExprWithSupportMetrics ClosedAtOccurrence scopeIndex scopedExpr contextGraph0
  pure (classId, classAnalysis, contextGraph1)

insertScopedExprWithSupportMetrics ::
  (Ord a, JoinSemilattice a) =>
  ClosedExpressionSupport ->
  ScopeIndex ->
  ScopedExpr ->
  ContextEGraph owner HsExprF a ScopeCtx ->
  Either
    (ContextDeltaError HsExprF ScopeCtx)
    (ClassId, a, ScopedInsertionMetrics, ContextEGraph owner HsExprF a ScopeCtx)
insertScopedExprWithSupportMetrics closedSupport scopeIndex scopedExpr contextGraph0 = do
  (contextGraph1, childResults) <-
    traverseAccumEither
      ( \graphValue childExpr -> do
          (childClassId, childAnalysis, childMetricsValue, nextGraph) <-
            insertScopedExprWithSupportMetrics ClosedAtRoot scopeIndex childExpr graphValue
          pure (nextGraph, (childClassId, childAnalysis, childMetricsValue))
      )
      contextGraph0
      (seNode scopedExpr)
  let childClassIds =
        fmap
          (\(childClassId, _, _) -> childClassId)
          childResults
      childAnalyses =
        fmap
          (\(_, childAnalysis, _) -> childAnalysis)
          childResults
      childMetrics =
        foldMap
          (\(_, _, metricsValue) -> metricsValue)
          childResults
      baseGraph1 = cegBase contextGraph1
      nodeAnalysis = asMake (eGraphAnalysisSpec baseGraph1) childAnalyses
      supportValue = supportForScopedExprWith closedSupport scopeIndex scopedExpr
  (classId, stagedBatch) <-
    stageENodeWithSupport
      supportValue
      (ENode childClassIds)
      nodeAnalysis
      (beginContextRebaseBatch contextGraph1)
  (_rebaseReport, contextGraph3) <- commitContextRebaseBatch stagedBatch
  let classAnalysis = maybe nodeAnalysis eClassData (lookupEClass (cegBase contextGraph3) classId)
      supportContextCount =
        supportContextWidthWith closedSupport scopeIndex (cegSite contextGraph3) scopedExpr
      localMetrics =
        mempty
          { simScopedExprCount = 1,
            simTotalSupportContextCount = supportContextCount,
            simMaxSupportContextCount = supportContextCount
          }
  pure (classId, classAnalysis, childMetrics <> localMetrics, contextGraph3)

insertConvertedModule ::
  (Ord a, JoinSemilattice a) =>
  InsertionSeeding a ->
  ConvertedModule ->
  ContextEGraph owner HsExprF a ScopeCtx ->
  Either HsExprInsertionError ([ClassId], [SpanClassRow], ContextEGraph owner HsExprF a ScopeCtx)
insertConvertedModule seeding convertedModule contextGraph0 = do
  (classIds, spanRows, _, stagedBatch) <-
    insertConvertedModuleBase seeding convertedModule contextGraph0
  (_rebaseReport, contextGraph1) <- firstInsertionDelta (commitContextRebaseBatch stagedBatch)
  pure (classIds, spanRows, contextGraph1)

insertConvertedModuleWithMetrics ::
  (Ord a, JoinSemilattice a) =>
  InsertionSeeding a ->
  ConvertedModule ->
  ContextEGraph owner HsExprF a ScopeCtx ->
  Either
    HsExprInsertionError
    ([ClassId], [SpanClassRow], HsExprInsertionMetrics, ContextEGraph owner HsExprF a ScopeCtx)
insertConvertedModuleWithMetrics seeding convertedModule contextGraph0 = do
  let scopeIndex = cmScopeIndex convertedModule
      baseGraph0 = cegBase contextGraph0
  (classIds, spanRows, scopedMetrics, stagedBatch) <-
    insertConvertedModuleBase seeding convertedModule contextGraph0
  dirtyContexts <- firstInsertionDelta (contextRebaseBatchDirtyContexts stagedBatch)
  (_rebaseReport, contextGraph1) <- firstInsertionDelta (commitContextRebaseBatch stagedBatch)
  let rebaseDirtyContextCount = Set.size dirtyContexts
      baseGraph1 = cegBase contextGraph1
      finalSnapshot = contextStructureSnapshot contextGraph1
      rebaseCount =
        if simScopedExprCount scopedMetrics == 0
          then 0
          else 1
      insertionMetrics =
        HsExprInsertionMetrics
          { himBindingCount = length (cmBindings convertedModule),
            himScopedExprCount = simScopedExprCount scopedMetrics,
            himObservedContextCount = scopeObservedCount scopeIndex,
            himTotalSupportContextCount = simTotalSupportContextCount scopedMetrics,
            himMaxSupportContextCount = simMaxSupportContextCount scopedMetrics,
            himRebaseCount = rebaseCount,
            himRebaseDirtyContextCount = rebaseDirtyContextCount,
            himFinalActiveContextCount = cssActiveContextCount finalSnapshot,
            himFinalRestrictionCount = cssRestrictionCount finalSnapshot,
            himFinalChangedContextCount = cssChangedContextCount finalSnapshot,
            himFinalClassSupportEntryCount = cssClassSupportEntryCount finalSnapshot,
            himFinalStoredSupportContextCount = cssSupportCarrierGeneratorCount finalSnapshot,
            himFinalRegionalParentEdgeCount = cssRegionalParentEdgeCount finalSnapshot,
            himFinalRegionalParentRegionCubeCount = cssRegionalParentRegionCubeCount finalSnapshot,
            himFinalRegionalVariantRowCount = cssRegionalVariantRowCount finalSnapshot,
            himFinalRegionalAbsorbedRowCount = cssRegionalAbsorbedRowCount finalSnapshot,
            himFinalContextAnalysisDeltaCount = cssContextAnalysisDeltaCount finalSnapshot,
            himBaseNodeCountBefore = eGraphNodeCount baseGraph0,
            himBaseNodeCountAfter = eGraphNodeCount baseGraph1,
            himBaseClassCountBefore = eGraphClassCount baseGraph0,
            himBaseClassCountAfter = eGraphClassCount baseGraph1
          }
  pure (classIds, spanRows, insertionMetrics, contextGraph1)

insertConvertedModuleBase ::
  (Ord a, JoinSemilattice a) =>
  InsertionSeeding a ->
  ConvertedModule ->
  ContextEGraph owner HsExprF a ScopeCtx ->
  Either
    HsExprInsertionError
    ([ClassId], [SpanClassRow], ScopedInsertionMetrics, ContextRebaseBatch owner HsExprF a ScopeCtx)
insertConvertedModuleBase seeding convertedModule contextGraph0 = do
  let scopeIndex = cmScopeIndex convertedModule
  (stagedBatch, reversedBindingResults) <-
    foldM
      ( \(batchValue, bindingResults) bindingValue -> do
          (classId, _, spanRows, metricsValue, nextBatch) <-
            insertTopLevelSpannedExprWithDeferredRefreshMetrics
              seeding
              scopeIndex
              (tlbScopedTerm bindingValue)
              (tlbSpannedTerm bindingValue)
              batchValue
          pure (nextBatch, (classId, spanRows, metricsValue) : bindingResults)
      )
      (beginContextRebaseBatch contextGraph0, [])
      (cmBindings convertedModule)
  let bindingResults = reverse reversedBindingResults
  pure
    ( fmap (\(classId, _, _) -> classId) bindingResults,
      foldMap (\(_, spanRows, _) -> spanRows) bindingResults,
      foldMap (\(_, _, metricsValue) -> metricsValue) bindingResults,
      stagedBatch
    )

insertTopLevelSpannedExprWithDeferredRefreshMetrics ::
  (Ord a, JoinSemilattice a) =>
  InsertionSeeding a ->
  ScopeIndex ->
  ScopedExpr ->
  SpannedExpr ->
  ContextRebaseBatch owner HsExprF a ScopeCtx ->
  Either
    HsExprInsertionError
    (ClassId, a, [SpanClassRow], ScopedInsertionMetrics, ContextRebaseBatch owner HsExprF a ScopeCtx)
insertTopLevelSpannedExprWithDeferredRefreshMetrics =
  insertSpannedExprWithDeferredRefreshMetrics ClosedAtOccurrence

insertSpannedExprWithDeferredRefreshMetrics ::
  (Ord a, JoinSemilattice a) =>
  ClosedExpressionSupport ->
  InsertionSeeding a ->
  ScopeIndex ->
  ScopedExpr ->
  SpannedExpr ->
  ContextRebaseBatch owner HsExprF a ScopeCtx ->
  Either
    HsExprInsertionError
    (ClassId, a, [SpanClassRow], ScopedInsertionMetrics, ContextRebaseBatch owner HsExprF a ScopeCtx)
insertSpannedExprWithDeferredRefreshMetrics closedSupport seeding scopeIndex scopedExpr spannedExpr batch0 = do
  nodePairs <-
    maybe
      (Left (HsExprSpanLockstepError (sxRegion spannedExpr)))
      Right
      (zipSpannedNode (seNode scopedExpr) (sxNode spannedExpr))
  (batch1, childResults) <-
    traverseAccumEither
      ( \batchValue (childScoped, childSpanned) -> do
          (childClassId, childAnalysis, childSpanRows, childMetricsValue, nextBatch) <-
            insertSpannedExprWithDeferredRefreshMetrics ClosedAtRoot seeding scopeIndex childScoped childSpanned batchValue
          pure (nextBatch, (childClassId, childAnalysis, childSpanRows, childMetricsValue))
      )
      batch0
      nodePairs
  let childClassIds =
        fmap
          (\(childClassId, _, _, _) -> childClassId)
          childResults
      childAnalyses =
        fmap
          (\(_, childAnalysis, _, _) -> childAnalysis)
          childResults
      childSpanRows =
        foldMap
          (\(_, _, spanRows, _) -> spanRows)
          childResults
      childMetrics =
        foldMap
          (\(_, _, _, metricsValue) -> metricsValue)
          childResults
      baseGraph1 = contextRebaseBatchBaseGraph batch1
      nodeAnalysis = applySeed seeding (sxRegion spannedExpr) (asMake (eGraphAnalysisSpec baseGraph1) childAnalyses)
      supportValue = supportForScopedExprWith closedSupport scopeIndex scopedExpr
  (classId, batch2) <-
    firstInsertionDelta
      ( stageENodeWithSupport
          supportValue
          (ENode childClassIds)
          nodeAnalysis
          batch1
      )
  let baseGraph2 = contextRebaseBatchBaseGraph batch2
      classAnalysis = maybe nodeAnalysis eClassData (lookupEClass baseGraph2 classId)
      supportContextCount =
        supportContextWidthWith closedSupport scopeIndex (contextRebaseBatchSite batch2) scopedExpr
      localMetrics =
        mempty
          { simScopedExprCount = 1,
            simTotalSupportContextCount = supportContextCount,
            simMaxSupportContextCount = supportContextCount
          }
      localSpanRows =
        maybe
          []
          (\region -> [SpanClassRow {scrRegion = region, scrClass = classId}])
          (sxRegion spannedExpr)
  pure (classId, classAnalysis, childSpanRows <> localSpanRows, childMetrics <> localMetrics, batch2)

firstInsertionDelta ::
  Either (ContextDeltaError HsExprF ScopeCtx) value ->
  Either HsExprInsertionError value
firstInsertionDelta =
  first HsExprContextDeltaError

zipSpannedNode :: HsExprF ScopedExpr -> HsExprF SpannedExpr -> Maybe (HsExprF (ScopedExpr, SpannedExpr))
zipSpannedNode scopedNode spannedNode =
  zipSameNodeShape scopedNode spannedNode

hsExprSupportedLawRules ::
  ConvertedModule ->
  Either HsExprSiteRuleError [(HsExprSiteRuleKind, SheafTwist.SupportedRuleSpec ScopeCtx (RewriteRule HsExprF))]
hsExprSupportedLawRules convertedModule = do
  let scopeIndex = cmScopeIndex convertedModule
  lambdaRules <-
    fold
      <$> traverse
        ( \binderAnn ->
            do
              siteScope <- first HsExprSiteRuleScopeLookupFailure (binderSiteScope scopeIndex (baId binderAnn))
              let siteCtx = ActualScope siteScope
              sequence
                [ (,) HsExprEtaRule
                    <$> supportedRuleAtWithCondition
                    1
                    binderAnn
                    siteCtx
                    (scopeVisibilityCondition siteCtx [EGraph.mkPatternVar 0])
                    (etaReductionSpanFor binderAnn),
                  (,) HsExprCompositionRule
                    <$> supportedRuleAtWithCondition
                    2
                    binderAnn
                    siteCtx
                    (scopeVisibilityCondition siteCtx [EGraph.mkPatternVar 0, EGraph.mkPatternVar 1])
                    (compositionSpanFor binderAnn),
                  (,) HsExprBetaRule
                    <$> supportedRuleAt 3 binderAnn siteCtx (betaReductionSpanFor binderAnn)
                ]
        )
        (cmLambdaSites convertedModule)
  letRules <-
    traverse
      ( \(binderAnn, provenanceValue) ->
          do
            siteScope <- first HsExprSiteRuleScopeLookupFailure (binderSiteScope scopeIndex (baId binderAnn))
            let siteCtx = ActualScope siteScope
            (,) HsExprLetRule
              <$> supportedRuleAt 4 binderAnn siteCtx (letReductionSpanFor binderAnn provenanceValue)
      )
      (cmLetSites convertedModule)
  pure (lambdaRules <> letRules)
  where
    supportedRuleAt ::
      Int ->
      BinderAnn ->
      ScopeCtx ->
      Either (PatternRewriteError HsExprF) (RewriteMorphism HsExprF) ->
      Either HsExprSiteRuleError (SheafTwist.SupportedRuleSpec ScopeCtx (RewriteRule HsExprF))
    supportedRuleAt salt binderAnn scopeCtx spanResult = do
      spanValue <- first HsExprSiteRuleRewriteFailure spanResult
      pure
        SheafTwist.SupportedRuleSpec
          { SheafTwist.srsSupport = principalSupport scopeCtx,
            SheafTwist.srsRule = rewriteRuleFromSpan (ruleIdFor salt (baId binderAnn)) spanValue
          }
    supportedRuleAtWithCondition ::
      Int ->
      BinderAnn ->
      ScopeCtx ->
      RewriteCondition ScopeCtx HsExprF ->
      Either (PatternRewriteError HsExprF) (RewriteMorphism HsExprF) ->
      Either HsExprSiteRuleError (SheafTwist.SupportedRuleSpec ScopeCtx (RewriteRule HsExprF))
    supportedRuleAtWithCondition salt binderAnn scopeCtx condition spanResult = do
      spanValue <- first HsExprSiteRuleRewriteFailure spanResult
      pure
        SheafTwist.SupportedRuleSpec
          { SheafTwist.srsSupport = principalSupport scopeCtx,
            SheafTwist.srsRule =
              rewriteRuleFromSpanWithCondition
                (ruleIdFor salt (baId binderAnn))
                (Just condition)
                spanValue
          }

hsExprSupportedRules ::
  ConvertedModule ->
  Either HsExprSiteRuleError [SheafTwist.SupportedRuleSpec ScopeCtx (RewriteRule HsExprF)]
hsExprSupportedRules =
  fmap (fmap snd) . hsExprSupportedLawRules

hsExprSupportRuleMetrics :: ConvertedModule -> HsExprSupportRuleMetrics
hsExprSupportRuleMetrics convertedModule =
  let lambdaSiteCount = length (cmLambdaSites convertedModule)
      letSiteCount = length (cmLetSites convertedModule)
      etaRuleCount = lambdaSiteCount
      compositionRuleCount = lambdaSiteCount
      betaRuleCount = lambdaSiteCount
      letRuleCount = letSiteCount
      totalRuleCount = etaRuleCount + compositionRuleCount + betaRuleCount + letRuleCount
   in HsExprSupportRuleMetrics
        { hsrmLambdaSiteCount = lambdaSiteCount,
          hsrmLetSiteCount = letSiteCount,
          hsrmEtaRuleCount = etaRuleCount,
          hsrmCompositionRuleCount = compositionRuleCount,
          hsrmBetaRuleCount = betaRuleCount,
          hsrmLetRuleCount = letRuleCount,
          hsrmTotalRuleCount = totalRuleCount,
          hsrmDiagnosticSpanCount = totalRuleCount
        }

matchesHsExprSpanLhs :: RewriteMorphism HsExprF -> Pattern HsExprF -> Bool
matchesHsExprSpanLhs spanValue patternValue =
  maybe False (const True) (matchHsExprPatternSubstitution (prLeft spanValue) patternValue)

applyHsExprSpanAtRoot :: RewriteMorphism HsExprF -> Pattern HsExprF -> Maybe (Pattern HsExprF)
applyHsExprSpanAtRoot spanValue patternValue = do
  substitution <- matchHsExprPatternSubstitution (prLeft spanValue) patternValue
  let instantiatedRight = instantiateHsExprPattern substitution (prRight spanValue)
  either
    (const Nothing)
    Just
    ( maybe
        (Right instantiatedRight)
        (\postSubst -> applyPostMatchSubst hsExprBinderSubstAlgebra substitution postSubst instantiatedRight)
        (ldPostSubst (prDecoration spanValue))
    )

hsExprDiagnosticSpans ::
  ConvertedModule ->
  Either (PatternRewriteError HsExprF) [RewriteMorphism HsExprF]
hsExprDiagnosticSpans convertedModule =
  let lambdaSpans =
        traverse
          (\binderAnn ->
             sequence
               [ etaReductionSpanFor binderAnn,
                 compositionSpanFor binderAnn,
                 betaReductionSpanFor binderAnn
               ]
          )
          (cmLambdaSites convertedModule)
      letSpans =
        traverse
          (uncurry letReductionSpanFor)
          (cmLetSites convertedModule)
   in (<>) <$> (fold <$> lambdaSpans) <*> letSpans

rewriteRuleFromSpan :: RewriteRuleId -> RewriteMorphism HsExprF -> RewriteRule HsExprF
rewriteRuleFromSpan rewriteRuleId spanValue =
  rewriteRuleFromSpanWithCondition rewriteRuleId Nothing spanValue

rewriteRuleFromSpanWithCondition ::
  RewriteRuleId ->
  Maybe (RewriteCondition ScopeCtx HsExprF) ->
  RewriteMorphism HsExprF ->
  RewriteRule HsExprF
rewriteRuleFromSpanWithCondition rewriteRuleId maybeCondition spanValue =
  RawRewriteRule
    { rrId = rewriteRuleId,
      rrLhs = prLeft spanValue,
      rrRhs = prRight spanValue,
      rrCondition = maybeCondition,
      rrApplicationCondition = Nothing,
      rrPostSubst = ldPostSubst (prDecoration spanValue)
    }

scopeVisibilityCondition :: ScopeCtx -> [EGraph.PatternVar] -> RewriteCondition ScopeCtx HsExprF
scopeVisibilityCondition scopeCtx =
  mconcat
    . fmap
      (\patternVar -> RewriteCondition (guardHasCapability scopeCtx [GuardVar patternVar]))

supportForScopedExprWith :: ClosedExpressionSupport -> ScopeIndex -> ScopedExpr -> SupportBasis ScopeCtx
supportForScopedExprWith closedSupport scopeIndex scopedExpr =
  case (closedSupport, freeScopeSummarySize (seFreeScopes scopedExpr)) of
    (ClosedAtOccurrence, 0) ->
      principalSupport (ActualScope (seOccScope scopedExpr))
    _ ->
      principalSupport rootSupport
  where
    rootSupport =
      ActualScope (freeScopeSupportAnchor scopeIndex (seFreeScopes scopedExpr))

ruleIdFor :: Int -> BinderId -> RewriteRuleId
ruleIdFor salt binderId =
  RewriteRuleId (binderIdKey binderId * 10 + salt)

compositionOperatorName :: RdrName
compositionOperatorName =
  mkRdrUnqual (mkVarOcc ".")

type ScopedInsertionMetrics :: Type
data ScopedInsertionMetrics = ScopedInsertionMetrics
  { simScopedExprCount :: !Int,
    simTotalSupportContextCount :: !Int,
    simMaxSupportContextCount :: !Int
  }

instance Semigroup ScopedInsertionMetrics where
  leftMetrics <> rightMetrics =
    ScopedInsertionMetrics
      { simScopedExprCount = simScopedExprCount leftMetrics + simScopedExprCount rightMetrics,
        simTotalSupportContextCount = simTotalSupportContextCount leftMetrics + simTotalSupportContextCount rightMetrics,
        simMaxSupportContextCount = max (simMaxSupportContextCount leftMetrics) (simMaxSupportContextCount rightMetrics)
      }

instance Monoid ScopedInsertionMetrics where
  mempty =
    ScopedInsertionMetrics
      { simScopedExprCount = 0,
        simTotalSupportContextCount = 0,
        simMaxSupportContextCount = 0
      }

supportContextWidthWith :: ClosedExpressionSupport -> ScopeIndex -> PreparedContextSite owner ScopeCtx -> ScopedExpr -> Int
supportContextWidthWith closedSupport scopeIndex site scopedExpr =
  either
    (const 0)
    Set.size
    (preparedSupportObjects site (supportForScopedExprWith closedSupport scopeIndex scopedExpr))
