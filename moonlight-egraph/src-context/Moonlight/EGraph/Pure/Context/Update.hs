{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

module Moonlight.EGraph.Pure.Context.Update
  ( withEmptyContextEGraph,
    emptyContextEGraphFromSite,
    ContextDeltaError (..),
    ContextRepairScope (..),
    ContextMergePlan,
    RebaseScope,
    ContextRebaseBatch,
    ContextRebaseReport (..),
    ContextMutationTrace (..),
    emptyContextMutationTrace,
    contextMutationTraceFromBase,
    appendContextMutationTrace,
    contextMutationTraceEffect,
    contextMutationTraceTouchedKeys,
    beginContextRebaseBatch,
    contextRebaseBatchBaseGraph,
    contextRebaseBatchSite,
    contextRebaseBatchClassSupportIndex,
    contextRebaseBatchTrace,
    contextRebaseBatchDirtyContexts,
    stageSupportClass,
    stageENodeWithSupport,
    stageTermWithSupport,
    stageTermGlobally,
    stageTermsGlobally,
    stageTermAtContext,
    planContextMerges,
    stageContextMerges,
    stageGlobalMerge,
    prepareContextProjectionAt,
    contextRepairScopeFromCachedObjects,
    prepareContextRebuildForScope,
    commitContextRebaseBatch,
    contextMerge,
    globalMerge,
    rebaseAffectedContexts,
    rebaseContextGraphAtContexts,
    rebaseContextGraphWithSupport,
  )
where

import Control.Monad (foldM)
import Control.Monad.Trans.State.Strict (StateT (..), runStateT)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core
  ( Language,
    UnionFindAllocationError,
  )
import Moonlight.Core.EGraph.Program
  ( EGraphProgramEffect,
    emptyEGraphProgramEffect,
    repeatEGraphProgramEffect,
    requiredClassMergeEffect,
  )
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec (asMake))
import Moonlight.EGraph.Pure.Change
  ( EGraphMutationResult (..),
    EGraphMutationTrace,
    ObservedClassUnions,
    appendEGraphMutationTrace,
    eGraphMutationTraceEffect,
    emptyEGraphMutationTrace,
    emtObservedClassUnions,
    emtTouchedClassKeys,
    observedClassUnionKeys,
    observedClassUnionCount,
    observedClassUnionPairs,
    observedClassUnionsNull,
    observedClassUnions,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons
  ( canonicalizeENodeByTheory,
    canonicalizeENodePure,
    insertENodeTracked,
    insertTermTracked,
    insertTermsTracked,
    insertTermTrackedWithClassFootprint,
  )
import Moonlight.EGraph.Pure.Context.AnnotatedDelta
  ( RegionalClosureObstruction (..),
    advanceAnnotatedDeltaCacheAtUnions,
    appendAnnotatedDeltaFrontier,
    annotatedRepresentativeKeyAt,
    bucketFrontierBetween,
    deriveAnnotatedDeltaBuckets,
    emptyAnnotatedDeltaCache,
    freshAnnotatedDeltaCache,
  )
import Moonlight.EGraph.Pure.Context.Core
  ( contextCachedObjectsForExecution,
    deriveContextAnalysisDeltaAtKey,
  )
import Moonlight.EGraph.Pure.Context.Internal.Store
  ( AnnotatedDeltaAdvanceMode (..),
    AnnotatedDeltaCache (..),
    AnnotatedDeltaCacheAdvance (..),
    ContextEGraph (..),
    ContextFiber (..),
    ContextRuntimeState (..),
    bumpContextRevision,
    emptyContextRuntimeState,
  )
import Moonlight.EGraph.Pure.Rebuild (equateClassesTracked, rebuildTracked)
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    EGraph,
    ENode (..),
    canonicalizeClassId,
    classIdKey,
    eGraphAnalysis,
    eGraphAnalysisSpec,
    eGraphRevision,
    eGraphTheorySpec,
  )
import Data.Fix (Fix (..))
import Numeric.Natural (Natural)
import Moonlight.Sheaf.Context.Core
  ( settledPropagationReport,
  )
import Moonlight.Sheaf.Context.Site
  ( ClassSupportDelta,
    ClassSupportIndex,
    PreparedContextSite,
    PreparedContextSupportError,
    SupportCarrier,
    appendClassSupportDelta,
    classSupportDeltaEmpty,
    classSupportDeltaTouchedClassKeys,
    classSupportIndexExplicitClassKeys,
    classSupportIndexInsertMany,
    classSupportIndexMergeInto,
    contextObjectKeyFor,
    contextObjectKeyValue,
    defaultPreparedSupport,
    emptyClassSupportDelta,
    emptyClassSupportIndex,
    normalizePreparedSupport,
    withPreparedContextSiteFromFiniteLattice,
    supportCarrierContainsKey,
    supportCarrierFromSupport,
    supportCarrierUnion,
    unionPreparedSupport,
  )
import Moonlight.Sheaf.Context.Algebra
  ( classSupportFor,
    propagationTargets,
  )
import Moonlight.Sheaf.Section.Congruence.Equivalence.Relation
import Moonlight.FiniteLattice
  ( ContextLattice
  )
import Moonlight.FiniteLattice
  ( SupportBasis,
    principalSupport,
    supportGenerators
  )
type ContextDeltaError :: (Type -> Type) -> Type -> Type
data ContextDeltaError f c
  = ContextSupportSiteFailed !(PreparedContextSupportError c)
  | ContextLocalUnionCanonicalizationFailed !EquivalenceRelationError
  | ContextRegionalClosureFailed !(RegionalClosureObstruction c)
  | ContextClassIdAllocationFailed !UnionFindAllocationError
  | ContextConstructionAfterMerge
  deriving stock (Eq, Ord, Show)

type ContextRepairScope :: Type -> Type
newtype ContextRepairScope c = ContextRepairScope
  { contextRepairScopeObjects :: Set.Set c
  }
  deriving stock (Eq, Ord, Show)

type RebaseSupportDemand :: Type -> Type -> Type
data RebaseSupportDemand owner c
  = RebaseNoSupport
  | RebaseAllSupport !(Set.Set c)
  | RebaseLimitedSupport !(SupportCarrier owner c) !(Set.Set c)

type role RebaseSupportDemand nominal nominal

type RebaseScope :: Type -> Type -> Type
data RebaseScope owner c = RebaseScope
  { rsExplicitContexts :: !(Set.Set c),
    rsSupportDemand :: !(RebaseSupportDemand owner c)
  }

type role RebaseScope nominal nominal

type ContextRebaseBatch :: Type -> (Type -> Type) -> Type -> Type -> Type
data ContextRebaseBatch owner f a c = ContextRebaseBatch
  { crbGraph :: !(ContextEGraph owner f a c),
    crbScope :: !(RebaseScope owner c),
    crbTrace :: !(ContextMutationTrace owner c f),
    crbOriginContextRevision :: !Natural
  }

type role ContextRebaseBatch nominal nominal nominal nominal

type ContextRebaseReport :: Type -> (Type -> Type) -> Type -> Type
data ContextRebaseReport owner f c = ContextRebaseReport
  { crrScope :: !(RebaseScope owner c),
    crrTrace :: !(ContextMutationTrace owner c f),
    crrContextRevisionBefore :: !Natural,
    crrContextRevisionAfter :: !Natural
  }

type role ContextRebaseReport nominal nominal nominal

type ContextMutationTrace :: Type -> Type -> (Type -> Type) -> Type
data ContextMutationTrace owner c f = ContextMutationTrace
  { cmtBaseTrace :: !(EGraphMutationTrace f),
    cmtContextTouchedKeys :: !IntSet,
    cmtDirtyContexts :: !(Set.Set c),
    cmtObservedLocalUnions :: !ObservedClassUnions,
    cmtObservedLocalUnionsByContext :: !(Map.Map c ObservedClassUnions),
    cmtSupportDelta :: !(ClassSupportDelta owner c)
  }

type role ContextMutationTrace nominal nominal nominal

emptyContextMutationTrace ::
  EGraph f a ->
  ContextMutationTrace owner c f
emptyContextMutationTrace graph =
  contextMutationTraceFromBase (emptyEGraphMutationTrace graph)
{-# INLINE emptyContextMutationTrace #-}

contextMutationTraceFromBase ::
  EGraphMutationTrace f ->
  ContextMutationTrace owner c f
contextMutationTraceFromBase baseTrace =
  ContextMutationTrace
    { cmtBaseTrace = baseTrace,
      cmtContextTouchedKeys = IntSet.empty,
      cmtDirtyContexts = Set.empty,
      cmtObservedLocalUnions = mempty,
      cmtObservedLocalUnionsByContext = Map.empty,
      cmtSupportDelta = emptyClassSupportDelta
    }
{-# INLINE contextMutationTraceFromBase #-}

appendContextMutationTrace ::
  Ord c =>
  ContextMutationTrace owner c f ->
  ContextMutationTrace owner c f ->
  ContextMutationTrace owner c f
appendContextMutationTrace leftTrace rightTrace =
  ContextMutationTrace
    { cmtBaseTrace = appendEGraphMutationTrace (cmtBaseTrace leftTrace) (cmtBaseTrace rightTrace),
      cmtContextTouchedKeys = cmtContextTouchedKeys leftTrace <> cmtContextTouchedKeys rightTrace,
      cmtDirtyContexts = Set.union (cmtDirtyContexts leftTrace) (cmtDirtyContexts rightTrace),
      cmtObservedLocalUnions = cmtObservedLocalUnions leftTrace <> cmtObservedLocalUnions rightTrace,
      cmtObservedLocalUnionsByContext =
        Map.unionWith
          (<>)
          (cmtObservedLocalUnionsByContext leftTrace)
          (cmtObservedLocalUnionsByContext rightTrace),
      cmtSupportDelta =
        appendClassSupportDelta (cmtSupportDelta leftTrace) (cmtSupportDelta rightTrace)
    }
{-# INLINE appendContextMutationTrace #-}

contextMutationTraceEffect ::
  ContextMutationTrace owner c f ->
  EGraphProgramEffect
contextMutationTraceEffect traceValue =
  eGraphMutationTraceEffect (cmtBaseTrace traceValue)
    <> repeatEGraphProgramEffect
      (observedClassUnionCount (cmtObservedLocalUnions traceValue))
      requiredClassMergeEffect
{-# INLINE contextMutationTraceEffect #-}

contextMutationTraceTouchedKeys ::
  ContextMutationTrace owner c f ->
  IntSet
contextMutationTraceTouchedKeys traceValue =
  emtTouchedClassKeys (cmtBaseTrace traceValue)
    <> cmtContextTouchedKeys traceValue
    <> classSupportDeltaTouchedClassKeys (cmtSupportDelta traceValue)
{-# INLINE contextMutationTraceTouchedKeys #-}

contextMutationTraceNull ::
  ContextMutationTrace owner c f ->
  Bool
contextMutationTraceNull traceValue =
  contextMutationTraceEffect traceValue == emptyEGraphProgramEffect
    && IntSet.null (contextMutationTraceTouchedKeys traceValue)
    && Set.null (cmtDirtyContexts traceValue)
    && Map.null (cmtObservedLocalUnionsByContext traceValue)
    && classSupportDeltaEmpty (cmtSupportDelta traceValue)
{-# INLINE contextMutationTraceNull #-}

contextMutationTraceWithDirtyContexts ::
  Ord c =>
  Set.Set c ->
  ContextMutationTrace owner c f ->
  ContextMutationTrace owner c f
contextMutationTraceWithDirtyContexts dirtyContexts traceValue =
  traceValue
    { cmtDirtyContexts = Set.union dirtyContexts (cmtDirtyContexts traceValue)
    }
{-# INLINE contextMutationTraceWithDirtyContexts #-}

withEmptyContextEGraph ::
  Ord c =>
  ContextLattice c ->
  EGraph f a ->
  (forall owner. ContextEGraph owner f a c -> result) ->
  result
withEmptyContextEGraph contextLatticeValue baseGraph useContextGraph =
  withPreparedContextSiteFromFiniteLattice
    contextLatticeValue
    (useContextGraph . flip emptyContextEGraphFromSite baseGraph)
{-# INLINE withEmptyContextEGraph #-}

emptyContextEGraphFromSite :: PreparedContextSite owner c -> EGraph f a -> ContextEGraph owner f a c
emptyContextEGraphFromSite contextSite baseGraph =
  let emptyGraph =
        ContextEGraph
          { cegBase = baseGraph,
            cegSite = contextSite,
            cegContextFibers = Map.empty,
            cegClassSupport = emptyClassSupportIndex,
            cegContextAnalysisDeltas = Map.empty,
            cegAnnotatedDeltaCache = emptyAnnotatedDeltaCache baseGraph 0,
            cegContextRevision = 0,
            cegRuntimeState = emptyContextRuntimeState
          }
   in emptyGraph
{-# INLINE emptyContextEGraphFromSite #-}

refreshContextDerivedState ::
  (Language f, Ord c) =>
  ContextEGraph owner f a c ->
  Either (ContextDeltaError f c) (ContextEGraph owner f a c)
refreshContextDerivedState contextGraph = do
  repairedFibers <-
    first ContextLocalUnionCanonicalizationFailed
      (traverse (repairContextFiber baseGraph) (cegContextFibers contextGraph))
  let graphWithFibers = contextGraph {cegContextFibers = repairedFibers}
  refreshedCache <-
    first ContextRegionalClosureFailed (freshAnnotatedDeltaCache graphWithFibers)
  let graphWithCache = graphWithFibers {cegAnnotatedDeltaCache = refreshedCache}
  refreshedContextSections <-
    Map.traverseWithKey
      ( \contextValue _ -> do
          contextKey <-
            first
              ContextSupportSiteFailed
              (contextObjectKeyFor (cegSite graphWithCache) contextValue)
          pure (contextKey, deriveContextAnalysisDeltaAtKey contextKey graphWithCache)
      )
      (cegContextAnalysisDeltas graphWithCache)
  let refreshedAnalysisDeltas = fmap snd refreshedContextSections
      stepFrontier =
        bucketFrontierBetween
          (fmap fst (Map.elems refreshedContextSections))
          (adcBuckets (cegAnnotatedDeltaCache contextGraph))
          (adcBuckets refreshedCache)
      refreshedFrontier =
        IntMap.unionWith
          appendAnnotatedDeltaFrontier
          (adcDirtyFrontierByContextKey (cegAnnotatedDeltaCache contextGraph))
          stepFrontier
      cacheWithFrontier =
        refreshedCache {adcDirtyFrontierByContextKey = refreshedFrontier}
  pure
    ( graphWithCache
        { cegAnnotatedDeltaCache = cacheWithFrontier,
          cegContextAnalysisDeltas = refreshedAnalysisDeltas
        }
    )
  where
    baseGraph = cegBase contextGraph

replaceClassSupportIfChanged ::
  IntSet.IntSet ->
  ClassSupportDelta owner c ->
  ClassSupportIndex owner c ->
  ContextEGraph owner f a c ->
  ContextEGraph owner f a c
replaceClassSupportIfChanged insertedClassKeys supportDelta supportIndex contextGraph =
  let carrierChanged =
        not (IntSet.null (classSupportDeltaTouchedClassKeys supportDelta))
      entryMaterialized =
        not
          ( IntSet.isSubsetOf
              insertedClassKeys
              (classSupportIndexExplicitClassKeys (cegClassSupport contextGraph))
          )
   in if carrierChanged || entryMaterialized
        then bumpSupportContextRevision contextGraph {cegClassSupport = supportIndex}
        else contextGraph
{-# INLINE replaceClassSupportIfChanged #-}

replaceClassSupportAfterMerge ::
  ClassSupportDelta owner c ->
  ClassSupportIndex owner c ->
  ContextEGraph owner f a c ->
  ContextEGraph owner f a c
replaceClassSupportAfterMerge supportDelta supportIndex contextGraph =
  if classSupportDeltaEmpty supportDelta
    then contextGraph
    else bumpSupportContextRevision contextGraph {cegClassSupport = supportIndex}
{-# INLINE replaceClassSupportAfterMerge #-}

bumpSupportContextRevision :: ContextEGraph owner f a c -> ContextEGraph owner f a c
bumpSupportContextRevision contextGraph =
  let previousContextRevision = cegContextRevision contextGraph
      previousCache = cegAnnotatedDeltaCache contextGraph
      bumpedGraph = bumpContextRevision contextGraph
      cacheAtRevision =
        if adcContextRevision previousCache == previousContextRevision
          then
            previousCache
              { adcContextRevision = cegContextRevision bumpedGraph
              }
          else previousCache
   in bumpedGraph {cegAnnotatedDeltaCache = cacheAtRevision}
{-# INLINE bumpSupportContextRevision #-}

repairContextFiber ::
  EGraph f a ->
  ContextFiber ->
  Either EquivalenceRelationError ContextFiber
repairContextFiber baseGraph fiberValue
  | cfBaseRevision fiberValue == eGraphRevision baseGraph =
      Right fiberValue
  | otherwise =
      let relationValue =
            cfRelation fiberValue
          domainKeys =
            equivalenceDomain relationValue
          remapKey classKey =
            classIdKey (canonicalizeClassId baseGraph (ClassId classKey))
          sourceToTarget =
            IntMap.fromSet (ClassId . remapKey) domainKeys
          targetDomain =
            IntSet.map remapKey domainKeys
       in fmap
            (\repairedRelation -> ContextFiber repairedRelation (eGraphRevision baseGraph))
            (equivalenceImage sourceToTarget targetDomain relationValue)
{-# INLINEABLE repairContextFiber #-}

replaceContextFiber ::
  Ord c =>
  c ->
  ContextFiber ->
  ContextEGraph owner f a c ->
  ContextEGraph owner f a c
replaceContextFiber contextValue fiberValue contextGraph =
  let currentStored =
        Map.lookup contextValue (cegContextFibers contextGraph)
      nextStored =
        if null (equivalencePairs (cfRelation fiberValue))
          then Nothing
          else Just fiberValue
   in if fmap cfRelation currentStored == fmap cfRelation nextStored
        then case nextStored of
          Just nextFiber
            | fmap cfBaseRevision currentStored /= Just (cfBaseRevision nextFiber) ->
                contextGraph
                  { cegContextFibers =
                      Map.insert contextValue nextFiber (cegContextFibers contextGraph)
                  }
          _ ->
            contextGraph
        else
          let graphWithFiber =
                bumpContextRevision
                  contextGraph
                    { cegContextFibers =
                        Map.alter (const nextStored) contextValue (cegContextFibers contextGraph)
                    }
           in graphWithFiber
{-# INLINEABLE replaceContextFiber #-}

stageFiberUnion ::
  Ord c =>
  c ->
  ClassId ->
  ClassId ->
  ContextEGraph owner f a c ->
  Either EquivalenceRelationError (ContextEGraph owner f a c)
stageFiberUnion contextValue canonicalLeft canonicalRight contextGraph
  | canonicalLeft == canonicalRight =
      Right contextGraph
  | otherwise = do
      let baseGraph =
            cegBase contextGraph
      storedFiber <-
        maybe
          (fmap (flip ContextFiber (eGraphRevision baseGraph)) (discreteEquivalence IntSet.empty))
          Right
          (Map.lookup contextValue (cegContextFibers contextGraph))
      repairedFiber <- repairContextFiber baseGraph storedFiber
      extendedRelation <-
        extendEquivalenceDomain
          (IntSet.fromList [classIdKey canonicalLeft, classIdKey canonicalRight])
          (cfRelation repairedFiber)
      mergedRelation <-
        fmap
          equivalenceMergeRelation
          (applyCanonicalEquivalenceSeeds [(canonicalLeft, canonicalRight)] extendedRelation)
      Right
        ( replaceContextFiber
            contextValue
            repairedFiber {cfRelation = mergedRelation}
            contextGraph
        )
{-# INLINEABLE stageFiberUnion #-}

contextMerge ::
  (Language f, Ord c) =>
  c ->
  ClassId ->
  ClassId ->
  ContextEGraph owner f a c ->
  Either (ContextDeltaError f c) (ContextEGraph owner f a c)
contextMerge context leftClassId rightClassId contextGraph =
  fmap snd $ do
    let initialBatch = beginContextRebaseBatch contextGraph
    mergePlan <- planContextMerges [context] leftClassId rightClassId initialBatch
    stageContextMerges mergePlan initialBatch >>= commitContextRebaseBatch

type ContextMergePlan :: Type -> Type
data ContextMergePlan c = ContextMergePlan
  { cmpLeftClass :: !ClassId,
    cmpRightClass :: !ClassId,
    cmpAuthorTargets :: ![(c, Set.Set c)]
  }

type ContextMergeStaging :: Type -> (Type -> Type) -> Type -> Type -> Type
data ContextMergeStaging owner f a c = ContextMergeStaging
  { cmsGraph :: !(ContextEGraph owner f a c),
    cmsChangedAuthorsReversed :: ![c],
    cmsAffectedContexts :: !(Set.Set c),
    cmsRequestedScopeContexts :: !(Set.Set c)
  }

type role ContextMergeStaging nominal nominal nominal nominal

-- | Resolve the compatibility obligation at the construction snapshot. Later
-- support widening cannot broaden an earlier equality's visibility region.
planContextMerges ::
  (Language f, Ord c) =>
  [c] ->
  ClassId ->
  ClassId ->
  ContextRebaseBatch owner f a c ->
  Either (ContextDeltaError f c) (ContextMergePlan c)
planContextMerges authorContexts leftClassId rightClassId batchValue =
  fmap
    (ContextMergePlan leftClassId rightClassId)
    ( traverse
        ( \contextValue ->
            fmap
              ((,) contextValue . Set.fromList)
              ( first
                  ContextSupportSiteFailed
                  (propagationTargets contextValue leftClassId rightClassId (crbGraph batchValue))
              )
        )
        authorContexts
    )

-- | Stage a resolved family of local sections. Fiber truth, scope, and trace
-- advance immediately; descent and gluing occur once at commit.
stageContextMerges ::
  Ord c =>
  ContextMergePlan c ->
  ContextRebaseBatch owner f a c ->
  Either (ContextDeltaError f c) (ContextRebaseBatch owner f a c)
stageContextMerges mergePlan batchValue = do
  let contextGraph = crbGraph batchValue
      baseGraph = cegBase contextGraph
      canonicalLeft =
        canonicalizeClassId baseGraph (cmpLeftClass mergePlan)
      canonicalRight =
        canonicalizeClassId baseGraph (cmpRightClass mergePlan)
      touchedKeys =
        IntSet.fromList [classIdKey canonicalLeft, classIdKey canonicalRight]
      initialStaging =
        ContextMergeStaging
          { cmsGraph = contextGraph,
            cmsChangedAuthorsReversed = [],
            cmsAffectedContexts = Set.empty,
            cmsRequestedScopeContexts = Set.empty
          }
  stagedMerges <-
    foldM
      (stageContextMergeFiber canonicalLeft canonicalRight)
      initialStaging
      (cmpAuthorTargets mergePlan)
  let changedAuthors = reverse (cmsChangedAuthorsReversed stagedMerges)
      changedTouchedKeys =
        if null changedAuthors
          then IntSet.empty
          else touchedKeys
      localUnions =
        if IntSet.null changedTouchedKeys
          then mempty
          else
            observedClassUnions
              (fmap (const (canonicalLeft, canonicalRight)) changedAuthors)
      localUnionsByContext =
        if IntSet.null changedTouchedKeys
          then Map.empty
          else
            Map.fromListWith
              (flip (<>))
              [ (authorContext, observedClassUnions [(canonicalLeft, canonicalRight)])
                | authorContext <- changedAuthors
              ]
  Right
    ( appendBatchContextMerge
        localUnions
        localUnionsByContext
        changedTouchedKeys
        (cmsAffectedContexts stagedMerges)
        batchValue
          { crbGraph = cmsGraph stagedMerges,
            crbScope =
              appendRebaseContexts
                (cmsRequestedScopeContexts stagedMerges)
                (crbScope batchValue)
          }
    )

stageContextMergeFiber ::
  Ord c =>
  ClassId ->
  ClassId ->
  ContextMergeStaging owner f a c ->
  (c, Set.Set c) ->
  Either (ContextDeltaError f c) (ContextMergeStaging owner f a c)
stageContextMergeFiber canonicalLeft canonicalRight staging (contextValue, targetContextSet) = do
  let contextGraph = cmsGraph staging
  graphWithMerge <-
    if Set.member contextValue targetContextSet
      then
        first
          ContextLocalUnionCanonicalizationFailed
          (stageFiberUnion contextValue canonicalLeft canonicalRight contextGraph)
      else Right contextGraph
  let mergeChanged =
        cegContextRevision graphWithMerge /= cegContextRevision contextGraph
  pure
    ContextMergeStaging
      { cmsGraph = graphWithMerge,
        cmsChangedAuthorsReversed =
          if mergeChanged
            then contextValue : cmsChangedAuthorsReversed staging
            else cmsChangedAuthorsReversed staging,
        cmsAffectedContexts =
          if mergeChanged
            then Set.union targetContextSet (cmsAffectedContexts staging)
            else cmsAffectedContexts staging,
        cmsRequestedScopeContexts =
          Set.union targetContextSet (cmsRequestedScopeContexts staging)
      }

prepareContextProjectionAt ::
  (Language f, Ord c) =>
  c ->
  ContextEGraph owner f a c ->
  Either (ContextDeltaError f c) (ContextEGraph owner f a c)
prepareContextProjectionAt contextValue contextGraph =
  case Map.lookup contextValue (cegContextFibers contextGraph) of
    Nothing ->
      Right contextGraph
    Just storedFiber -> do
      repairedFiber <-
        first
          ContextLocalUnionCanonicalizationFailed
          (repairContextFiber (cegBase contextGraph) storedFiber)
      let graphWithRepairedFiber =
            replaceContextFiber contextValue repairedFiber contextGraph
          cache = cegAnnotatedDeltaCache graphWithRepairedFiber
          cacheMatches =
            adcBaseRevision cache == eGraphRevision (cegBase graphWithRepairedFiber)
              && adcContextRevision cache == cegContextRevision graphWithRepairedFiber
      if cfRelation repairedFiber == cfRelation storedFiber && cacheMatches
        then Right graphWithRepairedFiber
        else refreshContextDerivedState graphWithRepairedFiber

contextRepairScopeFromCachedObjects ::
  Ord c =>
  ContextEGraph owner f a c ->
  ContextRepairScope c
contextRepairScopeFromCachedObjects contextGraph =
  ContextRepairScope
    ( Set.union
        (Set.fromList (contextCachedObjectsForExecution contextGraph))
        (crsDirtyContexts (cegRuntimeState contextGraph))
    )
{-# INLINE contextRepairScopeFromCachedObjects #-}

prepareContextRebuildForScope ::
  (Language f, Ord c) =>
  ContextRepairScope c ->
  ContextEGraph owner f a c ->
  Either (ContextDeltaError f c) (ContextEGraph owner f a c)
prepareContextRebuildForScope repairScope contextGraph =
  foldM
    (flip prepareContextProjectionAt)
    contextGraph
    (Set.toAscList (contextRepairScopeObjects repairScope))
{-# INLINE prepareContextRebuildForScope #-}

beginContextRebaseBatch :: ContextEGraph owner f a c -> ContextRebaseBatch owner f a c
beginContextRebaseBatch contextGraph =
  ContextRebaseBatch
    { crbGraph = clearAnnotatedDeltaFrontier contextGraph,
      crbScope = emptyRebaseScope,
      crbTrace = emptyContextMutationTrace (cegBase contextGraph),
      crbOriginContextRevision = cegContextRevision contextGraph
    }

emptyRebaseScope :: RebaseScope owner c
emptyRebaseScope =
  RebaseScope
    { rsExplicitContexts = Set.empty,
      rsSupportDemand = RebaseNoSupport
    }

-- | The staged base is safe for construction passes that never inspect the
-- pending contextual quotient. The latter remains authoritative only after
-- 'commitContextRebaseBatch'.
contextRebaseBatchBaseGraph :: ContextRebaseBatch owner f a c -> EGraph f a
contextRebaseBatchBaseGraph =
  cegBase . crbGraph

contextRebaseBatchSite :: ContextRebaseBatch owner f a c -> PreparedContextSite owner c
contextRebaseBatchSite =
  cegSite . crbGraph

contextRebaseBatchClassSupportIndex :: ContextRebaseBatch owner f a c -> ClassSupportIndex owner c
contextRebaseBatchClassSupportIndex =
  cegClassSupport . crbGraph

contextRebaseBatchTrace :: ContextRebaseBatch owner f a c -> ContextMutationTrace owner c f
contextRebaseBatchTrace =
  crbTrace

contextRebaseBatchDirtyContexts ::
  Ord c =>
  ContextRebaseBatch owner f a c ->
  Either (ContextDeltaError f c) (Set.Set c)
contextRebaseBatchDirtyContexts batchValue =
  rebaseAffectedContexts
    (crbScope batchValue)
    (crbGraph batchValue)

applyBaseMutationToBatch ::
  Ord c =>
  EGraphMutationResult f a result ->
  ContextRebaseBatch owner f a c ->
  Either (ContextDeltaError f c) (result, ContextRebaseBatch owner f a c)
applyBaseMutationToBatch mutationResult batchValue = do
  let mutationTrace =
        contextMutationTraceFromBase (emrTrace mutationResult)
  -- A tracked no-op has no local section to descend. Preserve the compiled
  -- regional predecessor instead of deriving an observationally equal copy.
  if IntSet.null (emtTouchedClassKeys (emrTrace mutationResult))
    then pure (emrResult mutationResult, batchValue)
    else
      pure
        ( emrResult mutationResult,
          appendBatchTrace
            mutationTrace
            ( batchValue
                { crbGraph =
                    (crbGraph batchValue)
                      { cegBase = emrGraph mutationResult
                      }
                }
            )
        )
{-# INLINE applyBaseMutationToBatch #-}

appendBatchTrace ::
  Ord c =>
  ContextMutationTrace owner c f ->
  ContextRebaseBatch owner f a c ->
  ContextRebaseBatch owner f a c
appendBatchTrace traceValue batchValue =
  if contextMutationTraceNull traceValue
    then batchValue
    else
      batchValue
        { crbTrace = appendContextMutationTrace (crbTrace batchValue) traceValue
        }
{-# INLINE appendBatchTrace #-}

contextMutationTraceFromLocalUnions ::
  EGraph f a ->
  ObservedClassUnions ->
  Map.Map c ObservedClassUnions ->
  IntSet ->
  Set.Set c ->
  ContextMutationTrace owner c f
contextMutationTraceFromLocalUnions graph localUnions localUnionsByContext touchedKeys dirtyContexts =
  (emptyContextMutationTrace graph)
    { cmtContextTouchedKeys = touchedKeys,
      cmtDirtyContexts = dirtyContexts,
      cmtObservedLocalUnions = localUnions,
      cmtObservedLocalUnionsByContext = localUnionsByContext
    }
{-# INLINE contextMutationTraceFromLocalUnions #-}

appendBatchContextMerge ::
  Ord c =>
  ObservedClassUnions ->
  Map.Map c ObservedClassUnions ->
  IntSet ->
  Set.Set c ->
  ContextRebaseBatch owner f a c ->
  ContextRebaseBatch owner f a c
appendBatchContextMerge localUnions localUnionsByContext touchedKeys dirtyContexts batchValue =
  if IntSet.null touchedKeys
    then batchValue
    else
      appendBatchTrace
        ( contextMutationTraceFromLocalUnions
            (cegBase (crbGraph batchValue))
            localUnions
            localUnionsByContext
            touchedKeys
            dirtyContexts
        )
        batchValue
{-# INLINE appendBatchContextMerge #-}

contextMutationTraceFromSupportDelta ::
  EGraph f a ->
  ClassSupportDelta owner c ->
  ContextMutationTrace owner c f
contextMutationTraceFromSupportDelta graph supportDelta =
  (emptyContextMutationTrace graph) {cmtSupportDelta = supportDelta}
{-# INLINE contextMutationTraceFromSupportDelta #-}

normalizeClassSupport :: Ord c => PreparedContextSite owner c -> SupportBasis c -> Either (ContextDeltaError f c) (SupportBasis c)
normalizeClassSupport site =
  first ContextSupportSiteFailed . normalizePreparedSupport site

unionClassSupport :: Ord c => PreparedContextSite owner c -> SupportBasis c -> SupportBasis c -> Either (ContextDeltaError f c) (SupportBasis c)
unionClassSupport site leftSupport =
  first ContextSupportSiteFailed . unionPreparedSupport site leftSupport

classSupportForChecked :: (Language f, Ord c) => ClassId -> ContextEGraph owner f a c -> Either (ContextDeltaError f c) (SupportBasis c)
classSupportForChecked classId =
  first ContextSupportSiteFailed . classSupportFor classId

stageSupportClass ::
  Ord c =>
  SupportBasis c ->
  ClassId ->
  ContextRebaseBatch owner f a c ->
  Either (ContextDeltaError f c) (ContextRebaseBatch owner f a c)
stageSupportClass supportValue classId =
  stageSupportClassKeys supportValue (IntSet.singleton (classIdKey classId))

stageSupportClassKeys ::
  Ord c =>
  SupportBasis c ->
  IntSet.IntSet ->
  ContextRebaseBatch owner f a c ->
  Either (ContextDeltaError f c) (ContextRebaseBatch owner f a c)
stageSupportClassKeys supportValue supportClassKeys batchValue = do
  let contextGraph = crbGraph batchValue
      site = cegSite contextGraph
  normalizedSupport <- normalizeClassSupport site supportValue
  stageSupportClassKeysKnown site normalizedSupport supportClassKeys batchValue

clearAnnotatedDeltaFrontier :: ContextEGraph owner f a c -> ContextEGraph owner f a c
clearAnnotatedDeltaFrontier contextGraph =
  contextGraph
    { cegAnnotatedDeltaCache =
        (cegAnnotatedDeltaCache contextGraph)
          { adcDirtyFrontierByContextKey = IntMap.empty
          }
    }
{-# INLINE clearAnnotatedDeltaFrontier #-}

stageSupportClassKeysKnown ::
  Ord c =>
  PreparedContextSite owner c ->
  SupportBasis c ->
  IntSet.IntSet ->
  ContextRebaseBatch owner f a c ->
  Either (ContextDeltaError f c) (ContextRebaseBatch owner f a c)
stageSupportClassKeysKnown site normalizedSupport supportClassKeys batchValue = do
  let contextGraph = crbGraph batchValue
      originalSupport = cegClassSupport contextGraph
  (updatedSupport, supportDelta) <-
    first ContextSupportSiteFailed $
      classSupportIndexInsertMany site normalizedSupport supportClassKeys originalSupport
  accumulatedScope <-
    appendRebaseSupport
      site
      normalizedSupport
      (crbScope batchValue)
  let updatedBatch =
        batchValue
          { crbGraph =
              replaceClassSupportIfChanged supportClassKeys supportDelta updatedSupport contextGraph,
            crbScope = accumulatedScope
          }
  pure
    ( appendBatchTrace
        (contextMutationTraceFromSupportDelta (cegBase contextGraph) supportDelta)
        updatedBatch
    )

appendRebaseSupport ::
  Ord c =>
  PreparedContextSite owner c ->
  SupportBasis c ->
  RebaseScope owner c ->
  Either (ContextDeltaError f c) (RebaseScope owner c)
appendRebaseSupport site supportValue scopeValue =
  appendRebaseSupportWithSeeds
    site
    supportValue
    (Set.fromList (supportGenerators supportValue))
    scopeValue

appendRebaseSupportWithSeeds ::
  Ord c =>
  PreparedContextSite owner c ->
  SupportBasis c ->
  Set.Set c ->
  RebaseScope owner c ->
  Either (ContextDeltaError f c) (RebaseScope owner c)
appendRebaseSupportWithSeeds site supportValue supportSeeds scopeValue = do
  supportDemand <- rebaseSupportDemandFromSupport site supportValue
  case supportDemand of
    RebaseNoSupport ->
      Right scopeValue
    RebaseAllSupport _defaultSeeds ->
      Right (appendRebaseAll supportSeeds scopeValue)
    RebaseLimitedSupport supportCarrier _defaultSeeds ->
      Right (appendRebaseSupportScope site supportCarrier supportSeeds scopeValue)

appendRebaseSupportScope ::
  Ord c =>
  PreparedContextSite owner c ->
  SupportCarrier owner c ->
  Set.Set c ->
  RebaseScope owner c ->
  RebaseScope owner c
appendRebaseSupportScope site supportCarrier supportSeeds scopeValue =
  scopeValue
    { rsSupportDemand =
        appendRebaseSupportDemand
          site
          supportCarrier
          supportSeeds
          (rsSupportDemand scopeValue)
    }

appendRebaseSupportDemand ::
  Ord c =>
  PreparedContextSite owner c ->
  SupportCarrier owner c ->
  Set.Set c ->
  RebaseSupportDemand owner c ->
  RebaseSupportDemand owner c
appendRebaseSupportDemand site supportCarrier supportSeeds supportDemand =
  case supportDemand of
    RebaseNoSupport ->
      RebaseLimitedSupport supportCarrier supportSeeds
    RebaseAllSupport allSeeds ->
      RebaseAllSupport (Set.union supportSeeds allSeeds)
    RebaseLimitedSupport existingCarrier existingSeeds ->
      RebaseLimitedSupport
        (supportCarrierUnion site supportCarrier existingCarrier)
        (Set.union supportSeeds existingSeeds)

appendRebaseContexts ::
  Ord c =>
  Set.Set c ->
  RebaseScope owner c ->
  RebaseScope owner c
appendRebaseContexts contextSet scopeValue =
  scopeValue
    { rsExplicitContexts =
        Set.union contextSet (rsExplicitContexts scopeValue)
    }
{-# INLINE appendRebaseContexts #-}

appendRebaseAll ::
  Ord c =>
  Set.Set c ->
  RebaseScope owner c ->
  RebaseScope owner c
appendRebaseAll supportSeeds scopeValue =
  scopeValue
    { rsSupportDemand =
        RebaseAllSupport
          (Set.union supportSeeds (rebaseSupportDemandSeeds (rsSupportDemand scopeValue)))
    }
{-# INLINE appendRebaseAll #-}

rebaseSupportDemandFromSupport :: Ord c => PreparedContextSite owner c -> SupportBasis c -> Either (ContextDeltaError f c) (RebaseSupportDemand owner c)
rebaseSupportDemandFromSupport site supportValue = do
  let globalSupport = isGlobalSupport site supportValue
      supportSeeds =
        Set.fromList (supportGenerators supportValue)
  if globalSupport
    then pure (RebaseAllSupport supportSeeds)
    else
      fmap
        (\supportCarrier -> RebaseLimitedSupport supportCarrier supportSeeds)
        (first ContextSupportSiteFailed (supportCarrierFromSupport site supportValue))

rebaseSupportDemandSeeds :: RebaseSupportDemand owner c -> Set.Set c
rebaseSupportDemandSeeds supportDemand =
  case supportDemand of
    RebaseNoSupport ->
      Set.empty
    RebaseAllSupport seeds ->
      seeds
    RebaseLimitedSupport _supportCarrier seeds ->
      seeds

stageENodeWithSupport ::
  (Language f, Ord c) =>
  SupportBasis c ->
  ENode f ->
  a ->
  ContextRebaseBatch owner f a c ->
  Either (ContextDeltaError f c) (ClassId, ContextRebaseBatch owner f a c)
stageENodeWithSupport supportValue enodeValue analysisValue batchValue = do
  let initialContextGraph = crbGraph batchValue
      site = cegSite initialContextGraph
  normalizedSupport <- normalizeClassSupport site supportValue
  let globalSupport = isGlobalSupport site normalizedSupport
  readableBatch <-
    prepareBatchForContextualConstruction globalSupport batchValue
  let contextGraph = crbGraph readableBatch
  canonicalizeClass <-
    if globalSupport
      then pure (canonicalizeClassId (cegBase contextGraph))
      else contextualSupportCanonicalizer normalizedSupport contextGraph
  let contextualENode =
        if globalSupport
          then enodeValue
          else canonicalizeENodePure canonicalizeClass enodeValue
  mutationResult <-
    first ContextClassIdAllocationFailed
      (insertENodeTracked contextualENode analysisValue (cegBase contextGraph))
  let supportClassKeys =
        enodeSupportFootprint (emrGraph mutationResult) (emrResult mutationResult) enodeValue
          <> enodeSupportFootprint (emrGraph mutationResult) (emrResult mutationResult) contextualENode
  stageBaseUpdateWithSupportFootprintKnown
    site
    normalizedSupport
    supportClassKeys
    mutationResult
    readableBatch

stageTermWithSupport ::
  (Language f, Ord c) =>
  SupportBasis c ->
  Fix f ->
  ContextRebaseBatch owner f a c ->
  Either (ContextDeltaError f c) (ClassId, ContextRebaseBatch owner f a c)
stageTermWithSupport supportValue term batchValue = do
  let initialContextGraph = crbGraph batchValue
      site = cegSite initialContextGraph
  normalizedSupport <- normalizeClassSupport site supportValue
  let globalSupport = isGlobalSupport site normalizedSupport
  readableBatch <-
    prepareBatchForContextualConstruction globalSupport batchValue
  let contextGraph = crbGraph readableBatch
  canonicalizeClass <-
    if globalSupport
      then pure (canonicalizeClassId (cegBase contextGraph))
      else contextualSupportCanonicalizer normalizedSupport contextGraph
  footprintMutationResult <-
    first ContextClassIdAllocationFailed
      ( if globalSupport
          then insertTermTrackedWithClassFootprint term (cegBase contextGraph)
          else insertTermTrackedWithContextCanonicalizer canonicalizeClass term (cegBase contextGraph)
      )
  let (classId, supportClassKeys) =
        emrResult footprintMutationResult
      mutationResult =
        EGraphMutationResult
          { emrResult = classId,
            emrTrace = emrTrace footprintMutationResult,
            emrGraph = emrGraph footprintMutationResult
          }
  stageBaseUpdateWithSupportFootprintKnown
    site
    normalizedSupport
    supportClassKeys
    mutationResult
    readableBatch

prepareBatchForContextualConstruction ::
  Bool ->
  ContextRebaseBatch owner f a c ->
  Either (ContextDeltaError f c) (ContextRebaseBatch owner f a c)
prepareBatchForContextualConstruction globalSupport batchValue
  | globalSupport = Right batchValue
  | contextMutationTraceHasMerges (crbTrace batchValue) =
      Left ContextConstructionAfterMerge
  | otherwise = Right batchValue

contextMutationTraceHasMerges :: ContextMutationTrace owner c f -> Bool
contextMutationTraceHasMerges traceValue =
  not (observedClassUnionsNull (emtObservedClassUnions (cmtBaseTrace traceValue)))
    || not (Map.null (cmtObservedLocalUnionsByContext traceValue))
{-# INLINE contextMutationTraceHasMerges #-}

type ContextTermInsertion :: (Type -> Type) -> Type -> Type
data ContextTermInsertion f a = ContextTermInsertion
  { ctiClassId :: !ClassId,
    ctiAnalysisData :: !a,
    ctiSupportFootprint :: !IntSet,
    ctiTrace :: !(EGraphMutationTrace f)
  }

insertTermTrackedWithContextCanonicalizer ::
  Language f =>
  (ClassId -> ClassId) ->
  Fix f ->
  EGraph f a ->
  Either UnionFindAllocationError (EGraphMutationResult f a (ClassId, IntSet))
insertTermTrackedWithContextCanonicalizer canonicalize term graph = do
  (insertion, updatedGraph) <-
    runStateT (insertContextTerm term) graph
  pure
    ( EGraphMutationResult
        { emrResult = (ctiClassId insertion, ctiSupportFootprint insertion),
          emrTrace = ctiTrace insertion,
          emrGraph = updatedGraph
        }
    )
  where
    insertContextTerm (Fix termLayer) =
      StateT $ \graphValue -> do
        (childInsertions, graphAfterChildren) <-
          runStateT (traverse insertContextTerm termLayer) graphValue
        let childClassIds =
              fmap ctiClassId childInsertions
            canonicalChildClassIds =
              fmap canonicalize childClassIds
            childAnalysisData =
              fmap (canonicalChildAnalysis graphAfterChildren) (fmap insertionClassAndAnalysis childInsertions)
            childTrace =
              foldl'
                appendEGraphMutationTrace
                (emptyEGraphMutationTrace graphValue)
                (fmap ctiTrace (toList childInsertions))
            nodeAnalysisData =
              asMake (eGraphAnalysisSpec graphAfterChildren) childAnalysisData
        mutationResult <-
          insertENodeTracked (ENode canonicalChildClassIds) nodeAnalysisData graphAfterChildren
        let classId =
              emrResult mutationResult
            traceValue =
              appendEGraphMutationTrace childTrace (emrTrace mutationResult)
            supportFootprint =
              IntSet.insert
                (classIdKey classId)
                (foldMap ctiSupportFootprint childInsertions <> foldMap (IntSet.singleton . classIdKey) canonicalChildClassIds)
        pure
          ( ContextTermInsertion
              { ctiClassId = classId,
                ctiAnalysisData = nodeAnalysisData,
                ctiSupportFootprint = supportFootprint,
                ctiTrace = traceValue
              },
            emrGraph mutationResult
          )
    insertionClassAndAnalysis insertion =
      (canonicalize (ctiClassId insertion), ctiAnalysisData insertion)
    canonicalChildAnalysis ::
      EGraph f a ->
      (ClassId, a) ->
      a
    canonicalChildAnalysis graphValue (classId, fallbackAnalysis) =
      maybe
        fallbackAnalysis
        id
        (IntMap.lookup (classIdKey classId) (eGraphAnalysis graphValue))
{-# INLINE insertTermTrackedWithContextCanonicalizer #-}

contextualSupportCanonicalizer ::
  Ord c =>
  SupportBasis c ->
  ContextEGraph owner f a c ->
  Either (ContextDeltaError f c) (ClassId -> ClassId)
contextualSupportCanonicalizer supportValue contextGraph = do
  let canonicalizeBase =
        canonicalizeClassId (cegBase contextGraph)
  generatorKeys <-
    first ContextSupportSiteFailed
      ( traverse
          (contextObjectKeyFor (cegSite contextGraph))
          (supportGenerators supportValue)
      )
  let canonicalizeAt contextKey classId =
        let baseClass = canonicalizeBase classId
         in ClassId
              ( annotatedRepresentativeKeyAt
                  contextKey
                  (adcBuckets (cegAnnotatedDeltaCache contextGraph))
                  (classIdKey baseClass)
              )
      generatorCanonicalizers = fmap canonicalizeAt generatorKeys
  pure
    ( \classId ->
        case fmap ($ classId) generatorCanonicalizers of
          [] ->
            canonicalizeBase classId
          firstClass : restClasses
            | all (== firstClass) restClasses ->
                firstClass
            | otherwise ->
                canonicalizeBase classId
    )
{-# INLINE contextualSupportCanonicalizer #-}

stageBaseUpdateWithSupportFootprintKnown ::
  Ord c =>
  PreparedContextSite owner c ->
  SupportBasis c ->
  IntSet ->
  EGraphMutationResult f a ClassId ->
  ContextRebaseBatch owner f a c ->
  Either (ContextDeltaError f c) (ClassId, ContextRebaseBatch owner f a c)
stageBaseUpdateWithSupportFootprintKnown site normalizedSupport supportClassKeys mutationResult batchValue = do
  (classId, batchWithTrace) <-
    applyBaseMutationToBatch mutationResult batchValue
  fmap
    ((,) classId)
    ( stageSupportClassKeysKnown
        site
        normalizedSupport
        supportClassKeys
        batchWithTrace
    )

enodeSupportFootprint ::
  Language f =>
  EGraph f a ->
  ClassId ->
  ENode f ->
  IntSet
enodeSupportFootprint graph classId enodeValue =
  let ENode canonicalNode =
        canonicalizeENodeByTheory
          (eGraphTheorySpec graph)
          (canonicalizeENodePure (canonicalizeClassId graph) enodeValue)
   in IntSet.insert
        (classIdKey (canonicalizeClassId graph classId))
        (foldMap (IntSet.singleton . classIdKey) canonicalNode)
{-# INLINE enodeSupportFootprint #-}

stageTermGlobally ::
  (Language f, Ord c) =>
  Fix f ->
  ContextRebaseBatch owner f a c ->
  Either (ContextDeltaError f c) (ClassId, ContextRebaseBatch owner f a c)
stageTermGlobally term batchValue =
  stageGlobalBaseUpdate
    (insertTermTracked term (cegBase (crbGraph batchValue)))
    batchValue

stageTermsGlobally ::
  (Language f, Ord c) =>
  [Fix f] ->
  ContextRebaseBatch owner f a c ->
  Either (ContextDeltaError f c) ([ClassId], ContextRebaseBatch owner f a c)
stageTermsGlobally terms batchValue =
  case terms of
    [] ->
      Right ([], batchValue)
    _ ->
      stageGlobalBaseUpdate
        (insertTermsTracked terms (cegBase (crbGraph batchValue)))
        batchValue
{-# INLINE stageTermsGlobally #-}

stageGlobalBaseUpdate ::
  Ord c =>
  Either UnionFindAllocationError (EGraphMutationResult f a result) ->
  ContextRebaseBatch owner f a c ->
  Either (ContextDeltaError f c) (result, ContextRebaseBatch owner f a c)
stageGlobalBaseUpdate allocation batchValue = do
  mutationResult <-
    first ContextClassIdAllocationFailed allocation
  (resultValue, batchWithTrace) <-
    applyBaseMutationToBatch mutationResult batchValue
  pure
    ( resultValue,
      batchWithTrace
        { crbScope = appendRebaseAll Set.empty (crbScope batchValue)
        }
    )
{-# INLINE stageGlobalBaseUpdate #-}

stageTermAtContext ::
  (Language f, Ord c) =>
  c ->
  Fix f ->
  ContextRebaseBatch owner f a c ->
  Either (ContextDeltaError f c) (ClassId, ContextRebaseBatch owner f a c)
stageTermAtContext contextValue term =
  stageTermWithSupport (principalSupport contextValue) term

commitContextRebaseBatch ::
  (Language f, Ord c) =>
  ContextRebaseBatch owner f a c ->
  Either (ContextDeltaError f c) (ContextRebaseReport owner f c, ContextEGraph owner f a c)
commitContextRebaseBatch batchValue = do
  dirtyContexts <-
    contextRebaseBatchDirtyContexts batchValue
  let contextGraph = crbGraph batchValue
  rebasedGraph <-
    rebaseContextGraphAtContextsWithTrace
      (crbTrace batchValue)
      dirtyContexts
      (cegBase contextGraph)
      contextGraph
  let
      report =
        ContextRebaseReport
          { crrScope = crbScope batchValue,
            crrTrace = contextMutationTraceWithDirtyContexts dirtyContexts (crbTrace batchValue),
            crrContextRevisionBefore = crbOriginContextRevision batchValue,
            crrContextRevisionAfter = cegContextRevision rebasedGraph
          }
  pure (report, rebasedGraph)

globalMerge ::
  (Language f, Ord c) =>
  ClassId ->
  ClassId ->
  ContextEGraph owner f a c ->
  Either (ContextDeltaError f c) (ContextEGraph owner f a c)
globalMerge leftClassId rightClassId contextGraph =
  fmap snd $
    stageGlobalMerge leftClassId rightClassId (beginContextRebaseBatch contextGraph)
      >>= commitContextRebaseBatch

stageGlobalMerge ::
  (Language f, Ord c) =>
  ClassId ->
  ClassId ->
  ContextRebaseBatch owner f a c ->
  Either (ContextDeltaError f c) (ContextRebaseBatch owner f a c)
stageGlobalMerge leftClassId rightClassId batchValue = do
  let contextGraph = crbGraph batchValue
      baseGraph = cegBase contextGraph
      canonicalLeft =
        canonicalizeClassId baseGraph leftClassId
      canonicalRight =
        canonicalizeClassId baseGraph rightClassId
      (mergeTrace, baseTrace, mergedBase) =
        if canonicalLeft == canonicalRight
          then
            let emptyTrace = emptyEGraphMutationTrace baseGraph
             in (emptyTrace, emptyTrace, baseGraph)
          else
            let EGraphMutationResult
                  { emrTrace = localMergeTrace,
                    emrGraph = dirtyBase
                  } =
                    equateClassesTracked canonicalLeft canonicalRight baseGraph
                EGraphMutationResult
                  { emrTrace = rebuildTrace,
                    emrGraph = rebuiltBase
                  } =
                    rebuildTracked dirtyBase
             in ( localMergeTrace,
                  appendEGraphMutationTrace localMergeTrace rebuildTrace,
                  rebuiltBase
                )
      site = cegSite contextGraph
      classSupportKeys =
        observedClassUnionKeys (emtObservedClassUnions mergeTrace)
      canonicalMergedKey =
        classIdKey (canonicalizeClassId mergedBase canonicalLeft)
      absorbedClassKeys =
        IntSet.delete canonicalMergedKey classSupportKeys
  leftSupport <- classSupportForChecked leftClassId contextGraph
  rightSupport <- classSupportForChecked rightClassId contextGraph
  affectedSupport <- unionClassSupport site leftSupport rightSupport
  let affectedSeeds =
        Set.fromList
          ( supportGenerators leftSupport
              <> supportGenerators rightSupport
              <> supportGenerators affectedSupport
          )
  (supportIndex, supportDelta) <-
    first ContextSupportSiteFailed $
      classSupportIndexMergeInto
        site
        affectedSupport
        canonicalMergedKey
        absorbedClassKeys
        (cegClassSupport contextGraph)
  affectedScope <-
    appendRebaseSupportWithSeeds
      site
      affectedSupport
      affectedSeeds
      (crbScope batchValue)
  let traceWithSupport =
        appendContextMutationTrace
          (contextMutationTraceFromBase baseTrace)
          (contextMutationTraceFromSupportDelta mergedBase supportDelta)
      graphWithMergedBase =
        replaceClassSupportAfterMerge supportDelta supportIndex contextGraph
          { cegBase = mergedBase
          }
  Right
    ( appendBatchTrace
        traceWithSupport
        batchValue
          { crbGraph = graphWithMergedBase,
            crbScope = affectedScope
          }
    )

rebaseContextGraphAtContexts ::
  (Language f, Ord c) =>
  Set.Set c ->
  EGraph f a ->
  ContextEGraph owner f a c ->
  Either (ContextDeltaError f c) (ContextEGraph owner f a c)
rebaseContextGraphAtContexts dirtyContexts baseGraph contextGraph =
  rebaseContextGraphAtContextsWithTrace
    (emptyContextMutationTrace baseGraph)
    dirtyContexts
    baseGraph
    contextGraph
{-# INLINE rebaseContextGraphAtContexts #-}

rebaseContextGraphAtContextsWithTrace ::
  (Language f, Ord c) =>
  ContextMutationTrace owner c f ->
  Set.Set c ->
  EGraph f a ->
  ContextEGraph owner f a c ->
  Either (ContextDeltaError f c) (ContextEGraph owner f a c)
rebaseContextGraphAtContextsWithTrace traceValue dirtyContexts baseGraph contextGraph = do
  repairedFibers <-
    first ContextLocalUnionCanonicalizationFailed
      (traverse (repairContextFiber baseGraph) (cegContextFibers contextGraph))
  let graphWithBase =
        contextGraph
          { cegBase = baseGraph,
            cegContextFibers = repairedFibers
          }
  bucketAdvance <-
    first ContextRegionalClosureFailed
      (advanceAnnotatedDeltaBuckets traceValue graphWithBase)
  let
      advancedBucketCache =
        adcaCache bucketAdvance
      graphWithBuckets =
        graphWithBase {cegAnnotatedDeltaCache = advancedBucketCache}
      activeContexts = Map.keysSet (cegContextAnalysisDeltas graphWithBuckets)
      changedActiveContexts = Set.intersection dirtyContexts activeContexts
      deriveAnalysisAt contextValue = do
        contextKey <-
          first
            ContextSupportSiteFailed
            (contextObjectKeyFor (cegSite graphWithBuckets) contextValue)
        pure (deriveContextAnalysisDeltaAtKey contextKey graphWithBuckets)
  repairedAnalysisDeltas <-
    Map.traverseWithKey
      ( \contextValue existingDelta ->
          case adcaAdvanceMode bucketAdvance of
            AnnotatedDeltaReused ->
              pure existingDelta
            AnnotatedDeltaRecompiledForBaseRevision _ _ ->
              deriveAnalysisAt contextValue
            AnnotatedDeltaAdvancedRegionally -> do
              contextKey <-
                first
                  ContextSupportSiteFailed
                  (contextObjectKeyFor (cegSite graphWithBuckets) contextValue)
              if IntMap.member (contextObjectKeyValue contextKey) (adcaFrontierByContextKey bucketAdvance)
                then pure (deriveContextAnalysisDeltaAtKey contextKey graphWithBuckets)
                else pure existingDelta
      )
      (cegContextAnalysisDeltas graphWithBuckets)
  pure
    graphWithBuckets
        { cegContextAnalysisDeltas = repairedAnalysisDeltas,
          cegRuntimeState =
            (cegRuntimeState graphWithBuckets)
              { crsDirtyContexts = Set.empty,
                crsLastRepair = Just (settledPropagationReport changedActiveContexts)
              }
        }
{-# INLINE rebaseContextGraphAtContextsWithTrace #-}

advanceAnnotatedDeltaBuckets ::
  (Language f, Ord c) =>
  ContextMutationTrace owner c f ->
  ContextEGraph owner f a c ->
  Either (RegionalClosureObstruction c) (AnnotatedDeltaCacheAdvance owner f)
advanceAnnotatedDeltaBuckets traceValue contextGraph
  | adcBaseRevision cache /= currentBaseRevision =
      recompile
        (AnnotatedDeltaRecompiledForBaseRevision (adcBaseRevision cache) currentBaseRevision)
  | not (Map.null localUnionsByContext) = do
      advancedCache <-
        advanceAnnotatedDeltaCacheAtUnions
          [ (contextValue, observedClassUnionPairs localUnions)
            | (contextValue, localUnions) <- Map.toAscList localUnionsByContext
          ]
          contextGraph
      let frontier = adcDirtyFrontierByContextKey advancedCache
      pure
        AnnotatedDeltaCacheAdvance
          { adcaCache = advancedCache,
            adcaFrontierByContextKey = frontier,
            adcaAdvanceMode = AnnotatedDeltaAdvancedRegionally
          }
  | contextClosureChanged =
      Left
        ( RegionalClosureContextRevisionMismatch
            (adcContextRevision cache)
            (cegContextRevision contextGraph)
        )
  | otherwise =
      let frontier =
            if contextMutationTraceNull traceValue
              then IntMap.empty
              else adcDirtyFrontierByContextKey cache
       in pure
            AnnotatedDeltaCacheAdvance
              { adcaCache =
                  cache
                    { adcContextRevision = cegContextRevision contextGraph,
                      adcDirtyFrontierByContextKey = frontier
                    },
                adcaFrontierByContextKey = frontier,
                adcaAdvanceMode = AnnotatedDeltaReused
              }
  where
    cache = cegAnnotatedDeltaCache contextGraph
    currentBaseRevision = eGraphRevision (cegBase contextGraph)
    localUnionsByContext = cmtObservedLocalUnionsByContext traceValue
    contextClosureChanged =
      adcContextRevision cache /= cegContextRevision contextGraph

    recompile advanceMode = do
      activeContextKeys <-
        traverse
          ( \contextValue ->
              first
                (const (RegionalClosureActiveContextMissing contextValue))
                (contextObjectKeyFor (cegSite contextGraph) contextValue)
          )
          (Map.keys (cegContextAnalysisDeltas contextGraph))
      newBuckets <- deriveAnnotatedDeltaBuckets contextGraph
      let stepFrontier =
            bucketFrontierBetween
              activeContextKeys
              (adcBuckets cache)
              newBuckets
          frontier =
            IntMap.unionWith
              appendAnnotatedDeltaFrontier
              (adcDirtyFrontierByContextKey cache)
              stepFrontier
          nextCache =
            AnnotatedDeltaCache
              { adcBaseRevision = currentBaseRevision,
                adcContextRevision = cegContextRevision contextGraph,
                adcBuckets = newBuckets,
                adcDirtyFrontierByContextKey = frontier
              }
      pure
        AnnotatedDeltaCacheAdvance
          { adcaCache = nextCache,
            adcaFrontierByContextKey = frontier,
            adcaAdvanceMode = advanceMode
          }

rebaseAffectedContexts ::
  Ord c =>
  RebaseScope owner c ->
  ContextEGraph owner f a c ->
  Either (ContextDeltaError f c) (Set.Set c)
rebaseAffectedContexts scopeValue contextGraph =
  let cachedContexts = Set.fromList (contextCachedObjectsForExecution contextGraph)
      explicitContexts = rsExplicitContexts scopeValue
   in case rsSupportDemand scopeValue of
        RebaseNoSupport ->
          checkedRebaseContexts contextGraph explicitContexts
        RebaseAllSupport supportSeeds ->
          checkedRebaseContexts contextGraph (Set.unions [explicitContexts, supportSeeds, cachedContexts])
        RebaseLimitedSupport supportCarrier supportSeeds -> do
          explicitChecked <- checkedRebaseContexts contextGraph explicitContexts
          supportContexts <- activateContextsForSupport supportSeeds supportCarrier contextGraph
          pure (Set.union explicitChecked supportContexts)

activateContextsForSupport ::
  Ord c =>
  Set.Set c ->
  SupportCarrier owner c ->
  ContextEGraph owner f a c ->
  Either (ContextDeltaError f c) (Set.Set c)
activateContextsForSupport supportSeeds supportCarrier contextGraph =
  let site =
        cegSite contextGraph
      candidateContexts =
        Set.union
          supportSeeds
          (Set.fromList (contextCachedObjectsForExecution contextGraph))
   in fmap (Set.fromList . concat) $
        first ContextSupportSiteFailed $
          traverse
            ( \contextValue -> do
                contextKey <- contextObjectKeyFor site contextValue
                pure [contextValue | supportCarrierContainsKey site supportCarrier contextKey]
            )
            (Set.toAscList candidateContexts)

checkedRebaseContexts ::
  Ord c =>
  ContextEGraph owner f a c ->
  Set.Set c ->
  Either (ContextDeltaError f c) (Set.Set c)
checkedRebaseContexts contextGraph contexts =
  fmap (const contexts) $
    first ContextSupportSiteFailed $
      traverse
        (contextObjectKeyFor (cegSite contextGraph))
        (Set.toAscList contexts)

rebaseContextGraphWithSupport ::
  (Language f, Ord c) =>
  SupportBasis c ->
  EGraph f a ->
  ContextEGraph owner f a c ->
  Either (ContextDeltaError f c) (ContextEGraph owner f a c)
rebaseContextGraphWithSupport supportValue baseGraph contextGraph =
  do
    supportDemand <- rebaseSupportDemandFromSupport (cegSite contextGraph) supportValue
    rebaseContextGraphWithScope
      ( emptyRebaseScope
          { rsSupportDemand = supportDemand
          }
      )
      baseGraph
      contextGraph

rebaseContextGraphWithScope ::
  (Language f, Ord c) =>
  RebaseScope owner c ->
  EGraph f a ->
  ContextEGraph owner f a c ->
  Either (ContextDeltaError f c) (ContextEGraph owner f a c)
rebaseContextGraphWithScope scopeValue baseGraph contextGraph =
  do
    dirtyContexts <- rebaseAffectedContexts scopeValue contextGraph
    if Set.null dirtyContexts
      then Right contextGraph
      else rebaseContextGraphAtContexts dirtyContexts baseGraph contextGraph

isGlobalSupport :: Eq c => PreparedContextSite owner c -> SupportBasis c -> Bool
isGlobalSupport site supportValue =
  defaultPreparedSupport site == supportValue
