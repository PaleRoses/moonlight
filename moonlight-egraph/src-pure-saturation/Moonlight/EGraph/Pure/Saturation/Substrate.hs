{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.EGraph.Pure.Saturation.Substrate
  ( EGraphU,
    EGraphRuleCompileError (..),
    EGraphSaturationChangeSummary (..),
    EGraphSaturationObstruction (..),
    RawRewriteMatch (..),
    eGraphSaturationChangeTrace,
    eGraphMatchingToSaturationObstruction,
  )
where

import Data.Bifunctor (bimap, first)
import Data.Foldable (foldlM, toList)
import Data.Functor (void)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Map.Lazy qualified as LazyMap
import Data.Set qualified as Set
import Moonlight.Algebra (JoinSemilattice)
import Moonlight.Core
  ( HasConstructorTag,
    Language,
    RewriteRuleId (..),
    SupportIndexedRule (..),
  )
import Moonlight.Delta.Scope qualified as Delta
import Moonlight.Core (scanMap)
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    ContextDeltaError,
    ContextMutationTrace (..),
    appendContextMutationTrace,
    contextMutationTraceEffect,
    emptyContextMutationTrace,
    materializeAmbientPayloadFor,
    emptyContextEGraphFromSite,
  )
import Moonlight.EGraph.Pure.Context
  ( ContextFiber (..),
    cegBase,
    cegClassSupportIndex,
    cegContextFibers,
    cegContextRevision,
    cegLattice,
    cegSite,
    contextPreparedObjects,
  )
import Moonlight.EGraph.Pure.Context.Proof
  ( ProofGraph (..),
    emptyProofEGraph,
  )
import Moonlight.EGraph.Pure.Extraction
  ( mutationTraceDirtyKeys,
  )
import Moonlight.EGraph.Pure.Context.AnnotatedDelta
  ( AnnotatedDeltaBuckets,
    AnnotatedRow (..),
    annotatedRepresentativeKeyAt,
    annotatedVariantRowsForTag,
    contextAnnotatedDeltaBuckets,
    contextAnnotatedDeltaDirtyFrontier,
  )
import Moonlight.EGraph.Pure.Context.AnnotatedView
  ( AnnotatedContextView,
    annotatedContextViewAtKey,
    annotatedViewCanonicalize,
    annotatedViewLookupLeastENode,
    annotatedViewProjectChildAt,
  )
import Moonlight.EGraph.Pure.Guard.Evaluation
  ( GuardGraphView (..),
    graphGuardView,
    resolveGuardTermWith,
  )
import Moonlight.EGraph.Pure.Guard.Region
  ( ContextFactStoreLookup (..),
    compileGuardRegion,
    guardRegion,
    guardRegionEvidenceAtKey,
  )
import Moonlight.EGraph.Pure.Relational
  ( EGraphPreparedMatchState,
    PatternAtomizeObstruction,
    RegionalAssignmentObstruction,
    compiledPatternQueryFingerprint,
    compiledPatternQueryKey,
    emptyEGraphPreparedMatchState,
    markEGraphPreparedMatchStateAnnotatedDirty,
    preparedPlanTemplate,
    wcojPreparedRegionalDeltaMatchCompiledWithRoots,
  )
import Moonlight.EGraph.Pure.Structural.Store
  ( StructuralLookup (..),
    structuralLookupTupleAll,
  )
import Moonlight.EGraph.Pure.Rewrite.Guard
  ( acceptRewriteCondition,
  )
import Moonlight.EGraph.Pure.Rewrite.Instantiate
  ( resolveExistingPatternClass,
    resolveExistingPatternClassWith,
  )
import Moonlight.EGraph.Pure.Rewrite.Env
  ( rewriteRuntimeGuardCapabilityResolver,
  )
import Moonlight.EGraph.Pure.Saturation.Apply
  ( EGraphApplicationResult (..),
    EGraphRewriteApplicationError (..),
    egraphApplyMatchesBaseReported,
    egraphApplyMatchesContextualReported,
    engineApplyMatchesWithProofReported,
  )
import Moonlight.Sheaf.Section.Context.Payload
  ( payloadMapToRepresentativeMap,
  )
import Moonlight.Sheaf.Section.Congruence.Equivalence.Relation
  ( equivalencePairs,
  )
import Moonlight.Sheaf.Context.Site
  ( ContextObjectKey,
    contextObjectKeyValue,
    PreparedContextSite,
    PreparedContextSupportError,
    UnitContextSiteOwner,
    classSupportExplicitCarrierForKey,
    contextObjectKeyFor,
    defaultPreparedSupport,
    preparedContextAtKey,
    preparedSupportFromContexts,
    preparedRegionTable,
    supportCarrierRegion,
    supportCarrierReachableObjects,
    unitPreparedContextSite,
    unionPreparedSupport,
  )
import Moonlight.EGraph.Pure.Saturation.Matching
  ( AnnotatedContextSource (..),
    EGraphMatchingObstruction (..),
    MatchingAdvanceCtx (..),
    MatchingAlgebra,
    MatchingDelta,
    MatchingDeltaPayload (..),
    MatchingProofContext,
    MatchingRequest,
    MatchingWorld,
    MatchingStrategy (..),
    eGraphRelationalMatchObstruction,
    matchingDeltaFromContextMutationTraceWithAnnotatedFrontier,
    matchingDeltaFromRebuildWithObstruction,
    matchingFrontierFromDelta,
    mkMatchingProofContext,
    wcojMatchingAlgebra,
  )
import Moonlight.EGraph.Pure.Saturation.Rebuild
  ( RoundRebuildReport (..),
    runRoundRebuildReport,
  )
import Moonlight.EGraph.Pure.Rebuild
  ( EGraphRebuildDelta (..),
  )
import Moonlight.EGraph.Pure.Saturation.Substrate.Compile.Query
  ( preparedRuleRequest,
    registerCompiledPatternQueries,
  )
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingContextEGraph,
    SaturatingProofEGraph,
    cssQueryRegistry,
    emptySaturatingContextEGraph,
    mapSaturatingContextGraph,
    sceContextGraph,
    sceSaturationState,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    EGraph,
    ENode (..),
    canonicalizeClassId,
    classIdKey,
    eGraphClassCount,
    eGraphRebuildDeltaTouchedKeys,
    materializeEGraphClasses,
    materializeEGraphHashCons,
    eGraphNodeCount,
    eGraphPendingClassUnions,
    eGraphRevision,
    eGraphRevisionValue,
    eGraphStore,
  )
import Moonlight.Flow.Execution.Direct qualified as RelRuntime
import Moonlight.Rewrite.ProofContext
  ( SupportMatchWitness (..),
    SupportedRewriteMatch (..),
  )
import Moonlight.Rewrite.Runtime
  ( ExecutableRewriteMatch (..),
  )
import Moonlight.Core
  ( Substitution,
    mapSubstitutionClasses,
  )
import Moonlight.Core.EGraph.Program (eGraphProgramEffectCount)
import Moonlight.Rewrite.Algebra
  ( CompiledPatternQuery,
  )
import Moonlight.Rewrite.Runtime
  ( RulePlan,
    RulePlanError,
    rpApplicationCondition,
    rpId,
    rpQuery,
    RewriteApplicationError,
    rulePlanCondition,
    rulePlanPostSubst,
    rulePlanRhsPattern
  )
import Moonlight.Rewrite.System
  ( RawRewriteRule (..)
  )
import Moonlight.Rewrite.System
  ( RewriteError,
    checkRawRewriteSystem,
  )
import Moonlight.Rewrite.System
  ( planRuleSet,
    rulePlans,
  )
import Moonlight.Rewrite.Runtime
  ( RewriteRuntimeCapabilities,
    emptyRewriteRuntimeCapabilities,
  )
import Moonlight.Rewrite.ProofContext
  ( ProofAnnotationBuilder,
  )
import Moonlight.Rewrite.System
  ( CompiledGuard,
    GuardCapabilityResolver,
    GuardEvidence,
    geFactWitnesses,
    RewriteCondition,
    emptyGuardCapabilityResolver,
  )
import Moonlight.Rewrite.System
  ( CompiledFactRule,
    cfrCompiledQuery,
    cfrId,
    FactDerivation,
    FactDerivationIndex,
    FactRule,
    FactRuleCompileError,
    FactRuleId (..),
    RawFactRule (..),
    canonicalizeFactDerivationIndex,
    emptyFactDerivationIndex,
    lookupFactDerivations,
    singletonFactDerivationIndex,
  )
import Moonlight.Rewrite.System qualified as LogicRule
import Moonlight.Rewrite.System
  ( FactClosureLimit,
    FactClosureRun (..),
    FactClosureRunError (..),
    FactClosureStats,
    SemiNaiveInput (..),
    SemiNaiveRound,
    defaultSemiNaiveConfig,
    deriveSeededFactClosureWithStateAndConfig,
    mkSemiNaiveMatcher,
    sncDerivations,
    sncFacts,
    sncRounds,
  )
import Moonlight.Rewrite.System
  ( FactStore,
  )
import Moonlight.Rewrite.System qualified as LogicStore
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Moonlight.Control.Gate
  ( GuideEvidence,
  )
import Moonlight.Saturation.Context.Match.State.Registry qualified as SaturationMatch
import Moonlight.Saturation.Matching qualified as GenericMatching
import Moonlight.Saturation.Obstruction.Cohomological.LivePruning
  ( ObstructionInvalidation (..),
    obstructionInvalidationFromKeys,
  )
import Moonlight.Saturation.Substrate
import Moonlight.FiniteLattice
  ( ContextLatticeLookupError
  )
import Moonlight.FiniteLattice
  ( principalSupport,
    supportGenerators,
  )
import Moonlight.Sheaf.Context.Region
  ( ContextRegion,
    RegionTable,
    regionEmpty,
    regionAtKey,
    regionGeneratorKeys,
    regionJoin,
    regionMeet,
    regionMemberKey,
  )

type EGraphU :: Type -> Type -> (Type -> Type) -> Type -> Type -> Type
data EGraphU (owner :: Type) (capability :: Type) (f :: Type -> Type) (a :: Type) (c :: Type)

type EGraphSaturationObstruction :: Type -> (Type -> Type) -> Type -> Type
data EGraphSaturationObstruction capability f c
  = EGraphSaturationPatternAtomizeObstruction !PatternAtomizeObstruction
  | EGraphSaturationContextLatticeObstruction !(ContextLatticeLookupError c)
  | EGraphSaturationPreparedSupportObstruction !(PreparedContextSupportError c)
  | EGraphSaturationRuntimeQueryObstruction !RelRuntime.RuntimeQueryPlanObstruction
  | EGraphSaturationRegionObjectKeyMissing !Int
  | EGraphSaturationRegionWitnessMissing !c
  | EGraphSaturationDirtySnapshot
  | EGraphSaturationRegionalAssignmentObstruction !RegionalAssignmentObstruction
  | EGraphSaturationHierarchicalPruningWithoutSeedFrontier
  | EGraphSaturationFactClosureLimitExceeded !FactClosureLimit !FactClosureStats
  | EGraphSaturationRebuildObstruction !(ContextDeltaError f c)
  deriving stock (Eq, Show)

eGraphMatchingToSaturationObstruction ::
  EGraphMatchingObstruction ->
  EGraphSaturationObstruction capability f c
eGraphMatchingToSaturationObstruction obstruction =
  case obstruction of
    EGraphMatchingPatternAtomizeObstruction atomizeObstruction ->
      EGraphSaturationPatternAtomizeObstruction atomizeObstruction
    EGraphMatchingRuntimeQueryObstruction runtimeObstruction ->
      EGraphSaturationRuntimeQueryObstruction runtimeObstruction
    EGraphMatchingDirtySnapshot ->
      EGraphSaturationDirtySnapshot
    EGraphMatchingRegionalAssignmentObstruction regionalObstruction ->
      EGraphSaturationRegionalAssignmentObstruction regionalObstruction
    EGraphMatchingHierarchicalPruningWithoutSeedFrontier ->
      EGraphSaturationHierarchicalPruningWithoutSeedFrontier

factClosureRunErrorToSaturationObstruction ::
  FactClosureRunError EGraphMatchingObstruction ->
  EGraphSaturationObstruction capability f c
factClosureRunErrorToSaturationObstruction factClosureRunError =
  case factClosureRunError of
    FactClosureMatcherError matchingObstruction ->
      eGraphMatchingToSaturationObstruction matchingObstruction
    FactClosureLimitExceeded limit stats ->
      EGraphSaturationFactClosureLimitExceeded limit stats

type EGraphSaturationChangeSummary :: Type -> Type -> (Type -> Type) -> Type
data EGraphSaturationChangeSummary owner c f = EGraphSaturationChangeSummary
  { egscApplicationTraces :: ![ContextMutationTrace owner c f],
    egscRebuildDeltas :: ![EGraphRebuildDelta],
    egscProofRestrictionRegistryConstructions :: !Int,
    egscProofExtractionTableConstructions :: !Int
  }

instance Semigroup (EGraphSaturationChangeSummary owner c f) where
  leftSummary <> rightSummary =
    EGraphSaturationChangeSummary
      { egscApplicationTraces = egscApplicationTraces leftSummary <> egscApplicationTraces rightSummary,
        egscRebuildDeltas = egscRebuildDeltas leftSummary <> egscRebuildDeltas rightSummary,
        egscProofRestrictionRegistryConstructions =
          egscProofRestrictionRegistryConstructions leftSummary
            + egscProofRestrictionRegistryConstructions rightSummary,
        egscProofExtractionTableConstructions =
          egscProofExtractionTableConstructions leftSummary
            + egscProofExtractionTableConstructions rightSummary
      }

instance Monoid (EGraphSaturationChangeSummary owner c f) where
  mempty =
    EGraphSaturationChangeSummary
      { egscApplicationTraces = [],
        egscRebuildDeltas = [],
        egscProofRestrictionRegistryConstructions = 0,
        egscProofExtractionTableConstructions = 0
      }

eGraphSaturationChangeTrace ::
  Ord c =>
  ContextEGraph owner f a c ->
  ContextEGraph owner f a c ->
  EGraphSaturationChangeSummary owner c f ->
  ContextMutationTrace owner c f
eGraphSaturationChangeTrace initialGraph finalGraph summary =
  appendContextMutationTrace applicationTrace rebuildTrace
  where
    applicationTrace =
      foldl' appendContextMutationTrace (emptyContextMutationTrace (cegBase initialGraph)) (egscApplicationTraces summary)

    rebuildDirtyKeys =
      foldMap eGraphRebuildDeltaTouchedKeys (egscRebuildDeltas summary)

    rebuildTrace =
      (emptyContextMutationTrace (cegBase finalGraph))
        { cmtContextTouchedKeys = rebuildDirtyKeys,
          cmtDirtyContexts = contextsTouchedByClassKeys finalGraph rebuildDirtyKeys
        }

contextsTouchedByClassKeys ::
  Ord c =>
  ContextEGraph owner f a c ->
  IntSet.IntSet ->
  Set.Set c
contextsTouchedByClassKeys contextGraph dirtyKeys
  | IntSet.null dirtyKeys =
      Set.empty
  | otherwise =
      IntSet.foldr (Set.union . contextsForKey) Set.empty dirtyKeys
  where
    site =
      cegSite contextGraph

    allContexts =
      Set.fromList (contextPreparedObjects contextGraph)

    supportIndex =
      cegClassSupportIndex contextGraph

    contextsForKey classKey =
      case explicitCarrierForResolvedKey classKey of
        Nothing ->
          allContexts
        Just carrier ->
          either (const allContexts) id (supportCarrierReachableObjects site allContexts carrier)

    explicitCarrierForResolvedKey classKey =
      case classSupportExplicitCarrierForKey supportIndex classKey of
        Just carrier ->
          Just carrier
        Nothing ->
          classSupportExplicitCarrierForKey
            supportIndex
            (classIdKey (canonicalizeClassId (cegBase contextGraph) (ClassId classKey)))

roundRebuildMatchingDelta ::
  RoundRebuildReport owner capability f a c ->
  MatchingDelta
roundRebuildMatchingDelta rebuildReport =
  let rebuildDelta = rrrRebuildDelta rebuildReport
   in matchingDeltaFromRebuildWithObstruction
        rebuildDelta
        ( Just
            ( obstructionInvalidationFromKeys
                ClassId
                (erdImpactedClassKeys rebuildDelta)
                (erdTopologyClassKeys rebuildDelta)
                (erdImpactedClassKeys rebuildDelta)
            )
        )
{-# INLINE roundRebuildMatchingDelta #-}

type instance SatGraph (EGraphU owner capability f a c) = SaturatingContextEGraph owner capability f a c
type instance SatBaseGraph (EGraphU owner capability f a c) = EGraph f a
type instance SatClassId (EGraphU owner capability f a c) = ClassId
type instance SatContextOwner (EGraphU owner capability f a c) = owner
type instance SatContext (EGraphU owner capability f a c) = c
type instance SatObstruction (EGraphU owner capability f a c) = EGraphSaturationObstruction capability f c
type instance SatCapabilityResolver (EGraphU owner capability f a c) = GuardCapabilityResolver capability
type instance SatFactStore (EGraphU owner capability f a c) = FactStore
type instance SatFactIndex (EGraphU owner capability f a c) = FactDerivationIndex
type instance SatFactSource (EGraphU owner capability f a c) = FactRule capability f
type instance SatFactCompileError (EGraphU owner capability f a c) = FactRuleCompileError
type instance SatFactRule (EGraphU owner capability f a c) = CompiledFactRule capability f
type instance SatFactRound (EGraphU owner capability f a c) = SemiNaiveRound
type instance SatQuery (EGraphU owner capability f a c) = CompiledPatternQuery (CompiledGuard capability f) f
type instance SatMatchSnapshot (EGraphU owner capability f a c) = AnnotatedContextSource owner f
type instance SatMatchSection (EGraphU owner capability f a c) = ()
type instance SatMatchingDelta (EGraphU owner capability f a c) = MatchingDelta
type instance SatChangeSummary (EGraphU owner capability f a c) = EGraphSaturationChangeSummary owner c f
type instance SatRuleSource (EGraphU owner capability f a c) = RawRewriteRule (RewriteCondition capability f) f
type instance SatRule (EGraphU owner capability f a c) = RulePlan (CompiledGuard capability f) f
type instance SatRuleKey (EGraphU owner capability f a c) = RewriteRuleId
type instance SatRawMatch (EGraphU owner capability f a c) = RawRewriteMatch capability f
type instance SatRawMatchRejection (EGraphU owner capability f a c) = RewriteApplicationError
type instance SatRequestMatch (EGraphU owner capability f a c) = (ClassId, Substitution)
type instance SatMatchWorld (EGraphU owner capability f a c) = MatchingWorld owner c capability f a
type instance SatMatchingRequest (EGraphU owner capability f a c) = MatchingRequest owner c capability f a
type instance SatMatch (EGraphU owner capability f a c) = ExecutableRewriteMatch (CompiledGuard capability f) GuardEvidence (GuideEvidence ClassId) f
type instance SatSupportedMatch (EGraphU owner capability f a c) = SupportedRewriteMatch c capability f
type instance SatSupportWitness (EGraphU owner capability f a c) = SupportMatchWitness f
type instance SatRebuild (EGraphU owner capability f a c) = RoundRebuildReport owner capability f a c
type instance SatMatchState (EGraphU owner capability f a c) = EGraphMatchState owner c capability f a
type instance SatMatchStrategy (EGraphU owner capability f a c) = MatchingStrategy owner c capability f a
type instance SatRewriteContext (EGraphU owner capability f a c) = RewriteRuntimeCapabilities (GuardCapabilityResolver capability) f
type instance SatRuleCompileError (EGraphU owner capability f a c) = EGraphRuleCompileError capability f
type instance SatApplicationError (EGraphU owner capability f a c) = EGraphRewriteApplicationError f c
type instance SatApplicationResult (EGraphU owner capability f a c) = EGraphApplicationResult owner c capability f
type instance SatProofGraph (EGraphU owner capability f a c) p = SaturatingProofEGraph owner capability f a c p
type instance SatProofBuilder (EGraphU owner capability f a c) p = ProofAnnotationBuilder c p

type RawRewriteMatch :: Type -> (Type -> Type) -> Type
data RawRewriteMatch capability f = RawRewriteMatch
  { rrmRule :: !(RulePlan (CompiledGuard capability f) f),
    rrmRootClass :: !ClassId,
    rrmSubstitution :: !Substitution
  }

type EGraphRuleCompileError :: Type -> (Type -> Type) -> Type
data EGraphRuleCompileError capability f
  = EGraphRuleCheckFailed !(RewriteError capability f)
  | EGraphRulePlanningFailed !RulePlanError

deriving stock instance Eq (RewriteError capability f) => Eq (EGraphRuleCompileError capability f)
deriving stock instance Show (RewriteError capability f) => Show (EGraphRuleCompileError capability f)

rawRewriteMatchKey :: RawRewriteMatch capability f -> (RewriteRuleId, ClassId, Substitution)
rawRewriteMatchKey rawMatch =
  (rpId (rrmRule rawMatch), rrmRootClass rawMatch, rrmSubstitution rawMatch)
{-# INLINE rawRewriteMatchKey #-}

type EGraphProductivityKey :: Type -> Type
data EGraphProductivityKey c = EGraphProductivityKey
  { epkRuleId :: !RewriteRuleId,
    epkRoot :: !ClassId,
    epkSubstitution :: !Substitution,
    epkSupport :: !(SupportBasis c)
  }
  deriving stock (Eq, Ord, Show)

type EGraphMatchEmission :: Type
data EGraphMatchEmission
  = RegionNativeEmission
  | PerContextEmission
  deriving stock (Eq, Show)

type EGraphMatchState :: Type -> Type -> Type -> (Type -> Type) -> Type -> Type
data EGraphMatchState owner c capability f a where
  EGraphMatchState ::
    !EGraphMatchEmission ->
    !(Set.Set (EGraphProductivityKey c)) ->
    !(ClassId -> ClassId) ->
    !(EGraphPreparedMatchState owner capability f) ->
    !(MatchingAlgebra state owner c capability f a) ->
    !state ->
    EGraphMatchState owner c capability f a

canonicalizeProductivityKey ::
  (ClassId -> ClassId) ->
  EGraphProductivityKey c ->
  EGraphProductivityKey c
canonicalizeProductivityKey canonicalize key =
  key
    { epkRoot =
        canonicalize (epkRoot key),
      epkSubstitution =
        mapSubstitutionClasses canonicalize (epkSubstitution key)
    }

mkEGraphMatchState ::
  EGraphMatchEmission ->
  MatchingAlgebra state owner c capability f a ->
  EGraphMatchState owner c capability f a
mkEGraphMatchState emission matchingAlgebra =
  EGraphMatchState
    emission
    Set.empty
    id
    emptyEGraphPreparedMatchState
    matchingAlgebra
    (GenericMatching.maInitialState matchingAlgebra)

runEGraphMatchingRequest ::
  MatchingDelta ->
  MatchingWorld owner c capability f a ->
  MatchingRequest owner c capability f a ->
  EGraphMatchState owner c capability f a ->
  (EGraphMatchState owner c capability f a, Either EGraphMatchingObstruction [(ClassId, Substitution)])
runEGraphMatchingRequest
  matchingDelta
  matchingWorld
  request
  (EGraphMatchState emission saturatedKeys canonicalize regionalState matchingAlgebra matchState) =
    let (preparedMatchState, frontier) =
          GenericMatching.prepareSingleQuery matchingAlgebra matchState matchingDelta matchingWorld request
        (nextState, matchesResult) =
          GenericMatching.runSingleQuery matchingAlgebra preparedMatchState matchingWorld frontier request
     in ( EGraphMatchState
            emission
            saturatedKeys
            canonicalize
            regionalState
            matchingAlgebra
            nextState,
          matchesResult
        )

runEGraphMatchingRequests ::
  MatchingDelta ->
  MatchingWorld owner c capability f a ->
  [MatchingRequest owner c capability f a] ->
  EGraphMatchState owner c capability f a ->
  (EGraphMatchState owner c capability f a, Either EGraphMatchingObstruction [[(ClassId, Substitution)]])
runEGraphMatchingRequests
  matchingDelta
  matchingWorld
  requests
  (EGraphMatchState emission saturatedKeys canonicalize regionalState matchingAlgebra matchState) =
    let (preparedState, frontierRequests) =
          scanMap
            ( \currentState request ->
                let (preparedStateForRequest, frontier) =
                      GenericMatching.prepareSingleQuery
                        matchingAlgebra
                        currentState
                        matchingDelta
                        matchingWorld
                        request
                 in (preparedStateForRequest, (frontier, request))
            )
            matchState
            requests
        (nextState, matchesByRequest) =
          GenericMatching.runPreparedQueries
            matchingAlgebra
            preparedState
            matchingWorld
            frontierRequests
     in ( EGraphMatchState
            emission
            saturatedKeys
            canonicalize
            regionalState
            matchingAlgebra
            nextState,
          matchesByRequest
        )

advanceRegionalPreparedMatchState ::
  Language f =>
  MatchingDelta ->
  EGraphPreparedMatchState owner capability f ->
  EGraphPreparedMatchState owner capability f
advanceRegionalPreparedMatchState matchingDelta state =
  Delta.foldScope
    state
    (\dirtyKeys -> markEGraphPreparedMatchStateAnnotatedDirty dirtyKeys state)
    (preparedPlanTemplate state)
    (matchingFrontierFromDelta matchingDelta)
{-# INLINE advanceRegionalPreparedMatchState #-}

consumedFactDerivations :: FactDerivationIndex -> ExecutableRewriteMatch (CompiledGuard capability f) GuardEvidence (GuideEvidence ClassId) f -> Set.Set FactDerivation
consumedFactDerivations factDerivations rewriteMatch =
  maybe
    Set.empty
    (foldMap (`lookupFactDerivations` factDerivations) . geFactWitnesses)
    (ermGuardEvidence rewriteMatch)

materializeRawRewriteMatch ::
  Language f =>
  GuardCapabilityResolver capability ->
  FactStore ->
  RulePlan (CompiledGuard capability f) f ->
  ClassId ->
  Substitution ->
  EGraph f a ->
  Either RewriteApplicationError (ExecutableRewriteMatch (CompiledGuard capability f) GuardEvidence (GuideEvidence ClassId) f)
materializeRawRewriteMatch capabilityResolver factStore preparedRewrite rootClassId substitution graph =
  let rewriteMatch =
        ExecutableRewriteMatch preparedRewrite rootClassId Nothing Nothing substitution
   in fmap
        (\guardEvidence -> rewriteMatch {ermGuardEvidence = guardEvidence})
        (acceptRewriteCondition factStore capabilityResolver rewriteMatch graph)

materializeRawRewriteMatchRegion ::
  Language f =>
  RegionTable owner ->
  AnnotatedDeltaBuckets owner f ->
  EGraph f a ->
  GuardCapabilityResolver capability ->
  FactStore ->
  ContextObjectKey owner ->
  RawRewriteMatch capability f ->
  Maybe (ExecutableRewriteMatch (CompiledGuard capability f) GuardEvidence (GuideEvidence ClassId) f)
materializeRawRewriteMatchRegion regionTable buckets graph capabilityResolver factStore contextKey rawMatch =
  let rewriteMatch =
        ExecutableRewriteMatch (rrmRule rawMatch) (rrmRootClass rawMatch) Nothing Nothing (rrmSubstitution rawMatch)
   in case rulePlanCondition (rrmRule rawMatch) of
        Nothing ->
          Just rewriteMatch
        Just compiledGuard ->
          let factStoreLookup =
                ContextFactStoreLookup
                  (Map.singleton contextKey factStore)
              compiledRegion =
                compileGuardRegion
                  regionTable
                  (regionAtKey regionTable contextKey)
                  buckets
                  graph
                  factStoreLookup
                  capabilityResolver
                  (rrmRootClass rawMatch)
                  (rrmSubstitution rawMatch)
                  compiledGuard
           in if regionMemberKey (guardRegion compiledRegion) contextKey
                then fmap (\guardEvidence -> rewriteMatch {ermGuardEvidence = Just guardEvidence}) (guardRegionEvidenceAtKey compiledRegion contextKey)
                else Nothing

type RegionedRawMatch :: Type -> Type -> (Type -> Type) -> Type
data RegionedRawMatch owner capability f = RegionedRawMatch
  { rrmRegionedRaw :: !(RawRewriteMatch capability f),
    rrmRegionedRegion :: !(ContextRegion owner)
  }

regionSupportedMatch ::
  (Language f, Ord c) =>
  PreparedContextSite owner c ->
  RegionTable owner ->
  AnnotatedDeltaBuckets owner f ->
  EGraph f a ->
  GuardCapabilityResolver capability ->
  Map.Map (ContextObjectKey owner) FactStore ->
  Map.Map c (FactStore, FactDerivationIndex, [RulePlan (CompiledGuard capability f) f]) ->
  RegionedRawMatch owner capability f ->
  Either (EGraphSaturationObstruction capability f c) (Maybe (SupportedRewriteMatch c capability f))
regionSupportedMatch site regionTable buckets graph capabilityResolver contextFactStoresByKey contextInputs regionedMatch = do
  let rawMatch =
        rrmRegionedRaw regionedMatch
      factStoreLookup =
        ContextFactStoreLookup contextFactStoresByKey
      rewriteMatch =
        ExecutableRewriteMatch (rrmRule rawMatch) (rrmRootClass rawMatch) Nothing Nothing (rrmSubstitution rawMatch)
      guarded =
        case rulePlanCondition (rrmRule rawMatch) of
          Nothing ->
            Just (rewriteMatch, rrmRegionedRegion regionedMatch, const (Just Nothing))
          Just compiledGuard ->
            let compiledRegion =
                  compileGuardRegion
                    regionTable
                    (rrmRegionedRegion regionedMatch)
                    buckets
                    graph
                    factStoreLookup
                    capabilityResolver
                    (rrmRootClass rawMatch)
                    (rrmSubstitution rawMatch)
                    compiledGuard
                matchRegion =
                  regionMeet (rrmRegionedRegion regionedMatch) (guardRegion compiledRegion)
             in Just
                  ( rewriteMatch,
                    matchRegion,
                    fmap Just . guardRegionEvidenceAtKey compiledRegion
                  )
  case guarded of
    Nothing ->
      Right Nothing
    Just (matchValue, matchRegion, evidenceAtKey)
      | regionEmpty matchRegion ->
          Right Nothing
      | otherwise -> do
          supportContexts <-
            contextsForRegionGenerators site regionTable matchRegion
          supportValue <-
            first
              EGraphSaturationPreparedSupportObstruction
              (preparedSupportFromContexts site supportContexts)
          witnesses <-
            fmap Map.fromList $
              traverse
                ( \contextValue -> do
                    contextKey <-
                      first
                        EGraphSaturationPreparedSupportObstruction
                        (contextObjectKeyFor site contextValue)
                    (contextFactStore, factDerivations, _) <-
                      maybe
                        (Left (EGraphSaturationRegionWitnessMissing contextValue))
                        Right
                        (Map.lookup contextValue contextInputs)
                    guardEvidence <- maybe (Left (EGraphSaturationRegionObjectKeyMissing (contextObjectKeyValue contextKey))) Right (evidenceAtKey contextKey)
                    let matchWithEvidence =
                          matchValue {ermGuardEvidence = guardEvidence}
                    pure
                      ( contextValue,
                        SupportMatchWitness
                          { smwFactStore = canonicalizeWitnessFactStore graph contextFactStore,
                            smwFactDerivations = consumedFactDerivations factDerivations matchWithEvidence,
                            smwGuardEvidence = ermGuardEvidence matchWithEvidence,
                            smwGuideEvidence = ermGuideEvidence matchWithEvidence
                          }
                      )
                )
                supportContexts
          let representativeMatch =
                maybe
                  matchValue
                  (\(_, witness) -> matchValue {ermGuardEvidence = smwGuardEvidence witness})
                  (listToMaybe (Map.toAscList witnesses))
          pure
            ( Just
                SupportedRewriteMatch
                  { srmMatch = representativeMatch,
                    srmSupport = supportValue,
                    srmWitnesses = witnesses
                  }
            )

canonicalizeWitnessFactStore :: EGraph f a -> FactStore -> FactStore
canonicalizeWitnessFactStore graph =
  LogicStore.canonicalizeFactStore (canonicalizeClassId graph)

contextsForRegionGenerators ::
  PreparedContextSite owner c ->
  RegionTable owner ->
  ContextRegion owner ->
  Either (EGraphSaturationObstruction capability f c) [c]
contextsForRegionGenerators site regionTable regionValue =
  traverse
    contextForKey
    (regionGeneratorKeys regionTable regionValue)
  where
    contextForKey keyValue =
      maybe
        (Left (EGraphSaturationRegionObjectKeyMissing (contextObjectKeyValue keyValue)))
        Right
        (preparedContextAtKey site keyValue)

eGraphMatchingWorld ::
  EGraph f a ->
  FactStore ->
  FactDerivationIndex ->
  GuardCapabilityResolver capability ->
  Maybe (MatchingProofContext owner c f a) ->
  Int ->
  MatchingWorld owner c capability f a
eGraphMatchingWorld graph facts factDerivations capabilityResolver proofContext iterationIndex =
  GenericMatching.MatchWorld
    { GenericMatching.mwGraph = graph,
      GenericMatching.mwFacts = facts,
      GenericMatching.mwFactDerivations = factDerivations,
      GenericMatching.mwCapabilities = capabilityResolver,
      GenericMatching.mwProofContext = proofContext,
      GenericMatching.mwIteration = iterationIndex
    }

-- | Guard evaluation over a context's annotated quotient: the three guard
-- capabilities served by the base store and delta buckets, no fiber view.
annotatedGuardView ::
  Language f =>
  AnnotatedContextView f ->
  EGraph f a ->
  GuardGraphView f
annotatedGuardView view graph =
  GuardGraphView
    { ggvCanonicalize = annotatedViewCanonicalize view graph,
      ggvLookupLeastENode = annotatedViewLookupLeastENode view graph,
      ggvChildAt = annotatedViewProjectChildAt view graph
    }

contextualCanonicalizer ::
  Ord c =>
  c ->
  SaturatingContextEGraph owner capability f a c ->
  Either
    (EGraphSaturationObstruction capability f c)
    (ClassId -> ClassId)
contextualCanonicalizer contextValue graph = do
  let contextGraph =
        sceContextGraph graph
      baseGraph =
        cegBase contextGraph
  contextKey <-
    first EGraphSaturationPreparedSupportObstruction
      (contextObjectKeyFor (cegSite contextGraph) contextValue)
  let contextView =
        annotatedContextViewAtKey
          contextKey
          (contextAnnotatedDeltaBuckets contextGraph)
  pure (annotatedViewCanonicalize contextView baseGraph)
{-# INLINE contextualCanonicalizer #-}

contextMatchInputFactStore :: (factStore, factDerivations, rules) -> factStore
contextMatchInputFactStore (factStore, _, _) =
  factStore
{-# INLINE contextMatchInputFactStore #-}

contextMatchInputRules :: (factStore, factDerivations, rules) -> rules
contextMatchInputRules (_, _, rules) =
  rules
{-# INLINE contextMatchInputRules #-}

nonEmptyContextRules :: [rule] -> Maybe [rule]
nonEmptyContextRules rules =
  case rules of
    [] -> Nothing
    _ : _ -> Just rules
{-# INLINE nonEmptyContextRules #-}

instance
  ( Language f,
    HasConstructorTag f,
    Show (f ()),
    Eq a,
    Ord a,
    JoinSemilattice a,
    Ord capability,
    Show capability,
    Ord c
  ) =>
  SaturationGraph (EGraphU owner capability f a c)
  where
  graphCanonicalizeClass classId graph =
    canonicalizeClassId (cegBase (sceContextGraph graph)) classId

  graphClassCount =
    eGraphClassCount . cegBase . sceContextGraph

  graphNodeCount =
    eGraphNodeCount . cegBase . sceContextGraph

  graphBase =
    cegBase . sceContextGraph

  baseGraphEquals left right =
    materializeEGraphClasses left == materializeEGraphClasses right
      && materializeEGraphHashCons left == materializeEGraphHashCons right
      && eGraphPendingClassUnions left == eGraphPendingClassUnions right
      && eGraphClassCount left == eGraphClassCount right

  graphPreparedSite =
    cegSite . sceContextGraph

  graphContextLattice =
    cegLattice . sceContextGraph

  graphExecutionContexts =
    contextPreparedObjects . sceContextGraph

  graphPendingMerges graph =
    let contextGraph =
          sceContextGraph graph
     in length (eGraphPendingClassUnions (cegBase contextGraph))
      + Map.foldl'
        (\mergeCount fiberValue -> mergeCount + length (equivalencePairs (cfRelation fiberValue)))
        0
        (cegContextFibers contextGraph)

  graphConvergenceStateEquals left right =
    let leftGraph =
          sceContextGraph left
        rightGraph =
          sceContextGraph right
     in eGraphRevision (cegBase leftGraph) == eGraphRevision (cegBase rightGraph)
          && eGraphPendingClassUnions (cegBase leftGraph) == eGraphPendingClassUnions (cegBase rightGraph)
          && cegContextFibers leftGraph == cegContextFibers rightGraph
          && cegClassSupportIndex leftGraph == cegClassSupportIndex rightGraph

  graphContextClassProjection ctx graph =
    first EGraphSaturationPreparedSupportObstruction
      (payloadMapToRepresentativeMap <$> materializeAmbientPayloadFor ctx (sceContextGraph graph))

  graphContextClasses ctx graph =
    fmap
      (Set.fromList . IntMap.elems)
      (graphContextClassProjection @(EGraphU owner capability f a c) ctx graph)

instance
  ( Language f,
    HasConstructorTag f,
    Show (f ()),
    Eq a,
    Ord a,
    JoinSemilattice a,
    Ord capability,
    Show capability
  ) =>
  BaseGraphEmbedding
    (EGraphU UnitContextSiteOwner capability f a TrivialContext)
    (SaturatingContextEGraph UnitContextSiteOwner capability f a TrivialContext)
  where
  embedBaseGraph baseGraph =
    emptySaturatingContextEGraph
      (emptyContextEGraphFromSite unitPreparedContextSite baseGraph)

instance
  ( Language f,
    HasConstructorTag f,
    Show (f ()),
    Eq a,
    Ord a,
    JoinSemilattice a,
    Ord capability,
    Show capability,
    Ord c
  ) =>
  CapabilitySystem (EGraphU owner capability f a c)
  where
  emptyCapabilityResolver = emptyGuardCapabilityResolver

instance
  ( Language f,
    HasConstructorTag f,
    Show (f ()),
    Eq a,
    Ord a,
    JoinSemilattice a,
    Ord capability,
    Show capability,
    Ord c
  ) =>
  QueryIndex (EGraphU owner capability f a c)
  where
  queryFingerprint =
    fmap GenericMatching.QueryFingerprint
      . first EGraphSaturationPatternAtomizeObstruction
      . compiledPatternQueryFingerprint
  matchSnapshotKey =
    GenericMatching.QueryFingerprint
      . contextObjectKeyValue
      . acsContextKey
  fullMatchingDelta = Delta.fullDelta
  registerQueries queries graph =
    first eGraphMatchingToSaturationObstruction (registerCompiledPatternQueries queries graph)
  contextMatchSections _graph =
    Map.empty
  lookupQueryId fingerprint graph =
    SaturationMatch.lookupQueryIdByFingerprint
      fingerprint
      (cssQueryRegistry (sceSaturationState graph))

instance
  ( Language f,
    HasConstructorTag f,
    Show (f ()),
    Eq a,
    Ord a,
    JoinSemilattice a,
    Ord capability,
    Show capability,
    Ord c
  ) =>
  FactSystem (EGraphU owner capability f a c)
  where
  type SatFactRuleIdentity (EGraphU owner capability f a c) = CompiledFactRule capability f

  emptyFactStore = LogicStore.emptyFactStore
  emptyFactIndex = emptyFactDerivationIndex
  canonicalizeFactStore graph = LogicStore.canonicalizeFactStore (canonicalizeClassId (cegBase (sceContextGraph graph)))
  canonicalizeFactIndex graph = canonicalizeFactDerivationIndex (canonicalizeClassId (cegBase (sceContextGraph graph)))
  canonicalizeFactStoreBase baseGraph = LogicStore.canonicalizeFactStore (canonicalizeClassId baseGraph)
  canonicalizeFactIndexBase baseGraph = canonicalizeFactDerivationIndex (canonicalizeClassId baseGraph)
  canonicalizeFactStoreAtContext contextValue graph factStore = do
    contextualCanonicalize <- contextualCanonicalizer contextValue graph
    pure (LogicStore.canonicalizeFactStore contextualCanonicalize factStore)
  canonicalizeFactIndexAtContext contextValue graph factIndex = do
    contextualCanonicalize <- contextualCanonicalizer contextValue graph
    pure (canonicalizeFactDerivationIndex contextualCanonicalize factIndex)
  unionFactStores = LogicStore.unionFactStores
  factChangeMatchingDelta _graph oldFacts newFacts =
    Delta.scopedDelta
      (Delta.dirtyScope (LogicStore.changedScopedFactStoreClassKeys oldFacts newFacts))
      Nothing
  compileFactRules = LogicRule.compileFactRules
  factRuleQuery = cfrCompiledQuery
  factRuleId rule = RewriteRuleId (unFactRuleId (cfrId rule))
  factRuleIdentity =
    Right
  factSourceId rule = RewriteRuleId (unFactRuleId (frId rule))
  deriveFactClosure capabilityResolver existingStore factRules baseGraph contextStore contextIndex =
    let initialFacts =
          LogicStore.unionFactStores existingStore contextStore
        closureMatchState :: EGraphMatchState owner c capability f a
        closureMatchState =
          mkEGraphMatchState
            RegionNativeEmission
            (wcojMatchingAlgebra capabilityResolver)
        closureResult =
          deriveSeededFactClosureWithStateAndConfig
            FactClosureRun
              { fcrConfig = defaultSemiNaiveConfig,
                fcrCapabilityResolver = capabilityResolver,
                fcrInitialFacts = initialFacts,
                fcrSeedDerivations = contextIndex,
                fcrInitialState = closureMatchState,
                fcrMatcher =
                  mkSemiNaiveMatcher
                    ( \matchState frontier rule graph ->
                        runEGraphMatchingRequest
                          Delta.fullDelta
                          ( eGraphMatchingWorld
                              graph
                              (sniAllFacts frontier)
                              (sniAllDerivations frontier)
                              capabilityResolver
                              Nothing
                              (sniRoundIndex frontier)
                          )
                          GenericMatching.QueryRequest
                            { GenericMatching.qrSite = GenericMatching.BaseSite,
                              GenericMatching.qrSnapshot = Nothing,
                              GenericMatching.qrQuery = cfrCompiledQuery rule,
                              GenericMatching.qrPurpose = GenericMatching.FactRulePurpose (cfrId rule)
                            }
                          matchState
                    ),
                fcrResolveTerm =
                  \root matchSubstitution guardTerm ->
                    resolveGuardTermWith (graphGuardView baseGraph) root matchSubstitution guardTerm,
                fcrCanonicalClass = canonicalizeClassId baseGraph,
                fcrRules = factRules,
                fcrHost = baseGraph
              }
     in bimap
          factClosureRunErrorToSaturationObstruction
          ( \(_matchState, derivedClosure) ->
              (sncFacts derivedClosure, sncDerivations derivedClosure, sncRounds derivedClosure)
          )
          closureResult
  deriveFactClosureAtContext capabilityResolver existingStore factRules graph contextValue contextStore contextIndex =
    snd
      <$> eGraphFactClosureAtContext
        capabilityResolver
        existingStore
        factRules
        graph
        contextValue
        contextStore
        contextIndex
        (mkEGraphMatchState RegionNativeEmission (wcojMatchingAlgebra capabilityResolver))
  deriveFactClosuresAtContexts capabilityResolver graph contextInputs =
    let initialState :: EGraphMatchState owner c capability f a
        initialState =
          mkEGraphMatchState
            RegionNativeEmission
            (wcojMatchingAlgebra capabilityResolver)
        closeNext (sharedState, results) (contextValue, (contextFactStore, factRules)) =
          fmap
            ( \(nextState, contextResult) ->
                (nextState, Map.insert contextValue contextResult results)
            )
            ( eGraphFactClosureAtContext
                capabilityResolver
                contextFactStore
                factRules
                graph
                contextValue
                contextFactStore
                emptyFactDerivationIndex
                sharedState
            )
     in snd
          <$> foldlM
            closeNext
            (initialState, Map.empty)
            (Map.toAscList contextInputs)

-- | Fact closure at a context through the annotated quotient — matching at
-- the context site over the shared base plus delta buckets, guard resolution
-- and canonicalization through the annotated view, no materialized fiber.
-- The threaded match state carries the prepared base-match memo, so a caller
-- folding it across contexts pays base structural matching once per rule.
eGraphFactClosureAtContext ::
  forall owner capability f a c.
  ( Language f,
    Ord c
  ) =>
  GuardCapabilityResolver capability ->
  FactStore ->
  [CompiledFactRule capability f] ->
  SaturatingContextEGraph owner capability f a c ->
  c ->
  FactStore ->
  FactDerivationIndex ->
  EGraphMatchState owner c capability f a ->
  Either
    (EGraphSaturationObstruction capability f c)
    (EGraphMatchState owner c capability f a, (FactStore, FactDerivationIndex, [SemiNaiveRound]))
eGraphFactClosureAtContext capabilityResolver existingStore factRules graph contextValue contextStore contextIndex sharedMatchState =
  do
    let innerContextGraph =
          sceContextGraph graph
        baseGraph =
          cegBase innerContextGraph
    contextKey <-
      first
        EGraphSaturationPreparedSupportObstruction
        (contextObjectKeyFor (cegSite innerContextGraph) contextValue)
    let annotatedBuckets =
          contextAnnotatedDeltaBuckets innerContextGraph
        annotatedView =
          annotatedContextViewAtKey contextKey annotatedBuckets
        annotatedSource =
          AnnotatedContextSource
            { acsBuckets = annotatedBuckets,
              acsContextKey = contextKey,
              acsContextRevision = cegContextRevision innerContextGraph
            }
        contextGuardView =
          annotatedGuardView annotatedView baseGraph
        initialFacts =
          LogicStore.unionFactStores existingStore contextStore
        closureResult =
          deriveSeededFactClosureWithStateAndConfig
            FactClosureRun
              { fcrConfig = defaultSemiNaiveConfig,
                fcrCapabilityResolver = capabilityResolver,
                fcrInitialFacts = initialFacts,
                fcrSeedDerivations = contextIndex,
                fcrInitialState = sharedMatchState,
                fcrMatcher =
                  mkSemiNaiveMatcher
                    ( \matchState frontier rule hostGraph ->
                        runEGraphMatchingRequest
                          Delta.fullDelta
                          ( eGraphMatchingWorld
                              hostGraph
                              (sniAllFacts frontier)
                              (sniAllDerivations frontier)
                              capabilityResolver
                              Nothing
                              (sniRoundIndex frontier)
                          )
                          GenericMatching.QueryRequest
                            { GenericMatching.qrSite = GenericMatching.ContextSite contextValue,
                              GenericMatching.qrSnapshot = Just annotatedSource,
                              GenericMatching.qrQuery = cfrCompiledQuery rule,
                              GenericMatching.qrPurpose = GenericMatching.FactRulePurpose (cfrId rule)
                            }
                          matchState
                    ),
                fcrResolveTerm =
                  \root matchSubstitution guardTerm ->
                    resolveGuardTermWith contextGuardView root matchSubstitution guardTerm,
                fcrCanonicalClass = annotatedViewCanonicalize annotatedView baseGraph,
                fcrRules = factRules,
                fcrHost = baseGraph
              }
    bimap
      factClosureRunErrorToSaturationObstruction
      ( \(nextMatchState, derivedClosure) ->
          ( nextMatchState,
            (sncFacts derivedClosure, sncDerivations derivedClosure, sncRounds derivedClosure)
          )
      )
      closureResult

instance
  ( Language f,
    HasConstructorTag f,
    Show (f ()),
    Eq a,
    Ord a,
    JoinSemilattice a,
    Ord capability,
    Show capability,
    Ord c
  ) =>
  RewriteSystem (EGraphU owner capability f a c)
  where
  type SatRewriteRuleIdentity (EGraphU owner capability f a c) = RulePlan (CompiledGuard capability f) f

  compileRewriteRules rawRules = do
    checkedSystem <-
      first EGraphRuleCheckFailed (checkRawRewriteSystem rawRules)
    rulePlanSet <-
      first EGraphRulePlanningFailed (planRuleSet checkedSystem)
    pure (rulePlans rulePlanSet)
  rewriteRuleSourceId = rrId
  rewriteRuleId =
    rpId
  rewriteRuleIdentity =
    Right
  rewriteRuleKey =
    rpId
  rewriteRuleQuery =
    rpQuery
  defaultRewriteContext = emptyRewriteRuntimeCapabilities
  rewriteCapabilityResolver rewriteContext _graph =
    rewriteRuntimeGuardCapabilityResolver rewriteContext

instance
  ( Language f,
    HasConstructorTag f,
    Show (f ()),
    Eq a,
    Ord a,
    JoinSemilattice a,
    Ord capability,
    Show capability,
    Ord c
  ) =>
  MatchView (EGraphU owner capability f a c)
  where
  matchKey match =
    ( rpId (ermRule match),
      ermRootClass match,
      ermSubstitution match
    )

  matchRuleKey =
    rpId . ermRule
  supportedMatchInner = srmMatch
  setSupportedMatchInner newMatch srm = srm {srmMatch = newMatch}
  supportedMatchBasis = srmSupport
  supportedMatchWitnesses = srmWitnesses
  mergeSupportedMatch graph left right =
    bimap
      EGraphSaturationPreparedSupportObstruction
      ( \mergedSupport ->
          left
            { srmSupport = mergedSupport,
              srmWitnesses = Map.union (srmWitnesses left) (srmWitnesses right)
            }
      )
      (unionPreparedSupport (graphPreparedSite @(EGraphU owner capability f a c) graph) (srmSupport left) (srmSupport right))

eGraphProductivityKeyFor ::
  (ClassId -> ClassId) ->
  SupportedRewriteMatch c capability f ->
  EGraphProductivityKey c
eGraphProductivityKeyFor canonicalize supportedMatch =
  let rewriteMatch =
        srmMatch supportedMatch
   in canonicalizeProductivityKey
        canonicalize
        EGraphProductivityKey
          { epkRuleId = rpId (ermRule rewriteMatch),
            epkRoot = ermRootClass rewriteMatch,
            epkSubstitution = ermSubstitution rewriteMatch,
            epkSupport = srmSupport supportedMatch
          }

filterObservedSaturatedMatches ::
  forall owner annotation capability f a c.
  Ord c =>
  EGraphMatchState owner c capability f a ->
  SaturatingContextEGraph owner capability f a c ->
  [(annotation, SupportedRewriteMatch c capability f)] ->
  [(annotation, SupportedRewriteMatch c capability f)]
filterObservedSaturatedMatches matchState graph matches =
  case matchState of
    EGraphMatchState _emission saturatedKeys canonicalize _regionalState _matchingAlgebra _state ->
      let canonicalizeKey =
            canonicalize . canonicalizeClassId (cegBase (sceContextGraph graph))
       in filter
            ( \(_, supportedMatch) ->
                Set.notMember
                  (eGraphProductivityKeyFor canonicalizeKey supportedMatch)
                  saturatedKeys
            )
            matches

filterProductiveSupportedMatches ::
  (Language f, Ord c) =>
  RewriteRuntimeCapabilities (GuardCapabilityResolver capability) f ->
  SaturatingContextEGraph owner capability f a c ->
  [(annotation, SupportedRewriteMatch c capability f)] ->
  [(annotation, SupportedRewriteMatch c capability f)]
filterProductiveSupportedMatches rewriteContext graph annotatedMatches =
  filter
    (supportedMatchMayProduceEffect rewriteContext graph . snd)
    annotatedMatches

supportedMatchMayProduceEffect ::
  (Language f, Ord c) =>
  RewriteRuntimeCapabilities (GuardCapabilityResolver capability) f ->
  SaturatingContextEGraph owner capability f a c ->
  SupportedRewriteMatch c capability f ->
  Bool
supportedMatchMayProduceEffect _rewriteContext graph supportedMatch =
  let contextGraph =
        sceContextGraph graph
   in case cheapSupportedMatchMayProduceEffect contextGraph supportedMatch of
    Just mayProduceEffect ->
      mayProduceEffect
    Nothing ->
      True

cheapSupportedMatchMayProduceEffect ::
  forall owner capability f c a.
  (Language f, Ord c) =>
  ContextEGraph owner f a c ->
  SupportedRewriteMatch c capability f ->
  Maybe Bool
cheapSupportedMatchMayProduceEffect contextGraph supportedMatch =
  let rewriteMatch =
        srmMatch supportedMatch
      rewriteRule =
        ermRule rewriteMatch
   in case (rpApplicationCondition rewriteRule, rulePlanPostSubst rewriteRule, Map.toAscList (srmWitnesses supportedMatch)) of
        (Nothing, Nothing, []) ->
          supportMayProduceEffect rewriteMatch rewriteRule contextGraph (srmSupport supportedMatch)
        (Nothing, Nothing, witnesses) ->
          summarizeWitnessProductivity
            (fmap (witnessMayProduceEffect rewriteMatch rewriteRule) witnesses)
        _ ->
          Nothing
  where
    witnessMayProduceEffect ::
      ExecutableRewriteMatch (CompiledGuard capability f) GuardEvidence guideEvidence f ->
      RulePlan (CompiledGuard capability f) f ->
      (c, SupportMatchWitness f) ->
      Maybe Bool
    witnessMayProduceEffect rewriteMatch rewriteRule (contextValue, _supportWitness) =
      contextualRewriteMayProduceEffect
        contextValue
        rewriteMatch
        rewriteRule

    supportMayProduceEffect ::
      ExecutableRewriteMatch (CompiledGuard capability f) GuardEvidence guideEvidence f ->
      RulePlan (CompiledGuard capability f) f ->
      ContextEGraph owner f a c ->
      SupportBasis c ->
      Maybe Bool
    supportMayProduceEffect rewriteMatch rewriteRule localContextGraph supportValue =
      if supportValue == defaultPreparedSupport (cegSite localContextGraph)
        then
          rewriteMayProduceEffect
            (cegBase localContextGraph)
            rewriteMatch
            rewriteRule
        else
          summarizeWitnessProductivity
            ( fmap
                ( \contextValue ->
                    contextualRewriteMayProduceEffect
                      contextValue
                      rewriteMatch
                      rewriteRule
                )
                (supportGenerators supportValue)
            )

    contextualRewriteMayProduceEffect ::
      c ->
      ExecutableRewriteMatch (CompiledGuard capability f) GuardEvidence guideEvidence f ->
      RulePlan (CompiledGuard capability f) f ->
      Maybe Bool
    contextualRewriteMayProduceEffect contextValue rewriteMatch rewriteRule = do
      contextKey <-
        either
          (const Nothing)
          Just
          (contextObjectKeyFor (cegSite contextGraph) contextValue)
      let baseGraph = cegBase contextGraph
          buckets = contextAnnotatedDeltaBuckets contextGraph
          canonicalize classId =
            let baseClass = canonicalizeClassId baseGraph classId
             in ClassId
                  ( annotatedRepresentativeKeyAt
                      contextKey
                      buckets
                      (classIdKey baseClass)
                  )
          lookupNode (ENode nodeShape) =
            let canonicalShape = fmap canonicalize nodeShape
                canonicalChildren = fmap classIdKey (toList canonicalShape)
                baseOwnerKeys =
                  fmap
                    (classIdKey . canonicalize)
                    ( case structuralLookupTupleAll (ENode canonicalShape) (eGraphStore baseGraph) of
                        StructuralMissing -> []
                        StructuralUnique ownerClass -> [ownerClass]
                        StructuralAmbiguous ownerClasses -> toList ownerClasses
                    )
                variantOwnerKeys =
                  [ arRootKey row
                    | row <- annotatedVariantRowsForTag (void nodeShape) buckets,
                      arChildKeys row == canonicalChildren,
                      regionMemberKey (arRegion row) contextKey
                  ]
             in ClassId <$> Set.lookupMin (Set.fromList (baseOwnerKeys <> variantOwnerKeys))
          rootClass = canonicalize (ermRootClass rewriteMatch)
      case resolveExistingPatternClassWith
        canonicalize
        lookupNode
        (ermSubstitution rewriteMatch)
        (rulePlanRhsPattern rewriteRule) of
        Nothing ->
          Nothing
        Just Nothing ->
          Just True
        Just (Just rhsClass) ->
          Just (rootClass /= canonicalize rhsClass)

    rewriteMayProduceEffect ::
      EGraph f a ->
      ExecutableRewriteMatch (CompiledGuard capability f) GuardEvidence guideEvidence f ->
      RulePlan (CompiledGuard capability f) f ->
      Maybe Bool
    rewriteMayProduceEffect graph rewriteMatch rewriteRule =
      let rootClass =
            canonicalizeClassId graph (ermRootClass rewriteMatch)
       in case resolveExistingPatternClass
            graph
            (ermSubstitution rewriteMatch)
            (rulePlanRhsPattern rewriteRule) of
            Nothing ->
              Nothing
            Just Nothing ->
              Just True
            Just (Just rhsClass) ->
              Just (rootClass /= canonicalizeClassId graph rhsClass)

    summarizeWitnessProductivity ::
      Foldable t =>
      t (Maybe Bool) ->
      Maybe Bool
    summarizeWitnessProductivity witnessResults
      | any (== Just True) witnessResults =
          Just True
      | all (== Just False) witnessResults =
          Just False
      | otherwise =
          Nothing

retainSaturatedKeysForDelta ::
  MatchingDelta ->
  Set.Set (EGraphProductivityKey c) ->
  Set.Set (EGraphProductivityKey c)
retainSaturatedKeysForDelta matchingDelta saturatedKeys
  | matchingDeltaDemandsSaturatedKeyReset matchingDelta =
      Set.empty
  | otherwise =
      Delta.foldScope
        saturatedKeys
        (const saturatedKeys)
        Set.empty
        (Delta.scopedDeltaSupport matchingDelta)

matchingDeltaDemandsSaturatedKeyReset :: MatchingDelta -> Bool
matchingDeltaDemandsSaturatedKeyReset matchingDelta =
  maybe
    False
    (maybe False oiInvalidateAllRoots . mdpObstructionInvalidation)
    (Delta.scopedDeltaPayload matchingDelta)

instance
  ( Language f,
    HasConstructorTag f,
    Show (f ()),
    Eq a,
    Ord a,
    JoinSemilattice a,
    Ord capability,
    Show capability,
    Ord c
  ) =>
  MatchingBackend (EGraphU owner capability f a c)
  where
  initialMatchState matchingStrategy rewriteContext =
    let capabilityResolver =
          rewriteRuntimeGuardCapabilityResolver rewriteContext
     in case matchingStrategy of
          GenericJoinMatching ->
            mkEGraphMatchState RegionNativeEmission (wcojMatchingAlgebra capabilityResolver)
          GenericJoinPerContextMatching ->
            mkEGraphMatchState PerContextEmission (wcojMatchingAlgebra capabilityResolver)
          CustomMatchingAlgebra matchingAlgebra ->
            mkEGraphMatchState PerContextEmission matchingAlgebra

  runMatchingRequests matchingDelta matchingWorld requests matchState =
    let (nextMatchState, matchesResult) =
          runEGraphMatchingRequests
            matchingDelta
            matchingWorld
            requests
            matchState
     in ( nextMatchState,
          first eGraphMatchingToSaturationObstruction matchesResult
        )

  materializeRawMatch _rewriteContext capResolver ctx factStore factIndex baseGraph rawMatch =
    fmap
      (\match -> SupportedRewriteMatch
        { srmMatch = match,
          srmSupport = principalSupport ctx,
          srmWitnesses = Map.singleton ctx SupportMatchWitness
            { smwFactStore = factStore,
              smwFactDerivations = consumedFactDerivations factIndex match,
              smwGuardEvidence = ermGuardEvidence match,
              smwGuideEvidence = ermGuideEvidence match
            }
        })
      (materializeRawRewriteMatch capResolver factStore (rrmRule rawMatch) (rrmRootClass rawMatch) (rrmSubstitution rawMatch) baseGraph)
  materializeRawMatchesAtContextView _rewriteContext capResolver ctx factStore factIndex graph rawMatches =
    let innerContextGraph =
          sceContextGraph graph
        baseGraph =
          cegBase innerContextGraph
     in do
          contextKey <-
            first
              EGraphSaturationPreparedSupportObstruction
              (contextObjectKeyFor (cegSite innerContextGraph) ctx)
          let buckets =
                contextAnnotatedDeltaBuckets innerContextGraph
              regionTable =
                preparedRegionTable (cegSite innerContextGraph)
              canonicalFactStore =
                LogicStore.canonicalizeFactStore (canonicalizeClassId baseGraph) factStore
              materializeAtView rawMatch =
                fmap
                  (\match -> SupportedRewriteMatch
                    { srmMatch = match,
                      srmSupport = principalSupport ctx,
                      srmWitnesses = Map.singleton ctx SupportMatchWitness
                        { smwFactStore = canonicalFactStore,
                          smwFactDerivations = consumedFactDerivations factIndex match,
                          smwGuardEvidence = ermGuardEvidence match,
                          smwGuideEvidence = ermGuideEvidence match
                        }
                    })
                  (materializeRawRewriteMatchRegion regionTable buckets baseGraph capResolver canonicalFactStore contextKey rawMatch)
          pure (mapMaybe materializeAtView rawMatches)
  rawBaseMatchesPrepared rewriteContext iterationIndex matchingDelta graph factStore rules matchState =
    let baseGraph =
          cegBase (sceContextGraph graph)
        capResolver =
          rewriteRuntimeGuardCapabilityResolver rewriteContext
        matchingWorld =
          eGraphMatchingWorld
            baseGraph
            factStore
            emptyFactDerivationIndex
            capResolver
            Nothing
            iterationIndex
        requests =
          fmap
            (\compiledRule ->
                preparedRuleRequest
                  GenericMatching.BaseSite
                  Nothing
                  compiledRule)
            rules
        (nextMatchState, rootedMatchesByRuleResult) =
          runMatchingRequests @(EGraphU owner capability f a c)
            matchingDelta
            matchingWorld
            requests
            matchState
     in fmap
          ( \rootedMatchesByRule ->
              ( nextMatchState,
                concat
                  ( zipWith
                      ( \compiledRule rootedMatches ->
                          fmap
                            (uncurry (RawRewriteMatch compiledRule))
                            rootedMatches
                      )
                      rules
                      rootedMatchesByRule
                  )
              )
          )
          rootedMatchesByRuleResult
  rawContextMatchesPrepared rewriteContext ctx iterationIndex matchingDelta contextGraph factStore factDerivations rules matchState =
    let capResolver =
          rewriteRuntimeGuardCapabilityResolver rewriteContext
        innerContextGraph =
          sceContextGraph contextGraph
        proofContext =
          either
            (const Nothing)
            Just
            (mkMatchingProofContext (emptyProofEGraph innerContextGraph))
        matchingWorld =
          eGraphMatchingWorld
            (cegBase innerContextGraph)
            factStore
            factDerivations
            capResolver
            proofContext
            iterationIndex
     in do
          contextKey <-
            first
              EGraphSaturationPreparedSupportObstruction
              (contextObjectKeyFor (cegSite innerContextGraph) ctx)
          let annotatedSource =
                AnnotatedContextSource
                  { acsBuckets = contextAnnotatedDeltaBuckets innerContextGraph,
                    acsContextKey = contextKey,
                    acsContextRevision = cegContextRevision innerContextGraph
                  }
              requests =
                fmap
                  ( preparedRuleRequest
                      (GenericMatching.ContextSite ctx)
                      (Just annotatedSource)
                  )
                  rules
              (nextMatchState, rootedMatchesByRuleResult) =
                runMatchingRequests @(EGraphU owner capability f a c)
                  matchingDelta
                  matchingWorld
                  requests
                  matchState
          fmap
            ( \rootedMatchesByRule ->
                ( nextMatchState,
                  concat
                    ( zipWith
                        ( \compiledRule rootedMatches ->
                            fmap
                              (uncurry (RawRewriteMatch compiledRule))
                              rootedMatches
                        )
                        rules
                        rootedMatchesByRule
                    )
                )
            )
            rootedMatchesByRuleResult
  contextSupportedMatchesPrepared rewriteContext capResolver iterationIndex matchingDelta contextGraph contextInputs supportedRules startingMatchState@(EGraphMatchState emission saturatedKeys canonicalize regionalState matchingAlgebra genericState)
    | PerContextEmission <- emission =
        contextSupportedMatchesPreparedViaContexts @(EGraphU owner capability f a c)
          rewriteContext
          capResolver
          iterationIndex
          matchingDelta
          contextGraph
          contextInputs
          supportedRules
          startingMatchState
    | otherwise =
    let innerContextGraph =
          sceContextGraph contextGraph
        baseGraph =
          cegBase innerContextGraph
        site =
          cegSite innerContextGraph
        regionTable =
          preparedRegionTable site
        buckets =
          contextAnnotatedDeltaBuckets innerContextGraph
        activeRulesById =
          Map.fromListWith
            combineActiveRule
            ( [ (rpId rule, (rule, regionAtKey regionTable contextKey))
              | (contextValue, rules) <- Map.toAscList contextRules,
                Right contextKey <- [contextObjectKeyFor site contextValue],
                rule <- rules
              ]
                <> [ (rpId (sirRule indexedRule), (sirRule indexedRule, supportCarrierRegion site carrier))
                     | (indexedRule, carrier) <- supportedRules
                   ]
            )
        combineActiveRule ::
          (RulePlan (CompiledGuard capability f) f, ContextRegion owner) ->
          (RulePlan (CompiledGuard capability f) f, ContextRegion owner) ->
          (RulePlan (CompiledGuard capability f) f, ContextRegion owner)
        combineActiveRule (leftRule, leftRegion) (_rightRule, rightRegion) =
          (leftRule, regionJoin leftRegion rightRegion)
        contextRules =
          LazyMap.mapMaybe
            (nonEmptyContextRules . contextMatchInputRules)
            contextInputs
        rulesByQuery =
          Map.fromListWith
            (<>)
            [ (compiledPatternQueryKey (rpQuery rule), [(rule, ruleRegion)])
              | (rule, ruleRegion) <- Map.elems activeRulesById
            ]
        combineRegionedMatch ::
          RegionedRawMatch owner capability f ->
          RegionedRawMatch owner capability f ->
          RegionedRawMatch owner capability f
        combineRegionedMatch left right =
          left
            { rrmRegionedRegion =
                regionJoin (rrmRegionedRegion left) (rrmRegionedRegion right)
            }
        runRegionalQuery (currentState, accumulatedMatches) (_queryKey, ruleRegions) =
          case ruleRegions of
            [] ->
              Right (currentState, accumulatedMatches)
            (representativeRule, _) : _ -> do
              (nextState, queryMatches) <-
                first
                  (eGraphMatchingToSaturationObstruction . eGraphRelationalMatchObstruction)
                  ( wcojPreparedRegionalDeltaMatchCompiledWithRoots
                      regionTable
                      buckets
                      (cegContextRevision innerContextGraph)
                      (rpQuery representativeRule)
                      baseGraph
                      currentState
                  )
              let regionedMatches =
                    [ RegionedRawMatch
                        { rrmRegionedRaw =
                            RawRewriteMatch rule rootClass substitutionValue,
                          rrmRegionedRegion = matchRegion
                        }
                      | (rule, ruleRegion) <- ruleRegions,
                        (rootClass, substitutionValue, queryRegion) <- queryMatches,
                        let matchRegion = regionMeet ruleRegion queryRegion,
                        not (regionEmpty matchRegion)
                    ]
              Right (nextState, regionedMatches <> accumulatedMatches)
     in do
          (nextRegionalState, rawRegionedMatches) <-
            foldlM
              runRegionalQuery
              (regionalState, [])
              (Map.toAscList rulesByQuery)
          let allRegionedMatches =
                Map.elems $
                  Map.fromListWith
                    combineRegionedMatch
                    [ (rawRewriteMatchKey (rrmRegionedRaw regionedMatch), regionedMatch)
                      | regionedMatch <- rawRegionedMatches
                    ]
          contextFactStoresByKey <-
            fmap LazyMap.fromList $
              traverse
                ( \(contextValue, contextInput) -> do
                    contextKey <-
                      first
                        EGraphSaturationPreparedSupportObstruction
                        (contextObjectKeyFor site contextValue)
                    pure (contextKey, contextMatchInputFactStore contextInput)
                )
                (Map.toAscList contextInputs)
          supportedMatches <-
            fmap catMaybes $
              traverse
                ( regionSupportedMatch
                    site
                    regionTable
                    buckets
                    baseGraph
                    capResolver
                    contextFactStoresByKey
                    contextInputs
                )
                allRegionedMatches
          pure
            ( EGraphMatchState
                emission
                saturatedKeys
                canonicalize
                nextRegionalState
                matchingAlgebra
                genericState,
              supportedMatches
            )
  consumedDerivations supportedMatch =
    foldMap
      singletonFactDerivationIndex
      (foldMap smwFactDerivations (srmWitnesses supportedMatch))
  rawMatchRuleKey =
    rpId . rrmRule
  filterSupportedMatches rewriteContext _factStore matchState matches graph =
    filterProductiveSupportedMatches rewriteContext graph
      (filterObservedSaturatedMatches matchState graph matches)
  advanceMatchStateForRound matchingDelta graph (EGraphMatchState emission saturatedKeys canonicalize regionalState matchingAlgebra matchState) =
    EGraphMatchState
      emission
      (retainSaturatedKeysForDelta matchingDelta saturatedKeys)
      canonicalize
      (advanceRegionalPreparedMatchState matchingDelta regionalState)
      matchingAlgebra
      ( GenericMatching.maAdvanceState
          matchingAlgebra
          matchingDelta
          MatchingAdvanceCtx
            { macGraph = cegBase (sceContextGraph graph),
              macCanonicalize = canonicalize,
              macContextSite = Just (cegSite (sceContextGraph graph)),
              macContextRevision = Just (cegContextRevision (sceContextGraph graph))
            }
          matchState
      )
  advanceMatchStateAfterRebuild rebuildReport (EGraphMatchState emission saturatedKeys _ regionalState matchingAlgebra matchState) =
    let rebuiltBase = cegBase (sceContextGraph (rrrGraph rebuildReport))
        canonicalize = canonicalizeClassId rebuiltBase
        touchedKeys =
          eGraphRebuildDeltaTouchedKeys (rrrRebuildDelta rebuildReport)
        nextSaturatedKeys
          | IntSet.null touchedKeys =
              saturatedKeys
          | otherwise =
              Set.map (canonicalizeProductivityKey canonicalize) saturatedKeys
     in EGraphMatchState
          emission
          nextSaturatedKeys
          canonicalize
          regionalState
          matchingAlgebra
          matchState
  recordScheduledMatches _scheduledMatches matchState =
    matchState
  recordApplicationResult graph applicationResult (EGraphMatchState emission saturatedKeys canonicalize regionalState matchingAlgebra matchState) =
    let canonicalizeKey =
          canonicalize . canonicalizeClassId (cegBase (sceContextGraph graph))
        appliedKeys =
          Set.fromList (fmap (eGraphProductivityKeyFor canonicalizeKey) (egarAppliedMatches applicationResult))
     in EGraphMatchState
          emission
          (saturatedKeys <> appliedKeys)
          canonicalize
          regionalState
          matchingAlgebra
          matchState
instance
  ( Language f,
    HasConstructorTag f,
    Show (f ()),
    Eq a,
    Ord a,
    JoinSemilattice a,
    Ord capability,
    Show capability,
    Ord c
  ) =>
  ApplicationResultSystem (EGraphU owner capability f a c)
  where
  applicationResultCount =
    eGraphProgramEffectCount . contextMutationTraceEffect . egarTrace

instance
  ( Language f,
    HasConstructorTag f,
    Show (f ()),
    Eq a,
    Ord a,
    JoinSemilattice a,
    Ord capability,
    Show capability,
    Ord c
  ) =>
  GraphApply (EGraphU owner capability f a c)
  where
  applyBaseMatches runtimeCapabilities factStore matches graph =
    fmap
      (fmap (\nextGraph -> mapSaturatingContextGraph (const nextGraph) graph))
      ( egraphApplyMatchesBaseReported
          runtimeCapabilities
          factStore
          matches
          (sceContextGraph graph)
      )

  applyContextualMatches runtimeCapabilities matches graph =
    fmap
      (fmap (\nextGraph -> mapSaturatingContextGraph (const nextGraph) graph))
      ( egraphApplyMatchesContextualReported
          runtimeCapabilities
          matches
          (sceContextGraph graph)
      )

instance
  ( Language f,
    HasConstructorTag f,
    Show (f ()),
    Eq a,
    Ord a,
    JoinSemilattice a,
    Ord capability,
    Show capability,
    Ord c
  ) =>
  RebuildSystem (EGraphU owner capability f a c)
  where
  rebuildGraph graph _factStore _factIndex =
    fmap
      (\rebuildReport -> (rrrGraph rebuildReport, rebuildReport))
      (first EGraphSaturationRebuildObstruction (runRoundRebuildReport graph))
  rebuildEpoch =
    eGraphRevisionValue
      . eGraphRevision
      . cegBase
      . sceContextGraph
      . rrrGraph
  rebuildMatchingDelta = roundRebuildMatchingDelta
  factViewGraphChanges summary =
    FactViewGraphChanges
      { fvgcBaseChanged =
          any
            (not . IntSet.null . mutationTraceDirtyKeys . cmtBaseTrace)
            (egscApplicationTraces summary),
        fvgcChangedFiberAuthors =
          foldMap
            (Map.keysSet . cmtObservedLocalUnionsByContext)
            (egscApplicationTraces summary)
      }
  postApplyMatchingDelta _matchState _scheduledMatches applicationResult rebuildReport =
    matchingDeltaFromContextMutationTraceWithAnnotatedFrontier
      (egarTrace applicationResult)
      (contextAnnotatedDeltaDirtyFrontier (sceContextGraph (rrrGraph rebuildReport)))
      <> roundRebuildMatchingDelta rebuildReport
  postApplyChangeSummary _matchState _scheduledMatches applicationResult rebuildReport =
    EGraphSaturationChangeSummary
      { egscApplicationTraces = [egarTrace applicationResult],
        egscRebuildDeltas = [rrrRebuildDelta rebuildReport],
        egscProofRestrictionRegistryConstructions = egarProofRestrictionRegistryConstructions applicationResult,
        egscProofExtractionTableConstructions = egarProofExtractionTableConstructions applicationResult
      }

instance
  ( Language f,
    HasConstructorTag f,
    Show (f ()),
    Eq a,
    Ord a,
    JoinSemilattice a,
    Ord capability,
    Show capability,
    Ord c
  ) =>
  ProofCarrier (EGraphU owner capability f a c) p
  where
  proofGraphContext =
    pgGraph

  setProofGraphContext newGraph proofGraph =
    proofGraph
      { pgGraph = newGraph
      }

  applyProofMatches =
    engineApplyMatchesWithProofReported
