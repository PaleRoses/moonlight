{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Pure.Saturation.Matching
  ( SaturationPurpose,
    AnnotatedContextSource (..),
    MatchSite,
    MatchingStrategy (..),
    MatchingWorld,
    MatchingRequest,
    MatchingAdvanceCtx (..),
    EGraphMatchingObstruction (..),
    MatchingAlgebra,
    MatchingFrontier,
    MatchingDelta,
    MatchingDeltaPayload (..),
    MatchingProofContext,
    mkMatchingProofContext,
    matchingProofContextGraph,
    matchingProofReachability,
    matchingDeltaFromTouchedKeys,
    matchingDeltaFromMutationTrace,
    matchingDeltaFromContextMutationTrace,
    matchingDeltaFromContextMutationTraceWithAnnotatedFrontier,
    matchingDeltaFromRebuildTrace,
    matchingDeltaFromRebuild,
    matchingDeltaFromRebuildWithObstruction,
    matchingDeltaObstructionInvalidation,
    matchingFrontierFromDelta,
    annotatedDeltaFrontierKeys,
    FrontierRefreshPosture (..),
    defaultFrontierRefreshPosture,
    PreparedWcojMatchingState (..),
    emptyPreparedWcojMatchingState,
    setPreparedWcojFrontierRefreshPosture,
    advancePreparedWcojMatchingState,
    rootFilterMatchingAlgebra,
    wcojMatchingAlgebra,
    eGraphRelationalMatchObstruction,
    filterObstructedMatches,
  )
where

import Data.Kind (Type)
import Data.Foldable (fold)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core (Language, Pattern, RewriteRuleId)
import Moonlight.Delta.Scope qualified as Delta
import Moonlight.EGraph.Pure.Query.RootFilter (RootClassFilter (..))
import Moonlight.Rewrite.Algebra (CompiledPatternQuery)
import Moonlight.Rewrite.System (CompiledGuard, GuardCapabilityResolver)
import Moonlight.Rewrite.System (FactDerivationIndex, FactRuleId)
import Moonlight.Rewrite.System (FactStore)
import Moonlight.Flow.Execution.Direct qualified as RelRuntime
import Moonlight.Core (Substitution)
import Moonlight.EGraph.Pure.Context.AnnotatedDelta (AnnotatedDeltaBuckets, AnnotatedDeltaFrontier (..))
import Moonlight.EGraph.Pure.Context (ContextEGraph)
import Moonlight.EGraph.Pure.Change
  ( EGraphMutationTrace (..),
    EGraphRebuildTrace (..),
    ObservedClassUnions,
    observedClassUnionKeys,
  )
import Moonlight.EGraph.Pure.Context.Proof (ProofEGraph, ProofGraph (pgGraph), proofReachability)
import Moonlight.EGraph.Pure.Context
  ( ContextMutationTrace (..),
  )
import Moonlight.Sheaf.Context.Site
  ( ContextObjectKey,
    contextObjectKeyValue,
    PreparedContextSite,
    classSupportDeltaTouchedClassKeys,
    contextObjectKeyFor,
  )
import Moonlight.Rewrite.ProofContext (ProofQueryError, ProofReachability)
import Moonlight.EGraph.Pure.Relational
  ( EGraphRelationalMatchObstruction (..),
    EGraphPreparedMatchState,
    RegionalAssignmentObstruction,
    PatternAtomizeObstruction,
    PreparedQueryKey,
    compiledPatternQueryKey,
    emptyEGraphPreparedMatchState,
    markEGraphPreparedMatchStateDirty,
    markEGraphPreparedMatchStateAnnotatedDirty,
    refreshEGraphPreparedMatchStateAnnotatedRevisions,
    preparedPlanCacheSize,
    preparedPlanTemplate,
    wcojPreparedAnnotatedContextDeltaMatchCompiledWithRootFilter,
    PreparedBaseMatchMemo,
    emptyPreparedBaseMatchMemo,
    wcojPreparedSharedBaseDeltaMatchCompiledWithRootFilter,
  )
import Moonlight.EGraph.Pure.Rebuild (EGraphRebuildDelta (..))
import Moonlight.EGraph.Pure.Types (ClassId (..), EGraph, canonicalizeClassId, classIdKey)
import Moonlight.Saturation.Matching qualified as GenericMatching
import Moonlight.Saturation.Obstruction.Cohomological.LivePruning
  ( ObstructionInvalidation,
    obstructionInvalidationWithResultsFromKeys,
    widenScopeWithObstruction,
  )
import Numeric.Natural (Natural)

type SaturationPurpose :: Type
type SaturationPurpose = GenericMatching.SaturationPurpose RewriteRuleId FactRuleId

type MatchSite :: Type -> Type
type MatchSite = GenericMatching.MatchSite

type MatchingStrategy :: Type -> Type -> Type -> (Type -> Type) -> Type -> Type
data MatchingStrategy owner c capability f a where
  GenericJoinMatching :: (Language f, Show (f ()), Show capability, Ord c) => MatchingStrategy owner c capability f a
  GenericJoinPerContextMatching :: (Language f, Show (f ()), Show capability, Ord c) => MatchingStrategy owner c capability f a
  CustomMatchingAlgebra :: MatchingAlgebra state owner c capability f a -> MatchingStrategy owner c capability f a

instance Eq (MatchingStrategy owner c capability f a) where
  GenericJoinMatching == GenericJoinMatching =
    True
  GenericJoinPerContextMatching == GenericJoinPerContextMatching =
    True
  CustomMatchingAlgebra _ == CustomMatchingAlgebra _ =
    True
  _ == _ =
    False

type MatchingProofContext :: Type -> Type -> (Type -> Type) -> Type -> Type
data MatchingProofContext owner c f a where
  MatchingProofContext :: ProofEGraph owner f a c p -> ProofReachability -> MatchingProofContext owner c f a

mkMatchingProofContext :: ProofEGraph owner f a c p -> Either ProofQueryError (MatchingProofContext owner c f a)
mkMatchingProofContext proofEGraph =
  MatchingProofContext proofEGraph <$> proofReachability proofEGraph

matchingProofContextGraph :: MatchingProofContext owner c f a -> ContextEGraph owner f a c
matchingProofContextGraph (MatchingProofContext proofEGraph _) =
  pgGraph proofEGraph

matchingProofReachability :: MatchingProofContext owner c f a -> ProofReachability
matchingProofReachability (MatchingProofContext _ proofReachabilityValue) =
  proofReachabilityValue

type MatchingWorld :: Type -> Type -> Type -> (Type -> Type) -> Type -> Type
type MatchingWorld owner c capability f a =
  GenericMatching.MatchWorld
    (EGraph f a)
    FactStore
    FactDerivationIndex
    (GuardCapabilityResolver capability)
    (Maybe (MatchingProofContext owner c f a))

-- | A context-site request's matching source: the round's annotated delta
-- buckets plus the context object key they are read at. Base-site requests
-- carry no source and match the shared base directly.
type AnnotatedContextSource :: Type -> (Type -> Type) -> Type
data AnnotatedContextSource owner f = AnnotatedContextSource
  { acsBuckets :: !(AnnotatedDeltaBuckets owner f),
    acsContextKey :: !(ContextObjectKey owner),
    acsContextRevision :: !Natural
  }

type MatchingRequest :: Type -> Type -> Type -> (Type -> Type) -> Type -> Type
type MatchingRequest owner c capability f =
  GenericMatching.QueryRequest
    (MatchSite c)
    (Maybe (AnnotatedContextSource owner f))
    (CompiledPatternQuery (CompiledGuard capability f) f)
    SaturationPurpose

type MatchingAdvanceCtx :: Type -> Type -> (Type -> Type) -> Type -> Type
data MatchingAdvanceCtx owner c f a = MatchingAdvanceCtx
  { macGraph :: !(EGraph f a),
    macCanonicalize :: !(ClassId -> ClassId),
    macContextSite :: !(Maybe (PreparedContextSite owner c)),
    macContextRevision :: !(Maybe Natural)
  }

type MatchingDeltaPayload :: Type
data MatchingDeltaPayload = MatchingDeltaPayload
  { mdpObstructionInvalidation :: !(Maybe (ObstructionInvalidation ClassId)),
    mdpContextDirtyKeys :: !(IntMap IntSet)
  }
  deriving stock (Eq, Show)

instance Semigroup MatchingDeltaPayload where
  left <> right =
    MatchingDeltaPayload
      { mdpObstructionInvalidation =
          mdpObstructionInvalidation left <> mdpObstructionInvalidation right,
        mdpContextDirtyKeys =
          IntMap.unionWith IntSet.union (mdpContextDirtyKeys left) (mdpContextDirtyKeys right)
      }

type MatchingDelta :: Type
type MatchingDelta = Delta.Scoped IntSet MatchingDeltaPayload

matchingDeltaPayload ::
  Maybe (ObstructionInvalidation ClassId) ->
  IntMap IntSet ->
  Maybe MatchingDeltaPayload
matchingDeltaPayload obstructionInvalidation contextDirtyKeys =
  let nonEmptyContextDirtyKeys =
        IntMap.filter (not . IntSet.null) contextDirtyKeys
   in case (obstructionInvalidation, IntMap.null nonEmptyContextDirtyKeys) of
        (Nothing, True) ->
          Nothing
        _ ->
          Just
            MatchingDeltaPayload
              { mdpObstructionInvalidation = obstructionInvalidation,
                mdpContextDirtyKeys = nonEmptyContextDirtyKeys
              }
{-# INLINE matchingDeltaPayload #-}

matchingDeltaFromTouchedKeys :: IntSet -> MatchingDelta
matchingDeltaFromTouchedKeys touchedKeys
  | IntSet.null touchedKeys =
      mempty
  | otherwise =
      Delta.scopedDelta
        (Delta.dirtyScope touchedKeys)
        ( matchingDeltaPayload
            ( Just
                ( obstructionInvalidationWithResultsFromKeys
                    ClassId
                    touchedKeys
                    IntSet.empty
                    touchedKeys
                    touchedKeys
                )
            )
            IntMap.empty
        )

matchingDeltaFromClassUnionKeys :: IntSet -> MatchingDelta
matchingDeltaFromClassUnionKeys impactedKeys
  | IntSet.null impactedKeys =
      mempty
  | otherwise =
      Delta.scopedDelta
        (Delta.dirtyScope impactedKeys)
        ( matchingDeltaPayload
            ( Just
                ( obstructionInvalidationWithResultsFromKeys
                    ClassId
                    impactedKeys
                    IntSet.empty
                    IntSet.empty
                    impactedKeys
                )
            )
            IntMap.empty
        )

matchingDeltaFromObservedClassUnions :: ObservedClassUnions -> MatchingDelta
matchingDeltaFromObservedClassUnions observedUnions =
  matchingDeltaFromClassUnionKeys (observedClassUnionKeys observedUnions)
{-# INLINE matchingDeltaFromObservedClassUnions #-}

matchingDeltaFromMutationTrace ::
  EGraphMutationTrace f ->
  MatchingDelta
matchingDeltaFromMutationTrace traceValue =
  matchingDeltaFromTouchedKeys (emtTouchedClassKeys traceValue)
    <> matchingDeltaFromObservedClassUnions (emtObservedClassUnions traceValue)
    <> foldMap matchingDeltaFromRebuildTrace (emtRebuildTraces traceValue)
{-# INLINE matchingDeltaFromMutationTrace #-}

matchingDeltaFromContextMutationTrace ::
  ContextMutationTrace owner c f ->
  MatchingDelta
matchingDeltaFromContextMutationTrace traceValue =
  matchingDeltaFromMutationTrace (cmtBaseTrace traceValue)
    <> matchingDeltaFromTouchedKeys (cmtContextTouchedKeys traceValue)
    <> matchingDeltaFromTouchedKeys (classSupportDeltaTouchedClassKeys (cmtSupportDelta traceValue))
    <> matchingDeltaFromObservedClassUnions (cmtObservedLocalUnions traceValue)
{-# INLINE matchingDeltaFromContextMutationTrace #-}

matchingDeltaFromContextMutationTraceWithAnnotatedFrontier ::
  ContextMutationTrace owner c f ->
  IntMap (AnnotatedDeltaFrontier f) ->
  MatchingDelta
matchingDeltaFromContextMutationTraceWithAnnotatedFrontier traceValue frontierByContextKey =
  matchingDeltaFromMutationTrace (cmtBaseTrace traceValue)
    <> if IntSet.null supportDirtyKeys
      then
        matchingDeltaFromContextDirtyKeys
          (IntMap.map annotatedDeltaFrontierKeys frontierByContextKey)
          (matchingDeltaObstructionInvalidation uniformContextDelta)
      else uniformContextDelta
  where
    supportDirtyKeys =
      classSupportDeltaTouchedClassKeys (cmtSupportDelta traceValue)

    uniformContextDelta =
      matchingDeltaFromTouchedKeys (cmtContextTouchedKeys traceValue)
        <> matchingDeltaFromTouchedKeys supportDirtyKeys
        <> matchingDeltaFromObservedClassUnions (cmtObservedLocalUnions traceValue)
{-# INLINE matchingDeltaFromContextMutationTraceWithAnnotatedFrontier #-}

matchingDeltaFromContextDirtyKeys ::
  IntMap IntSet ->
  Maybe (ObstructionInvalidation ClassId) ->
  MatchingDelta
matchingDeltaFromContextDirtyKeys contextDirtyKeys obstructionInvalidation =
  Delta.scopedDelta
    Delta.cleanScope
    (matchingDeltaPayload obstructionInvalidation contextDirtyKeys)
{-# INLINE matchingDeltaFromContextDirtyKeys #-}

annotatedDeltaFrontierKeys :: AnnotatedDeltaFrontier f -> IntSet
annotatedDeltaFrontierKeys frontier =
  IntSet.unions
    [ adfRepresentativeKeys frontier,
      annotatedDeltaFrontierRowKeys (adfVariantRowsByTag frontier),
      annotatedDeltaFrontierRowKeys (adfAbsorbedRowsByTag frontier)
    ]
{-# INLINE annotatedDeltaFrontierKeys #-}

annotatedDeltaFrontierRowKeys :: Map (f ()) (Set (Int, [Int])) -> IntSet
annotatedDeltaFrontierRowKeys =
  IntSet.fromList
    . foldMap
      ( foldMap
          ( \(rootKey, childKeys) ->
              rootKey : childKeys
          )
      )
{-# INLINE annotatedDeltaFrontierRowKeys #-}

matchingDeltaFromRebuildTrace ::
  EGraphRebuildTrace f ->
  MatchingDelta
matchingDeltaFromRebuildTrace =
  matchingDeltaFromRebuild . egrtRebuildDelta
{-# INLINE matchingDeltaFromRebuildTrace #-}

matchingDeltaFromRebuild :: EGraphRebuildDelta -> MatchingDelta
matchingDeltaFromRebuild rebuildDelta =
  matchingDeltaFromRebuildWithObstruction rebuildDelta Nothing

matchingDeltaFromRebuildWithObstruction ::
  EGraphRebuildDelta ->
  Maybe (ObstructionInvalidation ClassId) ->
  MatchingDelta
matchingDeltaFromRebuildWithObstruction rebuildDelta obstructionInvalidation =
  Delta.scopedDelta
    ( Delta.dirtyScope
        ( IntSet.union
            (erdImpactedClassKeys rebuildDelta)
            (erdDirtyResultKeys rebuildDelta)
        )
    )
    (matchingDeltaPayload obstructionInvalidation IntMap.empty)

matchingDeltaObstructionInvalidation :: MatchingDelta -> Maybe (ObstructionInvalidation ClassId)
matchingDeltaObstructionInvalidation matchingDelta =
  case Delta.scopedDeltaPayload matchingDelta of
    Just payload ->
      mdpObstructionInvalidation payload
    Nothing ->
      Nothing

matchingDeltaContextDirtyKeys :: MatchingDelta -> IntMap IntSet
matchingDeltaContextDirtyKeys matchingDelta =
  case Delta.scopedDeltaPayload matchingDelta of
    Just payload ->
      mdpContextDirtyKeys payload
    Nothing ->
      IntMap.empty
{-# INLINE matchingDeltaContextDirtyKeys #-}

type MatchingFrontier :: Type
type MatchingFrontier = Delta.Scope IntSet

type EGraphMatchingObstruction :: Type
data EGraphMatchingObstruction
  = EGraphMatchingPatternAtomizeObstruction !PatternAtomizeObstruction
  | EGraphMatchingRuntimeQueryObstruction !RelRuntime.RuntimeQueryPlanObstruction
  | EGraphMatchingDirtySnapshot
  | EGraphMatchingRegionalAssignmentObstruction !RegionalAssignmentObstruction
  | EGraphMatchingHierarchicalPruningWithoutSeedFrontier
  deriving stock (Eq, Show)

matchingFrontierFromDelta :: MatchingDelta -> MatchingFrontier
matchingFrontierFromDelta matchingDelta =
  Delta.unionScope
    ( widenScopeWithObstruction
        classIdKey
        (matchingDeltaObstructionInvalidation matchingDelta)
        (Delta.scopedDeltaSupport matchingDelta)
    )
    (Delta.dirtyScope (fold (matchingDeltaContextDirtyKeys matchingDelta)))

type MatchingAlgebra :: Type -> Type -> Type -> Type -> (Type -> Type) -> Type -> Type
type MatchingAlgebra state owner c capability f a =
  GenericMatching.MatchingAlgebra
    (GuardCapabilityResolver capability)
    state
    IntSet
    MatchingDeltaPayload
    (MatchingWorld owner c capability f a)
    (MatchingRequest owner c capability f)
    (MatchingAdvanceCtx owner c f)
    EGraphMatchingObstruction
    (ClassId, Substitution)

rootFilterMatchingAlgebra ::
  GuardCapabilityResolver capability ->
  (RootClassFilter -> CompiledPatternQuery (CompiledGuard capability f) f -> EGraph f a -> Either EGraphMatchingObstruction [(ClassId, Substitution)]) ->
  MatchingAlgebra () owner c capability f a
rootFilterMatchingAlgebra capabilityResolver runWithRootFilter =
  GenericMatching.MatchingAlgebra
    { GenericMatching.maInitialState = (),
      GenericMatching.maEnvironment = capabilityResolver,
      GenericMatching.maRunQueries =
        \state world preparedQueries ->
          let matchesByRequest =
                fmap
                  ( \preparedQuery ->
                      let matchingFrontier =
                            GenericMatching.mqScope preparedQuery
                          request =
                            GenericMatching.mqRequest preparedQuery
                       in runWithRootFilter
                            (matchingFrontierRootClassFilter matchingFrontier)
                            (GenericMatching.qrQuery request)
                            (GenericMatching.mwGraph world)
                  )
                  preparedQueries
           in (state, sequenceA matchesByRequest),
      GenericMatching.maPrepareQueries =
        \state matchingDelta _world requests ->
          ( state,
            fmap
              (GenericMatching.MatchingQuery (matchingFrontierFromDelta matchingDelta))
              requests
          ),
      GenericMatching.maPreviewQuery = \_ _ _ -> Nothing,
      GenericMatching.maAdvanceState = \_ _ state -> state,
      GenericMatching.maReplayDiagnostics = const Nothing
    }

wcojMatchingAlgebra ::
  (Language f, Show (f ()), Show capability, Ord c) =>
  GuardCapabilityResolver capability ->
  MatchingAlgebra (PreparedWcojMatchingState owner c capability f) owner c capability f a
wcojMatchingAlgebra capabilityResolver =
  GenericMatching.MatchingAlgebra
    { GenericMatching.maInitialState = emptyPreparedWcojMatchingState,
      GenericMatching.maEnvironment = capabilityResolver,
      GenericMatching.maRunQueries =
        \state world preparedQueries ->
          runPreparedWcojQueries state world preparedQueries,
      GenericMatching.maPrepareQueries =
        \state matchingDelta _world requests ->
          ( state,
            fmap
              (GenericMatching.MatchingQuery (matchingFrontierFromDelta matchingDelta))
              requests
          ),
      GenericMatching.maPreviewQuery = \_ _ _ -> Nothing,
      GenericMatching.maAdvanceState =
        \matchingDelta advanceCtx ->
          advancePreparedWcojMatchingState
            matchingDelta
            advanceCtx,
      GenericMatching.maReplayDiagnostics = const Nothing
    }

-- | Controls snapshot-revision refresh outside context-local dirty frontiers.
type FrontierRefreshPosture :: Type
data FrontierRefreshPosture
  = SkipUntouchedContextSnapshotRefresh
  | RefreshUntouchedContextSnapshots
  deriving stock (Eq, Ord, Show, Read)

defaultFrontierRefreshPosture :: FrontierRefreshPosture
defaultFrontierRefreshPosture =
  RefreshUntouchedContextSnapshots

type PreparedWcojMatchingState :: Type -> Type -> Type -> (Type -> Type) -> Type
data PreparedWcojMatchingState owner c capability f = PreparedWcojMatchingState
  { pwmsSites :: !(Map (MatchSite c) (EGraphPreparedMatchState owner capability f)),
    pwmsPlanTemplate :: !(EGraphPreparedMatchState owner capability f),
    pwmsBaseMemo :: !(PreparedBaseMatchMemo f),
    pwmsFrontierRefreshPosture :: !FrontierRefreshPosture
  }

emptyPreparedWcojMatchingState :: PreparedWcojMatchingState owner c capability f
emptyPreparedWcojMatchingState =
  PreparedWcojMatchingState
    { pwmsSites = Map.empty,
      pwmsPlanTemplate = emptyEGraphPreparedMatchState,
      pwmsBaseMemo = emptyPreparedBaseMatchMemo,
      pwmsFrontierRefreshPosture = defaultFrontierRefreshPosture
    }

setPreparedWcojFrontierRefreshPosture ::
  FrontierRefreshPosture ->
  PreparedWcojMatchingState owner c capability f ->
  PreparedWcojMatchingState owner c capability f
setPreparedWcojFrontierRefreshPosture posture state =
  state {pwmsFrontierRefreshPosture = posture}
{-# INLINE setPreparedWcojFrontierRefreshPosture #-}

data PreparedWcojQueryBatchKey owner c f = PreparedWcojQueryBatchKey
  { pwqbkSite :: !(MatchSite c),
    pwqbkContextKey :: !(Maybe (ContextObjectKey owner)),
    pwqbkRootFilter :: !(Maybe IntSet),
    pwqbkQuery :: !(PreparedQueryKey f)
  }

deriving stock instance (Eq c, Eq (Pattern f)) => Eq (PreparedWcojQueryBatchKey owner c f)

deriving stock instance (Ord c, Ord (Pattern f)) => Ord (PreparedWcojQueryBatchKey owner c f)

data PreparedWcojQueryBatch owner c capability f = PreparedWcojQueryBatch
  { pwqbState :: !(PreparedWcojMatchingState owner c capability f),
    pwqbCache :: !(Map (PreparedWcojQueryBatchKey owner c f) (Either EGraphMatchingObstruction [(ClassId, Substitution)])),
    pwqbResults :: ![Either EGraphMatchingObstruction [(ClassId, Substitution)]]
  }

runPreparedWcojQueries ::
  (Language f, Show (f ()), Show capability, Ord c) =>
  PreparedWcojMatchingState owner c capability f ->
  MatchingWorld owner c capability f a ->
  [GenericMatching.MatchingQuery IntSet (MatchingRequest owner c capability f) host] ->
  (PreparedWcojMatchingState owner c capability f, Either EGraphMatchingObstruction [[(ClassId, Substitution)]])
runPreparedWcojQueries state world preparedQueries =
  let batch =
        foldl'
          (runPreparedWcojQueryBatch world)
          PreparedWcojQueryBatch
            { pwqbState = state,
              pwqbCache = Map.empty,
              pwqbResults = []
            }
          preparedQueries
   in (pwqbState batch, sequenceA (reverse (pwqbResults batch)))

runPreparedWcojQueryBatch ::
  (Language f, Show (f ()), Show capability, Ord c) =>
  MatchingWorld owner c capability f a ->
  PreparedWcojQueryBatch owner c capability f ->
  GenericMatching.MatchingQuery IntSet (MatchingRequest owner c capability f) host ->
  PreparedWcojQueryBatch owner c capability f
runPreparedWcojQueryBatch world batch preparedQuery =
  let batchKey =
        preparedWcojQueryBatchKey preparedQuery
   in case Map.lookup batchKey (pwqbCache batch) of
        Just cachedResult ->
          batch {pwqbResults = cachedResult : pwqbResults batch}
        Nothing ->
          let (nextState, result) =
                runPreparedWcojQuery (pwqbState batch) world preparedQuery
           in batch
                { pwqbState = nextState,
                  pwqbCache = Map.insert batchKey result (pwqbCache batch),
                  pwqbResults = result : pwqbResults batch
                }

-- | The batch cache is load-bearing, not merely an optimization: the prepared
-- state protocol consumes a query key's pending delta on first use, so every
-- request sharing that key within one batch MUST be served the first run's
-- result. The batch key therefore reuses the protocol's own idempotence key —
-- the query's pattern list — never a per-rule identity.
preparedWcojQueryBatchKey ::
  GenericMatching.MatchingQuery IntSet (MatchingRequest owner c capability f) host ->
  PreparedWcojQueryBatchKey owner c f
preparedWcojQueryBatchKey preparedQuery =
  let request =
        GenericMatching.mqRequest preparedQuery
   in PreparedWcojQueryBatchKey
        { pwqbkSite = GenericMatching.qrSite request,
          pwqbkContextKey = fmap acsContextKey (GenericMatching.qrSnapshot request),
          pwqbkRootFilter =
            rootClassFilterBatchKey
              (matchingFrontierRootClassFilter (GenericMatching.mqScope preparedQuery)),
          pwqbkQuery = compiledPatternQueryKey (GenericMatching.qrQuery request)
        }

rootClassFilterBatchKey :: RootClassFilter -> Maybe IntSet
rootClassFilterBatchKey =
  \case
    AllRootClasses ->
      Nothing
    RestrictedRootClasses rootKeys ->
      Just rootKeys

runPreparedWcojQuery ::
  (Language f, Show (f ()), Show capability, Ord c) =>
  PreparedWcojMatchingState owner c capability f ->
  MatchingWorld owner c capability f a ->
  GenericMatching.MatchingQuery IntSet (MatchingRequest owner c capability f) host ->
  (PreparedWcojMatchingState owner c capability f, Either EGraphMatchingObstruction [(ClassId, Substitution)])
runPreparedWcojQuery state world preparedQuery =
  let request =
        GenericMatching.mqRequest preparedQuery
      requestSite =
        GenericMatching.qrSite request
      siteState =
        Map.findWithDefault
          (pwmsPlanTemplate state)
          requestSite
          (pwmsSites state)
      rootFilter =
        matchingFrontierRootClassFilter (GenericMatching.mqScope preparedQuery)
      matchResult =
        case GenericMatching.qrSnapshot request of
          Nothing -> do
            (memo, nextSiteState, matches) <-
              wcojPreparedSharedBaseDeltaMatchCompiledWithRootFilter
                rootFilter
                (GenericMatching.qrQuery request)
                (GenericMatching.mwGraph world)
                (pwmsBaseMemo state)
                siteState
            pure (memo, nextSiteState, matches)
          Just annotatedSource -> do
            (memo, baseSiteState, baseMatches) <-
              wcojPreparedSharedBaseDeltaMatchCompiledWithRootFilter
                rootFilter
                (GenericMatching.qrQuery request)
                (GenericMatching.mwGraph world)
                (pwmsBaseMemo state)
                siteState
            (variantSiteState, variantMatches) <-
              wcojPreparedAnnotatedContextDeltaMatchCompiledWithRootFilter
                rootFilter
                (acsBuckets annotatedSource)
                (acsContextKey annotatedSource)
                (acsContextRevision annotatedSource)
                (GenericMatching.qrQuery request)
                (GenericMatching.mwGraph world)
                baseSiteState
            pure
              ( memo,
                variantSiteState,
                Set.toAscList (Set.fromList (baseMatches <> variantMatches))
              )
   in case matchResult of
        Left obstruction ->
          (state, Left (eGraphRelationalMatchObstruction obstruction))
        Right (nextMemo, nextSiteState, matches) ->
          ( state
              { pwmsBaseMemo = nextMemo,
                pwmsSites =
                  Map.insert requestSite nextSiteState (pwmsSites state),
                pwmsPlanTemplate =
                  if preparedPlanCacheSize nextSiteState > preparedPlanCacheSize (pwmsPlanTemplate state)
                    then preparedPlanTemplate nextSiteState
                    else pwmsPlanTemplate state
              },
            Right matches
          )

advancePreparedWcojMatchingState ::
  (Language f, Ord c) =>
  MatchingDelta ->
  MatchingAdvanceCtx owner c f a ->
  PreparedWcojMatchingState owner c capability f ->
  PreparedWcojMatchingState owner c capability f
advancePreparedWcojMatchingState matchingDelta advanceCtx state =
  Delta.foldScope
    refreshedCleanState
    ( \dirtyKeys ->
        let canonicalDirtyKeys =
              IntSet.union
                dirtyKeys
                (IntSet.map (classIdKey . macCanonicalize advanceCtx . ClassId) dirtyKeys)
         in contextAdvancedState
              { pwmsSites =
                  fmap
                    (markEGraphPreparedMatchStateDirty canonicalDirtyKeys)
                    (pwmsSites contextAdvancedState),
                pwmsBaseMemo = emptyPreparedBaseMatchMemo
              }
    )
    resetState
    (Delta.scopedDeltaSupport matchingDelta)
  where
    refreshedCleanState =
      case pwmsFrontierRefreshPosture state of
        SkipUntouchedContextSnapshotRefresh ->
          contextAdvancedState
        RefreshUntouchedContextSnapshots ->
          case macContextRevision advanceCtx of
            Nothing ->
              contextAdvancedState
            Just contextRevision ->
              contextAdvancedState
                { pwmsSites =
                    Map.mapWithKey
                      (refreshUntouchedContextSite contextRevision)
                      (pwmsSites contextAdvancedState)
                }

    refreshUntouchedContextSite contextRevision matchSite siteState =
      case matchSite of
        GenericMatching.BaseSite ->
          siteState
        GenericMatching.ContextSite contextValue ->
          case contextKeyFor contextValue of
            Just contextKey
              | IntSet.null (IntMap.findWithDefault IntSet.empty contextKey (matchingDeltaContextDirtyKeys matchingDelta)) ->
                  refreshEGraphPreparedMatchStateAnnotatedRevisions contextRevision siteState
            _ ->
              siteState

    contextAdvancedState =
      state
        { pwmsSites =
            Map.mapWithKey
              markContextSiteDirty
              (pwmsSites state)
        }

    markContextSiteDirty matchSite siteState =
      case matchSite of
        GenericMatching.BaseSite ->
          siteState
        GenericMatching.ContextSite contextValue ->
          maybe
            siteState
            ( \contextKey ->
                markEGraphPreparedMatchStateAnnotatedDirty
                  (canonicalDirtyKeysAt contextKey)
                  siteState
            )
            (contextKeyFor contextValue)

    contextKeyFor contextValue = do
      contextSite <- macContextSite advanceCtx
      case contextObjectKeyFor contextSite contextValue of
        Right contextKey ->
          Just (contextObjectKeyValue contextKey)
        Left _ ->
          Nothing

    canonicalDirtyKeysAt contextKey =
      let dirtyKeys =
            IntMap.findWithDefault IntSet.empty contextKey (matchingDeltaContextDirtyKeys matchingDelta)
       in IntSet.union
            dirtyKeys
            (IntSet.map (classIdKey . macCanonicalize advanceCtx . ClassId) dirtyKeys)

    resetState =
      state
        { pwmsSites =
            fmap (const (pwmsPlanTemplate state)) (pwmsSites state),
          pwmsBaseMemo = emptyPreparedBaseMatchMemo
        }

filterObstructedMatches ::
  MatchingWorld owner c capability f a ->
  Set ClassId ->
  [(ClassId, Substitution)] ->
  [(ClassId, Substitution)]
filterObstructedMatches world obstructedRootSet =
  if Set.null obstructedRootSet
    then id
    else
      filter
        ( \(rootClass, _) ->
            canonicalizeClassId (GenericMatching.mwGraph world) rootClass `Set.notMember` obstructedRootSet
        )

matchingFrontierRootClassFilter :: MatchingFrontier -> RootClassFilter
matchingFrontierRootClassFilter matchingFrontier =
  Delta.foldScope
    (RestrictedRootClasses IntSet.empty)
    RestrictedRootClasses
    AllRootClasses
    matchingFrontier

eGraphRelationalMatchObstruction :: EGraphRelationalMatchObstruction -> EGraphMatchingObstruction
eGraphRelationalMatchObstruction obstruction =
  case obstruction of
    EGraphRelationalAtomizeObstruction atomizeObstruction ->
      EGraphMatchingPatternAtomizeObstruction atomizeObstruction
    EGraphRelationalRuntimeQueryObstruction runtimeObstruction ->
      EGraphMatchingRuntimeQueryObstruction runtimeObstruction
    EGraphRelationalDirtySnapshot ->
      EGraphMatchingDirtySnapshot
    EGraphRelationalRegionalAssignmentObstruction regionalObstruction ->
      EGraphMatchingRegionalAssignmentObstruction regionalObstruction
