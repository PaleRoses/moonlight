{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Saturation.Obstruction.Cohomological.LivePruning
  ( ObstructionDelta,
    ObstructionFootprint (..),
    ObstructionInvalidation (..),
    SeedCriticality (..),
    RegionAdmissibility (..),
    MaterializedSeed (..),
    RequestPruningState,
    LivePruningState (..),
    LivePruningAdapter (..),
    emptyObstructionFootprint,
    emptyObstructionInvalidation,
    obstructionFootprintSupportKeys,
    obstructionInvalidationFromFootprint,
    obstructionInvalidationFromKeys,
    obstructionInvalidationWithResultsFromKeys,
    obstructionInvalidationSupportKeys,
    obstructionInvalidationRootKeys,
    obstructionInvalidationRootInvalidation,
    affectedRootsForObstructionDelta,
    widenScopeWithObstruction,
    materializeSeedWithGates,
    materializeSeedsWithGates,
    admissibleMaterializedRegions,
    emptyLivePruningState,
    refreshRequestPruningState,
    refreshLivePruningState,
    canonicalizeLivePruningState,
    lookupRequestPruningState,
    requestFallbackRoots,
    requestFallbackRootKeys,
    requestExcludedRoots,
    requestExactResolvedRoots,
    livePruningMatchingAlgebra,
  )
where

import Data.Foldable (toList)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Delta.Scope qualified as Delta
import Moonlight.Saturation.Matching
  ( MatchingAlgebra (..),
    MatchingQuery (..),
    mapMatchingQueryScope,
  )
import Moonlight.Saturation.Obstruction.Cohomological.Aggregate
  ( RequestAggregateSummary (..),
    RootInvalidation (..),
    RootResolution,
    affectedRootsForDelta,
    mergeRequestAggregateSummaries,
    mergeRootResolution,
    requestAggregateSupportRoots,
    rootResolutionExactResolved,
    rootResolutionExcludesFallback,
    rootsSupportedByKeys,
    rootsMatchingResolution,
  )
import Moonlight.Saturation.Obstruction.Cohomological.Prepared
  ( PreparedRequestCacheKey,
  )
import Moonlight.Saturation.Obstruction.Cohomological.Seed
  ( SeedInterpreter (..),
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Evidence.Pruning
  ( CohomologicalPruningGates (..),
    CohomologicalPruningObstruction,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Types
  ( CandidateRegion,
    CandidateRegionSeed,
  )
import Moonlight.Sheaf.Pruning
  ( PruningCertificate (pcObstructions),
    PruningDecision (..),
  )

type ObstructionDelta :: Type -> Type
type ObstructionDelta root =
  Delta.Scoped IntSet (ObstructionInvalidation root)

type ObstructionFootprint :: Type -> Type
data ObstructionFootprint root = ObstructionFootprint
  { ofDependencyKeys :: !IntSet,
    ofTopologyKeys :: !IntSet,
    ofResultKeys :: !IntSet,
    ofRoots :: !(Set root)
  }
  deriving stock (Eq, Ord, Show, Read)

instance Ord root => Semigroup (ObstructionFootprint root) where
  left <> right =
    ObstructionFootprint
      { ofDependencyKeys =
          IntSet.union (ofDependencyKeys left) (ofDependencyKeys right),
        ofTopologyKeys =
          IntSet.union (ofTopologyKeys left) (ofTopologyKeys right),
        ofResultKeys =
          IntSet.union (ofResultKeys left) (ofResultKeys right),
        ofRoots =
          Set.union (ofRoots left) (ofRoots right)
      }

instance Ord root => Monoid (ObstructionFootprint root) where
  mempty =
    emptyObstructionFootprint

emptyObstructionFootprint :: ObstructionFootprint root
emptyObstructionFootprint =
  ObstructionFootprint
    { ofDependencyKeys = IntSet.empty,
      ofTopologyKeys = IntSet.empty,
      ofResultKeys = IntSet.empty,
      ofRoots = Set.empty
    }

obstructionFootprintSupportKeys :: ObstructionFootprint root -> IntSet
obstructionFootprintSupportKeys footprint =
  IntSet.unions
    [ ofDependencyKeys footprint,
      ofTopologyKeys footprint,
      ofResultKeys footprint
    ]
{-# INLINE obstructionFootprintSupportKeys #-}

type ObstructionInvalidation :: Type -> Type
data ObstructionInvalidation root = ObstructionInvalidation
  { oiDirtyDependencyKeys :: !IntSet,
    oiDirtyTopologyKeys :: !IntSet,
    oiDirtyResultKeys :: !IntSet,
    oiImpactedRoots :: !(Set root),
    oiInvalidateAllRoots :: !Bool
  }
  deriving stock (Eq, Ord, Show, Read)

instance Ord root => Semigroup (ObstructionInvalidation root) where
  left <> right =
    ObstructionInvalidation
      { oiDirtyDependencyKeys =
          IntSet.union (oiDirtyDependencyKeys left) (oiDirtyDependencyKeys right),
        oiDirtyTopologyKeys =
          IntSet.union (oiDirtyTopologyKeys left) (oiDirtyTopologyKeys right),
        oiDirtyResultKeys =
          IntSet.union (oiDirtyResultKeys left) (oiDirtyResultKeys right),
        oiImpactedRoots =
          Set.union (oiImpactedRoots left) (oiImpactedRoots right),
        oiInvalidateAllRoots =
          oiInvalidateAllRoots left || oiInvalidateAllRoots right
      }

instance Ord root => Monoid (ObstructionInvalidation root) where
  mempty =
    emptyObstructionInvalidation

emptyObstructionInvalidation :: ObstructionInvalidation root
emptyObstructionInvalidation =
  ObstructionInvalidation
    { oiDirtyDependencyKeys = IntSet.empty,
      oiDirtyTopologyKeys = IntSet.empty,
      oiDirtyResultKeys = IntSet.empty,
      oiImpactedRoots = Set.empty,
      oiInvalidateAllRoots = False
    }

obstructionInvalidationFromKeys ::
  Ord root =>
  (Int -> root) ->
  IntSet ->
  IntSet ->
  IntSet ->
  ObstructionInvalidation root
obstructionInvalidationFromKeys rootFromKey dirtyDependencies dirtyTopology impactedRootKeys =
  obstructionInvalidationWithResultsFromKeys
    rootFromKey
    dirtyDependencies
    dirtyTopology
    IntSet.empty
    impactedRootKeys

obstructionInvalidationFromFootprint ::
  ObstructionFootprint root ->
  ObstructionInvalidation root
obstructionInvalidationFromFootprint footprint =
  ObstructionInvalidation
    { oiDirtyDependencyKeys = ofDependencyKeys footprint,
      oiDirtyTopologyKeys = ofTopologyKeys footprint,
      oiDirtyResultKeys = ofResultKeys footprint,
      oiImpactedRoots = ofRoots footprint,
      oiInvalidateAllRoots = False
    }

obstructionInvalidationWithResultsFromKeys ::
  Ord root =>
  (Int -> root) ->
  IntSet ->
  IntSet ->
  IntSet ->
  IntSet ->
  ObstructionInvalidation root
obstructionInvalidationWithResultsFromKeys rootFromKey dirtyDependencies dirtyTopology dirtyResults impactedRootKeys =
  ObstructionInvalidation
    { oiDirtyDependencyKeys = dirtyDependencies,
      oiDirtyTopologyKeys = dirtyTopology,
      oiDirtyResultKeys = dirtyResults,
      oiImpactedRoots =
        Set.fromList (rootFromKey <$> IntSet.toList impactedRootKeys),
      oiInvalidateAllRoots = False
    }

obstructionInvalidationSupportKeys :: ObstructionInvalidation root -> IntSet
obstructionInvalidationSupportKeys invalidation =
  IntSet.unions
    [ oiDirtyDependencyKeys invalidation,
      oiDirtyTopologyKeys invalidation,
      oiDirtyResultKeys invalidation
    ]
{-# INLINE obstructionInvalidationSupportKeys #-}

obstructionInvalidationRootKeys ::
  (root -> Int) ->
  ObstructionInvalidation root ->
  IntSet
obstructionInvalidationRootKeys rootKey invalidation =
  rootKeySet rootKey (oiImpactedRoots invalidation)
{-# INLINE obstructionInvalidationRootKeys #-}

obstructionInvalidationRootInvalidation ::
  Ord root =>
  ObstructionInvalidation root ->
  RequestAggregateSummary root witness coverage ->
  RootInvalidation root
obstructionInvalidationRootInvalidation invalidation aggregateSummary
  | oiInvalidateAllRoots invalidation =
      RootInvalidationAll
  | otherwise =
      RootInvalidationSome
        ( Set.union
            (oiImpactedRoots invalidation)
            (rootsSupportedByKeys (obstructionInvalidationSupportKeys invalidation) aggregateSummary)
        )
{-# INLINE obstructionInvalidationRootInvalidation #-}

affectedRootsForObstructionDelta ::
  Ord root =>
  ObstructionDelta root ->
  RequestAggregateSummary root witness coverage ->
  Set root
affectedRootsForObstructionDelta =
  affectedRootsForDelta obstructionInvalidationRootInvalidation
{-# INLINE affectedRootsForObstructionDelta #-}

widenScopeWithObstruction ::
  (root -> Int) ->
  Maybe (ObstructionInvalidation root) ->
  Delta.Scope IntSet ->
  Delta.Scope IntSet
widenScopeWithObstruction rootKey maybeInvalidation scope =
  let impactedRootKeys =
        maybe
          IntSet.empty
          (obstructionInvalidationRootKeys rootKey)
          maybeInvalidation
   in Delta.foldScope
        (scopeFromRootKeys impactedRootKeys)
        (\dirtyKeys -> scopeFromRootKeys (IntSet.union dirtyKeys impactedRootKeys))
        Delta.fullScope
        scope
{-# INLINE widenScopeWithObstruction #-}

type SeedCriticality :: Type
data SeedCriticality
  = SeedCritical
  | SeedNonCritical
  | SeedUnknown
  deriving stock (Eq, Ord, Show, Read)

type RegionAdmissibility :: Type
data RegionAdmissibility
  = RegionAdmissible
  | RegionRejected !(NonEmpty CohomologicalPruningObstruction)
  | RegionNotMaterialized
  deriving stock (Eq, Ord, Show)

type MaterializedSeed :: Type -> Type
data MaterializedSeed root = MaterializedSeed
  { msSeed :: !(CandidateRegionSeed root),
    msCriticality :: !SeedCriticality,
    msRegion :: !(Maybe (CandidateRegion root)),
    msRegionAdmissibility :: !RegionAdmissibility
  }
  deriving stock (Eq, Show)

materializeSeedWithGates ::
  SeedInterpreter request seedPattern frontier root ->
  CohomologicalPruningGates root ->
  request runtime ->
  seedPattern ->
  CandidateRegionSeed root ->
  MaterializedSeed root
materializeSeedWithGates interpreter pruningGates request seedPattern seedValue =
  case cpgSeedDecision pruningGates seedValue of
    PruningRejected certificate ->
      MaterializedSeed
        { msSeed = seedValue,
          msCriticality = SeedNonCritical,
          msRegion = Nothing,
          msRegionAdmissibility = RegionRejected (pcObstructions certificate)
        }
    PruningAccepted _ ->
      case siMaterializeSeed interpreter request seedPattern seedValue of
        Nothing ->
          MaterializedSeed
            { msSeed = seedValue,
              msCriticality = SeedUnknown,
              msRegion = Nothing,
              msRegionAdmissibility = RegionNotMaterialized
            }
        Just regionValue ->
          MaterializedSeed
            { msSeed = seedValue,
              msCriticality = SeedCritical,
              msRegion = Just regionValue,
              msRegionAdmissibility = regionAdmissibility pruningGates regionValue
            }

materializeSeedsWithGates ::
  Foldable seeds =>
  SeedInterpreter request seedPattern frontier root ->
  CohomologicalPruningGates root ->
  request runtime ->
  seedPattern ->
  seeds (CandidateRegionSeed root) ->
  [MaterializedSeed root]
materializeSeedsWithGates interpreter pruningGates request seedPattern =
  map
    (materializeSeedWithGates interpreter pruningGates request seedPattern)
    . toList

regionAdmissibility ::
  CohomologicalPruningGates root ->
  CandidateRegion root ->
  RegionAdmissibility
regionAdmissibility pruningGates regionValue =
  case cpgRegionDecision pruningGates regionValue of
    PruningAccepted _ ->
      RegionAdmissible
    PruningRejected certificate ->
      RegionRejected (pcObstructions certificate)

admissibleMaterializedRegions ::
  [MaterializedSeed root] ->
  [CandidateRegion root]
admissibleMaterializedRegions =
  mapMaybe $ \materializedSeed ->
    case (msRegion materializedSeed, msRegionAdmissibility materializedSeed) of
      (Just regionValue, RegionAdmissible) ->
        Just regionValue
      _ ->
        Nothing

type RequestPruningState :: Type -> Type -> Type -> Type
type RequestPruningState root witness coverage =
  RequestAggregateSummary root witness coverage

type LivePruningState :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data LivePruningState inner purpose root witness coverage obstruction = LivePruningState
  { lpsInner :: !inner,
    lpsRequests :: !(Map (PreparedRequestCacheKey purpose) (RequestPruningState root witness coverage)),
    lpsReusableRequestKeys :: !(Set (PreparedRequestCacheKey purpose)),
    lpsExactScopeCover :: ![(PreparedRequestCacheKey purpose, Delta.Scope IntSet)],
    lpsRefreshObstruction :: !(Maybe obstruction)
  }
  deriving stock (Eq, Show)

emptyLivePruningState ::
  inner ->
  LivePruningState inner purpose root witness coverage obstruction
emptyLivePruningState inner =
  LivePruningState
    { lpsInner = inner,
      lpsRequests = Map.empty,
      lpsReusableRequestKeys = Set.empty,
      lpsExactScopeCover = [],
      lpsRefreshObstruction = Nothing
    }

type LivePruningAdapter ::
  Type ->
  (Type -> Type) ->
  (Type -> Type) ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type
data LivePruningAdapter world request advance root witness obstruction match coverage purpose = LivePruningAdapter
  { lpaRequestKey ::
      forall runtime.
      request runtime ->
      PreparedRequestCacheKey purpose,
    lpaRequestRoots ::
      forall runtime.
      world ->
      request runtime ->
      Set root,
    lpaRetainRequestState ::
      forall runtime.
      request runtime ->
      Bool,
    lpaRootKey ::
      root ->
      Int,
    lpaCanonicalizeRoot ::
      forall runtime.
      advance runtime ->
      root ->
      root,
    lpaRefreshRequest ::
      forall runtime.
      ObstructionDelta root ->
      world ->
      request runtime ->
      Set root ->
      Maybe (RequestPruningState root witness coverage) ->
      Either obstruction (RequestPruningState root witness coverage),
    lpaExactMatches ::
      forall runtime.
      world ->
      request runtime ->
      root ->
      RootResolution witness coverage ->
      [match]
  }

lookupRequestPruningState ::
  Ord purpose =>
  PreparedRequestCacheKey purpose ->
  LivePruningState inner purpose root witness coverage obstruction ->
  Maybe (RequestPruningState root witness coverage)
lookupRequestPruningState requestKey =
  Map.lookup requestKey . lpsRequests
{-# INLINE lookupRequestPruningState #-}

refreshRequestPruningState ::
  Ord root =>
  LivePruningAdapter world request advance root witness obstruction match coverage purpose ->
  ObstructionDelta root ->
  world ->
  request runtime ->
  Maybe (RequestPruningState root witness coverage) ->
  Either obstruction (RequestPruningState root witness coverage)
refreshRequestPruningState adapter matchingDelta world request priorState =
  case priorState of
    Nothing ->
      lpaRefreshRequest adapter matchingDelta world request (lpaRequestRoots adapter world request) Nothing
    Just prior
      | Set.null affectedRoots && not (obstructionDeltaDemandsFullRefresh matchingDelta) ->
          Right prior
      | otherwise ->
          fmap
            (mergeRequestAggregateSummaries affectedRoots prior)
            (lpaRefreshRequest adapter matchingDelta world request affectedRoots (Just prior))
      where
        affectedRoots =
          affectedRootsForObstructionDelta matchingDelta prior

obstructionDeltaDemandsFullRefresh :: ObstructionDelta root -> Bool
obstructionDeltaDemandsFullRefresh matchingDelta =
  let payloadDemandsRefresh =
        maybe False oiInvalidateAllRoots (Delta.scopedDeltaPayload matchingDelta)
   in Delta.foldScope
        payloadDemandsRefresh
        (const payloadDemandsRefresh)
        True
        (Delta.scopedDeltaSupport matchingDelta)

refreshLivePruningState ::
  (Ord purpose, Ord root) =>
  LivePruningAdapter world request advance root witness obstruction match coverage purpose ->
  ObstructionDelta root ->
  world ->
  [request runtime] ->
  LivePruningState inner purpose root witness coverage obstruction ->
  Either obstruction (LivePruningState inner purpose root witness coverage obstruction)
refreshLivePruningState adapter matchingDelta world requests state =
  fmap
    ( \refreshedRequests ->
        state
          { lpsRequests =
              Map.fromList refreshedRequests,
            lpsReusableRequestKeys =
              currentReusableRequestKeys,
            lpsRefreshObstruction =
              Nothing
          }
    )
    (traverse refreshOne (Map.toAscList currentRequestMap))
  where
    currentRequestMap =
      Map.fromList
        [ (lpaRequestKey adapter request, request)
        | request <- requests
        ]

    currentReusableRequestKeys =
      Map.keysSet
        ( Map.filter
            (lpaRetainRequestState adapter)
            currentRequestMap
        )

    reusablePriorRequests =
      Map.restrictKeys
        (lpsRequests state)
        (lpsReusableRequestKeys state)

    refreshOne (requestKey, request) =
      fmap
        (\requestState -> (requestKey, requestState))
        ( refreshRequestPruningState
            adapter
            matchingDelta
            world
            request
            ( if Set.member requestKey currentReusableRequestKeys
                then Map.lookup requestKey reusablePriorRequests
                else Nothing
            )
        )

canonicalizeLivePruningState ::
  (Ord root, Semigroup coverage) =>
  LivePruningAdapter world request advance root witness obstruction match coverage purpose ->
  advance runtime ->
  LivePruningState inner purpose root witness coverage obstruction ->
  LivePruningState inner purpose root witness coverage obstruction
canonicalizeLivePruningState adapter advance state =
  state
    { lpsRequests =
        fmap
          (canonicalizeAggregateSummary (lpaCanonicalizeRoot adapter advance))
          (lpsRequests state)
    }

canonicalizeAggregateSummary ::
  (Ord root, Semigroup coverage) =>
  (root -> root) ->
  RequestAggregateSummary root witness coverage ->
  RequestAggregateSummary root witness coverage
canonicalizeAggregateSummary canonicalizeRoot aggregateSummary =
  let canonicalRootSupport =
        Map.mapKeysWith
          IntSet.union
          canonicalizeRoot
          (rasRootSupport aggregateSummary)
   in RequestAggregateSummary
        { rasRootResolutions =
            Map.mapKeysWith
              mergeRootResolution
              canonicalizeRoot
              (rasRootResolutions aggregateSummary),
          rasRootSupport =
            canonicalRootSupport,
          rasSupportRoots =
            requestAggregateSupportRoots canonicalRootSupport
        }

requestFallbackRoots ::
  RequestPruningState root witness coverage ->
  Set root
requestFallbackRoots =
  rootsMatchingResolution
    (not . rootResolutionExcludesFallback)
    . rasRootResolutions

requestFallbackRootKeys ::
  LivePruningAdapter world request advance root witness obstruction match coverage purpose ->
  RequestPruningState root witness coverage ->
  IntSet
requestFallbackRootKeys adapter =
  rootKeySet (lpaRootKey adapter) . requestFallbackRoots

requestExcludedRoots ::
  RequestPruningState root witness coverage ->
  Set root
requestExcludedRoots =
  rootsMatchingResolution
    rootResolutionExcludesFallback
    . rasRootResolutions

requestExactResolvedRoots ::
  RequestPruningState root witness coverage ->
  Set root
requestExactResolvedRoots =
  rootsMatchingResolution
    rootResolutionExactResolved
    . rasRootResolutions

livePruningMatchingAlgebra ::
  (Ord purpose, Ord root, Semigroup coverage) =>
  (payload -> Maybe (ObstructionInvalidation root)) ->
  LivePruningAdapter world request advance root witness obstruction match coverage purpose ->
  MatchingAlgebra
    environment
    inner
    IntSet
    payload
    world
    request
    advance
    obstruction
    match ->
  MatchingAlgebra
    environment
    (LivePruningState inner purpose root witness coverage obstruction)
    IntSet
    payload
    world
    request
    advance
    obstruction
    match
livePruningMatchingAlgebra payloadInvalidation adapter inner =
  MatchingAlgebra
    { maInitialState =
        emptyLivePruningState (maInitialState inner),
      maEnvironment =
        maEnvironment inner,
      maPrepareQueries =
        \state matchingDelta world requests ->
          case refreshLivePruningState adapter (obstructionDeltaOf matchingDelta) world requests state of
            Left obstruction ->
              ( state
                  { lpsRefreshObstruction = Just obstruction,
                    lpsExactScopeCover = []
                  },
                fmap (MatchingQuery Delta.cleanScope) requests
              )
            Right refreshedState ->
              let (innerState', preparedQueries0) =
                    maPrepareQueries
                      inner
                      (lpsInner refreshedState)
                      matchingDelta
                      world
                      requests

                  stateWithInner =
                    refreshedState
                      { lpsInner = innerState'
                      }

                  stateWithScopes =
                    rememberPreparedScopes adapter preparedQueries0 stateWithInner
               in ( stateWithScopes,
                    fmap
                      (livePruningPreparedQuery adapter world stateWithScopes)
                      preparedQueries0
                  ),
      maRunQueries =
        \state world preparedQueries ->
          case lpsRefreshObstruction state of
            Just obstruction ->
              (state, Left obstruction)
            Nothing ->
              let restrictedPreparedQueries =
                    fmap
                      (livePruningPreparedQuery adapter world state)
                      preparedQueries

                  exactMatchesByQuery =
                    zipWith
                      (exactMatchesForPreparedQuery adapter world state)
                      (exactScopeCoverForPreparedQueries adapter state preparedQueries)
                      preparedQueries

                  (innerState', innerResult) =
                    maRunQueries inner (lpsInner state) world restrictedPreparedQueries

                  stateWithInner =
                    state
                      { lpsInner = innerState'
                      }
               in ( stateWithInner,
                    zipWith (<>) exactMatchesByQuery <$> innerResult
                  ),
      maPreviewQuery =
        \state world preparedQuery ->
          let restrictedPreparedQuery =
                livePruningPreparedQuery adapter world state preparedQuery
           in fmap
                ( \(innerState', diagnostics) ->
                    ( state {lpsInner = innerState'},
                      diagnostics
                    )
                )
                (maPreviewQuery inner (lpsInner state) world restrictedPreparedQuery),
      maAdvanceState =
        \matchingDelta advance state ->
          let advancedInner =
                maAdvanceState
                  inner
                  matchingDelta
                  advance
                  (lpsInner state)

              advancedOverlay =
                canonicalizeLivePruningState adapter advance state
           in advancedOverlay
                { lpsInner = advancedInner,
                  lpsRequests =
                    Map.restrictKeys
                      (lpsRequests advancedOverlay)
                      (lpsReusableRequestKeys advancedOverlay),
                  lpsExactScopeCover = [],
                  lpsRefreshObstruction = Nothing
                },
      maReplayDiagnostics =
        maReplayDiagnostics inner . lpsInner
    }
  where
    obstructionDeltaOf matchingDelta =
      Delta.scopedDelta
        (Delta.scopedDeltaSupport matchingDelta)
        (Delta.scopedDeltaPayload matchingDelta >>= payloadInvalidation)

rememberPreparedScopes ::
  Ord purpose =>
  LivePruningAdapter world request advance root witness obstruction match coverage purpose ->
  [MatchingQuery IntSet request runtime] ->
  LivePruningState inner purpose root witness coverage obstruction ->
  LivePruningState inner purpose root witness coverage obstruction
rememberPreparedScopes adapter preparedQueries state =
  state
    { lpsExactScopeCover =
        [ (requestKey, mqScope preparedQuery)
        | preparedQuery <- preparedQueries,
          let requestKey = lpaRequestKey adapter (mqRequest preparedQuery),
          Map.member requestKey (lpsRequests state)
        ]
    }

livePruningPreparedQuery ::
  Ord purpose =>
  LivePruningAdapter world request advance root witness obstruction match coverage purpose ->
  world ->
  LivePruningState inner purpose root witness coverage obstruction ->
  MatchingQuery IntSet request runtime ->
  MatchingQuery IntSet request runtime
livePruningPreparedQuery adapter world state preparedQuery =
  mapMatchingQueryScope
    (restrictScopeToRootKeys fallbackKeys)
    preparedQuery
  where
    request =
      mqRequest preparedQuery

    requestKey =
      lpaRequestKey adapter request

    fallbackKeys =
      case Map.lookup requestKey (lpsRequests state) of
        Nothing ->
          rootKeySet (lpaRootKey adapter) (lpaRequestRoots adapter world request)
        Just requestState ->
          requestFallbackRootKeys adapter requestState

exactMatchesForPreparedQuery ::
  Ord purpose =>
  LivePruningAdapter world request advance root witness obstruction match coverage purpose ->
  world ->
  LivePruningState inner purpose root witness coverage obstruction ->
  Delta.Scope IntSet ->
  MatchingQuery IntSet request runtime ->
  [match]
exactMatchesForPreparedQuery adapter world state originalScope preparedQuery =
  case Map.lookup requestKey (lpsRequests state) of
    Nothing ->
      []
    Just requestState ->
      concatMap exactForRoot
        (Map.toAscList (rasRootResolutions requestState))
  where
    request =
      mqRequest preparedQuery

    requestKey =
      lpaRequestKey adapter request

    exactForRoot (rootValue, rootResolution)
      | rootResolutionExactResolved rootResolution
          && rootKeyInScope (lpaRootKey adapter rootValue) originalScope =
          lpaExactMatches adapter world request rootValue rootResolution
      | otherwise =
          []

exactScopeCoverForPreparedQueries ::
  Eq purpose =>
  LivePruningAdapter world request advance root witness obstruction match coverage purpose ->
  LivePruningState inner purpose root witness coverage obstruction ->
  [MatchingQuery IntSet request runtime] ->
  [Delta.Scope IntSet]
exactScopeCoverForPreparedQueries adapter state preparedQueries
  | fmap fst exactScopeCover == preparedQueryKeys =
      fmap snd exactScopeCover
  | otherwise =
      fmap mqScope preparedQueries
  where
    exactScopeCover =
      lpsExactScopeCover state

    preparedQueryKeys =
      fmap (lpaRequestKey adapter . mqRequest) preparedQueries

restrictScopeToRootKeys ::
  IntSet ->
  Delta.Scope IntSet ->
  Delta.Scope IntSet
restrictScopeToRootKeys allowedRootKeys scope =
  Delta.foldScope
    Delta.cleanScope
    (\dirtyKeys -> scopeFromRootKeys (IntSet.intersection allowedRootKeys dirtyKeys))
    (scopeFromRootKeys allowedRootKeys)
    scope

scopeFromRootKeys :: IntSet -> Delta.Scope IntSet
scopeFromRootKeys =
  Delta.dirtyScope

rootKeyInScope ::
  Int ->
  Delta.Scope IntSet ->
  Bool
rootKeyInScope rootKey scope =
  Delta.foldScope
    False
    (IntSet.member rootKey)
    True
    scope

rootKeySet ::
  (root -> Int) ->
  Set root ->
  IntSet
rootKeySet rootKey =
  IntSet.fromList . fmap rootKey . Set.toList
