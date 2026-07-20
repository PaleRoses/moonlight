module Moonlight.EGraph.Saturation.Cohomological.Backend.Matching
  ( cohomologicalMatchingAlgebra,
    cohomologicalMatchingStrategy,
    livePrunedCohomologicalMatchingStrategy,
  )
where

import Moonlight.EGraph.Saturation.Cohomological.Types (SheafCapabilityAtom)
import Data.Bifunctor (first)
import Data.IntSet qualified as IntSet
import Data.Set qualified as Set
import Moonlight.Core (ZipMatch (..), ConstructorTag, HasConstructorTag, Language, classIdKey)
import Moonlight.Delta.Scope qualified as Delta
import Moonlight.Core (Substitution)
import Moonlight.EGraph.Introspection.Core.Rewrite (RewriteMorphism)
import Moonlight.EGraph.Pure.Relational (wcojMatchCompiledWithRootFilter)
import Moonlight.EGraph.Pure.Saturation.Matching
  ( EGraphMatchingObstruction (..),
    MatchingAdvanceCtx (macCanonicalize),
    MatchingAlgebra,
    MatchingDeltaPayload (mdpObstructionInvalidation),
    MatchingFrontier,
    MatchingRequest,
    MatchingStrategy (CustomMatchingAlgebra),
    MatchingWorld,
    SaturationPurpose,
    eGraphRelationalMatchObstruction,
    matchingProofReachability,
    rootFilterMatchingAlgebra,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    canonicalizeClassId,
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance
  ( CohomologicalBackend (..),
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance.Internal.Site
  ( EGraphObstructionWitness,
    EGraphRootCoverage,
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance.Internal.Prepared
  ( PreparedCohomologicalBackend (..),
    prepareCohomologicalBackend,
    preparedRequestCacheKeyFor,
    preparedRequestCachePolicyFor,
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Matching.Internal.Frontier
  ( analyzePreparedRequestOverRegionsToSummary,
    preparedInitialRegionsForRequest,
  )
import Moonlight.Rewrite.System
  ( GuardCapabilityResolver,
    emptyGuardCapabilityResolver,
  )
import Moonlight.Rewrite.Algebra
  ( cpqPrimaryPattern,
  )
import Moonlight.Saturation.Matching qualified as GenericMatching
import Moonlight.Saturation.Obstruction.Cohomological.Aggregate
  ( RootResolution,
    rootResolutionCoverage,
  )
import Moonlight.Saturation.Obstruction.Cohomological.LivePruning
  ( ObstructionDelta,
    LivePruningAdapter (..),
    LivePruningState,
    RequestPruningState,
    livePruningMatchingAlgebra,
  )
import Moonlight.Saturation.Obstruction.Cohomological.Prepared
  ( PreparedInitialRegionBatch (pirbRegions),
  )
import Moonlight.Saturation.Obstruction.Cohomological.Seed
  ( SeedInterpreter (siSeedPlan),
    seedFrontierPlanSeeds,
  )
import Moonlight.Sheaf.Obstruction
  ( CandidateRegionSeed (crsRoot),
    CohomologicalPolicy (..),
    emptyCohomologicalCache,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Evidence.Certification
  ( CachePolicy (..),
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section.Exact
  ( CohomologicalExactMatch (..),
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Section
  ( SectionCoverage (..),
  )

cohomologicalMatchingAlgebra ::
  (HasConstructorTag f, ZipMatch f, Show (ConstructorTag f), Show (f ()), Eq (RewriteMorphism f)) =>
  CohomologicalBackend owner c f ->
  MatchingAlgebra (LivePruningState () SaturationPurpose ClassId EGraphObstructionWitness EGraphRootCoverage EGraphMatchingObstruction) owner c SheafCapabilityAtom f a
cohomologicalMatchingAlgebra backend =
  livePruningMatchingAlgebra
    mdpObstructionInvalidation
    (preparedBackendLivePruningAdapter (prepareCohomologicalBackend backend))
    (cohomologicalFallbackMatchingAlgebra emptyGuardCapabilityResolver)

cohomologicalMatchingStrategy ::
  (HasConstructorTag f, ZipMatch f, Show (ConstructorTag f), Show (f ()), Eq (RewriteMorphism f)) =>
  CohomologicalBackend owner c f ->
  MatchingStrategy owner c SheafCapabilityAtom f a
cohomologicalMatchingStrategy =
  CustomMatchingAlgebra . cohomologicalMatchingAlgebra

livePrunedCohomologicalMatchingStrategy ::
  (HasConstructorTag f, ZipMatch f, Show (ConstructorTag f), Show (f ()), Eq (RewriteMorphism f)) =>
  PreparedCohomologicalBackend owner c f ->
  GuardCapabilityResolver SheafCapabilityAtom ->
  MatchingStrategy owner c SheafCapabilityAtom f a
livePrunedCohomologicalMatchingStrategy backend capabilityResolver =
  CustomMatchingAlgebra
    ( livePruningMatchingAlgebra
        mdpObstructionInvalidation
        (preparedBackendLivePruningAdapter backend)
        (cohomologicalFallbackMatchingAlgebra capabilityResolver)
    )

cohomologicalFallbackMatchingAlgebra ::
  (Language f, Show (f ())) =>
  GuardCapabilityResolver SheafCapabilityAtom ->
  MatchingAlgebra () owner c SheafCapabilityAtom f a
cohomologicalFallbackMatchingAlgebra capabilityResolver =
  rootFilterMatchingAlgebra capabilityResolver $ \rootFilter compiledQuery graph ->
    first
      eGraphRelationalMatchObstruction
      (wcojMatchCompiledWithRootFilter rootFilter compiledQuery graph)

preparedBackendLivePruningAdapter ::
  (HasConstructorTag f, ZipMatch f, Show (ConstructorTag f), Eq (RewriteMorphism f)) =>
  PreparedCohomologicalBackend owner c f ->
  LivePruningAdapter
    (MatchingWorld owner c SheafCapabilityAtom f a)
    (MatchingRequest owner c SheafCapabilityAtom f)
    (MatchingAdvanceCtx owner c f)
    ClassId
    EGraphObstructionWitness
    EGraphMatchingObstruction
    (ClassId, Substitution)
    EGraphRootCoverage
    SaturationPurpose
preparedBackendLivePruningAdapter backend =
  LivePruningAdapter
    { lpaRequestKey =
        preparedRequestCacheKeyFor (pcbConfiguration backend),
      lpaRequestRoots =
        requestSeedRoots backend,
      lpaRetainRequestState =
        requestCacheEnabled
          . preparedRequestCachePolicyFor (pcbConfiguration backend),
      lpaRootKey =
        classIdKey,
      lpaCanonicalizeRoot =
        \advanceCtx root -> macCanonicalize advanceCtx root,
      lpaRefreshRequest =
        refreshLivePruningRequestState backend,
      lpaExactMatches =
        exactMatchesForRootResolution
    }

requestCacheEnabled :: CachePolicy -> Bool
requestCacheEnabled cachePolicy =
  case cachePolicy of
    DoNotCache ->
      False
    SharedAcrossEnvironments ->
      True
    EnvironmentScoped _ ->
      True

requestSeedRoots ::
  PreparedCohomologicalBackend owner c f ->
  MatchingWorld owner c SheafCapabilityAtom f supportRuntime ->
  MatchingRequest owner c SheafCapabilityAtom f runtime ->
  Set.Set ClassId
requestSeedRoots backend _world request =
  case cbSeedInterpreter (pcbConfiguration backend) of
    Nothing ->
      Set.empty
    Just seedInterpreter ->
      Set.fromList
        . fmap crsRoot
        . seedFrontierPlanSeeds
        $ siSeedPlan seedInterpreter request (cpqPrimaryPattern (GenericMatching.qrQuery request))

refreshLivePruningRequestState ::
  (HasConstructorTag f, ZipMatch f, Show (ConstructorTag f), Eq (RewriteMorphism f)) =>
  PreparedCohomologicalBackend owner c f ->
  ObstructionDelta ClassId ->
  MatchingWorld owner c SheafCapabilityAtom f supportRuntime ->
  MatchingRequest owner c SheafCapabilityAtom f runtime ->
  Set.Set ClassId ->
  Maybe (RequestPruningState ClassId EGraphObstructionWitness EGraphRootCoverage) ->
  Either EGraphMatchingObstruction (RequestPruningState ClassId EGraphObstructionWitness EGraphRootCoverage)
refreshLivePruningRequestState backend matchingDelta world request affectedRoots priorState =
  if hierarchicalPruningWithoutSeedFrontier (pcbConfiguration backend)
    then Left EGraphMatchingHierarchicalPruningWithoutSeedFrontier
    else
      Right $
        let matchingFrontier =
              requestRefreshFrontier matchingDelta priorState affectedRoots
            graph =
              GenericMatching.mwGraph world
            canonicalize =
              canonicalizeClassId graph
            preparedRegions =
              preparedInitialRegionsForRequest backend matchingFrontier canonicalize request
            (_cache, aggregateSummary) =
              analyzePreparedRequestOverRegionsToSummary
                backend
                emptyCohomologicalCache
                graph
                (GenericMatching.mwFacts world)
                (GenericMatching.mwFactDerivations world)
                (matchingProofReachability <$> GenericMatching.mwProofContext world)
                request
                (pirbRegions preparedRegions)
         in aggregateSummary

hierarchicalPruningWithoutSeedFrontier :: CohomologicalBackend owner c f -> Bool
hierarchicalPruningWithoutSeedFrontier configuration =
  cpUseHierarchicalPruning (cbPolicy configuration)
    && case cbSeedInterpreter configuration of
      Nothing ->
        True
      Just _ ->
        False

requestRefreshFrontier ::
  ObstructionDelta ClassId ->
  Maybe (RequestPruningState ClassId EGraphObstructionWitness EGraphRootCoverage) ->
  Set.Set ClassId ->
  MatchingFrontier
requestRefreshFrontier matchingDelta priorState affectedRoots =
  case priorState of
    Nothing ->
      Delta.fullScope
    Just _ ->
      Delta.foldScope
        (scopeFromRoots affectedRoots)
        (const (scopeFromRoots affectedRoots))
        Delta.fullScope
        (Delta.scopedDeltaSupport matchingDelta)

scopeFromRoots :: Set.Set ClassId -> Delta.Scope IntSet.IntSet
scopeFromRoots roots =
  let rootKeys =
        Set.foldl'
          (\keys root -> IntSet.insert (classIdKey root) keys)
          IntSet.empty
          roots
   in if IntSet.null rootKeys
        then Delta.cleanScope
        else Delta.dirtyScope rootKeys

exactMatchesForRootResolution ::
  MatchingWorld owner c SheafCapabilityAtom f supportRuntime ->
  MatchingRequest owner c SheafCapabilityAtom f runtime ->
  ClassId ->
  RootResolution EGraphObstructionWitness EGraphRootCoverage ->
  [(ClassId, Substitution)]
exactMatchesForRootResolution _support _request root rootResolution =
  [ (cemRootClass exactMatch, cemSubstitution exactMatch)
  | exactMatch <- scMatches (rootResolutionCoverage rootResolution),
    cemRootClass exactMatch == root
  ]
