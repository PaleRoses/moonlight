module Moonlight.EGraph.Saturation.Cohomological.Backend.Matching.Internal.Analysis
  ( analyzeRequestOverRegionsToSummary,
  )
where

import Moonlight.EGraph.Saturation.Cohomological.Types (SheafCapabilityAtom)
import Moonlight.Sheaf.Pruning (pruningDecisionAllowed)
import Data.IntSet qualified as IntSet
import Moonlight.Saturation.Obstruction.Cohomological.Aggregate
  ( RequestAggregateProjection (..),
    RequestAggregateSummary,
    emptyRequestAggregateSummary,
    insertRequestAggregateSummaryWith,
    rootResolutionFromRegionTraversal,
  )
import Control.Foldl qualified as Foldl
import Moonlight.Saturation.Obstruction.Cohomological.Region
  ( regionFoldWith,
  )
import Moonlight.Core (ZipMatch (..), ConstructorTag, HasConstructorTag)
import Moonlight.EGraph.Introspection.Analysis.Resolution
  ( ResolutionBundle,
  )
import Moonlight.EGraph.Pure.Saturation.Matching
  ( MatchingRequest,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    EGraph,
    canonicalizeClassId,
    classIdKey,
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance
  ( CohomologicalBackend (..),
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance.Internal.Site
  ( CohomologicalRuntime (..),
    EGraphCandidateRegion,
    EGraphObstructionWitness,
    EGraphRootCoverage,
    EGraphObstructionCache,
    RegionTraversalSummary,
  )
import Moonlight.Rewrite.ProofContext
  ( ProofReachability,
  )
import Moonlight.Rewrite.System
  ( FactDerivationIndex,
  )
import Moonlight.Rewrite.System
  ( FactStore,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Analysis
  ( analyzeCohomologicalRegion,
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Seed
  ( requestPrefersCoarseRefinement,
    requestPruningGates,
  )

import Moonlight.Sheaf.Obstruction.Cohomological.Section
  ( SectionCoverage (..),
  )
import Moonlight.EGraph.Introspection.Core.Rewrite (RewriteMorphism)
import Moonlight.Sheaf.Obstruction
  ( CandidateRegion (crMembers, crRoot),
    CohomologicalPruningGates (..),
    recCoverage,
    rtsRegion,
  )

analyzeRequestOverRegionsToSummary ::
  (HasConstructorTag f, ZipMatch f, Show (ConstructorTag f), Eq (RewriteMorphism f)) =>
  Maybe (ResolutionBundle f) ->
  CohomologicalBackend owner c f ->
  EGraphObstructionCache ->
  EGraph f supportRuntime ->
  FactStore ->
  FactDerivationIndex ->
  Maybe ProofReachability ->
  MatchingRequest owner c SheafCapabilityAtom f runtime ->
  [EGraphCandidateRegion] ->
  (EGraphObstructionCache, RequestAggregateSummary ClassId EGraphObstructionWitness EGraphRootCoverage)
analyzeRequestOverRegionsToSummary maybeResolution configuration initialCache graph factStore factDerivations maybeProofReachability request initialRegions =
  let canonicalize =
        canonicalizeClassId graph
      runtimeConfiguration =
        CohomologicalRuntime configuration graph factStore factDerivations maybeProofReachability
      pruningGates =
        requestPruningGates maybeResolution (cbPolicy configuration) request
      preferCoarse =
        requestPrefersCoarseRefinement maybeResolution (cbPolicy configuration) request
   in Foldl.fold
        ( regionFoldWith
            (\_request -> pruningDecisionAllowed . cpgRegionDecision pruningGates)
            (analyzeCohomologicalRegion preferCoarse pruningGates runtimeConfiguration)
            (insertRequestAggregateSummary canonicalize)
            (const emptyRequestAggregateSummary)
            initialCache
            request
        )
        initialRegions

regionDependencyKeysForRequest ::
  (ClassId -> ClassId) ->
  EGraphCandidateRegion ->
  IntSet.IntSet
regionDependencyKeysForRequest canonicalize regionValue =
  IntSet.insert
    (canonicalClassKey canonicalize (crRoot regionValue))
    (IntSet.map (canonicalMemberKey canonicalize) (crMembers regionValue))

insertRequestAggregateSummary ::
  (ClassId -> ClassId) ->
  MatchingRequest owner c SheafCapabilityAtom f runtime ->
  RegionTraversalSummary ->
  RequestAggregateSummary ClassId EGraphObstructionWitness EGraphRootCoverage ->
  RequestAggregateSummary ClassId EGraphObstructionWitness EGraphRootCoverage
insertRequestAggregateSummary canonicalize _request =
  insertRequestAggregateSummaryWith
    (requestAggregateProjection canonicalize)

requestAggregateProjection ::
  (ClassId -> ClassId) ->
  RequestAggregateProjection RegionTraversalSummary ClassId EGraphObstructionWitness EGraphRootCoverage
requestAggregateProjection canonicalize =
  RequestAggregateProjection
    { rapRoot =
        canonicalize . crRoot . rtsRegion,
      rapSupportKeys =
        regionDependencyKeysForRequest canonicalize . rtsRegion,
      rapResolution =
        rootResolutionFromRegionTraversal
          (not . null . scMatches)
          recCoverage
    }

canonicalClassKey :: (ClassId -> ClassId) -> ClassId -> Int
canonicalClassKey canonicalize =
  classIdKey . canonicalize

canonicalMemberKey :: (ClassId -> ClassId) -> Int -> Int
canonicalMemberKey canonicalize =
  canonicalClassKey canonicalize . ClassId
