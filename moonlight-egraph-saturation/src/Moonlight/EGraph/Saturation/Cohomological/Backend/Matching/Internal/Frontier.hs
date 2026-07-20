module Moonlight.EGraph.Saturation.Cohomological.Backend.Matching.Internal.Frontier
  ( preparedInitialRegionsForRequest,
    analyzePreparedRequestOverRegionsToSummary,
  )
where

import Moonlight.EGraph.Saturation.Cohomological.Types (SheafCapabilityAtom)
import Moonlight.Delta.Scope qualified as Delta
import Moonlight.Core (ZipMatch (..), ConstructorTag, HasConstructorTag)
import Moonlight.EGraph.Pure.Saturation.Matching
  ( MatchingFrontier,
    MatchingRequest,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId,
    EGraph,
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
import Moonlight.EGraph.Saturation.Cohomological.Backend.Matching.Internal.Analysis
  ( analyzeRequestOverRegionsToSummary,
  )
import Moonlight.Saturation.Obstruction.Cohomological.Aggregate
  ( RequestAggregateSummary,
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance
  ( CohomologicalBackend (..),
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance.Internal.Prepared
  ( PreparedCohomologicalBackend (..),
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Instance.Internal.Site
  ( EGraphObstructionCache,
    EGraphObstructionWitness,
    EGraphRootCoverage,
  )
import Moonlight.EGraph.Saturation.Cohomological.Backend.Matching.Internal.Region
  ( carrierInitialRegionsForRequest,
    seededInitialRegionsForRequest,
  )
import Moonlight.Sheaf.Obstruction
  ( CandidateRegion,
  )
import Moonlight.EGraph.Introspection.Core.Rewrite (RewriteMorphism)
import Moonlight.Saturation.Obstruction.Cohomological.Prepared
  ( PreparedInitialRegionBatch (..),
    emptyPreparedInitialRegionBatch,
  )

preparedInitialRegionsForRequest ::
  (HasConstructorTag f, ZipMatch f, Eq (RewriteMorphism f)) =>
  PreparedCohomologicalBackend owner c f ->
  MatchingFrontier ->
  (ClassId -> ClassId) ->
  MatchingRequest owner c SheafCapabilityAtom f runtime ->
  PreparedInitialRegionBatch ClassId
preparedInitialRegionsForRequest preparedBackend matchingFrontier canonicalize request =
  Delta.foldScope
    emptyPreparedInitialRegionBatch
    ( \_ ->
      case (cbSeedInterpreter configuration, pcbResolution preparedBackend) of
        (Just seedInterpreter, Just resolutionValue) ->
          seededInitialRegionsForRequest configuration seedInterpreter resolutionValue matchingFrontier canonicalize request
        _ ->
          carrierInitialRegionsForRequest configuration matchingFrontier canonicalize request
    )
    (carrierInitialRegionsForRequest configuration matchingFrontier canonicalize request)
    matchingFrontier
  where
    configuration = pcbConfiguration preparedBackend

analyzePreparedRequestOverRegionsToSummary ::
  (HasConstructorTag f, ZipMatch f, Show (ConstructorTag f), Eq (RewriteMorphism f)) =>
  PreparedCohomologicalBackend owner c f ->
  EGraphObstructionCache ->
  EGraph f supportRuntime ->
  FactStore ->
  FactDerivationIndex ->
  Maybe ProofReachability ->
  MatchingRequest owner c SheafCapabilityAtom f runtime ->
  [CandidateRegion ClassId] ->
  (EGraphObstructionCache, RequestAggregateSummary ClassId EGraphObstructionWitness EGraphRootCoverage)
analyzePreparedRequestOverRegionsToSummary preparedBackend =
  analyzeRequestOverRegionsToSummary (pcbResolution preparedBackend) (pcbConfiguration preparedBackend)
