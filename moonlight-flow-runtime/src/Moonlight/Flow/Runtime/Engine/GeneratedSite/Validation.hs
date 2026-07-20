module Moonlight.Flow.Runtime.Engine.GeneratedSite.Validation
  ( runtimeTopologyTransitionError,
    validateGeneratedSiteCandidateRuntime,
  )
where

import Data.Either
  ( lefts,
  )
import Data.Map.Strict qualified as Map
import Moonlight.Flow.Runtime.Carrier.State
  ( RuntimeShardRegistry (..),
    runtimeShardRegistry,
  )
import Moonlight.Flow.Runtime.Error
  ( RuntimeError (..),
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Factor.State
  ( factorQueryIds,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
    RuntimeState,
    rsCarrierTopology,
    rsGeneratedSite,
    rsRouting,
  )
import Moonlight.Flow.Runtime.Topology
  ( RuntimeTopologyTransitionError (..),
  )
import Moonlight.Flow.Runtime.Topology.Site.Types
import Moonlight.Flow.Runtime.Topology.Validate
  ( validateRuntimeTopology,
  )

runtimeTopologyTransitionError ::
  RuntimeTopologyTransitionError ctx prop ->
  RelationalRuntimeError ctx prop boundary evidence
runtimeTopologyTransitionError topologyError =
  case topologyError of
    RuntimeTopologyTransitionPatchError patchError ->
      RuntimeGeneratedSitePatchError patchError
    RuntimeTopologyTransitionRoutingError routingError ->
      RuntimeGeneratedRoutingError routingError
{-# INLINE runtimeTopologyTransitionError #-}

validateGeneratedSiteCandidateRuntime ::
  (Ord ctx, Ord prop) =>
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
validateGeneratedSiteCandidateRuntime runtime =
  case generatedSiteProgramValidationErrors state of
    programErrors@(_ : _) ->
      Left (RuntimeGeneratedSitePatchError (GeneratedSiteContextShapeInvalid programErrors))
    [] ->
      validateRuntimeTopologyCandidate
  where
    state =
      rdrState runtime

    registry =
      runtimeShardRegistry state

    validateRuntimeTopologyCandidate =
      case
        validateRuntimeTopology
          (rsRouting state)
          (factorQueryIds state)
          (rsrRestrictOps registry)
          (rsrIndexOps registry)
          (rsCarrierTopology state)
        of
        Left errors ->
          Left (RuntimeGeneratedTopologyInvalid errors)
        Right () ->
          Right runtime
{-# INLINE validateGeneratedSiteCandidateRuntime #-}

generatedSiteProgramValidationErrors ::
  Ord prop =>
  RuntimeState ctx prop boundary evidence ->
  [GeneratedSiteValidationError ctx prop]
generatedSiteProgramValidationErrors state =
  concat $
    lefts
      [ validateGeneratedContextShapeWithPrograms knownQueryIds contextValue shape
      | (contextValue, shape) <- Map.toAscList (gssContexts (rsGeneratedSite state))
      ]
  where
    knownQueryIds =
      Map.fromSet (const ()) (factorQueryIds state)
{-# INLINE generatedSiteProgramValidationErrors #-}
