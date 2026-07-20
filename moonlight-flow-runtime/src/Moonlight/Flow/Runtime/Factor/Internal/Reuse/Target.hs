{-# LANGUAGE TypeFamilies #-}
module Moonlight.Flow.Runtime.Factor.Internal.Reuse.Target
  ( visibleFactorShapeManifestNodes,
    planReuseRequestsForManifestNodes,
    planReuseRequestForManifestNode,
  )
where

import Moonlight.Core
  ( QueryId,
  )
import Moonlight.Flow.Carrier.Core.Delta.Emit
  ( CarrierEmitSpec (..),
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )
import Moonlight.Flow.Execution.Subsumption.FactorShape
  ( FactorShapeNodeManifest (..),
    factorShapeFromManifestBoundary,
    factorShapeManifestNodes,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Differential.Row.Patch
  ( emptyPlainRowPatch,
  )
import Moonlight.Flow.Plan.Query.Core
  ( FactorNode,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.Flow.Plan.Shape
  ( factorShapeResidual,
  )
import Moonlight.Flow.Plan.Residual
  ( ResidualTheoryRegistry,
  )
import Moonlight.Flow.Runtime.Factor.Input
  ( factorInputSignatureRuntime,
  )
import Moonlight.Flow.Runtime.Factor.Program
  ( FactorProgram (..),
    factorProgramCanonical,
    factorProgramFactorShapeManifest,
  )
import Moonlight.Flow.Runtime.Carrier.Emit
  ( FactorCarrierEmitSpec,
    FactorCarrierPayload (..),
  )
import Moonlight.Flow.Runtime.Carrier.Emit.Factor
  ( factorNodeCarrierVisible,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
    RuntimeEnv (..),
  )
import Moonlight.Flow.Runtime.Error
  ( RuntimeError (..),
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Carrier.Reuse
  ( PlanReuseRequest (..),
    reuseValidityRequestFromTime,
  )

visibleFactorShapeManifestNodes ::
  FactorProgram ->
  [(FactorNode, FactorShapeNodeManifest)]
visibleFactorShapeManifestNodes program =
  filter
    (factorNodeCarrierVisible . fsnmNode . snd)
    (factorShapeManifestNodes (factorProgramFactorShapeManifest program))
{-# INLINE visibleFactorShapeManifestNodes #-}

planReuseRequestsForManifestNodes ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  FactorCarrierEmitSpec ctx prop boundary evidence ->
  RelationalCarrierTime ctx ->
  QueryId ->
  FactorProgram ->
  [(FactorNode, FactorShapeNodeManifest)] ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    [PlanReuseRequest ctx prop]
planReuseRequestsForManifestNodes spec eventTime queryId program nodes runtime0 = do
  inputDigest <-
    factorInputSignatureRuntime
      (reAtomCarrierEmitSpec (rdrEnv runtime0))
      queryId
      program
      runtime0
  traverse
    ( planReuseRequestForManifestNode
        spec
        eventTime
        queryId
        inputDigest
        (reResidualTheoryRegistry (rdrEnv runtime0))
        program
    )
    nodes
{-# INLINE planReuseRequestsForManifestNodes #-}

planReuseRequestForManifestNode ::
  boundary ~ RuntimeBoundary =>
  FactorCarrierEmitSpec ctx prop boundary evidence ->
  RelationalCarrierTime ctx ->
  QueryId ->
  StableDigest128 ->
  ResidualTheoryRegistry ->
  FactorProgram ->
  (FactorNode, FactorShapeNodeManifest) ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (PlanReuseRequest ctx prop)
planReuseRequestForManifestNode spec eventTime queryId inputDigest residualTheory program (_nodeKey, nodeManifest) =
  let payload =
        FactorCarrierPayload
          { fcpRelationalScope = mempty,
            fcpNode = fsnmNode nodeManifest,
            fcpSchema = fsnmOutputSchema nodeManifest,
            fcpRows = emptyPlainRowPatch
          }
      boundary =
        cesBoundaryOf spec (queryId, payload)
      targetCarrier =
        cesAddrOf spec (queryId, payload)
      viewDigest =
        Just inputDigest
   in case factorShapeFromManifestBoundary
        (factorProgramCanonical program)
        nodeManifest
        boundary of
        Left _shapeError ->
          Left (RuntimeMissingFactorProgram queryId)
        Right shapeKey ->
          let validity =
                reuseValidityRequestFromTime
                  viewDigest
                  (factorShapeResidual shapeKey)
                  eventTime
           in Right
                PlanReuseRequest
                  { prqTargetCarrier = targetCarrier,
                    prqShape = shapeKey,
                    prqBoundary = boundary,
                    prqValidity = validity,
                    prqResidualTheory = residualTheory
                  }
{-# INLINE planReuseRequestForManifestNode #-}
