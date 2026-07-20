{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Flow.Runtime.Kernel.Create
  ( deriveRuntimeConfig,
    seedInitialData,
    createRelDiffRuntimeWithBackend,
    createRuntimeWithBackend,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( QueryId,
    initialLiveEpoch,
    initialQuotientEpoch,
    mkAtomId,
    queryIdKey,
  )
import Moonlight.Differential.Time
  ( initialFrontierStamp,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
    CarrierAddressBook (..),
    queryAtomCarrier,
    queryFactorCarrier,
  )
import Moonlight.Flow.Carrier.Core.Frontier
  ( emptyRelDiffFrontier,
  )
import Moonlight.Flow.Model.Delta
  ( AtomEvent (..)
  )
import Moonlight.Flow.Carrier.Engine.Project
  ( CarrierProjectState (..),
  )
import Moonlight.Flow.Carrier.Morphism.Core.Program
  ( emptyCarrierMorphismRuntime,
  )
import Moonlight.Flow.Carrier.Store
  ( emptyCarrierStore,
  )
import Moonlight.Flow.Carrier.View.Section
  ( RelationalSection (..),
  )
import Moonlight.Flow.Execution.Subsumption.FactorShape
  ( FactorShapeNodeManifest (..),
    factorShapeManifestNodes,
  )
import Moonlight.Flow.Model.Event
  ( LocalRelationalAddr (..),
    LocalRelationalEvent (..),
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
    emptyRuntimeBoundary,
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )
import Moonlight.Flow.Runtime.Backend
  ( RuntimeBackend (..),
    RuntimeBackendError (..),
  )
import Moonlight.Flow.Runtime.Core.Create
  ( CompiledRuntimeSpec (..),
    compileRuntimeSpec,
    deferInitialData,
  )
import Moonlight.Flow.Runtime.Core.Patch.Validation
  ( CanonicalityOracle,
  )
import Moonlight.Flow.Runtime.Core.Patch.Internal
  ( patchNull,
    patchToQuotientPatch,
  )
import Moonlight.Flow.Runtime.Core.State qualified as Core
import Moonlight.Flow.Runtime.Types
  ( Runtime (..),
    RuntimeApplyError (..),
    RuntimeCreateError (..),
    RuntimeCreateOptions (..),
    RuntimeSeedMode (..),
  )
import Moonlight.Flow.Runtime.Carrier.Emit
  ( AtomCarrierPayload (..),
    atomCarrierEmitSpec,
    factorCarrierEmitSpec,
  )
import Moonlight.Flow.Runtime.Carrier.Emit.Factor
  ( factorNodeCarrierVisible,
  )
import Moonlight.Flow.Runtime.Engine.Patch.Apply qualified as EnginePatch
import Moonlight.Flow.Runtime.Factor.Compile
  ( CompiledRuntimeFactorPrograms (..),
    compileRuntimeFactorPrograms,
  )
import Moonlight.Flow.Runtime.Spec.FactorProgram
  ( FactorProgramSpec (..),
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
  )
import Moonlight.Flow.Runtime.Kernel.Config
  ( RuntimeConfig (..),
    mkRelDiffRuntime,
    mkRelDiffRuntimeConfig,
  )
import Moonlight.Flow.Runtime.Spec.Schema
  ( RuntimeAtomSchema,
    RuntimeInitialData (..),
    RuntimePlan (..),
    RuntimeSpec (..),
    runtimePlanOccurrenceAtomSchemas,
    runtimePlanQueryId,
  )
import Moonlight.Flow.Runtime.Topology.Generate
  ( deriveGeneratedSite,
    validateGeneratedSite,
  )
import Moonlight.Flow.Runtime.Topology.Site.Types
  ( GeneratedSiteState,
  )
import Moonlight.FiniteLattice
  ( ContextLattice
  )
import Moonlight.FiniteLattice
  ( principalSupport
  )


createRuntimeWithBackend ::
  ( Ord ctx,
    Ord prop,
    Eq evidence,
    Show evidence,
    Semigroup evidence,
    Show joinErr
  ) =>
  RuntimeBackend ctx prop evidence joinState joinErr ->
  RuntimeSpec ctx prop ->
  RuntimeCreateOptions ->
  Either (RuntimeCreateError ctx prop) (Runtime ctx prop)
createRuntimeWithBackend backend spec options =
  Runtime <$> createRelDiffRuntimeWithBackend backend spec options

createRelDiffRuntimeWithBackend ::
  ( Ord ctx,
    Ord prop,
    Eq evidence,
    Show evidence,
    Semigroup evidence,
    Show joinErr
  ) =>
  RuntimeBackend ctx prop evidence joinState joinErr ->
  RuntimeSpec ctx prop ->
  RuntimeCreateOptions ->
  Either
    (RuntimeCreateError ctx prop)
    (RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr)
createRelDiffRuntimeWithBackend backend spec options = do
  compiled <-
    first RuntimeCreateSpecError (compileRuntimeSpec spec)
  contextLattice <-
    first RuntimeCreateBackendError $
      rbContextLattice backend (rsSchema (crsRuntimeSpec compiled))
  _atomBoundaries <-
    first RuntimeCreateBackendError $
      compileAtomBoundaries backend (crsAtomSchemas compiled)
  compiledFactors <-
    first
      (uncurry RuntimeCreateFactorProgramInvalid)
      (compileRuntimeFactorPrograms (rsPlans (crsRuntimeSpec compiled)))
  generatedSite <-
    first
      RuntimeCreateCarrierSubscriptionBuildFailed
      ( deriveGeneratedSite
          (crsAtomSchemas compiled)
          (rsSchema (crsRuntimeSpec compiled))
          (rsPlans (crsRuntimeSpec compiled))
      )
  first
    RuntimeCreateGeneratedSiteInvalid
    (validateGeneratedSite (crfpQueryBindings compiledFactors) generatedSite)
  factorBoundaries <-
    compileFactorBoundaries backend (rsPlans (crsRuntimeSpec compiled))
  occurrenceAtomBoundaries <-
    first RuntimeCreateBackendError $
      compileOccurrenceAtomBoundaries backend (rsPlans (crsRuntimeSpec compiled))
  let carrierBoundaries =
        occurrenceAtomBoundaries
          <> factorBoundaries
      config =
        deriveRuntimeConfig
          backend
          options
          compiled
          contextLattice
          (rbCanonicalityOracle backend (rsSchema (crsRuntimeSpec compiled)))
          generatedSite
          compiledFactors
          carrierBoundaries
  relConfig <-
    first RuntimeCreateConfigRejected $
      mkRelDiffRuntimeConfig config
  let runtime0 =
        mkRelDiffRuntime relConfig
  case rcoSeedMode options of
    RuntimeSeedEager ->
      seedInitialData compiled runtime0
    RuntimeSeedDeferred ->
      Right (deferInitialData compiled runtime0)

deriveRuntimeConfig ::
  forall ctx prop evidence joinState joinErr.
  RuntimeBackend ctx prop evidence joinState joinErr ->
  RuntimeCreateOptions ->
  CompiledRuntimeSpec ctx prop ->
  ContextLattice ctx ->
  CanonicalityOracle RowTupleKey ->
  GeneratedSiteState ctx prop ->
  CompiledRuntimeFactorPrograms ->
  Map (QueryId, Carrier) RuntimeBoundary ->
  RuntimeConfig ctx prop RuntimeBoundary evidence joinState joinErr
deriveRuntimeConfig backend options compiled contextLattice canonicalityOracle generatedSite compiledFactors carrierBoundaries =
  RuntimeConfig
    { rcQuotientEpoch = initialQuotientEpoch,
      rcLiveEpoch = initialLiveEpoch,
      rcNextFrontierStamp = initialFrontierStamp,
      rcCanonicalityOracle = canonicalityOracle,
      rcAtomCarrierEmitSpec =
        atomCarrierEmitSpec
          addressBook
          atomSupportOf
          atomBoundaryOf
          (rbAtomEvidence backend),
      rcFactorCarrierEmitSpec =
        factorCarrierEmitSpec
          addressBook
          factorSupportOf
          factorBoundaryOf
          (rbFactorEvidence backend),
      rcCarrierOperators = rbCarrierOperators backend,
      rcCarrierSummaryOps = rbCarrierSummaryOps backend,
      rcFrontier = emptyRelDiffFrontier,
      rcProjectStates = IntMap.singleton 0 projectState,
      rcRestrictStates = IntMap.singleton 0 emptyCarrierMorphismRuntime,
      rcIndexStates = IntMap.singleton 0 emptyCarrierStore,
      rcVisibleCacheBudgetBytes = rcoVisibleCacheBudgetBytes options,
      rcVisibleSectionBytes = visibleSectionBytes,
      rcContextLattice = contextLattice,
      rcRepairTelemetry = rcoRepairTelemetry options,
      rcGeneratedSite = generatedSite,
      rcFactorPrograms = crfpPrograms compiledFactors,
      rcQueryBindings = crfpQueryBindings compiledFactors,
      rcReuseMode = rbReuseMode backend,
      rcResidualTheoryRegistry = rbResidualTheoryRegistry backend
    }
  where
    contextOfQuery queryId =
      IntMap.findWithDefault
        (crsDefaultContext compiled)
        (queryIdKey queryId)
        (crsQueryContexts compiled)

    propOfQuery queryId =
      IntMap.findWithDefault
        (crsDefaultProp compiled)
        (queryIdKey queryId)
        (crsQueryProps compiled)

    addressBook =
      CarrierAddressBook
        { cabContextOfQuery = contextOfQuery,
          cabPropOfQuery = propOfQuery
        }

    atomSupportOf payload =
      principalSupport
        (contextOfQuery (aeQueryId (acpEvent payload)))

    factorSupportOf queryId _payload =
      principalSupport (contextOfQuery queryId)

    atomBoundaryOf queryId atomId _rows =
      boundaryOfCarrier queryId (queryAtomCarrier queryId atomId)

    factorBoundaryOf queryId carrier _schema =
      boundaryOfCarrier queryId carrier

    boundaryOfCarrier queryId carrier =
      Map.findWithDefault
        emptyRuntimeBoundary
        (queryId, carrier)
        carrierBoundaries

    projectState =
      CarrierProjectState
        { cpsAddressBook = addressBook,
          cpsSupportOf =
            \delta _slice ->
              principalSupport
                (contextOfQuery (lraQueryId (lreAddr delta))),
          cpsBoundaryOf =
            \queryId carrier -> boundaryOfCarrier queryId carrier,
          cpsEvidenceOf =
            \_delta _slice ->
              rbDefaultEvidence backend
        }

    visibleSectionBytes :: RelationalSection ctx carrier prop -> Int
    visibleSectionBytes section =
      max 1 (Map.size (rsCarriers section))

seedInitialData ::
  (Ord ctx, Ord prop, Show evidence, Semigroup evidence) =>
  CompiledRuntimeSpec ctx prop ->
  RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr ->
  Either
    (RuntimeCreateError ctx prop)
    (RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr)
seedInitialData compiled runtime
  | patchNull patch =
      Right runtime
  | otherwise =
      first
        (RuntimeCreateSeedRejected . RuntimeApplyRejected)
        ( EnginePatch.applyInitialQuotientPatch
            (patchToQuotientPatch (Core.rsQuotientEpoch (rdrState runtime)) patch)
            runtime
        )
  where
    RuntimeInitialData patch =
      crsInitialData compiled

compileAtomBoundaries ::
  RuntimeBackend ctx prop evidence joinState joinErr ->
  IntMap.IntMap RuntimeAtomSchema ->
  Either (RuntimeBackendError ctx prop) (IntMap.IntMap RuntimeBoundary)
compileAtomBoundaries backend =
  IntMap.foldlWithKey'
    insertBoundary
    (Right IntMap.empty)
  where
    insertBoundary eitherBoundaries atomKey atomSchema = do
      boundaries <- eitherBoundaries
      boundary <-
        first
          (RuntimeBackendAtomBoundaryInvalid (mkAtomId atomKey))
          (rbAtomBoundary backend atomSchema)
      pure (IntMap.insert atomKey boundary boundaries)

compileFactorBoundaries ::
  RuntimeBackend ctx prop evidence joinState joinErr ->
  [RuntimePlan ctx prop] ->
  Either (RuntimeCreateError ctx prop) (Map (QueryId, Carrier) RuntimeBoundary)
compileFactorBoundaries backend plans =
  Map.fromList <$> traverse compileNode planNodes
  where
    planNodes =
      [ (runtimePlanQueryId plan, node, fsnmOutputSchema manifest)
      | plan <- plans,
        (node, manifest) <- factorShapeManifestNodes (fpsFactorShapeManifest (rpProgram plan)),
        factorNodeCarrierVisible node
      ]

    compileNode (queryId, node, schema) = do
      boundary <-
        first
          (RuntimeCreateBackendError . RuntimeBackendFactorBoundaryInvalid queryId node)
          (rbFactorBoundary backend queryId node schema)
      pure ((queryId, queryFactorCarrier queryId node), boundary)

compileOccurrenceAtomBoundaries ::
  RuntimeBackend ctx prop evidence joinState joinErr ->
  [RuntimePlan ctx prop] ->
  Either (RuntimeBackendError ctx prop) (Map (QueryId, Carrier) RuntimeBoundary)
compileOccurrenceAtomBoundaries backend plans =
  Map.unions <$> traverse compilePlan plans
  where
    compilePlan plan =
      IntMap.foldlWithKey'
        (insertOccurrenceBoundary (runtimePlanQueryId plan))
        (Right Map.empty)
        (runtimePlanOccurrenceAtomSchemas plan)

    insertOccurrenceBoundary queryId eitherBoundaries occurrenceKey atomSchema = do
      boundaries <- eitherBoundaries
      boundary <-
        first
          (RuntimeBackendAtomBoundaryInvalid (mkAtomId occurrenceKey))
          (rbAtomBoundary backend atomSchema)
      pure
        ( Map.insert
            (queryId, queryAtomCarrier queryId (mkAtomId occurrenceKey))
            boundary
            boundaries
        )
