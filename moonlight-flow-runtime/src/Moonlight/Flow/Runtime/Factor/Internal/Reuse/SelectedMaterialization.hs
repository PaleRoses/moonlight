{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Runtime.Factor.Internal.Reuse.SelectedMaterialization
  ( tryExactEquivalentReuse,
    tryExactByCoverContainmentReuse,
    tryLowerBoundContainmentReuse,
    SelectedMaterializationAlgebra (..),
    runtimeReuseConfig,
    planSelectedMaterialization,
    selectCandidateGroupMaterialization,
    selectedProjectedMaterialization,
  )
where

import Data.Foldable qualified as Foldable
import Data.Maybe
  ( catMaybes,
  )
import Data.Traversable qualified as Traversable
import Moonlight.Core
  ( QueryId,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )
import Moonlight.Flow.Carrier.Morphism.Engine
  ( CarrierReuseOps (..),
    checkedReuseSupportProject,
    runCarrierReuseMorphism,
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
  ( CarrierReuse (..),
    CarrierReuseError,
    ReuseWitness (..),
  )
import Moonlight.Flow.Carrier.Reuse
  ( CarrierReuseCandidateGroup (..),
    CarrierReuseStrategy (..),
    PlanReuseError (..),
    PlanReuseRequest,
    RequestedFactorShape (..),
    ReuseConfig (..),
    lookupReusableCarrierEntry,
    planCarrierReuseStrategy,
  )
import Moonlight.Flow.Execution.Subsumption.FactorShape
  ( factorShapeManifestNodes,
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchNull,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Flow.Runtime.Carrier.Emit
  ( FactorCarrierEmitSpec,
  )
import Moonlight.Flow.Runtime.Carrier.Reuse.CoverMaterialization
  ( CoverMaterializationError (..),
    CoverMaterializationPlan,
    mkCoverMaterializationPlan,
  )
import Moonlight.Flow.Runtime.Carrier.Reuse.State
  ( currentCarrierLookupE,
    recordRuntimeReuseProjectionRejection,
    replaceRuntimePlanReuse,
    runtimePlanReuseState,
  )
import Moonlight.Flow.Runtime.Carrier.Store
  ( currentCarrierMaybe,
    deltaAgainstCurrent,
  )
import Moonlight.Flow.Runtime.Error
  ( RuntimeError (..),
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
    RelationalRuntimeOpFailure (..),
  )
import Moonlight.Flow.Runtime.Factor.Internal.Reuse.Result
  ( ExactByCoverReuseResult (..),
    ExactReuseResult (..),
    FactorReuseMaterialization (..),
    LowerBoundReuseResult (..),
  )
import Moonlight.Flow.Runtime.Factor.Internal.Reuse.Target
  ( planReuseRequestsForManifestNodes,
    visibleFactorShapeManifestNodes,
  )
import Moonlight.Flow.Runtime.Factor.Program
  ( FactorProgram (..),
    factorProgramFactorShapeManifest,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
    RuntimeEnv (..),
    rsGeneratedSite,
  )

data SelectedMaterializationAlgebra ctx prop boundary evidence joinState joinErr materialization
  = SelectedMaterializationAlgebra
      { smaStrategy :: !CarrierReuseStrategy,
        smaCandidateMaterialization ::
          RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
          CarrierReuseCandidateGroup ctx prop ->
          CarrierReuse ctx prop ->
          Either
            (RelationalRuntimeError ctx prop boundary evidence)
            (RelDiffRuntime ctx prop boundary evidence joinState joinErr, Maybe materialization)
      }

tryExactEquivalentReuse ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  FactorCarrierEmitSpec ctx prop boundary evidence ->
  RelationalCarrierTime ctx ->
  QueryId ->
  FactorProgram ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr, Maybe (ExactReuseResult ctx prop boundary evidence))
tryExactEquivalentReuse spec eventTime queryId program runtime0 = do
  requests <-
    planReuseRequestsForManifestNodes
      spec
      eventTime
      queryId
      program
      (visibleFactorShapeManifestNodes program)
      runtime0
  (runtime1, maybeMaterializations) <-
    Traversable.mapAccumM
      (planSelectedMaterialization queryId (exactEquivalentAlgebra eventTime))
      runtime0
      requests
  case sequence maybeMaterializations of
    Nothing ->
      Right (runtime1, Nothing)
    Just materializations ->
      Right
        ( runtime1,
          Just
            ExactReuseResult
              { errSnapshots = fmap frumProjectedSnapshot materializations,
                errDeltas =
                  filter
                    (not . plainRowPatchNull . deRows)
                    (fmap frumProjectedDelta materializations)
              }
        )

tryExactByCoverContainmentReuse ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  FactorCarrierEmitSpec ctx prop boundary evidence ->
  RelationalCarrierTime ctx ->
  QueryId ->
  FactorProgram ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr, Maybe (ExactByCoverReuseResult ctx prop boundary evidence))
tryExactByCoverContainmentReuse spec eventTime queryId program runtime0 = do
  requests <-
    planReuseRequestsForManifestNodes
      spec
      eventTime
      queryId
      program
      (visibleFactorShapeManifestNodes program)
      runtime0
  (runtime1, maybePlans) <-
    Traversable.mapAccumM
      (planSelectedMaterialization queryId (exactByCoverAlgebra eventTime))
      runtime0
      requests
  case sequence maybePlans of
    Nothing ->
      Right (runtime1, Nothing)
    Just plans ->
      Right
        ( runtime1,
          Just
            ExactByCoverReuseResult
              { ebcrPlans = plans
              }
        )

tryLowerBoundContainmentReuse ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  FactorCarrierEmitSpec ctx prop boundary evidence ->
  RelationalCarrierTime ctx ->
  QueryId ->
  FactorProgram ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr, Maybe (LowerBoundReuseResult ctx prop boundary evidence))
tryLowerBoundContainmentReuse spec eventTime queryId program runtime0 = do
  requests <-
    planReuseRequestsForManifestNodes
      spec
      eventTime
      queryId
      program
      (factorShapeManifestNodes (factorProgramFactorShapeManifest program))
      runtime0
  (runtime1, maybeMaterializations) <-
    Traversable.mapAccumM
      (planSelectedMaterialization queryId (lowerBoundAlgebra eventTime))
      runtime0
      requests
  let materializations = catMaybes maybeMaterializations
  if null materializations
    then Right (runtime1, Nothing)
    else
      Right
        ( runtime1,
          Just
            LowerBoundReuseResult
              { lbrrMaterializations = materializations
              }
        )

runtimeReuseConfig ::
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  ReuseConfig
runtimeReuseConfig runtime =
  ReuseConfig
    { rcMode = reReuseMode (rdrEnv runtime),
      rcMaxContainmentCandidates = maxBound
    }

planSelectedMaterialization ::
  (Ord ctx, Ord prop) =>
  QueryId ->
  SelectedMaterializationAlgebra ctx prop boundary evidence joinState joinErr materialization ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  PlanReuseRequest ctx prop ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr, Maybe materialization)
planSelectedMaterialization queryId algebra runtime0 request = do
  (planReuse, group) <-
    firstPlanError $
      planCarrierReuseStrategy
        (runtimeReuseConfig runtime0)
        (smaStrategy algebra)
        request
        (runtimePlanReuseState runtime0)
  selectCandidateGroupMaterialization
    algebra
    (replaceRuntimePlanReuse planReuse runtime0)
    group
  where
    firstPlanError =
      either (Left . runtimePlanReuseSelectionError queryId) Right

runtimePlanReuseSelectionError ::
  QueryId ->
  PlanReuseError ctx prop ->
  RelationalRuntimeError ctx prop boundary evidence
runtimePlanReuseSelectionError queryId planError =
  case planError of
    ReuseNormalizeFailed normalizationError ->
      RuntimeOpFailure (RelationalRuntimeSubsumptionRegistrationFailed queryId normalizationError)
    _ ->
      RuntimeOpFailure (RelationalRuntimePlanReuseInstallFailed planError)

selectCandidateGroupMaterialization ::
  SelectedMaterializationAlgebra ctx prop boundary evidence joinState joinErr materialization ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  CarrierReuseCandidateGroup ctx prop ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr, Maybe materialization)
selectCandidateGroupMaterialization algebra runtime group =
  selectMaterializationFromCandidates algebra group (crcgCandidates group) runtime

selectMaterializationFromCandidates ::
  SelectedMaterializationAlgebra ctx prop boundary evidence joinState joinErr materialization ->
  CarrierReuseCandidateGroup ctx prop ->
  [CarrierReuse ctx prop] ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr, Maybe materialization)
selectMaterializationFromCandidates algebra group =
  foldr
    (selectMaterializationCandidate algebra group)
    noSelectedMaterialization

selectMaterializationCandidate ::
  SelectedMaterializationAlgebra ctx prop boundary evidence joinState joinErr materialization ->
  CarrierReuseCandidateGroup ctx prop ->
  CarrierReuse ctx prop ->
  ( RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
    Either
      (RelationalRuntimeError ctx prop boundary evidence)
      (RelDiffRuntime ctx prop boundary evidence joinState joinErr, Maybe materialization)
  ) ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr, Maybe materialization)
selectMaterializationCandidate algebra group candidate rest activeRuntime = do
  (runtime1, maybeMaterialization) <-
    smaCandidateMaterialization algebra activeRuntime group candidate
  case maybeMaterialization of
    Just materialization ->
      Right (runtime1, Just materialization)
    Nothing ->
      rest runtime1

noSelectedMaterialization ::
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr, Maybe materialization)
noSelectedMaterialization activeRuntime =
  Right (activeRuntime, Nothing)

selectedProjectedMaterialization ::
  (Ord ctx, Ord prop) =>
  RelationalCarrierTime ctx ->
  ( CarrierReuseError ctx prop evidence ->
    RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr ->
    RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr
  ) ->
  (CarrierReuseCandidateGroup ctx prop -> FactorReuseMaterialization ctx prop RuntimeBoundary evidence -> Bool) ->
  RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr ->
  CarrierReuseCandidateGroup ctx prop ->
  CarrierReuse ctx prop ->
  Either
    (RelationalRuntimeError ctx prop RuntimeBoundary evidence)
    ( RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr,
      Maybe (FactorReuseMaterialization ctx prop RuntimeBoundary evidence)
    )
selectedProjectedMaterialization eventTime recordProjectionRejection accept runtime group reuse = do
  maybeSource <-
    currentCarrierMaybe
      (rwSourceCarrier (cruWitness reuse))
      runtime
  case maybeSource of
    Nothing ->
      Right (runtime, Nothing)
    Just sourceSnapshot ->
      case
        runCarrierReuseMorphism
          (carrierReuseOps eventTime)
          reuse
          sourceSnapshot
      of
        Left projectionRejected ->
          Right (recordProjectionRejection projectionRejected runtime, Nothing)
        Right projectedSnapshot -> do
          projectedDelta <-
            deltaAgainstCurrent projectedSnapshot runtime
          let !materialization =
                FactorReuseMaterialization
                  { frumReuse = reuse,
                    frumSourceSnapshot = sourceSnapshot,
                    frumProjectedSnapshot = projectedSnapshot,
                    frumProjectedDelta = projectedDelta
                  }
          Right
            ( runtime,
              if accept group materialization
                then Just materialization
                else Nothing
            )

carrierReuseOps ::
  RelationalCarrierTime ctx ->
  CarrierReuseOps ctx prop evidence
carrierReuseOps eventTime =
  CarrierReuseOps
    { croEventTime = eventTime,
      croEvidenceOf = \_witness _rule _boundary evidence -> Right evidence,
      croSupportProject = checkedReuseSupportProject
    }

exactEquivalentAlgebra ::
  (Ord ctx, Ord prop) =>
  RelationalCarrierTime ctx ->
  SelectedMaterializationAlgebra ctx prop RuntimeBoundary evidence joinState joinErr (FactorReuseMaterialization ctx prop RuntimeBoundary evidence)
exactEquivalentAlgebra eventTime =
  SelectedMaterializationAlgebra
    { smaStrategy = ReuseExactEquivalent,
      smaCandidateMaterialization =
        selectedProjectedMaterialization
          eventTime
          (\_projectionRejected -> id)
          exactEquivalentAccepts
    }

exactByCoverAlgebra ::
  (Ord ctx, Ord prop) =>
  RelationalCarrierTime ctx ->
  SelectedMaterializationAlgebra ctx prop RuntimeBoundary evidence joinState joinErr (CoverMaterializationPlan ctx prop evidence)
exactByCoverAlgebra eventTime =
  SelectedMaterializationAlgebra
    { smaStrategy = ReuseExactByCover,
      smaCandidateMaterialization = exactByCoverCandidateMaterialization eventTime
    }

lowerBoundAlgebra ::
  (Ord ctx, Ord prop) =>
  RelationalCarrierTime ctx ->
  SelectedMaterializationAlgebra ctx prop RuntimeBoundary evidence joinState joinErr (FactorReuseMaterialization ctx prop RuntimeBoundary evidence)
lowerBoundAlgebra eventTime =
  SelectedMaterializationAlgebra
    { smaStrategy = ReuseLowerBound,
      smaCandidateMaterialization =
        selectedProjectedMaterialization
          eventTime
          recordRuntimeReuseProjectionRejection
          (\_group _materialization -> True)
    }

exactByCoverCandidateMaterialization ::
  (Ord ctx, Ord prop) =>
  RelationalCarrierTime ctx ->
  RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr ->
  CarrierReuseCandidateGroup ctx prop ->
  CarrierReuse ctx prop ->
  Either
    (RelationalRuntimeError ctx prop RuntimeBoundary evidence)
    (RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr, Maybe (CoverMaterializationPlan ctx prop evidence))
exactByCoverCandidateMaterialization eventTime activeRuntime group candidateReuse =
  case lookupReusableCarrierEntry sourceCarrier (runtimePlanReuseState activeRuntime) of
    Nothing ->
      Right (activeRuntime, Nothing)
    Just sourceEntry ->
      case
        mkCoverMaterializationPlan
          (rsGeneratedSite (rdrState activeRuntime))
          (runtimePlanReuseState activeRuntime)
          (currentCarrierLookupE activeRuntime)
          eventTime
          (crcgRequested group)
          sourceEntry
          candidateReuse
      of
        Left errors ->
          Right (recordCoverMaterializationErrors errors activeRuntime, Nothing)
        Right plan ->
          Right (activeRuntime, Just plan)
  where
    sourceCarrier = rwSourceCarrier (cruWitness candidateReuse)

recordCoverMaterializationErrors ::
  (Foldable f, Ord ctx, Ord prop) =>
  f (CoverMaterializationError ctx prop evidence) ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr
recordCoverMaterializationErrors errors runtime =
  Foldable.foldl' recordOne runtime errors
  where
    recordOne ::
      (Ord ctx, Ord prop) =>
      RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
      CoverMaterializationError ctx prop evidence ->
      RelDiffRuntime ctx prop boundary evidence joinState joinErr
    recordOne acc err =
      case err of
        CoverReuseProjectionRejected projectionError ->
          recordRuntimeReuseProjectionRejection projectionError acc
        _ ->
          acc

exactEquivalentAccepts ::
  (Eq ctx, Eq prop) =>
  CarrierReuseCandidateGroup ctx prop ->
  FactorReuseMaterialization ctx prop RuntimeBoundary evidence ->
  Bool
exactEquivalentAccepts group materialization =
  rwSourceCarrier (cruWitness (frumReuse materialization)) /= rfsTargetCarrier (crcgRequested group)
    || plainRowPatchNull (deRows (frumProjectedDelta materialization))
