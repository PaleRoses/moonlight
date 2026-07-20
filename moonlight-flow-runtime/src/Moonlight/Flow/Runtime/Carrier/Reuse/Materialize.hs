{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Runtime.Carrier.Reuse.Materialize
  ( deriveSubsumedCarrier,
    StaleCarrierReuseRetraction,
    prepareStaleCarrierReuseRetraction,
    prepareStaleInstalledReuseMaterializationRetraction,
    staleCarrierReuseRetractionContext,
    retractStaleCarrierReuseAt,
  )
where

import Data.Maybe
  ( fromMaybe,
  )
import Data.Set qualified as Set
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
    caCarrier,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    retimeRelationalCarrierPhase,
  )
import Moonlight.Flow.Carrier.Morphism.Engine
  ( CarrierReuseOps (..),
    checkedReuseSupportProject,
    runCarrierReuseMorphism,
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
  ( CarrierReuse (..),
    CarrierReuseError,
    CarrierReuseId,
    CoverageProjectionRule (..),
    ReuseWitness (..),
    carrierReuseExpectedTarget,
    carrierReuseId,
  )
import Moonlight.Flow.Carrier.Reuse
  ( InstalledReuseMaterialization (..),
    StaleCarrierReuse (..),
    recordObstructedProjection,
    removePlanReuseInstalledMaterialization,
  )
import Moonlight.Differential.Row.Patch
  ( negatePlainRowPatch,
    plainRowPatchNull,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (PhaseSubsumption),
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Flow.Runtime.Carrier.Core.Types
  ( CarrierCommitTrace,
  )
import Moonlight.Flow.Runtime.Carrier.Reuse.State
  ( dropSelectedCarrierReusesRuntime,
    lookupRuntimeCarrierReuse,
    replaceRuntimePlanReuse,
    runtimePlanReuseState,
    runtimePlanReuseTargetReuseIds,
    transformRuntimePlanReuseStats,
  )
import Moonlight.Flow.Runtime.Carrier.Store
  ( commitCarrierDelta,
    currentCarrierMaybe,
    deltaAgainstCurrent,
  )
import Moonlight.Flow.Runtime.Carrier.Store.Touch
  ( applyTouches,
  )
import Moonlight.Flow.Runtime.Carrier.Store.Write
  ( indexCarrierDelta,
  )
import Moonlight.Flow.Runtime.Error
  ( RuntimeError (..),
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
    RelationalRuntimeOpFailure (..),
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
  )

data StaleCarrierReuseRetraction ctx prop
  = NoStaleCarrierReuseRetraction
  | InstalledStaleCarrierReuseRetraction
      !(CarrierReuseId ctx prop)
      !(InstalledReuseMaterialization ctx prop)
  | DerivedStaleCarrierReuseRetraction
      !(StaleCarrierReuse ctx prop)
      !(CarrierAddr ctx Carrier prop)
  deriving stock (Eq, Show)

carrierSubsumptionPhase :: RelationalPhase
carrierSubsumptionPhase =
  PhaseSubsumption
{-# INLINE carrierSubsumptionPhase #-}

deriveSubsumedCarrier ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  RelationalCarrierTime ctx ->
  CarrierReuseId ctx prop ->
  CarrierAddr ctx Carrier prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      CarrierCommitTrace ctx prop
    )
deriveSubsumedCarrier workTime reuseId scheduledTarget runtime =
  case lookupRuntimeCarrierReuse reuseId runtime of
    Nothing ->
      missingScheduledReuseRuntime reuseId scheduledTarget runtime
    Just reuse
      | carrierReuseOneShot reuse ->
          retractReuseTargetRuntime
            workTime
            reuse
            runtime
      | otherwise -> do
          maybeSource <-
            currentCarrierMaybe
              (rwSourceCarrier (cruWitness reuse))
              runtime
          case maybeSource of
            Nothing ->
              retractReuseTargetRuntime
                workTime
                reuse
                runtime
            Just sourceSnapshot ->
              refreshReuseTargetRuntime workTime reuseId reuse sourceSnapshot runtime
{-# INLINE deriveSubsumedCarrier #-}

carrierReuseOneShot :: CarrierReuse ctx prop -> Bool
carrierReuseOneShot reuse =
  case cruCoverageRule reuse of
    ExactByCover ->
      True
    _ ->
      False
{-# INLINE carrierReuseOneShot #-}

missingScheduledReuseRuntime ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  CarrierReuseId ctx prop ->
  CarrierAddr ctx Carrier prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      CarrierCommitTrace ctx prop
    )
missingScheduledReuseRuntime reuseId scheduledTarget runtime = do
  maybeTarget <-
    currentCarrierMaybe scheduledTarget runtime
  case maybeTarget of
    Nothing ->
      Left (RuntimeOpFailure (RelationalRuntimeMissingCarrierReuse reuseId scheduledTarget))
    Just _current
      | isDerivedCarrierAddr scheduledTarget ->
          Left (RuntimeOpFailure (RelationalRuntimeDanglingDerivedCarrier scheduledTarget reuseId))
      | otherwise ->
          Left (RuntimeOpFailure (RelationalRuntimeMissingInstalledReuseMaterialization reuseId scheduledTarget))
{-# INLINE missingScheduledReuseRuntime #-}

isDerivedCarrierAddr :: CarrierAddr ctx Carrier prop -> Bool
isDerivedCarrierAddr addr =
  case caCarrier addr of
    DerivedCarrier {} ->
      True
    QueryCarrier {} ->
      False
{-# INLINE isDerivedCarrierAddr #-}

refreshReuseTargetRuntime ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  RelationalCarrierTime ctx ->
  CarrierReuseId ctx prop ->
  CarrierReuse ctx prop ->
  RelationalCarrierDelta ctx Carrier prop boundary evidence ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      CarrierCommitTrace ctx prop
    )
refreshReuseTargetRuntime workTime reuseId reuse sourceSnapshot runtime =
  case
    projectRuntimeReuse
      (runtimeCarrierReuseOps workTime)
      reuse
      sourceSnapshot
    of
      Left _projectionError ->
        retractReuseTargetRuntime
          workTime
          reuse
          (transformRuntimePlanReuseStats (recordObstructedProjection 1) runtime)
      Right projectedSnapshot0 -> do
        projectedSnapshot <-
          requireProjectedTarget reuseId reuse projectedSnapshot0
        projectedDelta <-
          deltaAgainstCurrent
            projectedSnapshot
            runtime
        (runtimeIndexed, touches) <-
          if plainRowPatchNull (deRows projectedDelta)
            then Right (runtime, [])
            else indexCarrierDelta projectedDelta runtime
        if plainRowPatchNull (deRows projectedDelta)
          then Right (runtimeIndexed, mempty)
          else applyTouches touches runtimeIndexed
{-# INLINE refreshReuseTargetRuntime #-}

retractReuseTargetRuntime ::
  (Ord ctx, Ord prop) =>
  RelationalCarrierTime ctx ->
  CarrierReuse ctx prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      CarrierCommitTrace ctx prop
    )
retractReuseTargetRuntime workTime reuse runtime0 =
  case carrierReuseExpectedTarget reuse of
    Nothing ->
      Right (runtime0, mempty)
    Just targetAddr -> do
      let runtimeDropped =
            dropSelectedCarrierReusesRuntime [carrierReuseId reuse] runtime0
      maybeTarget <-
        currentCarrierMaybe
          targetAddr
          runtimeDropped
      case maybeTarget of
        Nothing ->
          Right (runtimeDropped, mempty)
        Just currentTarget ->
          let retraction =
                currentTarget
                  { deTime =
                      retimeRelationalCarrierPhase carrierSubsumptionPhase workTime,
                    deRows =
                      negatePlainRowPatch (deRows currentTarget)
                  }
           in commitCarrierDelta retraction runtimeDropped
{-# INLINE retractReuseTargetRuntime #-}

prepareStaleCarrierReuseRetraction ::
  (Ord ctx, Ord prop) =>
  StaleCarrierReuse ctx prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      StaleCarrierReuseRetraction ctx prop
    )
prepareStaleCarrierReuseRetraction stale runtime =
  case scrExpectedTarget stale of
    Nothing ->
      Right (runtime, NoStaleCarrierReuseRetraction)
    Just target ->
      let (maybeInstalled, planReuse') =
            removePlanReuseInstalledMaterialization
              (scrReuseId stale)
              (runtimePlanReuseState runtime)
          runtimeWithoutInstalledMaterialization =
            replaceRuntimePlanReuse planReuse' runtime
       in case maybeInstalled of
            Just installed ->
              Right
                ( runtimeWithoutInstalledMaterialization,
                  InstalledStaleCarrierReuseRetraction (scrReuseId stale) installed
                )
            Nothing ->
              prepareDerivedReuseRetraction
                stale
                target
                runtimeWithoutInstalledMaterialization
{-# INLINE prepareStaleCarrierReuseRetraction #-}

prepareStaleInstalledReuseMaterializationRetraction ::
  (Ord ctx, Ord prop) =>
  (CarrierReuseId ctx prop, InstalledReuseMaterialization ctx prop) ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      StaleCarrierReuseRetraction ctx prop
    )
prepareStaleInstalledReuseMaterializationRetraction (reuseId, selectedInstalled) runtime =
  let (maybeInstalled, planReuse') =
        removePlanReuseInstalledMaterialization
          reuseId
          (runtimePlanReuseState runtime)
      runtimeWithoutInstalledMaterialization =
        replaceRuntimePlanReuse planReuse' runtime
   in Right
        ( runtimeWithoutInstalledMaterialization,
          InstalledStaleCarrierReuseRetraction reuseId (fromMaybe selectedInstalled maybeInstalled)
        )
{-# INLINE prepareStaleInstalledReuseMaterializationRetraction #-}

prepareDerivedReuseRetraction ::
  (Ord ctx, Ord prop) =>
  StaleCarrierReuse ctx prop ->
  CarrierAddr ctx Carrier prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      StaleCarrierReuseRetraction ctx prop
    )
prepareDerivedReuseRetraction stale target runtime =
  case caCarrier target of
    QueryCarrier {} ->
      Right (runtime, NoStaleCarrierReuseRetraction)
    DerivedCarrier {} ->
      if Set.size owners <= 1
        then Right (runtime, DerivedStaleCarrierReuseRetraction stale target)
        else Left (RuntimeOpFailure (RelationalRuntimeDerivedCarrierMultipleOwners target owners))
  where
    owners =
      runtimePlanReuseTargetReuseIds target runtime
{-# INLINE prepareDerivedReuseRetraction #-}

staleCarrierReuseRetractionContext ::
  StaleCarrierReuseRetraction ctx prop ->
  Maybe ctx
staleCarrierReuseRetractionContext retraction =
  case retraction of
    NoStaleCarrierReuseRetraction ->
      Nothing
    InstalledStaleCarrierReuseRetraction _reuseId installed ->
      Just (caContext (irmTarget installed))
    DerivedStaleCarrierReuseRetraction _stale target ->
      Just (caContext target)
{-# INLINE staleCarrierReuseRetractionContext #-}

retractStaleCarrierReuseAt ::
  (Ord ctx, Ord prop) =>
  RelationalCarrierTime ctx ->
  StaleCarrierReuseRetraction ctx prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      CarrierCommitTrace ctx prop
    )
retractStaleCarrierReuseAt workTime retraction runtime =
  case retraction of
    NoStaleCarrierReuseRetraction ->
      Right (runtime, mempty)
    InstalledStaleCarrierReuseRetraction reuseId installed ->
      retractInstalledReuseMaterializationRuntime workTime reuseId installed runtime
    DerivedStaleCarrierReuseRetraction stale _target ->
      retractReuseTargetRuntime workTime (scrReuse stale) runtime
{-# INLINE retractStaleCarrierReuseAt #-}

retractInstalledReuseMaterializationRuntime ::
  (Ord ctx, Ord prop) =>
  RelationalCarrierTime ctx ->
  CarrierReuseId ctx prop ->
  InstalledReuseMaterialization ctx prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      CarrierCommitTrace ctx prop
    )
retractInstalledReuseMaterializationRuntime workTime reuseId installed runtime0 = do
  let target =
        irmTarget installed
  maybeTarget <-
    currentCarrierMaybe target runtime0
  currentTarget <-
    case maybeTarget of
      Nothing ->
        Left (RuntimeOpFailure (RelationalRuntimeMissingInstalledReuseMaterialization reuseId target))
      Just current ->
        Right current
  let retraction =
        currentTarget
          { deTime = retimeRelationalCarrierPhase carrierSubsumptionPhase workTime,
            deRows = negatePlainRowPatch (irmRows installed)
          }
  commitCarrierDelta retraction runtime0
{-# INLINE retractInstalledReuseMaterializationRuntime #-}

runtimeCarrierReuseOps ::
  RelationalCarrierTime ctx ->
  CarrierReuseOps ctx prop evidence
runtimeCarrierReuseOps eventTime =
  CarrierReuseOps
    { croEventTime = eventTime,
      croEvidenceOf = \_witness _rule _boundary evidence -> Right evidence,
      croSupportProject = checkedReuseSupportProject
    }
{-# INLINE runtimeCarrierReuseOps #-}

projectRuntimeReuse ::
  (Ord ctx, Ord prop) =>
  CarrierReuseOps ctx prop evidence ->
  CarrierReuse ctx prop ->
  RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence ->
  Either
    (CarrierReuseError ctx prop evidence)
    (RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence)
projectRuntimeReuse =
  runCarrierReuseMorphism
{-# INLINE projectRuntimeReuse #-}

requireProjectedTarget ::
  (Eq ctx, Eq prop) =>
  CarrierReuseId ctx prop ->
  CarrierReuse ctx prop ->
  RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence ->
  Either
    (RelationalRuntimeError ctx prop RuntimeBoundary evidence)
    (RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence)
requireProjectedTarget reuseId reuse projectedSnapshot =
  case carrierReuseExpectedTarget reuse of
    Nothing ->
      Left (RuntimeOpFailure (RelationalRuntimeSubsumptionTargetMismatch reuseId (deAddr projectedSnapshot)))
    Just expectedTarget
      | deAddr projectedSnapshot == expectedTarget ->
          Right projectedSnapshot
      | otherwise ->
          Left (RuntimeOpFailure (RelationalRuntimeSubsumptionTargetMismatch reuseId (deAddr projectedSnapshot)))
{-# INLINE requireProjectedTarget #-}
