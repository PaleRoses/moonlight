{-# LANGUAGE TypeFamilies #-}

module Moonlight.Flow.Runtime.Factor.Internal.Commit
  ( ExactFactorRepairCommit (..),
    commitFactorReuseAction,
    commitExactFactorRepairResults,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( mapMaybe,
  )
import Moonlight.Core
  ( QueryId,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    QueryCarrierNode (..),
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caCarrier,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
    rowDeltaDigest,
  )
import Moonlight.Flow.Carrier.Core.Delta.Emit
  ( CarrierEmitSpec (..),
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
  )
import Moonlight.Flow.Carrier.Morphism.Subsumption
  ( CarrierReuse (..),
    carrierReuseId,
  )
import Moonlight.Flow.Carrier.Reuse
  ( InstalledReuseMaterialization (..),
    PlanReuseRegistrationEntry (..),
    PlanReuseState,
    PlanReuseStats (..),
    installCarrierReuse,
    installPlanReuseMaterialization,
    mapPlanReuseStats,
    recordContainmentReuseEmits,
    recordExactReuseEmits,
    registerCarrierReuses,
    registerFactorCarrierShapes,
  )
import Moonlight.Flow.Execution.Subsumption.FactorShape
  ( FactorShapeManifest,
    FactorShapeNodeManifest (..),
    lookupFactorShapeManifestNode,
  )
import Moonlight.Differential.Row.Patch
  ( emptyPlainRowPatch,
    plainRowPatchNull,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
    boundaryDigest,
  )
import Moonlight.Flow.Model.Schema.Digest
  ( StableDigest128,
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope,
    relationalScopeFromSets,
  )
import Moonlight.Flow.Plan.Query.Core
  ( FactorNode,
    mkAtomId,
  )
import Moonlight.Flow.Plan.Shape
  ( CanonicalizationResult (..),
  )
import Moonlight.Flow.Runtime.Carrier.Core.Types
  ( CarrierCommitTrace,
  )
import Moonlight.Flow.Runtime.Carrier.Emit
  ( FactorCarrierEmitSpec,
    FactorCarrierPayload (..),
  )
import Moonlight.Flow.Runtime.Carrier.Reuse.CoverMaterialization
  ( CoverMaterializationPlan (..),
  )
import Moonlight.Flow.Runtime.Factor.Input
  ( factorInputSignatureRuntime,
  )
import Moonlight.Flow.Runtime.Factor.Internal.Exact
  ( ExactFactorRepairResult (..),
  )
import Moonlight.Flow.Runtime.Factor.Internal.Reuse.Result
  ( FactorReuseMaterialization (..),
  )
import Moonlight.Flow.Runtime.Carrier.Reuse.State
  ( replaceRuntimePlanReuse,
    runtimePlanReuseState,
  )
import Moonlight.Flow.Runtime.Factor.Program
  ( FactorProgram (..),
    factorProgramCanonical,
    factorProgramFactorShapeManifest,
  )
import Moonlight.Flow.Runtime.Spec.FactorProgram
  ( RepairProgramKey,
  )
import Moonlight.Flow.Runtime.Factor.Request
  ( FactorRepairCause,
    FactorRepairRequest (..),
  )
import Moonlight.Flow.Runtime.Factor.Reuse
  ( FactorRepairReport (..),
    FactorReuseAction (..),
    FactorReuseKind (..),
    FactorRepairActionSummary (..),
  )
import Moonlight.Flow.Runtime.Factor.State
  ( appendFactorRepairStats,
    clearFactorProgramCacheForCause,
    installFactorPrograms,
  )
import Moonlight.Flow.Runtime.Carrier.Store.Touch
  ( applyTouches,
  )
import Moonlight.Flow.Runtime.Carrier.Store.Write
  ( indexCarrierDeltas,
  )
import Moonlight.Flow.Runtime.Core.Patch.Validation
  ( CanonicalityOracle (..),
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
    RelationalRuntimeOpFailure (..),
  )
import Moonlight.Flow.Runtime.Core.RepairStats
  ( RuntimeRepairStats,
    carrierDeltaRowCount,
    emptyRuntimeRepairStats,
    runtimeRepairStatsFromMaintenance,
  )

data FactorCommitResult ctx prop boundary evidence joinState joinErr = FactorCommitResult
  { fcrRuntime :: !(RelDiffRuntime ctx prop boundary evidence joinState joinErr),
    fcrCommitTrace :: !(CarrierCommitTrace ctx prop),
    fcrEmittedDeltaCount :: {-# UNPACK #-} !Int,
    fcrRegisteredNodeCount :: {-# UNPACK #-} !Int,
    fcrStats :: !RuntimeRepairStats
  }

data ExactFactorRepairCommit ctx prop boundary evidence joinState joinErr = ExactFactorRepairCommit
  { efrcRequest :: !(FactorRepairRequest ctx prop),
    efrcSubscriberCount :: {-# UNPACK #-} !Int,
    efrcResult :: !(ExactFactorRepairResult ctx prop boundary evidence joinState joinErr)
  }

commitFactorReuseAction ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  FactorRepairRequest ctx prop ->
  FactorProgram ->
  FactorReuseAction ctx prop boundary evidence ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( FactorRepairReport ctx prop boundary evidence,
      RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      CarrierCommitTrace ctx prop
    )
commitFactorReuseAction request program reuseAction runtime = do
  result <-
    commitReuseAction
      (frrRepairKey request)
      (frrQueryId request)
      (frrCause request)
      program
      reuseAction
      runtime
  let report =
        FactorRepairReport
          { frrpQueryId = frrQueryId request,
            frrpCause = frrCause request,
            frrpAction =
              case fruaKind reuseAction of
                FactorReuseExactEquivalent ->
                  FactorRepairUsedExactEquivalent
                FactorReuseExactByCover ->
                  FactorRepairUsedExactByCover
                FactorReuseLowerBound ->
                  FactorRepairUsedLowerBound,
            frrpEmittedDeltaCount = fcrEmittedDeltaCount result,
            frrpRegisteredNodeCount = fcrRegisteredNodeCount result,
            frrpStats = fcrStats result
          }
  pure (report, fcrRuntime result, fcrCommitTrace result)
{-# INLINE commitFactorReuseAction #-}

commitReuseAction ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  RepairProgramKey ->
  QueryId ->
  FactorRepairCause ->
  FactorProgram ->
  FactorReuseAction ctx prop boundary evidence ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (FactorCommitResult ctx prop boundary evidence joinState joinErr)
commitReuseAction repairKey queryId cause program reuseAction runtime =
  case fruaKind reuseAction of
    FactorReuseExactEquivalent ->
      commitExactEquivalentReuse
        repairKey
        queryId
        cause
        program
        (fruaSnapshots reuseAction)
        (fruaDeltas reuseAction)
        runtime
    FactorReuseExactByCover ->
      commitExactByCoverReuse
        repairKey
        queryId
        cause
        program
        (fruaCoverPlans reuseAction)
        runtime
    FactorReuseLowerBound ->
      commitLowerBoundReuse
        repairKey
        queryId
        cause
        (fruaMaterializations reuseAction)
        runtime
{-# INLINE commitReuseAction #-}

commitExactEquivalentReuse ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  RepairProgramKey ->
  QueryId ->
  FactorRepairCause ->
  FactorProgram ->
  [RelationalCarrierDelta ctx Carrier prop boundary evidence] ->
  [RelationalCarrierDelta ctx Carrier prop boundary evidence] ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (FactorCommitResult ctx prop boundary evidence joinState joinErr)
commitExactEquivalentReuse repairKey queryId cause program snapshots deltas runtime0 = do
  let runtimeStateSafe =
        clearFactorProgramCacheForCause repairKey cause runtime0
  (runtimeIndexed, touches) <-
    indexCarrierDeltas
      deltas
      runtimeStateSafe
  let registrationEntries =
        factorRegistrationEntriesFromSnapshots
          runtimeIndexed
          program
          snapshots
  planReuse1 <-
    registerFactorCarrierShapesRuntime
      queryId
      program
      registrationEntries
      (mapPlanReuseStats (recordExactReuseEmits (length deltas)) (runtimePlanReuseState runtimeIndexed))
      runtimeIndexed
  (runtimeTouched, commitTrace) <-
    applyTouches
      touches
      (replaceRuntimePlanReuse planReuse1 runtimeIndexed)
  pure
    FactorCommitResult
      { fcrRuntime = runtimeTouched,
        fcrCommitTrace = commitTrace,
        fcrEmittedDeltaCount = length deltas,
        fcrRegisteredNodeCount = length registrationEntries,
        fcrStats = emptyRuntimeRepairStats
      }
{-# INLINE commitExactEquivalentReuse #-}

commitExactByCoverReuse ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  RepairProgramKey ->
  QueryId ->
  FactorRepairCause ->
  FactorProgram ->
  [CoverMaterializationPlan ctx prop evidence] ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (FactorCommitResult ctx prop boundary evidence joinState joinErr)
commitExactByCoverReuse repairKey queryId cause program plans runtime0 = do
  let runtimeStateSafe =
        clearFactorProgramCacheForCause repairKey cause runtime0
      (materializationDeltas, planReuseInstalled) =
        installCoverMaterializationPlans
          plans
          (mapPlanReuseStats (recordExactReuseEmits 0) (runtimePlanReuseState runtimeStateSafe))
      runtimeInstalled =
        replaceRuntimePlanReuse planReuseInstalled runtimeStateSafe
      nonEmptyMaterializationDeltas =
        filter (not . plainRowPatchNull . deRows) materializationDeltas
  (runtimeIndexed, touches) <-
    indexCarrierDeltas
      nonEmptyMaterializationDeltas
      runtimeInstalled
  let registrationEntries =
        fmap cmpRegistration plans
  planReuseShaped <-
    registerFactorCarrierShapesRuntime
      queryId
      program
      registrationEntries
      ( mapPlanReuseStats
          ( \stats ->
              stats
                { prsExactProjectionEmits =
                    prsExactProjectionEmits stats + length nonEmptyMaterializationDeltas
                }
          )
          (runtimePlanReuseState runtimeIndexed)
      )
      runtimeIndexed
  (runtimeTouched, commitTrace) <-
    applyTouches
      touches
      (replaceRuntimePlanReuse planReuseShaped runtimeIndexed)
  pure
    FactorCommitResult
      { fcrRuntime = runtimeTouched,
        fcrCommitTrace = commitTrace,
        fcrEmittedDeltaCount = length nonEmptyMaterializationDeltas,
        fcrRegisteredNodeCount = length registrationEntries,
        fcrStats = emptyRuntimeRepairStats
      }
{-# INLINE commitExactByCoverReuse #-}

installCoverMaterializationPlans ::
  (Ord ctx, Ord prop) =>
  [CoverMaterializationPlan ctx prop evidence] ->
  PlanReuseState ctx prop ->
  ([RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence], PlanReuseState ctx prop)
installCoverMaterializationPlans plans state0 =
  Foldable.foldl'
    step
    ([], state0)
    plans
  where
    step ::
      (Ord ctx, Ord prop) =>
      ([RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence], PlanReuseState ctx prop) ->
      CoverMaterializationPlan ctx prop evidence ->
      ([RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence], PlanReuseState ctx prop)
    step (deltas, planReuse) plan =
      let (_rowsDelta, planReuse') =
            installPlanReuseMaterialization (cmpInstalled plan) planReuse
       in (cmpProjectedDelta plan : deltas, planReuse')
{-# INLINE installCoverMaterializationPlans #-}

commitLowerBoundReuse ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  RepairProgramKey ->
  QueryId ->
  FactorRepairCause ->
  [FactorReuseMaterialization ctx prop boundary evidence] ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (FactorCommitResult ctx prop boundary evidence joinState joinErr)
commitLowerBoundReuse repairKey _queryId cause materializations0 runtime0 = do
  let materializations =
        filter
          (not . plainRowPatchNull . deRows . frumProjectedDelta)
          materializations0
      reuses =
        fmap frumReuse materializations
      runtimeStateSafe =
        clearFactorProgramCacheForCause repairKey cause runtime0
      planReuseRegistered =
        registerCarrierReuses
          reuses
          ( mapPlanReuseStats
              (recordContainmentReuseEmits (length materializations))
              (runtimePlanReuseState runtimeStateSafe)
          )
      runtimeRegistered =
        replaceRuntimePlanReuse planReuseRegistered runtimeStateSafe
  (materializationDeltas, planReuseInstalled) <-
    registerInstalledReuseMaterializations
      materializations
      (runtimePlanReuseState runtimeRegistered)
  let deltas =
        filter (not . plainRowPatchNull . deRows) materializationDeltas
      runtimeInstalled =
        replaceRuntimePlanReuse planReuseInstalled runtimeRegistered
  (runtimeIndexed, touches) <-
    indexCarrierDeltas
      deltas
      runtimeInstalled
  (runtimeTouched, commitTrace) <-
    applyTouches
      touches
      runtimeIndexed
  pure
    FactorCommitResult
      { fcrRuntime = runtimeTouched,
        fcrCommitTrace = commitTrace,
        fcrEmittedDeltaCount = length deltas,
        fcrRegisteredNodeCount = 0,
        fcrStats = emptyRuntimeRepairStats
      }
{-# INLINE commitLowerBoundReuse #-}

registerInstalledReuseMaterializations ::
  (Ord ctx, Ord prop) =>
  [FactorReuseMaterialization ctx prop RuntimeBoundary evidence] ->
  PlanReuseState ctx prop ->
  Either
    (RelationalRuntimeError ctx prop RuntimeBoundary evidence)
    ([RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence], PlanReuseState ctx prop)
registerInstalledReuseMaterializations materializations state0 =
  foldM
    step
    ([], state0)
    materializations
  where
    step ::
      (Ord ctx, Ord prop) =>
      ( [RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence],
        PlanReuseState ctx prop
      ) ->
      FactorReuseMaterialization ctx prop RuntimeBoundary evidence ->
      Either
        (RelationalRuntimeError ctx prop RuntimeBoundary evidence)
        ( [RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence],
          PlanReuseState ctx prop
        )
    step
      (deltas, planReuse)
      materialization =
        let projected =
              frumProjectedSnapshot materialization
            reuse =
              frumReuse materialization
            installed =
              InstalledReuseMaterialization
                { irmReuseId = carrierReuseId reuse,
                  irmTarget = deAddr projected,
                  irmRows = deRows projected,
                  irmBoundaryDigest = boundaryDigest (deBoundary projected),
                  irmSourceCurrentDigest =
                    rowDeltaDigest (deRows (frumSourceSnapshot materialization)),
                  irmDeps = cruWitnessDeps reuse,
                  irmTopo = cruWitnessTopo reuse
                }
         in case installCarrierReuse installed planReuse of
              Left installError ->
                Left (RuntimeOpFailure (RelationalRuntimePlanReuseInstallFailed installError))
              Right (rowsDelta, planReuse') ->
                let delta =
                      projected {deRows = rowsDelta}
                 in Right (delta : deltas, planReuse')
{-# INLINE registerInstalledReuseMaterializations #-}

registerFactorCarrierShapesRuntime ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  QueryId ->
  FactorProgram ->
  [PlanReuseRegistrationEntry ctx prop] ->
  PlanReuseState ctx prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (PlanReuseState ctx prop)
registerFactorCarrierShapesRuntime queryId program entries planReuse runtime = do
  inputDigest <-
    factorInputSignatureRuntime
      (reAtomCarrierEmitSpec (rdrEnv runtime))
      queryId
      program
      runtime
  case
    registerFactorCarrierShapes
      queryId
      (factorProgramCanonical program)
      (factorProgramFactorShapeManifest program)
      inputDigest
      entries
      planReuse
    of
      Left registrationError ->
        Left (RuntimeOpFailure (RelationalRuntimeSubsumptionRegistrationFailed queryId registrationError))
      Right planReuse' ->
        Right planReuse'
{-# INLINE registerFactorCarrierShapesRuntime #-}

factorRegistrationEntriesFromSnapshots ::
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  FactorProgram ->
  [RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence] ->
  [PlanReuseRegistrationEntry ctx prop]
factorRegistrationEntriesFromSnapshots runtime program =
  fmap (factorRegistrationEntryFromSnapshot runtime program)
{-# INLINE factorRegistrationEntriesFromSnapshots #-}

factorRegistrationEntryFromSnapshot ::
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  FactorProgram ->
  RelationalCarrierDelta ctx Carrier prop RuntimeBoundary evidence ->
  PlanReuseRegistrationEntry ctx prop
factorRegistrationEntryFromSnapshot runtime program snapshot =
  PlanReuseRegistrationEntry
    { prreAddr = deAddr snapshot,
      prreTime = deTime snapshot,
      prreBoundary = deBoundary snapshot,
      prreScope =
        factorRegistrationScope
          (reCanonicalityOracle (rdrEnv runtime))
          (factorProgramCanonical program)
          (factorProgramFactorShapeManifest program)
          (deAddr snapshot)
    }
{-# INLINE factorRegistrationEntryFromSnapshot #-}

factorRegistrationEntriesFromNodes ::
  FactorCarrierEmitSpec ctx prop RuntimeBoundary evidence ->
  RelationalCarrierTime ctx ->
  QueryId ->
  RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr ->
  FactorProgram ->
  [FactorNode] ->
  [PlanReuseRegistrationEntry ctx prop]
factorRegistrationEntriesFromNodes spec eventTime queryId runtime program =
  mapMaybe (factorRegistrationEntryFromNode spec eventTime queryId runtime program)
{-# INLINE factorRegistrationEntriesFromNodes #-}

factorRegistrationEntryFromNode ::
  FactorCarrierEmitSpec ctx prop RuntimeBoundary evidence ->
  RelationalCarrierTime ctx ->
  QueryId ->
  RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr ->
  FactorProgram ->
  FactorNode ->
  Maybe (PlanReuseRegistrationEntry ctx prop)
factorRegistrationEntryFromNode spec eventTime queryId runtime program node = do
  nodeManifest <- lookupFactorShapeManifestNode node (factorProgramFactorShapeManifest program)
  let payload =
        FactorCarrierPayload
          { fcpRelationalScope = mempty,
            fcpNode = node,
            fcpSchema = fsnmOutputSchema nodeManifest,
            fcpRows = emptyPlainRowPatch
          }
      addr =
        cesAddrOf spec (queryId, payload)
      boundary =
        cesBoundaryOf spec (queryId, payload)
  pure
    PlanReuseRegistrationEntry
      { prreAddr = addr,
        prreTime = eventTime,
        prreBoundary = boundary,
        prreScope =
          factorNodeRegistrationScope
            (reCanonicalityOracle (rdrEnv runtime))
            (factorProgramCanonical program)
            nodeManifest
      }
{-# INLINE factorRegistrationEntryFromNode #-}

factorRegistrationScope ::
  CanonicalityOracle atomRow ->
  CanonicalizationResult ->
  FactorShapeManifest ->
  CarrierAddr ctx Carrier prop ->
  RelationalScope
factorRegistrationScope oracle canonical manifest addr =
  case caCarrier addr of
    QueryCarrier _queryId (QueryFactor node) ->
      case lookupFactorShapeManifestNode node manifest of
        Nothing ->
          mempty
        Just nodeManifest ->
          factorNodeRegistrationScope oracle canonical nodeManifest
    QueryCarrier {} ->
      mempty
    DerivedCarrier {} ->
      mempty
{-# INLINE factorRegistrationScope #-}

factorNodeRegistrationScope ::
  CanonicalityOracle atomRow ->
  CanonicalizationResult ->
  FactorShapeNodeManifest ->
  RelationalScope
factorNodeRegistrationScope oracle canonical nodeManifest =
  relationalScopeFromSets
    IntSet.empty
    ( IntMap.foldlWithKey'
        collectAtomTopo
        IntSet.empty
        (crAtomShapes canonical)
    )
    IntSet.empty
    IntSet.empty
    IntSet.empty
  where
    representedAtoms =
      fsnmAtoms nodeManifest

    collectAtomTopo acc atomKey atomShape
      | Map.member atomShape representedAtoms =
          IntSet.union acc (dirtyTopoForAtom oracle (mkAtomId atomKey))
      | otherwise =
          acc
{-# INLINE factorNodeRegistrationScope #-}

commitExactFactorRepairResults ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop
  ) =>
  RelationalCarrierTime ctx ->
  [ExactFactorRepairCommit ctx prop boundary evidence joinState joinErr] ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( [FactorRepairReport ctx prop boundary evidence],
      RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      CarrierCommitTrace ctx prop
    )
commitExactFactorRepairResults eventTime commits runtime0 = do
  let !deltas =
        concatMap
          (efrrEmittedDeltas . efrcResult)
          commits
      !statsByCommit =
        fmap exactCommitStats commits
      !totalStats =
        exactCommitGroupStats commits
  (runtimeIndexed, touches) <-
    indexCarrierDeltas
      deltas
      runtime0
  let !runtimeWithStats =
        appendFactorRepairStats totalStats runtimeIndexed
      !runtimeWithPrograms =
        installFactorPrograms
          (exactCommitPrograms commits)
          runtimeWithStats
      !spec =
        reFactorCarrierEmitSpec (rdrEnv runtimeWithPrograms)
      !registrations =
        fmap
          (exactCommitRegistration spec eventTime runtimeWithPrograms)
          commits
  planReuse1 <-
    Foldable.foldlM
      registerExactCommitShape
      (runtimePlanReuseState runtimeWithPrograms)
      registrations
  (runtimeTouched, commitTrace) <-
    applyTouches
      touches
      (replaceRuntimePlanReuse planReuse1 runtimeWithPrograms)
  pure
    ( zipWith3 exactCommitReport statsByCommit commits registrations,
      runtimeTouched,
      commitTrace
    )
{-# INLINE commitExactFactorRepairResults #-}

data ExactCommitRegistration ctx prop = ExactCommitRegistration
  { ecrRequest :: !(FactorRepairRequest ctx prop),
    ecrProgram :: !FactorProgram,
    ecrInputSignature :: !StableDigest128,
    ecrEntries :: ![PlanReuseRegistrationEntry ctx prop]
  }

exactCommitStats ::
  ExactFactorRepairCommit ctx prop boundary evidence joinState joinErr ->
  RuntimeRepairStats
exactCommitStats commit =
  runtimeRepairStatsFromMaintenance
    queryId
    (efrcSubscriberCount commit)
    (efrrInputStats result)
    (efrrEmittedDeltas result)
    (carrierDeltaRowCount (efrrEmittedDeltas result))
    []
    (efrrMaintenanceMetrics result)
  where
    request =
      efrcRequest commit

    result =
      efrcResult commit

    queryId =
      frrQueryId request
{-# INLINE exactCommitStats #-}

exactCommitGroupStats ::
  [ExactFactorRepairCommit ctx prop boundary evidence joinState joinErr] ->
  RuntimeRepairStats
exactCommitGroupStats commits =
  case commits of
    [] ->
      emptyRuntimeRepairStats
    commit : _ ->
      runtimeRepairStatsFromMaintenance
        (frrQueryId (efrcRequest commit))
        (Foldable.sum (fmap efrcSubscriberCount commits))
        (efrrInputStats result)
        deltas
        (carrierDeltaRowCount deltas)
        []
        (efrrMaintenanceMetrics result)
      where
        result =
          efrcResult commit

        deltas =
          concatMap (efrrEmittedDeltas . efrcResult) commits
{-# INLINE exactCommitGroupStats #-}

exactCommitPrograms ::
  [ExactFactorRepairCommit ctx prop boundary evidence joinState joinErr] ->
  Map.Map RepairProgramKey FactorProgram
exactCommitPrograms =
  Map.fromList
    . fmap
      ( \commit ->
          ( frrRepairKey (efrcRequest commit),
            efrrProgram (efrcResult commit)
          )
      )
{-# INLINE exactCommitPrograms #-}

exactCommitRegistration ::
  FactorCarrierEmitSpec ctx prop RuntimeBoundary evidence ->
  RelationalCarrierTime ctx ->
  RelDiffRuntime ctx prop RuntimeBoundary evidence joinState joinErr ->
  ExactFactorRepairCommit ctx prop RuntimeBoundary evidence joinState joinErr ->
  ExactCommitRegistration ctx prop
exactCommitRegistration spec eventTime runtime commit =
  ExactCommitRegistration
    { ecrRequest = request,
      ecrProgram = program,
      ecrInputSignature = efrrInputSignature result,
      ecrEntries =
        factorRegistrationEntriesFromNodes
          spec
          eventTime
          queryId
          runtime
          program
          (efrrRegistrationNodes result)
    }
  where
    request =
      efrcRequest commit

    result =
      efrcResult commit

    queryId =
      frrQueryId request

    program =
      efrrProgram result
{-# INLINE exactCommitRegistration #-}

registerExactCommitShape ::
  (Ord ctx, Ord prop) =>
  PlanReuseState ctx prop ->
  ExactCommitRegistration ctx prop ->
  Either
    (RelationalRuntimeError ctx prop RuntimeBoundary evidence)
    (PlanReuseState ctx prop)
registerExactCommitShape planReuse registration =
  case
    registerFactorCarrierShapes
      queryId
      (factorProgramCanonical (ecrProgram registration))
      (factorProgramFactorShapeManifest (ecrProgram registration))
      (ecrInputSignature registration)
      (ecrEntries registration)
      planReuse
    of
      Left registrationError ->
        Left (RuntimeOpFailure (RelationalRuntimeSubsumptionRegistrationFailed queryId registrationError))
      Right planReuse' ->
        Right planReuse'
  where
    queryId =
      frrQueryId (ecrRequest registration)
{-# INLINE registerExactCommitShape #-}

exactCommitReport ::
  RuntimeRepairStats ->
  ExactFactorRepairCommit ctx prop boundary evidence joinState joinErr ->
  ExactCommitRegistration ctx prop ->
  FactorRepairReport ctx prop boundary evidence
exactCommitReport stats commit registration =
  FactorRepairReport
    { frrpQueryId = frrQueryId request,
      frrpCause = frrCause request,
      frrpAction = FactorRepairRanExact,
      frrpEmittedDeltaCount = length (efrrEmittedDeltas result),
      frrpRegisteredNodeCount = length (ecrEntries registration),
      frrpStats = stats
    }
  where
    request =
      efrcRequest commit

    result =
      efrcResult commit
{-# INLINE exactCommitReport #-}
