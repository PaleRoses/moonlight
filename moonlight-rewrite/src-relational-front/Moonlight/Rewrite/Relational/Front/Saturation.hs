{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Rewrite.Relational.Front.Saturation
  ( SaturationConfig (..),
    defaultSaturationConfig,
    SaturationRound (..),
    SaturationResult (..),
    saturateBase,
    saturateContext,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Foldable
  ( foldlM,
  )
import Data.Fix
  ( Fix,
  )
import Data.Functor.Identity
  ( Identity,
    runIdentity,
  )
import Data.List
  ( sortBy,
  )
import Data.IntSet qualified as IntSet
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Ord
  ( comparing,
  )
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Moonlight.Control.Candidate
  ( CandidateSpace,
    candidateSpaceAvailableCount,
    finiteCandidateSpace,
    scheduledBatchMatches,
  )
import Moonlight.Control.Count
  ( WorkCoverage (WorkCoverageComplete),
    naturalToBoundedInt,
    workCountLowerBound,
    workCountLowerBoundToBoundedInt,
  )
import Moonlight.Control.Schedule
  ( SchedulerConfig,
  )
import Moonlight.Control.Schedule.Round
  ( ScheduleOutcome (..),
    scheduleCandidateSpace,
    schedulerTrace,
  )
import Moonlight.Core
  ( MatchActivationIndex (..),
    RewriteRuleId,
    SiteIndex (..),
    SiteProgram (..),
  )
import Moonlight.Core.EGraph.Program (eGraphProgramChanged)
import Moonlight.Rewrite.Runtime
  ( ExecutableRewriteMatch (..),
    ExecutedRewrite (..),
    compileExecutableRewriteMatch,
    executableRewriteMatchRuleKey,
    rpId,
  )
import Moonlight.Rewrite.DSL
  ( ContextName,
    Node,
    NodeTag,
    RewriteGuardAtom (..),
    RewriteSignature,
    canonicalSupportIndex,
  )
import Moonlight.Rewrite.Relational.Front.ApplicationCondition
  ( emptyRelationalApplicationConditionCache,
    recheckRelationalApplicationCondition,
  )
import Moonlight.Rewrite.Relational.Front.Error
  ( RelationalProgramError (..),
    relationalSaturationResumeError,
  )
import Moonlight.Rewrite.Relational.Front.Host
  ( Host,
    HostProgramResult (..),
    HostRebuildResult (..),
    hostClassCount,
    hostClassHasWitness,
    hostClassWitnessMemoized,
    hostRevision,
    rebuildHostBarrier,
    runHostRewriteProgram,
  )
import Moonlight.Rewrite.Relational.Front.Saturation.Substrate
  ( RelationalFrontSaturation,
    relationalSaturationMatchSubstitution,
    relationalSaturationSupportedScheduleKey,
  )
import Moonlight.Rewrite.Relational.Front.Saturation.Types
import Moonlight.Rewrite.Relational
  ( RelationalRewriteMatch (..),
    RewriteRunConfig (..),
    RewriteRunError (..),
    RewriteRunStats,
    appendRewriteRunStats,
    checkRewriteRunLimits,
    emptyRewriteRunStats,
    limitToBoundedInt,
    rrmRounds,
    statsForRewriteApplications,
    statsForRounds,
  )
import Moonlight.Rewrite.System
  ( CompiledGuard,
    ProofRetention (..),
    RuleName,
    baseSupportRuleNames,
    contextSupportRuleNames,
  )
import Moonlight.Rewrite.ProofContext
  ( ProofContextEvidence (..),
    ProofRegistry,
    ProofStepSummaryInput (..),
    ProofStepInput (..),
    defaultProofStepInput,
    defaultProofStepSummaryInput,
    emptyProofRegistryWithRetention,
    proofRegistryRetention,
    recordProofStepByRetention,
  )
import Moonlight.Saturation.Context.Driver
  ( ContextRunResult (..),
    contextExecutionSpec,
    resumableRuntimeState,
    runContextPlan,
  )
import Moonlight.Saturation.Context.Error
  ( SaturationRunError (..),
  )
import Moonlight.Saturation.Context.Program.Plan
  ( Program,
    ProgramStage (..),
    mkPlan,
  )
import Moonlight.Saturation.Context.Program.Spec
  ( PlanSpec,
    planSpec,
    withSchedulerConfig,
  )
import Moonlight.Saturation.Context.Program.View
  ( SaturationRoundView (..),
  )
import Moonlight.Saturation.Context.Runtime.Policy
  ( CarrierAccess (..),
    RuntimePolicy (..),
  )
import Moonlight.Saturation.Context.Runtime.Match.Batch
  ( MatchBatch,
    matchBatchFromList,
    matchBatchToList,
  )
import Moonlight.Saturation.Context.Runtime.Match.Pipeline
  ( CandidatePipelineStage (..),
    candidatePipelineIncrement,
    emptyCandidatePipelineCounts,
  )
import Moonlight.Saturation.Context.Runtime.Schedule.Decision
  ( RuntimeScheduleDecision (..),
  )
import Moonlight.Saturation.Context.Runtime.State
  ( RuntimeReportWindow (rrwFinalState),
    RuntimeState (..),
  )
import Moonlight.Saturation.Core
  ( ApplyOutcome (..),
    SaturationBudget (..),
  )
import Moonlight.FiniteLattice
  ( ContextLattice,
    compileContextLattice,
    contextOrderDecl,
    singletonContextLattice
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    withPreparedContextSiteFromFiniteLattice,
  )

saturateBase ::
  forall sig atom.
  (RewriteSignature sig, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  SaturationConfig sig ->
  Rules sig atom ->
  Host sig ->
  PreparedCache sig atom ->
  Either (RelationalProgramError sig) (PreparedCache sig atom, SaturationResult sig)
saturateBase config rulesValue initialHost preparedSystem =
  let contextLattice =
        singletonContextLattice RelationalBaseContext
   in withPreparedContextSiteFromFiniteLattice contextLattice $ \preparedSite -> do
        baseProgramValue <- baseSaturationProgram rulesValue
        runRelationalSaturation
          config
          preparedSystem
          baseProgramValue
          (initialCarrier config preparedSite RelationalBaseContext contextLattice initialHost initialHost)

saturateContext ::
  forall sig atom.
  (RewriteSignature sig, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  SaturationConfig sig ->
  ContextName ->
  Rules sig atom ->
  Host sig ->
  Host sig ->
  PreparedCache sig atom ->
  Either (RelationalProgramError sig) (PreparedCache sig atom, SaturationResult sig)
saturateContext config contextName rulesValue baseHost liveHost preparedSystem = do
  contextLattice <-
    first
      RelationalProgramSaturationPlanError
      (relationalContextLattice contextName)
  withPreparedContextSiteFromFiniteLattice contextLattice $ \preparedSite -> do
    programValue <-
      contextSaturationProgram contextName rulesValue
    runRelationalSaturation
      config
      preparedSystem
      programValue
      (initialCarrier config preparedSite (RelationalNamedContext contextName) contextLattice baseHost liveHost)

runRelationalSaturation ::
  forall owner sig atom.
  (RewriteSignature sig, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  SaturationConfig sig ->
  PreparedCache sig atom ->
  Program 'CompiledProgramStage (RelationalFrontSaturation owner sig atom ()) ->
  RelationalSaturationCarrier owner sig atom ->
  Either (RelationalProgramError sig) (PreparedCache sig atom, SaturationResult sig)
runRelationalSaturation config preparedSystem programValue carrier = do
  runResult <-
    first relationalSaturationRunError
      ( runContextPlan
          ( contextExecutionSpec
              (relationalSaturationPolicy @owner @sig @atom @())
              mempty
          )
          (mkPlan (relationalPlanSpec config preparedSystem) programValue)
          carrier
      )
  finalPreparedSystem <-
    case rsmsPreparedSystem (rsMatchState (resumableRuntimeState (crrState runResult))) of
      Just retainedPreparedSystem ->
        Right retainedPreparedSystem

      Nothing ->
        Left
          ( RelationalProgramSaturationObstruction
              RelationalSaturationPreparedSystemMissing
          )
  Right (finalPreparedSystem, crrResult runResult)

relationalPlanSpec ::
  SaturationConfig sig ->
  PreparedCache sig atom ->
  PlanSpec
    (RelationalFrontSaturation owner sig atom projection)
    (RelationalSaturationCarrier owner sig atom)
    RewriteRuleId
relationalPlanSpec config preparedSystem =
  withSchedulerConfig (scSchedulerConfig config) $
    planSpec
      SaturationBudget
        { sbMaxIterations = limitToBoundedInt (rrmRounds (rrcLimits (scRunConfig config))),
          sbMaxNodes = maybe maxBound naturalToBoundedInt (scHostNodeLimit config)
        }
      ()
      RelationalSaturationRuntimeContext
        { rsrcConfig = config,
          rsrcInitialPreparedSystem = Just preparedSystem
        }

relationalSaturationRunError ::
  SaturationRunError (RelationalFrontSaturation owner sig atom projection) ->
  RelationalProgramError sig
relationalSaturationRunError =
  \case
    SaturationRunSectionObstructed obstruction ->
      RelationalProgramSaturationObstruction obstruction

    SaturationRunApplyFailed applicationError ->
      applicationError

    SaturationRunSupportContextLookupFailed supportError ->
      RelationalProgramSaturationObstruction
        (RelationalSaturationPreparedSupportFailed supportError)

    SaturationRunResumeIncompatible resumeError ->
      RelationalProgramSaturationPlanError
        (relationalSaturationResumeError resumeError)

baseSaturationProgram ::
  forall owner sig atom.
  Rules sig atom ->
  Either
    (RelationalProgramError sig)
    (Program 'CompiledProgramStage (RelationalFrontSaturation owner sig atom ()))
baseSaturationProgram rulesValue = do
  baseRules <-
    rulesByName
      rulesValue
      (Set.toAscList (baseSupportRuleNames (canonicalSupportIndex (rulesCanonicalProgram rulesValue))))
  Right
    SiteProgram
      { spFactRules = SiteIndex [] Map.empty,
        spRewriteRules = SiteIndex baseRules Map.empty,
        spSupportedFactRules = [],
        spSupportedRewriteRules = Map.empty,
        spRewriteActivation =
          MatchActivationIndex
            { maiBase = Set.fromList (fmap rsrRuleId baseRules),
              maiContexts = Map.empty
            },
        spBaseRewriteSupport = Map.empty
      }

contextSaturationProgram ::
  forall owner sig atom.
  ContextName ->
  Rules sig atom ->
  Either
    (RelationalProgramError sig)
    (Program 'CompiledProgramStage (RelationalFrontSaturation owner sig atom ()))
contextSaturationProgram contextName rulesValue = do
  activeRules <-
    rulesByName
      rulesValue
      ( Set.toAscList
          ( baseSupportRuleNames supportIndex
              <> contextSupportRuleNames contextName supportIndex
          )
      )
  Right
    SiteProgram
      { spFactRules = SiteIndex [] Map.empty,
        spRewriteRules = SiteIndex [] (Map.singleton activeContext activeRules),
        spSupportedFactRules = [],
        spSupportedRewriteRules = Map.empty,
        spRewriteActivation =
          MatchActivationIndex
            { maiBase = Set.empty,
              maiContexts = Map.empty
            },
        spBaseRewriteSupport = Map.empty
      }
  where
    activeContext =
      RelationalNamedContext contextName

    supportIndex =
      canonicalSupportIndex (rulesCanonicalProgram rulesValue)

rulesByName ::
  Rules sig atom ->
  [RuleName] ->
  Either (RelationalProgramError sig) [RelationalSaturationRule sig atom]
rulesByName = traverse . ruleByName

ruleByName ::
  Rules sig atom ->
  RuleName ->
  Either (RelationalProgramError sig) (RelationalSaturationRule sig atom)
ruleByName rulesValue ruleNameValue = do
  relationalRule <-
    maybe
      (missingRulePlanError ruleNameValue)
      Right
      (Map.lookup ruleNameValue (rulesRelationalRules rulesValue))
  Right
    RelationalSaturationRule
      { rsrRuleName = ruleNameValue,
        rsrRuleId = rpId (rcrRulePlan relationalRule),
        rsrRulePlan = rcrRulePlan relationalRule,
        rsrPlan = rcrMatchPlan relationalRule,
        rsrApplicationConditionPlans = rcrApplicationConditionPlans relationalRule
      }

missingRulePlanError :: RuleName -> Either (RelationalProgramError sig) value
missingRulePlanError =
  Left . RelationalProgramSaturationPlanError . RelationalSaturationMissingRulePlan

relationalContextLattice ::
  ContextName ->
  Either RelationalSaturationPlanError (ContextLattice RelationalSaturationContext)
relationalContextLattice contextName =
  first RelationalSaturationContextLatticeCompileError $
    compileContextLattice
      (Set.fromList [RelationalBaseContext, activeContext])
      (contextOrderDecl activeContext RelationalBaseContext [(RelationalBaseContext, activeContext)])
  where
    activeContext =
      RelationalNamedContext contextName

initialCarrier ::
  SaturationConfig sig ->
  PreparedContextSite owner RelationalSaturationContext ->
  RelationalSaturationContext ->
  ContextLattice RelationalSaturationContext ->
  Host sig ->
  Host sig ->
  RelationalSaturationCarrier owner sig atom
initialCarrier config preparedSite activeContext contextLattice baseHost liveHost =
  RelationalSaturationCarrier
    { rscBaseHost = baseHost,
      rscLiveHost = liveHost,
      rscActiveContext = activeContext,
      rscContextLattice = contextLattice,
      rscPreparedSite = preparedSite,
      rscProofs = emptyProofRegistryWithRetention (scProofRetention config),
      rscApplicationConditionCache = emptyRelationalApplicationConditionCache,
      rscBannedScheduleKeys = Set.empty,
      rscPendingDirtyResults = IntSet.empty
    }

relationalSaturationPolicy ::
  forall owner sig atom projection.
  (RewriteSignature sig, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  RuntimePolicy
    (RelationalFrontSaturation owner sig atom projection)
    (RelationalSaturationCarrier owner sig atom)
    RewriteRuleId
    (SaturationResult sig)
relationalSaturationPolicy =
  RuntimePolicy
    { rpCarrier =
        CarrierAccess
          { caGraph = id,
            caSetGraph = const
          },
      rpCandidateSpace = relationalCandidateSpace,
      rpSchedule = scheduleRelationalRound,
      rpApply = applyRelationalMatches,
      rpBootstrap = rebuildRelationalState,
      rpRebuild = rebuildRelationalState,
      rpPostRebuildMatchingDelta = \_ _ _ _ _ -> (),
      rpReport = \_termination window -> Right (relationalSaturationResult (rrwFinalState window))
    }

scheduleRelationalRound ::
  forall owner sig atom projection.
  SchedulerConfig RewriteRuleId ->
  RelationalSaturationRuntimeContext sig atom ->
  SaturationRoundView (RelationalFrontSaturation owner sig atom projection) ->
  CandidateSpace Identity RewriteRuleId () (RelationalSaturationSupportedMatch sig atom) ->
  RuntimeState (RelationalFrontSaturation owner sig atom projection) (RelationalSaturationCarrier owner sig atom) RewriteRuleId ->
  RuntimeScheduleDecision RewriteRuleId (RelationalSaturationSupportedMatch sig atom)
scheduleRelationalRound schedulerConfig _rewriteContext roundView candidateSpace state =
  let candidateCount =
        runIdentity (candidateSpaceAvailableCount candidateSpace)
      scheduledOutcome ::
        ScheduleOutcome
          RewriteRuleId
          ()
          (RelationalSaturationSupportedMatch sig atom)
      scheduledOutcome =
        runIdentity
          ( scheduleCandidateSpace
              schedulerConfig
              (workCountLowerBound candidateCount)
              (srvIteration roundView)
              candidateSpace
              (rsScheduler state)
          )
      scheduledBatch =
        matchBatchFromList (scheduledBatchMatches (soScheduledBatch scheduledOutcome))
      guidedCount =
        workCountLowerBoundToBoundedInt candidateCount
      scheduledCount =
        naturalToBoundedInt (soScheduledCount scheduledOutcome)
      notSelectedCount =
        workCountLowerBoundToBoundedInt
          (soSuppressedCount scheduledOutcome <> soDeferredByBudgetCount scheduledOutcome)
      pipelineCounts =
        foldr
          ($)
          emptyCandidatePipelineCounts
          [ candidatePipelineIncrement CandidateGuided guidedCount,
            candidatePipelineIncrement CandidateAdmitted guidedCount,
            candidatePipelineIncrement CandidateScheduledBeforeValidation scheduledCount,
            candidatePipelineIncrement CandidateNotSelectedByScheduler notSelectedCount,
            candidatePipelineIncrement CandidateScheduled scheduledCount
          ]
   in RuntimeScheduleDecision
        { rsdScheduledMatches = scheduledBatch,
          rsdSchedulerState = soSchedulerState scheduledOutcome,
          rsdTracePolicy = soTracePolicy scheduledOutcome,
          rsdTraceDelta = Vector.fromList (soSchedulerTraceDelta scheduledOutcome),
          rsdAllCandidatesScheduled = soCoverage scheduledOutcome == WorkCoverageComplete,
          rsdPipelineCounts = pipelineCounts
        }

relationalCandidateSpace ::
  RuntimeState (RelationalFrontSaturation owner sig atom projection) (RelationalSaturationCarrier owner sig atom) RewriteRuleId ->
  MatchBatch (RelationalSaturationSupportedMatch sig atom) ->
  CandidateSpace Identity RewriteRuleId () (RelationalSaturationSupportedMatch sig atom)
relationalCandidateSpace state supportedMatches =
  finiteCandidateSpace
    (fmap (\match -> (relationalSchedulerGroup match, [match])) orderedLiveMatches)
  where
    relationalSchedulerGroup ::
      RelationalSaturationSupportedMatch sig atom ->
      RewriteRuleId
    relationalSchedulerGroup =
      rsrRuleId . rsmRule . rssmMatch

    orderedLiveMatches =
      sortBy
        (comparing relationalSaturationSupportedScheduleKey)
        liveMatches

    liveMatches =
      deduplicateSupportedMatches
        ( filter
            (not . (`Set.member` bannedScheduleKeys) . relationalSaturationSupportedScheduleKey)
            (matchBatchToList supportedMatches)
        )

    bannedScheduleKeys =
      rscBannedScheduleKeys (rsCarrier state)

deduplicateSupportedMatches ::
  [RelationalSaturationSupportedMatch sig atom] ->
  [RelationalSaturationSupportedMatch sig atom]
deduplicateSupportedMatches =
  Map.elems
    . Map.fromListWith (\_new old -> old)
    . fmap (\matchValue -> (relationalSaturationSupportedScheduleKey matchValue, matchValue))

applyRelationalMatches ::
  forall owner sig atom projection.
  (RewriteSignature sig, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  RelationalSaturationRuntimeContext sig atom ->
  [RelationalSaturationSupportedMatch sig atom] ->
  RuntimeState (RelationalFrontSaturation owner sig atom projection) (RelationalSaturationCarrier owner sig atom) RewriteRuleId ->
  Either
    (RelationalProgramError sig)
    (ApplyOutcome RelationalSaturationApplicationResult (RelationalSaturationCarrier owner sig atom))
applyRelationalMatches runtimeContext scheduledMatches state = do
  let snapshotHost =
        rscLiveHost (rsCarrier state)
  execution <-
    foldlM
      (applyRelationalMatch (rsrcConfig runtimeContext) snapshotHost)
      (emptyRelationalApplicationExecution (rsCarrier state))
      scheduledMatches
  checkRelationalSaturationLimits (rsrcConfig runtimeContext) (pendingTotalStats execution (rsMatchState state))
  pure
    ApplyOutcome
      { aoState = rsaeCarrier execution,
        aoEffect =
          RelationalSaturationApplicationResult
            (reverse (rsaeExecuted execution))
            (rsaeStats execution)
      }

rebuildRelationalState ::
  (RewriteSignature sig, Ord (NodeTag sig)) =>
  RuntimeState (RelationalFrontSaturation owner sig atom projection) (RelationalSaturationCarrier owner sig atom) RewriteRuleId ->
  Either
    (RelationalSaturationObstruction sig)
    ( RuntimeState (RelationalFrontSaturation owner sig atom projection) (RelationalSaturationCarrier owner sig atom) RewriteRuleId,
      RelationalSaturationRebuild sig
    )
rebuildRelationalState state = do
  let carrier =
        rsCarrier state
  HostRebuildResult rebuiltHost _rebuildDelta rebuildDirtyResults <-
    first RelationalSaturationHostRebuildFailed (rebuildHostBarrier (rscLiveHost carrier))
  let dirtyResults =
        rscPendingDirtyResults carrier <> rebuildDirtyResults
  Right
    ( state
        { rsCarrier =
            carrier
              { rscLiveHost = rebuiltHost,
                rscPendingDirtyResults = IntSet.empty
              }
        },
      RelationalSaturationRebuild
        { rsrEpoch = hostRevision rebuiltHost,
          rsrActiveContext = rscActiveContext carrier,
          rsrRebuiltHost = rebuiltHost,
          rsrDirtyResults = dirtyResults
        }
    )

relationalSaturationResult ::
  RuntimeState (RelationalFrontSaturation owner sig atom projection) (RelationalSaturationCarrier owner sig atom) RewriteRuleId ->
  SaturationResult sig
relationalSaturationResult state =
  SaturationResult
    { saturationHost = rscLiveHost carrier,
      saturationProofs = rscProofs carrier,
      saturationRounds = reverse (rsmsRounds (rsMatchState state)),
      saturationSchedulerTrace = schedulerTrace (rsScheduler state),
      saturationStats = rsmsStats (rsMatchState state)
    }
  where
    carrier =
      rsCarrier state

data RelationalSaturationApplicationExecution owner sig atom = RelationalSaturationApplicationExecution
  { rsaeCarrier :: !(RelationalSaturationCarrier owner sig atom),
    rsaeExecuted :: ![ExecutedRewrite],
    rsaeStats :: !RewriteRunStats,
    rsaeWitnessCache :: !(IntMap.IntMap (Fix (Node sig)))
  }

emptyRelationalApplicationExecution ::
  RelationalSaturationCarrier owner sig atom ->
  RelationalSaturationApplicationExecution owner sig atom
emptyRelationalApplicationExecution carrier =
  RelationalSaturationApplicationExecution
    { rsaeCarrier = carrier,
      rsaeExecuted = [],
      rsaeStats = emptyRewriteRunStats,
      rsaeWitnessCache = IntMap.empty
    }

applyRelationalMatch ::
  forall owner sig atom.
  (RewriteSignature sig, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  SaturationConfig sig ->
  Host sig ->
  RelationalSaturationApplicationExecution owner sig atom ->
  RelationalSaturationSupportedMatch sig atom ->
  Either (RelationalProgramError sig) (RelationalSaturationApplicationExecution owner sig atom)
applyRelationalMatch config snapshotHost execution supportedMatch = do
  let executableMatch =
        executableSupportedMatch supportedMatch
      carrier =
        rsaeCarrier execution
      liveHost =
        rscLiveHost carrier
  (applicationConditionCache', applicationConditionAccepted) <-
    recheckRelationalApplicationCondition
      (rsrApplicationConditionPlans (rsmRule (rssmMatch supportedMatch)))
      (rscApplicationConditionCache carrier)
      snapshotHost
      executableMatch
  if not applicationConditionAccepted
    then
      pure execution {rsaeCarrier = carrier {rscApplicationConditionCache = applicationConditionCache'}}
    else do
      rewriteProgram <-
        first RelationalProgramRewriteApplicationError $
          compileExecutableRewriteMatch
            (scResolveBindingPattern config)
            (scBinderSubstAlgebra config)
            executableMatch
      HostProgramResult host' executedRewrite applicationEffect _editDelta programDirtyResults <-
        first RelationalProgramRewriteApplicationError $
          runHostRewriteProgram rewriteProgram liveHost
      if not (eGraphProgramChanged applicationEffect)
        then
          pure
            execution
              { rsaeCarrier =
                  carrier
                    { rscLiveHost = host',
                      rscApplicationConditionCache = applicationConditionCache',
                      rscPendingDirtyResults =
                        rscPendingDirtyResults carrier <> programDirtyResults,
                      rscBannedScheduleKeys =
                        Set.insert
                          (relationalSaturationSupportedScheduleKey supportedMatch)
                          (rscBannedScheduleKeys carrier)
                    }
              }
        else do
          let (witnessCache', proofs') =
                recordSuccessfulRewrite
                  executableMatch
                  executedRewrite
                  host'
                  (rscActiveContext carrier)
                  (rsaeWitnessCache execution)
                  (rscProofs carrier)
          pure
            execution
              { rsaeCarrier =
                  carrier
                    { rscLiveHost = host',
                      rscProofs = proofs',
                      rscApplicationConditionCache = applicationConditionCache',
                      rscPendingDirtyResults =
                        rscPendingDirtyResults carrier <> programDirtyResults
                    },
                rsaeExecuted = executedRewrite : rsaeExecuted execution,
                rsaeStats = appendRewriteRunStats (rsaeStats execution) (statsForRewriteApplications 1),
                rsaeWitnessCache = witnessCache'
              }

executableSupportedMatch ::
  RelationalSaturationSupportedMatch sig atom ->
  ExecutableRewriteMatch (CompiledGuard (GuardCapabilityKey atom) (Node sig)) () () (Node sig)
executableSupportedMatch supportedMatch =
  ExecutableRewriteMatch
    { ermRule = rsrRulePlan ruleValue,
      ermRootClass = rrmRoot matchValue,
      ermGuardEvidence = Nothing,
      ermGuideEvidence = Nothing,
      ermSubstitution = relationalSaturationMatchSubstitution relationalMatch
    }
  where
    relationalMatch =
      rssmMatch supportedMatch
    ruleValue =
      rsmRule relationalMatch
    matchValue =
      rsmMatch relationalMatch

recordSuccessfulRewrite ::
  RewriteSignature sig =>
  ExecutableRewriteMatch (CompiledGuard capability (Node sig)) () () (Node sig) ->
  ExecutedRewrite ->
  Host sig ->
  RelationalSaturationContext ->
  IntMap.IntMap (Fix (Node sig)) ->
  ProofRegistry (Node sig) ContextName () ->
  (IntMap.IntMap (Fix (Node sig)), ProofRegistry (Node sig) ContextName ())
recordSuccessfulRewrite executableMatch executedRewrite host activeContext witnessCache proofRegistry =
  case proofRegistryRetention proofRegistry of
    KeepFullProof ->
      let (witnessCacheAfterLhs, lhsWitness) =
            hostClassWitnessMemoized witnessFuel lhsClass host witnessCache
          (witnessCacheAfterRhs, rhsWitness) =
            hostClassWitnessMemoized witnessFuel rhsClass host witnessCacheAfterLhs
       in recordRetainedWitnesses witnessCacheAfterRhs lhsWitness rhsWitness
    KeepRecentProofSteps retained
      | retained > 0 ->
          let (witnessCacheAfterLhs, lhsWitness) =
                hostClassWitnessMemoized witnessFuel lhsClass host witnessCache
              (witnessCacheAfterRhs, rhsWitness) =
                hostClassWitnessMemoized witnessFuel rhsClass host witnessCacheAfterLhs
           in recordRetainedWitnesses witnessCacheAfterRhs lhsWitness rhsWitness
    KeepNoProof ->
      recordWithoutWitnesses
    KeepProofSummary ->
      recordWithoutWitnesses
    KeepRecentProofSteps _ ->
      recordWithoutWitnesses
  where
    witnessFuel =
      max 1 (hostClassCount host + 1)
    rewriteRuleId =
      executableRewriteMatchRuleKey executableMatch
    lhsClass =
      erwLhsClass executedRewrite
    rhsClass =
      erwRhsClass executedRewrite
    contextEvidence =
      proofContextEvidence activeContext
    summaryInput =
      (defaultProofStepSummaryInput rewriteRuleId lhsClass rhsClass)
        { pssiWitnessed =
            hostClassHasWitness witnessFuel lhsClass host
              || hostClassHasWitness witnessFuel rhsClass host,
          pssiContextEvidence = contextEvidence
        }
    witnesslessInput =
      ( defaultProofStepInput
          rewriteRuleId
          lhsClass
          rhsClass
          (ermSubstitution executableMatch)
          ()
      )
        { psiFactDerivations = Set.empty,
          psiContextEvidence = contextEvidence
        }
    recordWithoutWitnesses =
      ( witnessCache,
        recordProofStepByRetention summaryInput witnesslessInput proofRegistry
      )
    recordRetainedWitnesses retainedCache lhsWitness rhsWitness =
      ( retainedCache,
        recordProofStepByRetention
          summaryInput
          ( witnesslessInput
              { psiLhsWitness = lhsWitness,
                psiRhsWitness = rhsWitness
              }
          )
          proofRegistry
      )

proofContextEvidence :: RelationalSaturationContext -> Maybe (ProofContextEvidence ContextName)
proofContextEvidence =
  \case
    RelationalBaseContext ->
      Nothing
    RelationalNamedContext contextName ->
      Just
        ProofContextEvidence
          { pceActiveContext = Just contextName,
            pceRestrictions = []
          }

pendingTotalStats ::
  RelationalSaturationApplicationExecution owner sig atom ->
  RelationalSaturationMatchState sig atom projection ->
  RewriteRunStats
pendingTotalStats execution matchState =
  appendRewriteRunStats (rsmsStats matchState) pendingRoundStats
  where
    pendingRoundStats =
      case rsmsPendingRound matchState of
        Nothing ->
          rsaeStats execution
        Just pendingRound ->
          appendRewriteRunStats
            (statsForRounds 1)
            (appendRewriteRunStats (rspMatchStats pendingRound) (rsaeStats execution))

checkRelationalSaturationLimits ::
  SaturationConfig sig ->
  RewriteRunStats ->
  Either (RelationalProgramError sig) ()
checkRelationalSaturationLimits config stats =
  first
    (\(limit, limitStats) -> RelationalProgramRunError (RewriteRunLimitExceeded limit limitStats))
    (checkRewriteRunLimits (rrcLimits (scRunConfig config)) stats)
