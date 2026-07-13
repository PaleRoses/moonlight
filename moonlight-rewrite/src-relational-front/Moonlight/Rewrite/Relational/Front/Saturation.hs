{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Relational-front saturation interpreter.
-- Owns base/context saturation programs, carrier setup, scheduler/lattice
-- integration, application-condition rechecks, rebuilds, proof recording, and
-- accumulated run stats.
-- Contracts: base uses a singleton context, named contexts combine base and
-- context support, and unchanged applications are banned by schedule key.
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
import Data.Functor.Identity
  ( Identity,
    runIdentity,
  )
import Data.List
  ( sortBy,
  )
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
  ( RewriteRuleId,
  )
import Moonlight.Core
  ( MatchActivationIndex (..),
    SiteIndex (..),
    SiteProgram (..),
  )
import Moonlight.Core.EGraph.Program (eGraphProgramChanged)
import Moonlight.Rewrite.Runtime
  ( aceDecision,
  )
import Moonlight.Rewrite.Runtime
  ( ExecutableRewriteMatch (..),
    ExecutedRewrite (..),
    compileExecutableRewriteMatch,
    executableRewriteMatchRuleKey,
  )
import Moonlight.Rewrite.Runtime (RulePlan (..))
import Moonlight.Rewrite.DSL
  ( canonicalSupportIndex,
  )
import Moonlight.Rewrite.DSL
  ( ContextName,
  )
import Moonlight.Rewrite.DSL
  ( RewriteGuardAtom (..),
  )
import Moonlight.Rewrite.DSL
  ( Node,
    NodeTag,
    RewriteSignature,
  )
import Moonlight.Rewrite.Relational.Front.ApplicationCondition
  ( RelationalApplicationConditionCache,
    emptyRelationalApplicationConditionCache,
    runRelationalApplicationConditionEvaluatorCached,
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
    hostClassWitness,
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
  ( RewriteRunStats,
    appendRewriteRunStats,
    checkRewriteRunLimits,
    emptyRewriteRunStats,
    limitToBoundedInt,
    rrmRounds,
    statsForProofSteps,
    statsForRounds,
  )
import Moonlight.Rewrite.Relational
  ( RewriteRunConfig (..),
    RewriteRunError (..),
  )
import Moonlight.Rewrite.Relational
  ( RelationalRewriteMatch (..),
  )
import Moonlight.Rewrite.System
  ( RuleName,
  )
import Moonlight.Rewrite.System
  ( baseSupportRuleNames,
    contextSupportRuleNames,
  )
import Moonlight.Rewrite.ProofContext
  ( ProofContextEvidence (..),
    ProofRegistry,
    ProofStepInput (..),
    defaultProofStepInput,
    emptyProofRegistryWithRetention,
    recordProofStepWith,
  )
import Moonlight.Saturation.Context.Driver
  ( ContextRunResult (..),
    contextExecutionSpec,
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
import Moonlight.Rewrite.System
  ( CompiledGuard,
  )
import Moonlight.FiniteLattice
  ( ContextLattice,
    compileContextLattice,
    contextOrderDecl,
    singletonContextLattice
  )
import Moonlight.Sheaf.Context.Site
  ( fromFiniteLattice,
  )

saturateBase ::
  forall sig atom.
  (RewriteSignature sig, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  SaturationConfig sig ->
  Rules sig atom ->
  Host sig ->
  Either (RelationalProgramError sig) (SaturationResult sig)
saturateBase config rulesValue initialHost = do
  baseProgramValue <- baseSaturationProgram rulesValue
  runRelationalSaturation
    config
    baseProgramValue
    (initialCarrier config RelationalBaseContext (singletonContextLattice RelationalBaseContext) initialHost initialHost)

saturateContext ::
  forall sig atom.
  (RewriteSignature sig, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  SaturationConfig sig ->
  ContextName ->
  Rules sig atom ->
  Host sig ->
  Host sig ->
  Either (RelationalProgramError sig) (SaturationResult sig)
saturateContext config contextName rulesValue baseHost liveHost = do
  contextLattice <-
    first
      RelationalProgramSaturationPlanError
      (relationalContextLattice contextName)
  programValue <-
    contextSaturationProgram contextName rulesValue
  runRelationalSaturation
    config
    programValue
    (initialCarrier config (RelationalNamedContext contextName) contextLattice baseHost liveHost)

runRelationalSaturation ::
  forall sig atom.
  (RewriteSignature sig, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  SaturationConfig sig ->
  Program 'CompiledProgramStage (RelationalFrontSaturation sig atom ()) ->
  RelationalSaturationCarrier sig atom ->
  Either (RelationalProgramError sig) (SaturationResult sig)
runRelationalSaturation config programValue carrier =
  first relationalSaturationRunError . fmap crrResult $
    runContextPlan
      ( contextExecutionSpec
          (relationalSaturationPolicy @sig @atom @())
          mempty
      )
      (mkPlan (relationalPlanSpec config) programValue)
      carrier

relationalPlanSpec ::
  SaturationConfig sig ->
  PlanSpec
    (RelationalFrontSaturation sig atom projection)
    (RelationalSaturationCarrier sig atom)
    RewriteRuleId
relationalPlanSpec config =
  withSchedulerConfig (scSchedulerConfig config) $
    planSpec
      SaturationBudget
        { sbMaxIterations = limitToBoundedInt (rrmRounds (rrcLimits (scRunConfig config))),
          sbMaxNodes = maybe maxBound naturalToBoundedInt (scHostNodeLimit config)
        }
      ()
      config

relationalSaturationRunError ::
  SaturationRunError (RelationalFrontSaturation sig atom projection) ->
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
  forall sig atom.
  Rules sig atom ->
  Either
    (RelationalProgramError sig)
    (Program 'CompiledProgramStage (RelationalFrontSaturation sig atom ()))
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
  forall sig atom.
  ContextName ->
  Rules sig atom ->
  Either
    (RelationalProgramError sig)
    (Program 'CompiledProgramStage (RelationalFrontSaturation sig atom ()))
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
  RelationalSaturationContext ->
  ContextLattice RelationalSaturationContext ->
  Host sig ->
  Host sig ->
  RelationalSaturationCarrier sig atom
initialCarrier config activeContext contextLattice baseHost liveHost =
  RelationalSaturationCarrier
    { rscBaseHost = baseHost,
      rscLiveHost = liveHost,
      rscActiveContext = activeContext,
      rscContextLattice = contextLattice,
      rscPreparedSite = fromFiniteLattice contextLattice,
      rscProofs = emptyProofRegistryWithRetention (scProofRetention config),
      rscApplicationConditionCache = emptyRelationalApplicationConditionCache,
      rscBannedScheduleKeys = Set.empty
    }

relationalSaturationPolicy ::
  forall sig atom projection.
  (RewriteSignature sig, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  RuntimePolicy
    (RelationalFrontSaturation sig atom projection)
    (RelationalSaturationCarrier sig atom)
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
  forall sig atom projection.
  SchedulerConfig RewriteRuleId ->
  SaturationConfig sig ->
  SaturationRoundView (RelationalFrontSaturation sig atom projection) ->
  CandidateSpace Identity RewriteRuleId () (RelationalSaturationSupportedMatch sig atom) ->
  RuntimeState (RelationalFrontSaturation sig atom projection) (RelationalSaturationCarrier sig atom) RewriteRuleId ->
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
  RuntimeState (RelationalFrontSaturation sig atom projection) (RelationalSaturationCarrier sig atom) RewriteRuleId ->
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
  forall sig atom projection.
  (RewriteSignature sig, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  SaturationConfig sig ->
  [RelationalSaturationSupportedMatch sig atom] ->
  RuntimeState (RelationalFrontSaturation sig atom projection) (RelationalSaturationCarrier sig atom) RewriteRuleId ->
  Either
    (RelationalProgramError sig)
    (ApplyOutcome RelationalSaturationApplicationResult (RelationalSaturationCarrier sig atom))
applyRelationalMatches config scheduledMatches state = do
  let snapshotHost =
        rscLiveHost (rsCarrier state)
  execution <-
    foldlM
      (applyRelationalMatch config snapshotHost)
      (emptyRelationalApplicationExecution (rsCarrier state))
      scheduledMatches
  checkRelationalSaturationLimits config (pendingTotalStats execution (rsMatchState state))
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
  RuntimeState (RelationalFrontSaturation sig atom projection) (RelationalSaturationCarrier sig atom) RewriteRuleId ->
  Either
    (RelationalSaturationObstruction sig)
    ( RuntimeState (RelationalFrontSaturation sig atom projection) (RelationalSaturationCarrier sig atom) RewriteRuleId,
      RelationalSaturationRebuild
    )
rebuildRelationalState state = do
  let carrier =
        rsCarrier state
  HostRebuildResult rebuiltHost _rebuildDelta _dirtyResults <-
    first RelationalSaturationHostRebuildFailed (rebuildHostBarrier (rscLiveHost carrier))
  Right
    ( state {rsCarrier = carrier {rscLiveHost = rebuiltHost}},
      RelationalSaturationRebuild (hostRevision rebuiltHost)
    )

relationalSaturationResult ::
  RuntimeState (RelationalFrontSaturation sig atom projection) (RelationalSaturationCarrier sig atom) RewriteRuleId ->
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

data RelationalSaturationApplicationExecution sig atom = RelationalSaturationApplicationExecution
  { rsaeCarrier :: !(RelationalSaturationCarrier sig atom),
    rsaeExecuted :: ![ExecutedRewrite],
    rsaeStats :: !RewriteRunStats
  }

emptyRelationalApplicationExecution ::
  RelationalSaturationCarrier sig atom ->
  RelationalSaturationApplicationExecution sig atom
emptyRelationalApplicationExecution carrier =
  RelationalSaturationApplicationExecution
    { rsaeCarrier = carrier,
      rsaeExecuted = [],
      rsaeStats = emptyRewriteRunStats
    }

applyRelationalMatch ::
  forall sig atom.
  (RewriteSignature sig, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  SaturationConfig sig ->
  Host sig ->
  RelationalSaturationApplicationExecution sig atom ->
  RelationalSaturationSupportedMatch sig atom ->
  Either (RelationalProgramError sig) (RelationalSaturationApplicationExecution sig atom)
applyRelationalMatch config snapshotHost execution supportedMatch = do
  let executableMatch =
        executableSupportedMatch supportedMatch
      carrier =
        rsaeCarrier execution
      liveHost =
        rscLiveHost carrier
  (applicationConditionCache', applicationConditionAccepted) <-
    recheckExecutableApplicationCondition
      (rscApplicationConditionCache carrier)
      snapshotHost
      (rsmRule (rssmMatch supportedMatch))
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
      HostProgramResult host' executedRewrite applicationEffect _editDelta _dirtyResults <-
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
                      rscBannedScheduleKeys =
                        Set.insert
                          (relationalSaturationSupportedScheduleKey supportedMatch)
                          (rscBannedScheduleKeys carrier)
                    }
              }
        else do
          let proofs' =
                recordSuccessfulRewrite
                  executableMatch
                  executedRewrite
                  host'
                  (rscActiveContext carrier)
                  (rscProofs carrier)
          pure
            execution
              { rsaeCarrier =
                  carrier
                    { rscLiveHost = host',
                      rscProofs = proofs',
                      rscApplicationConditionCache = applicationConditionCache'
                    },
                rsaeExecuted = executedRewrite : rsaeExecuted execution,
                rsaeStats = appendRewriteRunStats (rsaeStats execution) (statsForProofSteps 1)
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

recheckExecutableApplicationCondition ::
  (RewriteSignature sig, Ord (GuardCapabilityKey atom), Ord (NodeTag sig)) =>
  RelationalApplicationConditionCache (GuardCapabilityKey atom) sig ->
  Host sig ->
  RelationalSaturationRule sig atom ->
  ExecutableRewriteMatch
    (CompiledGuard (GuardCapabilityKey atom) (Node sig))
    guardEvidence
    guideEvidence
    (Node sig) ->
  Either (RelationalProgramError sig) (RelationalApplicationConditionCache (GuardCapabilityKey atom) sig, Bool)
recheckExecutableApplicationCondition applicationConditionCache host ruleValue executableMatch =
  case rpApplicationCondition (ermRule executableMatch) of
    Nothing ->
      Right (applicationConditionCache, True)
    Just applicationCondition -> do
      (applicationConditionCache', applicationConditionEvidence) <-
        runRelationalApplicationConditionEvaluatorCached
          (rsrApplicationConditionPlans ruleValue)
          applicationConditionCache
          host
          (ermRootClass executableMatch)
          (ermSubstitution executableMatch)
          applicationCondition
      Right (applicationConditionCache', aceDecision applicationConditionEvidence)

recordSuccessfulRewrite ::
  RewriteSignature sig =>
  ExecutableRewriteMatch (CompiledGuard capability (Node sig)) () () (Node sig) ->
  ExecutedRewrite ->
  Host sig ->
  RelationalSaturationContext ->
  ProofRegistry (Node sig) ContextName () ->
  ProofRegistry (Node sig) ContextName ()
recordSuccessfulRewrite executableMatch executedRewrite host activeContext =
  recordProofStepWith
    ( ( defaultProofStepInput
          (executableRewriteMatchRuleKey executableMatch)
          (erwLhsClass executedRewrite)
          (erwRhsClass executedRewrite)
          (ermSubstitution executableMatch)
          ()
      )
        { psiFactDerivations = Set.empty,
          psiContextEvidence = proofContextEvidence activeContext,
          psiLhsWitness = lhsWitness,
          psiRhsWitness = rhsWitness
        }
    )
  where
    witnessFuel =
      max 1 (hostClassCount host + 1)
    lhsWitness =
      hostClassWitness witnessFuel (erwLhsClass executedRewrite) host
    rhsWitness =
      hostClassWitness witnessFuel (erwRhsClass executedRewrite) host

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
  RelationalSaturationApplicationExecution sig atom ->
  RelationalSaturationMatchState sig projection ->
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
