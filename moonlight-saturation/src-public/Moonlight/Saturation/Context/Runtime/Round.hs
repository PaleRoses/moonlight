{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Runtime.Round
  ( RuntimeEnv (..),
    RuntimeRound (..),
    runtimeStateGraph,
    setRuntimeStateGraph,
    refreshRuntimeStateFactViewCapabilityGeneration,
    prepareRuntimeInitialState,
    runtimeKernel,
    planRuntimeRound,
    buildRuntimeRound,
  )
where

import Data.Bifunctor (bimap, first)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Moonlight.Core
  ( MatchActivationIndex (..),
    SiteIndex (..),
    SiteProgram (..),
    SupportIndexedRule (..),
  )
import Moonlight.Saturation.Context.Runtime.Policy.Internal
  ( CarrierAccess (..),
    RuntimePolicy (..),
  )
import Moonlight.Saturation.Context.Runtime.Match.Batch
  ( MatchBatch,
    matchBatchFromList,
    matchBatchLength,
    matchBatchNonEmpty,
    matchBatchToList,
  )
import Moonlight.Saturation.Context.Runtime.Match.Pipeline
  ( CandidatePipelineStage (..),
    candidatePipelineIncrement,
    nonNegativeDifference,
  )
import Moonlight.Saturation.Context.Runtime.Schedule.Decision
  ( RuntimeScheduleDecision (..),
  )
import Moonlight.Saturation.Context.Runtime.State
  ( DerivedFactArtifacts (..),
    EligibleMatchArtifacts (..),
    FactDerivationResult (..),
    FactViewKey (..),
    GuidedMatchArtifacts (..),
    RuntimeCore (..),
    RoundArtifacts (..),
    RoundRebuildDelta (..),
    RuntimeState (..),
    ScheduledMatchArtifacts (..),
    advanceRuntimeCoreFactViewGraphChanges,
    invalidateRuntimeCoreFactViews,
    roundArtifactsFrontierComplete,
    roundViewFromParts,
    runtimeCoreFactDerivationsAt,
    runtimeCoreFactsAt,
  )
import Moonlight.Saturation.Context.Error
  ( SaturationRunError (..),
  )
import Moonlight.Saturation.Context.Runtime.Facts
  ( deriveContextFactViews,
  )
import Moonlight.Saturation.Context.Runtime.Match.Candidates
  ( enumerateProjectedBaseSiteMatches,
    enumerateContextSiteMatches,
  )
import Moonlight.Saturation.Context.Runtime.PlanIdentity
  ( compiledContextQueries,
    runtimePlanIdentity,
    stampRuntimePlanIdentity,
  )
import Moonlight.Saturation.Context.Runtime.Round.Input
  ( RoundInput (..),
  )
import Moonlight.Saturation.Context.Program.Plan
  ( CompiledProgram,
    Plan,
    SaturationGuidanceView (..),
    planGuidance,
    planProgram,
    planRewriteContext,
    planRewriteContextSnapshot,
    planSchedulerConfig,
  )
import Moonlight.Saturation.Context.Program.Spec
  ( RewriteContextSnapshot (..),
  )
import Moonlight.Saturation.Context.Match.Algebra.Aggregate
  ( aggregateSupportedMatches,
  )
import Moonlight.Saturation.Core
  ( ApplyOutcome (..),
    RebuildOutcome (..),
    RoundPlan (..),
    SaturationKernel (..),
    TerminationGoal,
  )
import Moonlight.Control.Gate
  ( Gate (..),
    GuideRoundTrace,
    MatchSelector (..),
    MatchSelectorResult (..),
  )
import Moonlight.Saturation.Substrate
import Moonlight.Sheaf.Context.Site
  ( ContextObjectKey,
    PreparedContextSite,
    PreparedContextSupportError,
    SupportCarrier,
    contextObjectKeyFor,
    supportCarrierContainsKey,
    supportCarrierFromSupport,
  )
import Moonlight.Control.Diagnostics.Trace
  ( RoundMetrics (..),
    RoundTrace (..),
    TraceLog,
    appendTraceLogWithPolicy,
    singletonTraceLog,
  )
import Moonlight.FiniteLattice
  ( supportGenerators
  )

type RuntimeEnv :: Type -> Type -> Type -> Type -> Type
data RuntimeEnv u carrier schedulerGroup result = RuntimeEnv
  { rePolicy :: !(RuntimePolicy u carrier schedulerGroup result),
    rePlan :: !(Plan u carrier schedulerGroup),
    reGoal :: !(TerminationGoal (RuntimeState u carrier schedulerGroup))
  }

type RuntimeRound :: Type -> Type -> Type -> Type
data RuntimeRound u carrier schedulerGroup = RuntimeRound
  { rrArtifacts :: !(RoundArtifacts u schedulerGroup),
    rrApplyState :: !(RuntimeState u carrier schedulerGroup)
  }

type CandidateSeekResult :: Type -> Type
data CandidateSeekResult u = CandidateSeekResult
  { csrBaseMatches :: !(MatchBatch (SatSupportedMatch u)),
    csrContextMatches :: !(MatchBatch (SatSupportedMatch u)),
    csrAggregatedMatches :: !(MatchBatch (SatSupportedMatch u)),
    csrMatchState :: !(SatMatchState u)
  }

type RoundProgramCompilation :: Type -> Type
data RoundProgramCompilation u = RoundProgramCompilation
  { rpcFactRules ::
      !( Either
           (PreparedContextSupportError (SatContext u))
           [(SupportIndexedRule (SupportBasis (SatContext u)) (SatFactRule u), SupportCarrier (SatContext u))]
       ),
    rpcRewriteRules ::
      !( Either
           (PreparedContextSupportError (SatContext u))
           [(SupportIndexedRule (SupportBasis (SatContext u)) (SatRule u), SupportCarrier (SatContext u))]
       )
  }

runtimeStateGraph ::
  RuntimePolicy u carrier schedulerGroup result ->
  RuntimeState u carrier schedulerGroup ->
  SatGraph u
runtimeStateGraph ops =
  caGraph (rpCarrier ops) . rsCarrier
{-# INLINE runtimeStateGraph #-}

setRuntimeStateGraph ::
  RuntimePolicy u carrier schedulerGroup result ->
  SatGraph u ->
  RuntimeState u carrier schedulerGroup ->
  RuntimeState u carrier schedulerGroup
setRuntimeStateGraph ops graph state =
  state
    { rsCarrier =
        caSetGraph
          (rpCarrier ops)
          graph
          (rsCarrier state)
    }
{-# INLINE setRuntimeStateGraph #-}

refreshRuntimeStateFactViewCapabilityGeneration ::
  Plan u carrier schedulerGroup ->
  RuntimeState u carrier schedulerGroup ->
  RuntimeState u carrier schedulerGroup
refreshRuntimeStateFactViewCapabilityGeneration plan state =
  let capabilityGeneration =
        rcsCapabilityGeneration
          (planRewriteContextSnapshot plan (rsCarrier state))
   in state
        { rsCore =
            (rsCore state)
              { rcCurrentFactCapabilityGeneration = capabilityGeneration
              }
        }
{-# INLINE refreshRuntimeStateFactViewCapabilityGeneration #-}

derivedFactMatchingDelta ::
  forall u schedulerGroup.
  FactSystem u =>
  SatGraph u ->
  RuntimeCore u schedulerGroup ->
  DerivedFactArtifacts u ->
  SatMatchingDelta u
derivedFactMatchingDelta graph initialCore derivedFacts
  | dfaFactsChanged derivedFacts =
      factChangeMatchingDelta
        @u
        graph
        (rcContextFacts initialCore)
        (fdrFactsByContext (dfaFactDerivationResult derivedFacts))
  | otherwise =
      mempty
{-# INLINE derivedFactMatchingDelta #-}

compileRoundProgram ::
  forall u.
  Ord (SatContext u) =>
  PreparedContextSite (SatContext u) ->
  CompiledProgram u ->
  RoundProgramCompilation u
compileRoundProgram site sourceProgram =
  RoundProgramCompilation
    { rpcFactRules =
        compileSupportedRules site (spSupportedFactRules sourceProgram),
      rpcRewriteRules =
        compileSupportedRules site (Map.elems (spSupportedRewriteRules sourceProgram))
    }
{-# INLINE compileRoundProgram #-}

activateRoundProgram ::
  forall u schedulerGroup.
  (RewriteSystem u, Ord (SatContext u)) =>
  RoundProgramCompilation u ->
  SatGraph u ->
  RuntimeCore u schedulerGroup ->
  SatContext u ->
  CompiledProgram u ->
  Either (PreparedContextSupportError (SatContext u)) (CompiledProgram u)
activateRoundProgram compilation graph coreState baseContext siteProgram = do
  compiledFactRules <- rpcFactRules compilation
  let site =
        graphPreparedSite @u graph
      activeContexts =
        roundProgramActivationContexts @u graph baseContext coreState siteProgram
      activeFactRulesByContext =
        contextualizeCompiledRules site activeContexts compiledFactRules
      (baseFactRules, contextFactRules) =
        splitBaseSiteIndex baseContext activeFactRulesByContext
  pure
    siteProgram
      { spFactRules =
          SiteIndex
            { siBase = siBase (spFactRules siteProgram) <> baseFactRules,
              siContexts = Map.unionWith (<>) (siContexts (spFactRules siteProgram)) contextFactRules
            },
        spRewriteRules = spRewriteRules siteProgram,
        spRewriteActivation = spRewriteActivation siteProgram
      }
{-# INLINE activateRoundProgram #-}

roundProgramActivationContexts ::
  forall u schedulerGroup.
  (SaturationGraph u, Ord (SatContext u)) =>
  SatGraph u ->
  SatContext u ->
  RuntimeCore u schedulerGroup ->
  CompiledProgram u ->
  Set.Set (SatContext u)
roundProgramActivationContexts graph baseContext coreState siteProgram =
  Set.unions
    [ Set.singleton baseContext,
      Set.fromList (graphExecutionContexts @u graph),
      Map.keysSet (rcContextFactInputs coreState),
      Map.keysSet (rcContextFacts coreState),
      Map.keysSet (rcContextFactDerivations coreState),
      Map.keysSet (rcFactViewKeys coreState),
      Map.keysSet (rcFactRoundsByContext coreState),
      Map.keysSet (siContexts (spFactRules siteProgram)),
      Map.keysSet (siContexts (spRewriteRules siteProgram)),
      Map.keysSet (maiContexts (spRewriteActivation siteProgram)),
      supportedRuleGenerators (spSupportedFactRules siteProgram),
      supportedRuleGenerators (Map.elems (spSupportedRewriteRules siteProgram))
    ]
{-# INLINE roundProgramActivationContexts #-}

supportedRuleGenerators ::
  Ord context =>
  [SupportIndexedRule (SupportBasis context) rule] ->
  Set.Set context
supportedRuleGenerators =
  foldMap (Set.fromList . supportGenerators . sirSupport)
{-# INLINE supportedRuleGenerators #-}

contextualizeCompiledRules ::
  Ord context =>
  PreparedContextSite context ->
  Set.Set context ->
  [(SupportIndexedRule (SupportBasis context) rule, SupportCarrier context)] ->
  Map.Map context [rule]
contextualizeCompiledRules site contexts compiledRules =
  Map.filter
    (not . null)
    ( Map.fromList
        [ (contextValue, activeCompiledRulesAt site contextKey compiledRules)
          | (contextValue, contextKey) <- preparedActiveContexts site contexts
        ]
    )
{-# INLINE contextualizeCompiledRules #-}

preparedActiveContexts ::
  Ord context =>
  PreparedContextSite context ->
  Set.Set context ->
  [(context, ContextObjectKey)]
preparedActiveContexts site contexts =
  [ (contextValue, contextKey)
  | contextValue <- Set.toAscList contexts,
    Right contextKey <- [contextObjectKeyFor site contextValue]
  ]
{-# INLINE preparedActiveContexts #-}

compileSupportedRules ::
  Ord context =>
  PreparedContextSite context ->
  [SupportIndexedRule (SupportBasis context) rule] ->
  Either (PreparedContextSupportError context) [(SupportIndexedRule (SupportBasis context) rule, SupportCarrier context)]
compileSupportedRules site =
  traverse
    ( \indexedRule ->
        fmap
          ((,) indexedRule)
          (supportCarrierFromSupport site (sirSupport indexedRule))
    )
{-# INLINE compileSupportedRules #-}

activeCompiledRulesAt ::
  PreparedContextSite context ->
  ContextObjectKey ->
  [(SupportIndexedRule support rule, SupportCarrier context)] ->
  [rule]
activeCompiledRulesAt site contextKey =
  foldMap
    ( \(indexedRule, compiledSupport) ->
        [sirRule indexedRule | supportCarrierContainsKey site compiledSupport contextKey]
    )
{-# INLINE activeCompiledRulesAt #-}

splitBaseSiteIndex ::
  Ord context =>
  context ->
  Map.Map context [rule] ->
  ([rule], Map.Map context [rule])
splitBaseSiteIndex baseContext rulesByContext =
  ( Map.findWithDefault [] baseContext rulesByContext,
    Map.delete baseContext rulesByContext
  )
{-# INLINE splitBaseSiteIndex #-}

prepareRuntimeInitialState ::
  forall u carrier schedulerGroup result.
  RebuildSystem u =>
  RuntimePolicy u carrier schedulerGroup result ->
  Plan u carrier schedulerGroup ->
  RuntimeState u carrier schedulerGroup ->
  Either
    (SaturationRunError u)
    (RuntimeState u carrier schedulerGroup)
prepareRuntimeInitialState ops plan initialState = do
  planIdentity <-
    first SaturationRunSectionObstructed $
      runtimePlanIdentity @u plan

  registeredGraph <-
    first SaturationRunSectionObstructed $
      registerQueries @u
        (compiledContextQueries @u (planProgram plan))
        (runtimeStateGraph ops initialState)

  let invalidatedInitialState =
        initialState
          { rsCore = invalidateRuntimeCoreFactViews (rsCore initialState)
          }
      registeredState =
        stampRuntimePlanIdentity
          planIdentity
          (setRuntimeStateGraph ops registeredGraph invalidatedInitialState)

  (bootstrappedState, bootstrapRebuild) <-
    first SaturationRunSectionObstructed $
      rpBootstrap ops registeredState

  pure
    ( refreshRuntimeStateFactViewCapabilityGeneration
        plan
        (stampRuntimePlanIdentity planIdentity bootstrappedState)
          { rsMatchState =
              advanceMatchStateAfterRebuild
                @u
                bootstrapRebuild
                (rsMatchState bootstrappedState)
          }
    )
{-# INLINE prepareRuntimeInitialState #-}

runtimeKernel ::
  forall u carrier schedulerGroup result.
  ( RebuildSystem u,
    ApplicationResultSystem u,
    Ord (SatRuleKey u),
    Ord (SatContext u),
    Ord (SatClassId u),
    Eq (SatFactStore u),
    Semigroup (SatFactIndex u)
  ) =>
  RuntimeEnv u carrier schedulerGroup result ->
  RuntimeState u carrier schedulerGroup ->
  SaturationKernel
    (RuntimeState u carrier schedulerGroup)
    (RuntimeRound u carrier schedulerGroup)
    (SatSupportedMatch u)
    (SatApplicationResult u)
    (SaturationRunError u)
runtimeKernel env preparedState =
  let compilation =
        compileRoundProgram @u
          (graphPreparedSite @u (runtimeStateGraph (rePolicy env) preparedState))
          (planProgram (rePlan env))
   in SaturationKernel
        { skIterationOf =
            rcIterationCount . rsCore,
          skNodeCountOf =
            graphNodeCount @u . runtimeStateGraph (rePolicy env),
          skGoal =
            reGoal env,
          skPlanRound =
            planRuntimeRound compilation env,
          skApply =
            applyRuntimeMatches env,
          skRebuild =
            rebuildRuntimeRoundState env,
          skCommit =
            finalizeRuntimeRound,
          skConverged =
            runtimeConvergedAfterApply (rePolicy env)
        }

planRuntimeRound ::
  forall u carrier schedulerGroup result.
  ( RebuildSystem u,
    Ord (SatRuleKey u),
    Ord (SatContext u),
    Ord (SatClassId u),
    Eq (SatFactStore u),
    Semigroup (SatFactIndex u)
  ) =>
  RoundProgramCompilation u ->
  RuntimeEnv u carrier schedulerGroup result ->
  RuntimeState u carrier schedulerGroup ->
  Either
    (SaturationRunError u)
    ( RoundPlan
        (RuntimeRound u carrier schedulerGroup)
        (RuntimeState u carrier schedulerGroup)
        (SatSupportedMatch u)
    )
planRuntimeRound compilation env state = do
  roundValue <- buildRuntimeRound compilation env state

  let artifacts =
        rrArtifacts roundValue
      applyState =
        rrApplyState roundValue

  case matchBatchNonEmpty (smaMatches (raSchedule artifacts)) of
    Just scheduledMatches ->
      Right (ApplyRound roundValue applyState scheduledMatches)
    Nothing
      | roundArtifactsFrontierComplete artifacts
          && not (dfaFactsChanged (raDerivedFacts artifacts)) ->
          Right (StopRound applyState)
      | otherwise ->
          Right (AdvanceRound (advanceWithoutApply roundValue))
{-# INLINE planRuntimeRound #-}

buildRuntimeRound ::
  forall u carrier schedulerGroup result.
  ( RebuildSystem u,
    Ord (SatRuleKey u),
    Ord (SatContext u),
    Ord (SatClassId u),
    Eq (SatFactStore u),
    Semigroup (SatFactIndex u)
  ) =>
  RoundProgramCompilation u ->
  RuntimeEnv u carrier schedulerGroup result ->
  RuntimeState u carrier schedulerGroup ->
  Either
    (SaturationRunError u)
    (RuntimeRound u carrier schedulerGroup)
buildRuntimeRound compilation env state = do
  let ops =
        rePolicy env
      plan =
        rePlan env
      sourceProgram =
        planProgram plan
      core =
        rsCore state
      matchState =
        rsMatchState state
      graph =
        runtimeStateGraph ops state
      baseContext =
        graphBaseContext @u graph
      baseGraph =
        graphBase @u graph
      rewriteContextSnapshot =
        planRewriteContextSnapshot plan (rsCarrier state)
      rewriteContext =
        rcsRewriteContext rewriteContextSnapshot
      capabilityGeneration =
        rcsCapabilityGeneration rewriteContextSnapshot
      capabilityResolver =
        rewriteCapabilityResolver @u rewriteContext graph

  siteProgram <-
    first SaturationRunSupportContextLookupFailed $
      activateRoundProgram @u compilation graph core baseContext sourceProgram

  compiledSupportedRewriteRules <-
    first SaturationRunSupportContextLookupFailed $
      rpcRewriteRules compilation

  let factProgram =
        spFactRules siteProgram

  factDerivationResult <-
    first SaturationRunSectionObstructed $
      deriveContextFactViews @u
        capabilityResolver
        capabilityGeneration
        factProgram
        graph
        core

  let refreshedContextFacts =
        fdrFactsByContext factDerivationResult

      refreshedContextFactDerivations =
        fdrFactDerivationsByContext factDerivationResult

      factRoundsByContext =
        fdrFactRoundsByContext factDerivationResult

      factsChanged =
        refreshedContextFacts /= rcContextFacts core

      derivedFacts =
        DerivedFactArtifacts
          { dfaFactDerivationResult = factDerivationResult,
            dfaFactsChanged = factsChanged
          }

      factDelta =
        derivedFactMatchingDelta @u graph core derivedFacts

      effectiveMatchingDelta =
        rcMatchingDelta core <> factDelta

      refreshedCore =
        core
          { rcContextFacts = refreshedContextFacts,
            rcContextFactDerivations = refreshedContextFactDerivations,
            rcCurrentFactRuleIdsByContext =
              fmap
                fvkFactRuleIds
                (fdrFactViewKeysByContext factDerivationResult),
            rcCurrentFactCapabilityGeneration = capabilityGeneration,
            rcFactViewKeys =
              fdrFactViewKeysByContext factDerivationResult,
            rcFactRoundsByContext =
              Map.unionWith
                (<>)
                (rcFactRoundsByContext core)
                factRoundsByContext,
            rcFactRoundCount =
              rcFactRoundCount core
                + fdrFactRoundCount factDerivationResult,
            rcMatchingDelta = effectiveMatchingDelta
          }

      refreshedState =
        state {rsCore = refreshedCore}

      roundInput =
        RoundInput
          { riState = refreshedState,
            riGraph = graph,
            riBaseContext = baseContext,
            riBaseGraph = baseGraph,
            riBaseFacts =
              runtimeCoreFactsAt @u baseContext refreshedCore,
            riBaseFactDerivations =
              runtimeCoreFactDerivationsAt @u baseContext refreshedCore,
            riRewriteContext = rewriteContext,
            riCapabilityResolver = capabilityResolver
          }

      roundMatchState =
        advanceMatchStateForRound
          @u
          effectiveMatchingDelta
          graph
          matchState

  candidateSeek <-
    first SaturationRunSectionObstructed $
      seekRoundCandidates
        @u
        roundInput
        ops
        siteProgram
        compiledSupportedRewriteRules
        (rcIterationCount core)
        roundMatchState

  let baseEligibleBatch =
        csrBaseMatches candidateSeek

      contextEligibleBatch =
        csrContextMatches candidateSeek

      aggregatedEligibleBatch =
        csrAggregatedMatches candidateSeek

      aggregatedEligibleMatches =
        matchBatchToList aggregatedEligibleBatch

      nextMatchState =
        csrMatchState candidateSeek

      matchReadyState =
        refreshedState
          { rsMatchState = nextMatchState
          }

      eligibleMatches =
        EligibleMatchArtifacts
          { emaBaseMatches = baseEligibleBatch,
            emaContextMatches = contextEligibleBatch,
            emaAggregatedMatches = aggregatedEligibleBatch
          }

      roundView =
        roundViewFromParts @u
          core
          graph
          baseGraph
          baseContext
          derivedFacts
          eligibleMatches

      guidanceView =
        SaturationGuidanceView
          { sgvRoundView = roundView,
            sgvCandidates = aggregatedEligibleMatches
          }

      MatchSelectorResult
        { msrAcceptedMatches = guidedMatches,
          msrRejectedCount = guidanceRejectedCount,
          msrTrace = guideTraces
        } =
        runFiniteGuidance
          (planGuidance plan)
          guidanceView

      guidedMatchBatch =
        matchBatchFromList guidedMatches

      guidanceArtifacts =
        GuidedMatchArtifacts
          { gmaMatches = guidedMatchBatch,
            gmaTraceDelta = Vector.fromList guideTraces,
            gmaAllCandidatesAccepted = guidanceRejectedCount == 0
          }

      scheduleDecision =
        rpSchedule
          ops
          (planSchedulerConfig plan)
          rewriteContext
          roundView
          (rpCandidateSpace ops matchReadyState guidedMatchBatch)
          matchReadyState

      scheduledMatches =
        rsdScheduledMatches scheduleDecision

      roundPipelineCounts =
        foldr
          ($)
          (rsdPipelineCounts scheduleDecision)
          [ candidatePipelineIncrement CandidateEligibleBase (matchBatchLength baseEligibleBatch),
            candidatePipelineIncrement CandidateEligibleContext (matchBatchLength contextEligibleBatch),
            candidatePipelineIncrement CandidateEligibleAggregated (matchBatchLength aggregatedEligibleBatch),
            candidatePipelineIncrement
              CandidateDroppedByGuidance
              ( nonNegativeDifference
                  (matchBatchLength aggregatedEligibleBatch)
                  (matchBatchLength guidedMatchBatch)
              )
          ]

      scheduleArtifacts =
        ScheduledMatchArtifacts
          { smaMatches = scheduledMatches,
            smaTracePolicy = rsdTracePolicy scheduleDecision,
            smaTraceDelta = rsdTraceDelta scheduleDecision,
            smaAllCandidatesScheduled =
              rsdAllCandidatesScheduled scheduleDecision,
            smaPipelineCounts = roundPipelineCounts
          }

      frontierComplete =
        gmaAllCandidatesAccepted guidanceArtifacts
          && smaAllCandidatesScheduled scheduleArtifacts

      carryMatchingDelta =
        if frontierComplete
          then factDelta
          else effectiveMatchingDelta

      scheduledHostState =
        matchReadyState
          { rsScheduler = rsdSchedulerState scheduleDecision
          }

      recordedMatchState =
        recordScheduledMatches
          @u
          (matchBatchToList scheduledMatches)
          nextMatchState

      applyInputState =
        scheduledHostState
          { rsMatchState = recordedMatchState
          }

      artifactsWithoutTrace =
        RoundArtifacts
          { raInitialCore = core,
            raGraphBefore = graph,
            raBaseGraphBefore = baseGraph,
            raBaseContext = baseContext,
            raDerivedFacts = derivedFacts,
            raEligibleMatches = eligibleMatches,
            raGuidance = guidanceArtifacts,
            raSchedule = scheduleArtifacts,
            raTraceDelta = mempty,
            raNoApplyMatchingDelta = carryMatchingDelta,
            raRebuildDelta = Nothing
          }

      artifacts =
        artifactsWithoutTrace
          { raTraceDelta =
              roundTraceLogDelta @u graph artifactsWithoutTrace
          }

  pure
    RuntimeRound
      { rrArtifacts = artifacts,
        rrApplyState = applyInputState
      }
{-# INLINE buildRuntimeRound #-}

seekRoundCandidates ::
  forall u carrier schedulerGroup result.
  ( RebuildSystem u,
    Ord (SatRuleKey u),
    Ord (SatContext u),
    Ord (SatClassId u),
    Semigroup (SatFactIndex u)
  ) =>
  RoundInput u carrier schedulerGroup ->
  RuntimePolicy u carrier schedulerGroup result ->
  CompiledProgram u ->
  [(SupportIndexedRule (SupportBasis (SatContext u)) (SatRule u), SupportCarrier (SatContext u))] ->
  Int ->
  SatMatchState u ->
  Either (SatObstruction u) (CandidateSeekResult u)
seekRoundCandidates roundInput _policy siteProgram supportedRules iterationIndex roundMatchState =
  freshCandidateSeek @u roundInput siteProgram supportedRules iterationIndex roundMatchState
{-# INLINE seekRoundCandidates #-}

freshCandidateSeek ::
  forall u carrier schedulerGroup.
  ( RebuildSystem u,
    Ord (SatRuleKey u),
    Ord (SatContext u),
    Ord (SatClassId u),
    Semigroup (SatFactIndex u)
  ) =>
  RoundInput u carrier schedulerGroup ->
  CompiledProgram u ->
  [(SupportIndexedRule (SupportBasis (SatContext u)) (SatRule u), SupportCarrier (SatContext u))] ->
  Int ->
  SatMatchState u ->
  Either (SatObstruction u) (CandidateSeekResult u)
freshCandidateSeek roundInput siteProgram supportedRules iterationIndex roundMatchState = do
  (matchStateAfterBase, baseEligibleMatches) <-
    enumerateProjectedBaseSiteMatches @u
      roundInput
      iterationIndex
      roundMatchState
      siteProgram

  (nextMatchState, contextEligibleMatches) <-
    enumerateContextSiteMatches @u
      roundInput
      iterationIndex
      matchStateAfterBase
      (siContexts (spRewriteRules siteProgram))
      supportedRules

  aggregatedEligibleMatches <-
    aggregateSupportedMatches @u
      (riGraph roundInput)
      (baseEligibleMatches <> contextEligibleMatches)

  Right
    CandidateSeekResult
      { csrBaseMatches = matchBatchFromList baseEligibleMatches,
        csrContextMatches = matchBatchFromList contextEligibleMatches,
        csrAggregatedMatches = matchBatchFromList aggregatedEligibleMatches,
        csrMatchState = nextMatchState
      }
{-# INLINE freshCandidateSeek #-}

runFiniteGuidance ::
  Gate
    (SaturationGuidanceView u)
    ()
    (SatSupportedMatch u)
    GuideRoundTrace
    schedulerGroup ->
  SaturationGuidanceView u ->
  MatchSelectorResult (SatSupportedMatch u) GuideRoundTrace
runFiniteGuidance guidanceValue guidanceView =
  runMatchSelector
    (gateSelector guidanceValue)
    guidanceView
    ()
    (sgvCandidates guidanceView)
{-# INLINE runFiniteGuidance #-}

appendRoundTraceAndGuides ::
  RuntimeCore u schedulerGroup ->
  RoundArtifacts u schedulerGroup ->
  RuntimeCore u schedulerGroup ->
  RuntimeCore u schedulerGroup
appendRoundTraceAndGuides initialCore artifacts core =
  let guideTraceDelta =
        Seq.fromList (Vector.toList (gmaTraceDelta (raGuidance artifacts)))
      guideTraceDeltaCount =
        Seq.length guideTraceDelta
      tracePolicy =
        smaTracePolicy (raSchedule artifacts)
   in core
        { rcIterationCount = rcIterationCount initialCore + 1,
          rcTrace =
            appendTraceLogWithPolicy
              tracePolicy
              (rcTrace initialCore)
              (raTraceDelta artifacts),
          rcGuideTrace = rcGuideTrace initialCore Seq.>< guideTraceDelta,
          rcGuideRoundCount =
            rcGuideRoundCount initialCore + guideTraceDeltaCount
        }

advanceWithoutApply ::
  RuntimeRound u carrier schedulerGroup ->
  RuntimeState u carrier schedulerGroup
advanceWithoutApply roundValue =
  let artifacts =
        rrArtifacts roundValue
      state =
        rrApplyState roundValue
      initialCore =
        raInitialCore artifacts
      coreWithTrace =
        appendRoundTraceAndGuides initialCore artifacts (rsCore state)
      nextCore =
        coreWithTrace
          { rcMatchingDelta = raNoApplyMatchingDelta artifacts
          }
   in state {rsCore = nextCore}

applyRuntimeMatches ::
  forall u carrier schedulerGroup result.
  RuntimeEnv u carrier schedulerGroup result ->
  NonEmpty (SatSupportedMatch u) ->
  RuntimeState u carrier schedulerGroup ->
  Either
    (SaturationRunError u)
    (ApplyOutcome (SatApplicationResult u) (RuntimeState u carrier schedulerGroup))
applyRuntimeMatches env matches state =
  let ops =
        rePolicy env
      rewriteContext =
        planRewriteContext (rePlan env) (rsCarrier state)
   in bimap
        SaturationRunApplyFailed
        ( \carrierOutcome ->
            ApplyOutcome
              { aoState = state {rsCarrier = aoState carrierOutcome},
                aoEffect = aoEffect carrierOutcome
              }
        )
        (rpApply ops rewriteContext (NonEmpty.toList matches) state)

rebuildRuntimeRoundState ::
  forall u carrier schedulerGroup result.
  (RebuildSystem u, Ord (SatContext u)) =>
  RuntimeEnv u carrier schedulerGroup result ->
  RuntimeRound u carrier schedulerGroup ->
  SatApplicationResult u ->
  RuntimeState u carrier schedulerGroup ->
  Either
    (SaturationRunError u)
    (RebuildOutcome (RuntimeRound u carrier schedulerGroup) (RuntimeState u carrier schedulerGroup))
rebuildRuntimeRoundState env roundValue appliedResult state = do
  let ops =
        rePolicy env

  (rebuiltState, rebuildReport) <-
    first SaturationRunSectionObstructed $
      rpRebuild ops state

  let artifacts =
        rrArtifacts roundValue
      preRebuildMatchState =
        rsMatchState state
      rebuiltMatchState =
        advanceMatchStateAfterRebuild
          @u
          rebuildReport
          preRebuildMatchState
      postRebuildDelta =
        rpPostRebuildMatchingDelta
          ops
          preRebuildMatchState
          (matchBatchToList (smaMatches (raSchedule artifacts)))
          appliedResult
          rebuildReport
          rebuiltState
      postRebuildChangeSummary =
        postApplyChangeSummary
          @u
          preRebuildMatchState
          (matchBatchToList (smaMatches (raSchedule artifacts)))
          appliedResult
          rebuildReport
      nextMatchingDelta =
        raNoApplyMatchingDelta artifacts <> postRebuildDelta
      rebuildDelta =
        RoundRebuildDelta
          { rrdMatchingDelta = nextMatchingDelta,
            rrdContextRevision = rebuildEpoch @u rebuildReport
          }
      rebuiltCore =
        rsCore rebuiltState
      rebuiltCoreWithDelta =
        rebuiltCore
          { rcMatchingDelta = rrdMatchingDelta rebuildDelta,
            rcChangeSummary = rcChangeSummary rebuiltCore <> postRebuildChangeSummary,
            rcContextRevision = rrdContextRevision rebuildDelta
          }
      stateWithRebuiltGraph =
        rebuiltState
          { rsCore = rebuiltCoreWithDelta,
            rsMatchState = rebuiltMatchState
          }
      graphAfter =
        runtimeStateGraph ops stateWithRebuiltGraph
  factViewAdvancedCore <-
    first SaturationRunSupportContextLookupFailed $
      advanceRuntimeCoreFactViewGraphChanges @u
        (graphPreparedSite @u graphAfter)
        (factViewGraphChanges @u postRebuildChangeSummary)
        rebuiltCoreWithDelta

  let rebuiltStateWithDelta =
        refreshRuntimeStateFactViewCapabilityGeneration
          (rePlan env)
          stateWithRebuiltGraph
            { rsCore = factViewAdvancedCore,
              rsMatchState =
                recordApplicationResult
                  @u
                  graphAfter
                  appliedResult
                  rebuiltMatchState
            }
      artifactsWithRebuild =
        artifacts
          { raRebuildDelta = Just rebuildDelta
          }
      rebuiltArtifacts =
        artifactsWithRebuild
          { raTraceDelta =
              roundTraceLogDelta @u graphAfter artifactsWithRebuild
          }

  pure
    RebuildOutcome
      { roRound =
          roundValue {rrArtifacts = rebuiltArtifacts},
        roState = rebuiltStateWithDelta
      }

finalizeRuntimeRound ::
  forall u carrier schedulerGroup.
  ApplicationResultSystem u =>
  RuntimeRound u carrier schedulerGroup ->
  SatApplicationResult u ->
  RuntimeState u carrier schedulerGroup ->
  RuntimeState u carrier schedulerGroup
finalizeRuntimeRound roundValue appliedResult rebuiltState =
  let artifacts =
        rrArtifacts roundValue
      initialCore =
        raInitialCore artifacts
      appliedCount =
        applicationResultCount @u appliedResult
      coreWithTrace =
        appendRoundTraceAndGuides initialCore artifacts (rsCore rebuiltState)
      finalizedCore =
        coreWithTrace
          { rcTotalMatches =
              rcTotalMatches initialCore + appliedCount
          }
   in rebuiltState {rsCore = finalizedCore}

runtimeConvergedAfterApply ::
  forall u carrier schedulerGroup result.
  RebuildSystem u =>
  RuntimePolicy u carrier schedulerGroup result ->
  RuntimeRound u carrier schedulerGroup ->
  RuntimeState u carrier schedulerGroup ->
  Bool
runtimeConvergedAfterApply ops roundValue rebuiltState =
  let artifacts =
        rrArtifacts roundValue
      graphBefore =
        raGraphBefore artifacts
      graphAfter =
        runtimeStateGraph ops rebuiltState
   in roundArtifactsFrontierComplete artifacts
        && graphConvergenceStateEquals @u graphBefore graphAfter

roundTraceLogDelta ::
  forall u schedulerGroup.
  SaturationGraph u =>
  SatGraph u ->
  RoundArtifacts u schedulerGroup ->
  TraceLog (SatRuleKey u) schedulerGroup
roundTraceLogDelta graphAfter artifacts =
  let initialCore =
        raInitialCore artifacts
      eligibleMatches =
        raEligibleMatches artifacts
      guidance =
        raGuidance artifacts
      schedule =
        raSchedule artifacts
      derivedFacts =
        raDerivedFacts artifacts
   in singletonTraceLog
        RoundTrace
          { roundTraceMetrics =
              RoundMetrics
                { rmIteration =
                    rcIterationCount initialCore,
                  rmNodeCountBefore =
                    graphNodeCount @u (raGraphBefore artifacts),
                  rmNodeCountAfter =
                    graphNodeCount @u graphAfter,
                  rmBaseEligibleCount =
                    matchBatchLength (emaBaseMatches eligibleMatches),
                  rmContextEligibleCount =
                    matchBatchLength (emaContextMatches eligibleMatches),
                  rmAggregatedEligibleCount =
                    matchBatchLength (emaAggregatedMatches eligibleMatches),
                  rmGuidedCount =
                    matchBatchLength (gmaMatches guidance),
                  rmScheduledCount =
                    matchBatchLength (smaMatches schedule),
                  rmFactsChanged =
                    dfaFactsChanged derivedFacts,
                  rmFactRoundCount =
                    fdrFactRoundCount (dfaFactDerivationResult derivedFacts),
                  rmContextRevision =
                    rcContextRevision initialCore
                },
            roundTraceSchedule =
              Vector.toList (smaTraceDelta schedule)
          }
