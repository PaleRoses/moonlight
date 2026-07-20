{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Runtime.Carrier.Proof
  ( proofCarrierAccess,
    proofRuntimePolicy,
    proofRuntimePolicyWith,
    proofRuntimePolicyWithSummary,
  )
where

import Data.Functor.Identity
  ( Identity,
  )
import Moonlight.Control.Candidate
  ( CandidateSpace,
  )
import Moonlight.Control.Schedule
  ( SchedulerConfig,
  )
import Moonlight.Saturation.Context.Program.View
  ( SaturationRoundView,
  )
import Moonlight.Saturation.Context.Runtime.Carrier.Builder
  ( mkRuntimePolicy,
  )
import Moonlight.Saturation.Context.Runtime.Carrier.Schedule
  ( candidateSpaceForSupportedMatches,
    compareSupportedMatches,
    scheduleRoundSupportedMatches,
    supportedMatchRuleKey,
  )
import Moonlight.Saturation.Context.Runtime.Policy.Internal
  ( CarrierAccess (..),
    RuntimePolicy (..),
  )
import Moonlight.Saturation.Context.Runtime.Match.Batch
  ( MatchBatch,
  )
import Moonlight.Saturation.Context.Runtime.Schedule.Decision
  ( RuntimeScheduleDecision,
  )
import Moonlight.Saturation.Context.Runtime.Rebuild
  ( rebuildRuntimeState,
  )
import Moonlight.Saturation.Context.Runtime.Report
  ( ProofSaturationReport,
    SaturationReportOf,
    mkReport,
    mkReportOf,
  )
import Moonlight.Saturation.Context.Runtime.State
  ( RuntimeReportWindow (..),
    RuntimeCore (..),
    RuntimeState (..),
  )
import Moonlight.Saturation.Substrate

proofCarrierAccess ::
  forall u p.
  ProofCarrier u p =>
  CarrierAccess u (SatProofGraph u p)
proofCarrierAccess =
  CarrierAccess
    { caGraph = proofGraphContext @u @p,
      caSetGraph = setProofGraphContext @u @p
    }
{-# INLINE proofCarrierAccess #-}

proofRuntimePolicy ::
  forall u p.
  ( ProofCarrier u p,
    Ord (SatRuleKey u),
    Ord (SatContext u),
    Ord (SatClassId u)
  ) =>
  SatProofBuilder u p ->
  Maybe (SatContext u) ->
  RuntimePolicy
    u
    (SatProofGraph u p)
    (SatRuleKey u)
    (ProofSaturationReport u (SatProofGraph u p))
proofRuntimePolicy proofBuilder activeContext =
  proofRuntimePolicyWith
    @u
    @p
    (\_state matches -> candidateSpaceForSupportedMatches @u (supportedMatchRuleKey @u) (compareSupportedMatches @u) matches)
    (scheduleRoundSupportedMatches @u)
    proofBuilder
    activeContext
{-# INLINE proofRuntimePolicy #-}

proofRuntimePolicyWith ::
  forall u p schedulerGroup.
  ( ProofCarrier u p,
    Ord (SatContext u)
  ) =>
  ( RuntimeState u (SatProofGraph u p) schedulerGroup ->
    MatchBatch (SatSupportedMatch u) ->
    CandidateSpace Identity schedulerGroup () (SatSupportedMatch u)
  ) ->
  ( SchedulerConfig schedulerGroup ->
    SatRewriteContext u ->
    SaturationRoundView u ->
    CandidateSpace Identity schedulerGroup () (SatSupportedMatch u) ->
    RuntimeState u (SatProofGraph u p) schedulerGroup ->
    RuntimeScheduleDecision schedulerGroup (SatSupportedMatch u)
  ) ->
  SatProofBuilder u p ->
  Maybe (SatContext u) ->
  RuntimePolicy
    u
    (SatProofGraph u p)
    schedulerGroup
    (SaturationReportOf u (SatProofGraph u p) schedulerGroup ())
proofRuntimePolicyWith candidateSpace scheduleMatches proofBuilder activeContext =
  let carrierOps =
        proofCarrierAccess @u @p
      rebuildProofRuntimeState =
        rebuildRuntimeState @u carrierOps
   in mkRuntimePolicy @u
        carrierOps
        candidateSpace
        scheduleMatches
        applyProof
        rebuildProofRuntimeState
        rebuildProofRuntimeState
        postProofRebuildDelta
        (mkReport carrierOps)
  where
    applyProof rewriteContext matches state =
      applyProofMatches
        @u
        @p
        rewriteContext
        proofBuilder
        activeContext
        matches
        (rsCarrier state)

    postProofRebuildDelta _matchState _scheduledMatches _applicationResult rebuildReport _rebuiltState =
      rebuildMatchingDelta @u rebuildReport
{-# INLINE proofRuntimePolicyWith #-}

proofRuntimePolicyWithSummary ::
  forall u p schedulerGroup.
  ( ProofCarrier u p,
    Ord (SatContext u)
  ) =>
  ( RuntimeState u (SatProofGraph u p) schedulerGroup ->
    MatchBatch (SatSupportedMatch u) ->
    CandidateSpace Identity schedulerGroup () (SatSupportedMatch u)
  ) ->
  ( SchedulerConfig schedulerGroup ->
    SatRewriteContext u ->
    SaturationRoundView u ->
    CandidateSpace Identity schedulerGroup () (SatSupportedMatch u) ->
    RuntimeState u (SatProofGraph u p) schedulerGroup ->
    RuntimeScheduleDecision schedulerGroup (SatSupportedMatch u)
  ) ->
  SatProofBuilder u p ->
  Maybe (SatContext u) ->
  RuntimePolicy
    u
    (SatProofGraph u p)
    schedulerGroup
    (SaturationReportOf u (SatProofGraph u p) schedulerGroup (SatChangeSummary u))
proofRuntimePolicyWithSummary candidateSpace scheduleMatches proofBuilder activeContext =
  let policy =
        proofRuntimePolicyWith
          @u
          @p
          candidateSpace
          scheduleMatches
          proofBuilder
          activeContext
   in policy
        { rpReport =
            \termination window ->
              mkReportOf
                (rpCarrier policy)
                termination
                window
                (rcChangeSummary (rsCore (rrwFinalState window)))
        }
{-# INLINE proofRuntimePolicyWithSummary #-}
