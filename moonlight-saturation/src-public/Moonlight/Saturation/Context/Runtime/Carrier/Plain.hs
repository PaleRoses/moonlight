{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Runtime.Carrier.Plain
  ( plainRuntimePolicy,
    plainRuntimePolicyWith,
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
import Moonlight.Saturation.Context.Runtime.Carrier.Access
  ( plainCarrierAccess,
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
  ( RuntimePolicy,
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
  ( SaturationReport,
    SaturationReportOf,
    mkReport,
  )
import Moonlight.Saturation.Context.Runtime.State
  ( RuntimeState (..),
    runtimeCoreFactsAt,
  )
import Moonlight.Saturation.Substrate
import Moonlight.FiniteLattice
  ( principalSupport
  )

plainRuntimePolicy ::
  forall u.
  ( RebuildSystem u,
    GraphApply u,
    Ord (SatRuleKey u),
    Ord (SatContext u),
    Ord (SatClassId u)
  ) =>
  RuntimePolicy u (SatGraph u) (SatRuleKey u) (SaturationReport u)
plainRuntimePolicy =
  plainRuntimePolicyWith
    (\_state matches -> candidateSpaceForSupportedMatches @u (supportedMatchRuleKey @u) (compareSupportedMatches @u) matches)
    (scheduleRoundSupportedMatches @u)
{-# INLINE plainRuntimePolicy #-}

plainRuntimePolicyWith ::
  forall u schedulerGroup.
  ( RebuildSystem u,
    GraphApply u,
    Ord (SatContext u)
  ) =>
  ( RuntimeState u (SatGraph u) schedulerGroup ->
    MatchBatch (SatSupportedMatch u) ->
    CandidateSpace Identity schedulerGroup () (SatSupportedMatch u)
  ) ->
  ( SchedulerConfig schedulerGroup ->
    SatRewriteContext u ->
    SaturationRoundView u ->
    CandidateSpace Identity schedulerGroup () (SatSupportedMatch u) ->
    RuntimeState u (SatGraph u) schedulerGroup ->
    RuntimeScheduleDecision schedulerGroup (SatSupportedMatch u)
  ) ->
  RuntimePolicy
    u
    (SatGraph u)
    schedulerGroup
    (SaturationReportOf u (SatGraph u) schedulerGroup ())
plainRuntimePolicyWith candidateSpace scheduleMatches =
  mkRuntimePolicy @u
    plainCarrierAccess
    candidateSpace
    scheduleMatches
    plainApplyDispatched
    (rebuildRuntimeState @u plainCarrierAccess)
    (rebuildRuntimeState @u plainCarrierAccess)
    ( \matchState scheduledMatches applicationResult rebuildReport _state ->
        postApplyMatchingDelta @u matchState scheduledMatches applicationResult rebuildReport
    )
    (mkReport plainCarrierAccess)
  where
    plainApplyDispatched rewriteContext matches state =
      let graph =
            rsCarrier state
          baseContext =
            graphBaseContext @u graph
          baseFacts =
            runtimeCoreFactsAt @u baseContext (rsCore state)
          baseSupport =
            principalSupport baseContext
       in if all ((== baseSupport) . supportedMatchBasis @u) matches
            then
              applyBaseMatches
                @u
                rewriteContext
                baseFacts
                matches
                graph
            else
              applyContextualMatches
                @u
                rewriteContext
                matches
                graph
{-# INLINE plainRuntimePolicyWith #-}
