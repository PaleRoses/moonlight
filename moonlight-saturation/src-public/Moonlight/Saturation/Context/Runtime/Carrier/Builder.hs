{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Runtime.Carrier.Builder
  ( mkRuntimePolicy,
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
import Moonlight.Saturation.Context.Runtime.Match.Batch
  ( MatchBatch,
  )
import Moonlight.Saturation.Context.Runtime.Policy.Internal
  ( CarrierAccess,
    RuntimePolicy (..),
  )
import Moonlight.Saturation.Context.Runtime.Schedule.Decision
  ( RuntimeScheduleDecision,
  )
import Moonlight.Saturation.Context.Runtime.State
  ( RuntimeReportWindow,
    RuntimeState,
  )
import Moonlight.Saturation.Core
  ( ApplyOutcome,
    SaturationTermination,
  )
import Moonlight.Saturation.Substrate

mkRuntimePolicy ::
  CarrierAccess u carrier ->
  ( RuntimeState u carrier schedulerGroup ->
    MatchBatch (SatSupportedMatch u) ->
    CandidateSpace Identity schedulerGroup () (SatSupportedMatch u)
  ) ->
  ( SchedulerConfig schedulerGroup ->
    SatRewriteContext u ->
    SaturationRoundView u ->
    CandidateSpace Identity schedulerGroup () (SatSupportedMatch u) ->
    RuntimeState u carrier schedulerGroup ->
    RuntimeScheduleDecision schedulerGroup (SatSupportedMatch u)
  ) ->
  ( SatRewriteContext u ->
    [SatSupportedMatch u] ->
    RuntimeState u carrier schedulerGroup ->
    Either
      (SatApplicationError u)
      (ApplyOutcome (SatApplicationResult u) carrier)
  ) ->
  ( RuntimeState u carrier schedulerGroup ->
    Either
      (SatObstruction u)
      (RuntimeState u carrier schedulerGroup, SatRebuild u)
  ) ->
  ( RuntimeState u carrier schedulerGroup ->
    Either
      (SatObstruction u)
      (RuntimeState u carrier schedulerGroup, SatRebuild u)
  ) ->
  ( SatMatchState u ->
    [SatSupportedMatch u] ->
    SatApplicationResult u ->
    SatRebuild u ->
    RuntimeState u carrier schedulerGroup ->
    SatMatchingDelta u
  ) ->
  (SaturationTermination -> RuntimeReportWindow u carrier schedulerGroup -> Either (SatObstruction u) result) ->
  RuntimePolicy u carrier schedulerGroup result
mkRuntimePolicy carrierOps candidateSpace scheduleMatches applyCarrier bootstrapState rebuildState postRebuildMatchingDelta finalReport =
  RuntimePolicy
    { rpCarrier = carrierOps,
      rpCandidateSpace = candidateSpace,
      rpSchedule = scheduleMatches,
      rpApply = applyCarrier,
      rpBootstrap = bootstrapState,
      rpRebuild = rebuildState,
      rpPostRebuildMatchingDelta = postRebuildMatchingDelta,
      rpReport = finalReport
    }
{-# INLINE mkRuntimePolicy #-}
