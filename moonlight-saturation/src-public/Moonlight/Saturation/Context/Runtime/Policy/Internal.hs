{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Runtime.Policy.Internal
  ( CarrierAccess (..),
    RuntimePolicy (..),
  )
where

import Data.Functor.Identity (Identity)
import Data.Kind (Type)
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

type CarrierAccess :: Type -> Type -> Type
data CarrierAccess u carrier = CarrierAccess
  { caGraph :: carrier -> SatGraph u,
    caSetGraph :: SatGraph u -> carrier -> carrier
  }

type RuntimePolicy :: Type -> Type -> Type -> Type -> Type
data RuntimePolicy u carrier schedulerGroup report = RuntimePolicy
  { rpCarrier :: !(CarrierAccess u carrier),
    rpCandidateSpace ::
      RuntimeState u carrier schedulerGroup ->
      MatchBatch (SatSupportedMatch u) ->
      CandidateSpace Identity schedulerGroup () (SatSupportedMatch u),
    rpSchedule ::
      SchedulerConfig schedulerGroup ->
      SatRewriteContext u ->
      SaturationRoundView u ->
      CandidateSpace Identity schedulerGroup () (SatSupportedMatch u) ->
      RuntimeState u carrier schedulerGroup ->
      RuntimeScheduleDecision schedulerGroup (SatSupportedMatch u),
    rpApply ::
      SatRewriteContext u ->
      [SatSupportedMatch u] ->
      RuntimeState u carrier schedulerGroup ->
      Either
        (SatApplicationError u)
        (ApplyOutcome (SatApplicationResult u) carrier),
    rpBootstrap ::
      RuntimeState u carrier schedulerGroup ->
      Either
        (SatObstruction u)
        (RuntimeState u carrier schedulerGroup, SatRebuild u),
    rpRebuild ::
      RuntimeState u carrier schedulerGroup ->
      Either
        (SatObstruction u)
        (RuntimeState u carrier schedulerGroup, SatRebuild u),
    rpPostRebuildMatchingDelta ::
      SatMatchState u ->
      [SatSupportedMatch u] ->
      SatApplicationResult u ->
      SatRebuild u ->
      RuntimeState u carrier schedulerGroup ->
      SatMatchingDelta u,
    rpReport ::
      SaturationTermination ->
      RuntimeReportWindow u carrier schedulerGroup ->
      Either (SatObstruction u) report
  }
