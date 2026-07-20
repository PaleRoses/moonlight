{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Support.Algebra
  ( supportRuntimePolicy,
  )
where

import Data.Ord (comparing)
import Moonlight.Core (Substitution)
import Moonlight.Saturation.Context.Runtime.Carrier.Proof
  ( proofRuntimePolicyWithSummary,
  )
import Moonlight.Saturation.Context.Runtime.Carrier.Schedule
  ( candidateSpaceForSupportedMatches,
    scheduleRoundSupportedMatches,
  )
import Moonlight.Saturation.Context.Runtime.Policy
  ( RuntimePolicy,
  )
import Moonlight.Control.Schedule
  ( ScheduleGroup (..),
    SchedulerRefinement,
    applySchedulerRefinement,
  )
import Moonlight.Control.Schedule.Round
  ( ScheduleTrace,
  )
import Moonlight.Saturation.Substrate
import Moonlight.Saturation.Support.Core
  ( SupportSaturationReportFor,
    SupportScheduleGroup,
    supportSchedulerView,
  )
import Moonlight.Sheaf.Twist.Schedule qualified as SheafTwist
supportRuntimePolicy ::
  forall u p.
  ( ProofCarrier u p,
    Ord (SatRuleKey u),
    Ord (SatContext u),
    Ord (SupportBasis (SatContext u)),
    Ord (SatClassId u)
  ) =>
  SchedulerRefinement
    (SheafTwist.SupportSchedulerView
       (SatProofGraph u p)
       (ScheduleTrace (SupportScheduleGroup u)))
    (SupportScheduleGroup u) ->
  SatProofBuilder u p ->
  RuntimePolicy
    u
    (SatProofGraph u p)
    (SupportScheduleGroup u)
    (SupportSaturationReportFor u (SatProofGraph u p))
supportRuntimePolicy schedulerRefinement proofBuilder =
  proofRuntimePolicyWithSummary
    @u
    @p
    (\_state matches -> candidateSpaceForSupportedMatches @u (supportScheduleGroup @u) (compareSupportMatches @u) matches)
    scheduleSupportMatches
    proofBuilder
    Nothing
  where
    scheduleSupportMatches schedulerConfig rewriteContext roundView candidateSpace state =
      scheduleRoundSupportedMatches
        @u
        ( applySchedulerRefinement
            schedulerRefinement
            (supportSchedulerView state)
            schedulerConfig
        )
        rewriteContext
        roundView
        candidateSpace
        state
{-# INLINE supportRuntimePolicy #-}

supportScheduleGroup ::
  forall u.
  MatchView u =>
  SatSupportedMatch u ->
  SupportScheduleGroup u
supportScheduleGroup supportedMatch =
  SupportedGroup
    ( matchRuleKey @u
        (supportedMatchInner @u supportedMatch)
    )
    (supportedMatchBasis @u supportedMatch)

supportMatchOrderKey ::
  forall u.
  MatchView u =>
  SatSupportedMatch u ->
  (SupportScheduleGroup u, (SatRuleKey u, SatClassId u, Substitution))
supportMatchOrderKey supportedMatch =
  ( supportScheduleGroup @u supportedMatch,
    matchKey @u (supportedMatchInner @u supportedMatch)
  )

compareSupportMatches ::
  forall u.
  ( MatchView u,
    Ord (SatRuleKey u),
    Ord (SupportBasis (SatContext u)),
    Ord (SatClassId u)
  ) =>
  SatSupportedMatch u ->
  SatSupportedMatch u ->
  Ordering
compareSupportMatches =
  comparing (supportMatchOrderKey @u)
