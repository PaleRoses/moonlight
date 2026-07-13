{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Support.Core
  ( SaturationRunMetrics (..),
    SupportSaturationReportFor,
    SupportSaturationMetrics,
    SupportScheduleGroup,
    SupportState,
    supportSchedulerView,
    supportReportScheduleTrace,
    supportSaturationMetricsFromReport,
  )
where

import Data.Kind (Type)
import Moonlight.Saturation.Context.Runtime.Report
  ( ReportSummary (..),
    SaturationReportOf,
    reportDiagnosticTrace,
    reportScheduleTrace,
    reportSummary,
    srCarrier,
  )
import Moonlight.Saturation.Context.Runtime.State
  ( RuntimeState,
    rcIterationCount,
    rsCarrier,
    rsCore,
    rsScheduler,
  )
import Moonlight.Control.Schedule
  ( ScheduleGroup (..),
    sgRuleKey,
  )
import Moonlight.Control.Schedule.Round
  ( ScheduleTrace,
    schedulerTrace,
  )
import Moonlight.Saturation.Substrate
import Moonlight.Sheaf.Twist.Schedule qualified as SheafTwist
import Moonlight.Pale.Diagnostic.Section.Saturation
  ( SaturationTrace,
  )

type SaturationRunMetrics :: Type -> Type
data SaturationRunMetrics key = SaturationRunMetrics
  { srmInitialNodeCount :: !Int,
    srmFinalNodeCount :: !Int,
    srmInitialClassCount :: !Int,
    srmFinalClassCount :: !Int,
    srmIterations :: !Int,
    srmMatchesApplied :: !Int,
    srmTrace :: !(SaturationTrace key)
  }
  deriving stock (Eq, Show)

type SupportScheduleGroup :: Type -> Type
type SupportScheduleGroup u =
  ScheduleGroup
    (SatRuleKey u)
    (SupportBasis (SatContext u))

type SupportSaturationReportFor :: Type -> Type -> Type
type SupportSaturationReportFor u proofGraph =
  SaturationReportOf
    u
    proofGraph
    (SupportScheduleGroup u)
    (SatChangeSummary u)

type SupportSaturationMetrics :: Type -> Type
type SupportSaturationMetrics u =
  SaturationRunMetrics (SatRuleKey u)

type SupportState :: Type -> Type -> Type
type SupportState u proofGraph =
  RuntimeState u proofGraph (SupportScheduleGroup u)

supportSchedulerView ::
  forall u proofGraph.
  SupportState u proofGraph ->
  SheafTwist.SupportSchedulerView
    proofGraph
    (ScheduleTrace (SupportScheduleGroup u))
supportSchedulerView state =
  SheafTwist.SupportSchedulerView
    { SheafTwist.ssvIterationCount =
        rcIterationCount (rsCore state),
      SheafTwist.ssvTrace = schedulerTrace (rsScheduler state),
      SheafTwist.ssvHostState = rsCarrier state
    }

supportReportScheduleTrace ::
  SupportSaturationReportFor u proofGraph ->
  [ScheduleTrace (SupportScheduleGroup u)]
supportReportScheduleTrace =
  reportScheduleTrace
{-# INLINE supportReportScheduleTrace #-}

supportSaturationMetricsFromReport ::
  forall u proofGraph.
  SaturationGraph u =>
  (proofGraph -> SatGraph u) ->
  proofGraph ->
  SupportSaturationReportFor u proofGraph ->
  SupportSaturationMetrics u
supportSaturationMetricsFromReport projectProofGraphContext initialProofGraph report =
  let initialGraph = projectProofGraphContext initialProofGraph
      finalGraph = projectProofGraphContext (srCarrier report)
      summary = reportSummary report
   in SaturationRunMetrics
        { srmInitialNodeCount = graphNodeCount @u initialGraph,
          srmFinalNodeCount = graphNodeCount @u finalGraph,
          srmInitialClassCount = graphClassCount @u initialGraph,
          srmFinalClassCount = graphClassCount @u finalGraph,
          srmIterations = rsrIterations summary,
          srmMatchesApplied = rsrMatchesApplied summary,
          srmTrace = reportDiagnosticTrace sgRuleKey report
        }
