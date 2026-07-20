{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Runtime.Report.Internal
  ( SaturationReportOf (..),
    SaturationReportMetrics (..),
    SaturationReportEvidence (..),
    SaturationReport,
    ProofSaturationReport,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Moonlight.Saturation.Context.Runtime.State
  ( RuntimeCore,
  )
import Moonlight.Control.Diagnostics.Trace
  ( TraceLog,
  )
import Moonlight.Control.Gate
  ( GuideRoundTrace,
  )
import Moonlight.Saturation.Core
  ( SaturationTermination,
  )
import Moonlight.Control.Schedule.Round
  ( SchedulerState,
  )
import Moonlight.Saturation.Substrate

type SaturationReportMetrics :: Type
data SaturationReportMetrics = SaturationReportMetrics
  { srmIterationCount :: !Int,
    srmMatchesApplied :: !Int,
    srmFactRoundCount :: !Int,
    srmGuideRoundCount :: !Int
  }
  deriving stock (Eq, Ord, Show, Read)

type SaturationReportEvidence :: Type -> Type -> Type
data SaturationReportEvidence u schedulerGroup = SaturationReportEvidence
  { sreFactRoundsByContext :: !(Map (SatContext u) (Seq (SatFactRound u))),
    sreGuideTrace :: !(Seq GuideRoundTrace),
    sreTrace :: !(TraceLog (SatRuleKey u) schedulerGroup)
  }

type SaturationReportOf :: Type -> Type -> Type -> Type -> Type
data SaturationReportOf u carrier schedulerGroup tracePayload = SaturationReportOf
  { srResult :: !SaturationTermination,
    srFinalCore :: RuntimeCore u schedulerGroup,
    srMetrics :: !SaturationReportMetrics,
    srEvidence :: !(SaturationReportEvidence u schedulerGroup),
    srScheduler :: !(SchedulerState schedulerGroup),
    srCarrier :: !carrier,
    srTracePayload :: !tracePayload
  }

type SaturationReport :: Type -> Type
type SaturationReport u =
  SaturationReportOf u (SatGraph u) (SatRuleKey u) ()

type ProofSaturationReport :: Type -> Type -> Type
type ProofSaturationReport u proofGraph =
  SaturationReportOf u proofGraph (SatRuleKey u) ()
