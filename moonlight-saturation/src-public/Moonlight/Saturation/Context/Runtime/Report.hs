{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Saturation.Context.Runtime.Report
  ( SaturationReportOf,
    SaturationReportMetrics (..),
    SaturationReportEvidence (..),
    SaturationReport,
    ProofSaturationReport,
    ReportSummary (..),
    srResult,
    srFinalCore,
    srMetrics,
    srEvidence,
    srCarrier,
    srTracePayload,
    reportSummary,
    reportIterationCount,
    reportMatchesApplied,
    reportFactRoundCount,
    reportFactRounds,
    reportContextFacts,
    reportGuideRoundCount,
    reportGuideTrace,
    reportScheduleTrace,
    reportDiagnosticTrace,
    mkReport,
    mkReportOf,
    noOpSaturationReport,
    saturationReportBaseGraph,
    baseGraphStateEquals,
    pegBaseGraph,
    graphStateEquals,
    plainRuntimeStateFromReport,
    proofRuntimeStateFromReport,
  )
where

import Data.Foldable qualified as Foldable
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Moonlight.Saturation.Context.Runtime.Report.Internal
  ( ProofSaturationReport,
    SaturationReportEvidence (..),
    SaturationReportMetrics (..),
    SaturationReport,
    SaturationReportOf (..),
  )
import Moonlight.Saturation.Context.Runtime.State
  ( PlainRuntimeState,
    ProofRuntimeState,
    RuntimeCore (..),
    RuntimeReportWindow (..),
    RuntimeState (..),
    initialRuntimeCore,
    runtimeCoreFactViewKeyAt,
  )
import Moonlight.Saturation.Context.Runtime.Policy.Internal
  ( CarrierAccess (..),
  )
import Moonlight.Control.Diagnostics.Trace
  ( RoundTrace (..),
    emptyTraceLog,
    traceLogDropBeforeIteration,
    traceLogRounds,
  )
import Moonlight.Control.Diagnostics.Pale
  ( traceLogToPale,
  )
import Moonlight.Control.Gate
  ( GuideRoundTrace,
  )
import Moonlight.Pale.Diagnostic.Section.Saturation
  ( SaturationTrace,
  )
import Moonlight.Saturation.Core
  ( SaturationTermination (..),
  )
import Moonlight.Control.Schedule.Round (ScheduleTrace, emptySchedulerState)
import Moonlight.Saturation.Substrate

type ReportSummary :: Type
data ReportSummary = ReportSummary
  { rsrResult :: !SaturationTermination,
    rsrIterations :: !Int,
    rsrMatchesApplied :: !Int,
    rsrContextRevision :: !Int,
    rsrContextCount :: !Int,
    rsrFactRoundCount :: !Int,
    rsrGuideRoundCount :: !Int
  }
  deriving stock (Eq, Ord, Show, Read)

reportSummary :: SaturationReportOf u carrier schedulerGroup tracePayload -> ReportSummary
reportSummary report =
  let metrics =
        srMetrics report
      finalCore =
        srFinalCore report
   in ReportSummary
        { rsrResult = srResult report,
          rsrIterations = srmIterationCount metrics,
          rsrMatchesApplied = srmMatchesApplied metrics,
          rsrContextRevision = rcContextRevision finalCore,
          rsrContextCount = Map.size (rcContextFacts finalCore),
          rsrFactRoundCount = srmFactRoundCount metrics,
          rsrGuideRoundCount = srmGuideRoundCount metrics
        }

reportIterationCount :: SaturationReportOf u carrier schedulerGroup tracePayload -> Int
reportIterationCount =
  srmIterationCount . srMetrics
{-# INLINE reportIterationCount #-}

reportMatchesApplied :: SaturationReportOf u carrier schedulerGroup tracePayload -> Int
reportMatchesApplied =
  srmMatchesApplied . srMetrics
{-# INLINE reportMatchesApplied #-}

reportFactRoundCount :: SaturationReportOf u carrier schedulerGroup tracePayload -> Int
reportFactRoundCount =
  srmFactRoundCount . srMetrics
{-# INLINE reportFactRoundCount #-}

reportFactRounds :: SaturationReportOf u carrier schedulerGroup tracePayload -> [SatFactRound u]
reportFactRounds =
  foldMap Foldable.toList . sreFactRoundsByContext . srEvidence
{-# INLINE reportFactRounds #-}

reportContextFacts :: SaturationReportOf u carrier schedulerGroup tracePayload -> Map.Map (SatContext u) (SatFactStore u)
reportContextFacts =
  rcContextFacts . srFinalCore
{-# INLINE reportContextFacts #-}

reportGuideRoundCount :: SaturationReportOf u carrier schedulerGroup tracePayload -> Int
reportGuideRoundCount =
  srmGuideRoundCount . srMetrics
{-# INLINE reportGuideRoundCount #-}

reportGuideTrace :: SaturationReportOf u carrier schedulerGroup tracePayload -> [GuideRoundTrace]
reportGuideTrace =
  Foldable.toList . sreGuideTrace . srEvidence
{-# INLINE reportGuideTrace #-}

reportScheduleTrace ::
  SaturationReportOf u carrier schedulerGroup tracePayload ->
  [ScheduleTrace schedulerGroup]
reportScheduleTrace =
  foldMap roundTraceSchedule . traceLogRounds . sreTrace . srEvidence
{-# INLINE reportScheduleTrace #-}

reportDiagnosticTrace ::
  (schedulerGroup -> SatRuleKey u) ->
  SaturationReportOf u carrier schedulerGroup tracePayload ->
  SaturationTrace (SatRuleKey u)
reportDiagnosticTrace projectGroup =
  traceLogToPale projectGroup . sreTrace . srEvidence
{-# INLINE reportDiagnosticTrace #-}

saturationReportMetricsFromWindow ::
  RuntimeReportWindow u carrier schedulerGroup ->
  SaturationReportMetrics
saturationReportMetricsFromWindow window =
  let initialCore =
        rsCore (rrwInitialState window)
      finalCore =
        rsCore (rrwFinalState window)
   in SaturationReportMetrics
        { srmIterationCount =
            rcIterationCount finalCore - rcIterationCount initialCore,
          srmMatchesApplied =
            rcTotalMatches finalCore - rcTotalMatches initialCore,
          srmFactRoundCount =
            rcFactRoundCount finalCore - rcFactRoundCount initialCore,
          srmGuideRoundCount =
            rcGuideRoundCount finalCore - rcGuideRoundCount initialCore
        }
{-# INLINE saturationReportMetricsFromWindow #-}

saturationReportEvidenceFromWindow ::
  Ord (SatContext u) =>
  RuntimeReportWindow u carrier schedulerGroup ->
  SaturationReportEvidence u schedulerGroup
saturationReportEvidenceFromWindow window =
  let initialCore =
        rsCore (rrwInitialState window)
      finalCore =
        rsCore (rrwFinalState window)
   in SaturationReportEvidence
        { sreFactRoundsByContext =
            phaseFactRoundsByContext
              (rcFactRoundsByContext initialCore)
              (rcFactRoundsByContext finalCore),
          sreGuideTrace =
            Seq.drop
              (Seq.length (rcGuideTrace initialCore))
              (rcGuideTrace finalCore),
          sreTrace =
            traceLogDropBeforeIteration
              (rcIterationCount initialCore)
              (rcTrace finalCore)
        }
{-# INLINE saturationReportEvidenceFromWindow #-}

phaseFactRoundsByContext ::
  Ord context =>
  Map.Map context (Seq.Seq round) ->
  Map.Map context (Seq.Seq round) ->
  Map.Map context (Seq.Seq round)
phaseFactRoundsByContext initialRoundsByContext =
  Map.mapMaybeWithKey localRoundsAt
  where
    localRoundsAt contextValue finalRounds =
      let localRounds =
            Seq.drop
              (maybe 0 Seq.length (Map.lookup contextValue initialRoundsByContext))
              finalRounds
       in if Seq.null localRounds
            then Nothing
            else Just localRounds
{-# INLINE phaseFactRoundsByContext #-}

emptySaturationReportEvidence ::
  SaturationReportEvidence u schedulerGroup
emptySaturationReportEvidence =
  SaturationReportEvidence
    { sreFactRoundsByContext = Map.empty,
      sreGuideTrace = Seq.empty,
      sreTrace = emptyTraceLog
    }
{-# INLINE emptySaturationReportEvidence #-}

mkReport ::
  (FactSystem u, Ord (SatContext u)) =>
  CarrierAccess u carrier ->
  SaturationTermination ->
  RuntimeReportWindow u carrier schedulerGroup ->
  Either (SatObstruction u) (SaturationReportOf u carrier schedulerGroup ())
mkReport carrierAccess result window =
  mkReportOf carrierAccess result window ()
{-# INLINE mkReport #-}

mkReportOf ::
  forall u carrier schedulerGroup tracePayload.
  (FactSystem u, Ord (SatContext u)) =>
  CarrierAccess u carrier ->
  SaturationTermination ->
  RuntimeReportWindow u carrier schedulerGroup ->
  tracePayload ->
  Either (SatObstruction u) (SaturationReportOf u carrier schedulerGroup tracePayload)
mkReportOf carrierAccess result window tracePayload = do
  let finalState =
        rrwFinalState window
      finalCore =
        rsCore finalState
      finalGraph =
        caGraph carrierAccess (rsCarrier finalState)
  canonicalCore <-
    launderRuntimeCoreFactExports @u finalGraph finalCore
  pure
    SaturationReportOf
      { srResult = result,
        srFinalCore = canonicalCore,
        srMetrics = saturationReportMetricsFromWindow window,
        srEvidence = saturationReportEvidenceFromWindow window,
        srScheduler = rsScheduler finalState,
        srCarrier = rsCarrier finalState,
        srTracePayload = tracePayload
      }
{-# INLINE mkReportOf #-}

launderRuntimeCoreFactExports ::
  forall u schedulerGroup.
  (FactSystem u, Ord (SatContext u)) =>
  SatGraph u ->
  RuntimeCore u schedulerGroup ->
  Either (SatObstruction u) (RuntimeCore u schedulerGroup)
launderRuntimeCoreFactExports graph core
  | Set.null staleContexts =
      Right core
  | otherwise = do
      canonicalInputs <- launderFactStores (rcContextFactInputs core)
      canonicalFacts <- launderFactStores (rcContextFacts core)
      canonicalDerivations <-
        Map.traverseWithKey
          (\contextValue -> canonicalizeFactIndexAtContext @u contextValue graph)
          (Map.restrictKeys (rcContextFactDerivations core) staleContexts)
      pure
        core
          { rcContextFactInputs = canonicalInputs,
            rcContextFacts = canonicalFacts,
            rcContextFactDerivations =
              Map.union
                canonicalDerivations
                (Map.withoutKeys (rcContextFactDerivations core) staleContexts)
          }
  where
    contextsWithFactArtifacts =
      Set.unions
        [ Map.keysSet (rcContextFactInputs core),
          Map.keysSet (rcContextFacts core),
          Map.keysSet (rcContextFactDerivations core)
        ]
    staleContexts =
      Set.filter factViewIsStale contextsWithFactArtifacts
    factViewIsStale contextValue =
      case
        ( Map.lookup contextValue (rcFactViewKeys core),
          Map.lookup contextValue (rcCurrentFactRuleIdsByContext core)
        )
      of
        (Just storedKey, Just currentFactRuleIds) ->
          storedKey
            /= runtimeCoreFactViewKeyAt @u
              contextValue
              currentFactRuleIds
              (rcCurrentFactCapabilityGeneration core)
              core
        _ ->
          True
    launderFactStores factStores =
      fmap
        (`Map.union` Map.withoutKeys factStores staleContexts)
        ( Map.traverseWithKey
            (\contextValue -> canonicalizeFactStoreAtContext @u contextValue graph)
            (Map.restrictKeys factStores staleContexts)
        )
{-# INLINE launderRuntimeCoreFactExports #-}

noOpSaturationReport ::
  forall u.
  (BaseGraphEmbedding u (SatGraph u), FactSystem u, Monoid (SatChangeSummary u)) =>
  SatBaseGraph u ->
  SaturationReport u
noOpSaturationReport baseGraph =
  let graph =
        embedBaseGraph @u @(SatGraph u) baseGraph
      core =
        initialRuntimeCore @u
   in SaturationReportOf
        { srResult = ReachedFixedPoint,
          srFinalCore = core,
          srMetrics =
            SaturationReportMetrics
              { srmIterationCount = 0,
                srmMatchesApplied = 0,
                srmFactRoundCount = 0,
                srmGuideRoundCount = 0
              },
          srEvidence = emptySaturationReportEvidence,
          srScheduler = emptySchedulerState,
          srCarrier = graph,
          srTracePayload = ()
        }

saturationReportBaseGraph ::
  forall u.
  SaturationGraph u =>
  SaturationReport u ->
  SatBaseGraph u
saturationReportBaseGraph =
  graphBase @u . srCarrier

baseGraphStateEquals ::
  forall u.
  SaturationGraph u =>
  SatBaseGraph u ->
  SatBaseGraph u ->
  Bool
baseGraphStateEquals =
  baseGraphEquals @u

pegBaseGraph ::
  forall u p.
  ProofCarrier u p =>
  SatProofGraph u p ->
  SatBaseGraph u
pegBaseGraph =
  graphBase @u . proofGraphContext @u @p

graphStateEquals ::
  forall u.
  SaturationGraph u =>
  SatGraph u ->
  SatGraph u ->
  Bool
graphStateEquals =
  graphConvergenceStateEquals @u

plainRuntimeStateFromReport ::
  SatMatchState u ->
  SaturationReport u ->
  PlainRuntimeState u
plainRuntimeStateFromReport matchState report =
  RuntimeState
    { rsCore = srFinalCore report,
      rsCarrier = srCarrier report,
      rsMatchState = matchState,
      rsScheduler = srScheduler report
    }

proofRuntimeStateFromReport ::
  SatMatchState u ->
  ProofSaturationReport u proofGraph ->
  ProofRuntimeState u proofGraph
proofRuntimeStateFromReport matchState report =
  RuntimeState
    { rsCore = srFinalCore report,
      rsCarrier = srCarrier report,
      rsMatchState = matchState,
      rsScheduler = srScheduler report
    }
