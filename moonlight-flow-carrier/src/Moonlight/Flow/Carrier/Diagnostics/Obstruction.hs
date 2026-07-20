{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Carrier.Diagnostics.Obstruction
  ( CarrierEvidenceView (..),
    restrictionFailuresNow,
    propagationFailuresNow,
    cohomologicalFailuresNow,
  )
where

import Moonlight.Flow.Carrier.Core.Obstruction.Types
  ( CohomologicalFailure,
    PropagationFailure,
    RestrictionFailure,
  )
import Moonlight.Flow.Carrier.Store.Journal.Trace
  ( carrierTraceForContext,
  )
import Moonlight.Flow.Carrier.Core.Summary
  ( CarrierStoreSummaryEntry,
  )
import Moonlight.Flow.Carrier.Store.Core.State
  ( CarrierStore,
    carrierStoreSummaryEntryFromTraceEntry,
  )

data CarrierEvidenceView ctx carrier prop boundary evidence = CarrierEvidenceView
  { cevRestrictionFailures ::
      CarrierStoreSummaryEntry ctx carrier prop boundary evidence ->
      [RestrictionFailure ctx carrier prop boundary],
    cevPropagationFailures ::
      CarrierStoreSummaryEntry ctx carrier prop boundary evidence ->
      [PropagationFailure ctx carrier prop],
    cevCohomologicalFailures ::
      CarrierStoreSummaryEntry ctx carrier prop boundary evidence ->
      [CohomologicalFailure ctx carrier prop boundary]
  }

restrictionFailuresNow ::
  Ord ctx =>
  ctx ->
  CarrierEvidenceView ctx carrier prop boundary evidence ->
  CarrierStore ctx carrier prop boundary evidence ->
  [RestrictionFailure ctx carrier prop boundary]
restrictionFailuresNow contextValue evidenceView indexState =
  fmap carrierStoreSummaryEntryFromTraceEntry (carrierTraceForContext contextValue indexState)
    >>= cevRestrictionFailures evidenceView

propagationFailuresNow ::
  Ord ctx =>
  ctx ->
  CarrierEvidenceView ctx carrier prop boundary evidence ->
  CarrierStore ctx carrier prop boundary evidence ->
  [PropagationFailure ctx carrier prop]
propagationFailuresNow contextValue evidenceView indexState =
  fmap carrierStoreSummaryEntryFromTraceEntry (carrierTraceForContext contextValue indexState)
    >>= cevPropagationFailures evidenceView

cohomologicalFailuresNow ::
  Ord ctx =>
  ctx ->
  CarrierEvidenceView ctx carrier prop boundary evidence ->
  CarrierStore ctx carrier prop boundary evidence ->
  [CohomologicalFailure ctx carrier prop boundary]
cohomologicalFailuresNow contextValue evidenceView indexState =
  fmap carrierStoreSummaryEntryFromTraceEntry (carrierTraceForContext contextValue indexState)
    >>= cevCohomologicalFailures evidenceView
