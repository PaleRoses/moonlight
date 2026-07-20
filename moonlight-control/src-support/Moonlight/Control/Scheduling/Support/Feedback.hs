-- | Evidence policies observing support runtimes through the schedule
-- trace: the bridge from "Moonlight.Control.Scheduling.Support" views into
-- the engine's evidence feedback.
module Moonlight.Control.Scheduling.Support.Feedback
  ( supportEvidencePolicy,
    supportEvidencePolicyWithMode,
    ruleSupportEvidencePolicy,
    ruleSupportEvidencePolicyWithMode,
  )
where

import Moonlight.Control.Engine.Evidence
  ( EvidencePolicy (..),
    PriorityUpdateMode (..),
  )
import Moonlight.Control.Engine.Report
  ( Observation (..),
  )
import Moonlight.Control.Schedule
  ( ScheduleGroup,
  )
import Moonlight.Control.Scheduling.Support
  ( scheduleTraceSupportView,
    supportRuntimeRulePriorityObservation,
    supportRuntimeSupportPriorityObservation,
  )

supportEvidencePolicy ::
  (Ord rule, Ord support) =>
  EvidencePolicy
    (Observation (ScheduleGroup rule support) traceEntry evidence)
    (ScheduleGroup rule support)
supportEvidencePolicy =
  supportEvidencePolicyWithMode AccumulateDynamicPriority

supportEvidencePolicyWithMode ::
  (Ord rule, Ord support) =>
  PriorityUpdateMode ->
  EvidencePolicy
    (Observation (ScheduleGroup rule support) traceEntry evidence)
    (ScheduleGroup rule support)
supportEvidencePolicyWithMode updateMode =
  EvidencePolicy
    { epObserve =
        supportRuntimeSupportPriorityObservation scheduleTraceSupportView
          . obScheduleTrace,
      epUpdateMode = updateMode,
      epNeedsScheduleTrace = True
    }

ruleSupportEvidencePolicy ::
  (Ord rule, Ord support, Ord key) =>
  (rule -> key) ->
  EvidencePolicy
    (Observation (ScheduleGroup rule support) traceEntry evidence)
    key
ruleSupportEvidencePolicy =
  ruleSupportEvidencePolicyWithMode AccumulateDynamicPriority

ruleSupportEvidencePolicyWithMode ::
  (Ord rule, Ord support, Ord key) =>
  PriorityUpdateMode ->
  (rule -> key) ->
  EvidencePolicy
    (Observation (ScheduleGroup rule support) traceEntry evidence)
    key
ruleSupportEvidencePolicyWithMode updateMode ruleKeyOf =
  EvidencePolicy
    { epObserve =
        supportRuntimeRulePriorityObservation scheduleTraceSupportView ruleKeyOf
          . obScheduleTrace,
      epUpdateMode = updateMode,
      epNeedsScheduleTrace = True
    }
