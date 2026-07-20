{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Saturation.Context.Program.Spec
  ( RewriteContextSnapshot (..),
    staticRewriteContextSnapshot,
    PlanSpec,
    SaturationGuidanceView (..),
    planSpec,
    defaultPlanSpec,
    canonicalPlanSpec,
    planSpecSaturationBudget,
    planSpecMatchingStrategy,
    planSpecSchedulerConfig,
    planSpecRewriteContextSnapshot,
    planSpecRewriteContext,
    planSpecGuidance,
    withSaturationBudget,
    withMatchingStrategy,
    withSchedulerConfig,
    withRewriteContext,
    withGuidance,
    validatePlanSpec,
    deterministicSchedulerConfig,
    backoffSchedulerConfig,
    traceAllSchedulerConfig,
    traceLastSchedulerConfig,
    CriticalityRank (..),
    nonCriticalPriorityRank,
    criticalPriorityRank,
    priorityRankFromBool,
    PriorityEvidence (..),
    PriorityProfile,
    emptyPriorityProfile,
    withPriorityProfile,
    clearPriorityProfile,
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty qualified as NonEmpty
import Numeric.Natural (Natural)
import Moonlight.Saturation.Context.Error
  ( PlanCompileError (..),
    PlanSpecViolation (..),
    validateSaturationBudget
  )
import Moonlight.Saturation.Context.Program.View
  ( SaturationRoundView,
  )
import Moonlight.Saturation.Core
  ( SaturationBudget,
  )
import Moonlight.Control.Gate
  ( Gate,
    GuideRoundTrace,
    noGate,
    validateGateScheduler,
  )
import Moonlight.Control.Schedule
  ( BackoffConfig,
    ScheduleOrder (..),
    SchedulerConfig (..),
    TracePolicy (..),
    canonicalBackoffConfig,
    canonicalSchedulerConfig,
    canonicalTracePolicy,
    clearPriorityProfile,
    defaultSchedulerConfig,
    traceLastEntries,
    withPriorityProfile,
  )
import Moonlight.Control.Weight
  ( CriticalityRank (..),
    PriorityEvidence (..),
    PriorityProfile,
    criticalPriorityRank,
    emptyPriorityProfile,
    nonCriticalPriorityRank,
    priorityRankFromBool,
  )
import Moonlight.Saturation.Substrate

type RewriteContextSnapshot :: Type -> Type
data RewriteContextSnapshot u = RewriteContextSnapshot
  { rcsCapabilityGeneration :: !Natural,
    rcsRewriteContext :: !(SatRewriteContext u)
  }

staticRewriteContextSnapshot ::
  SatRewriteContext u ->
  RewriteContextSnapshot u
staticRewriteContextSnapshot rewriteContext =
  RewriteContextSnapshot
    { rcsCapabilityGeneration = 0,
      rcsRewriteContext = rewriteContext
    }
{-# INLINE staticRewriteContextSnapshot #-}

type PlanSpec :: Type -> Type -> Type -> Type
data PlanSpec u carrier schedulerGroup = PlanSpec
  { psSaturationBudget :: !SaturationBudget,
    psMatchingStrategy :: !(SatMatchStrategy u),
    psSchedulerConfig :: !(SchedulerConfig schedulerGroup),
    psRewriteContextSnapshot :: !(carrier -> RewriteContextSnapshot u),
    psGuidance ::
      !( Gate
           (SaturationGuidanceView u)
           ()
           (SatSupportedMatch u)
           GuideRoundTrace
           schedulerGroup
       )
  }

type SaturationGuidanceView :: Type -> Type
data SaturationGuidanceView u = SaturationGuidanceView
  { sgvRoundView :: !(SaturationRoundView u),
    sgvCandidates :: ![SatSupportedMatch u]
  }

planSpec ::
  SaturationBudget ->
  SatMatchStrategy u ->
  SatRewriteContext u ->
  PlanSpec u carrier schedulerGroup
planSpec budget matchingStrategy rewriteContext =
  canonicalPlanSpec
    PlanSpec
      { psSaturationBudget = budget,
        psMatchingStrategy = matchingStrategy,
        psSchedulerConfig = defaultSchedulerConfig,
        psRewriteContextSnapshot =
          const (staticRewriteContextSnapshot rewriteContext),
        psGuidance = noGate
      }
{-# INLINE planSpec #-}

defaultPlanSpec ::
  forall u schedulerGroup.
  RewriteSystem u =>
  SaturationBudget ->
  SatMatchStrategy u ->
  PlanSpec u (SatGraph u) schedulerGroup
defaultPlanSpec budget matchingStrategy =
  planSpec budget matchingStrategy (defaultRewriteContext @u)

canonicalPlanSpec ::
  PlanSpec u carrier schedulerGroup ->
  PlanSpec u carrier schedulerGroup
canonicalPlanSpec spec =
  spec
    { psSchedulerConfig =
        canonicalSchedulerConfig (psSchedulerConfig spec)
    }
{-# INLINE canonicalPlanSpec #-}

planSpecSaturationBudget :: PlanSpec u carrier schedulerGroup -> SaturationBudget
planSpecSaturationBudget =
  psSaturationBudget
{-# INLINE planSpecSaturationBudget #-}

planSpecMatchingStrategy :: PlanSpec u carrier schedulerGroup -> SatMatchStrategy u
planSpecMatchingStrategy =
  psMatchingStrategy
{-# INLINE planSpecMatchingStrategy #-}

planSpecSchedulerConfig ::
  PlanSpec u carrier schedulerGroup ->
  SchedulerConfig schedulerGroup
planSpecSchedulerConfig =
  canonicalSchedulerConfig . psSchedulerConfig
{-# INLINE planSpecSchedulerConfig #-}

planSpecRewriteContextSnapshot ::
  PlanSpec u carrier schedulerGroup ->
  carrier ->
  RewriteContextSnapshot u
planSpecRewriteContextSnapshot =
  psRewriteContextSnapshot
{-# INLINE planSpecRewriteContextSnapshot #-}

planSpecRewriteContext ::
  PlanSpec u carrier schedulerGroup ->
  carrier ->
  SatRewriteContext u
planSpecRewriteContext spec carrier =
  rcsRewriteContext (planSpecRewriteContextSnapshot spec carrier)
{-# INLINE planSpecRewriteContext #-}

planSpecGuidance ::
  PlanSpec u carrier schedulerGroup ->
  Gate
    (SaturationGuidanceView u)
    ()
    (SatSupportedMatch u)
    GuideRoundTrace
    schedulerGroup
planSpecGuidance =
  psGuidance
{-# INLINE planSpecGuidance #-}

withSaturationBudget ::
  SaturationBudget ->
  PlanSpec u carrier schedulerGroup ->
  PlanSpec u carrier schedulerGroup
withSaturationBudget budget spec =
  spec {psSaturationBudget = budget}
{-# INLINE withSaturationBudget #-}

withMatchingStrategy ::
  SatMatchStrategy u ->
  PlanSpec u carrier schedulerGroup ->
  PlanSpec u carrier schedulerGroup
withMatchingStrategy matchingStrategy spec =
  spec {psMatchingStrategy = matchingStrategy}
{-# INLINE withMatchingStrategy #-}

withSchedulerConfig ::
  SchedulerConfig schedulerGroup ->
  PlanSpec u carrier schedulerGroup ->
  PlanSpec u carrier schedulerGroup
withSchedulerConfig schedulerConfig spec =
  spec {psSchedulerConfig = schedulerConfig}
{-# INLINE withSchedulerConfig #-}

withRewriteContext ::
  (carrier' -> RewriteContextSnapshot u) ->
  PlanSpec u carrier schedulerGroup ->
  PlanSpec u carrier' schedulerGroup
withRewriteContext rewriteContextSnapshot spec =
  PlanSpec
    { psSaturationBudget = psSaturationBudget spec,
      psMatchingStrategy = psMatchingStrategy spec,
      psSchedulerConfig = psSchedulerConfig spec,
      psRewriteContextSnapshot = rewriteContextSnapshot,
      psGuidance = psGuidance spec
    }
{-# INLINE withRewriteContext #-}

withGuidance ::
  Gate
    (SaturationGuidanceView u)
    ()
    (SatSupportedMatch u)
    GuideRoundTrace
    schedulerGroup ->
  PlanSpec u carrier schedulerGroup ->
  PlanSpec u carrier schedulerGroup
withGuidance guidanceValue spec =
  spec {psGuidance = guidanceValue}
{-# INLINE withGuidance #-}

validatePlanSpec ::
  PlanSpec u carrier schedulerGroup ->
  Either (PlanCompileError schedulerGroup) ()
validatePlanSpec spec =
  case NonEmpty.nonEmpty (planSpecViolations spec) of
    Nothing ->
      Right ()
    Just violations ->
      Left (PlanCompileError violations)
{-# INLINE validatePlanSpec #-}

planSpecViolations ::
  PlanSpec u carrier schedulerGroup ->
  [PlanSpecViolation schedulerGroup]
planSpecViolations spec =
  violationFrom
    PlanSaturationBudgetViolation
    (validateSaturationBudget (psSaturationBudget spec))
    <> violationFrom
      PlanGuidanceCompatibilityViolation
      (validateGateScheduler (psGuidance spec) schedulerConfig)
  where
    schedulerConfig =
      psSchedulerConfig spec

violationFrom ::
  (e -> violation) ->
  Either e () ->
  [violation]
violationFrom toViolation =
  either (pure . toViolation) (const [])

deterministicSchedulerConfig :: SchedulerConfig key
deterministicSchedulerConfig =
  canonicalSchedulerConfig
    ( defaultSchedulerConfig
        { scOrder = ByRuleIdThenSubstitution,
          scTracePolicy = NoTrace
        }
    )
{-# INLINE deterministicSchedulerConfig #-}

backoffSchedulerConfig :: BackoffConfig -> SchedulerConfig key
backoffSchedulerConfig backoffConfig =
  canonicalSchedulerConfig
    ( defaultSchedulerConfig
        { scOrder = BackoffByGroup (canonicalBackoffConfig backoffConfig),
          scTracePolicy = NoTrace
        }
    )
{-# INLINE backoffSchedulerConfig #-}

traceAllSchedulerConfig :: SchedulerConfig key -> SchedulerConfig key
traceAllSchedulerConfig schedulerConfig =
  canonicalSchedulerConfig
    ( schedulerConfig
        { scTracePolicy = TraceAll
        }
    )
{-# INLINE traceAllSchedulerConfig #-}

traceLastSchedulerConfig :: Int -> SchedulerConfig key -> SchedulerConfig key
traceLastSchedulerConfig retainedCount schedulerConfig =
  canonicalSchedulerConfig
    ( schedulerConfig
        { scTracePolicy =
            canonicalTracePolicy (traceLastEntries retainedCount)
        }
    )
{-# INLINE traceLastSchedulerConfig #-}
