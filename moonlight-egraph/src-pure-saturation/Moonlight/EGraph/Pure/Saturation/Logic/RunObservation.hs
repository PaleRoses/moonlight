{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Run-trace observations: first-class verbs over what a saturation run
-- /did/, as opposed to what the final stable graph /is/ (the latter being the
-- province of "Moonlight.EGraph.Pure.Saturation.Logic.Observation").
--
-- These are total functions of the 'SaturationReport' (and the initial
-- carrier) — there is no stable-phase precondition, so they never fail.
module Moonlight.EGraph.Pure.Saturation.Logic.RunObservation
  ( RunObservation (..),
    SomeRunObservation (..),
    SomeRunObservationResult (..),
    runRunObservation,
    runSomeRunObservation,
    runSomeRunObservations,
  )
where

import Data.IntSet (IntSet)
import Data.Kind (Type)
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Control.Schedule.Round
  ( ScheduleTrace (..),
    scheduleTraceBannedUntil,
    scheduleTraceSkippedByScheduler,
  )
import Moonlight.Core (RewriteRuleId)
import Moonlight.EGraph.Pure.Context
  ( ContextMutationTrace (..),
  )
import Moonlight.EGraph.Pure.Saturation.Substrate
  ( EGraphU,
    eGraphSaturationChangeTrace,
  )
import Moonlight.EGraph.Saturation.Context.State
  ( SaturatingContextEGraph,
    sceContextGraph,
  )
import Moonlight.Saturation.Context.Runtime.Report
  ( SaturationReport,
    reportScheduleTrace,
    srCarrier,
    srFinalCore,
  )
import Moonlight.Saturation.Context.Runtime.State (RuntimeCore (..))

-- | An observation of a completed saturation run, indexed by its result type.
type RunObservation :: Type -> Type -> Type
data RunObservation c result where
  -- | Rule keys that fired at least once across the run.
  ObserveFiredRules :: RunObservation c (Set RewriteRuleId)
  -- | Rule keys the scheduler suppressed or held under cooldown.
  ObserveBlockedRules :: RunObservation c (Set RewriteRuleId)
  -- | Class keys touched by the run (the dirty footprint).
  ObserveDirtyKeys :: RunObservation c IntSet
  -- | Contexts touched by the run.
  ObserveDirtyContexts :: RunObservation c (Set c)

type SomeRunObservation :: Type -> Type
data SomeRunObservation c where
  SomeRunObservation :: RunObservation c result -> SomeRunObservation c

type SomeRunObservationResult :: Type -> Type
data SomeRunObservationResult c where
  SomeFiredRulesResult :: !(Set RewriteRuleId) -> SomeRunObservationResult c
  SomeBlockedRulesResult :: !(Set RewriteRuleId) -> SomeRunObservationResult c
  SomeDirtyKeysResult :: !IntSet -> SomeRunObservationResult c
  SomeDirtyContextsResult :: !(Set c) -> SomeRunObservationResult c

data RunObservationSummary owner c f = RunObservationSummary
  { rosFiredRules :: Set RewriteRuleId,
    rosBlockedRules :: Set RewriteRuleId,
    rosChangeTrace :: ContextMutationTrace owner c f
  }

runSomeRunObservations ::
  Ord c =>
  SaturatingContextEGraph owner capability f a c ->
  SaturationReport (EGraphU owner capability f a c) ->
  [SomeRunObservation c] ->
  [SomeRunObservationResult c]
runSomeRunObservations initialCarrier report observations =
  let summary =
        runObservationSummary initialCarrier report
   in fmap (`runSomeRunObservationWith` summary) observations
{-# INLINE runSomeRunObservations #-}

runSomeRunObservation ::
  Ord c =>
  SomeRunObservation c ->
  SaturatingContextEGraph owner capability f a c ->
  SaturationReport (EGraphU owner capability f a c) ->
  SomeRunObservationResult c
runSomeRunObservation (SomeRunObservation observation) initialCarrier report =
  runSomeRunObservationWith
    (SomeRunObservation observation)
    (runObservationSummary initialCarrier report)
{-# INLINE runSomeRunObservation #-}

runSomeRunObservationWith ::
  SomeRunObservation c ->
  RunObservationSummary owner c f ->
  SomeRunObservationResult c
runSomeRunObservationWith (SomeRunObservation observation) summary =
  case observation of
    ObserveFiredRules ->
      SomeFiredRulesResult (runRunObservationWith observation summary)
    ObserveBlockedRules ->
      SomeBlockedRulesResult (runRunObservationWith observation summary)
    ObserveDirtyKeys ->
      SomeDirtyKeysResult (runRunObservationWith observation summary)
    ObserveDirtyContexts ->
      SomeDirtyContextsResult (runRunObservationWith observation summary)
{-# INLINE runSomeRunObservationWith #-}

runRunObservation ::
  Ord c =>
  RunObservation c result ->
  SaturatingContextEGraph owner capability f a c ->
  SaturationReport (EGraphU owner capability f a c) ->
  result
runRunObservation observation initialCarrier report =
  runRunObservationWith observation (runObservationSummary initialCarrier report)
{-# INLINE runRunObservation #-}

runRunObservationWith ::
  RunObservation c result ->
  RunObservationSummary owner c f ->
  result
runRunObservationWith observation summary =
  case observation of
    ObserveFiredRules ->
      rosFiredRules summary
    ObserveBlockedRules ->
      rosBlockedRules summary
    ObserveDirtyKeys ->
      cmtContextTouchedKeys (rosChangeTrace summary)
    ObserveDirtyContexts ->
      cmtDirtyContexts (rosChangeTrace summary)
{-# INLINE runRunObservationWith #-}

runObservationSummary ::
  Ord c =>
  SaturatingContextEGraph owner capability f a c ->
  SaturationReport (EGraphU owner capability f a c) ->
  RunObservationSummary owner c f
runObservationSummary initialCarrier report =
  RunObservationSummary
    { rosFiredRules =
        Set.fromList
          [ strGroup scheduleTrace
            | scheduleTrace <- scheduleTraces,
              strScheduledCount scheduleTrace > 0
          ],
      rosBlockedRules =
        Set.fromList
          [ strGroup scheduleTrace
            | scheduleTrace <- scheduleTraces,
              scheduleTraceSkippedByScheduler scheduleTrace
                || isJust (scheduleTraceBannedUntil scheduleTrace)
          ],
      rosChangeTrace = runChangeTrace initialCarrier report
    }
  where
    scheduleTraces =
      reportScheduleTrace report
{-# INLINE runObservationSummary #-}

runChangeTrace ::
  Ord c =>
  SaturatingContextEGraph owner capability f a c ->
  SaturationReport (EGraphU owner capability f a c) ->
  ContextMutationTrace owner c f
runChangeTrace initialCarrier report =
  eGraphSaturationChangeTrace
    (sceContextGraph initialCarrier)
    (sceContextGraph (srCarrier report))
    (rcChangeSummary (srFinalCore report))
{-# INLINE runChangeTrace #-}
