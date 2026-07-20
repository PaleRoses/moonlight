-- | Progress-coupled scheduling: an agenda of priority-ordered, time-keyed
-- work cells coupled to the runtime frontier, so enqueue inserts a pending
-- pointstamp and completion advances it.  Differential owns the frontier; the
-- priority order is a caller-supplied policy knob, never a fixed vocabulary.
module Moonlight.Differential.Runtime.Schedule
  ( ScheduleError (..),
    SchedulePriorityPlan,
    mkSchedulePriorityPlan,
    schedulePriorityPlanOrder,
    ScheduleCell (..),
    ScheduledWork (..),
    ProgressSchedule,
    scheduleFrontier,
    schedulePriorityPlan,
    schedulePriorityOrder,
    mkProgressSchedule,
    scheduleEnqueue,
    scheduleDequeue,
    scheduleComplete,
    scheduleRetargetFrontier,
    scheduleQuiescent,
    scheduleCellsEmpty,
    scheduleWork,
    schedulePendingPointstamps,
  )
where

import Control.Applicative
  ( asum,
  )
import Control.Monad
  ( foldM,
  )
import Data.Kind
  ( Type,
  )
import Data.List.NonEmpty
  ( NonEmpty,
  )
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Core
  ( PartialOrder,
  )
import Moonlight.Differential.Frontier
  ( RuntimeCapability,
    RuntimeFrontier,
    RuntimeFrontierError,
    frontierCompletePending,
    frontierInsertPending,
    frontierPendingCounts,
    frontierPendingPointstamps,
    frontierWithPendingCounts,
    runtimeCapabilityTime,
  )
import Moonlight.Differential.Time
  ( RuntimeTime,
  )

type ScheduleError :: Type -> Type
data ScheduleError priority
  = SchedulePriorityOrderEmpty
  | SchedulePriorityDuplicate !priority
  | SchedulePriorityUnknown !priority
  deriving stock (Eq, Show)

type SchedulePriorityPlan :: Type -> Type
data SchedulePriorityPlan priority = SchedulePriorityPlan
  { sppOrder :: !(NonEmpty priority),
    sppMembers :: !(Set priority)
  }
  deriving stock (Eq, Show)

mkSchedulePriorityPlan ::
  Ord priority =>
  [priority] ->
  Either (ScheduleError priority) (SchedulePriorityPlan priority)
mkSchedulePriorityPlan priorityOrder = do
  order <-
    maybe
      (Left SchedulePriorityOrderEmpty)
      Right
      (NonEmpty.nonEmpty priorityOrder)
  members <-
    foldM insertPriority Set.empty priorityOrder
  pure
    SchedulePriorityPlan
      { sppOrder = order,
        sppMembers = members
      }
  where
    insertPriority ::
      Ord priority' =>
      Set priority' ->
      priority' ->
      Either (ScheduleError priority') (Set priority')
    insertPriority members priority
      | Set.member priority members =
          Left (SchedulePriorityDuplicate priority)
      | otherwise =
          Right (Set.insert priority members)

schedulePriorityPlanOrder ::
  SchedulePriorityPlan priority ->
  NonEmpty priority
schedulePriorityPlanOrder =
  sppOrder
{-# INLINE schedulePriorityPlanOrder #-}

type ScheduleCell :: Type -> Type -> Type -> Type -> Type
data ScheduleCell ctx epoch phase payload = ScheduleCell
  { scheduleCellCapability :: RuntimeCapability ctx epoch phase,
    scheduleCellPayload :: payload
  }
  deriving stock (Eq, Show)

instance Semigroup payload => Semigroup (ScheduleCell ctx epoch phase payload) where
  ScheduleCell _incomingCapability incomingPayload <> ScheduleCell retainedCapability retainedPayload =
    ScheduleCell retainedCapability (retainedPayload <> incomingPayload)
  {-# INLINE (<>) #-}

type ScheduledWork :: Type -> Type -> Type -> Type -> Type -> Type
data ScheduledWork ctx epoch phase priority payload = ScheduledWork
  { scheduledWorkPriority :: priority,
    scheduledWorkTime :: RuntimeTime ctx epoch phase,
    scheduledWorkCell :: ScheduleCell ctx epoch phase payload
  }
  deriving stock (Eq, Show)

type ProgressSchedule :: Type -> Type -> Type -> Type -> Type -> Type
data ProgressSchedule ctx epoch phase priority payload = ProgressSchedule
  { psAgenda :: Map priority (Map (RuntimeTime ctx epoch phase) (ScheduleCell ctx epoch phase payload)),
    psFrontier :: RuntimeFrontier ctx epoch phase,
    psPriorityPlan :: SchedulePriorityPlan priority
  }
  deriving stock (Eq, Show)

scheduleFrontier ::
  ProgressSchedule ctx epoch phase priority payload ->
  RuntimeFrontier ctx epoch phase
scheduleFrontier =
  psFrontier
{-# INLINE scheduleFrontier #-}

schedulePriorityPlan ::
  ProgressSchedule ctx epoch phase priority payload ->
  SchedulePriorityPlan priority
schedulePriorityPlan =
  psPriorityPlan
{-# INLINE schedulePriorityPlan #-}

schedulePriorityOrder ::
  ProgressSchedule ctx epoch phase priority payload ->
  [priority]
schedulePriorityOrder =
  NonEmpty.toList . sppOrder . psPriorityPlan
{-# INLINE schedulePriorityOrder #-}

mkProgressSchedule ::
  SchedulePriorityPlan priority ->
  RuntimeFrontier ctx epoch phase ->
  ProgressSchedule ctx epoch phase priority payload
mkProgressSchedule priorityPlan frontier =
  ProgressSchedule
    { psAgenda = Map.empty,
      psFrontier = frontier,
      psPriorityPlan = priorityPlan
    }
{-# INLINE mkProgressSchedule #-}

scheduleEnqueue ::
  (Ord ctx, Ord epoch, Ord phase, Ord priority, Semigroup payload) =>
  priority ->
  RuntimeCapability ctx epoch phase ->
  payload ->
  ProgressSchedule ctx epoch phase priority payload ->
  Either
    (ScheduleError priority)
    (ProgressSchedule ctx epoch phase priority payload)
scheduleEnqueue priority capability payload schedule
  | Set.notMember priority (sppMembers (psPriorityPlan schedule)) =
      Left (SchedulePriorityUnknown priority)
  | otherwise =
      Right
        schedule
          { psAgenda =
              Map.insertWith
                (Map.unionWith (<>))
                priority
                (Map.singleton time (ScheduleCell capability payload))
                (psAgenda schedule),
            psFrontier =
              if hadCell
                then psFrontier schedule
                else frontierInsertPending time (psFrontier schedule)
          }
  where
    time =
      runtimeCapabilityTime capability
    hadCell =
      maybe False (Map.member time) (Map.lookup priority (psAgenda schedule))

scheduleDequeue ::
  Ord priority =>
  ProgressSchedule ctx epoch phase priority payload ->
  Maybe
    ( ScheduledWork ctx epoch phase priority payload,
      ProgressSchedule ctx epoch phase priority payload
    )
scheduleDequeue schedule =
  asum (fmap dequeueAtPriority (NonEmpty.toList (sppOrder (psPriorityPlan schedule))))
  where
    dequeueAtPriority priority = do
      cellsByTime <- Map.lookup priority (psAgenda schedule)
      ((time, cell), remaining) <- Map.minViewWithKey cellsByTime
      let nextAgenda =
            if Map.null remaining
              then Map.delete priority (psAgenda schedule)
              else Map.insert priority remaining (psAgenda schedule)
      pure
        ( ScheduledWork priority time cell,
          schedule {psAgenda = nextAgenda}
        )

scheduleComplete ::
  (Ord ctx, Ord epoch, Ord phase, PartialOrder epoch, PartialOrder phase) =>
  RuntimeCapability ctx epoch phase ->
  ProgressSchedule ctx epoch phase priority payload ->
  Either
    (RuntimeFrontierError ctx epoch phase)
    (ProgressSchedule ctx epoch phase priority payload)
scheduleComplete capability schedule =
  fmap
    (\frontier -> schedule {psFrontier = frontier})
    (frontierCompletePending (runtimeCapabilityTime capability) (psFrontier schedule))

scheduleRetargetFrontier ::
  RuntimeFrontier ctx epoch phase ->
  ProgressSchedule ctx epoch phase priority payload ->
  ProgressSchedule ctx epoch phase priority payload
scheduleRetargetFrontier requestedFrontier schedule =
  schedule
    { psFrontier =
        frontierWithPendingCounts
          (frontierPendingCounts (psFrontier schedule))
          requestedFrontier
    }
{-# INLINE scheduleRetargetFrontier #-}

scheduleCellsEmpty ::
  ProgressSchedule ctx epoch phase priority payload ->
  Bool
scheduleCellsEmpty =
  all Map.null . Map.elems . psAgenda
{-# INLINE scheduleCellsEmpty #-}

scheduleQuiescent ::
  ProgressSchedule ctx epoch phase priority payload ->
  Bool
scheduleQuiescent schedule =
  scheduleCellsEmpty schedule
    && Set.null (schedulePendingPointstamps schedule)
{-# INLINE scheduleQuiescent #-}

scheduleWork ::
  Ord priority =>
  ProgressSchedule ctx epoch phase priority payload ->
  [ScheduledWork ctx epoch phase priority payload]
scheduleWork schedule =
  concatMap workAtPriority (NonEmpty.toList (sppOrder (psPriorityPlan schedule)))
  where
    workAtPriority priority =
      fmap
        (\(time, cell) -> ScheduledWork priority time cell)
        (Map.toAscList (Map.findWithDefault Map.empty priority (psAgenda schedule)))

schedulePendingPointstamps ::
  ProgressSchedule ctx epoch phase priority payload ->
  Set (RuntimeTime ctx epoch phase)
schedulePendingPointstamps =
  frontierPendingPointstamps . psFrontier
{-# INLINE schedulePendingPointstamps #-}
