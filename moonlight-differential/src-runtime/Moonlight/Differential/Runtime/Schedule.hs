-- | Progress-coupled scheduling: an agenda of priority-ordered, time-keyed
-- work cells coupled to the runtime frontier, so enqueue inserts a pending
-- pointstamp and completion advances it.  Differential owns the frontier; the
-- priority order is a caller-supplied policy knob, never a fixed vocabulary.
module Moonlight.Differential.Runtime.Schedule
  ( ScheduleCell (..),
    ScheduledWork (..),
    ProgressSchedule,
    scheduleFrontier,
    schedulePriorityOrder,
    mkProgressSchedule,
    scheduleEnqueue,
    scheduleDequeue,
    scheduleComplete,
    scheduleQuiescent,
    scheduleCellsEmpty,
    scheduleWork,
    schedulePendingPointstamps,
  )
where

import Control.Applicative
  ( asum,
  )
import Data.Kind
  ( Type,
  )
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
    frontierPendingPointstamps,
    runtimeCapabilityTime,
  )
import Moonlight.Differential.Time
  ( RuntimeTime,
  )

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
    psPriorityOrder :: [priority]
  }
  deriving stock (Eq, Show)

scheduleFrontier ::
  ProgressSchedule ctx epoch phase priority payload ->
  RuntimeFrontier ctx epoch phase
scheduleFrontier =
  psFrontier
{-# INLINE scheduleFrontier #-}

schedulePriorityOrder ::
  ProgressSchedule ctx epoch phase priority payload ->
  [priority]
schedulePriorityOrder =
  psPriorityOrder
{-# INLINE schedulePriorityOrder #-}

mkProgressSchedule ::
  [priority] ->
  RuntimeFrontier ctx epoch phase ->
  ProgressSchedule ctx epoch phase priority payload
mkProgressSchedule priorityOrder frontier =
  ProgressSchedule
    { psAgenda = Map.empty,
      psFrontier = frontier,
      psPriorityOrder = priorityOrder
    }
{-# INLINE mkProgressSchedule #-}

scheduleEnqueue ::
  (Ord ctx, Ord epoch, Ord phase, Ord priority, Semigroup payload) =>
  priority ->
  RuntimeCapability ctx epoch phase ->
  payload ->
  ProgressSchedule ctx epoch phase priority payload ->
  ProgressSchedule ctx epoch phase priority payload
scheduleEnqueue priority capability payload schedule =
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
  asum (fmap dequeueAtPriority (psPriorityOrder schedule))
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
  concatMap workAtPriority (psPriorityOrder schedule)
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
