{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Moonlight.Flow.Runtime.Engine.Dispatch.Core
  ( DispatchOp (..),
    flushRuntimeOnce,
    stepScheduledRuntimeDataflowOp,
    runRuntimeDataflowOp,
  )
where

import Control.Monad
  ( (>=>),
  )
import Moonlight.Delta.Time
  ( Timed (..),
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Flow.Runtime.Carrier.Amalgamation qualified as Carrier
import Moonlight.Flow.Runtime.Carrier.Restrict qualified as Carrier
import Moonlight.Flow.Runtime.Carrier.Reuse qualified as Carrier
import Moonlight.Flow.Runtime.Engine.Dispatch.Carrier
  ( scheduleCarrierActionFanout,
    stepAtomEventBatch,
  )
import Moonlight.Flow.Runtime.Engine.Dispatch.Shard
  ( flushIndexOps,
    flushProjectOps,
    flushRestrictOps,
    stepIndex,
    stepProject,
    stepRestrict,
  )
import Moonlight.Flow.Runtime.Engine.Touch
  ( scheduleCarrierCommitTraceFanout,
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Execution.IR
  ( (:+:) (..),
    AmalgamateCarrierFamilyOp (..),
    ApplyAtomEventsOp (..),
    DeriveSubsumedCarrierOp (..),
    RepairFactorBatchOp (..),
    RestrictCarrierOp (..),
    RunIndexOp (..),
    RunProjectOp (..),
    RunRestrictOp (..),
    RuntimeDataflowOp,
    RuntimeDataflowOpKind,
    ScheduledRuntimeDataflowOp,
    foldRuntimeDataflowOpKind,
    runtimeDataflowOpKind,
  )
import Moonlight.Flow.Runtime.Factor.Repair
  ( repairFactorBatch,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
  )
import Moonlight.Flow.Runtime.Time
  ( RuntimeEventTime,
  )

class DispatchOp op where
  dispatchRuntimeDataflowOp ::
    ( boundary ~ RuntimeBoundary,
      Ord ctx,
      Ord prop,
      Semigroup evidence
    ) =>
    RuntimeEventTime ctx ->
    op ctx prop boundary evidence ->
    RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
    Either
      (RelationalRuntimeError ctx prop boundary evidence)
      (RelDiffRuntime ctx prop boundary evidence joinState joinErr)

instance (DispatchOp f, DispatchOp g) => DispatchOp (f :+: g) where
  dispatchRuntimeDataflowOp eventTime op runtime =
    case op of
      InL leftOp ->
        dispatchRuntimeDataflowOp eventTime leftOp runtime
      InR rightOp ->
        dispatchRuntimeDataflowOp eventTime rightOp runtime

instance DispatchOp RuntimeDataflowOpKind where
  dispatchRuntimeDataflowOp eventTime kind runtime =
    foldRuntimeDataflowOpKind
      kind
      (\op -> dispatchRuntimeDataflowOp eventTime op runtime)
      (\op -> dispatchRuntimeDataflowOp eventTime op runtime)
      (\op -> dispatchRuntimeDataflowOp eventTime op runtime)
      (\op -> dispatchRuntimeDataflowOp eventTime op runtime)
      (\op -> dispatchRuntimeDataflowOp eventTime op runtime)
      (\op -> dispatchRuntimeDataflowOp eventTime op runtime)
      (\op -> dispatchRuntimeDataflowOp eventTime op runtime)
      (\op -> dispatchRuntimeDataflowOp eventTime op runtime)

instance DispatchOp ApplyAtomEventsOp where
  dispatchRuntimeDataflowOp eventTime (ApplyAtomEventsOp addr scope events) runtime =
    stepAtomEventBatch addr scope (Timed eventTime events) runtime

instance DispatchOp RunProjectOp where
  dispatchRuntimeDataflowOp eventTime (RunProjectOp _contextValue shard delta) runtime =
    stepProject shard (Timed eventTime delta) runtime

instance DispatchOp RunRestrictOp where
  dispatchRuntimeDataflowOp eventTime (RunRestrictOp shard delta) runtime =
    stepRestrict shard (Timed eventTime delta) runtime

instance DispatchOp RunIndexOp where
  dispatchRuntimeDataflowOp eventTime (RunIndexOp shard delta) runtime =
    stepIndex shard (Timed eventTime delta) runtime

instance DispatchOp RepairFactorBatchOp where
  dispatchRuntimeDataflowOp eventTime (RepairFactorBatchOp request _reads _writes) runtime = do
    (_reports, runtimeRepaired, commitTrace) <-
      repairFactorBatch eventTime request runtime
    scheduleCarrierCommitTraceFanout commitTrace runtimeRepaired

instance DispatchOp DeriveSubsumedCarrierOp where
  dispatchRuntimeDataflowOp eventTime (DeriveSubsumedCarrierOp reuseId _source target) runtime =
    scheduleCarrierActionFanout $
      Carrier.deriveSubsumedCarrier eventTime reuseId target runtime

instance DispatchOp RestrictCarrierOp where
  dispatchRuntimeDataflowOp eventTime (RestrictCarrierOp restrictKey) runtime =
    scheduleCarrierActionFanout $
      Carrier.restrictCarrier eventTime restrictKey runtime

instance DispatchOp AmalgamateCarrierFamilyOp where
  dispatchRuntimeDataflowOp eventTime (AmalgamateCarrierFamilyOp family _members _targets) runtime =
    scheduleCarrierActionFanout $
      Carrier.amalgamateCarrierFamily
        eventTime
        family
        runtime

flushRuntimeOnce ::
  (Ord ctx, Ord prop) =>
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
flushRuntimeOnce =
  flushProjectOps
    >=> flushRestrictOps
    >=> flushIndexOps

stepScheduledRuntimeDataflowOp ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop,
    Semigroup evidence
  ) =>
  ScheduledRuntimeDataflowOp ctx prop boundary evidence ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
stepScheduledRuntimeDataflowOp timedOp =
  runRuntimeDataflowOp (timedAt timedOp) (timedValue timedOp)

runRuntimeDataflowOp ::
  ( boundary ~ RuntimeBoundary,
    Ord ctx,
    Ord prop,
    Semigroup evidence
  ) =>
  RuntimeEventTime ctx ->
  RuntimeDataflowOp ctx prop boundary evidence ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
runRuntimeDataflowOp eventTime op =
  dispatchRuntimeDataflowOp eventTime (runtimeDataflowOpKind op)
{-# NOINLINE runRuntimeDataflowOp #-}
