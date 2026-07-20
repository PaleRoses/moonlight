module Moonlight.Flow.Runtime.Engine.Touch
  ( scheduleTouchFanout,
    scheduleCarrierCommitTraceFanout,
  )
where

import Data.Set
  ( Set,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
  )
import Moonlight.Flow.Runtime.Execution.IR
  ( RuntimeDataflowOp,
  )
import Moonlight.Flow.Runtime.Topology.Lowering.Edge
  ( RuntimeTopologyFanoutStep (..),
    lowerTouchedCarriersFanoutSteps,
  )
import Moonlight.Flow.Runtime.Engine.Schedule.Enqueue
  ( scheduleRuntimeDataflowOps,
  )
import Moonlight.Flow.Runtime.Carrier.Core.Types
  ( CarrierCommitTrace (..),
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
    rsCarrierTopology,
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )

scheduleTouchFanout ::
  (Ord ctx, Ord prop) =>
  Set (CarrierAddr ctx Carrier prop) ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    ( RelDiffRuntime ctx prop boundary evidence joinState joinErr,
      [RuntimeDataflowOp ctx prop boundary evidence],
      [RuntimeTopologyFanoutStep ctx prop boundary evidence]
    )
scheduleTouchFanout touchedAddrs runtime = do
  let carrierTopology =
        rsCarrierTopology (rdrState runtime)
      fanoutSteps =
        lowerTouchedCarriersFanoutSteps
          carrierTopology
          touchedAddrs
      scheduledOps =
        fmap rtfsDataflowOp fanoutSteps
  runtimeFanout <-
    scheduleRuntimeDataflowOps scheduledOps runtime
  Right
    ( runtimeFanout,
      scheduledOps,
      fanoutSteps
    )
{-# INLINE scheduleTouchFanout #-}

scheduleCarrierCommitTraceFanout ::
  (Ord ctx, Ord prop) =>
  CarrierCommitTrace ctx prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
scheduleCarrierCommitTraceFanout commitTrace runtime = do
  (runtimeFanout, _scheduledOps, _fanoutExplanations) <-
    scheduleTouchFanout (cctTouchedCarriers commitTrace) runtime
  Right runtimeFanout
{-# INLINE scheduleCarrierCommitTraceFanout #-}
