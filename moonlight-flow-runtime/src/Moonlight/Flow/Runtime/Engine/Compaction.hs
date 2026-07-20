module Moonlight.Flow.Runtime.Engine.Compaction
  ( compactRuntimeBefore,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict qualified as IntMap
import Moonlight.Flow.Carrier.Core.Frontier
  ( RelDiffFrontier,
  )
import Moonlight.Flow.Carrier.Store
  ( compactCarrierStoreBefore,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase,
  )
import Moonlight.Flow.Runtime.Factor.State
  ( factorProgramsHeldCarrierReads,
  )
import Moonlight.Flow.Runtime.Engine.Queue.Frontier
  ( runtimeDataflowQueueProgressFrontier,
    setRuntimeDataflowQueueFrontier,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    RuntimeEnvelope (..),
    RuntimeEnv (..),
  )
import Moonlight.Flow.Runtime.Error
  ( RuntimeError (..),
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Carrier.State
  ( RuntimeShardRegistry (..),
    runtimeShardRegistry,
    setRuntimeShardRegistry,
  )
import Moonlight.Flow.Runtime.Engine.State
  ( runtimeEngineQueue,
    setRuntimeEngineQueue,
  )
import Moonlight.Flow.Runtime.Topology.Routing
  ( Shard (..),
  )

compactRuntimeBefore ::
  (Ord ctx, Ord prop) =>
  RelDiffFrontier ctx RelationalPhase ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
compactRuntimeBefore requestedFrontier runtime = do
  let state0 =
        rdrState runtime
      queue0 =
        runtimeEngineQueue state0
      frontier =
        runtimeDataflowQueueProgressFrontier
          requestedFrontier
          queue0

  compactedIndexOps <-
    IntMap.traverseWithKey
      (compactOne frontier)
      (rsrIndexOps (runtimeShardRegistry state0))

  let registry0 =
        runtimeShardRegistry state0

  Right
    runtime
      { rdrState =
          setRuntimeShardRegistry
            registry0 {rsrIndexOps = compactedIndexOps}
            ( setRuntimeEngineQueue
                (setRuntimeDataflowQueueFrontier frontier queue0)
                state0
            )
      }
  where
    heldReads =
      factorProgramsHeldCarrierReads
        (reAtomCarrierEmitSpec (rdrEnv runtime))
        runtime

    compactOne frontier shardKey indexState =
      first
        (RuntimeCompactionError (Shard shardKey))
        ( compactCarrierStoreBefore
            (reCarrierSummaryOps (rdrEnv runtime))
            (reContextLattice (rdrEnv runtime))
            heldReads
            frontier
            indexState
        )
{-# INLINE compactRuntimeBefore #-}
