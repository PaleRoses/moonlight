module Moonlight.Flow.Runtime.Engine.Step.Flush
  ( flushRuntimeOnce,
  )
where

import Moonlight.Flow.Runtime.Engine.Dispatch.Core qualified as Dispatch
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
  )

flushRuntimeOnce ::
  (Ord ctx, Ord prop) =>
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  Either
    (RelationalRuntimeError ctx prop boundary evidence)
    (RelDiffRuntime ctx prop boundary evidence joinState joinErr)
flushRuntimeOnce =
  Dispatch.flushRuntimeOnce
{-# INLINE flushRuntimeOnce #-}
