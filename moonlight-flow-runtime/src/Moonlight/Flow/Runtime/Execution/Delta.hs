module Moonlight.Flow.Runtime.Execution.Delta
  ( mergeRowDeltaDedupCopy,
  )
where

import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Differential.Row.Patch
  ( composePlainRowPatch
  )

mergeRowDeltaDedupCopy ::
  RowDelta ->
  RowDelta ->
  RowDelta
mergeRowDeltaDedupCopy newer older =
  if newer == older
    then older
    else composePlainRowPatch newer older
{-# INLINE mergeRowDeltaDedupCopy #-}
