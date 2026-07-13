module Test.Moonlight.Differential.Trace.Indexed
  ( indexedTraceWithIndexesForValidation,
  )
where

import Moonlight.Differential.Internal.Trace.Indexed
  ( indexedTraceIndexesRaw,
  )
import Moonlight.Differential.Trace.Indexed
  ( IndexedTrace,
  )

indexedTraceWithIndexesForValidation ::
  indexes ->
  IndexedTrace entry indexes ->
  IndexedTrace entry indexes
indexedTraceWithIndexesForValidation indexes traceValue =
  traceValue {indexedTraceIndexesRaw = indexes}
{-# INLINE indexedTraceWithIndexesForValidation #-}
