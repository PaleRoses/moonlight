module Test.Moonlight.Differential.Trace.Indexed
  ( indexedTraceWithIndexesForValidation,
    indexedTraceWithNextIdForValidation,
  )
where

import Moonlight.Differential.Internal.Trace.Indexed
  ( TraceIdCursor (..),
    indexedTraceIndexesRaw,
    indexedTraceNextIdRaw,
  )
import Moonlight.Differential.Internal.Trace.Id
  ( TraceId (..),
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

indexedTraceWithNextIdForValidation ::
  Int ->
  IndexedTrace entry indexes ->
  IndexedTrace entry indexes
indexedTraceWithNextIdForValidation nextId traceValue =
  traceValue {indexedTraceNextIdRaw = TraceIdAvailable (TraceId nextId)}
{-# INLINE indexedTraceWithNextIdForValidation #-}
