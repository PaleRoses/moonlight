{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Trace.Id
  ( TraceId,
    traceIdKey,
    traceIdFromKey,
    initialTraceId,
    nextTraceId,
    maxTraceId,
  )
where

import Moonlight.Differential.Internal.Trace.Id
  ( TraceId (..),
  )

traceIdKey :: TraceId -> Int
traceIdKey =
  unTraceId

traceIdFromKey :: Int -> Maybe TraceId
traceIdFromKey traceKey
  | traceKey < 0 =
      Nothing
  | otherwise =
      Just (TraceId traceKey)

initialTraceId :: TraceId
initialTraceId =
  TraceId 0

nextTraceId :: TraceId -> TraceId
nextTraceId (TraceId traceKey) =
  TraceId (traceKey + 1)

maxTraceId :: TraceId -> TraceId -> TraceId
maxTraceId leftId rightId =
  if traceIdKey leftId >= traceIdKey rightId
    then leftId
    else rightId
