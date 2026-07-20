{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Differential.Trace.Id
  ( TraceId,
    TraceIdError (..),
    traceIdKey,
    traceIdFromKey,
    validateTraceId,
    initialTraceId,
    nextTraceId,
  )
where

import Moonlight.Differential.Internal.Trace.Id
  ( TraceId (..),
  )

data TraceIdError
  = TraceIdNegative !Int
  | TraceIdReserved !Int
  | TraceIdExhausted
  deriving stock (Eq, Ord, Show)

traceIdKey :: TraceId -> Int
traceIdKey =
  unTraceId

traceIdFromKey :: Int -> Either TraceIdError TraceId
traceIdFromKey traceKey
  | traceKey < 0 =
      Left (TraceIdNegative traceKey)
  | traceKey == maxBound =
      Left (TraceIdReserved traceKey)
  | otherwise =
      Right (TraceId traceKey)

validateTraceId :: TraceId -> Either TraceIdError TraceId
validateTraceId traceId =
  traceIdFromKey (traceIdKey traceId)

initialTraceId :: TraceId
initialTraceId =
  TraceId 0

nextTraceId :: TraceId -> Either TraceIdError TraceId
nextTraceId traceId = do
  TraceId traceKey <- validateTraceId traceId
  if traceKey == maxBound - 1
    then Left TraceIdExhausted
    else traceIdFromKey (traceKey + 1)
