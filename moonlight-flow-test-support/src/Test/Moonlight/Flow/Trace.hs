{-# LANGUAGE DerivingStrategies #-}

module Test.Moonlight.Flow.Trace
  ( ProjectionTraceId (..),
    TraceReplayError (..),
    TraceReplaySummary (..),
    loadProjectionTrace,
    replayProjectionTrace,
  )
where

import System.Directory (doesFileExist)

data ProjectionTraceId
  = ProjectionCutoffSynthetic001
  | ProjectionProduction001
  | ProjectionProductionChurn001
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

data TraceReplayError
  = TraceFileMissing !FilePath
  deriving stock (Eq, Ord, Show, Read)

data TraceReplaySummary = TraceReplaySummary
  { trsTraceId :: !ProjectionTraceId,
    trsEventCount :: !Int
  }
  deriving stock (Eq, Ord, Show, Read)

loadProjectionTrace :: FilePath -> ProjectionTraceId -> IO (Either TraceReplayError FilePath)
loadProjectionTrace root traceId = do
  let path = root <> "/" <> traceFileName traceId
  exists <- doesFileExist path
  pure $ if exists then Right path else Left (TraceFileMissing path)

replayProjectionTrace :: FilePath -> ProjectionTraceId -> IO (Either TraceReplayError TraceReplaySummary)
replayProjectionTrace root traceId = do
  loaded <- loadProjectionTrace root traceId
  pure (fmap (const TraceReplaySummary {trsTraceId = traceId, trsEventCount = 0}) loaded)

traceFileName :: ProjectionTraceId -> FilePath
traceFileName traceId =
  case traceId of
    ProjectionCutoffSynthetic001 -> "projection-cutoff-synthetic-001.trace"
    ProjectionProduction001 -> "projection-production-001.trace"
    ProjectionProductionChurn001 -> "projection-production-churn-001.trace"
