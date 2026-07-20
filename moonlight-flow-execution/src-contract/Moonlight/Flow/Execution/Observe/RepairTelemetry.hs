{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Execution.Observe.RepairTelemetry
  ( RepairTelemetryLevel (..),
    RepairTelemetryConfig (..),
    summaryRepairTelemetryConfig,
    detailedRepairTelemetryConfig,
    defaultRepairTelemetryConfig,
    repairTelemetryDetailed,
    RepairTelemetry (..),
    emptyRepairTelemetry,
    repairTelemetryDifference,
    repairTelemetryWeight,
    recordSelectedRepairKeys,
    recordSelectedRepairCell,
    recordRepairRowGauges,
    recordRepairSupportRowRefs,
    recordPvAtomCall,
    recordPvPlusCall,
    recordPvTimesCall,
    recordProvInternLookup,
    recordProvInternInsert,
    recordFactorCellInsert,
    recordFactorCellDelete,
    recordFactorPayloadSet,
    IncrementalUpdateTrace (..),
    emptyIncrementalUpdateTrace,
  )
where

import Data.Kind
  ( Type,
  )

type RepairTelemetryLevel :: Type
data RepairTelemetryLevel
  = RepairTelemetrySummary
  | RepairTelemetryDetailed
  deriving stock (Eq, Ord, Show, Read)

type RepairTelemetryConfig :: Type
data RepairTelemetryConfig = RepairTelemetryConfig
  { rtcLevel :: !RepairTelemetryLevel
  }
  deriving stock (Eq, Ord, Show, Read)

summaryRepairTelemetryConfig :: RepairTelemetryConfig
summaryRepairTelemetryConfig =
  RepairTelemetryConfig
    { rtcLevel = RepairTelemetrySummary
    }
{-# INLINE summaryRepairTelemetryConfig #-}

detailedRepairTelemetryConfig :: RepairTelemetryConfig
detailedRepairTelemetryConfig =
  RepairTelemetryConfig
    { rtcLevel = RepairTelemetryDetailed
    }
{-# INLINE detailedRepairTelemetryConfig #-}

defaultRepairTelemetryConfig :: RepairTelemetryConfig
defaultRepairTelemetryConfig =
  detailedRepairTelemetryConfig
{-# INLINE defaultRepairTelemetryConfig #-}

repairTelemetryDetailed :: RepairTelemetryConfig -> Bool
repairTelemetryDetailed config =
  rtcLevel config == RepairTelemetryDetailed
{-# INLINE repairTelemetryDetailed #-}

type RepairTelemetry :: Type
data RepairTelemetry = RepairTelemetry
  { rtSelectedOutputKeys :: {-# UNPACK #-} !Int,
    rtSelectedRepairCells :: {-# UNPACK #-} !Int,
    rtRepairRowMapEntries :: {-# UNPACK #-} !Int,
    rtRepairSupportOutputKeys :: {-# UNPACK #-} !Int,
    rtRepairSupportRowRefsEnumerated :: {-# UNPACK #-} !Int,
    rtRepairSupportRowRefsUnique :: {-# UNPACK #-} !Int,
    rtSupportCellsVisited :: {-# UNPACK #-} !Int,
    rtSupportPatchEdgesPreserved :: {-# UNPACK #-} !Int,
    rtSupportPatchEdgesInserted :: {-# UNPACK #-} !Int,
    rtSupportPatchEdgesDeleted :: {-# UNPACK #-} !Int,
    rtSupportPatchOutputKeysDeleted :: {-# UNPACK #-} !Int,
    rtPvAtomCalls :: {-# UNPACK #-} !Int,
    rtPvPlusCalls :: {-# UNPACK #-} !Int,
    rtPvTimesCalls :: {-# UNPACK #-} !Int,
    rtProvInternLookups :: {-# UNPACK #-} !Int,
    rtProvInternInserts :: {-# UNPACK #-} !Int,
    rtFactorCellInserts :: {-# UNPACK #-} !Int,
    rtFactorCellDeletes :: {-# UNPACK #-} !Int,
    rtFactorPayloadSets :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show, Read)

emptyRepairTelemetry :: RepairTelemetry
emptyRepairTelemetry =
  RepairTelemetry
    { rtSelectedOutputKeys = 0,
      rtSelectedRepairCells = 0,
      rtRepairRowMapEntries = 0,
      rtRepairSupportOutputKeys = 0,
      rtRepairSupportRowRefsEnumerated = 0,
      rtRepairSupportRowRefsUnique = 0,
      rtSupportCellsVisited = 0,
      rtSupportPatchEdgesPreserved = 0,
      rtSupportPatchEdgesInserted = 0,
      rtSupportPatchEdgesDeleted = 0,
      rtSupportPatchOutputKeysDeleted = 0,
      rtPvAtomCalls = 0,
      rtPvPlusCalls = 0,
      rtPvTimesCalls = 0,
      rtProvInternLookups = 0,
      rtProvInternInserts = 0,
      rtFactorCellInserts = 0,
      rtFactorCellDeletes = 0,
      rtFactorPayloadSets = 0
    }
{-# INLINE emptyRepairTelemetry #-}

instance Semigroup RepairTelemetry where
  left <> right =
    RepairTelemetry
      { rtSelectedOutputKeys = add rtSelectedOutputKeys,
        rtSelectedRepairCells = add rtSelectedRepairCells,
        rtRepairRowMapEntries = add rtRepairRowMapEntries,
        rtRepairSupportOutputKeys = add rtRepairSupportOutputKeys,
        rtRepairSupportRowRefsEnumerated = add rtRepairSupportRowRefsEnumerated,
        rtRepairSupportRowRefsUnique = add rtRepairSupportRowRefsUnique,
        rtSupportCellsVisited = add rtSupportCellsVisited,
        rtSupportPatchEdgesPreserved = add rtSupportPatchEdgesPreserved,
        rtSupportPatchEdgesInserted = add rtSupportPatchEdgesInserted,
        rtSupportPatchEdgesDeleted = add rtSupportPatchEdgesDeleted,
        rtSupportPatchOutputKeysDeleted = add rtSupportPatchOutputKeysDeleted,
        rtPvAtomCalls = add rtPvAtomCalls,
        rtPvPlusCalls = add rtPvPlusCalls,
        rtPvTimesCalls = add rtPvTimesCalls,
        rtProvInternLookups = add rtProvInternLookups,
        rtProvInternInserts = add rtProvInternInserts,
        rtFactorCellInserts = add rtFactorCellInserts,
        rtFactorCellDeletes = add rtFactorCellDeletes,
        rtFactorPayloadSets = add rtFactorPayloadSets
      }
    where
      add field =
        field left + field right
  {-# INLINE (<>) #-}

instance Monoid RepairTelemetry where
  mempty =
    emptyRepairTelemetry
  {-# INLINE mempty #-}

repairTelemetryDifference :: RepairTelemetry -> RepairTelemetry -> RepairTelemetry
repairTelemetryDifference newer older =
  RepairTelemetry
    { rtSelectedOutputKeys = sub rtSelectedOutputKeys,
      rtSelectedRepairCells = sub rtSelectedRepairCells,
      rtRepairRowMapEntries = sub rtRepairRowMapEntries,
      rtRepairSupportOutputKeys = sub rtRepairSupportOutputKeys,
      rtRepairSupportRowRefsEnumerated = sub rtRepairSupportRowRefsEnumerated,
      rtRepairSupportRowRefsUnique = sub rtRepairSupportRowRefsUnique,
      rtSupportCellsVisited = sub rtSupportCellsVisited,
      rtSupportPatchEdgesPreserved = sub rtSupportPatchEdgesPreserved,
      rtSupportPatchEdgesInserted = sub rtSupportPatchEdgesInserted,
      rtSupportPatchEdgesDeleted = sub rtSupportPatchEdgesDeleted,
      rtSupportPatchOutputKeysDeleted = sub rtSupportPatchOutputKeysDeleted,
      rtPvAtomCalls = sub rtPvAtomCalls,
      rtPvPlusCalls = sub rtPvPlusCalls,
      rtPvTimesCalls = sub rtPvTimesCalls,
      rtProvInternLookups = sub rtProvInternLookups,
      rtProvInternInserts = sub rtProvInternInserts,
      rtFactorCellInserts = sub rtFactorCellInserts,
      rtFactorCellDeletes = sub rtFactorCellDeletes,
      rtFactorPayloadSets = sub rtFactorPayloadSets
    }
  where
    sub field =
      max 0 (field newer - field older)
{-# INLINE repairTelemetryDifference #-}

repairTelemetryWeight :: RepairTelemetry -> Int
repairTelemetryWeight telemetry =
  rtRepairSupportRowRefsEnumerated telemetry
    + rtSupportCellsVisited telemetry
    + rtPvAtomCalls telemetry
    + rtPvPlusCalls telemetry
    + rtPvTimesCalls telemetry
    + rtProvInternLookups telemetry
    + rtSupportPatchEdgesInserted telemetry
    + rtSupportPatchEdgesDeleted telemetry
{-# INLINE repairTelemetryWeight #-}

recordSelectedRepairKeys :: RepairTelemetryConfig -> Int -> RepairTelemetry -> RepairTelemetry
recordSelectedRepairKeys config count telemetry
  | repairTelemetryDetailed config =
      telemetry {rtSelectedOutputKeys = rtSelectedOutputKeys telemetry + max 0 count}
  | otherwise =
      telemetry
{-# INLINE recordSelectedRepairKeys #-}

recordSelectedRepairCell :: RepairTelemetryConfig -> RepairTelemetry -> RepairTelemetry
recordSelectedRepairCell config telemetry
  | repairTelemetryDetailed config =
      telemetry {rtSelectedRepairCells = rtSelectedRepairCells telemetry + 1}
  | otherwise =
      telemetry
{-# INLINE recordSelectedRepairCell #-}

recordRepairRowGauges :: RepairTelemetryConfig -> Int -> Int -> RepairTelemetry -> RepairTelemetry
recordRepairRowGauges config rowEntries supportOutputKeys telemetry
  | repairTelemetryDetailed config =
      telemetry
        { rtRepairRowMapEntries = rtRepairRowMapEntries telemetry + max 0 rowEntries,
          rtRepairSupportOutputKeys = rtRepairSupportOutputKeys telemetry + max 0 supportOutputKeys
        }
  | otherwise =
      telemetry
{-# INLINE recordRepairRowGauges #-}

recordRepairSupportRowRefs :: RepairTelemetryConfig -> Int -> Int -> RepairTelemetry -> RepairTelemetry
recordRepairSupportRowRefs config enumerated uniqueRows telemetry
  | repairTelemetryDetailed config =
      telemetry
        { rtRepairSupportRowRefsEnumerated =
            rtRepairSupportRowRefsEnumerated telemetry + max 0 enumerated,
          rtRepairSupportRowRefsUnique =
            rtRepairSupportRowRefsUnique telemetry + max 0 uniqueRows
        }
  | otherwise =
      telemetry
{-# INLINE recordRepairSupportRowRefs #-}

recordPvAtomCall :: RepairTelemetryConfig -> RepairTelemetry -> RepairTelemetry
recordPvAtomCall config telemetry
  | repairTelemetryDetailed config =
      telemetry {rtPvAtomCalls = rtPvAtomCalls telemetry + 1}
  | otherwise =
      telemetry
{-# INLINE recordPvAtomCall #-}

recordPvPlusCall :: RepairTelemetryConfig -> RepairTelemetry -> RepairTelemetry
recordPvPlusCall config telemetry
  | repairTelemetryDetailed config =
      telemetry {rtPvPlusCalls = rtPvPlusCalls telemetry + 1}
  | otherwise =
      telemetry
{-# INLINE recordPvPlusCall #-}

recordPvTimesCall :: RepairTelemetryConfig -> RepairTelemetry -> RepairTelemetry
recordPvTimesCall config telemetry
  | repairTelemetryDetailed config =
      telemetry {rtPvTimesCalls = rtPvTimesCalls telemetry + 1}
  | otherwise =
      telemetry
{-# INLINE recordPvTimesCall #-}

recordProvInternLookup :: RepairTelemetryConfig -> RepairTelemetry -> RepairTelemetry
recordProvInternLookup config telemetry
  | repairTelemetryDetailed config =
      telemetry {rtProvInternLookups = rtProvInternLookups telemetry + 1}
  | otherwise =
      telemetry
{-# INLINE recordProvInternLookup #-}

recordProvInternInsert :: RepairTelemetryConfig -> RepairTelemetry -> RepairTelemetry
recordProvInternInsert config telemetry
  | repairTelemetryDetailed config =
      telemetry {rtProvInternInserts = rtProvInternInserts telemetry + 1}
  | otherwise =
      telemetry
{-# INLINE recordProvInternInsert #-}

recordFactorCellInsert :: RepairTelemetryConfig -> RepairTelemetry -> RepairTelemetry
recordFactorCellInsert config telemetry
  | repairTelemetryDetailed config =
      telemetry {rtFactorCellInserts = rtFactorCellInserts telemetry + 1}
  | otherwise =
      telemetry
{-# INLINE recordFactorCellInsert #-}

recordFactorCellDelete :: RepairTelemetryConfig -> RepairTelemetry -> RepairTelemetry
recordFactorCellDelete config telemetry
  | repairTelemetryDetailed config =
      telemetry {rtFactorCellDeletes = rtFactorCellDeletes telemetry + 1}
  | otherwise =
      telemetry
{-# INLINE recordFactorCellDelete #-}

recordFactorPayloadSet :: RepairTelemetryConfig -> RepairTelemetry -> RepairTelemetry
recordFactorPayloadSet config telemetry
  | repairTelemetryDetailed config =
      telemetry {rtFactorPayloadSets = rtFactorPayloadSets telemetry + 1}
  | otherwise =
      telemetry
{-# INLINE recordFactorPayloadSet #-}

type IncrementalUpdateTrace :: Type
data IncrementalUpdateTrace = IncrementalUpdateTrace
  { iutAffectedKeys :: {-# UNPACK #-} !Int,
    iutRecomputedCells :: {-# UNPACK #-} !Int,
    iutWorkKeys :: {-# UNPACK #-} !Int,
    iutJoinRuns :: {-# UNPACK #-} !Int,
    iutJoinLeaves :: {-# UNPACK #-} !Int,
    iutRepairTelemetry :: !RepairTelemetry
  }
  deriving stock (Eq, Show)

emptyIncrementalUpdateTrace :: IncrementalUpdateTrace
emptyIncrementalUpdateTrace =
  IncrementalUpdateTrace
    { iutAffectedKeys = 0,
      iutRecomputedCells = 0,
      iutWorkKeys = 0,
      iutJoinRuns = 0,
      iutJoinLeaves = 0,
      iutRepairTelemetry = emptyRepairTelemetry
    }
{-# INLINE emptyIncrementalUpdateTrace #-}
