module Moonlight.Differential.Effect.Harness.Arrangement
  ( TestTraceUpdate,
    arrangementCellMap,
    arrangementAppendCellMap,
    arrangementKeyCellMap,
    arrangementSliceAfterCellMap,
    arrangementSliceThroughCellMap,
    oracleKeyCellMap,
    updateCellMap,
    arrangeByKeyDenotesReplayedTrace,
    arrangementAppendDenotesAppendThenArrange,
    arrangementKeyFoldFiltersReplayOracle,
    arrangementSliceFoldsFilterReplayOracleByTime,
  )
where

import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Moonlight.Differential.Arrangement
  ( appendArrangementBatch,
    arrangeByKey,
    foldArrangement,
    foldArrangementKey,
    foldSliceAfter,
    foldSliceThrough,
  )
import Moonlight.Differential.Batch
  ( Batch,
    batchToUpdates,
    fromUpdates,
  )
import Moonlight.Differential.Trace
  ( traceFromUpdates,
  )
import Moonlight.Differential.Update
  ( Update (..),
  )

type TestBatch = Batch Int String Char Int

type TestTraceUpdate = Update Int String Char Int

type ArrangementCellMap = Map (Int, String, Char) Int

type ArrangementKeyCellMap = Map (Int, Char) Int

updateCellMap :: [TestTraceUpdate] -> ArrangementCellMap
updateCellMap updates =
  batchUpdatesCellMap (batchToUpdates (fromUpdates updates :: TestBatch))

batchUpdatesCellMap :: [TestTraceUpdate] -> ArrangementCellMap
batchUpdatesCellMap =
  foldMap
    ( \(Update time key value weight) ->
        Map.singleton (time, key, value) weight
    )

arrangementCellMap :: [TestTraceUpdate] -> ArrangementCellMap
arrangementCellMap updates =
  foldArrangement
    (\acc time key value weight -> Map.insertWith (+) (time, key, value) weight acc)
    Map.empty
    (arrangeByKey (traceFromUpdates updates))

arrangementAppendCellMap :: [TestTraceUpdate] -> [TestTraceUpdate] -> ArrangementCellMap
arrangementAppendCellMap initialUpdates batchUpdates =
  foldArrangement
    (\acc time key value weight -> Map.insertWith (+) (time, key, value) weight acc)
    Map.empty
    ( appendArrangementBatch
        (fromUpdates batchUpdates :: TestBatch)
        (arrangeByKey (traceFromUpdates initialUpdates))
    )

arrangementKeyCellMap :: String -> [TestTraceUpdate] -> ArrangementKeyCellMap
arrangementKeyCellMap key updates =
  foldArrangementKey
    key
    (\acc time value weight -> Map.insertWith (+) (time, value) weight acc)
    Map.empty
    (arrangeByKey (traceFromUpdates updates))

arrangementSliceThroughCellMap :: Int -> String -> [TestTraceUpdate] -> ArrangementKeyCellMap
arrangementSliceThroughCellMap upperBound key updates =
  foldSliceThrough
    upperBound
    key
    (\acc time value weight -> Map.insertWith (+) (time, value) weight acc)
    Map.empty
    (arrangeByKey (traceFromUpdates updates))

arrangementSliceAfterCellMap :: Int -> String -> [TestTraceUpdate] -> ArrangementKeyCellMap
arrangementSliceAfterCellMap lowerBound key updates =
  foldSliceAfter
    lowerBound
    key
    (\acc time value weight -> Map.insertWith (+) (time, value) weight acc)
    Map.empty
    (arrangeByKey (traceFromUpdates updates))

oracleKeyCellMap :: (Int -> Bool) -> String -> [TestTraceUpdate] -> ArrangementKeyCellMap
oracleKeyCellMap keepTime key =
  Map.foldMapWithKey
    ( \(time, cellKey, value) weight ->
        if cellKey == key && keepTime time
          then Map.singleton (time, value) weight
          else Map.empty
    )
    . updateCellMap

arrangeByKeyDenotesReplayedTrace :: [TestTraceUpdate] -> Bool
arrangeByKeyDenotesReplayedTrace updates =
  arrangementCellMap updates == updateCellMap updates

arrangementAppendDenotesAppendThenArrange :: [TestTraceUpdate] -> [TestTraceUpdate] -> Bool
arrangementAppendDenotesAppendThenArrange initialUpdates batchUpdates =
  arrangementAppendCellMap initialUpdates batchUpdates == updateCellMap (initialUpdates <> batchUpdates)

arrangementKeyFoldFiltersReplayOracle :: String -> [TestTraceUpdate] -> Bool
arrangementKeyFoldFiltersReplayOracle key updates =
  arrangementKeyCellMap key updates == oracleKeyCellMap (const True) key updates

arrangementSliceFoldsFilterReplayOracleByTime :: String -> [TestTraceUpdate] -> Int -> Bool
arrangementSliceFoldsFilterReplayOracleByTime key updates cutoff =
  arrangementSliceThroughCellMap cutoff key updates == oracleKeyCellMap (<= cutoff) key updates
    && arrangementSliceAfterCellMap cutoff key updates == oracleKeyCellMap (> cutoff) key updates
