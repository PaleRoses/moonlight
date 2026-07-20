module ArrangementSpec
  ( tests,
  )
where

import Moonlight.Differential.Algebra.ZSet
  ( Timed (..),
  )
import Moonlight.Differential.Algebra.ZSet qualified as ZSet
import Moonlight.Differential.Arrangement
  ( appendArrangementBatch,
    appendArrangementKeyRows,
    arrangeByKey,
    cursorAt,
    foldArrangementKey,
    foldSliceAfter,
  )
import Moonlight.Differential.Batch
  ( Batch,
    fromUpdates,
  )
import Moonlight.Differential.Cursor
  ( cursorCellCount,
    cursorNull,
    foldCursor,
  )
import Moonlight.Differential.Trace
  ( traceAppendBatch,
    traceFromBatch,
    traceFromBatches,
    traceFromUpdates,
    snapshotTraceBatch,
  )
import Moonlight.Differential.Update
  ( Update (..),
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
    testCase,
  )

type TestBatch = Batch Int String Char Int

type TestTraceUpdate = Update Int String Char Int

tests :: TestTree
tests =
  testGroup
    "arrangement laws"
    [ testCase "Arrangement cursor reads one key and folds values without allocating a trace list" arrangementCursorReadsKey,
      testCase "Arrangement slice reads only times after a lower frontier" arrangementSliceAfterFrontier,
      testCase "Arrangement appends batches through Cursor-owned packed merge" arrangementAppendBatchMergesPackedCursor,
      testCase "Arrangement appends key rows through Cursor-owned packed merge" arrangementAppendKeyRowsMergesPackedCursor,
      testCase "Arrangement append is a lawful materialized Trace view" arrangementAppendMatchesTraceView,
      testCase "Arrangement consumes trace batch cover without global collapse" arrangementConsumesTraceBatchCover,
      testCase "Arrangement direct ingestion prunes zero key fibers" arrangementDirectIngestionPrunesZeroFibers
    ]

testBatch :: [Update Int String Char Int] -> TestBatch
testBatch =
  fromUpdates

arrangementCursorReadsKey :: IO ()
arrangementCursorReadsKey = do
  let arrangement =
        arrangeByKey
          ( traceFromUpdates
              ( [ Update 0 "left" 'a' 2,
                  Update 1 "left" 'a' 3,
                  Update 0 "right" 'z' 9
                ] ::
                  [TestTraceUpdate]
              )
          )
      cursor =
        cursorAt "left" arrangement
      folded =
        foldCursor (\acc value weight -> (value, weight) : acc) [] cursor
  assertEqual
    "cursorAt isolates a key and folds consolidated values"
    [('a', 5)]
    folded
  assertEqual
    "cursorAt returns a packed timed fiber instead of rebuilding a trace fold"
    2
    (cursorCellCount cursor)

arrangementSliceAfterFrontier :: IO ()
arrangementSliceAfterFrontier = do
  let arrangement =
        arrangeByKey
          ( traceFromUpdates
              ( [ Update 0 "key" 'a' 2,
                  Update 1 "key" 'a' 3,
                  Update 2 "key" 'b' 4,
                  Update 2 "other" 'z' 9
                ] ::
                  [TestTraceUpdate]
              )
          )
      folded =
        foldSliceAfter
          (0 :: Int)
          "key"
          (\acc time value weight -> (time, value, weight) : acc)
          []
          arrangement
      foldedKey =
        foldArrangementKey
          "key"
          (\acc time value weight -> (time, value, weight) : acc)
          []
          arrangement
  assertEqual
    "sliceAfter keeps only strict frontier successors for the chosen key"
    [(2, 'b', 4), (1, 'a', 3)]
    folded
  assertEqual
    "full key fold still sees the entire fiber"
    [(2, 'b', 4), (1, 'a', 3), (0, 'a', 2)]
    foldedKey

arrangementAppendBatchMergesPackedCursor :: IO ()
arrangementAppendBatchMergesPackedCursor = do
  let arrangement0 =
        arrangeByKey
          ( traceFromUpdates
              ( [ Update 0 "key" 'a' 2,
                  Update 1 "key" 'b' 3
                ] ::
                  [TestTraceUpdate]
              )
          )
      batch =
        testBatch
          [ Update 0 "key" 'a' (-2),
            Update 2 "key" 'c' 4
          ]
      arrangement1 =
        appendArrangementBatch batch arrangement0
      cursor =
        cursorAt "key" arrangement1
      folded =
        foldArrangementKey "key" (\acc time value weight -> (time, value, weight) : acc) [] arrangement1
  assertEqual
    "packed cursor merge cancels equal timed cells and keeps nonzero cells"
    [(2, 'c', 4), (1, 'b', 3)]
    folded
  assertEqual
    "packed cursor merge preserves one canonical cell per live timed value"
    2
    (cursorCellCount cursor)

arrangementAppendKeyRowsMergesPackedCursor :: IO ()
arrangementAppendKeyRowsMergesPackedCursor = do
  let arrangement0 =
        arrangeByKey
          ( traceFromUpdates
              ( [ Update 0 "key" 'a' 2,
                  Update 1 "key" 'b' 3
                ] ::
                  [TestTraceUpdate]
              )
          )
      rows =
        ZSet.zsetFromList
          [ (Timed 0 'a', -2 :: Int),
            (Timed 3 'd', 5)
          ]
      arrangement1 =
        appendArrangementKeyRows "key" rows arrangement0
      cursor =
        cursorAt "key" arrangement1
      folded =
        foldArrangementKey "key" (\acc time value weight -> (time, value, weight) : acc) [] arrangement1
  assertEqual
    "key-row append delegates timed-cell cancellation to Cursor"
    [(3, 'd', 5), (1, 'b', 3)]
    folded
  assertEqual
    "key-row append leaves a compact live fiber"
    2
    (cursorCellCount cursor)

arrangementAppendMatchesTraceView :: IO ()
arrangementAppendMatchesTraceView = do
  let traceValue =
        traceFromUpdates
          ( [ Update 0 "key" 'a' 2,
              Update 1 "key" 'b' 3
            ] ::
              [TestTraceUpdate]
          )
      batch =
        testBatch
          [ Update 0 "key" 'a' (-2),
            Update 2 "key" 'c' 4,
            Update 3 "other" 'd' 5
          ]
  assertEqual
    "incremental arrangement append matches arranging the appended trace"
    (arrangeByKey (traceAppendBatch batch traceValue))
    (appendArrangementBatch batch (arrangeByKey traceValue))

arrangementConsumesTraceBatchCover :: IO ()
arrangementConsumesTraceBatchCover = do
  let left =
        testBatch
          [ Update 0 "key" 'a' 2,
            Update 1 "key" 'b' 3
          ]
      middle =
        testBatch
          [ Update 1 "key" 'b' (-3),
            Update 2 "other" 'c' 4
          ]
      right =
        testBatch
          [ Update 2 "other" 'c' 5,
            Update 3 "key" 'd' 7
          ]
      traceValue =
        traceFromBatches [left, middle, right]
      viaBatch =
        arrangeByKey (traceFromBatch (snapshotTraceBatch traceValue))
  assertEqual
    "direct batch-cover arrangement matches the globally collapsed trace view"
    viaBatch
    (arrangeByKey traceValue)

arrangementDirectIngestionPrunesZeroFibers :: IO ()
arrangementDirectIngestionPrunesZeroFibers = do
  let deadLeft =
        testBatch
          [Update 0 "dead" 'x' 4]
      deadRight =
        testBatch
          [Update 0 "dead" 'x' (-4)]
      live =
        testBatch
          [Update 1 "live" 'y' 3]
      arrangement =
        arrangeByKey (traceFromBatches [deadLeft, deadRight, live])
      deadFold =
        foldArrangementKey "dead" (\acc time value weight -> (time, value, weight) : acc) [] arrangement
      liveFold =
        foldArrangementKey "live" (\acc time value weight -> (time, value, weight) : acc) [] arrangement
  assertEqual
    "direct ingestion deletes a key whose local fiber cancels to zero"
    []
    deadFold
  assertBool
    "cancelled key has no cursor payload"
    (cursorNull (cursorAt "dead" arrangement))
  assertEqual
    "other key fibers survive the local cancellation"
    [(1, 'y', 3)]
    liveFold
