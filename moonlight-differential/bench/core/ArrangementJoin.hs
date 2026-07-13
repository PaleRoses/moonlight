module ArrangementJoin where

import Control.DeepSeq (NFData (..))
import Data.Foldable qualified as Foldable
import Moonlight.Differential.Algebra.ZSet qualified as ZSet
import Moonlight.Differential.Algebra.ZSet (Timed (..))
import Moonlight.Differential.Arrangement
import Moonlight.Differential.Batch
import Common
import Storage
import Moonlight.Differential.Cursor
import Moonlight.Differential.Operator.Join
import Moonlight.Differential.Trace
import Moonlight.Differential.Update (Update (..))

newtype PreparedArrangement = PreparedArrangement BenchArrangement

instance NFData PreparedArrangement where
  rnf (PreparedArrangement arrangement) =
    arrangementCellCount arrangement `seq` ()

newtype PreparedArrangementTrace = PreparedArrangementTrace BenchTrace

instance NFData PreparedArrangementTrace where
  rnf (PreparedArrangementTrace traceValue) =
    length (batchToUpdates (snapshotTraceBatch traceValue)) `seq` ()

newtype PreparedArrangementAppend = PreparedArrangementAppend (BenchArrangement, BenchBatch)

instance NFData PreparedArrangementAppend where
  rnf (PreparedArrangementAppend (arrangement, batch)) =
    arrangementCellCount arrangement `seq` length (batchToUpdates batch) `seq` ()

preparedArrangementCase :: Int -> PreparedArrangement
preparedArrangementCase size =
  PreparedArrangement (arrangeByKey (traceFromUpdates (preparedUpdates (updateCase size))))

preparedArrangementHotKeyCase :: Int -> PreparedArrangement
preparedArrangementHotKeyCase size =
  PreparedArrangement (arrangeByKey (traceFromUpdates (preparedUpdates (skewedKeyUpdateCase size))))

cursorMergeCase :: Int -> PreparedCursorMerge
cursorMergeCase size =
  PreparedCursorMerge
    ( cursorFromZSet (timedZSetFromUpdates (preparedUpdates (updateCase size))),
      cursorFromZSet (timedZSetFromUpdates (preparedUpdates (shiftedUpdateCase size)))
    )

timedZSetFromUpdates :: [BenchUpdate] -> ZSet.ZSet (Timed Int Char) Int
timedZSetFromUpdates =
  Foldable.foldl'
    ( \rows update ->
        ZSet.zsetInsert
          Timed
            { timedTime = updateTime update,
              timedValue = updateVal update
            }
          (updateWeight update)
          rows
    )
    ZSet.zsetEmpty

cursorMergeWeight :: PreparedCursorMerge -> Int
cursorMergeWeight (PreparedCursorMerge (leftCursor, rightCursor)) =
  cursorCellCount (cursorMerge leftCursor rightCursor)

arrangementManyBatchCase :: Int -> PreparedArrangementTrace
arrangementManyBatchCase size =
  PreparedArrangementTrace (traceFromBatches batches)
  where
    PreparedBatches batches =
      periodicTraceBatchCase size

arrangementAppendCase :: Int -> PreparedArrangementAppend
arrangementAppendCase size =
  PreparedArrangementAppend
    ( arrangeByKey (traceFromUpdates (preparedUpdates (updateCase size))),
      fromUpdates (preparedUpdates (shiftedUpdateCase size))
    )

arrangementHotKeyAppendCase :: Int -> PreparedArrangementAppend
arrangementHotKeyAppendCase size =
  PreparedArrangementAppend
    ( arrangeByKey (traceFromUpdates (preparedUpdates (skewedKeyUpdateCase size))),
      fromUpdates (preparedUpdates (shiftedSkewedKeyUpdateCase size))
    )

arrangementSliceWeight :: PreparedArrangement -> Int
arrangementSliceWeight (PreparedArrangement arrangement) =
  foldSliceThrough
    traceCutoff
    hotKey
    (\acc _time _value weight -> acc + weight)
    0
    arrangement

arrangementSliceAfterWeight :: PreparedArrangement -> Int
arrangementSliceAfterWeight (PreparedArrangement arrangement) =
  foldSliceAfter
    traceCutoff
    hotKey
    (\acc _time _value weight -> acc + weight)
    0
    arrangement

arrangementAppendWeight :: PreparedArrangementAppend -> Int
arrangementAppendWeight (PreparedArrangementAppend (arrangement, batch)) =
  arrangementCellCount (appendArrangementBatch batch arrangement)

arrangementBuildTraceWeight :: PreparedArrangementTrace -> Int
arrangementBuildTraceWeight (PreparedArrangementTrace traceValue) =
  arrangementCellCount (arrangeByKey traceValue)

arrangementBuildTraceViaBatchWeight :: PreparedArrangementTrace -> Int
arrangementBuildTraceViaBatchWeight (PreparedArrangementTrace traceValue) =
  arrangementCellCount (arrangeByKey (traceFromBatch (snapshotTraceBatch traceValue)))

data PreparedJoinInputs = PreparedJoinInputs
  { preparedJoinLeftBatch :: !BenchBatch,
    preparedJoinRightArrangement :: !BenchArrangement
  }

instance NFData PreparedJoinInputs where
  rnf joinInputs =
    length (batchToUpdates (preparedJoinLeftBatch joinInputs))
      `seq` arrangementCellCount (preparedJoinRightArrangement joinInputs)
      `seq` ()


materializedFoldDeltaJoin ::
  (Ord outKey, Ord outVal) =>
  (String -> Char -> Char -> Maybe (outKey, outVal)) ->
  BenchBatch ->
  Arrangement Int String Char Int ->
  Batch Int outKey outVal Int
materializedFoldDeltaJoin project delta arrangement =
  fromUpdates (foldDeltaJoin project collectJoinUpdate [] delta arrangement)

collectJoinUpdate ::
  [Update time outKey outVal weight] ->
  time ->
  outKey ->
  outVal ->
  weight ->
  [Update time outKey outVal weight]
collectJoinUpdate updates time outKey outVal weight =
  Update
    { updateTime = time,
      updateKey = outKey,
      updateVal = outVal,
      updateWeight = weight
    }
    : updates

deltaJoinMaterializedCount :: PreparedJoinInputs -> Int
deltaJoinMaterializedCount joinInputs =
  length
    ( batchToUpdates
        ( materializedFoldDeltaJoin
            pairProjection
            (preparedJoinLeftBatch joinInputs)
            (preparedJoinRightArrangement joinInputs)
        )
    )

foldDeltaJoinCount :: PreparedJoinInputs -> Int
foldDeltaJoinCount joinInputs =
  foldDeltaJoin
    pairProjection
    (\count _time _key _value _weight -> count + 1)
    0
    (preparedJoinLeftBatch joinInputs)
    (preparedJoinRightArrangement joinInputs)

joinCase :: Int -> PreparedJoinInputs
joinCase size =
  PreparedJoinInputs
    { preparedJoinLeftBatch = fromUpdates (preparedUpdates (updateCase size)),
      preparedJoinRightArrangement = arrangeByKey (traceFromUpdates (preparedUpdates (updateCase (size * 2))))
    }

joinSkewFanoutCase :: Int -> PreparedJoinInputs
joinSkewFanoutCase size =
  PreparedJoinInputs
    { preparedJoinLeftBatch = fromUpdates (preparedUpdates (skewedKeyUpdateCase size)),
      preparedJoinRightArrangement = arrangeByKey (traceFromUpdates (preparedUpdates (skewedKeyUpdateCase (size * 2))))
    }

joinEmptyPrefixCase :: Int -> PreparedJoinInputs
joinEmptyPrefixCase size =
  PreparedJoinInputs
    { preparedJoinLeftBatch = fromUpdates (preparedUpdates (updateCase size)),
      preparedJoinRightArrangement = emptyArrangement
    }

deltaJoinCollapsedOutputCount :: PreparedJoinInputs -> Int
deltaJoinCollapsedOutputCount joinInputs =
  length
    ( batchToUpdates
        ( materializedFoldDeltaJoin
            collapsedProjection
            (preparedJoinLeftBatch joinInputs)
            (preparedJoinRightArrangement joinInputs)
        )
    )

foldDeltaJoinCollapsedOutputCount :: PreparedJoinInputs -> Int
foldDeltaJoinCollapsedOutputCount joinInputs =
  foldDeltaJoin
    collapsedProjection
    (\count _time _key _value _weight -> count + 1)
    0
    (preparedJoinLeftBatch joinInputs)
    (preparedJoinRightArrangement joinInputs)

collapsedProjection :: String -> Char -> Char -> Maybe (String, Char)
collapsedProjection key _leftValue _rightValue =
  Just (key, 'x')
