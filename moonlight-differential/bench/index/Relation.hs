module Relation where

import Control.DeepSeq (NFData (..))
import Moonlight.Differential.Batch
  ( Batch,
    batchRowCount,
    fromUpdates,
    fromUpdatesDense,
    singletonBatch,
  )
import Common
  ( arrangementCellCount,
    eitherShow,
    negateUpdateWeight,
    rowChangesWeight,
    weightAt,
  )
import RowIndex
  ( benchIndexedRowFormat,
    indexedLayoutColumns,
  )
import RowProjection
  ( rowProjectionCell,
    rowProjectionUpdateAt,
  )
import Moonlight.Differential.Index.IndexedRows
  ( indexedRowsSize,
  )
import Moonlight.Differential.Relation qualified as Relation
import Moonlight.Differential.Trace
  ( Trace,
    traceFromBatches,
    traceSpine,
    traceSpineCompactedLayerCount,
    traceSpinePhysicalBatchCount,
    traceSpinePhysicalRowCount,
    traceSpinePhysicalVirtualWeight,
    traceSpineRecentBatchCount,
  )
import Moonlight.Differential.Update
  ( Update (..),
  )

type BenchRelationPlan =
  Relation.RelationPlan Int Int Int Int Int (Int, Int) Int

type BenchRelationState =
  Relation.RelationState Int Int Int Int Int (Int, Int) Int

data PreparedRelationBootstrap = PreparedRelationBootstrap
  { preparedRelationBootstrapPlan :: !BenchRelationPlan,
    preparedRelationBootstrapTrace :: !(Trace Int Int Int Int)
  }

instance NFData PreparedRelationBootstrap where
  rnf preparedCase =
    preparedRelationBootstrapPlan preparedCase
      `seq` traceSpinePhysicalVirtualWeight (traceSpine (preparedRelationBootstrapTrace preparedCase))
      `seq` ()

data PreparedRelationAdvance = PreparedRelationAdvance
  { preparedRelationAdvancePlan :: !BenchRelationPlan,
    preparedRelationAdvanceState :: !BenchRelationState,
    preparedRelationAdvanceBatch :: !(Batch Int Int Int Int)
  }

instance NFData PreparedRelationAdvance where
  rnf preparedCase =
    preparedRelationAdvancePlan preparedCase
      `seq` relationStateWeight (preparedRelationAdvanceState preparedCase)
      `seq` batchRowCount (preparedRelationAdvanceBatch preparedCase)
      `seq` ()

relationBootstrapCase :: Int -> PreparedRelationBootstrap
relationBootstrapCase size =
  PreparedRelationBootstrap
    { preparedRelationBootstrapPlan = rowProjectionRelationPlan,
      preparedRelationBootstrapTrace = traceFromBatches (fmap singletonBatch updates)
    }
  where
    updates =
      rowProjectionUpdateAt <$> [0 .. size - 1]

relationUniformCase :: Int -> Either String PreparedRelationAdvance
relationUniformCase size =
  relationAdvanceCase rowProjectionRelationPlan initialUpdates deltaUpdates
  where
    initialUpdates =
      rowProjectionUpdateAt <$> [0 .. size - 1]

    deltaUpdates =
      rowProjectionUpdateAt . (+ size) <$> [0 .. size - 1]

relationHotKeyCase :: Int -> Either String PreparedRelationAdvance
relationHotKeyCase size =
  relationAdvanceCase hotRelationPlan initialUpdates deltaUpdates
  where
    initialUpdates =
      relationHotUpdateAt <$> [0 .. size - 1]

    deltaUpdates =
      relationHotUpdateAt . (+ size) <$> [0 .. size - 1]

relationRetractionCase :: Int -> Either String PreparedRelationAdvance
relationRetractionCase size =
  relationAdvanceCase rowProjectionRelationPlan initialUpdates (negateUpdateWeight <$> initialUpdates)
  where
    initialUpdates =
      rowProjectionUpdateAt <$> [0 .. size - 1]

relationAdvanceCase ::
  BenchRelationPlan ->
  [Update Int Int Int Int] ->
  [Update Int Int Int Int] ->
  Either String PreparedRelationAdvance
relationAdvanceCase plan initialUpdates deltaUpdates = do
  state <-
    eitherShow (Relation.bootstrapRelation plan (traceFromBatches (fmap singletonBatch initialUpdates)))
  Right
    PreparedRelationAdvance
      { preparedRelationAdvancePlan = plan,
        preparedRelationAdvanceState = state,
        preparedRelationAdvanceBatch = fromUpdates deltaUpdates
      }

relationUniformCaseDense :: Int -> Either String PreparedRelationAdvance
relationUniformCaseDense size =
  relationAdvanceCaseDense rowProjectionRelationPlan initialUpdates deltaUpdates
  where
    initialUpdates =
      rowProjectionUpdateAt <$> [0 .. size - 1]

    deltaUpdates =
      rowProjectionUpdateAt . (+ size) <$> [0 .. size - 1]

relationHotKeyCaseDense :: Int -> Either String PreparedRelationAdvance
relationHotKeyCaseDense size =
  relationAdvanceCaseDense hotRelationPlan initialUpdates deltaUpdates
  where
    initialUpdates =
      relationHotUpdateAt <$> [0 .. size - 1]

    deltaUpdates =
      relationHotUpdateAt . (+ size) <$> [0 .. size - 1]

relationRetractionCaseDense :: Int -> Either String PreparedRelationAdvance
relationRetractionCaseDense size =
  relationAdvanceCaseDense rowProjectionRelationPlan initialUpdates (negateUpdateWeight <$> initialUpdates)
  where
    initialUpdates =
      rowProjectionUpdateAt <$> [0 .. size - 1]

relationAdvanceCaseDense ::
  BenchRelationPlan ->
  [Update Int Int Int Int] ->
  [Update Int Int Int Int] ->
  Either String PreparedRelationAdvance
relationAdvanceCaseDense plan initialUpdates deltaUpdates = do
  state <-
    eitherShow (Relation.bootstrapRelation plan (traceFromBatches (fmap singletonBatch initialUpdates)))
  Right
    PreparedRelationAdvance
      { preparedRelationAdvancePlan = plan,
        preparedRelationAdvanceState = state,
        preparedRelationAdvanceBatch = fromUpdatesDense deltaUpdates
      }

rowProjectionRelationPlan :: BenchRelationPlan
rowProjectionRelationPlan =
  Relation.RelationPlan
    { Relation.relationIndexedFormat = benchIndexedRowFormat,
      Relation.relationLayoutColumnIndex = indexedLayoutColumns,
      Relation.relationLayout = 2,
      Relation.relationProjectCell = rowProjectionCell
    }

hotRelationPlan :: BenchRelationPlan
hotRelationPlan =
  Relation.RelationPlan
    { Relation.relationIndexedFormat = benchIndexedRowFormat,
      Relation.relationLayoutColumnIndex = indexedLayoutColumns,
      Relation.relationLayout = 2,
      Relation.relationProjectCell = relationHotProjectionCell
    }

relationHotProjectionCell :: Int -> Int -> Int -> Int -> Maybe ((Int, Int), Int)
relationHotProjectionCell _time key value weight =
  Just ((key, value), weight)

relationHotUpdateAt :: Int -> Update Int Int Int Int
relationHotUpdateAt index =
  Update
    { updateTime = index,
      updateKey = 0,
      updateVal = index `mod` 17,
      updateWeight = weightAt index
    }

relationBootstrapWeight :: PreparedRelationBootstrap -> Either String Int
relationBootstrapWeight preparedCase =
  relationStateWeight
    <$> eitherShow
      ( Relation.bootstrapRelation
          (preparedRelationBootstrapPlan preparedCase)
          (preparedRelationBootstrapTrace preparedCase)
      )

relationAdvanceWeight :: PreparedRelationAdvance -> Either String Int
relationAdvanceWeight preparedCase =
  relationAdvanceResultWeight
    <$> eitherShow
      ( Relation.advanceRelation
          (preparedRelationAdvancePlan preparedCase)
          (preparedRelationAdvanceBatch preparedCase)
          (preparedRelationAdvanceState preparedCase)
      )

relationAdvanceResultWeight ::
  Relation.RelationAdvance
    (Batch Int Int Int Int)
    (Relation.RelationChanges (Int, Int) Int)
    BenchRelationState ->
  Int
relationAdvanceResultWeight advance =
  relationStateWeight (Relation.relationNextState advance)
    + relationChangesWeight (Relation.relationChanges advance)

relationStateWeight :: BenchRelationState -> Int
relationStateWeight state =
  traceIntProfileWeight (Relation.relationTrace state)
    + arrangementCellCount (Relation.relationByKey views)
    + indexedRowsSize (Relation.relationRows views)
  where
    views =
      Relation.relationViews state

relationChangesWeight :: Relation.RelationChanges (Int, Int) Int -> Int
relationChangesWeight changes =
  rowChangesWeight (Relation.relationRowChanges changes)

traceIntProfileWeight :: Trace Int Int Int Int -> Int
traceIntProfileWeight traceValue =
  let spine =
        traceSpine traceValue
   in traceSpinePhysicalBatchCount spine
        + traceSpinePhysicalRowCount spine
        + fromIntegral (traceSpinePhysicalVirtualWeight spine)
        + traceSpineCompactedLayerCount spine
        + traceSpineRecentBatchCount spine
