module Test.Moonlight.Flow.Oracle.Storage
  ( OracleIndexedRows (..),
    emptyOracleIndexedRows,
    oracleApplyIndexedRowsOp,
    oracleValueBuckets,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    tupleKeyIndexInt,
    tupleKeyWidth,
  )
import Test.Moonlight.Flow.Gen.Storage
  ( IndexedRowsOp (..),
  )

data OracleIndexedRows = OracleIndexedRows
  { oirWidth :: !Int,
    oirNextRowId :: !Int,
    oirIdByRow :: !(Map RowTupleKey Int),
    oirRowById :: !(IntMap RowTupleKey),
    oirPayloadByRow :: !(Map RowTupleKey Int)
  }
  deriving stock (Eq, Show)

emptyOracleIndexedRows :: Int -> OracleIndexedRows
emptyOracleIndexedRows width =
  OracleIndexedRows
    { oirWidth = width,
      oirNextRowId = 0,
      oirIdByRow = Map.empty,
      oirRowById = IntMap.empty,
      oirPayloadByRow = Map.empty
    }

oracleApplyIndexedRowsOp :: IndexedRowsOp -> OracleIndexedRows -> OracleIndexedRows
oracleApplyIndexedRowsOp op rows =
  case op of
    InsertRow rowValue payload ->
      if tupleKeyWidth rowValue == oirWidth rows && Map.notMember rowValue (oirIdByRow rows)
        then
          let rowId = oirNextRowId rows
           in rows
                { oirNextRowId = rowId + 1,
                  oirIdByRow = Map.insert rowValue rowId (oirIdByRow rows),
                  oirRowById = IntMap.insert rowId rowValue (oirRowById rows),
                  oirPayloadByRow = Map.insert rowValue payload (oirPayloadByRow rows)
                }
        else rows
    DeleteRow rowValue ->
      case Map.lookup rowValue (oirIdByRow rows) of
        Nothing -> rows
        Just rowId ->
          rows
            { oirIdByRow = Map.delete rowValue (oirIdByRow rows),
              oirRowById = IntMap.delete rowId (oirRowById rows),
              oirPayloadByRow = Map.delete rowValue (oirPayloadByRow rows)
            }
    SetPayload rowValue payload ->
      if Map.member rowValue (oirIdByRow rows)
        then rows {oirPayloadByRow = Map.insert rowValue payload (oirPayloadByRow rows)}
        else rows

oracleValueBuckets :: OracleIndexedRows -> IntMap (IntMap IntSet)
oracleValueBuckets rows =
  IntMap.foldlWithKey' insertRow IntMap.empty (oirRowById rows)
  where
    insertRow acc rowId rowValue =
      foldr (insertSlot rowId rowValue) acc [0 .. oirWidth rows - 1]

    insertSlot :: Int -> RowTupleKey -> Int -> IntMap (IntMap IntSet) -> IntMap (IntMap IntSet)
    insertSlot rowId rowValue slotIx acc =
      case tupleKeyIndexInt rowValue slotIx of
        Nothing -> acc
        Just repKey ->
          IntMap.insertWith
            (IntMap.unionWith IntSet.union)
            slotIx
            (IntMap.singleton repKey (IntSet.singleton rowId))
            acc
