module Test.Moonlight.Flow.Oracle.Model
  ( oracleNormalizeRows,
    oracleComposeRows,
    oracleRestrictRows,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Delta.Signed
  ( MultiplicityChange,
    addMultiplicityChange,
    zeroMultiplicityChange
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    RepKey,
    restrictTupleKey,
  )

oracleNormalizeRows :: Map RowTupleKey MultiplicityChange -> Map RowTupleKey MultiplicityChange
oracleNormalizeRows =
  Map.filter (/= zeroMultiplicityChange)

oracleComposeRows :: Map RowTupleKey MultiplicityChange -> Map RowTupleKey MultiplicityChange -> Map RowTupleKey MultiplicityChange
oracleComposeRows leftRows rightRows =
  oracleNormalizeRows (Map.unionWith addMultiplicityChange leftRows rightRows)

oracleRestrictRows :: IntMap RepKey -> Map RowTupleKey MultiplicityChange -> Map RowTupleKey MultiplicityChange
oracleRestrictRows restriction =
  oracleNormalizeRows
    . Map.fromListWith addMultiplicityChange
    . fmap (\(rowValue, multiplicity) -> (restrictTupleKey restriction rowValue, multiplicity))
    . Map.toAscList
