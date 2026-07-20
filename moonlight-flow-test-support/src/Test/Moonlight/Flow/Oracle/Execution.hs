module Test.Moonlight.Flow.Oracle.Execution
  ( oracleRows,
  )
where

import Moonlight.Differential.Row.Tuple (RowTupleKey)
import Test.Moonlight.Flow.Execution.RelProgram
  ( RelProgram,
    RelProgramError,
    programOracleRows,
  )

oracleRows :: RelProgram -> Either RelProgramError [RowTupleKey]
oracleRows =
  programOracleRows
