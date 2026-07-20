module CompiledApplicationConditionRecordUpdate where

import Moonlight.Rewrite.Algebra
  ( CompiledApplicationCondition,
    compiledApplicationConditionExpression,
  )

forgeCompiledApplicationCondition ::
  CompiledApplicationCondition guard f ->
  CompiledApplicationCondition guard f
forgeCompiledApplicationCondition condition =
  condition
    { compiledApplicationConditionExpression =
        compiledApplicationConditionExpression condition
    }
