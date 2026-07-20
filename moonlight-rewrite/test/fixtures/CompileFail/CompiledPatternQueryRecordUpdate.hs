module CompiledPatternQueryRecordUpdate where

import Moonlight.Rewrite.Algebra (CompiledPatternQuery, cpqQuery)

forgeCompiledPatternQuery :: CompiledPatternQuery guard f -> CompiledPatternQuery guard f
forgeCompiledPatternQuery queryValue =
  queryValue {cpqQuery = cpqQuery queryValue}
