module CompiledPatternExtensionRecordUpdate where

import Moonlight.Rewrite.Algebra (CompiledPatternExtension, cpePath)

forgeCompiledPatternExtension :: CompiledPatternExtension guard f -> CompiledPatternExtension guard f
forgeCompiledPatternExtension extension =
  extension {cpePath = cpePath extension}
