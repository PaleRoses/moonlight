module EngineSpecRecordUpdate where

import Moonlight.Control.Engine.Spec
  ( EngineSpec,
    Validated,
  )

bad ::
  EngineSpec Validated ->
  EngineSpec Validated
bad spec =
  spec {validatedMaxRounds = undefined}
