module EngineSpecValidatedConstructor where

import Moonlight.Control.Engine.Spec
  ( EngineSpec,
    Validated,
  )

bad :: EngineSpec Validated
bad =
  EngineSpec {}
