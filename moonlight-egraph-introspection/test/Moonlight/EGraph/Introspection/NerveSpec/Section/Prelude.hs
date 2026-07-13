{-# LANGUAGE DuplicateRecordFields #-}

module Moonlight.EGraph.Introspection.NerveSpec.Section.Prelude
  ( module Moonlight.EGraph.Introspection.NerveSpec.CommonPrelude,
    module Moonlight.Control.Gate,
    saturateWithSchedulerRefinement,
    saturateByStrategyWithSchedulerRefinement,
    module Moonlight.Control.Class,
    module Moonlight.Control.Machine,
    Program,
    ProgramAlgebra (..),
    foldProgram,
    module Moonlight.Control.Trace,
    module Moonlight.Homology,
    module Moonlight.Category.Simplicial,
  )
where

import Moonlight.EGraph.Introspection.NerveSpec.CommonPrelude
import Moonlight.Control.Gate
import Moonlight.Control.Class
import Moonlight.Control.Machine
import Moonlight.Control.Program
  ( Program,
    ProgramAlgebra (..),
    foldProgram,
  )
import Moonlight.Control.Trace
import Moonlight.EGraph.Test.Saturation
  ( saturateByStrategyWithSchedulerRefinement,
    saturateWithSchedulerRefinement,
  )
import Moonlight.Homology hiding (Dimension, cellDimension)
import Moonlight.Category.Simplicial
