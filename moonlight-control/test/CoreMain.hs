module Main (main) where

import Moonlight.Control.CompileFailSpec qualified as CompileFailSpec
import Moonlight.Control.Engine.ParallelSpec qualified as ParallelSpec
import Moonlight.Control.EngineRunSpec qualified as EngineRunSpec
import Moonlight.Control.GateSpec qualified as GateSpec
import Moonlight.Control.LawSpec qualified as LawSpec
import Moonlight.Control.MachineSpec qualified as MachineSpec
import Moonlight.Control.ProgramSpec qualified as ProgramSpec
import Moonlight.Control.ScheduleSpec qualified as ScheduleSpec
import Moonlight.Control.StarForgeSpec qualified as StarForgeSpec
import Moonlight.Control.SymbolicSpec qualified as SymbolicSpec
import Moonlight.Control.WeightCountSpec qualified as WeightCountSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-control-core"
        [ LawSpec.tests,
          ProgramSpec.tests,
          MachineSpec.tests,
          GateSpec.tests,
          ScheduleSpec.tests,
          WeightCountSpec.tests,
          EngineRunSpec.tests,
          ParallelSpec.tests,
          StarForgeSpec.tests,
          SymbolicSpec.tests,
          CompileFailSpec.tests
        ]
    )
