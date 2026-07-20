module Main
  ( main,
  )
where

import Test.Tasty (defaultMain, testGroup)
import Moonlight.EGraph.Saturation.CohomologicalSpec (cohomologicalTests)
import Moonlight.EGraph.Saturation.GoldenPathSpec (goldenPathTests)
import Moonlight.EGraph.Saturation.ChimeraSpec qualified as ChimeraSpec
import Moonlight.EGraph.Saturation.Atlas.Chimera qualified as AtlasChimera
import Moonlight.EGraph.Saturation.Atlas.Scoped qualified as AtlasScoped
import Moonlight.EGraph.Saturation.Atlas.SDF qualified as AtlasSDF
main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-egraph-saturation"
        [ cohomologicalTests,
          goldenPathTests,
          ChimeraSpec.tests,
          AtlasChimera.tests,
          AtlasScoped.tests,
          AtlasSDF.tests
        ]
    )
