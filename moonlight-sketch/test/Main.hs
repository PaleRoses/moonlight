module Main (main) where

import qualified Moonlight.Sketch.Effect.Laws as Laws
import qualified Moonlight.Sketch.FormatSpec as FormatSpec
import qualified Moonlight.Sketch.HashSpec as HashSpec
import qualified Moonlight.Sketch.InstancesSpec as InstancesSpec
import qualified Moonlight.Sketch.NormalizeSpec as NormalizeSpec
import qualified Moonlight.Sketch.ResolveSpec as ResolveSpec
import qualified Moonlight.Sketch.SubtypeSpec as SubtypeSpec
import qualified Moonlight.Sketch.ValidateSpec as ValidateSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-sketch"
        [ Laws.tests,
          NormalizeSpec.tests,
          HashSpec.tests,
          SubtypeSpec.tests,
          ResolveSpec.tests,
          FormatSpec.tests,
          ValidateSpec.tests,
          InstancesSpec.tests
        ]
    )
