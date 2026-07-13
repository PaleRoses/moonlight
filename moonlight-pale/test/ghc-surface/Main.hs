module Main
  ( main,
  )
where

import Moonlight.Pale.Ghc.Expr.RenderRoundTripSpec qualified as RenderRoundTripSpec
import Moonlight.Pale.Ghc.Hie.OracleSpec qualified as OracleSpec
import Moonlight.Pale.Ghc.ModuleSurfaceSpec qualified as ModuleSurfaceSpec
import Moonlight.Pale.Ghc.Hie.TypeWordsSpec qualified as TypeWordsSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
      defaultMain
    ( testGroup
        "pale-ghc-surface"
        [ OracleSpec.tests,
          TypeWordsSpec.tests,
          ModuleSurfaceSpec.tests,
          RenderRoundTripSpec.tests
        ]
    )
