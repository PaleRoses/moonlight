module Main (main) where

import Moonlight.Sheaf.Surface.PresentationSpec qualified as PresentationSpec
import Moonlight.Sheaf.Surface.Suite qualified as SurfaceSuite
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-sheaf-public-suite"
        [ SurfaceSuite.tests,
          PresentationSpec.tests
        ]
    )
