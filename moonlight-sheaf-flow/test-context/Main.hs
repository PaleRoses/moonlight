module Main
  ( main,
  )
where

import qualified Moonlight.Sheaf.ContextSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-sheaf-runtime-context"
        [ Moonlight.Sheaf.ContextSpec.tests
        ]
    )
