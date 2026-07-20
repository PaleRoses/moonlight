module Main (main) where

import Moonlight.Flow.Carrier.Reuse.RegistryCanonicalizationSpec qualified as RegistryCanonicalizationSpec
import Test.Tasty
  ( defaultMain,
    testGroup,
  )

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-flow-carrier-reuse"
        [ RegistryCanonicalizationSpec.spec
        ]
    )
