module Main (main) where

import Moonlight.EGraph.Pure.Session.CompileSpec qualified as CompileSpec
import Moonlight.EGraph.Pure.Session.RuntimeSpec qualified as RuntimeSpec
import Test.Tasty
  ( defaultMain,
    testGroup,
  )

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-egraph:session"
        [ CompileSpec.tests
        , RuntimeSpec.tests
        ]
    )
