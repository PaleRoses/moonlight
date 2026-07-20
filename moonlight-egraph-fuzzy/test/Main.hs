module Main
  ( main,
  )
where

import Moonlight.EGraph.Fuzzy.CoreSpec qualified as CoreSpec
import Moonlight.EGraph.Fuzzy.SimplicialSpec qualified as SimplicialSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-egraph-fuzzy"
        [ CoreSpec.tests,
          SimplicialSpec.tests
        ]
    )
