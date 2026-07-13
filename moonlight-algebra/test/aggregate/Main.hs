module Main (main) where

import AbstractTests qualified
import FiniteLatticeTests qualified
import Moonlight.Pale.Test.Runner (runTestTreeGroup)

main :: IO ()
main =
  runTestTreeGroup
    "moonlight-algebra"
    [ AbstractTests.tests,
      FiniteLatticeTests.tests
    ]
