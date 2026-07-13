module Main (main) where

import Moonlight.EGraph.Test.Suite (egraphSuite)
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain egraphSuite
