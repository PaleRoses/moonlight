module Main (main) where

import qualified SimplicialTests
import Moonlight.Pale.Test.Runner (runTestTree)

main :: IO ()
main =
  runTestTree SimplicialTests.tests
