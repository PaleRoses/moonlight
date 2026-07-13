module Main (main) where

import qualified SiteTests
import Moonlight.Pale.Test.Runner (runTestTree)

main :: IO ()
main =
  runTestTree SiteTests.tests
