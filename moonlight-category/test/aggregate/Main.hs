module Main (main) where

import qualified Moonlight.Category.Effect.Laws as Laws
import Moonlight.Pale.Test.Runner (runTestTreeGroup)
import qualified AbstractTests
import qualified FacadeTests
import qualified FiniteTests
import qualified IndexedTests
import qualified SimplicialTests
import qualified SiteTests

main :: IO ()
main =
  runTestTreeGroup
    "moonlight-category"
    [ Laws.tests,
      AbstractTests.tests,
      FiniteTests.tests,
      SiteTests.tests,
      IndexedTests.tests,
      SimplicialTests.tests,
      FacadeTests.tests
    ]
