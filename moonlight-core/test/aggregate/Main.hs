module Main where

import qualified BasisTests
import qualified NumericTests
import qualified PublicSurfaceSpec
import qualified SolverTests
import qualified SyntaxTests
import Test.Tasty (defaultMain, testGroup)
import qualified TermTests

main :: IO ()
main =
  defaultMain $
    testGroup
      "moonlight-core"
      [ BasisTests.tests,
        NumericTests.tests,
        SyntaxTests.tests,
        SolverTests.tests,
        TermTests.tests,
        PublicSurfaceSpec.tests
      ]
