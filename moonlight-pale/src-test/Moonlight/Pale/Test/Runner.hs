module Moonlight.Pale.Test.Runner
  ( runTestTree,
    runTestTreeGroup,
  )
where

import Test.Tasty (TestTree, defaultMain, testGroup)

runTestTree :: TestTree -> IO ()
runTestTree =
  defaultMain

runTestTreeGroup :: String -> [TestTree] -> IO ()
runTestTreeGroup groupLabel =
  runTestTree . testGroup groupLabel
