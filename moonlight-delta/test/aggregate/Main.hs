module Main (main) where

import qualified CoreTests
import qualified CrossCarrierLaws
import qualified EpochTests
import qualified PatchTests
import qualified RepairTests
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "moonlight-delta"
      [ CoreTests.tests,
        PatchTests.tests,
        EpochTests.tests,
        RepairTests.tests,
        CrossCarrierLaws.tests
      ]
