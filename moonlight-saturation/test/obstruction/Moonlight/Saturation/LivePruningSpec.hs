module Moonlight.Saturation.LivePruningSpec
  ( livePruningTests,
  )
where

import Moonlight.Saturation.Obstruction.Cohomological.LivePruning
import Moonlight.Saturation.Test.ObstructionFixture
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertFailure, testCase)

livePruningTests :: TestTree
livePruningTests =
  testGroup
    "live pruning"
    [ testCase "dirty support refresh descends per request and glues the expected state" $
        let fixture = liveRefreshFixture 4 16
         in either
              (assertFailure . show)
              (@?= liveRefreshExpectedState fixture)
              ( refreshLivePruningState
                  (livePruningAdapter (liveRefreshUpdatedRequests fixture))
                  (liveRefreshDelta fixture)
                  ()
                  (liveRefreshRequests fixture)
                  (liveRefreshPriorState fixture)
              )
    ]
