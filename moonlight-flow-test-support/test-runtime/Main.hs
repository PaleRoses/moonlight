module Main (main) where

import Moonlight.Flow.Runtime.IndexedJoinSpec qualified as IndexedJoinSpec
import Moonlight.Flow.Runtime.RuntimeAuthorityBoundarySpec qualified as RuntimeAuthorityBoundarySpec
import Moonlight.Flow.Runtime.RbacDataflowFixtureSpec qualified as RbacDataflowFixtureSpec
import Moonlight.Flow.Runtime.RbacEntitlementSpec qualified as RbacEntitlementSpec
import Moonlight.Flow.Runtime.RbacIncrementalStatsSpec qualified as RbacIncrementalStatsSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-flow-runtime"
        [ IndexedJoinSpec.tests,
          RbacEntitlementSpec.tests,
          RbacIncrementalStatsSpec.tests,
          RbacDataflowFixtureSpec.tests,
          RuntimeAuthorityBoundarySpec.tests
        ]
    )
