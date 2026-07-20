module Moonlight.Saturation.ProtocolTests
  ( tests,
  )
where

import Moonlight.Saturation.MatchingSpec qualified as MatchingSpec
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "protocol"
    [MatchingSpec.matchingTests]
