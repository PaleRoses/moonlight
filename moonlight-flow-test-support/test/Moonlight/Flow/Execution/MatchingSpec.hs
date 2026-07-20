module Moonlight.Flow.Execution.MatchingSpec
  ( tests,
  )
where

import Test.Moonlight.Flow.Property.Execution (executionProperties)
import Test.Tasty (TestTree)

tests :: TestTree
tests = executionProperties
