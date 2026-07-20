module Moonlight.Flow.Plan.PlanSpec
  ( tests,
  )
where

import Test.Moonlight.Flow.Property.Plan (planProperties)
import Test.Tasty (TestTree)

tests :: TestTree
tests = planProperties
