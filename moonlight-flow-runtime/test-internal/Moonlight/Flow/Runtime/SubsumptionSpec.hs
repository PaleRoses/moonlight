module Moonlight.Flow.Runtime.SubsumptionSpec
  ( tests,
  )
where

import Test.Moonlight.Flow.Property.Subsumption (subsumptionProperties)
import Test.Tasty (TestTree)

tests :: TestTree
tests = subsumptionProperties
