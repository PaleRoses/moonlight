module Moonlight.Flow.Carrier.CarrierSpec
  ( tests,
  )
where

import Test.Moonlight.Flow.Property.Carrier (carrierProperties)
import Test.Tasty (TestTree)

tests :: TestTree
tests = carrierProperties
