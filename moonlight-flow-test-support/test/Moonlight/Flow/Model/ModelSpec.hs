module Moonlight.Flow.Model.ModelSpec
  ( tests,
  )
where

import Test.Moonlight.Flow.Property.Model (modelProperties)
import Test.Tasty (TestTree)

tests :: TestTree
tests = modelProperties
