module Moonlight.Flow.Storage.StorageSpec
  ( tests,
  )
where

import Test.Moonlight.Flow.Property.Storage (storageProperties)
import Test.Tasty (TestTree)

tests :: TestTree
tests = storageProperties
