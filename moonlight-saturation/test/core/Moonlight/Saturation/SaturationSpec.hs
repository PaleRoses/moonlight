module Moonlight.Saturation.SaturationSpec
  ( tests,
  )
where

import Moonlight.Saturation.Property (saturationProperties)
import Test.Tasty (TestTree)

tests :: TestTree
tests = saturationProperties
