module Moonlight.EGraph.Test.Case
  ( HUnitCase (..),
    hunitCases,
    PropertyCase (..),
    propertyCases,
  )
where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, testCase)
import Test.Tasty.QuickCheck (Property, testProperty)

data HUnitCase = HUnitCase String Assertion

data PropertyCase = PropertyCase String Property

hunitCases :: [HUnitCase] -> [TestTree]
hunitCases =
  fmap (\(HUnitCase caseName assertion) -> testCase caseName assertion)

propertyCases :: [PropertyCase] -> [TestTree]
propertyCases =
  fmap (\(PropertyCase propertyName propertyValue) -> testProperty propertyName propertyValue)
