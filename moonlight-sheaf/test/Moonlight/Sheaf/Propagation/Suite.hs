module Moonlight.Sheaf.Propagation.Suite
  ( tests,
  )
where

import Moonlight.Sheaf.Propagation.ToySpec qualified as PropagationToySpec
import Test.Tasty (TestTree)

tests :: TestTree
tests =
  PropagationToySpec.tests
