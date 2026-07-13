module Moonlight.Sheaf.Descent.Suite
  ( tests,
  )
where

import Moonlight.Sheaf.Descent.ContextRegionSpec qualified as ContextRegionSpec
import Moonlight.Sheaf.Descent.ContextSitePowersetSpec qualified as ContextSitePowersetSpec
import Moonlight.Sheaf.Descent.ContextSiteSpec qualified as ContextSiteSpec
import Moonlight.Sheaf.Descent.DescentSpec qualified as DescentSpec
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "descent"
    [ DescentSpec.tests,
      ContextSiteSpec.tests,
      ContextSitePowersetSpec.tests,
      ContextRegionSpec.tests
    ]
