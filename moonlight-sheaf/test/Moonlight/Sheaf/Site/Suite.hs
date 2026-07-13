module Moonlight.Sheaf.Site.Suite
  ( tests,
  )
where

import Moonlight.Sheaf.Site.AdversarialSpec qualified as SiteAdversarialSpec
import Moonlight.Sheaf.Site.Analysis.Microsupport.SheafSubstrateSpec qualified as SheafSubstrateSpec
import Moonlight.Sheaf.Site.Analysis.MicrosupportSpec qualified as SiteMicrosupportSpec
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "site"
    [ SiteMicrosupportSpec.tests,
      SiteAdversarialSpec.siteAdversarialTests,
      SheafSubstrateSpec.tests
    ]
