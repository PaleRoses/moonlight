module Moonlight.EGraph.Introspection.PropertySpec
  ( tests,
  )
where

import Moonlight.EGraph.Introspection.PropertySpec.CommonPrelude
import qualified Moonlight.EGraph.Introspection.PropertySpec.Gluing as Gluing
import qualified Moonlight.EGraph.Introspection.PropertySpec.Global as Global
import qualified Moonlight.EGraph.Introspection.PropertySpec.Section as Section
import qualified Moonlight.EGraph.Introspection.PropertySpec.Site as Site

tests :: TestTree
tests =
  localOption (QuickCheckMaxSize 4)
    $ testGroup
      "property-laws"
      [ Site.tests,
        Section.tests,
        Gluing.tests,
        Global.tests
      ]
