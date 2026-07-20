module Moonlight.EGraph.Introspection.HsExprSpec
  ( tests,
  )
where

import Moonlight.EGraph.Introspection.HsExprSpec.Equation qualified as Equation
import Moonlight.EGraph.Introspection.HsExprSpec.Global qualified as Global
import Moonlight.EGraph.Introspection.HsExprSpec.Gluing qualified as Gluing
import Moonlight.EGraph.Introspection.HsExprSpec.LawFront qualified as LawFront
import Moonlight.EGraph.Introspection.HsExprSpec.Metrics qualified as Metrics
import Moonlight.EGraph.Introspection.HsExprSpec.Section qualified as Section
import Moonlight.EGraph.Introspection.HsExprSpec.Site qualified as Site
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "hs-expr"
    [ Site.tests,
      Section.tests,
      Gluing.tests,
      Global.tests,
      Equation.tests,
      LawFront.tests,
      Metrics.tests
    ]
