module Main
  ( main,
  )
where

import qualified Moonlight.EGraph.Introspection.FreeScopeSpec
import qualified Moonlight.EGraph.Introspection.HsExprBindingFrontSpec
import Moonlight.EGraph.Introspection.HsExprSpec
import qualified Moonlight.EGraph.Introspection.NerveSpec
import qualified Moonlight.EGraph.Introspection.PruningSpec
import qualified Moonlight.EGraph.Introspection.PropertySpec
import qualified Moonlight.EGraph.Introspection.ContextSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-egraph-introspection"
        [ Moonlight.EGraph.Introspection.FreeScopeSpec.tests
        , Moonlight.EGraph.Introspection.HsExprSpec.tests
        , Moonlight.EGraph.Introspection.HsExprBindingFrontSpec.tests
        , Moonlight.EGraph.Introspection.NerveSpec.tests
        , Moonlight.EGraph.Introspection.PruningSpec.tests
        , Moonlight.EGraph.Introspection.PropertySpec.tests
        , Moonlight.EGraph.Introspection.ContextSpec.tests
        ]
    )
