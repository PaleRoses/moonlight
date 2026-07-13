module Main where

import Test.Tasty (defaultMain, testGroup)
import qualified Moonlight.Geometry.Gluing.LawsSpec as LawsSpec
import qualified Moonlight.Geometry.Gluing.RewriteSpec as RewriteSpec
import qualified Moonlight.Geometry.Gluing.SafetySpec as SafetySpec
import qualified Moonlight.Geometry.Section.AnalysisSpec as AnalysisSpec
import qualified Moonlight.Geometry.Site.PrimitiveSpec as PrimitiveSpec
import qualified Moonlight.Geometry.Site.TokenSpec as TokenSpec

main :: IO ()
main =
  defaultMain $
    testGroup
      "moonlight-geometry"
      [ PrimitiveSpec.tests,
        TokenSpec.tests,
        AnalysisSpec.tests,
        RewriteSpec.tests,
        SafetySpec.tests,
        LawsSpec.tests
      ]
