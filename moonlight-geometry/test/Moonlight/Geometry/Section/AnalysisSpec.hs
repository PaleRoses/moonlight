module Moonlight.Geometry.Section.AnalysisSpec (tests) where

import Moonlight.Geometry.Section.Analysis
import Moonlight.Geometry.Site.Parameters
import Moonlight.Geometry.Site.Primitive
import Moonlight.Geometry.Site.Semantics
import Moonlight.Geometry.Site.Token
import Moonlight.LinAlg.Geometry (Vec3 (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

primitiveAnalysis :: SDFPrimitive -> SDFAnalysis
primitiveAnalysis primitive =
  SDFAnalysis
    { saSupport = maybe UnboundedSupport BoundedSupport (aabbFromPrimitive primitive),
      saDistanceCertificate = attachGlobalLipschitz (Certified (LipschitzUpperBound 1.0)) exactCertificate,
      saLipschitzBound = Certified (LipschitzUpperBound 1.0),
      saNodeCount = 1
    }

tests :: TestTree
tests =
  testGroup
    "Analysis"
    [ testCase "empty support is explicit" $ do
        tokenSupport SDFEmpty @?= EmptySupport,
      testCase "unbounded repeat support is explicit" $ do
        let childAnalysis = primitiveAnalysis (Sphere 1.0)
            repeated = tokenSupport (Repeat (RepeatParams (Vec3 1.0 0.0 0.0) Nothing) childAnalysis)
        repeated @?= UnboundedSupport,
      testCase "zero-count repeat is empty" $ do
        let childAnalysis = primitiveAnalysis (Sphere 1.0)
            repeated = tokenSupport (Repeat (RepeatParams (Vec3 1.0 0.0 0.0) (Just 0)) childAnalysis)
        repeated @?= EmptySupport,
      testCase "noise with merely continuous kernel loses Lipschitz bound" $ do
        let childAnalysis = primitiveAnalysis (Sphere 1.0)
            boundValue =
              tokenLipschitz
                ( NoisePerturbation
                    NoiseParams
                      { npKernel = FbmNoise,
                        npFrequency = 4.0,
                        npAmplitude = 0.1,
                        npOctaves = 3,
                        npSeed = 0
                      }
                    childAnalysis
                )
        boundValue @?= Unknown,
      testCase "certificate bounds expose explicit certification states" $ do
        beGlobalLipschitz (dcBounds exactCertificate) @?= Unknown
    ]
