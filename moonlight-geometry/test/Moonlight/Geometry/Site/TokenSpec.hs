module Moonlight.Geometry.Site.TokenSpec (tests) where

import Moonlight.Core (HasConstructorTag (constructorTag))
import Moonlight.Geometry.Site.Parameters
import Moonlight.Geometry.Site.Token
import Moonlight.LinAlg.Geometry (Vec3 (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

noiseParams :: NoiseParams
noiseParams =
  NoiseParams
    { npKernel = GradientNoise,
      npFrequency = 2.0,
      npAmplitude = 0.25,
      npOctaves = 3,
      npSeed = 7
    }

tests :: TestTree
tests =
  testGroup
    "Token"
    [ testCase "constructor tags classify nodes" $ do
        constructorTag (HardUnion (0 :: Int) (1 :: Int)) @?= TagHardUnion
        constructorTag (NoisePerturbation noiseParams (0 :: Int)) @?= TagNoisePerturbation,
      testCase "token ordering key includes kernel payload" $ do
        tokenOrderingKey (NoisePerturbation noiseParams 'x')
          @?= (TagNoisePerturbation, [2.0, 0.25], [fromEnum GradientNoise, 3, 7], ['x']),
      testCase "scale ordering preserves child ordering" $ do
        tokenOrderingKey (Scale (Vec3 1.0 2.0 3.0) (1 :: Int))
          @?= (TagScale, [1.0, 2.0, 3.0], [], [1])
    ]
