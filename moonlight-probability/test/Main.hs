module Main (main) where

import qualified Moonlight.Probability.AlgebraLawSpec as AlgebraLawSpec
import qualified Moonlight.Probability.CategoricalSpec as CategoricalSpec
import qualified Moonlight.Probability.CoreSpec as CoreSpec
import qualified Moonlight.Probability.DenseSimplexSpec as DenseSimplexSpec
import qualified Moonlight.Probability.DistributionSpec as DistributionSpec
import qualified Moonlight.Probability.EntropySpec as EntropySpec
import qualified Moonlight.Probability.FiniteSpec as FiniteSpec
import qualified Moonlight.Probability.SampleSpec as SampleSpec
import qualified Moonlight.Probability.TransformSpec as TransformSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-probability"
        [ CoreSpec.tests,
          DistributionSpec.tests,
          DenseSimplexSpec.tests,
          AlgebraLawSpec.tests,
          FiniteSpec.tests,
          TransformSpec.tests,
          CategoricalSpec.tests,
          EntropySpec.tests,
          SampleSpec.tests
        ]
    )
