module Stream where

import Control.DeepSeq (NFData (..))
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Common (weightAt)
import Moonlight.Differential.Order.LocallyFinite (integralSamplerGeneric)
import Moonlight.Differential.Stream
import Numeric.Natural (Natural)

data PreparedNaturalStream = PreparedNaturalStream
  { preparedNaturalStreamLimit :: !Int,
    preparedNaturalStreamSamples :: !(IntMap.IntMap Int)
  }

instance NFData PreparedNaturalStream where
  rnf preparedStream =
    preparedNaturalStreamLimit preparedStream
      `seq` IntMap.size (preparedNaturalStreamSamples preparedStream)
      `seq` ()

data PreparedProductStream = PreparedProductStream
  { preparedProductStreamSide :: !Int,
    preparedProductStreamSamples :: !(Map.Map (Natural, Natural) Int)
  }

instance NFData PreparedProductStream where
  rnf preparedStream =
    preparedProductStreamSide preparedStream
      `seq` Map.size (preparedProductStreamSamples preparedStream)
      `seq` ()

streamNaturalSizes :: [Int]
streamNaturalSizes =
  [512, 2048]

streamProductSizes :: [Int]
streamProductSizes =
  [4, 8, 16, 32]

naturalStreamCase :: Int -> PreparedNaturalStream
naturalStreamCase size =
  PreparedNaturalStream
    { preparedNaturalStreamLimit = size,
      preparedNaturalStreamSamples =
        IntMap.fromAscList (fmap (\index -> (index, weightAt index)) [0 .. size - 1])
    }

productStreamCase :: Int -> PreparedProductStream
productStreamCase side =
  PreparedProductStream
    { preparedProductStreamSide = side,
      preparedProductStreamSamples =
        Map.fromAscList
          [ ((fromIntegral left, fromIntegral right), weightAt (left * side + right))
          | left <- [0 .. side - 1],
            right <- [0 .. side - 1]
          ]
    }

naturalStreamValue :: PreparedNaturalStream -> Stream Natural Int
naturalStreamValue preparedStream =
  stream
    ( \time ->
        IntMap.findWithDefault
          0
          (fromIntegral time)
          (preparedNaturalStreamSamples preparedStream)
    )

productStreamValue :: PreparedProductStream -> Stream (Natural, Natural) Int
productStreamValue preparedStream =
  stream
    ( \time ->
        Map.findWithDefault 0 time (preparedProductStreamSamples preparedStream)
    )

streamDifferentiateWeight :: PreparedNaturalStream -> Int
streamDifferentiateWeight preparedStream =
  Foldable.sum
    (differentiateNaturalPrefix (preparedNaturalStreamLimit preparedStream) (naturalStreamValue preparedStream))

streamIntegrateWeight :: PreparedNaturalStream -> Int
streamIntegrateWeight preparedStream =
  Foldable.sum
    (integrateNaturalPrefix (preparedNaturalStreamLimit preparedStream) (naturalStreamValue preparedStream))

streamIncrementalizeMapWeight :: PreparedNaturalStream -> Int
streamIncrementalizeMapWeight preparedStream =
  Foldable.sum
    (incrementalizeScalarLinearNaturalPrefix (ScaleByInteger 2) (preparedNaturalStreamLimit preparedStream) (naturalStreamValue preparedStream))

streamIncrementalizeGenericMapWeight :: PreparedNaturalStream -> Int
streamIncrementalizeGenericMapWeight preparedStream =
  Foldable.sum
    (incrementalizeNaturalPrefix (mapStream (* 2)) (preparedNaturalStreamLimit preparedStream) (naturalStreamValue preparedStream))

productStreamDifferentiateIntegrateWeight :: PreparedProductStream -> Int
productStreamDifferentiateIntegrateWeight preparedStream =
  Foldable.sum
    (foldMap id (integrateNaturalProductRows (differentiateNaturalProductPrefix (preparedProductStreamSide preparedStream) (productStreamValue preparedStream))))

streamCalculusIntegrateWeight :: PreparedNaturalStream -> Int
streamCalculusIntegrateWeight preparedStream =
  Foldable.sum
    (streamNaturalPrefix (preparedNaturalStreamLimit preparedStream) (integrate (naturalStreamValue preparedStream)))

streamCalculusIntegrateFallbackWeight :: PreparedNaturalStream -> Int
streamCalculusIntegrateFallbackWeight preparedStream =
  Foldable.sum
    (streamNaturalPrefix (preparedNaturalStreamLimit preparedStream) (stream (integralSamplerGeneric (streamAt (naturalStreamValue preparedStream)))))

streamCalculusIncrementalizeMapWeight :: PreparedNaturalStream -> Int
streamCalculusIncrementalizeMapWeight preparedStream =
  Foldable.sum
    (streamNaturalPrefix (preparedNaturalStreamLimit preparedStream) (incrementalize (mapStream (* 2)) (naturalStreamValue preparedStream)))

streamCalculusIncrementalizeMapFallbackWeight :: PreparedNaturalStream -> Int
streamCalculusIncrementalizeMapFallbackWeight preparedStream =
  Foldable.sum
    (streamNaturalPrefix (preparedNaturalStreamLimit preparedStream) (differentiate (mapStream (* 2) (stream (integralSamplerGeneric (streamAt (naturalStreamValue preparedStream)))))))

productStreamCalculusWeight :: PreparedProductStream -> Int
productStreamCalculusWeight preparedStream =
  Foldable.sum
    (foldMap id (streamNaturalProductPrefix (preparedProductStreamSide preparedStream) (integrate (differentiate (productStreamValue preparedStream)))))

productStreamCalculusFallbackWeight :: PreparedProductStream -> Int
productStreamCalculusFallbackWeight preparedStream =
  Foldable.sum
    (foldMap id (streamNaturalProductPrefix (preparedProductStreamSide preparedStream) (stream (integralSamplerGeneric (streamAt (differentiate (productStreamValue preparedStream)))))))
