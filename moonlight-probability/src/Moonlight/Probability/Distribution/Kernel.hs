module Moonlight.Probability.Distribution.Kernel
  ( Kernel (..),
    pushforwardSimplex,
  )
where

import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Moonlight.Probability.Core (probValue)
import Moonlight.Probability.Distribution.Simplex (SimplexError, SimplexWeights, mkSimplexWeights, simplexWeightsToMap)
import Prelude

type Kernel :: Type -> Type -> Type
newtype Kernel source target = Kernel
  { runKernel :: source -> SimplexWeights target
  }

pushforwardSimplex :: Ord target => Kernel source target -> SimplexWeights source -> Either SimplexError (SimplexWeights target)
pushforwardSimplex kernel =
  mkSimplexWeights
    . Map.foldlWithKey'
      ( \accumulated sourceKey sourceProb ->
          Map.unionWith
            (+)
            accumulated
            ( Map.map
                ((* probValue sourceProb) . probValue)
                (simplexWeightsToMap (runKernel kernel sourceKey))
            )
      )
      Map.empty
    . simplexWeightsToMap
