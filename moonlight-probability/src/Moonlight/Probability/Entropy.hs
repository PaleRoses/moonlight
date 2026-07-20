module Moonlight.Probability.Entropy
  ( Entropy,
    entropyValue,
    EntropyError (..),
    Divergence,
    divergenceValue,
    DivergenceError (..),
    shannonEntropy,
    shannonEntropyFromWeights,
    renyiEntropy,
    klDivergence,
    jsDivergence,
  )
where

import Data.Kind (Type)
import Data.Foldable (toList)
import Data.Monoid (Sum (..))
import Data.Map.Strict qualified as Map
import Moonlight.Probability.Core (positiveProbValue, probValue)
import Moonlight.Probability.Distribution.Categorical
  ( Categorical,
    CategoricalError,
    categoricalEntropyValue,
    categoricalFoldMap,
    categoricalLookup,
    mkCategorical,
  )
import Prelude

type Entropy :: Type
newtype Entropy = Entropy
  { entropyValue :: Double
  }
  deriving stock (Eq, Ord, Show)

type Divergence :: Type
newtype Divergence = Divergence
  { divergenceValue :: Double
  }
  deriving stock (Eq, Ord, Show)

type EntropyError :: Type
data EntropyError
  = InvalidEntropyWeights CategoricalError
  | InvalidRenyiOrder Double
  deriving stock (Eq, Show)

type DivergenceError :: Type
data DivergenceError
  = DivergenceSupportMismatch
  deriving stock (Eq, Show)

shannonEntropy :: Categorical a -> Entropy
shannonEntropy = Entropy . categoricalEntropyValue

shannonEntropyFromWeights :: Foldable f => f Double -> Either EntropyError Entropy
shannonEntropyFromWeights weights =
  fmap shannonEntropy
    ( first InvalidEntropyWeights
        ( mkCategorical
            (Map.fromList (zip [0 :: Int ..] (toList weights)))
        )
    )

renyiEntropy :: Double -> Categorical a -> Either EntropyError Entropy
renyiEntropy alpha categorical
  | alpha <= 0.0 = Left (InvalidRenyiOrder alpha)
  | alpha == 1.0 = Right (shannonEntropy categorical)
  | otherwise =
      let powerSum =
            getSum
              ( categoricalFoldMap
                  (\(_, probability) -> Sum ((positiveProbValue probability) ** alpha))
                  categorical
              )
       in Right (Entropy (log powerSum / (1.0 - alpha)))

klDivergence :: Ord a => Categorical a -> Categorical a -> Either DivergenceError Divergence
klDivergence left right =
  fmap (Divergence . sum)
    ( traverse contribution
        (categoricalFoldMap (\weightedOutcome -> [weightedOutcome]) left)
    )
  where
    contribution (outcome, probability) =
      case categoricalLookup outcome right of
        Nothing -> Left DivergenceSupportMismatch
        Just targetProbability ->
          let leftValue = positiveProbValue probability
              rightValue = probValue targetProbability
           in Right (leftValue * log (leftValue / rightValue))

jsDivergence :: Ord a => Categorical a -> Categorical a -> Divergence
jsDivergence left right =
  Divergence ((jsContribution left right + jsContribution right left) / 2.0)
  where
    jsContribution :: Ord key => Categorical key -> Categorical key -> Double
    jsContribution source peer =
      getSum
        ( categoricalFoldMap
            (\(outcome, sourceProbability) ->
               let sourceValue = positiveProbValue sourceProbability
                   peerValue = maybe 0.0 probValue (categoricalLookup outcome peer)
                   mixtureValue = (sourceValue + peerValue) / 2.0
                in Sum (sourceValue * log (sourceValue / mixtureValue))
            )
            source
        )

first :: (left -> right) -> Either left value -> Either right value
first transform result =
  case result of
    Left leftValue -> Left (transform leftValue)
    Right value -> Right value
