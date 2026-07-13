module Moonlight.Probability.Distribution.Transform
  ( MixtureDistribution,
    mkMixtureDistribution,
    mixtureComponents,
    TruncatedDistribution,
    mkTruncatedDistribution,
    truncatedBounds,
    truncatedBase,
    mixtureCategorical,
    conditionCategorical,
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Moonlight.Core (MoonlightError (..), mkFiniteDouble)
import Moonlight.Probability.Core (PositiveProb, mkPositiveProb, positiveProbValue)
import Moonlight.Probability.Distribution
  ( HasContDistr,
    distributionComplCumulative,
    distributionCumulative,
    distributionDensity,
    distributionMean,
    distributionQuantile,
    distributionVariance,
  )
import Moonlight.Probability.Distribution.Categorical
  ( Categorical,
    CategoricalError,
    CategoricalError (..),
    blendCategorical,
    categoricalRestrict,
    categoricalSupport,
  )
import Statistics.Distribution
  ( ContDistr (..),
    Distribution (..),
    Mean (..),
    MaybeMean (..),
    MaybeVariance (..),
    Variance (..),
  )
import Prelude

type MixtureDistribution :: Type -> Type
data MixtureDistribution d = MixtureDistribution
  { mixtureWeightedComponents :: NonEmpty (PositiveProb, d)
  }
  deriving stock (Eq, Show)

type TruncatedDistribution :: Type -> Type
data TruncatedDistribution d = TruncatedDistribution
  { truncatedInterval :: (Double, Double),
    truncatedDistribution :: d,
    truncatedMass :: Double
  }
  deriving stock (Eq, Show)

mkMixtureDistribution :: NonEmpty (Double, d) -> Either MoonlightError (MixtureDistribution d)
mkMixtureDistribution = fmap MixtureDistribution . normalizeWeights

mixtureComponents :: MixtureDistribution d -> NonEmpty (PositiveProb, d)
mixtureComponents = mixtureWeightedComponents

mkTruncatedDistribution :: HasContDistr d => (Double, Double) -> d -> Either MoonlightError (TruncatedDistribution d)
mkTruncatedDistribution (lowerBound, upperBound) distribution = do
  finiteLower <- mkFiniteDouble "truncation lower bound" lowerBound
  finiteUpper <- mkFiniteDouble "truncation upper bound" upperBound
  if finiteLower >= finiteUpper
    then Left (InvariantViolation "truncation bounds must be ordered")
    else
      let retainedMass = distributionCumulative distribution finiteUpper - distributionCumulative distribution finiteLower
       in if retainedMass <= 0.0
            then Left (InvariantViolation "truncation interval must retain positive mass")
            else
              Right
                TruncatedDistribution
                  { truncatedInterval = (finiteLower, finiteUpper),
                    truncatedDistribution = distribution,
                    truncatedMass = retainedMass
                  }

truncatedBounds :: TruncatedDistribution d -> (Double, Double)
truncatedBounds = truncatedInterval

truncatedBase :: TruncatedDistribution d -> d
truncatedBase = truncatedDistribution

mixtureCategorical :: Ord a => NonEmpty (Double, Categorical a) -> Either CategoricalError (Categorical a)
mixtureCategorical = fmap blendCategorical . normalizeCategoricalInputs

conditionCategorical :: Ord a => (a -> Bool) -> Categorical a -> Maybe (Categorical a)
conditionCategorical predicate categorical =
  categoricalRestrict
    (Set.filter predicate (categoricalSupport categorical))
    categorical

instance Distribution d => Distribution (MixtureDistribution d) where
  cumulative mixture value =
    weightedMixtureSum mixture (`distributionCumulative` value)
  complCumulative mixture value =
    weightedMixtureSum mixture (`distributionComplCumulative` value)

instance HasContDistr d => ContDistr (MixtureDistribution d) where
  density mixture value =
    weightedMixtureSum mixture (`distributionDensity` value)
  logDensity mixture value = log (density mixture value)
  quantile mixture probability =
    approximateMixtureQuantile mixture probability
  complQuantile mixture probability =
    approximateMixtureQuantile mixture (1.0 - probability)

instance Mean d => MaybeMean (MixtureDistribution d) where
  maybeMean = Just . mean

instance Mean d => Mean (MixtureDistribution d) where
  mean mixture =
    weightedMixtureSum mixture distributionMean

instance Variance d => MaybeVariance (MixtureDistribution d) where
  maybeVariance = Just . variance

instance Variance d => Variance (MixtureDistribution d) where
  variance mixture =
    let mixtureMean = mean mixture
        secondMoment =
          weightedMixtureSum mixture (\distribution -> distributionVariance distribution + distributionMean distribution ** 2.0)
     in secondMoment - mixtureMean ** 2.0
  stdDev = sqrt . variance

instance Distribution d => Distribution (TruncatedDistribution d) where
  cumulative truncated value
    | value <= lowerBound = 0.0
    | value >= upperBound = 1.0
    | otherwise =
        (distributionCumulative distribution value - distributionCumulative distribution lowerBound) / retainedMass
    where
      (lowerBound, upperBound) = truncatedInterval truncated
      distribution = truncatedDistribution truncated
      retainedMass = truncatedMass truncated
  complCumulative truncated value = 1.0 - cumulative truncated value

instance HasContDistr d => ContDistr (TruncatedDistribution d) where
  density truncated value
    | value < lowerBound || value > upperBound = 0.0
    | otherwise = distributionDensity distribution value / retainedMass
    where
      (lowerBound, upperBound) = truncatedInterval truncated
      distribution = truncatedDistribution truncated
      retainedMass = truncatedMass truncated
  logDensity truncated value = log (density truncated value)
  quantile truncated probability =
    let (lowerBound, _) = truncatedInterval truncated
        distribution = truncatedDistribution truncated
        retainedMass = truncatedMass truncated
        offset = distributionCumulative distribution lowerBound
     in distributionQuantile distribution (offset + probability * retainedMass)
  complQuantile truncated probability = quantile truncated (1.0 - probability)

normalizeWeights :: NonEmpty (Double, value) -> Either MoonlightError (NonEmpty (PositiveProb, value))
normalizeWeights =
  normalizeWeighted validateWeight toPositiveProbability
  where
    validateWeight :: (Double, value) -> Either MoonlightError (Double, value)
    validateWeight (weight, value) = do
      finiteWeight <- mkFiniteDouble "mixture weight" weight
      if finiteWeight <= 0.0
        then Left (InvariantViolation "mixture weights must be positive")
        else Right (finiteWeight, value)
    toPositiveProbability :: Double -> (Double, value) -> Either MoonlightError (PositiveProb, value)
    toPositiveProbability totalWeight (weight, value) = do
      probability <-
        case mkPositiveProb (weight / totalWeight) of
          Left err -> Left err
          Right validProbability -> Right validProbability
      pure (probability, value)

normalizeCategoricalInputs :: NonEmpty (Double, Categorical a) -> Either CategoricalError (NonEmpty (PositiveProb, Categorical a))
normalizeCategoricalInputs =
  normalizeWeighted validateCategoricalWeight toCategoricalProbability
  where
    validateCategoricalWeight :: (Double, Categorical a) -> Either CategoricalError (Double, Categorical a)
    validateCategoricalWeight (weight, categorical) =
      case mkFiniteDouble "categorical mixture weight" weight of
        Left _ -> Left (InvalidCategoricalWeight weight)
        Right finiteWeight ->
          if finiteWeight <= 0.0
            then Left (NonPositiveCategoricalWeight weight)
            else Right (finiteWeight, categorical)
    toCategoricalProbability :: Double -> (Double, Categorical a) -> Either CategoricalError (PositiveProb, Categorical a)
    toCategoricalProbability totalWeight (weight, categorical) =
      case mkPositiveProb (weight / totalWeight) of
        Left _ -> Left (InvalidCategoricalWeight weight)
        Right probability -> Right (probability, categorical)

weightedMixtureSum :: MixtureDistribution d -> (d -> Double) -> Double
weightedMixtureSum mixture project =
  sum (fmap (\(weight, distribution) -> positiveProbValue weight * project distribution) (NonEmpty.toList (mixtureWeightedComponents mixture)))

normalizeWeighted ::
  ((Double, value) -> Either error (Double, value)) ->
  (Double -> (Double, value) -> Either error (PositiveProb, value)) ->
  NonEmpty (Double, value) ->
  Either error (NonEmpty (PositiveProb, value))
normalizeWeighted validateWeight toPositiveProbability weightedValues = do
  positiveWeights <- traverse validateWeight weightedValues
  let totalWeight = sum (fmap fst positiveWeights)
  traverse (toPositiveProbability totalWeight) positiveWeights

approximateMixtureQuantile :: HasContDistr d => MixtureDistribution d -> Double -> Double
approximateMixtureQuantile mixture probability =
  bisect 128 lowerBound upperBound
  where
    clippedProbability = max 1.0e-12 (min (1.0 - 1.0e-12) probability)
    componentBounds =
      fmap
        (\(_, distribution) ->
           ( distributionQuantile distribution 1.0e-12,
             distributionQuantile distribution (1.0 - 1.0e-12)
           ))
        (NonEmpty.toList (mixtureWeightedComponents mixture))
    lowerBound = minimum (fmap fst componentBounds)
    upperBound = maximum (fmap snd componentBounds)
    targetCdf = clippedProbability
    bisect :: Int -> Double -> Double -> Double
    bisect 0 left right = (left + right) / 2.0
    bisect remaining left right =
      let midpoint = (left + right) / 2.0
          midpointCdf = cumulative mixture midpoint
       in if midpointCdf < targetCdf
            then bisect (remaining - 1) midpoint right
            else bisect (remaining - 1) left midpoint
