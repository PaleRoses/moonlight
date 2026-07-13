module Moonlight.Probability.Distribution
  ( HasDistribution,
    HasContDistr,
    HasDiscreteDistr,
    HasMean,
    HasVariance,
    HasDistributionEntropy,
    distributionCumulative,
    distributionComplCumulative,
    distributionDensity,
    distributionLogDensity,
    distributionProbability,
    distributionLogProbability,
    distributionQuantile,
    distributionComplQuantile,
    distributionMean,
    distributionVariance,
    distributionStdDev,
    distributionEntropy,
    module Moonlight.Probability.Distribution.Simplex,
    module Moonlight.Probability.Distribution.Kernel,
  )
where

import Data.Kind (Constraint, Type)
import Moonlight.Probability.Distribution.Kernel
import Moonlight.Probability.Distribution.Simplex
import Statistics.Distribution
  ( ContDistr,
    DiscreteDistr,
    Distribution,
    Entropy,
    Mean,
    Variance,
    complCumulative,
    complQuantile,
    cumulative,
    density,
    entropy,
    logDensity,
    logProbability,
    mean,
    probability,
    quantile,
    stdDev,
    variance,
  )

type HasDistribution :: Type -> Constraint
type HasDistribution d = Distribution d

type HasContDistr :: Type -> Constraint
type HasContDistr d = (Distribution d, ContDistr d)

type HasDiscreteDistr :: Type -> Constraint
type HasDiscreteDistr d = (Distribution d, DiscreteDistr d)

type HasMean :: Type -> Constraint
type HasMean d = Mean d

type HasVariance :: Type -> Constraint
type HasVariance d = Variance d

type HasDistributionEntropy :: Type -> Constraint
type HasDistributionEntropy d = Entropy d

distributionCumulative :: Distribution d => d -> Double -> Double
distributionCumulative = cumulative

distributionComplCumulative :: Distribution d => d -> Double -> Double
distributionComplCumulative = complCumulative

distributionDensity :: ContDistr d => d -> Double -> Double
distributionDensity = density

distributionLogDensity :: ContDistr d => d -> Double -> Double
distributionLogDensity = logDensity

distributionProbability :: DiscreteDistr d => d -> Int -> Double
distributionProbability = probability

distributionLogProbability :: DiscreteDistr d => d -> Int -> Double
distributionLogProbability = logProbability

distributionQuantile :: ContDistr d => d -> Double -> Double
distributionQuantile = quantile

distributionComplQuantile :: ContDistr d => d -> Double -> Double
distributionComplQuantile = complQuantile

distributionMean :: Mean d => d -> Double
distributionMean = mean

distributionVariance :: Variance d => d -> Double
distributionVariance = variance

distributionStdDev :: Variance d => d -> Double
distributionStdDev = stdDev

distributionEntropy :: Entropy d => d -> Double
distributionEntropy = entropy
