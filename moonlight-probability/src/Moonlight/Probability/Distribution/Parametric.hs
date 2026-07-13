module Moonlight.Probability.Distribution.Parametric
  ( NormalDistribution,
    standardNormal,
    mkNormalDistribution,
    UniformDistribution,
    mkUniformDistribution,
    ExponentialDistribution,
    mkExponentialDistribution,
    GammaDistribution,
    mkGammaDistribution,
    BetaDistribution,
    mkBetaDistribution,
    BinomialDistribution,
    mkBinomialDistribution,
    PoissonDistribution,
    mkPoissonDistribution,
  )
where

import Moonlight.Core (MoonlightError (..), MoonlightErrorContext (..), mkFiniteDouble, mkNonNegativeFiniteDouble, mkPositiveFiniteDouble)
import Statistics.Distribution.Beta (BetaDistribution, betaDistrE)
import Statistics.Distribution.Binomial (BinomialDistribution, binomialE)
import Statistics.Distribution.Exponential (ExponentialDistribution, exponentialE)
import Statistics.Distribution.Gamma (GammaDistribution, gammaDistrE)
import Statistics.Distribution.Normal (NormalDistribution, normalDistrErr, standard)
import Statistics.Distribution.Poisson (PoissonDistribution, poissonE)
import Statistics.Distribution.Uniform (UniformDistribution, uniformDistrE)
import Prelude

standardNormal :: NormalDistribution
standardNormal = standard

mkNormalDistribution :: Double -> Double -> Either MoonlightError NormalDistribution
mkNormalDistribution meanValue standardDeviation = do
  finiteMean <- mkFiniteDouble "normal mean" meanValue
  positiveStdDev <- mkPositiveFiniteDouble "normal standard deviation" standardDeviation
  either (const (Left NonCanonicalFiniteValue)) Right (normalDistrErr finiteMean positiveStdDev)

mkUniformDistribution :: Double -> Double -> Either MoonlightError UniformDistribution
mkUniformDistribution lowerBound upperBound = do
  finiteLower <- mkFiniteDouble "uniform lower bound" lowerBound
  finiteUpper <- mkFiniteDouble "uniform upper bound" upperBound
  if finiteLower >= finiteUpper
    then Left (NegativeValue (DomainContext "uniform upper bound - lower bound"))
    else maybeToEither "uniform bounds are invalid" (uniformDistrE finiteLower finiteUpper)

mkExponentialDistribution :: Double -> Either MoonlightError ExponentialDistribution
mkExponentialDistribution lambdaValue = do
  positiveLambda <- mkPositiveFiniteDouble "exponential rate" lambdaValue
  maybeToEither "exponential rate is invalid" (exponentialE positiveLambda)

mkGammaDistribution :: Double -> Double -> Either MoonlightError GammaDistribution
mkGammaDistribution shape scale = do
  positiveShape <- mkPositiveFiniteDouble "gamma shape" shape
  positiveScale <- mkPositiveFiniteDouble "gamma scale" scale
  maybeToEither "gamma parameters are invalid" (gammaDistrE positiveShape positiveScale)

mkBetaDistribution :: Double -> Double -> Either MoonlightError BetaDistribution
mkBetaDistribution alpha beta = do
  positiveAlpha <- mkPositiveFiniteDouble "beta alpha" alpha
  positiveBeta <- mkPositiveFiniteDouble "beta beta" beta
  maybeToEither "beta parameters are invalid" (betaDistrE positiveAlpha positiveBeta)

mkBinomialDistribution :: Int -> Double -> Either MoonlightError BinomialDistribution
mkBinomialDistribution trials probability = do
  nonNegativeTrials <-
    if trials < 0
      then Left (NegativeValue (DomainContext "binomial trials"))
      else Right trials
  unitProbability <- mkNonNegativeFiniteDouble "binomial probability" probability >>= ensureUnitInterval
  maybeToEither "binomial parameters are invalid" (binomialE nonNegativeTrials unitProbability)

mkPoissonDistribution :: Double -> Either MoonlightError PoissonDistribution
mkPoissonDistribution lambdaValue = do
  positiveLambda <- mkPositiveFiniteDouble "poisson rate" lambdaValue
  maybeToEither "poisson rate is invalid" (poissonE positiveLambda)

ensureUnitInterval :: Double -> Either MoonlightError Double
ensureUnitInterval value =
  if value > 1.0
    then Left (NegativeValue (DomainContext "1 - probability"))
    else Right value

maybeToEither :: String -> Maybe value -> Either MoonlightError value
maybeToEither message maybeValue =
  case maybeValue of
    Nothing -> Left NonCanonicalFiniteValue
    Just value -> Right value
