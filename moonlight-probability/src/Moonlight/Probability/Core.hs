module Moonlight.Probability.Core
  ( Prob,
    mkProb,
    probZero,
    probOne,
    probValue,
    PositiveProb,
    mkPositiveProb,
    positiveProbOne,
    positiveProbToProb,
    positiveProbValue,
    probToPositiveProb,
    probToLogProb,
    LogProb,
    mkLogProb,
    logProbZero,
    logProbOne,
    logProbValue,
  )
where

import Moonlight.Core (MoonlightError (..), MoonlightErrorContext (..), mkFiniteDouble)
import Moonlight.Probability.Core.Internal (LogProb (..), PositiveProb (..), Prob (..))
import Numeric.Log (Log (..))
import Prelude

mkProb :: Double -> Either MoonlightError Prob
mkProb value = do
  finiteValue <- mkFiniteDouble "probability" value
  if finiteValue < 0.0 || finiteValue > 1.0
    then Left (probabilityBoundsError finiteValue)
    else Right (Prob finiteValue)

probZero :: Prob
probZero = Prob 0.0

probOne :: Prob
probOne = Prob 1.0

probValue :: Prob -> Double
probValue = unProb

mkPositiveProb :: Double -> Either MoonlightError PositiveProb
mkPositiveProb value = do
  probability <- mkProb value
  case probToPositiveProb probability of
    Nothing -> Left (NonPositiveValue (DomainContext "probability"))
    Just positiveProbability -> Right positiveProbability

probabilityBoundsError :: Double -> MoonlightError
probabilityBoundsError value
  | value < 0.0 = NegativeValue (DomainContext "probability")
  | otherwise = NegativeValue (DomainContext "1 - probability")

positiveProbOne :: PositiveProb
positiveProbOne = PositiveProb probOne

positiveProbToProb :: PositiveProb -> Prob
positiveProbToProb (PositiveProb probability) = probability

positiveProbValue :: PositiveProb -> Double
positiveProbValue = probValue . positiveProbToProb

probToPositiveProb :: Prob -> Maybe PositiveProb
probToPositiveProb probability
  | probValue probability <= 0.0 = Nothing
  | otherwise = Just (PositiveProb probability)

probToLogProb :: Prob -> LogProb
probToLogProb (Prob 0.0) = LogProb 0
probToLogProb (Prob value) = LogProb (Exp (log value))

mkLogProb :: Double -> Either MoonlightError LogProb
mkLogProb = fmap probToLogProb . mkProb

logProbZero :: LogProb
logProbZero = LogProb 0

logProbOne :: LogProb
logProbOne = LogProb 1

logProbValue :: LogProb -> Double
logProbValue (LogProb encoded)
  | encoded == 0 = 0.0
  | otherwise = exp (ln encoded)
