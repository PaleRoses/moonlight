{-# LANGUAGE BangPatterns #-}

module Moonlight.Probability.Sample.Input
  ( SampleInputError, expectSampleInput, isFinite, isStrictlyPositiveFinite, isNonNegativeFinite, isFinitePositive, negativeInfinity,
    validatePositiveTemperature, validatePossibleLogWeights, validateTopKLogWeights, normalizeLogWeights, weightsToLogWeights,
    possibleLogWeightCount, validatePossibleLogits, validateTopKLogits, softmaxStable, weightsToLogits,
  )
where

import Control.Monad ((>=>))
import qualified Data.Vector.Unboxed as VU
import Prelude

data SampleInputError = SampleInputError !String !SampleInputProblem
  deriving stock (Eq, Show)

data SampleInputProblem
  = Empty !InputSubject | Invalid !InputSubject
  | NonPositiveWeightTotal | NoPositiveWeight | NoPossibleWeight | InvalidTemperature | NegativeCount
  | CountExceedsFeasible !Int !Int
  | NormalizationFailed
  deriving stock (Eq, Show)

data InputSubject = WeightVector | Weights | LogWeights | Logits
  deriving stock (Eq, Show)

expectSampleInput :: Either SampleInputError a -> a
expectSampleInput = either (error . renderInputError) id

isFinite :: Double -> Bool
isFinite value = not (isNaN value) && not (isInfinite value)

isStrictlyPositiveFinite :: Double -> Bool
isStrictlyPositiveFinite value = value > 0.0 && isFinite value

isNonNegativeFinite :: Double -> Bool
isNonNegativeFinite value = value >= 0.0 && isFinite value

isFinitePositive :: Double -> Bool
isFinitePositive = isStrictlyPositiveFinite

negativeInfinity :: Double
negativeInfinity = negate positiveInfinity

validatePositiveTemperature :: String -> Double -> Either SampleInputError Double
validatePositiveTemperature = require InvalidTemperature isStrictlyPositiveFinite

validatePossibleLogWeights :: String -> VU.Vector Double -> Either SampleInputError (VU.Vector Double)
validatePossibleLogWeights label =
  validateLogWeights label
    >=> require (Empty WeightVector) (not . VU.null) label
    >=> require NoPossibleWeight ((> 0) . possibleLogWeightCount) label

validateTopKLogWeights :: String -> Int -> VU.Vector Double -> Either SampleInputError (VU.Vector Double)
validateTopKLogWeights label count logWeights =
  ensure label NegativeCount (count >= 0)
    *> validatePossibleLogWeights label logWeights
    <* ensure label (CountExceedsFeasible count possible) (count <= possible)
  where
    !possible = possibleLogWeightCount logWeights

normalizeLogWeights :: String -> VU.Vector Double -> Either SampleInputError (VU.Vector Double)
normalizeLogWeights label logWeights
  | VU.null logWeights = Right VU.empty
  | positiveInfinityCount > 0 =
      let !mass = recip (fromIntegral positiveInfinityCount)
       in Right (VU.map (\value -> if isPositiveInfinity value then mass else 0.0) logWeights)
  | otherwise =
      let !maxLogWeight = VU.maximum logWeights
          !shiftedWeights =
            VU.map (\logWeight -> if logWeight == negativeInfinity then 0.0 else exp (logWeight - maxLogWeight)) logWeights
          !normalizer = VU.sum shiftedWeights
       in ensure label NormalizationFailed (normalizer > 0.0 && isFinite normalizer)
            *> pure (VU.map (/ normalizer) shiftedWeights)
  where
    !positiveInfinityCount =
      VU.foldl' (\acc value -> if isPositiveInfinity value then acc + 1 else acc) (0 :: Int) logWeights

weightsToLogWeights :: String -> VU.Vector Double -> Either SampleInputError (VU.Vector Double)
weightsToLogWeights label weights =
  ensure label (Empty WeightVector) (not (VU.null weights))
    *> ensure label (Invalid Weights) (VU.all isNonNegativeFinite weights)
    *> ensure label NonPositiveWeightTotal (VU.sum weights > 0.0)
    *> pure (VU.map (\weight -> if weight == 0.0 then negativeInfinity else log weight) weights)

possibleLogWeightCount :: VU.Vector Double -> Int
possibleLogWeightCount =
  VU.foldl' (\acc value -> if isFinite value || isPositiveInfinity value then acc + 1 else acc) 0

validatePossibleLogits :: String -> [Double] -> Either SampleInputError [Double]
validatePossibleLogits label =
  validateLogits label
    >=> require (Empty Logits) (not . null) label
    >=> require NoPossibleWeight ((> 0) . possibleLogitCount) label

validateTopKLogits :: String -> Int -> [Double] -> Either SampleInputError [Double]
validateTopKLogits label count logits =
  ensure label NegativeCount (count >= 0)
    *> validatePossibleLogits label logits
    <* ensure label (CountExceedsFeasible count possible) (count <= possible)
  where
    !possible = possibleLogitCount logits

softmaxStable :: String -> [Double] -> Either SampleInputError [Double]
softmaxStable label logits =
  ensure label NoPossibleWeight (maxValue /= negativeInfinity && not (isNaN maxValue))
    *> ensure label NormalizationFailed (denominator > 0.0 && isFinite denominator)
    *> pure (fmap normalize logits)
  where
    !maxValue = foldl' max negativeInfinity logits
    !denominator =
      foldl'
        (\acc logit -> if logit == negativeInfinity then acc else acc + exp (logit - maxValue))
        0.0
        logits
    !invDenominator = recip denominator
    normalize logit =
      if logit == negativeInfinity then 0.0 else exp (logit - maxValue) * invDenominator

weightsToLogits :: String -> [Double] -> Either SampleInputError [Double]
weightsToLogits label weights =
  ensure label (Empty Weights) (not (null weights)) *> do
    logits <- traverse weightToLogit weights
    ensure label NoPositiveWeight (possibleLogitCount logits > 0) *> pure logits
  where
    weightToLogit weight =
      ensure label (Invalid Weights) (isNonNegativeFinite weight)
        *> pure (if weight == 0.0 then negativeInfinity else log weight)

possibleLogitCount :: [Double] -> Int
possibleLogitCount =
  foldl' (\count logit -> if logit == negativeInfinity then count else count + 1) 0

validateLogWeights :: String -> VU.Vector Double -> Either SampleInputError (VU.Vector Double)
validateLogWeights = require (Invalid LogWeights) (not . VU.any isNaN)

validateLogits :: String -> [Double] -> Either SampleInputError [Double]
validateLogits label =
  traverse (\logit -> ensure label (Invalid Logits) (not (isNaN logit) && logit /= positiveInfinity) *> pure logit)

require :: SampleInputProblem -> (a -> Bool) -> String -> a -> Either SampleInputError a
require problem predicate label value =
  ensure label problem (predicate value) *> pure value

ensure :: String -> SampleInputProblem -> Bool -> Either SampleInputError ()
ensure label problem predicate =
  if predicate then Right () else Left (SampleInputError label problem)

positiveInfinity :: Double
positiveInfinity = 1.0 / 0.0

isPositiveInfinity :: Double -> Bool
isPositiveInfinity value = isInfinite value && value > 0.0

renderInputError :: SampleInputError -> String
renderInputError (SampleInputError label problem) = label ++ ": " ++ renderInputProblem problem

renderInputProblem :: SampleInputProblem -> String
renderInputProblem problem =
  case problem of
    Empty WeightVector -> "empty weight vector"; Empty Weights -> "weights must be non-empty"; Empty LogWeights -> "empty log-weight vector"; Empty Logits -> "logits must be non-empty"
    Invalid WeightVector -> "invalid weight vector"; Invalid Weights -> "weights must be finite and non-negative"; Invalid LogWeights -> "log-weights must not be NaN"; Invalid Logits -> "logits must be finite or -Infinity"
    NonPositiveWeightTotal -> "total weight must be strictly positive"; NoPositiveWeight -> "at least one weight must be > 0"
    NoPossibleWeight -> "at least one weight must be possible"; InvalidTemperature -> "temperature must be finite and strictly positive"; NegativeCount -> "count must be non-negative"
    CountExceedsFeasible requested feasible ->
      "requested " ++ show requested ++ " samples from " ++ show feasible ++ " feasible outcomes"
    NormalizationFailed -> "normalization failed"
