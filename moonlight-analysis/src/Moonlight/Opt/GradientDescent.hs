
module Moonlight.Opt.GradientDescent
  ( LearningRate,
    mkLearningRate,
    learningRateValue,
    AdamParameters (..),
    mkAdamParameters,
    GradientMethod (..),
    Projection (..),
    applyProjection,
    gradientDescentSteps,
    optimizeGradientDescent,
    optimizeGradientDescentWithConfig,
  )
where

import Data.Kind (Type)
import Moonlight.Analysis.Convergence
  ( ConvergenceConfig (..),
    IterationLimit,
    ResidualCadence,
    Termination,
    Tolerance,
    defaultConvergenceConfig,
    evaluateStream,
    evaluateStreamWithConfig,
  )
import Moonlight.Analysis.Dual (Dual, derivative)
import Moonlight.Core (MoonlightError (InvariantViolation), mkFiniteDouble, mkPositiveFiniteDouble, mkPositiveFiniteWith)
import Prelude

type LearningRate :: Type
newtype LearningRate = LearningRate
  { learningRateValue :: Double
  }
  deriving stock (Eq, Show)

mkLearningRate :: Double -> Either MoonlightError LearningRate
mkLearningRate =
  mkPositiveFiniteWith
    (\_ -> InvariantViolation "learning rate must be positive and finite")
    LearningRate

type AdamParameters :: Type
data AdamParameters = AdamParameters
  { adamLearningRate :: !LearningRate,
    adamBeta1 :: !Double,
    adamBeta2 :: !Double,
    adamEpsilon :: !Double
  }
  deriving stock (Eq, Show)

mkAdamParameters :: LearningRate -> Double -> Double -> Double -> Either MoonlightError AdamParameters
mkAdamParameters rate beta1 beta2 epsilon = do
  finiteBeta1 <- mkFiniteDouble "adam beta1" beta1
  finiteBeta2 <- mkFiniteDouble "adam beta2" beta2
  validatedEpsilon <- mkPositiveFiniteDouble "adam epsilon" epsilon
  if finiteBeta1 <= 0.0 || finiteBeta1 >= 1.0
    then Left (InvariantViolation "adam beta1 must satisfy 0 < beta1 < 1")
    else if finiteBeta2 <= 0.0 || finiteBeta2 >= 1.0
      then Left (InvariantViolation "adam beta2 must satisfy 0 < beta2 < 1")
      else Right (AdamParameters rate finiteBeta1 finiteBeta2 validatedEpsilon)

type GradientMethod :: Type
data GradientMethod
  = SteepestDescent !LearningRate
  | Adam !AdamParameters
  deriving stock (Eq, Show)

type Projection :: Type
data Projection
  = IdentityProjection
  | ClampProjection !Double !Double
  deriving stock (Eq, Show)

applyProjection :: Projection -> Double -> Double
applyProjection projection value =
  case projection of
    IdentityProjection -> value
    ClampProjection lower upper
      | value < lower -> lower
      | value > upper -> upper
      | otherwise -> value

gradientDescentSteps ::
  GradientMethod ->
  Projection ->
  (forall s. Dual s Double -> Dual s Double) ->
  Double ->
  [Double]
gradientDescentSteps method projection objective initialValue =
  case method of
    SteepestDescent rate -> iterate steepestStep startingValue
      where
        steepestStep currentValue =
          let grad = gradient objective currentValue
              rateValue = learningRateValue rate
           in applyProjection projection (currentValue - rateValue * grad)
    Adam parameters -> startingValue : adamSteps 1 0.0 0.0 startingValue
      where
        rateValue = learningRateValue (adamLearningRate parameters)
        beta1 = adamBeta1 parameters
        beta2 = adamBeta2 parameters
        epsilon = adamEpsilon parameters

        adamSteps :: Int -> Double -> Double -> Double -> [Double]
        adamSteps iteration momentum velocity currentValue =
          let grad = gradient objective currentValue
              nextMomentum = beta1 * momentum + (1.0 - beta1) * grad
              nextVelocity = beta2 * velocity + (1.0 - beta2) * grad * grad
              correction1 = 1.0 - beta1 ** (fromIntegral iteration :: Double)
              correction2 = 1.0 - beta2 ** (fromIntegral iteration :: Double)
              momentumHat = nextMomentum / correction1
              velocityHat = nextVelocity / correction2
              stepSize = rateValue * momentumHat / (sqrt velocityHat + epsilon)
              nextValue = applyProjection projection (currentValue - stepSize)
           in nextValue : adamSteps (iteration + 1) nextMomentum nextVelocity nextValue
  where
    startingValue = applyProjection projection initialValue

optimizeGradientDescent ::
  Tolerance ->
  IterationLimit ->
  GradientMethod ->
  Projection ->
  (forall s. Dual s Double -> Dual s Double) ->
  Double ->
  Termination Double
optimizeGradientDescent toleranceValue iterationLimitValue method projection objective initialValue =
  evaluateStream toleranceValue iterationLimitValue (gradientDescentSteps method projection objective initialValue)

optimizeGradientDescentWithConfig ::
  Tolerance ->
  IterationLimit ->
  ResidualCadence ->
  GradientMethod ->
  Projection ->
  (forall s. Dual s Double -> Dual s Double) ->
  Double ->
  Termination Double
optimizeGradientDescentWithConfig toleranceValue iterationLimitValue residualCadenceValue method projection objective initialValue =
  let config =
        (defaultConvergenceConfig toleranceValue iterationLimitValue)
          { residualCadence = residualCadenceValue
          }
   in optimizeGradientDescentWithConfig' config method projection objective initialValue

optimizeGradientDescentWithConfig' ::
  ConvergenceConfig ->
  GradientMethod ->
  Projection ->
  (forall s. Dual s Double -> Dual s Double) ->
  Double ->
  Termination Double
optimizeGradientDescentWithConfig' config method projection objective initialValue =
  evaluateStreamWithConfig config (gradientDescentSteps method projection objective initialValue)

gradient :: (forall s. Dual s Double -> Dual s Double) -> Double -> Double
gradient = derivative
