module Moonlight.Analysis.Convergence
  ( IterationLimit,
    mkIterationLimit,
    iterationLimitValue,
    ResidualCadence,
    mkResidualCadence,
    residualCadenceValue,
    Tolerance (..),
    ConvergenceConfig (..),
    defaultConvergenceConfig,
    Termination (..),
    ConvergenceMetric (..),
    normalizeTolerance,
    withinTolerance,
    evaluateStream,
    evaluateStreamWithConfig,
  )
where

import Data.Kind (Constraint, Type)
import Moonlight.Core
  ( ApproxEq (..),
    MoonlightError,
    Tolerance (..),
    UlpTol,
    mkPositiveInt,
    normalizeTolerance,
    ulpTolValue,
    withinToleranceBy,
  )
import Prelude

type IterationLimit :: Type
newtype IterationLimit = IterationLimit
  { iterationLimitValue :: Int
  }
  deriving stock (Eq, Show)

mkIterationLimit :: Int -> Either MoonlightError IterationLimit
mkIterationLimit = fmap IterationLimit . mkPositiveInt "iteration limit"

type ResidualCadence :: Type
newtype ResidualCadence = ResidualCadence
  { residualCadenceValue :: Int
  }
  deriving stock (Eq, Show)

mkResidualCadence :: Int -> Either MoonlightError ResidualCadence
mkResidualCadence = fmap ResidualCadence . mkPositiveInt "residual cadence"

type ConvergenceConfig :: Type
data ConvergenceConfig = ConvergenceConfig
  { tolerance :: !Tolerance,
    iterationLimit :: !IterationLimit,
    residualCadence :: !ResidualCadence
  }
  deriving stock (Eq, Show)

defaultConvergenceConfig :: Tolerance -> IterationLimit -> ConvergenceConfig
defaultConvergenceConfig toleranceValue iterationLimitValue' =
  ConvergenceConfig
    { tolerance = toleranceValue,
      iterationLimit = iterationLimitValue',
      residualCadence = ResidualCadence 1
    }

type Termination :: Type -> Type
data Termination a
  = Converged !a
  | IterationLimitReached !a !Int
  | Diverged
  deriving stock (Eq, Show)

type ConvergenceMetric :: Type -> Constraint
class Eq a => ConvergenceMetric a where
  convergenceDistance :: a -> a -> Double
  convergenceScale :: a -> a -> Double
  convergenceWithinUlp :: UlpTol -> a -> a -> Bool

instance ConvergenceMetric Double where
  convergenceDistance left right = abs (left - right)
  convergenceScale left right = max (abs left) (abs right)
  convergenceWithinUlp = approxEq

instance ConvergenceMetric Float where
  convergenceDistance left right = abs (realToFrac left - realToFrac right)
  convergenceScale left right = max (abs (realToFrac left)) (abs (realToFrac right))
  convergenceWithinUlp tolerance left right = approxEq tolerance (realToFrac left :: Double) (realToFrac right :: Double)

instance ConvergenceMetric Int where
  convergenceDistance left right = fromIntegral (abs (left - right))
  convergenceScale left right = fromIntegral (max (abs left) (abs right))
  convergenceWithinUlp tolerance left right =
    let toleranceBound = fromIntegral (ulpTolValue tolerance) :: Int
     in abs (left - right) <= toleranceBound

instance ConvergenceMetric Integer where
  convergenceDistance left right = fromIntegral (abs (left - right))
  convergenceScale left right = fromIntegral (max (abs left) (abs right))
  convergenceWithinUlp tolerance left right =
    let toleranceBound = fromIntegral (ulpTolValue tolerance) :: Integer
     in abs (left - right) <= toleranceBound

evaluateStream :: (ConvergenceMetric a) => Tolerance -> IterationLimit -> [a] -> Termination a
evaluateStream toleranceValue iterationLimitValue' =
  evaluateStreamWithConfig (defaultConvergenceConfig toleranceValue iterationLimitValue')

evaluateStreamWithConfig :: (ConvergenceMetric a) => ConvergenceConfig -> [a] -> Termination a
evaluateStreamWithConfig config approximations =
  case approximations of
    [] -> Diverged
    firstValue : remainingValues
      | limit <= 1 -> IterationLimitReached firstValue 1
      | otherwise -> evaluate firstValue 1 remainingValues
  where
    limit = iterationLimitValue (iterationLimit config)
    cadence = residualCadenceValue (residualCadence config)

    evaluate previousValue iterationIndex remainingValues =
      case remainingValues of
        [] -> IterationLimitReached previousValue iterationIndex
        currentValue : nextValues ->
          let nextIterationIndex = iterationIndex + 1
              shouldEvaluateResidual = nextIterationIndex `mod` cadence == 0
              hasConverged = shouldEvaluateResidual && withinTolerance (tolerance config) previousValue currentValue
           in if hasConverged
                then Converged currentValue
                else
                  if nextIterationIndex >= limit
                    then IterationLimitReached currentValue nextIterationIndex
                    else evaluate currentValue nextIterationIndex nextValues

withinTolerance :: (ConvergenceMetric a) => Tolerance -> a -> a -> Bool
withinTolerance =
  withinToleranceBy
    convergenceDistance
    convergenceScale
    convergenceWithinUlp
