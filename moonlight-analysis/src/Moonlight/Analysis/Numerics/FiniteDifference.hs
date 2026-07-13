module Moonlight.Analysis.Numerics.FiniteDifference
  ( StepSize,
    mkStepSize,
    stepSizeValue,
    forwardDifference,
    centralDifference,
    richardsonExtrapolation,
  )
where

import Data.Kind (Type)
import Moonlight.Core (MoonlightError (InvariantViolation), mkPositiveFiniteWith)
import Prelude

type StepSize :: Type
newtype StepSize = StepSize
  { stepSizeValue :: Double
  }
  deriving stock (Eq, Show)

mkStepSize :: Double -> Either MoonlightError StepSize
mkStepSize =
  mkPositiveFiniteWith
    (\_ -> InvariantViolation "finite-difference step size must be positive and finite")
    StepSize

forwardDifference :: StepSize -> (Double -> Double) -> Double -> Double
forwardDifference step function value =
  let h = stepSizeValue step
   in (function (value + h) - function value) / h

centralDifference :: StepSize -> (Double -> Double) -> Double -> Double
centralDifference step function value =
  let h = stepSizeValue step
   in (function (value + h) - function (value - h)) / (2.0 * h)

richardsonExtrapolation :: StepSize -> (Double -> Double) -> Double -> Double
richardsonExtrapolation step function value =
  let h = stepSizeValue step
      coarse = centralDifference (StepSize h) function value
      refined = centralDifference (StepSize (h / 2.0)) function value
   in (4.0 * refined - coarse) / 3.0
