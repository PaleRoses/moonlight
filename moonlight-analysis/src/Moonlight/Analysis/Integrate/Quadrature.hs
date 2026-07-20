module Moonlight.Analysis.Integrate.Quadrature
  ( MaxDepth,
    mkMaxDepth,
    maxDepthValue,
    adaptiveSimpson,
  )
where

import Data.Kind (Type)
import Moonlight.Core (AbsTol, MoonlightError (InvariantViolation), absTolValue, mkPositiveInt)
import Prelude

type MaxDepth :: Type
newtype MaxDepth = MaxDepth
  { maxDepthValue :: Int
  }
  deriving stock (Eq, Show)

mkMaxDepth :: Int -> Either MoonlightError MaxDepth
mkMaxDepth = fmap MaxDepth . mkPositiveInt "adaptive Simpson max depth"

adaptiveSimpson :: AbsTol -> MaxDepth -> (Double -> Double) -> Double -> Double -> Either MoonlightError Double
adaptiveSimpson toleranceValue depthLimit function lower upper
  | lower == upper = Right 0.0
  | otherwise =
      let midpointValue = midpoint lower upper
          lowerValue = function lower
          midpointFunctionValue = function midpointValue
          upperValue = function upper
          totalArea = simpsonFromSamples lower upper lowerValue midpointFunctionValue upperValue
       in integrate (maxDepthValue depthLimit) (absTolValue toleranceValue) lower upper lowerValue midpointFunctionValue upperValue totalArea
  where
    integrate remainingDepth toleranceBudget left right leftValue midpointValue rightValue wholeArea
      | remainingDepth <= 0 = Left (InvariantViolation "adaptive Simpson integration exhausted recursion depth")
      | otherwise =
          let center = midpoint left right
              leftCenter = midpoint left center
              rightCenter = midpoint center right
              leftCenterValue = function leftCenter
              rightCenterValue = function rightCenter
              leftArea = simpsonFromSamples left center leftValue leftCenterValue midpointValue
              rightArea = simpsonFromSamples center right midpointValue rightCenterValue rightValue
              refinedArea = leftArea + rightArea
              discrepancy = refinedArea - wholeArea
              threshold = 15.0 * toleranceBudget
           in if abs discrepancy <= threshold
                then Right (refinedArea + discrepancy / 15.0)
                else
                  let halfBudget = toleranceBudget / 2.0
                      nextDepth = remainingDepth - 1
                   in do
                        leftResult <- integrate nextDepth halfBudget left center leftValue leftCenterValue midpointValue leftArea
                        rightResult <- integrate nextDepth halfBudget center right midpointValue rightCenterValue rightValue rightArea
                        pure (leftResult + rightResult)

simpsonFromSamples :: Double -> Double -> Double -> Double -> Double -> Double
simpsonFromSamples lower upper lowerValue midpointValue upperValue =
  let width = upper - lower
   in width * (lowerValue + 4.0 * midpointValue + upperValue) / 6.0

midpoint :: Double -> Double -> Double
midpoint left right = (left + right) / 2.0
