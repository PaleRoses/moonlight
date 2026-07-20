module Moonlight.Analysis.Integrate.ODE
  ( ODESystem,
    ODEStep (..),
    StepSizeControl (..),
    rk4Step,
    dormandPrinceStep,
    integrateRK4,
    integrateAdaptive,
  )
where

import Data.Kind (Type)
import Prelude

type ODESystem :: Type
type ODESystem = Double -> [Double] -> [Double]

type ODEStep :: Type
data ODEStep = ODEStep
  { odeTime :: Double,
    odeState :: [Double]
  }
  deriving stock (Eq, Show)

type StepSizeControl :: Type
data StepSizeControl = StepSizeControl
  { minimumStepSize :: Double,
    maximumStepSize :: Double,
    initialStepSize :: Double,
    errorTolerance :: Double,
    safetyFactor :: Double
  }
  deriving stock (Eq, Show)

rk4Step :: Double -> ODESystem -> Double -> [Double] -> [Double]
rk4Step stepSize system timeValue stateValue =
  let halfStep = stepSize / 2.0
      k1 = system timeValue stateValue
      k2 = system (timeValue + halfStep) (stateValue `addScaled` (halfStep, k1))
      k3 = system (timeValue + halfStep) (stateValue `addScaled` (halfStep, k2))
      k4 = system (timeValue + stepSize) (stateValue `addScaled` (stepSize, k3))
   in zipWith
        (+)
        stateValue
        (zipWith4Scaled (stepSize / 6.0) k1 (stepSize / 3.0) k2 (stepSize / 3.0) k3 (stepSize / 6.0) k4)

dormandPrinceStep :: Double -> ODESystem -> Double -> [Double] -> ([Double], Double)
dormandPrinceStep stepSize system timeValue stateValue =
  let k1 = system timeValue stateValue
      k2 = system (timeValue + stepSize * (1.0 / 5.0)) (stateValue `addScaled` (stepSize * (1.0 / 5.0), k1))
      k3 =
        system
          (timeValue + stepSize * (3.0 / 10.0))
          (combineState stateValue stepSize [(3.0 / 40.0, k1), (9.0 / 40.0, k2)])
      k4 =
        system
          (timeValue + stepSize * (4.0 / 5.0))
          (combineState stateValue stepSize [(44.0 / 45.0, k1), (-56.0 / 15.0, k2), (32.0 / 9.0, k3)])
      k5 =
        system
          (timeValue + stepSize * (8.0 / 9.0))
          (combineState stateValue stepSize [(19372.0 / 6561.0, k1), (-25360.0 / 2187.0, k2), (64448.0 / 6561.0, k3), (-212.0 / 729.0, k4)])
      k6 =
        system
          (timeValue + stepSize)
          (combineState stateValue stepSize [(9017.0 / 3168.0, k1), (-355.0 / 33.0, k2), (46732.0 / 5247.0, k3), (49.0 / 176.0, k4), (-5103.0 / 18656.0, k5)])
      k7 =
        system
          (timeValue + stepSize)
          (combineState stateValue stepSize [(35.0 / 384.0, k1), (500.0 / 1113.0, k3), (125.0 / 192.0, k4), (-2187.0 / 6784.0, k5), (11.0 / 84.0, k6)])
      fifthOrder =
        combineState stateValue stepSize [(35.0 / 384.0, k1), (500.0 / 1113.0, k3), (125.0 / 192.0, k4), (-2187.0 / 6784.0, k5), (11.0 / 84.0, k6)]
      fourthOrder =
        combineState stateValue stepSize [(5179.0 / 57600.0, k1), (7571.0 / 16695.0, k3), (393.0 / 640.0, k4), (-92097.0 / 339200.0, k5), (187.0 / 2100.0, k6), (1.0 / 40.0, k7)]
   in (fifthOrder, infinityNorm (zipWith (-) fifthOrder fourthOrder))

integrateRK4 :: Double -> Double -> Double -> ODESystem -> [Double] -> [ODEStep]
integrateRK4 stepSize startTime endTime system initialState =
  advance startTime initialState
  where
    advance timeValue stateValue
      | timeValue >= endTime = [ODEStep endTime stateValue]
      | otherwise =
          let actualStep = min stepSize (endTime - timeValue)
              nextState = rk4Step actualStep system timeValue stateValue
           in ODEStep timeValue stateValue : advance (timeValue + actualStep) nextState

integrateAdaptive :: StepSizeControl -> Double -> Double -> ODESystem -> [Double] -> [ODEStep]
integrateAdaptive control startTime endTime system initialState =
  advance startTime initialState (clampStep (initialStepSize control))
  where
    advance timeValue stateValue proposedStep
      | timeValue >= endTime = [ODEStep endTime stateValue]
      | otherwise =
          let actualStep = min proposedStep (endTime - timeValue)
              (nextState, localError) = dormandPrinceStep actualStep system timeValue stateValue
              accepted = localError <= errorTolerance control || actualStep <= minimumStepSize control
              nextStep = suggestStep actualStep localError
           in if accepted
                then ODEStep timeValue stateValue : advance (timeValue + actualStep) nextState nextStep
                else advance timeValue stateValue nextStep

    suggestStep stepSize localError =
      let normalizedError = max localError 1.0e-12
          rawScale = safetyFactor control * (errorTolerance control / normalizedError) ** (1.0 / 5.0)
       in clampStep (stepSize * min 5.0 (max 0.1 rawScale))

    clampStep = min (maximumStepSize control) . max (minimumStepSize control)

addScaled :: [Double] -> (Double, [Double]) -> [Double]
addScaled stateValue (scaleValue, directionValue) =
  zipWith (+) stateValue (map (scaleValue *) directionValue)

combineState :: [Double] -> Double -> [(Double, [Double])] -> [Double]
combineState stateValue stepSize weightedDirections =
  foldr
    (\(weight, directionValue) accumulator -> zipWith (+) accumulator (map ((stepSize * weight) *) directionValue))
    stateValue
    weightedDirections

zipWith4Scaled :: Double -> [Double] -> Double -> [Double] -> Double -> [Double] -> Double -> [Double] -> [Double]
zipWith4Scaled leftScale leftValues leftMidScale leftMidValues rightMidScale rightMidValues rightScale rightValues =
  zipWith4
    (\leftValue leftMidValue rightMidValue rightValue ->
        leftScale * leftValue
          + leftMidScale * leftMidValue
          + rightMidScale * rightMidValue
          + rightScale * rightValue
    )
    leftValues
    leftMidValues
    rightMidValues
    rightValues

zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 combine leftValues midLeftValues midRightValues rightValues =
  case (leftValues, midLeftValues, midRightValues, rightValues) of
    (leftHead : leftTail, midLeftHead : midLeftTail, midRightHead : midRightTail, rightHead : rightTail) ->
      combine leftHead midLeftHead midRightHead rightHead : zipWith4 combine leftTail midLeftTail midRightTail rightTail
    _ -> []

infinityNorm :: [Double] -> Double
infinityNorm = foldr (max . abs) 0.0
