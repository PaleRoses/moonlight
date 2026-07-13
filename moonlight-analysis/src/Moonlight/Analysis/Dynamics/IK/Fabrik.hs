module Moonlight.Analysis.Dynamics.IK.Fabrik
  ( IKChain (..),
    mkIKChain,
    solveFabrik,
    endEffector,
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.LinAlg.Geometry (Vec3 (..), addVec3, magnitudeVec3, normalizeVec3Safe, scaleVec3, subVec3)

type IKChain :: Type
data IKChain = IKChain
  { ikRoot :: Vec3,
    ikJoints :: NonEmpty Vec3
  }
  deriving stock (Eq, Show)

mkIKChain :: NonEmpty Vec3 -> IKChain
mkIKChain jointsValue =
  IKChain
    { ikRoot = NonEmpty.head jointsValue,
      ikJoints = jointsValue
    }

endEffector :: IKChain -> Vec3
endEffector = NonEmpty.last . ikJoints

solveFabrik :: Int -> Double -> Vec3 -> IKChain -> IKChain
solveFabrik maxRounds tolerance targetValue chainValue =
  let segmentLengths = segmentLengthsOf (ikJoints chainValue)
      totalReach = sum segmentLengths
      rootValue = ikRoot chainValue
   in if distance rootValue targetValue >= totalReach
        then chainValue {ikJoints = stretchToward rootValue targetValue segmentLengths}
        else iterateSolve maxRounds tolerance targetValue segmentLengths chainValue

iterateSolve :: Int -> Double -> Vec3 -> [Double] -> IKChain -> IKChain
iterateSolve remainingRounds tolerance targetValue segmentLengths chainValue
  | remainingRounds <= 0 = chainValue
  | distance (endEffector chainValue) targetValue <= tolerance = chainValue
  | otherwise =
       let backwardSolved = backwardPass targetValue segmentLengths (NonEmpty.toList (ikJoints chainValue))
           forwardSolved = forwardPass (ikRoot chainValue) segmentLengths backwardSolved
       in case nonEmptyChain forwardSolved of
            Just jointsValue ->
              iterateSolve (remainingRounds - 1) tolerance targetValue segmentLengths (chainValue {ikJoints = jointsValue})
            Nothing ->
              chainValue

segmentLengthsOf :: NonEmpty Vec3 -> [Double]
segmentLengthsOf jointsValue =
  case NonEmpty.toList jointsValue of
    leftValue : rightValue : remainingValues ->
      distance leftValue rightValue : segmentLengthsOf (rightValue :| remainingValues)
    _ ->
      []

stretchToward :: Vec3 -> Vec3 -> [Double] -> NonEmpty Vec3
stretchToward rootValue targetValue segmentLengths =
  let direction = normalizeVec3Safe (subVec3 targetValue rootValue)
      stretchedTail = snd (foldlStretch (rootValue, [rootValue]) segmentLengths direction)
   in case nonEmptyChain stretchedTail of
        Just chainJoints -> chainJoints
        Nothing -> rootValue :| []

foldlStretch :: (Vec3, [Vec3]) -> [Double] -> Vec3 -> (Vec3, [Vec3])
foldlStretch (startPoint, initialPts) lengthsValue direction =
  let (finalPoint, revPts) = go (startPoint, reverse initialPts) lengthsValue
   in (finalPoint, reverse revPts)
  where
    go acc [] = acc
    go (currentPoint, revPoints) (lengthValue : remainingLengths) =
      let nextPoint = addVec3 currentPoint (scaleVec3 lengthValue direction)
       in go (nextPoint, nextPoint : revPoints) remainingLengths

backwardPass :: Vec3 -> [Double] -> [Vec3] -> [Vec3]
backwardPass targetValue segmentLengths jointsValue =
  backwardAccumulate targetValue (reverse segmentLengths) (reverse (dropLast jointsValue)) [targetValue]

backwardAccumulate :: Vec3 -> [Double] -> [Vec3] -> [Vec3] -> [Vec3]
backwardAccumulate currentPoint lengthsValue remainingJoints acc =
  case (lengthsValue, remainingJoints) of
    (lengthValue : remainingLengths, jointValue : remainingValues) ->
      let nextPoint = placeAtDistance jointValue currentPoint lengthValue
       in backwardAccumulate nextPoint remainingLengths remainingValues (nextPoint : acc)
    _ ->
      acc

forwardPass :: Vec3 -> [Double] -> [Vec3] -> [Vec3]
forwardPass rootValue segmentLengths jointsValue =
  rootValue : forwardAccumulate rootValue segmentLengths (drop 1 jointsValue)

forwardAccumulate :: Vec3 -> [Double] -> [Vec3] -> [Vec3]
forwardAccumulate currentPoint lengthsValue remainingJoints =
  case (lengthsValue, remainingJoints) of
    (lengthValue : remainingLengths, jointValue : remainingValues) ->
      let nextPoint = placeAtDistance jointValue currentPoint lengthValue
       in nextPoint : forwardAccumulate nextPoint remainingLengths remainingValues
    _ ->
      []

placeAtDistance :: Vec3 -> Vec3 -> Double -> Vec3
placeAtDistance movingPoint anchorPoint distanceValue =
  let direction = normalizeVec3Safe (subVec3 movingPoint anchorPoint)
   in addVec3 anchorPoint (scaleVec3 distanceValue direction)

nonEmptyChain :: [Vec3] -> Maybe (NonEmpty Vec3)
nonEmptyChain values =
  case values of
    headValue : tailValues -> Just (headValue :| tailValues)
    [] -> Nothing

dropLast :: [a] -> [a]
dropLast values =
  case values of
    [] -> []
    [_] -> []
    headValue : tailValues -> headValue : dropLast tailValues

distance :: Vec3 -> Vec3 -> Double
distance leftValue rightValue = magnitudeVec3 (subVec3 leftValue rightValue)
