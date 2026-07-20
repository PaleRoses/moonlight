{-# LANGUAGE BangPatterns #-}

module Moonlight.Control.Count
  ( WorkCount (..),
    WorkCoverage (..),
    workCountZero,
    workCountExact,
    workCountAtLeast,
    workCountUnknown,
    workCountFromInt,
    workCountFromNatural,
    workCountToMaybeExact,
    workCountLowerBound,
    workCountLowerBoundToBoundedInt,
    workCountKnownZero,
    workCountKnownPositive,
    workCountMayBePositive,
    workCountMinusExactLowerBound,
    workCoverageFromRemaining,
    naturalToBoundedInt,
    SuppressionCounts,
    emptySuppressionCounts,
    singletonSuppressionCounts,
    observedRoundCount,
    cooldownSuppressedRoundCount,
    suppressionMatchedCount,
    suppressionScheduledCount,
    suppressionSuppressedCount,
    anySuppressed,
    anyCooldownSuppressed,
  )
where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Numeric.Natural (Natural)

data WorkCount
  = WorkCountExact !Natural
  | WorkCountAtLeast !Natural
  | WorkCountUnknown
  deriving stock (Eq, Show, Read)

instance Semigroup WorkCount where
  (<>) = workCountPlus

instance Monoid WorkCount where
  mempty = workCountZero

data WorkCoverage
  = WorkCoverageComplete
  | WorkCoveragePartial
  | WorkCoverageUnknown
  deriving stock (Eq, Show, Read)

instance Semigroup WorkCoverage where
  WorkCoverageUnknown <> _ = WorkCoverageUnknown
  _ <> WorkCoverageUnknown = WorkCoverageUnknown
  WorkCoveragePartial <> _ = WorkCoveragePartial
  _ <> WorkCoveragePartial = WorkCoveragePartial
  WorkCoverageComplete <> WorkCoverageComplete = WorkCoverageComplete

instance Monoid WorkCoverage where
  mempty = WorkCoverageComplete

workCountZero :: WorkCount
workCountZero = WorkCountExact 0

workCountExact :: Natural -> WorkCount
workCountExact = WorkCountExact

workCountAtLeast :: Natural -> WorkCount
workCountAtLeast lowerBound
  | lowerBound == 0 = WorkCountUnknown
  | otherwise = WorkCountAtLeast lowerBound

workCountUnknown :: WorkCount
workCountUnknown = WorkCountUnknown

workCountFromInt :: Int -> WorkCount
workCountFromInt = WorkCountExact . fromIntegral . max 0

workCountFromNatural :: Natural -> WorkCount
workCountFromNatural = WorkCountExact

workCountToMaybeExact :: WorkCount -> Maybe Natural
workCountToMaybeExact count =
  case count of
    WorkCountExact exactValue -> Just exactValue
    WorkCountAtLeast _lowerBound -> Nothing
    WorkCountUnknown -> Nothing

workCountLowerBound :: WorkCount -> Natural
workCountLowerBound count =
  case count of
    WorkCountExact exactValue -> exactValue
    WorkCountAtLeast lowerBound -> lowerBound
    WorkCountUnknown -> 0

workCountLowerBoundToBoundedInt :: WorkCount -> Int
workCountLowerBoundToBoundedInt = naturalToBoundedInt . workCountLowerBound

workCountKnownZero :: WorkCount -> Bool
workCountKnownZero count =
  case count of
    WorkCountExact 0 -> True
    WorkCountExact _positive -> False
    WorkCountAtLeast _lowerBound -> False
    WorkCountUnknown -> False

workCountKnownPositive :: WorkCount -> Bool
workCountKnownPositive count =
  case count of
    WorkCountExact exactValue -> exactValue > 0
    WorkCountAtLeast lowerBound -> lowerBound > 0
    WorkCountUnknown -> False

workCountMayBePositive :: WorkCount -> Bool
workCountMayBePositive count =
  case count of
    WorkCountExact 0 -> False
    WorkCountExact _positive -> True
    WorkCountAtLeast _lowerBound -> True
    WorkCountUnknown -> True

workCountPlus :: WorkCount -> WorkCount -> WorkCount
workCountPlus leftCount rightCount =
  case (leftCount, rightCount) of
    (WorkCountUnknown, _) -> WorkCountUnknown
    (_, WorkCountUnknown) -> WorkCountUnknown
    (WorkCountExact leftExact, WorkCountExact rightExact) ->
      WorkCountExact (leftExact + rightExact)
    (WorkCountExact leftExact, WorkCountAtLeast rightLower) ->
      workCountAtLeast (leftExact + rightLower)
    (WorkCountAtLeast leftLower, WorkCountExact rightExact) ->
      workCountAtLeast (leftLower + rightExact)
    (WorkCountAtLeast leftLower, WorkCountAtLeast rightLower) ->
      workCountAtLeast (leftLower + rightLower)

workCountMinusExactLowerBound :: WorkCount -> Natural -> WorkCount
workCountMinusExactLowerBound count exactSubtrahend =
  case count of
    WorkCountExact exactValue ->
      WorkCountExact (saturatingSubtract exactValue exactSubtrahend)
    WorkCountAtLeast lowerBound ->
      workCountAtLeast (saturatingSubtract lowerBound exactSubtrahend)
    WorkCountUnknown ->
      WorkCountUnknown

saturatingSubtract :: Natural -> Natural -> Natural
saturatingSubtract leftValue rightValue =
  if leftValue <= rightValue
    then 0
    else leftValue - rightValue

workCoverageFromRemaining :: WorkCount -> WorkCoverage
workCoverageFromRemaining remainingCount =
  case remainingCount of
    WorkCountExact 0 -> WorkCoverageComplete
    WorkCountExact _positive -> WorkCoveragePartial
    WorkCountAtLeast 0 -> WorkCoverageUnknown
    WorkCountAtLeast _positive -> WorkCoveragePartial
    WorkCountUnknown -> WorkCoverageUnknown

naturalToBoundedInt :: Natural -> Int
naturalToBoundedInt naturalValue =
  fromInteger (min (toInteger (maxBound :: Int)) (toInteger naturalValue))

data SuppressionCounts = SuppressionCounts
  { scObservedRounds :: !IntSet,
    scMatchedCount :: !WorkCount,
    scScheduledCount :: !Natural,
    scSuppressedCount :: !WorkCount,
    scCooldownSuppressedRounds :: !IntSet
  }
  deriving stock (Eq, Show)

instance Semigroup SuppressionCounts where
  leftCounts <> rightCounts =
    SuppressionCounts
      { scObservedRounds =
          IntSet.union
            (scObservedRounds leftCounts)
            (scObservedRounds rightCounts),
        scMatchedCount =
          scMatchedCount leftCounts <> scMatchedCount rightCounts,
        scScheduledCount =
          scScheduledCount leftCounts + scScheduledCount rightCounts,
        scSuppressedCount =
          scSuppressedCount leftCounts <> scSuppressedCount rightCounts,
        scCooldownSuppressedRounds =
          IntSet.union
            (scCooldownSuppressedRounds leftCounts)
            (scCooldownSuppressedRounds rightCounts)
      }

instance Monoid SuppressionCounts where
  mempty =
    SuppressionCounts
      { scObservedRounds = IntSet.empty,
        scMatchedCount = workCountZero,
        scScheduledCount = 0,
        scSuppressedCount = workCountZero,
        scCooldownSuppressedRounds = IntSet.empty
      }

emptySuppressionCounts :: SuppressionCounts
emptySuppressionCounts = mempty

singletonSuppressionCounts ::
  Int ->
  WorkCount ->
  Natural ->
  WorkCount ->
  Bool ->
  SuppressionCounts
singletonSuppressionCounts roundIndex matchedCount scheduledCount suppressedCount suppressedByCooldown =
  SuppressionCounts
    { scObservedRounds = IntSet.singleton roundIndex,
      scMatchedCount = matchedCount,
      scScheduledCount = scheduledCount,
      scSuppressedCount = suppressedCount,
      scCooldownSuppressedRounds =
        if suppressedByCooldown && workCountMayBePositive suppressedCount
          then IntSet.singleton roundIndex
          else IntSet.empty
    }

observedRoundCount :: SuppressionCounts -> Int
observedRoundCount = IntSet.size . scObservedRounds

cooldownSuppressedRoundCount :: SuppressionCounts -> Int
cooldownSuppressedRoundCount = IntSet.size . scCooldownSuppressedRounds

suppressionMatchedCount :: SuppressionCounts -> WorkCount
suppressionMatchedCount = scMatchedCount

suppressionScheduledCount :: SuppressionCounts -> Natural
suppressionScheduledCount = scScheduledCount

suppressionSuppressedCount :: SuppressionCounts -> WorkCount
suppressionSuppressedCount = scSuppressedCount

anySuppressed :: SuppressionCounts -> Bool
anySuppressed = workCountMayBePositive . scSuppressedCount

anyCooldownSuppressed :: SuppressionCounts -> Bool
anyCooldownSuppressed = not . IntSet.null . scCooldownSuppressedRounds
