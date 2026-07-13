{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Saturation.Context.Runtime.Match.Pipeline
  ( CandidatePipelineStage (..),
    CandidatePipelineCounts (..),
    emptyCandidatePipelineCounts,
    singletonCandidatePipelineCount,
    candidatePipelineCount,
    candidatePipelineGroupCount,
    candidatePipelineIncrement,
    candidatePipelineIncrementGroup,
    candidatePipelineFromNonNegativeInt,
    nonNegativeDifference,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Numeric.Natural (Natural)

type CandidatePipelineStage :: Type
data CandidatePipelineStage
  = CandidateEligibleBase
  | CandidateEligibleContext
  | CandidateEligibleAggregated
  | CandidateDroppedByGuidance
  | CandidateGuided
  | CandidateRejectedByAdmission
  | CandidateDeferredByBudget
  | CandidateAdmitted
  | CandidateScheduledBeforeValidation
  | CandidateNotSelectedByScheduler
  | CandidateRejectedByValidation
  | CandidateScheduled
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

type CandidatePipelineCounts :: Type -> Type
data CandidatePipelineCounts group = CandidatePipelineCounts
  { cpcGlobalCounts :: !(Map CandidatePipelineStage Natural),
    cpcGroupCounts :: !(Map group (Map CandidatePipelineStage Natural))
  }
  deriving stock (Eq, Ord, Show, Read)

instance Ord group => Semigroup (CandidatePipelineCounts group) where
  leftCounts <> rightCounts =
    CandidatePipelineCounts
      { cpcGlobalCounts =
          Map.unionWith
            (+)
            (cpcGlobalCounts leftCounts)
            (cpcGlobalCounts rightCounts),
        cpcGroupCounts =
          Map.unionWith
            (Map.unionWith (+))
            (cpcGroupCounts leftCounts)
            (cpcGroupCounts rightCounts)
      }

instance Ord group => Monoid (CandidatePipelineCounts group) where
  mempty =
    emptyCandidatePipelineCounts

emptyCandidatePipelineCounts :: CandidatePipelineCounts group
emptyCandidatePipelineCounts =
  CandidatePipelineCounts
    { cpcGlobalCounts = Map.empty,
      cpcGroupCounts = Map.empty
    }
{-# INLINE emptyCandidatePipelineCounts #-}

singletonCandidatePipelineCount ::
  CandidatePipelineStage ->
  Int ->
  CandidatePipelineCounts group
singletonCandidatePipelineCount stage rawCount =
  candidatePipelineIncrement
    stage
    rawCount
    emptyCandidatePipelineCounts
{-# INLINE singletonCandidatePipelineCount #-}

candidatePipelineCount ::
  CandidatePipelineStage ->
  CandidatePipelineCounts group ->
  Natural
candidatePipelineCount stage =
  Map.findWithDefault 0 stage . cpcGlobalCounts
{-# INLINE candidatePipelineCount #-}

candidatePipelineGroupCount ::
  Ord group =>
  group ->
  CandidatePipelineStage ->
  CandidatePipelineCounts group ->
  Natural
candidatePipelineGroupCount group stage counts =
  maybe
    0
    (Map.findWithDefault 0 stage)
    (Map.lookup group (cpcGroupCounts counts))
{-# INLINE candidatePipelineGroupCount #-}

candidatePipelineIncrement ::
  CandidatePipelineStage ->
  Int ->
  CandidatePipelineCounts group ->
  CandidatePipelineCounts group
candidatePipelineIncrement stage rawCount counts
  | rawCount <= 0 =
      counts
  | otherwise =
      counts
        { cpcGlobalCounts =
            Map.insertWith
              (+)
              stage
              (fromIntegral rawCount)
              (cpcGlobalCounts counts)
        }
{-# INLINE candidatePipelineIncrement #-}

candidatePipelineIncrementGroup ::
  Ord group =>
  group ->
  CandidatePipelineStage ->
  Int ->
  CandidatePipelineCounts group ->
  CandidatePipelineCounts group
candidatePipelineIncrementGroup group stage rawCount counts
  | rawCount <= 0 =
      counts
  | otherwise =
      counts
        { cpcGroupCounts =
            Map.insertWith
              (Map.unionWith (+))
              group
              (Map.singleton stage (fromIntegral rawCount))
              (cpcGroupCounts counts)
        }
{-# INLINE candidatePipelineIncrementGroup #-}

candidatePipelineFromNonNegativeInt :: Int -> Natural
candidatePipelineFromNonNegativeInt rawCount
  | rawCount <= 0 =
      0
  | otherwise =
      fromIntegral rawCount
{-# INLINE candidatePipelineFromNonNegativeInt #-}

nonNegativeDifference :: Int -> Int -> Int
nonNegativeDifference leftCount rightCount =
  max 0 (leftCount - rightCount)
{-# INLINE nonNegativeDifference #-}
