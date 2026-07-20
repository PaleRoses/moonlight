{-# LANGUAGE DerivingStrategies #-}
module Moonlight.Flow.Carrier.Reuse.Internal.Stats
  ( PlanReuseStats (..),
    emptyPlanReuseStats,
    recordRegisteredNew,
    recordExactReuseEmits,
    recordContainmentReuseEmits,
    recordBoundaryRejected,
    recordObstructedProjection,
    recordStaleRejected,
    recordResidualRejected,
  )
where

data PlanReuseStats = PlanReuseStats
  { prsRegisteredNew :: {-# UNPACK #-} !Int,
    prsExactHits :: {-# UNPACK #-} !Int,
    prsContainmentHits :: {-# UNPACK #-} !Int,
    prsLowerBoundEmits :: {-# UNPACK #-} !Int,
    prsExactProjectionEmits :: {-# UNPACK #-} !Int,
    prsObstructedProjections :: {-# UNPACK #-} !Int,
    prsStaleRejected :: {-# UNPACK #-} !Int,
    prsResidualRejected :: {-# UNPACK #-} !Int,
    prsBoundaryRejected :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show, Read)

emptyPlanReuseStats :: PlanReuseStats
emptyPlanReuseStats =
  PlanReuseStats
    { prsRegisteredNew = 0,
      prsExactHits = 0,
      prsContainmentHits = 0,
      prsLowerBoundEmits = 0,
      prsExactProjectionEmits = 0,
      prsObstructedProjections = 0,
      prsStaleRejected = 0,
      prsResidualRejected = 0,
      prsBoundaryRejected = 0
    }
{-# INLINE emptyPlanReuseStats #-}

recordRegisteredNew :: Int -> PlanReuseStats -> PlanReuseStats
recordRegisteredNew count stats =
  stats
    { prsRegisteredNew = prsRegisteredNew stats + max 0 count
    }
{-# INLINE recordRegisteredNew #-}

recordExactReuseEmits :: Int -> PlanReuseStats -> PlanReuseStats
recordExactReuseEmits emitCount stats =
  stats
    { prsExactHits = prsExactHits stats + 1,
      prsExactProjectionEmits = prsExactProjectionEmits stats + emitCount
    }
{-# INLINE recordExactReuseEmits #-}

recordContainmentReuseEmits :: Int -> PlanReuseStats -> PlanReuseStats
recordContainmentReuseEmits emitCount stats =
  stats
    { prsContainmentHits = prsContainmentHits stats + 1,
      prsLowerBoundEmits = prsLowerBoundEmits stats + emitCount
    }
{-# INLINE recordContainmentReuseEmits #-}

recordBoundaryRejected :: Int -> PlanReuseStats -> PlanReuseStats
recordBoundaryRejected count stats =
  stats
    { prsBoundaryRejected = prsBoundaryRejected stats + max 0 count
    }
{-# INLINE recordBoundaryRejected #-}

recordObstructedProjection :: Int -> PlanReuseStats -> PlanReuseStats
recordObstructedProjection count stats =
  stats
    { prsObstructedProjections = prsObstructedProjections stats + max 0 count
    }
{-# INLINE recordObstructedProjection #-}

recordStaleRejected :: Int -> PlanReuseStats -> PlanReuseStats
recordStaleRejected count stats =
  stats
    { prsStaleRejected = prsStaleRejected stats + max 0 count
    }
{-# INLINE recordStaleRejected #-}

recordResidualRejected :: Int -> PlanReuseStats -> PlanReuseStats
recordResidualRejected count stats =
  stats
    { prsResidualRejected = prsResidualRejected stats + max 0 count
    }
{-# INLINE recordResidualRejected #-}
