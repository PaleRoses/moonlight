{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Rewrite.Relational.Limits
  ( RewriteRunMetric (..),
    Limit (..),
    Count (..),
    RewriteRunMetrics (..),
    RewriteRunLimits,
    noRewriteRunLimits,
    defaultRewriteRunLimits,
    limitToBoundedInt,
    limitToOverflowSentinel,
    RewriteRunStats,
    emptyRewriteRunStats,
    RewriteRunLimit (..),
    checkRewriteRunLimits,
    appendRewriteRunStats,
    statsForMatches,
    statsForExists,
    statsForSupport,
    statsForRounds,
    statsForRewriteApplications,
  )
where

import Data.Functor.Barbie
  ( ApplicativeB (..),
    FunctorB (..),
    TraversableB (..),
    bfoldMap,
    bzipWith,
    bzipWith3,
  )
import Data.Functor.Product (Product (..))
import Data.Kind (Type)
import Data.Monoid (First (..))
import Moonlight.Flow.Storage.View
  ( SupportIds,
  )
import Moonlight.Flow.Execution.Prepared.Run
  ( supportRowCount,
  )
import Moonlight.Control.Count
  ( naturalToBoundedInt,
  )
import Numeric.Natural
  ( Natural,
  )

type RewriteRunMetric :: Type
data RewriteRunMetric
  = ResultRows
  | SupportRows
  | RewriteApplications
  | Rounds

type Limit :: RewriteRunMetric -> Type
newtype Limit metric = Limit
  { unLimit :: Maybe Natural
  }
  deriving stock (Eq, Ord, Show, Read)

type Count :: RewriteRunMetric -> Type
newtype Count metric = Count
  { unCount :: Natural
  }
  deriving stock (Eq, Ord, Show, Read)

type RewriteRunMetrics :: (RewriteRunMetric -> Type) -> Type
data RewriteRunMetrics value = RewriteRunMetrics
  { rrmResultRows :: !(value 'ResultRows),
    rrmSupportRows :: !(value 'SupportRows),
    rrmRewriteApplications :: !(value 'RewriteApplications),
    rrmRounds :: !(value 'Rounds)
  }

instance FunctorB RewriteRunMetrics where
  bmap transform metrics =
    RewriteRunMetrics
      { rrmResultRows = transform (rrmResultRows metrics),
        rrmSupportRows = transform (rrmSupportRows metrics),
        rrmRewriteApplications = transform (rrmRewriteApplications metrics),
        rrmRounds = transform (rrmRounds metrics)
      }

instance TraversableB RewriteRunMetrics where
  btraverse transform metrics =
    RewriteRunMetrics
      <$> transform (rrmResultRows metrics)
      <*> transform (rrmSupportRows metrics)
      <*> transform (rrmRewriteApplications metrics)
      <*> transform (rrmRounds metrics)

instance ApplicativeB RewriteRunMetrics where
  bpure metricValue =
    RewriteRunMetrics
      { rrmResultRows = metricValue,
        rrmSupportRows = metricValue,
        rrmRewriteApplications = metricValue,
        rrmRounds = metricValue
      }

  bprod leftMetrics rightMetrics =
    RewriteRunMetrics
      { rrmResultRows = Pair (rrmResultRows leftMetrics) (rrmResultRows rightMetrics),
        rrmSupportRows = Pair (rrmSupportRows leftMetrics) (rrmSupportRows rightMetrics),
        rrmRewriteApplications = Pair (rrmRewriteApplications leftMetrics) (rrmRewriteApplications rightMetrics),
        rrmRounds = Pair (rrmRounds leftMetrics) (rrmRounds rightMetrics)
      }

type RewriteRunLimits :: Type
type RewriteRunLimits = RewriteRunMetrics Limit

type RewriteRunStats :: Type
type RewriteRunStats = RewriteRunMetrics Count

deriving stock instance Eq RewriteRunLimits

deriving stock instance Ord RewriteRunLimits

deriving stock instance Show RewriteRunLimits

deriving stock instance Read RewriteRunLimits

deriving stock instance Eq RewriteRunStats

deriving stock instance Ord RewriteRunStats

deriving stock instance Show RewriteRunStats

deriving stock instance Read RewriteRunStats

noRewriteRunLimits :: RewriteRunLimits
noRewriteRunLimits =
  bpure (Limit Nothing)

defaultRewriteRunLimits :: RewriteRunLimits
defaultRewriteRunLimits =
  RewriteRunMetrics
    { rrmResultRows = Limit (Just 1000000),
      rrmSupportRows = Limit (Just 5000000),
      rrmRewriteApplications = Limit (Just 100000),
      rrmRounds = Limit (Just 1024)
    }

limitToBoundedInt :: Limit metric -> Int
limitToBoundedInt limit =
  case limit of
    Limit Nothing ->
      maxBound

    Limit (Just bound) ->
      naturalToBoundedInt bound
{-# INLINE limitToBoundedInt #-}

limitToOverflowSentinel :: Limit metric -> Maybe Int
limitToOverflowSentinel limit =
  case limit of
    Limit Nothing ->
      Nothing

    Limit (Just bound) ->
      Just (naturalToBoundedInt (bound + 1))
{-# INLINE limitToOverflowSentinel #-}

emptyRewriteRunStats :: RewriteRunStats
emptyRewriteRunStats =
  bpure (Count 0)

appendRewriteRunStats :: RewriteRunStats -> RewriteRunStats -> RewriteRunStats
appendRewriteRunStats =
  bzipWith appendCount
  where
    appendCount :: Count metric -> Count metric -> Count metric
    appendCount (Count leftCount) (Count rightCount) =
      Count (leftCount + rightCount)

type RewriteRunLimit :: Type
data RewriteRunLimit
  = MaxResultRowsExceeded !Natural
  | MaxSupportRowsExceeded !Natural
  | MaxRewriteApplicationsExceeded !Natural
  | MaxRoundsExceeded !Natural
  deriving stock (Eq, Ord, Show, Read)

checkRewriteRunLimits ::
  RewriteRunLimits ->
  RewriteRunStats ->
  Either (RewriteRunLimit, RewriteRunStats) ()
checkRewriteRunLimits limits stats =
  case getFirst (bfoldMap (First . unLimitResult) limitResults) of
    Nothing ->
      Right ()
    Just exceeded ->
      Left (exceeded, stats)
  where
    limitResults =
      bzipWith3 runRewriteRunLimitCheck rewriteRunLimitChecks limits stats

statsForMatches :: [match] -> RewriteRunStats
statsForMatches matches =
  emptyRewriteRunStats
    { rrmResultRows = Count (fromIntegral (length matches))
    }

statsForExists :: Bool -> RewriteRunStats
statsForExists _ =
  emptyRewriteRunStats
    { rrmResultRows = Count 1
    }

statsForSupport :: SupportIds -> RewriteRunStats
statsForSupport supportIds =
  emptyRewriteRunStats
    { rrmSupportRows = Count (fromIntegral (supportRowCount supportIds))
    }

statsForRounds :: Natural -> RewriteRunStats
statsForRounds rounds =
  emptyRewriteRunStats
    { rrmRounds = Count rounds
    }

statsForRewriteApplications :: Natural -> RewriteRunStats
statsForRewriteApplications applicationCount =
  emptyRewriteRunStats
    { rrmRewriteApplications = Count applicationCount
    }

type RewriteRunLimitCheck :: RewriteRunMetric -> Type
newtype RewriteRunLimitCheck metric = RewriteRunLimitCheck
  { runRewriteRunLimitCheck ::
      Limit metric ->
      Count metric ->
      LimitResult metric
  }

type LimitResult :: RewriteRunMetric -> Type
newtype LimitResult metric = LimitResult
  { unLimitResult :: Maybe RewriteRunLimit
  }

rewriteRunLimitChecks :: RewriteRunMetrics RewriteRunLimitCheck
rewriteRunLimitChecks =
  RewriteRunMetrics
    { rrmResultRows = RewriteRunLimitCheck (exceededNatural MaxResultRowsExceeded),
      rrmSupportRows = RewriteRunLimitCheck (exceededNatural MaxSupportRowsExceeded),
      rrmRewriteApplications = RewriteRunLimitCheck (exceededNatural MaxRewriteApplicationsExceeded),
      rrmRounds = RewriteRunLimitCheck (exceededNatural MaxRoundsExceeded)
    }

exceededNatural ::
  (Natural -> RewriteRunLimit) ->
  Limit metric ->
  Count metric ->
  LimitResult metric
exceededNatural _ (Limit Nothing) _ =
  LimitResult Nothing
exceededNatural buildLimit (Limit (Just bound)) (Count observed)
  | observed > bound =
      LimitResult (Just (buildLimit bound))
  | otherwise =
      LimitResult Nothing
