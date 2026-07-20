{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Query.Run
  ( FixedPointBound,
    FixedPointBoundError (..),
    fixedPointBound,
    fixedPointBoundIterations,
    defaultFixedPointBound,
    QuerySettle (..),
    QueryExecution,
    once,
    fixedPoint,
    boundedFixedPoint,
    queryExecutionPlan,
    queryExecutionSettle,
    QueryExecutionError (..),
    settleQueryExecution,
    readQueryRows,
    readQueryRowsFold,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Kind
  ( Type,
  )
import Moonlight.Delta.Signed
  ( Multiplicity
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )
import Moonlight.Flow.Read
  ( ReadError,
    Rows,
    readRows,
    readRowsFold,
  )
import Moonlight.Flow.Runtime.Settle qualified as Runtime
import Moonlight.Flow.Runtime.Types qualified as Runtime
import Moonlight.Flow.Runtime.Spec.Schema
  ( RuntimePlan,
  )

type FixedPointBound :: Type
newtype FixedPointBound = FixedPointBound
  { fixedPointBoundIterations :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

type FixedPointBoundError :: Type
newtype FixedPointBoundError = NonPositiveFixedPointBound Int
  deriving stock (Eq, Ord, Show, Read)

fixedPointBound :: Int -> Either FixedPointBoundError FixedPointBound
fixedPointBound iterationLimit
  | iterationLimit <= 0 =
      Left (NonPositiveFixedPointBound iterationLimit)
  | otherwise =
      Right (FixedPointBound iterationLimit)

defaultFixedPointBound :: FixedPointBound
defaultFixedPointBound =
  FixedPointBound 64

type QuerySettle :: Type
data QuerySettle
  = QueryOnce
  | QueryFixedPoint !FixedPointBound
  deriving stock (Eq, Ord, Show, Read)

type QueryExecution :: Type -> Type -> Type
data QueryExecution ctx prop = QueryExecution
  { qePlan :: !(RuntimePlan ctx prop),
    qeSettle :: !QuerySettle
  }

once ::
  RuntimePlan ctx prop ->
  QueryExecution ctx prop
once planValue =
  QueryExecution
    { qePlan = planValue,
      qeSettle = QueryOnce
    }

fixedPoint ::
  FixedPointBound ->
  RuntimePlan ctx prop ->
  QueryExecution ctx prop
fixedPoint bound planValue =
  QueryExecution
    { qePlan = planValue,
      qeSettle = QueryFixedPoint bound
    }

boundedFixedPoint ::
  Int ->
  RuntimePlan ctx prop ->
  Either FixedPointBoundError (QueryExecution ctx prop)
boundedFixedPoint iterationLimit planValue =
  (`fixedPoint` planValue) <$> fixedPointBound iterationLimit

queryExecutionPlan ::
  QueryExecution ctx prop ->
  RuntimePlan ctx prop
queryExecutionPlan =
  qePlan

queryExecutionSettle ::
  QueryExecution ctx prop ->
  QuerySettle
queryExecutionSettle =
  qeSettle

type QueryExecutionError :: Type -> Type -> Type
data QueryExecutionError ctx prop
  = QueryExecutionApplyFailed !(Runtime.RuntimeApplyError ctx prop)
  | QueryExecutionReadFailed !(ReadError ctx prop)

deriving stock instance
  (Show ctx, Show prop) =>
  Show (QueryExecutionError ctx prop)

settleQueryExecution ::
  QueryExecution ctx prop ->
  Runtime.Runtime ctx prop ->
  Either (Runtime.RuntimeApplyError ctx prop) (Runtime.Runtime ctx prop)
settleQueryExecution execution runtime =
  case qeSettle execution of
    QueryOnce ->
      Right runtime
    QueryFixedPoint bound ->
      Runtime.settleRuntimeFixedPointBounded
        (fixedPointBoundIterations bound)
        runtime

readQueryRows ::
  (Ord ctx, Ord prop) =>
  QueryExecution ctx prop ->
  Runtime.Runtime ctx prop ->
  Either (QueryExecutionError ctx prop) Rows
readQueryRows execution runtime = do
  settledRuntime <-
    first QueryExecutionApplyFailed $
      settleQueryExecution execution runtime
  first QueryExecutionReadFailed $
    readRows
      (qePlan execution)
      settledRuntime

readQueryRowsFold ::
  (Ord ctx, Ord prop) =>
  QueryExecution ctx prop ->
  Runtime.Runtime ctx prop ->
  result ->
  (RowTupleKey -> Multiplicity -> result -> result) ->
  Either (QueryExecutionError ctx prop) result
readQueryRowsFold execution runtime initial step = do
  settledRuntime <-
    first QueryExecutionApplyFailed $
      settleQueryExecution execution runtime
  first QueryExecutionReadFailed $
    readRowsFold
      (qePlan execution)
      settledRuntime
      initial
      step
