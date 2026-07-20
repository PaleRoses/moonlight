{-# LANGUAGE BangPatterns #-}

module Moonlight.Flow.Runtime.Visible
  ( visibleRows,
    visibleRowsFold,
    visibleRowsOfFamilyFold,
    visibleContext,
    pinVisibleContext,
    unpinVisibleContext,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Functor.Identity
  ( Identity,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( QueryId,
  )
import Moonlight.Flow.Model.Family
  ( SAtomFamily,
    atomIdOf,
    decodeAtomFamilyRow,
    schemaOf,
  )
import Moonlight.Delta.Signed
  ( Multiplicity (..),
    addMultiplicity
  )
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
  )
import Moonlight.Flow.Query qualified as Query
import Moonlight.Flow.Runtime.Carrier.Store qualified as Carrier
import Moonlight.Flow.Runtime.Core.State qualified as Core
import Moonlight.Flow.Runtime.Error
  ( RuntimeError (..),
  )
import Moonlight.Flow.Runtime.Execution.Failure
  ( RelationalRuntimeError,
  )
import Moonlight.Flow.Runtime.Factor.Read
  ( readFactorRows,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RuntimeEnvelope (..),
  )
import Moonlight.Flow.Runtime.Spec.Schema
  ( RuntimePlan (..),
    RuntimePlanProjection (..),
    runtimePlanQueryId,
  )
import Moonlight.Flow.Runtime.Types
  ( Runtime (..),
    RuntimeReadError (..),
    RuntimeSection (..),
  )

visibleContext ::
  (Ord ctx, Ord prop) =>
  ctx ->
  Runtime ctx prop ->
  Either (RuntimeReadError ctx prop) (Runtime ctx prop, RuntimeSection ctx prop)
visibleContext contextValue (Runtime kernel) =
  first runtimeReadErrorFromCarrierError $ do
    (kernelVisible, section) <-
      Carrier.visibleContext contextValue kernel
    pure (Runtime kernelVisible, RuntimeSection section)
{-# INLINE visibleContext #-}

pinVisibleContext ::
  ctx ->
  Runtime ctx prop ->
  Runtime ctx prop
pinVisibleContext contextValue (Runtime kernel) =
  Runtime (Carrier.pinVisibleContext contextValue kernel)
{-# INLINE pinVisibleContext #-}

unpinVisibleContext ::
  ctx ->
  Runtime ctx prop ->
  Runtime ctx prop
unpinVisibleContext contextValue (Runtime kernel) =
  Runtime (Carrier.unpinVisibleContext contextValue kernel)
{-# INLINE unpinVisibleContext #-}

runtimeReadErrorFromCarrierError ::
  RelationalRuntimeError ctx prop boundary evidence ->
  RuntimeReadError ctx prop
runtimeReadErrorFromCarrierError err =
  case err of
    RuntimeMissingIndexRoute addr ->
      RuntimeReadCarrierUnrouted addr
    RuntimeMissingIndexShard shard ->
      RuntimeReadCarrierIndexShardUnavailable shard
    RuntimeMissingCurrentCarrier addr ->
      RuntimeReadCarrierStoreUnavailable addr
    _ ->
      RuntimeReadCarrierRuntimeFailure
{-# INLINE runtimeReadErrorFromCarrierError #-}

visibleRows ::
  (Ord ctx, Ord prop) =>
  RuntimePlan ctx prop ->
  Runtime ctx prop ->
  Either (RuntimeReadError ctx prop) (Map RowTupleKey Multiplicity)
visibleRows plan runtime =
  visibleRowsFold plan runtime Map.empty $ \rowValue multiplicity !acc ->
    Map.insertWith addMultiplicity rowValue multiplicity acc
{-# INLINE visibleRows #-}

visibleRowsFold ::
  (Ord ctx, Ord prop) =>
  RuntimePlan ctx prop ->
  Runtime ctx prop ->
  r ->
  (RowTupleKey -> Multiplicity -> r -> r) ->
  Either (RuntimeReadError ctx prop) r
visibleRowsFold plan (Runtime kernel) initial step =
  let queryId =
        runtimePlanQueryId plan
      state =
        rdrState kernel
   in if not (Core.runtimeSeedStateSettled (Core.rsSeedState state))
        then Left (RuntimeReadSeedPending queryId)
        else readFactorRows plan kernel initial step
{-# INLINE visibleRowsFold #-}

visibleRowsOfFamilyFold ::
  (Ord ctx, Ord prop) =>
  SAtomFamily atomFamily ->
  RuntimePlan ctx prop ->
  Runtime ctx prop ->
  r ->
  (atomFamily Identity -> Multiplicity -> r -> r) ->
  Either (RuntimeReadError ctx prop) r
visibleRowsOfFamilyFold atomFamily plan runtime initial step = do
  ensureFamilyOutputSchema queryId atomFamily projection
  folded <-
    visibleRowsFold
      plan
      runtime
      (Right initial)
      (typedFamilyStep queryId atomFamily projection step)
  folded
  where
    queryId =
      runtimePlanQueryId plan
    projection =
      rpProjection plan
{-# INLINE visibleRowsOfFamilyFold #-}

ensureFamilyOutputSchema ::
  QueryId ->
  SAtomFamily atomFamily ->
  RuntimePlanProjection ->
  Either (RuntimeReadError ctx prop) ()
ensureFamilyOutputSchema queryId atomFamily projection =
  let expected =
        schemaOf atomFamily
      actual =
        rppOutputSlots projection
   in if actual == expected
        then Right ()
        else
          Left
            ( RuntimeReadFamilySchemaMismatch
                queryId
                (atomIdOf atomFamily)
                expected
                actual
            )
{-# INLINE ensureFamilyOutputSchema #-}

typedFamilyStep ::
  QueryId ->
  SAtomFamily atomFamily ->
  RuntimePlanProjection ->
  (atomFamily Identity -> Multiplicity -> r -> r) ->
  RowTupleKey ->
  Multiplicity ->
  Either (RuntimeReadError ctx prop) r ->
  Either (RuntimeReadError ctx prop) r
typedFamilyStep _queryId _atomFamily _projection _step _rowValue _multiplicity (Left err) =
  Left err
typedFamilyStep queryId atomFamily projection step rowValue multiplicity (Right acc0) = do
  projectedRow <-
    projectOutputRow queryId atomFamily projection rowValue
  typedRow <-
    first
      (RuntimeReadFamilyDecodeFailed queryId (atomIdOf atomFamily))
      (decodeAtomFamilyRow atomFamily projectedRow)
  let !acc1 =
        step typedRow multiplicity acc0
  pure acc1
{-# INLINE typedFamilyStep #-}

projectOutputRow ::
  QueryId ->
  SAtomFamily atomFamily ->
  RuntimePlanProjection ->
  RowTupleKey ->
  Either (RuntimeReadError ctx prop) RowTupleKey
projectOutputRow queryId atomFamily projection rowValue =
  if rppFullSchema projection == rppOutputSlots projection
    then Right rowValue
    else
      first
        (RuntimeReadFamilyProjectionFailed queryId (atomIdOf atomFamily))
        ( Query.projectRowWithSlots
            (rppFullSchema projection)
            (rppOutputSlots projection)
            rowValue
        )
{-# INLINE projectOutputRow #-}
