module Moonlight.Flow.Runtime.Execution.IR.Normalize
  ( mergeRuntimeDataflowOp,
    dedupeRuntimeDataflowOps,
  )
where

import Data.Foldable qualified as Foldable
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Moonlight.Flow.Runtime.Execution.IR
  ( RuntimeDataflowOp,
    RuntimeDataflowOpKey,
    RuntimeDataflowOpMetadata (mergeRuntimeDataflowOpKindOf),
    runtimeDataflowOpFromKind,
    runtimeDataflowOpKey,
    runtimeDataflowOpKind,
  )

mergeRuntimeDataflowOp ::
  (Ord ctx, Ord prop) =>
  RuntimeDataflowOp ctx prop boundary evidence ->
  RuntimeDataflowOp ctx prop boundary evidence ->
  RuntimeDataflowOp ctx prop boundary evidence
mergeRuntimeDataflowOp newer older =
  case mergeRuntimeDataflowOpKindOf (runtimeDataflowOpKind newer) (runtimeDataflowOpKind older) of
    Nothing ->
      older
    Just mergedKind ->
      runtimeDataflowOpFromKind mergedKind
{-# INLINE mergeRuntimeDataflowOp #-}

dedupeRuntimeDataflowOps ::
  (Ord ctx, Ord prop) =>
  [RuntimeDataflowOp ctx prop boundary evidence] ->
  [RuntimeDataflowOp ctx prop boundary evidence]
dedupeRuntimeDataflowOps =
  finishDedupeRuntimeDataflowOps
    . Foldable.foldl'
      insertDedupeRuntimeDataflowOp
      ([], Map.empty)
{-# INLINE dedupeRuntimeDataflowOps #-}

type DedupeRuntimeDataflowOpsState ctx prop boundary evidence =
  ( [RuntimeDataflowOp ctx prop boundary evidence],
    Map (RuntimeDataflowOpKey ctx prop) (RuntimeDataflowOp ctx prop boundary evidence)
  )

insertDedupeRuntimeDataflowOp ::
  (Ord ctx, Ord prop) =>
  DedupeRuntimeDataflowOpsState ctx prop boundary evidence ->
  RuntimeDataflowOp ctx prop boundary evidence ->
  DedupeRuntimeDataflowOpsState ctx prop boundary evidence
insertDedupeRuntimeDataflowOp (unkeyed, keyed) op =
  case runtimeDataflowOpKey op of
    Nothing ->
      (op : unkeyed, keyed)
    Just key ->
      (unkeyed, Map.insertWith mergeRuntimeDataflowOp key op keyed)
{-# INLINE insertDedupeRuntimeDataflowOp #-}

finishDedupeRuntimeDataflowOps ::
  DedupeRuntimeDataflowOpsState ctx prop boundary evidence ->
  [RuntimeDataflowOp ctx prop boundary evidence]
finishDedupeRuntimeDataflowOps (unkeyed, keyed) =
  reverse unkeyed <> Map.elems keyed
{-# INLINE finishDedupeRuntimeDataflowOps #-}
