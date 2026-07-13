{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Trace.Compact
  ( PartitionedPrefixCompactionOps (..),
    PartitionedPrefixCompactionResult (..),
    PartitionedPrefixCompactionError (..),
    compactPartitionedPrefixesBefore,
    compactPartitionedPrefixesBeforeDescription,
    planIndexedTraceCompactionBefore,
    planIndexedTraceCompactionBeforeDescription,
    applyIndexedTraceCompactionPlan,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.Kind
  ( Type,
  )
import Data.List.NonEmpty
  ( NonEmpty (..),
  )
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Moonlight.Core
  ( PartialOrder,
  )
import Moonlight.Differential.Frontier
  ( RuntimeFrontier,
    frontierTraceRetention,
    frontierPendingBeforeVisibleMinimum,
    frontierTimeCompactable,
    frontierVisibleAntichain,
    traceRetentionReferencedKeys,
  )
import Moonlight.Delta.Frontier
  ( emptyFrontier,
  )
import Moonlight.Differential.Time
  ( RuntimeTime,
  )
import Moonlight.Differential.Trace.Description
  ( TraceDescription,
    traceDescription,
    traceDescriptionTimeCompactable,
  )
import Moonlight.Differential.Trace.Indexed
  ( IndexedTrace,
    IndexedTraceError,
    TraceIndexOps,
    applyIndexedTraceRewrite,
    itEntries,
  )

type PartitionedPrefixCompactionOps :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data PartitionedPrefixCompactionOps ctx epoch phase batch partition group summaryErr = PartitionedPrefixCompactionOps
  { pcoBatchKey ::
      batch ->
      Int,
    pcoBatchTime ::
      batch ->
      RuntimeTime ctx epoch phase,
    pcoPartition ::
      batch ->
      partition,
    pcoPartitionBlockedByPending ::
      partition ->
      RuntimeTime ctx epoch phase ->
      Bool,
    pcoGroup ::
      batch ->
      group,
    pcoSummarizeRun ::
      RuntimeFrontier ctx epoch phase ->
      partition ->
      group ->
      NonEmpty batch ->
      Either summaryErr (Maybe batch)
  }

type PartitionedPrefixCompactionResult :: Type -> Type
data PartitionedPrefixCompactionResult batch = PartitionedPrefixCompactionResult
  { ppcrCompacted :: !(IntMap batch),
    ppcrSummaries :: !(IntMap batch),
    ppcrKept :: !(IntMap batch)
  }
  deriving stock (Eq, Show)

type PartitionedPrefixCompactionError :: Type -> Type -> Type -> Type -> Type
data PartitionedPrefixCompactionError ctx epoch phase summaryErr
  = PartitionedPrefixCompactionBatchKeyMismatch !Int !Int
  | PartitionedPrefixCompactionSummaryFailed !summaryErr
  | PartitionedPrefixCompactionSummaryKeyOutsideRun !Int !IntSet
  deriving stock (Eq, Show)

data PartitionRun batch partition group = PartitionRun
  { prPartition :: !partition,
    prGroup :: !group,
    prEntries :: !(NonEmpty (Int, batch))
  }

compactPartitionedPrefixesBefore ::
  (Ord ctx, Ord epoch, Ord phase, PartialOrder epoch, PartialOrder phase, Ord partition, Eq group) =>
  PartitionedPrefixCompactionOps ctx epoch phase batch partition group summaryErr ->
  RuntimeFrontier ctx epoch phase ->
  IntMap batch ->
  Either
    (PartitionedPrefixCompactionError ctx epoch phase summaryErr)
    (PartitionedPrefixCompactionResult batch)
compactPartitionedPrefixesBefore ops frontier =
  compactPartitionedPrefixesBeforeDescription
    ops
    frontier
    (traceDescription emptyFrontier visibleFrontier visibleFrontier)
  where
    visibleFrontier =
      frontierVisibleAntichain frontier
{-# INLINE compactPartitionedPrefixesBefore #-}

compactPartitionedPrefixesBeforeDescription ::
  (Ord ctx, PartialOrder epoch, PartialOrder phase, Ord partition, Eq group) =>
  PartitionedPrefixCompactionOps ctx epoch phase batch partition group summaryErr ->
  RuntimeFrontier ctx epoch phase ->
  TraceDescription (RuntimeTime ctx epoch phase) ->
  IntMap batch ->
  Either
    (PartitionedPrefixCompactionError ctx epoch phase summaryErr)
    (PartitionedPrefixCompactionResult batch)
compactPartitionedPrefixesBeforeDescription ops frontier description batches0 = do
  validateBatchKeys ops batches0

  let pendingBefore =
        frontierPendingBeforeVisibleMinimum frontier
      retained =
        retainedBatchKeys frontier
      compacted =
        partitionedCompactablePrefixes ops frontier description pendingBefore retained batches0
      kept =
        IntMap.difference batches0 compacted

  summaries <-
    summarizePartitionRuns
      ops
      frontier
      (concatMap (contiguousRuns ops) (Map.toAscList (partitionEntries ops compacted)))

  pure
    PartitionedPrefixCompactionResult
      { ppcrCompacted = compacted,
        ppcrSummaries = summaries,
        ppcrKept = kept
      }
{-# INLINE compactPartitionedPrefixesBeforeDescription #-}

planIndexedTraceCompactionBefore ::
  (Ord ctx, Ord epoch, Ord phase, PartialOrder epoch, PartialOrder phase, Ord partition, Eq group) =>
  PartitionedPrefixCompactionOps ctx epoch phase entry partition group summaryErr ->
  RuntimeFrontier ctx epoch phase ->
  IndexedTrace entry indexes ->
  Either
    (PartitionedPrefixCompactionError ctx epoch phase summaryErr)
    (PartitionedPrefixCompactionResult entry)
planIndexedTraceCompactionBefore ops frontier =
  compactPartitionedPrefixesBefore ops frontier . itEntries

planIndexedTraceCompactionBeforeDescription ::
  (Ord ctx, PartialOrder epoch, PartialOrder phase, Ord partition, Eq group) =>
  PartitionedPrefixCompactionOps ctx epoch phase entry partition group summaryErr ->
  RuntimeFrontier ctx epoch phase ->
  TraceDescription (RuntimeTime ctx epoch phase) ->
  IndexedTrace entry indexes ->
  Either
    (PartitionedPrefixCompactionError ctx epoch phase summaryErr)
    (PartitionedPrefixCompactionResult entry)
planIndexedTraceCompactionBeforeDescription ops frontier description =
  compactPartitionedPrefixesBeforeDescription ops frontier description . itEntries

applyIndexedTraceCompactionPlan ::
  TraceIndexOps entry indexes indexError ->
  PartitionedPrefixCompactionResult entry ->
  IndexedTrace entry indexes ->
  Either IndexedTraceError (IndexedTrace entry indexes)
applyIndexedTraceCompactionPlan ops prefixPlan =
  applyIndexedTraceRewrite ops (ppcrCompacted prefixPlan) (ppcrSummaries prefixPlan)

validateBatchKeys ::
  PartitionedPrefixCompactionOps ctx epoch phase batch partition group summaryErr ->
  IntMap batch ->
  Either
    (PartitionedPrefixCompactionError ctx epoch phase summaryErr)
    ()
validateBatchKeys ops =
  IntMap.foldlWithKey'
    ( \eitherUnit batchKey batch -> do
        eitherUnit
        let actualKey =
              pcoBatchKey ops batch
        if actualKey == batchKey
          then Right ()
          else Left (PartitionedPrefixCompactionBatchKeyMismatch batchKey actualKey)
    )
    (Right ())
{-# INLINE validateBatchKeys #-}

partitionedCompactablePrefixes ::
  (Ord ctx, PartialOrder epoch, PartialOrder phase, Ord partition) =>
  PartitionedPrefixCompactionOps ctx epoch phase batch partition group summaryErr ->
  RuntimeFrontier ctx epoch phase ->
  TraceDescription (RuntimeTime ctx epoch phase) ->
  Set (RuntimeTime ctx epoch phase) ->
  IntSet ->
  IntMap batch ->
  IntMap batch
partitionedCompactablePrefixes ops frontier description pendingBefore retained =
  Map.foldlWithKey'
    ( \compacted partitionValue entries ->
        if partitionBlockedByPending ops partitionValue pendingBefore
          then compacted
          else
            IntMap.union
              compacted
              (compactablePrefixForPartition ops frontier description retained entries)
    )
    IntMap.empty
    . partitionEntries ops
{-# INLINE partitionedCompactablePrefixes #-}

partitionEntries ::
  Ord partition =>
  PartitionedPrefixCompactionOps ctx epoch phase batch partition group summaryErr ->
  IntMap batch ->
  Map partition (IntMap batch)
partitionEntries ops =
  IntMap.foldlWithKey'
    ( \partitions batchKey batch ->
        Map.insertWith
          IntMap.union
          (pcoPartition ops batch)
          (IntMap.singleton batchKey batch)
          partitions
    )
    Map.empty
{-# INLINE partitionEntries #-}

partitionBlockedByPending ::
  PartitionedPrefixCompactionOps ctx epoch phase batch partition group summaryErr ->
  partition ->
  Set (RuntimeTime ctx epoch phase) ->
  Bool
partitionBlockedByPending ops partitionValue =
  Foldable.foldr
    ( \pendingTime blocked ->
        blocked || pcoPartitionBlockedByPending ops partitionValue pendingTime
    )
    False
{-# INLINE partitionBlockedByPending #-}

compactablePrefixForPartition ::
  (Ord ctx, PartialOrder epoch, PartialOrder phase) =>
  PartitionedPrefixCompactionOps ctx epoch phase batch partition group summaryErr ->
  RuntimeFrontier ctx epoch phase ->
  TraceDescription (RuntimeTime ctx epoch phase) ->
  IntSet ->
  IntMap batch ->
  IntMap batch
compactablePrefixForPartition ops frontier description retained =
  IntMap.fromAscList
    . fst
    . span (entryCompactable ops frontier description retained)
    . IntMap.toAscList
{-# INLINE compactablePrefixForPartition #-}

entryCompactable ::
  (Ord ctx, PartialOrder epoch, PartialOrder phase) =>
  PartitionedPrefixCompactionOps ctx epoch phase batch partition group summaryErr ->
  RuntimeFrontier ctx epoch phase ->
  TraceDescription (RuntimeTime ctx epoch phase) ->
  IntSet ->
  (Int, batch) ->
  Bool
entryCompactable ops frontier description retained (batchKey, batch) =
  not (IntSet.member batchKey retained)
    && traceDescriptionTimeCompactable (pcoBatchTime ops batch) description
    && frontierTimeCompactable frontier (pcoBatchTime ops batch)
{-# INLINE entryCompactable #-}

contiguousRuns ::
  Eq group =>
  PartitionedPrefixCompactionOps ctx epoch phase batch partition group summaryErr ->
  (partition, IntMap batch) ->
  [PartitionRun batch partition group]
contiguousRuns ops (partitionValue, entries) =
  case IntMap.toAscList entries of
    [] ->
      []
    firstEntry@(_firstKey, firstBatch) : rest ->
      go (pcoGroup ops firstBatch) (firstEntry :| []) rest
  where
    go group reversedRun remaining =
      case remaining of
        [] ->
          [PartitionRun partitionValue group (NonEmpty.reverse reversedRun)]
        entry@(_batchKey, batch) : restEntries
          | pcoGroup ops batch == group ->
              go group (entry NonEmpty.<| reversedRun) restEntries
          | otherwise ->
              PartitionRun partitionValue group (NonEmpty.reverse reversedRun)
                : go (pcoGroup ops batch) (entry :| []) restEntries
{-# INLINE contiguousRuns #-}

summarizePartitionRuns ::
  PartitionedPrefixCompactionOps ctx epoch phase batch partition group summaryErr ->
  RuntimeFrontier ctx epoch phase ->
  [PartitionRun batch partition group] ->
  Either
    (PartitionedPrefixCompactionError ctx epoch phase summaryErr)
    (IntMap batch)
summarizePartitionRuns ops frontier =
  Foldable.foldlM summarizeOne IntMap.empty
  where
    summarizeOne summaries run = do
      maybeSummary <-
        first PartitionedPrefixCompactionSummaryFailed $
          pcoSummarizeRun
            ops
            frontier
            (prPartition run)
            (prGroup run)
            (fmap snd (prEntries run))

      case maybeSummary of
        Nothing ->
          Right summaries
        Just summary -> do
          let summaryKey =
                pcoBatchKey ops summary
              runKeys =
                IntSet.fromList (fmap fst (Foldable.toList (prEntries run)))

          if IntSet.member summaryKey runKeys
            then Right ()
            else Left (PartitionedPrefixCompactionSummaryKeyOutsideRun summaryKey runKeys)

          Right (IntMap.insert summaryKey summary summaries)
{-# INLINE summarizePartitionRuns #-}

retainedBatchKeys ::
  RuntimeFrontier ctx epoch phase ->
  IntSet
retainedBatchKeys frontier =
  maybe IntSet.empty traceRetentionReferencedKeys (frontierTraceRetention frontier)
{-# INLINE retainedBatchKeys #-}
