{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Flow.Runtime.Factor.Request
  ( FactorFullRepairOracle,
    factorFullRepairOracle,
    FactorRepairScope (..),
    FactorFullRepairReason (..),
    FactorRepairCause (..),
    FactorRepairRequest (..),
    FactorRepairBatchMember (..),
    FactorRepairBatchRequest (..),
    manualFullRepairRequest,
    patchRepair,
    fullRepair,
    repairRequest,
    repairBatchRequest,
    singletonRepairBatchRequest,
    factorRepairBatchRequests,
    mergeRepairRequests,
    mergeRepairBatchRequests,
    mergeRepairCause,
    mergeRepairScope,
    repairCauseRelationalScope,
    repairCauseAtomDeltas,
    repairCauseHasAtomDeltas,
    repairCauseDropsCache,
    repairCauseIsFull,
  )
where
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Core
  ( QueryId,
  )
import Moonlight.Differential.Proposition
  ( PropositionKey,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta,
    dropEmptyRowDeltas,
  )
import Moonlight.Differential.Row.Patch
  ( composePlainRowPatch
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope,
  )
import Moonlight.Flow.Runtime.Spec.FactorProgram
  ( RepairProgramKey,
  )
data FactorRepairScope = FactorRepairScope
  { frsRelationalScope :: !RelationalScope,
    frsAtomDeltas :: !(IntMap RowDelta)
  }
  deriving stock (Eq, Show)
data FactorFullRepairReason
  = FullRepairContextInstalled
  | FullRepairProgramInstalled
  | FullRepairCacheInvalidated
  | FullRepairManual
  deriving stock (Eq, Ord, Show, Read)
data FactorRepairCause
  = PatchRepair !FactorRepairScope
  | FullRepair !(Set FactorFullRepairReason)
  deriving stock (Eq, Show)
data FactorRepairRequest ctx prop = FactorRepairRequest
  { frrContext :: !ctx,
    frrProp :: !(PropositionKey prop),
    frrRepairKey :: !RepairProgramKey,
    frrQueryId :: !QueryId,
    frrCause :: !FactorRepairCause
  }
  deriving stock (Eq, Show)

data FactorRepairBatchMember = FactorRepairBatchMember
  { frbmRepairKey :: !RepairProgramKey,
    frbmCause :: !FactorRepairCause
  }
  deriving stock (Eq, Show)

data FactorRepairBatchRequest ctx prop = FactorRepairBatchRequest
  { frbrContext :: !ctx,
    frbrProp :: !(PropositionKey prop),
    frbrRepairs :: !(Map QueryId FactorRepairBatchMember)
  }
  deriving stock (Eq, Show)
data FactorFullRepairOracle = FactorFullRepairOracle
  deriving stock (Eq, Ord, Show, Read)
factorFullRepairOracle :: FactorFullRepairOracle
factorFullRepairOracle =
  FactorFullRepairOracle
{-# INLINE factorFullRepairOracle #-}
manualFullRepairRequest ::
  FactorFullRepairOracle ->
  ctx ->
  PropositionKey prop ->
  RepairProgramKey ->
  QueryId ->
  FactorRepairRequest ctx prop
manualFullRepairRequest _oracle contextValue propKey repairKey queryId =
  repairRequest
    contextValue
    propKey
    repairKey
    queryId
    (fullRepair FullRepairManual)
{-# INLINE manualFullRepairRequest #-}
patchRepair :: FactorRepairScope -> FactorRepairCause
patchRepair =
  PatchRepair . normalizeFactorRepairScope
{-# INLINE patchRepair #-}
fullRepair :: FactorFullRepairReason -> FactorRepairCause
fullRepair reason =
  FullRepair (Set.singleton reason)
{-# INLINE fullRepair #-}
repairRequest ::
  ctx ->
  PropositionKey prop ->
  RepairProgramKey ->
  QueryId ->
  FactorRepairCause ->
  FactorRepairRequest ctx prop
repairRequest contextValue propKey repairKey queryId cause =
  FactorRepairRequest
    { frrContext = contextValue,
      frrProp = propKey,
      frrRepairKey = repairKey,
      frrQueryId = queryId,
      frrCause = cause
    }
{-# INLINE repairRequest #-}

repairBatchRequest ::
  ctx ->
  PropositionKey prop ->
  Map QueryId FactorRepairBatchMember ->
  FactorRepairBatchRequest ctx prop
repairBatchRequest contextValue propKey repairs =
  FactorRepairBatchRequest
    { frbrContext = contextValue,
      frbrProp = propKey,
      frbrRepairs = repairs
    }
{-# INLINE repairBatchRequest #-}

singletonRepairBatchRequest ::
  ctx ->
  PropositionKey prop ->
  RepairProgramKey ->
  QueryId ->
  FactorRepairCause ->
  FactorRepairBatchRequest ctx prop
singletonRepairBatchRequest contextValue propKey repairKey queryId cause =
  repairBatchRequest
    contextValue
    propKey
    ( Map.singleton
        queryId
        FactorRepairBatchMember
          { frbmRepairKey = repairKey,
            frbmCause = cause
          }
    )
{-# INLINE singletonRepairBatchRequest #-}

factorRepairBatchRequests ::
  FactorRepairBatchRequest ctx prop ->
  [FactorRepairRequest ctx prop]
factorRepairBatchRequests batch =
  [ repairRequest
      (frbrContext batch)
      (frbrProp batch)
      (frbmRepairKey member)
      queryId
      (frbmCause member)
  | (queryId, member) <- Map.toAscList (frbrRepairs batch)
  ]
{-# INLINE factorRepairBatchRequests #-}
mergeRepairRequests ::
  FactorRepairRequest ctx prop ->
  FactorRepairRequest ctx prop ->
  FactorRepairRequest ctx prop
mergeRepairRequests newer older =
  newer
    { frrCause =
        mergeRepairCause
          (frrCause newer)
          (frrCause older)
    }
{-# INLINE mergeRepairRequests #-}

mergeRepairBatchRequests ::
  FactorRepairBatchRequest ctx prop ->
  FactorRepairBatchRequest ctx prop ->
  FactorRepairBatchRequest ctx prop
mergeRepairBatchRequests newer older =
  newer
    { frbrRepairs =
        Map.unionWith
          mergeRepairBatchMembers
          (frbrRepairs newer)
          (frbrRepairs older)
    }
{-# INLINE mergeRepairBatchRequests #-}

mergeRepairBatchMembers ::
  FactorRepairBatchMember ->
  FactorRepairBatchMember ->
  FactorRepairBatchMember
mergeRepairBatchMembers newer older =
  newer
    { frbmCause =
        mergeRepairCause
          (frbmCause newer)
          (frbmCause older)
    }
{-# INLINE mergeRepairBatchMembers #-}
mergeRepairCause ::
  FactorRepairCause ->
  FactorRepairCause ->
  FactorRepairCause
mergeRepairCause newer older =
  case (newer, older) of
    (FullRepair newerReasons, FullRepair olderReasons) ->
      FullRepair (Set.union newerReasons olderReasons)
    (FullRepair reasons, PatchRepair _) ->
      FullRepair reasons
    (PatchRepair _, FullRepair reasons) ->
      FullRepair reasons
    (PatchRepair newerScope, PatchRepair olderScope) ->
      PatchRepair (mergeRepairScope newerScope olderScope)
{-# INLINE mergeRepairCause #-}
mergeRepairScope ::
  FactorRepairScope ->
  FactorRepairScope ->
  FactorRepairScope
mergeRepairScope newer older =
  normalizeFactorRepairScope
    FactorRepairScope
      { frsRelationalScope =
          frsRelationalScope newer <> frsRelationalScope older,
        frsAtomDeltas =
          mergeAtomDeltas
            (frsAtomDeltas newer)
            (frsAtomDeltas older)
      }
{-# INLINE mergeRepairScope #-}
repairCauseRelationalScope :: FactorRepairCause -> RelationalScope
repairCauseRelationalScope cause =
  case cause of
    PatchRepair scope ->
      frsRelationalScope scope
    FullRepair _ ->
      mempty
{-# INLINE repairCauseRelationalScope #-}
repairCauseAtomDeltas :: FactorRepairCause -> IntMap RowDelta
repairCauseAtomDeltas cause =
  case cause of
    PatchRepair scope ->
      frsAtomDeltas scope
    FullRepair _ ->
      IntMap.empty
{-# INLINE repairCauseAtomDeltas #-}
repairCauseHasAtomDeltas :: FactorRepairCause -> Bool
repairCauseHasAtomDeltas =
  not . rowDeltaMapNull . repairCauseAtomDeltas
{-# INLINE repairCauseHasAtomDeltas #-}
repairCauseDropsCache :: FactorRepairCause -> Bool
repairCauseDropsCache cause =
  case cause of
    PatchRepair _ ->
      repairCauseHasAtomDeltas cause
    FullRepair reasons ->
      Set.member FullRepairProgramInstalled reasons
        || Set.member FullRepairCacheInvalidated reasons
        || Set.member FullRepairManual reasons
{-# INLINE repairCauseDropsCache #-}
repairCauseIsFull :: FactorRepairCause -> Bool
repairCauseIsFull cause =
  case cause of
    FullRepair _ ->
      True
    PatchRepair _ ->
      False
{-# INLINE repairCauseIsFull #-}
normalizeFactorRepairScope :: FactorRepairScope -> FactorRepairScope
normalizeFactorRepairScope scope =
  scope
    { frsAtomDeltas =
        dropEmptyRowDeltas (frsAtomDeltas scope)
    }
{-# INLINE normalizeFactorRepairScope #-}
mergeAtomDeltas ::
  IntMap RowDelta ->
  IntMap RowDelta ->
  IntMap RowDelta
mergeAtomDeltas newer older =
  dropEmptyRowDeltas
    (IntMap.unionWith composePlainRowPatch newer older)
{-# INLINE mergeAtomDeltas #-}
rowDeltaMapNull ::
  IntMap RowDelta ->
  Bool
rowDeltaMapNull =
  IntMap.null . dropEmptyRowDeltas
{-# INLINE rowDeltaMapNull #-}
