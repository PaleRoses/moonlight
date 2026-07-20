{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Runtime.Topology.Lowering.Impact
  ( RuntimeImpact (..),
    RuntimeTouchCause (..),
    mergeRuntimeTouchCause,
    impactFromPatch,
    impactFromScopedAtomEvents,
    touchCausesForImpact,
    lowerImpactToDataflowOps,
  )
where

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
    atomIdKey,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    QueryCarrierNode (..),
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierProp,
    CarrierAddr,
    caContext,
    caProp,
    caCarrier,
  )
import Moonlight.Flow.Carrier.Core.Reuse
  ( CarrierReuseId,
  )
import Moonlight.Flow.Carrier.Core.Topology
  ( CarrierTopology,
    TouchKey (..),
    carrierTopologyDerivedOwners,
    carrierTopologyTouchedBy,
  )
import Moonlight.Flow.Model.Delta
  ( AtomEvent (..),
    QuotientPatch (..),
    ScopedAtomEvents (..)
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
    relationalScopeFromSets,
    scopeDeps,
    scopeTopo,
  )
import Moonlight.Flow.Runtime.Execution.Delta
  ( mergeRowDeltaDedupCopy,
  )
import Moonlight.Flow.Runtime.Execution.IR
  ( RuntimeDataflowOp,
    deriveSubsumedCarrierDataflowOp,
    fullRepairFactorBatchDataflowOp,
    repairFactorBatchDataflowOp,
  )
import Moonlight.Flow.Runtime.Execution.IR.Normalize
  ( dedupeRuntimeDataflowOps,
  )
import Moonlight.Flow.Runtime.Spec.FactorProgram
  ( RepairProgramKey,
  )
import Moonlight.Flow.Runtime.Factor.Request
  ( FactorFullRepairReason,
    FactorRepairScope (..),
    patchRepair,
    singletonRepairBatchRequest,
  )
import Moonlight.Flow.Runtime.Topology.Lowering.Edge
  ( lowerTouchedCarrierFanout,
  )
import Moonlight.Flow.Runtime.Topology.Lowering.Types
  ( RuntimeRepairRoute (..),
    RuntimeRepairRouting (..),
  )

type RuntimeImpact :: Type -> Type -> Type
data RuntimeImpact ctx prop = RuntimeImpact
  { riRelationalScope :: !RelationalScope,
    riAtomTouches :: !IntSet,
    riAtomScopeByAtom :: !(IntMap RelationalScope),
    riAtomDeltasByQuery :: !(Map QueryId (IntMap RowDelta))
  }
  deriving stock (Eq, Show)

type RuntimeTouchCause :: Type
data RuntimeTouchCause = RuntimeTouchCause
  { rtcTouchKeys :: !(Set TouchKey),
    rtcRelationalScope :: !RelationalScope
  }
  deriving stock (Eq, Show)

mergeRuntimeTouchCause ::
  RuntimeTouchCause ->
  RuntimeTouchCause ->
  RuntimeTouchCause
mergeRuntimeTouchCause newer older =
  RuntimeTouchCause
    { rtcTouchKeys = Set.union (rtcTouchKeys newer) (rtcTouchKeys older),
      rtcRelationalScope = rtcRelationalScope newer <> rtcRelationalScope older
    }
{-# INLINE mergeRuntimeTouchCause #-}

impactFromPatch :: QuotientPatch -> RuntimeImpact ctx prop
impactFromPatch patch =
  RuntimeImpact
    { riRelationalScope = qpScope patch,
      riAtomTouches = IntMap.keysSet (qpEvents patch),
      riAtomScopeByAtom = qpAtomScopeByAtom patch,
      riAtomDeltasByQuery = Map.empty
    }
{-# INLINE impactFromPatch #-}

impactFromScopedAtomEvents ::
  ScopedAtomEvents ->
  RuntimeImpact ctx prop
impactFromScopedAtomEvents scopedEvents =
  RuntimeImpact
    { riRelationalScope = saeScope scopedEvents,
      riAtomTouches =
        IntMap.keysSet (saeTouchScopeByAtom scopedEvents),
      riAtomScopeByAtom = saeTouchScopeByAtom scopedEvents,
      riAtomDeltasByQuery =
        Map.fromListWith
          mergeAtomDeltas
          [ (aeQueryId event, queryDeltas)
          | event <- saeEvents scopedEvents,
            let queryDeltas =
                  dropEmptyRowDeltas
                    (IntMap.singleton (atomIdKey (aeAtomId event)) (aeRows event)),
            not (IntMap.null queryDeltas)
          ]
    }
{-# INLINE impactFromScopedAtomEvents #-}

lowerImpactToDataflowOps ::
  (Ord ctx, Ord prop) =>
  RuntimeRepairRouting ->
  FactorFullRepairReason ->
  CarrierTopology ctx Carrier prop ->
  RuntimeImpact ctx prop ->
  [RuntimeDataflowOp ctx prop boundary evidence]
lowerImpactToDataflowOps repairRouting fullRepairReason carrierTopology impact =
  dedupeRuntimeDataflowOps
    ( repairDataflowOpsForTouchedFactorGroups
        repairRouting
        fullRepairReason
        repairDeltasByKey
        touched
        <> [ op
           | (addr, _cause) <- Map.toAscList touched,
             op <-
              derivedOwnerDataflowOpsForTouchedCarrier derivedOwners addr
                <> lowerTouchedCarrierFanout carrierTopology addr
           ]
    )
  where
    !derivedOwners =
      carrierTopologyDerivedOwners carrierTopology

    !repairDeltasByKey =
      atomDeltasByRepairKey repairRouting impact

    !touched =
      touchCausesForImpact carrierTopology impact
{-# INLINE lowerImpactToDataflowOps #-}

repairDataflowOpsForTouchedFactorGroups ::
  (Ord ctx, Ord prop) =>
  RuntimeRepairRouting ->
  FactorFullRepairReason ->
  Map RepairProgramKey (IntMap RowDelta) ->
  Map (CarrierAddr ctx Carrier prop) RuntimeTouchCause ->
  [RuntimeDataflowOp ctx prop boundary evidence]
repairDataflowOpsForTouchedFactorGroups repairRouting fullRepairReason repairDeltasByKey touched =
  fmap
    ( repairDataflowOpForTouchedFactorGroup
        repairRouting
        fullRepairReason
        repairDeltasByKey
    )
    (Map.toAscList (touchedFactorRepairGroups repairRouting touched))
{-# INLINE repairDataflowOpsForTouchedFactorGroups #-}

type RepairMemberGroupKey ctx prop = (ctx, CarrierProp prop, RuntimeRepairRoute)

touchedFactorRepairGroups ::
  (Ord ctx, Ord prop) =>
  RuntimeRepairRouting ->
  Map (CarrierAddr ctx Carrier prop) RuntimeTouchCause ->
  Map (RepairMemberGroupKey ctx prop) RuntimeTouchCause
touchedFactorRepairGroups repairRouting =
  Map.foldlWithKey' collectRepairGroup Map.empty
  where
    collectRepairGroup groups addr cause =
      case caCarrier addr of
        QueryCarrier queryId (QueryFactor _node) ->
          case rrRepairRouteOfQuery repairRouting queryId of
            Nothing ->
              groups
            Just route ->
              Map.insertWith
                mergeRuntimeTouchCause
                (caContext addr, caProp addr, route)
                cause
                groups
        QueryCarrier {} ->
          groups
        DerivedCarrier {} ->
          groups
{-# INLINE touchedFactorRepairGroups #-}

repairDataflowOpForTouchedFactorGroup ::
  (Ord ctx, Ord prop) =>
  RuntimeRepairRouting ->
  FactorFullRepairReason ->
  Map RepairProgramKey (IntMap RowDelta) ->
  (RepairMemberGroupKey ctx prop, RuntimeTouchCause) ->
  RuntimeDataflowOp ctx prop boundary evidence
repairDataflowOpForTouchedFactorGroup repairRouting fullRepairReason repairDeltasByKey ((contextValue, propKey, route), cause)
  | rrRepairIsCold repairRouting (rrtRepairKey route) =
      fullRepairFactorBatchDataflowOp
        contextValue
        propKey
        (rrtRepairKey route)
        representativeQueryId
        fullRepairReason
  | otherwise =
      repairFactorBatchDataflowOp
        ( singletonRepairBatchRequest
            contextValue
            propKey
            (rrtRepairKey route)
            representativeQueryId
            ( patchRepair
                ( FactorRepairScope
                    { frsRelationalScope = rtcRelationalScope cause,
                      frsAtomDeltas =
                        Map.findWithDefault
                          IntMap.empty
                          (rrtRepairKey route)
                          repairDeltasByKey
                    }
                )
            )
        )
  where
    representativeQueryId =
      rrtRepresentativeQueryId route
{-# INLINE repairDataflowOpForTouchedFactorGroup #-}

derivedOwnerDataflowOpsForTouchedCarrier ::
  (Ord ctx, Ord prop) =>
  Map
    (CarrierAddr ctx Carrier prop)
    (Set (CarrierReuseId ctx prop, CarrierAddr ctx Carrier prop)) ->
  CarrierAddr ctx Carrier prop ->
  [RuntimeDataflowOp ctx prop boundary evidence]
derivedOwnerDataflowOpsForTouchedCarrier derivedOwners addr =
  case caCarrier addr of
    DerivedCarrier _derivedId ->
      [ deriveSubsumedCarrierDataflowOp reuseId source addr
      | (reuseId, source) <- Set.toAscList (Map.findWithDefault Set.empty addr derivedOwners)
      ]
    QueryCarrier {} ->
      []
{-# INLINE derivedOwnerDataflowOpsForTouchedCarrier #-}

touchCausesForImpact ::
  (Ord ctx, Ord carrier, Ord prop) =>
  CarrierTopology ctx carrier prop ->
  RuntimeImpact ctx prop ->
  Map (CarrierAddr ctx carrier prop) RuntimeTouchCause
touchCausesForImpact graph impact =
  Map.fromListWith
    mergeRuntimeTouchCause
    [ ( addr,
        RuntimeTouchCause
          { rtcTouchKeys = Set.singleton touchKey,
            rtcRelationalScope = touchScope
          }
      )
    | (touchKey, touchScope) <- impactTouchEntries impact,
      addr <- Set.toAscList (carrierTopologyTouchedBy touchKey graph)
    ]
{-# INLINE touchCausesForImpact #-}

impactTouchEntries :: RuntimeImpact ctx prop -> [(TouchKey, RelationalScope)]
impactTouchEntries impact =
  atomEntries <> depEntries <> topoEntries
  where
    atomEntries =
      [ ( TouchAtom atomKey,
          IntMap.findWithDefault mempty atomKey (riAtomScopeByAtom impact)
        )
      | atomKey <- IntSet.toAscList (riAtomTouches impact)
      ]

    depEntries =
      [ (TouchDep depKey, depTouchScope depKey)
      | depKey <- IntSet.toAscList (scopeDeps (riRelationalScope impact))
      ]

    topoEntries =
      [ (TouchTopo topoKey, topoTouchScope topoKey)
      | topoKey <- IntSet.toAscList (scopeTopo (riRelationalScope impact))
      ]
{-# INLINE impactTouchEntries #-}

depTouchScope :: Int -> RelationalScope
depTouchScope depKey =
  relationalScopeFromSets
    (IntSet.singleton depKey)
    IntSet.empty
    IntSet.empty
    IntSet.empty
    IntSet.empty
{-# INLINE depTouchScope #-}

topoTouchScope :: Int -> RelationalScope
topoTouchScope topoKey =
  relationalScopeFromSets
    IntSet.empty
    (IntSet.singleton topoKey)
    IntSet.empty
    IntSet.empty
    IntSet.empty
{-# INLINE topoTouchScope #-}

atomDeltasByRepairKey ::
  RuntimeRepairRouting ->
  RuntimeImpact ctx prop ->
  Map RepairProgramKey (IntMap RowDelta)
atomDeltasByRepairKey repairRouting impact =
  Map.foldlWithKey'
    insertQueryDeltas
    Map.empty
    (riAtomDeltasByQuery impact)
  where
    insertQueryDeltas byKey queryId atomDeltas =
      case rrRepairRouteOfQuery repairRouting queryId of
        Nothing ->
          byKey
        Just route ->
          Map.insertWith
            mergeAtomDeltasDedupCopies
            (rrtRepairKey route)
            (dropEmptyRowDeltas atomDeltas)
            byKey
{-# INLINE atomDeltasByRepairKey #-}

mergeAtomDeltasDedupCopies ::
  IntMap RowDelta ->
  IntMap RowDelta ->
  IntMap RowDelta
mergeAtomDeltasDedupCopies newer older =
  dropEmptyRowDeltas
    (IntMap.unionWith mergeRowDeltaDedupCopy newer older)
{-# INLINE mergeAtomDeltasDedupCopies #-}

mergeAtomDeltas ::
  IntMap RowDelta ->
  IntMap RowDelta ->
  IntMap RowDelta
mergeAtomDeltas newer older =
  dropEmptyRowDeltas
    (IntMap.unionWith composePlainRowPatch newer older)
{-# INLINE mergeAtomDeltas #-}
