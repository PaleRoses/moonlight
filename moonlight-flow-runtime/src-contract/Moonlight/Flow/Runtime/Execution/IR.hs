{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Moonlight.Flow.Runtime.Execution.IR
  ( RuntimeDataflowContract,
    mkRuntimeDataflowContract,
    runtimeDataflowContractPhase,
    runtimeDataflowContractReads,
    runtimeDataflowContractWrites,
    RelationalCapabilityTransport (..),
    RuntimeDataflowOpMetadata (..),
    (:+:) (..),
    ApplyAtomEventsOp (..),
    RunProjectOp (..),
    RunRestrictOp (..),
    RunIndexOp (..),
    RepairFactorBatchOp (..),
    DeriveSubsumedCarrierOp (..),
    RestrictCarrierOp (..),
    AmalgamateCarrierFamilyOp (..),
    RuntimeDataflowOp,
    RuntimeDataflowOpKind,
    ScheduledRuntimeDataflowOp,
    RuntimeDataflowOpKey (..),
    foldRuntimeDataflowOpKind,
    runtimeDataflowOpFromKind,
    runtimeDataflowOpKind,
    runtimeDataflowOpContract,
    runtimeDataflowOpContext,
    runtimeDataflowOpKey,
    runtimeDataflowOpTransport,
    runtimeDataflowOpProgressPointstamps,
    applyAtomEventsDataflowOpKind,
    runProjectDataflowOpKind,
    runRestrictDataflowOpKind,
    runIndexDataflowOpKind,
    repairFactorBatchDataflowOpKind,
    deriveSubsumedCarrierDataflowOpKind,
    restrictCarrierDataflowOpKind,
    amalgamateCarrierFamilyDataflowOpKind,
    applyAtomEventsDataflowOp,
    runProjectDataflowOp,
    runRestrictDataflowOp,
    runIndexDataflowOp,
    repairFactorBatchDataflowOp,
    fullRepairFactorBatchDataflowOp,
    deriveSubsumedCarrierDataflowOp,
    restrictCarrierDataflowOp,
    amalgamateCarrierFamilyDataflowOp,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.Kind
  ( Type,
  )
import Data.List.NonEmpty
  ( NonEmpty,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Delta.Frontier
  ( frontierPoints,
    singletonFrontier,
  )
import Moonlight.Delta.Time
  ( Timed,
    timedAt,
    timedValue,
  )
import Moonlight.Core
  ( QueryId,
    mkAtomId,
  )
import Moonlight.Differential.Proposition
  ( PropositionKey,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier,
    queryAtomCarrier,
    queryRootCarrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
    RestrictKey,
    rkSource,
    rkTarget,
    carrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Core.Reuse
  ( CarrierReuseId,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    retimeRelationalCarrierPhase,
  )
import Moonlight.Differential.Carrier.Topology
  ( CarrierFamily,
    carrierFamilyMembers,
    carrierFamilyTargetContext,
    carrierFamilyTargets,
  )
import Moonlight.Flow.Model.Delta
  ( AtomEvent
  )
import Moonlight.Flow.Model.Event
  ( LocalRelationalEvent,
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (..),
  )
import Moonlight.Flow.Model.Scope
  ( RelationalScope,
  )
import Moonlight.Flow.Runtime.Spec.FactorProgram
  ( RepairProgramKey,
  )
import Moonlight.Flow.Runtime.Factor.Request
  ( FactorFullRepairReason,
    FactorRepairBatchMember (..),
    FactorRepairBatchRequest (..),
    fullRepair,
    mergeRepairBatchRequests,
    repairCauseAtomDeltas,
    singletonRepairBatchRequest,
  )
import Moonlight.Flow.Runtime.Execution.Shard
  ( Shard,
  )

type RuntimeDataflowContract :: Type -> Type -> Type -> Type
data RuntimeDataflowContract ctx carrier prop = RuntimeDataflowContract
  { rdcPhase :: !RelationalPhase,
    rdcReads :: !(Set (CarrierAddr ctx carrier prop)),
    rdcWrites :: !(Set (CarrierAddr ctx carrier prop))
  }
  deriving stock (Eq, Ord, Show)

mkRuntimeDataflowContract ::
  RelationalPhase ->
  Set (CarrierAddr ctx carrier prop) ->
  Set (CarrierAddr ctx carrier prop) ->
  RuntimeDataflowContract ctx carrier prop
mkRuntimeDataflowContract phaseValue readAddrs writeAddrs =
  RuntimeDataflowContract
    { rdcPhase = phaseValue,
      rdcReads = readAddrs,
      rdcWrites = writeAddrs
    }
{-# INLINE mkRuntimeDataflowContract #-}

runtimeDataflowContractPhase :: RuntimeDataflowContract ctx carrier prop -> RelationalPhase
runtimeDataflowContractPhase =
  rdcPhase
{-# INLINE runtimeDataflowContractPhase #-}

runtimeDataflowContractReads ::
  RuntimeDataflowContract ctx carrier prop ->
  Set (CarrierAddr ctx carrier prop)
runtimeDataflowContractReads =
  rdcReads
{-# INLINE runtimeDataflowContractReads #-}

runtimeDataflowContractWrites ::
  RuntimeDataflowContract ctx carrier prop ->
  Set (CarrierAddr ctx carrier prop)
runtimeDataflowContractWrites =
  rdcWrites
{-# INLINE runtimeDataflowContractWrites #-}

type RelationalCapabilityTransport :: Type -> Type -> Type
data RelationalCapabilityTransport ctx prop
  = TransportViaRestriction !(RestrictKey ctx Carrier prop)
  | TransportViaAmalgamation !(CarrierFamily ctx Carrier prop)
  | TransportViaSubsumption
      !(CarrierReuseId ctx prop)
      !(CarrierAddr ctx Carrier prop)
      !(CarrierAddr ctx Carrier prop)
  deriving stock (Eq, Ord, Show)

type RuntimeDataflowOpKey :: Type -> Type -> Type
data RuntimeDataflowOpKey ctx prop
  = RepairFactorBatchKey !ctx !(PropositionKey prop)
  | DeriveSubsumedCarrierKey
      !(CarrierReuseId ctx prop)
      !(CarrierAddr ctx Carrier prop)
      !(CarrierAddr ctx Carrier prop)
  | RestrictCarrierKey !(RestrictKey ctx Carrier prop)
  | AmalgamateCarrierFamilyKey !(CarrierFamily ctx Carrier prop)
  deriving stock (Eq, Ord, Show)

type ApplyAtomEventsOp :: Type -> Type -> Type -> Type -> Type
data ApplyAtomEventsOp ctx prop boundary evidence = ApplyAtomEventsOp
  !(CarrierAddr ctx Carrier prop)
  !RelationalScope
  !(NonEmpty AtomEvent)
  deriving stock (Eq, Show)

type RunProjectOp :: Type -> Type -> Type -> Type -> Type
data RunProjectOp ctx prop boundary evidence = RunProjectOp
  !ctx
  !Shard
  !LocalRelationalEvent
  deriving stock (Eq, Show)

type RunRestrictOp :: Type -> Type -> Type -> Type -> Type
data RunRestrictOp ctx prop boundary evidence = RunRestrictOp
  !Shard
  !(RelationalCarrierDelta ctx Carrier prop boundary evidence)
  deriving stock (Eq, Show)

type RunIndexOp :: Type -> Type -> Type -> Type -> Type
data RunIndexOp ctx prop boundary evidence = RunIndexOp
  !Shard
  !(RelationalCarrierDelta ctx Carrier prop boundary evidence)
  deriving stock (Eq, Show)

type RepairFactorBatchOp :: Type -> Type -> Type -> Type -> Type
data RepairFactorBatchOp ctx prop boundary evidence = RepairFactorBatchOp
  !(FactorRepairBatchRequest ctx prop)
  !(Set (CarrierAddr ctx Carrier prop))
  !(Set (CarrierAddr ctx Carrier prop))
  deriving stock (Eq, Show)

type DeriveSubsumedCarrierOp :: Type -> Type -> Type -> Type -> Type
data DeriveSubsumedCarrierOp ctx prop boundary evidence = DeriveSubsumedCarrierOp
  !(CarrierReuseId ctx prop)
  !(CarrierAddr ctx Carrier prop)
  !(CarrierAddr ctx Carrier prop)
  deriving stock (Eq, Show)

type RestrictCarrierOp :: Type -> Type -> Type -> Type -> Type
data RestrictCarrierOp ctx prop boundary evidence = RestrictCarrierOp
  !(RestrictKey ctx Carrier prop)
  deriving stock (Eq, Show)

type AmalgamateCarrierFamilyOp :: Type -> Type -> Type -> Type -> Type
data AmalgamateCarrierFamilyOp ctx prop boundary evidence = AmalgamateCarrierFamilyOp
  !(CarrierFamily ctx Carrier prop)
  !(Set (CarrierAddr ctx Carrier prop))
  !(Set (CarrierAddr ctx Carrier prop))
  deriving stock (Eq, Show)

infixr 6 :+:

type (:+:) ::
  (Type -> Type -> Type -> Type -> Type) ->
  (Type -> Type -> Type -> Type -> Type) ->
  Type ->
  Type ->
  Type ->
  Type ->
  Type
data (f :+: g) ctx prop boundary evidence
  = InL !(f ctx prop boundary evidence)
  | InR !(g ctx prop boundary evidence)
  deriving stock (Eq, Show)

type RuntimeDataflowOpSum :: Type -> Type -> Type -> Type -> Type
type RuntimeDataflowOpSum =
  ApplyAtomEventsOp
    :+: RunProjectOp
    :+: RunRestrictOp
    :+: RunIndexOp
    :+: RepairFactorBatchOp
    :+: DeriveSubsumedCarrierOp
    :+: RestrictCarrierOp
    :+: AmalgamateCarrierFamilyOp

type RuntimeDataflowOpKind :: Type -> Type -> Type -> Type -> Type
newtype RuntimeDataflowOpKind ctx prop boundary evidence = RuntimeDataflowOpKind
  { unRuntimeDataflowOpKind :: RuntimeDataflowOpSum ctx prop boundary evidence
  }
  deriving stock (Eq, Show)

foldRuntimeDataflowOpKind ::
  RuntimeDataflowOpKind ctx prop boundary evidence ->
  (ApplyAtomEventsOp ctx prop boundary evidence -> result) ->
  (RunProjectOp ctx prop boundary evidence -> result) ->
  (RunRestrictOp ctx prop boundary evidence -> result) ->
  (RunIndexOp ctx prop boundary evidence -> result) ->
  (RepairFactorBatchOp ctx prop boundary evidence -> result) ->
  (DeriveSubsumedCarrierOp ctx prop boundary evidence -> result) ->
  (RestrictCarrierOp ctx prop boundary evidence -> result) ->
  (AmalgamateCarrierFamilyOp ctx prop boundary evidence -> result) ->
  result
foldRuntimeDataflowOpKind (RuntimeDataflowOpKind op) onApply onProject onRestrict onIndex onRepair onDerive onRestrictCarrier onAmalgamate =
  case op of
    InL applyOp ->
      onApply applyOp
    InR projectOrLater ->
      case projectOrLater of
        InL projectOp ->
          onProject projectOp
        InR restrictOrLater ->
          case restrictOrLater of
            InL restrictOp ->
              onRestrict restrictOp
            InR indexOrLater ->
              case indexOrLater of
                InL indexOp ->
                  onIndex indexOp
                InR repairOrLater ->
                  case repairOrLater of
                    InL repairOp ->
                      onRepair repairOp
                    InR deriveOrLater ->
                      case deriveOrLater of
                        InL deriveOp ->
                          onDerive deriveOp
                        InR restrictCarrierOrLater ->
                          case restrictCarrierOrLater of
                            InL restrictCarrierOp ->
                              onRestrictCarrier restrictCarrierOp
                            InR amalgamateOp ->
                              onAmalgamate amalgamateOp
{-# INLINE foldRuntimeDataflowOpKind #-}

class RuntimeDataflowOpMetadata op where
  runtimeDataflowOpContractOf ::
    op ctx prop boundary evidence ->
    RuntimeDataflowContract ctx Carrier prop
  runtimeDataflowOpContextOf ::
    op ctx prop boundary evidence ->
    ctx
  runtimeDataflowOpKeyOf ::
    op ctx prop boundary evidence ->
    Maybe (RuntimeDataflowOpKey ctx prop)
  runtimeDataflowOpTransportOf ::
    op ctx prop boundary evidence ->
    Maybe (RelationalCapabilityTransport ctx prop)
  mergeRuntimeDataflowOpKindOf ::
    (Ord ctx, Ord prop) =>
    op ctx prop boundary evidence ->
    op ctx prop boundary evidence ->
    Maybe (RuntimeDataflowOpKind ctx prop boundary evidence)
  mergeRuntimeDataflowOpKindOf _newer _older =
    Nothing
  {-# INLINE mergeRuntimeDataflowOpKindOf #-}

instance (RuntimeDataflowOpMetadata f, RuntimeDataflowOpMetadata g) => RuntimeDataflowOpMetadata (f :+: g) where
  runtimeDataflowOpContractOf op =
    case op of
      InL leftOp ->
        runtimeDataflowOpContractOf leftOp
      InR rightOp ->
        runtimeDataflowOpContractOf rightOp
  {-# INLINE runtimeDataflowOpContractOf #-}

  runtimeDataflowOpContextOf op =
    case op of
      InL leftOp ->
        runtimeDataflowOpContextOf leftOp
      InR rightOp ->
        runtimeDataflowOpContextOf rightOp
  {-# INLINE runtimeDataflowOpContextOf #-}

  runtimeDataflowOpKeyOf op =
    case op of
      InL leftOp ->
        runtimeDataflowOpKeyOf leftOp
      InR rightOp ->
        runtimeDataflowOpKeyOf rightOp
  {-# INLINE runtimeDataflowOpKeyOf #-}

  runtimeDataflowOpTransportOf op =
    case op of
      InL leftOp ->
        runtimeDataflowOpTransportOf leftOp
      InR rightOp ->
        runtimeDataflowOpTransportOf rightOp
  {-# INLINE runtimeDataflowOpTransportOf #-}

  mergeRuntimeDataflowOpKindOf newer older =
    case (newer, older) of
      (InL newerLeft, InL olderLeft) ->
        mergeRuntimeDataflowOpKindOf newerLeft olderLeft
      (InR newerRight, InR olderRight) ->
        mergeRuntimeDataflowOpKindOf newerRight olderRight
      _ ->
        Nothing
  {-# INLINE mergeRuntimeDataflowOpKindOf #-}

instance RuntimeDataflowOpMetadata RuntimeDataflowOpKind where
  runtimeDataflowOpContractOf (RuntimeDataflowOpKind op) =
    runtimeDataflowOpContractOf op
  {-# INLINE runtimeDataflowOpContractOf #-}

  runtimeDataflowOpContextOf (RuntimeDataflowOpKind op) =
    runtimeDataflowOpContextOf op
  {-# INLINE runtimeDataflowOpContextOf #-}

  runtimeDataflowOpKeyOf (RuntimeDataflowOpKind op) =
    runtimeDataflowOpKeyOf op
  {-# INLINE runtimeDataflowOpKeyOf #-}

  runtimeDataflowOpTransportOf (RuntimeDataflowOpKind op) =
    runtimeDataflowOpTransportOf op
  {-# INLINE runtimeDataflowOpTransportOf #-}

  mergeRuntimeDataflowOpKindOf (RuntimeDataflowOpKind newer) (RuntimeDataflowOpKind older) =
    mergeRuntimeDataflowOpKindOf newer older
  {-# INLINE mergeRuntimeDataflowOpKindOf #-}

instance RuntimeDataflowOpMetadata ApplyAtomEventsOp where
  runtimeDataflowOpContractOf (ApplyAtomEventsOp addr _scope _events) =
    mkRuntimeDataflowContract PhaseProject Set.empty (Set.singleton addr)
  {-# INLINE runtimeDataflowOpContractOf #-}

  runtimeDataflowOpContextOf (ApplyAtomEventsOp addr _scope _events) =
    caContext addr
  {-# INLINE runtimeDataflowOpContextOf #-}

  runtimeDataflowOpKeyOf _ =
    Nothing
  {-# INLINE runtimeDataflowOpKeyOf #-}

  runtimeDataflowOpTransportOf _ =
    Nothing
  {-# INLINE runtimeDataflowOpTransportOf #-}

instance RuntimeDataflowOpMetadata RunProjectOp where
  runtimeDataflowOpContractOf _ =
    mkRuntimeDataflowContract PhaseProject Set.empty Set.empty
  {-# INLINE runtimeDataflowOpContractOf #-}

  runtimeDataflowOpContextOf (RunProjectOp contextValue _shard _event) =
    contextValue
  {-# INLINE runtimeDataflowOpContextOf #-}

  runtimeDataflowOpKeyOf _ =
    Nothing
  {-# INLINE runtimeDataflowOpKeyOf #-}

  runtimeDataflowOpTransportOf _ =
    Nothing
  {-# INLINE runtimeDataflowOpTransportOf #-}

instance RuntimeDataflowOpMetadata RunRestrictOp where
  runtimeDataflowOpContractOf (RunRestrictOp _shard deltaValue) =
    mkRuntimeDataflowContract PhaseRestrict (Set.singleton (deAddr deltaValue)) Set.empty
  {-# INLINE runtimeDataflowOpContractOf #-}

  runtimeDataflowOpContextOf (RunRestrictOp _shard deltaValue) =
    caContext (deAddr deltaValue)
  {-# INLINE runtimeDataflowOpContextOf #-}

  runtimeDataflowOpKeyOf _ =
    Nothing
  {-# INLINE runtimeDataflowOpKeyOf #-}

  runtimeDataflowOpTransportOf _ =
    Nothing
  {-# INLINE runtimeDataflowOpTransportOf #-}

instance RuntimeDataflowOpMetadata RunIndexOp where
  runtimeDataflowOpContractOf (RunIndexOp _shard deltaValue) =
    mkRuntimeDataflowContract PhaseIndex Set.empty (Set.singleton (deAddr deltaValue))
  {-# INLINE runtimeDataflowOpContractOf #-}

  runtimeDataflowOpContextOf (RunIndexOp _shard deltaValue) =
    caContext (deAddr deltaValue)
  {-# INLINE runtimeDataflowOpContextOf #-}

  runtimeDataflowOpKeyOf _ =
    Nothing
  {-# INLINE runtimeDataflowOpKeyOf #-}

  runtimeDataflowOpTransportOf _ =
    Nothing
  {-# INLINE runtimeDataflowOpTransportOf #-}

instance RuntimeDataflowOpMetadata RepairFactorBatchOp where
  runtimeDataflowOpContractOf (RepairFactorBatchOp _request atomAddrs rootAddrs) =
    mkRuntimeDataflowContract PhaseProject atomAddrs rootAddrs
  {-# INLINE runtimeDataflowOpContractOf #-}

  runtimeDataflowOpContextOf (RepairFactorBatchOp request _atomAddrs _rootAddrs) =
    frbrContext request
  {-# INLINE runtimeDataflowOpContextOf #-}

  runtimeDataflowOpKeyOf (RepairFactorBatchOp request _atomAddrs _rootAddrs) =
    Just (RepairFactorBatchKey (frbrContext request) (frbrProp request))
  {-# INLINE runtimeDataflowOpKeyOf #-}

  runtimeDataflowOpTransportOf _ =
    Nothing
  {-# INLINE runtimeDataflowOpTransportOf #-}

  mergeRuntimeDataflowOpKindOf (RepairFactorBatchOp newer _ _) (RepairFactorBatchOp older _ _) =
    Just (repairFactorBatchDataflowOpKind (mergeRepairBatchRequests newer older))
  {-# INLINE mergeRuntimeDataflowOpKindOf #-}

instance RuntimeDataflowOpMetadata DeriveSubsumedCarrierOp where
  runtimeDataflowOpContractOf (DeriveSubsumedCarrierOp _reuseId source target) =
    mkRuntimeDataflowContract PhaseSubsumption (Set.singleton source) (Set.singleton target)
  {-# INLINE runtimeDataflowOpContractOf #-}

  runtimeDataflowOpContextOf (DeriveSubsumedCarrierOp _reuseId _source target) =
    caContext target
  {-# INLINE runtimeDataflowOpContextOf #-}

  runtimeDataflowOpKeyOf (DeriveSubsumedCarrierOp reuseId source target) =
    Just (DeriveSubsumedCarrierKey reuseId source target)
  {-# INLINE runtimeDataflowOpKeyOf #-}

  runtimeDataflowOpTransportOf (DeriveSubsumedCarrierOp reuseId source target) =
    Just (TransportViaSubsumption reuseId source target)
  {-# INLINE runtimeDataflowOpTransportOf #-}

instance RuntimeDataflowOpMetadata RestrictCarrierOp where
  runtimeDataflowOpContractOf (RestrictCarrierOp key) =
    mkRuntimeDataflowContract PhaseRestrict (Set.singleton (rkSource key)) (Set.singleton (rkTarget key))
  {-# INLINE runtimeDataflowOpContractOf #-}

  runtimeDataflowOpContextOf (RestrictCarrierOp key) =
    caContext (rkTarget key)
  {-# INLINE runtimeDataflowOpContextOf #-}

  runtimeDataflowOpKeyOf (RestrictCarrierOp key) =
    Just (RestrictCarrierKey key)
  {-# INLINE runtimeDataflowOpKeyOf #-}

  runtimeDataflowOpTransportOf (RestrictCarrierOp key) =
    Just (TransportViaRestriction key)
  {-# INLINE runtimeDataflowOpTransportOf #-}

instance RuntimeDataflowOpMetadata AmalgamateCarrierFamilyOp where
  runtimeDataflowOpContractOf (AmalgamateCarrierFamilyOp _family members targets) =
    mkRuntimeDataflowContract PhaseAmalgamate members targets
  {-# INLINE runtimeDataflowOpContractOf #-}

  runtimeDataflowOpContextOf (AmalgamateCarrierFamilyOp family _members _targets) =
    carrierFamilyTargetContext family
  {-# INLINE runtimeDataflowOpContextOf #-}

  runtimeDataflowOpKeyOf (AmalgamateCarrierFamilyOp family _members _targets) =
    Just (AmalgamateCarrierFamilyKey family)
  {-# INLINE runtimeDataflowOpKeyOf #-}

  runtimeDataflowOpTransportOf (AmalgamateCarrierFamilyOp family _members _targets) =
    Just (TransportViaAmalgamation family)
  {-# INLINE runtimeDataflowOpTransportOf #-}

type RuntimeDataflowOp :: Type -> Type -> Type -> Type -> Type
data RuntimeDataflowOp ctx prop boundary evidence = RuntimeDataflowOp
  { rdopKind :: !(RuntimeDataflowOpKind ctx prop boundary evidence),
    rdopContract :: !(RuntimeDataflowContract ctx Carrier prop),
    rdopContext :: !ctx,
    rdopKey :: !(Maybe (RuntimeDataflowOpKey ctx prop)),
    rdopTransport :: !(Maybe (RelationalCapabilityTransport ctx prop))
  }
  deriving stock (Eq, Show)

type ScheduledRuntimeDataflowOp :: Type -> Type -> Type -> Type -> Type
type ScheduledRuntimeDataflowOp ctx prop boundary evidence =
  Timed
    (RelationalCarrierTime ctx)
    (RuntimeDataflowOp ctx prop boundary evidence)

runtimeDataflowOpKind ::
  RuntimeDataflowOp ctx prop boundary evidence ->
  RuntimeDataflowOpKind ctx prop boundary evidence
runtimeDataflowOpKind =
  rdopKind
{-# INLINE runtimeDataflowOpKind #-}

runtimeDataflowOpContract ::
  RuntimeDataflowOp ctx prop boundary evidence ->
  RuntimeDataflowContract ctx Carrier prop
runtimeDataflowOpContract =
  rdopContract
{-# INLINE runtimeDataflowOpContract #-}

runtimeDataflowOpContext ::
  RuntimeDataflowOp ctx prop boundary evidence ->
  ctx
runtimeDataflowOpContext =
  rdopContext
{-# INLINE runtimeDataflowOpContext #-}

runtimeDataflowOpKey ::
  RuntimeDataflowOp ctx prop boundary evidence ->
  Maybe (RuntimeDataflowOpKey ctx prop)
runtimeDataflowOpKey =
  rdopKey
{-# INLINE runtimeDataflowOpKey #-}

runtimeDataflowOpTransport ::
  RuntimeDataflowOp ctx prop boundary evidence ->
  Maybe (RelationalCapabilityTransport ctx prop)
runtimeDataflowOpTransport =
  rdopTransport
{-# INLINE runtimeDataflowOpTransport #-}

runtimeDataflowOpFromKind ::
  RuntimeDataflowOpKind ctx prop boundary evidence ->
  RuntimeDataflowOp ctx prop boundary evidence
runtimeDataflowOpFromKind !kind =
  RuntimeDataflowOp
    { rdopKind = kind,
      rdopContract = runtimeDataflowOpContractOf kind,
      rdopContext = runtimeDataflowOpContextOf kind,
      rdopKey = runtimeDataflowOpKeyOf kind,
      rdopTransport = runtimeDataflowOpTransportOf kind
    }
{-# INLINE runtimeDataflowOpFromKind #-}

runtimeDataflowOpProgressPointstamps ::
  ScheduledRuntimeDataflowOp ctx prop boundary evidence ->
  [RelationalCarrierTime ctx]
runtimeDataflowOpProgressPointstamps timedOp =
  frontierPoints (singletonFrontier progressTime)
  where
    !contract =
      runtimeDataflowOpContract (timedValue timedOp)

    !progressTime =
      retimeRelationalCarrierPhase
        (runtimeDataflowContractPhase contract)
        (timedAt timedOp)
{-# INLINE runtimeDataflowOpProgressPointstamps #-}

applyAtomEventsDataflowOp ::
  CarrierAddr ctx Carrier prop ->
  RelationalScope ->
  NonEmpty AtomEvent ->
  RuntimeDataflowOp ctx prop boundary evidence
applyAtomEventsDataflowOp addr scope events =
  runtimeDataflowOpFromKind
    (applyAtomEventsDataflowOpKind addr scope events)
{-# INLINE applyAtomEventsDataflowOp #-}

applyAtomEventsDataflowOpKind ::
  CarrierAddr ctx Carrier prop ->
  RelationalScope ->
  NonEmpty AtomEvent ->
  RuntimeDataflowOpKind ctx prop boundary evidence
applyAtomEventsDataflowOpKind addr scope events =
  RuntimeDataflowOpKind (InL (ApplyAtomEventsOp addr scope events))
{-# INLINE applyAtomEventsDataflowOpKind #-}

runProjectDataflowOp ::
  ctx ->
  Shard ->
  LocalRelationalEvent ->
  RuntimeDataflowOp ctx prop boundary evidence
runProjectDataflowOp contextValue shard event =
  runtimeDataflowOpFromKind
    (runProjectDataflowOpKind contextValue shard event)
{-# INLINE runProjectDataflowOp #-}

runProjectDataflowOpKind ::
  ctx ->
  Shard ->
  LocalRelationalEvent ->
  RuntimeDataflowOpKind ctx prop boundary evidence
runProjectDataflowOpKind contextValue shard event =
  RuntimeDataflowOpKind (InR (InL (RunProjectOp contextValue shard event)))
{-# INLINE runProjectDataflowOpKind #-}

runRestrictDataflowOp ::
  Shard ->
  RelationalCarrierDelta ctx Carrier prop boundary evidence ->
  RuntimeDataflowOp ctx prop boundary evidence
runRestrictDataflowOp shard deltaValue =
  runtimeDataflowOpFromKind
    (runRestrictDataflowOpKind shard deltaValue)
{-# INLINE runRestrictDataflowOp #-}

runRestrictDataflowOpKind ::
  Shard ->
  RelationalCarrierDelta ctx Carrier prop boundary evidence ->
  RuntimeDataflowOpKind ctx prop boundary evidence
runRestrictDataflowOpKind shard deltaValue =
  RuntimeDataflowOpKind (InR (InR (InL (RunRestrictOp shard deltaValue))))
{-# INLINE runRestrictDataflowOpKind #-}

runIndexDataflowOp ::
  Shard ->
  RelationalCarrierDelta ctx Carrier prop boundary evidence ->
  RuntimeDataflowOp ctx prop boundary evidence
runIndexDataflowOp shard deltaValue =
  runtimeDataflowOpFromKind
    (runIndexDataflowOpKind shard deltaValue)
{-# INLINE runIndexDataflowOp #-}

runIndexDataflowOpKind ::
  Shard ->
  RelationalCarrierDelta ctx Carrier prop boundary evidence ->
  RuntimeDataflowOpKind ctx prop boundary evidence
runIndexDataflowOpKind shard deltaValue =
  RuntimeDataflowOpKind (InR (InR (InR (InL (RunIndexOp shard deltaValue)))))
{-# INLINE runIndexDataflowOpKind #-}

repairFactorBatchDataflowOp ::
  (Ord ctx, Ord prop) =>
  FactorRepairBatchRequest ctx prop ->
  RuntimeDataflowOp ctx prop boundary evidence
repairFactorBatchDataflowOp request =
  runtimeDataflowOpFromKind
    (repairFactorBatchDataflowOpKind request)
{-# INLINE repairFactorBatchDataflowOp #-}

repairFactorBatchDataflowOpKind ::
  (Ord ctx, Ord prop) =>
  FactorRepairBatchRequest ctx prop ->
  RuntimeDataflowOpKind ctx prop boundary evidence
repairFactorBatchDataflowOpKind request =
  RuntimeDataflowOpKind
    ( InR
        ( InR
            ( InR
                ( InR
                    ( InL
                        ( RepairFactorBatchOp
                            request
                            (factorRepairBatchAtomAddrs request)
                            (factorRepairBatchRootAddrs request)
                        )
                    )
                )
            )
        )
    )
{-# INLINE repairFactorBatchDataflowOpKind #-}

fullRepairFactorBatchDataflowOp ::
  (Ord ctx, Ord prop) =>
  ctx ->
  PropositionKey prop ->
  RepairProgramKey ->
  QueryId ->
  FactorFullRepairReason ->
  RuntimeDataflowOp ctx prop boundary evidence
fullRepairFactorBatchDataflowOp contextValue propKey repairKey queryId reason =
  repairFactorBatchDataflowOp
    ( singletonRepairBatchRequest
        contextValue
        propKey
        repairKey
        queryId
        (fullRepair reason)
    )
{-# INLINE fullRepairFactorBatchDataflowOp #-}

deriveSubsumedCarrierDataflowOp ::
  CarrierReuseId ctx prop ->
  CarrierAddr ctx Carrier prop ->
  CarrierAddr ctx Carrier prop ->
  RuntimeDataflowOp ctx prop boundary evidence
deriveSubsumedCarrierDataflowOp reuseId source target =
  runtimeDataflowOpFromKind
    (deriveSubsumedCarrierDataflowOpKind reuseId source target)
{-# INLINE deriveSubsumedCarrierDataflowOp #-}

deriveSubsumedCarrierDataflowOpKind ::
  CarrierReuseId ctx prop ->
  CarrierAddr ctx Carrier prop ->
  CarrierAddr ctx Carrier prop ->
  RuntimeDataflowOpKind ctx prop boundary evidence
deriveSubsumedCarrierDataflowOpKind reuseId source target =
  RuntimeDataflowOpKind
    (InR (InR (InR (InR (InR (InL (DeriveSubsumedCarrierOp reuseId source target)))))))
{-# INLINE deriveSubsumedCarrierDataflowOpKind #-}

restrictCarrierDataflowOp ::
  RestrictKey ctx Carrier prop ->
  RuntimeDataflowOp ctx prop boundary evidence
restrictCarrierDataflowOp key =
  runtimeDataflowOpFromKind
    (restrictCarrierDataflowOpKind key)
{-# INLINE restrictCarrierDataflowOp #-}

restrictCarrierDataflowOpKind ::
  RestrictKey ctx Carrier prop ->
  RuntimeDataflowOpKind ctx prop boundary evidence
restrictCarrierDataflowOpKind key =
  RuntimeDataflowOpKind
    (InR (InR (InR (InR (InR (InR (InL (RestrictCarrierOp key))))))))
{-# INLINE restrictCarrierDataflowOpKind #-}

amalgamateCarrierFamilyDataflowOp ::
  (Ord ctx, Ord prop) =>
  CarrierFamily ctx Carrier prop ->
  RuntimeDataflowOp ctx prop boundary evidence
amalgamateCarrierFamilyDataflowOp family =
  runtimeDataflowOpFromKind
    (amalgamateCarrierFamilyDataflowOpKind family)
{-# INLINE amalgamateCarrierFamilyDataflowOp #-}

amalgamateCarrierFamilyDataflowOpKind ::
  (Ord ctx, Ord prop) =>
  CarrierFamily ctx Carrier prop ->
  RuntimeDataflowOpKind ctx prop boundary evidence
amalgamateCarrierFamilyDataflowOpKind family =
  RuntimeDataflowOpKind
    ( InR
        ( InR
            ( InR
                ( InR
                    ( InR
                        ( InR
                            ( InR
                                ( AmalgamateCarrierFamilyOp
                                    family
                                    (carrierFamilyMembers family)
                                    (carrierFamilyTargets family)
                                )
                            )
                        )
                    )
                )
            )
        )
    )
{-# INLINE amalgamateCarrierFamilyDataflowOpKind #-}

factorRepairBatchRootAddrs ::
  (Ord ctx, Ord prop) =>
  FactorRepairBatchRequest ctx prop ->
  Set (CarrierAddr ctx Carrier prop)
factorRepairBatchRootAddrs request =
  Set.fromList
    [ carrierAddr
        (frbrContext request)
        (frbrProp request)
        (queryRootCarrier queryId)
    | queryId <- Map.keys (frbrRepairs request)
    ]
{-# INLINE factorRepairBatchRootAddrs #-}

factorRepairBatchAtomAddrs ::
  (Ord ctx, Ord prop) =>
  FactorRepairBatchRequest ctx prop ->
  Set (CarrierAddr ctx Carrier prop)
factorRepairBatchAtomAddrs request =
  Set.fromList
    [ carrierAddr
        (frbrContext request)
        (frbrProp request)
        (queryAtomCarrier queryId (mkAtomId atomKey))
    | (queryId, member) <- Map.toAscList (frbrRepairs request),
      atomKey <- IntMap.keys (repairCauseAtomDeltas (frbmCause member))
    ]
{-# INLINE factorRepairBatchAtomAddrs #-}
