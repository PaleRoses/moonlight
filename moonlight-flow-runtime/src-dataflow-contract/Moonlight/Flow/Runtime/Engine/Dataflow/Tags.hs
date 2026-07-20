{-# LANGUAGE OverloadedStrings #-}

module Moonlight.Flow.Runtime.Engine.Dataflow.Tags
  ( runtimeDataflowNodeKindForCarrier,
    runtimeDataflowCarrierLabel,
    runtimeDataflowOpViewKind,
    runtimeDataflowOpViewKindLabel,
    runtimeDataflowPhaseView,
    runtimeDataflowPhaseTag,
    runtimeDataflowPhaseLabel,
    runtimeDataflowNodeKindTag,
    runtimeDataflowEdgeKindTag,
    runtimeDataflowOpViewKindTag,
    runtimeDataflowRepairNodeActionTag,
  )
where

import Data.Text
  ( Text,
  )
import Data.Text qualified as Text
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    QueryCarrierNode (..),
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (..),
  )
import Moonlight.Flow.Plan.Query.Core
  ( FactorNode (..),
  )
import Moonlight.Flow.Runtime.Engine.Dataflow.Types
import Moonlight.Flow.Runtime.Execution.IR
  ( RuntimeDataflowOpKind,
    foldRuntimeDataflowOpKind,
  )

runtimeDataflowNodeKindForCarrier :: Carrier -> RuntimeDataflowNodeKind
runtimeDataflowNodeKindForCarrier carrierValue =
  case carrierValue of
    QueryCarrier _queryId (QueryAtom _atomId) ->
      DataflowNodeAtom
    QueryCarrier _queryId (QueryFactor FactorNodeRoot) ->
      DataflowNodeQueryRoot
    QueryCarrier _queryId (QueryFactor _) ->
      DataflowNodeQueryFactor
    DerivedCarrier _derivedId ->
      DataflowNodeDerived
{-# INLINE runtimeDataflowNodeKindForCarrier #-}

runtimeDataflowCarrierLabel :: Carrier -> Text
runtimeDataflowCarrierLabel carrierValue =
  case carrierValue of
    QueryCarrier queryId (QueryAtom atomId) ->
      Text.concat ["atom ", showText atomId, " / ", showText queryId]
    QueryCarrier queryId (QueryFactor FactorNodeRoot) ->
      Text.concat ["query root ", showText queryId]
    QueryCarrier queryId (QueryFactor factorNode) ->
      Text.concat ["factor ", showText factorNode, " / ", showText queryId]
    DerivedCarrier derivedId ->
      Text.concat ["derived ", showText derivedId]
{-# INLINE runtimeDataflowCarrierLabel #-}

runtimeDataflowOpViewKind ::
  RuntimeDataflowOpKind ctx prop boundary evidence ->
  RuntimeDataflowOpViewKind
runtimeDataflowOpViewKind kind =
  foldRuntimeDataflowOpKind
    kind
    (const DataflowOpViewApplyAtomEvents)
    (const DataflowOpViewRunProject)
    (const DataflowOpViewRunRestrict)
    (const DataflowOpViewRunIndex)
    (const DataflowOpViewRepairFactorBatch)
    (const DataflowOpViewDeriveSubsumedCarrier)
    (const DataflowOpViewRestrictCarrier)
    (const DataflowOpViewAmalgamateCarrierFamily)
{-# INLINE runtimeDataflowOpViewKind #-}

runtimeDataflowOpViewKindLabel :: RuntimeDataflowOpViewKind -> Text
runtimeDataflowOpViewKindLabel kind =
  case kind of
    DataflowOpViewApplyAtomEvents -> "apply atom events"
    DataflowOpViewRunProject -> "run project"
    DataflowOpViewRunRestrict -> "run restrict"
    DataflowOpViewRunIndex -> "run index"
    DataflowOpViewRepairFactorBatch -> "repair factor batch"
    DataflowOpViewDeriveSubsumedCarrier -> "derive subsumed carrier"
    DataflowOpViewRestrictCarrier -> "restrict carrier"
    DataflowOpViewAmalgamateCarrierFamily -> "amalgamate carrier family"
{-# INLINE runtimeDataflowOpViewKindLabel #-}

runtimeDataflowPhaseView :: RelationalPhase -> RuntimeDataflowPhaseView
runtimeDataflowPhaseView phaseValue =
  RuntimeDataflowPhaseView
    { rdpvTag = runtimeDataflowPhaseTag phaseValue,
      rdpvLabel = runtimeDataflowPhaseLabel phaseValue
    }
{-# INLINE runtimeDataflowPhaseView #-}

runtimeDataflowPhaseTag :: RelationalPhase -> Text
runtimeDataflowPhaseTag phaseValue =
  case phaseValue of
    PhaseJoin -> "join"
    PhaseProject -> "project"
    PhaseSubsumption -> "subsumption"
    PhaseRestrict -> "restrict"
    PhaseAmalgamate -> "amalgamate"
    PhaseIndex -> "index"
    PhaseVisible -> "visible"
    PhaseObstruction -> "obstruction"
{-# INLINE runtimeDataflowPhaseTag #-}

runtimeDataflowPhaseLabel :: RelationalPhase -> Text
runtimeDataflowPhaseLabel phaseValue =
  case phaseValue of
    PhaseJoin -> "Join"
    PhaseProject -> "Project"
    PhaseSubsumption -> "Subsumption"
    PhaseRestrict -> "Restrict"
    PhaseAmalgamate -> "Amalgamate"
    PhaseIndex -> "Index"
    PhaseVisible -> "Visible"
    PhaseObstruction -> "Obstruction"
{-# INLINE runtimeDataflowPhaseLabel #-}

runtimeDataflowNodeKindTag :: RuntimeDataflowNodeKind -> Text
runtimeDataflowNodeKindTag kind =
  case kind of
    DataflowNodeTouch -> "touch"
    DataflowNodeAtom -> "atom"
    DataflowNodeQueryRoot -> "queryRoot"
    DataflowNodeQueryFactor -> "queryFactor"
    DataflowNodeDerived -> "derived"
    DataflowNodeOperation -> "operation"
{-# INLINE runtimeDataflowNodeKindTag #-}

runtimeDataflowEdgeKindTag :: RuntimeDataflowEdgeKind -> Text
runtimeDataflowEdgeKindTag kind =
  case kind of
    DataflowEdgeTouch -> "touch"
    DataflowEdgeRestriction -> "restriction"
    DataflowEdgeSubsumption -> "subsumption"
    DataflowEdgeAmalgamation -> "amalgamation"
    DataflowEdgeScheduleRead -> "scheduleRead"
    DataflowEdgeScheduleWrite -> "scheduleWrite"
{-# INLINE runtimeDataflowEdgeKindTag #-}

runtimeDataflowOpViewKindTag :: RuntimeDataflowOpViewKind -> Text
runtimeDataflowOpViewKindTag kind =
  case kind of
    DataflowOpViewApplyAtomEvents -> "applyAtomEvents"
    DataflowOpViewRunProject -> "runProject"
    DataflowOpViewRunRestrict -> "runRestrict"
    DataflowOpViewRunIndex -> "runIndex"
    DataflowOpViewRepairFactorBatch -> "repairFactorBatch"
    DataflowOpViewDeriveSubsumedCarrier -> "deriveSubsumedCarrier"
    DataflowOpViewRestrictCarrier -> "restrictCarrier"
    DataflowOpViewAmalgamateCarrierFamily -> "amalgamateCarrierFamily"
{-# INLINE runtimeDataflowOpViewKindTag #-}

runtimeDataflowRepairNodeActionTag :: RuntimeRepairNodeAction -> Text
runtimeDataflowRepairNodeActionTag action =
  case action of
    RuntimeRepairNodeBuilt -> "built"
    RuntimeRepairNodeReused -> "reused"
    RuntimeRepairNodePatched -> "patched"
{-# INLINE runtimeDataflowRepairNodeActionTag #-}

showText :: Show a => a -> Text
showText =
  Text.pack . show
{-# INLINE showText #-}
