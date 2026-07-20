{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Runtime.Engine.Dataflow.Build
  ( runtimeDataflowSnapshot,
    runtimeDataflowSnapshotWith,
    runtimeDataflowSnapshotFromRuntime,
    runtimeDataflowSnapshotFromRuntimeWith,
    runtimeDataflowSnapshotFromTopologyQueue,
    runtimeDataflowOrphanEdges,
    dataflowNodeIdForCarrierAddr,
    dataflowNodeForCarrierAddr,
    runtimeDataflowFrontier,
  )
where

import Data.Foldable qualified as Foldable
import Data.IntSet qualified as IntSet
import Data.Kind
  ( Type,
  )
import Data.List
  ( mapAccumL,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Data.Text
  ( Text,
  )
import Data.Text qualified as Text
import Moonlight.Differential.Frontier
  ( RuntimeFrontier,
    frontierPendingCounts,
    frontierTraceRetention,
    frontierVisibleMinimums,
    traceRetentionExactEvidenceTraceIds,
    traceRetentionPinnedTraceIds,
    traceRetentionProvenanceTraceIds,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    QueryCarrierNode (..),
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
    caProp,
    caCarrier,
    rkSource,
    rkTarget,
  )
import Moonlight.Flow.Carrier.Core.Topology
  ( CarrierEdge (..),
    CarrierTopology,
    TouchKey,
    carrierTopologyAddresses,
    carrierTopologyEdges,
    carrierTopologyTouches,
  )
import Moonlight.Differential.Carrier.Topology
  ( carrierFamilyTargets,
  )
import Moonlight.Flow.Plan.Query.Core
  ( FactorNode (..),
  )
import Moonlight.Flow.Runtime.Core.Env
  ( RuntimeEnvelope (..),
  )
import Moonlight.Flow.Runtime.Engine.Dataflow.Tags
  ( runtimeDataflowCarrierLabel,
    runtimeDataflowEdgeKindTag,
    runtimeDataflowNodeKindForCarrier,
    runtimeDataflowOpViewKind,
    runtimeDataflowOpViewKindLabel,
    runtimeDataflowOpViewKindTag,
    runtimeDataflowPhaseTag,
    runtimeDataflowPhaseView,
  )
import Moonlight.Flow.Runtime.Engine.Dataflow.Types
import Moonlight.Flow.Runtime.Engine.Queue.Types
  ( RuntimeDataflowQueue,
    runtimeDataflowQueueFrontier,
    runtimeDataflowQueuedOps,
  )
import Moonlight.Flow.Runtime.Engine.State
  ( runtimeEngineQueue,
  )
import Moonlight.Flow.Runtime.Execution.IR
  ( RuntimeDataflowOp,
    runtimeDataflowContractPhase,
    runtimeDataflowContractReads,
    runtimeDataflowContractWrites,
    runtimeDataflowOpContract,
    runtimeDataflowOpContext,
    runtimeDataflowOpKind,
  )
import Moonlight.Flow.Runtime.Kernel
  ( RelDiffRuntime,
    rsCarrierTopology,
  )
import Moonlight.Flow.Runtime.Types qualified as RuntimeTypes

type RuntimeDataflowGraph :: Type
data RuntimeDataflowGraph = RuntimeDataflowGraph
  { rdgNodes :: !(Map DataflowNodeId RuntimeDataflowNode),
    rdgEdges :: !(Map DataflowEdgeId RuntimeDataflowEdge),
    rdgOps :: !(Map DataflowOpViewId RuntimeDataflowOpView),
    rdgOrphanEdges :: !(Set DataflowEdgeId)
  }
  deriving stock (Eq, Show)

type RuntimeDataflowOpViewDraft :: Type
data RuntimeDataflowOpViewDraft = RuntimeDataflowOpViewDraft
  { rdovBaseId :: !Text,
    rdovView :: !RuntimeDataflowOpView
  }
  deriving stock (Eq, Show)

runtimeDataflowSnapshot ::
  (Ord ctx, Ord prop, Show ctx, Show prop) =>
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RuntimeDataflowSnapshot
runtimeDataflowSnapshot =
  runtimeDataflowSnapshotWith defaultRuntimeDataflowRenderers
{-# INLINE runtimeDataflowSnapshot #-}

runtimeDataflowSnapshotWith ::
  (Ord ctx, Ord prop) =>
  RuntimeDataflowRenderers ctx prop ->
  RelDiffRuntime ctx prop boundary evidence joinState joinErr ->
  RuntimeDataflowSnapshot
runtimeDataflowSnapshotWith renderers runtime =
  runtimeDataflowSnapshotFromTopologyQueue
    renderers
    (rsCarrierTopology stateValue)
    (runtimeEngineQueue stateValue)
  where
    stateValue =
      rdrState runtime
{-# INLINE runtimeDataflowSnapshotWith #-}

runtimeDataflowSnapshotFromRuntime ::
  (Ord ctx, Ord prop, Show ctx, Show prop) =>
  RuntimeTypes.Runtime ctx prop ->
  RuntimeDataflowSnapshot
runtimeDataflowSnapshotFromRuntime =
  runtimeDataflowSnapshotFromRuntimeWith defaultRuntimeDataflowRenderers
{-# INLINE runtimeDataflowSnapshotFromRuntime #-}

runtimeDataflowSnapshotFromRuntimeWith ::
  (Ord ctx, Ord prop) =>
  RuntimeDataflowRenderers ctx prop ->
  RuntimeTypes.Runtime ctx prop ->
  RuntimeDataflowSnapshot
runtimeDataflowSnapshotFromRuntimeWith renderers (RuntimeTypes.Runtime runtime) =
  runtimeDataflowSnapshotWith renderers runtime
{-# INLINE runtimeDataflowSnapshotFromRuntimeWith #-}

runtimeDataflowSnapshotFromTopologyQueue ::
  (Ord ctx, Ord prop) =>
  RuntimeDataflowRenderers ctx prop ->
  CarrierTopology ctx Carrier prop ->
  RuntimeDataflowQueue ctx prop boundary evidence ->
  RuntimeDataflowSnapshot
runtimeDataflowSnapshotFromTopologyQueue renderers topology queue =
  let !graph =
        runtimeDataflowGraphFromTopologyQueue renderers topology queue
   in RuntimeDataflowSnapshot
        { rdsVersion = 2,
          rdsNodes = Map.elems (rdgNodes graph),
          rdsEdges = Map.elems (rdgEdges graph),
          rdsOps = Map.elems (rdgOps graph),
          rdsFrontier = runtimeDataflowFrontier (runtimeDataflowQueueFrontier queue),
          rdsWorkload = Nothing,
          rdsDiagnostics = runtimeDataflowDiagnosticsFromGraph graph
        }
{-# INLINE runtimeDataflowSnapshotFromTopologyQueue #-}

runtimeDataflowGraphFromTopologyQueue ::
  (Ord ctx, Ord prop) =>
  RuntimeDataflowRenderers ctx prop ->
  CarrierTopology ctx Carrier prop ->
  RuntimeDataflowQueue ctx prop boundary evidence ->
  RuntimeDataflowGraph
runtimeDataflowGraphFromTopologyQueue renderers topology queue =
  let !runtimeOps =
        runtimeDataflowOpsForQueue queue
      !opViews =
        runtimeDataflowOpViewsForRuntimeDataflowOps renderers runtimeOps
      !nodes =
        runtimeDataflowNodeMapForTopologyOps renderers topology runtimeOps opViews
      !edges =
        edgeMapFromList
          ( runtimeDataflowEdgesForTopologyOps
              renderers
              topology
              opViews
          )
      !orphans =
        runtimeDataflowOrphanEdgesFrom (Map.keysSet nodes) (Map.elems edges)
   in RuntimeDataflowGraph
        { rdgNodes = nodes,
          rdgEdges = edges,
          rdgOps = opMapFromList opViews,
          rdgOrphanEdges = orphans
        }
{-# INLINE runtimeDataflowGraphFromTopologyQueue #-}

runtimeDataflowDiagnosticsFromGraph ::
  RuntimeDataflowGraph ->
  RuntimeDataflowDiagnostics
runtimeDataflowDiagnosticsFromGraph graph =
  RuntimeDataflowDiagnostics
    { rddNodeCount = Map.size (rdgNodes graph),
      rddEdgeCount = Map.size (rdgEdges graph),
      rddOpCount = Map.size (rdgOps graph),
      rddOrphanEdgeCount = Set.size (rdgOrphanEdges graph)
    }
{-# INLINE runtimeDataflowDiagnosticsFromGraph #-}

runtimeDataflowOrphanEdges :: RuntimeDataflowSnapshot -> Set DataflowEdgeId
runtimeDataflowOrphanEdges snapshot =
  runtimeDataflowOrphanEdgesFrom
    (Set.fromList (fmap rdnId (rdsNodes snapshot)))
    (rdsEdges snapshot)
{-# INLINE runtimeDataflowOrphanEdges #-}

runtimeDataflowOrphanEdgesFrom ::
  Set DataflowNodeId ->
  [RuntimeDataflowEdge] ->
  Set DataflowEdgeId
runtimeDataflowOrphanEdgesFrom nodeIds edges =
  Set.fromList
    [ rdeId edge
    | edge <- edges,
      not
        ( Set.member (rdeSource edge) nodeIds
            && Set.member (rdeTarget edge) nodeIds
        )
    ]
{-# INLINE runtimeDataflowOrphanEdgesFrom #-}

runtimeDataflowNodeMapForTopologyOps ::
  (Ord ctx, Ord prop) =>
  RuntimeDataflowRenderers ctx prop ->
  CarrierTopology ctx Carrier prop ->
  [RuntimeDataflowOp ctx prop boundary evidence] ->
  [RuntimeDataflowOpView] ->
  Map DataflowNodeId RuntimeDataflowNode
runtimeDataflowNodeMapForTopologyOps renderers topology runtimeOps opViews =
  Foldable.foldl'
    insertNode
    Map.empty
    ( topologyCarrierNodes
        <> touchNodes
        <> foldMap (runtimeDataflowCarrierNodesForOp renderers) runtimeOps
        <> fmap runtimeDataflowNodeForOp opViews
    )
  where
    topologyCarrierNodes =
      fmap
        (dataflowNodeForCarrierAddr renderers)
        (Set.toAscList (carrierTopologyAddresses topology))

    touchNodes =
      fmap
        (touchNodeForTouchKey . fst)
        (carrierTopologyTouches topology)
{-# INLINE runtimeDataflowNodeMapForTopologyOps #-}

insertNode ::
  Map DataflowNodeId RuntimeDataflowNode ->
  RuntimeDataflowNode ->
  Map DataflowNodeId RuntimeDataflowNode
insertNode nodes node =
  Map.insert (rdnId node) node nodes
{-# INLINE insertNode #-}

runtimeDataflowCarrierNodesForOp ::
  (Ord ctx, Ord prop) =>
  RuntimeDataflowRenderers ctx prop ->
  RuntimeDataflowOp ctx prop boundary evidence ->
  [RuntimeDataflowNode]
runtimeDataflowCarrierNodesForOp renderers op =
  fmap
    (dataflowNodeForCarrierAddr renderers)
    (Set.toAscList (runtimeDataflowContractReads contract <> runtimeDataflowContractWrites contract))
  where
    contract =
      runtimeDataflowOpContract op
{-# INLINE runtimeDataflowCarrierNodesForOp #-}

edgeMapFromList :: [RuntimeDataflowEdge] -> Map DataflowEdgeId RuntimeDataflowEdge
edgeMapFromList =
  Map.fromList . fmap (\edge -> (rdeId edge, edge))
{-# INLINE edgeMapFromList #-}

opMapFromList :: [RuntimeDataflowOpView] -> Map DataflowOpViewId RuntimeDataflowOpView
opMapFromList =
  Map.fromList . fmap (\op -> (rdoId op, op))
{-# INLINE opMapFromList #-}

runtimeDataflowEdgesForTopologyOps ::
  (Ord ctx, Ord prop) =>
  RuntimeDataflowRenderers ctx prop ->
  CarrierTopology ctx Carrier prop ->
  [RuntimeDataflowOpView] ->
  [RuntimeDataflowEdge]
runtimeDataflowEdgesForTopologyOps renderers topology ops =
  assignRuntimeDataflowEdgeIds
    ( runtimeDataflowTouchEdges renderers topology
        <> runtimeDataflowTopologyEdges renderers topology
        <> runtimeDataflowScheduleEdges ops
    )
{-# INLINE runtimeDataflowEdgesForTopologyOps #-}

runtimeDataflowTouchEdges ::
  RuntimeDataflowRenderers ctx prop ->
  CarrierTopology ctx Carrier prop ->
  [RuntimeDataflowEdge]
runtimeDataflowTouchEdges renderers topology =
  [ RuntimeDataflowEdge
      { rdeId = DataflowEdgeId mempty,
        rdeKind = DataflowEdgeTouch,
        rdeSource = touchNodeIdForTouchKey touchKey,
        rdeTarget = dataflowNodeIdForCarrierAddr renderers addr,
        rdeLabel = showText touchKey
      }
  | (touchKey, addr) <- carrierTopologyTouches topology
  ]
{-# INLINE runtimeDataflowTouchEdges #-}

runtimeDataflowTopologyEdges ::
  (Ord ctx, Ord prop) =>
  RuntimeDataflowRenderers ctx prop ->
  CarrierTopology ctx Carrier prop ->
  [RuntimeDataflowEdge]
runtimeDataflowTopologyEdges renderers topology =
  [ RuntimeDataflowEdge
      { rdeId = DataflowEdgeId mempty,
        rdeKind = kind,
        rdeSource = dataflowNodeIdForCarrierAddr renderers source,
        rdeTarget = dataflowNodeIdForCarrierAddr renderers target,
        rdeLabel = label
      }
  | (anchor, edge) <- carrierTopologyEdges topology,
    (kind, source, target, label) <- runtimeDataflowEdgeEndpoints anchor edge
  ]
{-# INLINE runtimeDataflowTopologyEdges #-}

runtimeDataflowEdgeEndpoints ::
  (Ord ctx, Ord prop) =>
  CarrierAddr ctx Carrier prop ->
  CarrierEdge ctx Carrier prop ->
  [(RuntimeDataflowEdgeKind, CarrierAddr ctx Carrier prop, CarrierAddr ctx Carrier prop, Text)]
runtimeDataflowEdgeEndpoints anchor edge =
  case edge of
    EdgeRestriction key ->
      [(DataflowEdgeRestriction, rkSource key, rkTarget key, "restriction")]
    EdgeSubsumption _reuseId source target ->
      [(DataflowEdgeSubsumption, source, target, "subsumption")]
    EdgeAmalgamation family ->
      fmap (amalgamationEdge anchor) (Set.toAscList (carrierFamilyTargets family))
{-# INLINE runtimeDataflowEdgeEndpoints #-}

amalgamationEdge ::
  CarrierAddr ctx Carrier prop ->
  CarrierAddr ctx Carrier prop ->
  (RuntimeDataflowEdgeKind, CarrierAddr ctx Carrier prop, CarrierAddr ctx Carrier prop, Text)
amalgamationEdge source target =
  (DataflowEdgeAmalgamation, source, target, "amalgamation")
{-# INLINE amalgamationEdge #-}

runtimeDataflowScheduleEdges :: [RuntimeDataflowOpView] -> [RuntimeDataflowEdge]
runtimeDataflowScheduleEdges =
  foldMap scheduleEdgesForOp
{-# INLINE runtimeDataflowScheduleEdges #-}

scheduleEdgesForOp :: RuntimeDataflowOpView -> [RuntimeDataflowEdge]
scheduleEdgesForOp op =
  fmap readEdge (rdoReads op) <> fmap writeEdge (rdoWrites op)
  where
    readEdge source =
      RuntimeDataflowEdge
        { rdeId = DataflowEdgeId mempty,
          rdeKind = DataflowEdgeScheduleRead,
          rdeSource = source,
          rdeTarget = rdoNodeId op,
          rdeLabel = "reads"
        }

    writeEdge target =
      RuntimeDataflowEdge
        { rdeId = DataflowEdgeId mempty,
          rdeKind = DataflowEdgeScheduleWrite,
          rdeSource = rdoNodeId op,
          rdeTarget = target,
          rdeLabel = "writes"
        }
{-# INLINE scheduleEdgesForOp #-}

runtimeDataflowOpViewsForRuntimeDataflowOps ::
  RuntimeDataflowRenderers ctx prop ->
  [RuntimeDataflowOp ctx prop boundary evidence] ->
  [RuntimeDataflowOpView]
runtimeDataflowOpViewsForRuntimeDataflowOps renderers =
  fmap rdovView
    . assignRuntimeDataflowOpViewDraftIds
    . fmap (runtimeDataflowOpViewDraftFromRuntimeDataflowOp renderers)
{-# INLINE runtimeDataflowOpViewsForRuntimeDataflowOps #-}

runtimeDataflowOpViewDraftFromRuntimeDataflowOp ::
  RuntimeDataflowRenderers ctx prop ->
  RuntimeDataflowOp ctx prop boundary evidence ->
  RuntimeDataflowOpViewDraft
runtimeDataflowOpViewDraftFromRuntimeDataflowOp renderers op =
  RuntimeDataflowOpViewDraft
    { rdovBaseId = runtimeDataflowOpBaseId renderers op,
      rdovView =
        RuntimeDataflowOpView
          { rdoId = DataflowOpViewId mempty,
            rdoNodeId = DataflowNodeId mempty,
            rdoKind = opKind,
            rdoPhase = runtimeDataflowPhaseView phaseValue,
            rdoContext = rdrContextLabel renderers (runtimeDataflowOpContext op),
            rdoReads = readNodeIds,
            rdoWrites = writeNodeIds,
            rdoLabel = runtimeDataflowOpViewKindLabel opKind
          }
    }
  where
    contract =
      runtimeDataflowOpContract op

    phaseValue =
      runtimeDataflowContractPhase contract

    opKind =
      runtimeDataflowOpViewKind (runtimeDataflowOpKind op)

    readNodeIds =
      fmap
        (dataflowNodeIdForCarrierAddr renderers)
        (Set.toAscList (runtimeDataflowContractReads contract))

    writeNodeIds =
      fmap
        (dataflowNodeIdForCarrierAddr renderers)
        (Set.toAscList (runtimeDataflowContractWrites contract))
{-# INLINE runtimeDataflowOpViewDraftFromRuntimeDataflowOp #-}

runtimeDataflowOpBaseId ::
  RuntimeDataflowRenderers ctx prop ->
  RuntimeDataflowOp ctx prop boundary evidence ->
  Text
runtimeDataflowOpBaseId renderers op =
  joinKeyParts
    [ "op",
      runtimeDataflowOpViewKindTag opKind,
      rdrContextId renderers (runtimeDataflowOpContext op),
      runtimeDataflowPhaseTag (runtimeDataflowContractPhase contract),
      joinNodeIds readNodeIds,
      joinNodeIds writeNodeIds
    ]
  where
    contract =
      runtimeDataflowOpContract op

    opKind =
      runtimeDataflowOpViewKind (runtimeDataflowOpKind op)

    readNodeIds =
      fmap
        (dataflowNodeIdForCarrierAddr renderers)
        (Set.toAscList (runtimeDataflowContractReads contract))

    writeNodeIds =
      fmap
        (dataflowNodeIdForCarrierAddr renderers)
        (Set.toAscList (runtimeDataflowContractWrites contract))
{-# INLINE runtimeDataflowOpBaseId #-}

runtimeDataflowOpsForQueue ::
  RuntimeDataflowQueue ctx prop boundary evidence ->
  [RuntimeDataflowOp ctx prop boundary evidence]
runtimeDataflowOpsForQueue =
  runtimeDataflowQueuedOps
{-# INLINE runtimeDataflowOpsForQueue #-}

assignRuntimeDataflowEdgeIds :: [RuntimeDataflowEdge] -> [RuntimeDataflowEdge]
assignRuntimeDataflowEdgeIds =
  assignNumberedIds
    runtimeDataflowEdgeBaseId
    (\base ordinal -> DataflowEdgeId (numberedId base ordinal))
    (\edgeId edge -> edge {rdeId = edgeId})
{-# INLINE assignRuntimeDataflowEdgeIds #-}

runtimeDataflowEdgeBaseId :: RuntimeDataflowEdge -> Text
runtimeDataflowEdgeBaseId edge =
  joinKeyParts
    [ "edge",
      runtimeDataflowEdgeKindTag (rdeKind edge),
      unDataflowNodeId (rdeSource edge),
      unDataflowNodeId (rdeTarget edge)
    ]
{-# INLINE runtimeDataflowEdgeBaseId #-}

assignRuntimeDataflowOpViewDraftIds ::
  [RuntimeDataflowOpViewDraft] ->
  [RuntimeDataflowOpViewDraft]
assignRuntimeDataflowOpViewDraftIds =
  assignNumberedIds
    rdovBaseId
    (\base ordinal -> DataflowOpViewId (numberedId base ordinal))
    setRuntimeDataflowOpViewDraftId
{-# INLINE assignRuntimeDataflowOpViewDraftIds #-}

setRuntimeDataflowOpViewDraftId ::
  DataflowOpViewId ->
  RuntimeDataflowOpViewDraft ->
  RuntimeDataflowOpViewDraft
setRuntimeDataflowOpViewDraftId opId draft =
  draft
    { rdovView =
        (rdovView draft)
          { rdoId = opId,
            rdoNodeId =
              DataflowNodeId
                (joinKeyParts ["op", unDataflowOpViewId opId])
          }
    }
{-# INLINE setRuntimeDataflowOpViewDraftId #-}

assignNumberedIds ::
  Ord base =>
  (a -> base) ->
  (base -> Int -> ident) ->
  (ident -> a -> a) ->
  [a] ->
  [a]
assignNumberedIds baseOf identOf setIdent =
  snd . mapAccumL step Map.empty
  where
    step !counts !value =
      let !base =
            baseOf value
          !ordinal =
            Map.findWithDefault 0 base counts
          !ident =
            identOf base ordinal
       in ( Map.insert base (ordinal + 1) counts,
            setIdent ident value
          )
{-# INLINE assignNumberedIds #-}

numberedId :: Text -> Int -> Text
numberedId baseId ordinal
  | ordinal == 0 =
      baseId
  | otherwise =
      Text.concat [baseId, "#", showText ordinal]
{-# INLINE numberedId #-}

runtimeDataflowNodeForOp :: RuntimeDataflowOpView -> RuntimeDataflowNode
runtimeDataflowNodeForOp op =
  RuntimeDataflowNode
    { rdnId = rdoNodeId op,
      rdnKind = DataflowNodeOperation,
      rdnLabel = rdoLabel op,
      rdnContext = Just (rdoContext op),
      rdnProp = Nothing,
      rdnCarrier = Nothing
    }
{-# INLINE runtimeDataflowNodeForOp #-}

touchNodeForTouchKey :: TouchKey -> RuntimeDataflowNode
touchNodeForTouchKey touchKey =
  RuntimeDataflowNode
    { rdnId = touchNodeIdForTouchKey touchKey,
      rdnKind = DataflowNodeTouch,
      rdnLabel = showText touchKey,
      rdnContext = Nothing,
      rdnProp = Nothing,
      rdnCarrier = Nothing
    }
{-# INLINE touchNodeForTouchKey #-}

touchNodeIdForTouchKey :: TouchKey -> DataflowNodeId
touchNodeIdForTouchKey touchKey =
  DataflowNodeId (joinKeyParts ["touch", showText touchKey])
{-# INLINE touchNodeIdForTouchKey #-}

dataflowNodeIdForCarrierAddr ::
  RuntimeDataflowRenderers ctx prop ->
  CarrierAddr ctx Carrier prop ->
  DataflowNodeId
dataflowNodeIdForCarrierAddr renderers addr =
  DataflowNodeId
    ( joinKeyParts
        [ "carrier",
          rdrContextId renderers (caContext addr),
          rdrPropId renderers (caProp addr),
          carrierIdText (caCarrier addr)
        ]
    )
{-# INLINE dataflowNodeIdForCarrierAddr #-}

dataflowNodeForCarrierAddr ::
  RuntimeDataflowRenderers ctx prop ->
  CarrierAddr ctx Carrier prop ->
  RuntimeDataflowNode
dataflowNodeForCarrierAddr renderers addr =
  RuntimeDataflowNode
    { rdnId = dataflowNodeIdForCarrierAddr renderers addr,
      rdnKind = runtimeDataflowNodeKindForCarrier (caCarrier addr),
      rdnLabel = runtimeDataflowCarrierLabel (caCarrier addr),
      rdnContext = Just (rdrContextLabel renderers (caContext addr)),
      rdnProp = Just (rdrPropLabel renderers (caProp addr)),
      rdnCarrier = Just (carrierIdText (caCarrier addr))
    }
{-# INLINE dataflowNodeForCarrierAddr #-}

carrierIdText :: Carrier -> Text
carrierIdText carrierValue =
  case carrierValue of
    QueryCarrier queryId node ->
      joinKeyParts ["query", showText queryId, queryCarrierNodeIdText node]
    DerivedCarrier derivedId ->
      joinKeyParts ["derived", showText derivedId]
{-# INLINE carrierIdText #-}

queryCarrierNodeIdText :: QueryCarrierNode -> Text
queryCarrierNodeIdText node =
  case node of
    QueryAtom atomId ->
      joinKeyParts ["atom", showText atomId]
    QueryFactor factorNode ->
      joinKeyParts ["factor", factorNodeIdText factorNode]
{-# INLINE queryCarrierNodeIdText #-}

factorNodeIdText :: FactorNode -> Text
factorNodeIdText factorNode =
  case factorNode of
    FactorNodeRoot ->
      "root"
    _ ->
      showText factorNode
{-# INLINE factorNodeIdText #-}

runtimeDataflowFrontier :: RuntimeFrontier ctx epoch phase -> RuntimeDataflowFrontier
runtimeDataflowFrontier frontier =
  RuntimeDataflowFrontier
    { rdfVisibleMinimumCount = Map.size (frontierVisibleMinimums frontier),
      rdfPendingPointstampCount = Map.size positivePending,
      rdfPendingOpCount = sum positivePending,
      rdfRetentionPinnedCount =
        maybe 0 (IntSet.size . traceRetentionPinnedTraceIds) (frontierTraceRetention frontier),
      rdfRetentionExactEvidenceCount =
        maybe 0 (IntSet.size . traceRetentionExactEvidenceTraceIds) (frontierTraceRetention frontier),
      rdfRetentionProvenanceCount =
        maybe 0 (IntSet.size . traceRetentionProvenanceTraceIds) (frontierTraceRetention frontier)
    }
  where
    positivePending =
      Map.filter (> 0) (frontierPendingCounts frontier)
{-# INLINE runtimeDataflowFrontier #-}

joinNodeIds :: [DataflowNodeId] -> Text
joinNodeIds =
  joinKeyParts . fmap unDataflowNodeId
{-# INLINE joinNodeIds #-}

joinKeyParts :: [Text] -> Text
joinKeyParts =
  Text.intercalate "|" . fmap keyPart
{-# INLINE joinKeyParts #-}

keyPart :: Text -> Text
keyPart part =
  Text.concat [showText (Text.length part), ":", part]
{-# INLINE keyPart #-}

showText :: Show a => a -> Text
showText =
  Text.pack . show
{-# INLINE showText #-}
