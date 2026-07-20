{-# LANGUAGE OverloadedStrings #-}

module Moonlight.Flow.Runtime.Engine.Dataflow.CBOR
  ( runtimeDataflowSnapshotEncoding,
    encodeRuntimeDataflowCBOR,
    writeRuntimeDataflowCBOR,
  )
where

import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Data.ByteString.Lazy qualified as BSL
import Data.Text
  ( Text,
  )
import Moonlight.Flow.Runtime.Engine.Dataflow.Tags
  ( runtimeDataflowEdgeKindTag,
    runtimeDataflowNodeKindTag,
    runtimeDataflowOpViewKindTag,
    runtimeDataflowRepairNodeActionTag,
  )
import Moonlight.Flow.Runtime.Engine.Dataflow.Types

data Field a = Field !Text !(a -> CBOR.Encoding)

field :: Text -> (a -> b) -> (b -> CBOR.Encoding) -> Field a
field name project encode =
  Field name (encode . project)
{-# INLINE field #-}

record :: [Field a] -> a -> CBOR.Encoding
record fields value =
  CBOR.encodeMapLen (fromIntegral (length fields))
    <> foldMap encodeField fields
  where
    encodeField (Field name encodeValue) =
      CBOR.encodeString name <> encodeValue value
{-# INLINE record #-}

list :: (a -> CBOR.Encoding) -> [a] -> CBOR.Encoding
list encodeValue values =
  CBOR.encodeListLen (fromIntegral (length values))
    <> foldMap encodeValue values
{-# INLINE list #-}

int :: Int -> CBOR.Encoding
int =
  CBOR.encodeInt
{-# INLINE int #-}

text :: Text -> CBOR.Encoding
text =
  CBOR.encodeString
{-# INLINE text #-}

maybeText :: Maybe Text -> CBOR.Encoding
maybeText =
  maybe CBOR.encodeNull text
{-# INLINE maybeText #-}

maybeValue :: (a -> CBOR.Encoding) -> Maybe a -> CBOR.Encoding
maybeValue encodeValue =
  maybe CBOR.encodeNull encodeValue
{-# INLINE maybeValue #-}

runtimeDataflowSnapshotEncoding :: RuntimeDataflowSnapshot -> CBOR.Encoding
runtimeDataflowSnapshotEncoding =
  record
    [ field "version" rdsVersion int,
      field "nodes" rdsNodes (list encodeNode),
      field "edges" rdsEdges (list encodeEdge),
      field "ops" rdsOps (list encodeOp),
      field "frontier" rdsFrontier encodeFrontier,
      field "workload" rdsWorkload (maybeValue encodeWorkload),
      field "diagnostics" rdsDiagnostics encodeDiagnostics
    ]
{-# INLINE runtimeDataflowSnapshotEncoding #-}

encodeRuntimeDataflowCBOR :: RuntimeDataflowSnapshot -> BSL.ByteString
encodeRuntimeDataflowCBOR =
  CBOR.toLazyByteString . runtimeDataflowSnapshotEncoding
{-# INLINE encodeRuntimeDataflowCBOR #-}

writeRuntimeDataflowCBOR :: FilePath -> RuntimeDataflowSnapshot -> IO ()
writeRuntimeDataflowCBOR path =
  BSL.writeFile path . encodeRuntimeDataflowCBOR
{-# INLINE writeRuntimeDataflowCBOR #-}

encodeNode :: RuntimeDataflowNode -> CBOR.Encoding
encodeNode =
  record
    [ field "id" (unDataflowNodeId . rdnId) text,
      field "kind" (runtimeDataflowNodeKindTag . rdnKind) text,
      field "label" rdnLabel text,
      field "context" rdnContext maybeText,
      field "prop" rdnProp maybeText,
      field "carrier" rdnCarrier maybeText
    ]
{-# INLINE encodeNode #-}

encodeEdge :: RuntimeDataflowEdge -> CBOR.Encoding
encodeEdge =
  record
    [ field "id" (unDataflowEdgeId . rdeId) text,
      field "kind" (runtimeDataflowEdgeKindTag . rdeKind) text,
      field "source" (unDataflowNodeId . rdeSource) text,
      field "target" (unDataflowNodeId . rdeTarget) text,
      field "label" rdeLabel text
    ]
{-# INLINE encodeEdge #-}

encodeOp :: RuntimeDataflowOpView -> CBOR.Encoding
encodeOp =
  record
    [ field "id" (unDataflowOpViewId . rdoId) text,
      field "nodeId" (unDataflowNodeId . rdoNodeId) text,
      field "kind" (runtimeDataflowOpViewKindTag . rdoKind) text,
      field "phase" rdoPhase encodePhase,
      field "context" rdoContext text,
      field "reads" rdoReads (list (text . unDataflowNodeId)),
      field "writes" rdoWrites (list (text . unDataflowNodeId)),
      field "label" rdoLabel text
    ]
{-# INLINE encodeOp #-}

encodePhase :: RuntimeDataflowPhaseView -> CBOR.Encoding
encodePhase =
  record
    [ field "tag" rdpvTag text,
      field "label" rdpvLabel text
    ]
{-# INLINE encodePhase #-}

encodeFrontier :: RuntimeDataflowFrontier -> CBOR.Encoding
encodeFrontier =
  record
    [ field "visibleMinimumCount" rdfVisibleMinimumCount int,
      field "pendingPointstampCount" rdfPendingPointstampCount int,
      field "pendingOperationCount" rdfPendingOpCount int,
      field "retentionPinnedCount" rdfRetentionPinnedCount int,
      field "retentionExactEvidenceCount" rdfRetentionExactEvidenceCount int,
      field "retentionProvenanceCount" rdfRetentionProvenanceCount int
    ]
{-# INLINE encodeFrontier #-}

encodeWorkload :: RuntimeDataflowWorkload -> CBOR.Encoding
encodeWorkload =
  record
    [ field "queuedOperationCount" rdwQueuedOperationCount int,
      field "touchedCarrierCount" rdwTouchedCarrierCount int,
      field "scheduledReadCarrierCount" rdwScheduledReadCarrierCount int,
      field "scheduledWriteCarrierCount" rdwScheduledWriteCarrierCount int,
      field "deltaSummary" rdwDeltaSummary encodeSignedSummary,
      field "versionTrace" rdwVersionTrace encodeVersionTrace,
      field "repairStats" rdwRepairStats encodeRepairStats
    ]
{-# INLINE encodeWorkload #-}

encodeSignedSummary :: RuntimeDataflowSignedSummary -> CBOR.Encoding
encodeSignedSummary =
  record
    [ field "atomPatchCount" rdsdsAtomPatchCount int,
      field "touchedRowCount" rdsdsTouchedRowCount int,
      field "insertedRowMultiplicity" rdsdsInsertedRowMultiplicity int,
      field "removedRowMultiplicity" rdsdsRemovedRowMultiplicity int,
      field "netRowMultiplicity" rdsdsNetRowMultiplicity int
    ]
{-# INLINE encodeSignedSummary #-}

encodeVersionTrace :: RuntimeDataflowVersionTrace -> CBOR.Encoding
encodeVersionTrace =
  record
    [ field "quotientBefore" rdvtQuotientBefore text,
      field "quotientAfter" rdvtQuotientAfter text,
      field "liveBefore" rdvtLiveBefore text,
      field "liveScheduled" rdvtLiveScheduled text,
      field "order" rdvtOrder text
    ]
{-# INLINE encodeVersionTrace #-}

encodeRepairStats :: RuntimeDataflowRepairStats -> CBOR.Encoding
encodeRepairStats =
  record
    [ field "factorRepairs" rdrsFactorRepairs int,
      field "canonicalRepairs" rdrsCanonicalRepairs int,
      field "repairSubscribers" rdrsRepairSubscribers int,
      field "nodesBuilt" rdrsNodesBuilt int,
      field "nodesReused" rdrsNodesReused int,
      field "nodesPatched" rdrsNodesPatched int,
      field "affectedKeys" rdrsAffectedKeys int,
      field "semanticAffectedKeys" rdrsSemanticAffectedKeys int,
      field "recomputedCells" rdrsRecomputedCells int,
      field "emittedCarrierDeltas" rdrsEmittedCarrierDeltas int,
      field "emittedCarrierRows" rdrsEmittedCarrierRows int,
      field "projectionRowsEmitted" rdrsProjectionRowsEmitted int,
      field "materializedSnapshots" rdrsMaterializedSnapshots int,
      field "inputDeltaRows" rdrsInputDeltaRows int,
      field "preparedInputRebuilds" rdrsPreparedInputRebuilds int,
      field "preparedInputPatchHits" rdrsPreparedInputPatchHits int,
      field "preparedRelationRows" rdrsPreparedRelationRows int,
      field "storeRebuilds" rdrsStoreRebuilds int,
      field "supportEvaluations" rdrsSupportEvaluations int,
      field "supportMemoHits" rdrsSupportMemoHits int,
      field "nodeRepairs" rdrsNodeRepairs (list encodeRepairNode)
    ]
{-# INLINE encodeRepairStats #-}

encodeRepairNode :: RuntimeDataflowRepairNode -> CBOR.Encoding
encodeRepairNode =
  record
    [ field "queryId" rdrnQueryId text,
      field "factorNode" rdrnFactorNode text,
      field "action" (runtimeDataflowRepairNodeActionTag . rdrnAction) text,
      field "affectedKeys" rdrnAffectedKeys int,
      field "recomputedCells" rdrnRecomputedCells int
    ]
{-# INLINE encodeRepairNode #-}

encodeDiagnostics :: RuntimeDataflowDiagnostics -> CBOR.Encoding
encodeDiagnostics =
  record
    [ field "nodeCount" rddNodeCount int,
      field "edgeCount" rddEdgeCount int,
      field "opCount" rddOpCount int,
      field "orphanEdgeCount" rddOrphanEdgeCount int
    ]
{-# INLINE encodeDiagnostics #-}
