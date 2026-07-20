{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Flow.Runtime.Engine.Dataflow.Types
  ( RuntimeDataflowRenderers (..),
    defaultRuntimeDataflowRenderers,
    DataflowNodeId (..),
    DataflowEdgeId (..),
    DataflowOpViewId (..),
    RuntimeDataflowNodeKind (..),
    RuntimeDataflowEdgeKind (..),
    RuntimeDataflowOpViewKind (..),
    RuntimeRepairNodeAction (..),
    RuntimeDataflowPhaseView (..),
    RuntimeDataflowNode (..),
    RuntimeDataflowEdge (..),
    RuntimeDataflowOpView (..),
    RuntimeDataflowFrontier (..),
    RuntimeDataflowSignedSummary (..),
    RuntimeDataflowVersionTrace (..),
    RuntimeDataflowRepairNode (..),
    RuntimeDataflowRepairStats (..),
    RuntimeDataflowWorkload (..),
    RuntimeDataflowDiagnostics (..),
    RuntimeDataflowSnapshot (..),
  )
where

import Data.Kind
  ( Type,
  )
import Data.Text
  ( Text,
  )
import Data.Text qualified as Text
import Moonlight.Differential.Carrier.Address
  ( CarrierProp,
  )
import Moonlight.Flow.Runtime.Core.RepairStats
  ( RuntimeRepairNodeAction (..),
  )

type RuntimeDataflowRenderers :: Type -> Type -> Type
data RuntimeDataflowRenderers ctx prop = RuntimeDataflowRenderers
  { rdrContextId :: ctx -> Text,
    rdrContextLabel :: ctx -> Text,
    rdrPropId :: CarrierProp prop -> Text,
    rdrPropLabel :: CarrierProp prop -> Text
  }

defaultRuntimeDataflowRenderers ::
  (Show ctx, Show prop) =>
  RuntimeDataflowRenderers ctx prop
defaultRuntimeDataflowRenderers =
  RuntimeDataflowRenderers
    { rdrContextId = showText,
      rdrContextLabel = showText,
      rdrPropId = showText,
      rdrPropLabel = showText
    }
{-# INLINE defaultRuntimeDataflowRenderers #-}

showText :: Show a => a -> Text
showText =
  Text.pack . show
{-# INLINE showText #-}

type DataflowNodeId :: Type
newtype DataflowNodeId = DataflowNodeId {unDataflowNodeId :: Text}
  deriving stock (Eq, Ord, Show, Read)

type DataflowEdgeId :: Type
newtype DataflowEdgeId = DataflowEdgeId {unDataflowEdgeId :: Text}
  deriving stock (Eq, Ord, Show, Read)

type DataflowOpViewId :: Type
newtype DataflowOpViewId = DataflowOpViewId {unDataflowOpViewId :: Text}
  deriving stock (Eq, Ord, Show, Read)

type RuntimeDataflowNodeKind :: Type
data RuntimeDataflowNodeKind
  = DataflowNodeTouch
  | DataflowNodeAtom
  | DataflowNodeQueryRoot
  | DataflowNodeQueryFactor
  | DataflowNodeDerived
  | DataflowNodeOperation
  deriving stock (Eq, Ord, Show, Read)

type RuntimeDataflowEdgeKind :: Type
data RuntimeDataflowEdgeKind
  = DataflowEdgeTouch
  | DataflowEdgeRestriction
  | DataflowEdgeSubsumption
  | DataflowEdgeAmalgamation
  | DataflowEdgeScheduleRead
  | DataflowEdgeScheduleWrite
  deriving stock (Eq, Ord, Show, Read)

type RuntimeDataflowOpViewKind :: Type
data RuntimeDataflowOpViewKind
  = DataflowOpViewApplyAtomEvents
  | DataflowOpViewRunProject
  | DataflowOpViewRunRestrict
  | DataflowOpViewRunIndex
  | DataflowOpViewRepairFactorBatch
  | DataflowOpViewDeriveSubsumedCarrier
  | DataflowOpViewRestrictCarrier
  | DataflowOpViewAmalgamateCarrierFamily
  deriving stock (Eq, Ord, Show, Read)

type RuntimeDataflowPhaseView :: Type
data RuntimeDataflowPhaseView = RuntimeDataflowPhaseView
  { rdpvTag :: !Text,
    rdpvLabel :: !Text
  }
  deriving stock (Eq, Ord, Show, Read)

type RuntimeDataflowNode :: Type
data RuntimeDataflowNode = RuntimeDataflowNode
  { rdnId :: !DataflowNodeId,
    rdnKind :: !RuntimeDataflowNodeKind,
    rdnLabel :: !Text,
    rdnContext :: !(Maybe Text),
    rdnProp :: !(Maybe Text),
    rdnCarrier :: !(Maybe Text)
  }
  deriving stock (Eq, Ord, Show, Read)

type RuntimeDataflowEdge :: Type
data RuntimeDataflowEdge = RuntimeDataflowEdge
  { rdeId :: !DataflowEdgeId,
    rdeKind :: !RuntimeDataflowEdgeKind,
    rdeSource :: !DataflowNodeId,
    rdeTarget :: !DataflowNodeId,
    rdeLabel :: !Text
  }
  deriving stock (Eq, Ord, Show, Read)

type RuntimeDataflowOpView :: Type
data RuntimeDataflowOpView = RuntimeDataflowOpView
  { rdoId :: !DataflowOpViewId,
    rdoNodeId :: !DataflowNodeId,
    rdoKind :: !RuntimeDataflowOpViewKind,
    rdoPhase :: !RuntimeDataflowPhaseView,
    rdoContext :: !Text,
    rdoReads :: ![DataflowNodeId],
    rdoWrites :: ![DataflowNodeId],
    rdoLabel :: !Text
  }
  deriving stock (Eq, Ord, Show, Read)

type RuntimeDataflowFrontier :: Type
data RuntimeDataflowFrontier = RuntimeDataflowFrontier
  { rdfVisibleMinimumCount :: {-# UNPACK #-} !Int,
    rdfPendingPointstampCount :: {-# UNPACK #-} !Int,
    rdfPendingOpCount :: {-# UNPACK #-} !Int,
    rdfRetentionPinnedCount :: {-# UNPACK #-} !Int,
    rdfRetentionExactEvidenceCount :: {-# UNPACK #-} !Int,
    rdfRetentionProvenanceCount :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show, Read)

type RuntimeDataflowSignedSummary :: Type
data RuntimeDataflowSignedSummary = RuntimeDataflowSignedSummary
  { rdsdsAtomPatchCount :: {-# UNPACK #-} !Int,
    rdsdsTouchedRowCount :: {-# UNPACK #-} !Int,
    rdsdsInsertedRowMultiplicity :: {-# UNPACK #-} !Int,
    rdsdsRemovedRowMultiplicity :: {-# UNPACK #-} !Int,
    rdsdsNetRowMultiplicity :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show, Read)

type RuntimeDataflowVersionTrace :: Type
data RuntimeDataflowVersionTrace = RuntimeDataflowVersionTrace
  { rdvtQuotientBefore :: !Text,
    rdvtQuotientAfter :: !Text,
    rdvtLiveBefore :: !Text,
    rdvtLiveScheduled :: !Text,
    rdvtOrder :: !Text
  }
  deriving stock (Eq, Ord, Show, Read)

type RuntimeDataflowRepairNode :: Type
data RuntimeDataflowRepairNode = RuntimeDataflowRepairNode
  { rdrnQueryId :: !Text,
    rdrnFactorNode :: !Text,
    rdrnAction :: !RuntimeRepairNodeAction,
    rdrnAffectedKeys :: {-# UNPACK #-} !Int,
    rdrnRecomputedCells :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show, Read)

type RuntimeDataflowRepairStats :: Type
data RuntimeDataflowRepairStats = RuntimeDataflowRepairStats
  { rdrsFactorRepairs :: {-# UNPACK #-} !Int,
    rdrsCanonicalRepairs :: {-# UNPACK #-} !Int,
    rdrsRepairSubscribers :: {-# UNPACK #-} !Int,
    rdrsNodesBuilt :: {-# UNPACK #-} !Int,
    rdrsNodesReused :: {-# UNPACK #-} !Int,
    rdrsNodesPatched :: {-# UNPACK #-} !Int,
    rdrsAffectedKeys :: {-# UNPACK #-} !Int,
    rdrsSemanticAffectedKeys :: {-# UNPACK #-} !Int,
    rdrsRecomputedCells :: {-# UNPACK #-} !Int,
    rdrsEmittedCarrierDeltas :: {-# UNPACK #-} !Int,
    rdrsEmittedCarrierRows :: {-# UNPACK #-} !Int,
    rdrsProjectionRowsEmitted :: {-# UNPACK #-} !Int,
    rdrsMaterializedSnapshots :: {-# UNPACK #-} !Int,
    rdrsInputDeltaRows :: {-# UNPACK #-} !Int,
    rdrsPreparedInputRebuilds :: {-# UNPACK #-} !Int,
    rdrsPreparedInputPatchHits :: {-# UNPACK #-} !Int,
    rdrsPreparedRelationRows :: {-# UNPACK #-} !Int,
    rdrsStoreRebuilds :: {-# UNPACK #-} !Int,
    rdrsSupportEvaluations :: {-# UNPACK #-} !Int,
    rdrsSupportMemoHits :: {-# UNPACK #-} !Int,
    rdrsNodeRepairs :: ![RuntimeDataflowRepairNode]
  }
  deriving stock (Eq, Ord, Show, Read)

type RuntimeDataflowWorkload :: Type
data RuntimeDataflowWorkload = RuntimeDataflowWorkload
  { rdwQueuedOperationCount :: {-# UNPACK #-} !Int,
    rdwTouchedCarrierCount :: {-# UNPACK #-} !Int,
    rdwScheduledReadCarrierCount :: {-# UNPACK #-} !Int,
    rdwScheduledWriteCarrierCount :: {-# UNPACK #-} !Int,
    rdwDeltaSummary :: !RuntimeDataflowSignedSummary,
    rdwVersionTrace :: !RuntimeDataflowVersionTrace,
    rdwRepairStats :: !RuntimeDataflowRepairStats
  }
  deriving stock (Eq, Ord, Show, Read)

type RuntimeDataflowDiagnostics :: Type
data RuntimeDataflowDiagnostics = RuntimeDataflowDiagnostics
  { rddNodeCount :: {-# UNPACK #-} !Int,
    rddEdgeCount :: {-# UNPACK #-} !Int,
    rddOpCount :: {-# UNPACK #-} !Int,
    rddOrphanEdgeCount :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show, Read)

type RuntimeDataflowSnapshot :: Type
data RuntimeDataflowSnapshot = RuntimeDataflowSnapshot
  { rdsVersion :: {-# UNPACK #-} !Int,
    rdsNodes :: ![RuntimeDataflowNode],
    rdsEdges :: ![RuntimeDataflowEdge],
    rdsOps :: ![RuntimeDataflowOpView],
    rdsFrontier :: !RuntimeDataflowFrontier,
    rdsWorkload :: !(Maybe RuntimeDataflowWorkload),
    rdsDiagnostics :: !RuntimeDataflowDiagnostics
  }
  deriving stock (Eq, Ord, Show, Read)
