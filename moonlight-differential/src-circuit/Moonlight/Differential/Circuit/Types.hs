{-# LANGUAGE StandaloneKindSignatures #-}

-- | Circuit vocabulary: region-typed node handles, input ports, the sealed
-- shape rendering, and the typed build/advance refusals.
module Moonlight.Differential.Circuit.Types
  ( Node,
    IndexedNode,
    InputPort,
    nodeId,
    indexedNodeId,
    inputPortId,
    NodeKind (..),
    NodeShape (..),
    CircuitBuildError (..),
    CircuitAdvanceError (..),
    CircuitOutputError (..),
  )
where

import Data.Kind
  ( Type,
  )
import Moonlight.Differential.Circuit.Handle
  ( IndexedNode (..),
    InputPort (..),
    Node (..),
  )
import Numeric.Natural
  ( Natural,
  )

nodeId :: Node s value -> Int
nodeId (Node nodeKey) =
  nodeKey
{-# INLINE nodeId #-}

indexedNodeId :: IndexedNode s key value -> Int
indexedNodeId (IndexedNode nodeKey) =
  nodeKey
{-# INLINE indexedNodeId #-}

inputPortId :: InputPort s value -> Int
inputPortId (InputPort portKey) =
  portKey
{-# INLINE inputPortId #-}

type NodeKind :: Type
data NodeKind
  = InputNode
  | MapNode
  | FilterNode
  | FlatMapNode
  | ConcatNode
  | NegateNode
  | IndexByNode
  | DeindexNode
  | JoinNode
  | CountByNode
  | AggregateNode
  | DistinctNode
  | FeedbackNode
  | FixpointNode
  | ForeignNode
  deriving stock (Eq, Ord, Show)

type NodeShape :: Type
data NodeShape = NodeShape
  { nodeShapeKind :: !NodeKind,
    nodeShapeParents :: ![Int]
  }
  deriving stock (Eq, Show)

type CircuitBuildError :: Type
data CircuitBuildError
  = CircuitFeedbackEscapesScope
      { escapeFixpointId :: !Int,
        escapeOffendingId :: !Int
      }
  | CircuitFixpointBodyEscapesScope
      { escapeFixpointId :: !Int,
        escapeReferencedId :: !Int,
        escapeOffendingId :: !Int
      }
  | CircuitBuildMissingParent
      { missingParentConsumerId :: !Int,
        missingParentId :: !Int
      }
  | CircuitBuildMissingArrangement
      { missingArrangementConsumerId :: !Int,
        missingArrangementParentId :: !Int
      }
  deriving stock (Eq, Show)

type CircuitAdvanceError :: Type -> Type
data CircuitAdvanceError fault
  = CircuitFixpointDiverged
      { divergedNodeId :: !Int,
        divergedRoundsSpent :: !Natural,
        divergedResidualSize :: !Int,
        divergedAccumulatedSize :: !Int
      }
  | CircuitForeignFault
      { faultedNodeId :: !Int,
        foreignFault :: !fault
      }
  | CircuitEvaluationMissingNode
      { missingEvaluationNodeId :: !Int
      }
  | CircuitEvaluationMissingParent
      { missingEvaluationParentId :: !Int
      }
  deriving stock (Eq, Show)

type CircuitOutputError :: Type
newtype CircuitOutputError = CircuitOutputMissing
  { missingOutputNodeId :: Int
  }
  deriving stock (Eq, Show)
