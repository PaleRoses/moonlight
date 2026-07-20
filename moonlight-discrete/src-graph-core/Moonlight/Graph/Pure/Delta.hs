module Moonlight.Graph.Pure.Delta
  ( GraphEdit (..),
    GraphDelta,
    singletonGraphDelta,
    graphDeltaFromList,
    graphDeltaToList,
  )
where

import Data.Kind (Type)
import Moonlight.Algebra (Batch (..), singletonBatch)
import Moonlight.Graph.Pure.Types
  ( AttrDelta,
    AttrKey,
    Attributes,
    EdgeRef,
    NodeRef,
  )
import Prelude (Eq, Monoid, Semigroup, Show, (.))

type GraphEdit :: Type -> Type -> Type
data GraphEdit nodeKind edgeKind
  = InsertNode NodeRef nodeKind Attributes
  | RemoveNode NodeRef
  | InsertEdge EdgeRef edgeKind [NodeRef] Attributes
  | RemoveEdge EdgeRef
  | MutateNodeAttr NodeRef AttrKey AttrDelta
  | MutateEdgeAttr EdgeRef AttrKey AttrDelta
  deriving stock (Eq, Show)

type GraphDelta :: Type -> Type -> Type
newtype GraphDelta nodeKind edgeKind = GraphDelta (Batch (GraphEdit nodeKind edgeKind))
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

singletonGraphDelta :: GraphEdit nodeKind edgeKind -> GraphDelta nodeKind edgeKind
singletonGraphDelta = GraphDelta . singletonBatch

graphDeltaFromList :: [GraphEdit nodeKind edgeKind] -> GraphDelta nodeKind edgeKind
graphDeltaFromList = GraphDelta . Batch

graphDeltaToList :: GraphDelta nodeKind edgeKind -> [GraphEdit nodeKind edgeKind]
graphDeltaToList (GraphDelta (Batch graphEdits)) = graphEdits
