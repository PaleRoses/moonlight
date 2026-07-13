module Moonlight.Graph.Pure.Delta
  ( GraphDelta (..),
    flattenGraphDelta,
  )
where

import Data.Kind (Type)
import Moonlight.Graph.Pure.Types
  ( AttrDelta,
    AttrKey,
    Attributes,
    EdgeRef,
    NodeRef,
  )
import Prelude (Eq, Monoid (..), Semigroup (..), Show, foldMap)

type GraphDelta :: Type -> Type -> Type
data GraphDelta nodeKind edgeKind
  = InsertNode NodeRef nodeKind Attributes
  | RemoveNode NodeRef
  | InsertEdge EdgeRef edgeKind [NodeRef] Attributes
  | RemoveEdge EdgeRef
  | MutateNodeAttr NodeRef AttrKey AttrDelta
  | MutateEdgeAttr EdgeRef AttrKey AttrDelta
  | Batch [GraphDelta nodeKind edgeKind]

deriving stock instance (Eq nodeKind, Eq edgeKind) => Eq (GraphDelta nodeKind edgeKind)
deriving stock instance (Show nodeKind, Show edgeKind) => Show (GraphDelta nodeKind edgeKind)

flattenGraphDelta :: GraphDelta nodeKind edgeKind -> [GraphDelta nodeKind edgeKind]
flattenGraphDelta graphDelta =
  case graphDelta of
    Batch nestedDeltas -> foldMap flattenGraphDelta nestedDeltas
    _ -> [graphDelta]

instance Semigroup (GraphDelta nodeKind edgeKind) where
  (<>) leftDelta rightDelta =
    case (leftDelta, rightDelta) of
      (Batch leftDeltas, Batch rightDeltas) -> Batch (leftDeltas <> rightDeltas)
      (Batch leftDeltas, _) -> Batch (leftDeltas <> [rightDelta])
      (_, Batch rightDeltas) -> Batch (leftDelta : rightDeltas)
      _ -> Batch [leftDelta, rightDelta]


instance Monoid (GraphDelta nodeKind edgeKind) where
  mempty = Batch []
