module Moonlight.Graph.Pure.View
  ( GraphView (..),
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Moonlight.Graph.Pure.Types (Attributes, EdgeRef, NodeRef)

type GraphView :: Type -> Type -> Type -> Type
data GraphView graph nodeKind edgeKind = GraphView
  { viewNodes :: graph -> Map NodeRef (nodeKind, Attributes),
    viewEdges :: graph -> Map EdgeRef (edgeKind, [NodeRef], Attributes)
  }
