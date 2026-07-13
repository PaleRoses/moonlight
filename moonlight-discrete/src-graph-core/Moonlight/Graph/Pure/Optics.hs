module Moonlight.Graph.Pure.Optics
  ( GraphOptics (..),
    NodeMembership (..),
    EdgeMembership (..),
    GraphSubgraph (..),
    graphViewFromOptics,
    graphNodeMembershipMapLens,
    graphEdgeMembershipMapLens,
    graphNodeAttributesTraversal,
    graphEdgeAttributesTraversal,
    graphNodeMembershipTraversal,
    graphEdgeMembershipTraversal,
    graphNodeMembershipIxTraversal,
    graphEdgeMembershipIxTraversal,
    selectSubgraph,
    selectorSubgraph,
    subgraphNodeAttributesTraversal,
    subgraphEdgeAttributesTraversal,
    subgraphNodeMembershipTraversal,
    subgraphEdgeMembershipTraversal,
    subgraphNodeMembershipIxTraversal,
    subgraphEdgeMembershipIxTraversal,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Moonlight.Graph.Pure.Selector (GraphSelector, resolveGraphSelector)
import Moonlight.Graph.Pure.Types (Attributes, EdgeRef, EntityRef (..), NodeRef)
import Moonlight.Graph.Pure.View (GraphView (..))
import Moonlight.Optics (Getter, IxTraversal, Lens', Traversal', itraversed, lens, set, to, traversed, view, (%))

type GraphOptics :: Type -> Type -> Type -> Type
data GraphOptics graph nodeKind edgeKind = GraphOptics
  { graphNodesLens :: Lens' graph (Map NodeRef (nodeKind, Attributes)),
    graphEdgesLens :: Lens' graph (Map EdgeRef (edgeKind, [NodeRef], Attributes))
  }

type NodeMembership :: Type -> Type
data NodeMembership nodeKind = NodeMembership
  { nodeMembershipKind :: nodeKind,
    nodeMembershipAttributes :: Attributes
  }
  deriving stock (Eq, Show)

type EdgeMembership :: Type -> Type
data EdgeMembership edgeKind = EdgeMembership
  { edgeMembershipKind :: edgeKind,
    edgeMembershipEndpoints :: [NodeRef],
    edgeMembershipAttributes :: Attributes
  }
  deriving stock (Eq, Show)

type GraphSubgraph :: Type -> Type -> Type
data GraphSubgraph nodeKind edgeKind = GraphSubgraph
  { subgraphNodes :: Map NodeRef (nodeKind, Attributes),
    subgraphEdges :: Map EdgeRef (edgeKind, [NodeRef], Attributes)
  }
  deriving stock (Eq, Show)

graphViewFromOptics :: GraphOptics graph nodeKind edgeKind -> GraphView graph nodeKind edgeKind
graphViewFromOptics optics =
  GraphView
    { viewNodes = view (graphNodesLens optics),
      viewEdges = view (graphEdgesLens optics)
    }

graphNodeMembershipMapLens :: GraphOptics graph nodeKind edgeKind -> Lens' graph (Map NodeRef (NodeMembership nodeKind))
graphNodeMembershipMapLens optics =
  lens
    (Map.map (uncurry NodeMembership) . view (graphNodesLens optics))
    (\graphValue updatedNodes ->
        setGraphNodes optics (Map.map fromNodeMembership updatedNodes) graphValue
    )

graphEdgeMembershipMapLens :: GraphOptics graph nodeKind edgeKind -> Lens' graph (Map EdgeRef (EdgeMembership edgeKind))
graphEdgeMembershipMapLens optics =
  lens
    (Map.map (\(edgeKindValue, nodeRefsValue, attributesValue) -> EdgeMembership edgeKindValue nodeRefsValue attributesValue) . view (graphEdgesLens optics))
    (\graphValue updatedEdges ->
        setGraphEdges optics (Map.map fromEdgeMembership updatedEdges) graphValue
    )

graphNodeAttributesTraversal :: GraphOptics graph nodeKind edgeKind -> Traversal' graph Attributes
graphNodeAttributesTraversal optics =
  graphNodeMembershipMapLens optics % traversed % nodeMembershipAttributesLens

graphEdgeAttributesTraversal :: GraphOptics graph nodeKind edgeKind -> Traversal' graph Attributes
graphEdgeAttributesTraversal optics =
  graphEdgeMembershipMapLens optics % traversed % edgeMembershipAttributesLens

graphNodeMembershipTraversal :: GraphOptics graph nodeKind edgeKind -> Traversal' graph (NodeMembership nodeKind)
graphNodeMembershipTraversal optics =
  graphNodeMembershipMapLens optics % traversed

graphEdgeMembershipTraversal :: GraphOptics graph nodeKind edgeKind -> Traversal' graph (EdgeMembership edgeKind)
graphEdgeMembershipTraversal optics =
  graphEdgeMembershipMapLens optics % traversed

graphNodeMembershipIxTraversal :: GraphOptics graph nodeKind edgeKind -> IxTraversal NodeRef graph graph (NodeMembership nodeKind) (NodeMembership nodeKind)
graphNodeMembershipIxTraversal optics =
  graphNodeMembershipMapLens optics % itraversed

graphEdgeMembershipIxTraversal :: GraphOptics graph nodeKind edgeKind -> IxTraversal EdgeRef graph graph (EdgeMembership edgeKind) (EdgeMembership edgeKind)
graphEdgeMembershipIxTraversal optics =
  graphEdgeMembershipMapLens optics % itraversed

selectSubgraph ::
  (Eq nodeKind, Eq edgeKind) =>
  GraphOptics graph nodeKind edgeKind ->
  GraphSelector nodeKind edgeKind ->
  graph ->
  GraphSubgraph nodeKind edgeKind
selectSubgraph optics selectorValue graphValue =
  let matchedEntities = resolveGraphSelector (graphViewFromOptics optics) selectorValue graphValue
      matchedNodeRefs = foldMap matchedNodeRefSet matchedEntities
      matchedEdgeRefs = foldMap matchedEdgeRefSet matchedEntities
   in GraphSubgraph
        { subgraphNodes = Map.restrictKeys (view (graphNodesLens optics) graphValue) matchedNodeRefs,
          subgraphEdges = Map.restrictKeys (view (graphEdgesLens optics) graphValue) matchedEdgeRefs
        }

selectorSubgraph ::
  (Eq nodeKind, Eq edgeKind) =>
  GraphOptics graph nodeKind edgeKind ->
  GraphSelector nodeKind edgeKind ->
  Getter graph (GraphSubgraph nodeKind edgeKind)
selectorSubgraph optics selectorValue =
  to (selectSubgraph optics selectorValue)

subgraphNodeAttributesTraversal :: Traversal' (GraphSubgraph nodeKind edgeKind) Attributes
subgraphNodeAttributesTraversal = graphNodeAttributesTraversal subgraphOptics

subgraphEdgeAttributesTraversal :: Traversal' (GraphSubgraph nodeKind edgeKind) Attributes
subgraphEdgeAttributesTraversal = graphEdgeAttributesTraversal subgraphOptics

subgraphNodeMembershipTraversal :: Traversal' (GraphSubgraph nodeKind edgeKind) (NodeMembership nodeKind)
subgraphNodeMembershipTraversal = graphNodeMembershipTraversal subgraphOptics

subgraphEdgeMembershipTraversal :: Traversal' (GraphSubgraph nodeKind edgeKind) (EdgeMembership edgeKind)
subgraphEdgeMembershipTraversal = graphEdgeMembershipTraversal subgraphOptics

subgraphNodeMembershipIxTraversal :: IxTraversal NodeRef (GraphSubgraph nodeKind edgeKind) (GraphSubgraph nodeKind edgeKind) (NodeMembership nodeKind) (NodeMembership nodeKind)
subgraphNodeMembershipIxTraversal = graphNodeMembershipIxTraversal subgraphOptics

subgraphEdgeMembershipIxTraversal :: IxTraversal EdgeRef (GraphSubgraph nodeKind edgeKind) (GraphSubgraph nodeKind edgeKind) (EdgeMembership edgeKind) (EdgeMembership edgeKind)
subgraphEdgeMembershipIxTraversal = graphEdgeMembershipIxTraversal subgraphOptics

nodeMembershipAttributesLens :: Lens' (NodeMembership nodeKind) Attributes
nodeMembershipAttributesLens =
  lens
    nodeMembershipAttributes
    (\membershipValue updatedAttributes -> membershipValue {nodeMembershipAttributes = updatedAttributes})

edgeMembershipAttributesLens :: Lens' (EdgeMembership edgeKind) Attributes
edgeMembershipAttributesLens =
  lens
    edgeMembershipAttributes
    (\membershipValue updatedAttributes -> membershipValue {edgeMembershipAttributes = updatedAttributes})

subgraphOptics :: GraphOptics (GraphSubgraph nodeKind edgeKind) nodeKind edgeKind
subgraphOptics =
  GraphOptics
    { graphNodesLens = lens subgraphNodes (\subgraphValue updatedNodes -> subgraphValue {subgraphNodes = updatedNodes}),
      graphEdgesLens = lens subgraphEdges (\subgraphValue updatedEdges -> subgraphValue {subgraphEdges = updatedEdges})
    }

setGraphNodes :: GraphOptics graph nodeKind edgeKind -> Map NodeRef (nodeKind, Attributes) -> graph -> graph
setGraphNodes optics updatedNodes graphValue =
  set (graphNodesLens optics) updatedNodes graphValue

setGraphEdges :: GraphOptics graph nodeKind edgeKind -> Map EdgeRef (edgeKind, [NodeRef], Attributes) -> graph -> graph
setGraphEdges optics updatedEdges graphValue =
  set (graphEdgesLens optics) updatedEdges graphValue

fromNodeMembership :: NodeMembership nodeKind -> (nodeKind, Attributes)
fromNodeMembership membershipValue =
  (nodeMembershipKind membershipValue, nodeMembershipAttributes membershipValue)

fromEdgeMembership :: EdgeMembership edgeKind -> (edgeKind, [NodeRef], Attributes)
fromEdgeMembership membershipValue =
  ( edgeMembershipKind membershipValue,
    edgeMembershipEndpoints membershipValue,
    edgeMembershipAttributes membershipValue
  )

matchedNodeRefSet :: EntityRef -> Set NodeRef
matchedNodeRefSet entityRefValue =
  case entityRefValue of
    NodeEntity nodeRefValue -> Set.singleton nodeRefValue
    EdgeEntity _ -> Set.empty

matchedEdgeRefSet :: EntityRef -> Set EdgeRef
matchedEdgeRefSet entityRefValue =
  case entityRefValue of
    NodeEntity _ -> Set.empty
    EdgeEntity edgeRefValue -> Set.singleton edgeRefValue
