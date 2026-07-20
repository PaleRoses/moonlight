module OpticsSpec
  ( tests,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Moonlight.Graph
import Moonlight.Optics.Effect.Laws (traversalCompositionLaw, traversalIdentityLaw)
import Moonlight.Optics (iover, itoListOf, lens, over, view, (^..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Data.Kind (Type)

type TestNodeKind :: Type
data TestNodeKind = TestRoot | TestLeaf deriving stock (Eq, Show)
type TestEdgeKind :: Type
data TestEdgeKind = TestArc deriving stock (Eq, Show)

type TestGraph :: Type
data TestGraph = TestGraph
  { testGraphNodes :: Map NodeRef (TestNodeKind, Attributes),
    testGraphEdges :: Map EdgeRef (TestEdgeKind, [NodeRef], Attributes)
  }
  deriving stock (Eq, Show)

graphOptics :: GraphOptics TestGraph TestNodeKind TestEdgeKind
graphOptics =
  GraphOptics
    { graphNodesLens = lens testGraphNodes (\graphValue updatedNodes -> graphValue {testGraphNodes = updatedNodes}),
      graphEdgesLens = lens testGraphEdges (\graphValue updatedEdges -> graphValue {testGraphEdges = updatedEdges})
    }

nodeRefRoot :: NodeRef
nodeRefRoot = NodeRef 1

nodeRefLeaf :: NodeRef
nodeRefLeaf = NodeRef 2

edgeRefArc :: EdgeRef
edgeRefArc = EdgeRef 9

massKey :: AttrKey
massKey = AttrKey (pack "mass")

tagKey :: AttrKey
tagKey = AttrKey (pack "tag")

sampleGraph :: TestGraph
sampleGraph =
  TestGraph
    { testGraphNodes =
        Map.fromList
          [ (nodeRefRoot, (TestRoot, numericAttributes 1.0)),
            (nodeRefLeaf, (TestLeaf, tagAttributes [pack "leaf"]))
          ],
      testGraphEdges =
        Map.fromList
          [ (edgeRefArc, (TestArc, [nodeRefRoot, nodeRefLeaf], tagAttributes [pack "bridge"]))
          ]
    }

tests :: TestTree
tests =
  testGroup
    "Optics"
    [ testCase "node attribute traversal satisfies identity" $
        assertBool
          "node attribute traversal should satisfy identity"
          (traversalIdentityLaw (graphNodeAttributesTraversal graphOptics) sampleGraph),
      testCase "edge attribute traversal satisfies composition" $
        assertBool
          "edge attribute traversal should satisfy composition"
          (traversalCompositionLaw (graphEdgeAttributesTraversal graphOptics) addMarker removeMarker sampleGraph),
      testCase "node membership traversal updates kinds and attributes" $ do
        let updatedGraph =
              over
                (graphNodeMembershipTraversal graphOptics)
                (\membershipValue ->
                    NodeMembership
                      { nodeMembershipKind = TestLeaf,
                        nodeMembershipAttributes = addMarker (nodeMembershipAttributes membershipValue)
                      }
                )
                sampleGraph
            expectedNodes =
              Map.fromList
                [ (nodeRefRoot, (TestLeaf, addMarker (numericAttributes 1.0))),
                  (nodeRefLeaf, (TestLeaf, addMarker (tagAttributes [pack "leaf"])))
                ]
        assertEqual "membership traversal should rebuild node payloads" expectedNodes (testGraphNodes updatedGraph),
      testCase "indexed node membership traversal exposes NodeRef ordering" $ do
        let received = itoListOf (graphNodeMembershipIxTraversal graphOptics) sampleGraph
            expected =
              [ (nodeRefRoot, NodeMembership TestRoot (numericAttributes 1.0)),
                (nodeRefLeaf, NodeMembership TestLeaf (tagAttributes [pack "leaf"]))
              ]
        assertEqual "indexed node traversal should carry node refs" expected received,
      testCase "indexed edge membership traversal updates endpoints by EdgeRef" $ do
        let updatedGraph =
              iover
                (graphEdgeMembershipIxTraversal graphOptics)
                (\edgeRefValue membershipValue ->
                    EdgeMembership
                      { edgeMembershipKind = edgeMembershipKind membershipValue,
                        edgeMembershipEndpoints = edgeMembershipEndpoints membershipValue <> [nodeFromEdge edgeRefValue],
                        edgeMembershipAttributes = edgeMembershipAttributes membershipValue
                      }
                )
                sampleGraph
            expectedEdges =
              Map.fromList
                [ (edgeRefArc, (TestArc, [nodeRefRoot, nodeRefLeaf, NodeRef 109], tagAttributes [pack "bridge"]))
                ]
        assertEqual "indexed edge traversal should update using edge refs" expectedEdges (testGraphEdges updatedGraph),
      testCase "selector subgraph getter and subgraph traversals compose" $ do
        let selected = view (selectorSubgraph graphOptics (ByAttribute tagKey (AttrHasTag (pack "leaf")))) sampleGraph
            selectedMemberships = selected ^.. subgraphNodeMembershipTraversal
        assertEqual
          "selector should isolate the leaf node subgraph"
          [NodeMembership TestLeaf (tagAttributes [pack "leaf"])]
          selectedMemberships
    ]

numericAttributes :: Double -> Attributes
numericAttributes value =
  attributesFromList [(massKey, ContinuousVal value 0.0 1.0)]

tagAttributes :: [Text] -> Attributes
tagAttributes tags =
  attributesFromList [(tagKey, TagVal (Set.fromList tags))]

addMarker :: Attributes -> Attributes
addMarker = insertAttribute tagKey (TagVal (Set.singleton (pack "marked")))

removeMarker :: Attributes -> Attributes
removeMarker = deleteAttribute tagKey

nodeFromEdge :: EdgeRef -> NodeRef
nodeFromEdge (EdgeRef rawEdgeRef) = NodeRef (rawEdgeRef + 100)
