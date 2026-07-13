module SelectorSpec
  ( tests,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (pack)
import Moonlight.Graph
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
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

graphView :: GraphView TestGraph TestNodeKind TestEdgeKind
graphView = GraphView testGraphNodes testGraphEdges

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

graphValue :: TestGraph
graphValue =
  TestGraph
    { testGraphNodes =
        Map.fromList
          [ (nodeRefRoot, (TestRoot, attributesFromList [(massKey, ContinuousVal 1.0 0.0 1.0)])),
            (nodeRefLeaf, (TestLeaf, attributesFromList [(tagKey, TagVal (Set.singleton (pack "leaf")))]))
          ],
      testGraphEdges =
        Map.fromList
          [ (edgeRefArc, (TestArc, [nodeRefRoot, nodeRefLeaf], emptyAttributes))
          ]
    }

tests :: TestTree
tests =
  testGroup
    "Selector"
    [ testCase "resolveGraphSelector finds node kind matches" $ do
        let expected = Set.singleton (NodeEntity nodeRefRoot)
        assertEqual "selector should recover matching node entities" expected (resolveGraphSelector graphView (ByNodeKind TestRoot) graphValue),
      testCase "resolveGraphSelector matches attribute predicates" $ do
        let expected = Set.singleton (NodeEntity nodeRefLeaf)
        assertEqual "attribute selector should recover tagged node" expected (resolveGraphSelector graphView (ByAttribute tagKey (AttrHasTag (pack "leaf"))) graphValue),
      testCase "lintGraphSelector reports unresolved point selectors" $ do
        assertEqual
          "missing point selector should be unresolved"
          (Just SelectorUnresolved)
          (lintGraphSelector graphView (ByNodeRef (NodeRef 999)) graphValue)
    ]
