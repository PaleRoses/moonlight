module DeltaSpec
  ( tests,
  )
where

import Moonlight.Graph
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Data.Kind (Type)

type TestNodeKind :: Type
data TestNodeKind = NodeAlpha deriving stock (Eq, Show)
type TestEdgeKind :: Type
data TestEdgeKind = EdgeLink deriving stock (Eq, Show)

nodeRefAlpha :: NodeRef
nodeRefAlpha = NodeRef 1

edgeRefAlpha :: EdgeRef
edgeRefAlpha = EdgeRef 7

insertNodeDelta :: GraphDelta TestNodeKind TestEdgeKind
insertNodeDelta = InsertNode nodeRefAlpha NodeAlpha emptyAttributes

removeEdgeDelta :: GraphDelta TestNodeKind TestEdgeKind
removeEdgeDelta = RemoveEdge edgeRefAlpha

tests :: TestTree
tests =
  testGroup
    "Delta"
    [ testCase "magma batches graph edits canonically" $ do
        let expected = Batch [insertNodeDelta, removeEdgeDelta]
        assertEqual "batch concatenation should preserve edit order" expected ((<>) insertNodeDelta removeEdgeDelta),
      testCase "monoid identity is empty batch" $ do
        assertEqual "graph delta identity should be an empty batch" (Batch []) (mempty :: GraphDelta TestNodeKind TestEdgeKind),
      testCase "flattenGraphDelta removes nested batch structure" $ do
        let deltaValue = Batch [insertNodeDelta, Batch [removeEdgeDelta]]
            expected = [insertNodeDelta, removeEdgeDelta]
        assertEqual "flattening should erase nested batches" expected (flattenGraphDelta deltaValue)
    ]
