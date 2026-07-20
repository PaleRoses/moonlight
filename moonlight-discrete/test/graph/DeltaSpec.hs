module DeltaSpec
  ( tests,
  )
where

import Data.Kind (Type)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Moonlight.Algebra (endoPatchAdds, endoPatchRemoves)
import Moonlight.Graph
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, (@?=), testCase)
import Test.Tasty.QuickCheck
  ( Gen,
    elements,
    forAll,
    listOf,
    testProperty,
    (===),
  )

type TestNodeKind :: Type
data TestNodeKind = NodeAlpha deriving stock (Eq, Show)

type TestEdgeKind :: Type
data TestEdgeKind = EdgeLink deriving stock (Eq, Show)

nodeRefAlpha :: NodeRef
nodeRefAlpha = NodeRef 1

nodeRefBeta :: NodeRef
nodeRefBeta = NodeRef 2

edgeRefAlpha :: EdgeRef
edgeRefAlpha = EdgeRef 7

graphEditGenerator :: Gen (GraphEdit TestNodeKind TestEdgeKind)
graphEditGenerator =
  elements
    [ InsertNode nodeRefAlpha NodeAlpha emptyAttributes,
      RemoveNode nodeRefBeta,
      InsertEdge edgeRefAlpha EdgeLink [nodeRefAlpha, nodeRefBeta] emptyAttributes,
      RemoveEdge edgeRefAlpha,
      MutateNodeAttr nodeRefAlpha (AttrKey (Text.pack "mass")) (ContinuousDelta 1.0 2.0),
      MutateEdgeAttr edgeRefAlpha (AttrKey (Text.pack "tags")) (tagDelta (Set.singleton (Text.pack "new")) Set.empty)
    ]

graphDeltaGenerator :: Gen (GraphDelta TestNodeKind TestEdgeKind)
graphDeltaGenerator = graphDeltaFromList <$> listOf graphEditGenerator

tests :: TestTree
tests =
  testGroup
    "Delta"
    [ testProperty "left identity holds for every generated edit stream" $
        forAll graphDeltaGenerator $ \graphDelta ->
          mempty <> graphDelta === graphDelta,
      testProperty "right identity holds for every generated edit stream" $
        forAll graphDeltaGenerator $ \graphDelta ->
          graphDelta <> mempty === graphDelta,
      testProperty "composition is associative" $
        forAll graphDeltaGenerator $ \firstDelta ->
          forAll graphDeltaGenerator $ \secondDelta ->
            forAll graphDeltaGenerator $ \thirdDelta ->
              (firstDelta <> secondDelta) <> thirdDelta
                === firstDelta <> (secondDelta <> thirdDelta),
      testProperty "composition preserves source edit order" $
        forAll (listOf graphEditGenerator) $ \graphEdits ->
          graphDeltaToList (graphDeltaFromList graphEdits) === graphEdits,
      testProperty "singleton embeds exactly one atomic edit" $
        forAll graphEditGenerator $ \graphEdit ->
          graphDeltaToList (singletonGraphDelta graphEdit) === [graphEdit],
      testCase "tag deltas canonicalize overlapping assignments as add-wins" $
        case tagDelta (Set.singleton (Text.pack "shared")) (Set.singleton (Text.pack "shared")) of
          TagDelta patch -> do
            endoPatchAdds patch @?= Set.singleton (Text.pack "shared")
            endoPatchRemoves patch @?= Set.empty
          ContinuousDelta _ _ ->
            assertFailure "tagDelta constructed a continuous delta"
          DiscreteDelta _ ->
            assertFailure "tagDelta constructed a discrete delta"
    ]
