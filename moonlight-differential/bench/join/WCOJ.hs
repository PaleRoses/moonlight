module WCOJ where

import Control.DeepSeq (NFData (..))
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Moonlight.Differential.Join.WCOJ
import Moonlight.Differential.Join.WCOJ.Dense.Triangle

newtype WCOJProblem = WCOJProblem
  { wpIndexedProblem :: IntIndexedJoinProblem
  }
  deriving stock (Eq, Show)

instance NFData WCOJProblem where
  rnf (WCOJProblem problem) =
    intIndexedJoinProblemWeight problem
      `seq` ()

newtype PreparedDenseTriangle = PreparedDenseTriangle DenseTriangleTrie

instance NFData PreparedDenseTriangle where
  rnf (PreparedDenseTriangle trie) =
    rnf trie

newtype PreparedTriangleEdges = PreparedTriangleEdges [(Int, Int)]

instance NFData PreparedTriangleEdges where
  rnf (PreparedTriangleEdges edges) =
    triangleEdgeWeight edges `seq` ()

wcojSizes :: [Int]
wcojSizes =
  [8, 16]

denseTriangleSizes :: [Int]
denseTriangleSizes =
  [128, 512]

wcojProblem :: Int -> WCOJProblem
wcojProblem size =
  WCOJProblem
    ( intIndexedJoinProblem
        universe
        [ intBinaryConstraintIndex 0 1 (relationBy (\left right -> (left + right) `mod` 3 == 0)),
          intBinaryConstraintIndex 0 2 (relationBy (\left right -> (left * 2 + right) `mod` 5 <= 1)),
          intBinaryConstraintIndex 1 2 (relationBy (\left right -> (left + right * 3) `mod` 7 <= 2))
        ]
    )
  where
    universe =
      IntSet.fromAscList [0 .. size - 1]

    relationBy predicate =
      intBinaryRelationIndexFromList
        [ (left, right)
        | left <- IntSet.toAscList universe,
          right <- IntSet.toAscList universe,
          predicate left right
        ]

wcojSlots :: [Slot]
wcojSlots =
  [0, 1, 2]

wcojAlgebra :: JoinAlgebra WCOJProblem Int
wcojAlgebra =
  JoinAlgebra
    { joinCount = wcojDirectCount,
      joinPropose = wcojPropose,
      joinValidate =
        \(WCOJProblem problem) ->
          intIndexedJoinValidate problem
    }

wcojMaterializedCountAlgebra :: JoinAlgebra WCOJProblem Int
wcojMaterializedCountAlgebra =
  wcojAlgebra
    { joinCount =
        \problem assignmentEnv slot ->
          domainSize (wcojPropose problem assignmentEnv slot)
    }

wcojPropose :: WCOJProblem -> Env Int -> Slot -> Domain Int
wcojPropose (WCOJProblem problem) =
  intIndexedJoinPropose problem

wcojDirectCount :: WCOJProblem -> Env Int -> Slot -> Int
wcojDirectCount (WCOJProblem problem) =
  intIndexedJoinCount problem

foldGenericJoinMaterializedCount :: WCOJProblem -> Int
foldGenericJoinMaterializedCount problem =
  length (foldGenericJoin wcojAlgebra problem wcojSlots IntMap.empty (\envs joinedEnv -> joinedEnv : envs) [])

foldGenericJoinCount :: WCOJProblem -> Int
foldGenericJoinCount problem =
  foldGenericJoin wcojAlgebra problem wcojSlots IntMap.empty (\count _env -> count + 1) 0

adaptiveJoinCount :: WCOJProblem -> Int
adaptiveJoinCount problem =
  length (adaptiveJoin wcojAlgebra problem wcojSlots IntMap.empty)

foldAdaptiveJoinCount :: WCOJProblem -> Int
foldAdaptiveJoinCount problem =
  foldAdaptiveJoin wcojAlgebra problem wcojSlots IntMap.empty (\count _env -> count + 1) 0

foldIntIndexedAdaptiveJoinCount :: WCOJProblem -> Int
foldIntIndexedAdaptiveJoinCount (WCOJProblem problem) =
  foldIntIndexedAdaptiveJoin problem wcojSlots IntMap.empty (\count _env -> count + 1) 0

wcojChooseSmallestSlotMaterializedWeight :: WCOJProblem -> Int
wcojChooseSmallestSlotMaterializedWeight =
  wcojChooseSmallestSlotWeight wcojMaterializedCountAlgebra

wcojChooseSmallestSlotDirectWeight :: WCOJProblem -> Int
wcojChooseSmallestSlotDirectWeight =
  wcojChooseSmallestSlotWeight wcojAlgebra

wcojChooseSmallestSlotWeight :: JoinAlgebra WCOJProblem Int -> WCOJProblem -> Int
wcojChooseSmallestSlotWeight algebra problem =
  maybe
    0
    ( \(slot, remainingSlots) ->
        slot + length remainingSlots
    )
    (chooseSmallestSlot algebra problem wcojSlots IntMap.empty)

denseTrianglePathCase :: Int -> Either DenseTriangleBuildError PreparedDenseTriangle
denseTrianglePathCase size =
  denseTriangleCaseFromEdges size (pathGraphEdges size)

denseTriangleStarCase :: Int -> Either DenseTriangleBuildError PreparedDenseTriangle
denseTriangleStarCase size =
  denseTriangleCaseFromEdges size (starGraphEdges size)

denseTriangleCliqueCase :: Int -> Either DenseTriangleBuildError PreparedDenseTriangle
denseTriangleCliqueCase size =
  denseTriangleCaseFromEdges size (cliqueGraphEdges size)

denseTriangleExactCliqueCase :: Int -> PreparedTriangleEdges
denseTriangleExactCliqueCase =
  PreparedTriangleEdges . cliqueGraphEdges

denseTriangleSkewedCase :: Int -> Either DenseTriangleBuildError PreparedDenseTriangle
denseTriangleSkewedCase size =
  denseTriangleCaseFromEdges size (skewedTriangleGraphEdges size)

denseTriangleCaseFromEdges :: Int -> [(Int, Int)] -> Either DenseTriangleBuildError PreparedDenseTriangle
denseTriangleCaseFromEdges vertexCount =
  fmap PreparedDenseTriangle . buildDenseTriangleTrie vertexCount

denseTriangleCountWeight :: PreparedDenseTriangle -> Int
denseTriangleCountWeight (PreparedDenseTriangle trie) =
  let countValue =
        countTrianglesWCOJ trie
   in tcTriangles countValue + tcIntersectionSteps countValue

exactTriangleCountWeight :: PreparedTriangleEdges -> Int
exactTriangleCountWeight (PreparedTriangleEdges rawEdges) =
  exactTriangleCount rawEdges

exactTriangleCount :: [(Int, Int)] -> Int
exactTriangleCount rawEdges =
  let edges =
        Set.toAscList (Set.fromList (mapMaybe normalizeUndirectedEdge rawEdges))
      adjacency =
        Foldable.foldl' insertExactTriangleForwardEdge IntMap.empty edges
   in Foldable.foldl'
        (\count edge -> count + exactTriangleForwardEdgeCount adjacency edge)
        0
        edges

insertExactTriangleForwardEdge :: IntMap.IntMap IntSet.IntSet -> (Int, Int) -> IntMap.IntMap IntSet.IntSet
insertExactTriangleForwardEdge adjacency (leftVertex, rightVertex) =
  IntMap.insertWith
    IntSet.union
    leftVertex
    (IntSet.singleton rightVertex)
    adjacency

exactTriangleForwardEdgeCount :: IntMap.IntMap IntSet.IntSet -> (Int, Int) -> Int
exactTriangleForwardEdgeCount adjacency (leftVertex, rightVertex) =
  IntSet.size
    ( IntSet.intersection
        (IntMap.findWithDefault IntSet.empty leftVertex adjacency)
        (IntMap.findWithDefault IntSet.empty rightVertex adjacency)
    )

triangleEdgeWeight :: [(Int, Int)] -> Int
triangleEdgeWeight =
  Foldable.foldl' (\acc (leftVertex, rightVertex) -> acc + leftVertex + rightVertex) 0

pathGraphEdges :: Int -> [(Int, Int)]
pathGraphEdges size =
  zip [0 .. size - 2] [1 .. size - 1]

starGraphEdges :: Int -> [(Int, Int)]
starGraphEdges size =
  (\vertex -> (0, vertex)) <$> [1 .. size - 1]

cliqueGraphEdges :: Int -> [(Int, Int)]
cliqueGraphEdges size =
  [ (leftVertex, rightVertex)
  | leftVertex <- [0 .. size - 1],
    rightVertex <- [leftVertex + 1 .. size - 1]
  ]

skewedTriangleGraphEdges :: Int -> [(Int, Int)]
skewedTriangleGraphEdges size =
  (0, 1)
    : foldMap
      (\vertex -> [(0, vertex), (1, vertex)])
      [2 .. size - 1]
