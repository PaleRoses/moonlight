{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Graph.Pure.LocalTopology
  ( ChildMulti,
    LocalTopologyError (..),
    LocalAdj,
    buildLocalAdjFromChildren,
    cyclicCellsFromChildren,
    cyclicCellsFromAdjacency,
    MergeTopology,
    mergeCreatesNewCycle,
    mergeTopologyNeighborhood,
    mergeTopologyFromAdj,
    closedStarAdj,
    siblingAwareStar,
    localEdges,
    countLocalEdges,
    beta1,
    constantSheafBeta1,
  )
where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm qualified as AdjacencyMapAlgorithm
import Algebra.Graph.NonEmpty.AdjacencyMap qualified as NonEmptyAdjacencyMap
import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set

type ChildMulti = IntMap (IntMap Int)

data LocalTopologyError
  = NonPositiveChildMultiplicity
      { invalidParentCell :: !Int,
        invalidChildCell :: !Int,
        invalidChildMultiplicity :: !Int
      }
  deriving stock (Eq, Ord, Show, Read)

data LocalAdj = LocalAdj
  { laParents :: !IntSet,
    laChildren :: !(IntMap Int)
  }
  deriving stock (Eq, Show)

data MergeTopology = MergeTopology
  { mtCreatesNewCycle :: !Bool,
    mtNeighborhood :: !IntSet
  }
  deriving stock (Eq, Show)

mergeCreatesNewCycle :: MergeTopology -> Bool
mergeCreatesNewCycle =
  mtCreatesNewCycle

mergeTopologyNeighborhood :: MergeTopology -> IntSet
mergeTopologyNeighborhood =
  mtNeighborhood

buildLocalAdjFromChildren ::
  ChildMulti ->
  Either LocalTopologyError (IntMap LocalAdj)
buildLocalAdjFromChildren childrenByParent = do
  validateChildMultiplicities childrenByParent
  pure (IntMap.fromSet localAdjAt allCells)
  where
    parentsByChild =
      IntMap.foldlWithKey' insertParentChildren IntMap.empty childrenByParent

    insertParentChildren :: IntMap IntSet -> Int -> IntMap Int -> IntMap IntSet
    insertParentChildren parents parentCell =
      IntMap.foldlWithKey'
        (\currentParents childCell _ ->
           IntMap.insertWith IntSet.union childCell (IntSet.singleton parentCell) currentParents
        )
        parents

    allCells =
      IntMap.foldl'
        (\cells children -> IntSet.union cells (IntMap.keysSet children))
        (IntMap.keysSet childrenByParent)
        childrenByParent

    localAdjAt cell =
      LocalAdj
        { laParents = IntMap.findWithDefault IntSet.empty cell parentsByChild,
          laChildren = IntMap.findWithDefault IntMap.empty cell childrenByParent
        }

validateChildMultiplicities ::
  ChildMulti ->
  Either LocalTopologyError ()
validateChildMultiplicities =
  traverse_ validateParentEntry . IntMap.toAscList

validateParentEntry ::
  (Int, IntMap Int) ->
  Either LocalTopologyError ()
validateParentEntry (parentCell, children) =
  traverse_ (validateChildEntry parentCell) (IntMap.toAscList children)

validateChildEntry ::
  Int ->
  (Int, Int) ->
  Either LocalTopologyError ()
validateChildEntry parentCell (childCell, multiplicity) =
  if multiplicity > 0
    then Right ()
    else Left (NonPositiveChildMultiplicity parentCell childCell multiplicity)

cyclicCellsFromChildren ::
  ChildMulti ->
  Either LocalTopologyError IntSet
cyclicCellsFromChildren childrenByParent = do
  validateChildMultiplicities childrenByParent
  pure (cyclicCellsFromGraph (childMultiAdjacencyMap childrenByParent))

cyclicCellsFromAdjacency :: IntMap LocalAdj -> IntSet
cyclicCellsFromAdjacency =
  cyclicCellsFromGraph . localAdjacencyMap

cyclicCellsFromGraph :: AdjacencyMap Int -> IntSet
cyclicCellsFromGraph =
  IntSet.unions
    . fmap (IntSet.fromList . NonEmpty.toList)
    . cyclicComponentVertexLists

childMultiAdjacencyMap :: ChildMulti -> AdjacencyMap Int
childMultiAdjacencyMap childrenByParent =
  AdjacencyMap.overlay
    (AdjacencyMap.vertices (IntSet.toAscList allCells))
    ( AdjacencyMap.fromAdjacencySets
        [ (parentCell, Set.fromAscList (IntMap.keys children))
        | (parentCell, children) <- IntMap.toAscList childrenByParent
        ]
    )
  where
    allCells =
      IntMap.foldl'
        (\cells children -> IntSet.union cells (IntMap.keysSet children))
        (IntMap.keysSet childrenByParent)
        childrenByParent

localAdjacencyMap :: IntMap LocalAdj -> AdjacencyMap Int
localAdjacencyMap adjacency =
  AdjacencyMap.overlay
    (AdjacencyMap.vertices (IntMap.keys adjacency))
    ( AdjacencyMap.fromAdjacencySets
        [ (parentCell, Set.fromAscList (IntMap.keys (laChildren localAdj)))
        | (parentCell, localAdj) <- IntMap.toAscList adjacency
        ]
    )

mergeTopologyFromAdj :: Int -> Int -> IntMap LocalAdj -> MergeTopology
mergeTopologyFromAdj leftCell rightCell adjacency =
  let leftNeighborhood = siblingAwareStar adjacency leftCell
      rightNeighborhood = siblingAwareStar adjacency rightCell
      neighborhood = IntSet.union leftNeighborhood rightNeighborhood
      overlapVertices = IntSet.intersection leftNeighborhood rightNeighborhood
      -- Rebuild needs the local obstruction only: identifying two distinct
      -- closed stars can close a cycle precisely when the stars already
      -- overlap. Cohomological rank diagnostics are derived views, not a toll
      -- booth on every e-graph merge.
      createsNewCycle =
        leftCell /= rightCell && not (IntSet.null overlapVertices)
   in MergeTopology
        { mtCreatesNewCycle = createsNewCycle,
          mtNeighborhood = neighborhood
        }

closedStarAdj :: IntMap LocalAdj -> Int -> IntSet
closedStarAdj adjacency cell =
  case IntMap.lookup cell adjacency of
    Nothing -> IntSet.singleton cell
    Just localAdj ->
      IntSet.insert cell (IntSet.union (laParents localAdj) (IntMap.keysSet (laChildren localAdj)))

siblingAwareStar :: IntMap LocalAdj -> Int -> IntSet
siblingAwareStar adjacency cell =
  let star = closedStarAdj adjacency cell
      parents = maybe IntSet.empty laParents (IntMap.lookup cell adjacency)
      siblings =
        IntSet.foldl'
          (\cells parentCell ->
             IntSet.union cells
               (maybe IntSet.empty (IntMap.keysSet . laChildren) (IntMap.lookup parentCell adjacency))
          )
          IntSet.empty
          parents
   in IntSet.union star siblings

localEdges :: IntMap LocalAdj -> IntSet -> [(Int, Int)]
localEdges adjacency vertices =
  IntSet.toList vertices >>= edgesFromParent
  where
    edgesFromParent parentCell =
      case IntMap.lookup parentCell adjacency of
        Nothing -> []
        Just localAdj ->
          IntMap.toList (laChildren localAdj)
            >>= \(childCell, multiplicity) ->
              if IntSet.member childCell vertices
                then replicate multiplicity (parentCell, childCell)
                else []

countLocalEdges :: IntMap LocalAdj -> IntSet -> Int
countLocalEdges adjacency vertices =
  IntSet.foldl' countEdges 0 vertices
  where
    countEdges edgeCount parentCell =
      case IntMap.lookup parentCell adjacency of
        Nothing -> edgeCount
        Just localAdj ->
          edgeCount
            + IntMap.foldlWithKey'
              (\count childCell multiplicity ->
                 if IntSet.member childCell vertices
                   then count + multiplicity
                   else count
              )
              0
              (laChildren localAdj)

cyclicComponentVertexLists :: AdjacencyMap Int -> [NonEmpty.NonEmpty Int]
cyclicComponentVertexLists graph =
  List.sortOn
    (IntSet.lookupMin . IntSet.fromList . NonEmpty.toList)
    (filter cyclicComponent componentVertices)
  where
    componentVertices =
      fmap
        NonEmptyAdjacencyMap.vertexList1
        (AdjacencyMap.vertexList (AdjacencyMapAlgorithm.scc graph))

    cyclicComponent vertices =
      case NonEmpty.toList vertices of
        [vertex] -> AdjacencyMap.hasEdge vertex vertex graph
        _ -> True

beta1 :: Int -> Int -> Int -> Int
beta1 edgeCount vertexCount componentCount =
  edgeCount - vertexCount + componentCount

constantSheafBeta1 :: Int -> Int -> Int
constantSheafBeta1 edgeCount vertexCount =
  edgeCount - vertexCount + 1
