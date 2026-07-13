{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Graph.Pure.LocalTopology
  ( ChildMulti,
    LocalAdj (..),
    buildLocalAdjFromMaps,
    buildLocalAdjFromIntMaps,
    cyclicCellsFromChildren,
    cyclicCellsFromChildrenInt,
    MergeTopology (..),
    mergeCreatesNewCycle,
    mergeTopologyNeighborhood,
    mergeTopologyFromAdj,
    closedStarAdj,
    closedStarAdjInt,
    siblingAwareStar,
    siblingAwareStarInt,
    localEdges,
    localEdgesInt,
    countLocalEdges,
    countLocalEdgesInt,
    beta1,
    constantSheafBeta1,
  )
where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm qualified as AdjacencyMapAlgorithm
import Algebra.Graph.NonEmpty.AdjacencyMap qualified as NonEmptyAdjacencyMap
import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

type ChildMulti :: Type -> Type
type ChildMulti cell = Map cell (Map cell Int)

type LocalAdj :: Type -> Type
data LocalAdj cell = LocalAdj
  { laParents :: !(Set cell),
    laChildren :: !(Map cell Int)
  }
  deriving stock (Eq, Show)

type MergeTopology :: Type -> Type
data MergeTopology cell = MergeTopology
  { mtCreatesNewCycle :: !Bool,
    mtOverlapSize :: !Int,
    mtNeighborhoodSize :: !Int,
    mtEdgeCount :: !Int,
    mtNeighborhoodU :: !(Set cell),
    mtNeighborhoodV :: !(Set cell),
    mtOverlapVertices :: !(Set cell)
  }
  deriving stock (Eq, Show)

mergeCreatesNewCycle :: MergeTopology cell -> Bool
mergeCreatesNewCycle =
  mtCreatesNewCycle

mergeTopologyNeighborhood :: MergeTopology Int -> IntSet
mergeTopologyNeighborhood topology =
  setToIntSet
    (Set.union (mtNeighborhoodU topology) (mtNeighborhoodV topology))

buildLocalAdjFromMaps :: Ord cell => Map cell (Set cell) -> ChildMulti cell -> Map cell (LocalAdj cell)
buildLocalAdjFromMaps parentsByChild childrenByParent =
  Map.fromSet localAdjAt allCells
  where
    allCells =
      Set.unions
        ( Map.keysSet parentsByChild
            : Map.keysSet childrenByParent
            : fmap Map.keysSet (Map.elems childrenByParent)
        )

    localAdjAt cell =
      LocalAdj
        { laParents = Map.findWithDefault Set.empty cell parentsByChild,
          laChildren = Map.findWithDefault Map.empty cell childrenByParent
        }

buildLocalAdjFromIntMaps :: IntMap IntSet -> IntMap (IntMap Int) -> Map Int (LocalAdj Int)
buildLocalAdjFromIntMaps parentsByChild childrenByParent =
  Map.fromAscList
    [ ( cell,
        LocalAdj
          { laParents = intSetToSet (IntMap.findWithDefault IntSet.empty cell parentsByChild),
            laChildren = intMapToMap (IntMap.findWithDefault IntMap.empty cell childrenByParent)
          }
      )
      | cell <- IntSet.toAscList allCells
    ]
  where
    allCells =
      IntSet.unions
        ( IntMap.keysSet parentsByChild
            : IntMap.keysSet childrenByParent
            : fmap IntMap.keysSet (IntMap.elems childrenByParent)
        )

cyclicCellsFromChildren :: Ord cell => ChildMulti cell -> Set cell
cyclicCellsFromChildren childMulti =
  Set.unions
    (fmap (Set.fromList . NonEmpty.toList) (cyclicComponentVertexLists graph))
  where
    graph = childMultiAdjacencyMap childMulti

childMultiAdjacencyMap :: Ord cell => ChildMulti cell -> AdjacencyMap cell
childMultiAdjacencyMap childMulti =
  AdjacencyMap.overlay
    (AdjacencyMap.vertices (Set.toAscList vertices))
    (AdjacencyMap.fromAdjacencySets (Map.toAscList (Map.map Map.keysSet childMulti)))
  where
    vertices =
      Set.unions
        ( Map.keysSet childMulti
            : fmap Map.keysSet (Map.elems childMulti)
        )

cyclicCellsFromChildrenInt :: IntMap (IntMap Int) -> IntSet
cyclicCellsFromChildrenInt =
  setToIntSet . cyclicCellsFromChildren . intChildMultiToMap

mergeTopologyFromAdj :: Ord cell => cell -> cell -> Map cell (LocalAdj cell) -> MergeTopology cell
mergeTopologyFromAdj leftCell rightCell adjacency =
  let leftNeighborhood = siblingAwareStar adjacency leftCell
      rightNeighborhood = siblingAwareStar adjacency rightCell
      neighborhood = Set.union leftNeighborhood rightNeighborhood
      overlapVertices = Set.intersection leftNeighborhood rightNeighborhood
      -- Rebuild needs the local obstruction only: identifying two distinct
      -- closed stars can close a cycle precisely when the stars already
      -- overlap. Cohomological rank diagnostics are derived views, not a toll
      -- booth on every e-graph merge.
      createsNewCycle =
        leftCell /= rightCell && not (Set.null overlapVertices)
   in MergeTopology
        { mtCreatesNewCycle = createsNewCycle,
          mtOverlapSize = Set.size overlapVertices,
          mtNeighborhoodSize = Set.size neighborhood,
          mtEdgeCount = countLocalEdges adjacency neighborhood,
          mtNeighborhoodU = leftNeighborhood,
          mtNeighborhoodV = rightNeighborhood,
          mtOverlapVertices = overlapVertices
        }

closedStarAdj :: Ord cell => Map cell (LocalAdj cell) -> cell -> Set cell
closedStarAdj adjacency cell =
  case Map.lookup cell adjacency of
    Nothing -> Set.singleton cell
    Just localAdj ->
      Set.insert cell (Set.union (laParents localAdj) (Map.keysSet (laChildren localAdj)))

closedStarAdjInt :: Map Int (LocalAdj Int) -> Int -> IntSet
closedStarAdjInt adjacency =
  setToIntSet . closedStarAdj adjacency

siblingAwareStar :: Ord cell => Map cell (LocalAdj cell) -> cell -> Set cell
siblingAwareStar adjacency cell =
  let star = closedStarAdj adjacency cell
      parents = maybe Set.empty laParents (Map.lookup cell adjacency)
      siblings =
        Set.foldl'
          (\cells parentCell ->
             Set.union cells
               (maybe Set.empty (Map.keysSet . laChildren) (Map.lookup parentCell adjacency))
          )
          Set.empty
          parents
   in Set.union star siblings

siblingAwareStarInt :: Map Int (LocalAdj Int) -> Int -> IntSet
siblingAwareStarInt adjacency =
  setToIntSet . siblingAwareStar adjacency

localEdges :: Ord cell => Map cell (LocalAdj cell) -> Set cell -> [(cell, cell)]
localEdges adjacency vertices =
  Set.toList vertices >>= edgesFromParent
  where
    edgesFromParent parentCell =
      case Map.lookup parentCell adjacency of
        Nothing -> []
        Just localAdj ->
          Map.toList (laChildren localAdj)
            >>= \(childCell, multiplicity) ->
              if Set.member childCell vertices && multiplicity > 0
                then replicate multiplicity (parentCell, childCell)
                else []

localEdgesInt :: Map Int (LocalAdj Int) -> IntSet -> [(Int, Int)]
localEdgesInt adjacency =
  localEdges adjacency . intSetToSet

countLocalEdges :: Ord cell => Map cell (LocalAdj cell) -> Set cell -> Int
countLocalEdges adjacency vertices =
  Set.foldl' countEdges 0 vertices
  where
    countEdges edgeCount parentCell =
      case Map.lookup parentCell adjacency of
        Nothing -> edgeCount
        Just localAdj ->
          edgeCount
            + Map.foldlWithKey'
              (\count childCell multiplicity ->
                 if Set.member childCell vertices
                   then count + multiplicity
                   else count
              )
              0
              (laChildren localAdj)

countLocalEdgesInt :: Map Int (LocalAdj Int) -> IntSet -> Int
countLocalEdgesInt adjacency =
  countLocalEdges adjacency . intSetToSet

cyclicComponentVertexLists :: Ord vertex => AdjacencyMap vertex -> [NonEmpty.NonEmpty vertex]
cyclicComponentVertexLists graph =
  List.sortOn
    (Set.lookupMin . Set.fromList . NonEmpty.toList)
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

intMapToMap :: IntMap value -> Map Int value
intMapToMap =
  Map.fromAscList . IntMap.toAscList

intChildMultiToMap :: IntMap (IntMap Int) -> ChildMulti Int
intChildMultiToMap =
  Map.fromAscList . fmap (fmap intMapToMap) . IntMap.toAscList

intSetToSet :: IntSet -> Set Int
intSetToSet =
  Set.fromAscList . IntSet.toAscList

setToIntSet :: Set Int -> IntSet
setToIntSet =
  Set.foldl' (flip IntSet.insert) IntSet.empty
