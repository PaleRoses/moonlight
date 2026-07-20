module Moonlight.Sheaf.Section.Restriction.Reachability
  ( cellNeighborhood,
    cellReachable,
  )
where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm qualified as AdjacencyMapAlgorithm
import Data.Set (Set)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Sheaf.Index.Dense
  ( denseIndexValues,
  )
import Moonlight.Sheaf.Section.Morphism
  ( rSource,
    rTarget,
  )
import Moonlight.Sheaf.Section.ObjectIndex
  ( ObjectIndex,
  )
import Moonlight.Sheaf.Section.Restriction
  ( RestrictionIndex,
    restrictionEntries,
    restrictionsFrom,
    restrictionsTo,
  )

cellNeighborhood :: Ord cell => ObjectIndex cell -> RestrictionIndex cell witness -> cell -> Set cell
cellNeighborhood objects restrictions cell =
  let outgoingEntries = restrictionsFrom objects cell restrictions
      incomingEntries = restrictionsTo objects cell restrictions
   in Set.fromList
        ( fmap rTarget outgoingEntries
            <> fmap rSource incomingEntries
        )

cellReachable :: Ord cell => ObjectIndex cell -> RestrictionIndex cell witness -> cell -> Set cell
cellReachable objects restrictions startCell =
  cellReachableFromMany objects restrictions (Set.singleton startCell)

cellReachableFromMany ::
  (Foldable container, Ord cell) =>
  ObjectIndex cell ->
  RestrictionIndex cell witness ->
  container cell ->
  Set cell
cellReachableFromMany objects restrictions startCells =
  let seeds =
        foldMap Set.singleton startCells
   in reachableSetFromMany (restrictionGraph objects restrictions) seeds

restrictionGraph :: Ord cell => ObjectIndex cell -> RestrictionIndex cell witness -> AdjacencyMap cell
restrictionGraph objects restrictions =
  AdjacencyMap.overlay
    (AdjacencyMap.vertices (denseIndexValues objects))
    (AdjacencyMap.fromAdjacencySets (Map.toAscList (restrictionUndirectedAdjacency restrictions)))

restrictionUndirectedAdjacency :: Ord cell => RestrictionIndex cell witness -> Map cell (Set cell)
restrictionUndirectedAdjacency restrictions =
  foldr
    ( \restriction ->
        Map.insertWith Set.union (rSource restriction) (Set.singleton (rTarget restriction))
          . Map.insertWith Set.union (rTarget restriction) (Set.singleton (rSource restriction))
    )
    mempty
    (restrictionEntries restrictions)

reachableSetFromMany :: Ord vertex => AdjacencyMap vertex -> Set vertex -> Set vertex
reachableSetFromMany graph seeds =
  Set.union seeds
    . Set.fromList
    . concatMap (AdjacencyMapAlgorithm.reachable graph)
    . Set.toAscList
    $ seeds
