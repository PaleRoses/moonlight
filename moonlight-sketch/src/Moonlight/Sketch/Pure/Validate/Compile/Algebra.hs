module Moonlight.Sketch.Pure.Validate.Compile.Algebra
  ( mkRegistryIdentity,
    buildRefComponentGraph,
    collectRefDependencies,
  )
where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm qualified as AdjacencyMapAlgorithm
import Algebra.Graph.NonEmpty.AdjacencyMap qualified as NonEmptyAdjacencyMap
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Sketch.Pure.Types
  ( RefId,
    SchemaF (..),
    SchemaNode,
    cataSchema,
  )
import Moonlight.Sketch.Pure.Validate.Compile.Core
  ( ComponentId (..),
    NormalForm (..),
    RefComponentGraph (..),
    SchemaIdentity (..),
    mkRegistryIdentity,
  )

buildRefComponentGraph :: Map.Map RefId SchemaIdentity -> RefComponentGraph
buildRefComponentGraph definitions =
  let knownRefs = Set.fromList (Map.keys definitions)
      dependencyGraph =
        AdjacencyMap.fromAdjacencySets
          . Map.toAscList
          $
          ( Map.map
              (Set.intersection knownRefs . collectRefDependencies . unNormalForm . siNormalForm)
              definitions
          )
      components =
        strongComponentSets dependencyGraph
      componentEntries =
        zipWith
          (\index members -> (ComponentId index, members))
          [0 ..]
          components
      membersByComponent = Map.fromList componentEntries
      componentByRef =
        Map.fromList
          ( concatMap
              (\(componentId, members) ->
                 map (\refId -> (refId, componentId)) (Set.toList members)
              )
              componentEntries
          )
   in
    RefComponentGraph
      { rcgDefinitions = definitions,
        rcgComponentByRef = componentByRef,
        rcgMembersByComponent = membersByComponent
      }

collectRefDependencies :: SchemaNode -> Set.Set RefId
collectRefDependencies =
  cataSchema
    (\layer ->
       let nestedDependencies = foldMap id layer
        in case layer of
             SRefF refId -> Set.insert refId nestedDependencies
             SLazyF refId -> Set.insert refId nestedDependencies
             _ -> nestedDependencies
    )

strongComponentSets :: Ord vertex => AdjacencyMap vertex -> [Set.Set vertex]
strongComponentSets graph =
  List.sortOn
    Set.lookupMin
    ( fmap
        (Set.fromList . NonEmpty.toList . NonEmptyAdjacencyMap.vertexList1)
        (AdjacencyMap.vertexList (AdjacencyMapAlgorithm.scc graph))
    )
