module Moonlight.Sketch.Pure.Resolve
  ( resolve,
    resolveWith,
    resolveNode,
    detectCycles,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Sketch.Pure.Types
  ( ObjectProperty (..),
    RefId,
    SchemaNode (..),
    SchemaRegistry (..),
    schemaNodeChildren,
  )

resolve :: SchemaRegistry -> SchemaNode -> SchemaNode
resolve registry = resolveWith registry Set.empty

resolveWith :: SchemaRegistry -> Set.Set RefId -> SchemaNode -> SchemaNode
resolveWith registry visited node =
  case node of
    SRef refId -> resolveRef registry visited refId
    SLazy refId -> resolveRef registry visited refId
    SArray element constraint ->
      SArray (resolveWith registry visited element) constraint
    STuple elements rest ->
      STuple
        (map (resolveWith registry visited) elements)
        (fmap (resolveWith registry visited) rest)
    SRecord value -> SRecord (resolveWith registry visited value)
    SObject fields ->
      SObject
        ( Map.map
            (\propertyValue -> propertyValue {opSchema = resolveWith registry visited (opSchema propertyValue)})
            fields
        )
    SUnion members ->
      SUnion (map (resolveWith registry visited) members)
    SDiscriminatedUnion tag members ->
      SDiscriminatedUnion tag (map (resolveWith registry visited) members)
    SOptional inner -> SOptional (resolveWith registry visited inner)
    SNullable inner -> SNullable (resolveWith registry visited inner)
    SDefault inner defaultValue ->
      SDefault (resolveWith registry visited inner) defaultValue
    SBrand inner brandName -> SBrand (resolveWith registry visited inner) brandName
    SRefine inner refinementId -> SRefine (resolveWith registry visited inner) refinementId
    SPreprocess inner preprocessId -> SPreprocess (resolveWith registry visited inner) preprocessId
    SConstrain inner constraintId -> SConstrain (resolveWith registry visited inner) constraintId
    STransform input output transformId ->
      STransform
        (resolveWith registry visited input)
        (resolveWith registry visited output)
        transformId
    other -> other

resolveRef :: SchemaRegistry -> Set.Set RefId -> RefId -> SchemaNode
resolveRef registry visited refId =
  if Set.member refId visited
    then SRef refId
    else
      case Map.lookup refId (srSchemas registry) of
        Nothing -> SRef refId
        Just definition ->
          resolveWith registry (Set.insert refId visited) definition

resolveNode :: SchemaRegistry -> SchemaNode -> SchemaNode
resolveNode = resolve

detectCycles :: SchemaRegistry -> SchemaNode -> [RefId]
detectCycles registry = detectCyclesWith registry Set.empty []

detectCyclesWith :: SchemaRegistry -> Set.Set RefId -> [RefId] -> SchemaNode -> [RefId]
detectCyclesWith registry visited cycles node =
  case node of
    SRef refId -> checkRef registry visited cycles refId
    SLazy refId -> checkRef registry visited cycles refId
    other -> foldl (detectCyclesWith registry visited) cycles (schemaNodeChildren other)

checkRef :: SchemaRegistry -> Set.Set RefId -> [RefId] -> RefId -> [RefId]
checkRef registry visited cycles refId =
  if Set.member refId visited
    then refId : cycles
    else
      case Map.lookup refId (srSchemas registry) of
        Nothing -> cycles
        Just definition ->
          detectCyclesWith registry (Set.insert refId visited) cycles definition
