module Moonlight.Differential.Index.IntSet
  ( insertSetIndex,
    deleteSetIndex,
    insertMapIndex,
    deleteMapIntSetIndex,
    lookupMapIntSetIndex,
    insertIntSetIndex,
    deleteIntSetIndex,
    lookupIntSetIndex,
    intSetAxisMembers,
    intSetIntersects,
    deleteIntMapKeys,
    alterIntMapNull,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set

insertSetIndex ::
  (Ord key, Ord value) =>
  key ->
  value ->
  Map key (Set value) ->
  Map key (Set value)
insertSetIndex key value =
  Map.insertWith Set.union key (Set.singleton value)

deleteSetIndex ::
  (Ord key, Ord value) =>
  key ->
  value ->
  Map key (Set value) ->
  Map key (Set value)
deleteSetIndex key value =
  Map.update (pruneSet . Set.delete value) key

insertMapIndex ::
  Ord key =>
  key ->
  IntSet ->
  Map key IntSet ->
  Map key IntSet
insertMapIndex key members
  | IntSet.null members =
      id
  | otherwise =
      Map.insertWith IntSet.union key members

deleteMapIntSetIndex ::
  Ord key =>
  key ->
  Int ->
  Map key IntSet ->
  Map key IntSet
deleteMapIntSetIndex key member =
  Map.update (pruneIntSet . IntSet.delete member) key

lookupMapIntSetIndex ::
  Ord key =>
  key ->
  Map key IntSet ->
  IntSet
lookupMapIntSetIndex key =
  Map.findWithDefault IntSet.empty key

insertIntSetIndex ::
  IntSet ->
  IntSet ->
  IntMap IntSet ->
  IntMap IntSet
insertIntSetIndex keys members index
  | IntSet.null keys || IntSet.null members =
      index
  | otherwise =
      IntMap.unionWith
        IntSet.union
        (IntMap.fromSet (const members) keys)
        index

deleteIntSetIndex ::
  IntSet ->
  Int ->
  IntMap IntSet ->
  IntMap IntSet
deleteIntSetIndex keys member index
  | IntSet.null keys =
      index
  | otherwise =
      IntMap.differenceWith
        (\members _unit -> pruneIntSet (IntSet.delete member members))
        index
        (IntMap.fromSet (const ()) keys)

lookupIntSetIndex ::
  Int ->
  IntMap IntSet ->
  IntSet
lookupIntSetIndex key =
  IntMap.findWithDefault IntSet.empty key

intSetAxisMembers ::
  IntMap IntSet ->
  Int
intSetAxisMembers =
  IntMap.foldl' (\total members -> total + IntSet.size members) 0

intSetIntersects :: IntSet -> IntSet -> Bool
intSetIntersects left right =
  not (IntSet.null (IntSet.intersection left right))

deleteIntMapKeys ::
  IntSet ->
  IntMap value ->
  IntMap value
deleteIntMapKeys keys values
  | IntSet.null keys =
      values
  | otherwise =
      IntMap.difference values (IntMap.fromSet (const ()) keys)

alterIntMapNull ::
  (value -> Bool) ->
  Int ->
  value ->
  IntMap value ->
  IntMap value
alterIntMapNull isNull key value =
  if isNull value
    then IntMap.delete key
    else IntMap.insert key value

pruneSet :: Set value -> Maybe (Set value)
pruneSet values
  | Set.null values =
      Nothing
  | otherwise =
      Just values

pruneIntSet :: IntSet -> Maybe IntSet
pruneIntSet values
  | IntSet.null values =
      Nothing
  | otherwise =
      Just values
