module Moonlight.Differential.Index.Reverse.Batch
  ( addMembership,
    dropMembership,
    lookupMany,
    insertMapAxis,
    dropMapAxis,
    insertIntSetAxis,
    lookupManyIntSet,
    rebuildIntAxisFromMap,
    rebuildIntAxisFromIntMap,
    validateIntAxisFromMap,
    validateIntAxisFromIntMap,
  )
where

import Data.Foldable qualified as Foldable
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Differential.Index.IntSet
  ( insertIntSetIndex,
    lookupIntSetIndex,
  )
import Moonlight.Differential.Index.Reverse
  ( deleteIntMember,
    deleteMapMember,
    insertIntMembers,
    insertMapMembers,
  )

insertMapAxis ::
  (Foldable keys, Ord axis, Ord member) =>
  member ->
  keys axis ->
  Map axis (Set member) ->
  Map axis (Set member)
insertMapAxis member keys index0 =
  Foldable.foldl'
    ( \index key ->
        insertMapMembers key (Set.singleton member) index
    )
    index0
    keys
{-# INLINE insertMapAxis #-}

dropMapAxis ::
  (Foldable keys, Ord axis, Ord member) =>
  member ->
  keys axis ->
  Map axis (Set member) ->
  Map axis (Set member)
dropMapAxis member keys index0 =
  Foldable.foldl'
    ( \index key ->
        deleteMapMember key member index
    )
    index0
    keys
{-# INLINE dropMapAxis #-}

insertIntSetAxis ::
  Int ->
  IntSet ->
  IntMap IntSet ->
  IntMap IntSet
insertIntSetAxis member keys index0 =
  insertIntSetIndex keys (IntSet.singleton member) index0
{-# INLINE insertIntSetAxis #-}

addMembership ::
  (Ord member) =>
  member ->
  IntSet ->
  IntMap (Set member) ->
  IntMap (Set member)
addMembership member keys index0 =
  IntSet.foldl'
    ( \index key ->
        insertIntMembers key (Set.singleton member) index
    )
    index0
    keys
{-# INLINE addMembership #-}

dropMembership ::
  (Ord member) =>
  member ->
  IntSet ->
  IntMap (Set member) ->
  IntMap (Set member)
dropMembership member keys index0 =
  IntSet.foldl'
    ( \index key ->
        deleteIntMember key member index
    )
    index0
    keys
{-# INLINE dropMembership #-}

lookupMany ::
  (Ord member) =>
  IntMap (Set member) ->
  IntSet ->
  Set member
lookupMany index =
  IntSet.foldl'
    ( \members key ->
        Set.union members (IntMap.findWithDefault Set.empty key index)
    )
    Set.empty
{-# INLINE lookupMany #-}

lookupManyIntSet ::
  IntMap IntSet ->
  IntSet ->
  IntSet
lookupManyIntSet index =
  IntSet.foldl'
    ( \members key ->
        IntSet.union members (lookupIntSetIndex key index)
    )
    IntSet.empty
{-# INLINE lookupManyIntSet #-}

rebuildIntAxisFromMap ::
  (Ord member) =>
  (member -> value -> IntSet) ->
  Map member value ->
  IntMap (Set member)
rebuildIntAxisFromMap project =
  Map.foldlWithKey'
    ( \index member value ->
        addMembership member (project member value) index
    )
    IntMap.empty
{-# INLINE rebuildIntAxisFromMap #-}

rebuildIntAxisFromIntMap ::
  (Int -> value -> IntSet) ->
  IntMap value ->
  IntMap IntSet
rebuildIntAxisFromIntMap project =
  IntMap.foldlWithKey'
    ( \index member value ->
        insertIntSetAxis member (project member value) index
    )
    IntMap.empty
{-# INLINE rebuildIntAxisFromIntMap #-}

validateIntAxisFromMap ::
  (Ord member) =>
  (IntMap (Set member) -> IntMap (Set member) -> error) ->
  (member -> value -> IntSet) ->
  Map member value ->
  IntMap (Set member) ->
  Either error ()
validateIntAxisFromMap mkMismatch project source actual =
  let expected =
        rebuildIntAxisFromMap project source
   in if expected == actual
        then Right ()
        else Left (mkMismatch expected actual)
{-# INLINE validateIntAxisFromMap #-}

validateIntAxisFromIntMap ::
  (IntMap IntSet -> IntMap IntSet -> error) ->
  (Int -> value -> IntSet) ->
  IntMap value ->
  IntMap IntSet ->
  Either error ()
validateIntAxisFromIntMap mkMismatch project source actual =
  let expected =
        rebuildIntAxisFromIntMap project source
   in if expected == actual
        then Right ()
        else Left (mkMismatch expected actual)
{-# INLINE validateIntAxisFromIntMap #-}
