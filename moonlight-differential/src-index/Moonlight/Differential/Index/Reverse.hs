{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Index.Reverse
  ( ReverseAxis (..),
    intSetIntersects,
    mapReverseAxis,
    intReverseAxis,
    expectedReverseIndex,
    expectedMapReverseIndex,
    expectedIntReverseIndex,
    validateReverseIndex,
    validateMapReverseIndex,
    validateIntReverseIndex,
    insertMapMembers,
    deleteMapMember,
    insertIntMembers,
    deleteIntMember,
    pruneEmpty,
    finishInvariantErrors,
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
import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Differential.Index.IntSet
  ( intSetIntersects,
  )

type ReverseAxis :: Type -> Type -> Type -> Type -> Type
data ReverseAxis keys index key ident = ReverseAxis
  { raEmpty :: !index,
    raInsertMembers :: !(key -> Set ident -> index -> index),
    raFoldKeys :: !((index -> key -> index) -> index -> keys -> index),
    raToAscList :: !(index -> [(key, Set ident)]),
    raFindMembers :: !(key -> index -> Set ident)
  }

mapReverseAxis ::
  (Ord key, Ord ident) =>
  ReverseAxis (Set key) (Map key (Set ident)) key ident
mapReverseAxis =
  ReverseAxis
    { raEmpty = Map.empty,
      raInsertMembers = insertMapMembers,
      raFoldKeys = Set.foldl',
      raToAscList = Map.toAscList,
      raFindMembers = \key index -> Map.findWithDefault Set.empty key index
    }

intReverseAxis ::
  Ord ident =>
  ReverseAxis IntSet (IntMap (Set ident)) Int ident
intReverseAxis =
  ReverseAxis
    { raEmpty = IntMap.empty,
      raInsertMembers = insertIntMembers,
      raFoldKeys = IntSet.foldl',
      raToAscList = IntMap.toAscList,
      raFindMembers = \key index -> IntMap.findWithDefault Set.empty key index
    }

expectedReverseIndex ::
  ReverseAxis keys index key ident ->
  (ident -> entity -> keys) ->
  Map ident entity ->
  index
expectedReverseIndex axis keysOf =
  Map.foldlWithKey'
    ( \index ident entity ->
        raFoldKeys
          axis
          ( \acc key ->
              raInsertMembers axis key (Set.singleton ident) acc
          )
          index
          (keysOf ident entity)
    )
    (raEmpty axis)

expectedMapReverseIndex ::
  (Ord key, Ord ident) =>
  (ident -> entity -> Set key) ->
  Map ident entity ->
  Map key (Set ident)
expectedMapReverseIndex keysOf =
  expectedReverseIndex mapReverseAxis keysOf

expectedIntReverseIndex ::
  Ord ident =>
  (ident -> entity -> IntSet) ->
  Map ident entity ->
  IntMap (Set ident)
expectedIntReverseIndex keysOf =
  expectedReverseIndex intReverseAxis keysOf

validateReverseIndex ::
  Ord ident =>
  ReverseAxis keys index key ident ->
  (ident -> entity -> keys) ->
  (ident -> key -> errorValue) ->
  (ident -> key -> errorValue) ->
  Map ident entity ->
  index ->
  [errorValue]
validateReverseIndex axis keysOf missingError staleError rows actual =
  missingErrors <> staleErrors
  where
    expected =
      expectedReverseIndex axis keysOf rows

    missingErrors =
      [ missingError ident key
      | (key, expectedIdents) <- raToAscList axis expected,
        let actualIdents = raFindMembers axis key actual,
        ident <- Set.toAscList (Set.difference expectedIdents actualIdents)
      ]

    staleErrors =
      [ staleError ident key
      | (key, actualIdents) <- raToAscList axis actual,
        let expectedIdents = raFindMembers axis key expected,
        ident <- Set.toAscList (Set.difference actualIdents expectedIdents)
      ]

validateMapReverseIndex ::
  (Ord key, Ord ident) =>
  (ident -> entity -> Set key) ->
  (ident -> key -> errorValue) ->
  (ident -> key -> errorValue) ->
  Map ident entity ->
  Map key (Set ident) ->
  [errorValue]
validateMapReverseIndex keysOf missingError staleError rows actual =
  validateReverseIndex mapReverseAxis keysOf missingError staleError rows actual

validateIntReverseIndex ::
  Ord ident =>
  (ident -> entity -> IntSet) ->
  (ident -> Int -> errorValue) ->
  (ident -> Int -> errorValue) ->
  Map ident entity ->
  IntMap (Set ident) ->
  [errorValue]
validateIntReverseIndex keysOf missingError staleError rows actual =
  validateReverseIndex intReverseAxis keysOf missingError staleError rows actual

insertMapMembers ::
  (Ord key, Ord ident) =>
  key ->
  Set ident ->
  Map key (Set ident) ->
  Map key (Set ident)
insertMapMembers key members index
  | Set.null members =
      index
  | otherwise =
      Map.insertWith Set.union key members index

deleteMapMember ::
  (Ord key, Ord ident) =>
  key ->
  ident ->
  Map key (Set ident) ->
  Map key (Set ident)
deleteMapMember key member =
  Map.update (pruneEmpty . Set.delete member) key

insertIntMembers ::
  Ord ident =>
  Int ->
  Set ident ->
  IntMap (Set ident) ->
  IntMap (Set ident)
insertIntMembers key members index
  | Set.null members =
      index
  | otherwise =
      IntMap.insertWith Set.union key members index

deleteIntMember ::
  Ord ident =>
  Int ->
  ident ->
  IntMap (Set ident) ->
  IntMap (Set ident)
deleteIntMember key member =
  IntMap.update (pruneEmpty . Set.delete member) key

pruneEmpty :: Set value -> Maybe (Set value)
pruneEmpty values
  | Set.null values =
      Nothing
  | otherwise =
      Just values

finishInvariantErrors ::
  [errorValue] ->
  Either [errorValue] ()
finishInvariantErrors errors =
  case errors of
    [] ->
      Right ()
    _ ->
      Left errors
