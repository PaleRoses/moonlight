{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.EGraph.Pure.Structural.Store
  ( StructuralStore,
    StructuralLookup (..),
    StructuralEdit (..),
    StructuralTuplePatch (..),
    emptyStructuralStore,
    emptyStructuralTuplePatch,
    tuplePatchNull,
    tuplePatchTouchedKeys,
    structuralLookupTupleAll,
    structuralLookupLeastTuple,
    structuralLookupLeast,
    structuralResultKeys,
    structuralEntries,
    structuralTuplesForResultKey,
    structuralRowBucketForTag,
    structuralParentKeysOf,
    structuralChildrenByResult,
    structuralChildrenByResultWithin,
    structuralDirtyResultKeys,
    structuralRepairClosure,
    structuralRepairIndex,
    canonicalizeStructuralDirtyRows,
    insertCanonicalTuple,
    storeNodeCount,
  )
where

import Data.Foldable (toList)
import Data.Functor (void)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core (ClassId (..), Language, classIdKey)
import Moonlight.Core
  ( reachabilityFromInt,
  )
import Moonlight.Core (TheorySpec, canonicalizeLayerByTheory)
import Moonlight.EGraph.Pure.Types.Core (ENode (..))
import Moonlight.Repair.Index (RepairIndex (..))

type StructuralStore :: (Type -> Type) -> Type
data StructuralStore f = StructuralStore
  { ssOwnersByNode :: !(Map (ENode f) IntSet),
    ssNodesByResult :: !(IntMap (Set (ENode f))),
    ssParentKeysByChild :: !(IntMap IntSet),
    ssRowsByTag :: !(Map (f ()) (IntMap (Set [Int])))
  }

type StructuralLookup :: Type
data StructuralLookup
  = StructuralMissing
  | StructuralUnique !ClassId
  | StructuralAmbiguous !(NonEmpty ClassId)
  deriving stock (Eq, Ord, Show)

type StructuralTuplePatch :: (Type -> Type) -> Type
data StructuralTuplePatch f = StructuralTuplePatch
  { stpRemoved :: !(IntMap (Set (ENode f))),
    stpInserted :: !(IntMap (Set (ENode f)))
  }

type StructuralEdit :: (Type -> Type) -> Type
data StructuralEdit f = StructuralEdit
  { seStore :: !(StructuralStore f),
    seTuplePatch :: !(StructuralTuplePatch f),
    seCongruenceObstructions :: ![(ClassId, ClassId)]
  }

type StructuralRowRewrite :: (Type -> Type) -> Type
data StructuralRowRewrite f = StructuralRowRewrite
  { srrRemovedKey :: !Int,
    srrRemovedNode :: !(ENode f),
    srrInsertedKey :: !Int,
    srrInsertedNode :: !(ENode f)
  }

emptyStructuralStore :: StructuralStore f
emptyStructuralStore =
  StructuralStore
    { ssOwnersByNode = Map.empty,
      ssNodesByResult = IntMap.empty,
      ssParentKeysByChild = IntMap.empty,
      ssRowsByTag = Map.empty
    }

emptyStructuralTuplePatch :: StructuralTuplePatch f
emptyStructuralTuplePatch =
  StructuralTuplePatch
    { stpRemoved = IntMap.empty,
      stpInserted = IntMap.empty
    }

instance Ord (ENode f) => Semigroup (StructuralTuplePatch f) where
  left <> right =
    StructuralTuplePatch
      { stpRemoved = unionPatch (stpRemoved left) (stpRemoved right),
        stpInserted = unionPatch (stpInserted left) (stpInserted right)
      }
    where
      unionPatch =
        IntMap.unionWith Set.union

instance Ord (ENode f) => Monoid (StructuralTuplePatch f) where
  mempty =
    emptyStructuralTuplePatch

tuplePatchNull :: StructuralTuplePatch f -> Bool
tuplePatchNull patchValue =
  IntMap.null (stpRemoved patchValue)
    && IntMap.null (stpInserted patchValue)
{-# INLINE tuplePatchNull #-}

tuplePatchTouchedKeys :: StructuralTuplePatch f -> IntSet
tuplePatchTouchedKeys patchValue =
  IntMap.keysSet (stpRemoved patchValue)
    <> IntMap.keysSet (stpInserted patchValue)
{-# INLINE tuplePatchTouchedKeys #-}

structuralLookupLeastTuple ::
  Language f =>
  ENode f ->
  StructuralStore f ->
  Maybe ClassId
structuralLookupLeastTuple enode =
  structuralLookupLeast . structuralLookupTupleAll enode
{-# INLINE structuralLookupLeastTuple #-}

structuralLookupTupleAll ::
  Language f =>
  ENode f ->
  StructuralStore f ->
  StructuralLookup
structuralLookupTupleAll enode =
  structuralLookupFromResultKeys . structuralOwnersForTuple enode
{-# INLINE structuralLookupTupleAll #-}

structuralResultKeys :: StructuralStore f -> IntSet
structuralResultKeys =
  IntMap.keysSet . ssNodesByResult
{-# INLINE structuralResultKeys #-}

structuralEntries ::
  StructuralStore f ->
  [(ClassId, ENode f)]
structuralEntries store =
  [ (ClassId resultKey, enode)
    | (resultKey, nodes) <- IntMap.toAscList (ssNodesByResult store),
      enode <- Set.toAscList nodes
  ]
{-# INLINE structuralEntries #-}

repairTuplesByResult :: StructuralStore f -> IntMap [ENode f]
repairTuplesByResult =
  IntMap.map Set.toAscList . ssNodesByResult
{-# INLINE repairTuplesByResult #-}

structuralTuplesForResultKey :: Int -> StructuralStore f -> [ENode f]
structuralTuplesForResultKey resultKey store =
  Set.toAscList (IntMap.findWithDefault Set.empty resultKey (ssNodesByResult store))
{-# INLINE structuralTuplesForResultKey #-}

-- | Rows for one operator tag, bucketed by result key with child-key lists as
-- the row payload. A traversable layer is determined by its erased shape plus
-- its child list, so the bucket is a faithful projection of the tagged rows
-- and lookups through it never compare node layers.
structuralRowBucketForTag :: Language f => f () -> StructuralStore f -> IntMap (Set [Int])
structuralRowBucketForTag tag store =
  Map.findWithDefault IntMap.empty tag (ssRowsByTag store)
{-# INLINE structuralRowBucketForTag #-}

enodeTag :: Functor f => ENode f -> f ()
enodeTag (ENode node) =
  void node
{-# INLINE enodeTag #-}

enodeChildKeys :: Foldable f => ENode f -> [Int]
enodeChildKeys (ENode node) =
  fmap classIdKey (toList node)
{-# INLINE enodeChildKeys #-}

structuralOwnersForTuple :: Language f => ENode f -> StructuralStore f -> IntSet
structuralOwnersForTuple enode store =
  Map.findWithDefault IntSet.empty enode (ssOwnersByNode store)
{-# INLINE structuralOwnersForTuple #-}

structuralParentKeysOf :: StructuralStore f -> IntSet -> IntSet
structuralParentKeysOf store keys =
  IntSet.foldl'
    (\parents childKey -> parents <> IntMap.findWithDefault IntSet.empty childKey (ssParentKeysByChild store))
    IntSet.empty
    keys
{-# INLINE structuralParentKeysOf #-}

structuralChildrenByResult :: Language f => StructuralStore f -> IntMap (IntMap Int)
structuralChildrenByResult =
  IntMap.mapMaybe nonEmptyChildren . IntMap.map nodesChildMultiplicity . ssNodesByResult
  where
    nonEmptyChildren children
      | IntMap.null children =
          Nothing
      | otherwise =
          Just children
{-# INLINE structuralChildrenByResult #-}

-- | Children-by-result restricted to the given result keys on both the outer
-- domain and the inner child keys, touching only the rows those keys own.
structuralChildrenByResultWithin :: Language f => IntSet -> StructuralStore f -> IntMap (IntMap Int)
structuralChildrenByResultWithin resultKeys store =
  IntMap.mapMaybe nonEmptyChildren (IntMap.fromSet childrenAt resultKeys)
  where
    childrenAt resultKey =
      maybe
        IntMap.empty
        ((`IntMap.restrictKeys` resultKeys) . nodesChildMultiplicity)
        (IntMap.lookup resultKey (ssNodesByResult store))

    nonEmptyChildren children
      | IntMap.null children =
          Nothing
      | otherwise =
          Just children
{-# INLINE structuralChildrenByResultWithin #-}

nodesChildMultiplicity :: Language f => Set (ENode f) -> IntMap Int
nodesChildMultiplicity =
  Set.foldl'
    (\children enode -> IntMap.unionWith (+) children (nodeChildMultiplicity enode))
    IntMap.empty
{-# INLINE nodesChildMultiplicity #-}

nodeChildMultiplicity :: Foldable f => ENode f -> IntMap Int
nodeChildMultiplicity (ENode node) =
  IntMap.fromListWith (+) [(classIdKey child, 1) | child <- toList node]
{-# INLINE nodeChildMultiplicity #-}

structuralDirtyResultKeys :: StructuralStore f -> IntSet -> IntSet
structuralDirtyResultKeys store impactedKeys =
  let impactedAndParents =
        IntSet.union impactedKeys (structuralParentKeysOf store impactedKeys)
   in IntSet.filter (`IntMap.member` ssNodesByResult store) impactedAndParents
{-# INLINE structuralDirtyResultKeys #-}

structuralRepairClosure :: StructuralStore f -> IntSet -> IntSet
structuralRepairClosure store seedKeys =
  reachabilityFromInt
    (\key -> structuralParentKeysOf store (IntSet.singleton key))
    seedKeys
{-# INLINE structuralRepairClosure #-}

structuralRepairIndex :: Language f => StructuralStore f -> RepairIndex (ENode f)
structuralRepairIndex store =
  RepairIndex
    { riParents = ssParentKeysByChild store,
      riChildren = structuralChildrenByResult store,
      riTuplesByResult = repairTuplesByResult store
    }
{-# INLINE structuralRepairIndex #-}

canonicalizeStructuralDirtyRows ::
  Language f =>
  TheorySpec f ->
  (ClassId -> ClassId) ->
  IntSet ->
  StructuralStore f ->
  StructuralEdit f
canonicalizeStructuralDirtyRows theorySpec canonicalize dirtyKeys store =
  StructuralEdit
    { seStore = nextStore,
      seTuplePatch = structuralRowRewritePatch rewrites,
      seCongruenceObstructions = residualObstructions
    }
  where
    rewrites =
      structuralRowRewrites theorySpec canonicalize dirtyKeys store

    (nextStore, residualObstructions) =
      applyStructuralRowRewrites rewrites store
{-# INLINE canonicalizeStructuralDirtyRows #-}

structuralRowRewrites ::
  Language f =>
  TheorySpec f ->
  (ClassId -> ClassId) ->
  IntSet ->
  StructuralStore f ->
  [StructuralRowRewrite f]
structuralRowRewrites theorySpec canonicalize dirtyKeys store =
  mapMaybe
    (uncurry (structuralRowRewrite theorySpec canonicalize))
    dirtyRows
  where
    dirtyRows =
      [ (ClassId resultKey, enode)
        | resultKey <- IntSet.toAscList (structuralDirtyResultKeys store dirtyKeys),
          enode <- structuralTuplesForResultKey resultKey store
      ]

structuralRowRewrite ::
  Language f =>
  TheorySpec f ->
  (ClassId -> ClassId) ->
  ClassId ->
  ENode f ->
  Maybe (StructuralRowRewrite f)
structuralRowRewrite theorySpec canonicalize oldResult oldNode@(ENode node)
  | oldResult == newResult && oldNode == newNode =
      Nothing
  | otherwise =
      Just
        StructuralRowRewrite
          { srrRemovedKey = classIdKey oldResult,
            srrRemovedNode = oldNode,
            srrInsertedKey = classIdKey newResult,
            srrInsertedNode = newNode
          }
  where
    newResult =
      canonicalize oldResult
    newNode =
      ENode (canonicalizeLayerByTheory theorySpec (fmap canonicalize node))

applyStructuralRowRewrites ::
  forall f.
  Language f =>
  [StructuralRowRewrite f] ->
  StructuralStore f ->
  (StructuralStore f, [(ClassId, ClassId)])
applyStructuralRowRewrites rewrites store =
  foldl' applyRewrite (store, []) rewrites
  where
    applyRewrite ::
      (StructuralStore f, [(ClassId, ClassId)]) ->
      StructuralRowRewrite f ->
      (StructuralStore f, [(ClassId, ClassId)])
    applyRewrite (currentStore, currentObstructions) rewrite =
      let withoutOld =
            deleteStructuralRow (srrRemovedKey rewrite) (srrRemovedNode rewrite) currentStore
          (withNew, newObstructions) =
            insertStructuralRow (srrInsertedKey rewrite) (srrInsertedNode rewrite) withoutOld
       in (withNew, currentObstructions <> newObstructions)
{-# INLINE applyStructuralRowRewrites #-}

structuralRowRewritePatch ::
  forall f.
  Language f =>
  [StructuralRowRewrite f] ->
  StructuralTuplePatch f
structuralRowRewritePatch =
  foldl' insertRewrite emptyStructuralTuplePatch
  where
    insertRewrite ::
      StructuralTuplePatch f ->
      StructuralRowRewrite f ->
      StructuralTuplePatch f
    insertRewrite patchValue rewrite =
      StructuralTuplePatch
        { stpRemoved =
            IntMap.insertWith
              Set.union
              (srrRemovedKey rewrite)
              (Set.singleton (srrRemovedNode rewrite))
              (stpRemoved patchValue),
          stpInserted =
            IntMap.insertWith
              Set.union
              (srrInsertedKey rewrite)
              (Set.singleton (srrInsertedNode rewrite))
              (stpInserted patchValue)
        }

insertCanonicalTuple ::
  Language f =>
  ClassId ->
  ENode f ->
  StructuralStore f ->
  StructuralEdit f
insertCanonicalTuple resultClassId enode store
  | IntSet.member resultKey existingOwners =
      StructuralEdit
        { seStore = store,
          seTuplePatch = emptyStructuralTuplePatch,
          seCongruenceObstructions = ownerObstructionPairs resultKey existingOwners
        }
  | otherwise =
      let (nextStore, residualObstructions) =
            insertStructuralRow resultKey enode store
       in StructuralEdit
            { seStore = nextStore,
              seTuplePatch =
                StructuralTuplePatch
                  { stpRemoved = IntMap.empty,
                    stpInserted = IntMap.singleton resultKey (Set.singleton enode)
                  },
              seCongruenceObstructions = residualObstructions
            }
  where
    resultKey =
      classIdKey resultClassId
    existingOwners =
      structuralOwnersForTuple enode store

storeNodeCount :: StructuralStore f -> Int
storeNodeCount =
  IntMap.foldl' (\count nodes -> count + Set.size nodes) 0 . ssNodesByResult
{-# INLINE storeNodeCount #-}

structuralLookupFromResultKeys :: IntSet -> StructuralLookup
structuralLookupFromResultKeys resultKeys =
  case IntSet.minView resultKeys of
    Nothing ->
      StructuralMissing
    Just (firstKey, remainingKeys)
      | IntSet.null remainingKeys ->
          StructuralUnique (ClassId firstKey)
      | otherwise ->
          StructuralAmbiguous (ClassId firstKey :| fmap ClassId (IntSet.toAscList remainingKeys))
{-# INLINE structuralLookupFromResultKeys #-}

structuralLookupLeast :: StructuralLookup -> Maybe ClassId
structuralLookupLeast lookupResult =
  case lookupResult of
    StructuralMissing ->
      Nothing
    StructuralUnique key ->
      Just key
    StructuralAmbiguous (key :| _) ->
      Just key
{-# INLINE structuralLookupLeast #-}

insertStructuralRow ::
  Language f =>
  Int ->
  ENode f ->
  StructuralStore f ->
  (StructuralStore f, [(ClassId, ClassId)])
insertStructuralRow resultKey enode store =
  (nextStore, ownerObstructionPairs resultKey existingOwners)
  where
    existingOwners =
      structuralOwnersForTuple enode store

    nextStore =
      if IntSet.member resultKey existingOwners
        then store
        else insertStructuralNode resultKey enode store
{-# INLINE insertStructuralRow #-}

insertStructuralNode ::
  Language f =>
  Int ->
  ENode f ->
  StructuralStore f ->
  StructuralStore f
insertStructuralNode resultKey enode store =
  StructuralStore
    { ssOwnersByNode =
        Map.insertWith
          IntSet.union
          enode
          (IntSet.singleton resultKey)
          (ssOwnersByNode store),
      ssNodesByResult =
        IntMap.insertWith
          Set.union
          resultKey
          (Set.singleton enode)
          (ssNodesByResult store),
      ssParentKeysByChild =
        insertParentEdges resultKey enode (ssParentKeysByChild store),
      ssRowsByTag =
        Map.insertWith
          (IntMap.unionWith Set.union)
          (enodeTag enode)
          (IntMap.singleton resultKey (Set.singleton (enodeChildKeys enode)))
          (ssRowsByTag store)
    }
{-# INLINE insertStructuralNode #-}

deleteStructuralRow ::
  Language f =>
  Int ->
  ENode f ->
  StructuralStore f ->
  StructuralStore f
deleteStructuralRow resultKey enode store =
  StructuralStore
    { ssOwnersByNode = nextOwnersByNode,
      ssNodesByResult = nextNodesByResult,
      ssParentKeysByChild = nextParentKeysByChild,
      ssRowsByTag = nextRowsByTag
    }
  where
    nextOwnersByNode =
      Map.update deleteOwner enode (ssOwnersByNode store)

    nextRowsByTag =
      Map.update deleteTagRows (enodeTag enode) (ssRowsByTag store)

    deleteTagRows tagRows =
      let nextTagRows = IntMap.update deleteRowSet resultKey tagRows
       in if IntMap.null nextTagRows then Nothing else Just nextTagRows

    deleteRowSet rows =
      let nextRows = Set.delete (enodeChildKeys enode) rows
       in if Set.null nextRows then Nothing else Just nextRows

    nextNodesByResult =
      IntMap.update deleteNode resultKey (ssNodesByResult store)

    deleteOwner owners =
      let nextOwners = IntSet.delete resultKey owners
       in if IntSet.null nextOwners then Nothing else Just nextOwners

    deleteNode nodes =
      let nextNodes = Set.delete enode nodes
       in if Set.null nextNodes then Nothing else Just nextNodes

    retainedNodes =
      IntMap.findWithDefault Set.empty resultKey nextNodesByResult

    deletedChildKeys =
      case enode of
        ENode node ->
          IntSet.fromList (fmap classIdKey (toList node))

    childStillReferenced childKey =
      any
        (\(ENode retained) -> any ((== childKey) . classIdKey) (toList retained))
        (Set.toList retainedNodes)

    dropParentEdge parentKeys =
      let nextParents = IntSet.delete resultKey parentKeys
       in if IntSet.null nextParents then Nothing else Just nextParents

    nextParentKeysByChild =
      IntSet.foldl'
        ( \parents childKey ->
            if childStillReferenced childKey
              then parents
              else IntMap.update dropParentEdge childKey parents
        )
        (ssParentKeysByChild store)
        deletedChildKeys
{-# INLINE deleteStructuralRow #-}

insertParentEdges :: Foldable f => Int -> ENode f -> IntMap IntSet -> IntMap IntSet
insertParentEdges resultKey (ENode node) parentKeysByChild =
  foldl'
    (\parents child ->
       IntMap.insertWith
         IntSet.union
         (classIdKey child)
         (IntSet.singleton resultKey)
         parents
    )
    parentKeysByChild
    (toList node)
{-# INLINE insertParentEdges #-}

ownerObstructionPairs :: Int -> IntSet -> [(ClassId, ClassId)]
ownerObstructionPairs resultKey owners
  | IntSet.null owners =
      []
  | IntSet.member resultKey owners =
      obstructionPairs owners
  | otherwise =
      fmap
        (\owner -> obstructionPair (ClassId resultKey) (ClassId owner))
        (IntSet.toAscList owners)
{-# INLINE ownerObstructionPairs #-}

obstructionPair :: ClassId -> ClassId -> (ClassId, ClassId)
obstructionPair left right =
  if left <= right then (left, right) else (right, left)
{-# INLINE obstructionPair #-}

obstructionPairs :: IntSet -> [(ClassId, ClassId)]
obstructionPairs owners =
  case IntSet.minView owners of
    Nothing ->
      []
    Just (representative, others) ->
      fmap
        (\other -> (ClassId representative, ClassId other))
        (IntSet.toAscList others)
