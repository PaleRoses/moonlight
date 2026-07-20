-- | A persistent union-find over e-graph class identifiers. The public value
-- remains immutable; batched mutation is available explicitly from
-- "Moonlight.Core.UnionFind.Transaction".
module Moonlight.Core.UnionFind
  ( UnionFind,
    UnionFindAllocationError (..),
    emptyUnionFind,
    fromClassIds,
    makeSet,
    insertClassId,
    member,
    find,
    findExisting,
    canonicalClass,
    union,
    equivalent,
    samePartition,
    canonicalMap,
    canonicalMapAndCompress,
  )
where

import Data.Foldable qualified as Foldable
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Moonlight.Core.Identifier.EGraph
  ( ClassId (..),
    classIdKey,
  )
import Moonlight.Core.UnionFind.Internal.Semantics
  ( LinkDecision (..),
    chooseLink,
  )
import Moonlight.Core.UnionFind.Internal.Types
  ( UnionFind (..),
    UnionFindAllocationError,
    advanceNextFreshForClassIdKey,
    allocateNextClassId,
  )
import Moonlight.Core.UnionFind.Transaction qualified as Transaction
import Prelude
  ( Bool,
    Eq ((==)),
    Foldable,
    Either,
    Int,
    Maybe (..),
    fmap,
    min,
    otherwise,
    pure,
    (.),
  )

emptyUnionFind :: UnionFind
emptyUnionFind =
  UnionFind
    { ufParent = IntMap.empty,
      ufRank = IntMap.empty,
      ufNextFresh = 0
    }

fromClassIds :: Foldable t => t ClassId -> UnionFind
fromClassIds =
  Foldable.foldl' (\unionFind classId -> insertClassId classId unionFind) emptyUnionFind

insertClassId :: ClassId -> UnionFind -> UnionFind
insertClassId classId unionFind =
  let key = classIdKey classId
   in if IntMap.member key (ufParent unionFind)
        then unionFind {ufNextFresh = advanceNextFreshForClassIdKey key (ufNextFresh unionFind)}
        else
          unionFind
            { ufParent = IntMap.insert key classId (ufParent unionFind),
              ufRank = IntMap.insert key 0 (ufRank unionFind),
              ufNextFresh = advanceNextFreshForClassIdKey key (ufNextFresh unionFind)
            }

member :: ClassId -> UnionFind -> Bool
member classId =
  IntMap.member (classIdKey classId) . ufParent

makeSet :: UnionFind -> Either UnionFindAllocationError (ClassId, UnionFind)
makeSet unionFind = do
  (classId, nextFresh) <- allocateNextClassId (ufNextFresh unionFind)
  let key = classIdKey classId
  pure
    ( classId,
      unionFind
        { ufParent = IntMap.insert key classId (ufParent unionFind),
          ufRank = IntMap.insert key 0 (ufRank unionFind),
          ufNextFresh = nextFresh
        }
    )

find :: ClassId -> UnionFind -> (ClassId, UnionFind)
find classId unionFind =
  let (rootKey, compressedParents) =
        compressRootKey (ufParent unionFind) (classIdKey classId)
   in ( ClassId rootKey,
        unionFind {ufParent = compressedParents}
      )

findExisting :: ClassId -> UnionFind -> Maybe (ClassId, UnionFind)
findExisting classId unionFind =
  if member classId unionFind
    then Just (find classId unionFind)
    else Nothing

canonicalClass :: ClassId -> UnionFind -> Maybe ClassId
canonicalClass classId unionFind =
  if member classId unionFind
    then Just (ClassId (rootKeyOf unionFind (classIdKey classId)))
    else Nothing

union :: ClassId -> ClassId -> UnionFind -> UnionFind
union leftClassId rightClassId unionFind =
  let seededUnionFind = insertClassId rightClassId (insertClassId leftClassId unionFind)
      (leftRoot, unionFindAfterLeft) = find leftClassId seededUnionFind
      (rightRoot, unionFindAfterRight) = find rightClassId unionFindAfterLeft
   in if leftRoot == rightRoot
        then unionFindAfterRight
        else linkRoots leftRoot rightRoot unionFindAfterRight

equivalent :: ClassId -> ClassId -> UnionFind -> Bool
equivalent leftClassId rightClassId unionFind =
  rootKeyOf unionFind (classIdKey leftClassId)
    == rootKeyOf unionFind (classIdKey rightClassId)

canonicalMap :: UnionFind -> IntMap ClassId
canonicalMap unionFind =
  IntMap.mapWithKey
    (\key _ -> ClassId (rootKeyOf unionFind key))
    (ufParent unionFind)

samePartition :: UnionFind -> UnionFind -> Bool
samePartition leftUnionFind rightUnionFind =
  partitionProjection leftUnionFind == partitionProjection rightUnionFind

partitionProjection :: UnionFind -> IntMap ClassId
partitionProjection unionFind =
  IntMap.map canonicalRootClass canonicalParents
  where
    canonicalParents =
      canonicalMap unionFind
    classMinima =
      IntMap.foldlWithKey' insertMinimum IntMap.empty canonicalParents
    insertMinimum minima memberKey rootClass =
      IntMap.insertWith min (classIdKey rootClass) (ClassId memberKey) minima
    canonicalRootClass rootClass =
      IntMap.findWithDefault rootClass (classIdKey rootClass) classMinima

canonicalMapAndCompress :: UnionFind -> (IntMap ClassId, UnionFind)
canonicalMapAndCompress unionFind =
  Transaction.runUnionFindTransaction unionFind Transaction.transactionCanonicalMapAndCompress

parentKeyOf :: UnionFind -> Int -> Maybe Int
parentKeyOf unionFind key =
  fmap classIdKey (IntMap.lookup key (ufParent unionFind))

rankOf :: ClassId -> UnionFind -> Int
rankOf classId unionFind =
  IntMap.findWithDefault 0 (classIdKey classId) (ufRank unionFind)

rootKeyOf :: UnionFind -> Int -> Int
rootKeyOf unionFind key =
  case parentKeyOf unionFind key of
    Nothing ->
      key
    Just parentKey
      | parentKey == key ->
          key
      | otherwise ->
          rootKeyOf unionFind parentKey

compressRootKey :: IntMap ClassId -> Int -> (Int, IntMap ClassId)
compressRootKey parents key =
  case IntMap.lookup key parents of
    Nothing ->
      (key, parents)
    Just parentClassId ->
      let parentKey = classIdKey parentClassId
       in if parentKey == key
            then (key, parents)
            else
              let (rootKey, compressedParents) =
                    compressRootKey parents parentKey
               in (rootKey, IntMap.insert key (ClassId rootKey) compressedParents)

linkRoots :: ClassId -> ClassId -> UnionFind -> UnionFind
linkRoots leftRoot rightRoot unionFind =
  applyLinkDecision
    ( chooseLink
        leftRoot
        (rankOf leftRoot unionFind)
        rightRoot
        (rankOf rightRoot unionFind)
    )
    unionFind

applyLinkDecision :: LinkDecision -> UnionFind -> UnionFind
applyLinkDecision decision unionFind =
  case decision of
    AttachRoot childRoot parentRoot ->
      setParent childRoot parentRoot unionFind
    AttachRootAndRaise childRoot parentRoot raisedRank ->
      (setParent childRoot parentRoot unionFind)
        { ufRank =
            IntMap.insert
              (classIdKey parentRoot)
              raisedRank
              (ufRank unionFind)
        }

setParent :: ClassId -> ClassId -> UnionFind -> UnionFind
setParent childClassId parentClassId unionFind =
  unionFind
    { ufParent =
        IntMap.insert
          (classIdKey childClassId)
          parentClassId
          (ufParent unionFind)
    }
