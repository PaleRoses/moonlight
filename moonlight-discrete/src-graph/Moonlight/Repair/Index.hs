module Moonlight.Repair.Index
  ( RepairIndex (..),
    repairParentClosure,
    repairDirtyResultClosure,
    repairTotalTupleCount,
    repairTouchedTupleCount,
    repairDirtyResultFrontier,
    repairSupport,
    repairIndexUniverse,
    repairAdjacencyFootprint,
    repairAdjacencyChanged,
  )
where

import Algebra.Graph.AdjacencyIntMap qualified as AdjacencyIntMap
import Algebra.Graph.AdjacencyIntMap.Algorithm qualified as AdjacencyIntMapAlgorithm
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

type RepairIndex :: Type -> Type
data RepairIndex tuple = RepairIndex
  { riParents :: !(IntMap IntSet),
    riChildren :: !(IntMap (IntMap Int)),
    riTuplesByResult :: !(IntMap [tuple])
  }
  deriving stock (Eq, Show)

repairParentClosure :: RepairIndex tuple -> IntSet -> IntSet
repairParentClosure repairIndex =
  IntSet.foldl'
    ( \acc key ->
        IntSet.insert key
          (IntSet.union acc (IntMap.findWithDefault IntSet.empty key (riParents repairIndex)))
    )
    IntSet.empty

repairDirtyResultClosure :: RepairIndex tuple -> IntSet -> IntSet
repairDirtyResultClosure repairIndex keys =
  IntSet.union
    (IntSet.intersection keys (IntMap.keysSet (riTuplesByResult repairIndex)))
    (repairParentClosure repairIndex keys)

repairTotalTupleCount :: RepairIndex tuple -> Int
repairTotalTupleCount =
  sum . fmap length . riTuplesByResult

repairTouchedTupleCount :: RepairIndex tuple -> IntSet -> Int
repairTouchedTupleCount repairIndex =
  IntSet.foldl'
    (\countValue resultKey -> countValue + length (IntMap.findWithDefault [] resultKey (riTuplesByResult repairIndex)))
    0

repairDirtyResultFrontier :: RepairIndex tuple -> IntSet -> IntSet
repairDirtyResultFrontier repairIndex changedMembers =
  IntSet.union
    (repairParentClosure repairIndex changedMembers)
    (IntSet.intersection changedMembers (IntMap.keysSet (riTuplesByResult repairIndex)))

repairSupport :: RepairIndex tuple -> IntSet -> IntSet
repairSupport repairIndex seeds =
  let graph =
        AdjacencyIntMap.overlay
          (AdjacencyIntMap.vertices (IntSet.toAscList seeds))
          (AdjacencyIntMap.fromAdjacencyIntSets (IntMap.toAscList (riParents repairIndex)))
   in IntSet.union seeds
        . IntSet.fromList
        . concatMap (AdjacencyIntMapAlgorithm.reachable graph)
        . IntSet.toAscList
        $ seeds

repairIndexUniverse :: RepairIndex tuple -> IntSet
repairIndexUniverse repairIndex =
  IntSet.union
    (repairAdjacencyFootprint repairIndex)
    (IntMap.keysSet (riTuplesByResult repairIndex))

repairAdjacencyFootprint :: RepairIndex tuple -> IntSet
repairAdjacencyFootprint repairIndex =
  IntSet.unions
    [ IntMap.keysSet (riParents repairIndex),
      foldMap id (riParents repairIndex),
      IntMap.keysSet (riChildren repairIndex),
      foldMap IntMap.keysSet (riChildren repairIndex)
    ]

repairAdjacencyChanged :: RepairIndex tuple -> RepairIndex tuple -> Bool
repairAdjacencyChanged oldIndex newIndex =
  riParents oldIndex /= riParents newIndex
    || riChildren oldIndex /= riChildren newIndex
