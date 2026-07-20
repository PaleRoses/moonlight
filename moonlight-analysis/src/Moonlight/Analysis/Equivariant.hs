{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Analysis.Equivariant
  ( GlobalAutomorphismGroup (..),
    OrbitFingerprint (..),
    GlobalOrbitModel (..),
    globalAutomorphismGroup,
    representativeOf,
    equivariantRepresentatives,
    equivariantPruningGate,
  )
where

import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map

type GlobalAutomorphismGroup :: Type
data GlobalAutomorphismGroup = GlobalAutomorphismGroup
  { gagOrbitMembers :: !(IntMap IntSet),
    gagRepresentativeByNode :: !(IntMap Int)
  }
  deriving stock (Eq, Show)

type OrbitFingerprint :: Type -> Type
data OrbitFingerprint token
  = SingletonOrbit !Int
  | SymmetricOrbit ![token]
  deriving stock (Eq, Ord, Show)

type GlobalOrbitModel :: Type -> Type -> Type -> Type
data GlobalOrbitModel scaffold cell fingerprint = GlobalOrbitModel
  { gomCellsWithNodeIds :: scaffold -> [(cell, Int)],
    gomFingerprintOf :: cell -> Int -> fingerprint
  }

globalAutomorphismGroup ::
  Ord fingerprint =>
  GlobalOrbitModel scaffold cell fingerprint ->
  scaffold ->
  GlobalAutomorphismGroup
globalAutomorphismGroup orbitModel scaffoldValue =
  let orbitBuckets =
        Map.fromListWith IntSet.union
          ( fmap
              (\(cellValue, nodeIdValue) -> (gomFingerprintOf orbitModel cellValue nodeIdValue, IntSet.singleton nodeIdValue))
              (gomCellsWithNodeIds orbitModel scaffoldValue)
          )
      orbitMembers =
        IntMap.fromList
          ( fmap
              (\memberSet -> (IntSet.findMin memberSet, memberSet))
              (Map.elems orbitBuckets)
          )
      representativeByNode =
        IntMap.fromList
          ( concatMap
              (\(representativeNode, memberSet) -> fmap (\memberNode -> (memberNode, representativeNode)) (IntSet.toAscList memberSet))
              (IntMap.toList orbitMembers)
          )
   in GlobalAutomorphismGroup
        { gagOrbitMembers = orbitMembers,
          gagRepresentativeByNode = representativeByNode
        }

representativeOf :: GlobalAutomorphismGroup -> Int -> Int
representativeOf globalGroup nodeIdValue =
  IntMap.findWithDefault nodeIdValue nodeIdValue (gagRepresentativeByNode globalGroup)

equivariantRepresentatives ::
  (seed -> Int) ->
  GlobalAutomorphismGroup ->
  [seed] ->
  [seed]
equivariantRepresentatives projectNodeId globalGroup =
  reverse . snd . foldl retainRepresentativeSeed (IntSet.empty, [])
  where
    retainRepresentativeSeed (seenRepresentatives, keptSeeds) seedValue =
      let representativeNode = representativeOf globalGroup (projectNodeId seedValue)
       in if IntSet.member representativeNode seenRepresentatives
            then (seenRepresentatives, keptSeeds)
            else (IntSet.insert representativeNode seenRepresentatives, seedValue : keptSeeds)

equivariantPruningGate ::
  (seed -> Int) ->
  (seed -> IntSet) ->
  (seed -> IntSet) ->
  GlobalAutomorphismGroup ->
  seed ->
  Bool
equivariantPruningGate projectNodeId projectLocalObjects projectLocalOrbitRepresentatives globalGroup seedValue =
  localOrbitPass && globalOrbitPass
  where
    nodeIdValue = projectNodeId seedValue
    localOrbitPass =
      if IntSet.member nodeIdValue (projectLocalObjects seedValue)
        then IntSet.member nodeIdValue (projectLocalOrbitRepresentatives seedValue)
        else True
    globalOrbitPass =
      nodeIdValue == representativeOf globalGroup nodeIdValue
