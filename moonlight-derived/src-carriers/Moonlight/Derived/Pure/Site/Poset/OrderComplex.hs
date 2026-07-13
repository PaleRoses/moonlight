{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Derived.Pure.Site.Poset.OrderComplex
  ( PreparedOrderComplex
  , PosetChain
  , strictLeq
  , sortTopo
  , isChain
  , prepareOrderComplex
  , orderComplexChainsByDegree
  , preparedOrderComplexChainsByDegree
  , preparedOrderComplexChainIndexMaps
  , restrictOrderComplexChainsTo
  , facesOfChain
  ) where

import Data.IntSet qualified as IS
import Data.Kind (Type)
import Data.List (sortBy)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Vector qualified as V
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset (..)
  , FinObjectId (..)
  , leq
  )

type PosetChain :: Type
type PosetChain = [FinObjectId]

type PreparedOrderComplex :: Type
data PreparedOrderComplex = PreparedOrderComplex
  { pocChainsByDegree :: !(V.Vector [PosetChain])
  , pocChainIndexMaps :: !(V.Vector (Map.Map PosetChain Int))
  }
  deriving stock (Eq, Show)

strictLeq :: DerivedPoset -> FinObjectId -> FinObjectId -> Bool
strictLeq posetValue leftNode rightNode =
  leftNode /= rightNode && leq posetValue leftNode rightNode

topoIndexMap :: DerivedPoset -> Map.Map FinObjectId Int
topoIndexMap posetValue =
  Map.fromList (zip (V.toList (derivedPosetTopoAsc posetValue)) [0 :: Int ..])

sortTopo :: DerivedPoset -> [FinObjectId] -> [FinObjectId]
sortTopo posetValue =
  let orderIndex = topoIndexMap posetValue
   in sortBy
        (comparing (\objectValue -> Map.findWithDefault (unFinObjectId objectValue) objectValue orderIndex))

isChain :: DerivedPoset -> [FinObjectId] -> Bool
isChain posetValue nodeValues =
  let orderedNodes = sortTopo posetValue nodeValues
   in and (zipWith (strictLeq posetValue) orderedNodes (drop 1 orderedNodes))

prepareOrderComplex :: DerivedPoset -> PreparedOrderComplex
prepareOrderComplex posetValue =
  let chainsByDegreeValue = enumerateChainsByDegree posetValue
   in PreparedOrderComplex
        { pocChainsByDegree = chainsByDegreeValue
        , pocChainIndexMaps = V.map chainIndexMap chainsByDegreeValue
        }

orderComplexChainsByDegree :: DerivedPoset -> V.Vector [PosetChain]
orderComplexChainsByDegree =
  pocChainsByDegree . prepareOrderComplex

preparedOrderComplexChainsByDegree :: PreparedOrderComplex -> V.Vector [PosetChain]
preparedOrderComplexChainsByDegree = pocChainsByDegree

preparedOrderComplexChainIndexMaps :: PreparedOrderComplex -> V.Vector (Map.Map PosetChain Int)
preparedOrderComplexChainIndexMaps = pocChainIndexMaps

restrictOrderComplexChainsTo :: IS.IntSet -> PreparedOrderComplex -> V.Vector [PosetChain]
restrictOrderComplexChainsTo nodeKeys =
  V.filter (not . null)
    . V.map (filter (chainContainedIn nodeKeys))
    . pocChainsByDegree

chainContainedIn :: IS.IntSet -> PosetChain -> Bool
chainContainedIn nodeKeys =
  all (\(FinObjectId objectKey) -> IS.member objectKey nodeKeys)

enumerateChainsByDegree :: DerivedPoset -> V.Vector [PosetChain]
enumerateChainsByDegree posetValue =
  let build chainLength accumulatedChains =
        case enumerateChainsOfLength posetValue chainLength of
          [] ->
            case accumulatedChains of
              [] -> V.singleton []
              _ -> V.fromList (reverse accumulatedChains)
          nextChains ->
            build (chainLength + 1) (nextChains : accumulatedChains)
   in build 1 []

enumerateChainsOfLength :: DerivedPoset -> Int -> [PosetChain]
enumerateChainsOfLength posetValue chainLength
  | chainLength <= 0 = [[]]
  | otherwise =
      extendChains
        posetValue
        Nothing
        (V.toList (derivedPosetTopoAsc posetValue))
        chainLength

extendChains :: DerivedPoset -> Maybe FinObjectId -> [FinObjectId] -> Int -> [PosetChain]
extendChains _ _ _ remainingLength
  | remainingLength <= 0 = [[]]
extendChains posetValue previousNode candidateNodes remainingLength =
  case candidateNodes of
    [] -> []
    nodeValue : restNodes ->
      let withNode =
            if admissible previousNode nodeValue
              then
                fmap
                  (nodeValue :)
                  (extendChains posetValue (Just nodeValue) restNodes (remainingLength - 1))
              else []
          withoutNode =
            extendChains posetValue previousNode restNodes remainingLength
       in withNode <> withoutNode
  where
    admissible Nothing _ = True
    admissible (Just previousValue) nodeValue =
      previousValue /= nodeValue && leq posetValue previousValue nodeValue

facesOfChain :: PosetChain -> [PosetChain]
facesOfChain =
  go []
  where
    go :: PosetChain -> PosetChain -> [PosetChain]
    go prefix chainValue =
      case chainValue of
        [] -> []
        nodeValue : suffixValue ->
          (prefix <> suffixValue) : go (prefix <> [nodeValue]) suffixValue

chainIndexMap :: [PosetChain] -> Map.Map PosetChain Int
chainIndexMap chainsValue =
  Map.fromList (zip chainsValue [0 :: Int ..])
