{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Section.Stalk.Groupoid
  ( InterfaceStalkGroupoid,
    mkInterfaceStalkGroupoid,
    fromDiscreteStalk,
    interfaceStalkObjects,
    interfaceStalkAutomorphismCounts,
    maxInterfaceStalkAutomorphismCount,
    orbitRepresentatives,
    orbitsWithSize,
  )
where

import Algebra.Graph.AdjacencyIntMap (AdjacencyIntMap)
import Algebra.Graph.AdjacencyIntMap qualified as AdjacencyIntMap
import Algebra.Graph.AdjacencyIntMap.Algorithm qualified as AdjacencyIntMapAlgorithm
import Algebra.Graph.AdjacencyMap qualified as AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm qualified as AdjacencyMapAlgorithm
import Algebra.Graph.NonEmpty.AdjacencyMap qualified as NonEmptyAdjacencyMap
import Control.Category ((>>>))
import Data.Function ((&))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Moonlight.Category
  ( Category (..),
    CoreGroupoid,
    FiniteComposableCategory (..),
    automorphismGroupAt,
    automorphismGroupoid,
    automorphismGroupoidObjects,
    coreGroupoid,
    coreGroupoidMorphisms,
    coreGroupoidObjects,
    forgetAutomorphismGroupoidObject,
    forgetCoreGroupoidMorphism,
    forgetCoreGroupoidObject,
  )

type InterfaceStalkGroupoid :: Type
data InterfaceStalkGroupoid = InterfaceStalkGroupoid
  { rsgObjects :: !IntSet,
    rsgAutomorphismCounts :: !(IntMap Int),
    rsgMorphisms :: !(IntMap [(Int, Int)])
  }
  deriving stock (Eq, Show)

type LocalOrbitCategory :: Type
data LocalOrbitCategory = LocalOrbitCategory
  { locObjects :: ![Int],
    locReachablePairs :: ![(Int, Int)]
  }

type LocalOrbitObject :: Type
newtype LocalOrbitObject = LocalOrbitObject
  { unLocalOrbitObject :: Int
  }
  deriving stock (Eq, Ord, Show)

type LocalOrbitMorphism :: Type
data LocalOrbitMorphism = LocalOrbitMorphism
  { lomSource :: !Int,
    lomTarget :: !Int
  }
  deriving stock (Eq, Ord, Show)

type LocalOrbitTwoMorphism :: Type
data LocalOrbitTwoMorphism = LocalOrbitTwoMorphism
  deriving stock (Eq, Ord, Show)

type LocalOrbitCompositor :: Type
data LocalOrbitCompositor = LocalOrbitCompositor
  deriving stock (Eq, Ord, Show)

instance Category LocalOrbitCategory where
  type Ob LocalOrbitCategory = LocalOrbitObject
  type Mor LocalOrbitCategory = LocalOrbitMorphism
  type TwoMor LocalOrbitCategory = LocalOrbitTwoMorphism
  type Compositor LocalOrbitCategory = LocalOrbitCompositor

  identity _ (LocalOrbitObject objectValue) =
    Right (LocalOrbitMorphism objectValue objectValue)

  compose _ leftMorphism rightMorphism
    | lomTarget rightMorphism == lomSource leftMorphism =
        Right
          ( LocalOrbitMorphism
              { lomSource = lomSource rightMorphism,
                lomTarget = lomTarget leftMorphism
              },
            LocalOrbitCompositor
          )
    | otherwise =
        Left ()

  source _ = Right . LocalOrbitObject . lomSource
  target _ = Right . LocalOrbitObject . lomTarget

instance FiniteComposableCategory LocalOrbitCategory where
  enumerateObjects = fmap LocalOrbitObject . locObjects
  enumerateMorphisms = fmap (uncurry LocalOrbitMorphism) . locReachablePairs
  enumerateMorphismsFrom localOrbitCategoryValue (LocalOrbitObject sourceObject) =
    locReachablePairs localOrbitCategoryValue
      & foldMap
        (\(candidateSource, candidateTarget) ->
          [ LocalOrbitMorphism candidateSource candidateTarget
          | candidateSource == sourceObject
          ]
        )

mkInterfaceStalkGroupoid :: IntSet -> IntMap Int -> IntMap [(Int, Int)] -> InterfaceStalkGroupoid
mkInterfaceStalkGroupoid objectSet automorphismCounts morphismCounts =
  let provisional =
        InterfaceStalkGroupoid
          { rsgObjects = normalizedObjectUniverse objectSet automorphismCounts morphismCounts,
            rsgAutomorphismCounts = positiveAutomorphismCounts automorphismCounts,
            rsgMorphisms = morphismCounts
          }
   in provisional
        { rsgAutomorphismCounts =
            IntMap.unionWith
              max
              (rsgAutomorphismCounts provisional)
              (inferredIdentityCounts provisional)
        }

normalizedObjectUniverse :: IntSet -> IntMap Int -> IntMap [(Int, Int)] -> IntSet
normalizedObjectUniverse objectSet automorphismCounts morphismCounts =
  let edgeSources = IntMap.keysSet morphismCounts
      edgeTargets =
        morphismCounts
          & IntMap.elems
          & concatMap (fmap fst)
          & IntSet.fromList
      automorphismObjects = IntMap.keysSet (positiveAutomorphismCounts automorphismCounts)
   in IntSet.unions [objectSet, edgeSources, edgeTargets, automorphismObjects]

fromDiscreteStalk :: IntSet -> InterfaceStalkGroupoid
fromDiscreteStalk objectSet =
  mkInterfaceStalkGroupoid
    objectSet
    (IntMap.fromSet (const 1) objectSet)
    IntMap.empty

interfaceStalkObjects :: InterfaceStalkGroupoid -> IntSet
interfaceStalkObjects =
  rsgObjects

interfaceStalkAutomorphismCounts :: InterfaceStalkGroupoid -> IntMap Int
interfaceStalkAutomorphismCounts =
  rsgAutomorphismCounts

maxInterfaceStalkAutomorphismCount :: InterfaceStalkGroupoid -> Int
maxInterfaceStalkAutomorphismCount =
  IntMap.foldr max 1 . interfaceStalkAutomorphismCounts

orbitRepresentatives :: InterfaceStalkGroupoid -> IntSet
orbitRepresentatives groupoidValue =
  orbitsWithSize groupoidValue
    & fmap fst
    & IntSet.fromList

orbitsWithSize :: InterfaceStalkGroupoid -> [(Int, Int)]
orbitsWithSize groupoidValue =
  localCoreComponents groupoidValue
    & mapMaybe componentSummary

componentSummary :: IntSet -> Maybe (Int, Int)
componentSummary componentValue =
  fmap
    (\representative -> (representative, IntSet.size componentValue))
    (IntSet.lookupMin componentValue)

localCoreComponents :: InterfaceStalkGroupoid -> [IntSet]
localCoreComponents groupoidValue =
  let coreValue = coreGroupoid (localOrbitCategory groupoidValue)
      adjacency = coreAdjacency coreValue
   in componentsFromAdjacency adjacency (rsgObjects groupoidValue)

localOrbitCategory :: InterfaceStalkGroupoid -> LocalOrbitCategory
localOrbitCategory groupoidValue =
  LocalOrbitCategory
    { locObjects = IntSet.toAscList (rsgObjects groupoidValue),
      locReachablePairs = reachablePairs groupoidValue
    }

reachablePairs :: InterfaceStalkGroupoid -> [(Int, Int)]
reachablePairs groupoidValue =
  let graph = groupoidAdjacencyGraph groupoidValue
   in rsgObjects groupoidValue
        & IntSet.toAscList
        & concatMap
          (\sourceObject ->
            reachableIntSetFrom graph sourceObject
              & IntSet.toAscList
              & fmap (sourceObject,)
          )

groupoidAdjacencyGraph :: InterfaceStalkGroupoid -> AdjacencyIntMap
groupoidAdjacencyGraph groupoidValue =
  AdjacencyIntMap.overlay
    (AdjacencyIntMap.vertices (IntSet.toAscList (rsgObjects groupoidValue)))
    (AdjacencyIntMap.fromAdjacencyIntSets (IntMap.toAscList (adjacencyMap groupoidValue)))

adjacencyMap :: InterfaceStalkGroupoid -> IntMap IntSet
adjacencyMap groupoidValue =
  let emptyBuckets =
        rsgObjects groupoidValue
          & IntSet.toAscList
          & fmap (, IntSet.empty)
          & IntMap.fromList
   in rsgMorphisms groupoidValue
        & IntMap.foldrWithKey
          (\sourceObject targetsValue ->
            IntMap.insertWith
              IntSet.union
              sourceObject
              ( targetsValue
                  & filter ((> 0) . snd)
                  & fmap fst
                  & IntSet.fromList
              )
          )
          emptyBuckets

positiveAutomorphismCounts :: IntMap Int -> IntMap Int
positiveAutomorphismCounts =
  IntMap.filter (> 0)

inferredIdentityCounts :: InterfaceStalkGroupoid -> IntMap Int
inferredIdentityCounts groupoidValue =
  let automorphismValue = automorphismGroupoid (localOrbitCategory groupoidValue)
   in automorphismGroupoidObjects automorphismValue
        & fmap
          (\objectValue ->
            ( unLocalOrbitObject (forgetAutomorphismGroupoidObject objectValue),
              max 1 (length (automorphismGroupAt automorphismValue objectValue))
            )
          )
        & IntMap.fromList

coreAdjacency :: CoreGroupoid LocalOrbitCategory -> IntMap IntSet
coreAdjacency coreValue =
  let objectBuckets =
        fmap ((, IntSet.empty) . (forgetCoreGroupoidObject >>> unLocalOrbitObject)) (coreGroupoidObjects coreValue)
          & IntMap.fromList
   in coreGroupoidMorphisms coreValue
        & foldr
          (\morphismValue ->
            let LocalOrbitMorphism {..} = forgetCoreGroupoidMorphism morphismValue
             in IntMap.insertWith IntSet.union lomSource (IntSet.singleton lomTarget)
                  . IntMap.insertWith IntSet.union lomTarget (IntSet.singleton lomSource)
          )
          objectBuckets

componentsFromAdjacency :: IntMap IntSet -> IntSet -> [IntSet]
componentsFromAdjacency adjacency objects =
  fmap setToIntSet $
    strongComponentSets
      ( AdjacencyMap.symmetricClosure
          ( AdjacencyMap.overlay
              (AdjacencyMap.vertices (IntSet.toAscList objects))
              (AdjacencyMap.fromAdjacencySets (IntMap.toAscList (fmap intSetToSet adjacency)))
          )
      )

reachableIntSetFrom :: AdjacencyIntMap -> Int -> IntSet
reachableIntSetFrom graph source =
  IntSet.insert source
    . IntSet.fromList
    $ AdjacencyIntMapAlgorithm.reachable graph source

strongComponentSets :: Ord vertex => AdjacencyMap.AdjacencyMap vertex -> [Set.Set vertex]
strongComponentSets graph =
  List.sortOn
    Set.lookupMin
    ( fmap
        (Set.fromList . NonEmpty.toList . NonEmptyAdjacencyMap.vertexList1)
        (AdjacencyMap.vertexList (AdjacencyMapAlgorithm.scc graph))
    )

intSetToSet :: IntSet -> Set.Set Int
intSetToSet =
  Set.fromAscList . IntSet.toAscList

setToIntSet :: Set.Set Int -> IntSet
setToIntSet =
  IntSet.fromAscList . Set.toAscList
