{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Derived.Pure.Site.Gorenstein
  ( isGorensteinStar
  , orderComplexLink
  ) where

import Control.Monad (foldM)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Kind (Type)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Vector qualified as V
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset (..)
  , FinObjectId (..)
  , categoryFromOrderClosure
  , leq
  )
import Moonlight.Derived.Pure.Site.Poset.OrderComplex
  ( PosetChain
  , PreparedOrderComplex
  , facesOfChain
  , isChain
  , prepareOrderComplex
  , restrictOrderComplexChainsTo
  , sortTopo
  , strictLeq
  )
import Moonlight.Derived.Pure.Failure (DerivedFailure (..))
import Moonlight.Homology
  ( FiniteChainComplex
  , HomologicalDegree (..)
  , HomologyFailure (..)
  , freeBettiVector
  , mkBoundaryIncidence
  , mkFiniteChainComplexChecked
  )
import Moonlight.Homology
  ( BoundaryIncidence
  , emptyBoundaryIncidenceOf
  , mkBoundaryEntry
  )

type AugmentedEndpoint :: Type
data AugmentedEndpoint
  = LowerBoundary
  | PosetBoundary !FinObjectId
  | UpperBoundary
  deriving stock (Eq, Ord, Show)

type PreparedGorensteinStar :: Type
data PreparedGorensteinStar = PreparedGorensteinStar
  { pgsPoset :: !DerivedPoset
  , pgsMobiusByInterval :: !(Map.Map (AugmentedEndpoint, AugmentedEndpoint) Int)
  }
  deriving stock (Eq, Show)

type GorensteinInterval :: Type
data GorensteinInterval = GorensteinInterval
  { giEndpoints :: !(AugmentedEndpoint, AugmentedEndpoint)
  , giNodeKeys :: !IS.IntSet
  , giSphereDimension :: !Int
  }
  deriving stock (Eq, Show)

type IntervalTopologyKey :: Type
type IntervalTopologyKey = [[Int]]

emptyPoset :: DerivedPoset
emptyPoset =
  DerivedPoset
    { derivedPosetCategory = categoryFromOrderClosure [] IM.empty
    , derivedPosetNodes = V.empty
    , derivedPosetUpper = IM.empty
    , derivedPosetLower = IM.empty
    , derivedPosetCoversUp = IM.empty
    , derivedPosetTopoDesc = V.empty
    , derivedPosetTopoAsc = V.empty
    }

inducedSubposet :: DerivedPoset -> [FinObjectId] -> DerivedPoset
inducedSubposet posetValue rawNodes =
  let nodeValues = sortTopo posetValue (nub rawNodes)
      nodeKeys = IS.fromList (fmap unFinObjectId nodeValues)
      restrictedUpper = restrictedReachability (derivedPosetUpper posetValue) nodeKeys nodeValues
   in case nodeValues of
        [] -> emptyPoset
        _ ->
          DerivedPoset
            { derivedPosetCategory = categoryFromOrderClosure nodeValues restrictedUpper
            , derivedPosetNodes = V.fromList nodeValues
            , derivedPosetUpper = restrictedUpper
            , derivedPosetLower = restrictedReachability (derivedPosetLower posetValue) nodeKeys nodeValues
            , derivedPosetCoversUp = inducedCovers posetValue nodeKeys nodeValues
            , derivedPosetTopoDesc = V.fromList (reverse nodeValues)
            , derivedPosetTopoAsc = V.fromList nodeValues
            }

restrictedReachability :: IM.IntMap IS.IntSet -> IS.IntSet -> [FinObjectId] -> IM.IntMap IS.IntSet
restrictedReachability relation nodeKeys =
  IM.fromList
    . fmap
      ( \(FinObjectId nodeKey) ->
          ( nodeKey
          , IS.intersection
              nodeKeys
              (IM.findWithDefault (IS.singleton nodeKey) nodeKey relation)
          )
      )

inducedCovers :: DerivedPoset -> IS.IntSet -> [FinObjectId] -> IM.IntMap IS.IntSet
inducedCovers posetValue nodeKeys nodeValues =
  IM.fromList
    [ (sourceKey, IS.fromList (filter (isInducedCover sourceNode) candidateKeys))
    | sourceNode@(FinObjectId sourceKey) <- nodeValues
    , let candidateKeys =
            IS.toList
              ( IS.delete
                  sourceKey
                  ( IS.intersection
                      nodeKeys
                      (IM.findWithDefault IS.empty sourceKey (derivedPosetUpper posetValue))
                  )
              )
    ]
  where
    isInducedCover sourceNode targetKey =
      not
        ( any
            ( \middleNode ->
                middleNode /= sourceNode
                  && middleNode /= FinObjectId targetKey
                  && strictLeq posetValue sourceNode middleNode
                  && strictLeq posetValue middleNode (FinObjectId targetKey)
            )
            nodeValues
        )

orderComplexLink :: DerivedPoset -> [FinObjectId] -> DerivedPoset
orderComplexLink posetValue rawFace =
  let faceNodes = sortTopo posetValue (nub rawFace)
      linkNodes =
        [ nodeValue
        | nodeValue <- V.toList (derivedPosetNodes posetValue)
        , nodeValue `notElem` faceNodes
        , all
            (\anchorNode -> leq posetValue nodeValue anchorNode || leq posetValue anchorNode nodeValue)
            faceNodes
        ]
   in if null faceNodes || isChain posetValue faceNodes
        then inducedSubposet posetValue linkNodes
        else emptyPoset

boundaryAt :: V.Vector [[FinObjectId]] -> Int -> Either HomologyFailure (BoundaryIncidence Integer)
boundaryAt simplicesByDimensionValue dimensionValue
  | dimensionValue <= 0 =
      let zeroSimplices =
            fromMaybe [] (simplicesByDimensionValue V.!? 0)
       in pure (emptyBoundaryIncidenceOf (fromIntegral (length zeroSimplices)) 0)
  | otherwise =
      let sourceSimplices =
            fromMaybe [] (simplicesByDimensionValue V.!? dimensionValue)
          targetSimplices =
            fromMaybe [] (simplicesByDimensionValue V.!? (dimensionValue - 1))
          targetIndexBySimplex =
            Map.fromList (zip targetSimplices [0 :: Int ..])
          entries =
            concat
              [ [ mkBoundaryEntry
                    (fromIntegral sourceIndexValue)
                    (fromIntegral targetIndexValue)
                    ( if even faceIndexValue
                        then 1
                        else -1
                    )
                | (faceIndexValue, faceSimplex) <- zip [0 :: Int ..] (facesOfChain simplexValue)
                , Just targetIndexValue <- [Map.lookup faceSimplex targetIndexBySimplex]
                ]
              | (sourceIndexValue, simplexValue) <- zip [0 :: Int ..] sourceSimplices
              ]
       in firstShapeError
            ( mkBoundaryIncidence
                (fromIntegral (length sourceSimplices))
                (fromIntegral (length targetSimplices))
                entries
            )

orderComplexChainComplexFromSimplices ::
  V.Vector [PosetChain] ->
  Either HomologyFailure (FiniteChainComplex Integer)
orderComplexChainComplexFromSimplices simplices =
  let topDegreeValue = max 0 (V.length simplices - 1)
      boundaryDegrees = [0 .. topDegreeValue]
   in do
        incidences <-
          traverse
            (\degreeValue -> fmap ((,) degreeValue) (boundaryAt simplices degreeValue))
            boundaryDegrees
        let incidenceByDegree = Map.fromList incidences
        mkFiniteChainComplexChecked
          (HomologicalDegree topDegreeValue)
          (\(HomologicalDegree dimensionValue) -> Map.findWithDefault (emptyBoundaryIncidenceOf 0 0) dimensionValue incidenceByDegree)

sphereLikeBetti :: [Int] -> Bool
sphereLikeBetti bettiValues =
  case bettiValues of
    [] -> True
    [2] -> True
    firstValue : restValues ->
      case reverse restValues of
        lastValue : reversedMiddleValues ->
          firstValue == 1
            && lastValue == 1
            && all (== 0) (reverse reversedMiddleValues)
        [] -> False

sphereLikeRestrictedOrderComplex :: PreparedOrderComplex -> IS.IntSet -> Either DerivedFailure Bool
sphereLikeRestrictedOrderComplex preparedOrderComplex nodeKeys
  | IS.null nodeKeys = Right True
  | otherwise =
      either
        (Left . DerivedGorensteinHomologyFailure . show)
        (Right . sphereLikeBetti . freeBettiVector)
        (orderComplexChainComplexFromSimplices (restrictOrderComplexChainsTo nodeKeys preparedOrderComplex))

isGorensteinStar :: DerivedPoset -> Either DerivedFailure Bool
isGorensteinStar =
  preparedGorensteinStarIsGorenstein . prepareGorensteinStar

prepareGorensteinStar :: DerivedPoset -> PreparedGorensteinStar
prepareGorensteinStar posetValue =
  PreparedGorensteinStar
    { pgsPoset = posetValue
    , pgsMobiusByInterval = augmentedMobius posetValue
    }

preparedGorensteinStarIsGorenstein :: PreparedGorensteinStar -> Either DerivedFailure Bool
preparedGorensteinStarIsGorenstein preparedStar =
  let intervalValues = nonemptyGorensteinIntervals preparedStar
   in if all (mobiusMatchesSphere preparedStar) intervalValues
        then intervalTopologyAll preparedStar intervalValues
        else Right False

nonemptyGorensteinIntervals :: PreparedGorensteinStar -> [GorensteinInterval]
nonemptyGorensteinIntervals preparedStar =
  [ GorensteinInterval
      { giEndpoints = intervalEndpoints
      , giNodeKeys = intervalNodeKeys
      , giSphereDimension = chainHeightInNodeSet (pgsPoset preparedStar) intervalNodeKeys - 1
      }
  | intervalEndpoints <- augmentedIntervalPairs (pgsPoset preparedStar)
  , let intervalNodeKeys = openIntervalNodeKeys (pgsPoset preparedStar) intervalEndpoints
  , not (IS.null intervalNodeKeys)
  ]

mobiusMatchesSphere ::
  PreparedGorensteinStar ->
  GorensteinInterval ->
  Bool
mobiusMatchesSphere preparedStar intervalValue =
  Map.findWithDefault 0 (giEndpoints intervalValue) (pgsMobiusByInterval preparedStar)
    == sphereMobius (giSphereDimension intervalValue)

sphereMobius :: Int -> Int
sphereMobius dimensionValue
  | even dimensionValue = 1
  | otherwise = -1

intervalTopologyAll :: PreparedGorensteinStar -> [GorensteinInterval] -> Either DerivedFailure Bool
intervalTopologyAll preparedStar intervalValues =
  case intervalValues of
    [] -> Right True
    _ ->
      let posetValue = pgsPoset preparedStar
          preparedOrderComplex = prepareOrderComplex posetValue
       in fmap
            isJust
            ( foldM
                (insertIntervalTopology posetValue preparedOrderComplex)
                (Just Map.empty)
                intervalValues
            )

insertIntervalTopology ::
  DerivedPoset ->
  PreparedOrderComplex ->
  Maybe (Map.Map IntervalTopologyKey Bool) ->
  GorensteinInterval ->
  Either DerivedFailure (Maybe (Map.Map IntervalTopologyKey Bool))
insertIntervalTopology _ _ Nothing _ = Right Nothing
insertIntervalTopology posetValue preparedOrderComplex (Just topologyCache) intervalValue =
  case intervalTopologyShortcut posetValue (giNodeKeys intervalValue) of
    Just True -> Right (Just topologyCache)
    Just False -> Right Nothing
    Nothing ->
      let topologyKey = intervalTopologyKey posetValue (giNodeKeys intervalValue)
       in case Map.lookup topologyKey topologyCache of
            Just True -> Right (Just topologyCache)
            Just False -> Right Nothing
            Nothing -> do
              sphereLikeValue <-
                sphereLikeRestrictedOrderComplex preparedOrderComplex (giNodeKeys intervalValue)
              Right
                ( if sphereLikeValue
                    then Just (Map.insert topologyKey True topologyCache)
                    else Nothing
                )

intervalTopologyShortcut :: DerivedPoset -> IS.IntSet -> Maybe Bool
intervalTopologyShortcut posetValue nodeKeys
  | IS.null nodeKeys = Just True
  | intervalHasConePoint posetValue nodeKeys = Just False
  | intervalHasNoComparableNodes posetValue nodeKeys = Just (IS.size nodeKeys == 2)
  | otherwise = Nothing

intervalHasConePoint :: DerivedPoset -> IS.IntSet -> Bool
intervalHasConePoint posetValue nodeKeys =
  any
    ( \(FinObjectId nodeKey) ->
        nodeKeys `IS.isSubsetOf` IM.findWithDefault (IS.singleton nodeKey) nodeKey (derivedPosetUpper posetValue)
          || nodeKeys `IS.isSubsetOf` IM.findWithDefault (IS.singleton nodeKey) nodeKey (derivedPosetLower posetValue)
    )
    (nodesInNodeSet posetValue nodeKeys)

intervalHasNoComparableNodes :: DerivedPoset -> IS.IntSet -> Bool
intervalHasNoComparableNodes posetValue nodeKeys =
  not
    ( any
        ( \(FinObjectId nodeKey) ->
            not
              ( IS.null
                  ( IS.delete
                      nodeKey
                      ( IS.intersection
                          nodeKeys
                          (IM.findWithDefault (IS.singleton nodeKey) nodeKey (derivedPosetUpper posetValue))
                      )
                  )
              )
        )
        (nodesInNodeSet posetValue nodeKeys)
    )

intervalTopologyKey :: DerivedPoset -> IS.IntSet -> IntervalTopologyKey
intervalTopologyKey posetValue nodeKeys =
  let intervalNodes = nodesInNodeSet posetValue nodeKeys
      relativeIndexByKey = IM.fromList (zip (fmap unFinObjectId intervalNodes) [0 :: Int ..])
      relativeUpperKeys (FinObjectId nodeKey) =
        [ relativeKey
        | upperKey <-
            IS.toAscList
              ( IS.intersection
                  nodeKeys
                  (IM.findWithDefault (IS.singleton nodeKey) nodeKey (derivedPosetUpper posetValue))
              )
        , Just relativeKey <- [IM.lookup upperKey relativeIndexByKey]
        ]
   in fmap relativeUpperKeys intervalNodes

nodesInNodeSet :: DerivedPoset -> IS.IntSet -> [FinObjectId]
nodesInNodeSet posetValue nodeKeys =
  filter
    (\(FinObjectId nodeKey) -> IS.member nodeKey nodeKeys)
    (V.toList (derivedPosetTopoAsc posetValue))

augmentedEndpoints :: DerivedPoset -> [AugmentedEndpoint]
augmentedEndpoints posetValue =
  [LowerBoundary]
    <> fmap PosetBoundary (V.toList (derivedPosetTopoAsc posetValue))
    <> [UpperBoundary]

augmentedIntervalPairs :: DerivedPoset -> [(AugmentedEndpoint, AugmentedEndpoint)]
augmentedIntervalPairs posetValue =
  [ (lowerEndpoint, upperEndpoint)
  | lowerEndpoint <- endpoints
  , upperEndpoint <- endpoints
  , augmentedStrictLeq posetValue lowerEndpoint upperEndpoint
  ]
  where
    endpoints = augmentedEndpoints posetValue

augmentedStrictLeq :: DerivedPoset -> AugmentedEndpoint -> AugmentedEndpoint -> Bool
augmentedStrictLeq posetValue leftEndpoint rightEndpoint =
  leftEndpoint /= rightEndpoint && augmentedLeq posetValue leftEndpoint rightEndpoint

augmentedLeq :: DerivedPoset -> AugmentedEndpoint -> AugmentedEndpoint -> Bool
augmentedLeq _ LowerBoundary _ = True
augmentedLeq _ _ UpperBoundary = True
augmentedLeq _ UpperBoundary _ = False
augmentedLeq _ _ LowerBoundary = False
augmentedLeq posetValue (PosetBoundary leftNode) (PosetBoundary rightNode) =
  leq posetValue leftNode rightNode

augmentedMobius :: DerivedPoset -> Map.Map (AugmentedEndpoint, AugmentedEndpoint) Int
augmentedMobius posetValue =
  foldl'
    (insertMobiusUpper posetValue)
    Map.empty
    endpoints
  where
    endpoints = augmentedEndpoints posetValue

insertMobiusUpper ::
  DerivedPoset ->
  Map.Map (AugmentedEndpoint, AugmentedEndpoint) Int ->
  AugmentedEndpoint ->
  Map.Map (AugmentedEndpoint, AugmentedEndpoint) Int
insertMobiusUpper posetValue mobiusMap upperEndpoint =
  let strictLowerValues = strictLowerEndpoints posetValue upperEndpoint
   in foldl'
        (insertMobiusInterval posetValue strictLowerValues upperEndpoint)
        (Map.insert (upperEndpoint, upperEndpoint) 1 mobiusMap)
        strictLowerValues

insertMobiusInterval ::
  DerivedPoset ->
  [AugmentedEndpoint] ->
  AugmentedEndpoint ->
  Map.Map (AugmentedEndpoint, AugmentedEndpoint) Int ->
  AugmentedEndpoint ->
  Map.Map (AugmentedEndpoint, AugmentedEndpoint) Int
insertMobiusInterval posetValue strictLowerValues upperEndpoint mobiusMap lowerEndpoint =
  Map.insert
    (lowerEndpoint, upperEndpoint)
    ( negate
        ( sum
            [ Map.findWithDefault 0 (lowerEndpoint, middleEndpoint) mobiusMap
            | middleEndpoint <- strictLowerValues
            , augmentedLeq posetValue lowerEndpoint middleEndpoint
            ]
        )
    )
    mobiusMap

strictLowerEndpoints :: DerivedPoset -> AugmentedEndpoint -> [AugmentedEndpoint]
strictLowerEndpoints posetValue endpointValue =
  case endpointValue of
    LowerBoundary -> []
    PosetBoundary (FinObjectId nodeKey) ->
      LowerBoundary
        : fmap
          PosetBoundary
          ( nodesInNodeSet
              posetValue
              (IS.delete nodeKey (IM.findWithDefault (IS.singleton nodeKey) nodeKey (derivedPosetLower posetValue)))
          )
    UpperBoundary ->
      LowerBoundary : fmap PosetBoundary (V.toList (derivedPosetTopoAsc posetValue))

openIntervalNodeKeys :: DerivedPoset -> (AugmentedEndpoint, AugmentedEndpoint) -> IS.IntSet
openIntervalNodeKeys posetValue (lowerEndpoint, upperEndpoint) =
  IS.intersection
    (upperConeAfterEndpoint posetValue lowerEndpoint)
    (lowerConeBeforeEndpoint posetValue upperEndpoint)

upperConeAfterEndpoint :: DerivedPoset -> AugmentedEndpoint -> IS.IntSet
upperConeAfterEndpoint posetValue endpointValue =
  case endpointValue of
    LowerBoundary -> nodeKeySet posetValue
    PosetBoundary (FinObjectId nodeKey) ->
      IS.delete
        nodeKey
        (IM.findWithDefault (IS.singleton nodeKey) nodeKey (derivedPosetUpper posetValue))
    UpperBoundary -> IS.empty

lowerConeBeforeEndpoint :: DerivedPoset -> AugmentedEndpoint -> IS.IntSet
lowerConeBeforeEndpoint posetValue endpointValue =
  case endpointValue of
    LowerBoundary -> IS.empty
    PosetBoundary (FinObjectId nodeKey) ->
      IS.delete
        nodeKey
        (IM.findWithDefault (IS.singleton nodeKey) nodeKey (derivedPosetLower posetValue))
    UpperBoundary -> nodeKeySet posetValue

nodeKeySet :: DerivedPoset -> IS.IntSet
nodeKeySet =
  IS.fromList . fmap unFinObjectId . V.toList . derivedPosetNodes

chainHeightInNodeSet :: DerivedPoset -> IS.IntSet -> Int
chainHeightInNodeSet posetValue nodeKeys =
  maximum (0 : IM.elems heightByNode)
  where
    intervalNodes =
      filter
        (\(FinObjectId nodeKey) -> IS.member nodeKey nodeKeys)
        (V.toList (derivedPosetTopoAsc posetValue))

    heightByNode =
      foldl' insertHeight IM.empty intervalNodes

    insertHeight heightMap (FinObjectId nodeKey) =
      let lowerKeys =
            IS.delete
              nodeKey
              ( IS.intersection
                  nodeKeys
                  (IM.findWithDefault IS.empty nodeKey (derivedPosetLower posetValue))
              )
          heightValue =
            1
              + maximum
                ( 0
                    : fmap
                      (\lowerKey -> IM.findWithDefault 0 lowerKey heightMap)
                      (IS.toList lowerKeys)
                )
       in IM.insert nodeKey heightValue heightMap

firstShapeError :: Show errorValue => Either errorValue boundary -> Either HomologyFailure boundary
firstShapeError =
  either (Left . InvalidBoundaryIncidence . show) Right
