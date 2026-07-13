{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Site.Analysis.Microsupport
  ( ContextOrderComplex (..),
    ContextOrderComplexProjectionObstruction (..),
    MicrosupportLookupObstruction (..),
    buildContextOrderComplex,
    localMicrosupport,
    localMicrosupportFromGenerators,
    localMicrosupportForContexts,
    localMicrosupportPairwiseMeets,
  )
where

import Algebra.Graph.AdjacencyIntMap qualified as AdjacencyIntMap
import Algebra.Graph.AdjacencyIntMap.Algorithm qualified as AdjacencyIntMapAlgorithm
import Algebra.Graph.AdjacencyMap qualified as AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm qualified as AdjacencyMapAlgorithm
import Algebra.Graph.NonEmpty.AdjacencyMap qualified as NonEmptyAdjacencyMap
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IS
import Data.List (nub)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Vector qualified as V
import Moonlight.Algebra (MeetSemilattice (..))
import Moonlight.Derived.Morse (hypercohomologyReducedVanishes)
import Moonlight.Derived.Morse (MicrosupportResult (..))
import Moonlight.Derived.Site (derivedFromFiniteChainComplex)
import Moonlight.Derived.Site (Criticality (..))
import Moonlight.Derived.Site
  ( DerivedPoset
  , FinObjectId (..)
  , derivedPosetTopoAsc
  , derivedPosetUpper
  , mkDerivedPosetFromOrderEdges
  )
import Moonlight.Derived.Site (facesOfChain, orderComplexChainsByDegree)
import Moonlight.Sheaf.Site.Context.GeneratorCover
  ( ContextGeneratorCover (..),
  )
import Moonlight.Homology
  ( BasisCellRef (..),
    basisCellNodeId,
    BoundaryIncidence,
    FiniteChainComplex,
    HomologicalDegree (..),
    HomologyFailure (..),
    emptyBoundaryIncidenceOf,
    mkBoundaryEntry,
    mkBoundaryIncidence,
    mkFiniteChainComplexChecked,
  )
import Moonlight.Sheaf.Site.Context.Pairs
  ( ContextPairStrategy (ExhaustivePairs),
  )
import Moonlight.Sheaf.Site.Context.Presentation
  ( ContextPresentation,
    ContextPresentationSystem (..),
    contextPresentationWith,
  )
import Moonlight.Sheaf.Site.Grothendieck.Category
  ( GrothendieckMor (..),
    GrothendieckOb (..),
    baseGrothendieckMorphisms,
    grothendieckObjects,
  )
import Moonlight.Sheaf.Site.System (ContextOrdinalSystem (..), LatticeAnalyzableSystem, SystemCtx, allContexts)

collapseStronglyConnected :: Int -> [(Int, Int)] -> IntMap Int
collapseStronglyConnected nodeCount edges =
  strongComponentSets graph
    & concatMap componentMappings
    & IntMap.fromList
  where
    graph =
      AdjacencyMap.overlay
        (AdjacencyMap.vertices [0 .. nodeCount - 1])
        (AdjacencyMap.edges edges)

    componentMappings :: Set.Set Int -> [(Int, Int)]
    componentMappings component =
      case Set.minView component of
        Nothing ->
          []
        Just (representative, rest) ->
          (representative, representative) : fmap (, representative) (Set.toAscList rest)

type MicrosupportLookupObstruction :: Type -> Type
data MicrosupportLookupObstruction system
  = MicrosupportGrothendieckObjectAbsent !(GrothendieckOb system)

microsupportLookupObstructionToHomologyFailure :: MicrosupportLookupObstruction system -> HomologyFailure
microsupportLookupObstructionToHomologyFailure obstruction =
  InvalidTopologyInput
    ( case obstruction of
        MicrosupportGrothendieckObjectAbsent _ ->
          "MicrosupportGrothendieckObjectAbsent"
    )

localMicrosupport ::
  (ContextPresentationSystem system, ContextOrdinalSystem system, LatticeAnalyzableSystem system) =>
  system ->
  Either HomologyFailure MicrosupportResult
localMicrosupport systemValue =
  buildAndCheckMicrosupport
    (systemContextPresentation systemValue)
    (allContexts systemValue)
    systemValue

localMicrosupportFromGenerators ::
  ( ContextGeneratorCover system,
    ContextOrdinalSystem system,
    LatticeAnalyzableSystem system
  ) =>
  system ->
  Either HomologyFailure MicrosupportResult
localMicrosupportFromGenerators systemValue =
  let generators = filter (not . contextIsBottom systemValue) (contextGenerators systemValue)
   in buildAndCheckMicrosupport
        (contextPresentationWith systemValue generators ExhaustivePairs)
        generators
        systemValue

localMicrosupportPairwiseMeets ::
  ( ContextGeneratorCover system,
    ContextOrdinalSystem system,
    LatticeAnalyzableSystem system
  ) =>
  system ->
  Either HomologyFailure MicrosupportResult
localMicrosupportPairwiseMeets systemValue =
  localMicrosupportForContexts systemValue (generatorsWithPairwiseMeets systemValue)

generatorsWithPairwiseMeets :: ContextGeneratorCover system => system -> [SystemCtx system]
generatorsWithPairwiseMeets systemValue =
  let generators = filter (not . contextIsBottom systemValue) (contextGenerators systemValue)
      pairwiseMeets =
        [ meet a b
        | a <- generators,
          b <- generators,
          a /= b
        ]
   in nub (generators ++ filter (not . contextIsBottom systemValue) pairwiseMeets)

localMicrosupportForContexts ::
  (ContextOrdinalSystem system, LatticeAnalyzableSystem system) =>
  system ->
  [SystemCtx system] ->
  Either HomologyFailure MicrosupportResult
localMicrosupportForContexts systemValue contexts =
  buildAndCheckMicrosupport
    (contextPresentationWith systemValue contexts ExhaustivePairs)
    contexts
    systemValue

buildAndCheckMicrosupport ::
  (ContextOrdinalSystem system, LatticeAnalyzableSystem system) =>
  ContextPresentation system ->
  [SystemCtx system] ->
  system ->
  Either HomologyFailure MicrosupportResult
buildAndCheckMicrosupport cf contexts systemValue = do
  let allObjs = grothendieckObjects cf
      baseMors = baseGrothendieckMorphisms cf
      objToId = Map.fromList (zip allObjs [0 :: Int ..])
      lookupId obj =
        maybe
          (Left (MicrosupportGrothendieckObjectAbsent obj))
          Right
          (Map.lookup obj objToId)
  rawEdges <-
    first microsupportLookupObstructionToHomologyFailure
      (fmap concat (traverse (baseMorphismEdge lookupId) baseMors))
  let
      sccMap = collapseStronglyConnected (length allObjs) rawEdges
      sccLookup nodeId = IntMap.findWithDefault nodeId nodeId sccMap
      collapsedEdges =
        rawEdges
          & fmap (\(s, t) -> (sccLookup s, sccLookup t))
          & filter (uncurry (/=))
      collapsedNodes =
        IS.toList (IS.fromList (fmap sccLookup [0 .. length allObjs - 1]))
  grothendieckPoset <-
    first (InvalidTopologyInput . show)
      (mkDerivedPosetFromOrderEdges (fmap FinObjectId collapsedNodes) (fmap (\(s, t) -> (FinObjectId s, FinObjectId t)) collapsedEdges))
  let upperSets = derivedPosetUpper grothendieckPoset
      topoRank = topoRankMap grothendieckPoset
  objsByContext <-
    first microsupportLookupObstructionToHomologyFailure
      ( traverse
          ( \ctx ->
              fmap
                (contextOrdinal systemValue ctx,)
                (contextObjectIds lookupId sccLookup allObjs ctx)
          )
          contexts
      )
  fiberResults <-
    traverse
      ( \(ctxOrd, startIds) ->
          fmap
            (\vanishes -> (FinObjectId ctxOrd, if vanishes then NonCritical else Critical))
            (checkFiberVanishing topoRank upperSets startIds)
      )
      objsByContext
  let criticalCount = length [() | (_, Critical) <- fiberResults]
  pure
    MicrosupportResult
      { mrMicrosupport = [],
        mrCriticalFibers = fiberResults,
        mrCriticalCount = criticalCount,
        mrNoncriticalCount = length fiberResults - criticalCount
      }

baseMorphismEdge ::
  LatticeAnalyzableSystem system =>
  (GrothendieckOb system -> Either (MicrosupportLookupObstruction system) Int) ->
  GrothendieckMor system ->
  Either (MicrosupportLookupObstruction system) [(Int, Int)]
baseMorphismEdge lookupId morphismValue =
  if sourceObject == targetObject
    then Right []
    else
      fmap
        (: [])
        ((,) <$> lookupId sourceObject <*> lookupId targetObject)
  where
    sourceObject =
      GrothendieckOb (gmSourceContext morphismValue) (gmSystem morphismValue) (gmSourceObject morphismValue)

    targetObject =
      GrothendieckOb (gmTargetContext morphismValue) (gmSystem morphismValue) (gmTargetObject morphismValue)

contextObjectIds ::
  Eq (SystemCtx system) =>
  (GrothendieckOb system -> Either (MicrosupportLookupObstruction system) Int) ->
  (Int -> Int) ->
  [GrothendieckOb system] ->
  SystemCtx system ->
  Either (MicrosupportLookupObstruction system) [Int]
contextObjectIds lookupId sccLookup allObjs contextValue =
  fmap
    (IS.toList . IS.fromList)
    ( traverse
        (fmap sccLookup . lookupId)
        [objectValue | objectValue <- allObjs, goContext objectValue == contextValue]
    )

checkFiberVanishing :: IntMap Int -> IntMap IS.IntSet -> [Int] -> Either HomologyFailure Bool
checkFiberVanishing topoRank upperSets startIds =
  if startIdsAreAntichain upperSets startIds
    then fmap and (traverse (checkSingleLinkAcyclicity topoRank upperSets) startIds)
    else checkFiberVanishingViaChains topoRank upperSets startIds

checkFiberVanishingViaChains :: IntMap Int -> IntMap IS.IntSet -> [Int] -> Either HomologyFailure Bool
checkFiberVanishingViaChains topoRank upperSets startIds =
  let chainsByDegree = enumerateChainsFrom topoRank upperSets startIds
   in if maxChainDegree chainsByDegree <= 0
        then Right True
        else reducedVanishingOnChains chainsByDegree

checkSingleLinkAcyclicity :: IntMap Int -> IntMap IS.IntSet -> Int -> Either HomologyFailure Bool
checkSingleLinkAcyclicity topoRank upperSets startId =
  let linkNodes =
        IS.delete startId (IntMap.findWithDefault IS.empty startId upperSets)
   in if IS.null linkNodes
        then Right True
        else checkLinkAcyclicOverGF2 topoRank upperSets linkNodes

checkLinkAcyclicOverGF2 :: IntMap Int -> IntMap IS.IntSet -> IS.IntSet -> Either HomologyFailure Bool
checkLinkAcyclicOverGF2 topoRank upperSets linkNodes =
  let chainsByDegree =
        enumerateChainsFrom
          topoRank
          (restrictUpperSets upperSets linkNodes)
          (IS.toList linkNodes)
   in case maxChainDegree chainsByDegree of
        0 -> Right (IS.size linkNodes == 1)
        1 -> Right (isConnectedTree chainsByDegree linkNodes)
        _ -> reducedVanishingOnChains chainsByDegree

reducedVanishingOnChains :: IntMap [[Int]] -> Either HomologyFailure Bool
reducedVanishingOnChains chainsByDegree = do
  (chainComplex, _) <- chainComplexFromChains chainsByDegree
  derivedValue <- first (BackendFailure . show) (derivedFromFiniteChainComplex chainComplex)
  first (BackendFailure . show) (hypercohomologyReducedVanishes derivedValue)

startIdsAreAntichain :: IntMap IS.IntSet -> [Int] -> Bool
startIdsAreAntichain upperSets startIds =
  let startSet = IS.fromList startIds
      comparableStarts startId =
        IS.intersection
          (IS.delete startId startSet)
          (IntMap.findWithDefault IS.empty startId upperSets)
   in all (IS.null . comparableStarts) startIds

enumerateChainsFrom :: IntMap Int -> IntMap IS.IntSet -> [Int] -> IntMap [[Int]]
enumerateChainsFrom topoRank upperSets startNodes =
  let topoIndex nodeId = IntMap.findWithDefault nodeId nodeId topoRank
      extendChain lastNode revChain =
        let candidates =
              IntMap.findWithDefault IS.empty lastNode upperSets
                & IS.delete lastNode
                & IS.filter (\candidate -> topoIndex candidate > topoIndex lastNode)
                & IS.toList
         in revChain : concatMap (\c -> extendChain c (c : revChain)) candidates
   in IntMap.fromListWith (<>)
        [ (length c - 1, [reverse c])
        | n <- startNodes,
          c <- extendChain n [n]
        ]

type ContextOrderComplex :: Type
data ContextOrderComplex = ContextOrderComplex
  { cocChainComplex :: FiniteChainComplex Int,
    cocSourcePoset :: DerivedPoset,
    cocProjection :: FinObjectId -> Either ContextOrderComplexProjectionObstruction FinObjectId,
    cocSimplexCount :: IntMap Int
  }

type ContextOrderComplexProjectionObstruction :: Type
data ContextOrderComplexProjectionObstruction
  = ContextOrderComplexProjectionAbsent !FinObjectId
  deriving (Eq, Show)

buildContextOrderComplex :: DerivedPoset -> Either HomologyFailure ContextOrderComplex
buildContextOrderComplex contextPoset = do
  let chainsByDegree = enumerateChainsByDegree contextPoset
  (chainComplex, chainIndices) <- chainComplexFromChains chainsByDegree
  let simplexCounts = IntMap.map Map.size chainIndices
      projMap = projectionMap chainComplex chainsByDegree chainIndices
  srcPoset <- orderComplexPoset chainComplex chainsByDegree chainIndices
  pure
    ContextOrderComplex
      { cocChainComplex = chainComplex,
        cocSourcePoset = srcPoset,
        cocProjection = projectContextOrderComplexNode projMap,
        cocSimplexCount = simplexCounts
      }

projectContextOrderComplexNode ::
  IntMap Int ->
  FinObjectId ->
  Either ContextOrderComplexProjectionObstruction FinObjectId
projectContextOrderComplexNode projectionByNode (FinObjectId nodeId) =
  maybe
    (Left (ContextOrderComplexProjectionAbsent (FinObjectId nodeId)))
    (Right . FinObjectId)
    (IntMap.lookup nodeId projectionByNode)

enumerateChainsByDegree :: DerivedPoset -> IntMap [[Int]]
enumerateChainsByDegree =
  IntMap.fromList
    . zip [0 :: Int ..]
    . fmap (fmap (fmap unFinObjectId))
    . V.toList
    . orderComplexChainsByDegree

topoRankMap :: DerivedPoset -> IntMap Int
topoRankMap poset =
  IntMap.fromList
    ( zip
        (fmap unFinObjectId (V.toList (derivedPosetTopoAsc poset)))
        [0 :: Int ..]
    )

maxChainDegree :: IntMap [[Int]] -> Int
maxChainDegree chainsByDegree =
  if IntMap.null chainsByDegree
    then 0
    else fst (IntMap.findMax chainsByDegree)

chainComplexFromChains :: IntMap [[Int]] -> Either HomologyFailure (FiniteChainComplex Int, IntMap (Map [Int] Int))
chainComplexFromChains chainsByDegree = do
  let maxDeg = maxChainDegree chainsByDegree
      chainIndices = IntMap.map indexChains chainsByDegree
  boundaries <-
    traverse
      (\degree -> (degree,) <$> simplicialBoundary chainIndices degree)
      [0 .. maxDeg]
  let boundaryMap = IntMap.fromList boundaries
  chainComplex <-
    mkFiniteChainComplexChecked (HomologicalDegree maxDeg) $
      \(HomologicalDegree k) ->
        IntMap.findWithDefault (emptyBoundaryIncidenceOf 0 0) k boundaryMap
  pure (chainComplex, chainIndices)

restrictUpperSets :: IntMap IS.IntSet -> IS.IntSet -> IntMap IS.IntSet
restrictUpperSets upperSets nodeSubset =
  IS.foldl'
    ( \acc nodeId ->
        IntMap.insert
          nodeId
          (IS.intersection nodeSubset (IntMap.findWithDefault IS.empty nodeId upperSets))
          acc
    )
    IntMap.empty
    nodeSubset

isConnectedTree :: IntMap [[Int]] -> IS.IntSet -> Bool
isConnectedTree chainsByDegree linkNodes =
  let edgeChains = IntMap.findWithDefault [] 1 chainsByDegree
      edgeCount = length edgeChains
      adjacency =
        IntMap.fromListWith IS.union
          ( concatMap
              ( \edgeChain ->
                  case edgeChain of
                    [sourceNode, targetNode] ->
                      [ (sourceNode, IS.singleton targetNode),
                        (targetNode, IS.singleton sourceNode)
                      ]
                    _ -> []
              )
              edgeChains
          )
   in isConnectedBFS adjacency linkNodes
        && edgeCount == IS.size linkNodes - 1

isConnectedBFS :: IntMap IS.IntSet -> IS.IntSet -> Bool
isConnectedBFS adjacency nodes =
  case IS.minView nodes of
    Nothing -> True
    Just (start, _) ->
      reachableIntSetFrom graph start == nodes
  where
    graph =
      AdjacencyIntMap.overlay
        (AdjacencyIntMap.vertices (IS.toAscList nodes))
        (AdjacencyIntMap.fromAdjacencyIntSets (IntMap.toAscList adjacency))

strongComponentSets :: Ord vertex => AdjacencyMap.AdjacencyMap vertex -> [Set.Set vertex]
strongComponentSets graph =
  List.sortOn
    Set.lookupMin
    ( fmap
        (Set.fromList . NonEmpty.toList . NonEmptyAdjacencyMap.vertexList1)
        (AdjacencyMap.vertexList (AdjacencyMapAlgorithm.scc graph))
    )

reachableIntSetFrom :: AdjacencyIntMap.AdjacencyIntMap -> Int -> IS.IntSet
reachableIntSetFrom graph sourceNode =
  IS.insert sourceNode
    . IS.fromList
    $ AdjacencyIntMapAlgorithm.reachable graph sourceNode

indexChains :: [[Int]] -> Map [Int] Int
indexChains chains =
  Map.fromList (zip chains [0 ..])

simplicialBoundary :: IntMap (Map [Int] Int) -> Int -> Either HomologyFailure (BoundaryIncidence Int)
simplicialBoundary chainIndices degree =
  let sourceCount = maybe 0 Map.size (IntMap.lookup degree chainIndices)
      targetCount =
        if degree <= 0
          then 0
          else maybe 0 Map.size (IntMap.lookup (degree - 1) chainIndices)
      entries =
        if degree <= 0
          then []
          else
            case (IntMap.lookup degree chainIndices, IntMap.lookup (degree - 1) chainIndices) of
              (Just sources, Just targets) ->
                Map.toList sources
                  & concatMap
                    ( \(chain, sourceIdx) ->
                        faces chain
                          & concatMap
                            ( \(facePosition, face) ->
                                case Map.lookup face targets of
                                  Just targetIdx ->
                                    [ mkBoundaryEntry
                                        (fromIntegral sourceIdx)
                                        (fromIntegral targetIdx)
                                        (if even facePosition then 1 else (-1))
                                    ]
                                  Nothing -> []
                            )
                    )
              _ -> []
   in first
        (InvalidBoundaryIncidence . show)
        ( mkBoundaryIncidence
            (fromIntegral sourceCount)
            (fromIntegral targetCount)
            entries
        )

faces :: [Int] -> [(Int, [Int])]
faces =
  zip [0 ..] . fmap (fmap unFinObjectId) . facesOfChain . fmap FinObjectId

projectionMap ::
  FiniteChainComplex Int ->
  IntMap [[Int]] ->
  IntMap (Map [Int] Int) ->
  IntMap Int
projectionMap chainComplex chainsByDegree chainIndices =
  IntMap.foldlWithKey'
    ( \acc degree chains ->
        case IntMap.lookup degree chainIndices of
          Nothing -> acc
          Just idxMap ->
            foldl
              ( \innerAcc chain ->
                  case Map.lookup chain idxMap of
                    Nothing -> innerAcc
                    Just cellIdx ->
                      case chain of
                        [] -> innerAcc
                        startContext : _ ->
                          let basisRef = BasisCellRef (HomologicalDegree degree) cellIdx
                              nodeId = basisCellNodeId chainComplex basisRef
                           in IntMap.insert nodeId startContext innerAcc
              )
              acc
              chains
    )
    IntMap.empty
    chainsByDegree

orderComplexPoset ::
  FiniteChainComplex Int ->
  IntMap [[Int]] ->
  IntMap (Map [Int] Int) ->
  Either HomologyFailure DerivedPoset
orderComplexPoset chainComplex chainsByDegree chainIndices =
  let allNodes =
        IntMap.foldlWithKey'
          ( \acc degree _ ->
              case IntMap.lookup degree chainIndices of
                Nothing -> acc
                Just idxMap ->
                  acc
                    ++ fmap
                      ( \cellIdx ->
                          FinObjectId (basisCellNodeId chainComplex (BasisCellRef (HomologicalDegree degree) cellIdx))
                      )
                      [0 .. Map.size idxMap - 1]
          )
          []
          chainsByDegree
      covers =
        IntMap.foldlWithKey'
          ( \acc degree chains ->
              if degree <= 0
                then acc
                else
                  case (IntMap.lookup degree chainIndices, IntMap.lookup (degree - 1) chainIndices) of
                    (Just sourceIdxMap, Just targetIdxMap) ->
                      acc
                        ++ concatMap
                          ( \chain ->
                              case Map.lookup chain sourceIdxMap of
                                Nothing -> []
                                Just sourceIdx ->
                                  let sourceNodeId =
                                        basisCellNodeId
                                          chainComplex
                                          (BasisCellRef (HomologicalDegree degree) sourceIdx)
                                   in concatMap
                                        ( \(_, face) ->
                                            case Map.lookup face targetIdxMap of
                                              Nothing -> []
                                              Just targetIdx ->
                                                let targetNodeId =
                                                      basisCellNodeId
                                                        chainComplex
                                                        (BasisCellRef (HomologicalDegree (degree - 1)) targetIdx)
                                                 in [(FinObjectId targetNodeId, FinObjectId sourceNodeId)]
                                        )
                                        (faces chain)
                          )
                          chains
                    _ -> acc
          )
          []
          chainsByDegree
   in first (InvalidTopologyInput . show) (mkDerivedPosetFromOrderEdges allNodes covers)
