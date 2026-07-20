{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Sheaf.Site.Analysis.Microsupport
  ( ContextOrderComplex (..),
    ContextOrderComplexProjectionObstruction (..),
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
import Data.List (tails)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
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
    BoundaryEntry,
    BoundaryIncidence,
    FiniteChainComplex,
    HomologicalDegree (..),
    HomologyFailure (..),
    TopologyInputObstruction (..),
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
  let generators =
        stableUniqueAfter
          Set.empty
          (filter (not . contextIsBottom systemValue) (contextGenerators systemValue))
      generatorSet = Set.fromList generators
      pairwiseMeets =
        stableUniqueAfter
          generatorSet
          ( filter
              (not . contextIsBottom systemValue)
              [ meet leftContext rightContext
              | leftContext : remainingContexts <- tails generators,
                rightContext <- remainingContexts
              ]
          )
   in generators <> pairwiseMeets

stableUniqueAfter :: Ord value => Set.Set value -> [value] -> [value]
stableUniqueAfter initialSeen values =
  reverse
    ( snd
        ( List.foldl'
            ( \(seen, uniqueValues) value ->
                if Set.member value seen
                  then (seen, uniqueValues)
                  else (Set.insert value seen, value : uniqueValues)
            )
            (initialSeen, [])
            values
        )
    )

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
          (Left (TopologyInputRejected TopologyObjectAbsent))
          Right
          (Map.lookup obj objToId)
  rawEdges <- fmap concat (traverse (baseMorphismEdge lookupId) baseMors)
  let sccMap = collapseStronglyConnected (length allObjs) rawEdges
      sccLookup nodeId =
        maybe
          (Left (TopologyInputRejected (TopologyStrongComponentAbsent nodeId)))
          Right
          (IntMap.lookup nodeId sccMap)
  collapsedEdges <-
    fmap (filter (uncurry (/=)))
      (traverse (\(sourceNode, targetNode) -> (,) <$> sccLookup sourceNode <*> sccLookup targetNode) rawEdges)
  collapsedNodes <- fmap (IS.toList . IS.fromList) (traverse sccLookup [0 .. length allObjs - 1])
  grothendieckPoset <-
    first (InvalidTopologyInput . show)
      (mkDerivedPosetFromOrderEdges (fmap FinObjectId collapsedNodes) (fmap (\(s, t) -> (FinObjectId s, FinObjectId t)) collapsedEdges))
  let upperSets = derivedPosetUpper grothendieckPoset
      topoRank = topoRankMap grothendieckPoset
  objsByContext <-
    traverse
      ( \ctx ->
          fmap
            (contextOrdinal systemValue ctx,)
            (contextObjectIds lookupId sccLookup allObjs ctx)
      )
      contexts
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
  (GrothendieckOb system -> Either HomologyFailure Int) ->
  GrothendieckMor system ->
  Either HomologyFailure [(Int, Int)]
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
  (GrothendieckOb system -> Either HomologyFailure Int) ->
  (Int -> Either HomologyFailure Int) ->
  [GrothendieckOb system] ->
  SystemCtx system ->
  Either HomologyFailure [Int]
contextObjectIds lookupId sccLookup allObjs contextValue =
  fmap
    (IS.toList . IS.fromList)
    ( traverse
        (\objectValue -> lookupId objectValue >>= sccLookup)
        [objectValue | objectValue <- allObjs, goContext objectValue == contextValue]
    )

checkFiberVanishing :: IntMap Int -> IntMap IS.IntSet -> [Int] -> Either HomologyFailure Bool
checkFiberVanishing topoRank upperSets startIds = do
  startsFormAntichain <- startIdsAreAntichain upperSets startIds
  if startsFormAntichain
    then fmap and (traverse (checkSingleLinkAcyclicity topoRank upperSets) startIds)
    else checkFiberVanishingViaChains topoRank upperSets startIds

checkFiberVanishingViaChains :: IntMap Int -> IntMap IS.IntSet -> [Int] -> Either HomologyFailure Bool
checkFiberVanishingViaChains topoRank upperSets startIds = do
  chainsByDegree <- enumerateChainsFrom topoRank upperSets startIds
  if maxChainDegree chainsByDegree <= 0
    then Right True
    else reducedVanishingOnChains chainsByDegree

checkSingleLinkAcyclicity :: IntMap Int -> IntMap IS.IntSet -> Int -> Either HomologyFailure Bool
checkSingleLinkAcyclicity topoRank upperSets startId = do
  upperSet <- lookupUpperSet upperSets startId
  let linkNodes = IS.delete startId upperSet
  if IS.null linkNodes
    then Right True
    else checkLinkAcyclicOverGF2 topoRank upperSets linkNodes

checkLinkAcyclicOverGF2 :: IntMap Int -> IntMap IS.IntSet -> IS.IntSet -> Either HomologyFailure Bool
checkLinkAcyclicOverGF2 topoRank upperSets linkNodes = do
  restrictedUpperSets <- restrictUpperSets upperSets linkNodes
  chainsByDegree <- enumerateChainsFrom topoRank restrictedUpperSets (IS.toList linkNodes)
  case maxChainDegree chainsByDegree of
    0 -> Right (IS.size linkNodes == 1)
    1 -> Right (isConnectedTree chainsByDegree linkNodes)
    _ -> reducedVanishingOnChains chainsByDegree

reducedVanishingOnChains :: IntMap [[Int]] -> Either HomologyFailure Bool
reducedVanishingOnChains chainsByDegree = do
  (chainComplex, _) <- chainComplexFromChains chainsByDegree
  derivedValue <- first (BackendFailure . show) (derivedFromFiniteChainComplex chainComplex)
  first (BackendFailure . show) (hypercohomologyReducedVanishes derivedValue)

startIdsAreAntichain :: IntMap IS.IntSet -> [Int] -> Either HomologyFailure Bool
startIdsAreAntichain upperSets startIds =
  fmap (all IS.null)
    ( traverse
        ( \startId ->
            fmap
              (IS.intersection (IS.delete startId startSet))
              (lookupUpperSet upperSets startId)
        )
        startIds
    )
  where
    startSet = IS.fromList startIds

enumerateChainsFrom :: IntMap Int -> IntMap IS.IntSet -> [Int] -> Either HomologyFailure (IntMap [[Int]])
enumerateChainsFrom topoRank upperSets startNodes = do
  chains <- fmap concat (traverse (\nodeId -> extendChain nodeId [nodeId]) startNodes)
  pure
    ( IntMap.fromListWith (<>)
        [ (length chainValue - 1, [reverse chainValue])
        | chainValue <- chains
        ]
    )
  where
    topoIndex nodeId =
      maybe
        (Left (TopologyInputRejected (TopologyRankAbsent nodeId)))
        Right
        (IntMap.lookup nodeId topoRank)

    extendChain lastNode reversedChain = do
      lastRank <- topoIndex lastNode
      upperSet <- lookupUpperSet upperSets lastNode
      candidates <-
        fmap catMaybes
          ( traverse
              ( \candidate -> do
                  candidateRank <- topoIndex candidate
                  pure (if candidateRank > lastRank then Just candidate else Nothing)
              )
              (IS.toList (IS.delete lastNode upperSet))
          )
      extensions <- fmap concat (traverse (\candidate -> extendChain candidate (candidate : reversedChain)) candidates)
      pure (reversedChain : extensions)

lookupUpperSet :: IntMap IS.IntSet -> Int -> Either HomologyFailure IS.IntSet
lookupUpperSet upperSets nodeId =
  maybe
    (Left (TopologyInputRejected (TopologyUpperSetAbsent nodeId)))
    Right
    (IntMap.lookup nodeId upperSets)

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
  deriving stock (Eq, Show)

buildContextOrderComplex :: DerivedPoset -> Either HomologyFailure ContextOrderComplex
buildContextOrderComplex contextPoset = do
  let chainsByDegree = enumerateChainsByDegree contextPoset
  (chainComplex, chainIndices) <- chainComplexFromChains chainsByDegree
  let simplexCounts = IntMap.map Map.size chainIndices
  projMap <- projectionMap chainComplex chainIndices
  srcPoset <- orderComplexPoset chainComplex chainIndices
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
  chainIndices <- IntMap.traverseWithKey indexChainsChecked chainsByDegree
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

restrictUpperSets :: IntMap IS.IntSet -> IS.IntSet -> Either HomologyFailure (IntMap IS.IntSet)
restrictUpperSets upperSets nodeSubset =
  fmap IntMap.fromList
    ( traverse
        ( \nodeId ->
            fmap
              (nodeId,)
              (IS.intersection nodeSubset <$> lookupUpperSet upperSets nodeId)
        )
        (IS.toAscList nodeSubset)
    )

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

indexChainsChecked :: Int -> [[Int]] -> Either HomologyFailure (Map [Int] Int)
indexChainsChecked degree chains =
  let indexedChains = Map.fromList (zip chains [0 ..])
   in if Map.size indexedChains == length chains
        then Right indexedChains
        else Left (TopologyInputRejected (TopologyGeneratedChainIndexCollision degree))

degreeIndicesOrEmpty ::
  IntMap (Map [Int] Int) ->
  Int ->
  Either HomologyFailure (Map [Int] Int)
degreeIndicesOrEmpty chainIndices degree
  | IntMap.null chainIndices = Right Map.empty
  | otherwise =
      maybe
        (Left (TopologyInputRejected (TopologyGeneratedDegreeAbsent degree)))
        Right
        (IntMap.lookup degree chainIndices)

simplicialBoundary :: IntMap (Map [Int] Int) -> Int -> Either HomologyFailure (BoundaryIncidence Int)
simplicialBoundary chainIndices degree = do
  sources <- degreeIndicesOrEmpty chainIndices degree
  (targetCount, entries) <-
    if degree <= 0
      then Right (0, [])
      else do
        targets <- degreeIndicesOrEmpty chainIndices (degree - 1)
        boundaryEntries <- concat <$> traverse (chainBoundaryEntries degree targets) (Map.toAscList sources)
        Right (Map.size targets, boundaryEntries)
  first (InvalidBoundaryIncidence . show) $
    mkBoundaryIncidence
      (fromIntegral (Map.size sources))
      (fromIntegral targetCount)
      entries

chainBoundaryEntries ::
  Int ->
  Map [Int] Int ->
  ([Int], Int) ->
  Either HomologyFailure [BoundaryEntry Int]
chainBoundaryEntries degree targets (chain, sourceIndexValue) =
  traverse
    ( \(facePosition, face) ->
        maybe
          (Left (TopologyInputRejected (TopologyGeneratedFaceAbsent degree face)))
          ( \targetIndexValue ->
              Right
                ( mkBoundaryEntry
                    (fromIntegral sourceIndexValue)
                    (fromIntegral targetIndexValue)
                    (if even facePosition then 1 else (-1))
                )
          )
          (Map.lookup face targets)
    )
    (faces chain)

faces :: [Int] -> [(Int, [Int])]
faces =
  zip [0 ..] . fmap (fmap unFinObjectId) . facesOfChain . fmap FinObjectId

projectionMap ::
  FiniteChainComplex Int ->
  IntMap (Map [Int] Int) ->
  Either HomologyFailure (IntMap Int)
projectionMap chainComplex chainIndices =
  IntMap.fromList . concat
    <$> traverse
      ( \(degree, indexedChains) ->
          traverse
            ( \(chain, cellIndex) ->
                case chain of
                  [] -> Left (TopologyInputRejected (TopologyGeneratedEmptyChain degree))
                  startContext : _ ->
                    Right
                      ( basisCellNodeId
                          chainComplex
                          (BasisCellRef (HomologicalDegree degree) cellIndex),
                        startContext
                      )
            )
            (Map.toAscList indexedChains)
      )
      (IntMap.toAscList chainIndices)

orderComplexPoset ::
  FiniteChainComplex Int ->
  IntMap (Map [Int] Int) ->
  Either HomologyFailure DerivedPoset
orderComplexPoset chainComplex chainIndices = do
  covers <- concat <$> traverse coversAtDegree positiveDegreeIndices
  first (InvalidTopologyInput . show) (mkDerivedPosetFromOrderEdges allNodes covers)
  where
    allNodes =
      [ FinObjectId
          (basisCellNodeId chainComplex (BasisCellRef (HomologicalDegree degree) cellIndex))
      | (degree, indexedChains) <- IntMap.toAscList chainIndices,
        cellIndex <- Map.elems indexedChains
      ]

    positiveDegreeIndices =
      filter ((> 0) . fst) (IntMap.toAscList chainIndices)

    coversAtDegree (degree, sourceIndices) = do
      targetIndices <- degreeIndicesOrEmpty chainIndices (degree - 1)
      concat <$> traverse (chainCoverEdges degree targetIndices) (Map.toAscList sourceIndices)

    chainCoverEdges degree targetIndices (chain, sourceIndexValue) =
      traverse
        ( \(_, face) ->
            maybe
              (Left (TopologyInputRejected (TopologyGeneratedFaceAbsent degree face)))
              ( \targetIndexValue ->
                  Right
                    ( FinObjectId
                        ( basisCellNodeId
                            chainComplex
                            (BasisCellRef (HomologicalDegree (degree - 1)) targetIndexValue)
                        ),
                      FinObjectId
                        ( basisCellNodeId
                            chainComplex
                            (BasisCellRef (HomologicalDegree degree) sourceIndexValue)
                        )
                    )
              )
              (Map.lookup face targetIndices)
        )
        (faces chain)
