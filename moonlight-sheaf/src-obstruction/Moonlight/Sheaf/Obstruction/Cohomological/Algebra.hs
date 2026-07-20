module Moonlight.Sheaf.Obstruction.Cohomological.Algebra
  ( buildOneLayer,
    buildCycleLayer,
    nerveFromAdjacency,
    nerveFromSupport,
    cycleReportFromEdgeAssignment,
    holonomyCoverCohomologyReport,
    orientedCycleEdges,
    tupleWitnessCoverCohomologyReport,
    assignmentWitnessCoverCohomologyReport,
    boundaryAt,
    rankGapLowerBound,
    rankUpperBoundary2,
    supportCellsFromBasis,
    supportCellsFromRepresentatives,
  )
where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as AdjacencyMap
import Data.Kind (Type)
import Data.Containers.ListUtils (nubOrd)
import Data.Function ((&))
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Homology
  ( BoundaryIncidence,
    RepresentativeCocycle,
    representativeTerms,
    FiniteChainComplex,
    HomologicalDegree (..),
    emptyBoundaryIncidenceOf,
    incidenceMatrixAt,
    mapBoundaryCoefficients,
    sourceCardinality,
    targetCardinality,
    transposeBoundaryIncidence,
  )
import Moonlight.Sheaf.Obstruction.Cohomological.Types
  ( C1LocalConflict (..),
    CoverCohomologyReport (..),
    CoverNerve (..),
    CycleCohomologyReport (..),
    CycleDescriptor (..),
    CycleExactness (..),
    CycleId (..),
    ExactConstraint (..),
    ExactLabelCode (..),
    ExpandedMorphism (..),
    ExpandedObstructionCell (..),
    H1Class (..),
    Nerve1Cochain (..),
    NerveEdge (..),
    NervePotential (..),
    ObstructionCell (..),
    OrientedNerveEdge (..),
    AssignmentWitnessBasis,
    AssignmentWitnessCoverCohomologyReport,
    AnalysisCompleteness (..),
    AnalysisTruncationCause (..),
    TupleWitnessBasis,
    TupleWitnessCoverCohomologyReport,
    WitnessStalk,
    assignmentWitnessFromDescent,
    edgeAssignmentWitnessFromDescent,
    edgeTupleWitnessFromDescent,
    tupleWitnessFromDescent,
    witnessIsZero,
    witnessMagnitude,
  )
import Moonlight.Sheaf.Descent.Assignment qualified as AssignmentDescent
import Moonlight.Sheaf.Descent.Kernel
  ( CoverSearchCost (cscAssignmentUpperBound),
    CoverSearchRefusal (..),
  )
import Moonlight.Sheaf.Descent.Quotient qualified as QuotientDescent
import Moonlight.Sheaf.Operator.GradedComplex
import Moonlight.Sheaf.Operator.BuildError
  ( SheafOperatorBuildError,
  )
import Moonlight.Sheaf.Kernel.Basis
  ( SheafBasis,
    basisCells,
    basisIndexedCells,
    basisCardinality,
  )

buildOneLayer ::
  (anchor -> ExactLabelCode -> ExpandedObstructionCell) ->
  [ExactConstraint anchor] ->
  ([ExpandedObstructionCell], [ExpandedMorphism], Map (ExpandedObstructionCell, ExpandedObstructionCell) Int)
buildOneLayer zeroCellFor constraints =
  let (cells, morphisms, orientationEntries) =
        foldMap (oneLayerForConstraint zeroCellFor) constraints
   in (cells, morphisms, orientationMapFromEntries orientationEntries)

oneLayerForConstraint ::
  (anchor -> ExactLabelCode -> ExpandedObstructionCell) ->
  ExactConstraint anchor ->
  ([ExpandedObstructionCell], [ExpandedMorphism], [((ExpandedObstructionCell, ExpandedObstructionCell), Int)])
oneLayerForConstraint zeroCellFor constraintValue =
  case constraintValue of
    EqualityConstraint constraintId leftAnchor rightAnchor supportDomain ->
      classConstraintLayer ExpandedEqualityConstraintCell constraintId leftAnchor rightAnchor supportDomain
    GuardConstraint constraintId leftAnchor rightAnchor supportDomain ->
      classConstraintLayer ExpandedGuardConstraintCell constraintId leftAnchor rightAnchor supportDomain
    RelationConstraint relationFlavor constraintId anchorValues supportTuples ->
      tupleConstraintLayer (ExpandedRelationConstraintCell relationFlavor) constraintId anchorValues supportTuples
  where
    classConstraintLayer cellConstructor constraintId leftAnchor rightAnchor supportDomain =
      (cells, morphisms, orientationEntries)
      where
        classKeys =
          IntSet.toAscList supportDomain

        cellFor classKey =
          cellConstructor constraintId (ClassLabelCode classKey)

        morphismsFor classKey =
          let classCode = ClassLabelCode classKey
              constraintCell = cellConstructor constraintId classCode
           in [ ExpandedMorphism constraintCell (zeroCellFor leftAnchor classCode),
                ExpandedMorphism constraintCell (zeroCellFor rightAnchor classCode)
              ]

        orientationEntriesFor classKey =
          let classCode = ClassLabelCode classKey
              constraintCell = cellConstructor constraintId classCode
           in [ ((constraintCell, zeroCellFor leftAnchor classCode), 1),
                ((constraintCell, zeroCellFor rightAnchor classCode), -1)
              ]

        cells =
          fmap cellFor classKeys

        morphisms =
          concatMap morphismsFor classKeys

        orientationEntries =
          concatMap orientationEntriesFor classKeys
    tupleConstraintLayer cellConstructor constraintId anchorValues supportTuples =
      (cells, morphisms, orientationEntries)
      where
        cellFor supportTuple =
          cellConstructor constraintId (TupleLabelCode supportTuple)

        targetCellsFor supportTuple =
          zipWith zeroCellFor anchorValues supportTuple
            & nubOrd

        cells =
          fmap cellFor supportTuples

        morphisms =
          concatMap
            (\supportTuple ->
               fmap
                 (ExpandedMorphism (cellFor supportTuple))
                 (targetCellsFor supportTuple)
            )
            supportTuples

        orientationEntries =
          concatMap
            ( \supportTuple ->
                let constraintCell = cellFor supportTuple
                 in fmap
                      (\(targetCell, signValue) -> ((constraintCell, targetCell), signValue))
                      (alternatingSigns (targetCellsFor supportTuple))
            )
            supportTuples

buildCycleLayer ::
  [ExpandedMorphism] ->
  ([ExpandedObstructionCell], [ExpandedMorphism], Map (ExpandedObstructionCell, ExpandedObstructionCell) Int)
buildCycleLayer morphisms01 =
  let adjacencyMap = adjacencyFromMorphisms morphisms01
      cycleDescriptors =
        zipWith
          (\cycleIndex cycleVertices ->
             CycleDescriptor (CycleId cycleIndex) (filter isOneCell cycleVertices)
          )
          [0 :: Int ..]
          (fundamentalCycleVertexLists adjacencyMap)
          & filter (not . null . ccdBoundary)
      cycleCells = fmap (ExpandedCycleCell . ccdId) cycleDescriptors
      cycleMorphisms =
        foldMap
          (\descriptor ->
             fmap (ExpandedMorphism (ExpandedCycleCell (ccdId descriptor))) (ccdBoundary descriptor)
          )
          cycleDescriptors
      orientationMap =
        foldMap
          (\descriptor ->
             alternatingSigns (ccdBoundary descriptor)
               & fmap
                 (\(oneCell, signValue) -> ((ExpandedCycleCell (ccdId descriptor), oneCell), signValue))
          )
          cycleDescriptors
          & orientationMapFromEntries
   in (cycleCells, cycleMorphisms, orientationMap)

adjacencyFromMorphisms ::
  [ExpandedMorphism] ->
  AdjacencyMap ExpandedObstructionCell
adjacencyFromMorphisms =
  AdjacencyMap.edges
    . foldMap
      ( \morphismValue ->
          let sourceCell = emSource morphismValue
              targetCell = emTarget morphismValue
           in [(sourceCell, targetCell), (targetCell, sourceCell)]
      )

type WitnessContributionAlgebra :: Type -> Type -> Type -> Type
data WitnessContributionAlgebra ctx obstruction basis = WitnessContributionAlgebra
  { wcaLocalContribution :: obstruction -> WitnessStalk basis,
    wcaEdgeContribution :: OrientedNerveEdge ctx -> obstruction -> WitnessStalk basis
  }

type EdgeContributionIndex :: Type -> Type -> Type
type EdgeContributionIndex ctx basis = Map (OrientedNerveEdge ctx) [(ctx, WitnessStalk basis)]

tupleWitnessContributionAlgebra ::
  (Ord ctx, Ord rep) =>
  WitnessContributionAlgebra
    ctx
    (QuotientDescent.QuotientDescentObstruction ctx rep)
    (TupleWitnessBasis ctx rep)
tupleWitnessContributionAlgebra =
  WitnessContributionAlgebra
    { wcaLocalContribution = tupleWitnessFromDescent,
      wcaEdgeContribution = edgeTupleWitnessFromDescent
    }

assignmentWitnessContributionAlgebra ::
  (Ord ctx, Ord coord, Ord value) =>
  WitnessContributionAlgebra
    ctx
    (AssignmentDescent.AssignmentDescentObstruction ctx coord value admissibilityWitness admissibilityCost)
    (AssignmentWitnessBasis ctx coord value)
assignmentWitnessContributionAlgebra =
  WitnessContributionAlgebra
    { wcaLocalContribution = assignmentWitnessFromDescent,
      wcaEdgeContribution = edgeAssignmentWitnessFromDescent
    }

nerveFromAdjacency ::
  Ord ctx =>
  [ctx] ->
  (ctx -> Set ctx) ->
  CoverNerve ctx
nerveFromAdjacency contexts neighbors =
  CoverNerve
    { cnVertices = orderedVertices,
      cnEdges = edgeSet,
      cnAdjacency = adjacencyMap,
      cnFundamentalCycles =
        mapMaybe
          NonEmpty.nonEmpty
          (fundamentalCycleVertexLists (AdjacencyMap.fromAdjacencySets (Map.toAscList adjacencyMap)))
    }
  where
    orderedVertices =
      nubOrd contexts

    vertexSet =
      Set.fromList orderedVertices

    adjacencyMap =
      List.foldl'
        insertNeighbors
        (Map.fromList [(contextValue, Set.empty) | contextValue <- orderedVertices])
        orderedVertices

    insertNeighbors accumulatedAdjacency contextValue =
      let visibleNeighbors =
            Set.filter
              (\neighborValue -> neighborValue /= contextValue && Set.member neighborValue vertexSet)
              (neighbors contextValue)
          withForward =
            Map.insertWith Set.union contextValue visibleNeighbors accumulatedAdjacency
       in Set.foldl'
            (\adjacencyValue neighborValue ->
               Map.insertWith Set.union neighborValue (Set.singleton contextValue) adjacencyValue
            )
            withForward
            visibleNeighbors

    edgeSet =
      Set.fromList
        [ let (leftContext, rightContext) =
                normalizeNerveEdge contextValue neighborValue
           in NerveEdge leftContext rightContext
        | (contextValue, neighborValues) <- Map.toList adjacencyMap,
          neighborValue <- Set.toAscList neighborValues
        ]

nerveFromSupport ::
  (Ord ctx, Ord atom) =>
  [ctx] ->
  (ctx -> Set atom) ->
  CoverNerve ctx
nerveFromSupport contexts supportAt =
  nerveFromAdjacency
    orderedContexts
    (\contextValue -> Map.findWithDefault Set.empty contextValue supportAdjacency)
  where
    orderedContexts =
      nubOrd contexts

    contextSupports =
      fmap (\contextValue -> (contextValue, supportAt contextValue)) orderedContexts

    atomContexts =
      Map.fromListWith
        Set.union
        [ (atomValue, Set.singleton contextValue)
        | (contextValue, support) <- contextSupports,
          atomValue <- Set.toAscList support
        ]

    supportAdjacency =
      Map.unionsWith Set.union (fmap cooccurrenceAdjacency (Map.elems atomContexts))

cooccurrenceAdjacency :: Ord ctx => Set ctx -> Map ctx (Set ctx)
cooccurrenceAdjacency contextBucket =
  Map.fromList
    [ (contextValue, Set.delete contextValue contextBucket)
    | contextValue <- Set.toAscList contextBucket
    ]

normalizeNerveEdge ::
  Ord vertex =>
  vertex ->
  vertex ->
  (vertex, vertex)
normalizeNerveEdge leftVertex rightVertex =
  if leftVertex <= rightVertex
    then (leftVertex, rightVertex)
    else (rightVertex, leftVertex)

fundamentalCycleVertexLists :: Ord vertex => AdjacencyMap vertex -> [[vertex]]
fundamentalCycleVertexLists adjacency =
  let undirectedEdges =
        Set.toAscList
          ( Set.fromList
              (fmap (uncurry normalizeNerveEdge) (AdjacencyMap.edgeList adjacency))
          )
      spanningForest =
        buildSpanningForest undirectedEdges
      forestIndex =
        forestIndexFromAdjacency undirectedEdges (sfTreeAdjacency spanningForest)
   in mapMaybe (uncurry (pathInForest forestIndex)) (sfSkippedEdges spanningForest)

type UnionFind :: Type -> Type
data UnionFind vertex = UnionFind
  { ufParents :: !(Map vertex vertex),
    ufRanks :: !(Map vertex Int)
  }

type SpanningForest :: Type -> Type
data SpanningForest vertex = SpanningForest
  { sfUnionFind :: !(UnionFind vertex),
    sfTreeAdjacency :: !(Map vertex (Set vertex)),
    sfSkippedEdges :: ![(vertex, vertex)]
  }

type ForestIndex :: Type -> Type
data ForestIndex vertex = ForestIndex
  { fiParents :: !(Map vertex vertex),
    fiDepths :: !(Map vertex Int)
  }

emptyUnionFind :: UnionFind vertex
emptyUnionFind =
  UnionFind
    { ufParents = Map.empty,
      ufRanks = Map.empty
    }

buildSpanningForest :: Ord vertex => [(vertex, vertex)] -> SpanningForest vertex
buildSpanningForest =
  List.foldl' insertSpanningForestEdge initialSpanningForest
  where
    initialSpanningForest =
      SpanningForest
        { sfUnionFind = emptyUnionFind,
          sfTreeAdjacency = Map.empty,
          sfSkippedEdges = []
        }

insertSpanningForestEdge :: Ord vertex => SpanningForest vertex -> (vertex, vertex) -> SpanningForest vertex
insertSpanningForestEdge spanningForest (leftVertex, rightVertex) =
  let (alreadyConnected, connectedUnionFind) =
        unionFindConnected leftVertex rightVertex (sfUnionFind spanningForest)
   in if alreadyConnected
        then
          spanningForest
            { sfUnionFind = connectedUnionFind,
              sfSkippedEdges = (leftVertex, rightVertex) : sfSkippedEdges spanningForest
            }
        else
          spanningForest
            { sfUnionFind = unionFindUnion leftVertex rightVertex connectedUnionFind,
              sfTreeAdjacency = insertUndirectedTreeEdge leftVertex rightVertex (sfTreeAdjacency spanningForest)
            }

insertUndirectedTreeEdge :: Ord vertex => vertex -> vertex -> Map vertex (Set vertex) -> Map vertex (Set vertex)
insertUndirectedTreeEdge leftVertex rightVertex =
  Map.insertWith Set.union leftVertex (Set.singleton rightVertex)
    . Map.insertWith Set.union rightVertex (Set.singleton leftVertex)

unionFindConnected :: Ord vertex => vertex -> vertex -> UnionFind vertex -> (Bool, UnionFind vertex)
unionFindConnected leftVertex rightVertex unionFind =
  let (leftRoot, leftCompressed) =
        unionFindRoot leftVertex unionFind
      (rightRoot, rightCompressed) =
        unionFindRoot rightVertex leftCompressed
   in (leftRoot == rightRoot, rightCompressed)

unionFindUnion :: Ord vertex => vertex -> vertex -> UnionFind vertex -> UnionFind vertex
unionFindUnion leftVertex rightVertex unionFind =
  let (leftRoot, leftCompressed) =
        unionFindRoot leftVertex unionFind
      (rightRoot, rightCompressed) =
        unionFindRoot rightVertex leftCompressed
   in if leftRoot == rightRoot
        then rightCompressed
        else unionFindLinkRoots leftRoot rightRoot rightCompressed

unionFindRoot :: Ord vertex => vertex -> UnionFind vertex -> (vertex, UnionFind vertex)
unionFindRoot vertexValue unionFind =
  case Map.lookup vertexValue (ufParents unionFind) of
    Nothing ->
      ( vertexValue,
        unionFind
          { ufParents = Map.insert vertexValue vertexValue (ufParents unionFind),
            ufRanks = Map.insert vertexValue 0 (ufRanks unionFind)
          }
      )
    Just parentVertex
      | parentVertex == vertexValue ->
          (vertexValue, unionFind)
      | otherwise ->
          let (rootVertex, compressedUnionFind) =
                unionFindRoot parentVertex unionFind
           in ( rootVertex,
                compressedUnionFind
                  { ufParents = Map.insert vertexValue rootVertex (ufParents compressedUnionFind)
                  }
              )

unionFindLinkRoots :: Ord vertex => vertex -> vertex -> UnionFind vertex -> UnionFind vertex
unionFindLinkRoots leftRoot rightRoot unionFind =
  case compare leftRank rightRank of
    LT ->
      unionFind
        { ufParents = Map.insert leftRoot rightRoot (ufParents unionFind)
        }
    GT ->
      unionFind
        { ufParents = Map.insert rightRoot leftRoot (ufParents unionFind)
        }
    EQ ->
      unionFind
        { ufParents = Map.insert rightRoot leftRoot (ufParents unionFind),
          ufRanks = Map.insert leftRoot (leftRank + 1) (ufRanks unionFind)
        }
  where
    leftRank =
      Map.findWithDefault 0 leftRoot (ufRanks unionFind)

    rightRank =
      Map.findWithDefault 0 rightRoot (ufRanks unionFind)

forestIndexFromAdjacency :: Ord vertex => [(vertex, vertex)] -> Map vertex (Set vertex) -> ForestIndex vertex
forestIndexFromAdjacency undirectedEdges treeAdjacency =
  snd
    ( List.foldl'
        indexRoot
        (Set.empty, ForestIndex {fiParents = Map.empty, fiDepths = Map.empty})
        forestVertices
    )
  where
    forestVertices =
      Set.toAscList
        ( Set.fromList
            [ vertexValue
            | (leftVertex, rightVertex) <- undirectedEdges,
              vertexValue <- [leftVertex, rightVertex]
            ]
        )

    indexRoot indexedForest vertexValue =
      if Set.member vertexValue (fst indexedForest)
        then indexedForest
        else indexForestFrom treeAdjacency Nothing 0 vertexValue indexedForest

indexForestFrom ::
  Ord vertex =>
  Map vertex (Set vertex) ->
  Maybe vertex ->
  Int ->
  vertex ->
  (Set vertex, ForestIndex vertex) ->
  (Set vertex, ForestIndex vertex)
indexForestFrom treeAdjacency maybeParent depthValue vertexValue (visitedVertices, forestIndex)
  | Set.member vertexValue visitedVertices =
      (visitedVertices, forestIndex)
  | otherwise =
      List.foldl'
        ( \indexedForest neighborVertex ->
            indexForestFrom treeAdjacency (Just vertexValue) (depthValue + 1) neighborVertex indexedForest
        )
        (nextVisitedVertices, nextForestIndex)
        (Set.toAscList visibleNeighbors)
  where
    nextVisitedVertices =
      Set.insert vertexValue visitedVertices

    nextForestIndex =
      ForestIndex
        { fiParents =
            maybe
              (fiParents forestIndex)
              (\parentVertex -> Map.insert vertexValue parentVertex (fiParents forestIndex))
              maybeParent,
          fiDepths = Map.insert vertexValue depthValue (fiDepths forestIndex)
        }

    visibleNeighbors =
      maybe
        id
        Set.delete
        maybeParent
        (Map.findWithDefault Set.empty vertexValue treeAdjacency)

pathInForest :: Ord vertex => ForestIndex vertex -> vertex -> vertex -> Maybe [vertex]
pathInForest forestIndex sourceVertex targetVertex =
  ascend sourceVertex targetVertex [] []
  where
    ascend leftVertex rightVertex leftPath rightPath = do
      leftDepth <- Map.lookup leftVertex (fiDepths forestIndex)
      rightDepth <- Map.lookup rightVertex (fiDepths forestIndex)
      case compare leftDepth rightDepth of
        GT -> do
          leftParent <- Map.lookup leftVertex (fiParents forestIndex)
          ascend leftParent rightVertex (leftVertex : leftPath) rightPath
        LT -> do
          rightParent <- Map.lookup rightVertex (fiParents forestIndex)
          ascend leftVertex rightParent leftPath (rightVertex : rightPath)
        EQ
          | leftVertex == rightVertex ->
              Just (reverse leftPath <> [leftVertex] <> rightPath)
          | otherwise -> do
              leftParent <- Map.lookup leftVertex (fiParents forestIndex)
              rightParent <- Map.lookup rightVertex (fiParents forestIndex)
              ascend leftParent rightParent (leftVertex : leftPath) (rightVertex : rightPath)

cycleReportFromEdgeAssignment ::
  (Ord ctx, Ord basis) =>
  NonEmpty ctx ->
  Set ctx ->
  (OrientedNerveEdge ctx -> WitnessStalk basis) ->
  Maybe (CycleCohomologyReport ctx (WitnessStalk basis))
cycleReportFromEdgeAssignment cycleContexts touchedLocalContexts edgeAssignment =
  if Map.null nonZeroEdgeValues
    then Nothing
    else
      let representative = Nerve1Cochain nonZeroEdgeValues
          integral = List.foldl' (<>) mempty (Map.elems nonZeroEdgeValues)
          support =
            Set.fromList
              [ contextValue
                | (OrientedNerveEdge sourceContext targetContext, edgeValue) <- Map.toList nonZeroEdgeValues,
                  not (witnessIsZero edgeValue),
                  contextValue <- [sourceContext, targetContext]
              ]
          exactness =
            if witnessIsZero integral
              then ExactOnCycle (cyclePotential cycleContexts nonZeroEdgeValues)
              else
                NonExactOnCycle
                  H1Class
                    { h1cCycle = cycleContexts,
                      h1cRepresentative = representative,
                      h1cIntegral = integral,
                      h1cSupport = support,
                      h1cTouchedLocalContexts = touchedLocalContexts,
                      h1cMagnitude = witnessMagnitude integral
                    }
       in Just
            CycleCohomologyReport
              { ccrCycle = cycleContexts,
                ccrRepresentative = representative,
                ccrIntegral = integral,
                ccrSupport = support,
                ccrTouchedLocalContexts = touchedLocalContexts,
                ccrExactness = exactness
              }
  where
    edgeValues =
      Map.fromList
        [ (edgeValue, edgeAssignment edgeValue)
          | edgeValue <- NonEmpty.toList (orientedCycleEdges cycleContexts)
        ]

    nonZeroEdgeValues =
      Map.filter (not . witnessIsZero) edgeValues

edgeContributionIndex ::
  Ord ctx =>
  WitnessContributionAlgebra ctx obstruction basis ->
  [OrientedNerveEdge ctx] ->
  [(ctx, obstruction)] ->
  EdgeContributionIndex ctx basis
edgeContributionIndex contributionAlgebra cycleEdges scopedDescentObstructions =
  foldr insertScopedObstruction Map.empty scopedDescentObstructions
  where
    indexedEdges =
      Set.toAscList (Set.fromList cycleEdges)

    insertScopedObstruction (contextValue, obstructionValue) indexedContributions =
      foldr (insertEdgeContribution contextValue obstructionValue) indexedContributions indexedEdges

    insertEdgeContribution contextValue obstructionValue edgeValue indexedContributions =
      let contribution =
            wcaEdgeContribution contributionAlgebra edgeValue obstructionValue
       in if witnessIsZero contribution
            then indexedContributions
            else Map.insertWith (<>) edgeValue [(contextValue, contribution)] indexedContributions

cycleCohomologyReportFromIndex ::
  (Ord ctx, Ord basis) =>
  NonEmpty ctx ->
  EdgeContributionIndex ctx basis ->
  Maybe (CycleCohomologyReport ctx (WitnessStalk basis))
cycleCohomologyReportFromIndex cycleContexts indexedContributions =
  cycleReportFromEdgeAssignment cycleContexts touchedLocalContexts edgeAssignment
  where
    cycleEdges =
      NonEmpty.toList (orientedCycleEdges cycleContexts)

    edgeAssignment edgeValue =
      foldMap snd (Map.findWithDefault [] edgeValue indexedContributions)

    touchedLocalContexts =
      Set.fromList
        [ contextValue
        | edgeValue <- cycleEdges,
          (contextValue, _contribution) <- Map.findWithDefault [] edgeValue indexedContributions
        ]

coverCohomologyReport ::
  (Ord ctx, Ord basis) =>
  (report -> [obstruction]) ->
  (report -> [AnalysisTruncationCause]) ->
  (obstruction -> Maybe (ctx, [ctx])) ->
  WitnessContributionAlgebra ctx obstruction basis ->
  CoverNerve ctx ->
  report ->
  CoverCohomologyReport ctx report obstruction (WitnessStalk basis)
coverCohomologyReport reportObstructions reportTruncationCauses obstructionScope contributionAlgebra nerveValue descentReportValue =
  CoverCohomologyReport
    { corNerve = nerveValue,
      corDescentReport = descentReportValue,
      corCompleteness = analysisCompletenessFromCauses (reportTruncationCauses descentReportValue),
      corLocalC1Conflicts = localConflicts,
      corCycleReports = cycleReports,
      corH1Obstructions = h1Obstructions
    }
  where
    scopedLocalObstructions =
      mapMaybe (scopedObstruction obstructionScope) (reportObstructions descentReportValue)

    cycleScopedObstructions =
      fmap (\(contextValue, _coverElements, obstructionValue) -> (contextValue, obstructionValue)) scopedLocalObstructions

    localConflicts =
      fmap
        ( \(contextValue, coverElements, obstructionValue) ->
           C1LocalConflict
             { c1cContext = contextValue,
               c1cDescentObstruction = obstructionValue,
               c1cWitness = wcaLocalContribution contributionAlgebra obstructionValue,
               c1cSupport = Set.fromList (contextValue : coverElements)
             }
        )
        scopedLocalObstructions

    indexedEdgeContributions =
      edgeContributionIndex contributionAlgebra cycleEdges cycleScopedObstructions

    cycleEdges =
      cnFundamentalCycles nerveValue
        & foldMap (NonEmpty.toList . orientedCycleEdges)
        & Set.fromList
        & Set.toAscList

    cycleReports =
      Map.fromList
        [ (cycleContexts, reportValue)
        | cycleContexts <- cnFundamentalCycles nerveValue,
          Just reportValue <- [cycleCohomologyReportFromIndex cycleContexts indexedEdgeContributions]
        ]

    h1Obstructions =
      [ h1Class
        | reportValue <- Map.elems cycleReports,
          NonExactOnCycle h1Class <- [ccrExactness reportValue]
      ]

holonomyCoverCohomologyReport ::
  (Ord ctx, Ord basis) =>
  CoverNerve ctx ->
  report ->
  Nerve1Cochain ctx (WitnessStalk basis) ->
  CoverCohomologyReport ctx report obstruction (WitnessStalk basis)
holonomyCoverCohomologyReport nerveValue descentReportValue (Nerve1Cochain transitionEdges) =
  CoverCohomologyReport
    { corNerve = nerveValue,
      corDescentReport = descentReportValue,
      corCompleteness = AnalysisComplete,
      corLocalC1Conflicts = [],
      corCycleReports = cycleReports,
      corH1Obstructions = h1Obstructions
    }
  where
    edgeAssignment edgeValue =
      Map.findWithDefault mempty edgeValue transitionEdges

    cycleReports =
      Map.fromList
        [ (cycleContexts, reportValue)
          | cycleContexts <- cnFundamentalCycles nerveValue,
            Just reportValue <- [cycleReportFromEdgeAssignment cycleContexts Set.empty edgeAssignment]
        ]

    h1Obstructions =
      [ h1Class
        | reportValue <- Map.elems cycleReports,
          NonExactOnCycle h1Class <- [ccrExactness reportValue]
      ]

tupleWitnessCoverCohomologyReport ::
  (Ord ctx, Ord rep) =>
  CoverNerve ctx ->
  QuotientDescent.DescentReport ctx (CoverSearchRefusal Int) (QuotientDescent.QuotientDescentObstruction ctx rep) ->
  TupleWitnessCoverCohomologyReport ctx rep
tupleWitnessCoverCohomologyReport =
  coverCohomologyReport
    QuotientDescent.drObstructions
    (fmap coverSearchRefusalTruncationCause . QuotientDescent.drRefusals)
    quotientDescentObstructionScope
    tupleWitnessContributionAlgebra

assignmentWitnessCoverCohomologyReport ::
  (Ord ctx, Ord coord, Ord value) =>
  CoverNerve ctx ->
  AssignmentDescent.DescentReport ctx (CoverSearchRefusal ctx) (AssignmentDescent.AssignmentDescentObstruction ctx coord value admissibilityWitness admissibilityCost) ->
  AssignmentWitnessCoverCohomologyReport ctx coord value admissibilityWitness admissibilityCost
assignmentWitnessCoverCohomologyReport =
  coverCohomologyReport
    AssignmentDescent.drObstructions
    (fmap coverSearchRefusalTruncationCause . AssignmentDescent.drRefusals)
    assignmentDescentObstructionScope
    assignmentWitnessContributionAlgebra

analysisCompletenessFromCauses :: [AnalysisTruncationCause] -> AnalysisCompleteness
analysisCompletenessFromCauses causes =
  maybe AnalysisComplete AnalysisTruncated (NonEmpty.nonEmpty causes)

coverSearchRefusalTruncationCause :: CoverSearchRefusal coordinate -> AnalysisTruncationCause
coverSearchRefusalTruncationCause (CoverSearchBudgetExceeded budget cost) =
  TruncatedByDescentSearchRefusal budget (cscAssignmentUpperBound cost)

scopedObstruction ::
  (obstruction -> Maybe (ctx, [ctx])) ->
  obstruction ->
  Maybe (ctx, [ctx], obstruction)
scopedObstruction obstructionScope obstructionValue =
  case obstructionScope obstructionValue of
    Just (contextValue, coverElements) ->
      Just (contextValue, coverElements, obstructionValue)
    Nothing ->
      Nothing

quotientDescentObstructionScope ::
  QuotientDescent.QuotientDescentObstruction ctx rep ->
  Maybe (ctx, [ctx])
quotientDescentObstructionScope obstructionValue =
  case obstructionValue of
    QuotientDescent.QuotientDescentObstruction contextValue coverElements _obstructedTuples ->
      Just (contextValue, coverElements)
    QuotientDescent.DescentCoverLookupObstruction _contextValue _lookupError ->
      Nothing
    QuotientDescent.DescentClassSectionLookupObstruction _contextValue _lookupError ->
      Nothing
    QuotientDescent.DescentMeetLookupObstruction _contextValue _coverElements _leftContext _rightContext _lookupError ->
      Nothing
    QuotientDescent.DescentSupportLookupObstruction _contextValue _coverElements _classId _lookupError ->
      Nothing
    QuotientDescent.DescentJoinLookupObstruction _leftContext _rightContext _lookupError ->
      Nothing
    QuotientDescent.DescentVacuousCoverObstruction contextValue coverElements _vacuousCoordinates ->
      Just (contextValue, coverElements)
    QuotientDescent.DescentMonotonicityObstruction contextValue coverElement _parentClassId _divergentImages _missingMemberKeys ->
      Just (contextValue, [coverElement])

assignmentDescentObstructionScope ::
  AssignmentDescent.AssignmentDescentObstruction ctx coord value admissibilityWitness admissibilityCost ->
  Maybe (ctx, [ctx])
assignmentDescentObstructionScope obstructionValue =
  Just (AssignmentDescent.descentObstructionScope obstructionValue)

orientedCycleEdges :: NonEmpty ctx -> NonEmpty (OrientedNerveEdge ctx)
orientedCycleEdges (singleContext :| []) =
  OrientedNerveEdge singleContext singleContext :| []
orientedCycleEdges cycleContexts@(firstContext :| secondContext : remainingContexts) =
  NonEmpty.zipWith
    OrientedNerveEdge
    cycleContexts
    (secondContext :| remainingContexts <> [firstContext])

cyclePotential ::
  (Ord ctx, Ord basis) =>
  NonEmpty ctx ->
  Map (OrientedNerveEdge ctx) (WitnessStalk basis) ->
  NervePotential ctx (WitnessStalk basis)
cyclePotential cycleContexts edgeValues =
  NervePotential . Map.fromList . reverse . snd $
    List.foldl'
      step
      (mempty, [(firstContext, mempty)])
      adjacentPairs
  where
    firstContext :| remainingContexts =
      cycleContexts

    adjacentPairs =
      zip (firstContext : remainingContexts) remainingContexts

    step (currentValue, accumulatedValues) (sourceContext, targetContext) =
      let edgeValue =
            Map.findWithDefault
              mempty
              (OrientedNerveEdge sourceContext targetContext)
              edgeValues
          nextValue =
            currentValue <> edgeValue
       in (nextValue, (targetContext, nextValue) : accumulatedValues)

orientationMapFromEntries :: Ord cell => [((cell, cell), Int)] -> Map (cell, cell) Int
orientationMapFromEntries =
  Map.filter (/= 0) . Map.fromListWith (+)

alternatingSigns :: [cell] -> [(cell, Int)]
alternatingSigns =
  zipWith
    (\offset cellValue -> (cellValue, if even offset then 1 else -1))
    [0 :: Int ..]

boundaryAt ::
  SheafBasis ExpandedObstructionCell ->
  GradedComplex ExpandedObstructionCell Int ->
  Int ->
  Either
    (SheafOperatorBuildError ExpandedObstructionCell)
    (BoundaryIncidence Integer)
boundaryAt zeroBasis cochainComplex dimensionValue =
  case dimensionValue of
    0 ->
      Right (emptyBoundaryIncidenceOf (fromIntegral (basisCardinality zeroBasis)) 0)
    1 ->
      fmap
        (widenBoundaryIncidence . transposeBoundaryIncidence . gradedOperatorIncidence)
        (gradedOperatorAt (HomologicalDegree 0) cochainComplex)
    2 ->
      fmap
        (widenBoundaryIncidence . transposeBoundaryIncidence . gradedOperatorIncidence)
        (gradedOperatorAt (HomologicalDegree 1) cochainComplex)
    _ ->
      Right (emptyBoundaryIncidenceOf 0 0)

widenBoundaryIncidence :: BoundaryIncidence Int -> BoundaryIncidence Integer
widenBoundaryIncidence = mapBoundaryCoefficients fromIntegral

rankGapLowerBound :: FiniteChainComplex Integer -> Int
rankGapLowerBound finiteComplex =
  let boundary1 = incidenceMatrixAt finiteComplex (HomologicalDegree 1)
      boundary2 = incidenceMatrixAt finiteComplex (HomologicalDegree 2)
   in max 0 (sourceCardinality boundary1 - rankUpper boundary1 - rankUpper boundary2)
  where
    rankUpper :: BoundaryIncidence r -> Int
    rankUpper incidence = min (sourceCardinality incidence) (targetCardinality incidence)

rankUpperBoundary2 :: FiniteChainComplex Integer -> Int
rankUpperBoundary2 finiteComplex =
  let boundary2 = incidenceMatrixAt finiteComplex (HomologicalDegree 2)
   in min (sourceCardinality boundary2) (targetCardinality boundary2)

supportCellsFromBasis :: SheafBasis ExpandedObstructionCell -> [ObstructionCell]
supportCellsFromBasis basis =
  basisCells basis
    & fmap baseCellOfExpanded
    & Set.fromList
    & Set.toAscList

supportCellsFromRepresentatives ::
  SheafBasis ExpandedObstructionCell ->
  [RepresentativeCocycle Rational Int] ->
  [ObstructionCell]
supportCellsFromRepresentatives basis representatives =
  let indexedCells = Map.fromList (basisIndexedCells basis)
   in representatives
        & foldMap
          (\representative ->
             representativeTerms representative
               & mapMaybe
                 (\(_, cellIndex) -> baseCellOfExpanded <$> Map.lookup cellIndex indexedCells)
          )
        & Set.fromList
        & Set.toAscList

baseCellOfExpanded :: ExpandedObstructionCell -> ObstructionCell
baseCellOfExpanded expandedCell =
  case expandedCell of
    ExpandedRootCell {} -> RegionRootCell
    ExpandedOccurrenceCell occurrenceId _ -> OccurrenceCell occurrenceId
    ExpandedEqualityConstraintCell constraintId _ -> EqualityConstraintCell constraintId
    ExpandedGuardConstraintCell constraintId _ -> GuardConstraintCell constraintId
    ExpandedRelationConstraintCell relationFlavor constraintId _ ->
      RelationConstraintCell relationFlavor constraintId
    ExpandedCycleCell cycleId -> CycleCell cycleId

isOneCell :: ExpandedObstructionCell -> Bool
isOneCell expandedCell =
  case expandedCell of
    ExpandedEqualityConstraintCell {} -> True
    ExpandedGuardConstraintCell {} -> True
    ExpandedRelationConstraintCell {} -> True
    ExpandedRootCell {} -> False
    ExpandedOccurrenceCell {} -> False
    ExpandedCycleCell {} -> False
