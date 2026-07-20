{-# LANGUAGE RankNTypes #-}

module Moonlight.EGraph.Pure.Introspection.Diagnostic
  ( ContextDiagnostic (..),
    ContextStructureSnapshot (..),
    EClassSnapshot (..),
    EGraphSnapshot (..),
    EGraphDiagnostic (..),
    snapshotEGraph,
    graphDiagnostic,
    contextDiagnostic,
    contextStructureSnapshot,
    renderEGraphSummary,
    renderEGraphDot,
    renderEGraphDotWith,
  )
where

import Data.Functor (void)
import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core (Language)
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    checkedContextRestrictionMismatchesAt,
    contextCachedObjectsForExecution,
  )
import Moonlight.EGraph.Pure.Context
  ( cegClassSupportIndex,
    cegContextAnalysisDeltas,
    cegSite,
    cegRuntimeState,
    ContextRuntimeState (..),
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    EClass (..),
    EGraph,
    ENode (..),
    eGraphClassCount,
    materializeEGraphClasses,
    materializeEGraphHashCons,
    eGraphNodeCount,
    eGraphPendingClassUnions,
  )
import Moonlight.Sheaf.Context.Core qualified as SheafCore
import Moonlight.Sheaf.Context.Algebra (classesFor)
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSupportError,
    classSupportIndexCarrierGeneratorCount,
    classSupportIndexSupportEntryCount,
    contextFragmentRestrictionPairs,
    preparedContextFragment,
  )

type ContextDiagnostic :: Type -> Type -> Type -> Type
data ContextDiagnostic c mismatch diagnostic = ContextDiagnostic
  { cdCachedContexts :: [c],
    cdRestrictionCount :: Int,
    cdChangedContexts :: [c],
    cdPropagationObstructionCount :: Int,
    cdPropagationConverged :: Bool,
    cdTopRestrictionMismatches :: [mismatch],
    cdProjectionDiagnostics :: [diagnostic]
  }
  deriving stock (Eq, Show)

type ContextStructureSnapshot :: Type
data ContextStructureSnapshot = ContextStructureSnapshot
  { cssCachedContextCount :: !Int,
    cssRestrictionCount :: !Int,
    cssChangedContextCount :: !Int,
    cssPropagationObstructionCount :: !Int,
    cssClassSupportEntryCount :: !Int,
    cssTotalSupportContextCount :: !Int,
    cssContextClassEntryCount :: !Int,
    cssContextAnalysisEntryCount :: !Int
  }
  deriving stock (Eq, Ord, Show)

type EClassSnapshot :: (Type -> Type) -> Type -> Type
data EClassSnapshot f a = EClassSnapshot
  { ecsClassId :: ClassId,
    ecsClassData :: a,
    ecsNodes :: [ENode f],
    ecsParents :: [(ClassId, ENode f)]
  }

type EGraphSnapshot :: (Type -> Type) -> Type -> Type
data EGraphSnapshot f a = EGraphSnapshot
  { egsClasses :: [EClassSnapshot f a],
    egsPendingMerges :: [(ClassId, ClassId)]
  }

type EGraphDiagnostic :: Type
data EGraphDiagnostic = EGraphDiagnostic
  { egdClassCount :: Int,
    egdNodeCount :: Int,
    egdHashConsCount :: Int,
    egdPendingMergeCount :: Int,
    egdLargestClassSize :: Int,
    egdClassSizes :: IntMap Int
  }
  deriving stock (Eq, Show)

snapshotEGraph :: Language f => EGraph f a -> EGraphSnapshot f a
snapshotEGraph graph =
  EGraphSnapshot
    { egsClasses =
        fmap
          ( \eClass ->
              EClassSnapshot
                { ecsClassId = eClassId eClass,
                  ecsClassData = eClassData eClass,
                  ecsNodes = Set.toAscList (eClassNodes eClass),
                  ecsParents = eClassParents eClass
                }
          )
          (IntMap.elems (materializeEGraphClasses graph)),
      egsPendingMerges = eGraphPendingClassUnions graph
    }

graphDiagnostic :: Language f => EGraph f a -> EGraphDiagnostic
graphDiagnostic graph =
  let classSizes = fmap (Set.size . eClassNodes) (materializeEGraphClasses graph)
   in EGraphDiagnostic
        { egdClassCount = eGraphClassCount graph,
          egdNodeCount = eGraphNodeCount graph,
          egdHashConsCount = Map.size (materializeEGraphHashCons graph),
          egdPendingMergeCount = length (eGraphPendingClassUnions graph),
          egdLargestClassSize = maximum (0 : IntMap.elems classSizes),
          egdClassSizes = classSizes
        }

contextDiagnostic ::
  (Language f, Ord c, Eq a) =>
  ContextEGraph owner f a c ->
  ContextDiagnostic
    c
    (SheafCore.SectionMismatch ClassId a)
    ()
contextDiagnostic contextGraph =
  let cachedContexts =
        contextCachedObjectsForExecution contextGraph
      maybeReport = contextGraphPropagationReport contextGraph
   in ContextDiagnostic
        { cdCachedContexts = cachedContexts,
          cdRestrictionCount =
            length
              (contextFragmentRestrictionPairs (preparedContextFragment (cegSite contextGraph))),
          cdChangedContexts = maybe [] SheafCore.contextPropagationChangedContexts maybeReport,
          cdPropagationObstructionCount = maybe 0 SheafCore.contextPropagationObstructionCount maybeReport,
          cdPropagationConverged = maybe False SheafCore.contextPropagationSettled maybeReport,
          cdTopRestrictionMismatches =
            take 5
              ( cachedContexts >>= \contextValue ->
                  either
                    (const [])
                    id
                    (checkedContextRestrictionMismatchesAt contextValue contextGraph)
              ),
          cdProjectionDiagnostics = []
        }

contextStructureSnapshot :: (Language f, Ord c) => ContextEGraph owner f a c -> Either (PreparedContextSupportError c) ContextStructureSnapshot
contextStructureSnapshot contextGraph = do
  let maybeReport = contextGraphPropagationReport contextGraph
      contextAnalysisDeltas = cegContextAnalysisDeltas contextGraph
      cachedContexts = contextCachedObjectsForExecution contextGraph
  classSections <- traverse (`classesFor` contextGraph) cachedContexts
  pure
    ContextStructureSnapshot
        { cssCachedContextCount =
            Map.size contextAnalysisDeltas,
          cssRestrictionCount =
            length
              (contextFragmentRestrictionPairs (preparedContextFragment (cegSite contextGraph))),
          cssChangedContextCount =
            length (maybe [] SheafCore.contextPropagationChangedContexts maybeReport),
          cssPropagationObstructionCount = maybe 0 SheafCore.contextPropagationObstructionCount maybeReport,
          cssClassSupportEntryCount =
            classSupportIndexSupportEntryCount (cegClassSupportIndex contextGraph),
          cssTotalSupportContextCount =
            classSupportIndexCarrierGeneratorCount (cegClassSupportIndex contextGraph),
          cssContextClassEntryCount =
            sum (fmap IntMap.size classSections),
          cssContextAnalysisEntryCount =
            sum (fmap IntMap.size (Map.elems contextAnalysisDeltas))
        }

contextGraphPropagationReport ::
  ContextEGraph owner f a c ->
  Maybe (SheafCore.ContextPropagationReport c)
contextGraphPropagationReport contextGraph =
  crsLastRepair (cegRuntimeState contextGraph)

renderEGraphSummary :: (Language f, Show a, forall value. Show value => Show (f value)) => EGraph f a -> String
renderEGraphSummary graph =
  let graphSnapshot = snapshotEGraph graph
   in foldMap renderClassSummary (egsClasses graphSnapshot)
        <> renderPendingMergeSummary (egsPendingMerges graphSnapshot)

renderEGraphDot :: (Language f, Show a, forall value. Show value => Show (f value)) => EGraph f a -> String
renderEGraphDot =
  renderEGraphDotWith show

renderEGraphDotWith :: (Language f, forall value. Show value => Show (f value)) => (a -> String) -> EGraph f a -> String
renderEGraphDotWith renderAnalysis graph =
  unlines
    ( ["digraph egraph {"]
        <> (IntMap.elems (materializeEGraphClasses graph) >>= renderEClass renderAnalysis)
        <> foldMap renderPendingMergeEdge (eGraphPendingClassUnions graph)
        <> ["}"]
    )

renderEClass :: (Language f, forall value. Show value => Show (f value)) => (a -> String) -> EClass f a -> [String]
renderEClass renderAnalysis eClass =
  let classNodeName = classNodeIdentifier (eClassId eClass)
      nodeLines = zip [0 :: Int ..] (Set.toAscList (eClassNodes eClass)) >>= uncurry (renderENodeEdge classNodeName)
   in
    [ "  "
        <> classNodeName
        <> " [shape=record,label=\"class "
        <> showClassId (eClassId eClass)
        <> "|analysis="
        <> escapeDotLabel (renderAnalysis (eClassData eClass))
        <> "|nodes="
        <> escapeDotLabel (foldMap (\enode -> renderNodeLabel enode <> "\\l") (Set.toAscList (eClassNodes eClass)))
        <> "\"];"
    ]
      <> nodeLines

renderENodeEdge :: (Language f, forall value. Show value => Show (f value)) => String -> Int -> ENode f -> [String]
renderENodeEdge classNodeName nodeIndex enode@(ENode childClassIds) =
  let enodeName = classNodeName <> "_node_" <> show nodeIndex
      childLines =
        foldr
          (\childClassId linesAcc -> ("  " <> enodeName <> " -> " <> classNodeIdentifier childClassId <> " [label=\"child\"];") : linesAcc)
          []
          childClassIds
   in
    [ "  " <> enodeName <> " [shape=box,label=\"" <> escapeDotLabel (show enode) <> "\"];",
      "  " <> classNodeName <> " -> " <> enodeName <> " [label=\"contains\"];"
    ]
      <> childLines

renderPendingMergeEdge :: (ClassId, ClassId) -> [String]
renderPendingMergeEdge (leftClassId, rightClassId) =
  [ "  "
      <> classNodeIdentifier leftClassId
      <> " -> "
      <> classNodeIdentifier rightClassId
      <> " [style=dashed,color=red,label=\"pending_merge\"];"
  ]

renderClassSummary :: (Functor f, Show a, forall value. Show value => Show (f value)) => EClassSnapshot f a -> String
renderClassSummary classSnapshot =
  "Class "
    <> showClassId (ecsClassId classSnapshot)
    <> " data="
    <> show (ecsClassData classSnapshot)
    <> " nodes="
    <> show (fmap renderNodeLabel (ecsNodes classSnapshot))
    <> "\n"

renderPendingMergeSummary :: [(ClassId, ClassId)] -> String
renderPendingMergeSummary pendingMerges =
  if null pendingMerges
    then ""
    else "pending_merges=" <> show pendingMerges <> "\n"

renderNodeLabel :: (Functor f, forall value. Show value => Show (f value)) => ENode f -> String
renderNodeLabel (ENode nodeValue) =
  show (void nodeValue)

classNodeIdentifier :: ClassId -> String
classNodeIdentifier (ClassId classKey) =
  "class_" <> show classKey

showClassId :: ClassId -> String
showClassId (ClassId classKey) =
  show classKey

escapeDotLabel :: String -> String
escapeDotLabel =
  concatMap escapeCharacter

escapeCharacter :: Char -> String
escapeCharacter character =
  case character of
    '"' -> "\\\""
    '\\' -> "\\\\"
    '\n' -> "\\n"
    _ -> [character]
