module Moonlight.EGraph.Pure.Types
  ( module Moonlight.EGraph.Pure.Delta,
    module Moonlight.EGraph.Pure.Types.Core,
    ClassId (..),
    classIdKey,
    ENodeId (..),
    RewriteRuleId (..),
    rewriteRuleIdKey,
    ProofStepId (..),
    EGraph,
    emptyEGraph,
    emptyEGraphWithTheory,
    eGraphUnionFind,
    eGraphClasses,
    eGraphClassNodes,
    eGraphHashCons,
    eGraphAnalysis,
    eGraphPendingDelta,
    eGraphPendingClassUnions,
    eGraphAnalysisSpec,
    eGraphTheorySpec,
    eGraphStore,
    eGraphRevision,
    eGraphNodeCount,
    eGraphClassCount,
    enqueueEditDelta,
    canonicalizeClassId,
    lookupEClass,
  )
where

import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core
  ( ClassId (..),
    ENodeId (..),
    Language,
    ProofStepId (..),
    RewriteRuleId (..),
    classIdKey,
    rewriteRuleIdKey,
  )
import Moonlight.Core (TheorySpec, emptyTheorySpec)
import Moonlight.EGraph.Pure.Analysis.Spec (AnalysisSpec)
import Moonlight.EGraph.Pure.Delta
import Moonlight.EGraph.Pure.Structural.Store
  ( StructuralStore,
    emptyStructuralStore,
    storeNodeCount,
    structuralEntries,
    structuralTuplesForResultKey,
  )
import Moonlight.EGraph.Pure.Types.Core
import Moonlight.EGraph.Pure.Types.Internal (EGraph (..), bumpEGraphRevision)
import Moonlight.Core (UnionFind, emptyUnionFind)
import Moonlight.Core qualified as UnionFind

emptyEGraph :: AnalysisSpec f a -> EGraph f a
emptyEGraph analysisSpec =
  emptyEGraphWithTheory analysisSpec emptyTheorySpec

emptyEGraphWithTheory :: AnalysisSpec f a -> TheorySpec f -> EGraph f a
emptyEGraphWithTheory analysisSpec theorySpec =
  EGraph
    { egUnionFind = emptyUnionFind,
      egStore = emptyStructuralStore,
      egAnalysis = IntMap.empty,
      egPendingDelta = emptyEGraphEditDelta,
      egAnalysisSpec = analysisSpec,
      egTheorySpec = theorySpec,
      egRevision = initialEGraphRevision
    }

eGraphUnionFind :: EGraph f a -> UnionFind
eGraphUnionFind =
  egUnionFind

eGraphClasses :: Language f => EGraph f a -> IntMap (EClass f a)
eGraphClasses graph =
  materializeClasses (egStore graph) (egAnalysis graph)

eGraphClassNodes :: Language f => EGraph f a -> ClassId -> Set.Set (ENode f)
eGraphClassNodes graph classId =
  Set.fromList
    ( structuralTuplesForResultKey
        (classIdKey (canonicalizeClassId graph classId))
        (egStore graph)
    )

eGraphHashCons :: Language f => EGraph f a -> Map (ENode f) ClassId
eGraphHashCons graph =
  materializeHashCons (egStore graph)

eGraphAnalysis :: EGraph f a -> IntMap a
eGraphAnalysis =
  egAnalysis

eGraphPendingDelta :: EGraph f a -> EGraphEditDelta
eGraphPendingDelta =
  egPendingDelta

eGraphPendingClassUnions :: EGraph f a -> [(ClassId, ClassId)]
eGraphPendingClassUnions =
  eGraphEditDeltaClassUnions . egPendingDelta

eGraphAnalysisSpec :: EGraph f a -> AnalysisSpec f a
eGraphAnalysisSpec =
  egAnalysisSpec

eGraphTheorySpec :: EGraph f a -> TheorySpec f
eGraphTheorySpec =
  egTheorySpec

eGraphStore :: EGraph f a -> StructuralStore f
eGraphStore =
  egStore

eGraphRevision :: EGraph f a -> EGraphRevision
eGraphRevision =
  egRevision

eGraphNodeCount :: EGraph f a -> Int
eGraphNodeCount =
  storeNodeCount . egStore

eGraphClassCount :: EGraph f a -> Int
eGraphClassCount =
  IntMap.size . egAnalysis

enqueueEditDelta :: EGraphEditDelta -> EGraph f a -> EGraph f a
enqueueEditDelta delta graph =
  if eGraphEditDeltaNull delta
    then graph
    else bumpEGraphRevision (graph {egPendingDelta = egPendingDelta graph <> delta})

canonicalizeClassId :: EGraph f a -> ClassId -> ClassId
canonicalizeClassId graph classId =
  fst (UnionFind.find classId (eGraphUnionFind graph))

lookupEClass :: Language f => EGraph f a -> ClassId -> Maybe (EClass f a)
lookupEClass graph classId =
  IntMap.lookup (classIdKey (canonicalizeClassId graph classId)) (eGraphClasses graph)

materializeClasses :: Language f => StructuralStore f -> IntMap a -> IntMap (EClass f a)
materializeClasses store analysisMap =
  let entries =
        structuralEntries store
      nodesByClass =
        IntMap.fromListWith Set.union
          [(classIdKey cid, Set.singleton enode) | (cid, enode) <- entries]
      parentsByClass =
        IntMap.fromListWith (<>)
          [ (classIdKey child, [(cid, enode)])
            | (cid, enode@(ENode children)) <- entries,
              child <- toList children
          ]
   in IntMap.mapWithKey
        (\key analysisValue ->
           EClass
             { eClassId = ClassId key,
               eClassNodes = IntMap.findWithDefault Set.empty key nodesByClass,
               eClassData = analysisValue,
               eClassParents = IntMap.findWithDefault [] key parentsByClass
             }
        )
        analysisMap

materializeHashCons :: Language f => StructuralStore f -> Map (ENode f) ClassId
materializeHashCons store =
  Map.fromListWith
    min
    [(node, resultClassId) | (resultClassId, node) <- structuralEntries store]
