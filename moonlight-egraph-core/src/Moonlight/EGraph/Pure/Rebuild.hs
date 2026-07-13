{-# LANGUAGE TypeFamilies #-}

module Moonlight.EGraph.Pure.Rebuild
  ( EGraphRebuildDelta (..),
    BaseRepairIndex (..),
    emptyRepairIndex,
    merge,
    equateClassesTracked,
    equateClassPairsTracked,
    drainPendingEditDelta,
    drainPendingEditDeltaPre,
    runRepairBFSFromStore,
    PreRepairOutcome (..),
    graphAfterPreRepair,
    rebuild,
    rebuildCollectingImpactedKeys,
    rebuildTracked,
    rebuildWithDelta,
    repairAnalysisFromRows,
    recomputeAnalysisFromENodes,
    recomputeClassFromStore,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Graph qualified as Graph
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NE
import Moonlight.Core
  ( Language,
  )
import Moonlight.EGraph.Pure.Delta
  ( EGraphEditDelta,
    EGraphRebuildDelta (..),
    classUnionDelta,
    classUnionsDelta,
    eGraphEditDeltaClassUnions,
    eGraphEditDeltaNull,
    eGraphRebuildDeltaTouchedKeys,
    emptyEGraphEditDelta,
  )
import Moonlight.Core (TheorySpec)
import Moonlight.EGraph.Pure.Analysis.Spec (AnalysisSpec (..))
import Moonlight.EGraph.Pure.Change
  ( EGraphMutationResult (..),
    EGraphRebuildTrace (..),
    makeEGraphMutationResult,
    observedClassUnionKeys,
    observedClassUnionsFromEditDelta,
  )
import Moonlight.EGraph.Pure.Rebuild.Index
  ( BaseRepairIndex (..),
    baseRepairIndexFromStore,
    canonicalizeClassKeys,
    emptyRepairIndex,
    ensureRepairIndex,
  )
import Moonlight.EGraph.Pure.Structural.Store
  ( StructuralEdit (..),
    StructuralStore,
    StructuralTuplePatch,
    canonicalizeStructuralDirtyRows,
    emptyStructuralTuplePatch,
    structuralChildrenByResultWithin,
    structuralDirtyResultKeys,
    structuralRepairClosure,
    structuralTuplesForResultKey,
    tuplePatchTouchedKeys,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    ENode (..),
    classIdKey,
  )
import Moonlight.EGraph.Pure.Types.Internal (EGraph (..))
import Moonlight.EGraph.Pure.Types.Internal qualified as EGraph
import Moonlight.Core qualified as UnionFind

-- | Stage a class union for the next rebuild.
--
-- This does not immediately union the representatives observed by
-- 'Moonlight.EGraph.Pure.Types.canonicalizeClassId'.  It records a pending
-- class-union delta; 'rebuild' drains that delta, canonicalizes the database,
-- repairs analysis, and recursively stages congruence obstructions until the
-- graph is stable.
merge :: ClassId -> ClassId -> EGraph f a -> EGraph f a
merge leftClassId rightClassId graph =
  snd (mergeWithObservedDelta leftClassId rightClassId graph)
{-# INLINE merge #-}

mergeWithObservedDelta ::
  ClassId ->
  ClassId ->
  EGraph f a ->
  (EGraphEditDelta, EGraph f a)
mergeWithObservedDelta leftClassId rightClassId graph =
  let (leftRoot, uf1) = UnionFind.find leftClassId (egUnionFind graph)
      (rightRoot, uf2) = UnionFind.find rightClassId uf1
   in if leftRoot == rightRoot
        then (emptyEGraphEditDelta, graph {egUnionFind = uf2})
        else
          let observedDelta =
                classUnionDelta leftRoot rightRoot
           in ( observedDelta,
                EGraph.bumpEGraphRevision
                  ( graph
                      { egUnionFind = uf2,
                        egPendingDelta = egPendingDelta graph <> observedDelta
                      }
                  )
              )
{-# INLINE mergeWithObservedDelta #-}

equateClassesTracked ::
  ClassId ->
  ClassId ->
  EGraph f a ->
  EGraphMutationResult f a ClassId
equateClassesTracked leftClassId rightClassId graph =
  let (canonicalLeft, _) =
        UnionFind.find leftClassId (egUnionFind graph)
      (observedDelta, updatedGraph) =
        mergeWithObservedDelta leftClassId rightClassId graph
      observedUnions =
        observedClassUnionsFromEditDelta observedDelta
      touchedKeys =
        observedClassUnionKeys observedUnions
   in makeEGraphMutationResult
        canonicalLeft
        graph
        updatedGraph
        touchedKeys
        IntSet.empty
        IntSet.empty
        observedUnions
        []
{-# INLINE equateClassesTracked #-}

equateClassPairsTracked ::
  [(ClassId, ClassId)] ->
  EGraph f a ->
  EGraphMutationResult f a ()
equateClassPairsTracked pairs graph =
  let (observedPairs, compressedUnionFind) =
        foldl'
          collectObservedPair
          ([], egUnionFind graph)
          pairs
      observedDelta =
        classUnionsDelta (reverse observedPairs)
      observedUnions =
        observedClassUnionsFromEditDelta observedDelta
      touchedKeys =
        observedClassUnionKeys observedUnions
      updatedGraph =
        if eGraphEditDeltaNull observedDelta
          then graph {egUnionFind = compressedUnionFind}
          else
            EGraph.bumpEGraphRevision
              ( graph
                  { egUnionFind = compressedUnionFind,
                    egPendingDelta = egPendingDelta graph <> observedDelta
                  }
              )
   in makeEGraphMutationResult
        ()
        graph
        updatedGraph
        touchedKeys
        IntSet.empty
        IntSet.empty
        observedUnions
        []
  where
    collectObservedPair (observedPairs, unionFind) (leftClassId, rightClassId) =
      let (leftRoot, afterLeft) =
            UnionFind.find leftClassId unionFind
          (rightRoot, afterRight) =
            UnionFind.find rightClassId afterLeft
       in if leftRoot == rightRoot
            then (observedPairs, afterRight)
            else ((leftRoot, rightRoot) : observedPairs, afterRight)
{-# INLINE equateClassPairsTracked #-}

rebuild :: Language f => EGraph f a -> EGraph f a
rebuild graph =
  let (_delta, _index, result) = rebuildWithDelta graph in result

-- | Rebuild while collecting the raw class-union endpoint keys of every
-- drain level, congruence cascades included. Unlike the canonicalized
-- 'EGraphRebuildDelta' keys, the raw endpoints retain absorbed
-- representatives, so the result names every representative whose class
-- membership the rebuild disturbed.
rebuildCollectingImpactedKeys :: Language f => EGraph f a -> (EGraph f a, IntSet)
rebuildCollectingImpactedKeys graph
  | eGraphEditDeltaNull (egPendingDelta graph) =
      (graph, IntSet.empty)
  | otherwise =
      let preRepair =
            drainPendingEditDeltaPre (egPendingDelta graph) graph
          graphAfterDrain = graphAfterPreRepair preRepair
          rawImpactedKeys = proImpactedKeys preRepair
       in case proCongruenceObstructions preRepair of
            [] ->
              (graphAfterDrain, rawImpactedKeys)
            obstructions ->
              let (finalGraph, recursiveImpactedKeys) =
                    rebuildCollectingImpactedKeys
                      (graphAfterDrain {egPendingDelta = classUnionsDelta obstructions})
               in (finalGraph, IntSet.union rawImpactedKeys recursiveImpactedKeys)

rebuildTracked ::
  Language f =>
  Maybe (BaseRepairIndex f) ->
  EGraph f a ->
  EGraphMutationResult f a (EGraphRebuildTrace f)
rebuildTracked _cachedIndex graph =
  let (rebuildDelta, tuplePatch, updatedGraph) =
        if eGraphEditDeltaNull (egPendingDelta graph)
          then (mempty, emptyStructuralTuplePatch, graph)
          else
            let (delta, patchValue, _repairIndex, nextGraph) =
                  rebuildWithTuplePatch graph
             in (delta, patchValue, nextGraph)
      rebuildTrace =
        EGraphRebuildTrace
          { egrtRebuildDelta = rebuildDelta,
            egrtTuplePatch = tuplePatch
          }
      observedRebuildTraces =
        if eGraphRebuildDeltaNull rebuildDelta
          then []
          else [rebuildTrace]
   in makeEGraphMutationResult
        rebuildTrace
        graph
        updatedGraph
        (eGraphRebuildDeltaTouchedKeys rebuildDelta)
        IntSet.empty
        IntSet.empty
        mempty
        observedRebuildTraces
{-# INLINE rebuildTracked #-}

rebuildWithDelta :: Language f => EGraph f a -> (EGraphRebuildDelta, BaseRepairIndex f, EGraph f a)
rebuildWithDelta graph
  | eGraphEditDeltaNull (egPendingDelta graph) =
      (mempty, ensureRepairIndex Nothing graph, graph)
  | otherwise =
      let (delta, tuplePatch, repairIndex, updatedGraph) = rebuildWithTuplePatch graph
       in tuplePatch `seq` (delta, repairIndex, updatedGraph)

rebuildWithTuplePatch :: Language f => EGraph f a -> (EGraphRebuildDelta, StructuralTuplePatch f, BaseRepairIndex f, EGraph f a)
rebuildWithTuplePatch graph
  | eGraphEditDeltaNull (egPendingDelta graph) =
      (mempty, emptyStructuralTuplePatch, emptyRepairIndex, graph)
  | otherwise =
      let preRepair =
            drainPendingEditDeltaPre (egPendingDelta graph) graph
          canonicalize = proCanonicalize preRepair
          rawImpactedKeys = proImpactedKeys preRepair
          graphAfterDrain = graphAfterPreRepair preRepair
          impactedKeys = canonicalizeClassKeys canonicalize rawImpactedKeys
          dirtyResultKeys = proDirtyResultKeys preRepair
          drainedDelta = EGraphRebuildDelta impactedKeys dirtyResultKeys IntSet.empty
          obstructions = proCongruenceObstructions preRepair
       in case obstructions of
            [] ->
              (drainedDelta, proTuplePatch preRepair, proRepairIndex preRepair, graphAfterDrain)
            _ ->
              let (recDelta, recPatch, finalRepairIndex, finalGraph) =
                    rebuildWithTuplePatch
                      (graphAfterDrain {egPendingDelta = classUnionsDelta obstructions})
               in ( drainedDelta <> recDelta,
                    proTuplePatch preRepair <> recPatch,
                    finalRepairIndex,
                    finalGraph
                  )

eGraphRebuildDeltaNull :: EGraphRebuildDelta -> Bool
eGraphRebuildDeltaNull rebuildDelta =
  IntSet.null (erdImpactedClassKeys rebuildDelta)
    && IntSet.null (erdDirtyResultKeys rebuildDelta)
    && IntSet.null (erdTopologyClassKeys rebuildDelta)
{-# INLINE eGraphRebuildDeltaNull #-}

drainPendingEditDelta :: Language f => EGraph f a -> EGraph f a
drainPendingEditDelta graph
  | eGraphEditDeltaNull pendingDelta = graph
  | otherwise =
      graphAfterPreRepair (drainPendingEditDeltaPre pendingDelta graph)
  where
    pendingDelta =
      egPendingDelta graph

graphAfterPreRepair ::
  Language f =>
  PreRepairOutcome f a ->
  EGraph f a
graphAfterPreRepair preRepair =
  let canonicalize = proCanonicalize preRepair
      rawImpactedKeys = proImpactedKeys preRepair
      preRepairGraph = proGraph preRepair
      repairedAnalysis =
        runRepairBFSFromStore
          (egAnalysisSpec preRepairGraph)
          canonicalize
          (IntSet.union rawImpactedKeys (proDirtyResultKeys preRepair))
          (egStore preRepairGraph)
          (egAnalysis preRepairGraph)
   in preRepairGraph {egAnalysis = repairedAnalysis}

type PreRepairOutcome :: (Type -> Type) -> Type -> Type
data PreRepairOutcome f a = PreRepairOutcome
  { proCanonicalize :: !(ClassId -> ClassId),
    proImpactedKeys :: !IntSet,
    proDirtyResultKeys :: !IntSet,
    proTuplePatch :: !(StructuralTuplePatch f),
    proCongruenceObstructions :: ![(ClassId, ClassId)],
    proRepairIndex :: BaseRepairIndex f,
    proGraph :: !(EGraph f a)
  }

type StructuralRepairState :: (Type -> Type) -> Type
data StructuralRepairState f = StructuralRepairState
  { srsStore :: !(StructuralStore f),
    srsTuplePatch :: !(StructuralTuplePatch f),
    srsDirtyResultKeys :: !IntSet,
    srsObstructions :: ![(ClassId, ClassId)]
  }

drainPendingEditDeltaPre ::
  Language f =>
  EGraphEditDelta ->
  EGraph f a ->
  PreRepairOutcome f a
drainPendingEditDeltaPre pendingDelta graph =
  let mergedUnionFind =
        unionFindAfterClassUnions (egUnionFind graph) pendingClassUnions
      canonicalize cid =
        fst (UnionFind.find cid mergedUnionFind)
      impactedKeys =
        IntSet.fromList
          [ classIdKey classIdValue
            | (leftClassId, rightClassId) <- pendingClassUnions,
              classIdValue <- [leftClassId, rightClassId]
          ]
      canonicalImpactedKeys =
        canonicalizeClassKeys canonicalize impactedKeys
      seedKeys =
        IntSet.union impactedKeys canonicalImpactedKeys
      initialDirtyResultKeys =
        structuralDirtyResultKeys (egStore graph) seedKeys
      repairedStructure =
        repairStructuralFrontier
          (egTheorySpec graph)
          canonicalize
          seedKeys
          initialDirtyResultKeys
          (egStore graph)
      mergedAnalysis =
        mergeAbsorbedAnalysis canonicalize impactedKeys (egAnalysis graph) (egAnalysisSpec graph)
      revisionBaseGraph =
        if eGraphEditDeltaNull pendingDelta
          then graph
          else EGraph.bumpEGraphRevision graph
      dirtyResultKeys =
        srsDirtyResultKeys repairedStructure
      preRepairGraph =
        revisionBaseGraph
          { egUnionFind = mergedUnionFind,
            egAnalysis = mergedAnalysis,
            egStore = srsStore repairedStructure,
            egPendingDelta = emptyEGraphEditDelta
          }
   in PreRepairOutcome
        { proCanonicalize = canonicalize,
          proImpactedKeys = impactedKeys,
          proDirtyResultKeys = dirtyResultKeys,
          proTuplePatch = srsTuplePatch repairedStructure,
          proCongruenceObstructions = srsObstructions repairedStructure,
          proRepairIndex = baseRepairIndexFromStore (srsStore repairedStructure),
          proGraph = preRepairGraph
        }
  where
    pendingClassUnions =
      eGraphEditDeltaClassUnions pendingDelta

unionFindAfterClassUnions ::
  UnionFind.UnionFind ->
  [(ClassId, ClassId)] ->
  UnionFind.UnionFind
unionFindAfterClassUnions =
  foldl'
    ( \unionFind (leftClassId, rightClassId) ->
        UnionFind.union leftClassId rightClassId unionFind
    )

repairStructuralFrontier ::
  Language f =>
  TheorySpec f ->
  (ClassId -> ClassId) ->
  IntSet ->
  IntSet ->
  StructuralStore f ->
  StructuralRepairState f
repairStructuralFrontier theorySpec canonicalize initialExpandedKeys initialDirtyKeys initialStore =
  StructuralRepairState
    { srsStore = seStore repairEdit,
      srsTuplePatch = seTuplePatch repairEdit,
      srsDirtyResultKeys = dirtyResultKeys,
      srsObstructions = seCongruenceObstructions repairEdit
    }
  where
    repairKeys =
      structuralRepairClosure initialStore (IntSet.union initialExpandedKeys initialDirtyKeys)

    repairEdit =
      canonicalizeStructuralDirtyRows theorySpec canonicalize repairKeys initialStore

    dirtyResultKeys =
      IntSet.union
        initialDirtyKeys
        (structuralDirtyResultKeys (seStore repairEdit) (tuplePatchTouchedKeys (seTuplePatch repairEdit)))

runRepairBFSFromStore ::
  forall f a.
  Language f =>
  AnalysisSpec f a ->
  (ClassId -> ClassId) ->
  IntSet ->
  StructuralStore f ->
  IntMap.IntMap a ->
  IntMap.IntMap a
runRepairBFSFromStore spec canonicalize dirtyKeys store analysisMap =
  let canonicalDirtyKeys =
        canonicalizeClassKeys canonicalize dirtyKeys
      repairKeys =
        structuralRepairClosure store canonicalDirtyKeys
      childrenWithin =
        structuralChildrenByResultWithin repairKeys store
   in repairAnalysisFromRows
        spec
        canonicalize
        (`structuralTuplesForResultKey` store)
        childrenWithin
        canonicalDirtyKeys
        repairKeys
        analysisMap

-- | Repair an analysis section from an arbitrary canonical row source.
--
-- The ordinary structural store and a contextual annotated view are merely
-- two interpretations of the same dependency-component solver.  Keeping the
-- solver row-parametric prevents contextual analysis from growing a second,
-- subtly different rebuild engine.
repairAnalysisFromRows ::
  Language f =>
  AnalysisSpec f a ->
  (ClassId -> ClassId) ->
  (Int -> [ENode f]) ->
  IntMap.IntMap (IntMap.IntMap Int) ->
  IntSet ->
  IntSet ->
  IntMap.IntMap a ->
  IntMap.IntMap a
repairAnalysisFromRows spec canonicalize tuplesAt childrenWithin frozenKeys repairKeys initialAnalysis =
  foldl' solveComponent initialAnalysis components
  where
    repairKeyList =
      IntSet.toAscList repairKeys

    localByKey =
      IntMap.fromList (zip repairKeyList [0 ..])

    keyByLocal =
      IntMap.fromList (zip [0 ..] repairKeyList)

    components =
      Graph.stronglyConnComp
        [ (localKey, localKey, IntSet.toAscList (localDependencies localKey))
          | localKey <- IntMap.keys keyByLocal
        ]

    solveComponent repairedAnalysis component =
      case component of
        Graph.AcyclicSCC localKey ->
          case evaluateLocal repairedAnalysis localKey of
            Just (key, value)
              | IntSet.member key frozenKeys && IntSet.null (localDependencies localKey) ->
                  repairedAnalysis
              | otherwise ->
                  IntMap.insert key value repairedAnalysis
            Nothing ->
              repairedAnalysis
        Graph.CyclicSCC _ ->
          repairCyclicComponent repairedAnalysis component

    evaluateLocal repairedAnalysis localKey =
      case IntMap.lookup localKey keyByLocal of
        Nothing ->
          Nothing
        Just key ->
          fmap
            ( (,) key
                . preserveSeededAnalysis key
            )
            (recomputeAnalysisValueFromENodes spec (solverAnalysisValue repairedAnalysis) (tuplesAt key))

    -- A canonical union root already contains the join of its absorbed
    -- classes. Structural recomputation may add information, but it must not
    -- erase analysis seeded outside 'asMake' (for example HIE type evidence).
    -- The same law applies to ordinary and regional rebuilds because both
    -- call this solver with their canonical union roots as frozen keys.
    preserveSeededAnalysis key recomputed
      | IntSet.member key frozenKeys =
          maybe recomputed (\seeded -> asJoin spec seeded recomputed) (IntMap.lookup key initialAnalysis)
      | otherwise =
          recomputed

    solverAnalysisValue repairedAnalysis classIdValue =
      IntMap.lookup (classIdKey (canonicalize classIdValue)) repairedAnalysis

    localDependencies localKey =
      maybe IntSet.empty dependenciesForKey (IntMap.lookup localKey keyByLocal)

    dependenciesForKey key =
      IntSet.fromList
        [ localChild
          | childKey <- IntMap.keys (IntMap.findWithDefault IntMap.empty key childrenWithin),
            Just localChild <- [IntMap.lookup childKey localByKey]
        ]

    repairCyclicComponent repairedAnalysis component =
      case component of
        Graph.AcyclicSCC _ ->
          repairedAnalysis
        Graph.CyclicSCC localKeys ->
          let roundBudget =
                max 1 (length localKeys)
              (boundedAnalysis, _) =
                foldl'
                  ( \(currentAnalysis, continue) _ ->
                      if continue
                        then case cyclicRepairRound currentAnalysis localKeys of
                          CyclicRepairDescended nextAnalysis ->
                            (nextAnalysis, True)
                          CyclicRepairStable ->
                            (currentAnalysis, False)
                          CyclicRepairPreserved ->
                            (currentAnalysis, False)
                        else (currentAnalysis, False)
                  )
                  (repairedAnalysis, True)
                  [1 .. roundBudget]
           in boundedAnalysis

    cyclicRepairRound repairedAnalysis localKeys =
      let proposed =
            traverse (cyclicProposal repairedAnalysis) localKeys
       in case proposed of
            Nothing ->
              CyclicRepairPreserved
            Just proposals
              | any proposalAscends proposals ->
                  CyclicRepairPreserved
              | any proposalChanges proposals ->
                  CyclicRepairDescended
                    ( foldl'
                        ( \current (key, _, recomputed) ->
                            IntMap.insert key recomputed current
                        )
                        repairedAnalysis
                        proposals
                    )
              | otherwise ->
                  CyclicRepairStable

    cyclicProposal repairedAnalysis localKey = do
      (key, recomputed) <- evaluateLocal repairedAnalysis localKey
      existing <- IntMap.lookup key repairedAnalysis
      pure (key, existing, recomputed)

    proposalAscends (_, existing, recomputed) =
      snd (asJoinChanged spec existing recomputed)

    proposalChanges (_, existing, recomputed) =
      snd (asJoinChanged spec recomputed existing)

type CyclicRepairStep :: Type -> Type
data CyclicRepairStep a
  = CyclicRepairDescended !(IntMap.IntMap a)
  | CyclicRepairStable
  | CyclicRepairPreserved

recomputeAnalysisValueFromENodes ::
  Language f =>
  AnalysisSpec f a ->
  (ClassId -> Maybe a) ->
  [ENode f] ->
  Maybe a
recomputeAnalysisValueFromENodes spec childValue enodes =
  case NE.nonEmpty =<< traverse recomputeENode enodes of
    Nothing ->
      Nothing
    Just (firstAnalysis NE.:| remainingAnalysis) ->
      Just (foldl' (asJoin spec) firstAnalysis remainingAnalysis)
  where
    recomputeENode (ENode children) =
      asMake spec <$> traverse childValue children

mergeAbsorbedAnalysis ::
  (ClassId -> ClassId) ->
  IntSet ->
  IntMap.IntMap a ->
  AnalysisSpec f a ->
  IntMap.IntMap a
mergeAbsorbedAnalysis canonicalize impactedKeys analysisMap spec =
  let absorbed =
        [ (key, canonicalKey, value)
          | key <- IntSet.toList impactedKeys,
            Just value <- [IntMap.lookup key analysisMap],
            let canonicalKey = classIdKey (canonicalize (ClassId key)),
            key /= canonicalKey
        ]
      absorbedKeys =
        IntSet.fromList [key | (key, _, _) <- absorbed]
      deleted =
        IntMap.withoutKeys analysisMap absorbedKeys
   in foldl'
        ( \entries (_, canonicalKey, value) ->
            IntMap.insertWith
              (flip (asJoin spec))
              canonicalKey
              value
              entries
        )
        deleted
        absorbed

recomputeAnalysisFromENodes ::
  Language f =>
  AnalysisSpec f a ->
  (ClassId -> Maybe a) ->
  [ENode f] ->
  IntMap.IntMap a ->
  Int ->
  Maybe a
recomputeAnalysisFromENodes spec childValue enodes analysisMap key =
  case NE.nonEmpty =<< traverse recomputeENode enodes of
    Nothing ->
      Nothing
    Just (firstAnalysis NE.:| remainingAnalysis) ->
      let recomputed =
            foldl' (asJoin spec) firstAnalysis remainingAnalysis
       in case IntMap.lookup key analysisMap of
            Just existing ->
              let (joinedValue, didChange) =
                    asJoinChanged spec existing recomputed
               in if didChange then Just joinedValue else Nothing
            Nothing ->
              Just recomputed
  where
    recomputeENode (ENode children) =
      asMake spec <$> traverse childValue children

recomputeClassFromStore ::
  Language f =>
  AnalysisSpec f a ->
  (ClassId -> ClassId) ->
  StructuralStore f ->
  IntMap.IntMap a ->
  Int ->
  Maybe a
recomputeClassFromStore spec canonicalize store analysisMap key =
  recomputeClassFromTuples
    spec
    canonicalize
    (`structuralTuplesForResultKey` store)
    analysisMap
    key

recomputeClassFromTuples ::
  Language f =>
  AnalysisSpec f a ->
  (ClassId -> ClassId) ->
  (Int -> [ENode f]) ->
  IntMap.IntMap a ->
  Int ->
  Maybe a
recomputeClassFromTuples spec canonicalize tuplesAt analysisMap key =
  recomputeAnalysisFromENodes
    spec
    (\classIdValue -> IntMap.lookup (canonKey classIdValue) analysisMap)
    (tuplesAt key)
    analysisMap
    key
  where
    canonKey =
      classIdKey . canonicalize
