module Moonlight.EGraph.Pure.Kernel.HashCons
  ( canonicalizeENode,
    canonicalizeENodePure,
    canonicalizeENodeByTheory,
    lookupENodeAll,
    lookupLeastENode,
    addENode,
    insertENodeTracked,
    addTerm,
    insertTermTracked,
    insertTermsTracked,
    insertTermTrackedWithClassFootprint,
  )
where

import Control.Monad.Trans.State.Strict (StateT (..), runStateT)
import Data.Foldable (toList)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import Moonlight.Core (Language)
import Moonlight.Core (scanMap)
import Moonlight.Core (TheorySpec, canonicalizeLayerByTheory)
import Moonlight.EGraph.Pure.Analysis (asJoinChanged, asMake)
import Moonlight.EGraph.Pure.Change
  ( EGraphMutationResult (..),
    EGraphMutationTrace,
    InsertENodeChange (..),
    appendEGraphMutationTrace,
    emptyEGraphMutationTrace,
    makeEGraphMutationTrace,
    makeEGraphMutationResult,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    ENode (..),
    classIdKey,
  )
import Moonlight.EGraph.Pure.Types.Internal (EGraph (..))
import Moonlight.EGraph.Pure.Types.Internal qualified as EGraph
import Moonlight.EGraph.Pure.Delta (classUnionsDelta)
import Moonlight.EGraph.Pure.Structural.Store
  ( StructuralLookup (..),
    insertCanonicalTuple,
    seCongruenceObstructions,
    seStore,
    structuralLookupLeast,
    structuralLookupTupleAll,
  )
import Moonlight.Core qualified as UnionFind
import Data.Fix (Fix (..))

canonicalizeENode :: Language f => ENode f -> UnionFind.UnionFind -> (ENode f, UnionFind.UnionFind)
canonicalizeENode (ENode childClassIds) unionFind =
  let (updatedUnionFind, canonicalChildren) =
        scanMap canonicalizeClassId unionFind childClassIds
   in (ENode canonicalChildren, updatedUnionFind)
  where
    canonicalizeClassId currentUnionFind classId =
      let (rootClassId, nextUnionFind) = UnionFind.find classId currentUnionFind
       in (nextUnionFind, rootClassId)

canonicalizeENodePure :: Functor f => (ClassId -> ClassId) -> ENode f -> ENode f
canonicalizeENodePure canonicalize (ENode childClassIds) =
  ENode (fmap canonicalize childClassIds)

canonicalizeENodeByTheory :: Language f => TheorySpec f -> ENode f -> ENode f
canonicalizeENodeByTheory spec (ENode node) =
  ENode (canonicalizeLayerByTheory spec node)

lookupENodeAll :: Language f => ENode f -> EGraph f a -> StructuralLookup
lookupENodeAll enode graph =
  let (classIdCanonical, updatedUnionFind) = canonicalizeENode enode (egUnionFind graph)
      theoryCanonical = canonicalizeENodeByTheory (egTheorySpec graph) classIdCanonical
      canonicalizeResult resultClassId =
        fst (UnionFind.find resultClassId updatedUnionFind)
   in canonicalizeStructuralLookup canonicalizeResult (structuralLookupTupleAll theoryCanonical (egStore graph))

lookupLeastENode :: Language f => ENode f -> EGraph f a -> Maybe ClassId
lookupLeastENode enode =
  structuralLookupLeast . lookupENodeAll enode

addENode :: Language f => ENode f -> a -> EGraph f a -> Either UnionFind.UnionFindAllocationError (ClassId, EGraph f a)
addENode enode analysisData graph = do
  EGraphMutationResult
    { emrResult = classId,
      emrGraph = updatedGraph
    } <- insertENodeTracked enode analysisData graph
  pure (classId, updatedGraph)

insertENodeTracked :: Language f => ENode f -> a -> EGraph f a -> Either UnionFind.UnionFindAllocationError (EGraphMutationResult f a ClassId)
insertENodeTracked enode analysisData graph =
  fmap
    (\result -> result {emrResult = fst (emrResult result)})
    (insertENodeTrackedWithAnalysis enode analysisData graph)

addTerm :: Language f => Fix f -> EGraph f a -> Either UnionFind.UnionFindAllocationError (ClassId, EGraph f a)
addTerm term graph = do
  EGraphMutationResult
    { emrResult = classId,
      emrGraph = updatedGraph
    } <- insertTermTracked term graph
  pure (classId, updatedGraph)

insertTermTracked :: Language f => Fix f -> EGraph f a -> Either UnionFind.UnionFindAllocationError (EGraphMutationResult f a ClassId)
insertTermTracked term graph =
  fmap
    (\result -> result {emrResult = fst (emrResult result)})
    (insertTermTrackedWithClassFootprint term graph)

insertTermsTracked :: Language f => [Fix f] -> EGraph f a -> Either UnionFind.UnionFindAllocationError (EGraphMutationResult f a [ClassId])
insertTermsTracked terms graph = do
  (classIds, batchState) <-
    runStateT (traverse insertTermIntoBatch terms) (TermBatchInsertState graph mempty)
  let updatedGraph =
        tbisGraph batchState
      insertedKeys =
        fst (tbisChangeKeys batchState)
      analysisChangedKeys =
        snd (tbisChangeKeys batchState)
  pure
    ( makeEGraphMutationResult
        classIds
        graph
        updatedGraph
        (insertedKeys <> analysisChangedKeys)
        insertedKeys
        analysisChangedKeys
        mempty
        []
    )
{-# INLINE insertTermsTracked #-}

data TermBatchInsertState f a = TermBatchInsertState
  { tbisGraph :: !(EGraph f a),
    tbisChangeKeys :: !(IntSet, IntSet)
  }

insertTermIntoBatch ::
  Language f =>
  Fix f ->
  StateT (TermBatchInsertState f a) (Either UnionFind.UnionFindAllocationError) ClassId
insertTermIntoBatch term =
  StateT $ \state -> do
    (classId, _analysisData, changeKeys, updatedGraph) <-
      insertTermWithMonoidalAnalysis
        (\_beforeGraph _afterGraph _classId -> insertENodeChangeKeys)
        term
        (tbisGraph state)
    pure
      ( classId,
        state
          { tbisGraph = updatedGraph,
            tbisChangeKeys = tbisChangeKeys state <> changeKeys
          }
      )
{-# INLINE insertTermIntoBatch #-}

insertTermTrackedWithClassFootprint :: Language f => Fix f -> EGraph f a -> Either UnionFind.UnionFindAllocationError (EGraphMutationResult f a (ClassId, IntSet))
insertTermTrackedWithClassFootprint term graph = do
  (classId, _analysisData, (traceValues, supportFootprint), updatedGraph) <-
    insertTermWithMonoidalAnalysis trackedTermChange term graph
  pure
    ( EGraphMutationResult
        { emrResult = (classId, supportFootprint),
          emrTrace =
            foldl'
              appendEGraphMutationTrace
              (emptyEGraphMutationTrace graph)
              traceValues,
          emrGraph = updatedGraph
        }
    )

insertTermWithMonoidalAnalysis ::
  Language f =>
  Monoid w =>
  (EGraph f a -> EGraph f a -> ClassId -> InsertENodeChange -> w) ->
  Fix f ->
  EGraph f a ->
  Either UnionFind.UnionFindAllocationError (ClassId, a, w, EGraph f a)
insertTermWithMonoidalAnalysis observeChange (Fix termLayer) graph = do
  (childResults, graphAfterChildren) <-
    runStateT (traverse addChildTerm termLayer) graph
  let childClassIds =
        fmap (\(childClassId, _, _) -> childClassId) childResults
      childAnalysisData =
        fmap (\(_, childData, _) -> childData) childResults
      childChange =
        foldMap (\(_, _, childChangeValue) -> childChangeValue) childResults
      nodeAnalysisData =
        asMake (egAnalysisSpec graphAfterChildren) childAnalysisData
  (classId, analysisData, change, updatedGraph) <-
    addENodeDetailed (ENode childClassIds) nodeAnalysisData graphAfterChildren
  pure
    ( classId,
      analysisData,
      childChange <> observeChange graphAfterChildren updatedGraph classId change,
      updatedGraph
    )
  where
    addChildTerm childTerm =
      StateT $ \currentGraph -> do
        (childClassId, childData, childChange, nextGraph) <-
          insertTermWithMonoidalAnalysis observeChange childTerm currentGraph
        pure ((childClassId, childData, childChange), nextGraph)
{-# INLINE insertTermWithMonoidalAnalysis #-}

trackedTermChange ::
  EGraph f a ->
  EGraph f a ->
  ClassId ->
  InsertENodeChange ->
  ([EGraphMutationTrace f], IntSet)
trackedTermChange beforeGraph afterGraph classId change =
  ([traceValue], IntSet.singleton (classIdKey classId))
  where
    (insertedKeys, analysisChangedKeys) =
      insertENodeChangeKeys change
    touchedKeys =
      insertedKeys <> analysisChangedKeys
    traceValue =
      makeEGraphMutationTrace
        beforeGraph
        afterGraph
        touchedKeys
        insertedKeys
        analysisChangedKeys
        mempty
        []

insertENodeTrackedWithAnalysis ::
  Language f =>
  ENode f ->
  a ->
  EGraph f a ->
  Either UnionFind.UnionFindAllocationError (EGraphMutationResult f a (ClassId, a))
insertENodeTrackedWithAnalysis enode analysisData graph = do
  (classId, updatedAnalysisData, change, updatedGraph) <-
    addENodeDetailed enode analysisData graph
  let (insertedKeys, analysisChangedKeys) =
        insertENodeChangeKeys change
      touchedKeys =
        insertedKeys <> analysisChangedKeys
  pure
    ( makeEGraphMutationResult
        (classId, updatedAnalysisData)
        graph
        updatedGraph
        touchedKeys
        insertedKeys
        analysisChangedKeys
        mempty
        []
    )

addENodeDetailed :: Language f => ENode f -> a -> EGraph f a -> Either UnionFind.UnionFindAllocationError (ClassId, a, InsertENodeChange, EGraph f a)
addENodeDetailed enode analysisData graph =
  let (classIdCanonical, updatedUnionFind) = canonicalizeENode enode (egUnionFind graph)
      canonicalENode = canonicalizeENodeByTheory (egTheorySpec graph) classIdCanonical
      graphWithCanonicalChildren = graph {egUnionFind = updatedUnionFind}
      canonicalizeResult resultClassId =
        fst (UnionFind.find resultClassId updatedUnionFind)
   in case canonicalizeStructuralLookup canonicalizeResult (structuralLookupTupleAll canonicalENode (egStore graphWithCanonicalChildren)) of
        StructuralMissing ->
          createFreshClass canonicalENode analysisData graphWithCanonicalChildren
        StructuralUnique existingClassId ->
          pure (attachToExistingClass existingClassId analysisData graphWithCanonicalChildren)
        StructuralAmbiguous owners ->
          pure (attachToAmbiguousExistingClass owners analysisData graphWithCanonicalChildren)

attachToExistingClass ::
  ClassId ->
  a ->
  EGraph f a ->
  (ClassId, a, InsertENodeChange, EGraph f a)
attachToExistingClass existingClassId analysisData graph =
  let (rootClassId, updatedUnionFind) = UnionFind.find existingClassId (egUnionFind graph)
      rootKey = classIdKey rootClassId
      maybeExistingAnalysis =
        IntMap.lookup rootKey (egAnalysis graph)
      (updatedAnalysisData, analysisChanged) =
        maybe
          (analysisData, True)
          (\existingData -> asJoinChanged (egAnalysisSpec graph) existingData analysisData)
          maybeExistingAnalysis
      updatedGraph =
        let graphWithCompressedUnionFind =
              graph {egUnionFind = updatedUnionFind}
         in if analysisChanged
              then
                EGraph.bumpEGraphRevision
                  ( graphWithCompressedUnionFind
                      { egAnalysis = IntMap.insert rootKey updatedAnalysisData (egAnalysis graph)
                      }
                  )
              else graphWithCompressedUnionFind
      change =
        if analysisChanged
          then ReusedClassAnalysisChanged rootClassId
          else ReusedClassUnchanged rootClassId
   in (rootClassId, updatedAnalysisData, change, updatedGraph)

attachToAmbiguousExistingClass ::
  NonEmpty ClassId ->
  a ->
  EGraph f a ->
  (ClassId, a, InsertENodeChange, EGraph f a)
attachToAmbiguousExistingClass owners analysisData graph =
  attachToExistingClass representative analysisData graphWithPendingUnions
  where
    representative :| otherOwners =
      owners
    graphWithPendingUnions =
      stageClassUnions (fmap (\owner -> (representative, owner)) otherOwners) graph

createFreshClass :: Language f => ENode f -> a -> EGraph f a -> Either UnionFind.UnionFindAllocationError (ClassId, a, InsertENodeChange, EGraph f a)
createFreshClass canonicalENode analysisData graph = do
  (newClassId, nextUnionFind) <- UnionFind.makeSet (egUnionFind graph)
  let updatedGraph =
        let structuralEdit =
              insertCanonicalTuple
                newClassId
                canonicalENode
                (egStore graph)
            pendingUnions =
              classUnionsDelta (seCongruenceObstructions structuralEdit)
         in EGraph.bumpEGraphRevision
          ( graph
              { egUnionFind = nextUnionFind,
                egStore = seStore structuralEdit,
                egAnalysis = IntMap.insert (classIdKey newClassId) analysisData (egAnalysis graph),
                egPendingDelta = egPendingDelta graph <> pendingUnions
              }
          )
  pure (newClassId, analysisData, InsertedFreshClass newClassId, updatedGraph)

insertENodeChangeKeys ::
  InsertENodeChange ->
  (IntSet, IntSet)
insertENodeChangeKeys change =
  case change of
    InsertedFreshClass classId ->
      let classKey = IntSet.singleton (classIdKey classId)
       in (classKey, classKey)
    ReusedClassAnalysisChanged classId ->
      (IntSet.empty, IntSet.singleton (classIdKey classId))
    ReusedClassUnchanged _classId ->
      (IntSet.empty, IntSet.empty)

stageClassUnions :: [(ClassId, ClassId)] -> EGraph f a -> EGraph f a
stageClassUnions classUnions graph =
  case classUnions of
    [] ->
      graph
    _ ->
      EGraph.bumpEGraphRevision
        ( graph
            { egPendingDelta =
                egPendingDelta graph <> classUnionsDelta classUnions
            }
        )
{-# INLINE stageClassUnions #-}

canonicalizeStructuralLookup :: (ClassId -> ClassId) -> StructuralLookup -> StructuralLookup
canonicalizeStructuralLookup canonicalize lookupResult =
  structuralLookupFromClassKeys $
    case lookupResult of
      StructuralMissing ->
        IntSet.empty
      StructuralUnique classId ->
        IntSet.singleton (classIdKey (canonicalize classId))
      StructuralAmbiguous classIds ->
        IntSet.fromList (fmap (classIdKey . canonicalize) (toList classIds))
{-# INLINE canonicalizeStructuralLookup #-}

structuralLookupFromClassKeys :: IntSet -> StructuralLookup
structuralLookupFromClassKeys classKeys =
  case fmap ClassId (IntSet.toAscList classKeys) of
    [] ->
      StructuralMissing
    [classId] ->
      StructuralUnique classId
    classId : otherClassIds ->
      StructuralAmbiguous (classId :| otherClassIds)
{-# INLINE structuralLookupFromClassKeys #-}
