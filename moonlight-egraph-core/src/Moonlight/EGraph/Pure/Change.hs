{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Pure.Change
  ( GraphPhase (..),
    InsertENodeChange (..),
    ObservedClassUnions,
    observedClassUnions,
    observedClassUnionsFromEditDelta,
    observedClassUnionPairs,
    observedClassUnionCount,
    observedClassUnionsNull,
    observedClassUnionKeys,
    EGraphRebuildTrace (..),
    EGraphMutationTrace (..),
    EGraphMutationResult (..),
    eGraphPhase,
    makeEGraphMutationTrace,
    makeEGraphMutationResult,
    emptyEGraphMutationTrace,
    appendEGraphMutationTrace,
    eGraphMutationTraceEffect,
  )
where

import Data.Foldable qualified as Foldable
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Semigroup (stimes)
import Moonlight.Core
  ( ClassId,
    classIdKey,
  )
import Moonlight.Core.EGraph.Program
  ( EGraphProgramEffect,
    emptyEGraphProgramEffect,
    insertedFreshNodeEffect,
    requiredClassMergeEffect,
  )
import Moonlight.EGraph.Pure.Delta
  ( ClassUnionPair,
    EGraphEditDelta,
    EGraphRebuildDelta (..),
    classUnionPair,
    classUnionPairClasses,
    eGraphEditDeltaClassUnions,
    eGraphEditDeltaNull,
  )
import Moonlight.EGraph.Pure.Structural.Store
  ( StructuralTuplePatch,
  )
import Moonlight.EGraph.Pure.Types.Core
  ( EGraphRevision,
  )
import Moonlight.EGraph.Pure.Types.Internal
  ( EGraph (..),
  )

type GraphPhase :: Type
data GraphPhase
  = Stable
  | Dirty
  deriving stock (Eq, Ord, Show)

type InsertENodeChange :: Type
data InsertENodeChange
  = InsertedFreshClass !ClassId
  | ReusedClassAnalysisChanged !ClassId
  | ReusedClassUnchanged !ClassId
  deriving stock (Eq, Ord, Show)

type ObservedClassUnions :: Type
newtype ObservedClassUnions = ObservedClassUnions
  { observedClassUnionPairsValue :: Seq ClassUnionPair
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup ObservedClassUnions where
  ObservedClassUnions leftPairs <> ObservedClassUnions rightPairs =
    ObservedClassUnions (leftPairs <> rightPairs)
  {-# INLINE (<>) #-}

instance Monoid ObservedClassUnions where
  mempty =
    ObservedClassUnions Seq.empty
  {-# INLINE mempty #-}

observedClassUnions ::
  [(ClassId, ClassId)] ->
  ObservedClassUnions
observedClassUnions pairs =
  ObservedClassUnions
    ( Seq.fromList
        [ classUnionPair leftClassId rightClassId
          | (leftClassId, rightClassId) <- pairs,
            leftClassId /= rightClassId
        ]
    )
{-# INLINE observedClassUnions #-}

observedClassUnionsFromEditDelta ::
  EGraphEditDelta ->
  ObservedClassUnions
observedClassUnionsFromEditDelta =
  observedClassUnions . eGraphEditDeltaClassUnions
{-# INLINE observedClassUnionsFromEditDelta #-}

observedClassUnionPairs ::
  ObservedClassUnions ->
  [(ClassId, ClassId)]
observedClassUnionPairs =
  fmap classUnionPairClasses . Foldable.toList . observedClassUnionPairsValue
{-# INLINE observedClassUnionPairs #-}

observedClassUnionCount ::
  ObservedClassUnions ->
  Int
observedClassUnionCount =
  Seq.length . observedClassUnionPairsValue
{-# INLINE observedClassUnionCount #-}

observedClassUnionsNull ::
  ObservedClassUnions ->
  Bool
observedClassUnionsNull =
  Seq.null . observedClassUnionPairsValue
{-# INLINE observedClassUnionsNull #-}

observedClassUnionKeys ::
  ObservedClassUnions ->
  IntSet
observedClassUnionKeys observedUnions =
  foldMap
    (uncurry classUnionKeySet . classUnionPairClasses)
    (observedClassUnionPairsValue observedUnions)
{-# INLINE observedClassUnionKeys #-}

type EGraphRebuildTrace :: (Type -> Type) -> Type
data EGraphRebuildTrace f = EGraphRebuildTrace
  { egrtRebuildDelta :: !EGraphRebuildDelta,
    egrtTuplePatch :: !(StructuralTuplePatch f)
  }

type EGraphMutationTrace :: (Type -> Type) -> Type
data EGraphMutationTrace f = EGraphMutationTrace
  { emtRevisionBefore :: !EGraphRevision,
    emtRevisionAfter :: !EGraphRevision,
    emtPhaseBefore :: !GraphPhase,
    emtPhaseAfter :: !GraphPhase,
    emtTouchedClassKeys :: !IntSet,
    emtInsertedClassKeys :: !IntSet,
    emtAnalysisChangedKeys :: !IntSet,
    emtObservedClassUnions :: !ObservedClassUnions,
    emtRebuildTraces :: ![EGraphRebuildTrace f]
  }

type EGraphMutationResult :: (Type -> Type) -> Type -> Type -> Type
data EGraphMutationResult f analysis result = EGraphMutationResult
  { emrResult :: result,
    emrTrace :: !(EGraphMutationTrace f),
    emrGraph :: !(EGraph f analysis)
  }

instance Functor (EGraphMutationResult f analysis) where
  fmap transform mutationResult =
    mutationResult {emrResult = transform (emrResult mutationResult)}
  {-# INLINE fmap #-}

eGraphPhase ::
  EGraph f analysis ->
  GraphPhase
eGraphPhase graph =
  if eGraphEditDeltaNull (egPendingDelta graph)
    then Stable
    else Dirty
{-# INLINE eGraphPhase #-}

makeEGraphMutationTrace ::
  EGraph f analysis ->
  EGraph f analysis ->
  IntSet ->
  IntSet ->
  IntSet ->
  ObservedClassUnions ->
  [EGraphRebuildTrace f] ->
  EGraphMutationTrace f
makeEGraphMutationTrace beforeGraph afterGraph touchedClassKeys insertedClassKeys analysisChangedKeys observedUnions rebuildTraces =
  EGraphMutationTrace
    { emtRevisionBefore = egRevision beforeGraph,
      emtRevisionAfter = egRevision afterGraph,
      emtPhaseBefore = eGraphPhase beforeGraph,
      emtPhaseAfter = eGraphPhase afterGraph,
      emtTouchedClassKeys = touchedClassKeys,
      emtInsertedClassKeys = insertedClassKeys,
      emtAnalysisChangedKeys = analysisChangedKeys,
      emtObservedClassUnions = observedUnions,
      emtRebuildTraces = rebuildTraces
    }
{-# INLINE makeEGraphMutationTrace #-}

makeEGraphMutationResult ::
  result ->
  EGraph f analysis ->
  EGraph f analysis ->
  IntSet ->
  IntSet ->
  IntSet ->
  ObservedClassUnions ->
  [EGraphRebuildTrace f] ->
  EGraphMutationResult f analysis result
makeEGraphMutationResult result beforeGraph afterGraph touchedClassKeys insertedClassKeys analysisChangedKeys observedUnions rebuildTraces =
  EGraphMutationResult
    { emrResult = result,
      emrTrace =
        makeEGraphMutationTrace
          beforeGraph
          afterGraph
          touchedClassKeys
          insertedClassKeys
          analysisChangedKeys
          observedUnions
          rebuildTraces,
      emrGraph = afterGraph
    }
{-# INLINE makeEGraphMutationResult #-}

emptyEGraphMutationTrace ::
  EGraph f analysis ->
  EGraphMutationTrace f
emptyEGraphMutationTrace graph =
  makeEGraphMutationTrace
    graph
    graph
    IntSet.empty
    IntSet.empty
    IntSet.empty
    mempty
    []
{-# INLINE emptyEGraphMutationTrace #-}

appendEGraphMutationTrace ::
  EGraphMutationTrace f ->
  EGraphMutationTrace f ->
  EGraphMutationTrace f
appendEGraphMutationTrace leftTrace rightTrace =
  EGraphMutationTrace
    { emtRevisionBefore = emtRevisionBefore leftTrace,
      emtRevisionAfter = emtRevisionAfter rightTrace,
      emtPhaseBefore = emtPhaseBefore leftTrace,
      emtPhaseAfter = emtPhaseAfter rightTrace,
      emtTouchedClassKeys = emtTouchedClassKeys leftTrace <> emtTouchedClassKeys rightTrace,
      emtInsertedClassKeys = emtInsertedClassKeys leftTrace <> emtInsertedClassKeys rightTrace,
      emtAnalysisChangedKeys = emtAnalysisChangedKeys leftTrace <> emtAnalysisChangedKeys rightTrace,
      emtObservedClassUnions = emtObservedClassUnions leftTrace <> emtObservedClassUnions rightTrace,
      emtRebuildTraces = emtRebuildTraces leftTrace <> emtRebuildTraces rightTrace
    }
{-# INLINE appendEGraphMutationTrace #-}

eGraphMutationTraceEffect ::
  EGraphMutationTrace f ->
  EGraphProgramEffect
eGraphMutationTraceEffect traceValue =
  effectTimes
    (IntSet.size (emtInsertedClassKeys traceValue))
    insertedFreshNodeEffect
    <> effectTimes
      (observedClassUnionCount (emtObservedClassUnions traceValue))
      requiredClassMergeEffect
{-# INLINE eGraphMutationTraceEffect #-}

effectTimes ::
  Int ->
  EGraphProgramEffect ->
  EGraphProgramEffect
effectTimes count effectValue =
  if count <= 0
    then emptyEGraphProgramEffect
    else stimes count effectValue
{-# INLINE effectTimes #-}

classUnionKeySet :: ClassId -> ClassId -> IntSet
classUnionKeySet leftClassId rightClassId =
  IntSet.fromList [classIdKey leftClassId, classIdKey rightClassId]
{-# INLINE classUnionKeySet #-}
