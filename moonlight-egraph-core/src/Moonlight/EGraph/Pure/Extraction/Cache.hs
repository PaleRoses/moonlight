{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Pure.Extraction.Cache
  ( ExtractionChoiceCache,
    eccRevision,
    ExtractionCacheObstruction (..),
    extractionChoiceCacheFromStableGraph,
    advanceExtractionChoiceCache,
    mutationTraceDirtyKeys,
    extractionChoiceCacheSection,
    extractCached,
    extractAllCached,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Moonlight.Core (Language)
import Moonlight.Core
  ( reachabilityFromInt,
  )
import Moonlight.EGraph.Pure.Change
  ( EGraphMutationTrace (..),
    EGraphRebuildTrace (..),
    GraphPhase (..),
    eGraphPhase,
    observedClassUnionPairs,
  )
import Moonlight.EGraph.Pure.Delta
  ( EGraphRebuildDelta (..),
  )
import Moonlight.EGraph.Pure.Extraction.Algebra
  ( ExtractionChoiceSection,
    extractAllFromChoiceSection,
    extractFromChoiceSection,
    extractionChoiceSectionChoices,
    extractionChoiceSectionCostAlgebra,
    extractionChoiceSectionFromChoices,
  )
import Moonlight.EGraph.Pure.Extraction.Core
  ( AnalysisCostAlgebra,
    BestChoice,
    ExtractionBudgetExhaustion,
    ExtractionWorkBudget,
    ExtractionResult,
    ExtractionTable,
    extractionCanonicalClass,
    extractionClasses,
    stableExtractionSnapshotFromEGraph,
    stableExtractionSnapshotTable,
  )
import Moonlight.EGraph.Pure.Extraction.Worklist
  ( ChoiceParentIndex,
    choiceParentIndex,
    worklistChoices,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId (..),
    EGraph,
    EGraphRevision,
    classIdKey,
    eGraphRevision,
  )

type ExtractionChoiceCache :: (Type -> Type) -> Type -> Type -> Type
data ExtractionChoiceCache f a cost = ExtractionChoiceCache
  { eccRevision :: !EGraphRevision,
    eccSection :: !(ExtractionChoiceSection f a cost)
  }

type ExtractionCacheObstruction :: Type
data ExtractionCacheObstruction
  = ExtractionCacheGraphUnstable
  | ExtractionCacheLineageMismatch !EGraphRevision !EGraphRevision
  | ExtractionCacheBudgetExhausted !ExtractionBudgetExhaustion
  deriving stock (Eq, Show)

extractionChoiceCacheFromStableGraph ::
  (Language f, Ord cost) =>
  ExtractionWorkBudget ->
  AnalysisCostAlgebra f a cost ->
  EGraph f a ->
  Either ExtractionCacheObstruction (ExtractionChoiceCache f a cost)
extractionChoiceCacheFromStableGraph budget costAlgebraValue graph =
  case stableExtractionSnapshotFromEGraph graph of
    Nothing ->
      Left ExtractionCacheGraphUnstable
    Just snapshot ->
      let table =
            stableExtractionSnapshotTable snapshot
       in case worklistChoices budget costAlgebraValue table IntMap.empty of
            Left report ->
              Left (ExtractionCacheBudgetExhausted report)
            Right choices ->
              Right
                ExtractionChoiceCache
                  { eccRevision = eGraphRevision graph,
                    eccSection = extractionChoiceSectionFromChoices costAlgebraValue table choices
                  }

-- | Fine-grained cache advance: classes outside the parents-closure cone of
-- the trace's dirty keys keep their finalized choices verbatim and act as the
-- worklist boundary; the cone — where analysis changes may move costs in
-- either direction — is discarded and re-finalized from scratch.  Every cached
-- and seeded key is transported through the after-graph's canonicalizer, so no
-- pre-union class id survives a merge.
advanceExtractionChoiceCache ::
  (Language f, Ord cost) =>
  ExtractionWorkBudget ->
  EGraphMutationTrace f ->
  EGraph f a ->
  ExtractionChoiceCache f a cost ->
  Either ExtractionCacheObstruction (ExtractionChoiceCache f a cost)
advanceExtractionChoiceCache budget traceValue graph cacheValue
  | eccRevision cacheValue /= emtRevisionBefore traceValue =
      Left (ExtractionCacheLineageMismatch (eccRevision cacheValue) (emtRevisionBefore traceValue))
  | eGraphRevision graph /= emtRevisionAfter traceValue =
      Left (ExtractionCacheLineageMismatch (eGraphRevision graph) (emtRevisionAfter traceValue))
  | eGraphPhase graph /= Stable =
      Left ExtractionCacheGraphUnstable
  | otherwise =
      case stableExtractionSnapshotFromEGraph graph of
        Nothing ->
          Left ExtractionCacheGraphUnstable
        Just snapshot ->
          let table =
                stableExtractionSnapshotTable snapshot
              parentIndex =
                choiceParentIndex table
           in case coneAdvanceChoices budget costAlgebraValue table parentIndex (mutationTraceDirtyKeys traceValue) previousChoices of
                Left report ->
                  Left (ExtractionCacheBudgetExhausted report)
                Right choices ->
                  Right
                    ExtractionChoiceCache
                      { eccRevision = eGraphRevision graph,
                        eccSection = extractionChoiceSectionFromChoices costAlgebraValue table choices
                      }
  where
    costAlgebraValue =
      extractionChoiceSectionCostAlgebra (eccSection cacheValue)

    previousChoices =
      extractionChoiceSectionChoices (eccSection cacheValue)

-- | Table-level cone advance: the shared heart of every maintained extraction
-- view (the whole-graph cache above, per-context section caches in the
-- saturation layer).  The supplied parent index MUST be 'choiceParentIndex'
-- of the supplied table.  Previous choices survive iff their key is canonical
-- in the table, still present, and outside the parents-closure cone of the
-- (canonically transported) dirty seed; survivors enter the worklist as the
-- pre-finalized boundary and everything else is re-finalized from scratch.
coneAdvanceChoices ::
  forall f a cost.
  (Language f, Ord cost) =>
  ExtractionWorkBudget ->
  AnalysisCostAlgebra f a cost ->
  ExtractionTable f a ->
  ChoiceParentIndex ->
  IntSet ->
  IntMap (Maybe (BestChoice f cost)) ->
  Either ExtractionBudgetExhaustion (IntMap (Maybe (BestChoice f cost)))
coneAdvanceChoices budget costAlgebraValue table parentIndex dirtyKeys previousChoices =
  worklistChoices budget costAlgebraValue table boundary
  where
    cone :: IntSet
    cone =
      parentClosureCone parentIndex (transportKeys table dirtyKeys)

    boundary :: IntMap (Maybe (BestChoice f cost))
    boundary =
      IntMap.filterWithKey surviving previousChoices

    surviving :: Int -> Maybe (BestChoice f cost) -> Bool
    surviving classKey choice =
      case choice of
        Nothing ->
          False
        Just _bestChoice ->
          not (IntSet.member classKey cone)
            && extractionCanonicalClass table (ClassId classKey) == Just (ClassId classKey)
            && IntMap.member classKey (extractionClasses table)

parentClosureCone :: ChoiceParentIndex -> IntSet -> IntSet
parentClosureCone parentIndex seedKeys =
  reachabilityFromInt
    (\classKey -> IntMap.findWithDefault IntSet.empty classKey parentIndex)
    seedKeys
{-# INLINE parentClosureCone #-}

-- | Every class key a mutation trace can have perturbed: touched, inserted,
-- analysis-changed, both sides of every observed union, and the key sets of
-- every rebuild delta.  This is the dirty seed for cone advances.
mutationTraceDirtyKeys :: EGraphMutationTrace f -> IntSet
mutationTraceDirtyKeys traceValue =
  IntSet.unions
    ( [ emtTouchedClassKeys traceValue,
        emtInsertedClassKeys traceValue,
        emtAnalysisChangedKeys traceValue,
        IntSet.fromList
          [ classIdKey classId
          | (leftClass, rightClass) <- observedClassUnionPairs (emtObservedClassUnions traceValue),
            classId <- [leftClass, rightClass]
          ]
      ]
        <> fmap rebuildDirtyKeys (emtRebuildTraces traceValue)
    )
  where
    rebuildDirtyKeys :: EGraphRebuildTrace g -> IntSet
    rebuildDirtyKeys rebuildTrace =
      let delta = egrtRebuildDelta rebuildTrace
       in IntSet.unions
            [ erdImpactedClassKeys delta,
              erdDirtyResultKeys delta,
              erdTopologyClassKeys delta
            ]

transportKeys :: ExtractionTable f a -> IntSet -> IntSet
transportKeys table =
  IntSet.foldl' addTransported IntSet.empty
  where
    addTransported acc rawKey =
      case extractionCanonicalClass table (ClassId rawKey) of
        Just canonicalId ->
          IntSet.insert (classIdKey canonicalId) acc
        Nothing ->
          acc

extractionChoiceCacheSection :: ExtractionChoiceCache f a cost -> ExtractionChoiceSection f a cost
extractionChoiceCacheSection =
  eccSection

extractCached :: (Language f, Ord cost) => ClassId -> ExtractionChoiceCache f a cost -> Maybe (ExtractionResult f cost)
extractCached classId =
  extractFromChoiceSection classId . extractionChoiceCacheSection

extractAllCached :: (Language f, Ord cost) => ExtractionChoiceCache f a cost -> IntMap (ExtractionResult f cost)
extractAllCached =
  extractAllFromChoiceSection . extractionChoiceCacheSection
