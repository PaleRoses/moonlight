module Moonlight.EGraph.Pure.Saturation.Extraction
  ( ContextualExtractionObstruction (..),
    ContextScope (..),
    ContextualExtractionSection,
    cesContext,
    cesChoiceSection,
    contextualExtractionTable,
    contextualExtractionSectionBounded,
    contextualExtractionSectionWithAnalysisBounded,
    contextualExtractFromSection,
    contextualExtractionSectionMetrics,
    contextualExtractBounded,
    contextualExtractWithAnalysisBounded,
    ContextualExtractionMetrics (..),
    contextualExtractWithMetricsBounded,
    contextualExtractWithAnalysisAndMetricsBounded,
    contextualExtractionPartitionsBounded,
    contextualExtractionPartitionsWithAnalysisBounded,
    ContextualSectionCache,
    cscBaseRevision,
    cscContextRevision,
    cscSections,
    ContextualSectionObstruction (..),
    contextualSectionCacheBounded,
    advanceContextualSections,
  )
where

import Data.Bifunctor (first)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Moonlight.Core (Language)
import Moonlight.EGraph.Pure.Change
  ( EGraphMutationTrace (..),
    GraphPhase (..),
    eGraphPhase,
  )
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    ContextMutationTrace (..),
    ContextRebaseReport (..),
    contextCachedObjectsForExecution,
    contextPreparedObjects,
    materializeAmbientPayloadFor,
  )
import Moonlight.EGraph.Pure.Context.AnnotatedDelta
  ( contextAnnotatedDeltaBuckets,
  )
import Moonlight.EGraph.Pure.Context.AnnotatedView
  ( annotatedContextViewAtKey,
    annotatedViewCanonicalize,
    annotatedViewRowsByRepresentative,
  )
import Moonlight.EGraph.Pure.Context
  ( cegBase,
    cegContextRevision,
    cegSite,
  )
import Moonlight.EGraph.Pure.Extraction
  ( AnalysisCostAlgebra,
    CostAlgebra,
    ExtractionChoiceSection,
    ExtractionBudgetExhaustion,
    ExtractionWorkBudget,
    ExtractionResult,
    ExtractionTable,
    extractChoiceSectionBounded,
    extractFromTableBounded,
    extractFromChoiceSection,
    extractionChoiceSectionTable,
    extractionClass,
    extractionClassNodes,
    extractionClasses,
    extractionTable,
    liftCostAlgebra,
    mutationTraceDirtyKeys,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId,
    EGraphRevision,
    classIdKey,
    eGraphRevision,
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSupportError,
    contextObjectKeyFor,
    preparedContextRestrictsTo,
  )
import Moonlight.Sheaf.Section.Context.Payload
  ( ContextClassPayload (..),
  )
import Moonlight.Sheaf.Twist.Cost
  ( CostOverlay,
    applyCostOverlay,
  )
import Moonlight.Sheaf.Twist.Extraction qualified as SheafTwist
import Numeric.Natural (Natural)

type ContextualExtractionObstruction :: Type -> Type
data ContextualExtractionObstruction c
  = ContextualExtractionDirtyLocalizedGraph !c
  | ContextualExtractionInvalidTable !c
  | ContextualExtractionBudgetExhausted !ExtractionBudgetExhaustion
  | ContextualExtractionSupportSiteFailed !(PreparedContextSupportError c)
  deriving stock (Eq, Ord, Show)

type ContextualExtractionMetrics :: Type
data ContextualExtractionMetrics = ContextualExtractionMetrics
  { cemLocalizedNodeCount :: !Int,
    cemLocalizedClassCount :: !Int,
    cemLocalizedContextCount :: !Int,
    cemResultFound :: !Bool
  }
  deriving stock (Eq, Ord, Show)

type ContextScope :: Type -> Type
data ContextScope c
  = CachedObjects
  | Objects !(Set.Set c)
  | AllPreparedObjectsForDiagnostics
  deriving stock (Eq, Ord, Show)

type ContextualExtractionSection :: (Type -> Type) -> Type -> Type -> Type -> Type
data ContextualExtractionSection f a c cost = ContextualExtractionSection
  { cesContext :: !c,
    cesContextCount :: !Int,
    cesChoiceSection :: !(ExtractionChoiceSection f a cost)
  }

contextualExtractionTable ::
  (Language f, Ord c) =>
  c ->
  ContextEGraph owner f a c ->
  Either (ContextualExtractionObstruction c) (ExtractionTable f a)
contextualExtractionTable contextValue contextGraph
  | eGraphPhase baseGraph /= Stable =
      Left (ContextualExtractionDirtyLocalizedGraph contextValue)
  | otherwise = do
      contextKey <-
        first ContextualExtractionSupportSiteFailed
          (contextObjectKeyFor (cegSite contextGraph) contextValue)
      payloads <-
        first ContextualExtractionSupportSiteFailed
          (materializeAmbientPayloadFor contextValue contextGraph)
      let contextView =
            annotatedContextViewAtKey
              contextKey
              (contextAnnotatedDeltaBuckets contextGraph)
          rowsByRepresentative =
            annotatedViewRowsByRepresentative contextView baseGraph
          extractionClassMap =
            IntMap.mapWithKey
              ( \representativeKey payload ->
                  extractionClass
                    (ccpAnalysis payload)
                    (IntMap.findWithDefault [] representativeKey rowsByRepresentative)
              )
              payloads
          canonicalClass classId =
            let representative =
                  annotatedViewCanonicalize contextView baseGraph classId
             in if IntMap.member (classIdKey representative) extractionClassMap
                  then Just representative
                  else Nothing
      maybe
        (Left (ContextualExtractionInvalidTable contextValue))
        Right
        (extractionTable extractionClassMap canonicalClass)
  where
    baseGraph = cegBase contextGraph
{-# INLINE contextualExtractionTable #-}

contextualExtractBounded ::
  (Language f, Ord cost, Ord c) =>
  ExtractionWorkBudget ->
  c ->
  CostOverlay c (AnalysisCostAlgebra f a cost) ->
  CostAlgebra f cost ->
  ClassId ->
  ContextEGraph owner f a c ->
  Either (ContextualExtractionObstruction c) (Maybe (ExtractionResult f cost))
contextualExtractBounded budget contextValue overlayValue baseCost =
  contextualExtractWithAnalysisBounded budget contextValue overlayValue (liftCostAlgebra baseCost)

contextualExtractWithMetricsBounded ::
  (Language f, Ord cost, Ord c) =>
  ExtractionWorkBudget ->
  c ->
  CostOverlay c (AnalysisCostAlgebra f a cost) ->
  CostAlgebra f cost ->
  ClassId ->
  ContextEGraph owner f a c ->
  Either (ContextualExtractionObstruction c) (ContextualExtractionMetrics, Maybe (ExtractionResult f cost))
contextualExtractWithMetricsBounded budget contextValue overlayValue baseCost =
  contextualExtractWithAnalysisAndMetricsBounded budget contextValue overlayValue (liftCostAlgebra baseCost)

contextualExtractWithAnalysisBounded ::
  (Language f, Ord cost, Ord c) =>
  ExtractionWorkBudget ->
  c ->
  CostOverlay c (AnalysisCostAlgebra f a cost) ->
  AnalysisCostAlgebra f a cost ->
  ClassId ->
  ContextEGraph owner f a c ->
  Either (ContextualExtractionObstruction c) (Maybe (ExtractionResult f cost))
contextualExtractWithAnalysisBounded budget contextValue overlayValue baseCost classId contextGraph =
  snd
    <$> contextualExtractWithAnalysisAndMetricsBounded budget contextValue overlayValue baseCost classId contextGraph

contextualExtractWithAnalysisAndMetricsBounded ::
  (Language f, Ord cost, Ord c) =>
  ExtractionWorkBudget ->
  c ->
  CostOverlay c (AnalysisCostAlgebra f a cost) ->
  AnalysisCostAlgebra f a cost ->
  ClassId ->
  ContextEGraph owner f a c ->
  Either (ContextualExtractionObstruction c) (ContextualExtractionMetrics, Maybe (ExtractionResult f cost))
contextualExtractWithAnalysisAndMetricsBounded budget contextValue overlayValue baseCost classId contextGraph =
  do
    table <- contextualExtractionTable contextValue contextGraph
    extractionResult <-
      first ContextualExtractionBudgetExhausted
        ( extractFromTableBounded
            budget
            (applyCostOverlay contextValue overlayValue baseCost)
            classId
            table
        )
    pure
      ( contextualExtractionMetrics
          (length (contextCachedObjectsForExecution contextGraph))
          table
          extractionResult,
        extractionResult
      )

contextualExtractionSectionBounded ::
  (Language f, Ord cost, Ord c) =>
  ExtractionWorkBudget ->
  c ->
  CostOverlay c (AnalysisCostAlgebra f a cost) ->
  CostAlgebra f cost ->
  ContextEGraph owner f a c ->
  Either (ContextualExtractionObstruction c) (ContextualExtractionSection f a c cost)
contextualExtractionSectionBounded budget contextValue overlayValue baseCost =
  contextualExtractionSectionWithAnalysisBounded budget contextValue overlayValue (liftCostAlgebra baseCost)

contextualExtractionSectionWithAnalysisBounded ::
  (Language f, Ord cost, Ord c) =>
  ExtractionWorkBudget ->
  c ->
  CostOverlay c (AnalysisCostAlgebra f a cost) ->
  AnalysisCostAlgebra f a cost ->
  ContextEGraph owner f a c ->
  Either (ContextualExtractionObstruction c) (ContextualExtractionSection f a c cost)
contextualExtractionSectionWithAnalysisBounded budget contextValue overlayValue baseCost contextGraph = do
  table <- contextualExtractionTable contextValue contextGraph
  choiceSection <-
    first ContextualExtractionBudgetExhausted
      ( extractChoiceSectionBounded
          budget
          (applyCostOverlay contextValue overlayValue baseCost)
          table
      )
  pure
    ContextualExtractionSection
      { cesContext = contextValue,
        cesContextCount = length (contextCachedObjectsForExecution contextGraph),
        cesChoiceSection = choiceSection
      }

type ContextualSectionCache :: (Type -> Type) -> Type -> Type -> Type -> Type
data ContextualSectionCache f a c cost = ContextualSectionCache
  { cscBaseRevision :: !EGraphRevision,
    cscContextRevision :: !Natural,
    cscOverlay :: !(CostOverlay c (AnalysisCostAlgebra f a cost)),
    cscBaseCost :: !(AnalysisCostAlgebra f a cost),
    cscSections :: !(Map.Map c (ContextualExtractionSection f a c cost))
  }

type ContextualSectionObstruction :: Type -> Type
data ContextualSectionObstruction c
  = ContextualSectionBaseLineageMismatch !EGraphRevision !EGraphRevision
  | ContextualSectionContextLineageMismatch !Natural !Natural
  | ContextualSectionObstructed !(ContextualExtractionObstruction c)
  deriving stock (Eq, Show)

contextualSectionCacheBounded ::
  (Language f, Ord cost, Ord c) =>
  ExtractionWorkBudget ->
  ContextScope c ->
  CostOverlay c (AnalysisCostAlgebra f a cost) ->
  AnalysisCostAlgebra f a cost ->
  ContextEGraph owner f a c ->
  Either (ContextualSectionObstruction c) (ContextualSectionCache f a c cost)
contextualSectionCacheBounded budget contextScope overlayValue baseCost contextGraph =
  fmap
    ( \sections ->
        ContextualSectionCache
          { cscBaseRevision = eGraphRevision (cegBase contextGraph),
            cscContextRevision = cegContextRevision contextGraph,
            cscOverlay = overlayValue,
            cscBaseCost = baseCost,
            cscSections = Map.fromList sections
          }
    )
    ( traverse
        ( \contextValue ->
            fmap
              ((,) contextValue)
              ( first
                  ContextualSectionObstructed
                  (contextualExtractionSectionWithAnalysisBounded budget contextValue overlayValue baseCost contextGraph)
              )
        )
        (contextScopeObjects contextScope contextGraph)
    )

advanceContextualSections ::
  forall owner f a c cost.
  (Language f, Ord cost, Ord c) =>
  ExtractionWorkBudget ->
  ContextScope c ->
  ContextRebaseReport owner f c ->
  ContextEGraph owner f a c ->
  ContextualSectionCache f a c cost ->
  Either (ContextualSectionObstruction c) (ContextualSectionCache f a c cost)
advanceContextualSections budget contextScope reportValue contextGraph cacheValue
  | cscContextRevision cacheValue /= crrContextRevisionBefore reportValue =
      Left
        ( ContextualSectionContextLineageMismatch
            (cscContextRevision cacheValue)
            (crrContextRevisionBefore reportValue)
        )
  | cegContextRevision contextGraph /= crrContextRevisionAfter reportValue =
      Left
        ( ContextualSectionContextLineageMismatch
            (cegContextRevision contextGraph)
            (crrContextRevisionAfter reportValue)
        )
  | cscBaseRevision cacheValue /= emtRevisionBefore (cmtBaseTrace traceValue) =
      Left
        ( ContextualSectionBaseLineageMismatch
            (cscBaseRevision cacheValue)
            (emtRevisionBefore (cmtBaseTrace traceValue))
        )
  | eGraphRevision (cegBase contextGraph) /= emtRevisionAfter (cmtBaseTrace traceValue) =
      Left
        ( ContextualSectionBaseLineageMismatch
            (eGraphRevision (cegBase contextGraph))
            (emtRevisionAfter (cmtBaseTrace traceValue))
        )
  | otherwise =
      fmap
        ( \sections ->
            ContextualSectionCache
              { cscBaseRevision = eGraphRevision (cegBase contextGraph),
                cscContextRevision = cegContextRevision contextGraph,
                cscOverlay = cscOverlay cacheValue,
                cscBaseCost = cscBaseCost cacheValue,
                cscSections = Map.fromList sections
              }
        )
        (traverse advanceAt (contextScopeObjects contextScope contextGraph))
  where
    traceValue = crrTrace reportValue

    baseChanged =
      not (IntSet.null (mutationTraceDirtyKeys (cmtBaseTrace traceValue)))

    advanceAt contextValue = do
      sectionDirty <- contextSectionDirty contextValue
      case Map.lookup contextValue (cscSections cacheValue) of
        Just section
          | not baseChanged && not sectionDirty ->
              Right (contextValue, section)
        _ ->
          fmap
            ((,) contextValue)
            ( first
                ContextualSectionObstructed
                (contextualExtractionSectionWithAnalysisBounded budget contextValue overlayValue baseCost contextGraph)
            )

    overlayValue =
      cscOverlay cacheValue

    baseCost =
      cscBaseCost cacheValue

    contextSectionDirty contextValue =
      fmap
        (Set.member contextValue (cmtDirtyContexts traceValue) ||)
        ( fmap or
            ( traverse
                ( first (ContextualSectionObstructed . ContextualExtractionSupportSiteFailed)
                    . preparedContextRestrictsTo (cegSite contextGraph) contextValue
                )
                (Map.keys (cmtObservedLocalUnionsByContext traceValue))
            )
        )

contextualExtractFromSection ::
  (Language f, Ord cost) =>
  ClassId ->
  ContextualExtractionSection f a c cost ->
  Either (ContextualExtractionObstruction c) (ContextualExtractionMetrics, Maybe (ExtractionResult f cost))
contextualExtractFromSection classId section =
  let extractionResult =
        extractFromChoiceSection classId (cesChoiceSection section)
   in Right (contextualExtractionSectionMetrics section extractionResult, extractionResult)

contextualExtractionSectionMetrics ::
  ContextualExtractionSection f a c cost ->
  Maybe (ExtractionResult f cost) ->
  ContextualExtractionMetrics
contextualExtractionSectionMetrics section extractionResult =
  contextualExtractionMetrics
    (cesContextCount section)
    (extractionChoiceSectionTable (cesChoiceSection section))
    extractionResult

contextualExtractionMetrics ::
  Int ->
  ExtractionTable f a ->
  Maybe (ExtractionResult f cost) ->
  ContextualExtractionMetrics
contextualExtractionMetrics contextCount table extractionResult =
  ContextualExtractionMetrics
    { cemLocalizedNodeCount =
        sum (fmap (length . extractionClassNodes) (IntMap.elems (extractionClasses table))),
      cemLocalizedClassCount = IntMap.size (extractionClasses table),
      cemLocalizedContextCount = contextCount,
      cemResultFound = isJust extractionResult
    }

contextualExtractionPartitionsBounded ::
  (Language f, Ord cost, Ord c) =>
  ExtractionWorkBudget ->
  ContextScope c ->
  CostOverlay c (AnalysisCostAlgebra f a cost) ->
  CostAlgebra f cost ->
  ClassId ->
  ContextEGraph owner f a c ->
  Either (ContextualExtractionObstruction c) [SheafTwist.ContextualExtractionPartition (ExtractionResult f cost) c]
contextualExtractionPartitionsBounded budget contextScope overlayValue baseCost =
  contextualExtractionPartitionsWithAnalysisBounded budget contextScope overlayValue (liftCostAlgebra baseCost)

contextualExtractionPartitionsWithAnalysisBounded ::
  (Language f, Ord cost, Ord c) =>
  ExtractionWorkBudget ->
  ContextScope c ->
  CostOverlay c (AnalysisCostAlgebra f a cost) ->
  AnalysisCostAlgebra f a cost ->
  ClassId ->
  ContextEGraph owner f a c ->
  Either (ContextualExtractionObstruction c) [SheafTwist.ContextualExtractionPartition (ExtractionResult f cost) c]
contextualExtractionPartitionsWithAnalysisBounded budget contextScope overlayValue baseCost classId contextGraph =
  fmap
    contextualExtractionPartitionsFromResults
    ( traverse
        ( \contextValue ->
            fmap
              ((,) contextValue)
              (contextualExtractWithAnalysisBounded budget contextValue overlayValue baseCost classId contextGraph)
        )
        (contextScopeObjects contextScope contextGraph)
    )

contextScopeObjects ::
  Ord c =>
  ContextScope c ->
  ContextEGraph owner f a c ->
  [c]
contextScopeObjects contextScope contextGraph =
  case contextScope of
    CachedObjects ->
      contextCachedObjectsForExecution contextGraph
    Objects objects ->
      Set.toAscList objects
    AllPreparedObjectsForDiagnostics ->
      contextPreparedObjects contextGraph
{-# INLINE contextScopeObjects #-}

contextualExtractionPartitionsFromResults ::
  (Ord ctx, Ord result) =>
  [(ctx, Maybe result)] ->
  [SheafTwist.ContextualExtractionPartition result ctx]
contextualExtractionPartitionsFromResults =
  fmap
    (\(resultValue, contextSet) -> SheafTwist.ContextualExtractionPartition contextSet resultValue)
    . Map.toAscList
    . foldr accumulateResult Map.empty
  where
    accumulateResult ::
      (Ord ctx, Ord result) =>
      (ctx, Maybe result) ->
      Map.Map result (Set.Set ctx) ->
      Map.Map result (Set.Set ctx)
    accumulateResult (contextValue, maybeResult) grouped =
      maybe
        grouped
        (\resultValue -> Map.insertWith Set.union resultValue (Set.singleton contextValue) grouped)
        maybeResult
