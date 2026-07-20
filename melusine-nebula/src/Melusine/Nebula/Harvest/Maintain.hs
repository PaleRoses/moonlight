{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.Harvest.Maintain
  ( HarvestDelta (..),
    HarvestAdvanceDecision (..),
    HarvestFallbackReason (..),
    advanceHarvest,
  )
where

import Data.Kind (Type)
import Data.Set qualified as Set
import Melusine.Nebula.Discovery.Choose
  ( BindingHarvestRow (..),
    CandidateSite (..),
    ShapeBucket,
    bindingHarvestRows,
    nebulaSizeExtractionSectionCache,
    nebulaSizeExtractionSectionsFromCache,
  )
import Melusine.Nebula.Core (NebulaConfig (..), NebulaAnalysis, NebulaError (..))
import Melusine.Nebula.Harvest.Core
  ( HarvestState (..),
    SiteRow,
    advanceHarvestFromSections,
    buildHarvest,
    harvestDirtyBuckets,
    harvestIndexDelta,
  )
import Melusine.Nebula.Source.Ingest (IngestedModule)
import Melusine.Nebula.Rewrite.Saturate (SaturatedModule, smContextGraph, smMutationTrace)
import Moonlight.Differential.Algebra.ZSet (IndexedZSet)
import Moonlight.EGraph.Introspection.Core.HsExpr (HsExprF, ScopeCtx)
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    ContextMutationTrace (..),
    ContextRebaseReport (..),
  )
import Moonlight.EGraph.Pure.Context (cegContextRevision)
import Moonlight.EGraph.Pure.Saturation.Extraction
  ( ContextScope (Objects),
    ContextualSectionObstruction,
    advanceContextualSections,
  )

type HarvestDelta :: Type
data HarvestDelta = HarvestDelta
  { hdSiteDelta :: !(IndexedZSet ShapeBucket SiteRow Int),
    hdDirtyBuckets :: !(Set.Set ShapeBucket),
    hdDecision :: !HarvestAdvanceDecision
  }

type HarvestAdvanceDecision :: Type
data HarvestAdvanceDecision
  = HarvestAdvanced
  | HarvestFellBack !HarvestFallbackReason
  deriving stock (Eq, Ord, Show)

type HarvestFallbackReason :: Type
data HarvestFallbackReason
  = HarvestFallbackGlobalPlanMerge
  | HarvestFallbackDirtyRatio !Int !Int !Double
  | HarvestFallbackStageSectionObstruction !String
  | HarvestFallbackSaturationSectionObstruction !String
  deriving stock (Eq, Ord, Show)

advanceHarvest ::
  NebulaConfig ->
  IngestedModule ->
  Bool ->
  ContextRebaseReport HsExprF ScopeCtx ->
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  SaturatedModule ->
  HarvestState ->
  Either NebulaError (HarvestDelta, HarvestState)
advanceHarvest config ingested planUsedGlobalFallback stageReport stagedGraph saturated previousHarvest =
  case fallbackReason of
    Just reason ->
      rebuildWithDecision (HarvestFellBack reason)
    Nothing ->
      case advancedSections of
        Left reason ->
          rebuildWithDecision (HarvestFellBack reason)
        Right sections -> do
          (siteDelta, advancedHarvest) <-
            advanceHarvestFromSections config ingested saturated (nebulaSizeExtractionSectionsFromCache sections) dirtyContexts previousHarvest
          pure (deltaWithDecision siteDelta HarvestAdvanced, advancedHarvest)
  where
    fallbackReason =
      if planUsedGlobalFallback
        then Just HarvestFallbackGlobalPlanMerge
        else dirtyRatioFallback config previousHarvest dirtyContexts

    dirtyContexts =
      cmtDirtyContexts (crrTrace stageReport) <> cmtDirtyContexts (smMutationTrace saturated)

    advancedSections = do
      stagedSections <-
        mapSectionObstruction HarvestFallbackStageSectionObstruction $
          advanceContextualSections
            (ncExtractionBudget config)
            (Objects advanceScopeContexts)
            stageReport
            stagedGraph
            (nebulaSizeExtractionSectionCache (hsSections previousHarvest))
      mapSectionObstruction HarvestFallbackSaturationSectionObstruction $
        advanceContextualSections
          (ncExtractionBudget config)
          (Objects advanceScopeContexts)
          saturationReport
          (smContextGraph saturated)
          stagedSections

    advanceScopeContexts =
      dirtyContexts <> dirtyBindingCoverContexts ingested dirtyContexts

    saturationReport =
      ContextRebaseReport
        { crrScope = crrScope stageReport,
          crrTrace = smMutationTrace saturated,
          crrContextRevisionBefore = cegContextRevision stagedGraph,
          crrContextRevisionAfter = cegContextRevision (smContextGraph saturated)
        }

    rebuildWithDecision decision = do
      rebuilt <- buildHarvest config ingested saturated
      let siteDelta =
            harvestIndexDelta (hsBucketIndex previousHarvest) (hsBucketIndex rebuilt)
      pure (deltaWithDecision siteDelta decision, rebuilt)

csContextFromSite :: CandidateSite -> ScopeCtx
csContextFromSite =
  csContext

dirtyBindingCoverContexts :: IngestedModule -> Set.Set ScopeCtx -> Set.Set ScopeCtx
dirtyBindingCoverContexts ingested dirtyContexts =
  -- Reharvesting a binding after a nested-context edit needs labels for every
  -- context in that binding's local cover; rebuilding only the raw dirty fiber
  -- produces extraction results whose signature labels were never maintained.
  foldMap dirtyRowContexts (bindingHarvestRows ingested)
  where
    dirtyRowContexts bindingRow
      | bindingRowIsDirty dirtyContexts bindingRow =
          bhrContexts bindingRow
      | otherwise =
          Set.empty

    bindingRowIsDirty dirtyScopeContexts bindingRow =
      not (Set.null (Set.intersection dirtyScopeContexts (bhrContexts bindingRow)))

mapSectionObstruction ::
  (String -> HarvestFallbackReason) ->
  Either (ContextualSectionObstruction ScopeCtx) value ->
  Either HarvestFallbackReason value
mapSectionObstruction reason =
  either (Left . reason . show) Right

deltaWithDecision ::
  IndexedZSet ShapeBucket SiteRow Int ->
  HarvestAdvanceDecision ->
  HarvestDelta
deltaWithDecision siteDelta decision =
  HarvestDelta
    { hdSiteDelta = siteDelta,
      hdDirtyBuckets = harvestDirtyBuckets siteDelta,
      hdDecision = decision
    }

dirtyRatioFallback :: NebulaConfig -> HarvestState -> Set.Set ScopeCtx -> Maybe HarvestFallbackReason
dirtyRatioFallback config previousHarvest dirtyContexts
  | totalCount <= 0 = Nothing
  | dirtyRatio >= ncIncrementalFallbackRatio config =
      Just (HarvestFallbackDirtyRatio dirtyCount totalCount dirtyRatio)
  | otherwise = Nothing
  where
    harvestContexts =
      Set.fromList (fmap csContextFromSite (hsSites previousHarvest))

    dirtyCount =
      Set.size (Set.intersection dirtyContexts harvestContexts)

    totalCount =
      Set.size harvestContexts

    dirtyRatio =
      fromIntegral dirtyCount / fromIntegral totalCount
