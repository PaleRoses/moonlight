{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.Discovery.Choose
  ( ChosenBinding (..),
    CandidateSiteKind (..),
    candidateSiteKindKey,
    CandidateSite (..),
    AbstractionCandidate (..),
    ShapeBucket (..),
    BindingHarvestRow (..),
    NebulaSizeExtractionSections (..),
    assignCandidateOrdinals,
    bindingHarvestRows,
    chooseBindingRow,
    candidateSitesForBinding,
    shapeBuckets,
    sitePairKey,
    nebulaCostAlgebra,
    chooseBindings,
    candidateSites,
    harvestContexts,
    sizeExtractionSections,
    nebulaSizeExtractionSectionsFromCache,
    nebulaSizeExtractionSectionCache,
    resolvePatternClass,
    sharedAbstractionCandidates,
  )
where

import Data.Kind (Type)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Functor.Foldable (cata)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (nub, sortOn, zip5)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Melusine.Nebula.Discovery.AlphaUnify (alphaUnifyTerms)
import Melusine.Nebula.Core
  ( NebulaConfig (..),
    NebulaAnalysis (..),
    NebulaCostModel (..),
    NebulaError (..),
    TypeEvidence,
  )
import Melusine.Nebula.Source.Ingest (IngestedModule (..))
import Melusine.Nebula.Rewrite.Saturate (SaturatedModule, smContextGraph)
import Melusine.Nebula.Synthesis.Scope (closePairDefinitionBoundaryLocals, wellScopedDefinitionPattern)
import Moonlight.Core (Pattern (..))
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( ConvertedModule (..),
    HsExprF,
    HsExprF (..),
    HsExprTag (..),
    ScopeCtx (..),
    ScopeId,
    SourceRegion,
    ScopedExpr (..),
    SpannedExpr (..),
    TagSignature,
    TopLevelBinding (..),
    eraseScopedExpr,
    freeScopeSummarySize,
    tagSignatureFromTag,
  )
import Moonlight.EGraph.Pure.AntiUnify (BinaryLGGResult (..), antiUnify)
import Moonlight.EGraph.Pure.Context (ContextEGraph)
import Moonlight.EGraph.Pure.Context (cegBase)
import Moonlight.EGraph.Pure.Extraction
  ( CostAlgebra (..),
    ExtractionResult (..),
    depthCost,
    extractAllFromChoiceSection,
    liftCostAlgebra,
    termSize,
    termCost,
  )
import Moonlight.EGraph.Pure.Kernel.HashCons (lookupLeastENode)
import Moonlight.EGraph.Pure.Saturation.Extraction
  ( ContextScope (Objects),
    ContextualExtractionObstruction,
    ContextualExtractionSection,
    ContextualSectionCache,
    cesChoiceSection,
    contextualExtractBounded,
    contextualExtractFromSection,
    contextualSectionCacheBounded,
    cscSections,
  )
import Moonlight.EGraph.Pure.Types (ClassId, EClass (..), EGraph, ENode (..), canonicalizeClassId, classIdKey, lookupEClass)
import Data.Fix (Fix (..))

type ChosenBinding :: Type
data ChosenBinding = ChosenBinding
  { cbName :: !String,
    cbContext :: !ScopeCtx,
    cbSeedClass :: !ClassId,
    cbOriginalSize :: !Int,
    cbTerm :: !(Fix HsExprF),
    cbExtractedSize :: !Int,
    cbExtractionCost :: !Int,
    cbSignature :: !TagSignature
  }

type NebulaSizeExtractionSections :: Type
data NebulaSizeExtractionSections = NebulaSizeExtractionSections
  { nssSections :: !(ContextualSectionCache HsExprF NebulaAnalysis ScopeCtx Int),
    nssSignatures :: !(Map.Map ScopeCtx (IntMap TagSignature))
  }

type CandidateSiteKind :: Type
data CandidateSiteKind
  = BindingCandidateSite
  | RegionCandidateSite
  deriving stock (Eq, Ord, Show)

candidateSiteKindKey :: CandidateSiteKind -> String
candidateSiteKindKey = \case
  BindingCandidateSite ->
    "binding-candidate-site"
  RegionCandidateSite ->
    "region-candidate-site"

type CandidateSite :: Type
data CandidateSite = CandidateSite
  { csOrdinal :: !Int,
    csBindingName :: !String,
    csSiteKind :: !CandidateSiteKind,
    csRegion :: !(Maybe SourceRegion),
    csContext :: !ScopeCtx,
    csClass :: !ClassId,
    csTerm :: !(Fix HsExprF),
    csSourceTerm :: !(Fix HsExprF),
    csOriginalSize :: !Int,
    csSize :: !Int,
    csFreeScopeWidth :: !Int,
    csSignature :: !TagSignature,
    csTypeEvidence :: !(Maybe TypeEvidence)
  }

instance Eq CandidateSite where
  leftSite == rightSite =
    csOrdinal leftSite == csOrdinal rightSite

instance Ord CandidateSite where
  compare leftSite rightSite =
    compare (csOrdinal leftSite) (csOrdinal rightSite)

instance Show CandidateSite where
  show site =
    "CandidateSite{binding="
      <> show (csBindingName site)
      <> ", ordinal="
      <> show (csOrdinal site)
      <> ", kind="
      <> show (csSiteKind site)
      <> "}"

type AbstractionCandidate :: Type
data AbstractionCandidate = AbstractionCandidate
  { acLeftSite :: !CandidateSite,
    acRightSite :: !CandidateSite,
    acResult :: !(BinaryLGGResult HsExprF ClassId)
  }

type ShapeBucket :: Type
data ShapeBucket = ShapeBucket
  { sbRootTag :: !HsExprTag,
    sbSizeBand :: !Int,
    sbFreeScopeWidth :: !Int,
    sbSignature :: !TagSignature,
    sbTypeEvidence :: !(Maybe TypeEvidence)
  }
  deriving stock (Eq, Ord, Show)

type BindingHarvestRow :: Type
data BindingHarvestRow = BindingHarvestRow
  { bhrOrdinal :: !Int,
    bhrName :: !String,
    bhrContext :: !ScopeCtx,
    bhrSeedClass :: !ClassId,
    bhrOriginalSize :: !Int,
    bhrBinding :: !TopLevelBinding,
    bhrContexts :: !(Set.Set ScopeCtx)
  }

type RegionProjection :: Type
data RegionProjection = RegionProjection
  { rpTerm :: !(Fix HsExprF),
    rpSignature :: !TagSignature
  }

nebulaCostAlgebra :: NebulaCostModel -> CostAlgebra HsExprF Int
nebulaCostAlgebra costModel =
  case costModel of
    SizeCost ->
      CostAlgebra hsExprSizeCost
    DepthCost ->
      depthCost

hsExprSizeCost :: HsExprF Int -> Int
hsExprSizeCost = \case
  ParF innerCost ->
    innerCost
  OpAppF leftCost operatorCost rightCost ->
    leftCost + operatorCost + rightCost
  nodeCosts ->
    1 + sum nodeCosts

chooseBindings :: NebulaConfig -> IngestedModule -> SaturatedModule -> NebulaSizeExtractionSections -> Either NebulaError [ChosenBinding]
chooseBindings config ingested saturated sizeSections =
  traverse (chooseBindingRow config saturated sizeSections) (bindingHarvestRows ingested)

bindingHarvestRows :: IngestedModule -> [BindingHarvestRow]
bindingHarvestRows ingested =
  zipWith
    bindingHarvestRowFromParts
    [0 ..]
    ( zip5
        (imBindingNames ingested)
        (imBindingContexts ingested)
        (imSeedClasses ingested)
        (imOriginalSizes ingested)
        (cmBindings (imConverted ingested))
    )

bindingHarvestRowFromParts ::
  Int ->
  (String, ScopeCtx, ClassId, Int, TopLevelBinding) ->
  BindingHarvestRow
bindingHarvestRowFromParts ordinal (bindingName, bindingContext, seedClass, originalSize, sourceBinding) =
  BindingHarvestRow
    { bhrOrdinal = ordinal,
      bhrName = bindingName,
      bhrContext = bindingContext,
      bhrSeedClass = seedClass,
      bhrOriginalSize = originalSize,
      bhrBinding = sourceBinding,
      bhrContexts = Set.fromList (bindingContext : topLevelBindingScopeContexts sourceBinding)
    }

chooseBindingRow ::
  NebulaConfig ->
  SaturatedModule ->
  NebulaSizeExtractionSections ->
  BindingHarvestRow ->
  Either NebulaError ChosenBinding
chooseBindingRow config saturated sizeSections bindingRow =
  case sizeExtractAt config contextGraph sizeSections (bhrContext bindingRow) (bhrSeedClass bindingRow) of
    Left obstruction ->
      Left (NebulaExtractionError (bhrName bindingRow) (show obstruction))
    Right Nothing ->
      Left (NebulaExtractionError (bhrName bindingRow) "no representative admitted at the binding context")
    Right (Just extractionResult) ->
      case extractionSignatureAt sizeSections (bhrContext bindingRow) (erClass extractionResult) of
        Nothing ->
          Left (NebulaExtractionError (bhrName bindingRow) "no maintained signature label at the binding context")
        Just signature ->
          let (finalExtraction, finalSignature) =
                regionRecompositionExtraction (bhrOriginalSize bindingRow) (bhrBinding bindingRow) signature extractionResult
           in Right
                ( chosenBindingFromExtraction
                    (bhrName bindingRow)
                    (bhrContext bindingRow)
                    (bhrSeedClass bindingRow)
                    (bhrOriginalSize bindingRow)
                    finalSignature
                    finalExtraction
                )
  where
    contextGraph =
      smContextGraph saturated

    regionRecompositionExtraction originalSize sourceBinding signature extractionResult =
      scopeRegionRecompositionExtraction
        config
        originalSize
        sourceBinding
        signature
        contextGraph
        sizeSections
        extractionResult

chosenBindingFromExtraction ::
  String ->
  ScopeCtx ->
  ClassId ->
  Int ->
  TagSignature ->
  ExtractionResult HsExprF Int ->
  ChosenBinding
chosenBindingFromExtraction bindingName bindingContext seedClass originalSize signature extractionResult =
  ChosenBinding
    { cbName = bindingName,
      cbContext = bindingContext,
      cbSeedClass = seedClass,
      cbOriginalSize = originalSize,
      cbTerm = erTerm extractionResult,
      cbExtractedSize = termSize (erTerm extractionResult),
      cbExtractionCost = erCost extractionResult,
      cbSignature = signature
    }

scopeRegionRecompositionExtraction ::
  NebulaConfig ->
  Int ->
  TopLevelBinding ->
  TagSignature ->
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  NebulaSizeExtractionSections ->
  ExtractionResult HsExprF Int ->
  (ExtractionResult HsExprF Int, TagSignature)
scopeRegionRecompositionExtraction config originalSize sourceBinding signature contextGraph sizeSections extractionResult =
  let recomposedProjection =
        recomposeScopeRegion config contextGraph sizeSections (tlbScopedTerm sourceBinding)
   in if termSize (rpTerm recomposedProjection) < originalSize
        then
          ( ExtractionResult
              { erTerm = rpTerm recomposedProjection,
                erCost = termCost (nebulaCostAlgebra (ncCostModel config)) (rpTerm recomposedProjection),
                erClass = erClass extractionResult
              },
            rpSignature recomposedProjection
          )
        else (extractionResult, signature)

recomposeScopeRegion ::
  NebulaConfig ->
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  NebulaSizeExtractionSections ->
  ScopedExpr ->
  RegionProjection
recomposeScopeRegion config contextGraph sizeSections scopedExpr =
  let regionPattern =
        eraseScopedExpr scopedExpr
      rebuiltOriginal =
        recomposeOriginalChildren config contextGraph sizeSections scopedExpr
   in case extractScopeRegionProjection config contextGraph sizeSections scopedExpr regionPattern of
        Just extractedProjection ->
          extractedProjection
        Nothing ->
          rebuiltOriginal

recomposeOriginalChildren ::
  NebulaConfig ->
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  NebulaSizeExtractionSections ->
  ScopedExpr ->
  RegionProjection
recomposeOriginalChildren config contextGraph sizeSections scopedExpr =
  let childProjections =
        fmap (recomposeChildScope config contextGraph sizeSections (seOccScope scopedExpr)) (seNode scopedExpr)
      nodeTerms =
        fmap rpTerm childProjections
      nodeSignature =
        tagSignatureFromTag (hsExprRootTag nodeTerms) <> foldMap rpSignature childProjections
   in RegionProjection
        { rpTerm = Fix nodeTerms,
          rpSignature = nodeSignature
        }

recomposeChildScope ::
  NebulaConfig ->
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  NebulaSizeExtractionSections ->
  ScopeId ->
  ScopedExpr ->
  RegionProjection
recomposeChildScope config contextGraph sizeSections parentScope childScoped =
  if seOccScope childScoped == parentScope
    then recomposeOriginalChildren config contextGraph sizeSections childScoped
    else recomposeScopeRegion config contextGraph sizeSections childScoped

extractScopeRegionProjection ::
  NebulaConfig ->
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  NebulaSizeExtractionSections ->
  ScopedExpr ->
  Pattern HsExprF ->
  Maybe RegionProjection
extractScopeRegionProjection config contextGraph sizeSections scopedExpr regionPattern = do
  regionClass <- resolvePatternClass (cegBase contextGraph) regionPattern
  projection <-
    either (const Nothing) id $
      normalizedRegionProjection config contextGraph sizeSections scopedExpr regionClass
  if termSize (rpTerm projection) < patternNodeCount regionPattern
    then Just projection
    else Nothing

resolvePatternClass :: EGraph HsExprF a -> Pattern HsExprF -> Maybe ClassId
resolvePatternClass graph patternValue =
  case patternValue of
    PatternVar _ ->
      Nothing
    PatternNode node -> do
      childClasses <- traverse (resolvePatternClass graph) node
      canonicalizeClassId graph <$> lookupLeastENode (ENode childClasses) graph

patternNodeCount :: Pattern HsExprF -> Int
patternNodeCount patternValue =
  case patternValue of
    PatternVar _ ->
      0
    PatternNode node ->
      1 + sum (fmap patternNodeCount node)

candidateSites :: NebulaConfig -> IngestedModule -> SaturatedModule -> [ChosenBinding] -> NebulaSizeExtractionSections -> Either NebulaError [CandidateSite]
candidateSites config ingested saturated chosenBindings sizeSections =
  fmap assignCandidateOrdinals unnumberedSites
  where
    bindingRows =
      zip (bindingHarvestRows ingested) chosenBindings
    unnumberedSites =
      fmap concat (traverse (uncurry (candidateSitesForBinding config saturated sizeSections)) bindingRows)

assignCandidateOrdinals :: [CandidateSite] -> [CandidateSite]
assignCandidateOrdinals =
  zipWith (\ordinal site -> site {csOrdinal = ordinal}) [0 ..]

candidateSitesForBinding ::
  NebulaConfig ->
  SaturatedModule ->
  NebulaSizeExtractionSections ->
  BindingHarvestRow ->
  ChosenBinding ->
  Either NebulaError [CandidateSite]
candidateSitesForBinding config saturated sizeSections bindingRow chosenBinding =
  fmap
    (bindingCandidateSite baseGraph bindingRow chosenBinding :)
    (regionCandidateSites config baseGraph contextGraph sizeSections bindingRow chosenBinding)
  where
    contextGraph =
      smContextGraph saturated
    baseGraph =
      cegBase contextGraph

bindingCandidateSite ::
  EGraph HsExprF NebulaAnalysis ->
  BindingHarvestRow ->
  ChosenBinding ->
  CandidateSite
bindingCandidateSite baseGraph bindingRow chosenBinding =
  let binding =
        bhrBinding bindingRow
   in candidateSiteFromProjection
        baseGraph
        (cbName chosenBinding)
        BindingCandidateSite
        (tlbRegion binding)
        (cbContext chosenBinding)
        (cbSeedClass chosenBinding)
        (cbTerm chosenBinding)
        (fixScopedExpr (tlbScopedTerm binding))
        (cbOriginalSize chosenBinding)
        (cbExtractedSize chosenBinding)
        (scopedFreeScopeWidth (tlbScopedTerm binding))
        (cbSignature chosenBinding)

candidateSiteFromProjection ::
  EGraph HsExprF NebulaAnalysis ->
  String ->
  CandidateSiteKind ->
  Maybe SourceRegion ->
  ScopeCtx ->
  ClassId ->
  Fix HsExprF ->
  Fix HsExprF ->
  Int ->
  Int ->
  Int ->
  TagSignature ->
  CandidateSite
candidateSiteFromProjection baseGraph bindingName siteKind region context classId term sourceTerm originalSize size freeScopeWidth signature =
  let canonicalClass = canonicalizeClassId baseGraph classId
   in CandidateSite
        { csOrdinal = -1,
          csBindingName = bindingName,
          csSiteKind = siteKind,
          csRegion = region,
          csContext = context,
          csClass = canonicalClass,
          csTerm = term,
          csSourceTerm = sourceTerm,
          csOriginalSize = originalSize,
          csSize = size,
          csFreeScopeWidth = freeScopeWidth,
          csSignature = signature,
          csTypeEvidence = classTypeEvidence baseGraph canonicalClass
        }

regionCandidateSites ::
  NebulaConfig ->
  EGraph HsExprF NebulaAnalysis ->
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  NebulaSizeExtractionSections ->
  BindingHarvestRow ->
  ChosenBinding ->
  Either NebulaError [CandidateSite]
regionCandidateSites config baseGraph contextGraph sizeSections bindingRow chosenBinding =
  fmap (filter siteLargeEnough) $
    nestedRegionSites (tlbRegion binding) (tlbScopedTerm binding) (tlbSpannedTerm binding)
  where
    binding =
      bhrBinding bindingRow

    siteLargeEnough site =
      csSize site >= max 1 (ncDiagnosticMinShared config + 1)

    nestedRegionSites rootRegion scopedExpr spannedExpr =
      (<>) <$> currentSite rootRegion scopedExpr spannedExpr
        <*> fmap concat
          ( traverse
              (uncurry (nestedRegionSites rootRegion))
              (zip (toList (seNode scopedExpr)) (toList (sxNode spannedExpr)))
          )

    currentSite rootRegion scopedExpr spannedExpr = do
      case sxRegion spannedExpr of
        Nothing ->
          Right []
        Just region
          | Just region == rootRegion ->
              Right []
          | otherwise ->
              case resolvePatternClass baseGraph (eraseScopedExpr scopedExpr) of
                Nothing ->
                  Right []
                Just classId ->
                  case normalizedRegionProjection config contextGraph sizeSections scopedExpr classId of
                    Left obstruction ->
                      Left obstruction
                    Right Nothing ->
                      Right []
                    Right (Just projection) ->
                      Right
                        [ candidateSiteFromProjection
                            baseGraph
                            (cbName chosenBinding)
                            RegionCandidateSite
                            (Just region)
                            (ActualScope (seOccScope scopedExpr))
                            classId
                            (rpTerm projection)
                            (fixScopedExpr scopedExpr)
                            (patternNodeCount (eraseScopedExpr scopedExpr))
                            (termSize (rpTerm projection))
                            (scopedFreeScopeWidth scopedExpr)
                            (rpSignature projection)
                        ]

normalizedRegionProjection ::
  NebulaConfig ->
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  NebulaSizeExtractionSections ->
  ScopedExpr ->
  ClassId ->
  Either NebulaError (Maybe RegionProjection)
normalizedRegionProjection config contextGraph sizeSections scopedExpr classId =
  case sizeExtractAt config contextGraph sizeSections (ActualScope (seOccScope scopedExpr)) classId of
    Right (Just extractionResult) ->
      case extractionSignatureAt sizeSections (ActualScope (seOccScope scopedExpr)) (erClass extractionResult) of
        Nothing ->
          Left (NebulaExtractionError (show (ActualScope (seOccScope scopedExpr))) "no maintained signature label at the region context")
        Just signature ->
          Right
            ( Just
                RegionProjection
                  { rpTerm = erTerm extractionResult,
                    rpSignature = signature
                  }
            )
    Right Nothing ->
      Right Nothing
    Left obstruction ->
      Left (NebulaExtractionError (show (ActualScope (seOccScope scopedExpr))) (show obstruction))

sizeExtractionSections ::
  NebulaConfig ->
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  [ScopeCtx] ->
  Either NebulaError NebulaSizeExtractionSections
sizeExtractionSections config contextGraph contexts =
  fmap nebulaSizeExtractionSectionsFromCache
    ( first
        (NebulaExtractionError "size-extraction-sections" . show)
        ( contextualSectionCacheBounded
            (ncExtractionBudget config)
            (Objects (Set.fromList contexts))
            mempty
            (liftCostAlgebra (nebulaCostAlgebra (ncCostModel config)))
            contextGraph
        )
    )

nebulaSizeExtractionSectionsFromCache ::
  ContextualSectionCache HsExprF NebulaAnalysis ScopeCtx Int ->
  NebulaSizeExtractionSections
nebulaSizeExtractionSectionsFromCache sectionCache =
  NebulaSizeExtractionSections
    { nssSections = sectionCache,
      nssSignatures = fmap choiceLabelsForSection (cscSections sectionCache)
    }

nebulaSizeExtractionSectionCache ::
  NebulaSizeExtractionSections ->
  ContextualSectionCache HsExprF NebulaAnalysis ScopeCtx Int
nebulaSizeExtractionSectionCache =
  nssSections

sizeExtractAt ::
  NebulaConfig ->
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  NebulaSizeExtractionSections ->
  ScopeCtx ->
  ClassId ->
  Either (ContextualExtractionObstruction ScopeCtx) (Maybe (ExtractionResult HsExprF Int))
sizeExtractAt config contextGraph sections contextValue classId =
  case Map.lookup contextValue (cscSections (nssSections sections)) of
    Just section ->
      snd <$> contextualExtractFromSection classId section
    Nothing ->
      contextualExtractBounded
        (ncExtractionBudget config)
        contextValue
        mempty
        (nebulaCostAlgebra (ncCostModel config))
        classId
        contextGraph

harvestContexts :: IngestedModule -> [ScopeCtx]
harvestContexts ingested =
  imBindingContexts ingested <> foldMap topLevelBindingScopeContexts (cmBindings (imConverted ingested))

topLevelBindingScopeContexts :: TopLevelBinding -> [ScopeCtx]
topLevelBindingScopeContexts =
  scopedExprScopeContexts . tlbScopedTerm

scopedExprScopeContexts :: ScopedExpr -> [ScopeCtx]
scopedExprScopeContexts scopedExpr =
  ActualScope (seOccScope scopedExpr) : foldMap scopedExprScopeContexts (seNode scopedExpr)

fixScopedExpr :: ScopedExpr -> Fix HsExprF
fixScopedExpr scopedExpr =
  Fix (fmap fixScopedExpr (seNode scopedExpr))

scopedFreeScopeWidth :: ScopedExpr -> Int
scopedFreeScopeWidth =
  freeScopeSummarySize . seFreeScopes

classTypeEvidence :: EGraph HsExprF NebulaAnalysis -> ClassId -> Maybe TypeEvidence
classTypeEvidence baseGraph classId =
  fmap (naType . eClassData) (lookupEClass baseGraph classId)

sharedAbstractionCandidates :: NebulaConfig -> SaturatedModule -> [(CandidateSite, CandidateSite)] -> Either NebulaError [AbstractionCandidate]
sharedAbstractionCandidates config saturated sitePairs =
  let baseGraph = cegBase (smContextGraph saturated)
      candidateFrom leftSite rightSite lggResult =
        AbstractionCandidate
          { acLeftSite = leftSite,
            acRightSite = rightSite,
            acResult = lggResult
          }
      candidateFor (leftSite, rightSite) =
        let kernelResult =
              either
                (const Nothing)
                Just
                (antiUnify (nebulaCostAlgebra (ncCostModel config)) (csClass leftSite) (csClass rightSite) baseGraph)
            kernelCandidate =
              fmap (candidateFrom leftSite rightSite . closePairDefinitionBoundaryLocals (resolvePatternClass baseGraph)) kernelResult
            alphaCandidate =
              fmap
                (candidateFrom leftSite rightSite . closePairDefinitionBoundaryLocals (resolvePatternClass baseGraph))
                (alphaUnifyTerms (resolvePatternClass baseGraph) (csTerm leftSite) (csTerm rightSite))
            scopedCandidates =
              filter candidateIsDefinitionScoped (toList kernelCandidate <> toList alphaCandidate)
         in case scopedCandidates of
              [] ->
                maybe [] pure (bestCandidate [kernelCandidate, alphaCandidate])
              _ ->
                scopedCandidates
      candidateLimit =
        max 0 (ncAntiUnifyMaxPairs config)
      rankedCandidates =
        take candidateLimit . sortOn (Down . binaryLggSharedStructure . acResult) . (>>= candidateFor)
   in Right (rankedCandidates sitePairs)

bestCandidate :: [Maybe AbstractionCandidate] -> Maybe AbstractionCandidate
bestCandidate =
  foldr betterCandidate Nothing

betterCandidate :: Maybe AbstractionCandidate -> Maybe AbstractionCandidate -> Maybe AbstractionCandidate
betterCandidate Nothing currentBest =
  currentBest
betterCandidate candidate Nothing =
  candidate
betterCandidate candidate@(Just nextCandidate) currentBest@(Just currentBestCandidate)
  | candidateIsDefinitionScoped nextCandidate && not (candidateIsDefinitionScoped currentBestCandidate) =
      candidate
  | not (candidateIsDefinitionScoped nextCandidate) && candidateIsDefinitionScoped currentBestCandidate =
      currentBest
  | binaryLggSharedStructure (acResult nextCandidate) > binaryLggSharedStructure (acResult currentBestCandidate) =
      candidate
  | otherwise =
      currentBest

candidateIsDefinitionScoped :: AbstractionCandidate -> Bool
candidateIsDefinitionScoped =
  wellScopedDefinitionPattern . binaryLggPattern . acResult

shapeBuckets :: CandidateSite -> [ShapeBucket]
shapeBuckets site =
  [ ShapeBucket
      { sbRootTag = fixRootTag (csTerm site),
        sbSizeBand = sizeBand,
        sbFreeScopeWidth = csFreeScopeWidth site,
        sbSignature = csSignature site,
        sbTypeEvidence = typeEvidence
      }
  | sizeBand <- overlappingSizeBands (csSize site),
    typeEvidence <- Nothing : maybe [] (pure . Just) (csTypeEvidence site)
  ]

overlappingSizeBands :: Int -> [Int]
overlappingSizeBands size =
  nub (fmap (max 0 . (`div` 4)) [size - 2, size, size + 2])

sitePairKey :: CandidateSite -> CandidateSite -> (Int, Int, Down Int, Int, Int)
sitePairKey leftSite rightSite =
  ( abs (csSize leftSite - csSize rightSite),
    abs (csFreeScopeWidth leftSite - csFreeScopeWidth rightSite),
    Down (min (csSize leftSite) (csSize rightSite)),
    min (csOrdinal leftSite) (csOrdinal rightSite),
    max (csOrdinal leftSite) (csOrdinal rightSite)
  )

fixRootTag :: Fix HsExprF -> HsExprTag
fixRootTag (Fix node) =
  hsExprRootTag node

hsExprRootTag :: HsExprF child -> HsExprTag
hsExprRootTag = \case
  VarF {} -> VarTag
  AppF {} -> AppTag
  LamF {} -> LamTag
  LetF {} -> LetTag
  OpAppF {} -> OpAppTag
  SectionLF {} -> SectionLTag
  SectionRF {} -> SectionRTag
  ParF {} -> ParTag
  LitF {} -> LitTag
  OverLitF {} -> OverLitTag
  IfF {} -> IfTag
  CaseF {} -> CaseTag
  DoF {} -> DoTag
  NegF {} -> NegTag
  ExplicitListF {} -> ExplicitListTag
  ExplicitTupleF {} -> ExplicitTupleTag
  RecordConF {} -> RecordConTag
  RecordUpdF {} -> RecordUpdTag
  ArithSeqF {} -> ArithSeqTag
  GuardedF {} -> GuardedTag
  ClausesF {} -> ClausesTag
  MultiIfF {} -> MultiIfTag
  ExprWithTySigF {} -> ExprWithTySigTag
  AppTypeF {} -> AppTypeTag
  OpaqueF {} -> OpaqueTag

extractionSignatureAt ::
  NebulaSizeExtractionSections ->
  ScopeCtx ->
  ClassId ->
  Maybe TagSignature
extractionSignatureAt sizeSections contextValue classId =
  Map.lookup contextValue (nssSignatures sizeSections)
    >>= IntMap.lookup (classIdKey classId)

choiceLabelsForSection ::
  ContextualExtractionSection HsExprF NebulaAnalysis ScopeCtx Int ->
  IntMap TagSignature
choiceLabelsForSection section =
  fmap
    (cata (\nodeLabels -> tagSignatureFromTag (hsExprRootTag nodeLabels) <> foldMap id nodeLabels) . erTerm)
    (extractAllFromChoiceSection (cesChoiceSection section))
