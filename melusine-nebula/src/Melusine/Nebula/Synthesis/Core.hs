{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}

module Melusine.Nebula.Synthesis.Core
  ( SynthesizedName (..),
    SynthesizedSite (..),
    SynthesizedDefinition (..),
    CandidateRejection (..),
    RecordOwnershipFinding (..),
    RecordOwnershipKind (..),
    CandidateSiteLabel (..),
    RejectedCandidate (..),
    PlanStagingReport (..),
    SynthesisOutcome (..),
    synthesizeAbstractions,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Char (isAlphaNum, isLower, isUpper, toLower, toUpper)
import Data.Foldable (toList)
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List (find, intercalate, mapAccumL, nub, sortOn)
import Data.Map.Lazy qualified as LazyMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid (First (..), getFirst)
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Data.Traversable (mapAccumM)
import GHC.Types.Name.Occurrence (mkVarOcc, occNameString)
import GHC.Types.Name.Reader (mkRdrUnqual, rdrNameOcc)
import Melusine.Nebula.Discovery.Choose
  ( AbstractionCandidate (..),
    CandidateSite (..),
    CandidateSiteKind (..),
    ChosenBinding (..),
    resolvePatternClass,
    sharedAbstractionCandidates,
  )
import Melusine.Nebula.Core
  ( NebulaConfig (..),
    NebulaError (..),
    NebulaAnalysis,
  )
import Melusine.Nebula.Rewrite.Corpus (RuleCorpus)
import Melusine.Nebula.Source.Ingest (IngestedModule (..))
import Melusine.Nebula.Harvest.Cluster
  ( AbstractionCluster (..),
    ClusterSiteArgs (..),
    abstractionClusters,
    clusterPatternVarKeys,
  )
import Melusine.Nebula.Harvest.Core (HarvestState (..))
import Melusine.Nebula.Harvest.Maintain (HarvestDelta (..), advanceHarvest)
import Melusine.Nebula.Harvest.Pairs (admittedSitePairs)
import Melusine.Nebula.Synthesis.Diagnostics (structuralDiagnosticRejections)
import Melusine.Nebula.Synthesis.Scope (wellScopedDefinitionPattern, wellScopedDefinitionTerm)
import Melusine.Nebula.Synthesis.Types
import Melusine.Nebula.Rewrite.Saturate
  ( SaturatedModule,
    saturateEditedContextGraph,
    smContextGraph,
  )
import Moonlight.Core (Pattern (..), binderIdKey)
import Moonlight.Core qualified as EGraph
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( BinderAnn (..),
    ConvertedModule (..),
    FreeScopeSummary,
    GuardedAltF (..),
    HsExprF (..),
    HsGuardStmtF (..),
    HsPatF (..),
    HsStmtF (..),
    HsVarRef (..),
    LetMode (..),
    LetRecursion (..),
    ScopeCtx (..),
    ScopeId,
    ScopeIndex,
    SourceRegion (..),
    ScopedExpr (..),
    TopLevelBinding (..),
    binderIntroScope,
    deleteFreeScopeSummary,
    emptyFreeScopeSummary,
    eraseScopedExpr,
    freeScopeSummaryToList,
    mergeFreeScopeSummary,
    patBinders,
    scopeIsAncestorOf,
    scopeCtxLeq,
    scopeCtxJoin,
    scopeCtxMeet,
    scopeObservedContexts,
    scopeTopCtx,
    singletonFreeScopeSummary,
    traversePatBinders,
  )
import Moonlight.EGraph.Pure.Analysis (AnalysisSpec (asMake))
import Moonlight.EGraph.Pure.AntiUnify (BinaryLGGResult (..))
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
    ContextMergePlan,
    ContextRebaseReport (..),
    beginContextRebaseBatch,
    commitContextRebaseBatch,
    contextRebaseBatchBaseGraph,
    contextRebaseBatchClassSupportIndex,
    contextRebaseBatchSite,
    contextVisibleClassKeys,
    planContextMerges,
    stageENodeWithSupport,
    stageContextMerges,
    stageGlobalMerge,
  )
import Moonlight.EGraph.Pure.Context.Core (cegBase)
import Moonlight.EGraph.Pure.Context.Core qualified as ContextCore
import Moonlight.EGraph.Pure.Extraction (CostAlgebra (..), ExtractionResult (..), termSize)
import Moonlight.EGraph.Pure.Saturation.Extraction
  ( ContextualExtractionObstruction,
    contextualExtractBounded,
    contextualExtractFromSection,
    contextualExtractionSectionBounded,
  )
import Moonlight.EGraph.Pure.Types
  ( ClassId,
    EClass (..),
    ENode (..),
    EGraph,
    canonicalizeClassId,
    classIdKey,
    eGraphAnalysisSpec,
    lookupEClass,
  )
import Data.Fix (Fix (..))
import Moonlight.Sheaf.Context.Site
  ( classSupportExplicitCarrierForKey,
    supportCarrierReachableObjects,
    supportCarrierToSupport,
  )
import Moonlight.FiniteLattice
  ( ContextLattice,
    SupportBasis,
    principalSupport,
    supportBasis,
    supportGenerators
  )
import Moonlight.Pale.Ghc.Expr (ScopeLookupFailure)

emptyPlanStagingReport :: PlanStagingReport
emptyPlanStagingReport =
  PlanStagingReport
    { psrLocalizedMerges = 0,
      psrGlobalFallbackMerges = 0,
      psrLocalizedDefinitionMerges = 0,
      psrLocalizedApplicationMerges = 0,
      psrGlobalDefinitionFallbackMerges = 0,
      psrGlobalApplicationFallbackMerges = 0,
      psrDirtyContextCount = 0
    }

synthesizeAbstractions ::
  NebulaConfig ->
  IngestedModule ->
  RuleCorpus ->
  SaturatedModule ->
  HarvestState ->
  Either NebulaError SynthesisOutcome
synthesizeAbstractions config ingested corpus saturated preHarvest = do
  let preBindings = hsBindings preHarvest
      preTotal = sum (fmap cbExtractedSize preBindings)
      contextGraph1 = smContextGraph saturated
      sites = hsSites preHarvest
  discoveredCandidates <-
    fmap
      (filter ((>= ncDiagnosticMinShared config) . binaryLggSharedStructure . acResult))
      ( sharedAbstractionCandidates
          config
          saturated
          ( admittedSitePairs
              (ncAntiUnifyMaxPairs config)
              (hsSites preHarvest)
              (hsPairs preHarvest)
          )
      )
  discoveredClusters <-
    first
      (NebulaSynthesisError . ("cluster gluing failed: " <>) . show)
      (abstractionClusters config discoveredCandidates)
  let structuralDiagnostics =
        structuralDiagnosticRejections (imPath ingested) (imSource ingested) sites
  plannedCandidates <-
    traverse (planCluster ingested contextGraph1) discoveredClusters
  let (negativeRejects, eligiblePlans) = splitByEstimate plannedCandidates
      rankedEligiblePlans = sortOn (Down . cpEstimatedWin) eligiblePlans
  (visibilityRejects, admittedEligiblePlans) <-
    admissiblePlans config ingested contextGraph1 rankedEligiblePlans
  let (overlapRejects, selectedPlans) = greedyNonOverlapping admittedEligiblePlans
      preStageRejects = negativeRejects <> visibilityRejects <> overlapRejects <> structuralDiagnostics
  if null selectedPlans
    then
      Right
        SynthesisOutcome
          { soDefinitions = [],
            soEstimatedWin = 0,
            soRealizedWin = 0,
            soPreExtractedTotal = preTotal,
            soPostExtractedTotal = preTotal,
            soRejected = preStageRejects,
            soStagingReport = emptyPlanStagingReport,
            soHarvestDecision = Nothing,
            soBindings = preBindings,
            soSaturatedModule = saturated
          }
    else do
      (definitions, stagingReport, rebaseReport, contextGraph2) <-
        stageSelectedPlans ingested preBindings contextGraph1 selectedPlans
      resaturated <-
        saturateEditedContextGraph mempty corpus contextGraph2 saturated
      (harvestDelta, postHarvest) <-
        advanceHarvest
          config
          ingested
          (psrGlobalFallbackMerges stagingReport > 0)
          rebaseReport
          contextGraph2
          resaturated
          preHarvest
      let postBindings = hsBindings postHarvest
      let definitionTotal = sum (fmap sdSize definitions)
          postTotal = sum (fmap cbExtractedSize postBindings) + definitionTotal
          realizedWin = preTotal - postTotal
          estimatedWin = sum (fmap cpEstimatedWin selectedPlans)
      Right $
        if realizedWin > 0
          then
            SynthesisOutcome
              { soDefinitions = definitions,
                soEstimatedWin = estimatedWin,
                soRealizedWin = realizedWin,
                soPreExtractedTotal = preTotal,
                soPostExtractedTotal = postTotal,
                soRejected = preStageRejects,
                soStagingReport = stagingReport,
                soHarvestDecision = Just (hdDecision harvestDelta),
                soBindings = postBindings,
                soSaturatedModule = resaturated
              }
          else
            SynthesisOutcome
              { soDefinitions = [],
                soEstimatedWin = estimatedWin,
                soRealizedWin = realizedWin,
                soPreExtractedTotal = preTotal,
                soPostExtractedTotal = postTotal,
                soRejected =
                  negativeRejects
                    <> visibilityRejects
                    <> overlapRejects
                    <> structuralDiagnostics
                    <> [ RejectedCandidate
                           { rejSites = planSiteLabels plan,
                             rejReason = RejectedRealizedRegression,
                             rejEstimatedWin = cpEstimatedWin plan,
                             rejRealizedWin = Just realizedWin
                           }
                       | plan <- selectedPlans
                       ],
                soStagingReport = stagingReport,
                soHarvestDecision = Just (hdDecision harvestDelta),
                soBindings = preBindings,
                soSaturatedModule = saturated
              }

planCluster ::
  IngestedModule ->
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  AbstractionCluster ->
  Either NebulaError (CandidatePlan ClassId)
planCluster ingested contextGraph cluster = do
  let body = aclBody cluster
      canonicalize = canonicalizeClassId (cegBase contextGraph)
      occurrenceKeys = clusterPatternVarKeys body
      distinctKeys = nub occurrenceKeys
      clusterLabel = intercalate "/" (fmap (csBindingName . csaSite) (aclSites cluster))
      siteArgsRows = aclSites cluster
  varVectors <-
    traverse
      ( \varKey ->
          fmap
            (varKey,)
            (traverse (fmap canonicalize . classForVar clusterLabel varKey) siteArgsRows)
      )
      distinctKeys
  let distinctVectors = nub (fmap snd varVectors)
      slotByVar =
        IntMap.fromList
          (fmap (fmap (`slotIndexFor` distinctVectors)) varVectors)
  siteRows <-
    traverse
      ( \(siteIndex, siteArgs) ->
          CandidatePlanSite
            (csaSite siteArgs)
            (canonicalize (csClass (csaSite siteArgs)))
            <$> traverse (classAtSite clusterLabel siteIndex) distinctVectors
      )
      (zip [0 ..] siteArgsRows)
  let distinctCount = length distinctVectors
      occurrenceCount = length occurrenceKeys
      sharedCount = aclSharedStructure cluster
      definitionSize = distinctCount + sharedCount + occurrenceCount
      perSideWin = (sharedCount - distinctCount - 1) + (occurrenceCount - distinctCount)
  joinContext <- joinPlanSiteContexts (cmScopeIndex (imConverted ingested)) siteRows
  Right
    CandidatePlan
      { cpSites = siteRows,
        cpJoinContext = joinContext,
        cpBody = body,
        cpSlotByVar = slotByVar,
        cpSlotCount = distinctCount,
        cpEstimatedWin = length siteRows * perSideWin - definitionSize
      }

classForVar :: String -> Int -> ClusterSiteArgs -> Either NebulaError ClassId
classForVar clusterLabel varKey siteArgs =
  maybe
    (Left (NebulaSynthesisError ("anti-unification substitution misses variable " <> show varKey <> " for " <> clusterLabel)))
    Right
    (IntMap.lookup varKey (csaArgsByVar siteArgs))

classAtSite :: String -> Int -> [ClassId] -> Either NebulaError ClassId
classAtSite clusterLabel siteIndex classVector =
  maybe
    (Left (NebulaSynthesisError ("argument vector misses site " <> show siteIndex <> " for " <> clusterLabel)))
    Right
    (indexMaybe classVector siteIndex)

slotIndexFor :: Eq value => value -> [value] -> Int
slotIndexFor targetValue =
  length . takeWhile (/= targetValue)

scopeLookupFailure :: String -> ScopeLookupFailure -> NebulaError
scopeLookupFailure label failure =
  NebulaSynthesisError (label <> ": " <> show failure)

scopeLookup :: String -> Either ScopeLookupFailure value -> Either NebulaError value
scopeLookup label =
  first (scopeLookupFailure label)

joinPlanSiteContexts :: ScopeIndex -> [CandidatePlanSite argument] -> Either NebulaError ScopeCtx
joinPlanSiteContexts scopeIndex sites =
  case sites of
    [] ->
      scopeLookup "candidate plan top context" (scopeTopCtx scopeIndex)
    firstSite : restSites ->
      foldM
        ( \joinedContext site ->
            scopeLookup
              "candidate plan context join"
              (scopeCtxJoin scopeIndex (csContext (cpsSite site)) joinedContext)
        )
        (csContext (cpsSite firstSite))
        restSites

splitByEstimate :: [CandidatePlan argument] -> ([RejectedCandidate], [CandidatePlan argument])
splitByEstimate =
  foldr
    ( \plan (rejects, keeps) ->
        case estimateRejection plan of
          Nothing ->
            (rejects, plan : keeps)
          Just rejection ->
            (rejectedCandidate rejection plan : rejects, keeps)
    )
    ([], [])

estimateRejection :: CandidatePlan argument -> Maybe CandidateRejection
estimateRejection plan
  | cpEstimatedWin plan <= 0 =
      Just RejectedNoEstimatedWin
  | cpSlotCount plan <= 0 =
      Just RejectedNoDistinctArgs
  | otherwise =
      Nothing

admissiblePlans ::
  NebulaConfig ->
  IngestedModule ->
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  [CandidatePlan ClassId] ->
  Either NebulaError ([RejectedCandidate], [CandidatePlan AdmittedArgument])
admissiblePlans config ingested contextGraph plans =
  do
    captureSections <-
      captureExtractionSections config ingested sourceContexts contextGraph plans
    foldr (classify captureSections) (Right ([], [])) plans
  where
    sourceContexts =
      scopedClassContextIndex ingested contextGraph
    classify captureSections plan accumulated = do
      (rejects, keeps) <- accumulated
      admission <- admitPlan config ingested sourceContexts contextGraph captureSections plan
      pure $
        case admission of
          Right admittedPlan ->
            (rejects, admittedPlan : keeps)
          Left rejection ->
            (rejectedCandidate rejection plan : rejects, keeps)

admitPlan ::
  NebulaConfig ->
  IngestedModule ->
  IntMap.IntMap [ScopeCtx] ->
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  CaptureExtractionSections ->
  CandidatePlan ClassId ->
  Either NebulaError (Either CandidateRejection (CandidatePlan AdmittedArgument))
admitPlan config ingested sourceContexts contextGraph captureSections plan =
  if wellScopedDefinitionPattern (cpBody plan)
    then do
      maybeSites <- traverse admitSite (cpSites plan)
      pure $
        case sequenceA maybeSites of
          Just sites ->
            Right plan {cpSites = sites}
          Nothing ->
            Left RejectedNotVisible
    else pure (Left RejectedScopeEscape)
  where
    admitSite site = do
      maybeArguments <-
        traverse
          (admitArgument config ingested sourceContexts contextGraph captureSections (csContext (cpsSite site)))
          (cpsArguments site)
      pure (fmap (\arguments -> site {cpsArguments = arguments}) (sequenceA maybeArguments))

admitArgument ::
  NebulaConfig ->
  IngestedModule ->
  IntMap.IntMap [ScopeCtx] ->
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  CaptureExtractionSections ->
  ScopeCtx ->
  ClassId ->
  Either NebulaError (Maybe AdmittedArgument)
admitArgument config ingested sourceContexts contextGraph captureSections joinContext classId = do
    visibleAtJoin <- argumentVisibleAtJoin contextGraph joinContext classId
    if visibleAtJoin
      then pure
        ( Just
            AdmittedArgument
              { aaOriginalClass = classId,
                aaRealization = VisibleAtJoin
              }
        )
      else do
        maybeRepresentative <-
          closedArgumentRepresentative config ingested sourceContexts contextGraph captureSections joinContext classId
        pure
          ( fmap
              ( \representative ->
                  AdmittedArgument
                    { aaOriginalClass = classId,
                      aaRealization = MaterializedAtJoin representative
                    }
              )
              maybeRepresentative
          )

argumentVisibleAtJoin ::
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  ScopeCtx ->
  ClassId ->
  Either NebulaError Bool
argumentVisibleAtJoin contextGraph joinContext classId =
  fmap
    (IntSet.member (classIdKey classId))
    ( first
        NebulaContextSupportError
        (contextVisibleClassKeys joinContext contextGraph)
    )

closedArgumentRepresentative ::
  NebulaConfig ->
  IngestedModule ->
  IntMap.IntMap [ScopeCtx] ->
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  CaptureExtractionSections ->
  ScopeCtx ->
  ClassId ->
  Either NebulaError (Maybe (Fix HsExprF))
closedArgumentRepresentative config ingested sourceContexts contextGraph captureSections joinContext classId =
  foldM
    firstClosedRepresentative
    Nothing
    (argumentOwnContexts sourceContexts contextGraph classId)
  where
    scopeIndex = cmScopeIndex (imConverted ingested)

    firstClosedRepresentative maybeFound contextValue =
      case maybeFound of
        Just {} ->
          pure maybeFound
        Nothing ->
          extractClosedArgumentAt contextValue

    extractClosedArgumentAt contextValue =
      case captureExtractAt config scopeIndex contextGraph captureSections joinContext contextValue classId of
        Right (Just extractionResult) ->
          case ccScopeLookupFailure (erCost extractionResult) of
            Just failure ->
              Left (scopeLookupFailure "capture extraction scope lookup" failure)
            Nothing
              | ccEscaping (erCost extractionResult) == 0 ->
                  Right (Just (erTerm extractionResult))
              | otherwise ->
                  Right Nothing
        _ ->
          Right Nothing

argumentOwnContexts ::
  IntMap.IntMap [ScopeCtx] ->
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  ClassId ->
  [ScopeCtx]
argumentOwnContexts sourceContexts contextGraph classId =
  nub
    ( IntMap.findWithDefault [] (classIdKey canonicalClass) sourceContexts
        <> argumentSupportContexts contextGraph classId
    )
  where
    canonicalClass =
      canonicalizeClassId (cegBase contextGraph) classId

argumentSupportContexts ::
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  ClassId ->
  [ScopeCtx]
argumentSupportContexts contextGraph classId =
  nub
    ( foldMap
        explicitSupportGenerators
        [classId, canonicalClass]
    )
  where
    site =
      ContextCore.cegSite contextGraph

    supportIndex =
      ContextCore.cegClassSupportIndex contextGraph

    explicitSupportGenerators candidateClass =
      maybe
        []
        (either (const []) supportGenerators . supportCarrierToSupport site)
        (classSupportExplicitCarrierForKey supportIndex (classIdKey candidateClass))

    canonicalClass =
      canonicalizeClassId (cegBase contextGraph) classId

scopedClassContextIndex ::
  IngestedModule ->
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  IntMap.IntMap [ScopeCtx]
scopedClassContextIndex ingested contextGraph =
  IntMap.fromListWith
    (<>)
    ( foldMap
        (scopedClassContextRows (cegBase contextGraph) . tlbScopedTerm)
        (cmBindings (imConverted ingested))
    )

scopedClassContextRows ::
  EGraph HsExprF NebulaAnalysis ->
  ScopedExpr ->
  [(Int, [ScopeCtx])]
scopedClassContextRows baseGraph scopedExpr =
  selfRow <> foldMap (scopedClassContextRows baseGraph) (seNode scopedExpr)
  where
    selfRow =
      case resolvePatternClass baseGraph (eraseScopedExpr scopedExpr) of
        Nothing ->
          []
        Just classId ->
          [(classIdKey (canonicalizeClassId baseGraph classId), [ActualScope (seOccScope scopedExpr)])]

captureCostAlgebra :: ScopeIndex -> ScopeCtx -> CostAlgebra HsExprF CaptureCost
captureCostAlgebra scopeIndex joinContext =
  CostAlgebra (captureNodeCost scopeIndex joinContext)

captureNodeCost :: ScopeIndex -> ScopeCtx -> HsExprF CaptureCost -> CaptureCost
captureNodeCost scopeIndex joinContext nodeValue =
  let scopeResult = do
        nodeFreeScopes <- captureNodeFreeScopes scopeIndex nodeValue
        nodeEscapingCount <- escapingFreeScopeCount scopeIndex joinContext nodeFreeScopes
        pure (nodeEscapingCount, nodeFreeScopes)
      localFailure =
        either Just (const Nothing) scopeResult
      childFailure =
        getFirst (foldMap (First . ccScopeLookupFailure) nodeValue)
      scopeFailure =
        childFailure <|> localFailure
      (escapingCount, freeScopes) =
        either (const (maxBound, emptyFreeScopeSummary)) id scopeResult
   in CaptureCost
        { ccEscaping = maybe escapingCount (const maxBound) scopeFailure,
          ccSize = 1 + foldr ((+) . ccSize) 0 nodeValue,
          ccFreeScopes = freeScopes,
          ccScopeLookupFailure = scopeFailure
        }

captureNodeFreeScopes :: ScopeIndex -> HsExprF CaptureCost -> Either ScopeLookupFailure FreeScopeSummary
captureNodeFreeScopes scopeIndex = \case
  VarF (GlobalName _) ->
    Right emptyFreeScopeSummary
  VarF (LocalName binderAnn) ->
    singletonFreeScopeSummary <$> binderIntroScope scopeIndex (baId binderAnn)
  LamF binderAnn bodyCost ->
    deleteBinderScope scopeIndex binderAnn (ccFreeScopes bodyCost)
  LetF letModeValue bindingValues bodyCost ->
    captureFreeScopesLet scopeIndex letModeValue bindingValues (ccFreeScopes bodyCost)
  CaseF scrutineeCost branchValues -> do
    branchFreeScopes <- traverse (captureFreeScopesCaseAlternative scopeIndex) branchValues
    mergeFreeScopeSummaries scopeIndex (ccFreeScopes scrutineeCost : branchFreeScopes)
  DoF statementValues ->
    captureFreeScopesDo scopeIndex statementValues
  GuardedF guardedAlts -> do
    altFreeScopes <- traverse (captureFreeScopesGuardedAlt scopeIndex) guardedAlts
    mergeFreeScopeSummaries scopeIndex altFreeScopes
  MultiIfF guardedAlts -> do
    altFreeScopes <- traverse (captureFreeScopesGuardedAlt scopeIndex) guardedAlts
    mergeFreeScopeSummaries scopeIndex altFreeScopes
  ClausesF clauseValues -> do
    clauseFreeScopes <- traverse (captureFreeScopesClause scopeIndex) clauseValues
    mergeFreeScopeSummaries scopeIndex clauseFreeScopes
  nodeValue ->
    mergeFreeScopeSummaries scopeIndex (fmap ccFreeScopes (toList nodeValue))

captureFreeScopesLet ::
  ScopeIndex ->
  LetMode ->
  [(HsPatF, CaptureCost)] ->
  FreeScopeSummary ->
  Either ScopeLookupFailure FreeScopeSummary
captureFreeScopesLet scopeIndex letModeValue bindingValues bodyFree0 = do
  bodyFree <-
    foldM
      ( \accumulated (rowPattern, _) ->
          deletePatBinderScopes scopeIndex rowPattern accumulated
      )
      bodyFree0
      bindingValues
  rhsFree <-
    case lmRecursion letModeValue of
      NonRecursiveBinds ->
        mergeFreeScopeSummaries scopeIndex (fmap (ccFreeScopes . snd) bindingValues)
      RecursiveOpaqueBinds ->
        traverse
          (\(rowPattern, rhsCost) -> deletePatBinderScopes scopeIndex rowPattern (ccFreeScopes rhsCost))
          bindingValues
          >>= mergeFreeScopeSummaries scopeIndex
  mergeFreeScopeSummary scopeIndex rhsFree bodyFree

captureFreeScopesCaseAlternative :: ScopeIndex -> (HsPatF, CaptureCost) -> Either ScopeLookupFailure FreeScopeSummary
captureFreeScopesCaseAlternative scopeIndex (casePattern, branchCost) =
  deletePatBinderScopes scopeIndex casePattern (ccFreeScopes branchCost)

captureFreeScopesClause :: ScopeIndex -> ([HsPatF], CaptureCost) -> Either ScopeLookupFailure FreeScopeSummary
captureFreeScopesClause scopeIndex (clausePatterns, bodyCost) =
  foldM
    ( \summaryValue clausePattern ->
        deletePatBinderScopes scopeIndex clausePattern summaryValue
    )
    (ccFreeScopes bodyCost)
    clausePatterns

captureFreeScopesDo :: ScopeIndex -> [HsStmtF CaptureCost] -> Either ScopeLookupFailure FreeScopeSummary
captureFreeScopesDo scopeIndex =
  foldr
    ( \statementValue laterFree ->
        captureFreeScopesStmt scopeIndex statementValue =<< laterFree
    )
    (Right emptyFreeScopeSummary)

captureFreeScopesStmt :: ScopeIndex -> HsStmtF CaptureCost -> FreeScopeSummary -> Either ScopeLookupFailure FreeScopeSummary
captureFreeScopesStmt scopeIndex statementValue laterFree =
  case statementValue of
    BindStmtF bindPattern rhsCost -> do
      scopedLaterFree <- deletePatBinderScopes scopeIndex bindPattern laterFree
      mergeFreeScopeSummary scopeIndex (ccFreeScopes rhsCost) scopedLaterFree
    BodyStmtF exprCost ->
      mergeFreeScopeSummary scopeIndex (ccFreeScopes exprCost) laterFree
    LetStmtF letModeValue bindingValues ->
      captureFreeScopesLet scopeIndex letModeValue bindingValues laterFree

captureFreeScopesGuardedAlt :: ScopeIndex -> GuardedAltF CaptureCost -> Either ScopeLookupFailure FreeScopeSummary
captureFreeScopesGuardedAlt scopeIndex guardedAlt =
  captureFreeScopesGuardStmts scopeIndex (gaGuards guardedAlt) (ccFreeScopes (gaBody guardedAlt))

captureFreeScopesGuardStmts ::
  ScopeIndex ->
  [HsGuardStmtF CaptureCost] ->
  FreeScopeSummary ->
  Either ScopeLookupFailure FreeScopeSummary
captureFreeScopesGuardStmts scopeIndex guardStatements bodyFree =
  foldr
    ( \guardStatement laterFree ->
        captureFreeScopesGuardStmt scopeIndex guardStatement =<< laterFree
    )
    (Right bodyFree)
    guardStatements

captureFreeScopesGuardStmt ::
  ScopeIndex ->
  HsGuardStmtF CaptureCost ->
  FreeScopeSummary ->
  Either ScopeLookupFailure FreeScopeSummary
captureFreeScopesGuardStmt scopeIndex guardStatement laterFree =
  case guardStatement of
    GuardBoolF exprCost ->
      mergeFreeScopeSummary scopeIndex (ccFreeScopes exprCost) laterFree
    GuardPatF guardPattern rhsCost -> do
      scopedLaterFree <- deletePatBinderScopes scopeIndex guardPattern laterFree
      mergeFreeScopeSummary scopeIndex (ccFreeScopes rhsCost) scopedLaterFree
    GuardLetF letModeValue bindingValues ->
      captureFreeScopesLet scopeIndex letModeValue bindingValues laterFree

mergeFreeScopeSummaries :: ScopeIndex -> [FreeScopeSummary] -> Either ScopeLookupFailure FreeScopeSummary
mergeFreeScopeSummaries scopeIndex =
  foldr
    (\summaryValue mergedRest -> mergeFreeScopeSummary scopeIndex summaryValue =<< mergedRest)
    (Right emptyFreeScopeSummary)

deleteBinderScope :: ScopeIndex -> BinderAnn -> FreeScopeSummary -> Either ScopeLookupFailure FreeScopeSummary
deleteBinderScope scopeIndex binderAnn summaryValue =
  flip deleteFreeScopeSummary summaryValue <$> binderIntroScope scopeIndex (baId binderAnn)

deletePatBinderScopes :: ScopeIndex -> HsPatF -> FreeScopeSummary -> Either ScopeLookupFailure FreeScopeSummary
deletePatBinderScopes scopeIndex patternValue summaryValue =
  foldr
    ( \binderAnn accumulated ->
        deleteBinderScope scopeIndex binderAnn =<< accumulated
    )
    (Right summaryValue)
    (patBinders patternValue)

escapingFreeScopeCount :: ScopeIndex -> ScopeCtx -> FreeScopeSummary -> Either ScopeLookupFailure Int
escapingFreeScopeCount scopeIndex joinContext =
  foldM countEscapingScope 0 . freeScopeSummaryToList
  where
    countEscapingScope countValue freeScope = do
      inScope <- freeScopeInScopeAtJoin scopeIndex joinContext freeScope
      pure $
        if inScope
          then countValue
          else countValue + 1

freeScopeInScopeAtJoin :: ScopeIndex -> ScopeCtx -> ScopeId -> Either ScopeLookupFailure Bool
freeScopeInScopeAtJoin scopeIndex joinContext freeScope =
  case joinContext of
    ActualScope joinScope ->
      scopeIsAncestorOf scopeIndex freeScope joinScope
    IncompatibleScope ->
      Right False

captureExtractionSections ::
  NebulaConfig ->
  IngestedModule ->
  IntMap.IntMap [ScopeCtx] ->
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  [CandidatePlan ClassId] ->
  Either NebulaError CaptureExtractionSections
captureExtractionSections config ingested sourceContexts contextGraph plans =
  fmap
    (LazyMap.fromList . fmap sectionRow . nub . concat)
    (traverse planCaptureSectionKeys plans)
  where
    scopeIndex =
      cmScopeIndex (imConverted ingested)

    sectionRow key@(joinContext, contextValue) =
      ( key,
        contextualExtractionSectionBounded
          (ncExtractionBudget config)
          contextValue
          mempty
          (captureCostAlgebra scopeIndex joinContext)
          contextGraph
      )

    planCaptureSectionKeys plan =
      fmap concat
        ( traverse
            (fmap concat . traverse (argumentCaptureSectionKeys (cpJoinContext plan)) . cpsArguments)
            (cpSites plan)
        )

    argumentCaptureSectionKeys joinContext classId = do
      visibleAtJoin <-
        argumentVisibleAtJoin contextGraph joinContext classId
      pure
        [ (joinContext, contextValue)
        | not visibleAtJoin,
          contextValue <- argumentOwnContexts sourceContexts contextGraph classId
        ]

captureExtractAt ::
  NebulaConfig ->
  ScopeIndex ->
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  CaptureExtractionSections ->
  ScopeCtx ->
  ScopeCtx ->
  ClassId ->
  Either (ContextualExtractionObstruction ScopeCtx) (Maybe (ExtractionResult HsExprF CaptureCost))
captureExtractAt config scopeIndex contextGraph sections joinContext contextValue classId =
  case LazyMap.lookup (joinContext, contextValue) sections of
    Just (Right section) ->
      snd <$> contextualExtractFromSection classId section
    Just (Left obstruction) ->
      Left obstruction
    Nothing ->
      contextualExtractBounded
        (ncExtractionBudget config)
        contextValue
        mempty
        (captureCostAlgebra scopeIndex joinContext)
        classId
        contextGraph

greedyNonOverlapping :: [CandidatePlan argument] -> ([RejectedCandidate], [CandidatePlan argument])
greedyNonOverlapping =
  finalize . foldl' claim (Set.empty, [], [], [])
  where
    finalize :: (Set.Set Int, [SourceRegion], [RejectedCandidate], [CandidatePlan argument]) -> ([RejectedCandidate], [CandidatePlan argument])
    finalize (_, _, rejected, kept) =
      (reverse rejected, reverse kept)
    claim ::
      (Set.Set Int, [SourceRegion], [RejectedCandidate], [CandidatePlan argument]) ->
      CandidatePlan argument ->
      (Set.Set Int, [SourceRegion], [RejectedCandidate], [CandidatePlan argument])
    claim (usedSeeds, usedRegions, rejected, kept) plan =
      let seeds = fmap (classIdKey . cpsSeed) (cpSites plan)
          regions = mapMaybe (csRegion . cpsSite) (cpSites plan)
       in if any (`Set.member` usedSeeds) seeds
            then (usedSeeds, usedRegions, rejectedCandidate RejectedOverlap plan : rejected, kept)
            else
              if any (regionOverlapsAny usedRegions) regions
                then (usedSeeds, usedRegions, rejectedCandidate RejectedRegionOverlap plan : rejected, kept)
                else (foldr Set.insert usedSeeds seeds, usedRegions <> regions, rejected, plan : kept)

regionOverlapsAny :: [SourceRegion] -> SourceRegion -> Bool
regionOverlapsAny regions region =
  any (regionsOverlap region) regions

regionsOverlap :: SourceRegion -> SourceRegion -> Bool
regionsOverlap leftRegion rightRegion =
  regionStart leftRegion < regionEnd rightRegion
    && regionStart rightRegion < regionEnd leftRegion

regionStart :: SourceRegion -> (Int, Int)
regionStart region =
  (srStartLine region, srStartCol region)

regionEnd :: SourceRegion -> (Int, Int)
regionEnd region =
  (srEndLine region, srEndCol region)

rejectedCandidate :: CandidateRejection -> CandidatePlan argument -> RejectedCandidate
rejectedCandidate rejection plan =
  RejectedCandidate
    { rejSites = planSiteLabels plan,
      rejReason = rejection,
      rejEstimatedWin = cpEstimatedWin plan,
      rejRealizedWin = Nothing
    }

planSiteLabels :: CandidatePlan argument -> [CandidateSiteLabel]
planSiteLabels =
  fmap (candidateSiteLabel . cpsSite) . cpSites


type PlanMergeCounts :: Type
data PlanMergeCounts = PlanMergeCounts
  { pmcLocalizedDefinitionMerges :: !Int,
    pmcLocalizedApplicationMerges :: !Int,
    pmcGlobalDefinitionFallbackMerges :: !Int,
    pmcGlobalApplicationFallbackMerges :: !Int
  }

instance Semigroup PlanMergeCounts where
  leftCounts <> rightCounts =
    PlanMergeCounts
      { pmcLocalizedDefinitionMerges = pmcLocalizedDefinitionMerges leftCounts + pmcLocalizedDefinitionMerges rightCounts,
        pmcLocalizedApplicationMerges = pmcLocalizedApplicationMerges leftCounts + pmcLocalizedApplicationMerges rightCounts,
        pmcGlobalDefinitionFallbackMerges = pmcGlobalDefinitionFallbackMerges leftCounts + pmcGlobalDefinitionFallbackMerges rightCounts,
        pmcGlobalApplicationFallbackMerges = pmcGlobalApplicationFallbackMerges leftCounts + pmcGlobalApplicationFallbackMerges rightCounts
      }

instance Monoid PlanMergeCounts where
  mempty =
    PlanMergeCounts
      { pmcLocalizedDefinitionMerges = 0,
        pmcLocalizedApplicationMerges = 0,
        pmcGlobalDefinitionFallbackMerges = 0,
        pmcGlobalApplicationFallbackMerges = 0
      }

type MergeDisposition :: Type
data MergeDisposition
  = MergeLocalized
  | MergeGlobalFallback

type MergeIntentOrigin :: Type
data MergeIntentOrigin
  = DefinitionMergeIntent
  | ApplicationMergeIntent

type NebulaMergeIntent :: Type
data NebulaMergeIntent = NebulaMergeIntent
  { nmiOrigin :: !MergeIntentOrigin,
    nmiAction :: !(NebulaMergeAction ScopeCtx)
  }

type NebulaMergeAction :: Type -> Type
data NebulaMergeAction c
  = NebulaGlobalMerge !ClassId !ClassId
  | NebulaLocalMerge !(ContextMergePlan c)

definitionMergeDispositionCounts :: MergeDisposition -> PlanMergeCounts
definitionMergeDispositionCounts = \case
  MergeLocalized ->
    mempty {pmcLocalizedDefinitionMerges = 1}
  MergeGlobalFallback ->
    mempty {pmcGlobalDefinitionFallbackMerges = 1}

applicationMergeDispositionCounts :: MergeDisposition -> PlanMergeCounts
applicationMergeDispositionCounts = \case
  MergeLocalized ->
    mempty {pmcLocalizedApplicationMerges = 1}
  MergeGlobalFallback ->
    mempty {pmcGlobalApplicationFallbackMerges = 1}

totalLocalizedMerges :: PlanMergeCounts -> Int
totalLocalizedMerges mergeCounts =
  pmcLocalizedDefinitionMerges mergeCounts + pmcLocalizedApplicationMerges mergeCounts

totalGlobalFallbackMerges :: PlanMergeCounts -> Int
totalGlobalFallbackMerges mergeCounts =
  pmcGlobalDefinitionFallbackMerges mergeCounts + pmcGlobalApplicationFallbackMerges mergeCounts

planStagingReportFromCommit ::
  PlanMergeCounts ->
  ContextRebaseReport HsExprF ScopeCtx ->
  PlanStagingReport
planStagingReportFromCommit mergeCounts rebaseReport =
  PlanStagingReport
    { psrLocalizedMerges = totalLocalizedMerges mergeCounts,
      psrGlobalFallbackMerges = totalGlobalFallbackMerges mergeCounts,
      psrLocalizedDefinitionMerges = pmcLocalizedDefinitionMerges mergeCounts,
      psrLocalizedApplicationMerges = pmcLocalizedApplicationMerges mergeCounts,
      psrGlobalDefinitionFallbackMerges = pmcGlobalDefinitionFallbackMerges mergeCounts,
      psrGlobalApplicationFallbackMerges = pmcGlobalApplicationFallbackMerges mergeCounts,
      psrDirtyContextCount = Set.size (crrDirtyContexts rebaseReport)
    }

stageSelectedPlans ::
  IngestedModule ->
  [ChosenBinding] ->
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  [CandidatePlan AdmittedArgument] ->
  Either NebulaError ([SynthesizedDefinition], PlanStagingReport, ContextRebaseReport HsExprF ScopeCtx, ContextEGraph HsExprF NebulaAnalysis ScopeCtx)
stageSelectedPlans ingested preBindings contextGraph1 selectedPlans = do
  let binderFloor = 1 + moduleMaxBinderKey (imConverted ingested)
      scopeIndex = cmScopeIndex (imConverted ingested)
      contextLattice = ContextCore.cegLattice contextGraph1
      bindingContexts = bindingContextIndex preBindings
      reservedNames = Set.fromList (fmap cbName preBindings)
  (definitions, _, mergeIntentsReversed, constructionBatch) <-
    foldM
      ( \(builtDefinitions, (usedNames, binderBase), accumulatedIntents, batchValue) plan -> do
          (definition, nextBinderBase, planIntents, batchValue') <- stagePlan contextLattice scopeIndex bindingContexts usedNames binderBase plan batchValue
          pure
            ( definition : builtDefinitions,
              (Set.insert (synthesizedNameText (sdName definition)) usedNames, nextBinderBase),
              reverse planIntents <> accumulatedIntents,
              batchValue'
            )
      )
      ([], (reservedNames, binderFloor), [], beginContextRebaseBatch contextGraph1)
      selectedPlans
  (mergeCounts, stagedBatch) <-
    foldM
      stageNebulaMergeIntent
      (mempty, constructionBatch)
      (reverse mergeIntentsReversed)
  (rebaseReport, contextGraph2) <-
    first (NebulaSynthesisError . show) (commitContextRebaseBatch stagedBatch)
  pure (reverse definitions, planStagingReportFromCommit mergeCounts rebaseReport, rebaseReport, contextGraph2)

bindingContextIndex :: [ChosenBinding] -> Map String ScopeCtx
bindingContextIndex =
  Map.fromList . fmap (\binding -> (cbName binding, cbContext binding))

type ReservedNames :: Type
type ReservedNames = Set.Set String

stagePlan ::
  ContextLattice ScopeCtx ->
  ScopeIndex ->
  Map String ScopeCtx ->
  ReservedNames ->
  Int ->
  CandidatePlan AdmittedArgument ->
  NebulaBatch ->
  Either NebulaError (SynthesizedDefinition, Int, [NebulaMergeIntent], NebulaBatch)
stagePlan contextLattice scopeIndex bindingContexts reservedNames binderBase plan batch0 = do
  let definitionName = synthesizedDefinitionName reservedNames plan
      definitionNameText = synthesizedNameText definitionName
      parameterCount = cpSlotCount plan
      parameterNames = synthesizedParameterNames plan
      binderAnns =
        [ BinderAnn
            { baId = toEnum (binderBase + slotIndex),
              baName = mkRdrUnqual (mkVarOcc parameterName)
            }
        | (slotIndex, parameterName) <- zip [0 .. parameterCount - 1] parameterNames
        ]
      (freshBody, nextBinderBase) =
        freshenDefinitionBodyBinders (binderBase + parameterCount) (cpBody plan)
  definitionSupport <- planSiteSupport contextLattice plan
  let definitionStageNode = stagePlanNode definitionSupport
  binderByVar <- binderAnnByVar plan binderAnns
  (binderVarClasses, batch1) <-
    foldM
      ( \(varClasses, batchValue) binderAnn -> do
          (varClass, batchValue') <- definitionStageNode (VarF (LocalName binderAnn)) batchValue
          pure (varClasses <> [varClass], batchValue')
      )
      ([], batch0)
      binderAnns
  varClassByVar <-
    traverse
      ( \slotIndex ->
          maybe
            (Left (NebulaSynthesisError "binder slot has no staged variable class"))
            Right
            (indexMaybe binderVarClasses slotIndex)
      )
      (cpSlotByVar plan)
  (bodyClass, batch2) <-
    stageBodyPattern definitionStageNode varClassByVar freshBody batch1
  (definitionClass, batch3) <-
    foldM
      (\(innerClass, batchValue) binderAnn -> definitionStageNode (LamF binderAnn innerClass) batchValue)
      (bodyClass, batch2)
      (reverse binderAnns)
  (nameClass, batch4) <-
    definitionStageNode (VarF (GlobalName (mkRdrUnqual (mkVarOcc definitionNameText)))) batch3
  definitionMergeIntents <-
    definitionMergeIntentsFor scopeIndex plan nameClass definitionClass batch4
  (mergeIntentsReversed, batch6) <-
    foldM
      ( \(intents, batchValue) site -> do
          (siteIntent, batchValue') <- stageSideApplication scopeIndex bindingContexts nameClass site batchValue
          pure (siteIntent : intents, batchValue')
      )
      (reverse definitionMergeIntents, batch4)
      (cpSites plan)
  definitionTerm <- definitionFixTerm binderAnns binderByVar freshBody
  pure
    ( SynthesizedDefinition
        { sdName = definitionName,
          sdSites = fmap (synthesizedSite . cpsSite) (cpSites plan),
          sdClass = definitionClass,
          sdTerm = definitionTerm,
          sdSize = termSize definitionTerm,
          sdEstimatedWin = cpEstimatedWin plan
        },
      nextBinderBase,
      reverse mergeIntentsReversed,
      batch6
    )

synthesizedDefinitionName :: ReservedNames -> CandidatePlan argument -> SynthesizedName
synthesizedDefinitionName reservedNames plan =
  SynthesizedName (firstAvailableName reservedNames (candidatePlanBaseName plan))

candidatePlanBaseName :: CandidatePlan argument -> String
candidatePlanBaseName plan =
  renderLowerCamelIdentifier (boundedNameWords (candidatePlanNameWords plan))

candidatePlanNameWords :: CandidatePlan argument -> [String]
candidatePlanNameWords plan
  | singleSourceSiteFamily siteNames =
      candidateBodyNameWords plan `orElseWords` distinctSiteWords
  | length distinctSiteWords > maxSynthesizedNameWords =
      candidateBodyNameWords plan `orElseWords` distinctSiteWords
  | otherwise =
      distinctSiteWords
  where
    siteNames =
      fmap (csBindingName . cpsSite) (cpSites plan)
    distinctSiteWords =
      mergedSiteNameWords (nub siteNames)

singleSourceSiteFamily :: [String] -> Bool
singleSourceSiteFamily siteNames =
  case nub siteNames of
    [_] ->
      True
    _ ->
      False

candidateBodyNameWords :: CandidatePlan argument -> [String]
candidateBodyNameWords plan =
  case bodyHeadWords (cpBody plan) of
    [] ->
      []
    wordsValue ->
      wordsValue <> ["Shared"]

boundedNameWords :: [String] -> [String]
boundedNameWords wordsValue =
  case take maxSynthesizedNameWords (dedupeAdjacentNameWords (filter (not . null) wordsValue)) of
    [] ->
      ["synthesized", "rewrite"]
    bounded ->
      bounded

dedupeAdjacentNameWords :: [String] -> [String]
dedupeAdjacentNameWords =
  foldr collectWord []
  where
    collectWord wordValue suffixWords =
      case suffixWords of
        nextWord : _
          | lowerWord wordValue == lowerWord nextWord ->
              suffixWords
        _ ->
          wordValue : suffixWords
    lowerWord =
      fmap toLower

maxSynthesizedNameWords :: Int
maxSynthesizedNameWords =
  5

orElseWords :: [String] -> [String] -> [String]
orElseWords primaryWords fallbackWords =
  case primaryWords of
    [] ->
      fallbackWords
    _ ->
      primaryWords

bodyHeadWords :: Pattern HsExprF -> [String]
bodyHeadWords = \case
  PatternVar _ ->
    []
  PatternNode nodeValue ->
    case nodeValue of
      VarF variableReference ->
        identifierWords (varRefName variableReference)
      AppF functionValue _ ->
        bodyHeadWords functionValue
      LamF _ bodyValue ->
        bodyHeadWords bodyValue
      LetF _ _ bodyValue ->
        bodyHeadWords bodyValue
      OpAppF _ operatorValue _ ->
        bodyHeadWords operatorValue
      SectionLF _ operatorValue ->
        bodyHeadWords operatorValue
      SectionRF operatorValue _ ->
        bodyHeadWords operatorValue
      ParF innerValue ->
        bodyHeadWords innerValue
      IfF {} ->
        ["branch"]
      CaseF {} ->
        ["case", "Branch"]
      DoF {} ->
        ["do", "Block"]
      NegF {} ->
        ["negated"]
      ExplicitListF {} ->
        ["list"]
      ExplicitTupleF {} ->
        ["tuple"]
      RecordConF constructorValue _ ->
        bodyHeadWords constructorValue
      RecordUpdF recordValue _ ->
        bodyHeadWords recordValue
      ArithSeqF {} ->
        ["arithmetic", "Sequence"]
      GuardedF {} ->
        ["guarded", "Branch"]
      ClausesF {} ->
        ["clause"]
      MultiIfF {} ->
        ["multi", "If"]
      ExprWithTySigF bodyValue _ ->
        bodyHeadWords bodyValue
      AppTypeF functionValue _ ->
        bodyHeadWords functionValue
      LitF {} ->
        ["literal"]
      OverLitF {} ->
        ["literal"]
      OpaqueF opaqueTag ->
        ["opaque", show opaqueTag]

varRefName :: HsVarRef -> String
varRefName = \case
  GlobalName rdrName ->
    occNameString (rdrNameOcc rdrName)
  LocalName binderAnn ->
    occNameString (rdrNameOcc (baName binderAnn))

mergedSiteNameWords :: [String] -> [String]
mergedSiteNameWords siteNames =
  case fmap identifierWords siteNames of
    [] ->
      ["synthesized", "rewrite"]
    firstWords : restWords ->
      firstWords <> foldMap distinctSuffix restWords
      where
        sharedPrefix =
          commonIdentifierPrefix (firstWords : restWords)
        distinctSuffix wordsValue =
          case drop (length sharedPrefix) wordsValue of
            [] -> wordsValue
            suffix -> suffix

commonIdentifierPrefix :: [[String]] -> [String]
commonIdentifierPrefix = \case
  [] ->
    []
  firstWords : restWords ->
    foldl' commonWordsPrefix firstWords restWords

commonWordsPrefix :: [String] -> [String] -> [String]
commonWordsPrefix leftWords rightWords =
  fmap fst (takeWhile (uncurry (==)) (zip leftWords rightWords))

identifierWords :: String -> [String]
identifierWords =
  concatMap camelWords . words . fmap sanitizeIdentifierChar
  where
    sanitizeIdentifierChar character
      | isAlphaNum character = character
      | otherwise = ' '

camelWords :: String -> [String]
camelWords =
  reverse . fmap reverse . foldl' collectChar []
  where
    collectChar [] character =
      [[character]]
    collectChar groups@(currentWord : remainingWords) character
      | isUpper character && any isLower currentWord =
          [character] : groups
      | otherwise =
          (character : currentWord) : remainingWords

renderLowerCamelIdentifier :: [String] -> String
renderLowerCamelIdentifier wordsValue =
  validValueName $
    case filter (not . null) wordsValue of
      [] ->
        "synthesizedRewrite"
      firstWord : remainingWords ->
        lowerInitial firstWord <> concatMap upperInitial remainingWords

lowerInitial :: String -> String
lowerInitial = \case
  [] -> []
  firstChar : restChars -> toLower firstChar : restChars

upperInitial :: String -> String
upperInitial = \case
  [] -> []
  firstChar : restChars -> toUpper firstChar : restChars

validValueName :: String -> String
validValueName candidateName =
  avoidReservedIdentifier $
    case filter isAlphaNum candidateName of
      [] ->
        "synthesizedRewrite"
      firstChar : _
        | isValueIdentifierStart firstChar ->
            filter isAlphaNum candidateName
      _ ->
        "synthesized" <> upperInitial (filter isAlphaNum candidateName)

isValueIdentifierStart :: Char -> Bool
isValueIdentifierStart character =
  isLower character || character == '_'

avoidReservedIdentifier :: String -> String
avoidReservedIdentifier candidateName =
  if Set.member candidateName reservedHaskellIdentifiers
    then candidateName <> "Value"
    else candidateName

reservedHaskellIdentifiers :: Set.Set String
reservedHaskellIdentifiers =
  Set.fromList
    [ "case",
      "class",
      "data",
      "default",
      "deriving",
      "do",
      "else",
      "foreign",
      "if",
      "import",
      "in",
      "infix",
      "infixl",
      "infixr",
      "instance",
      "let",
      "module",
      "newtype",
      "of",
      "then",
      "type",
      "where"
    ]

firstAvailableName :: ReservedNames -> String -> String
firstAvailableName reservedNames baseName =
  fromMaybe baseName (find (`Set.notMember` reservedNames) candidateNames)
  where
    candidateNames =
      baseName : fmap ((baseName <>) . show) [1 :: Int ..]

synthesizedParameterNames :: CandidatePlan AdmittedArgument -> [String]
synthesizedParameterNames plan =
  snd (mapAccumL allocateName Set.empty (fmap (parameterBaseName plan) [0 .. cpSlotCount plan - 1]))
  where
    allocateName usedNames baseName =
      let parameterName = firstAvailableName usedNames (validValueName baseName)
       in (Set.insert parameterName usedNames, parameterName)

parameterBaseName :: CandidatePlan AdmittedArgument -> Int -> String
parameterBaseName plan slotIndex =
  case nub (mapMaybe argumentSimpleName (slotArguments plan slotIndex)) of
    [simpleName] ->
      simpleName
    _ ->
      fallbackParameterName slotIndex

slotArguments :: CandidatePlan AdmittedArgument -> Int -> [AdmittedArgument]
slotArguments plan slotIndex =
  [ argument
  | site <- cpSites plan,
    Just argument <- [indexMaybe (cpsArguments site) slotIndex]
  ]

argumentSimpleName :: AdmittedArgument -> Maybe String
argumentSimpleName argument =
  case aaRealization argument of
    VisibleAtJoin ->
      Nothing
    MaterializedAtJoin representativeTerm ->
      simpleTermName representativeTerm

simpleTermName :: Fix HsExprF -> Maybe String
simpleTermName (Fix nodeValue) =
  case nodeValue of
    VarF (GlobalName rdrName) ->
      Just (occNameString (rdrNameOcc rdrName))
    VarF (LocalName binderAnn) ->
      Just (occNameString (rdrNameOcc (baName binderAnn)))
    _ ->
      Nothing

fallbackParameterName :: Int -> String
fallbackParameterName slotIndex =
  fromMaybe
    ("value" <> show slotIndex)
    (indexMaybe fallbackParameterNames slotIndex)

fallbackParameterNames :: [String]
fallbackParameterNames =
  [ "step",
    "value",
    "leftValue",
    "rightValue",
    "contextValue",
    "stateValue",
    "inputValue",
    "outputValue"
  ]

synthesizedSite :: CandidateSite -> SynthesizedSite
synthesizedSite site =
  SynthesizedSite
    { ssBindingName = csBindingName site,
      ssRegion = csRegion site,
      ssKind = csSiteKind site
    }

binderAnnByVar :: CandidatePlan argument -> [BinderAnn] -> Either NebulaError (IntMap.IntMap BinderAnn)
binderAnnByVar plan binderAnns =
  traverse
    ( \slotIndex ->
        maybe
          (Left (NebulaSynthesisError "binder slot index exceeds the minted binder list"))
          Right
          (indexMaybe binderAnns slotIndex)
    )
    (cpSlotByVar plan)

type StagePlanNode :: Type
type StagePlanNode =
  HsExprF ClassId ->
  NebulaBatch ->
  Either NebulaError (ClassId, NebulaBatch)

stagePlanNode :: SupportBasis ScopeCtx -> StagePlanNode
stagePlanNode supportValue node batchValue = do
  let baseGraph = contextRebaseBatchBaseGraph batchValue
  childAnalyses <-
    traverse
      ( \childClass ->
          maybe
            (Left (NebulaSynthesisError ("staged child class is missing from the base graph: " <> show childClass)))
            (Right . eClassData)
            (lookupEClass baseGraph (canonicalizeClassId baseGraph childClass))
      )
      node
  let nodeAnalysis = asMake (eGraphAnalysisSpec baseGraph) childAnalyses
  first
    (NebulaSynthesisError . show)
    (stageENodeWithSupport supportValue (ENode node) nodeAnalysis batchValue)

planSiteSupport :: ContextLattice ScopeCtx -> CandidatePlan argument -> Either NebulaError (SupportBasis ScopeCtx)
planSiteSupport contextLattice plan =
  first
    (NebulaSynthesisError . show)
    (supportBasis contextLattice generatorContexts)
  where
    generatorContexts =
      case Set.toAscList (Set.fromList (fmap (csContext . cpsSite) (cpSites plan))) of
        [] ->
          [cpJoinContext plan]
        siteContexts ->
          siteContexts

stageBodyPattern ::
  StagePlanNode ->
  IntMap.IntMap ClassId ->
  Pattern HsExprF ->
  NebulaBatch ->
  Either NebulaError (ClassId, NebulaBatch)
stageBodyPattern stageNode varClassByVar = go
  where
    go patternValue batchValue =
      case patternValue of
        PatternVar patternVar ->
          maybe
            (Left (NebulaSynthesisError ("definition body references an unbound variable slot: " <> show (EGraph.patternVarKey patternVar))))
            (\varClass -> Right (varClass, batchValue))
            (IntMap.lookup (EGraph.patternVarKey patternVar) varClassByVar)
        PatternNode node -> do
          (batchValue', stagedNode) <-
            mapAccumM
              ( \batchState childPattern -> do
                  (childClass, batchState') <- go childPattern batchState
                  pure (batchState', childClass)
              )
              batchValue
              node
          stageNode stagedNode batchValue'

definitionMergeIntentsFor ::
  ScopeIndex ->
  CandidatePlan AdmittedArgument ->
  ClassId ->
  ClassId ->
  NebulaBatch ->
  Either NebulaError [NebulaMergeIntent]
definitionMergeIntentsFor scopeIndex plan nameClass definitionClass batchValue = do
  mergeContexts <- definitionMergeContexts scopeIndex plan
  traverse
    ( \contextValue -> do
        mergeAction <-
          planNebulaMerge (Just contextValue) nameClass definitionClass batchValue
        pure
          NebulaMergeIntent
            { nmiOrigin = DefinitionMergeIntent,
              nmiAction = mergeAction
            }
    )
    mergeContexts

definitionMergeContexts :: ScopeIndex -> CandidatePlan argument -> Either NebulaError [ScopeCtx]
definitionMergeContexts scopeIndex plan = do
  maybeContext <- definitionMergeContext scopeIndex plan
  pure $
    case maybeContext of
      Just contextValue ->
        [contextValue]
      Nothing ->
        case Set.toAscList (Set.fromList (fmap (csContext . cpsSite) (cpSites plan))) of
          [] ->
            [cpJoinContext plan]
          siteContexts ->
            siteContexts

definitionMergeContext :: ScopeIndex -> CandidatePlan argument -> Either NebulaError (Maybe ScopeCtx)
definitionMergeContext scopeIndex plan = do
  maybeMeet <- meetPlanSiteContexts scopeIndex (cpSites plan)
  pure $
    if maybeMeet == Just (cpJoinContext plan)
      then Just (cpJoinContext plan)
      else Nothing

meetPlanSiteContexts :: ScopeIndex -> [CandidatePlanSite argument] -> Either NebulaError (Maybe ScopeCtx)
meetPlanSiteContexts scopeIndex sites =
  case sites of
    [] ->
      Right Nothing
    firstSite : restSites ->
      Just
        <$> foldM
          ( \meetContext site ->
              scopeLookup
                "candidate plan context meet"
                (scopeCtxMeet scopeIndex (csContext (cpsSite site)) meetContext)
          )
          (csContext (cpsSite firstSite))
          restSites

planNebulaMerge ::
  Maybe ScopeCtx ->
  ClassId ->
  ClassId ->
  NebulaBatch ->
  Either NebulaError (NebulaMergeAction ScopeCtx)
planNebulaMerge maybeContext leftClass rightClass batchValue =
  case maybeContext of
    Just contextValue -> do
      mergePlan <-
        first
          (NebulaSynthesisError . show)
          (planContextMerges [contextValue] leftClass rightClass batchValue)
      pure (NebulaLocalMerge mergePlan)
    Nothing ->
      Right (NebulaGlobalMerge leftClass rightClass)

stageNebulaMergeIntent ::
  (PlanMergeCounts, NebulaBatch) ->
  NebulaMergeIntent ->
  Either NebulaError (PlanMergeCounts, NebulaBatch)
stageNebulaMergeIntent (mergeCounts, batchValue) mergeIntent = do
  stagedBatch <-
    first (NebulaSynthesisError . show) $
      case nmiAction mergeIntent of
        NebulaGlobalMerge leftClass rightClass ->
          stageGlobalMerge leftClass rightClass batchValue
        NebulaLocalMerge mergePlan ->
          stageContextMerges mergePlan batchValue
  let mergeDisposition =
        case nmiAction mergeIntent of
          NebulaLocalMerge _ -> MergeLocalized
          NebulaGlobalMerge _ _ -> MergeGlobalFallback
      mergeCountsForOrigin =
        case nmiOrigin mergeIntent of
          DefinitionMergeIntent ->
            definitionMergeDispositionCounts mergeDisposition
          ApplicationMergeIntent ->
            applicationMergeDispositionCounts mergeDisposition
  pure (mergeCounts <> mergeCountsForOrigin, stagedBatch)

stageSideApplication ::
  ScopeIndex ->
  Map String ScopeCtx ->
  ClassId ->
  CandidatePlanSite AdmittedArgument ->
  NebulaBatch ->
  Either NebulaError (NebulaMergeIntent, NebulaBatch)
stageSideApplication scopeIndex bindingContexts nameClass site batch0 = do
  let siteStageNode =
        stagePlanNode (principalSupport (csContext (cpsSite site)))
  (batchAfterArguments, argClasses) <-
    mapAccumM
      ( \batchValue argument -> do
          (argClass, batchValue') <- stageArgument siteStageNode argument batchValue
          pure (batchValue', argClass)
      )
      batch0
      (cpsArguments site)
  (applicationClass, batchAfterApplication) <-
    foldM
      (\(headClass, batchValue) argClass -> siteStageNode (AppF headClass argClass) batchValue)
      (nameClass, batchAfterArguments)
      argClasses
  mergeContext <-
    applicationMergeContext scopeIndex bindingContexts batchAfterApplication site
  mergeAction <-
    planNebulaMerge mergeContext applicationClass (cpsSeed site) batchAfterApplication
  pure
    ( NebulaMergeIntent
        { nmiOrigin = ApplicationMergeIntent,
          nmiAction = mergeAction
        },
      batchAfterApplication
    )

applicationMergeContext ::
  ScopeIndex ->
  Map String ScopeCtx ->
  NebulaBatch ->
  CandidatePlanSite argument ->
  Either NebulaError (Maybe ScopeCtx)
applicationMergeContext scopeIndex bindingContexts batchValue site =
  case csSiteKind siteValue of
    BindingCandidateSite ->
      case Map.lookup (csBindingName siteValue) bindingContexts of
        Just bindingContext
          | siteContext == bindingContext -> do
              contained <- seedSupportReachContainedBySite scopeIndex batchValue siteContext (cpsSeed site)
              pure $
                if contained
                  then Just siteContext
                  else Nothing
        _ ->
          Right Nothing
    RegionCandidateSite
      | Just _ <- csRegion siteValue ->
          Right (Just siteContext)
    _ ->
      Right Nothing
  where
    siteValue =
      cpsSite site

    siteContext =
      csContext siteValue

seedSupportReachContainedBySite ::
  ScopeIndex ->
  NebulaBatch ->
  ScopeCtx ->
  ClassId ->
  Either NebulaError Bool
seedSupportReachContainedBySite scopeIndex batchValue siteContext classId = do
  maybeReachableContexts <- classExplicitSupportReachableContexts scopeIndex batchValue classId
  case maybeReachableContexts of
    Just reachableContexts@(_ : _) ->
      and
        <$> traverse
          (scopeLookup "support reach context order" . scopeCtxLeq scopeIndex siteContext)
          reachableContexts
    _ ->
      Right False

classExplicitSupportReachableContexts ::
  ScopeIndex ->
  NebulaBatch ->
  ClassId ->
  Either NebulaError (Maybe [ScopeCtx])
classExplicitSupportReachableContexts scopeIndex batchValue classId = do
  candidateContexts <-
    Set.fromList <$> scopeLookup "support observed contexts" (scopeObservedContexts scopeIndex)
  let baseGraph =
        contextRebaseBatchBaseGraph batchValue
      canonicalClass =
        canonicalizeClassId baseGraph classId
      site =
        contextRebaseBatchSite batchValue
      supportIndex =
        contextRebaseBatchClassSupportIndex batchValue
      carrierRows =
        mapMaybe
          (classSupportExplicitCarrierForKey supportIndex . classIdKey)
          (nub [classId, canonicalClass])
  pure $
    case traverse (supportCarrierReachableObjects site candidateContexts) carrierRows of
      Right reachableSets
        | not (null carrierRows) ->
            Just (Set.toAscList (Set.unions reachableSets))
      _ ->
        Nothing

stageArgument ::
  StagePlanNode ->
  AdmittedArgument ->
  NebulaBatch ->
  Either NebulaError (ClassId, NebulaBatch)
stageArgument stageNode argument batchValue =
  case aaRealization argument of
    VisibleAtJoin ->
      Right (aaOriginalClass argument, batchValue)
    MaterializedAtJoin representativeTerm ->
      stageFixTerm stageNode representativeTerm batchValue

stageFixTerm ::
  StagePlanNode ->
  Fix HsExprF ->
  NebulaBatch ->
  Either NebulaError (ClassId, NebulaBatch)
stageFixTerm stageNode (Fix nodeValue) batch0 = do
  (batch1, childClasses) <-
    mapAccumM
      ( \batchValue childTerm -> do
          (childClass, batchValue') <- stageFixTerm stageNode childTerm batchValue
          pure (batchValue', childClass)
      )
      batch0
      nodeValue
  stageNode childClasses batch1

definitionFixTerm ::
  [BinderAnn] ->
  IntMap.IntMap BinderAnn ->
  Pattern HsExprF ->
  Either NebulaError (Fix HsExprF)
definitionFixTerm binderAnns binderByVar bodyPattern = do
  bodyTerm <- instantiate bodyPattern
  let definitionTerm =
        foldr (\binderAnn inner -> Fix (LamF binderAnn inner)) bodyTerm binderAnns
  if wellScopedDefinitionTerm definitionTerm
    then Right definitionTerm
    else Left (NebulaSynthesisError "staged definition contains an unbound local binder occurrence")
  where
    instantiate = \case
      PatternVar patternVar ->
        maybe
          (Left (NebulaSynthesisError ("definition term references an unbound variable slot: " <> show (EGraph.patternVarKey patternVar))))
          (Right . Fix . VarF . LocalName)
          (IntMap.lookup (EGraph.patternVarKey patternVar) binderByVar)
      PatternNode node ->
        Fix <$> traverse instantiate node

freshenDefinitionBodyBinders :: Int -> Pattern HsExprF -> (Pattern HsExprF, Int)
freshenDefinitionBodyBinders binderBase bodyPattern =
  let (renaming, nextBinderBase) =
        foldl' freshenIntroducedBinder (IntMap.empty, binderBase) (patternBinderAnns bodyPattern)
   in (renamePatternBinders renaming bodyPattern, nextBinderBase)

freshenIntroducedBinder ::
  (IntMap.IntMap BinderAnn, Int) ->
  BinderAnn ->
  (IntMap.IntMap BinderAnn, Int)
freshenIntroducedBinder (renaming, nextBinderBase) binderAnn =
  let binderKey = binderIdKey (baId binderAnn)
   in case IntMap.lookup binderKey renaming of
        Just _ ->
          (renaming, nextBinderBase)
        Nothing ->
          ( IntMap.insert
              binderKey
              binderAnn {baId = toEnum nextBinderBase}
              renaming,
            nextBinderBase + 1
          )

renamePatternBinders :: IntMap.IntMap BinderAnn -> Pattern HsExprF -> Pattern HsExprF
renamePatternBinders renaming = \case
  PatternVar patternVar ->
    PatternVar patternVar
  PatternNode nodeValue ->
    PatternNode (renameNodeBinders renaming (fmap (renamePatternBinders renaming) nodeValue))

renameNodeBinders :: IntMap.IntMap BinderAnn -> HsExprF (Pattern HsExprF) -> HsExprF (Pattern HsExprF)
renameNodeBinders renaming = \case
  VarF (LocalName binderAnn) ->
    VarF (LocalName (renameBinderAnn renaming binderAnn))
  VarF (GlobalName rdrName) ->
    VarF (GlobalName rdrName)
  LamF binderAnn bodyPattern ->
    LamF (renameBinderAnn renaming binderAnn) bodyPattern
  LetF letModeValue localBinds bodyPattern ->
    LetF letModeValue (fmap (first (renamePatBinders renaming)) localBinds) bodyPattern
  CaseF scrutineePattern alternatives ->
    CaseF scrutineePattern (fmap (first (renamePatBinders renaming)) alternatives)
  ClausesF clauses ->
    ClausesF (fmap (first (fmap (renamePatBinders renaming))) clauses)
  DoF statements ->
    DoF (fmap (renameStatementBinders renaming) statements)
  GuardedF alternatives ->
    GuardedF (fmap (renameGuardedAltBinders renaming) alternatives)
  MultiIfF alternatives ->
    MultiIfF (fmap (renameGuardedAltBinders renaming) alternatives)
  nodeValue ->
    nodeValue

renameStatementBinders :: IntMap.IntMap BinderAnn -> HsStmtF (Pattern HsExprF) -> HsStmtF (Pattern HsExprF)
renameStatementBinders renaming = \case
  BindStmtF binderPat rhsPattern ->
    BindStmtF (renamePatBinders renaming binderPat) rhsPattern
  BodyStmtF bodyPattern ->
    BodyStmtF bodyPattern
  LetStmtF letModeValue localBinds ->
    LetStmtF letModeValue (fmap (first (renamePatBinders renaming)) localBinds)

renameGuardedAltBinders :: IntMap.IntMap BinderAnn -> GuardedAltF (Pattern HsExprF) -> GuardedAltF (Pattern HsExprF)
renameGuardedAltBinders renaming guardedAlt =
  GuardedAltF
    { gaGuards = fmap (renameGuardStmtBinders renaming) (gaGuards guardedAlt),
      gaBody = gaBody guardedAlt
    }

renameGuardStmtBinders :: IntMap.IntMap BinderAnn -> HsGuardStmtF (Pattern HsExprF) -> HsGuardStmtF (Pattern HsExprF)
renameGuardStmtBinders renaming = \case
  GuardBoolF guardPattern ->
    GuardBoolF guardPattern
  GuardPatF binderPat rhsPattern ->
    GuardPatF (renamePatBinders renaming binderPat) rhsPattern
  GuardLetF letModeValue localBinds ->
    GuardLetF letModeValue (fmap (first (renamePatBinders renaming)) localBinds)

renamePatBinders :: IntMap.IntMap BinderAnn -> HsPatF -> HsPatF
renamePatBinders renaming =
  runIdentity . traversePatBinders (Identity . renameBinderAnn renaming)

renameBinderAnn :: IntMap.IntMap BinderAnn -> BinderAnn -> BinderAnn
renameBinderAnn renaming binderAnn =
  IntMap.findWithDefault binderAnn (binderIdKey (baId binderAnn)) renaming

indexMaybe :: [a] -> Int -> Maybe a
indexMaybe values position =
  case drop position values of
    [] -> Nothing
    (value : _) -> Just value

patternBinderAnns :: Pattern HsExprF -> [BinderAnn]
patternBinderAnns = \case
  PatternVar {} -> []
  PatternNode node -> nodeBinderAnns node <> foldMap patternBinderAnns node

nodeBinderAnns :: HsExprF r -> [BinderAnn]
nodeBinderAnns = \case
  LamF binderAnn _ -> [binderAnn]
  LetF _ localBinds _ -> foldMap (patBinders . fst) localBinds
  CaseF _ alternatives -> foldMap (patBinders . fst) alternatives
  ClausesF clauses -> foldMap (foldMap patBinders . fst) clauses
  DoF statements -> foldMap statementBinderAnns statements
  GuardedF alternatives -> foldMap guardedAltBinderAnns alternatives
  MultiIfF alternatives -> foldMap guardedAltBinderAnns alternatives
  _ -> []

statementBinderAnns :: HsStmtF r -> [BinderAnn]
statementBinderAnns = \case
  BindStmtF binderPat _ -> patBinders binderPat
  BodyStmtF _ -> []
  LetStmtF _ localBinds -> foldMap (patBinders . fst) localBinds

guardedAltBinderAnns :: GuardedAltF r -> [BinderAnn]
guardedAltBinderAnns (GuardedAltF guards _) =
  foldMap guardStmtBinderAnns guards

guardStmtBinderAnns :: HsGuardStmtF r -> [BinderAnn]
guardStmtBinderAnns = \case
  GuardBoolF _ -> []
  GuardPatF binderPat _ -> patBinders binderPat
  GuardLetF _ localBinds -> foldMap (patBinders . fst) localBinds

moduleMaxBinderKey :: ConvertedModule -> Int
moduleMaxBinderKey convertedModule =
  foldl' max 0 (foldMap (patternBinderKeys . tlbTerm) (cmBindings convertedModule))

patternBinderKeys :: Pattern HsExprF -> [Int]
patternBinderKeys = \case
  PatternVar {} -> []
  PatternNode node -> nodeBinderKeys node <> foldMap patternBinderKeys node

nodeBinderKeys :: HsExprF r -> [Int]
nodeBinderKeys = \case
  VarF (LocalName binderAnn) -> [binderIdKey (baId binderAnn)]
  VarF (GlobalName _) -> []
  LamF binderAnn _ -> [binderIdKey (baId binderAnn)]
  LetF _ localBinds _ -> foldMap (fmap (binderIdKey . baId) . patBinders . fst) localBinds
  CaseF _ alternatives -> foldMap (fmap (binderIdKey . baId) . patBinders . fst) alternatives
  ClausesF clauses -> foldMap (fmap (binderIdKey . baId) . foldMap patBinders . fst) clauses
  DoF statements -> foldMap statementBinderKeys statements
  GuardedF alternatives -> foldMap guardedAltBinderKeys alternatives
  MultiIfF alternatives -> foldMap guardedAltBinderKeys alternatives
  _ -> []

statementBinderKeys :: HsStmtF r -> [Int]
statementBinderKeys = \case
  BindStmtF binderPat _ -> fmap (binderIdKey . baId) (patBinders binderPat)
  BodyStmtF _ -> []
  LetStmtF _ localBinds -> foldMap (fmap (binderIdKey . baId) . patBinders . fst) localBinds

guardedAltBinderKeys :: GuardedAltF r -> [Int]
guardedAltBinderKeys (GuardedAltF guards _) =
  foldMap guardStmtBinderKeys guards

guardStmtBinderKeys :: HsGuardStmtF r -> [Int]
guardStmtBinderKeys = \case
  GuardBoolF _ -> []
  GuardPatF binderPat _ -> fmap (binderIdKey . baId) (patBinders binderPat)
  GuardLetF _ localBinds -> foldMap (fmap (binderIdKey . baId) . patBinders . fst) localBinds
