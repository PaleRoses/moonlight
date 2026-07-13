{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula
  ( NebulaConfig (..),
    defaultNebulaConfig,
    ModuleWorkload (..),
    workloadOracle,
    NebulaError (..),
    nebulaErrorKey,
    nebulaErrorPath,
    ModuleReport (..),
    HunkDisposition (..),
    HunkBlockReason (..),
    WorkspaceReport (..),
    ModuleImprovement (..),
    WriteStatus (..),
    writeStatusKey,
    WriteOutcome (..),
    HunkCertificate (..),
    NebulaProvenance (..),
    ProvenanceEntry (..),
    TypeEvidenceCensus (..),
    TypeVerdict (..),
    ModulePatch (..),
    WriteBackRefusal (..),
    SourceQualityRefusal (..),
    LineOnlyMinificationEvidence (..),
    SourceLineQualityEvidence (..),
    AppendedDefinition (..),
    RecordDeclaration (..),
    RecordFieldRow (..),
    RecordSelectorRewrite (..),
    DeclarationPatch (..),
    DeclarationSealObligation (..),
    LawStamp (..),
    GatedLawReport (..),
    EvidenceFactCensus (..),
    NumTypeFactCensus (..),
    SelfLawRow (..),
    SealedSource,
    sealedSourceText,
    SealOutcome (..),
    sealPatchedSourceParseCount,
    NebulaLedger (..),
    LatentGroup (..),
    moduleLedger,
    workspaceLatent,
    renderModuleReport,
    renderWorkspaceReport,
    renderModuleDiff,
    renderModuleCandidateDiff,
    patchedModuleSource,
    modulePatchHasContent,
    recordDeclarations,
    planRecordFieldDeletion,
    planRecordOwnershipRewrite,
    patchedDeclarationSource,
    sealDeclarationPatch,
    sealDeclarationObligations,
    sealModulePatch,
    improveModule,
    improveWorkspace,
    moduleCertificates,
    DiagnoseRegion (..),
    DiagnoseVerdict (..),
    diagnoseVerdict,
    diagnoseVerdictKey,
    scopeCtxKey,
    scopeCtxScopeId,
    DiagnoseModuleReport (..),
    DiagnoseReport (..),
    diagnoseEnvelopeJson,
    diagnoseModule,
    diagnoseWorkspace,
    renderDiagnoseRegions,
    renderDiagnoseReport,
    enumerateModuleWorkloads,
  )
where

import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Melusine.Nebula.Proof.Certificate
  ( HunkCertificate (..),
    NebulaProvenance (..),
    ProvenanceEntry (..),
    auditAdmissibleProofSteps,
    hunkCertificate,
    replayStepOf,
  )
import Melusine.Nebula.Proof.Audit
  ( TypeEvidenceCensus (..),
    TypeVerdict (..),
    classEvidenceByKey,
    replayStepVerdicts,
    typeEvidenceCensus,
  )
import Melusine.Nebula.Discovery.Choose (ChosenBinding (..), nebulaCostAlgebra, resolvePatternClass)
import Melusine.Nebula.Core
  ( ModuleWorkload (..),
    NebulaConfig (..),
    NebulaError (..),
    defaultNebulaConfig,
    nebulaErrorKey,
    nebulaErrorPath,
    workloadOracle,
  )
import Melusine.Nebula.Rewrite.Corpus
  ( EvidenceFactCensus (..),
    GatedLawReport (..),
    LawStamp (..),
    NumTypeFactCensus (..),
    RuleCorpus,
    SelfLawRow (..),
    deriveRuleCorpusWithOracleKeysAndReason,
    rcLawTable,
  )
import Melusine.Nebula.Write.Diff (renderModuleCandidateDiff, renderModuleDiff)
import Melusine.Nebula.Source.Ingest (IngestedModule (..), ingestModule)
import Melusine.Nebula.Harvest.Core (buildHarvest)
import Melusine.Nebula.Report.Text
  ( ModuleReport (..),
    HunkDisposition (..),
    HunkBlockReason (..),
    LatentGroup (..),
    NebulaLedger (..),
    OracleProvenance (..),
    WorkspaceReport (..),
    moduleLedger,
    moduleReport,
    renderModuleReport,
    renderWorkspaceReport,
    renderNebulaError,
    workspaceLatent,
    workspaceReport,
  )
import Melusine.Nebula.Report.Json
  ( J (..),
    nebulaFailureJson,
    schemaVersion,
  )
import Melusine.Nebula.Rewrite.Saturate
  ( SaturatedModule,
    defaultSaturationOptions,
    saturateModule,
    smContextGraph,
    smProofSteps,
  )
import Melusine.Nebula.Write.Seal (SealOutcome (..), SealedSource, sealModulePatch, sealModulePatchOutcome, sealPatchedSourceParseCount, sealedSourceText)
import Melusine.Nebula.Synthesis.Core (SynthesisOutcome (..), SynthesizedDefinition (..), SynthesizedName (..), synthesizeAbstractions)
import Melusine.Nebula.Source.Workspace (enumerateModuleWorkloads)
import Moonlight.EGraph.Pure.Context.Core (cegBase)
import Melusine.Nebula.Write.Back
  ( AppendedDefinition (..),
    LineOnlyMinificationEvidence (..),
    ModulePatch (..),
    SourceLineQualityEvidence (..),
    SourceQualityRefusal (..),
    WriteBackRefusal (..),
    WriteOutcome (..),
    WriteStatus (..),
    modulePatchHasContent,
    patchedModuleSource,
    planWriteBack,
    refuseTypeIncompatible,
    writeStatusKey,
  )
import Melusine.Nebula.Write.Declaration
  ( DeclarationPatch (..),
    DeclarationSealObligation (..),
    RecordDeclaration (..),
    RecordFieldRow (..),
    RecordSelectorRewrite (..),
    patchedDeclarationSource,
    planRecordFieldDeletion,
    planRecordOwnershipRewrite,
    recordDeclarations,
    sealDeclarationPatch,
    sealDeclarationObligations,
  )
import Moonlight.Core (Pattern (..))
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( ConvertedModule (..),
    HsExprF,
    ScopeCtx (..),
    ScopedExpr (..),
    TopLevelBinding (..),
    eraseScopedExpr,
    hsExprOracleKeyTable,
    scopeIdKey,
  )
import Moonlight.EGraph.Pure.Extraction (ExtractionResult (..), termSize)
import Moonlight.EGraph.Pure.Saturation.Extraction (contextualExtractBounded)
import Moonlight.Rewrite.System (OracleKey)
import Moonlight.Pale.Ghc.Hie.Oracle (ModuleNameOracle, occResolvesUniquely)
import Moonlight.Pale.Ghc.Hie.SourceKey (OracleAttachFailure (..), OracleLookup (..), oracleAttachFailure)

type ModuleImprovement :: Type
data ModuleImprovement = ModuleImprovement
  { miReport :: !ModuleReport,
    miPatch :: !ModulePatch,
    miCertificates :: ![HunkCertificate],
    miSeal :: !SealOutcome
  }

improveModule :: NebulaConfig -> ModuleWorkload -> Either (FilePath, NebulaError) ModuleImprovement
improveModule config workload =
  first ((,) (mwPath workload)) $ do
    ingested <- ingestModule workload
    oracleKeys <-
      oracleKeysForModule (workloadOracle workload)
    corpus <-
      deriveRuleCorpusWithOracleKeysAndReason
        config
        oracleKeys
        (oracleAttachFailure (mwOracleLookup workload))
        (imSpanRows ingested)
        (workloadOracle workload)
        (imConverted ingested)
    saturated <- saturateModule defaultSaturationOptions config ingested corpus
    typeEvidence <-
      first NebulaContextSupportError (typeEvidenceCensus (smContextGraph saturated))
    harvest <- buildHarvest config ingested saturated
    outcome <- synthesizeAbstractions config ingested corpus saturated harvest
    plannedPatch <- planWriteBack workload ingested outcome
    certificates <- moduleCertificates ingested corpus outcome plannedPatch
    let verdicts = Map.fromList [(hcBinding certificate, hcTypeVerdict certificate) | certificate <- certificates]
        modulePatch = refuseTypeIncompatible verdicts plannedPatch
        sealOutcome = sealModulePatchOutcome (mwPath workload) (mwSource workload) modulePatch
    Right
      ModuleImprovement
        { miReport = moduleReport config (mwPath workload) (mwSource workload) (oracleProvenance (mwOracleLookup workload)) corpus saturated typeEvidence outcome modulePatch certificates sealOutcome,
          miPatch = modulePatch,
          miCertificates = certificates,
          miSeal = sealOutcome
        }

improveWorkspace :: NebulaConfig -> [FilePath] -> [FilePath] -> IO ([(ModuleWorkload, ModuleImprovement)], WorkspaceReport)
improveWorkspace config roots hieRoots = do
  (workspaceErrors, workloads) <- enumerateModuleWorkloads roots hieRoots
  let (moduleFailures, improvements) =
        partitionEithers (fmap (\workload -> (,) workload <$> improveModule config workload) workloads)
  pure
    ( improvements,
      workspaceReport (fmap (miReport . snd) improvements) moduleFailures workspaceErrors
    )

type DiagnoseRegion :: Type
data DiagnoseRegion = DiagnoseRegion
  { drBinding :: !String,
    drPath :: !String,
    drContext :: !ScopeCtx,
    drOriginalSize :: !Int,
    drLocalSize :: !(Maybe Int),
    drBindingSize :: !(Maybe Int)
  }
  deriving stock (Eq, Show)

type DiagnoseVerdict :: Type
data DiagnoseVerdict
  = DiagnoseLocalWin
  | DiagnoseLocalWinInvisibleAtBinding
  | DiagnoseLocalWinNoBindingView
  | DiagnoseNoWin
  deriving stock (Eq, Ord, Show)

diagnoseVerdict :: DiagnoseRegion -> DiagnoseVerdict
diagnoseVerdict row =
  case (drLocalSize row, drBindingSize row) of
    (Just localSize, Just bindingSize)
      | localSize < drOriginalSize row && bindingSize >= drOriginalSize row ->
          DiagnoseLocalWinInvisibleAtBinding
      | localSize < drOriginalSize row ->
          DiagnoseLocalWin
    (Just localSize, Nothing)
      | localSize < drOriginalSize row ->
          DiagnoseLocalWinNoBindingView
    _ ->
      DiagnoseNoWin

diagnoseVerdictKey :: DiagnoseVerdict -> String
diagnoseVerdictKey = \case
  DiagnoseLocalWin ->
    "local-win"
  DiagnoseLocalWinInvisibleAtBinding ->
    "local-win-invisible-at-binding"
  DiagnoseLocalWinNoBindingView ->
    "local-win-no-binding-view"
  DiagnoseNoWin ->
    "none"

scopeCtxKey :: ScopeCtx -> String
scopeCtxKey = \case
  ActualScope {} ->
    "actual"
  IncompatibleScope ->
    "incompatible"

scopeCtxScopeId :: ScopeCtx -> Maybe Int
scopeCtxScopeId = \case
  ActualScope scopeId ->
    Just (scopeIdKey scopeId)
  IncompatibleScope ->
    Nothing

type DiagnoseModuleReport :: Type
data DiagnoseModuleReport = DiagnoseModuleReport
  { dmrPath :: !FilePath,
    dmrRegions :: ![DiagnoseRegion]
  }
  deriving stock (Eq, Show)

type DiagnoseReport :: Type
data DiagnoseReport = DiagnoseReport
  { dgrModules :: ![DiagnoseModuleReport],
    dgrModuleFailures :: ![(FilePath, NebulaError)],
    dgrWorkspaceErrors :: ![NebulaError]
  }
  deriving stock (Eq, Show)

diagnoseModule :: NebulaConfig -> ModuleWorkload -> Either (FilePath, NebulaError) [DiagnoseRegion]
diagnoseModule config workload =
  first ((,) (mwPath workload)) $ do
    ingested <- ingestModule workload
    oracleKeys <-
      oracleKeysForModule (workloadOracle workload)
    corpus <-
      deriveRuleCorpusWithOracleKeysAndReason
        config
        oracleKeys
        (oracleAttachFailure (mwOracleLookup workload))
        (imSpanRows ingested)
        (workloadOracle workload)
        (imConverted ingested)
    saturated <- saturateModule defaultSaturationOptions config ingested corpus
    Right
      ( concat
          ( zipWith3
              (bindingDiagnoseRegions config saturated)
              (imBindingNames ingested)
              (imBindingContexts ingested)
              (cmBindings (imConverted ingested))
          )
      )

diagnoseWorkspace :: NebulaConfig -> [FilePath] -> [FilePath] -> IO DiagnoseReport
diagnoseWorkspace config roots hieRoots = do
  (workspaceErrors, workloads) <- enumerateModuleWorkloads roots hieRoots
  let (moduleFailures, moduleReports) =
        partitionEithers
          ( fmap
              ( \workload ->
                  DiagnoseModuleReport (mwPath workload)
                    <$> diagnoseModule config workload
              )
              workloads
          )
  pure
    DiagnoseReport
      { dgrModules = moduleReports,
        dgrModuleFailures = moduleFailures,
        dgrWorkspaceErrors = workspaceErrors
      }

diagnoseEnvelopeJson :: [NebulaError] -> [(FilePath, Either NebulaError [DiagnoseRegion])] -> J
diagnoseEnvelopeJson workspaceErrors moduleResults =
  JObj
    [ ("schemaVersion", JInt schemaVersion),
      ("tool", JStr "melusine-nebula"),
      ("mode", JStr "diagnose"),
      ("summary", diagnoseSummaryJson (length modules) (length failures)),
      ("latent", JArr []),
      ("modules", JArr modules),
      ("failures", JArr failures)
    ]
  where
    modules =
      [ diagnoseModuleJson path regions
      | (path, Right regions) <- moduleResults
      ]
    failures =
      [ nebulaFailureJson (Just path) failure
      | (path, Left failure) <- moduleResults
      ]
        <> fmap (nebulaFailureJson Nothing) workspaceErrors

diagnoseSummaryJson :: Int -> Int -> J
diagnoseSummaryJson moduleCount failureCount =
  JObj
    [ ("status", JStr status),
      ("modules", JInt moduleCount),
      ("sealed", JInt 0),
      ("failed", JInt failureCount),
      ("bytes", JObj [("original", JInt 0), ("sealed", JInt 0)]),
      ( "nodeTotals",
        JObj
          [ ("originalTotal", JInt 0),
            ("finalTotal", JInt 0),
            ("compressionRatio", JNull)
          ]
      ),
      ("nodesSaved", JInt 0)
    ]
  where
    status
      | failureCount > 0 = "degraded"
      | moduleCount == 0 = "empty"
      | otherwise = "clean"

diagnoseModuleJson :: FilePath -> [DiagnoseRegion] -> J
diagnoseModuleJson path regions =
  JObj
    [ ("path", JStr path),
      ("regions", JArr (fmap diagnoseRegionJson regions))
    ]

diagnoseRegionJson :: DiagnoseRegion -> J
diagnoseRegionJson region =
  JObj
    [ ("binding", JStr (drBinding region)),
      ("region", JStr (drPath region)),
      ("context", diagnoseContextJson (drContext region)),
      ("originalSize", JInt (drOriginalSize region)),
      ("localExtract", maybe JNull JInt (drLocalSize region)),
      ("bindingExtract", maybe JNull JInt (drBindingSize region)),
      ("verdict", JStr (diagnoseVerdictKey (diagnoseVerdict region)))
    ]

diagnoseContextJson :: ScopeCtx -> J
diagnoseContextJson context =
  JObj
    [ ("kind", JStr (scopeCtxKey context)),
      ("scopeId", maybe JNull JInt (scopeCtxScopeId context))
    ]

bindingDiagnoseRegions :: NebulaConfig -> SaturatedModule -> String -> ScopeCtx -> TopLevelBinding -> [DiagnoseRegion]
bindingDiagnoseRegions config saturated bindingName bindingContext binding =
  fmap regionRow (scopeRegions (tlbScopedTerm binding))
  where
    contextGraph = smContextGraph saturated
    baseGraph = cegBase contextGraph
    extractedSizeAt contextValue classId =
      case contextualExtractBounded
        (ncExtractionBudget config)
        contextValue
        mempty
        (nebulaCostAlgebra (ncCostModel config))
        classId
        contextGraph of
        Right (Just extractionResult) -> Just (termSize (erTerm extractionResult))
        _ -> Nothing
    regionRow (pathLabel, regionScoped) =
      let regionClass = resolvePatternClass baseGraph (eraseScopedExpr regionScoped)
          regionContext = ActualScope (seOccScope regionScoped)
       in DiagnoseRegion
            { drBinding = bindingName,
              drPath = pathLabel,
              drContext = regionContext,
              drOriginalSize = diagnosePatternSize (eraseScopedExpr regionScoped),
              drLocalSize = extractedSizeAt regionContext =<< regionClass,
              drBindingSize = extractedSizeAt bindingContext =<< regionClass
            }

scopeRegions :: ScopedExpr -> [(String, ScopedExpr)]
scopeRegions rootScoped =
  ("root", rootScoped) : regionWalk "root" rootScoped

regionWalk :: String -> ScopedExpr -> [(String, ScopedExpr)]
regionWalk pathLabel scopedExpr =
  concat
    [ let childLabel = pathLabel <> "." <> show childIndex
       in if seOccScope childScoped == seOccScope scopedExpr
            then regionWalk childLabel childScoped
            else (childLabel, childScoped) : regionWalk childLabel childScoped
    | (childIndex, childScoped) <- zip [0 :: Int ..] (toList (seNode scopedExpr))
    ]

diagnosePatternSize :: Pattern HsExprF -> Int
diagnosePatternSize = \case
  PatternVar _ -> 0
  PatternNode node -> 1 + sum (fmap diagnosePatternSize node)

renderDiagnoseRegions :: FilePath -> [DiagnoseRegion] -> [String]
renderDiagnoseRegions path rows =
  ("diagnose " <> path) : fmap renderRow rows
  where
    renderRow row =
      "  binding="
        <> drBinding row
        <> " region="
        <> drPath row
        <> " context="
        <> scopeCtxKey (drContext row)
        <> maybe "" ((" scope-id=" <>) . show) (scopeCtxScopeId (drContext row))
        <> " original="
        <> show (drOriginalSize row)
        <> " localExtract="
        <> maybe "none" show (drLocalSize row)
        <> " bindingExtract="
        <> maybe "none" show (drBindingSize row)
        <> " verdict="
        <> diagnoseVerdictKey (diagnoseVerdict row)

renderDiagnoseReport :: DiagnoseReport -> [String]
renderDiagnoseReport report =
  foldMap (\moduleReportValue -> renderDiagnoseRegions (dmrPath moduleReportValue) (dmrRegions moduleReportValue)) (dgrModules report)
    <> fmap
      (\(path, failure) -> "diagnose failure path=" <> path <> " error=" <> renderNebulaError failure)
      (dgrModuleFailures report)
    <> fmap
      (\failure -> "diagnose workspace-error " <> renderNebulaError failure)
      (dgrWorkspaceErrors report)

oracleKeysForModule :: Maybe ModuleNameOracle -> Either NebulaError (Set.Set OracleKey)
oracleKeysForModule =
  maybe
    (Right Set.empty)
    ( \oracle ->
        first (NebulaRuleDerivationError . ("oracle key table parse failed: " <>) . show) $
          Set.fromList
            . fmap (\(oracleKey, _, _) -> oracleKey)
            . filter (\(_, occurrence, acceptedOrigins) -> occResolvesUniquely oracle occurrence acceptedOrigins)
            <$> hsExprOracleKeyTable
    )

oracleProvenance :: OracleLookup -> OracleProvenance
oracleProvenance = \case
  OracleFound keyKind recordedPath _ ->
    OracleAttached keyKind recordedPath
  OracleMissing triedKeys ->
    OracleUnattached (OracleLookupMissing triedKeys)
  OracleAmbiguous keyKind keyValue candidates ->
    OracleUnattached (OracleLookupAmbiguous keyKind keyValue candidates)

moduleCertificates ::
  IngestedModule ->
  RuleCorpus ->
  SynthesisOutcome ->
  ModulePatch ->
  Either NebulaError [HunkCertificate]
moduleCertificates ingested corpus outcome modulePatch = do
  let proofSteps = smProofSteps finalSaturated
      judgedSteps =
        zip
          proofSteps
          ( replayStepVerdicts
              (cegBase (imContextGraph ingested))
              (classEvidenceByKey (imContextGraph ingested))
              (fmap replayStepOf proofSteps)
          )
  auditAdmissibleProofSteps (rcLawTable corpus) proofSteps
  splicedCertificates <-
    traverse (certificateForBinding judgedSteps) (mpSpliced modulePatch)
  pure
    ( splicedCertificates
        <> fmap (certificateForDefinition judgedSteps) appendedDefinitions
    )
  where
    finalSaturated =
      soSaturatedModule outcome
    finalContextGraph =
      smContextGraph finalSaturated
    bindingSeeds =
      Map.fromList [(cbName binding, cbSeedClass binding) | binding <- soBindings outcome]
    appendedNames =
      Set.fromList (fmap adName (mpAppendedDefinitions modulePatch))
    appendedDefinitions =
      filter ((`Set.member` appendedNames) . synthesizedNameText . sdName) (soDefinitions outcome)
    certificateForBinding judgedSteps (bindingName, _) =
      maybe
        (Left (NebulaWriteBackError ("certificate seed missing for spliced binding " <> bindingName)))
        (\seedClass -> Right (hunkCertificate finalContextGraph bindingName seedClass judgedSteps))
        (Map.lookup bindingName bindingSeeds)
    certificateForDefinition judgedSteps definition =
      hunkCertificate finalContextGraph (synthesizedNameText (sdName definition)) (sdClass definition) judgedSteps
