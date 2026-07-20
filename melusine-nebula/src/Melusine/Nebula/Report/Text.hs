{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.Report.Text
  ( BindingReport (..),
    OracleProvenance (..),
    HunkDisposition (..),
    HunkBlockReason (..),
    ModuleReport (..),
    WorkspaceReport (..),
    NebulaLedger (..),
    LatentGroup (..),
    moduleLedger,
    workspaceLatent,
    blockReasonKey,
    dispositionReasonKey,
    hsOpaqueTagKey,
    hsPatOpaqueTagKey,
    renderRefusalKey,
    hieSourceKeyKindKey,
    oracleAttachFailureKey,
    trustTierKey,
    semanticFidelityKey,
    saturationTerminationKey,
    typeVerdictKey,
    renderNebulaError,
    moduleReport,
    workspaceReport,
    renderModuleReport,
    renderWorkspaceReport,
  )
where

import Data.Kind (Type)
import Data.List (intercalate, nub)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Melusine.Nebula.Proof.Audit (TypeEvidenceCensus (..), TypeVerdict (..))
import Melusine.Nebula.Proof.Certificate (HunkCertificate (..), NebulaProvenance (..), ProvenanceEntry (..))
import Melusine.Nebula.Discovery.Choose (ChosenBinding (..), candidateSiteKindKey)
import Melusine.Nebula.Core (NebulaConfig (..), NebulaError (..), nebulaErrorKey)
import Melusine.Nebula.Rewrite.Corpus
  ( EvidenceFactCensus (..),
    GatedLawReport (..),
    LawGateReason (..),
    LawStamp (..),
    NumTypeFactCensus (..),
    RuleCorpus,
    SelfLawRow (..),
    lawGateReasonKey,
    rcBindingMetrics,
    rcEvidenceFactCensus,
    rcGatedLaws,
    rcNumTypeFactCensus,
    rcSelfLawRows,
    rcSiteMetrics,
    rcVocabularyMetrics,
    selfLawRefusalKey,
  )
import Melusine.Nebula.Harvest.Maintain (HarvestAdvanceDecision (..), HarvestFallbackReason (..))
import Melusine.Nebula.Rewrite.Saturate
  ( RuleFire (..),
    SaturatedModule,
    SaturationLifecycleCounts (..),
    smFinalClassCount,
    smFinalNodeCount,
    smInitialClassCount,
    smInitialNodeCount,
    smIterations,
    smLifecycleCounts,
    smMatchesApplied,
    smRuleFires,
    smScheduledTotal,
    smTermination,
  )
import Melusine.Nebula.Write.Seal (SealOutcome (..), sealPatchedSourceParseCount, sealedSourceText)
import Melusine.Nebula.Synthesis.Core
  ( CandidateRejection (..),
    CandidateSiteLabel (..),
    PlanStagingReport (..),
    RejectedCandidate (..),
    SynthesisOutcome (..),
    SynthesizedDefinition (..),
    SynthesizedName (..),
    SynthesizedSite (..),
  )
import Melusine.Nebula.Synthesis.Types (CandidateRejection (..), RecordOwnershipFinding (..), candidateRejectionKey, recordOwnershipKindKey)
import Melusine.Nebula.Write.Back (AppendedDefinition (..), ModulePatch (..), WriteBackRefusal (..), hsOpaqueTagKey, hsPatOpaqueTagKey, renderRefusalKey, sourceQualityRefusalKey, writeBackRefusalKey)
import Moonlight.Core (RewriteRuleId (..))
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( HsExprBindingRuleMetrics (..),
    HsOpaqueTag (..),
    HsPatOpaqueTag (..),
    HsExprSupportRuleMetrics (..),
    HsExprVocabularyRuleMetrics (..),
    RenderRefusal (..),
  )
import Moonlight.Flow.Model.Schema.Digest (StableDigest128 (..))
import Moonlight.Rewrite.System (SemanticFidelity (..), TrustTier (..), lawIdKey, oracleKeyString)
import Moonlight.Saturation.Core (SaturationTermination (..))
import Numeric (showFFloat, showHex)
import Moonlight.Pale.Ghc.Hie.SourceKey (HieSourceKeyKind (..), OracleAttachFailure (..), TriedKey (..))

type BindingReport :: Type
data BindingReport = BindingReport
  { brName :: !String,
    brOriginalSize :: !Int,
    brExtractedSize :: !Int,
    brExtractionCost :: !Int
  }
  deriving stock (Eq, Show)

type OracleProvenance :: Type
data OracleProvenance
  = OracleAttached !HieSourceKeyKind !FilePath
  | OracleUnattached !OracleAttachFailure
  deriving stock (Eq, Show)

type HunkDisposition :: Type
data HunkDisposition
  = HunkSealed
  | HunkBlocked !HunkBlockReason
  deriving stock (Eq, Show)

type HunkBlockReason :: Type
data HunkBlockReason
  = BlockedWriteBack !WriteBackRefusal
  | BlockedSeal !NebulaError
  | BlockedCandidate !CandidateRejection
  deriving stock (Eq, Show)

type ModuleReport :: Type
data ModuleReport = ModuleReport
  { mrPath :: !FilePath,
    mrOracleProvenance :: !OracleProvenance,
    mrBindingReports :: ![BindingReport],
    mrOriginalBytes :: !Int,
    mrSealedBytes :: !(Maybe Int),
    mrOriginalTotal :: !Int,
    mrFinalTotal :: !Int,
    mrCompressionRatio :: !(Maybe Double),
    mrTermination :: !SaturationTermination,
    mrIterations :: !Int,
    mrMatchesApplied :: !Int,
    mrScheduledTotal :: !Int,
    mrInitialNodeCount :: !Int,
    mrFinalNodeCount :: !Int,
    mrInitialClassCount :: !Int,
    mrFinalClassCount :: !Int,
    mrSiteMetrics :: !HsExprSupportRuleMetrics,
    mrVocabularyMetrics :: !HsExprVocabularyRuleMetrics,
    mrEvidenceFactCensus :: !EvidenceFactCensus,
    mrNumTypeFactCensus :: !NumTypeFactCensus,
    mrSelfLawRows :: ![SelfLawRow],
    mrGatedLaws :: ![GatedLawReport],
    mrBindingFrontMetrics :: !(Maybe HsExprBindingRuleMetrics),
    mrRuleFires :: ![RuleFire],
    mrTypeEvidence :: !TypeEvidenceCensus,
    mrAntiUnifyPairBound :: !Int,
    mrSynthesis :: !SynthesisOutcome,
    mrCertificates :: ![HunkCertificate],
    mrDispositions :: ![(String, HunkDisposition)],
    mrSeal :: !SealOutcome,
    mrDiff :: [String]
  }

dispositions :: ModulePatch -> SynthesisOutcome -> SealOutcome -> [(String, HunkDisposition)]
dispositions modulePatch outcome sealOutcome =
  [ (bindingName, dispositionFor bindingName)
  | bindingName <- dispositionNames
  ]
  where
    dispositionNames =
      fmap cbName (soBindings outcome)
        <> fmap adName (mpAppendedDefinitions modulePatch)
        <> fmap fst (mpDeclarationSpliceGroups modulePatch)
        <> fmap fst (mpProtocolSpliceGroups modulePatch)
    patchDispositions =
      Map.fromList (writeBackDispositions <> sealDispositions)
    candidateDispositions =
      fmap (HunkBlocked . BlockedCandidate . rejReason) $
        Map.fromListWith
          preferCandidateRejection
          [ (bindingName, rejection)
          | rejection <- soRejected outcome,
            bindingName <- nub (fmap cslBindingName (rejSites rejection))
          ]
    dispositionFor bindingName =
      case Map.lookup bindingName patchDispositions of
        Just (HunkBlocked (BlockedWriteBack RefusedUnchanged)) ->
          candidateOrUnchanged bindingName
        Just disposition ->
          disposition
        Nothing ->
          candidateOrUnchanged bindingName
    candidateOrUnchanged bindingName =
      Map.findWithDefault
        (HunkBlocked (BlockedWriteBack RefusedUnchanged))
        bindingName
        candidateDispositions
    writeBackDispositions =
      [ (bindingName, HunkBlocked (BlockedWriteBack refusal))
      | (bindingName, refusal) <- mpSkipped modulePatch,
        refusal /= RefusedUnchanged
      ]
    sealDispositions =
      [ (bindingName, sealDisposition)
      | bindingName <- acceptedNames
      ]
    acceptedNames =
      fmap fst (mpSpliced modulePatch)
        <> fmap adName (mpAppendedDefinitions modulePatch)
        <> fmap fst (mpDeclarationSpliceGroups modulePatch)
        <> fmap fst (mpProtocolSpliceGroups modulePatch)
    sealDisposition =
      case sealOutcome of
        Sealed {} ->
          HunkSealed
        SealRefused _ sealFailure ->
          HunkBlocked (BlockedSeal sealFailure)
        SealEmpty ->
          HunkBlocked (BlockedWriteBack RefusedUnchanged)

preferCandidateRejection :: RejectedCandidate -> RejectedCandidate -> RejectedCandidate
preferCandidateRejection leftRejection rightRejection =
  if candidateRejectionPriority leftRejection >= candidateRejectionPriority rightRejection
    then leftRejection
    else rightRejection

candidateRejectionPriority :: RejectedCandidate -> Int
candidateRejectionPriority rejection =
  case rejReason rejection of
    RejectedNoEstimatedWin ->
      0
    _
      | rejEstimatedWin rejection > 0 ->
          2
      | otherwise ->
          1

type WorkspaceReport :: Type
data WorkspaceReport = WorkspaceReport
  { wrModules :: ![ModuleReport],
    wrModuleFailures :: ![(FilePath, NebulaError)],
    wrWorkspaceErrors :: ![NebulaError],
    wrOriginalTotal :: !Int,
    wrFinalTotal :: !Int,
    wrCompressionRatio :: !(Maybe Double)
  }

type LatentGroup :: Type
data LatentGroup = LatentGroup
  { lgReason :: !String,
    lgNodes :: !Int,
    lgBindings :: !Int
  }
  deriving stock (Eq, Show)

type NebulaLedger :: Type
data NebulaLedger = NebulaLedger
  { nlOriginalBytes :: !Int,
    nlSealedBytes :: !(Maybe Int),
    nlRealizedNodesSaved :: !Int,
    nlLatent :: ![LatentGroup]
  }
  deriving stock (Eq, Show)

bindingDeltaByName :: ModuleReport -> Map.Map String Int
bindingDeltaByName report =
  Map.fromListWith
    (+)
    [ (brName binding, brOriginalSize binding - brExtractedSize binding)
    | binding <- mrBindingReports report
    ]

moduleLedger :: ModuleReport -> NebulaLedger
moduleLedger report =
  NebulaLedger
    { nlOriginalBytes = mrOriginalBytes report,
      nlSealedBytes = mrSealedBytes report,
      nlRealizedNodesSaved = realizedNodesSaved,
      nlLatent = latentGroups report
    }
  where
    deltaByName = bindingDeltaByName report
    realizedNodesSaved =
      sum
        [ Map.findWithDefault 0 bindingName deltaByName
        | (bindingName, HunkSealed) <- mrDispositions report
        ]

latentGroups :: ModuleReport -> [LatentGroup]
latentGroups report =
  latentGroupsFrom
    [ (blockReasonKey reason, Map.findWithDefault 0 bindingName deltaByName)
    | (bindingName, HunkBlocked reason) <- mrDispositions report
    ]
  where
    deltaByName = bindingDeltaByName report

latentGroupsFrom :: [(String, Int)] -> [LatentGroup]
latentGroupsFrom rows =
  [ LatentGroup
      { lgReason = reasonKey,
        lgNodes = sum groupDeltas,
        lgBindings = length groupDeltas
      }
  | (reasonKey, groupDeltas) <- Map.toAscList grouped
  ]
  where
    grouped =
      Map.fromListWith
        (<>)
        [(reasonKey, [max 0 nodeDelta]) | (reasonKey, nodeDelta) <- rows]

workspaceLatent :: WorkspaceReport -> [LatentGroup]
workspaceLatent report =
  [ LatentGroup
      { lgReason = reasonKey,
        lgNodes = sum (fmap lgNodes groupRows),
        lgBindings = sum (fmap lgBindings groupRows)
      }
  | (reasonKey, groupRows) <- Map.toAscList grouped
  ]
  where
    grouped =
      Map.fromListWith
        (<>)
        [ (lgReason group, [group])
        | moduleReportValue <- wrModules report,
          group <- nlLatent (moduleLedger moduleReportValue)
        ]

dispositionReasonKey :: HunkDisposition -> String
dispositionReasonKey = \case
  HunkSealed ->
    "sealed"
  HunkBlocked reason ->
    blockReasonKey reason

blockReasonKey :: HunkBlockReason -> String
blockReasonKey = \case
  BlockedWriteBack (RefusedRender renderRefusal) ->
    renderRefusalKey renderRefusal
  BlockedWriteBack refusal ->
    writeBackRefusalKey refusal
  BlockedSeal _ ->
    "seal-refused"
  BlockedCandidate rejection ->
    "candidate:" <> candidateRejectionKey rejection

hieSourceKeyKindKey :: HieSourceKeyKind -> String
hieSourceKeyKindKey = \case
  GivenPathKey -> "given-path"
  AbsolutePathKey -> "absolute-path"
  RootRelativeKey -> "root-relative"
  ModuleSuffixKey -> "module-suffix"

oracleAttachFailureKey :: OracleAttachFailure -> String
oracleAttachFailureKey = \case
  OracleLookupMissing {} -> "missing"
  OracleLookupAmbiguous {} -> "ambiguous"

trustTierKey :: TrustTier -> String
trustTierKey = \case
  ParserVerified -> "parser-verified"
  GhcVerified -> "ghc-verified"
  RegistryTrusted -> "registry-trusted"
  MachineProved -> "machine-proved"
  ModuleDerived -> "module-derived"

semanticFidelityKey :: SemanticFidelity -> String
semanticFidelityKey = \case
  Observational -> "observational"
  UpToBottom -> "up-to-bottom"

saturationTerminationKey :: SaturationTermination -> String
saturationTerminationKey = \case
  ReachedFixedPoint -> "reached-fixed-point"
  ReachedGoal -> "reached-goal"
  HitIterationLimit -> "hit-iteration-limit"
  HitNodeLimit -> "hit-node-limit"

typeVerdictKey :: TypeVerdict -> String
typeVerdictKey = \case
  TypeCompatible -> "compatible"
  TypePolymorphic -> "polymorphic"
  TypeUnknown -> "unknown"
  TypeIncompatible {} -> "incompatible"

moduleReport ::
  NebulaConfig ->
  FilePath ->
  String ->
  OracleProvenance ->
  RuleCorpus ->
  SaturatedModule ->
  TypeEvidenceCensus ->
  SynthesisOutcome ->
  ModulePatch ->
  [HunkCertificate] ->
  SealOutcome ->
  ModuleReport
moduleReport config modulePath moduleSource oracleProvenance corpus saturated typeEvidence outcome modulePatch certificates sealOutcome =
  let bindingReports =
        [ BindingReport
            { brName = cbName binding,
              brOriginalSize = cbOriginalSize binding,
              brExtractedSize = cbExtractedSize binding,
              brExtractionCost = cbExtractionCost binding
            }
        | binding <- soBindings outcome
        ]
      originalTotal = sum (fmap brOriginalSize bindingReports)
      finalTotal =
        sum (fmap brExtractedSize bindingReports)
          + sum (fmap sdSize (soDefinitions outcome))
   in ModuleReport
        { mrPath = modulePath,
          mrOracleProvenance = oracleProvenance,
          mrBindingReports = bindingReports,
          mrOriginalBytes = length moduleSource,
          mrSealedBytes = sealedByteCount sealOutcome,
          mrOriginalTotal = originalTotal,
          mrFinalTotal = finalTotal,
          mrCompressionRatio = compressionRatio originalTotal finalTotal,
          mrTermination = smTermination saturated,
          mrIterations = smIterations saturated,
          mrMatchesApplied = smMatchesApplied saturated,
          mrScheduledTotal = smScheduledTotal saturated,
          mrInitialNodeCount = smInitialNodeCount saturated,
          mrFinalNodeCount = smFinalNodeCount saturated,
          mrInitialClassCount = smInitialClassCount saturated,
          mrFinalClassCount = smFinalClassCount saturated,
          mrSiteMetrics = rcSiteMetrics corpus,
          mrVocabularyMetrics = rcVocabularyMetrics corpus,
          mrEvidenceFactCensus = rcEvidenceFactCensus corpus,
          mrNumTypeFactCensus = rcNumTypeFactCensus corpus,
          mrSelfLawRows = rcSelfLawRows corpus,
          mrGatedLaws = rcGatedLaws corpus,
          mrBindingFrontMetrics = rcBindingMetrics corpus,
          mrRuleFires = smRuleFires saturated,
          mrTypeEvidence = typeEvidence,
          mrAntiUnifyPairBound = ncAntiUnifyMaxPairs config,
          mrSynthesis = outcome,
          mrCertificates = certificates,
          mrDispositions = dispositions modulePatch outcome sealOutcome,
          mrSeal = sealOutcome,
          mrDiff = sourceDiffLines moduleSource sealOutcome
        }

sourceDiffLines :: String -> SealOutcome -> [String]
sourceDiffLines originalSource = \case
  Sealed sealedSource ->
    changedSourceLines originalSource (sealedSourceText sealedSource)
  SealRefused {} -> []
  SealEmpty -> []

changedSourceLines :: String -> String -> [String]
changedSourceLines originalSource sealedSource
  | originalSource == sealedSource = []
  | otherwise =
      ["--- original", "+++ sealed"]
        <> fmap ("-" <>) changedOriginalLines
        <> fmap ("+" <>) changedSealedLines
  where
    originalLines = lines originalSource
    sealedLines = lines sealedSource
    commonPrefixCount = sharedPrefixLength originalLines sealedLines
    originalAfterPrefix = drop commonPrefixCount originalLines
    sealedAfterPrefix = drop commonPrefixCount sealedLines
    commonSuffixCount = sharedPrefixLength (reverse originalAfterPrefix) (reverse sealedAfterPrefix)
    changedOriginalLines = take (length originalAfterPrefix - commonSuffixCount) originalAfterPrefix
    changedSealedLines = take (length sealedAfterPrefix - commonSuffixCount) sealedAfterPrefix

sharedPrefixLength :: Eq a => [a] -> [a] -> Int
sharedPrefixLength [] _ = 0
sharedPrefixLength _ [] = 0
sharedPrefixLength (left : leftRest) (right : rightRest)
  | left == right = 1 + sharedPrefixLength leftRest rightRest
  | otherwise = 0

workspaceReport ::
  [ModuleReport] ->
  [(FilePath, NebulaError)] ->
  [NebulaError] ->
  WorkspaceReport
workspaceReport moduleReports moduleFailures workspaceErrors =
  let originalTotal = sum (fmap mrOriginalTotal moduleReports)
      finalTotal = sum (fmap mrFinalTotal moduleReports)
   in WorkspaceReport
        { wrModules = moduleReports,
          wrModuleFailures = moduleFailures,
          wrWorkspaceErrors = workspaceErrors,
          wrOriginalTotal = originalTotal,
          wrFinalTotal = finalTotal,
          wrCompressionRatio = compressionRatio originalTotal finalTotal
        }

sealedByteCount :: SealOutcome -> Maybe Int
sealedByteCount = \case
  Sealed sealedSource ->
    Just (length (sealedSourceText sealedSource))
  _ ->
    Nothing

compressionRatio :: Int -> Int -> Maybe Double
compressionRatio originalTotal finalTotal =
  if originalTotal == 0
    then Nothing
    else Just (fromIntegral finalTotal / fromIntegral originalTotal)

renderRatio :: Maybe Double -> String
renderRatio =
  maybe "n/a" (\ratioValue -> showFFloat (Just 4) ratioValue "")

renderModuleReport :: ModuleReport -> [String]
renderModuleReport report =
  [ "module path=" <> mrPath report,
    renderOracleProvenance (mrOracleProvenance report),
    "termination result="
      <> saturationTerminationKey (mrTermination report)
      <> " iterations="
      <> show (mrIterations report)
      <> " matches-applied="
      <> show (mrMatchesApplied report)
      <> " scheduled-total="
      <> show (mrScheduledTotal report),
    "graph nodes-before="
      <> show (mrInitialNodeCount report)
      <> " nodes-after="
      <> show (mrFinalNodeCount report)
      <> " classes-before="
      <> show (mrInitialClassCount report)
      <> " classes-after="
      <> show (mrFinalClassCount report),
    "saturation-lifecycle plan-preparations="
      <> show (slcPlanPreparations lifecycleCounts)
      <> " fresh-runs="
      <> show (slcFreshRuns lifecycleCounts)
      <> " resumptions="
      <> show (slcResumptions lifecycleCounts),
    "sites lambda="
      <> show (hsrmLambdaSiteCount siteMetrics)
      <> " let="
      <> show (hsrmLetSiteCount siteMetrics)
      <> " rules-total="
      <> show (hsrmTotalRuleCount siteMetrics)
      <> " diagnostic-spans="
      <> show (hsrmDiagnosticSpanCount siteMetrics),
    renderVocabularyMetrics (mrVocabularyMetrics report),
    renderEvidenceFactCensus (mrEvidenceFactCensus report),
    renderNumTypeFactCensus (mrNumTypeFactCensus report),
    "gated-laws count=" <> show (length (mrGatedLaws report)),
    renderBindingFrontMetrics (mrBindingFrontMetrics report),
    renderTypeEvidence (mrTypeEvidence report),
    "anti-unify pair-bound=" <> show (mrAntiUnifyPairBound report),
    "certificates count=" <> show (length (mrCertificates report)),
    renderSealOutcome (mrSeal report)
  ]
    <> fmap renderGatedLaw (mrGatedLaws report)
    <> fmap renderSelfLawRow (mrSelfLawRows report)
    <> fmap renderRuleFire (mrRuleFires report)
    <> fmap renderBindingReport (mrBindingReports report)
    <> renderSynthesis (mrSynthesis report)
    <> fmap renderCertificate (mrCertificates report)
    <> fmap renderDisposition (mrDispositions report)
    <> renderLedger (moduleLedger report)
    <> [ "totals original="
           <> show (mrOriginalTotal report)
           <> " final="
           <> show (mrFinalTotal report)
           <> " compression-ratio="
           <> renderRatio (mrCompressionRatio report)
       ]
  where
    siteMetrics = mrSiteMetrics report
    lifecycleCounts = smLifecycleCounts (soSaturatedModule (mrSynthesis report))

renderLedger :: NebulaLedger -> [String]
renderLedger ledger =
  ( "realized bytes-original="
      <> show (nlOriginalBytes ledger)
      <> " bytes-sealed="
      <> maybe "n/a" show (nlSealedBytes ledger)
      <> " nodes-saved="
      <> show (nlRealizedNodesSaved ledger)
  )
    : fmap renderLatentGroup (nlLatent ledger)

renderLatentGroup :: LatentGroup -> String
renderLatentGroup group =
  "latent reason="
    <> lgReason group
    <> " nodes="
    <> show (lgNodes group)
    <> " bindings="
    <> show (lgBindings group)

renderOracleProvenance :: OracleProvenance -> String
renderOracleProvenance = \case
  OracleAttached keyKind recordedPath ->
    "oracle status=attached key-kind=" <> hieSourceKeyKindKey keyKind <> " recorded=" <> recordedPath
  OracleUnattached attachFailure ->
    case attachFailure of
      OracleLookupMissing triedKeys ->
        "oracle status=missing tried=" <> bracketed (fmap renderTriedKey triedKeys)
      OracleLookupAmbiguous keyKind keyValue candidates ->
        "oracle status=ambiguous key-kind="
          <> hieSourceKeyKindKey keyKind
          <> " key="
          <> keyValue
          <> " candidates="
          <> bracketed candidates

renderBindingFrontMetrics :: Maybe HsExprBindingRuleMetrics -> String
renderBindingFrontMetrics = \case
  Nothing ->
    "binding-front enabled=False"
  Just bridgeMetrics ->
    "binding-front enabled=True redex-sites="
      <> show (hbrmRedexSiteCount bridgeMetrics)
      <> " allowed="
      <> show (hbrmAllowedCount bridgeMetrics)
      <> " freshened="
      <> show (hbrmFresheningCount bridgeMetrics)
      <> " obstructions="
      <> show (hbrmObstructionCount bridgeMetrics)
      <> " rules="
      <> show (hbrmGeneratedRuleCount bridgeMetrics)
      <> " facts="
      <> show (hbrmFactRuleCount bridgeMetrics)

renderVocabularyMetrics :: HsExprVocabularyRuleMetrics -> String
renderVocabularyMetrics vocabularyMetrics =
  "vocabulary-laws laws="
    <> show (hvrmVocabularyLawCount vocabularyMetrics)
    <> " rules-generated="
    <> show (hvrmVocabularyGeneratedRuleCount vocabularyMetrics)
    <> " rules-admitted="
    <> show (hvrmVocabularyAdmittedRuleCount vocabularyMetrics)
    <> " gated="
    <> show (hvrmVocabularyGatedLawCount vocabularyMetrics)

renderEvidenceFactCensus :: EvidenceFactCensus -> String
renderEvidenceFactCensus evidenceCensus =
  "evidence-facts lawful="
    <> show (efcLawful evidenceCensus)
    <> " unlawful="
    <> show (efcUnlawful evidenceCensus)
    <> " ambiguous="
    <> show (efcAmbiguous evidenceCensus)

renderNumTypeFactCensus :: NumTypeFactCensus -> String
renderNumTypeFactCensus numTypeCensus =
  "num-type-facts lawful="
    <> show (ntfcLawfulSpanCount numTypeCensus)
    <> " unlawful="
    <> show (ntfcUnlawfulSpanCount numTypeCensus)
    <> " unobserved="
    <> show (ntfcUnobservedSpanCount numTypeCensus)

renderTypeEvidence :: TypeEvidenceCensus -> String
renderTypeEvidence census =
  "type-evidence observed="
    <> show (tecObservedClassCount census)
    <> " polymorphic="
    <> show (tecPolymorphicClassCount census)
    <> " unobserved="
    <> show (tecUnobservedClassCount census)

renderGatedLaw :: GatedLawReport -> String
renderGatedLaw gatedLaw =
  "gated-law id="
    <> show (lawIdKey (glrLaw gatedLaw))
    <> " reason="
    <> lawGateReasonKey (glrReason gatedLaw)
    <> renderLawGateReasonDetail (glrReason gatedLaw)
    <> " rules="
    <> show (glrRuleCount gatedLaw)

renderSelfLawRow :: SelfLawRow -> String
renderSelfLawRow row =
  "self-law binding="
    <> slrBinding row
    <> " outcome="
    <> either (\reason -> "refused reason=" <> selfLawRefusalKey reason) (\lawValue -> "admitted law=" <> show (lawIdKey lawValue)) (slrOutcome row)

renderLawGateReasonDetail :: LawGateReason -> String
renderLawGateReasonDetail = \case
  GateMissingOracleKeys keys ->
    " keys=" <> bracketed (fmap oracleKeyString (Set.toAscList keys))
  GateOracleUnattached attachFailure ->
    " failure=" <> renderOracleAttachFailure attachFailure
  GateTierInadmissible trustTier ->
    " tier=" <> trustTierKey trustTier
  GateFidelityInadmissible semanticFidelity ->
    " fidelity=" <> semanticFidelityKey semanticFidelity

renderOracleAttachFailure :: OracleAttachFailure -> String
renderOracleAttachFailure = \case
  OracleLookupMissing triedKeys ->
    oracleAttachFailureKey (OracleLookupMissing triedKeys) <> " triedKeys=" <> bracketed (fmap renderTriedKey triedKeys)
  OracleLookupAmbiguous keyKind keyValue candidates ->
    oracleAttachFailureKey (OracleLookupAmbiguous keyKind keyValue candidates)
      <> " key-kind="
      <> hieSourceKeyKindKey keyKind
      <> " key="
      <> keyValue
      <> " candidates="
      <> bracketed candidates

renderTriedKey :: TriedKey -> String
renderTriedKey (TriedKey keyKind keyValue) =
  hieSourceKeyKindKey keyKind <> ":" <> keyValue

renderSealOutcome :: SealOutcome -> String
renderSealOutcome sealOutcome =
  renderStatus sealOutcome
    <> " patched-source-parses="
    <> show (sealPatchedSourceParseCount sealOutcome)
  where
    renderStatus = \case
      SealEmpty ->
        "seal status=empty"
      Sealed sealedSource ->
        "seal status=sealed bytes=" <> show (length (sealedSourceText sealedSource))
      SealRefused _ sealFailure ->
        "seal status=refused error=" <> renderNebulaError sealFailure

renderRuleFire :: RuleFire -> String
renderRuleFire ruleFire =
  let RewriteRuleId ruleId = rfRuleId ruleFire
   in "rule-fire rule="
        <> show ruleId
        <> " matched="
        <> show (rfMatchedTotal ruleFire)
        <> " scheduled="
        <> show (rfScheduledTotal ruleFire)

renderCertificate :: HunkCertificate -> String
renderCertificate certificate =
  "certificate binding="
    <> hcBinding certificate
    <> " steps="
    <> show (length (hcEntries certificate))
    <> " type-verdict="
    <> renderTypeVerdict (hcTypeVerdict certificate)
    <> " laws="
    <> bracketed (fmap (show . lawIdKey . lsLaw) stamps)
    <> " tiers="
    <> bracketed (fmap (trustTierKey . lsTier) stamps)
    <> " digest="
    <> stableDigestKey (hcDigest certificate)
  where
    stamps =
      Set.toAscList
        ( Set.fromList
            [ lawStamp
            | entry <- hcEntries certificate,
              Just lawStamp <- [npStamp (peProvenance entry)]
            ]
      )

renderTypeVerdict :: TypeVerdict -> String
renderTypeVerdict = \case
  TypeCompatible ->
    typeVerdictKey TypeCompatible
  TypePolymorphic ->
    typeVerdictKey TypePolymorphic
  TypeUnknown ->
    typeVerdictKey TypeUnknown
  TypeIncompatible conflicts ->
    typeVerdictKey (TypeIncompatible conflicts) <> " conflicts=" <> show (length conflicts)

bracketed :: [String] -> String
bracketed values =
  "[" <> intercalate "," values <> "]"

stableDigestKey :: StableDigest128 -> String
stableDigestKey (StableDigest128 high low) =
  padWord64 high <> padWord64 low

padWord64 :: Word64 -> String
padWord64 wordValue =
  let rendered = showHex wordValue ""
   in replicate (16 - length rendered) '0' <> rendered

renderNebulaError :: NebulaError -> String
renderNebulaError failure =
  nebulaErrorKey failure <> foldMap ((" " <>) . uncurry (<>)) (nebulaErrorFields failure)

nebulaErrorFields :: NebulaError -> [(String, String)]
nebulaErrorFields = \case
  NebulaWorkspaceError path message ->
    [("path=", path), ("message=", message)]
  NebulaParseError message ->
    [("message=", message)]
  NebulaLatticeError message ->
    [("message=", message)]
  NebulaInsertionError message ->
    [("message=", message)]
  NebulaRuleDerivationError message ->
    [("message=", message)]
  NebulaBindingFrontError message ->
    [("message=", message)]
  NebulaSaturationError message ->
    [("message=", message)]
  NebulaProofReplayAllocationError allocationError ->
    [("allocation-error=", show allocationError)]
  NebulaContextSupportError supportError ->
    [("support-error=", show supportError)]
  NebulaExtractionError subject message ->
    [("subject=", subject), ("message=", message)]
  NebulaSynthesisError message ->
    [("message=", message)]
  NebulaArityMismatch nameCount contextCount seedCount ->
    [ ("names=", show nameCount),
      ("contexts=", show contextCount),
      ("seeds=", show seedCount)
    ]
  NebulaWriteBackError message ->
    [("message=", message)]
  NebulaSpliceError message ->
    [("message=", message)]
  NebulaSealError subject message ->
    [("subject=", subject), ("message=", message)]

renderBindingReport :: BindingReport -> String
renderBindingReport binding =
  "binding name="
    <> brName binding
    <> " original-size="
    <> show (brOriginalSize binding)
    <> " extracted-size="
    <> show (brExtractedSize binding)
    <> " extraction-cost="
    <> show (brExtractionCost binding)

renderSynthesis :: SynthesisOutcome -> [String]
renderSynthesis outcome =
  [ "synthesis estimated-win="
      <> show (soEstimatedWin outcome)
      <> " realized-win="
      <> show (soRealizedWin outcome)
      <> " pre-total="
      <> show (soPreExtractedTotal outcome)
      <> " post-total="
      <> show (soPostExtractedTotal outcome)
      <> " localized-merges="
      <> show (psrLocalizedMerges (soStagingReport outcome))
      <> " global-fallback-merges="
      <> show (psrGlobalFallbackMerges (soStagingReport outcome))
      <> " localized-definition-merges="
      <> show (psrLocalizedDefinitionMerges (soStagingReport outcome))
      <> " localized-application-merges="
      <> show (psrLocalizedApplicationMerges (soStagingReport outcome))
      <> " global-definition-fallback-merges="
      <> show (psrGlobalDefinitionFallbackMerges (soStagingReport outcome))
      <> " global-application-fallback-merges="
      <> show (psrGlobalApplicationFallbackMerges (soStagingReport outcome))
      <> " dirty-contexts="
      <> show (psrDirtyContextCount (soStagingReport outcome))
      <> " harvest-advance="
      <> renderHarvestAdvanceDecision (soHarvestDecision outcome)
  ]
    <> fmap renderDefinition (soDefinitions outcome)
    <> fmap renderRejection (soRejected outcome)
  where
    renderHarvestAdvanceDecision = \case
      Nothing ->
        "none"
      Just HarvestAdvanced ->
        "advanced"
      Just (HarvestFellBack reason) ->
        "fallback(" <> renderHarvestFallbackReason reason <> ")"
    renderHarvestFallbackReason = \case
      HarvestFallbackGlobalPlanMerge ->
        "global-plan-merge"
      HarvestFallbackDirtyRatio dirtyCount totalCount ratio ->
        "dirty-ratio:" <> show dirtyCount <> "/" <> show totalCount <> "=" <> show ratio
      HarvestFallbackStageSectionObstruction obstruction ->
        "stage-section-obstruction:" <> obstruction
      HarvestFallbackSaturationSectionObstruction obstruction ->
        "saturation-section-obstruction:" <> obstruction
    renderDefinition definition =
      "definition name="
        <> synthesizedNameText (sdName definition)
        <> " size="
        <> show (sdSize definition)
        <> " sites="
        <> intercalate "," (fmap renderSynthesizedSite (sdSites definition))
        <> " estimated-win="
        <> show (sdEstimatedWin definition)
    renderRejection rejection =
      "rejected sites="
        <> unwords (fmap renderCandidateSiteLabel (rejSites rejection))
        <> " reason="
        <> candidateRejectionKey (rejReason rejection)
        <> renderRejectionDetail (rejReason rejection)
        <> " estimated-win="
        <> show (rejEstimatedWin rejection)
        <> " realized-win="
        <> maybe "none" show (rejRealizedWin rejection)
    renderRejectionDetail = \case
      RejectedRecordOwnershipDiagnostic findings ->
        foldMap (\finding -> " ownership-kind=" <> recordOwnershipKindKey (rofKind finding)) findings
      _ ->
        ""

renderSynthesizedSite :: SynthesizedSite -> String
renderSynthesizedSite site =
  ssBindingName site <> maybe "" renderRegionSuffix (ssRegion site)

renderCandidateSiteLabel :: CandidateSiteLabel -> String
renderCandidateSiteLabel site =
  cslBindingName site
    <> maybe "" renderRegionSuffix (cslRegion site)
    <> " kind="
    <> candidateSiteKindKey (cslKind site)

renderRegionSuffix :: sourceRegion -> String
renderRegionSuffix _ =
  "@region"

renderDisposition :: (String, HunkDisposition) -> String
renderDisposition (bindingName, disposition) =
  "disposition binding="
    <> bindingName
    <> " status="
    <> renderDispositionStatus disposition

renderDispositionStatus :: HunkDisposition -> String
renderDispositionStatus = \case
  HunkSealed ->
    "sealed"
  HunkBlocked blockReason ->
    "blocked reason=" <> blockReasonKey blockReason <> renderBlockReasonDetail blockReason

renderBlockReasonDetail :: HunkBlockReason -> String
renderBlockReasonDetail = \case
  BlockedWriteBack (RefusedSourceQuality refusal) ->
    " source-quality=" <> sourceQualityRefusalKey refusal
  BlockedWriteBack _ ->
    ""
  BlockedSeal sealFailure ->
    " error=" <> renderNebulaError sealFailure
  BlockedCandidate _ ->
    ""

renderWorkspaceReport :: WorkspaceReport -> [String]
renderWorkspaceReport report =
  foldMap renderModuleReport (wrModules report)
    <> fmap renderModuleFailure (wrModuleFailures report)
    <> fmap renderWorkspaceError (wrWorkspaceErrors report)
    <> [ "workspace modules="
           <> show (length (wrModules report))
           <> " failures="
           <> show (length (wrModuleFailures report) + length (wrWorkspaceErrors report))
           <> " realized-bytes-original="
           <> show (sum (fmap mrOriginalBytes (wrModules report)))
           <> " realized-bytes-sealed="
           <> show (sum (fmap (maybe 0 id . mrSealedBytes) (wrModules report)))
           <> " realized-nodes-saved="
           <> show (sum (fmap (nlRealizedNodesSaved . moduleLedger) (wrModules report)))
       ]
    <> fmap renderLatentGroup (workspaceLatent report)
    <> [ "workspace-nodes original="
           <> show (wrOriginalTotal report)
           <> " final="
           <> show (wrFinalTotal report)
           <> " compression-ratio="
           <> renderRatio (wrCompressionRatio report)
       ]

renderModuleFailure :: (FilePath, NebulaError) -> String
renderModuleFailure (modulePath, moduleFailure) =
  "failure path=" <> modulePath <> " error=" <> renderNebulaError moduleFailure

renderWorkspaceError :: NebulaError -> String
renderWorkspaceError workspaceFailure =
  "workspace-error " <> renderNebulaError workspaceFailure
