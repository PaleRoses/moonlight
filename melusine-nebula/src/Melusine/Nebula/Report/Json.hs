{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.Report.Json
  ( J (..),
    DetailTier (..),
    schemaVersion,
    BaselineFailure (..),
    baselineFailureKey,
    renderJson,
    parseJson,
    workspaceEnvelopeJson,
    parseBaseline,
    corpusEnvelopeJson,
    nebulaErrorJson,
    nebulaFailureJson,
  )
where

import Data.Char (chr, isDigit, isHexDigit, ord)
import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Melusine.Nebula.Core
  ( NebulaError (..),
    nebulaErrorKey,
    nebulaErrorMessage,
    nebulaErrorPath,
  )
import Melusine.Nebula.Discovery.Choose (candidateSiteKindKey)
import Melusine.Nebula.Harvest.Maintain
  ( HarvestAdvanceDecision (..),
    HarvestFallbackReason (..),
  )
import Melusine.Nebula.Proof.Audit
  ( StepTypeConflict (..),
    TypeEvidenceCensus (..),
    TypeVerdict (..),
  )
import Moonlight.Core (ClassId (..), RewriteRuleId (..))
import Melusine.Nebula.Proof.Certificate
  ( HunkCertificate (..),
    NebulaProvenance (..),
    ProvenanceEntry (..),
  )
import Melusine.Nebula.Report.Text
  ( BindingReport (..),
    HunkBlockReason (..),
    HunkDisposition (..),
    LatentGroup (..),
    ModuleReport (..),
    NebulaLedger (..),
    OracleProvenance (..),
    WorkspaceReport (..),
    hieSourceKeyKindKey,
    moduleLedger,
    oracleAttachFailureKey,
    renderRefusalKey,
    saturationTerminationKey,
    semanticFidelityKey,
    trustTierKey,
    typeVerdictKey,
    workspaceLatent,
  )
import Melusine.Nebula.Rewrite.Corpus
  ( EvidenceFactCensus (..),
    GatedLawReport (..),
    LawGateReason (..),
    LawStamp (..),
    NumTypeFactCensus (..),
    SelfLawRow (..),
    lawGateReasonKey,
    selfLawRefusalKey,
  )
import Melusine.Nebula.Rewrite.Saturate
  ( RuleFire (..),
    SaturationLifecycleCounts (..),
    SaturationTraceImpact (..),
    smLifecycleCounts,
    smTraceImpact,
  )
import Melusine.Nebula.Synthesis.Core
  ( CandidateSiteLabel (..),
    PlanStagingReport (..),
    RejectedCandidate (..),
    SynthesisOutcome (..),
    SynthesizedDefinition (..),
    SynthesizedName (..),
    SynthesizedSite (..),
  )
import Melusine.Nebula.Synthesis.Types
  ( CandidateRejection (..),
    RecordOwnershipFinding (..),
    candidateRejectionKey,
    recordOwnershipKindKey,
  )
import Melusine.Nebula.Write.Back
  ( LineOnlyMinificationEvidence (..),
    SourceLineQualityEvidence (..),
    SourceQualityRefusal (..),
    WriteBackRefusal (..),
    WriteOutcome (..),
    WriteStatus (..),
    sourceQualityRefusalKey,
    writeBackRefusalKey,
    writeStatusKey,
  )
import Melusine.Nebula.Write.Seal (SealOutcome (..), sealPatchedSourceParseCount, sealedSourceText)
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( HsExprBindingRuleMetrics (..),
    HsExprSupportRuleMetrics (..),
    HsExprVocabularyRuleMetrics (..),
    SourceRegion (..),
  )
import Moonlight.Flow.Model.Schema.Digest (StableDigest128 (..))
import Moonlight.Rewrite.System (lawIdKey, oracleKeyString)
import Numeric (showFFloat, showHex)
import Moonlight.Pale.Ghc.Hie.SourceKey (OracleAttachFailure (..), TriedKey (..))

type J :: Type
data J
  = JObj ![(String, J)]
  | JArr ![J]
  | JStr !String
  | JInt !Int
  | JDouble !Double
  | JBool !Bool
  | JNull
  deriving stock (Eq, Show)

type DetailTier :: Type
data DetailTier
  = TierSummary
  | TierStandard
  | TierFull
  deriving stock (Eq, Ord, Show)

schemaVersion :: Int
schemaVersion =
  1

type BaselineFailure :: Type
data BaselineFailure
  = BaselineUnreadable !String
  | BaselineParse !String
  | BaselineSchemaVersion !Int
  deriving stock (Eq, Show)

baselineFailureKey :: BaselineFailure -> String
baselineFailureKey = \case
  BaselineUnreadable {} -> "unreadable"
  BaselineParse {} -> "parse"
  BaselineSchemaVersion {} -> "schema-version"

type CorpusBaseline :: Type
newtype CorpusBaseline = CorpusBaseline (Map FilePath BaselineModule)
  deriving stock (Eq, Show)

type BaselineModule :: Type
data BaselineModule = BaselineModule
  { bmRealized :: !Int,
    bmLatent :: !(Map String Int)
  }
  deriving stock (Eq, Show)

renderJson :: J -> String
renderJson value =
  renderValue value ""

renderValue :: J -> ShowS
renderValue = \case
  JObj members ->
    showChar '{' . renderMembers members . showChar '}'
  JArr elements ->
    showChar '[' . renderElements elements . showChar ']'
  JStr text ->
    renderString text
  JInt intValue ->
    shows intValue
  JDouble doubleValue ->
    showFFloat (Just 6) doubleValue
  JBool True ->
    showString "true"
  JBool False ->
    showString "false"
  JNull ->
    showString "null"

renderMembers :: [(String, J)] -> ShowS
renderMembers members =
  foldr (.) id
    ( intersperseS
        (showChar ',')
        [renderString key . showChar ':' . renderValue memberValue | (key, memberValue) <- members]
    )

renderElements :: [J] -> ShowS
renderElements elements =
  foldr (.) id (intersperseS (showChar ',') (fmap renderValue elements))

intersperseS :: ShowS -> [ShowS] -> [ShowS]
intersperseS separator = \case
  [] ->
    []
  [single] ->
    [single]
  first : rest ->
    first : separator : intersperseS separator rest

renderString :: String -> ShowS
renderString text =
  showChar '"' . foldr (\character acc -> renderChar character . acc) id text . showChar '"'

renderChar :: Char -> ShowS
renderChar = \case
  '"' ->
    showString "\\\""
  '\\' ->
    showString "\\\\"
  '\n' ->
    showString "\\n"
  '\t' ->
    showString "\\t"
  '\r' ->
    showString "\\r"
  character
    | ord character < 0x20 ->
        showString (renderControl (ord character))
    | otherwise ->
        showChar character

renderControl :: Int -> String
renderControl codePoint =
  let hexDigits = showHex codePoint ""
   in "\\u" <> replicate (4 - length hexDigits) '0' <> hexDigits

parseJson :: String -> Either String J
parseJson input =
  case parseValue (dropWhitespace input) of
    Left message ->
      Left message
    Right (value, rest) ->
      case dropWhitespace rest of
        [] ->
          Right value
        trailing ->
          Left ("trailing characters after JSON value: " <> take 16 trailing)

dropWhitespace :: String -> String
dropWhitespace =
  dropWhile (`elem` " \t\n\r")

parseValue :: String -> Either String (J, String)
parseValue input =
  case dropWhitespace input of
    [] ->
      Left "unexpected end of input"
    rest@('{' : _) ->
      parseObject rest
    rest@('[' : _) ->
      parseArray rest
    rest@('"' : _) ->
      fmap (\(text, remaining) -> (JStr text, remaining)) (parseStringLiteral rest)
    't' : 'r' : 'u' : 'e' : remaining ->
      Right (JBool True, remaining)
    'f' : 'a' : 'l' : 's' : 'e' : remaining ->
      Right (JBool False, remaining)
    'n' : 'u' : 'l' : 'l' : remaining ->
      Right (JNull, remaining)
    rest ->
      parseNumber rest

parseObject :: String -> Either String (J, String)
parseObject input =
  case dropWhitespace input of
    '{' : afterBrace ->
      case dropWhitespace afterBrace of
        '}' : afterClose ->
          Right (JObj [], afterClose)
        memberStart ->
          parseMembers memberStart []
    _ ->
      Left "expected '{'"

parseMembers :: String -> [(String, J)] -> Either String (J, String)
parseMembers input acc = do
  (key, afterKey) <- parseStringLiteral (dropWhitespace input)
  afterColon <-
    case dropWhitespace afterKey of
      ':' : remaining ->
        Right remaining
      _ ->
        Left "expected ':' in object member"
  (memberValue, afterValue) <- parseValue afterColon
  let members = (key, memberValue) : acc
  case dropWhitespace afterValue of
    ',' : afterComma ->
      parseMembers afterComma members
    '}' : afterClose ->
      Right (JObj (reverse members), afterClose)
    _ ->
      Left "expected ',' or '}' in object"

parseArray :: String -> Either String (J, String)
parseArray input =
  case dropWhitespace input of
    '[' : afterBracket ->
      case dropWhitespace afterBracket of
        ']' : afterClose ->
          Right (JArr [], afterClose)
        elementStart ->
          parseElements elementStart []
    _ ->
      Left "expected '['"

parseElements :: String -> [J] -> Either String (J, String)
parseElements input acc = do
  (elementValue, afterValue) <- parseValue input
  let elements = elementValue : acc
  case dropWhitespace afterValue of
    ',' : afterComma ->
      parseElements afterComma elements
    ']' : afterClose ->
      Right (JArr (reverse elements), afterClose)
    _ ->
      Left "expected ',' or ']' in array"

parseStringLiteral :: String -> Either String (String, String)
parseStringLiteral = \case
  '"' : afterQuote ->
    parseStringChars afterQuote []
  _ ->
    Left "expected '\"'"

parseStringChars :: String -> [Char] -> Either String (String, String)
parseStringChars input acc =
  case input of
    [] ->
      Left "unterminated string literal"
    '"' : remaining ->
      Right (reverse acc, remaining)
    '\\' : escaped ->
      parseEscape escaped acc
    character : remaining
      | ord character < 0x20 ->
          Left "unescaped control character in string literal"
      | otherwise ->
          parseStringChars remaining (character : acc)

parseEscape :: String -> [Char] -> Either String (String, String)
parseEscape input acc =
  case input of
    '"' : remaining ->
      parseStringChars remaining ('"' : acc)
    '\\' : remaining ->
      parseStringChars remaining ('\\' : acc)
    '/' : remaining ->
      parseStringChars remaining ('/' : acc)
    'n' : remaining ->
      parseStringChars remaining ('\n' : acc)
    't' : remaining ->
      parseStringChars remaining ('\t' : acc)
    'r' : remaining ->
      parseStringChars remaining ('\r' : acc)
    'b' : remaining ->
      parseStringChars remaining ('\b' : acc)
    'f' : remaining ->
      parseStringChars remaining ('\f' : acc)
    'u' : a : b : c : d : remaining
      | all isHexDigit [a, b, c, d] ->
          parseStringChars remaining (chr (hexValue [a, b, c, d]) : acc)
    _ ->
      Left "invalid escape sequence"

hexValue :: [Char] -> Int
hexValue =
  foldl' (\acc character -> acc * 16 + hexDigit character) 0

hexDigit :: Char -> Int
hexDigit character
  | isDigit character =
      ord character - ord '0'
  | otherwise =
      10 + ord character - ord (if character >= 'a' then 'a' else 'A')

parseNumber :: String -> Either String (J, String)
parseNumber input =
  let (numberText, rest) = span isNumberChar input
   in if null numberText
        then Left ("expected a JSON value at: " <> take 16 input)
        else
          if any (`elem` ".eE") numberText
            then case reads numberText of
              [(doubleValue, "")] ->
                Right (JDouble doubleValue, rest)
              _ ->
                Left ("malformed number: " <> numberText)
            else case reads numberText of
              [(intValue, "")] ->
                Right (JInt intValue, rest)
              _ ->
                Left ("malformed number: " <> numberText)

isNumberChar :: Char -> Bool
isNumberChar character =
  isDigit character || character `elem` "+-.eE"

workspaceEnvelopeJson :: DetailTier -> WorkspaceReport -> Maybe [WriteOutcome] -> J
workspaceEnvelopeJson detailTier report writeOutcomes =
  envelopeJson
    (maybe "report" (const "write") writeOutcomes)
    (workspaceSummaryJson report writeFailureCount)
    (fmap latentGroupJson (workspaceLatent report))
    (fmap (moduleReportJson detailTier writeOutcomesByPath) (wrModules report))
    failures
    []
  where
    writeFailures = maybe [] (mapMaybe writeOutcomeFailureJson) writeOutcomes
    writeOutcomesByPath = fmap (Map.fromList . fmap (\outcome -> (woPath outcome, outcome))) writeOutcomes
    writeFailureCount = length writeFailures
    failures =
      fmap (uncurry (nebulaFailureJson . Just)) (wrModuleFailures report)
        <> fmap (nebulaFailureJson Nothing) (wrWorkspaceErrors report)
        <> writeFailures

envelopeJson :: String -> J -> [J] -> [J] -> [J] -> [(String, J)] -> J
envelopeJson mode summary latent modules failures additions =
  JObj
    ( [ ("schemaVersion", JInt schemaVersion),
        ("tool", JStr "melusine-nebula"),
        ("mode", JStr mode),
        ("summary", summary),
        ("latent", JArr latent),
        ("modules", JArr modules),
        ("failures", JArr failures)
      ]
        <> additions
    )

workspaceSummaryJson :: WorkspaceReport -> Int -> J
workspaceSummaryJson report additionalFailureCount =
  summaryJson
    (length modules)
    (length (filter moduleIsSealed modules))
    failureCount
    (sum (fmap mrOriginalBytes modules))
    (sum (fmap (maybe 0 id . mrSealedBytes) modules))
    (wrOriginalTotal report)
    (wrFinalTotal report)
    (wrCompressionRatio report)
    (sum (fmap (nlRealizedNodesSaved . moduleLedger) modules))
  where
    modules = wrModules report
    failureCount =
      length (wrModuleFailures report)
        + length (wrWorkspaceErrors report)
        + additionalFailureCount

summaryJson :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Maybe Double -> Int -> J
summaryJson moduleCount sealedCount failureCount originalBytes sealedBytes originalNodes finalNodes compressionRatio nodesSaved =
  JObj
    [ ("status", JStr (summaryStatusKey moduleCount failureCount)),
      ("modules", JInt moduleCount),
      ("sealed", JInt sealedCount),
      ("failed", JInt failureCount),
      ( "bytes",
        JObj
          [ ("original", JInt originalBytes),
            ("sealed", JInt sealedBytes)
          ]
      ),
      ( "nodeTotals",
        JObj
          [ ("originalTotal", JInt originalNodes),
            ("finalTotal", JInt finalNodes),
            ("compressionRatio", maybe JNull JDouble compressionRatio)
          ]
      ),
      ("nodesSaved", JInt nodesSaved)
    ]

summaryStatusKey :: Int -> Int -> String
summaryStatusKey moduleCount failureCount
  | failureCount > 0 = "degraded"
  | moduleCount == 0 = "empty"
  | otherwise = "clean"

moduleReportJson :: DetailTier -> Maybe (Map FilePath WriteOutcome) -> ModuleReport -> J
moduleReportJson detailTier writeOutcomesByPath report =
  case detailTier of
    TierSummary ->
      moduleSummaryJson report
    TierStandard ->
      JObj (moduleStandardMembers TierStandard writeOutcomesByPath report)
    TierFull ->
      JObj
        ( moduleStandardMembers TierFull writeOutcomesByPath report
            <> moduleFullMembers report
        )

moduleSummaryJson :: ModuleReport -> J
moduleSummaryJson report =
  JObj
    [ ("path", JStr (mrPath report)),
      ("sealStatus", JStr (sealStatusKey (mrSeal report))),
      ("nodesSaved", JInt (nlRealizedNodesSaved (moduleLedger report))),
      ("blockedCount", JInt (length [() | (_, HunkBlocked {}) <- mrDispositions report]))
    ]

moduleStandardMembers :: DetailTier -> Maybe (Map FilePath WriteOutcome) -> ModuleReport -> [(String, J)]
moduleStandardMembers detailTier writeOutcomesByPath report =
  [ ("path", JStr (mrPath report)),
    ("oracle", oracleProvenanceJson (mrOracleProvenance report)),
    ("saturation", saturationJson report),
    ("sites", siteMetricsJson (mrSiteMetrics report)),
    ("vocabulary", vocabularyMetricsJson (mrVocabularyMetrics report)),
    ("evidenceFacts", evidenceFactCensusJson (mrEvidenceFactCensus report)),
    ("numTypeFacts", numTypeFactCensusJson (mrNumTypeFactCensus report)),
    ("typeEvidence", typeEvidenceCensusJson (mrTypeEvidence report)),
    ("bindingFront", maybe JNull bindingFrontJson (mrBindingFrontMetrics report)),
    ("antiUnifyPairBound", JInt (mrAntiUnifyPairBound report)),
    ("certificateCount", JInt (length (mrCertificates report))),
    ("synthesis", synthesisJson detailTier (mrSynthesis report)),
    ("dispositions", JArr (fmap (dispositionJson (ruleLawJoin (mrCertificates report))) (mrDispositions report))),
    ("ledger", ledgerJson (moduleLedger report)),
    ("nodeTotals", moduleNodeTotalsJson report),
    ("seal", sealJson (mrSeal report))
  ]
    <> writeMember
  where
    writeMember =
      case writeOutcomesByPath >>= Map.lookup (mrPath report) of
        Nothing -> []
        Just outcome -> [("write", writeOutcomeJson outcome)]

moduleFullMembers :: ModuleReport -> [(String, J)]
moduleFullMembers report =
  [ ("ruleFires", JArr (fmap ruleFireJson (mrRuleFires report))),
    ("certificates", JArr (fmap certificateJson (mrCertificates report))),
    ("selfLaws", JArr (fmap selfLawJson (mrSelfLawRows report))),
    ("gatedLaws", JArr (fmap gatedLawJson (mrGatedLaws report))),
    ("bindings", JArr (fmap bindingReportJson (mrBindingReports report))),
    ("diff", JArr (fmap JStr (mrDiff report)))
  ]

moduleIsSealed :: ModuleReport -> Bool
moduleIsSealed report =
  case mrSeal report of
    Sealed {} -> True
    SealRefused {} -> False
    SealEmpty -> False

sealStatusKey :: SealOutcome -> String
sealStatusKey = \case
  Sealed {} -> "sealed"
  SealRefused {} -> "refused"
  SealEmpty -> "empty"

oracleProvenanceJson :: OracleProvenance -> J
oracleProvenanceJson = \case
  OracleAttached keyKind recordedPath ->
    JObj
      [ ("status", JStr "attached"),
        ("keyKind", JStr (hieSourceKeyKindKey keyKind)),
        ("recordedPath", JStr recordedPath)
      ]
  OracleUnattached failure ->
    case failure of
      OracleLookupMissing triedKeys ->
        JObj
          [ ("status", JStr "missing"),
            ("triedKeys", JArr (fmap triedKeyJson triedKeys))
          ]
      OracleLookupAmbiguous keyKind keyValue candidates ->
        JObj
          [ ("status", JStr "ambiguous"),
            ("keyKind", JStr (hieSourceKeyKindKey keyKind)),
            ("key", JStr keyValue),
            ("candidates", JArr (fmap JStr candidates))
          ]

saturationJson :: ModuleReport -> J
saturationJson report =
  JObj
    [ ("termination", JStr (saturationTerminationKey (mrTermination report))),
      ("iterations", JInt (mrIterations report)),
      ("matchesApplied", JInt (mrMatchesApplied report)),
      ("scheduledTotal", JInt (mrScheduledTotal report)),
      ( "nodes",
        JObj
          [ ("before", JInt (mrInitialNodeCount report)),
            ("after", JInt (mrFinalNodeCount report))
          ]
      ),
      ( "classes",
        JObj
          [ ("before", JInt (mrInitialClassCount report)),
            ("after", JInt (mrFinalClassCount report))
          ]
      ),
      ( "lifecycle",
        saturationLifecycleJson
          (smLifecycleCounts (soSaturatedModule (mrSynthesis report)))
      )
    ]

saturationLifecycleJson :: SaturationLifecycleCounts -> J
saturationLifecycleJson lifecycle =
  JObj
    [ ("planPreparations", JInt (slcPlanPreparations lifecycle)),
      ("freshRuns", JInt (slcFreshRuns lifecycle)),
      ("resumptions", JInt (slcResumptions lifecycle))
    ]

siteMetricsJson :: HsExprSupportRuleMetrics -> J
siteMetricsJson metrics =
  JObj
    [ ("lambda", JInt (hsrmLambdaSiteCount metrics)),
      ("let", JInt (hsrmLetSiteCount metrics)),
      ("rulesTotal", JInt (hsrmTotalRuleCount metrics)),
      ("diagnosticSpans", JInt (hsrmDiagnosticSpanCount metrics))
    ]

vocabularyMetricsJson :: HsExprVocabularyRuleMetrics -> J
vocabularyMetricsJson metrics =
  JObj
    [ ("laws", JInt (hvrmVocabularyLawCount metrics)),
      ("rulesGenerated", JInt (hvrmVocabularyGeneratedRuleCount metrics)),
      ("rulesAdmitted", JInt (hvrmVocabularyAdmittedRuleCount metrics)),
      ("gated", JInt (hvrmVocabularyGatedLawCount metrics))
    ]

evidenceFactCensusJson :: EvidenceFactCensus -> J
evidenceFactCensusJson census =
  JObj
    [ ("lawful", JInt (efcLawful census)),
      ("unlawful", JInt (efcUnlawful census)),
      ("ambiguous", JInt (efcAmbiguous census))
    ]

numTypeFactCensusJson :: NumTypeFactCensus -> J
numTypeFactCensusJson census =
  JObj
    [ ("lawful", JInt (ntfcLawfulSpanCount census)),
      ("unlawful", JInt (ntfcUnlawfulSpanCount census)),
      ("unobserved", JInt (ntfcUnobservedSpanCount census))
    ]

typeEvidenceCensusJson :: TypeEvidenceCensus -> J
typeEvidenceCensusJson census =
  JObj
    [ ("observed", JInt (tecObservedClassCount census)),
      ("polymorphic", JInt (tecPolymorphicClassCount census)),
      ("unobserved", JInt (tecUnobservedClassCount census))
    ]

bindingFrontJson :: HsExprBindingRuleMetrics -> J
bindingFrontJson metrics =
  JObj
    [ ("redexSites", JInt (hbrmRedexSiteCount metrics)),
      ("allowed", JInt (hbrmAllowedCount metrics)),
      ("freshened", JInt (hbrmFresheningCount metrics)),
      ("obstructions", JInt (hbrmObstructionCount metrics)),
      ("rules", JInt (hbrmGeneratedRuleCount metrics)),
      ("facts", JInt (hbrmFactRuleCount metrics))
    ]

synthesisJson :: DetailTier -> SynthesisOutcome -> J
synthesisJson detailTier outcome =
  JObj
    [ ("estimatedWin", JInt (soEstimatedWin outcome)),
      ("realizedWin", JInt (soRealizedWin outcome)),
      ("preExtractedTotal", JInt (soPreExtractedTotal outcome)),
      ("postExtractedTotal", JInt (soPostExtractedTotal outcome)),
      ("definitions", JArr (fmap synthesizedDefinitionJson (soDefinitions outcome))),
      ("rejectedCandidates", JArr (fmap rejectedCandidateJson visibleRejections)),
      ("rejectedCandidatesOmitted", JInt omittedCount),
      ("staging", stagingJson outcome)
    ]
  where
    orderedRejections =
      sortOn (Down . abs . rejEstimatedWin) (soRejected outcome)
    visibleRejections =
      case detailTier of
        TierFull -> orderedRejections
        TierStandard -> take 20 orderedRejections
        TierSummary -> []
    omittedCount = length orderedRejections - length visibleRejections

synthesizedDefinitionJson :: SynthesizedDefinition -> J
synthesizedDefinitionJson definition =
  JObj
    [ ("name", JStr (synthesizedNameText (sdName definition))),
      ("size", JInt (sdSize definition)),
      ("estimatedWin", JInt (sdEstimatedWin definition)),
      ("sites", JArr (fmap synthesizedSiteJson (sdSites definition)))
    ]

synthesizedSiteJson :: SynthesizedSite -> J
synthesizedSiteJson site =
  siteJson (ssBindingName site) (candidateSiteKindKey (ssKind site)) (ssRegion site)

rejectedCandidateJson :: RejectedCandidate -> J
rejectedCandidateJson rejection =
  JObj
    [ ("sites", JArr (fmap candidateSiteLabelJson (rejSites rejection))),
      ("reason", candidateRejectionJson (rejReason rejection)),
      ("estimatedWin", JInt (rejEstimatedWin rejection)),
      ("realizedWin", maybe JNull JInt (rejRealizedWin rejection))
    ]

candidateSiteLabelJson :: CandidateSiteLabel -> J
candidateSiteLabelJson site =
  siteJson (cslBindingName site) (candidateSiteKindKey (cslKind site)) (cslRegion site)

siteJson :: String -> String -> Maybe SourceRegion -> J
siteJson bindingName kindKey sourceRegion =
  JObj
    [ ("binding", JStr bindingName),
      ("kind", JStr kindKey),
      ("region", maybe JNull sourceRegionJson sourceRegion)
    ]

sourceRegionJson :: SourceRegion -> J
sourceRegionJson (SourceRegion startLine startColumn endLine endColumn) =
  JObj
    [ ("startLine", JInt startLine),
      ("startCol", JInt startColumn),
      ("endLine", JInt endLine),
      ("endCol", JInt endColumn)
    ]

candidateRejectionJson :: CandidateRejection -> J
candidateRejectionJson rejection =
  taggedJson (candidateRejectionKey rejection) (candidateRejectionDetail rejection)

candidateRejectionDetail :: CandidateRejection -> [(String, J)]
candidateRejectionDetail = \case
  RejectedRecordOwnershipDiagnostic findings ->
    [("findings", JArr (fmap recordOwnershipFindingJson findings))]
  _ -> []

recordOwnershipFindingJson :: RecordOwnershipFinding -> J
recordOwnershipFindingJson finding =
  JObj
    [ ("constructorName", JStr (rofConstructorName finding)),
      ("derivedField", JStr (rofDerivedField finding)),
      ("projectionName", JStr (rofProjectionName finding)),
      ("ownerField", JStr (rofOwnerField finding)),
      ("ownerBinder", JStr (rofOwnerBinder finding)),
      ("kind", taggedJson (recordOwnershipKindKey (rofKind finding)) [])
    ]

stagingJson :: SynthesisOutcome -> J
stagingJson outcome =
  JObj
    [ ("localizedMerges", JInt (psrLocalizedMerges report)),
      ("globalFallbackMerges", JInt (psrGlobalFallbackMerges report)),
      ("localizedDefinitionMerges", JInt (psrLocalizedDefinitionMerges report)),
      ("localizedApplicationMerges", JInt (psrLocalizedApplicationMerges report)),
      ("globalDefinitionFallbackMerges", JInt (psrGlobalDefinitionFallbackMerges report)),
      ("globalApplicationFallbackMerges", JInt (psrGlobalApplicationFallbackMerges report)),
      ("dirtyContexts", JInt (psrDirtyContextCount report)),
      ("harvestAdvance", harvestAdvanceJson (soHarvestDecision outcome)),
      ("saturationTrace", saturationTraceImpactJson (smTraceImpact (soSaturatedModule outcome)))
    ]
  where
    report = soStagingReport outcome

saturationTraceImpactJson :: SaturationTraceImpact -> J
saturationTraceImpactJson impact =
  JObj
    [ ("touchedClassKeys", JInt (stiTouchedClassKeys impact)),
      ("touchedExplicitClassKeys", JInt (stiTouchedExplicitClassKeys impact)),
      ("touchedDefaultClassKeys", JInt (stiTouchedDefaultClassKeys impact)),
      ("dirtyContexts", JInt (stiDirtyContexts impact)),
      ("explicitDirtyContexts", JInt (stiExplicitDirtyContexts impact)),
      ("cachedContexts", JInt (stiCachedContexts impact))
    ]

harvestAdvanceJson :: Maybe HarvestAdvanceDecision -> J
harvestAdvanceJson = \case
  Nothing ->
    JObj [("decision", JStr "none")]
  Just HarvestAdvanced ->
    JObj [("decision", JStr "advanced")]
  Just (HarvestFellBack reason) ->
    JObj
      [ ("decision", JStr "fallback"),
        ("reason", taggedJson (harvestFallbackReasonKey reason) (harvestFallbackDetail reason))
      ]

harvestFallbackReasonKey :: HarvestFallbackReason -> String
harvestFallbackReasonKey = \case
  HarvestFallbackGlobalPlanMerge -> "global-plan-merge"
  HarvestFallbackDirtyRatio {} -> "dirty-ratio"
  HarvestFallbackStageSectionObstruction {} -> "stage-section-obstruction"
  HarvestFallbackSaturationSectionObstruction {} -> "saturation-section-obstruction"

harvestFallbackDetail :: HarvestFallbackReason -> [(String, J)]
harvestFallbackDetail = \case
  HarvestFallbackGlobalPlanMerge -> []
  HarvestFallbackDirtyRatio dirtyCount totalCount ratio ->
    [ ("dirtyContexts", JInt dirtyCount),
      ("totalContexts", JInt totalCount),
      ("ratio", JDouble ratio)
    ]
  HarvestFallbackStageSectionObstruction obstruction ->
    [("obstruction", JStr obstruction)]
  HarvestFallbackSaturationSectionObstruction obstruction ->
    [("obstruction", JStr obstruction)]

dispositionJson :: Map RewriteRuleId LawStamp -> (String, HunkDisposition) -> J
dispositionJson lawJoin (bindingName, disposition) =
  JObj
    [ ("binding", JStr bindingName),
      ("status", JStr (dispositionStatusKey disposition)),
      ("reason", dispositionReasonJson lawJoin disposition)
    ]

dispositionStatusKey :: HunkDisposition -> String
dispositionStatusKey = \case
  HunkSealed -> "sealed"
  HunkBlocked {} -> "blocked"

dispositionReasonJson :: Map RewriteRuleId LawStamp -> HunkDisposition -> J
dispositionReasonJson lawJoin = \case
  HunkSealed -> JNull
  HunkBlocked (BlockedWriteBack refusal) -> writeBackRefusalJson lawJoin refusal
  HunkBlocked (BlockedSeal failure) -> nebulaErrorJson failure
  HunkBlocked (BlockedCandidate rejection) -> candidateRejectionJson rejection

writeBackRefusalJson :: Map RewriteRuleId LawStamp -> WriteBackRefusal -> J
writeBackRefusalJson lawJoin refusal =
  taggedJson (writeBackRefusalKey refusal) (writeBackRefusalDetail lawJoin refusal)

writeBackRefusalDetail :: Map RewriteRuleId LawStamp -> WriteBackRefusal -> [(String, J)]
writeBackRefusalDetail lawJoin = \case
  RefusedRender _ -> []
  RefusedTypeIncompatible conflicts -> [("conflicts", JArr (fmap (stepTypeConflictJson lawJoin) conflicts))]
  RefusedDeclarationRewrite failure -> [("error", nebulaErrorJson failure)]
  RefusedProtocolRewrite failure -> [("error", nebulaErrorJson failure)]
  RefusedSourceQuality refusal -> [("refusal", sourceQualityRefusalJson refusal)]
  RefusedUnchanged -> []
  RefusedMultiName -> []
  RefusedNoRegion -> []

sourceQualityRefusalJson :: SourceQualityRefusal -> J
sourceQualityRefusalJson refusal =
  taggedJson (sourceQualityRefusalKey refusal) (sourceQualityRefusalDetail refusal)

sourceQualityRefusalDetail :: SourceQualityRefusal -> [(String, J)]
sourceQualityRefusalDetail = \case
  SourceQualityCompactBlockSyntax marker ->
    [("marker", JStr marker)]
  SourceQualityLineOnlyMinification evidence ->
    [ ("originalLines", JInt (lomOriginalLines evidence)),
      ("replacementLines", JInt (lomReplacementLines evidence)),
      ("originalBytes", JInt (lomOriginalBytes evidence)),
      ("replacementBytes", JInt (lomReplacementBytes evidence))
    ]
  SourceQualityOverlongGeneratedLine evidence -> lineQualityDetail evidence
  SourceQualityInlineListLayout evidence -> lineQualityDetail evidence
  SourceQualityInlineConsPatternLayout evidence -> lineQualityDetail evidence

lineQualityDetail :: SourceLineQualityEvidence -> [(String, J)]
lineQualityDetail evidence =
  [ ("lineLimit", JInt (slqeLineLimit evidence)),
    ("lineNumber", JInt (slqeLineNumber evidence)),
    ("originalMaxLineLength", JInt (slqeOriginalMaxLineLength evidence)),
    ("replacementLineLength", JInt (slqeReplacementLineLength evidence)),
    ("replacementLinePreview", JStr (slqeReplacementLinePreview evidence))
  ]

ledgerJson :: NebulaLedger -> J
ledgerJson ledger =
  JObj
    [ ( "realized",
        JObj
          [ ("bytesOriginal", JInt (nlOriginalBytes ledger)),
            ("bytesSealed", maybe JNull JInt (nlSealedBytes ledger)),
            ("nodesSaved", JInt (nlRealizedNodesSaved ledger))
          ]
      ),
      ("latent", JArr (fmap latentGroupJson (nlLatent ledger)))
    ]

latentGroupJson :: LatentGroup -> J
latentGroupJson group =
  JObj
    [ ("reason", JStr (lgReason group)),
      ("nodes", JInt (lgNodes group)),
      ("bindings", JInt (lgBindings group))
    ]

moduleNodeTotalsJson :: ModuleReport -> J
moduleNodeTotalsJson report =
  JObj
    [ ("originalTotal", JInt (mrOriginalTotal report)),
      ("finalTotal", JInt (mrFinalTotal report)),
      ("compressionRatio", maybe JNull JDouble (mrCompressionRatio report))
    ]

sealJson :: SealOutcome -> J
sealJson sealOutcome =
  case sealOutcome of
    SealEmpty ->
      JObj
        [ ("status", JStr "empty"),
          ("bytes", JNull),
          ("reason", JNull),
          parseCountMember
        ]
    Sealed sealedSource ->
      JObj
        [ ("status", JStr "sealed"),
          ("bytes", JInt (length (sealedSourceText sealedSource))),
          ("reason", JNull),
          parseCountMember
        ]
    SealRefused _ failure ->
      JObj
        [ ("status", JStr "refused"),
          ("bytes", JNull),
          ("reason", nebulaErrorJson failure),
          parseCountMember
        ]
  where
    parseCountMember =
      ("patchedSourceParses", JInt (sealPatchedSourceParseCount sealOutcome))

writeOutcomeJson :: WriteOutcome -> J
writeOutcomeJson outcome =
  JObj
    [ ("status", JStr (writeStatusKey (woStatus outcome))),
      ("reason", writeOutcomeReasonJson outcome)
    ]

writeOutcomeReasonJson :: WriteOutcome -> J
writeOutcomeReasonJson outcome =
  case (woReasonKey outcome, woMessage outcome) of
    (Nothing, Nothing) -> JNull
    (reasonKey, message) ->
      taggedJson
        (maybe (writeStatusKey (woStatus outcome)) id reasonKey)
        (maybe [] (\value -> [("message", JStr value)]) message)

writeOutcomeFailureJson :: WriteOutcome -> Maybe J
writeOutcomeFailureJson outcome =
  case woStatus outcome of
    WriteIoError ->
      Just
        ( failureJson
            (Just (woPath outcome))
            "io-error"
            (maybe [] (\message -> [("message", JStr message)]) (woMessage outcome))
        )
    WriteWritten -> Nothing
    WriteCandidateWritten -> Nothing
    WriteRefused -> Nothing
    WriteSkipped -> Nothing

ruleFireJson :: RuleFire -> J
ruleFireJson ruleFire =
  JObj
    [ ("rule", JInt (let RewriteRuleId ruleId = rfRuleId ruleFire in ruleId)),
      ("matched", JInt (rfMatchedTotal ruleFire)),
      ("scheduled", JInt (rfScheduledTotal ruleFire))
    ]

certificateJson :: HunkCertificate -> J
certificateJson certificate =
  JObj
    [ ("binding", JStr (hcBinding certificate)),
      ("steps", JInt (length (hcEntries certificate))),
      ("typeVerdict", typeVerdictJson (ruleLawJoin [certificate]) (hcTypeVerdict certificate)),
      ("laws", JArr (fmap (JInt . fromIntegral . lawIdKey . lsLaw) stamps)),
      ("tiers", JArr (fmap (JStr . trustTierKey . lsTier) stamps)),
      ("digest", JStr (stableDigestKey (hcDigest certificate)))
    ]
  where
    stamps =
      Set.toAscList
        ( Set.fromList
            [ lawStamp
            | entry <- hcEntries certificate,
              Just lawStamp <- [npStamp (peProvenance entry)]
            ]
        )

typeVerdictJson :: Map RewriteRuleId LawStamp -> TypeVerdict -> J
typeVerdictJson lawJoin verdict =
  taggedJson (typeVerdictKey verdict) (typeVerdictDetail lawJoin verdict)

typeVerdictDetail :: Map RewriteRuleId LawStamp -> TypeVerdict -> [(String, J)]
typeVerdictDetail lawJoin = \case
  TypeIncompatible conflicts ->
    [("conflicts", JArr (fmap (stepTypeConflictJson lawJoin) conflicts))]
  TypeCompatible -> []
  TypePolymorphic -> []
  TypeUnknown -> []

ruleLawJoin :: [HunkCertificate] -> Map RewriteRuleId LawStamp
ruleLawJoin certificates =
  Map.fromList
    [ (npRule provenance, lawStamp)
    | certificate <- certificates,
      entry <- hcEntries certificate,
      let provenance = peProvenance entry,
      Just lawStamp <- [npStamp provenance]
    ]

stepTypeConflictJson :: Map RewriteRuleId LawStamp -> StepTypeConflict -> J
stepTypeConflictJson lawJoin conflict =
  let RewriteRuleId ruleId = stcRule conflict
   in JObj
        ( [ ("rule", JInt ruleId),
            ("lhsClass", JInt (let ClassId classId = stcLhsClass conflict in classId)),
            ("rhsClass", JInt (let ClassId classId = stcRhsClass conflict in classId))
          ]
            <> foldMap lawStampMembers (Map.lookup (stcRule conflict) lawJoin)
        )

lawStampMembers :: LawStamp -> [(String, J)]
lawStampMembers stamp =
  [ ("law", JInt (fromIntegral (lawIdKey (lsLaw stamp)))),
    ("tier", JStr (trustTierKey (lsTier stamp))),
    ("fidelity", JStr (semanticFidelityKey (lsFidelity stamp)))
  ]

selfLawJson :: SelfLawRow -> J
selfLawJson row =
  JObj
    [ ("binding", JStr (slrBinding row)),
      ("outcome", either refused admitted (slrOutcome row))
    ]
  where
    refused refusal =
      taggedJson
        "refused"
        [("reason", taggedJson (selfLawRefusalKey refusal) [])]
    admitted lawValue =
      taggedJson
        "admitted"
        [("law", JInt (fromIntegral (lawIdKey lawValue)))]

gatedLawJson :: GatedLawReport -> J
gatedLawJson report =
  JObj
    [ ("law", JInt (fromIntegral (lawIdKey (glrLaw report)))),
      ("reason", lawGateReasonJson (glrReason report)),
      ("rules", JInt (glrRuleCount report))
    ]

lawGateReasonJson :: LawGateReason -> J
lawGateReasonJson reason =
  taggedJson (lawGateReasonKey reason) (lawGateReasonDetail reason)

lawGateReasonDetail :: LawGateReason -> [(String, J)]
lawGateReasonDetail = \case
  GateMissingOracleKeys keys ->
    [("keys", JArr (fmap (JStr . oracleKeyString) (Set.toAscList keys)))]
  GateOracleUnattached failure ->
    [("failure", oracleAttachFailureJson failure)]
  GateTierInadmissible tier ->
    [("tier", JStr (trustTierKey tier))]
  GateFidelityInadmissible fidelity ->
    [("fidelity", JStr (semanticFidelityKey fidelity))]

oracleAttachFailureJson :: OracleAttachFailure -> J
oracleAttachFailureJson failure =
  taggedJson (oracleAttachFailureKey failure) (oracleAttachFailureDetail failure)

oracleAttachFailureDetail :: OracleAttachFailure -> [(String, J)]
oracleAttachFailureDetail = \case
  OracleLookupMissing triedKeys ->
    [("triedKeys", JArr (fmap triedKeyJson triedKeys))]
  OracleLookupAmbiguous keyKind keyValue candidates ->
    [ ("keyKind", JStr (hieSourceKeyKindKey keyKind)),
      ("key", JStr keyValue),
      ("candidates", JArr (fmap JStr candidates))
    ]

triedKeyJson :: TriedKey -> J
triedKeyJson (TriedKey keyKind keyValue) =
  JObj
    [ ("kind", JStr (hieSourceKeyKindKey keyKind)),
      ("path", JStr keyValue)
    ]

bindingReportJson :: BindingReport -> J
bindingReportJson report =
  JObj
    [ ("name", JStr (brName report)),
      ("originalSize", JInt (brOriginalSize report)),
      ("extractedSize", JInt (brExtractedSize report)),
      ("extractionCost", JInt (brExtractionCost report))
    ]

stableDigestKey :: StableDigest128 -> String
stableDigestKey (StableDigest128 high low) =
  padWord64 high <> padWord64 low

padWord64 :: Word64 -> String
padWord64 wordValue =
  let rendered = showHex wordValue ""
   in replicate (16 - length rendered) '0' <> rendered

nebulaErrorJson :: NebulaError -> J
nebulaErrorJson failure =
  taggedJson (nebulaErrorKey failure) (nebulaErrorDetail failure)

nebulaErrorDetail :: NebulaError -> [(String, J)]
nebulaErrorDetail failure =
  case failure of
    NebulaExtractionError subject _ ->
      [("subject", JStr subject)] <> messageDetail
    NebulaArityMismatch nameCount contextCount seedCount ->
      [ ("nameCount", JInt nameCount),
        ("contextCount", JInt contextCount),
        ("seedCount", JInt seedCount)
      ]
    NebulaSealError subject _ ->
      [("subject", JStr subject)] <> messageDetail
    _ ->
      messageDetail
  where
    messageDetail =
      maybe [] (\message -> [("message", JStr message)]) (nebulaErrorMessage failure)

nebulaFailureJson :: Maybe FilePath -> NebulaError -> J
nebulaFailureJson suppliedPath failure =
  failureJson
    (case suppliedPath of Just path -> Just path; Nothing -> nebulaErrorPath failure)
    (nebulaErrorKey failure)
    (nebulaErrorDetail failure)

failureJson :: Maybe FilePath -> String -> [(String, J)] -> J
failureJson path kind details =
  JObj
    [ ("path", maybe JNull JStr path),
      ("error", taggedJson kind details)
    ]

taggedJson :: String -> [(String, J)] -> J
taggedJson kind details =
  JObj
    [ ("kind", JStr kind),
      ("detail", JObj details)
    ]

parseBaseline :: String -> Either BaselineFailure J
parseBaseline input = do
  value <- either (Left . BaselineParse) Right (parseJson input)
  _ <- decodeCorpusBaseline value
  Right value

corpusEnvelopeJson :: WorkspaceReport -> Maybe (Either BaselineFailure J) -> J
corpusEnvelopeJson report baselineInput =
  envelopeJson
    "corpus"
    (workspaceSummaryJson report baselineFailureCount)
    (fmap latentGroupJson (workspaceLatent report))
    (fmap corpusModuleJson (wrModules report))
    failures
    ([ ("totals", corpusTotalsJson report) ] <> comparisonMember)
  where
    decodedBaseline = baselineInput >>= either (Just . Left) (Just . decodeCorpusBaseline)
    baselineFailures =
      case decodedBaseline of
        Just (Left failure) -> [baselineFailureJson failure]
        _ -> []
    baselineFailureCount = length baselineFailures
    failures =
      fmap (uncurry (nebulaFailureJson . Just)) (wrModuleFailures report)
        <> fmap (nebulaFailureJson Nothing) (wrWorkspaceErrors report)
        <> baselineFailures
    comparisonMember =
      case decodedBaseline of
        Just (Right baseline) -> [("comparison", corpusComparisonJson baseline report)]
        _ -> []

corpusModuleJson :: ModuleReport -> J
corpusModuleJson report =
  JObj
    [ ("path", JStr (mrPath report)),
      ("originalBytes", JInt (mrOriginalBytes report)),
      ("sealedBytes", maybe JNull JInt (mrSealedBytes report)),
      ("realized", JInt (nlRealizedNodesSaved ledger)),
      ("latentByReason", latentByReasonJson (nlLatent ledger))
    ]
  where
    ledger = moduleLedger report

corpusTotalsJson :: WorkspaceReport -> J
corpusTotalsJson report =
  JObj
    [ ("originalBytes", JInt (sum (fmap mrOriginalBytes modules))),
      ("sealedBytes", JInt (sum (fmap (maybe 0 id . mrSealedBytes) modules))),
      ("realized", JInt (sum (fmap (nlRealizedNodesSaved . moduleLedger) modules))),
      ("latentByReason", latentByReasonJson (workspaceLatent report))
    ]
  where
    modules = wrModules report

latentByReasonJson :: [LatentGroup] -> J
latentByReasonJson groups =
  JObj [(lgReason group, JInt (lgNodes group)) | group <- groups]

baselineFailureJson :: BaselineFailure -> J
baselineFailureJson failure =
  failureJson Nothing (baselineFailureKey failure) (baselineFailureDetail failure)

baselineFailureDetail :: BaselineFailure -> [(String, J)]
baselineFailureDetail = \case
  BaselineUnreadable message -> [("message", JStr message)]
  BaselineParse message -> [("message", JStr message)]
  BaselineSchemaVersion actual ->
    [ ("expected", JInt schemaVersion),
      ("actual", JInt actual)
    ]

decodeCorpusBaseline :: J -> Either BaselineFailure CorpusBaseline
decodeCorpusBaseline value = do
  members <- expectObject "$" value
  baselineVersion <- requireIntMember "$" "schemaVersion" members
  if baselineVersion == schemaVersion
    then Right ()
    else Left (BaselineSchemaVersion baselineVersion)
  tool <- requireStringMember "$" "tool" members
  requireValue "$.tool" "melusine-nebula" tool
  mode <- requireStringMember "$" "mode" members
  requireValue "$.mode" "corpus" mode
  requireMember "$" "summary" members >>= validateSummary "$.summary"
  requireMember "$" "latent" members >>= validateLatentRows "$.latent"
  requireMember "$" "failures" members >>= validateFailures "$.failures"
  requireMember "$" "totals" members >>= validateCorpusTotals "$.totals"
  moduleValues <- requireMember "$" "modules" members >>= expectArray "$.modules"
  modules <- traverse (uncurry decodeBaselineModule) (zip [0 :: Int ..] moduleValues)
  case firstDuplicate (fmap fst modules) of
    Just duplicatePath ->
      Left (BaselineParse ("$.modules: duplicate path " <> duplicatePath))
    Nothing ->
      Right (CorpusBaseline (Map.fromList modules))

decodeBaselineModule :: Int -> J -> Either BaselineFailure (FilePath, BaselineModule)
decodeBaselineModule index value = do
  let path = "$.modules[" <> show index <> "]"
  members <- expectObject path value
  modulePath <- requireStringMember path "path" members
  _ <- requireIntMember path "originalBytes" members
  sealedBytes <- requireMember path "sealedBytes" members
  validateNullableInt (path <> ".sealedBytes") sealedBytes
  realized <- requireIntMember path "realized" members
  latent <- requireMember path "latentByReason" members >>= decodeIntObject (path <> ".latentByReason")
  Right (modulePath, BaselineModule realized latent)

validateSummary :: String -> J -> Either BaselineFailure ()
validateSummary path value = do
  members <- expectObject path value
  status <- requireStringMember path "status" members
  if status `elem` ["clean", "degraded", "empty"]
    then Right ()
    else parseFailure (path <> ".status") ("unexpected value " <> status)
  _ <- requireIntMember path "modules" members
  _ <- requireIntMember path "sealed" members
  _ <- requireIntMember path "failed" members
  bytes <- requireMember path "bytes" members >>= expectObject (path <> ".bytes")
  _ <- requireIntMember (path <> ".bytes") "original" bytes
  _ <- requireIntMember (path <> ".bytes") "sealed" bytes
  nodeTotals <- requireMember path "nodeTotals" members >>= expectObject (path <> ".nodeTotals")
  _ <- requireIntMember (path <> ".nodeTotals") "originalTotal" nodeTotals
  _ <- requireIntMember (path <> ".nodeTotals") "finalTotal" nodeTotals
  requireMember (path <> ".nodeTotals") "compressionRatio" nodeTotals
    >>= validateNullableNumber (path <> ".nodeTotals.compressionRatio")
  _ <- requireIntMember path "nodesSaved" members
  Right ()

validateLatentRows :: String -> J -> Either BaselineFailure ()
validateLatentRows path value = do
  rows <- expectArray path value
  traverse_Indexed validateLatentRow path rows

validateLatentRow :: String -> J -> Either BaselineFailure ()
validateLatentRow path value = do
  members <- expectObject path value
  _ <- requireStringMember path "reason" members
  _ <- requireIntMember path "nodes" members
  _ <- requireIntMember path "bindings" members
  Right ()

validateFailures :: String -> J -> Either BaselineFailure ()
validateFailures path value = do
  rows <- expectArray path value
  traverse_Indexed validateFailure path rows

validateFailure :: String -> J -> Either BaselineFailure ()
validateFailure path value = do
  members <- expectObject path value
  requireMember path "path" members >>= validateNullableString (path <> ".path")
  errorMembers <- requireMember path "error" members >>= expectObject (path <> ".error")
  _ <- requireStringMember (path <> ".error") "kind" errorMembers
  _ <- requireMember (path <> ".error") "detail" errorMembers >>= expectObject (path <> ".error.detail")
  Right ()

validateCorpusTotals :: String -> J -> Either BaselineFailure ()
validateCorpusTotals path value = do
  members <- expectObject path value
  _ <- requireIntMember path "originalBytes" members
  _ <- requireIntMember path "sealedBytes" members
  _ <- requireIntMember path "realized" members
  _ <- requireMember path "latentByReason" members >>= decodeIntObject (path <> ".latentByReason")
  Right ()

traverse_Indexed :: (String -> J -> Either BaselineFailure ()) -> String -> [J] -> Either BaselineFailure ()
traverse_Indexed validate path values =
  foldr
    (\(index, value) rest -> validate (path <> "[" <> show index <> "]") value >> rest)
    (Right ())
    (zip [0 :: Int ..] values)

expectObject :: String -> J -> Either BaselineFailure [(String, J)]
expectObject path = \case
  JObj members -> Right members
  value -> wrongType path "object" value

expectArray :: String -> J -> Either BaselineFailure [J]
expectArray path = \case
  JArr values -> Right values
  value -> wrongType path "array" value

requireMember :: String -> String -> [(String, J)] -> Either BaselineFailure J
requireMember path key members =
  case [value | (memberKey, value) <- members, memberKey == key] of
    [] -> parseFailure (path <> "." <> key) "missing member"
    [value] -> Right value
    _ -> parseFailure (path <> "." <> key) "duplicate member"

requireIntMember :: String -> String -> [(String, J)] -> Either BaselineFailure Int
requireIntMember path key members =
  requireMember path key members >>= expectInt (path <> "." <> key)

requireStringMember :: String -> String -> [(String, J)] -> Either BaselineFailure String
requireStringMember path key members =
  requireMember path key members >>= expectString (path <> "." <> key)

expectInt :: String -> J -> Either BaselineFailure Int
expectInt path = \case
  JInt value -> Right value
  value -> wrongType path "integer" value

expectString :: String -> J -> Either BaselineFailure String
expectString path = \case
  JStr value -> Right value
  value -> wrongType path "string" value

validateNullableInt :: String -> J -> Either BaselineFailure ()
validateNullableInt _ JNull = Right ()
validateNullableInt _ (JInt _) = Right ()
validateNullableInt path value = wrongType path "integer or null" value

validateNullableNumber :: String -> J -> Either BaselineFailure ()
validateNullableNumber _ JNull = Right ()
validateNullableNumber _ (JInt _) = Right ()
validateNullableNumber _ (JDouble _) = Right ()
validateNullableNumber path value = wrongType path "number or null" value

validateNullableString :: String -> J -> Either BaselineFailure ()
validateNullableString _ JNull = Right ()
validateNullableString _ (JStr _) = Right ()
validateNullableString path value = wrongType path "string or null" value

decodeIntObject :: String -> J -> Either BaselineFailure (Map String Int)
decodeIntObject path value = do
  members <- expectObject path value
  case firstDuplicate (fmap fst members) of
    Just duplicateKey -> parseFailure (path <> "." <> duplicateKey) "duplicate member"
    Nothing -> Map.fromList <$> traverse decodeMember members
  where
    decodeMember (key, memberValue) =
      (,) key <$> expectInt (path <> "." <> key) memberValue

requireValue :: String -> String -> String -> Either BaselineFailure ()
requireValue path expected actual
  | actual == expected = Right ()
  | otherwise = parseFailure path ("expected " <> expected <> ", found " <> actual)

wrongType :: String -> String -> J -> Either BaselineFailure a
wrongType path expected value =
  parseFailure path ("expected " <> expected <> ", found " <> jsonTypeName value)

jsonTypeName :: J -> String
jsonTypeName = \case
  JObj {} -> "object"
  JArr {} -> "array"
  JStr {} -> "string"
  JInt {} -> "integer"
  JDouble {} -> "number"
  JBool {} -> "boolean"
  JNull -> "null"

parseFailure :: String -> String -> Either BaselineFailure a
parseFailure path message =
  Left (BaselineParse (path <> ": " <> message))

firstDuplicate :: Ord a => [a] -> Maybe a
firstDuplicate =
  duplicateWithin Set.empty

duplicateWithin :: Ord a => Set a -> [a] -> Maybe a
duplicateWithin _ [] = Nothing
duplicateWithin seen (value : rest)
  | Set.member value seen = Just value
  | otherwise = duplicateWithin (Set.insert value seen) rest

corpusComparisonJson :: CorpusBaseline -> WorkspaceReport -> J
corpusComparisonJson (CorpusBaseline baselineModules) report =
  JArr
    ( mapMaybe
        comparisonForPath
        (Set.toAscList (Map.keysSet baselineModules `Set.union` Map.keysSet currentModules))
    )
  where
    currentModules =
      Map.fromList
        [ (mrPath moduleReportValue, baselineModuleFromReport moduleReportValue)
        | moduleReportValue <- wrModules report
        ]
    failedPaths =
      Set.fromList
        ( fmap fst (wrModuleFailures report)
            <> mapMaybe nebulaErrorPath (wrWorkspaceErrors report)
        )
    comparisonForPath path =
      case (Map.lookup path baselineModules, Map.lookup path currentModules) of
        (Nothing, Just current) -> Just (newComparisonJson path current)
        (Just baseline, Just current) -> Just (knownComparisonJson path baseline current)
        (Just _, Nothing)
          | Set.member path failedPaths -> Just (statusComparisonJson path "failed")
          | otherwise -> Just (statusComparisonJson path "removed")
        (Nothing, Nothing) -> Nothing

baselineModuleFromReport :: ModuleReport -> BaselineModule
baselineModuleFromReport report =
  BaselineModule
    { bmRealized = nlRealizedNodesSaved ledger,
      bmLatent = Map.fromList [(lgReason group, lgNodes group) | group <- nlLatent ledger]
    }
  where
    ledger = moduleLedger report

statusComparisonJson :: FilePath -> String -> J
statusComparisonJson path status =
  JObj
    [ ("path", JStr path),
      ("status", JStr status)
    ]

newComparisonJson :: FilePath -> BaselineModule -> J
newComparisonJson path current =
  JObj
    [ ("path", JStr path),
      ("status", JStr "new"),
      ("realized", realizedDeltaJson 0 (bmRealized current)),
      ("latent", latentDeltasJson Map.empty (bmLatent current))
    ]

knownComparisonJson :: FilePath -> BaselineModule -> BaselineModule -> J
knownComparisonJson path baseline current =
  JObj
    [ ("path", JStr path),
      ("status", JStr "known"),
      ("realized", realizedDeltaJson (bmRealized baseline) (bmRealized current)),
      ("latent", latentDeltasJson (bmLatent baseline) (bmLatent current))
    ]

realizedDeltaJson :: Int -> Int -> J
realizedDeltaJson baseline current =
  JObj
    [ ("baseline", JInt baseline),
      ("current", JInt current),
      ("delta", JInt (current - baseline))
    ]

latentDeltasJson :: Map String Int -> Map String Int -> J
latentDeltasJson baseline current =
  JArr
    [ JObj
        [ ("reason", JStr reason),
          ("baseline", JInt baselineValue),
          ("current", JInt currentValue),
          ("delta", JInt (currentValue - baselineValue))
        ]
    | reason <- Set.toAscList (Map.keysSet baseline `Set.union` Map.keysSet current),
      let baselineValue = Map.findWithDefault 0 reason baseline,
      let currentValue = Map.findWithDefault 0 reason current
    ]
