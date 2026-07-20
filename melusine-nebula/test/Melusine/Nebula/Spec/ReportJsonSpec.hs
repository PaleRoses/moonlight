module Melusine.Nebula.Spec.ReportJsonSpec (spec) where

import Control.Monad (foldM)
import Data.List (isInfixOf, sort, stripPrefix)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Melusine.Nebula
  ( DiagnoseRegion (..),
    ModuleImprovement (..),
    ModuleReport (..),
    ModuleWorkload (..),
    defaultNebulaConfig,
    diagnoseEnvelopeJson,
    improveModule,
    renderDiagnoseRegions,
    sealPatchedSourceParseCount,
  )
import Melusine.Nebula.Core
  ( NebulaError (..),
    nebulaErrorKey,
  )
import Melusine.Nebula.Discovery.Choose
  ( CandidateSiteKind (..),
    candidateSiteKindKey,
  )
import Melusine.Nebula.Proof.Audit
  ( StepTypeConflict (..),
    TypeVerdict (..),
  )
import Melusine.Nebula.Proof.Certificate
  ( HunkCertificate (..),
    NebulaProvenance (..),
    ProvenanceEntry (..),
  )
import Melusine.Nebula.Report.Json
  ( DetailTier (..),
    J (..),
    parseJson,
    renderJson,
    workspaceEnvelopeJson,
  )
import Melusine.Nebula.Report.Text
  ( HunkBlockReason (..),
    HunkDisposition (..),
    OracleProvenance (..),
    blockReasonKey,
    renderWorkspaceReport,
    workspaceReport,
  )
import Melusine.Nebula.Rewrite.Corpus
  ( GatedLawReport (..),
    LawGateReason (..),
    LawStamp (..),
    lawGateReasonKey,
  )
import Melusine.Nebula.Rewrite.Saturate (RuleFire (..))
import Melusine.Nebula.Synthesis.Types
  ( CandidateRejection (..),
    CandidateSiteLabel (..),
    RecordOwnershipFinding (..),
    RecordOwnershipKind (..),
    RejectedCandidate (..),
    SynthesisOutcome (..),
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
import Moonlight.Core (RewriteRuleId (..))
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( HsOpaqueTag (..),
    HsPatOpaqueTag (..),
    SelfLawRefusal (..),
    SelfLawRow (..),
  )
import Moonlight.EGraph.Pure.Types (ClassId (..))
import Moonlight.Flow.Model.Schema.Digest (stableDigest128)
import Moonlight.Rewrite.System
  ( SemanticFidelity (..),
    TrustTier (..),
    mkLawId,
    mkOracleKey,
  )
import Moonlight.Saturation.Core (SaturationTermination (..))
import Moonlight.Pale.Ghc.Expr
  ( RenderRefusal (..),
    ScopeCtx (..),
    rootScopeId,
  )
import Moonlight.Pale.Ghc.Hie.SourceKey
  ( HieSourceKeyKind (..),
    OracleAttachFailure (..),
    OracleLookup (..),
    TriedKey (..),
  )
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

spec :: TestTree
spec =
  withResource requireBaseModuleReport (const (pure ())) $ \getBaseReport ->
    testGroup
      "nebula.reportjson"
      [ escapingCases,
        roundTripCases,
        envelopeShapeCases getBaseReport,
        vocabularyCases,
        constructorEncodingCases getBaseReport
      ]

escapingCases :: TestTree
escapingCases =
  testGroup
    "nebula.reportjson.escaping"
    [ testCase "the renderer escapes quotes, backslashes, and control characters" $ do
        let nastyString = "a\"b\\c\nd\te\rf\x01g"
            rendered = renderJson (JStr nastyString)
        assertBool "the quote is escaped" ("\\\"" `isInfixOf` rendered)
        assertBool "the backslash is escaped" ("\\\\" `isInfixOf` rendered)
        assertBool "the newline becomes a shorthand escape" ("\\n" `isInfixOf` rendered)
        assertBool "the tab becomes a shorthand escape" ("\\t" `isInfixOf` rendered)
        assertBool "the carriage return becomes a shorthand escape" ("\\r" `isInfixOf` rendered)
        assertBool "the control byte becomes a unicode escape" ("\\u0001" `isInfixOf` rendered)
        assertBool "no raw control byte survives" (not ('\x01' `elem` rendered))
    ]

roundTripCases :: TestTree
roundTripCases =
  testGroup
    "nebula.reportjson.roundtrip"
    [ testCase "parsing the rendered value reproduces the structure" $ do
        let representative =
              JObj
                [ ("path", JStr "Some/Module.hs"),
                  ("nasty", JStr "line\nbreak\ttab\"quote\\slash"),
                  ("count", JInt 42),
                  ("ratio", JDouble 0.5),
                  ("flag", JBool True),
                  ("missing", JNull),
                  ( "latent",
                    JArr
                      [ JObj [("reason", JStr "render:lambda-match-group"), ("nodes", JInt 3), ("bindings", JInt 1)]
                      ]
                  )
                ]
        parseJson (renderJson representative) @?= Right representative
    ]

envelopeShapeCases :: IO ModuleReport -> TestTree
envelopeShapeCases getBaseReport =
  testGroup
    "nebula.reportjson.envelope"
    [ testCase "the report envelope pins schema version one" $ do
        let envelope = workspaceEnvelopeJson TierStandard (workspaceReport [] [] []) Nothing
        requireJsonAt [Key "schemaVersion"] envelope >>= (@?= JInt 1),
      testCase "the empty report still exposes the complete envelope authority" $ do
        let envelope = workspaceEnvelopeJson TierStandard (workspaceReport [] [] []) Nothing
        requireJsonAt [Key "tool"] envelope >>= (@?= JStr "melusine-nebula")
        requireJsonAt [Key "mode"] envelope >>= (@?= JStr "report")
        requireJsonAt [Key "summary", Key "status"] envelope >>= (@?= JStr "empty")
        assertRequiredKeys ["schemaVersion", "tool", "mode", "summary", "latent", "modules", "failures"] envelope
        assertAbsentKey "realized" envelope
        assertAbsentKey "diagnostics" envelope,
      testCase "summary modules are deliberately one-line projections" $ do
        baseReport <- getBaseReport
        let envelope = workspaceEnvelopeJson TierSummary (workspaceReport [baseReport] [] []) Nothing
        moduleValue <- requireJsonAt [Key "modules", Index 0] envelope
        objectKeys moduleValue @?= Right (sort ["path", "sealStatus", "nodesSaved", "blockedCount"]),
      testCase "standard modules expose semantic structure rather than the legacy diagnostics subset" $ do
        baseReport <- getBaseReport
        let envelope = workspaceEnvelopeJson TierStandard (workspaceReport [baseReport] [] []) Nothing
        moduleValue <- requireJsonAt [Key "modules", Index 0] envelope
        assertRequiredKeys standardModuleKeys moduleValue
        assertAbsentKey "diagnostics" moduleValue
        requireJsonAt [Key "seal", Key "patchedSourceParses"] moduleValue
          >>= (@?= JInt (sealPatchedSourceParseCount (mrSeal baseReport))),
      testCase "saturation lifecycle is serialized from the final carrier" $ do
        baseReport <- getBaseReport
        let envelope = workspaceEnvelopeJson TierStandard (workspaceReport [baseReport] [] []) Nothing
            rendered = renderWorkspaceReport (workspaceReport [baseReport] [] [])
            lifecyclePath = [Key "modules", Index 0, Key "saturation", Key "lifecycle"]
        requireJsonAt (lifecyclePath <> [Key "planPreparations"]) envelope >>= (@?= JInt 1)
        requireJsonAt (lifecyclePath <> [Key "freshRuns"]) envelope >>= (@?= JInt 1)
        requireJsonAt (lifecyclePath <> [Key "resumptions"]) envelope >>= (@?= JInt 0)
        assertTextFieldValue "plan-preparations" "1" rendered
        assertTextFieldValue "fresh-runs" "1" rendered
        assertTextFieldValue "resumptions" "0" rendered,
      testCase "full modules add every heavy section" $ do
        baseReport <- getBaseReport
        let envelope = workspaceEnvelopeJson TierFull (workspaceReport [baseReport] [] []) Nothing
        moduleValue <- requireJsonAt [Key "modules", Index 0] envelope
        assertRequiredKeys fullModuleKeys moduleValue
    ]

standardModuleKeys :: [String]
standardModuleKeys =
  [ "path",
    "oracle",
    "saturation",
    "sites",
    "vocabulary",
    "evidenceFacts",
    "numTypeFacts",
    "typeEvidence",
    "bindingFront",
    "antiUnifyPairBound",
    "certificateCount",
    "synthesis",
    "dispositions",
    "ledger",
    "nodeTotals",
    "seal"
  ]

fullModuleKeys :: [String]
fullModuleKeys =
  standardModuleKeys
    <> [ "ruleFires",
         "certificates",
         "selfLaws",
         "gatedLaws",
         "bindings",
         "diff"
       ]

data VocabularyCase value = VocabularyCase
  { vocabularyValue :: !value,
    vocabularyKey :: !String,
    vocabularyDetailKeys :: ![String]
  }

vocabularyCases :: TestTree
vocabularyCases =
  testGroup
    "nebula.reportjson.vocabulary"
    [ vocabularyTestGroup "candidate-rejection" candidateRejectionKey candidateRejectionCases,
      vocabularyTestGroup "record-ownership-kind" recordOwnershipKindKey recordOwnershipKindCases,
      vocabularyTestGroup "nebula-error" nebulaErrorKey nebulaErrorCases,
      vocabularyTestGroup "law-gate-reason" lawGateReasonKey lawGateReasonCases,
      vocabularyTestGroup "write-back-refusal" writeBackRefusalKey writeBackRefusalCases,
      vocabularyTestGroup "render-refusal" (writeBackRefusalKey . RefusedRender) renderRefusalCases,
      vocabularyTestGroup "source-quality-refusal" sourceQualityRefusalKey sourceQualityRefusalCases,
      vocabularyTestGroup "write-status" writeStatusKey writeStatusCases,
      vocabularyTestGroup "candidate-site-kind" candidateSiteKindKey candidateSiteKindCases,
      boundedCoverageCases
    ]

boundedCoverageCases :: TestTree
boundedCoverageCases =
  testGroup
    "constructor-coverage"
    [ boundedCoverageTest "opaque-tags" (fmap fst opaqueTagKeys),
      boundedCoverageTest "pattern-opaque-tags" (fmap fst patOpaqueTagKeys),
      boundedCoverageTest "trust-tiers" (fmap fst trustTierCases),
      boundedCoverageTest "semantic-fidelities" (fmap fst semanticFidelityCases),
      boundedCoverageTest "self-law-refusals" (fmap vocabularyValue selfLawRefusalCases),
      boundedCoverageTest "HIE source-key kinds" (fmap fst hieSourceKeyCases)
    ]

boundedCoverageTest :: (Bounded value, Enum value, Eq value, Show value) => String -> [value] -> TestTree
boundedCoverageTest vocabularyName expectedValues =
  testCase vocabularyName $
    [minBound .. maxBound] @?= expectedValues

vocabularyTestGroup :: String -> (value -> String) -> [VocabularyCase value] -> TestTree
vocabularyTestGroup vocabularyName keyFunction cases =
  testGroup
    vocabularyName
    ( goldenVocabularyTest keyFunction cases
        : fmap (constructorKeyTest keyFunction) cases
    )

goldenVocabularyTest :: (value -> String) -> [VocabularyCase value] -> TestTree
goldenVocabularyTest keyFunction cases =
  testCase "golden key list" $
    fmap (keyFunction . vocabularyValue) cases
      @?= fmap vocabularyKey cases

constructorKeyTest :: (value -> String) -> VocabularyCase value -> TestTree
constructorKeyTest keyFunction vocabularyCase =
  testCase (vocabularyKey vocabularyCase) $
    keyFunction (vocabularyValue vocabularyCase)
      @?= vocabularyKey vocabularyCase

candidateRejectionCases :: [VocabularyCase CandidateRejection]
candidateRejectionCases =
  [ VocabularyCase RejectedNoEstimatedWin "no-estimated-win" [],
    VocabularyCase RejectedNoDistinctArgs "no-distinct-args" [],
    VocabularyCase RejectedNotVisible "not-visible" [],
    VocabularyCase RejectedOverlap "overlap" [],
    VocabularyCase RejectedRegionOverlap "region-overlap" [],
    VocabularyCase RejectedScopeEscape "scope-escape" [],
    VocabularyCase RejectedTypeEvidenceInsufficient "type-evidence-insufficient" [],
    VocabularyCase RejectedEffectOrderUnknown "effect-order-unknown" [],
    VocabularyCase RejectedCaseOrderUnsafe "case-order-unsafe" [],
    VocabularyCase RejectedTreeEditDiagnostic "tree-edit-diagnostic" [],
    VocabularyCase RejectedProjectionVectorDiagnostic "projection-vector-diagnostic" [],
    VocabularyCase RejectedFoldSkeletonDiagnostic "fold-skeleton-diagnostic" [],
    VocabularyCase RejectedLetRowsProtocolDiagnostic "let-rows-protocol-diagnostic" [],
    VocabularyCase RejectedPatternBindRhsProtocolDiagnostic "pattern-bind-rhs-protocol-diagnostic" [],
    VocabularyCase RejectedKeyedRowAlignmentProtocolDiagnostic "keyed-row-alignment-protocol-diagnostic" [],
    VocabularyCase RejectedArityChildUnifierProtocolDiagnostic "arity-child-unifier-protocol-diagnostic" [],
    VocabularyCase (RejectedRecordOwnershipDiagnostic [recordOwnershipFinding]) "record-ownership-diagnostic" ["findings"],
    VocabularyCase RejectedRecordConstructionSkeletonDiagnostic "record-construction-skeleton-diagnostic" [],
    VocabularyCase RejectedRedundantPatternClassCanonicalizationDiagnostic "redundant-pattern-class-canonicalization-diagnostic" [],
    VocabularyCase RejectedScopedRegionExtractionProtocolDiagnostic "scoped-region-extraction-protocol-diagnostic" [],
    VocabularyCase RejectedFiniteValidationDiagnostic "finite-validation-diagnostic" [],
    VocabularyCase RejectedThresholdRefinementDiagnostic "threshold-refinement-diagnostic" [],
    VocabularyCase RejectedEitherValidationDiagnostic "either-validation-diagnostic" [],
    VocabularyCase RejectedOracleMissing "oracle-missing" [],
    VocabularyCase RejectedRealizedRegression "realized-regression" []
  ]

recordOwnershipFinding :: RecordOwnershipFinding
recordOwnershipFinding =
  RecordOwnershipFinding
    { rofConstructorName = "Fixture",
      rofDerivedField = "cached",
      rofProjectionName = "deriveCached",
      rofOwnerField = "owner",
      rofOwnerBinder = "ownerValue",
      rofKind = ProjectionOwnedCachedField
    }

recordOwnershipKindCases :: [VocabularyCase RecordOwnershipKind]
recordOwnershipKindCases =
  [ VocabularyCase ProjectionOwnedCachedField "projection-owned-cached-field" [],
    VocabularyCase StaleDerivedField "stale-derived-field" []
  ]

nebulaErrorCases :: [VocabularyCase NebulaError]
nebulaErrorCases =
  [ VocabularyCase (NebulaWorkspaceError "root" "message") "workspace-error" ["message"],
    VocabularyCase (NebulaParseError "message") "parse-error" ["message"],
    VocabularyCase (NebulaLatticeError "message") "lattice-error" ["message"],
    VocabularyCase (NebulaInsertionError "message") "insertion-error" ["message"],
    VocabularyCase (NebulaRuleDerivationError "message") "rule-derivation-error" ["message"],
    VocabularyCase (NebulaBindingFrontError "message") "binding-front-error" ["message"],
    VocabularyCase (NebulaSaturationError "message") "saturation-error" ["message"],
    VocabularyCase (NebulaExtractionError "binding" "message") "extraction-error" ["subject", "message"],
    VocabularyCase (NebulaSynthesisError "message") "synthesis-error" ["message"],
    VocabularyCase (NebulaArityMismatch 1 2 3) "arity-mismatch" ["nameCount", "contextCount", "seedCount"],
    VocabularyCase (NebulaWriteBackError "message") "write-back-error" ["message"],
    VocabularyCase (NebulaSpliceError "message") "splice-error" ["message"],
    VocabularyCase (NebulaSealError "subject" "message") "seal-error" ["subject", "message"]
  ]

lawGateReasonCases :: [VocabularyCase LawGateReason]
lawGateReasonCases =
  [ VocabularyCase (GateMissingOracleKeys (Set.singleton (mkOracleKey "map"))) "missing-oracle-keys" ["keys"],
    VocabularyCase (GateOracleUnattached (OracleLookupMissing [TriedKey GivenPathKey "Fixture.hs"])) "oracle-unattached" ["failure"],
    VocabularyCase (GateTierInadmissible ParserVerified) "tier-inadmissible" ["tier"],
    VocabularyCase (GateFidelityInadmissible Observational) "fidelity-inadmissible" ["fidelity"]
  ]

writeBackRefusalCases :: [VocabularyCase WriteBackRefusal]
writeBackRefusalCases =
  [ VocabularyCase RefusedUnchanged "unchanged" [],
    VocabularyCase (RefusedRender RenderGuardedExpression) "render:guarded-expression" [],
    VocabularyCase RefusedMultiName "multi-name" [],
    VocabularyCase RefusedNoRegion "no-region" [],
    VocabularyCase (RefusedTypeIncompatible [stepTypeConflict]) "type-incompatible" ["conflicts"],
    VocabularyCase (RefusedDeclarationRewrite (NebulaSpliceError "message")) "declaration-rewrite-refused" ["error"],
    VocabularyCase (RefusedProtocolRewrite (NebulaSpliceError "message")) "protocol-rewrite-refused" ["error"],
    VocabularyCase (RefusedSourceQuality (SourceQualityCompactBlockSyntax "do {")) "source-quality-refused" ["refusal"]
  ]

stepTypeConflict :: StepTypeConflict
stepTypeConflict =
  StepTypeConflict
    { stcRule = RewriteRuleId 7,
      stcLhsClass = ClassId 11,
      stcRhsClass = ClassId 13
    }

renderRefusalCases :: [VocabularyCase RenderRefusal]
renderRefusalCases =
  fmap
    (\(opaqueTag, keyValue) -> VocabularyCase (RenderOpaque opaqueTag) ("render:" <> keyValue) [])
    opaqueTagKeys
    <> [ VocabularyCase RenderGuardedExpression "render:guarded-expression" [],
         VocabularyCase RenderWhereExpression "render:where-expression" [],
         VocabularyCase RenderPatternVariable "render:pattern-variable" [],
         VocabularyCase RenderNonVarOperator "render:non-var-operator" []
       ]
    <> fmap
      (\(opaqueTag, keyValue) -> VocabularyCase (RenderPatOpaque opaqueTag) ("render:" <> keyValue) [])
      patOpaqueTagKeys
    <> [ VocabularyCase RenderEmptyBindingName "render:empty-binding-name" [],
         VocabularyCase RenderClausesShape "render:clauses-shape" []
       ]

opaqueTagKeys :: [(HsOpaqueTag, String)]
opaqueTagKeys =
  [ (OpaqueOverLabel, "opaque-over-label"),
    (OpaqueIPVar, "opaque-ip-var"),
    (OpaqueAppType, "opaque-app-type"),
    (OpaqueExplicitSum, "opaque-explicit-sum"),
    (OpaqueMultiIf, "opaque-multi-if"),
    (OpaqueRecordUpd, "opaque-record-update"),
    (OpaqueGetField, "opaque-get-field"),
    (OpaqueProjection, "opaque-projection"),
    (OpaqueExprWithTySig, "opaque-expression-with-type-signature"),
    (OpaqueArithSeq, "opaque-arithmetic-sequence"),
    (OpaqueTypedBracket, "opaque-typed-bracket"),
    (OpaqueUntypedBracket, "opaque-untyped-bracket"),
    (OpaqueTypedSplice, "opaque-typed-splice"),
    (OpaqueUntypedSplice, "opaque-untyped-splice"),
    (OpaqueProc, "opaque-proc"),
    (OpaqueStatic, "opaque-static"),
    (OpaquePragE, "opaque-expression-pragma"),
    (OpaqueEmbTy, "opaque-embedded-type"),
    (OpaqueHole, "opaque-hole"),
    (OpaqueForAll, "opaque-for-all"),
    (OpaqueQual, "opaque-qualified-type"),
    (OpaqueFunArr, "opaque-function-arrow"),
    (OpaqueXExpr, "opaque-expression-extension"),
    (OpaqueLambdaMatchGroup, "opaque-lambda-match-group"),
    (OpaqueCaseAlternative, "opaque-case-alternative"),
    (OpaqueLocalIPBinds, "opaque-local-implicit-parameter-bindings"),
    (OpaqueXLocalBinds, "opaque-local-bindings-extension"),
    (OpaqueValBindsExtension, "opaque-value-bindings-extension"),
    (OpaqueUnsupportedBind, "opaque-unsupported-binding"),
    (OpaqueUnsupportedStmt, "opaque-unsupported-statement"),
    (OpaqueUnsupportedGuard, "opaque-unsupported-guard"),
    (OpaqueMissingGuardFallback, "opaque-missing-guard-fallback"),
    (OpaqueUnsupportedRecordField, "opaque-unsupported-record-field")
  ]

patOpaqueTagKeys :: [(HsPatOpaqueTag, String)]
patOpaqueTagKeys =
  [ (PatOpaqueOr, "pattern-opaque-or"),
    (PatOpaqueSum, "pattern-opaque-sum"),
    (PatOpaqueView, "pattern-opaque-view"),
    (PatOpaqueSplice, "pattern-opaque-splice"),
    (PatOpaqueNPlusK, "pattern-opaque-n-plus-k"),
    (PatOpaqueSig, "pattern-opaque-signature"),
    (PatOpaqueEmbTy, "pattern-opaque-embedded-type"),
    (PatOpaqueInvis, "pattern-opaque-invisible"),
    (PatOpaqueRecCon, "pattern-opaque-record-constructor"),
    (PatOpaqueNegativeLit, "pattern-opaque-negative-literal"),
    (PatOpaqueUnboxedTuple, "pattern-opaque-unboxed-tuple"),
    (PatOpaqueExtension, "pattern-opaque-extension")
  ]

sourceQualityRefusalCases :: [VocabularyCase SourceQualityRefusal]
sourceQualityRefusalCases =
  [ VocabularyCase (SourceQualityCompactBlockSyntax "do {") "compact-block-syntax" ["marker"],
    VocabularyCase (SourceQualityLineOnlyMinification lineOnlyEvidence) "line-only-minification" ["originalLines", "replacementLines", "originalBytes", "replacementBytes"],
    VocabularyCase (SourceQualityOverlongGeneratedLine lineQualityEvidence) "overlong-generated-line" lineQualityDetailKeys,
    VocabularyCase (SourceQualityInlineListLayout lineQualityEvidence) "inline-list-layout" lineQualityDetailKeys,
    VocabularyCase (SourceQualityInlineConsPatternLayout lineQualityEvidence) "inline-cons-pattern-layout" lineQualityDetailKeys
  ]

lineOnlyEvidence :: LineOnlyMinificationEvidence
lineOnlyEvidence =
  LineOnlyMinificationEvidence
    { lomOriginalLines = 4,
      lomReplacementLines = 2,
      lomOriginalBytes = 20,
      lomReplacementBytes = 24
    }

lineQualityEvidence :: SourceLineQualityEvidence
lineQualityEvidence =
  SourceLineQualityEvidence
    { slqeLineLimit = 80,
      slqeLineNumber = 3,
      slqeOriginalMaxLineLength = 50,
      slqeReplacementLineLength = 90,
      slqeReplacementLinePreview = "preview"
    }

lineQualityDetailKeys :: [String]
lineQualityDetailKeys =
  [ "lineLimit",
    "lineNumber",
    "originalMaxLineLength",
    "replacementLineLength",
    "replacementLinePreview"
  ]

writeStatusCases :: [VocabularyCase WriteStatus]
writeStatusCases =
  [ VocabularyCase WriteWritten "written" [],
    VocabularyCase WriteCandidateWritten "candidate-written" [],
    VocabularyCase WriteRefused "refused" [],
    VocabularyCase WriteSkipped "skipped" [],
    VocabularyCase WriteIoError "io-error" []
  ]

candidateSiteKindCases :: [VocabularyCase CandidateSiteKind]
candidateSiteKindCases =
  [ VocabularyCase BindingCandidateSite "binding-candidate-site" [],
    VocabularyCase RegionCandidateSite "region-candidate-site" []
  ]

constructorEncodingCases :: IO ModuleReport -> TestTree
constructorEncodingCases getBaseReport =
  testGroup
    "nebula.reportjson.constructor-encoding"
    [ testGroup "candidate-rejection" (fmap (candidateRejectionEncodingCase getBaseReport) candidateRejectionCases),
      testGroup "nebula-error" (fmap nebulaErrorEncodingCase nebulaErrorCases),
      testGroup "law-gate-reason" (fmap (lawGateReasonEncodingCase getBaseReport) lawGateReasonCases),
      testGroup "write-back-refusal" (fmap (writeBackRefusalEncodingCase getBaseReport) writeBackRefusalCases),
      testGroup "render-refusal" (fmap (renderRefusalEncodingCase getBaseReport) renderRefusalCases),
      testGroup "source-quality-refusal" (fmap (sourceQualityEncodingCase getBaseReport) sourceQualityRefusalCases),
      testGroup "write-status" (fmap (writeStatusEncodingCase getBaseReport) writeStatusCases),
      testGroup "candidate-site-kind" (fmap (candidateSiteKindEncodingCase getBaseReport) candidateSiteKindCases),
      saturationTerminationEncodingCases getBaseReport,
      trustTierEncodingCases getBaseReport,
      semanticFidelityEncodingCases getBaseReport,
      typeVerdictEncodingCases getBaseReport,
      selfLawEncodingCases getBaseReport,
      oracleEncodingCases getBaseReport,
      diagnoseEncodingCases,
      recordOwnershipEncodingCases getBaseReport,
      typeConflictEncodingCase getBaseReport,
      ruleFireEncodingCase getBaseReport,
      oracleAttachFailureEncodingCases getBaseReport,
      workspaceErrorPathCase
    ]

candidateRejectionEncodingCase :: IO ModuleReport -> VocabularyCase CandidateRejection -> TestTree
candidateRejectionEncodingCase getBaseReport vocabularyCase =
  testCase (vocabularyKey vocabularyCase) $ do
    baseReport <- getBaseReport
    let candidate =
          RejectedCandidate
            { rejSites = [CandidateSiteLabel "fixture" Nothing BindingCandidateSite],
              rejReason = vocabularyValue vocabularyCase,
              rejEstimatedWin = 1,
              rejRealizedWin = Nothing
            }
        synthesis = (mrSynthesis baseReport) {soRejected = [candidate]}
        moduleValue = baseReport {mrSynthesis = synthesis, mrDispositions = []}
        envelope = reportEnvelope moduleValue
    assertReasonAt
      [Key "modules", Index 0, Key "synthesis", Key "rejectedCandidates", Index 0, Key "reason"]
      vocabularyCase
      envelope
    blockReasonKey (BlockedCandidate (vocabularyValue vocabularyCase))
      @?= "candidate:" <> vocabularyKey vocabularyCase
    assertTextFieldValue
      "reason"
      (vocabularyKey vocabularyCase)
      (renderWorkspaceReport (workspaceReport [moduleValue] [] []))

nebulaErrorEncodingCase :: VocabularyCase NebulaError -> TestTree
nebulaErrorEncodingCase vocabularyCase =
  testCase (vocabularyKey vocabularyCase) $ do
    let envelope =
          workspaceEnvelopeJson
            TierStandard
            (workspaceReport [] [("Fixture.hs", vocabularyValue vocabularyCase)] [])
            Nothing
        textReport = renderWorkspaceReport (workspaceReport [] [("Fixture.hs", vocabularyValue vocabularyCase)] [])
    assertReasonAt [Key "failures", Index 0, Key "error"] vocabularyCase envelope
    assertTextFieldValue "error" (vocabularyKey vocabularyCase) textReport

lawGateReasonEncodingCase :: IO ModuleReport -> VocabularyCase LawGateReason -> TestTree
lawGateReasonEncodingCase getBaseReport vocabularyCase =
  testCase (vocabularyKey vocabularyCase) $ do
    baseReport <- getBaseReport
    let moduleValue =
          baseReport
            { mrGatedLaws = [GatedLawReport (mkLawId 1) (vocabularyValue vocabularyCase) 1]
            }
        envelope = reportEnvelope moduleValue
    assertReasonAt [Key "modules", Index 0, Key "gatedLaws", Index 0, Key "reason"] vocabularyCase envelope
    assertTextFieldValue "reason" (vocabularyKey vocabularyCase) (renderWorkspaceReport (workspaceReport [moduleValue] [] []))

writeBackRefusalEncodingCase :: IO ModuleReport -> VocabularyCase WriteBackRefusal -> TestTree
writeBackRefusalEncodingCase getBaseReport vocabularyCase =
  testCase (vocabularyKey vocabularyCase) $ do
    baseReport <- getBaseReport
    let moduleValue =
          baseReport
            { mrDispositions = [("fixture", HunkBlocked (BlockedWriteBack (vocabularyValue vocabularyCase)))]
            }
        envelope = reportEnvelope moduleValue
    assertReasonAt [Key "modules", Index 0, Key "dispositions", Index 0, Key "reason"] vocabularyCase envelope
    blockReasonKey (BlockedWriteBack (vocabularyValue vocabularyCase)) @?= vocabularyKey vocabularyCase
    assertTextFieldValue "reason" (vocabularyKey vocabularyCase) (renderWorkspaceReport (workspaceReport [moduleValue] [] []))

renderRefusalEncodingCase :: IO ModuleReport -> VocabularyCase RenderRefusal -> TestTree
renderRefusalEncodingCase getBaseReport vocabularyCase =
  writeBackRefusalEncodingCase
    getBaseReport
    VocabularyCase
      { vocabularyValue = RefusedRender (vocabularyValue vocabularyCase),
        vocabularyKey = vocabularyKey vocabularyCase,
        vocabularyDetailKeys = vocabularyDetailKeys vocabularyCase
      }

sourceQualityEncodingCase :: IO ModuleReport -> VocabularyCase SourceQualityRefusal -> TestTree
sourceQualityEncodingCase getBaseReport vocabularyCase =
  testCase (vocabularyKey vocabularyCase) $ do
    baseReport <- getBaseReport
    let refusal = RefusedSourceQuality (vocabularyValue vocabularyCase)
        moduleValue =
          baseReport
            { mrDispositions = [("fixture", HunkBlocked (BlockedWriteBack refusal))]
            }
        envelope = reportEnvelope moduleValue
    assertReasonAt
      [Key "modules", Index 0, Key "dispositions", Index 0, Key "reason", Key "detail", Key "refusal"]
      vocabularyCase
      envelope
    assertTextFieldValue
      "source-quality"
      (vocabularyKey vocabularyCase)
      (renderWorkspaceReport (workspaceReport [moduleValue] [] []))

writeStatusEncodingCase :: IO ModuleReport -> VocabularyCase WriteStatus -> TestTree
writeStatusEncodingCase getBaseReport vocabularyCase =
  testCase (vocabularyKey vocabularyCase) $ do
    baseReport <- getBaseReport
    let outcome =
          WriteOutcome
            { woPath = mrPath baseReport,
              woStatus = vocabularyValue vocabularyCase,
              woReasonKey = Just "unchanged",
              woMessage = Just "message"
            }
        envelope = workspaceEnvelopeJson TierStandard (workspaceReport [baseReport] [] []) (Just [outcome])
    requireJsonAt [Key "mode"] envelope >>= (@?= JStr "write")
    requireJsonAt [Key "modules", Index 0, Key "write", Key "status"] envelope
      >>= (@?= JStr (vocabularyKey vocabularyCase))

candidateSiteKindEncodingCase :: IO ModuleReport -> VocabularyCase CandidateSiteKind -> TestTree
candidateSiteKindEncodingCase getBaseReport vocabularyCase =
  testCase (vocabularyKey vocabularyCase) $ do
    baseReport <- getBaseReport
    let candidate =
          RejectedCandidate
            { rejSites = [CandidateSiteLabel "fixture" Nothing (vocabularyValue vocabularyCase)],
              rejReason = RejectedNoEstimatedWin,
              rejEstimatedWin = 0,
              rejRealizedWin = Nothing
            }
        moduleValue = baseReport {mrSynthesis = (mrSynthesis baseReport) {soRejected = [candidate]}}
        envelope = reportEnvelope moduleValue
    requireJsonAt
      [Key "modules", Index 0, Key "synthesis", Key "rejectedCandidates", Index 0, Key "sites", Index 0, Key "kind"]
      envelope
      >>= (@?= JStr (vocabularyKey vocabularyCase))
    assertTextFieldValue "kind" (vocabularyKey vocabularyCase) (renderWorkspaceReport (workspaceReport [moduleValue] [] []))

saturationTerminationEncodingCases :: IO ModuleReport -> TestTree
saturationTerminationEncodingCases getBaseReport =
  testGroup
    "saturation-termination"
    ( fmap
        (\(termination, keyValue) ->
           testCase keyValue $ do
             baseReport <- getBaseReport
             let moduleValue = baseReport {mrTermination = termination}
                 envelope = reportEnvelope moduleValue
             requireJsonAt [Key "modules", Index 0, Key "saturation", Key "termination"] envelope
               >>= (@?= JStr keyValue)
             assertTextFieldValue "result" keyValue (renderWorkspaceReport (workspaceReport [moduleValue] [] []))
        )
        saturationTerminationCases
    )

saturationTerminationCases :: [(SaturationTermination, String)]
saturationTerminationCases =
  [ (ReachedFixedPoint, "reached-fixed-point"),
    (ReachedGoal, "reached-goal"),
    (HitIterationLimit, "hit-iteration-limit"),
    (HitNodeLimit, "hit-node-limit")
  ]

trustTierEncodingCases :: IO ModuleReport -> TestTree
trustTierEncodingCases getBaseReport =
  testGroup
    "trust-tier"
    (fmap (trustTierEncodingCase getBaseReport) trustTierCases)

trustTierCases :: [(TrustTier, String)]
trustTierCases =
  [ (ParserVerified, "parser-verified"),
    (GhcVerified, "ghc-verified"),
    (RegistryTrusted, "registry-trusted"),
    (MachineProved, "machine-proved"),
    (ModuleDerived, "module-derived")
  ]

trustTierEncodingCase :: IO ModuleReport -> (TrustTier, String) -> TestTree
trustTierEncodingCase getBaseReport (trustTier, keyValue) =
  testCase keyValue $ do
    baseReport <- getBaseReport
    let moduleValue = baseReport {mrGatedLaws = [GatedLawReport (mkLawId 1) (GateTierInadmissible trustTier) 1]}
        envelope = reportEnvelope moduleValue
    requireJsonAt [Key "modules", Index 0, Key "gatedLaws", Index 0, Key "reason", Key "detail", Key "tier"] envelope
      >>= (@?= JStr keyValue)
    assertTextFieldValue "tier" keyValue (renderWorkspaceReport (workspaceReport [moduleValue] [] []))

semanticFidelityEncodingCases :: IO ModuleReport -> TestTree
semanticFidelityEncodingCases getBaseReport =
  testGroup
    "semantic-fidelity"
    (fmap (semanticFidelityEncodingCase getBaseReport) semanticFidelityCases)

semanticFidelityCases :: [(SemanticFidelity, String)]
semanticFidelityCases =
  [ (Observational, "observational"),
    (UpToBottom, "up-to-bottom")
  ]

semanticFidelityEncodingCase :: IO ModuleReport -> (SemanticFidelity, String) -> TestTree
semanticFidelityEncodingCase getBaseReport (semanticFidelity, keyValue) =
  testCase keyValue $ do
    baseReport <- getBaseReport
    let moduleValue = baseReport {mrGatedLaws = [GatedLawReport (mkLawId 1) (GateFidelityInadmissible semanticFidelity) 1]}
        envelope = reportEnvelope moduleValue
    requireJsonAt [Key "modules", Index 0, Key "gatedLaws", Index 0, Key "reason", Key "detail", Key "fidelity"] envelope
      >>= (@?= JStr keyValue)
    assertTextFieldValue "fidelity" keyValue (renderWorkspaceReport (workspaceReport [moduleValue] [] []))

typeVerdictEncodingCases :: IO ModuleReport -> TestTree
typeVerdictEncodingCases getBaseReport =
  testGroup
    "type-verdict"
    (fmap (typeVerdictEncodingCase getBaseReport) typeVerdictCases)

typeVerdictCases :: [VocabularyCase TypeVerdict]
typeVerdictCases =
  [ VocabularyCase TypeCompatible "compatible" [],
    VocabularyCase TypePolymorphic "polymorphic" [],
    VocabularyCase TypeUnknown "unknown" [],
    VocabularyCase (TypeIncompatible [stepTypeConflict]) "incompatible" ["conflicts"]
  ]

typeVerdictEncodingCase :: IO ModuleReport -> VocabularyCase TypeVerdict -> TestTree
typeVerdictEncodingCase getBaseReport vocabularyCase =
  testCase (vocabularyKey vocabularyCase) $ do
    baseReport <- getBaseReport
    let certificate =
          HunkCertificate
            { hcBinding = "fixture",
              hcEntries = [],
              hcTypeVerdict = vocabularyValue vocabularyCase,
              hcDigest = stableDigest128 [1]
            }
        moduleValue = baseReport {mrCertificates = [certificate]}
        envelope = reportEnvelope moduleValue
    assertReasonAt [Key "modules", Index 0, Key "certificates", Index 0, Key "typeVerdict"] vocabularyCase envelope
    assertTextFieldValue "type-verdict" (vocabularyKey vocabularyCase) (renderWorkspaceReport (workspaceReport [moduleValue] [] []))

selfLawEncodingCases :: IO ModuleReport -> TestTree
selfLawEncodingCases getBaseReport =
  testGroup
    "self-law"
    ( admittedSelfLawEncodingCase getBaseReport
        : fmap (refusedSelfLawEncodingCase getBaseReport) selfLawRefusalCases
    )

selfLawRefusalCases :: [VocabularyCase SelfLawRefusal]
selfLawRefusalCases =
  [ VocabularyCase RefusedNotLambdaSpine "not-lambda-spine" [],
    VocabularyCase RefusedNotSizeDecreasing "not-size-decreasing" [],
    VocabularyCase RefusedSelfRecursive "self-recursive" [],
    VocabularyCase RefusedMultiNameEquation "multi-name-equation" []
  ]

admittedSelfLawEncodingCase :: IO ModuleReport -> TestTree
admittedSelfLawEncodingCase getBaseReport =
  testCase "admitted" $ do
    baseReport <- getBaseReport
    let moduleValue = baseReport {mrSelfLawRows = [SelfLawRow "fixture" (Right (mkLawId 1))]}
        envelope = reportEnvelope moduleValue
    assertReasonValue
      "admitted"
      ["law"]
      [Key "modules", Index 0, Key "selfLaws", Index 0, Key "outcome"]
      envelope
    assertTextFieldValue "outcome" "admitted" (renderWorkspaceReport (workspaceReport [moduleValue] [] []))

refusedSelfLawEncodingCase :: IO ModuleReport -> VocabularyCase SelfLawRefusal -> TestTree
refusedSelfLawEncodingCase getBaseReport vocabularyCase =
  testCase (vocabularyKey vocabularyCase) $ do
    baseReport <- getBaseReport
    let moduleValue = baseReport {mrSelfLawRows = [SelfLawRow "fixture" (Left (vocabularyValue vocabularyCase))]}
        envelope = reportEnvelope moduleValue
    assertReasonValue
      "refused"
      ["reason"]
      [Key "modules", Index 0, Key "selfLaws", Index 0, Key "outcome"]
      envelope
    assertReasonAt
      [Key "modules", Index 0, Key "selfLaws", Index 0, Key "outcome", Key "detail", Key "reason"]
      vocabularyCase
      envelope
    assertTextFieldValue "outcome" "refused" (renderWorkspaceReport (workspaceReport [moduleValue] [] []))
    assertTextFieldValue "reason" (vocabularyKey vocabularyCase) (renderWorkspaceReport (workspaceReport [moduleValue] [] []))

oracleEncodingCases :: IO ModuleReport -> TestTree
oracleEncodingCases getBaseReport =
  testGroup
    "oracle"
    ( fmap (attachedOracleEncodingCase getBaseReport) hieSourceKeyCases
        <> [missingOracleEncodingCase getBaseReport, ambiguousOracleEncodingCase getBaseReport]
    )

hieSourceKeyCases :: [(HieSourceKeyKind, String)]
hieSourceKeyCases =
  [ (GivenPathKey, "given-path"),
    (AbsolutePathKey, "absolute-path"),
    (RootRelativeKey, "root-relative"),
    (ModuleSuffixKey, "module-suffix")
  ]

attachedOracleEncodingCase :: IO ModuleReport -> (HieSourceKeyKind, String) -> TestTree
attachedOracleEncodingCase getBaseReport (keyKind, keyValue) =
  testCase keyValue $ do
    baseReport <- getBaseReport
    let moduleValue = baseReport {mrOracleProvenance = OracleAttached keyKind "Fixture.hs"}
        envelope = reportEnvelope moduleValue
    requireJsonAt [Key "modules", Index 0, Key "oracle", Key "status"] envelope >>= (@?= JStr "attached")
    requireJsonAt [Key "modules", Index 0, Key "oracle", Key "keyKind"] envelope >>= (@?= JStr keyValue)
    assertTextFieldValue "key-kind" keyValue (renderWorkspaceReport (workspaceReport [moduleValue] [] []))

missingOracleEncodingCase :: IO ModuleReport -> TestTree
missingOracleEncodingCase getBaseReport =
  testCase "missing" $ do
    baseReport <- getBaseReport
    let moduleValue =
          baseReport
            { mrOracleProvenance = OracleUnattached (OracleLookupMissing [TriedKey GivenPathKey "Fixture.hs"])
            }
        envelope = reportEnvelope moduleValue
    requireJsonAt [Key "modules", Index 0, Key "oracle", Key "status"] envelope >>= (@?= JStr "missing")
    requireJsonAt [Key "modules", Index 0, Key "oracle", Key "triedKeys", Index 0, Key "kind"] envelope >>= (@?= JStr "given-path")
    assertTextFieldValue "status" "missing" (renderWorkspaceReport (workspaceReport [moduleValue] [] []))

ambiguousOracleEncodingCase :: IO ModuleReport -> TestTree
ambiguousOracleEncodingCase getBaseReport =
  testCase "ambiguous" $ do
    baseReport <- getBaseReport
    let moduleValue =
          baseReport
            { mrOracleProvenance = OracleUnattached (OracleLookupAmbiguous RootRelativeKey "Fixture.hs" ["A.hie", "B.hie"])
            }
        envelope = reportEnvelope moduleValue
    requireJsonAt [Key "modules", Index 0, Key "oracle", Key "status"] envelope >>= (@?= JStr "ambiguous")
    requireJsonAt [Key "modules", Index 0, Key "oracle", Key "keyKind"] envelope >>= (@?= JStr "root-relative")
    assertTextFieldValue "status" "ambiguous" (renderWorkspaceReport (workspaceReport [moduleValue] [] []))

diagnoseEncodingCases :: TestTree
diagnoseEncodingCases =
  testCase "diagnose contexts and verdicts use their closed vocabularies" $ do
    let rows =
          [ DiagnoseRegion "local" "root.0" (ActualScope rootScopeId) 10 (Just 5) (Just 5),
            DiagnoseRegion "invisible" "root.1" (ActualScope rootScopeId) 10 (Just 5) (Just 10),
            DiagnoseRegion "no-binding" "root.2" IncompatibleScope 10 (Just 5) Nothing,
            DiagnoseRegion "none" "root.3" IncompatibleScope 10 (Just 10) (Just 10)
          ]
        envelope = diagnoseEnvelopeJson [] [("Fixture.hs", Right rows)]
        textReport = renderDiagnoseRegions "Fixture.hs" rows
    requireJsonAt [Key "mode"] envelope >>= (@?= JStr "diagnose")
    requireJsonAt [Key "modules", Index 0, Key "regions", Index 0, Key "context", Key "kind"] envelope
      >>= (@?= JStr "actual")
    requireJsonAt [Key "modules", Index 0, Key "regions", Index 0, Key "context", Key "scopeId"] envelope
      >>= (@?= JInt 0)
    requireJsonAt [Key "modules", Index 0, Key "regions", Index 2, Key "context", Key "kind"] envelope
      >>= (@?= JStr "incompatible")
    traverseJsonAssertions
      envelope
      [ ([Key "modules", Index 0, Key "regions", Index 0, Key "verdict"], JStr "local-win"),
        ([Key "modules", Index 0, Key "regions", Index 1, Key "verdict"], JStr "local-win-invisible-at-binding"),
        ([Key "modules", Index 0, Key "regions", Index 2, Key "verdict"], JStr "local-win-no-binding-view"),
        ([Key "modules", Index 0, Key "regions", Index 3, Key "verdict"], JStr "none")
      ]
    assertTextFieldKey "context" "actual" textReport
    assertTextFieldKey "context" "incompatible" textReport
    assertTextFieldValue "verdict" "local-win" textReport
    assertTextFieldValue "verdict" "local-win-invisible-at-binding" textReport
    assertTextFieldValue "verdict" "local-win-no-binding-view" textReport
    assertTextFieldValue "verdict" "none" textReport

recordOwnershipEncodingCases :: IO ModuleReport -> TestTree
recordOwnershipEncodingCases getBaseReport =
  testGroup
    "record-ownership-kind"
    (fmap (recordOwnershipEncodingCase getBaseReport) recordOwnershipKindCases)

recordOwnershipEncodingCase :: IO ModuleReport -> VocabularyCase RecordOwnershipKind -> TestTree
recordOwnershipEncodingCase getBaseReport vocabularyCase =
  testCase (vocabularyKey vocabularyCase) $ do
    baseReport <- getBaseReport
    let finding = recordOwnershipFinding {rofKind = vocabularyValue vocabularyCase}
        candidate =
          RejectedCandidate
            { rejSites = [],
              rejReason = RejectedRecordOwnershipDiagnostic [finding],
              rejEstimatedWin = 0,
              rejRealizedWin = Nothing
            }
        moduleValue = baseReport {mrSynthesis = (mrSynthesis baseReport) {soRejected = [candidate]}}
        envelope = reportEnvelope moduleValue
        findingPath =
          [ Key "modules",
            Index 0,
            Key "synthesis",
            Key "rejectedCandidates",
            Index 0,
            Key "reason",
            Key "detail",
            Key "findings",
            Index 0
          ]
    finding <- requireJsonAt findingPath envelope
    objectKeys finding
      @?= Right
        ( sort
            [ "constructorName",
              "derivedField",
              "projectionName",
              "ownerField",
              "ownerBinder",
              "kind"
            ]
        )
    requireJsonAt (findingPath <> [Key "kind", Key "kind"]) envelope
      >>= (@?= JStr (vocabularyKey vocabularyCase))
    requireJsonAt (findingPath <> [Key "kind", Key "detail"]) envelope >>= assertEmptyObject
    assertTextFieldValue
      "ownership-kind"
      (vocabularyKey vocabularyCase)
      (renderWorkspaceReport (workspaceReport [moduleValue] [] []))

typeConflictEncodingCase :: IO ModuleReport -> TestTree
typeConflictEncodingCase getBaseReport =
  testGroup
    "type-conflict"
    [ testCase "conflicts without provenance encode numeric ids only" $ do
        baseReport <- getBaseReport
        let certificate =
              HunkCertificate
                { hcBinding = "fixture",
                  hcEntries = [],
                  hcTypeVerdict = TypeIncompatible [stepTypeConflict],
                  hcDigest = stableDigest128 [1]
                }
            envelope = reportEnvelope baseReport {mrCertificates = [certificate]}
        conflict <- requireJsonAt conflictPath envelope
        objectKeys conflict @?= Right (sort ["rule", "lhsClass", "rhsClass"])
        requireJsonAt (conflictPath <> [Key "rule"]) envelope >>= (@?= JInt 7)
        requireJsonAt (conflictPath <> [Key "lhsClass"]) envelope >>= (@?= JInt 11)
        requireJsonAt (conflictPath <> [Key "rhsClass"]) envelope >>= (@?= JInt 13),
      testCase "conflicts join the certificate's law stamp by rule id" $ do
        baseReport <- getBaseReport
        let lawStamp =
              LawStamp
                { lsLaw = mkLawId 42,
                  lsTier = ParserVerified,
                  lsFidelity = Observational
                }
            entry =
              ProvenanceEntry
                { peProvenance =
                    NebulaProvenance
                      { npStamp = Just lawStamp,
                        npRule = RewriteRuleId 7,
                        npGuarded = False,
                        npFactful = False
                      },
                  peLhsClass = ClassId 11,
                  peRhsClass = ClassId 13
                }
            certificate =
              HunkCertificate
                { hcBinding = "fixture",
                  hcEntries = [entry],
                  hcTypeVerdict = TypeIncompatible [stepTypeConflict],
                  hcDigest = stableDigest128 [1]
                }
            envelope = reportEnvelope baseReport {mrCertificates = [certificate]}
        conflict <- requireJsonAt conflictPath envelope
        objectKeys conflict @?= Right (sort ["rule", "lhsClass", "rhsClass", "law", "tier", "fidelity"])
        requireJsonAt (conflictPath <> [Key "rule"]) envelope >>= (@?= JInt 7)
        requireJsonAt (conflictPath <> [Key "law"]) envelope >>= (@?= JInt 42)
        requireJsonAt (conflictPath <> [Key "tier"]) envelope >>= (@?= JStr "parser-verified")
        requireJsonAt (conflictPath <> [Key "fidelity"]) envelope >>= (@?= JStr "observational")
    ]
  where
    conflictPath =
      [ Key "modules",
        Index 0,
        Key "certificates",
        Index 0,
        Key "typeVerdict",
        Key "detail",
        Key "conflicts",
        Index 0
      ]

ruleFireEncodingCase :: IO ModuleReport -> TestTree
ruleFireEncodingCase getBaseReport =
  testCase "rule fires encode the numeric rule id" $ do
    baseReport <- getBaseReport
    let moduleValue = baseReport {mrRuleFires = [RuleFire (RewriteRuleId 7) 3 2]}
        envelope = reportEnvelope moduleValue
        firePath = [Key "modules", Index 0, Key "ruleFires", Index 0]
    fire <- requireJsonAt firePath envelope
    objectKeys fire @?= Right (sort ["rule", "matched", "scheduled"])
    requireJsonAt (firePath <> [Key "rule"]) envelope >>= (@?= JInt 7)
    requireJsonAt (firePath <> [Key "matched"]) envelope >>= (@?= JInt 3)
    requireJsonAt (firePath <> [Key "scheduled"]) envelope >>= (@?= JInt 2)
    let renderedLines = renderWorkspaceReport (workspaceReport [moduleValue] [] [])
    assertTextFieldValue "rule" "7" renderedLines
    assertTextFieldValue "matched" "3" renderedLines
    assertTextFieldValue "scheduled" "2" renderedLines

oracleAttachFailureEncodingCases :: IO ModuleReport -> TestTree
oracleAttachFailureEncodingCases getBaseReport =
  testGroup
    "oracle-attach-failure"
    [ testCase "missing" $ do
        baseReport <- getBaseReport
        let moduleValue =
              baseReport
                { mrGatedLaws =
                    [ GatedLawReport
                        (mkLawId 1)
                        (GateOracleUnattached (OracleLookupMissing [TriedKey GivenPathKey "Fixture.hs"]))
                        1
                    ]
                }
            envelope = reportEnvelope moduleValue
            failurePath = [Key "modules", Index 0, Key "gatedLaws", Index 0, Key "reason", Key "detail", Key "failure"]
        assertReasonValue "missing" ["triedKeys"] failurePath envelope
        triedKey <- requireJsonAt (failurePath <> [Key "detail", Key "triedKeys", Index 0]) envelope
        objectKeys triedKey @?= Right (sort ["kind", "path"])
        requireJsonAt (failurePath <> [Key "detail", Key "triedKeys", Index 0, Key "kind"]) envelope >>= (@?= JStr "given-path"),
      testCase "ambiguous" $ do
        baseReport <- getBaseReport
        let moduleValue =
              baseReport
                { mrGatedLaws =
                    [ GatedLawReport
                        (mkLawId 1)
                        (GateOracleUnattached (OracleLookupAmbiguous RootRelativeKey "Fixture.hs" ["A.hie", "B.hie"]))
                        1
                    ]
                }
            envelope = reportEnvelope moduleValue
            failurePath = [Key "modules", Index 0, Key "gatedLaws", Index 0, Key "reason", Key "detail", Key "failure"]
        assertReasonValue "ambiguous" ["keyKind", "key", "candidates"] failurePath envelope
        requireJsonAt (failurePath <> [Key "detail", Key "keyKind"]) envelope >>= (@?= JStr "root-relative")
    ]

workspaceErrorPathCase :: TestTree
workspaceErrorPathCase =
  testCase "workspace failures retain the path carried by the error" $ do
    let envelope =
          workspaceEnvelopeJson
            TierStandard
            (workspaceReport [] [] [NebulaWorkspaceError "missing-root" "no such file"])
            Nothing
    requireJsonAt [Key "failures", Index 0, Key "path"] envelope >>= (@?= JStr "missing-root")

reportEnvelope :: ModuleReport -> J
reportEnvelope moduleValue =
  workspaceEnvelopeJson TierFull (workspaceReport [moduleValue] [] []) Nothing

assertReasonAt :: JsonPath -> VocabularyCase value -> J -> IO ()
assertReasonAt path vocabularyCase envelope = do
  assertReasonValue
    (vocabularyKey vocabularyCase)
    (vocabularyDetailKeys vocabularyCase)
    path
    envelope

assertReasonValue :: String -> [String] -> JsonPath -> J -> IO ()
assertReasonValue expectedKind expectedDetailKeys path envelope = do
  reasonValue <- requireJsonAt path envelope
  objectKeys reasonValue @?= Right (sort ["kind", "detail"])
  requireJsonAt (path <> [Key "kind"]) envelope >>= (@?= JStr expectedKind)
  detailValue <- requireJsonAt (path <> [Key "detail"]) envelope
  objectKeys detailValue @?= Right (sort expectedDetailKeys)

assertTextFieldValue :: String -> String -> [String] -> IO ()
assertTextFieldValue fieldName expectedValue renderedLines =
  assertBool
    ("the text report omits " <> fieldName <> "=" <> expectedValue)
    (expectedValue `elem` textFieldValues fieldName renderedLines)

assertTextFieldKey :: String -> String -> [String] -> IO ()
assertTextFieldKey fieldName expectedKey renderedLines =
  assertBool
    ("the text report omits " <> fieldName <> " key " <> expectedKey)
    (expectedKey `elem` fmap (takeWhile (/= ':')) (textFieldValues fieldName renderedLines))

textFieldValues :: String -> [String] -> [String]
textFieldValues fieldName =
  mapMaybe (stripPrefix (fieldName <> "=")) . foldMap words

assertRequiredKeys :: [String] -> J -> IO ()
assertRequiredKeys requiredKeys value =
  case objectKeys value of
    Left message ->
      assertFailure message
    Right actualKeys ->
      let missingKeys = filter (`notElem` actualKeys) requiredKeys
       in assertBool ("missing object keys: " <> show missingKeys) (null missingKeys)

assertAbsentKey :: String -> J -> IO ()
assertAbsentKey forbiddenKey = \case
  JObj members ->
    assertBool ("unexpected object key: " <> forbiddenKey) (forbiddenKey `notElem` fmap fst members)
  other ->
    assertFailure ("expected object, found " <> show other)

assertEmptyObject :: J -> IO ()
assertEmptyObject value =
  objectKeys value @?= Right []

objectKeys :: J -> Either String [String]
objectKeys = \case
  JObj members ->
    Right (sort (fmap fst members))
  other ->
    Left ("expected object, found " <> show other)

data JsonPathSegment
  = Key !String
  | Index !Int
  deriving stock (Eq, Show)

type JsonPath = [JsonPathSegment]

jsonAt :: JsonPath -> J -> Either String J
jsonAt path root =
  foldM descendJson root path

descendJson :: J -> JsonPathSegment -> Either String J
descendJson current = \case
  Key memberName ->
    case current of
      JObj members ->
        maybe
          (Left ("missing JSON member " <> memberName))
          Right
          (lookup memberName members)
      other ->
        Left ("cannot select member " <> memberName <> " from " <> show other)
  Index elementIndex ->
    case current of
      JArr elements
        | elementIndex >= 0 ->
            maybe
              (Left ("missing JSON array index " <> show elementIndex))
              Right
              (listToMaybe (drop elementIndex elements))
      JArr _ ->
        Left ("negative JSON array index " <> show elementIndex)
      other ->
        Left ("cannot select array index " <> show elementIndex <> " from " <> show other)

requireJsonAt :: JsonPath -> J -> IO J
requireJsonAt path root =
  either assertFailure pure (jsonAt path root)

traverseJsonAssertions :: J -> [(JsonPath, J)] -> IO ()
traverseJsonAssertions envelope =
  foldMap
    (\(path, expectedValue) -> requireJsonAt path envelope >>= (@?= expectedValue))

requireBaseModuleReport :: IO ModuleReport
requireBaseModuleReport =
  either
    (\(modulePath, moduleFailure) -> assertFailure ("improve failed for " <> modulePath <> ": " <> show moduleFailure))
    (pure . miReport)
    (improveModule defaultNebulaConfig baseWorkload)

baseWorkload :: ModuleWorkload
baseWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/ReportJsonFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.ReportJsonFixture where",
            "",
            "identity value = value"
          ],
      mwOracleLookup = OracleMissing []
    }
