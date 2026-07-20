module Melusine.Nebula.Spec.PipelineSpec (spec) where

import Control.Monad (filterM)
import Data.Foldable (traverse_)
import Data.List (find, isInfixOf, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set qualified as Set
import GHC.Types.Name.Occurrence (mkVarOcc, occNameString)
import GHC.Types.Name.Reader (RdrName, mkRdrUnqual, rdrNameOcc)
import Melusine.Nebula
  ( ModuleImprovement (..),
    ModulePatch (..),
    SealOutcome (..),
    SealedSource,
    improveModule,
    sealedSourceText,
  )
import Melusine.Nebula.Discovery.Choose
  ( AbstractionCandidate (..),
    CandidateSite (..),
    CandidateSiteKind (..),
    ChosenBinding (..),
    NebulaSizeExtractionSections,
    candidateSites,
    chooseBindings,
    harvestContexts,
    sharedAbstractionCandidates,
    sizeExtractionSections,
  )
import Melusine.Nebula.Core
  ( CorpusSources (..),
    ModuleWorkload (..),
    NebulaConfig (..),
    NebulaError (..),
    defaultNebulaConfig,
    workloadOracle,
  )
import Melusine.Nebula.Proof.Certificate (HunkCertificate (..), NebulaProvenance (..), ProvenanceEntry (..))
import Melusine.Nebula.Rewrite.Corpus
  ( EvidenceFactCensus (..),
    GatedLawReport (..),
    LawGateReason (..),
    LawStamp (..),
    NumTypeFactCensus (..),
    RuleCorpus,
    deriveRuleCorpus,
    deriveRuleCorpusWithOracleKeys,
    deriveRuleCorpusWithOracleKeysAndReason,
    rcBindingMetrics,
    rcCompiledProgram,
    rcEvidenceFactCensus,
    rcFactBook,
    rcGatedLaws,
    rcLawTable,
    rcNumTypeFactCensus,
    rcRuleBook,
    rcSelfLawRows,
    rcSiteMetrics,
    rcVocabularyMetrics,
  )
import Melusine.Nebula.Source.Ast qualified as SourceAst
import Melusine.Nebula.Source.Ingest (IngestedModule (..), ingestModule)
import Melusine.Nebula.Write.Protocol
  ( ProtocolRewriteKind (..),
    ProtocolRewritePlan (..),
    ProtocolRewriteSkip (..),
    ProtocolSealObligation (..),
    planProtocolRewrites,
    sealProtocolObligations,
  )
import Melusine.Nebula.Harvest.Core
  ( HarvestState (..),
    SiteRow,
    advanceHarvestFromSections,
    buildHarvest,
    candidateSiteSupportGroups,
    harvestDirtyBuckets,
    siteRow,
  )
import Melusine.Nebula.Harvest.Maintain (HarvestAdvanceDecision (..))
import Melusine.Nebula.Harvest.Pairs (admittedSitePairs, buildPairLedger)
import Melusine.Nebula.Rewrite.Saturate
  ( SaturatedModule,
    SaturationLifecycleCounts (..),
    defaultSaturationOptions,
    resumeSaturatedModule,
    saturateModule,
    smContextGraph,
    smFinalClassCount,
    smFinalNodeCount,
    smInitialClassCount,
    smInitialNodeCount,
    smIterations,
    smLifecycleCounts,
    smMatchesApplied,
    smProofSteps,
    smRuleFires,
    smScheduledTotal,
    smTermination,
    smRuntimeState,
  )
import Melusine.Nebula.Synthesis.Core
  ( CandidateRejection (..),
    CandidateSiteLabel (..),
    RecordOwnershipFinding (..),
    RecordOwnershipKind (..),
    PlanStagingReport (..),
    RejectedCandidate (..),
    SynthesisOutcome (..),
    SynthesizedDefinition (..),
    SynthesizedName (..),
    SynthesizedSite (..),
    synthesizeAbstractions,
  )
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( BinderAnn (..),
    ConvertedModule (..),
    GuardedAltF (..),
    HsExprBindingRuleMetrics (..),
    HsExprF (..),
    HsPatF (..),
    HsGuardStmtF (..),
    LetMode (..),
    LetRecursion (..),
    patBinders,
    HsExprInsertionMetrics (..),
    HsExprSupportRuleMetrics (..),
    HsExprVocabularyRuleMetrics (..),
    HsStmtF (..),
    HsVarRef (..),
    SpanClassRow (..),
    SpannedExpr (..),
    SelfLawRefusal (..),
    SelfLawRow (..),
    SourceRegion,
    TopLevelBinding (..),
    hsAcceptedComposeOrigins,
    hsAcceptedReverseOrigins,
    hsExprCompositionLawId,
    hsExprFmapIdLawId,
    hsExprFmapFusionLawId,
    hsExprMapAppendFactorLawId,
    hsExprMapFusionLawId,
    hsExprMonadLeftIdentityLawId,
    hsExprMonadRightIdentityLawId,
    hsExprParErasureLawId,
    hsExprPlusUnitLawId,
    hsExprOracleKeyTable,
    hsExprReverseOracleKey,
    hsExprReverseInvolutionLawId,
    hsExprSelfUnfoldLawFamily,
    hsExprSelfUnfoldLawId,
    hsExprSelfUnfoldRuleIdBase,
    hsExprVocabularyLawIds,
  )
import Moonlight.Core
  ( Pattern (..),
    RewriteRuleId (..),
    SiteProgram (..),
    SupportIndexedRule (..),
  )
import Moonlight.EGraph.Pure.AntiUnify (BinaryLGGResult (..))
import Moonlight.EGraph.Pure.Context (contextPreparedObjects)
import Moonlight.EGraph.Pure.Context (cegBase)
import Moonlight.EGraph.Pure.Context.Proof (ProofGraph (pgGraph), serializeProofLog)
import Moonlight.EGraph.Saturation.Context.State (sceContextGraph)
import Moonlight.EGraph.Bench.Harness.Digest (contextGraphDigest)
import Moonlight.EGraph.Pure.Types (eGraphClassCount, eGraphNodeCount)
import Moonlight.Rewrite.System (RawRewriteRule (..))
import Moonlight.Rewrite.System
  ( CompiledFactRule,
    cfrId,
    RawFactRule (..),
  )
import Moonlight.Rewrite.System
  ( LawBook (..),
    LawSpec (..),
    LawId,
    OracleKey,
    OracleRequirement (..),
    SemanticFidelity (..),
    TrustTier (..),
    lawIdKey,
    mkLawId,
  )
import Data.Fix (Fix (..))
import Moonlight.Rewrite.ProofContext (ProofKind (..), ProofStep (..))
import Moonlight.Saturation.Context.Driver (resumableRuntimeState)
import Moonlight.Saturation.Context.Runtime.State
  ( RuntimeCore (..),
    RuntimeState (..),
  )
import Moonlight.Saturation.Core (SaturationTermination (ReachedFixedPoint))
import Moonlight.Sheaf.Twist.SupportedRuleSpec
  ( SupportedFactSpec (..),
    SupportedRuleSpec (..),
    supportedFactSpecs,
    supportedRules,
  )
import Moonlight.Pale.Ghc.Hie.Oracle (ModuleNameOracle (..), PackageUnitParseFailure, ResolvedOrigin, mkResolvedOrigin, occResolvesUniquely)
import Moonlight.Pale.Ghc.Hie.SourceKey (HieSourceKeyKind (..), OracleAttachFailure (..), OracleLookup (..))
import Moonlight.Pale.Ghc.Hie.TypeWords (TypeWords, tyConTypeWords)
import System.Directory (doesFileExist)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

spec :: TestTree
spec =
  testGroup
    "nebula.pipeline"
    [ ingestCases,
      spanCases,
      corpusCases,
      oracleCases,
      renamerCases,
      evidenceCases,
      selfLawCases,
      saturateCases,
      chooseCases,
      harvestMaintenanceCases,
      scopeSoundnessCases,
      synthesizeCases,
      unionCases
    ]

oracleCases :: TestTree
oracleCases =
  testGroup
    "nebula.pipeline.oracle"
    [ testCase "missing hie data is rendered as the oracle obstruction" $ do
        ingested <- requireIngested fixtureWorkload
        corpus <-
          requireRight
            "corpus with missing oracle"
            (deriveRuleCorpusWithOracleKeysAndReason defaultNebulaConfig Set.empty (Just (OracleLookupMissing [])) (imSpanRows ingested) Nothing (imConverted ingested))
        assertEqual
          "composition gate reports the missing hie channel"
          [GateOracleUnattached (OracleLookupMissing []), GateOracleUnattached (OracleLookupMissing []), GateOracleUnattached (OracleLookupMissing [])]
          (fmap glrReason (passZeroGatedLaws corpus)),
      testCase "stub oracle evidence satisfies the composition key" $ do
        ingested <- requireIngested fixtureWorkload
        oracle <- requireRight "compose oracle" composeOracle
        acceptedComposeOrigins <- requireRight "accepted composition origins" hsAcceptedComposeOrigins
        assertBool
          "stub oracle resolves composition into the accepted base origin"
          (occResolvesUniquely oracle "." acceptedComposeOrigins)
        satisfiedKeys <- requireRight "oracle satisfied keys" (oracleSatisfiedKeys oracle)
        corpus <-
          requireRight
            "corpus with stub oracle"
            (deriveRuleCorpusWithOracleKeys defaultNebulaConfig satisfiedKeys (imSpanRows ingested) (Just oracle) (imConverted ingested))
        assertEqual "oracle evidence admits every composition rule" (hsrmLambdaSiteCount (rcSiteMetrics corpus)) (hsrmCompositionRuleCount (rcSiteMetrics corpus))
        assertEqual "oracle-satisfied corpus has no pass-0 gated law rows" [] (passZeroGatedLaws corpus)
    ]

composeOracle :: Either PackageUnitParseFailure ModuleNameOracle
composeOracle =
  (\oracle -> oracle {mnoSourcePath = "Melusine/Nebula/Fixture.hs"})
    <$> baseVocabularyOracle

baseVocabularyOracle :: Either PackageUnitParseFailure ModuleNameOracle
baseVocabularyOracle =
  do
    vocabularyUses <- baseVocabularyUses
    pure
      ModuleNameOracle
        { mnoSourcePath = "Melusine/Nebula/Fixture.hs",
          mnoGlobalUses = vocabularyUses,
          mnoEvidenceAtSpan = Map.empty,
          mnoTypeAtSpan = Map.empty
        }

baseVocabularyUses :: Either PackageUnitParseFailure (Map.Map String (Set.Set ResolvedOrigin))
baseVocabularyUses =
  Map.fromList
    <$> traverse
      ( \(occurrence, unitText, moduleText, occurrenceText) ->
          (\origin -> (occurrence, Set.singleton origin))
            <$> mkResolvedOrigin unitText moduleText occurrenceText
      )
      [ (".", "base", "GHC.Internal.Base", "."),
        ("&&", "base", "GHC.Internal.Classes", "&&"),
        ("*", "base", "GHC.Internal.Num", "*"),
        (">>=", "base", "GHC.Internal.Base", ">>="),
        ("+", "base", "GHC.Internal.Num", "+"),
        ("++", "base", "GHC.Internal.Base", "++"),
        ("concat", "base", "GHC.Internal.Data.Foldable", "concat"),
        ("concatMap", "base", "GHC.Internal.Data.Foldable", "concatMap"),
        ("filter", "base", "GHC.Internal.List", "filter"),
        ("fmap", "base", "GHC.Internal.Base", "fmap"),
        ("id", "base", "GHC.Internal.Base", "id"),
        ("map", "base", "GHC.Internal.Base", "map"),
        ("pure", "base", "GHC.Internal.Base", "pure"),
        ("return", "base", "GHC.Internal.Base", "return"),
        ("reverse", "base", "GHC.Internal.List", "reverse")
      ]

oracleSatisfiedKeys :: ModuleNameOracle -> Either PackageUnitParseFailure (Set.Set OracleKey)
oracleSatisfiedKeys oracle =
  Set.fromList
    . fmap (\(oracleKey, _, _) -> oracleKey)
    . filter (\(_, occurrence, acceptedOrigins) -> occResolvesUniquely oracle occurrence acceptedOrigins)
    <$> hsExprOracleKeyTable

passZeroGatedLaws :: RuleCorpus -> [GatedLawReport]
passZeroGatedLaws corpus =
  filter ((`Set.member` passZeroLawIds) . glrLaw) (rcGatedLaws corpus)

passZeroLawIds :: Set.Set LawId
passZeroLawIds =
  Set.fromList
    [ hsExprCompositionLawId,
      hsExprMapFusionLawId,
      hsExprFmapFusionLawId
    ]

renamerCases :: TestTree
renamerCases =
  testGroup
    "nebula.pipeline.renamer"
    [ testCase "map fusion is gated off without oracle evidence" $ do
        ingested <- requireIngested mapFusionWorkloadWithoutOracle
        corpus <-
          requireRight
            "map-fusion corpus without oracle"
            (deriveRuleCorpus defaultNebulaConfig (imSpanRows ingested) Nothing (imConverted ingested))
        assertBool
          "map fusion law is reported as gated"
          (hsExprMapFusionLawId `elem` fmap glrLaw (rcGatedLaws corpus))
        improvement <- requireImproved mapFusionWorkloadWithoutOracle
        assertBool "map fusion does not splice without oracle evidence" (null (mpSpliced (miPatch improvement))),
      testCase "map fusion fires under accepted oracle evidence and seals the composed form" $ do
        workload <- requireRight "map-fusion oracle workload" mapFusionWorkloadWithOracle
        improvement <- requireImproved workload
        assertBool
          "map fusion fixture must splice the fused binding"
          ("incDouble" `elem` fmap fst (mpSpliced (miPatch improvement)))
        sealedSource <- requireSealed improvement
        assertBool
          ("sealed source contains a law-derived composed map form; got: " <> sealedSourceText sealedSource)
          ( "incDouble = map (inc . dbl)" `isInfixOf` sealedSourceText sealedSource
              || "map (inc . dbl) xs" `isInfixOf` sealedSourceText sealedSource
              || "map inc . map dbl" `isInfixOf` sealedSourceText sealedSource
              || "(map inc) . (map dbl)" `isInfixOf` sealedSourceText sealedSource
          )
        assertBool
          ("map fusion certificate is RegistryTrusted; tiers: " <> show (Set.toList (certificateLawTiers "incDouble" improvement)))
          ((hsExprMapFusionLawId, RegistryTrusted) `Set.member` certificateLawTiers "incDouble" improvement),
      testCase "a user composition origin keeps composition-dependent laws gated" $ do
        oracle <- requireRight "user composition oracle" userCompositionOracle
        acceptedComposeOrigins <- requireRight "accepted composition origins" hsAcceptedComposeOrigins
        assertBool
          "bad oracle does not satisfy the accepted composition origin"
          (not (occResolvesUniquely oracle "." acceptedComposeOrigins))
        workload <- requireRight "map-fusion user-composition workload" mapFusionWorkloadWithUserComposition
        satisfiedKeys <- requireRight "oracle satisfied keys" (oracleSatisfiedKeys oracle)
        ingested <- requireIngested workload
        corpus <-
          requireRight
            "map-fusion corpus with user composition"
            (deriveRuleCorpusWithOracleKeys defaultNebulaConfig satisfiedKeys (imSpanRows ingested) (Just oracle) (imConverted ingested))
        assertBool
          "composition remains gated"
          (hsExprCompositionLawId `elem` fmap glrLaw (rcGatedLaws corpus))
        assertBool
          "map fusion remains gated by the missing composition key"
          (hsExprMapFusionLawId `elem` fmap glrLaw (rcGatedLaws corpus)),
      testCase "reverse involution fires only under accepted reverse evidence" $ do
        workload <- requireRight "reverse oracle workload" reverseInvolutionWorkloadWithOracle
        improvement <- requireImproved workload
        assertBool
          "reverse involution fixture must splice the simplified binding"
          ("rev" `elem` fmap fst (mpSpliced (miPatch improvement)))
        sealedSource <- requireSealed improvement
        assertBool
          "sealed source contains the involution result"
          ("rev = sourceList" `isInfixOf` sealedSourceText sealedSource)
        assertBool
          "reverse involution certificate is RegistryTrusted"
          ((hsExprReverseInvolutionLawId, RegistryTrusted) `Set.member` certificateLawTiers "rev" improvement),
      testCase "map-append factorization fires under map and append evidence" $ do
        workload <- requireRight "map-append oracle workload" mapAppendWorkloadWithOracle
        improvement <- requireImproved workload
        assertBool
          "map-append fixture must splice the factored binding"
          ("mapAppend" `elem` fmap fst (mpSpliced (miPatch improvement)))
        sealedSource <- requireSealed improvement
        assertBool
          "sealed source contains the factored map append form"
          ("map f (xs ++ ys)" `isInfixOf` sealedSourceText sealedSource && not ("map f xs ++ map f ys" `isInfixOf` sealedSourceText sealedSource))
        assertBool
          "map-append certificate is RegistryTrusted"
          ((hsExprMapAppendFactorLawId, RegistryTrusted) `Set.member` certificateLawTiers "mapAppend" improvement),
      testCase "a user reverse origin keeps reverse involution gated" $ do
        oracle <- requireRight "reverse shadow oracle" reverseShadowOracle
        acceptedReverseOrigins <- requireRight "accepted reverse origins" hsAcceptedReverseOrigins
        assertBool
          "bad oracle does not satisfy the accepted reverse origin"
          (not (occResolvesUniquely oracle "reverse" acceptedReverseOrigins))
        workload <- requireRight "reverse shadow workload" reverseInvolutionWorkloadWithShadow
        satisfiedKeys <- requireRight "oracle satisfied keys" (oracleSatisfiedKeys oracle)
        ingested <- requireIngested workload
        corpus <-
          requireRight
            "reverse corpus with user reverse"
            (deriveRuleCorpusWithOracleKeys defaultNebulaConfig satisfiedKeys (imSpanRows ingested) (Just oracle) (imConverted ingested))
        assertBool
          "reverse involution remains gated by the shadowed reverse key"
          (hsExprReverseInvolutionLawId `elem` fmap glrLaw (rcGatedLaws corpus))
        reverseGate <-
          maybe
            (assertFailure "reverse involution gate missing")
            pure
            (find ((== hsExprReverseInvolutionLawId) . glrLaw) (rcGatedLaws corpus))
        assertEqual
          "reverse involution reports the missing reverse oracle key"
          (GateMissingOracleKeys (Set.singleton hsExprReverseOracleKey))
          (glrReason reverseGate)
        improvement <- requireImproved workload
        assertBool
          "reverse involution does not splice with shadowed reverse evidence"
          (not ("rev" `elem` fmap fst (mpSpliced (miPatch improvement))))
    ]

evidenceCases :: TestTree
evidenceCases =
  testGroup
    "nebula.pipeline.evidence"
    [ testCase "fmap evidence facts are minted per evidenced occurrence" $ do
        ingested <- requireIngested fmapEvidenceWorkloadWithoutOracle
        evidenceRegion <-
          case fmapFusionCandidateRegions (imConverted ingested) of
            firstRegion : _ : _ ->
              pure firstRegion
            regions ->
              assertFailure ("expected at least two fmap-fusion candidate regions, got " <> show regions)
        oracle <- requireRight "fmap evidence oracle" (fmapEvidenceOracle evidenceRegion)
        satisfiedKeys <- requireRight "oracle satisfied keys" (oracleSatisfiedKeys oracle)
        corpus <-
          requireRight
            "fmap evidence corpus"
            ( deriveRuleCorpusWithOracleKeys
                fmapEvidenceConfig
                satisfiedKeys
                (imSpanRows ingested)
                (Just oracle)
                (imConverted ingested)
            )
        assertEqual "exactly one evidence fact rule is minted" 1 (length (supportedFactSpecs (rcFactBook corpus)))
        assertBool "fmap fusion is admitted under name oracle evidence" (hsExprFmapFusionLawId `notElem` fmap glrLaw (rcGatedLaws corpus)),
      testCase "fmap fusion rewrites only the evidenced occurrence" $ do
        ingested <- requireIngested fmapEvidenceWorkloadWithoutOracle
        evidenceRegion <-
          case fmapFusionCandidateRegions (imConverted ingested) of
            firstRegion : _ : _ ->
              pure firstRegion
            regions ->
              assertFailure ("expected at least two fmap-fusion candidate regions, got " <> show regions)
        workload <- requireRight "fmap evidence workload" (fmapEvidenceWorkloadWithOracle evidenceRegion)
        improvement <- requireImprovedWith fmapEvidenceConfig workload
        sealedSource <- requireSealed improvement
        assertBool
          "the evidenced fmap chain is fused"
          ("fmap (inc . dbl) xs" `isInfixOf` sealedSourceText sealedSource)
        assertBool
          "the unevidenced fmap chain remains nested"
          ("fmap inc (fmap dbl ys)" `isInfixOf` sealedSourceText sealedSource)
        assertBool
          "the unevidenced fmap chain is not silently fused"
          (not ("fmap (inc . dbl) ys" `isInfixOf` sealedSourceText sealedSource))
      ,
      testCase "fmap id seals only with lawful functor evidence" $ do
        workloadWithoutEvidence <- requireRight "fmap-id workload" fmapIdWorkloadWithoutEvidence
        lawfulOrigin <- requireRight "lawful functor origin" lawfulFunctorListOrigin
        evidenceRegion <- requireSingleRegion "fmap-id candidate" fmapIdCandidateRegions workloadWithoutEvidence
        workloadWithEvidence <- requireRight "fmap-id lawful evidence workload" (fmapIdWorkloadWithEvidence lawfulOrigin evidenceRegion)
        improvement <- requireImproved workloadWithEvidence
        sealedSource <- requireSealed improvement
        assertBool
          "fmap id seals to the identity payload"
          ("mapped = xs" `isInfixOf` sealedSourceText sealedSource)
        assertBool
          "fmap-id certificate is guarded and fact-backed"
          ((hsExprFmapIdLawId, True, True) `Set.member` certificateLawFacts "mapped" improvement)
        improvementWithoutEvidence <- requireImproved workloadWithoutEvidence
        assertBool
          "fmap id does not splice without a lawful evidence fact"
          (not ("mapped" `elem` fmap fst (mpSpliced (miPatch improvementWithoutEvidence)))),
      testCase "unlawful functor evidence is counted and mints no fact" $ do
        workloadWithoutEvidence <- requireRight "fmap-id workload" fmapIdWorkloadWithoutEvidence
        unlawfulOrigin <- requireRight "unlawful functor origin" unlawfulFunctorOrigin
        oracle <- requireRight "compose oracle" composeOracle
        satisfiedKeys <- requireRight "oracle satisfied keys" (oracleSatisfiedKeys oracle)
        evidenceRegion <- requireSingleRegion "fmap-id candidate" fmapIdCandidateRegions workloadWithoutEvidence
        workloadWithEvidence <- requireRight "fmap-id unlawful evidence workload" (fmapIdWorkloadWithEvidence unlawfulOrigin evidenceRegion)
        ingested <- requireIngested workloadWithEvidence
        corpus <-
          requireRight
            "fmap-id corpus with unlawful evidence"
            (deriveRuleCorpusWithOracleKeys defaultNebulaConfig satisfiedKeys (imSpanRows ingested) (workloadOracle workloadWithEvidence) (imConverted ingested))
        assertEqual "unlawful evidence is visible in the census" (EvidenceFactCensus 0 1 0) (rcEvidenceFactCensus corpus)
        assertEqual "unlawful evidence mints no fact rule" 0 (length (supportedFactSpecs (rcFactBook corpus))),
      testCase "monad identities seal under lawful monad evidence" $ do
        workloadWithoutEvidence <- requireRight "monad identity workload" monadIdentityWorkloadWithoutEvidence
        evidenceRegions <- bindCandidateRegions <$> (imConverted <$> requireIngested workloadWithoutEvidence)
        workloadWithEvidence <- requireRight "monad identity evidence workload" (monadIdentityWorkloadWithEvidence evidenceRegions)
        improvement <- requireImproved workloadWithEvidence
        sealedSource <- requireSealed improvement
        assertBool "left identity seals to function application" ("monadLeft = next value" `isInfixOf` sealedSourceText sealedSource)
        assertBool "right identity seals to the action" ("monadRight = action" `isInfixOf` sealedSourceText sealedSource)
        assertBool
          "monad-left certificate is guarded and fact-backed"
          ((hsExprMonadLeftIdentityLawId, True, True) `Set.member` certificateLawFacts "monadLeft" improvement)
        assertBool
          "monad-right certificate is guarded and fact-backed"
          ((hsExprMonadRightIdentityLawId, True, True) `Set.member` certificateLawFacts "monadRight" improvement)
      ,
      testCase "numeric unit laws seal only under lawful numeric type evidence" $ do
        workloadWithoutEvidence <- requireRight "plus-unit workload" plusUnitWorkloadWithoutTypeEvidence
        plusRegion <- requireSingleRegion "plus-unit candidate" plusCandidateRegions workloadWithoutEvidence
        intWorkload <- requireRight "plus-unit int evidence workload" (plusUnitWorkloadWithTypeEvidence intTypeWords plusRegion)
        improvement <- requireImproved intWorkload
        sealedSource <- requireSealed improvement
        assertBool "plus unit seals to the payload" ("plusUnit = x" `isInfixOf` sealedSourceText sealedSource)
        assertBool
          "plus-unit certificate is guarded and fact-backed"
          ((hsExprPlusUnitLawId, True, True) `Set.member` certificateLawFacts "plusUnit" improvement)
        doubleWorkload <- requireRight "plus-unit double evidence workload" (plusUnitWorkloadWithTypeEvidence doubleTypeWords plusRegion)
        oracle <- requireRight "compose oracle" composeOracle
        satisfiedKeys <- requireRight "oracle satisfied keys" (oracleSatisfiedKeys oracle)
        ingested <- requireIngested doubleWorkload
        corpus <-
          requireRight
            "plus-unit corpus with unlawful numeric type"
            (deriveRuleCorpusWithOracleKeys defaultNebulaConfig satisfiedKeys (imSpanRows ingested) (workloadOracle doubleWorkload) (imConverted ingested))
        assertEqual
          "double evidence is visible in the numeric census"
          (NumTypeFactCensus 0 1 (Set.size (Set.fromList (fmap scrRegion (imSpanRows ingested))) - 1))
          (rcNumTypeFactCensus corpus)
        improvementWithDouble <- requireImproved doubleWorkload
        assertBool
          "plus unit does not splice with unlawful numeric type evidence"
          (not ("plusUnit" `elem` fmap fst (mpSpliced (miPatch improvementWithDouble))))
    ]

selfLawCases :: TestTree
selfLawCases =
  testGroup
    "nebula.pipeline.self-law"
    [ testCase "module self-unfold laws admit size-decreasing local definitions" $ do
        ingested <- requireIngested selfUnfoldWorkload
        corpus <-
          requireRight
            "self-unfold corpus"
            (deriveRuleCorpus defaultNebulaConfig (imSpanRows ingested) (workloadOracle selfUnfoldWorkload) (imConverted ingested))
        assertBool
          "identity is recorded as an admitted self law"
          (SelfLawRow "identity" (Right hsExprSelfUnfoldLawId) `elem` rcSelfLawRows corpus)
        improvement <- requireImproved selfUnfoldWorkload
        sealedSource <- requireSealed improvement
        assertBool "self-unfold seals the local call" ("use = payload" `isInfixOf` sealedSourceText sealedSource)
        assertBool
          "self-unfold certificate is module-derived"
          ((hsExprSelfUnfoldLawId, ModuleDerived) `Set.member` certificateLawTiers "use" improvement),
      testCase "self-unfold rule ids live in the 400000000 allocation" $ do
        ingested <- requireIngested selfUnfoldWorkload
        assertEqual
          "one admitted self-unfold rule uses base + binding-index"
          [RewriteRuleId hsExprSelfUnfoldRuleIdBase]
          (selfUnfoldRuleIds (imConverted ingested)),
      testCase "module self-unfold laws remain behind the trust-tier gate" $ do
        ingested <- requireIngested selfUnfoldWorkload
        corpus <-
          requireRight
            "tier-gated self-unfold corpus"
            (deriveRuleCorpus noModuleDerivedConfig (imSpanRows ingested) (workloadOracle selfUnfoldWorkload) (imConverted ingested))
        assertBool
          "self-unfold is reported as a tier-gated law"
          ( GatedLawReport
              { glrLaw = hsExprSelfUnfoldLawId,
                glrReason = GateTierInadmissible ModuleDerived,
                glrRuleCount = 1
              }
              `elem` rcGatedLaws corpus
          )
        improvement <- requireImprovedWith noModuleDerivedConfig selfUnfoldWorkload
        assertBool "tier-gated self-unfold does not splice" (not ("use" `elem` fmap fst (mpSpliced (miPatch improvement)))),
      testCase "self-unfold refusal rows expose typed obstructions" $ do
        ingested <- requireIngested selfLawRefusalWorkload
        corpus <-
          requireRight
            "self-law refusal corpus"
            (deriveRuleCorpus defaultNebulaConfig (imSpanRows ingested) (workloadOracle selfLawRefusalWorkload) (imConverted ingested))
        assertEqual
          "every refusal lands in the closed refusal vocabulary"
          ( Map.fromList
              [ ("constant", Left RefusedNotLambdaSpine),
                ("self", Left RefusedSelfRecursive),
                ("expand", Left RefusedNotSizeDecreasing)
              ]
          )
          (Map.fromList [(slrBinding row, slrOutcome row) | row <- rcSelfLawRows corpus]),
      testCase "synthetic multi-name bindings are refused before any rewrite is minted" $ do
        convertedModule <- syntheticMultiNameModule
        let (selfLawRows, LawBook selfLawSpecs) = hsExprSelfUnfoldLawFamily convertedModule
        assertEqual
          "multi-name rows are refused as a typed obstruction"
          [SelfLawRow "left" (Left RefusedMultiNameEquation)]
          selfLawRows
        assertEqual "refused multi-name bindings mint no law specs" 0 (length selfLawSpecs)
    ]

noModuleDerivedConfig :: NebulaConfig
noModuleDerivedConfig =
  defaultNebulaConfig {ncAdmissibleTiers = Set.delete ModuleDerived (ncAdmissibleTiers defaultNebulaConfig)}

selfUnfoldWorkload :: ModuleWorkload
selfUnfoldWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/SelfUnfoldFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.SelfUnfoldFixture where",
            "",
            "identity x = x",
            "use = identity payload"
          ],
      mwOracleLookup = OracleMissing []
    }

selfLawRefusalWorkload :: ModuleWorkload
selfLawRefusalWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/SelfLawRefusalFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.SelfLawRefusalFixture where",
            "",
            "constant = payload",
            "self x = self x",
            "expand x = wrap x x"
          ],
      mwOracleLookup = OracleMissing []
    }

syntheticMultiNameModule :: IO ConvertedModule
syntheticMultiNameModule = do
  ingested <- requireIngested selfUnfoldWorkload
  case cmBindings (imConverted ingested) of
    binding : _ ->
      pure
        (imConverted ingested)
          { cmBindings =
              [ binding
                  { tlbNames = fmap unqualifiedName ["left", "right"]
                  }
              ]
          }
    [] ->
      assertFailure "self-unfold fixture unexpectedly produced no bindings"

unqualifiedName :: String -> RdrName
unqualifiedName =
  mkRdrUnqual . mkVarOcc

selfUnfoldRuleIds :: ConvertedModule -> [RewriteRuleId]
selfUnfoldRuleIds convertedModule =
  [ rrId (srsRule (lawRule lawSpec))
  | lawSpec <- selfLawSpecs
  ]
  where
    (_, LawBook selfLawSpecs) =
      hsExprSelfUnfoldLawFamily convertedModule

fmapEvidenceConfig :: NebulaConfig
fmapEvidenceConfig =
  defaultNebulaConfig {ncCorpusSources = SiteFamilyOnly}

fmapEvidenceWorkloadWithoutOracle :: ModuleWorkload
fmapEvidenceWorkloadWithoutOracle =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/FmapEvidenceFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.FmapEvidenceFixture where",
            "",
            "paired xs ys = (fmap inc (fmap dbl xs), fmap inc (fmap dbl ys))"
          ],
      mwOracleLookup = OracleMissing []
    }

fmapEvidenceWorkloadWithOracle :: SourceRegion -> Either PackageUnitParseFailure ModuleWorkload
fmapEvidenceWorkloadWithOracle evidenceRegion =
  (\oracle -> fmapEvidenceWorkloadWithoutOracle {mwOracleLookup = attachedOracle oracle})
    <$> fmapEvidenceOracle evidenceRegion

fmapEvidenceOracle :: SourceRegion -> Either PackageUnitParseFailure ModuleNameOracle
fmapEvidenceOracle evidenceRegion =
  do
    vocabularyUses <- baseVocabularyUses
    functorOrigin <- lawfulFunctorListOrigin
    pure
      ModuleNameOracle
        { mnoSourcePath = "Melusine/Nebula/FmapEvidenceFixture.hs",
          mnoGlobalUses = vocabularyUses,
          mnoEvidenceAtSpan = Map.singleton evidenceRegion (Set.singleton functorOrigin),
          mnoTypeAtSpan = Map.empty
        }

lawfulFunctorListOrigin :: Either PackageUnitParseFailure ResolvedOrigin
lawfulFunctorListOrigin =
  mkResolvedOrigin "base" "GHC.Internal.Base" "$fFunctorList"

lawfulMonadListOrigin :: Either PackageUnitParseFailure ResolvedOrigin
lawfulMonadListOrigin =
  mkResolvedOrigin "base" "GHC.Internal.Base" "$fMonadList"

unlawfulFunctorOrigin :: Either PackageUnitParseFailure ResolvedOrigin
unlawfulFunctorOrigin =
  mkResolvedOrigin "melusine-local" "Melusine.Nebula.LocalFunctor" "$fFunctorWidget"

fmapFusionCandidateRegions :: ConvertedModule -> [SourceRegion]
fmapFusionCandidateRegions convertedModule =
  candidateRegions spannedFmapFusionCandidate convertedModule

fmapIdCandidateRegions :: ConvertedModule -> [SourceRegion]
fmapIdCandidateRegions =
  candidateRegions spannedFmapIdCandidate

bindCandidateRegions :: ConvertedModule -> [SourceRegion]
bindCandidateRegions =
  candidateRegions spannedBindCandidate

plusCandidateRegions :: ConvertedModule -> [SourceRegion]
plusCandidateRegions =
  candidateRegions (spannedOperatorCandidate "+")

candidateRegions :: (SpannedExpr -> Bool) -> ConvertedModule -> [SourceRegion]
candidateRegions predicate convertedModule =
  foldMap (spannedCandidateRegions predicate . tlbSpannedTerm) (cmBindings convertedModule)

spannedCandidateRegions :: (SpannedExpr -> Bool) -> SpannedExpr -> [SourceRegion]
spannedCandidateRegions predicate spannedExpr =
  ownRegion <> foldMap childRegions (sxNode spannedExpr)
  where
    ownRegion =
      [ region
      | Just region <- [sxRegion spannedExpr],
        predicate spannedExpr
      ]

    childRegions =
      spannedCandidateRegions predicate

spannedFmapFusionCandidate :: SpannedExpr -> Bool
spannedFmapFusionCandidate spannedExpr =
  case sxNode spannedExpr of
    AppF functionExpr argumentExpr ->
      spannedFmapHead functionExpr && spannedFmapApplication (stripSpannedParens argumentExpr)
    _ ->
      False

spannedFmapIdCandidate :: SpannedExpr -> Bool
spannedFmapIdCandidate spannedExpr =
  case sxNode spannedExpr of
    AppF functionExpr _ ->
      spannedFmapIdHead functionExpr
    _ ->
      False

spannedFmapIdHead :: SpannedExpr -> Bool
spannedFmapIdHead spannedExpr =
  case sxNode spannedExpr of
    AppF functionExpr argumentExpr ->
      spannedGlobalOcc functionExpr == Just "fmap"
        && spannedGlobalOcc argumentExpr == Just "id"
    _ ->
      False

spannedBindCandidate :: SpannedExpr -> Bool
spannedBindCandidate spannedExpr =
  spannedOperatorCandidate ">>=" spannedExpr

spannedOperatorCandidate :: String -> SpannedExpr -> Bool
spannedOperatorCandidate operatorName spannedExpr =
  case sxNode spannedExpr of
    OpAppF _ operatorExpr _ ->
      spannedGlobalOcc operatorExpr == Just operatorName
    _ ->
      False

spannedFmapApplication :: SpannedExpr -> Bool
spannedFmapApplication spannedExpr =
  case sxNode spannedExpr of
    AppF functionExpr _ ->
      spannedFmapHead functionExpr
    _ ->
      False

spannedFmapHead :: SpannedExpr -> Bool
spannedFmapHead spannedExpr =
  case sxNode spannedExpr of
    AppF functionExpr _ ->
      spannedGlobalOcc functionExpr == Just "fmap"
    _ ->
      False

spannedGlobalOcc :: SpannedExpr -> Maybe String
spannedGlobalOcc spannedExpr =
  case sxNode spannedExpr of
    VarF (GlobalName rdrName) ->
      Just (occNameString (rdrNameOcc rdrName))
    _ ->
      Nothing

stripSpannedParens :: SpannedExpr -> SpannedExpr
stripSpannedParens spannedExpr =
  case sxNode spannedExpr of
    ParF innerExpr ->
      stripSpannedParens innerExpr
    _ ->
      spannedExpr

fmapIdWorkloadWithoutEvidence :: Either PackageUnitParseFailure ModuleWorkload
fmapIdWorkloadWithoutEvidence =
  do
    oracle <- vocabularyOracleFor "Melusine/Nebula/FmapIdFixture.hs"
    pure
      ModuleWorkload
        { mwPath = "Melusine/Nebula/FmapIdFixture.hs",
          mwSource =
            unlines
              [ "module Melusine.Nebula.FmapIdFixture where",
                "",
                "mapped = fmap id xs"
              ],
          mwOracleLookup = attachedOracle oracle
        }

fmapIdWorkloadWithEvidence :: ResolvedOrigin -> SourceRegion -> Either PackageUnitParseFailure ModuleWorkload
fmapIdWorkloadWithEvidence evidenceOrigin evidenceRegion =
  do
    workload <- fmapIdWorkloadWithoutEvidence
    oracle <- vocabularyOracleFor "Melusine/Nebula/FmapIdFixture.hs"
    pure
      workload
        { mwOracleLookup =
            attachedOracle
              oracle
                { mnoEvidenceAtSpan = Map.singleton evidenceRegion (Set.singleton evidenceOrigin)
                }
        }

monadIdentityWorkloadWithoutEvidence :: Either PackageUnitParseFailure ModuleWorkload
monadIdentityWorkloadWithoutEvidence =
  do
    oracle <- vocabularyOracleFor "Melusine/Nebula/MonadIdentityFixture.hs"
    pure
      ModuleWorkload
        { mwPath = "Melusine/Nebula/MonadIdentityFixture.hs",
          mwSource =
            unlines
              [ "module Melusine.Nebula.MonadIdentityFixture where",
                "",
                "monadLeft = return value >>= next",
                "monadRight = action >>= return"
              ],
          mwOracleLookup = attachedOracle oracle
        }

monadIdentityWorkloadWithEvidence :: [SourceRegion] -> Either PackageUnitParseFailure ModuleWorkload
monadIdentityWorkloadWithEvidence evidenceRegions =
  do
    workload <- monadIdentityWorkloadWithoutEvidence
    oracle <- vocabularyOracleFor "Melusine/Nebula/MonadIdentityFixture.hs"
    monadOrigin <- lawfulMonadListOrigin
    pure
      workload
        { mwOracleLookup =
            attachedOracle
              oracle
                { mnoEvidenceAtSpan =
                    Map.fromList [(evidenceRegion, Set.singleton monadOrigin) | evidenceRegion <- evidenceRegions]
                }
        }

plusUnitWorkloadWithoutTypeEvidence :: Either PackageUnitParseFailure ModuleWorkload
plusUnitWorkloadWithoutTypeEvidence =
  do
    oracle <- vocabularyOracleFor "Melusine/Nebula/PlusUnitFixture.hs"
    pure
      ModuleWorkload
        { mwPath = "Melusine/Nebula/PlusUnitFixture.hs",
          mwSource =
            unlines
              [ "module Melusine.Nebula.PlusUnitFixture where",
                "",
                "plusUnit = x + 0"
              ],
          mwOracleLookup = attachedOracle oracle
        }

plusUnitWorkloadWithTypeEvidence :: TypeWords -> SourceRegion -> Either PackageUnitParseFailure ModuleWorkload
plusUnitWorkloadWithTypeEvidence typeWords region =
  do
    workload <- plusUnitWorkloadWithoutTypeEvidence
    oracle <- vocabularyOracleFor "Melusine/Nebula/PlusUnitFixture.hs"
    pure
      workload
        { mwOracleLookup =
            attachedOracle
              oracle
                { mnoTypeAtSpan = Map.singleton region (Set.singleton typeWords)
                }
        }

intTypeWords :: TypeWords
intTypeWords =
  tyConTypeWords "Int"

doubleTypeWords :: TypeWords
doubleTypeWords =
  tyConTypeWords "Double"

mapFusionWorkloadWithoutOracle :: ModuleWorkload
mapFusionWorkloadWithoutOracle =
  mapFusionWorkload Nothing

mapFusionWorkloadWithOracle :: Either PackageUnitParseFailure ModuleWorkload
mapFusionWorkloadWithOracle =
  mapFusionWorkload . Just <$> composeOracle

mapFusionWorkloadWithUserComposition :: Either PackageUnitParseFailure ModuleWorkload
mapFusionWorkloadWithUserComposition =
  mapFusionWorkload . Just <$> userCompositionOracle

mapFusionWorkload :: Maybe ModuleNameOracle -> ModuleWorkload
mapFusionWorkload maybeOracle =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/MapFusionFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.MapFusionFixture where",
            "",
            "incDouble xs = map inc (map dbl xs)"
          ],
      mwOracleLookup = maybe (OracleMissing []) attachedOracle maybeOracle
    }

reverseInvolutionWorkloadWithOracle :: Either PackageUnitParseFailure ModuleWorkload
reverseInvolutionWorkloadWithOracle =
  reverseInvolutionWorkload . Just <$> vocabularyOracleFor "Melusine/Nebula/ReverseFixture.hs"

reverseInvolutionWorkloadWithShadow :: Either PackageUnitParseFailure ModuleWorkload
reverseInvolutionWorkloadWithShadow =
  reverseInvolutionWorkload . Just <$> reverseShadowOracle

reverseInvolutionWorkload :: Maybe ModuleNameOracle -> ModuleWorkload
reverseInvolutionWorkload maybeOracle =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/ReverseFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.ReverseFixture where",
            "",
            "sourceList = input",
            "rev = reverse (reverse sourceList)"
          ],
      mwOracleLookup = maybe (OracleMissing []) attachedOracle maybeOracle
    }

mapAppendWorkloadWithOracle :: Either PackageUnitParseFailure ModuleWorkload
mapAppendWorkloadWithOracle =
  do
    oracle <- vocabularyOracleFor "Melusine/Nebula/MapAppendFixture.hs"
    pure
      ModuleWorkload
        { mwPath = "Melusine/Nebula/MapAppendFixture.hs",
          mwSource =
            unlines
              [ "module Melusine.Nebula.MapAppendFixture where",
                "",
                "mapAppend f xs ys = map f xs ++ map f ys"
              ],
          mwOracleLookup = attachedOracle oracle
        }

attachedOracle :: ModuleNameOracle -> OracleLookup
attachedOracle oracle =
  OracleFound GivenPathKey (mnoSourcePath oracle) oracle

vocabularyOracleFor :: FilePath -> Either PackageUnitParseFailure ModuleNameOracle
vocabularyOracleFor sourcePath =
  (\oracle -> oracle {mnoSourcePath = sourcePath})
    <$> baseVocabularyOracle

userCompositionOracle :: Either PackageUnitParseFailure ModuleNameOracle
userCompositionOracle =
  do
    vocabularyUses <- baseVocabularyUses
    localComposition <- mkResolvedOrigin "melusine-local" "Melusine.Nebula.LocalOperators" "."
    pure
      ModuleNameOracle
        { mnoSourcePath = "Melusine/Nebula/MapFusionFixture.hs",
          mnoGlobalUses =
            Map.insert
              "."
              (Set.singleton localComposition)
              vocabularyUses,
          mnoEvidenceAtSpan = Map.empty,
          mnoTypeAtSpan = Map.empty
        }

reverseShadowOracle :: Either PackageUnitParseFailure ModuleNameOracle
reverseShadowOracle =
  do
    vocabularyUses <- baseVocabularyUses
    localReverse <- mkResolvedOrigin "melusine-local" "Melusine.Nebula.LocalOperators" "reverse"
    pure
      ModuleNameOracle
        { mnoSourcePath = "Melusine/Nebula/ReverseFixture.hs",
          mnoGlobalUses =
            Map.insert
              "reverse"
              (Set.singleton localReverse)
              vocabularyUses,
          mnoEvidenceAtSpan = Map.empty,
          mnoTypeAtSpan = Map.empty
        }

fixtureWorkload :: ModuleWorkload
fixtureWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/Fixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.Fixture where",
            "",
            "etaSite = \\evt -> processEvent evt",
            "letSite = let conn = getConnection in query conn",
            "shareLeft = combine (transform alpha) (transform alpha)",
            "shareRight = combine (transform beta) (transform beta)"
          ],
      mwOracleLookup = OracleMissing []
    }

parErasureWorkload :: ModuleWorkload
parErasureWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/ParErasureFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.ParErasureFixture where",
            "",
            "wrapped = (((payload)))"
          ],
      mwOracleLookup = OracleMissing []
    }

triClusterWorkload :: ModuleWorkload
triClusterWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/TriCluster.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.TriCluster where",
            "",
            "triLeft = combine (transform alpha) (transform alpha)",
            "triMid = combine (transform beta) (transform beta)",
            "triRight = combine (transform gamma) (transform gamma)"
          ],
      mwOracleLookup = OracleMissing []
    }

reorderedCaseWorkload :: ModuleWorkload
reorderedCaseWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/ReorderedCase.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.ReorderedCase where",
            "",
            "caseLeft x = case x of { Just value -> wrap value; Nothing -> zero }",
            "caseRight x = case x of { Nothing -> zero; Just item -> wrap item }"
          ],
      mwOracleLookup = OracleMissing []
    }

reorderedRecordWorkload :: ModuleWorkload
reorderedRecordWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/ReorderedRecord.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.ReorderedRecord where",
            "",
            "recordLeft = wrap MkThing { fieldA = alpha, fieldB = beta }",
            "recordRight = wrap MkThing { fieldB = delta, fieldA = gamma }"
          ],
      mwOracleLookup = OracleMissing []
    }

constructorCaseWorkload :: ModuleWorkload
constructorCaseWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/ConstructorCaseFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.ConstructorCaseFixture where",
            "",
            "classify x = case x of { Just v -> v; Nothing -> 0 }"
          ],
      mwOracleLookup = OracleMissing []
    }

multiClauseWorkload :: ModuleWorkload
multiClauseWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/MultiClauseFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.MultiClauseFixture where",
            "",
            "multi 0 = zero",
            "multi n = use n",
            "multi m = keep m"
          ],
      mwOracleLookup = OracleMissing []
    }

patternWhereWorkload :: ModuleWorkload
patternWhereWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/PatternWhereFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.PatternWhereFixture where",
            "",
            "patternWhere x = combine y y where",
            "  (y, kept) = splitPair x"
          ],
      mwOracleLookup = OracleMissing []
    }

termHasOpaque :: Pattern HsExprF -> Bool
termHasOpaque = \case
  PatternVar _ ->
    False
  PatternNode (OpaqueF _) ->
    True
  PatternNode nodeValue ->
    any termHasOpaque nodeValue

termHasClauses :: Pattern HsExprF -> Bool
termHasClauses = \case
  PatternVar _ ->
    False
  PatternNode (ClausesF _) ->
    True
  PatternNode nodeValue ->
    any termHasClauses nodeValue

termHasTupleLocalBind :: Pattern HsExprF -> Bool
termHasTupleLocalBind = \case
  PatternVar _ ->
    False
  PatternNode nodeValue ->
    ownTupleLocalBind nodeValue || any termHasTupleLocalBind nodeValue
  where
    ownTupleLocalBind :: HsExprF r -> Bool
    ownTupleLocalBind = \case
      LetF _ bindingValues _ ->
        any (patternHasTuple . fst) bindingValues
      DoF statementValues ->
        any statementHasTupleLocalBind statementValues
      GuardedF alternativeValues ->
        any guardedAltHasTupleLocalBind alternativeValues
      MultiIfF alternativeValues ->
        any guardedAltHasTupleLocalBind alternativeValues
      _ ->
        False

    statementHasTupleLocalBind :: HsStmtF r -> Bool
    statementHasTupleLocalBind = \case
      BindStmtF {} -> False
      BodyStmtF {} -> False
      LetStmtF _ bindingValues -> any (patternHasTuple . fst) bindingValues

    guardedAltHasTupleLocalBind :: GuardedAltF r -> Bool
    guardedAltHasTupleLocalBind (GuardedAltF guardValues _) =
      any guardHasTupleLocalBind guardValues

    guardHasTupleLocalBind :: HsGuardStmtF r -> Bool
    guardHasTupleLocalBind = \case
      GuardBoolF {} -> False
      GuardPatF {} -> False
      GuardLetF _ bindingValues -> any (patternHasTuple . fst) bindingValues

patternHasTuple :: HsPatF -> Bool
patternHasTuple = \case
  PConP _ subPatterns -> any patternHasTuple subPatterns
  PTupleP _ -> True
  PListP subPatterns -> any patternHasTuple subPatterns
  PRecP _ fieldPatterns -> any (patternHasTuple . snd) fieldPatterns
  PAsP _ subPattern -> patternHasTuple subPattern
  PBangP subPattern -> patternHasTuple subPattern
  PLazyP subPattern -> patternHasTuple subPattern
  PParP subPattern -> patternHasTuple subPattern
  _ -> False

termAltPatterns :: Pattern HsExprF -> [HsPatF]
termAltPatterns = \case
  PatternVar _ ->
    []
  PatternNode nodeValue ->
    ownAltPatterns nodeValue <> foldMap termAltPatterns nodeValue
  where
    ownAltPatterns :: HsExprF r -> [HsPatF]
    ownAltPatterns = \case
      CaseF _ branchValues ->
        fmap fst branchValues
      _ ->
        []

isConPattern :: HsPatF -> Bool
isConPattern = \case
  PConP _ _ ->
    True
  PRecP _ _ ->
    True
  _ ->
    False

bindingRootRowsPresent :: IngestedModule -> Bool
bindingRootRowsPresent ingested =
  all
    (`elem` imSpanRows ingested)
    [ SpanClassRow region seedClass
    | (bindingValue, seedClass) <- zip (cmBindings (imConverted ingested)) (imSeedClasses ingested),
      Just region <- [tlbRegion bindingValue]
    ]

brokenWorkload :: ModuleWorkload
brokenWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/Broken.hs",
      mwSource = "module Melusine.Nebula.Broken where\n\nbroken = (\n",
      mwOracleLookup = OracleMissing []
    }

requireIngested :: ModuleWorkload -> IO IngestedModule
requireIngested workload =
  case ingestModule workload of
    Left ingestFailure ->
      assertFailure ("ingest failed: " <> show ingestFailure)
    Right ingested ->
      pure ingested

requireSingleRegion :: String -> (ConvertedModule -> [SourceRegion]) -> ModuleWorkload -> IO SourceRegion
requireSingleRegion label regionSelector workload = do
  ingested <- requireIngested workload
  case regionSelector (imConverted ingested) of
    [region] ->
      pure region
    regions ->
      assertFailure (label <> " expected exactly one region, got " <> show regions)

requireRight :: Show failure => String -> Either failure stage -> IO stage
requireRight stageName =
  either
    (\stageFailure -> assertFailure (stageName <> " failed: " <> show stageFailure))
    pure

requireImproved :: ModuleWorkload -> IO ModuleImprovement
requireImproved workload =
  requireImprovedWith defaultNebulaConfig workload

requireImprovedWith :: NebulaConfig -> ModuleWorkload -> IO ModuleImprovement
requireImprovedWith config workload =
  either
    (\(modulePath, moduleFailure) -> assertFailure ("improve failed for " <> modulePath <> ": " <> show moduleFailure))
    pure
    (improveModule config workload)

requireSealed :: ModuleImprovement -> IO SealedSource
requireSealed improvement =
  case miSeal improvement of
    Sealed sealedSource ->
      pure sealedSource
    otherOutcome ->
      assertFailure ("expected sealed source, got: " <> show otherOutcome)

certificateLawTiers :: String -> ModuleImprovement -> Set.Set (LawId, TrustTier)
certificateLawTiers bindingName improvement =
  Set.fromList
    [ (lsLaw stamp, lsTier stamp)
    | certificate <- miCertificates improvement,
      hcBinding certificate == bindingName,
      entry <- hcEntries certificate,
      Just stamp <- [npStamp (peProvenance entry)]
    ]

lawTableLawTiers :: RuleCorpus -> Set.Set (LawId, TrustTier)
lawTableLawTiers corpus =
  Set.fromList
    [ (lsLaw stamp, lsTier stamp)
    | stamp <- Map.elems (rcLawTable corpus)
    ]

certificateLawFacts :: String -> ModuleImprovement -> Set.Set (LawId, Bool, Bool)
certificateLawFacts bindingName improvement =
  Set.fromList
    [ (lsLaw stamp, npGuarded provenance, npFactful provenance)
    | certificate <- miCertificates improvement,
      hcBinding certificate == bindingName,
      entry <- hcEntries certificate,
      let provenance = peProvenance entry,
      Just stamp <- [npStamp provenance]
    ]

requireSaturatedFixture :: IO (IngestedModule, SaturatedModule)
requireSaturatedFixture = do
  (ingested, _, saturated) <- requireSaturatedFixtureWithCorpus
  pure (ingested, saturated)

requireSaturatedFixtureWithCorpus :: IO (IngestedModule, RuleCorpus, SaturatedModule)
requireSaturatedFixtureWithCorpus = do
  ingested <- requireIngested fixtureWorkload
  corpus <- requireRight "corpus derivation" (deriveRuleCorpus defaultNebulaConfig (imSpanRows ingested) (workloadOracle fixtureWorkload) (imConverted ingested))
  saturated <- requireRight "saturation" (saturateModule defaultSaturationOptions defaultNebulaConfig ingested corpus)
  pure (ingested, corpus, saturated)

sizeSectionsFor :: NebulaConfig -> IngestedModule -> SaturatedModule -> IO NebulaSizeExtractionSections
sizeSectionsFor config ingested saturated =
  requireRight
    "size extraction sections"
    (sizeExtractionSections config (smContextGraph saturated) (harvestContexts ingested))

requireSynthesisOutcome :: ModuleWorkload -> IO ([ChosenBinding], SynthesisOutcome)
requireSynthesisOutcome workload = do
  (_, _, _, preBindings, outcome) <- requireSynthesisRun workload
  pure (preBindings, outcome)

requireSynthesisRun :: ModuleWorkload -> IO (IngestedModule, RuleCorpus, SaturatedModule, [ChosenBinding], SynthesisOutcome)
requireSynthesisRun workload = do
  ingested <- requireIngested workload
  corpus <- requireRight "corpus derivation" (deriveRuleCorpus defaultNebulaConfig (imSpanRows ingested) (workloadOracle workload) (imConverted ingested))
  saturated <- requireRight "saturation" (saturateModule defaultSaturationOptions defaultNebulaConfig ingested corpus)
  harvest <- requireRight "harvest" (buildHarvest defaultNebulaConfig ingested saturated)
  outcome <-
    requireRight
      "synthesis"
      (synthesizeAbstractions defaultNebulaConfig ingested corpus saturated harvest)
  pure (ingested, corpus, saturated, hsBindings harvest, outcome)

requireChosenWith :: NebulaConfig -> ModuleWorkload -> IO [ChosenBinding]
requireChosenWith config workload = do
  ingested <- requireIngested workload
  corpus <- requireRight "corpus derivation" (deriveRuleCorpus config (imSpanRows ingested) (workloadOracle workload) (imConverted ingested))
  saturated <- requireRight "saturation" (saturateModule defaultSaturationOptions config ingested corpus)
  sizeSections <- sizeSectionsFor config ingested saturated
  requireRight "binding extraction" (chooseBindings config ingested saturated sizeSections)

siteOnlyConfig :: NebulaConfig
siteOnlyConfig =
  defaultNebulaConfig {ncCorpusSources = SiteFamilyOnly}

noSharingWorkload :: ModuleWorkload
noSharingWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/NoSharing.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.NoSharing where",
            "",
            "soloAlpha = alpha",
            "soloBeta = beta gamma"
          ],
      mwOracleLookup = OracleMissing []
    }

thinSharingWorkload :: ModuleWorkload
thinSharingWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/ThinSharing.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.ThinSharing where",
            "",
            "thinLeft = wrap (inc alpha)",
            "thinRight = wrap (inc beta)"
          ],
      mwOracleLookup = OracleMissing []
    }

tinyDuplicateWorkload :: ModuleWorkload
tinyDuplicateWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/TinyDuplicate.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.TinyDuplicate where",
            "",
            "sortedLeft minor = IntSet.toAscList (IntSet.fromList (V.toList (pmRows minor)))",
            "sortedRight minor = IntSet.toAscList (IntSet.fromList (V.toList (pmCols minor)))"
          ],
      mwOracleLookup = OracleMissing []
    }

projectionVectorWorkload :: ModuleWorkload
projectionVectorWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/ProjectionVector.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.ProjectionVector where",
            "",
            "profileRunner stage fixture = forceEitherWith checksumProfile (profileSummary stage (bfAmbientPoset fixture) (bfSourceDerived fixture) (bfSecondaryDerived fixture))",
            "tensorRunner fixture = forceEitherWith checksumTensor (tensorProduct (bfAmbientPoset fixture) (bfSourceDerived fixture) (bfSecondaryDerived fixture))",
            "homRunner fixture = forceEitherWith checksumTensor (internalHom (bfAmbientPoset fixture) (bfSourceDerived fixture) (bfSecondaryDerived fixture))"
          ],
      mwOracleLookup = OracleMissing []
    }

recordProjectionOwnershipWorkload :: ModuleWorkload
recordProjectionOwnershipWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/RecordProjectionOwnership.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.RecordProjectionOwnership where",
            "",
            "data CandidateSite = CandidateSite",
            "data AbstractionCandidate = AbstractionCandidate",
            "  { acLeftSite :: CandidateSite",
            "  , acRightSite :: CandidateSite",
            "  , acLeftName :: String",
            "  , acRightName :: String",
            "  , acLeftContext :: Int",
            "  , acRightContext :: Int",
            "  , acLeftClass :: Int",
            "  , acRightClass :: Int",
            "  }",
            "",
            "sharedCandidates leftSite rightSite =",
            "  let candidateFrom left right =",
            "        AbstractionCandidate",
            "          { acLeftSite = left",
            "          , acRightSite = right",
            "          , acLeftName = csBindingName left",
            "          , acRightName = csBindingName right",
            "          , acLeftContext = csContext left",
            "          , acRightContext = csContext right",
            "          , acLeftClass = csClass left",
            "          , acRightClass = csClass right",
            "          }",
            "  in candidateFrom leftSite rightSite"
          ],
      mwOracleLookup = OracleMissing []
    }

recordFactHarvestWorkload :: ModuleWorkload
recordFactHarvestWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/RecordFactHarvest.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.RecordFactHarvest where",
            "",
            "data CacheRow = CacheRow",
            "  { crOwner :: Owner",
            "  , crCachedName :: String",
            "  }",
            "",
            "mkCache owner =",
            "  CacheRow",
            "    { crOwner = owner",
            "    , crCachedName = Qualified.ownerName owner",
            "    }",
            "",
            "readCache row = crCachedName row",
            "",
            "instance Show CacheRow where",
            "  show row = show (crCachedName row)"
          ],
      mwOracleLookup = OracleMissing []
    }

staleDerivedFieldSingleWorkload :: ModuleWorkload
staleDerivedFieldSingleWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/StaleDerivedField.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.StaleDerivedField where",
            "",
            "data CacheRow = CacheRow",
            "  { crOwner :: Owner",
            "  , crCachedName :: String",
            "  , crPayload :: Int",
            "  }",
            "",
            "mkCache owner payload =",
            "  CacheRow",
            "    { crOwner = owner",
            "    , crCachedName = Qualified.ownerName owner",
            "    , crPayload = payload",
            "    }",
            "",
            "summarize row = combine (crCachedName row) (crPayload row)"
          ],
      mwOracleLookup = OracleMissing []
    }

genericRecordSkeletonWorkload :: ModuleWorkload
genericRecordSkeletonWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/GenericRecordSkeleton.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.GenericRecordSkeleton where",
            "",
            "data CacheRow = CacheRow",
            "  { crOwner :: Owner",
            "  , crCachedName :: String",
            "  , crPayload :: Int",
            "  }",
            "",
            "cacheLeft owner payload =",
            "  CacheRow",
            "    { crOwner = owner",
            "    , crCachedName = ownerName owner",
            "    , crPayload = payload",
            "    }",
            "",
            "cacheRight payload owner =",
            "  CacheRow",
            "    { crPayload = payload",
            "    , crCachedName = ownerName owner",
            "    , crOwner = owner",
            "    }"
          ],
      mwOracleLookup = OracleMissing []
    }

foldSkeletonWorkload :: ModuleWorkload
foldSkeletonWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/FoldSkeleton.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.FoldSkeleton where",
            "",
            "clearBelow pivotRow pivotColumn pivotInverse matrixValue =",
            "  foldM clearRow matrixValue (indexRangeFromTo (pivotRow + 1) (dmRows matrixValue))",
            "  where",
            "    clearRow accumulated rowIndex = do",
            "      entryValue <- denseIndex \"below entry\" accumulated rowIndex pivotColumn",
            "      if isZero entryValue",
            "        then Right accumulated",
            "        else addScaledDenseRow \"below row\" rowIndex (neg (entryValue `mul` pivotInverse)) pivotRow accumulated",
            "",
            "clearInverse context matrixSize pivotIndex workMatrix inverseMatrix =",
            "  foldM clearRow (workMatrix, inverseMatrix) (indexRange matrixSize)",
            "  where",
            "    clearRow (workAccumulated, inverseAccumulated) rowIndex = do",
            "      entryValue <- denseIndex (context <> \" entry\") workAccumulated rowIndex pivotIndex",
            "      if isZero entryValue",
            "        then Right (workAccumulated, inverseAccumulated)",
            "        else do",
            "          nextWork <- addScaledDenseRow (context <> \" work\") rowIndex (neg entryValue) pivotIndex workAccumulated",
            "          nextInverse <- addScaledDenseRow (context <> \" inverse\") rowIndex (neg entryValue) pivotIndex inverseAccumulated",
            "          Right (nextWork, nextInverse)"
          ],
      mwOracleLookup = OracleMissing []
    }

eitherValidationWorkload :: ModuleWorkload
eitherValidationWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/EitherValidation.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.EitherValidation where",
            "",
            "finitePlain value",
            "  | isNaN value = Left (InvariantViolation \"NaN\")",
            "  | isInfinite value = Left (InvariantViolation \"Infinite\")",
            "  | otherwise = Right (normalizeNegativeZero value)",
            "",
            "finiteLabel domainLabel value",
            "  | isNaN value = Left (InvariantViolation (domainLabel <> \" must not be NaN\"))",
            "  | isInfinite value = Left (InvariantViolation (domainLabel <> \" must be finite\"))",
            "  | otherwise = Right (normalizeNegativeZero value)",
            "",
            "finiteWith errorValue wrapValue value =",
            "  case finitePlain value of",
            "    Left _ -> Left (errorValue value)",
            "    Right finiteValue -> Right (wrapValue finiteValue)",
            "",
            "positiveLabel domainLabel value =",
            "  finiteLabel domainLabel value >>= \\finiteValue ->",
            "    if finiteValue <= 0",
            "      then Left (InvariantViolation (domainLabel <> \" must be positive\"))",
            "      else Right finiteValue",
            "",
            "positiveWith errorValue wrapValue value =",
            "  case finiteWith errorValue (\\finiteValue -> finiteValue) value of",
            "    Left validationError -> Left validationError",
            "    Right finiteValue ->",
            "      if finiteValue <= 0",
            "        then Left (errorValue value)",
            "        else Right (wrapValue finiteValue)"
          ],
      mwOracleLookup = OracleMissing []
    }

letRowsProtocolWorkload :: ModuleWorkload
letRowsProtocolWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/LetRowsProtocol.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.LetRowsProtocol where",
            "",
            "letExpr resolveClass binders stateValue letModeValue leftRows rightRows leftBody rightBody = do",
            "  (matchedRows, bodyBinders) <- matchBindingRowPatterns binders leftRows rightRows",
            "  let rhsBinders =",
            "        case lmRecursion letModeValue of",
            "          NonRecursiveBinds -> binders",
            "          RecursiveOpaqueBinds -> bodyBinders",
            "  (rhsRows, stateAfterRows) <- alphaUnifyBindingRhsRows resolveClass rhsBinders stateValue matchedRows",
            "  (bodyPattern, stateAfterBody) <- alphaUnifyTerm resolveClass bodyBinders stateAfterRows leftBody rightBody",
            "  AlphaMatched (LetF letModeValue rhsRows bodyPattern, stateAfterBody)",
            "",
            "letStmt resolveClass binders stateValue letModeValue leftRows rightRows = do",
            "  (matchedRows, nextBinders) <- matchBindingRowPatterns binders leftRows rightRows",
            "  let rhsBinders =",
            "        case lmRecursion letModeValue of",
            "          NonRecursiveBinds -> binders",
            "          RecursiveOpaqueBinds -> nextBinders",
            "  (rhsRows, nextState) <- alphaUnifyBindingRhsRows resolveClass rhsBinders stateValue matchedRows",
            "  AlphaMatched (LetStmtF letModeValue rhsRows, nextBinders, nextState)",
            "",
            "guardStmt resolveClass binders stateValue leftGuard rightGuard =",
            "  case (leftGuard, rightGuard) of",
            "    (GuardLetF leftMode leftRows, GuardLetF rightMode rightRows)",
            "      | leftMode == rightMode -> do",
            "          (matchedRows, nextBinders) <- matchBindingRowPatterns binders leftRows rightRows",
            "          let rhsBinders =",
            "                case lmRecursion leftMode of",
            "                  NonRecursiveBinds -> binders",
            "                  RecursiveOpaqueBinds -> nextBinders",
            "          (rhsRows, nextState) <- alphaUnifyBindingRhsRows resolveClass rhsBinders stateValue matchedRows",
            "          AlphaMatched (GuardLetF leftMode rhsRows, nextBinders, nextState)",
            "    _ -> AlphaMismatch"
          ],
      mwOracleLookup = OracleMissing []
    }

patternBindRhsProtocolWorkload :: ModuleWorkload
patternBindRhsProtocolWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/PatternBindRhsProtocol.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.PatternBindRhsProtocol where",
            "",
            "bindStmt resolveClass binders stateValue leftStatement rightStatement =",
            "  case (leftStatement, rightStatement) of",
            "    (BindStmtF leftPattern leftRhs, BindStmtF rightPattern rightRhs) -> do",
            "      (statementPattern, nextBinders) <- matchPattern binders leftPattern rightPattern",
            "      (rhsPattern, nextState) <- alphaUnifyTerm resolveClass binders stateValue leftRhs rightRhs",
            "      AlphaMatched (BindStmtF statementPattern rhsPattern, nextBinders, nextState)",
            "    _ -> AlphaMismatch",
            "",
            "guardPat resolveClass binders stateValue leftGuard rightGuard =",
            "  case (leftGuard, rightGuard) of",
            "    (GuardPatF leftPattern leftRhs, GuardPatF rightPattern rightRhs) -> do",
            "      (guardPattern, nextBinders) <- matchPattern binders leftPattern rightPattern",
            "      (rhsPattern, nextState) <- alphaUnifyTerm resolveClass binders stateValue leftRhs rightRhs",
            "      AlphaMatched (GuardPatF guardPattern rhsPattern, nextBinders, nextState)",
            "    _ -> AlphaMismatch"
          ],
      mwOracleLookup = OracleMissing []
    }

keyedRowAlignmentProtocolWorkload :: ModuleWorkload
keyedRowAlignmentProtocolWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/KeyedRowAlignmentProtocol.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.KeyedRowAlignmentProtocol where",
            "",
            "fieldRows resolveClass binders stateValue leftFields rightFields = do",
            "  fieldPairs <- zipEqual (sortOn fst leftFields) (sortOn fst rightFields)",
            "  fmap swapAccumResult $",
            "    mapAccumM",
            "      (\\currentState ((leftField, leftTerm), (rightField, rightTerm)) ->",
            "        if leftField == rightField",
            "          then fmap (\\(fieldPattern, nextState) -> (nextState, (leftField, fieldPattern))) (alphaUnifyTerm resolveClass binders currentState leftTerm rightTerm)",
            "          else AlphaMismatch)",
            "      stateValue",
            "      fieldPairs",
            "",
            "patternFields binders leftFields rightFields = do",
            "  fieldPairs <- zipEqual (sortOn fst leftFields) (sortOn fst rightFields)",
            "  fmap swapAccumResult $",
            "    mapAccumM",
            "      (\\currentBinders ((leftField, leftPattern), (rightField, rightPattern)) ->",
            "        if leftField == rightField",
            "          then do",
            "            (fieldPattern, nextBinders) <- matchPattern currentBinders leftPattern rightPattern",
            "            AlphaMatched (nextBinders, (leftField, fieldPattern))",
            "          else AlphaMismatch)",
            "      binders",
            "      fieldPairs"
          ],
      mwOracleLookup = OracleMissing []
    }

arityChildUnifierProtocolWorkload :: ModuleWorkload
arityChildUnifierProtocolWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/ArityChildUnifierProtocol.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.ArityChildUnifierProtocol where",
            "",
            "nodeChildren resolveClass binders stateValue leftNode rightNode =",
            "  case (leftNode, rightNode) of",
            "    (ParF leftA, ParF rightA) -> unifyUnary ParF stateValue leftA rightA",
            "    (AppF leftA leftB, AppF rightA rightB) -> unifyBinary AppF stateValue leftA leftB rightA rightB",
            "    (IfF leftA leftB leftC, IfF rightA rightB rightC) -> unifyTernary IfF stateValue leftA leftB leftC rightA rightB rightC",
            "    _ -> AlphaMismatch",
            "  where",
            "    unifyUnary makeNode currentState leftA rightA = do",
            "      (patternA, stateAfterA) <- alphaUnifyTerm resolveClass binders currentState leftA rightA",
            "      AlphaMatched (makeNode patternA, stateAfterA)",
            "    unifyBinary makeNode currentState leftA leftB rightA rightB = do",
            "      (patternA, stateAfterA) <- alphaUnifyTerm resolveClass binders currentState leftA rightA",
            "      (patternB, stateAfterB) <- alphaUnifyTerm resolveClass binders stateAfterA leftB rightB",
            "      AlphaMatched (makeNode patternA patternB, stateAfterB)",
            "    unifyTernary makeNode currentState leftA leftB leftC rightA rightB rightC = do",
            "      (patternA, stateAfterA) <- alphaUnifyTerm resolveClass binders currentState leftA rightA",
            "      (patternB, stateAfterB) <- alphaUnifyTerm resolveClass binders stateAfterA leftB rightB",
            "      (patternC, stateAfterC) <- alphaUnifyTerm resolveClass binders stateAfterB leftC rightC",
            "      AlphaMatched (makeNode patternA patternB patternC, stateAfterC)",
            "",
            "arithSeqChildren resolveClass binders stateValue leftNode rightNode =",
            "  case (leftNode, rightNode) of",
            "    (ArithSeqFrom leftA, ArithSeqFrom rightA) -> wrap ArithSeqFrom stateValue leftA rightA",
            "    (ArithSeqFromThen leftA leftB, ArithSeqFromThen rightA rightB) -> wrap2 ArithSeqFromThen stateValue leftA leftB rightA rightB",
            "    (ArithSeqFromThenTo leftA leftB leftC, ArithSeqFromThenTo rightA rightB rightC) -> wrap3 ArithSeqFromThenTo stateValue leftA leftB leftC rightA rightB rightC",
            "    _ -> AlphaMismatch",
            "  where",
            "    wrap makeSeq currentState leftA rightA = do",
            "      (patternA, stateAfterA) <- alphaUnifyTerm resolveClass binders currentState leftA rightA",
            "      AlphaMatched (ArithSeqF (makeSeq patternA), stateAfterA)",
            "    wrap2 makeSeq currentState leftA leftB rightA rightB = do",
            "      (patternA, stateAfterA) <- alphaUnifyTerm resolveClass binders currentState leftA rightA",
            "      (patternB, stateAfterB) <- alphaUnifyTerm resolveClass binders stateAfterA leftB rightB",
            "      AlphaMatched (ArithSeqF (makeSeq patternA patternB), stateAfterB)",
            "    wrap3 makeSeq currentState leftA leftB leftC rightA rightB rightC = do",
            "      (patternA, stateAfterA) <- alphaUnifyTerm resolveClass binders currentState leftA rightA",
            "      (patternB, stateAfterB) <- alphaUnifyTerm resolveClass binders stateAfterA leftB rightB",
            "      (patternC, stateAfterC) <- alphaUnifyTerm resolveClass binders stateAfterB leftC rightC",
            "      AlphaMatched (ArithSeqF (makeSeq patternA patternB patternC), stateAfterC)"
          ],
      mwOracleLookup = OracleMissing []
    }

ingestCases :: TestTree
ingestCases =
  testGroup
    "nebula.pipeline.ingest"
    [ testCase "fixture module ingests with coherent binding rows" $ do
        ingested <- requireIngested fixtureWorkload
        assertEqual
          "binding names follow source order"
          ["etaSite", "letSite", "shareLeft", "shareRight"]
          (imBindingNames ingested)
        let rowCount = length (imBindingNames ingested)
        assertEqual "binding contexts align with names" rowCount (length (imBindingContexts ingested))
        assertEqual "seed classes align with names" rowCount (length (imSeedClasses ingested))
        assertEqual "original sizes align with names" rowCount (length (imOriginalSizes ingested))
        assertBool "every original term has at least one node" (all (>= 1) (imOriginalSizes ingested))
        assertEqual
          "insertion metrics count the same bindings"
          rowCount
          (himBindingCount (imInsertionMetrics ingested))
        assertBool
          "binding root regions are recorded against seed classes"
          (bindingRootRowsPresent ingested),
      testCase "constructor-pattern case alternatives convert faithfully without opaque fallout" $ do
        ingested <- requireIngested constructorCaseWorkload
        bindingTerm <-
          case cmBindings (imConverted ingested) of
            [bindingValue] ->
              pure (tlbTerm bindingValue)
            otherBindings ->
              assertFailure ("expected exactly one converted binding, got " <> show (length otherBindings))
        assertBool
          "the constructor-pattern binding contains no opaque node"
          (not (termHasOpaque bindingTerm))
        assertBool
          "the case alternatives include a faithful constructor pattern"
          (any isConPattern (termAltPatterns bindingTerm)),
      testCase "multi-clause definitions convert visibly without changing binding row cardinality" $ do
        ingested <- requireIngested multiClauseWorkload
        assertEqual
          "the source name owns exactly one binding row"
          ["multi"]
          (imBindingNames ingested)
        let bindingRows = cmBindings (imConverted ingested)
        assertEqual
          "conversion preserves the one-row-per-name invariant"
          (length (imBindingNames ingested))
          (length bindingRows)
        bindingTerm <-
          case bindingRows of
            [bindingValue] ->
              pure (tlbTerm bindingValue)
            otherBindings ->
              assertFailure ("expected exactly one converted binding, got " <> show (length otherBindings))
        assertBool
          "the multi-clause binding contains no opaque node"
          (not (termHasOpaque bindingTerm))
        assertBool
          "the multi-clause binding is visible as ClausesF"
          (termHasClauses bindingTerm),
      testCase "pattern-bind where groups convert visibly without opaque fallout" $ do
        ingested <- requireIngested patternWhereWorkload
        assertEqual
          "the source name owns exactly one binding row"
          ["patternWhere"]
          (imBindingNames ingested)
        bindingTerm <-
          case cmBindings (imConverted ingested) of
            [bindingValue] ->
              pure (tlbTerm bindingValue)
            otherBindings ->
              assertFailure ("expected exactly one converted binding, got " <> show (length otherBindings))
        assertBool
          "the pattern-bind where binding contains no opaque node"
          (not (termHasOpaque bindingTerm))
        assertBool
          "the tuple-pattern where binding is visible as a local bind row"
          (termHasTupleLocalBind bindingTerm),
      testCase "malformed source lands in the parse channel" $
        case ingestModule brokenWorkload of
          Left (NebulaParseError _) ->
            pure ()
          Left otherFailure ->
            assertFailure ("expected a parse error, got: " <> show otherFailure)
          Right _ ->
            assertFailure "expected a parse error, got an ingested module"
    ]

spanCases :: TestTree
spanCases =
  testGroup
    "nebula.spans"
    [ testCase "binding root regions map to seed classes" $ do
        ingested <- requireIngested fixtureWorkload
        assertBool "binding root regions are recorded against seed classes" (bindingRootRowsPresent ingested)
    ]

corpusCases :: TestTree
corpusCases =
  testGroup
    "nebula.pipeline.corpus"
    [ testCase "law books compose without changing entry order" $ do
        let firstLaw =
              LawSpec
                { lawId = mkLawId 1,
                  lawTier = ParserVerified,
                  lawFidelity = Observational,
                  lawOracle = NoOracleRequired,
                  lawRule = "alpha"
                }
            secondLaw =
              LawSpec
                { lawId = mkLawId 2,
                  lawTier = RegistryTrusted,
                  lawFidelity = UpToBottom,
                  lawOracle = NoOracleRequired,
                  lawRule = "beta"
                }
            firstBook = LawBook [firstLaw]
            secondBook = LawBook [secondLaw]
        assertEqual "right identity" firstBook (firstBook <> mempty)
        assertEqual "left identity" firstBook (mempty <> firstBook)
        assertEqual "ordered append" (LawBook [firstLaw, secondLaw]) (firstBook <> secondBook),
      testCase "finalized corpus compiles exactly the authored rule and fact identities" $ do
        ingested <- requireIngested fixtureWorkload
        corpus <-
          requireRight
            "compiled corpus"
            (deriveRuleCorpus defaultNebulaConfig (imSpanRows ingested) Nothing (imConverted ingested))
        let compiledProgram = rcCompiledProgram corpus
            rawRuleIds =
              sort (fmap (rrId . srsRule) (supportedRules (rcRuleBook corpus)))
            compiledRuleIds =
              sort (Map.keys (spSupportedRewriteRules compiledProgram))
            rawFactIds =
              sort (fmap (frId . sfsRule) (supportedFactSpecs (rcFactBook corpus)))
            compiledFactIds =
              sort (fmap (cfrId . sirRule) (spSupportedFactRules compiledProgram))
        assertEqual "compiled rewrite identities equal the raw rule book" rawRuleIds compiledRuleIds
        assertEqual "compiled fact identities equal the raw fact book" rawFactIds compiledFactIds,
      testCase "parenthesis erasure is admitted without oracle evidence" $ do
        ingested <- requireIngested parErasureWorkload
        corpus <-
          requireRight
            "parenthesis-erasure corpus"
            (deriveRuleCorpus defaultNebulaConfig (imSpanRows ingested) Nothing (imConverted ingested))
        assertBool
          "parenthesis erasure is an admitted no-oracle syntax law"
          ((hsExprParErasureLawId, RegistryTrusted) `Set.member` lawTableLawTiers corpus)
        assertBool
          "parenthesis erasure is not gated by missing HIE oracle data"
          (hsExprParErasureLawId `notElem` fmap glrLaw (rcGatedLaws corpus)),
      testCase "site-derived rule admission gates composition without losing the rest of the family" $ do
        ingested <- requireIngested fixtureWorkload
        corpus <-
          either
            (assertFailure . ("corpus derivation failed: " <>) . show)
            pure
            (deriveRuleCorpus defaultNebulaConfig (imSpanRows ingested) Nothing (imConverted ingested))
        let siteMetrics = rcSiteMetrics corpus
            lambdaSites = hsrmLambdaSiteCount siteMetrics
            letSites = hsrmLetSiteCount siteMetrics
        assertBool "fixture exercises at least one lambda site" (lambdaSites >= 1)
        assertBool "fixture exercises at least one let site" (letSites >= 1)
        assertEqual "eta rules are one per lambda site" lambdaSites (hsrmEtaRuleCount siteMetrics)
        assertEqual "composition rules are gated without oracle evidence" 0 (hsrmCompositionRuleCount siteMetrics)
        assertEqual "beta rules are one per lambda site" lambdaSites (hsrmBetaRuleCount siteMetrics)
        assertEqual "let rules are one per let site" letSites (hsrmLetRuleCount siteMetrics)
        assertEqual "rule total decomposes by admitted family" (2 * lambdaSites + letSites) (hsrmTotalRuleCount siteMetrics)
        assertEqual "pass-0 composition and renamer-fusion law rows are gated" [lawIdKey hsExprCompositionLawId, lawIdKey hsExprMapFusionLawId, lawIdKey hsExprFmapFusionLawId] (fmap (lawIdKey . glrLaw) (passZeroGatedLaws corpus))
        assertEqual "gated composition reports every lambda-site rule" lambdaSites (sum (fmap glrRuleCount (filter ((== hsExprCompositionLawId) . glrLaw) (rcGatedLaws corpus))))
        assertEqual
          "all vocabulary laws are generated but gated without oracle evidence"
          HsExprVocabularyRuleMetrics
            { hvrmVocabularyLawCount = Set.size hsExprVocabularyLawIds,
              hvrmVocabularyGeneratedRuleCount = Set.size hsExprVocabularyLawIds,
              hvrmVocabularyAdmittedRuleCount = 0,
              hvrmVocabularyGatedLawCount = Set.size hsExprVocabularyLawIds
            }
          (rcVocabularyMetrics corpus)
        assertEqual "law table is total over the admitted corpus" (length (supportedRules (rcRuleBook corpus))) (Map.size (rcLawTable corpus)),
      testCase "satisfied oracle keys restore the full site family arithmetic" $ do
        ingested <- requireIngested fixtureWorkload
        oracle <- requireRight "compose oracle" composeOracle
        satisfiedKeys <- requireRight "oracle satisfied keys" (oracleSatisfiedKeys oracle)
        corpus <-
          requireRight
            "oracle-admitted corpus"
            (deriveRuleCorpusWithOracleKeys defaultNebulaConfig satisfiedKeys (imSpanRows ingested) (Just oracle) (imConverted ingested))
        let siteMetrics = rcSiteMetrics corpus
            lambdaSites = hsrmLambdaSiteCount siteMetrics
            letSites = hsrmLetSiteCount siteMetrics
        assertEqual "composition rules are restored by oracle evidence" lambdaSites (hsrmCompositionRuleCount siteMetrics)
        assertEqual "full rule total decomposes by family" (3 * lambdaSites + letSites) (hsrmTotalRuleCount siteMetrics)
        assertEqual "no laws are gated once the oracle key is satisfied" [] (rcGatedLaws corpus)
        assertEqual
          "all vocabulary laws are admitted under base vocabulary evidence"
          HsExprVocabularyRuleMetrics
            { hvrmVocabularyLawCount = Set.size hsExprVocabularyLawIds,
              hvrmVocabularyGeneratedRuleCount = Set.size hsExprVocabularyLawIds,
              hvrmVocabularyAdmittedRuleCount = Set.size hsExprVocabularyLawIds,
              hvrmVocabularyGatedLawCount = 0
            }
          (rcVocabularyMetrics corpus)
        assertEqual "law table remains total over admitted corpus" (length (supportedRules (rcRuleBook corpus))) (Map.size (rcLawTable corpus)),
      testCase "corpus sources gate the binding-front contribution" $ do
        ingested <- requireIngested fixtureWorkload
        unionCorpus <-
          requireRight "union corpus" (deriveRuleCorpus defaultNebulaConfig (imSpanRows ingested) Nothing (imConverted ingested))
        bridgeMetrics <-
          maybe
            (assertFailure "binding-front metrics missing under the union default")
            pure
            (rcBindingMetrics unionCorpus)
        assertEqual
          "the fixture let redex elaborates through the binding front"
          HsExprBindingRuleMetrics
            { hbrmRedexSiteCount = 1,
              hbrmAllowedCount = 1,
              hbrmFresheningCount = 0,
              hbrmObstructionCount = 0,
              hbrmGeneratedRuleCount = 1,
              hbrmFactRuleCount = 1
            }
          bridgeMetrics
        siteCorpus <-
          requireRight "site-only corpus" (deriveRuleCorpus siteOnlyConfig (imSpanRows ingested) Nothing (imConverted ingested))
        assertEqual
          "the site-only corpus carries no binding-front metrics"
          Nothing
          (rcBindingMetrics siteCorpus)
        assertEqual
          "both corpora agree on the site family arithmetic"
          (rcSiteMetrics unionCorpus)
          (rcSiteMetrics siteCorpus)
    ]

saturateCases :: TestTree
saturateCases =
  testGroup
    "nebula.pipeline.saturate"
    [ testCase "fixture saturation reaches a fixed point with real matches" $ do
        (ingested, saturated) <- requireSaturatedFixture
        assertEqual
          "initial saturation prepares one plan and performs one fresh run"
          (SaturationLifecycleCounts 1 1 0)
          (smLifecycleCounts saturated)
        assertEqual "termination is a fixed point" ReachedFixedPoint (smTermination saturated)
        assertBool "saturation applied at least one match" (smMatchesApplied saturated >= 1)
        assertBool "iterations were spent to converge" (smIterations saturated >= 1)
        assertEqual
          "saturation starts from the graph the insertion produced"
          ( himBaseNodeCountAfter (imInsertionMetrics ingested),
            himBaseClassCountAfter (imInsertionMetrics ingested)
          )
          (smInitialNodeCount saturated, smInitialClassCount saturated)
        assertBool
          "final counts remain positive"
          (smFinalNodeCount saturated >= 1 && smFinalClassCount saturated >= 1)
        assertBool
          "applied matches are witnessed by scheduled work in the round trace"
          (smScheduledTotal saturated >= 1)
        assertEqual
          "the deterministic scheduler records no per-rule schedule trace"
          []
          (smRuleFires saturated)
        assertBool
          "applied matches retain proof steps for certificate extraction"
          (smMatchesApplied saturated <= 0 || not (null (smProofSteps saturated)))
        assertBool
          "rewrite proof entries are law-stamped"
          (all rewriteStepStamped (filter isRewriteProofStep (smProofSteps saturated)))
        assertBool
          "non-rewrite proof entries do not fabricate law stamps"
          (all nonRewriteStepUnstamped (filter (not . isRewriteProofStep) (smProofSteps saturated)))
        assertSaturatedModuleCarrierCoherence saturated,
      testCase "unchanged fixed-point carriers resume without rebuilding authority" $ do
        (_, corpus, saturated) <- requireSaturatedFixtureWithCorpus
        resumed <- requireRight "fixed-point resume" (resumeSaturatedModule mempty corpus saturated)
        assertEqual
          "resume reuses the one prepared plan"
          (SaturationLifecycleCounts 1 1 1)
          (smLifecycleCounts resumed)
        assertEqual
          "fixed-point resume applies no new matches"
          0
          (smMatchesApplied resumed)
        assertEqual
          "fixed-point resume preserves the authoritative graph digest"
          (contextGraphDigest (smContextGraph saturated))
          (contextGraphDigest (smContextGraph resumed))
        assertEqual
          "fixed-point resume preserves proof step order"
          (proofLogReceipt (smProofSteps saturated))
          (proofLogReceipt (smProofSteps resumed))
        assertSaturatedModuleCarrierCoherence resumed
    ]

assertSaturatedModuleCarrierCoherence :: SaturatedModule -> IO ()
assertSaturatedModuleCarrierCoherence saturated = do
  let runtimeState = resumableRuntimeState (smRuntimeState saturated)
      carrier = rsCarrier runtimeState
      carrierContextGraph = sceContextGraph (pgGraph carrier)
      derivedContextGraph = smContextGraph saturated
  assertEqual
    "derived graph is the runtime carrier graph"
    (contextGraphDigest carrierContextGraph)
    (contextGraphDigest derivedContextGraph)
  assertEqual
    "derived graph preserves the carrier's prepared cover"
    (contextPreparedObjects carrierContextGraph)
    (contextPreparedObjects derivedContextGraph)
  assertEqual
    "node receipt agrees with the runtime carrier graph"
    (eGraphNodeCount (cegBase carrierContextGraph))
    (smFinalNodeCount saturated)
  assertEqual
    "class receipt agrees with the runtime carrier graph"
    (eGraphClassCount (cegBase carrierContextGraph))
    (smFinalClassCount saturated)
  assertEqual
    "derived proof log serializes only the runtime carrier registry"
    (proofLogReceipt (serializeProofLog carrier))
    (proofLogReceipt (smProofSteps saturated))

proofLogReceipt :: (Show c, Show p) => [ProofStep HsExprF c p] -> [(String, Maybe String, Maybe String)]
proofLogReceipt =
  fmap
    ( \proofStep ->
        ( show proofStep,
          fmap hsExprWitnessReceipt (psLhsWitness proofStep),
          fmap hsExprWitnessReceipt (psRhsWitness proofStep)
        )
    )

hsExprWitnessReceipt :: Fix HsExprF -> String
hsExprWitnessReceipt (Fix nodeValue) =
  show (fmap hsExprWitnessReceipt nodeValue)

isRewriteProofStep :: ProofStep f c NebulaProvenance -> Bool
isRewriteProofStep proofStep =
  case psKind proofStep of
    ProofRewrite {} -> True
    ProofRewriteOrigin {} -> True
    ProofCongruence -> False
    ProofAnalysis -> False

rewriteStepStamped :: ProofStep f c NebulaProvenance -> Bool
rewriteStepStamped proofStep =
  maybe False (const True) (npStamp (psAnnotation proofStep))

nonRewriteStepUnstamped :: ProofStep f c NebulaProvenance -> Bool
nonRewriteStepUnstamped proofStep =
  maybe True (const False) (npStamp (psAnnotation proofStep))

chooseCases :: TestTree
chooseCases =
  testGroup
    "nebula.pipeline.choose"
    [ testCase "extraction never inflates a binding under the size cost" $ do
        (ingested, saturated) <- requireSaturatedFixture
        sizeSections <- sizeSectionsFor defaultNebulaConfig ingested saturated
        chosen <- requireRight "binding extraction" (chooseBindings defaultNebulaConfig ingested saturated sizeSections)
        assertEqual
          "one chosen binding per ingested binding"
          (length (imBindingNames ingested))
          (length chosen)
        mapM_
          ( \binding -> do
              assertBool
                (cbName binding <> " extracted within its original size")
                (cbExtractedSize binding <= cbOriginalSize binding)
              assertEqual
                (cbName binding <> " realizes its reported cost under the size algebra")
                (cbExtractedSize binding)
                (cbExtractionCost binding)
          )
          chosen,
      testCase "parenthesis erasure changes the extracted binding" $ do
        chosen <- requireChosenWith defaultNebulaConfig parErasureWorkload
        wrapped <- requireBinding "wrapped" chosen
        assertBool
          "parenthesis erasure strictly reduces the wrapped binding"
          (cbExtractedSize wrapped < cbOriginalSize wrapped)
        assertBool
          "the extracted wrapped binding contains no ParF nodes"
          (not (fixContainsParF (cbTerm wrapped))),
      testCase "anti-unification surfaces the shared application spine first" $ do
        (ingested, saturated) <- requireSaturatedFixture
        sizeSections <- sizeSectionsFor defaultNebulaConfig ingested saturated
        chosen <- requireRight "binding extraction" (chooseBindings defaultNebulaConfig ingested saturated sizeSections)
        sites <- requireRight "candidate sites" (candidateSites defaultNebulaConfig ingested saturated chosen sizeSections)
        supportGroups <- requireRight "shape support grouping" (candidateSiteSupportGroups sites)
        candidates <-
          requireRight
            "shape support candidate grouping"
            ( sharedAbstractionCandidates
                defaultNebulaConfig
                saturated
                ( admittedSitePairs
                    (ncAntiUnifyMaxPairs defaultNebulaConfig)
                    sites
                    (buildPairLedger (ncAntiUnifyMaxPairs defaultNebulaConfig) supportGroups)
                )
            )
        assertBool
          "candidate list respects the pair bound"
          (length candidates <= ncAntiUnifyMaxPairs defaultNebulaConfig)
        sharePair <-
          maybe
            (assertFailure "shareLeft/shareRight pair missing from the candidate list")
            pure
            ( find
                (\candidate -> abstractionCandidateNames candidate == ("shareLeft", "shareRight"))
                candidates
            )
        assertBool
          "share pair exposes nontrivial shared structure"
          (binaryLggSharedStructure (acResult sharePair) >= 1)
        firstCandidate <-
          maybe (assertFailure "candidate list is empty") pure (find (const True) candidates)
        assertEqual
          "the share pair dominates the ranking"
          ("shareLeft", "shareRight")
          (abstractionCandidateNames firstCandidate),
      testCase "nested region candidate sites are discovered from scoped terms" $ do
        (ingested, saturated) <- requireSaturatedFixture
        sizeSections <- sizeSectionsFor defaultNebulaConfig ingested saturated
        chosen <- requireRight "binding extraction" (chooseBindings defaultNebulaConfig ingested saturated sizeSections)
        sites <- requireRight "candidate sites" (candidateSites defaultNebulaConfig ingested saturated chosen sizeSections)
        assertBool
          "top-level binding sites remain available"
          (any ((== BindingCandidateSite) . csSiteKind) sites)
        assertBool
          "shareLeft contributes at least one nested region site"
          (any (\site -> csBindingName site == "shareLeft" && csSiteKind site == RegionCandidateSite) sites)
        assertBool
          "region sites carry normalized nonempty node sizes"
          (all ((> 0) . csSize) (filter ((== RegionCandidateSite) . csSiteKind) sites)),
      testCase "shape support colimit groups candidate sites through bucket witnesses" $ do
        (ingested, saturated) <- requireSaturatedFixture
        sizeSections <- sizeSectionsFor defaultNebulaConfig ingested saturated
        chosen <- requireRight "binding extraction" (chooseBindings defaultNebulaConfig ingested saturated sizeSections)
        sites <- requireRight "candidate sites" (candidateSites defaultNebulaConfig ingested saturated chosen sizeSections)
        supportGroups <- requireRight "shape support colimit groups" (candidateSiteSupportGroups sites)
        assertBool
          "support colimit produces at least one multi-site cover group"
          (any ((>= 2) . length) supportGroups)
        assertBool
          "shareLeft and shareRight descend into one support-colimit class"
          ( any
              ( \group ->
                  Set.fromList ["shareLeft", "shareRight"]
                    `Set.isSubsetOf` Set.fromList (fmap csBindingName group)
              )
              supportGroups
          ),
      testCase "constructor and label reordering feed the alpha-aware candidate lane" $ do
        caseCandidate <- requireCandidateBetween reorderedCaseWorkload "caseLeft" "caseRight"
        assertBool
          "case alternatives align by constructor before mismatch"
          (binaryLggSharedStructure (acResult caseCandidate) >= 1)
        recordCandidate <- requireCandidateBetween reorderedRecordWorkload "recordLeft" "recordRight"
        assertBool
          "record fields align by label before mismatch"
          (binaryLggSharedStructure (acResult recordCandidate) >= 1)
    ]

requireCandidateBetween :: ModuleWorkload -> String -> String -> IO AbstractionCandidate
requireCandidateBetween workload leftName rightName = do
  ingested <- requireIngested workload
  corpus <- requireRight "corpus derivation" (deriveRuleCorpus defaultNebulaConfig (imSpanRows ingested) (workloadOracle workload) (imConverted ingested))
  saturated <- requireRight "saturation" (saturateModule defaultSaturationOptions defaultNebulaConfig ingested corpus)
  sizeSections <- sizeSectionsFor defaultNebulaConfig ingested saturated
  chosen <- requireRight "binding extraction" (chooseBindings defaultNebulaConfig ingested saturated sizeSections)
  let targetNames = Set.fromList [leftName, rightName]
  sites <- requireRight "candidate sites" (candidateSites defaultNebulaConfig ingested saturated chosen sizeSections)
  supportGroups <- requireRight "shape support grouping" (candidateSiteSupportGroups sites)
  candidates <-
    requireRight
      "shape support candidate grouping"
      ( sharedAbstractionCandidates
          defaultNebulaConfig
          saturated
          ( admittedSitePairs
              (ncAntiUnifyMaxPairs defaultNebulaConfig)
              sites
              (buildPairLedger (ncAntiUnifyMaxPairs defaultNebulaConfig) supportGroups)
          )
      )
  maybe
    (assertFailure ("candidate pair missing for " <> leftName <> "/" <> rightName))
    pure
    (find (\candidate -> Set.fromList (abstractionCandidateNameSet candidate) == targetNames) candidates)

abstractionCandidateNames :: AbstractionCandidate -> (String, String)
abstractionCandidateNames candidate =
  (csBindingName (acLeftSite candidate), csBindingName (acRightSite candidate))

abstractionCandidateNameSet :: AbstractionCandidate -> [String]
abstractionCandidateNameSet candidate =
  let (leftName, rightName) = abstractionCandidateNames candidate
   in [leftName, rightName]

harvestMaintenanceCases :: TestTree
harvestMaintenanceCases =
  testGroup
    "nebula.pipeline.harvest-maintenance"
    [ testCase "dirty-context harvest advance equals a fresh harvest on the same graph" $ do
        (ingested, saturated) <- requireSaturatedFixture
        previousHarvest <- requireRight "previous harvest" (buildHarvest defaultNebulaConfig ingested saturated)
        dirtyContext <-
          maybe
            (assertFailure "fixture produced no harvested binding contexts")
            pure
            (listToMaybe (fmap cbContext (hsBindings previousHarvest)))
        (siteDelta, advancedHarvest) <-
          requireRight
            "advanced harvest"
            ( advanceHarvestFromSections
                defaultNebulaConfig
                ingested
                saturated
                (hsSections previousHarvest)
                (Set.singleton dirtyContext)
                previousHarvest
            )
        freshHarvest <- requireRight "fresh harvest" (buildHarvest defaultNebulaConfig ingested saturated)
        assertEqual
          "advanced harvest rows equal fresh rows"
          (fmap siteRow (hsSites freshHarvest))
          (fmap siteRow (hsSites advancedHarvest))
        assertEqual
          "advanced harvest index equals fresh index"
          (hsBucketIndex freshHarvest)
          (hsBucketIndex advancedHarvest)
        assertEqual
          "advanced harvest support groups equal fresh groups"
          (groupSiteRows (hsGroups freshHarvest))
          (groupSiteRows (hsGroups advancedHarvest))
        assertEqual
          "identity graph advance produces no dirty buckets"
          Set.empty
          (harvestDirtyBuckets siteDelta)
    ]

groupSiteRows :: [[CandidateSite]] -> [[SiteRow]]
groupSiteRows =
  fmap (fmap siteRow)

synthesizeCases :: TestTree
synthesizeCases =
  testGroup
    "nebula.pipeline.synthesize"
    [ testCase "shared spine synthesizes one paying abstraction" $ do
        (_, _, initialSaturated, preBindings, outcome) <- requireSynthesisRun fixtureWorkload
        let finalSaturated = soSaturatedModule outcome
            initialProofSteps = smProofSteps initialSaturated
            finalRuntimeCore = rsCore (resumableRuntimeState (smRuntimeState finalSaturated))
        definition <-
          case soDefinitions outcome of
            [oneDefinition] -> pure oneDefinition
            others -> assertFailure ("expected exactly one definition, got " <> show (length others))
        assertEqual "the definition name is derived from the selected sites" "shareLeftRight" (synthesizedNameText (sdName definition))
        assertEqual
          "the definition abstracts the share sites"
          ["shareLeft", "shareRight"]
          (sort (fmap ssBindingName (sdSites definition)))
        assertBool "the realized win is positive" (soRealizedWin outcome > 0)
        assertEqual
          "accepted external staging reuses one plan across two fresh runtime states"
          (SaturationLifecycleCounts 1 2 0)
          (smLifecycleCounts finalSaturated)
        assertBool
          "the preserved proof-prefix law exercises a nonempty initial registry"
          (not (null initialProofSteps))
        assertEqual
          "external staging preserves the initial proof registry as an ordered prefix"
          (proofLogReceipt initialProofSteps)
          (proofLogReceipt (take (length initialProofSteps) (smProofSteps finalSaturated)))
        assertEqual
          "the edited graph run starts a fresh iteration counter"
          (smIterations finalSaturated)
          (rcIterationCount finalRuntimeCore)
        assertEqual
          "the edited graph run starts a fresh match counter"
          (smMatchesApplied finalSaturated)
          (rcTotalMatches finalRuntimeCore)
        assertSaturatedModuleCarrierCoherence finalSaturated
        assertEqual
          "the ledger arithmetic is exact"
          (soRealizedWin outcome)
          (soPreExtractedTotal outcome - soPostExtractedTotal outcome)
        assertBool
          "the synthesized total undercuts the pre-synthesis total"
          (soPostExtractedTotal outcome < soPreExtractedTotal outcome)
        assertBool
          "non-selected region opportunities stay diagnostic-only"
          (all ((== Nothing) . rejRealizedWin) (soRejected outcome))
        assertBool
          "at least one accepted merge is staged context-locally"
          (psrLocalizedMerges (soStagingReport outcome) > 0)
        assertEqual
          "accepted plan staging accounts for site application merges"
          (length (sdSites definition))
          ( psrLocalizedApplicationMerges (soStagingReport outcome)
              + psrGlobalApplicationFallbackMerges (soStagingReport outcome)
          )
        assertBool
          "accepted plan staging glues the synthesized definition over at least one cover element"
          ( psrLocalizedDefinitionMerges (soStagingReport outcome)
              + psrGlobalDefinitionFallbackMerges (soStagingReport outcome)
              >= 1
          )
        assertBool
          "accepted plan staging surfaces the committed dirty-context footprint"
          (psrDirtyContextCount (soStagingReport outcome) > 0)
        preShare <- requireBinding "shareLeft" preBindings
        postShare <- requireBinding "shareLeft" (soBindings outcome)
        assertBool
          "shareLeft re-extracts through the named abstraction"
          (cbExtractedSize postShare < cbExtractedSize preShare),
      testCase "localized accepted plan advances harvest without dirtying the whole prepared cover" $ do
        (_, outcome) <- requireSynthesisOutcome fixtureWorkload
        let staging = soStagingReport outcome
            preparedContextCount = length (contextPreparedObjects (smContextGraph (soSaturatedModule outcome)))
        assertBool
          "fixture exercises localized staging"
          (psrLocalizedMerges staging > 0)
        assertBool
          "definition equality remains authored in its local section"
          (psrLocalizedDefinitionMerges staging > 0)
        assertEqual
          "fixture should not use global fallback"
          0
          (psrGlobalFallbackMerges staging)
        assertBool
          ( "localized staging dirtied the whole prepared cover: dirtyContexts="
              <> show (psrDirtyContextCount staging)
              <> " preparedContexts="
              <> show preparedContextCount
              <> " harvestDecision="
              <> show (soHarvestDecision outcome)
          )
          (psrDirtyContextCount staging < preparedContextCount)
        assertEqual
          "localized staging should permit maintained harvest advance"
          (Just HarvestAdvanced)
          (soHarvestDecision outcome),
      testCase "three structurally similar regions synthesize one shared cluster" $ do
        (_, outcome) <- requireSynthesisOutcome triClusterWorkload
        definition <-
          case soDefinitions outcome of
            [oneDefinition] -> pure oneDefinition
            others -> assertFailure ("expected exactly one cluster definition, got " <> show (length others))
        assertEqual
          "the cluster glues all three sites into one definition"
          ["triLeft", "triMid", "triRight"]
          (sort (fmap ssBindingName (sdSites definition)))
        assertBool "the realized cluster win is positive" (soRealizedWin outcome > 0)
        assertEqual
          "cluster ledger arithmetic is exact"
          (soRealizedWin outcome)
          (soPreExtractedTotal outcome - soPostExtractedTotal outcome),
      testCase "structurally disjoint bindings synthesize nothing" $ do
        (_, _, initialSaturated, preBindings, outcome) <- requireSynthesisRun noSharingWorkload
        let finalSaturated = soSaturatedModule outcome
        assertBool "no definitions are synthesized" (null (soDefinitions outcome))
        assertEqual "no candidate lands in the rejection ledger" [] (soRejected outcome)
        assertEqual "the realized win is zero" 0 (soRealizedWin outcome)
        assertEqual
          "the extracted total is untouched"
          (soPreExtractedTotal outcome)
          (soPostExtractedTotal outcome)
        assertEqual
          "the bindings pass through unchanged in count"
          (length preBindings)
          (length (soBindings outcome))
        assertEqual
          "no selected plan performs no extra run"
          (SaturationLifecycleCounts 1 1 0)
          (smLifecycleCounts finalSaturated)
        assertEqual
          "no selected plan returns the initial graph whole"
          (contextGraphDigest (smContextGraph initialSaturated))
          (contextGraphDigest (smContextGraph finalSaturated))
        assertEqual
          "no selected plan returns the initial proof registry whole"
          (proofLogReceipt (smProofSteps initialSaturated))
          (proofLogReceipt (smProofSteps finalSaturated)),
      testCase "thin sharing is rejected by the net-win gate with its numbers" $ do
        (_, outcome) <- requireSynthesisOutcome thinSharingWorkload
        assertBool "no definitions are synthesized" (null (soDefinitions outcome))
        assertBool
          "the diagnostic lane records at least one thin pair rejection"
          (any thinPairNoWin (soRejected outcome))
        assertBool
          "every thin rejection happens before staging"
          (all ((== Nothing) . rejRealizedWin) (soRejected outcome)),
      testCase "tiny positive duplicate candidates are synthesized instead of prefiltered" $ do
        (_, outcome) <- requireSynthesisOutcome tinyDuplicateWorkload
        assertBool
          "the tiny duplicate becomes a sealed positive synthesis candidate"
          (any ((== ["sortedLeft", "sortedRight"]) . sort . fmap ssBindingName . sdSites) (soDefinitions outcome))
        assertBool "the tiny duplicate still has a positive realized win" (soRealizedWin outcome > 0),
      testCase "synthesized definitions preserve qualified global names" $ do
        improvement <- requireImproved tinyDuplicateWorkload
        sealedSource <- sealedSourceText <$> requireSealed improvement
        assertBool "qualified IntSet access survives helper rendering" ("IntSet.toAscList" `isInfixOf` sealedSource)
        assertBool "qualified Vector access survives helper rendering" ("V.toList" `isInfixOf` sealedSource)
        assertBool "helper rendering does not strip qualifiers into unbound names" (not ("= toAscList" `isInfixOf` sealedSource))
        assertBool "helper names are no longer generated oatmeal" (not ("nebulaAbs" `isInfixOf` sealedSource))
        assertBool "helper parameters are no longer generated oatmeal" (not ("nebulaArg" `isInfixOf` sealedSource)),
      testCase "projection-vector runner shapes are diagnosed as projection opportunities" $ do
        (_, outcome) <- requireSynthesisOutcome projectionVectorWorkload
        assertBool
          "the projection-vector diagnostic sees the shared fixture projection cover"
          (any (rejectionNamesAre RejectedProjectionVectorDiagnostic ["profileRunner", "tensorRunner", "homRunner"]) (soRejected outcome)),
      testCase "row-clearing fold skeletons are diagnosed without pretending to seal" $ do
        (_, outcome) <- requireSynthesisOutcome foldSkeletonWorkload
        assertBool
          "the fold skeleton diagnostic sees both row-clearing loops"
          (any (rejectionNamesAre RejectedFoldSkeletonDiagnostic ["clearBelow", "clearInverse"]) (soRejected outcome)),
      testCase "Either validation carriers are diagnosed without sealed semantics" $ do
        (_, outcome) <- requireSynthesisOutcome eitherValidationWorkload
        assertBool
          "finite rejection guards are recognized across different error carriers"
          (any (rejectionNamesAre RejectedFiniteValidationDiagnostic ["finitePlain", "finiteLabel"]) (soRejected outcome))
        assertBool
          "case and bind continuations are normalized as the same Either carrier protocol"
          (any (rejectionNamesAre RejectedEitherValidationDiagnostic ["finiteWith", "positiveLabel", "positiveWith"]) (soRejected outcome))
        assertBool
          "threshold refinements are recognized across direct and delegated Either carriers"
          (any (rejectionNamesAre RejectedThresholdRefinementDiagnostic ["positiveLabel", "positiveWith"]) (soRejected outcome)),
      testCase "let-row recursion protocols are diagnosed across expression, statement, and guard carriers" $ do
        (_, outcome) <- requireSynthesisOutcome letRowsProtocolWorkload
        assertBool
          "the let-row protocol skeleton sees all transported carriers"
          (any (rejectionNamesAre RejectedLetRowsProtocolDiagnostic ["letExpr", "letStmt", "guardStmt"]) (soRejected outcome)),
      testCase "pattern-bind RHS protocols are diagnosed across statement and guard carriers" $ do
        (_, outcome) <- requireSynthesisOutcome patternBindRhsProtocolWorkload
        assertBool
          "the pattern-bind RHS skeleton sees both transported carriers"
          (any (rejectionNamesAre RejectedPatternBindRhsProtocolDiagnostic ["bindStmt", "guardPat"]) (soRejected outcome)),
      testCase "keyed row alignment protocols are diagnosed across term and pattern payloads" $ do
        (_, outcome) <- requireSynthesisOutcome keyedRowAlignmentProtocolWorkload
        assertBool
          "the keyed row alignment skeleton sees both payload unifiers"
          (any (rejectionNamesAre RejectedKeyedRowAlignmentProtocolDiagnostic ["fieldRows", "patternFields"]) (soRejected outcome)),
      testCase "arity child-unifier protocols are diagnosed across node and arithmetic-sequence carriers" $ do
        (_, outcome) <- requireSynthesisOutcome arityChildUnifierProtocolWorkload
        assertBool
          "the arity child-unifier skeleton sees both constructor families"
          (any (rejectionNamesAre RejectedArityChildUnifierProtocolDiagnostic ["nodeChildren", "arithSeqChildren"]) (soRejected outcome)),
      testCase "protocol diagnostics stay out of the realized-write ledger" $ do
        assertDiagnosticOnlyRejection RejectedLetRowsProtocolDiagnostic ["letExpr", "letStmt", "guardStmt"] letRowsProtocolWorkload
        assertDiagnosticOnlyRejection RejectedPatternBindRhsProtocolDiagnostic ["bindStmt", "guardPat"] patternBindRhsProtocolWorkload
        assertDiagnosticOnlyRejection RejectedKeyedRowAlignmentProtocolDiagnostic ["fieldRows", "patternFields"] keyedRowAlignmentProtocolWorkload
        assertDiagnosticOnlyRejection RejectedArityChildUnifierProtocolDiagnostic ["nodeChildren", "arithSeqChildren"] arityChildUnifierProtocolWorkload,
      realAlphaUnifySynthesisCases,
      testCase "record construction diagnostics report projection-owned fields" $ do
        (_, outcome) <- requireSynthesisOutcome recordProjectionOwnershipWorkload
        case find abstractionCandidateOwnershipDiagnostic (soRejected outcome) of
          Just rejected -> do
            assertEqual
              "the ownership diagnostic is pinned to the construction owner"
              (Set.singleton "sharedCandidates")
              (Set.fromList (fmap cslBindingName (rejSites rejected)))
            assertBool
              "the duplicated AbstractionCandidate projections are exposed"
              (all (`isInfixOf` rejectionReasonText rejected) ["acLeftName", "acRightName", "acLeftContext", "acRightContext", "acLeftClass", "acRightClass"])
            assertEqual
              "diagnostic ownership findings are not realized writes"
              Nothing
              (rejRealizedWin rejected)
          Nothing ->
            assertFailure "missing record projection ownership diagnostic",
      testCase "source-backed record fact harvest exposes construction fields and selector sites" $ do
        bindings <-
          requireRight
            "record fact binding harvest"
            (SourceAst.locatedValueBindings (mwPath recordFactHarvestWorkload) (mwSource recordFactHarvestWorkload))
        let constructions = foldMap SourceAst.bindingRecordConstructions bindings
        assertBool
          "instance methods participate in source-backed value discovery"
          ("show" `elem` fmap SourceAst.lbName bindings)
        assertEqual
          "record construction fields are source-backed"
          (Set.fromList ["crCachedName", "crOwner"])
          (Set.unions (fmap SourceAst.rcFields constructions))
        assertBool
          "record field values distinguish owner and projection"
          (any constructionHasOwnerProjection constructions)
        selectors <-
          requireRight
            "record selector harvest"
            (SourceAst.locatedSelectorApplications (mwPath recordFactHarvestWorkload) (mwSource recordFactHarvestWorkload))
        assertBool
          "selector applications keep application spans"
          (any ((== "crCachedName") . SourceAst.saSelectorName) selectors),
      testCase "single stale derived fields are diagnosed without a two-field superstition" $ do
        (_, outcome) <- requireSynthesisOutcome staleDerivedFieldSingleWorkload
        case find staleDerivedOwnershipDiagnostic (soRejected outcome) of
          Just rejected -> do
            assertEqual
              "single derived-field diagnostics do not pretend to be realized writes"
              Nothing
              (rejRealizedWin rejected)
            assertBool
              "the stale derived field is named"
              ("crCachedName" `isInfixOf` rejectionReasonText rejected)
          Nothing ->
            assertFailure "missing stale derived field diagnostic",
      testCase "generic record construction skeletons are diagnosed without CandidateSite hardcoding" $ do
        (_, outcome) <- requireSynthesisOutcome genericRecordSkeletonWorkload
        assertBool
          "reordered non-CandidateSite record constructors share a skeleton"
          (any (rejectionNamesAre RejectedRecordConstructionSkeletonDiagnostic ["cacheLeft", "cacheRight"]) (soRejected outcome)),
      testCase "real Choose source has consumed construction, canonicalization, and extraction protocols" $ do
        workload <- realChooseWorkload
        (_, outcome) <- requireSynthesisOutcome workload
        assertBool
          "the promoted Choose protocol cuts should not keep reporting as live duplication"
          (not (any chooseProtocolDiagnostic (soRejected outcome))),
      testCase "real Choose protocol planner refuses stale protocol rewrite surfaces after normalization" $ do
        workload <- realChooseWorkload
        let (skips, protocolPlan) =
              planProtocolRewrites
                (mwPath workload)
                (mwSource workload)
                ( Set.fromList
                    [ ProtocolRedundantPatternClassCanonicalization,
                      ProtocolScopedRegionExtraction
                    ]
                )
        assertEqual
          "the stale protocol rewrite requests should all refuse against the normalized Choose source"
          (Set.fromList ["extractScopeRegionProjection", "regionCandidateSites"])
          (Set.fromList (fmap prsName skips))
        assertEqual "normalized Choose source should not stage stale protocol splice groups" [] (prpSpliceGroups protocolPlan)
        assertEqual "normalized Choose source should not stage stale protocol obligations" [] (prpObligations protocolPlan),
      testCase "real Choose source satisfies construction, canonicalization, and extraction protocol postconditions" $ do
        workload <- realChooseWorkload
        case sealProtocolObligations
          (mwPath workload)
          (mwSource workload)
          [ RedundantPatternClassCanonicalizationRemoved,
            ScopedRegionExtractionDelegates
          ] of
          Left failure ->
            assertFailure ("normalized Choose source failed protocol postconditions: " <> show failure)
          Right () -> do
            assertBool
              "CandidateSite construction is centralized"
              ("candidateSiteFromProjection ::" `isInfixOf` mwSource workload)
            assertBool
              "region sites no longer perform the redundant local canonicalization"
              (not ("in case normalizedRegionProjection config contextGraph sizeSections scopedExpr canonicalClass of" `isInfixOf` mwSource workload))
            assertBool
              "scoped extraction delegates to normalizedRegionProjection"
              ("normalizedRegionProjection config contextGraph sizeSections scopedExpr regionClass" `isInfixOf` mwSource workload)
    ]

realAlphaUnifySynthesisCases :: TestTree
realAlphaUnifySynthesisCases =
  withResource acquireOutcome (const (pure ())) $ \sharedOutcome ->
    testGroup
      "real AlphaUnify source"
      [ testCase "reports the protocol opportunities pair LGG missed" $ do
          (_, outcome) <- sharedOutcome
          traverse_ (assertProtocolDiagnosticPresent outcome) realAlphaUnifyProtocolExpectations,
        testCase "protocol diagnostics expand beyond selected LGG synthesis" $ do
          (_, outcome) <- sharedOutcome
          traverse_ (assertProtocolDiagnosticBeyondSelectedDefinitions outcome) realAlphaUnifyProtocolExpectations,
        testCase "boundary-aware wrapper candidates do not poison synthesis as scope escapes" $ do
          (_, outcome) <- sharedOutcome
          assertPositiveDiagnostic
            outcome
            RejectedArityChildUnifierProtocolDiagnostic
            ["alphaUnifyArithSeq", "alphaUnifyNode"]
          assertBool
            "boundary-aware wrapper families must not be fed to the scope gate as poisoned top-level helpers"
            (not (any (scopeEscapeMentionsAny ["alphaUnifyNode", "matchPattern"]) (soRejected outcome))),
        testCase "pattern-bind protocol turns negative pair LGGs into positive signal" $ do
          (_, outcome) <- sharedOutcome
          let patternBindSites =
                ["alphaUnifyGuardStatement", "alphaUnifyStatement"]
          assertBool
            "plain pair LGG sees the statement/guard pair but estimates it as a loss"
            (any (rejectionNamesAre RejectedNoEstimatedWin patternBindSites) (filter ((< 0) . rejEstimatedWin) (soRejected outcome)))
          assertPositiveDiagnostic
            outcome
            RejectedPatternBindRhsProtocolDiagnostic
            patternBindSites,
        testCase "realized-regression rollback returns the initial carrier whole" $ do
          (initialSaturated, outcome) <- sharedOutcome
          let finalSaturated = soSaturatedModule outcome
          assertBool
            "fixture exercises realized-regression rollback"
            (any ((== RejectedRealizedRegression) . rejReason) (soRejected outcome))
          assertBool
            "fixture realizes a non-paying staged edit"
            (soRealizedWin outcome <= 0)
          assertSaturatedModuleAuthorityEqual initialSaturated finalSaturated
      ]
  where
    acquireOutcome = do
      workload <- realAlphaUnifyWorkload
      (_, _, initialSaturated, _, outcome) <- requireSynthesisRun workload
      pure (initialSaturated, outcome)

assertSaturatedModuleAuthorityEqual :: SaturatedModule -> SaturatedModule -> IO ()
assertSaturatedModuleAuthorityEqual expected actual = do
  assertEqual "rollback graph" (contextGraphDigest (smContextGraph expected)) (contextGraphDigest (smContextGraph actual))
  assertEqual "rollback proof registry" (proofLogReceipt (smProofSteps expected)) (proofLogReceipt (smProofSteps actual))
  assertEqual "rollback lifecycle" (smLifecycleCounts expected) (smLifecycleCounts actual)
  assertEqual "rollback termination" (smTermination expected) (smTermination actual)
  assertEqual "rollback iterations" (smIterations expected) (smIterations actual)
  assertEqual "rollback matches" (smMatchesApplied expected) (smMatchesApplied actual)
  assertEqual "rollback node receipt" (smInitialNodeCount expected, smFinalNodeCount expected) (smInitialNodeCount actual, smFinalNodeCount actual)
  assertEqual "rollback class receipt" (smInitialClassCount expected, smFinalClassCount expected) (smInitialClassCount actual, smFinalClassCount actual)
  assertEqual "rollback schedule receipt" (smScheduledTotal expected) (smScheduledTotal actual)

chooseProtocolDiagnostic :: RejectedCandidate -> Bool
chooseProtocolDiagnostic rejected =
  rejReason rejected
    `Set.member` Set.fromList
      [ RejectedRecordConstructionSkeletonDiagnostic,
        RejectedRedundantPatternClassCanonicalizationDiagnostic,
        RejectedScopedRegionExtractionProtocolDiagnostic
      ]

thinPairNoWin :: RejectedCandidate -> Bool
thinPairNoWin rejected =
  rejReason rejected == RejectedNoEstimatedWin
    && rejEstimatedWin rejected <= 0
    && Set.fromList (fmap cslBindingName (rejSites rejected)) == Set.fromList ["thinLeft", "thinRight"]

rejectionNamesAre :: CandidateRejection -> [String] -> RejectedCandidate -> Bool
rejectionNamesAre rejection expectedNames rejected =
  rejReason rejected == rejection
    && Set.fromList (fmap cslBindingName (rejSites rejected)) == Set.fromList expectedNames

assertDiagnosticOnlyRejection :: CandidateRejection -> [String] -> ModuleWorkload -> IO ()
assertDiagnosticOnlyRejection rejection expectedNames workload = do
  (_, outcome) <- requireSynthesisOutcome workload
  case find (rejectionNamesAre rejection expectedNames) (soRejected outcome) of
    Just rejected ->
      assertEqual
        "protocol diagnostics must not claim realized savings before synthesis proof/writeback"
        Nothing
        (rejRealizedWin rejected)
    Nothing ->
      assertFailure ("missing diagnostic rejection " <> show rejection <> " for " <> show expectedNames)

realAlphaUnifyProtocolExpectations :: [(CandidateRejection, [String])]
realAlphaUnifyProtocolExpectations =
  [ (RejectedLetRowsProtocolDiagnostic, ["alphaUnifyGuardStatement", "alphaUnifyLet", "alphaUnifyLetStatement"]),
    (RejectedPatternBindRhsProtocolDiagnostic, ["alphaUnifyGuardStatement", "alphaUnifyStatement"]),
    (RejectedKeyedRowAlignmentProtocolDiagnostic, ["alphaUnifyFieldRows", "matchPatternFields"]),
    (RejectedArityChildUnifierProtocolDiagnostic, ["alphaUnifyArithSeq", "alphaUnifyNode"])
  ]

assertProtocolDiagnosticPresent :: SynthesisOutcome -> (CandidateRejection, [String]) -> IO ()
assertProtocolDiagnosticPresent outcome (rejection, expectedNames) =
  assertBool
    ("real AlphaUnify detector must report " <> show rejection <> " for " <> show expectedNames)
    (any (rejectionNamesAre rejection expectedNames) (soRejected outcome))

assertProtocolDiagnosticBeyondSelectedDefinitions :: SynthesisOutcome -> (CandidateRejection, [String]) -> IO ()
assertProtocolDiagnosticBeyondSelectedDefinitions outcome (rejection, expectedNames) = do
  assertProtocolDiagnosticPresent outcome (rejection, expectedNames)
  assertBool
    ("the protocol diagnostic should add a frontier beyond selected definitions for " <> show expectedNames)
    (not (any (definitionNamesAre expectedNames) (soDefinitions outcome)))
  assertPositiveDiagnostic outcome rejection expectedNames

assertPositiveDiagnostic :: SynthesisOutcome -> CandidateRejection -> [String] -> IO ()
assertPositiveDiagnostic outcome rejection expectedNames =
  case find (rejectionNamesAre rejection expectedNames) (soRejected outcome) of
    Just rejected -> do
      assertBool
        ("diagnostic should be positive signal for " <> show rejection)
        (rejEstimatedWin rejected > 0)
      assertEqual
        "positive protocol diagnostics still cannot count as realized writeback"
        Nothing
        (rejRealizedWin rejected)
    Nothing ->
      assertFailure ("missing positive diagnostic " <> show rejection <> " for " <> show expectedNames)

definitionNamesAre :: [String] -> SynthesizedDefinition -> Bool
definitionNamesAre expectedNames definition =
  Set.fromList (fmap ssBindingName (sdSites definition)) == Set.fromList expectedNames

scopeEscapeMentionsAny :: [String] -> RejectedCandidate -> Bool
scopeEscapeMentionsAny bindingNames rejected =
  let expectedBindings =
        Set.fromList bindingNames
   in rejReason rejected == RejectedScopeEscape
        && any (`Set.member` expectedBindings) (fmap cslBindingName (rejSites rejected))

constructionHasOwnerProjection :: SourceAst.RecordConstruction -> Bool
constructionHasOwnerProjection construction =
  any ((== SourceAst.RecordFieldDirect "owner") . SourceAst.rcfValue) fields
    && any ((== SourceAst.RecordFieldProjection "Qualified.ownerName" "owner") . SourceAst.rcfValue) fields
  where
    fields =
      SourceAst.rcFieldRows construction

staleDerivedOwnershipDiagnostic :: RejectedCandidate -> Bool
staleDerivedOwnershipDiagnostic rejected =
  case rejReason rejected of
    RejectedRecordOwnershipDiagnostic findings ->
      any
        ( \finding ->
            rofConstructorName finding == "CacheRow"
              && rofDerivedField finding == "crCachedName"
              && rofOwnerField finding == "crOwner"
              && rofProjectionName finding == "Qualified.ownerName"
              && rofKind finding == StaleDerivedField
        )
        findings
    _ ->
      False

abstractionCandidateOwnershipDiagnostic :: RejectedCandidate -> Bool
abstractionCandidateOwnershipDiagnostic rejected =
  case rejReason rejected of
    RejectedRecordOwnershipDiagnostic findings ->
      any
        ( \finding ->
            rofConstructorName finding == "AbstractionCandidate"
              && rofDerivedField finding == "acLeftName"
        )
        findings
        && any
          ( \finding ->
              rofConstructorName finding == "AbstractionCandidate"
                && rofDerivedField finding == "acRightClass"
          )
          findings
    _ ->
      False

rejectionReasonText :: RejectedCandidate -> String
rejectionReasonText =
  show . rejReason

realAlphaUnifyWorkload :: IO ModuleWorkload
realAlphaUnifyWorkload = do
  existingPaths <- filterM doesFileExist realAlphaUnifySourceCandidates
  sourcePath <-
    maybe
      (assertFailure ("missing AlphaUnify source; tried " <> show realAlphaUnifySourceCandidates))
      pure
      (listToMaybe existingPaths)
  sourceText <- readFile sourcePath
  pure
    ModuleWorkload
      { mwPath = "Melusine/Nebula/Discovery/AlphaUnify.hs",
        mwSource = sourceText,
        mwOracleLookup = OracleMissing []
      }

realAlphaUnifySourceCandidates :: [FilePath]
realAlphaUnifySourceCandidates =
  [ "src/Melusine/Nebula/Discovery/AlphaUnify.hs",
    "engine/melusine-nebula/src/Melusine/Nebula/Discovery/AlphaUnify.hs",
    "compiler/engine/melusine-nebula/src/Melusine/Nebula/Discovery/AlphaUnify.hs"
  ]

realChooseWorkload :: IO ModuleWorkload
realChooseWorkload = do
  existingPaths <- filterM doesFileExist realChooseSourceCandidates
  sourcePath <-
    maybe
      (assertFailure ("missing Choose source; tried " <> show realChooseSourceCandidates))
      pure
      (listToMaybe existingPaths)
  sourceText <- readFile sourcePath
  pure
    ModuleWorkload
      { mwPath = "Melusine/Nebula/Discovery/Choose.hs",
        mwSource = sourceText,
        mwOracleLookup = OracleMissing []
      }

realChooseSourceCandidates :: [FilePath]
realChooseSourceCandidates =
  [ "src/Melusine/Nebula/Discovery/Choose.hs",
    "engine/melusine-nebula/src/Melusine/Nebula/Discovery/Choose.hs",
    "compiler/engine/melusine-nebula/src/Melusine/Nebula/Discovery/Choose.hs"
  ]

shadowWorkload :: ModuleWorkload
shadowWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/Shadow.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.Shadow where",
            "",
            "shadow = let g = \\x -> use x x in g alpha"
          ],
      mwOracleLookup = OracleMissing []
    }

captureWorkload :: ModuleWorkload
captureWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/Capture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.Capture where",
            "",
            "capture = (\\x -> \\w -> use x) w"
          ],
      mwOracleLookup = OracleMissing []
    }

unionCases :: TestTree
unionCases =
  testGroup
    "nebula.pipeline.union"
    [ testCase "the binding front contracts the shadow binding past the site family alone" $ do
        siteChosen <- requireChosenWith siteOnlyConfig shadowWorkload
        unionChosen <- requireChosenWith defaultNebulaConfig shadowWorkload
        shadowSite <- requireBinding "shadow" siteChosen
        shadowUnion <- requireBinding "shadow" unionChosen
        assertBool
          "the site family alone already improves the let binding"
          (cbExtractedSize shadowSite < cbOriginalSize shadowSite)
        assertBool
          "the union extraction is strictly smaller than the site family's"
          (cbExtractedSize shadowUnion < cbExtractedSize shadowSite),
      testCase "a capture-threatened redex reports one obstruction and one freshening" $ do
        ingested <- requireIngested captureWorkload
        corpus <-
          requireRight "union corpus" (deriveRuleCorpus defaultNebulaConfig (imSpanRows ingested) Nothing (imConverted ingested))
        bridgeMetrics <-
          maybe
            (assertFailure "binding-front metrics missing under the union default")
            pure
            (rcBindingMetrics corpus)
        assertEqual
          "the capture analysis freshens instead of contracting naively"
          HsExprBindingRuleMetrics
            { hbrmRedexSiteCount = 1,
              hbrmAllowedCount = 0,
              hbrmFresheningCount = 1,
              hbrmObstructionCount = 1,
              hbrmGeneratedRuleCount = 2,
              hbrmFactRuleCount = 1
            }
          bridgeMetrics
        chosen <- requireChosenWith defaultNebulaConfig captureWorkload
        captureBinding <- requireBinding "capture" chosen
        assertBool
          "the freshened contraction strictly improves the capture binding"
          (cbExtractedSize captureBinding < cbOriginalSize captureBinding)
    ]

requireBinding :: String -> [ChosenBinding] -> IO ChosenBinding
requireBinding bindingName bindings =
  maybe
    (assertFailure ("binding " <> bindingName <> " is missing"))
    pure
    (find ((== bindingName) . cbName) bindings)

fixContainsParF :: Fix HsExprF -> Bool
fixContainsParF (Fix nodeValue) =
  case nodeValue of
    ParF {} ->
      True
    _ ->
      any fixContainsParF nodeValue

scopeSoundnessCases :: TestTree
scopeSoundnessCases =
  testGroup
    "nebula.pipeline.scope-soundness"
    [ testCase "chosen terms are well-scoped across binder-introducing scope kinds" $ do
        chosen <- requireChosenWith defaultNebulaConfig scopeKindsWorkload
        mapM_
          ( \binding ->
              assertBool
                ("chosen term for " <> cbName binding <> " is well-scoped")
                (wellScopedTerm (cbTerm binding))
          )
          chosen,
      testCase "equivalences from one branch never rewrite a sibling branch with a same-named binder" $ do
        chosen <- requireChosenWith defaultNebulaConfig scopeIsolationWorkload
        chosenTerm <- singleChosenTerm chosen
        ingested <- requireIngested scopeIsolationWorkload
        originalTerm <-
          case cmBindings (imConverted ingested) of
            [bindingValue] -> pure (fixOfSpanned (tlbSpannedTerm bindingValue))
            bindings -> assertFailure ("expected one converted binding, got " <> show (length bindings))
        (originalRight, chosenRight) <-
          case (firstCaseBranches originalTerm, firstCaseBranches chosenTerm) of
            (Just [_, (_, originalBranch)], Just [_, (_, chosenBranch)]) ->
              pure (originalBranch, chosenBranch)
            _ ->
              assertFailure "expected a two-branch case in the isolation fixture"
        assertBool "the sibling Right branch stays structurally unchanged" (sameFixTerm originalRight chosenRight)
        assertBool "the isolation chosen term is well-scoped" (wellScopedTerm chosenTerm),
      testCase "binder-mentioning equivalences stay confined to their region" $ do
        chosen <- requireChosenWith defaultNebulaConfig scopeConfinementWorkload
        confinedTerm <- singleChosenTerm chosen
        branchRows <-
          maybe (assertFailure "expected a case in the confinement fixture") pure (firstCaseBranches confinedTerm)
        case branchRows of
          [(_, boundBranch), (_, siblingBranch)] -> do
            assertEqual
              "every occurrence of the pattern binder lives inside its own branch"
              (localNameOccurrences "value" confinedTerm)
              (localNameOccurrences "value" boundBranch)
            assertEqual
              "the sibling branch never mentions the pattern binder"
              0
              (localNameOccurrences "value" siblingBranch)
            assertBool "the confinement chosen term is well-scoped" (wellScopedTerm confinedTerm)
          otherRows ->
            assertFailure ("expected two case branches, got " <> show (length otherRows)),
      testCase "synthesis abstracts local-binder parameters only through scoped call sites" $ do
        (chosen, outcome) <- requireSynthesisOutcome synthesisCaptureWorkload
        mapM_
          ( \binding ->
              assertBool
                ("pre-synthesis term for " <> cbName binding <> " is well-scoped")
                (wellScopedTerm (cbTerm binding))
          )
          chosen
        mapM_
          ( \definition ->
              assertBool
                ("synthesized definition " <> synthesizedNameText (sdName definition) <> " is well-scoped")
                (wellScopedTerm (sdTerm definition))
          )
          (soDefinitions outcome)
        mapM_
          ( \binding -> do
              assertBool
                ("post-synthesis term for " <> cbName binding <> " is well-scoped")
                (wellScopedTerm (cbTerm binding))
              branchRows <-
                maybe (assertFailure ("expected a case in post-synthesis " <> cbName binding)) pure (firstCaseBranches (cbTerm binding))
              case branchRows of
                [(_, boundBranch), (_, bareBranch)] -> do
                  assertEqual
                    ("every occurrence of the pattern binder in " <> cbName binding <> " lives inside its binding branch")
                    (localNameOccurrences "value" (cbTerm binding))
                    (localNameOccurrences "value" boundBranch)
                  assertEqual
                    ("the binder-free branch of " <> cbName binding <> " never mentions the pattern binder")
                    0
                    (localNameOccurrences "value" bareBranch)
                otherRows ->
                  assertFailure ("expected two case branches in " <> cbName binding <> ", got " <> show (length otherRows))
          )
          (soBindings outcome)
    ]

scopeKindsWorkload :: ModuleWorkload
scopeKindsWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/ScopeKindsFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.ScopeKindsFixture where",
            "",
            "branching v0 = case v0 of",
            "  Just value -> (\\x -> use x x) value",
            "  Nothing -> zero",
            "",
            "guarded v1",
            "  | Just inner <- v1 = (\\x -> use x x) inner",
            "  | otherwise = zero",
            "",
            "monadic action = do",
            "  result <- action",
            "  pure ((\\y -> use y y) result)"
          ],
      mwOracleLookup = OracleMissing []
    }

scopeIsolationWorkload :: ModuleWorkload
scopeIsolationWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/ScopeIsolationFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.ScopeIsolationFixture where",
            "",
            "siblings v0 = case v0 of",
            "  Left value -> (\\x -> combine x x) value",
            "  Right value -> consume value"
          ],
      mwOracleLookup = OracleMissing []
    }

scopeConfinementWorkload :: ModuleWorkload
scopeConfinementWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/ScopeConfinementFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.ScopeConfinementFixture where",
            "",
            "confined box = case box of",
            "  Just value -> (\\x -> use x x) value",
            "  Nothing -> zero"
          ],
      mwOracleLookup = OracleMissing []
    }

synthesisCaptureWorkload :: ModuleWorkload
synthesisCaptureWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/SynthesisCaptureFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.SynthesisCaptureFixture where",
            "",
            "captureLeft box = case box of",
            "  Just value -> wrap (assemble (combine value zero) (combine value zero) (combine value zero))",
            "  Nothing -> fallback zero zero",
            "",
            "captureRight box = case box of",
            "  Just value -> wrap (assemble (combine zero value) (combine zero value) (combine zero value))",
            "  Nothing -> fallback zero zero"
          ],
      mwOracleLookup = OracleMissing []
    }

wellScopedTerm :: Fix HsExprF -> Bool
wellScopedTerm = scopedUnder Set.empty
  where
    scopedUnder bound (Fix nodeValue) =
      case nodeValue of
        VarF (LocalName binderAnn) -> binderAnn `Set.member` bound
        VarF (GlobalName _) -> True
        LamF binderAnn bodyTerm -> scopedUnder (Set.insert binderAnn bound) bodyTerm
        LetF letMode bindingRows bodyTerm ->
          scopedLetRows bound letMode bindingRows
            && scopedUnder (bound <> letRowBinders bindingRows) bodyTerm
        CaseF scrutineeTerm branchRows ->
          scopedUnder bound scrutineeTerm
            && all
              (\(branchPattern, branchTerm) -> scopedUnder (bound <> Set.fromList (patBinders branchPattern)) branchTerm)
              branchRows
        ClausesF clauseRows ->
          all
            (\(clausePatterns, bodyTerm) -> scopedUnder (bound <> Set.fromList (concatMap patBinders clausePatterns)) bodyTerm)
            clauseRows
        GuardedF alternatives -> all (scopedAlt bound) alternatives
        MultiIfF alternatives -> all (scopedAlt bound) alternatives
        DoF statements -> scopedStatements bound statements
        _ -> all (scopedUnder bound) nodeValue
    scopedAlt bound (GuardedAltF guards bodyTerm) = scopedGuards bound guards bodyTerm
    scopedGuards bound [] bodyTerm = scopedUnder bound bodyTerm
    scopedGuards bound (guardStmt : remaining) bodyTerm =
      case guardStmt of
        GuardBoolF guardTerm ->
          scopedUnder bound guardTerm && scopedGuards bound remaining bodyTerm
        GuardPatF guardPattern rhsTerm ->
          scopedUnder bound rhsTerm
            && scopedGuards (bound <> Set.fromList (patBinders guardPattern)) remaining bodyTerm
        GuardLetF letMode bindingRows ->
          scopedLetRows bound letMode bindingRows
            && scopedGuards (bound <> letRowBinders bindingRows) remaining bodyTerm
    scopedStatements _ [] = True
    scopedStatements bound (statement : remaining) =
      case statement of
        BindStmtF bindPattern rhsTerm ->
          scopedUnder bound rhsTerm
            && scopedStatements (bound <> Set.fromList (patBinders bindPattern)) remaining
        BodyStmtF bodyTerm ->
          scopedUnder bound bodyTerm && scopedStatements bound remaining
        LetStmtF letMode bindingRows ->
          scopedLetRows bound letMode bindingRows
            && scopedStatements (bound <> letRowBinders bindingRows) remaining
    scopedLetRows bound letMode bindingRows =
      let rhsBound =
            case lmRecursion letMode of
              NonRecursiveBinds -> bound
              RecursiveOpaqueBinds -> bound <> letRowBinders bindingRows
       in all (scopedUnder rhsBound . snd) bindingRows
    letRowBinders :: [(HsPatF, rhs)] -> Set.Set BinderAnn
    letRowBinders bindingRows =
      Set.fromList (concatMap (patBinders . fst) bindingRows)

sameFixTerm :: Fix HsExprF -> Fix HsExprF -> Bool
sameFixTerm (Fix leftNode) (Fix rightNode) =
  fmap (const ()) leftNode == fmap (const ()) rightNode
    && length leftChildren == length rightChildren
    && and (zipWith sameFixTerm leftChildren rightChildren)
  where
    leftChildren = foldr (:) [] leftNode
    rightChildren = foldr (:) [] rightNode

fixOfSpanned :: SpannedExpr -> Fix HsExprF
fixOfSpanned spannedValue =
  Fix (fmap fixOfSpanned (sxNode spannedValue))

firstCaseBranches :: Fix HsExprF -> Maybe [(HsPatF, Fix HsExprF)]
firstCaseBranches (Fix nodeValue) =
  case nodeValue of
    CaseF _ branchRows -> Just branchRows
    _ -> listToMaybe (mapMaybe firstCaseBranches (foldr (:) [] nodeValue))

localNameOccurrences :: String -> Fix HsExprF -> Int
localNameOccurrences targetName (Fix nodeValue) =
  case nodeValue of
    VarF (LocalName binderAnn)
      | occNameString (rdrNameOcc (baName binderAnn)) == targetName -> 1
    _ -> sum (fmap (localNameOccurrences targetName) nodeValue)

singleChosenTerm :: [ChosenBinding] -> IO (Fix HsExprF)
singleChosenTerm chosenBindings =
  case chosenBindings of
    [binding] -> pure (cbTerm binding)
    bindings -> assertFailure ("expected one chosen binding, got " <> show (length bindings))
