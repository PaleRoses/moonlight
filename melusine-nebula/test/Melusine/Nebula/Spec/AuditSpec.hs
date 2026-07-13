module Melusine.Nebula.Spec.AuditSpec (spec) where

import Data.Bifunctor (first)
import Data.List (find, isInfixOf, isPrefixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Melusine.Nebula
  ( HunkCertificate (..),
    ModuleImprovement (..),
    ModulePatch (..),
    ModuleWorkload (..),
    NebulaConfig (..),
    SealOutcome (..),
    WriteBackRefusal (..),
    defaultNebulaConfig,
    improveModule,
    moduleCertificates,
    renderModuleReport,
    sealedSourceText,
    workloadOracle,
  )
import Melusine.Nebula.Proof.Audit
  ( StepTypeConflict (..),
    TypeEvidenceCensus (..),
    TypeVerdict (..),
    classEvidenceByKey,
    replayStepVerdicts,
    typeEvidenceCensus,
  )
import Melusine.Nebula.Proof.Certificate (hunkCertificate, replayStepOf)
import Melusine.Nebula.Discovery.Choose (ChosenBinding (..))
import Melusine.Nebula.Harvest.Core (buildHarvest)
import Melusine.Nebula.Core (NebulaAnalysis (..), typeEvidenceObservations, typeObservations)
import Melusine.Nebula.Rewrite.Corpus
  ( LawStamp (..),
    RuleCorpus,
    deriveRuleCorpusWithOracleKeys,
    extendRuleCorpusRules,
  )
import Melusine.Nebula.Source.Ingest (IngestedModule (..), bindingDisplayName, ingestModule)
import Melusine.Nebula.Rewrite.Saturate (defaultSaturationOptions, saturateModule, smContextGraph, smProofSteps)
import Melusine.Nebula.Write.Seal (sealModulePatchOutcome)
import Melusine.Nebula.Synthesis.Core (SynthesisOutcome (..), synthesizeAbstractions)
import Melusine.Nebula.Write.Back (planWriteBack, refuseTypeIncompatible)
import Moonlight.Algebra (JoinSemilattice (..))
import Moonlight.FiniteLattice
  ( principalSupport
  )

import Moonlight.Core (RewriteRuleId (..))
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( SourceRegion,
    TopLevelBinding (..),
    cmBindings,
  )
import Moonlight.EGraph.Pure.Context.Core
  ( cegBase,
    cegSite,
    contextAnalysisValueAt,
    contextRepresentativeAt,
  )
import Moonlight.Flow.Model.Schema.Digest (stableDigest128)
import Moonlight.Rewrite.System (SemanticFidelity (..), TrustTier (..), mkLawId)
import Moonlight.Rewrite.System (RawRewriteRule (..))
import Moonlight.Saturation.Core (SaturationBudget (..))
import Moonlight.Sheaf.Twist.SupportedRuleSpec (SupportedRuleSpec (..), supportedRuleBook)
import Moonlight.Pale.Ghc.Hie.Oracle (ModuleNameOracle (..))
import Moonlight.Pale.Ghc.Hie.SourceKey (HieSourceKeyKind (..), OracleLookup (..))
import Moonlight.Pale.Ghc.Hie.TypeWords (TypeWord (..), TypeWords, typeWords)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

spec :: TestTree
spec =
  testGroup
    "nebula.audit"
    [ testCase "type evidence lattice has identity idempotence commutativity and no absorbing conflict" $
        let alphaFingerprint = typeObservations (Set.singleton (stableDigest128 [1]))
            betaFingerprint = typeObservations (Set.singleton (stableDigest128 [2]))
            joined = join alphaFingerprint betaFingerprint
         in do
              assertEqual "empty observations are left identity" alphaFingerprint (join (typeObservations Set.empty) alphaFingerprint)
              assertEqual "empty observations are right identity" alphaFingerprint (join alphaFingerprint (typeObservations Set.empty))
              assertEqual "observations are idempotent" alphaFingerprint (join alphaFingerprint alphaFingerprint)
              assertEqual "differing observations are retained together" 2 (Set.size (typeEvidenceObservations joined))
              assertEqual "join is commutative" joined (join betaFingerprint alphaFingerprint),
      testCase "unseeded modules audit cleanly with zero typed classes" $ do
        ingested <- requireIngested typeAuditWorkloadWithoutOracle
        report <- requireRight "unseeded type evidence census" (typeEvidenceCensus (imContextGraph ingested))
        assertEqual "no observed classes without a type oracle" 0 (tecObservedClassCount report)
        assertEqual "unseeded audit has no polymorphic classes" 0 (tecPolymorphicClassCount report),
      testCase "stub type spans seed typed classes and audit cleanly" $ do
        workload <- typedAuditWorkload (Map.fromList [("leftTyped", [11, 12]), ("rightTyped", [11, 12])])
        ingested <- requireIngested workload
        report <- requireRight "seeded type evidence census" (typeEvidenceCensus (imContextGraph ingested))
        assertBool "type seeding marks at least one class observed" (tecObservedClassCount report >= 1)
        assertEqual "matching seeded types do not produce polymorphic classes" 0 (tecPolymorphicClassCount report),
      testCase "an injected unsound rewrite is refused at the hunk while the sound sibling seals" $ do
        workload <- seededWorkload refusalFixtureWorkload (Map.fromList [("bloat", [21]), ("lean", [22])])
        ingested <- requireIngested workload
        corpus <-
          requireRight
            "derive corpus"
            ( deriveRuleCorpusWithOracleKeys
                auditConfig
                Set.empty
                (imSpanRows ingested)
                (workloadOracle workload)
                (imConverted ingested)
            )
        unsound <- requireRight "inject unsound rule" (injectUnsoundRule ingested corpus)
        saturated <- requireRight "unsound saturation" (saturateModule defaultSaturationOptions auditConfig ingested unsound)
        let bindingSeeds =
              Map.fromList
                (zip (imBindingNames ingested) (zip (imBindingContexts ingested) (imSeedClasses ingested)))
        case (Map.lookup "bloat" bindingSeeds, Map.lookup "lean" bindingSeeds) of
          (Just (bloatContext, bloatClass), Just (_leanContext, leanClass)) -> do
            bloatRepresentative <-
              requireRight
                "bloat contextual representative"
                (contextRepresentativeAt bloatContext bloatClass (smContextGraph saturated))
            leanRepresentative <-
              requireRight
                "lean contextual representative"
                (contextRepresentativeAt bloatContext leanClass (smContextGraph saturated))
            assertEqual
              "the injected rewrite merges its binding seeds at the authored context"
              bloatRepresentative
              leanRepresentative
            mergedAnalysis <-
              requireRight
                "merged contextual analysis"
                (contextAnalysisValueAt bloatContext bloatRepresentative (smContextGraph saturated))
            assertEqual
              "the merged contextual analysis retains both incompatible type observations"
              2
              (Set.size (foldMap (typeEvidenceObservations . naType) mergedAnalysis))
          _ ->
            assertFailure "expected bloat and lean binding seeds"
        census <- requireRight "saturated type evidence census" (typeEvidenceCensus (smContextGraph saturated))
        assertBool
          ("merged evidence stays visible in the census: " <> show census)
          (tecPolymorphicClassCount census >= 1)
        harvest <- requireRight "harvest" (buildHarvest auditConfig ingested saturated)
        outcome <- requireRight "synthesize" (synthesizeAbstractions auditConfig ingested unsound saturated harvest)
        plannedPatch <- requireRight "plan write-back" (planWriteBack workload ingested outcome)
        certificates <- requireRight "certificates" (moduleCertificates ingested unsound outcome plannedPatch)
        let finalSaturated = soSaturatedModule outcome
            finalProofSteps = smProofSteps finalSaturated
            judgedFinalProofSteps =
              zip
                finalProofSteps
                ( replayStepVerdicts
                    (cegBase (imContextGraph ingested))
                    (classEvidenceByKey (imContextGraph ingested))
                    (fmap replayStepOf finalProofSteps)
                )
        shadowSeed <-
          maybe
            (assertFailure "shadow seed missing from synthesis outcome")
            (pure . cbSeedClass)
            (find ((== "shadow") . cbName) (soBindings outcome))
        shadowCertificate <-
          maybe
            (assertFailure "shadow certificate missing")
            pure
            (find ((== "shadow") . hcBinding) certificates)
        assertEqual
          "module certificates serialize the final registry exactly once"
          (hunkCertificate (smContextGraph finalSaturated) "shadow" shadowSeed judgedFinalProofSteps)
          shadowCertificate
        let verdicts = Map.fromList [(hcBinding certificate, hcTypeVerdict certificate) | certificate <- certificates]
            modulePatch = refuseTypeIncompatible verdicts plannedPatch
        case Map.lookup "bloat" verdicts of
          Just (TypeIncompatible conflicts) ->
            assertBool
              "the conflict names the unsound rule"
              (any ((== RewriteRuleId 2900000) . stcRule) conflicts)
          other ->
            assertFailure ("bloat verdict is not incompatible: " <> show other)
        assertBool
          "bloat is skipped with a type refusal"
          (any (uncurry bloatTypeRefusal) (mpSkipped modulePatch))
        assertBool "shadow remains spliced" (any ((== "shadow") . fst) (mpSpliced modulePatch))
        case sealModulePatchOutcome (mwPath workload) (mwSource workload) modulePatch of
          Sealed sealedSource -> do
            let sealedText = sealedSourceText sealedSource
            assertBool "refused bloat keeps its original body" ("bloat = use alpha alpha" `isInfixOf` sealedText)
            assertBool "sound shadow is contracted" ("shadow = use gamma gamma" `isInfixOf` sealedText)
          _ ->
            assertFailure "module did not seal around the refused hunk",
      testCase "module reports render the type evidence line" $ do
        workload <- typedAuditWorkload (Map.singleton "leftTyped" [31])
        improvement <- requireImproved workload
        assertBool
          "rendered report exposes observed/polymorphic/unobserved counts"
          (any ("type-evidence observed=" `isPrefixOf`) (renderModuleReport (miReport improvement)))
    ]


auditConfig :: NebulaConfig
auditConfig =
  defaultNebulaConfig
    { ncSaturationBudget =
        (ncSaturationBudget defaultNebulaConfig)
          { sbMaxIterations = 2,
            sbMaxNodes = 2000
          }
    }

typeAuditWorkloadWithoutOracle :: ModuleWorkload
typeAuditWorkloadWithoutOracle =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/TypeAuditFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.TypeAuditFixture where",
            "",
            "leftTyped = alpha",
            "rightTyped = beta"
          ],
      mwOracleLookup = OracleMissing []
    }

typedAuditWorkload :: Map String [Word64] -> IO ModuleWorkload
typedAuditWorkload =
  seededWorkload typeAuditWorkloadWithoutOracle

seededWorkload :: ModuleWorkload -> Map String [Word64] -> IO ModuleWorkload
seededWorkload baseWorkload typeWordsByBinding = do
  untypedIngested <- requireIngested baseWorkload
  typeRows <- bindingTypeRows typeWordsByBinding (cmBindings (imConverted untypedIngested))
  pure
    baseWorkload
      { mwOracleLookup =
          let oracle =
                ModuleNameOracle
                  { mnoSourcePath = mwPath baseWorkload,
                    mnoGlobalUses = Map.empty,
                    mnoEvidenceAtSpan = Map.empty,
                    mnoTypeAtSpan = Map.fromList typeRows
                  }
           in OracleFound GivenPathKey (mnoSourcePath oracle) oracle
      }

refusalFixtureWorkload :: ModuleWorkload
refusalFixtureWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/TypeRefusalFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.TypeRefusalFixture where",
            "",
            "bloat = use alpha alpha",
            "lean = beta",
            "shadow = (\\x -> use x x) gamma"
          ],
      mwOracleLookup = OracleMissing []
    }

bloatTypeRefusal :: String -> WriteBackRefusal -> Bool
bloatTypeRefusal bindingName refusal =
  bindingName == "bloat"
    && case refusal of
      RefusedTypeIncompatible {} ->
        True
      _ ->
        False

bindingTypeRows :: Map String [Word64] -> [TopLevelBinding] -> IO [(SourceRegion, Set.Set TypeWords)]
bindingTypeRows typeWordsByBinding bindings =
  concat <$> traverse bindingTypeRow bindings
  where
    bindingTypeRow binding =
      case Map.lookup (bindingDisplayName binding) typeWordsByBinding of
        Nothing ->
          pure []
        Just typeWordValues ->
          case tlbRegion binding of
            Just region ->
              pure [(region, Set.singleton (testTypeWords typeWordValues))]
            Nothing ->
              assertFailure ("binding has no source region: " <> bindingDisplayName binding)

testTypeWords :: [Word64] -> TypeWords
testTypeWords =
  typeWords . fmap (TypeArgumentCount . fromIntegral)

injectUnsoundRule :: IngestedModule -> RuleCorpus -> Either String RuleCorpus
injectUnsoundRule ingested corpus =
  case (findNamedBinding "bloat", findNamedBinding "lean", imBindingContexts ingested) of
    (Just bloatBinding, Just leanBinding, contextValue : _) -> do
      let ruleId = RewriteRuleId 2900000
          lawStamp =
            LawStamp
              { lsLaw = mkLawId 2900000,
                lsTier = RegistryTrusted,
                lsFidelity = Observational
              }
          rule =
            RawRewriteRule
              { rrId = ruleId,
                rrLhs = tlbTerm bloatBinding,
                rrRhs = tlbTerm leanBinding,
                rrCondition = Nothing,
                rrApplicationCondition = Nothing,
                rrPostSubst = Nothing
              }
      unsoundBook <-
        first show $
          supportedRuleBook
            (cegSite (imContextGraph ingested))
            [ SupportedRuleSpec
                { srsSupport = principalSupport contextValue,
                  srsRule = rule
                }
            ]
      first show $
        extendRuleCorpusRules
          (cegSite (imContextGraph ingested))
          unsoundBook
          (Map.singleton ruleId lawStamp)
          corpus
    _ ->
      Left "expected bloat and lean bindings with a binding context"
  where
    findNamedBinding bindingName =
      find ((== bindingName) . bindingDisplayName) (cmBindings (imConverted ingested))

requireIngested :: ModuleWorkload -> IO IngestedModule
requireIngested workload =
  case ingestModule workload of
    Left ingestFailure ->
      assertFailure ("ingest failed: " <> show ingestFailure)
    Right ingested ->
      pure ingested

requireRight :: Show failure => String -> Either failure stage -> IO stage
requireRight stageName =
  either
    (\stageFailure -> assertFailure (stageName <> " failed: " <> show stageFailure))
    pure

requireImproved :: ModuleWorkload -> IO ModuleImprovement
requireImproved workload =
  either
    (\(modulePath, moduleFailure) -> assertFailure ("improve failed for " <> modulePath <> ": " <> show moduleFailure))
    pure
    (improveModule defaultNebulaConfig workload)
