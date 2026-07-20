{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Melusine.Nebula.Rewrite.Corpus
  ( RuleCorpus,
    rcRuleBook,
    rcFactBook,
    rcCompiledProgram,
    rcEvidenceFactCensus,
    rcNumTypeFactCensus,
    rcSelfLawRows,
    rcBindingMetrics,
    rcSiteMetrics,
    rcVocabularyMetrics,
    rcLawTable,
    rcGatedLaws,
    LawStamp (..),
    GatedLawReport (..),
    LawGateReason (..),
    lawGateReasonKey,
    EvidenceFactCensus (..),
    NumTypeFactCensus (..),
    SelfLawRow (..),
    selfLawRefusalKey,
    deriveRuleCorpus,
    deriveRuleCorpusWithOracleKeys,
    deriveRuleCorpusWithOracleKeysAndReason,
    extendRuleCorpusRules,
    disjointUnionRuleBooks,
  )
where

import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Melusine.Nebula.Core
  ( CorpusSources (..),
    NebulaConfig (..),
    NebulaError (..),
    NebulaFactBook,
    NebulaRule,
    NebulaRuleBook,
    NebulaUniverse,
  )
import Moonlight.Core (Pattern, RewriteRuleId (..))
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( ConvertedModule,
    HsExprBindingCorpus (..),
    HsExprBindingRuleMetrics,
    HsExprF,
    HsExprSupportRuleMetrics (..),
    HsExprVocabularyRuleMetrics (..),
    ScopeCtx,
    SourceRegion,
    SpanClassRow (..),
    SpannedExpr (..),
    TopLevelBinding (..),
    cmBindings,
    cmScopeIndex,
    convertedModuleContextLattice,
    eraseSpannedExpr,
    hsExprBindingCorpus,
    hsExprBindingFrontLawId,
    hsExprBetaLawId,
    hsExprCompositionLawId,
    hsExprEtaLawId,
    hsExprEvidenceFactRulesFor,
    hsExprLetInlineLawId,
    hsExprLawfulFunctorFactId,
    hsExprLawfulMonadFactId,
    hsExprLawfulNumTypeFactId,
    hsExprParErasureLawFamily,
    hsExprRenamerLawFamily,
    hsExprSelfUnfoldLawFamily,
    hsExprSiteLawFamily,
    hsExprSupportRuleMetrics,
    hsExprVocabularyLawIds,
    hsLawfulFunctorInstanceOrigins,
    hsLawfulMonadInstanceOrigins,
    hsLawfulNumTypeWords,
    SelfLawRefusal (..),
    SelfLawRow (..),
    scopeBottomCtx,
  )
import Moonlight.Flow.Model.Schema.Digest (StableDigest128, stableDigest128)
import Moonlight.Rewrite.System
  ( LawBook (..),
    LawId,
    LawSpec (..),
    OracleKey,
    SemanticFidelity (..),
    TrustTier (..),
    lawIdKey,
    oracleRequirementKeys,
    oracleRequirementSatisfiedBy,
  )
import Moonlight.Rewrite.System (RawRewriteRule (..))
import Moonlight.Saturation.Context.Program.Plan
  ( Program,
    ProgramStage (CompiledProgramStage),
  )
import Moonlight.Saturation.Support.Compile (compileSupportProgram)
import Moonlight.Sheaf.Twist.SupportedRuleSpec
  ( SupportedRuleSpec (..),
    supportedRuleBook,
    supportedRules,
  )
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    fromFiniteLattice,
  )
import Moonlight.Pale.Ghc.Hie.Oracle (ModuleNameOracle (..), PackageUnitParseFailure, ResolvedOrigin, originAcceptedBy)
import Moonlight.Pale.Ghc.Hie.SourceKey (OracleAttachFailure)
import Moonlight.Pale.Ghc.Hie.TypeWords (TypeWords, typeWordsList)

type LawStamp :: Type
data LawStamp = LawStamp
  { lsLaw :: !LawId,
    lsTier :: !TrustTier,
    lsFidelity :: !SemanticFidelity
  }
  deriving stock (Eq, Ord, Show)

type GatedLawReport :: Type
data GatedLawReport = GatedLawReport
  { glrLaw :: !LawId,
    glrReason :: !LawGateReason,
    glrRuleCount :: !Int
  }
  deriving stock (Eq, Ord, Show)

type LawGateReason :: Type
data LawGateReason
  = GateMissingOracleKeys !(Set.Set OracleKey)
  | GateOracleUnattached !OracleAttachFailure
  | GateTierInadmissible !TrustTier
  | GateFidelityInadmissible !SemanticFidelity
  deriving stock (Eq, Ord, Show)

lawGateReasonKey :: LawGateReason -> String
lawGateReasonKey = \case
  GateMissingOracleKeys {} ->
    "missing-oracle-keys"
  GateOracleUnattached {} ->
    "oracle-unattached"
  GateTierInadmissible {} ->
    "tier-inadmissible"
  GateFidelityInadmissible {} ->
    "fidelity-inadmissible"

selfLawRefusalKey :: SelfLawRefusal -> String
selfLawRefusalKey = \case
  RefusedNotLambdaSpine ->
    "not-lambda-spine"
  RefusedNotSizeDecreasing ->
    "not-size-decreasing"
  RefusedSelfRecursive ->
    "self-recursive"
  RefusedMultiNameEquation ->
    "multi-name-equation"

type EvidenceFactCensus :: Type
data EvidenceFactCensus = EvidenceFactCensus
  { efcLawful :: !Int,
    efcUnlawful :: !Int,
    efcAmbiguous :: !Int
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup EvidenceFactCensus where
  left <> right =
    EvidenceFactCensus
      { efcLawful = efcLawful left + efcLawful right,
        efcUnlawful = efcUnlawful left + efcUnlawful right,
        efcAmbiguous = efcAmbiguous left + efcAmbiguous right
      }

instance Monoid EvidenceFactCensus where
  mempty =
    EvidenceFactCensus
      { efcLawful = 0,
        efcUnlawful = 0,
        efcAmbiguous = 0
      }

type NumTypeFactCensus :: Type
data NumTypeFactCensus = NumTypeFactCensus
  { ntfcLawfulSpanCount :: !Int,
    ntfcUnlawfulSpanCount :: !Int,
    ntfcUnobservedSpanCount :: !Int
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup NumTypeFactCensus where
  left <> right =
    NumTypeFactCensus
      { ntfcLawfulSpanCount = ntfcLawfulSpanCount left + ntfcLawfulSpanCount right,
        ntfcUnlawfulSpanCount = ntfcUnlawfulSpanCount left + ntfcUnlawfulSpanCount right,
        ntfcUnobservedSpanCount = ntfcUnobservedSpanCount left + ntfcUnobservedSpanCount right
      }

instance Monoid NumTypeFactCensus where
  mempty =
    NumTypeFactCensus
      { ntfcLawfulSpanCount = 0,
        ntfcUnlawfulSpanCount = 0,
        ntfcUnobservedSpanCount = 0
      }

type RuleCorpus :: Type
data RuleCorpus = RuleCorpus
  { rcRuleBook :: !NebulaRuleBook,
    rcFactBook :: !NebulaFactBook,
    rcCompiledProgram :: !(Program 'CompiledProgramStage NebulaUniverse),
    rcEvidenceFactCensus :: !EvidenceFactCensus,
    rcNumTypeFactCensus :: !NumTypeFactCensus,
    rcSelfLawRows :: ![SelfLawRow],
    rcBindingMetrics :: !(Maybe HsExprBindingRuleMetrics),
    rcSiteMetrics :: !HsExprSupportRuleMetrics,
    rcVocabularyMetrics :: !HsExprVocabularyRuleMetrics,
    rcLawTable :: !(Map RewriteRuleId LawStamp),
    rcGatedLaws :: ![GatedLawReport]
  }

deriveRuleCorpus :: NebulaConfig -> [SpanClassRow] -> Maybe ModuleNameOracle -> ConvertedModule -> Either NebulaError RuleCorpus
deriveRuleCorpus config =
  deriveRuleCorpusWithOracleKeys config Set.empty

deriveRuleCorpusWithOracleKeys ::
  NebulaConfig ->
  Set.Set OracleKey ->
  [SpanClassRow] ->
  Maybe ModuleNameOracle ->
  ConvertedModule ->
  Either NebulaError RuleCorpus
deriveRuleCorpusWithOracleKeys config satisfiedOracleKeys spanRows maybeOracle convertedModule = do
  deriveRuleCorpusWithOracleKeysAndReason config satisfiedOracleKeys Nothing spanRows maybeOracle convertedModule

deriveRuleCorpusWithOracleKeysAndReason ::
  NebulaConfig ->
  Set.Set OracleKey ->
  Maybe OracleAttachFailure ->
  [SpanClassRow] ->
  Maybe ModuleNameOracle ->
  ConvertedModule ->
  Either NebulaError RuleCorpus
deriveRuleCorpusWithOracleKeysAndReason config satisfiedOracleKeys oracleMissingReason spanRows maybeOracle convertedModule = do
  site <-
    convertedModuleSite convertedModule
  siteLawBook <-
    first (NebulaRuleDerivationError . show) (hsExprSiteLawFamily convertedModule)
  parErasureLawBook <-
    first (NebulaRuleDerivationError . show) (hsExprParErasureLawFamily convertedModule)
  renamerLawBook <-
    first (NebulaRuleDerivationError . show) (hsExprRenamerLawFamily convertedModule)
  let (selfLawRows, selfLawBook) = hsExprSelfUnfoldLawFamily convertedModule
      lawBook = siteLawBook <> parErasureLawBook <> renamerLawBook <> selfLawBook
      vocabularyLawCounts =
        Map.filterWithKey (\lawIdValue _ -> lawIdValue `Set.member` hsExprVocabularyLawIds) (lawCountsFromBook renamerLawBook)
  admission <-
    admitLawBook site config satisfiedOracleKeys oracleMissingReason lawBook
  (evidenceFacts, evidenceCensus) <-
    evidenceFactBook site convertedModule spanRows maybeOracle
  (numTypeFacts, numTypeCensus) <-
    numTypeFactBook site convertedModule spanRows maybeOracle
  let factBook = evidenceFacts <> numTypeFacts
  siteMetrics <-
    coherentSiteMetrics (hsExprSupportRuleMetrics convertedModule) (admittedLawCounts admission)
  vocabularyMetrics <-
    coherentVocabularyMetrics vocabularyLawCounts (admittedLawCounts admission)
  case ncCorpusSources config of
    SiteFamilyOnly ->
      finalizeRuleCorpus
        site
        (admittedRuleBook admission)
        factBook
        evidenceCensus
        numTypeCensus
        selfLawRows
        Nothing
        siteMetrics
        vocabularyMetrics
        (admittedLawTable admission)
        (gatedLawReports admission)
    SiteAndBindingFront -> do
      bindingCorpus <-
        first (NebulaBindingFrontError . show) (hsExprBindingCorpus site convertedModule)
      case lawStampRejectionReason config bindingFrontLawStamp of
        Nothing -> do
          unionBook <- disjointUnionRuleBooks [admittedRuleBook admission, hbcRules bindingCorpus]
          let bindingLawTable = lawTableFromRuleBook bindingFrontLawStamp (hbcRules bindingCorpus)
              lawTable = admittedLawTable admission <> bindingLawTable
          finalizeRuleCorpus
            site
            unionBook
            (factBook <> hbcFacts bindingCorpus)
            evidenceCensus
            numTypeCensus
            selfLawRows
            (Just (hbcMetrics bindingCorpus))
            siteMetrics
            vocabularyMetrics
            lawTable
            (gatedLawReports admission)
        Just reason ->
          finalizeRuleCorpus
            site
            (admittedRuleBook admission)
            factBook
            evidenceCensus
            numTypeCensus
            selfLawRows
            Nothing
            siteMetrics
            vocabularyMetrics
            (admittedLawTable admission)
            ( gatedLawReports admission
                <> [ GatedLawReport
                       { glrLaw = hsExprBindingFrontLawId,
                         glrReason = reason,
                         glrRuleCount = length (supportedRules (hbcRules bindingCorpus))
                       }
                   ]
            )

convertedModuleSite :: ConvertedModule -> Either NebulaError (PreparedContextSite ScopeCtx)
convertedModuleSite convertedModule =
  fromFiniteLattice
    <$> first (NebulaLatticeError . show) (convertedModuleContextLattice convertedModule)

finalizeRuleCorpus ::
  PreparedContextSite ScopeCtx ->
  NebulaRuleBook ->
  NebulaFactBook ->
  EvidenceFactCensus ->
  NumTypeFactCensus ->
  [SelfLawRow] ->
  Maybe HsExprBindingRuleMetrics ->
  HsExprSupportRuleMetrics ->
  HsExprVocabularyRuleMetrics ->
  Map RewriteRuleId LawStamp ->
  [GatedLawReport] ->
  Either NebulaError RuleCorpus
finalizeRuleCorpus site ruleBook factBook evidenceCensus numTypeCensus selfLawRows bindingMetrics siteMetrics vocabularyMetrics lawTable gatedLaws = do
  ensureLawTableTotal ruleBook lawTable
  compiledProgram <-
    compileRuleCorpusProgram site ruleBook factBook
  pure
    RuleCorpus
      { rcRuleBook = ruleBook,
        rcFactBook = factBook,
        rcCompiledProgram = compiledProgram,
        rcEvidenceFactCensus = evidenceCensus,
        rcNumTypeFactCensus = numTypeCensus,
        rcSelfLawRows = selfLawRows,
        rcBindingMetrics = bindingMetrics,
        rcSiteMetrics = siteMetrics,
        rcVocabularyMetrics = vocabularyMetrics,
        rcLawTable = lawTable,
        rcGatedLaws = gatedLaws
      }

compileRuleCorpusProgram ::
  PreparedContextSite ScopeCtx ->
  NebulaRuleBook ->
  NebulaFactBook ->
  Either NebulaError (Program 'CompiledProgramStage NebulaUniverse)
compileRuleCorpusProgram site ruleBook factBook =
  first
    (NebulaSaturationError . show)
    (compileSupportProgram @NebulaUniverse site ruleBook factBook)

extendRuleCorpusRules ::
  PreparedContextSite ScopeCtx ->
  NebulaRuleBook ->
  Map RewriteRuleId LawStamp ->
  RuleCorpus ->
  Either NebulaError RuleCorpus
extendRuleCorpusRules site additionalRuleBook additionalLawTable corpus = do
  ensureLawTableTotal additionalRuleBook additionalLawTable
  extendedRuleBook <-
    disjointUnionRuleBooks [rcRuleBook corpus, additionalRuleBook]
  finalizeRuleCorpus
    site
    extendedRuleBook
    (rcFactBook corpus)
    (rcEvidenceFactCensus corpus)
    (rcNumTypeFactCensus corpus)
    (rcSelfLawRows corpus)
    (rcBindingMetrics corpus)
    (rcSiteMetrics corpus)
    (rcVocabularyMetrics corpus)
    (Map.union (rcLawTable corpus) additionalLawTable)
    (rcGatedLaws corpus)

evidenceFactBook ::
  PreparedContextSite ScopeCtx ->
  ConvertedModule ->
  [SpanClassRow] ->
  Maybe ModuleNameOracle ->
  Either NebulaError (NebulaFactBook, EvidenceFactCensus)
evidenceFactBook site convertedModule spanRows maybeOracle = do
  lawfulFunctorOrigins <-
    resolveEvidenceOrigins hsLawfulFunctorInstanceOrigins
  lawfulMonadOrigins <-
    resolveEvidenceOrigins hsLawfulMonadInstanceOrigins
  lawfulFunctorFacts <-
    first (NebulaRuleDerivationError . show) $
      hsExprEvidenceFactRulesFor
        site
        hsExprLawfulFunctorFactId
        0
        (evidencePatternsFor lawfulFunctorOrigins convertedModule spanRows maybeOracle)
  lawfulMonadFacts <-
    first (NebulaRuleDerivationError . show) $
      hsExprEvidenceFactRulesFor
        site
        hsExprLawfulMonadFactId
        10000
        (evidencePatternsFor lawfulMonadOrigins convertedModule spanRows maybeOracle)
  pure
    ( lawfulFunctorFacts <> lawfulMonadFacts,
      evidenceFactCensus (lawfulFunctorOrigins <> lawfulMonadOrigins) spanRows maybeOracle
    )

resolveEvidenceOrigins :: Either PackageUnitParseFailure (Set.Set ResolvedOrigin) -> Either NebulaError (Set.Set ResolvedOrigin)
resolveEvidenceOrigins =
  first (NebulaRuleDerivationError . ("evidence origin parse failed: " <>) . show)

evidencePatternsFor :: Set.Set ResolvedOrigin -> ConvertedModule -> [SpanClassRow] -> Maybe ModuleNameOracle -> [(ScopeCtx, Pattern HsExprF)]
evidencePatternsFor acceptedOrigins convertedModule spanRows maybeOracle =
  maybe
    []
    ( \oracle ->
        let regions =
              evidencedSpanRows spanRows oracle
            patternsByRegion =
              foldMap (spannedPatternsByRegion . tlbSpannedTerm) (cmBindings convertedModule)
            supportScope =
              scopeBottomCtx (cmScopeIndex convertedModule)
         in [ (supportScope, patternValue)
            | region <- Set.toAscList regions,
              Just origins <- [Map.lookup region (mnoEvidenceAtSpan oracle)],
              evidenceOriginsAcceptedBy acceptedOrigins origins,
              Just patternValue <- [Map.lookup region patternsByRegion]
            ]
    )
    maybeOracle

evidenceFactCensus :: Set.Set ResolvedOrigin -> [SpanClassRow] -> Maybe ModuleNameOracle -> EvidenceFactCensus
evidenceFactCensus lawfulEvidenceOrigins spanRows maybeOracle =
  maybe
    emptyEvidenceFactCensus
    ( \oracle ->
        foldMap
          (evidenceOriginsCensus lawfulEvidenceOrigins)
          [ origins
          | region <- Set.toAscList (evidencedSpanRows spanRows oracle),
            Just origins <- [Map.lookup region (mnoEvidenceAtSpan oracle)]
          ]
    )
    maybeOracle

emptyEvidenceFactCensus :: EvidenceFactCensus
emptyEvidenceFactCensus =
  EvidenceFactCensus
    { efcLawful = 0,
      efcUnlawful = 0,
      efcAmbiguous = 0
    }

evidencedSpanRows :: [SpanClassRow] -> ModuleNameOracle -> Set.Set SourceRegion
evidencedSpanRows spanRows oracle =
  Map.keysSet (mnoEvidenceAtSpan oracle)
    `Set.intersection` Set.fromList (fmap scrRegion spanRows)

evidenceOriginsAcceptedBy :: Set.Set ResolvedOrigin -> Set.Set ResolvedOrigin -> Bool
evidenceOriginsAcceptedBy acceptedOrigins origins =
  case Set.toAscList origins of
    [origin] ->
      originAcceptedBy origin acceptedOrigins
    _ ->
      False

evidenceOriginsCensus :: Set.Set ResolvedOrigin -> Set.Set ResolvedOrigin -> EvidenceFactCensus
evidenceOriginsCensus acceptedOrigins origins =
  case Set.toAscList origins of
    [origin]
      | originAcceptedBy origin acceptedOrigins ->
          emptyEvidenceFactCensus {efcLawful = 1}
      | otherwise ->
          emptyEvidenceFactCensus {efcUnlawful = 1}
    _ ->
      emptyEvidenceFactCensus {efcAmbiguous = 1}

numTypeFactBook ::
  PreparedContextSite ScopeCtx ->
  ConvertedModule ->
  [SpanClassRow] ->
  Maybe ModuleNameOracle ->
  Either NebulaError (NebulaFactBook, NumTypeFactCensus)
numTypeFactBook site convertedModule spanRows maybeOracle = do
  numTypeFacts <-
    first (NebulaRuleDerivationError . show) $
      hsExprEvidenceFactRulesFor
        site
        hsExprLawfulNumTypeFactId
        20000
        (numTypePatterns convertedModule spanRows maybeOracle)
  pure
    ( numTypeFacts,
      numTypeFactCensus spanRows maybeOracle
    )

numTypePatterns :: ConvertedModule -> [SpanClassRow] -> Maybe ModuleNameOracle -> [(ScopeCtx, Pattern HsExprF)]
numTypePatterns convertedModule spanRows maybeOracle =
  maybe
    []
    ( \oracle ->
        let regions =
              Set.fromList (fmap scrRegion spanRows)
            patternsByRegion =
              foldMap (spannedPatternsByRegion . tlbSpannedTerm) (cmBindings convertedModule)
            supportScope =
              scopeBottomCtx (cmScopeIndex convertedModule)
         in [ (supportScope, patternValue)
            | region <- Set.toAscList regions,
              Just typeWords <- [Map.lookup region (mnoTypeAtSpan oracle)],
              lawfulNumTypeWords typeWords,
              Just patternValue <- [Map.lookup region patternsByRegion]
            ]
    )
    maybeOracle

numTypeFactCensus :: [SpanClassRow] -> Maybe ModuleNameOracle -> NumTypeFactCensus
numTypeFactCensus spanRows maybeOracle =
  maybe
    (mempty {ntfcUnobservedSpanCount = Set.size (Set.fromList (fmap scrRegion spanRows))})
    ( \oracle ->
        foldMap
          ( \region ->
              maybe
                (mempty {ntfcUnobservedSpanCount = 1})
                numTypeWordsCensus
                (Map.lookup region (mnoTypeAtSpan oracle))
          )
          (Set.toAscList (Set.fromList (fmap scrRegion spanRows)))
    )
    maybeOracle

numTypeWordsCensus :: Set.Set TypeWords -> NumTypeFactCensus
numTypeWordsCensus typeWords
  | Set.null typeWords =
      mempty {ntfcUnobservedSpanCount = 1}
  | lawfulNumTypeWords typeWords =
      mempty {ntfcLawfulSpanCount = 1}
  | otherwise =
      mempty {ntfcUnlawfulSpanCount = 1}

lawfulNumTypeWords :: Set.Set TypeWords -> Bool
lawfulNumTypeWords typeWords =
  not (Set.null typeWords)
    && Set.isSubsetOf (Set.map (stableDigest128 . typeWordsList) typeWords) lawfulNumTypeDigests

lawfulNumTypeDigests :: Set.Set StableDigest128
lawfulNumTypeDigests =
  Set.map (stableDigest128 . typeWordsList) hsLawfulNumTypeWords

spannedPatternsByRegion :: SpannedExpr -> Map SourceRegion (Pattern HsExprF)
spannedPatternsByRegion spannedExpr =
  maybe
    childPatterns
    (\region -> Map.insert region (eraseSpannedExpr spannedExpr) childPatterns)
    (sxRegion spannedExpr)
  where
    childPatterns =
      foldMap spannedPatternsByRegion (sxNode spannedExpr)

type LawAdmission :: Type
data LawAdmission = LawAdmission
  { admittedRuleBook :: !NebulaRuleBook,
    admittedLawTable :: !(Map RewriteRuleId LawStamp),
    admittedLawCounts :: !(Map LawId Int),
    gatedLawReports :: ![GatedLawReport]
  }

admitLawBook ::
  PreparedContextSite ScopeCtx ->
  NebulaConfig ->
  Set.Set OracleKey ->
  Maybe OracleAttachFailure ->
  LawBook (SupportedRuleSpec ScopeCtx NebulaRule) ->
  Either NebulaError LawAdmission
admitLawBook site config satisfiedOracleKeys oracleMissingReason (LawBook lawSpecs) = do
  let (gatedRows, admittedSpecs) =
        partitionEithers (fmap admitOne lawSpecs)
      admittedRules =
        fmap lawRule admittedSpecs
  ruleBook <-
    first (NebulaRuleDerivationError . show) (supportedRuleBook site admittedRules)
  pure
    LawAdmission
      { admittedRuleBook = ruleBook,
        admittedLawTable = lawTableFromLawSpecs admittedSpecs,
        admittedLawCounts = lawCounts admittedSpecs,
        gatedLawReports = gatedReportsFromRows gatedRows
      }
  where
    admitOne lawSpec =
      case lawRejectionReason config satisfiedOracleKeys oracleMissingReason lawSpec of
        Nothing ->
          Right lawSpec
        Just reason ->
          Left (lawId lawSpec, reason)

lawRejectionReason :: NebulaConfig -> Set.Set OracleKey -> Maybe OracleAttachFailure -> LawSpec rule -> Maybe LawGateReason
lawRejectionReason config satisfiedOracleKeys oracleAttachFailure lawSpec
  | Just reason <- lawStampRejectionReason config (lawStampFromSpec lawSpec) =
      Just reason
  | not (oracleRequirementSatisfiedBy satisfiedOracleKeys (lawOracle lawSpec)) =
      Just $
        maybe
          (GateMissingOracleKeys (Set.difference (oracleRequirementKeys (lawOracle lawSpec)) satisfiedOracleKeys))
          GateOracleUnattached
          oracleAttachFailure
  | otherwise =
      Nothing

lawStampRejectionReason :: NebulaConfig -> LawStamp -> Maybe LawGateReason
lawStampRejectionReason config lawStamp
  | not (lsTier lawStamp `Set.member` ncAdmissibleTiers config) =
      Just (GateTierInadmissible (lsTier lawStamp))
  | not (lsFidelity lawStamp `Set.member` ncAdmissibleFidelities config) =
      Just (GateFidelityInadmissible (lsFidelity lawStamp))
  | otherwise =
      Nothing

lawStampFromSpec :: LawSpec rule -> LawStamp
lawStampFromSpec lawSpec =
  LawStamp
    { lsLaw = lawId lawSpec,
      lsTier = lawTier lawSpec,
      lsFidelity = lawFidelity lawSpec
    }

bindingFrontLawStamp :: LawStamp
bindingFrontLawStamp =
  LawStamp
    { lsLaw = hsExprBindingFrontLawId,
      lsTier = ParserVerified,
      lsFidelity = Observational
    }

lawTableFromLawSpecs :: [LawSpec (SupportedRuleSpec ScopeCtx NebulaRule)] -> Map RewriteRuleId LawStamp
lawTableFromLawSpecs =
  Map.fromList . fmap (\lawSpec -> (rrId (srsRule (lawRule lawSpec)), lawStampFromSpec lawSpec))

lawTableFromRuleBook :: LawStamp -> NebulaRuleBook -> Map RewriteRuleId LawStamp
lawTableFromRuleBook lawStamp =
  Map.fromList . fmap (\ruleSpec -> (rrId (srsRule ruleSpec), lawStamp)) . supportedRules

lawCounts :: [LawSpec rule] -> Map LawId Int
lawCounts =
  Map.fromListWith (+) . fmap (\lawSpec -> (lawId lawSpec, 1))

lawCountsFromBook :: LawBook rule -> Map LawId Int
lawCountsFromBook (LawBook lawSpecs) =
  lawCounts lawSpecs

coherentVocabularyMetrics :: Map LawId Int -> Map LawId Int -> Either NebulaError HsExprVocabularyRuleMetrics
coherentVocabularyMetrics generatedCounts admittedCounts =
  if all vocabularyLawCoherent (Map.toAscList generatedCounts)
    then
      Right
        HsExprVocabularyRuleMetrics
          { hvrmVocabularyLawCount = Map.size generatedCounts,
            hvrmVocabularyGeneratedRuleCount = sum (Map.elems generatedCounts),
            hvrmVocabularyAdmittedRuleCount = sum (fmap admittedVocabularyCount (Map.keys generatedCounts)),
            hvrmVocabularyGatedLawCount = length (filter vocabularyLawGated (Map.toAscList generatedCounts))
          }
    else
      Left
        ( NebulaRuleDerivationError
            ( "admitted vocabulary rule family is not all-in-or-gated: generated="
                <> show [(lawIdKey lawIdValue, count) | (lawIdValue, count) <- Map.toAscList generatedCounts]
                <> " admitted="
                <> show [(lawIdKey lawIdValue, count) | (lawIdValue, count) <- Map.toAscList admittedCounts, Map.member lawIdValue generatedCounts]
            )
        )
  where
    admittedVocabularyCount lawIdValue =
      Map.findWithDefault 0 lawIdValue admittedCounts
    vocabularyLawCoherent (lawIdValue, generatedCount) =
      let admittedCount = admittedVocabularyCount lawIdValue
       in admittedCount == 0 || admittedCount == generatedCount
    vocabularyLawGated (lawIdValue, _) =
      admittedVocabularyCount lawIdValue == 0

gatedReportsFromRows :: [(LawId, LawGateReason)] -> [GatedLawReport]
gatedReportsFromRows =
  fmap rowReport . Map.toAscList . Map.fromListWith (+) . fmap (\row -> (row, 1 :: Int))
  where
    rowReport ((lawIdValue, reason), count) =
      GatedLawReport
        { glrLaw = lawIdValue,
          glrReason = reason,
          glrRuleCount = count
        }

ensureLawTableTotal :: NebulaRuleBook -> Map RewriteRuleId LawStamp -> Either NebulaError ()
ensureLawTableTotal ruleBook lawTable =
  let ruleSpecs = supportedRules ruleBook
      ruleIds = Set.fromList (fmap (rrId . srsRule) ruleSpecs)
      tableIds = Map.keysSet lawTable
   in if ruleIds == tableIds && Map.size lawTable == length ruleSpecs
        then Right ()
        else
          Left
            ( NebulaRuleDerivationError
                ( "law table is not total over admitted rules; missing="
                    <> show [ruleKey | RewriteRuleId ruleKey <- Set.toList (Set.difference ruleIds tableIds)]
                    <> " extra="
                    <> show [ruleKey | RewriteRuleId ruleKey <- Set.toList (Set.difference tableIds ruleIds)]
                    <> " rule-count="
                    <> show (length ruleSpecs)
                    <> " law-table-count="
                    <> show (Map.size lawTable)
                )
            )

disjointUnionRuleBooks :: [NebulaRuleBook] -> Either NebulaError NebulaRuleBook
disjointUnionRuleBooks ruleBooks =
  let identifierCounts =
        Map.fromListWith
          (+)
          [ (rrId (srsRule ruleSpec), 1 :: Int)
          | ruleBook <- ruleBooks,
            ruleSpec <- supportedRules ruleBook
          ]
      overlap =
        Map.keysSet (Map.filter (> 1) identifierCounts)
   in if Set.null overlap
        then Right (mconcat ruleBooks)
        else
          Left
            ( NebulaRuleDerivationError
                ( "admitted rule identifiers overlap: "
                    <> show [ruleKey | RewriteRuleId ruleKey <- Set.toList overlap]
                )
            )

coherentSiteMetrics :: HsExprSupportRuleMetrics -> Map LawId Int -> Either NebulaError HsExprSupportRuleMetrics
coherentSiteMetrics siteMetrics admittedCounts =
  let lambdaSites = hsrmLambdaSiteCount siteMetrics
      letSites = hsrmLetSiteCount siteMetrics
      etaRuleCount = countLaw hsExprEtaLawId
      compositionRuleCount = countLaw hsExprCompositionLawId
      betaRuleCount = countLaw hsExprBetaLawId
      letRuleCount = countLaw hsExprLetInlineLawId
      totalRuleCount = etaRuleCount + compositionRuleCount + betaRuleCount + letRuleCount
      familyCoherent =
        etaRuleCount == lambdaSites
          && compositionRuleCount `elem` [0, lambdaSites]
          && betaRuleCount == lambdaSites
          && letRuleCount == letSites
   in if familyCoherent
        then
          Right
            siteMetrics
              { hsrmEtaRuleCount = etaRuleCount,
                hsrmCompositionRuleCount = compositionRuleCount,
                hsrmBetaRuleCount = betaRuleCount,
                hsrmLetRuleCount = letRuleCount,
                hsrmTotalRuleCount = totalRuleCount,
                hsrmDiagnosticSpanCount = totalRuleCount
              }
        else
          Left
            ( NebulaRuleDerivationError
                ( "admitted support rule family incoherent with site counts: "
                    <> show siteMetrics
                    <> " admitted="
                    <> show [(lawIdKey lawIdValue, count) | (lawIdValue, count) <- Map.toAscList admittedCounts]
                )
            )
  where
    countLaw lawIdValue =
      Map.findWithDefault 0 lawIdValue admittedCounts
