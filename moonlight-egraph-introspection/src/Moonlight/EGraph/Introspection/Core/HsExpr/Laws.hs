{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.EGraph.Introspection.Core.HsExpr.Laws
  ( hsExprEtaLawId,
    hsExprCompositionLawId,
    hsExprBetaLawId,
    hsExprLetInlineLawId,
    hsExprBindingFrontLawId,
    hsExprComposeOracleKey,
    hsExprMapOracleKey,
    hsExprFmapOracleKey,
    hsExprFilterOracleKey,
    hsExprAppendOracleKey,
    hsExprConcatOracleKey,
    hsExprConcatMapOracleKey,
    hsExprReverseOracleKey,
    hsExprIdOracleKey,
    hsExprAndOracleKey,
    hsExprBindOracleKey,
    hsExprReturnOracleKey,
    hsExprPureOracleKey,
    hsExprPlusOracleKey,
    hsExprTimesOracleKey,
    hsExprMapFusionLawId,
    hsExprFmapFusionLawId,
    hsExprFilterFusionLawId,
    hsExprMapFilterInterchangeLawId,
    hsExprAppendRightIdentityLawId,
    hsExprAppendLeftIdentityLawId,
    hsExprAppendAssociativityLawId,
    hsExprMapAppendFactorLawId,
    hsExprConcatMapLawId,
    hsExprReverseInvolutionLawId,
    hsExprMapIdLawId,
    hsExprFmapIdLawId,
    hsExprMonadLeftIdentityLawId,
    hsExprMonadRightIdentityLawId,
    hsExprParErasureLawId,
    hsExprPlusAssociativityLawId,
    hsExprPlusCommutativityLawId,
    hsExprTimesAssociativityLawId,
    hsExprTimesCommutativityLawId,
    hsExprPlusUnitLawId,
    hsExprTimesUnitLawId,
    hsExprLawfulFunctorFactId,
    hsExprLawfulMonadFactId,
    hsExprLawfulNumTypeFactId,
    hsExprEvidenceFactRulesFor,
    HsExprNameTable (..),
    HsExprVocabularyRuleMetrics (..),
    hsExprOracleKeyTable,
    hsExprVocabularyLawIds,
    hsAcceptedComposeOrigins,
    hsAcceptedMapOrigins,
    hsAcceptedFmapOrigins,
    hsAcceptedFilterOrigins,
    hsAcceptedAppendOrigins,
    hsAcceptedConcatOrigins,
    hsAcceptedConcatMapOrigins,
    hsAcceptedReverseOrigins,
    hsAcceptedIdOrigins,
    hsAcceptedAndOrigins,
    hsAcceptedBindOrigins,
    hsAcceptedReturnOrigins,
    hsAcceptedPureOrigins,
    hsAcceptedPlusOrigins,
    hsAcceptedTimesOrigins,
    hsLawfulFunctorInstanceOrigins,
    hsLawfulMonadInstanceOrigins,
    hsLawfulNumTypeWords,
    hsExprLawRuleIdBase,
    hsExprSiteLawFamily,
    hsExprParErasureLawFamily,
    hsExprRenamerLawFamily,
  )
where

import Data.Kind (Type)
import Data.Set qualified as Set
import GHC.Types.Name.Occurrence (mkVarOcc)
import GHC.Types.Name.Reader (RdrName, mkRdrUnqual)
import Moonlight.Core (Pattern)
import Moonlight.EGraph.Introspection.Core.HsExpr.Equation
  ( HsExprLawRule,
    equationRule,
  )
import Moonlight.EGraph.Introspection.Core.HsExpr.Front
  ( HsExprLawEmitError (..),
  )
import Moonlight.EGraph.Introspection.Core.HsExpr.Spans
  ( HsExprSiteRuleError,
    HsExprSiteRuleKind (..),
    hsExprSupportedLawRules,
  )
import Moonlight.Rewrite.System
  ( LawBook (..),
    LawId,
    LawSpec (..),
    OracleKey,
    OracleRequirement (..),
    SemanticFidelity (..),
    TrustTier (..),
    mkLawId,
    mkOracleKey,
  )
import Moonlight.Rewrite.System (RewriteCondition (..), data GuardRoot, guardHasFact)
import Moonlight.Rewrite.System (FactRule, FactRuleId (..), RawFactRule (..))
import Moonlight.Rewrite.System (FactId (..))
import Moonlight.Rewrite.System (RawRewriteRule (..))
import Moonlight.Sheaf.Context.Site
  ( PreparedContextSite,
    PreparedContextSupportError,
  )
import Moonlight.Sheaf.Twist.SupportedRuleSpec
  ( SupportedFactBook,
    SupportedFactSpec (..),
    SupportedRuleSpec (..),
    supportedFactBook,
  )
import Moonlight.Pale.Ghc.Expr (ConvertedModule (..), HsExprF, ScopeCtx, scopeBottomCtx)
import Moonlight.Pale.Ghc.Hie.Oracle (PackageUnitParseFailure, ResolvedOrigin, mkResolvedOrigin)
import Moonlight.Pale.Ghc.Hie.TypeWords (TypeWords, tyConTypeWords)
import Moonlight.FiniteLattice
  ( principalSupport
  )

hsExprLawRuleIdBase :: Int
hsExprLawRuleIdBase =
  2000000

hsExprEtaLawId :: LawId
hsExprEtaLawId =
  mkLawId (hsExprLawRuleIdBase + 1)

hsExprCompositionLawId :: LawId
hsExprCompositionLawId =
  mkLawId (hsExprLawRuleIdBase + 2)

hsExprBetaLawId :: LawId
hsExprBetaLawId =
  mkLawId (hsExprLawRuleIdBase + 3)

hsExprLetInlineLawId :: LawId
hsExprLetInlineLawId =
  mkLawId (hsExprLawRuleIdBase + 4)

hsExprMapFusionLawId :: LawId
hsExprMapFusionLawId =
  mkLawId (hsExprLawRuleIdBase + 5)

hsExprFmapFusionLawId :: LawId
hsExprFmapFusionLawId =
  mkLawId (hsExprLawRuleIdBase + 6)

hsExprFilterFusionLawId :: LawId
hsExprFilterFusionLawId =
  mkLawId (hsExprLawRuleIdBase + 7)

hsExprMapFilterInterchangeLawId :: LawId
hsExprMapFilterInterchangeLawId =
  mkLawId (hsExprLawRuleIdBase + 8)

hsExprAppendRightIdentityLawId :: LawId
hsExprAppendRightIdentityLawId =
  mkLawId (hsExprLawRuleIdBase + 9)

hsExprAppendLeftIdentityLawId :: LawId
hsExprAppendLeftIdentityLawId =
  mkLawId (hsExprLawRuleIdBase + 10)

hsExprAppendAssociativityLawId :: LawId
hsExprAppendAssociativityLawId =
  mkLawId (hsExprLawRuleIdBase + 11)

hsExprMapAppendFactorLawId :: LawId
hsExprMapAppendFactorLawId =
  mkLawId (hsExprLawRuleIdBase + 12)

hsExprConcatMapLawId :: LawId
hsExprConcatMapLawId =
  mkLawId (hsExprLawRuleIdBase + 13)

hsExprReverseInvolutionLawId :: LawId
hsExprReverseInvolutionLawId =
  mkLawId (hsExprLawRuleIdBase + 14)

hsExprMapIdLawId :: LawId
hsExprMapIdLawId =
  mkLawId (hsExprLawRuleIdBase + 15)

hsExprFmapIdLawId :: LawId
hsExprFmapIdLawId =
  mkLawId (hsExprLawRuleIdBase + 16)

hsExprMonadLeftIdentityLawId :: LawId
hsExprMonadLeftIdentityLawId =
  mkLawId (hsExprLawRuleIdBase + 17)

hsExprMonadRightIdentityLawId :: LawId
hsExprMonadRightIdentityLawId =
  mkLawId (hsExprLawRuleIdBase + 18)

hsExprParErasureLawId :: LawId
hsExprParErasureLawId =
  mkLawId (hsExprLawRuleIdBase + 19)

hsExprPlusAssociativityLawId :: LawId
hsExprPlusAssociativityLawId =
  mkLawId (hsExprLawRuleIdBase + 20)

hsExprPlusCommutativityLawId :: LawId
hsExprPlusCommutativityLawId =
  mkLawId (hsExprLawRuleIdBase + 21)

hsExprTimesAssociativityLawId :: LawId
hsExprTimesAssociativityLawId =
  mkLawId (hsExprLawRuleIdBase + 22)

hsExprTimesCommutativityLawId :: LawId
hsExprTimesCommutativityLawId =
  mkLawId (hsExprLawRuleIdBase + 23)

hsExprPlusUnitLawId :: LawId
hsExprPlusUnitLawId =
  mkLawId (hsExprLawRuleIdBase + 24)

hsExprTimesUnitLawId :: LawId
hsExprTimesUnitLawId =
  mkLawId (hsExprLawRuleIdBase + 25)

hsExprBindingFrontLawId :: LawId
hsExprBindingFrontLawId =
  mkLawId (hsExprLawRuleIdBase + 1000000)

hsExprLawfulFunctorFactId :: FactId
hsExprLawfulFunctorFactId =
  FactId (hsExprLawRuleIdBase + 9001)

hsExprLawfulMonadFactId :: FactId
hsExprLawfulMonadFactId =
  FactId (hsExprLawRuleIdBase + 9002)

hsExprLawfulNumTypeFactId :: FactId
hsExprLawfulNumTypeFactId =
  FactId (hsExprLawRuleIdBase + 9003)

hsExprEvidenceFactRulesFor ::
  PreparedContextSite owner ScopeCtx ->
  FactId ->
  Int ->
  [(ScopeCtx, Pattern HsExprF)] ->
  Either
    (PreparedContextSupportError ScopeCtx)
    (SupportedFactBook owner ScopeCtx (FactRule ScopeCtx HsExprF))
hsExprEvidenceFactRulesFor site factId ruleIdOffset evidenceRows =
  supportedFactBook site
    [ SupportedFactSpec
        { sfsSupport = principalSupport supportScope,
          sfsRule =
            FactRule
              { frId = FactRuleId (hsExprLawRuleIdBase + 90000 + ruleIdOffset + factRuleIndex),
                frName = "hsexpr/evidence/" <> show ruleIdOffset <> "/" <> show factRuleIndex,
                frPattern = patternValue,
                frProjection = [GuardRoot],
                frFactId = factId,
                frCondition = Nothing
              }
        }
    | (factRuleIndex, (supportScope, patternValue)) <- zip [0 :: Int ..] evidenceRows
    ]

hsExprComposeOracleKey :: OracleKey
hsExprComposeOracleKey =
  mkOracleKey "hsexpr.compose"

hsExprMapOracleKey :: OracleKey
hsExprMapOracleKey =
  mkOracleKey "hsexpr.map"

hsExprFmapOracleKey :: OracleKey
hsExprFmapOracleKey =
  mkOracleKey "hsexpr.fmap"

hsExprFilterOracleKey :: OracleKey
hsExprFilterOracleKey =
  mkOracleKey "hsexpr.filter"

hsExprAppendOracleKey :: OracleKey
hsExprAppendOracleKey =
  mkOracleKey "hsexpr.append"

hsExprConcatOracleKey :: OracleKey
hsExprConcatOracleKey =
  mkOracleKey "hsexpr.concat"

hsExprConcatMapOracleKey :: OracleKey
hsExprConcatMapOracleKey =
  mkOracleKey "hsexpr.concatMap"

hsExprReverseOracleKey :: OracleKey
hsExprReverseOracleKey =
  mkOracleKey "hsexpr.reverse"

hsExprIdOracleKey :: OracleKey
hsExprIdOracleKey =
  mkOracleKey "hsexpr.id"

hsExprAndOracleKey :: OracleKey
hsExprAndOracleKey =
  mkOracleKey "hsexpr.and"

hsExprBindOracleKey :: OracleKey
hsExprBindOracleKey =
  mkOracleKey "hsexpr.bind"

hsExprReturnOracleKey :: OracleKey
hsExprReturnOracleKey =
  mkOracleKey "hsexpr.return"

hsExprPureOracleKey :: OracleKey
hsExprPureOracleKey =
  mkOracleKey "hsexpr.pure"

hsExprPlusOracleKey :: OracleKey
hsExprPlusOracleKey =
  mkOracleKey "hsexpr.plus"

hsExprTimesOracleKey :: OracleKey
hsExprTimesOracleKey =
  mkOracleKey "hsexpr.times"

type HsExprNameTable :: Type
data HsExprNameTable = HsExprNameTable
  { hntComposeForms :: ![RdrName],
    hntMapForms :: ![RdrName],
    hntFmapForms :: ![RdrName],
    hntFilterForms :: ![RdrName],
    hntAppendForms :: ![RdrName],
    hntConcatForms :: ![RdrName],
    hntConcatMapForms :: ![RdrName],
    hntReverseForms :: ![RdrName],
    hntIdForms :: ![RdrName],
    hntAndForms :: ![RdrName],
    hntBindForms :: ![RdrName],
    hntReturnForms :: ![RdrName],
    hntPureForms :: ![RdrName],
    hntPlusForms :: ![RdrName],
    hntTimesForms :: ![RdrName]
  }
  deriving stock (Eq, Ord)

type HsExprVocabularyRuleMetrics :: Type
data HsExprVocabularyRuleMetrics = HsExprVocabularyRuleMetrics
  { hvrmVocabularyLawCount :: !Int,
    hvrmVocabularyGeneratedRuleCount :: !Int,
    hvrmVocabularyAdmittedRuleCount :: !Int,
    hvrmVocabularyGatedLawCount :: !Int
  }
  deriving stock (Eq, Ord, Show)

originSet :: [(String, String, String)] -> Either PackageUnitParseFailure (Set.Set ResolvedOrigin)
originSet rows =
  Set.fromList <$> traverse originRow rows

originRow :: (String, String, String) -> Either PackageUnitParseFailure ResolvedOrigin
originRow (unitText, moduleText, occText) =
  mkResolvedOrigin unitText moduleText occText

hsAcceptedComposeOrigins :: Either PackageUnitParseFailure (Set.Set ResolvedOrigin)
hsAcceptedComposeOrigins =
  originSet
    [ ("base", "GHC.Base", "."),
      ("base", "GHC.Internal.Base", "."),
      ("ghc-internal", "GHC.Internal.Base", ".")
    ]

hsAcceptedMapOrigins :: Either PackageUnitParseFailure (Set.Set ResolvedOrigin)
hsAcceptedMapOrigins =
  originSet
    [ ("base", "GHC.Base", "map"),
      ("base", "GHC.Internal.Base", "map"),
      ("ghc-internal", "GHC.Internal.Base", "map")
    ]

hsAcceptedFmapOrigins :: Either PackageUnitParseFailure (Set.Set ResolvedOrigin)
hsAcceptedFmapOrigins =
  originSet
    [ ("base", "GHC.Base", "fmap"),
      ("base", "GHC.Internal.Base", "fmap"),
      ("ghc-internal", "GHC.Internal.Base", "fmap")
    ]

hsAcceptedFilterOrigins :: Either PackageUnitParseFailure (Set.Set ResolvedOrigin)
hsAcceptedFilterOrigins =
  originSet
    [ ("base", "GHC.List", "filter"),
      ("base", "GHC.Internal.List", "filter"),
      ("ghc-internal", "GHC.Internal.List", "filter")
    ]

hsAcceptedAppendOrigins :: Either PackageUnitParseFailure (Set.Set ResolvedOrigin)
hsAcceptedAppendOrigins =
  originSet
    [ ("base", "GHC.Base", "++"),
      ("base", "GHC.Internal.Base", "++"),
      ("ghc-internal", "GHC.Internal.Base", "++")
    ]

hsAcceptedConcatOrigins :: Either PackageUnitParseFailure (Set.Set ResolvedOrigin)
hsAcceptedConcatOrigins =
  originSet
    [ ("base", "GHC.List", "concat"),
      ("base", "GHC.Internal.List", "concat"),
      ("base", "Data.Foldable", "concat"),
      ("base", "GHC.Internal.Data.Foldable", "concat"),
      ("ghc-internal", "GHC.Internal.Data.Foldable", "concat")
    ]

hsAcceptedConcatMapOrigins :: Either PackageUnitParseFailure (Set.Set ResolvedOrigin)
hsAcceptedConcatMapOrigins =
  originSet
    [ ("base", "GHC.List", "concatMap"),
      ("base", "GHC.Internal.List", "concatMap"),
      ("base", "Data.Foldable", "concatMap"),
      ("base", "GHC.Internal.Data.Foldable", "concatMap"),
      ("ghc-internal", "GHC.Internal.Data.Foldable", "concatMap")
    ]

hsAcceptedReverseOrigins :: Either PackageUnitParseFailure (Set.Set ResolvedOrigin)
hsAcceptedReverseOrigins =
  originSet
    [ ("base", "GHC.List", "reverse"),
      ("base", "GHC.Internal.List", "reverse"),
      ("ghc-internal", "GHC.Internal.List", "reverse")
    ]

hsAcceptedIdOrigins :: Either PackageUnitParseFailure (Set.Set ResolvedOrigin)
hsAcceptedIdOrigins =
  originSet
    [ ("base", "GHC.Base", "id"),
      ("base", "GHC.Internal.Base", "id"),
      ("ghc-internal", "GHC.Internal.Base", "id")
    ]

hsAcceptedAndOrigins :: Either PackageUnitParseFailure (Set.Set ResolvedOrigin)
hsAcceptedAndOrigins =
  originSet
    [ ("base", "GHC.Classes", "&&"),
      ("base", "GHC.Internal.Classes", "&&"),
      ("ghc-internal", "GHC.Internal.Classes", "&&")
    ]

hsAcceptedBindOrigins :: Either PackageUnitParseFailure (Set.Set ResolvedOrigin)
hsAcceptedBindOrigins =
  originSet
    [ ("base", "GHC.Base", ">>="),
      ("base", "GHC.Internal.Base", ">>="),
      ("ghc-internal", "GHC.Internal.Base", ">>=")
    ]

hsAcceptedReturnOrigins :: Either PackageUnitParseFailure (Set.Set ResolvedOrigin)
hsAcceptedReturnOrigins =
  originSet
    [ ("base", "GHC.Base", "return"),
      ("base", "GHC.Internal.Base", "return"),
      ("ghc-internal", "GHC.Internal.Base", "return")
    ]

hsAcceptedPureOrigins :: Either PackageUnitParseFailure (Set.Set ResolvedOrigin)
hsAcceptedPureOrigins =
  originSet
    [ ("base", "GHC.Base", "pure"),
      ("base", "GHC.Internal.Base", "pure"),
      ("ghc-internal", "GHC.Internal.Base", "pure")
    ]

hsAcceptedPlusOrigins :: Either PackageUnitParseFailure (Set.Set ResolvedOrigin)
hsAcceptedPlusOrigins =
  originSet
    [ ("base", "GHC.Num", "+"),
      ("base", "GHC.Internal.Num", "+"),
      ("ghc-internal", "GHC.Internal.Num", "+")
    ]

hsAcceptedTimesOrigins :: Either PackageUnitParseFailure (Set.Set ResolvedOrigin)
hsAcceptedTimesOrigins =
  originSet
    [ ("base", "GHC.Num", "*"),
      ("base", "GHC.Internal.Num", "*"),
      ("ghc-internal", "GHC.Internal.Num", "*")
    ]

hsLawfulFunctorInstanceOrigins :: Either PackageUnitParseFailure (Set.Set ResolvedOrigin)
hsLawfulFunctorInstanceOrigins =
  originSet
    [ ("base", "GHC.Base", "$fFunctorList"),
      ("base", "GHC.Internal.Base", "$fFunctorList"),
      ("ghc-internal", "GHC.Internal.Base", "$fFunctorList"),
      ("base", "GHC.Base", "$fFunctorMaybe"),
      ("base", "GHC.Internal.Base", "$fFunctorMaybe"),
      ("ghc-internal", "GHC.Internal.Base", "$fFunctorMaybe"),
      ("base", "Data.Either", "$fFunctorEither"),
      ("base", "GHC.Internal.Data.Either", "$fFunctorEither"),
      ("ghc-internal", "GHC.Internal.Data.Either", "$fFunctorEither")
    ]

hsLawfulMonadInstanceOrigins :: Either PackageUnitParseFailure (Set.Set ResolvedOrigin)
hsLawfulMonadInstanceOrigins =
  originSet
    [ ("base", "GHC.Base", "$fMonadList"),
      ("base", "GHC.Internal.Base", "$fMonadList"),
      ("ghc-internal", "GHC.Internal.Base", "$fMonadList"),
      ("base", "GHC.Base", "$fMonadMaybe"),
      ("base", "GHC.Internal.Base", "$fMonadMaybe"),
      ("ghc-internal", "GHC.Internal.Base", "$fMonadMaybe"),
      ("base", "Data.Either", "$fMonadEither"),
      ("base", "GHC.Internal.Data.Either", "$fMonadEither"),
      ("ghc-internal", "GHC.Internal.Data.Either", "$fMonadEither")
    ]

hsLawfulNumTypeWords :: Set.Set TypeWords
hsLawfulNumTypeWords =
  Set.fromList (fmap tyConTypeWords ["Int", "Integer", "Word"])

hsExprOracleKeyTable :: Either PackageUnitParseFailure [(OracleKey, String, Set.Set ResolvedOrigin)]
hsExprOracleKeyTable =
  sequence
    [ oracleRow hsExprComposeOracleKey "." hsAcceptedComposeOrigins,
      oracleRow hsExprMapOracleKey "map" hsAcceptedMapOrigins,
      oracleRow hsExprFmapOracleKey "fmap" hsAcceptedFmapOrigins,
      oracleRow hsExprFilterOracleKey "filter" hsAcceptedFilterOrigins,
      oracleRow hsExprAppendOracleKey "++" hsAcceptedAppendOrigins,
      oracleRow hsExprConcatOracleKey "concat" hsAcceptedConcatOrigins,
      oracleRow hsExprConcatMapOracleKey "concatMap" hsAcceptedConcatMapOrigins,
      oracleRow hsExprReverseOracleKey "reverse" hsAcceptedReverseOrigins,
      oracleRow hsExprIdOracleKey "id" hsAcceptedIdOrigins,
      oracleRow hsExprAndOracleKey "&&" hsAcceptedAndOrigins,
      oracleRow hsExprBindOracleKey ">>=" hsAcceptedBindOrigins,
      oracleRow hsExprReturnOracleKey "return" hsAcceptedReturnOrigins,
      oracleRow hsExprPureOracleKey "pure" hsAcceptedPureOrigins,
      oracleRow hsExprPlusOracleKey "+" hsAcceptedPlusOrigins,
      oracleRow hsExprTimesOracleKey "*" hsAcceptedTimesOrigins
    ]

oracleRow ::
  OracleKey ->
  String ->
  Either PackageUnitParseFailure (Set.Set ResolvedOrigin) ->
  Either PackageUnitParseFailure (OracleKey, String, Set.Set ResolvedOrigin)
oracleRow oracleKey occName origins =
  (\resolvedOrigins -> (oracleKey, occName, resolvedOrigins)) <$> origins

hsExprVocabularyLawIds :: Set.Set LawId
hsExprVocabularyLawIds =
  Set.fromList
    [ hsExprFilterFusionLawId,
      hsExprMapFilterInterchangeLawId,
      hsExprAppendRightIdentityLawId,
      hsExprAppendLeftIdentityLawId,
      hsExprAppendAssociativityLawId,
      hsExprMapAppendFactorLawId,
      hsExprConcatMapLawId,
      hsExprReverseInvolutionLawId,
      hsExprMapIdLawId
    ]

hsExprSiteLawFamily ::
  ConvertedModule ->
  Either HsExprSiteRuleError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
hsExprSiteLawFamily convertedModule =
  LawBook . fmap (uncurry siteLawSpec)
    <$> hsExprSupportedLawRules convertedModule

hsExprParErasureLawFamily :: ConvertedModule -> Either HsExprLawEmitError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
hsExprParErasureLawFamily convertedModule =
  parErasureRule >>= \rule ->
    Right
      ( LawBook
          [ LawSpec
              { lawId = hsExprParErasureLawId,
                lawTier = RegistryTrusted,
                lawFidelity = Observational,
                lawOracle = NoOracleRequired,
                lawRule =
                  SupportedRuleSpec
                    { srsSupport = principalSupport (scopeBottomCtx (cmScopeIndex convertedModule)),
                      srsRule = rule
                    }
              }
          ]
      )

parErasureRule :: Either HsExprLawEmitError HsExprLawRule
parErasureRule =
  equationRule
    hsExprParErasureLawId
    0
    ["x"]
    []
    "(x) = x"

hsExprRenamerLawFamily :: ConvertedModule -> Either HsExprLawEmitError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
hsExprRenamerLawFamily convertedModule =
  mconcat
    <$> sequence
      [ hsExprMapFusionLaw defaultHsExprNameTable convertedModule,
        hsExprFmapFusionLaw defaultHsExprNameTable convertedModule,
        hsExprFilterFusionLaw defaultHsExprNameTable convertedModule,
        hsExprMapFilterInterchangeLaw defaultHsExprNameTable convertedModule,
        hsExprAppendRightIdentityLaw defaultHsExprNameTable convertedModule,
        hsExprAppendLeftIdentityLaw defaultHsExprNameTable convertedModule,
        hsExprAppendAssociativityLaw defaultHsExprNameTable convertedModule,
        hsExprMapAppendFactorLaw defaultHsExprNameTable convertedModule,
        hsExprConcatMapLaw defaultHsExprNameTable convertedModule,
        hsExprReverseInvolutionLaw defaultHsExprNameTable convertedModule,
        hsExprMapIdLaw defaultHsExprNameTable convertedModule,
        hsExprFmapIdLaw defaultHsExprNameTable convertedModule,
        hsExprMonadLeftIdentityLaw defaultHsExprNameTable convertedModule,
        hsExprMonadRightIdentityLaw defaultHsExprNameTable convertedModule,
        hsExprPlusAssociativityLaw defaultHsExprNameTable convertedModule,
        hsExprPlusCommutativityLaw defaultHsExprNameTable convertedModule,
        hsExprTimesAssociativityLaw defaultHsExprNameTable convertedModule,
        hsExprTimesCommutativityLaw defaultHsExprNameTable convertedModule,
        hsExprPlusUnitLaw defaultHsExprNameTable convertedModule,
        hsExprTimesUnitLaw defaultHsExprNameTable convertedModule
      ]

defaultHsExprNameTable :: HsExprNameTable
defaultHsExprNameTable =
  HsExprNameTable
    { hntComposeForms = [mkRdrUnqual (mkVarOcc ".")],
      hntMapForms = [mkRdrUnqual (mkVarOcc "map")],
      hntFmapForms = [mkRdrUnqual (mkVarOcc "fmap")],
      hntFilterForms = [mkRdrUnqual (mkVarOcc "filter")],
      hntAppendForms = [mkRdrUnqual (mkVarOcc "++")],
      hntConcatForms = [mkRdrUnqual (mkVarOcc "concat")],
      hntConcatMapForms = [mkRdrUnqual (mkVarOcc "concatMap")],
      hntReverseForms = [mkRdrUnqual (mkVarOcc "reverse")],
      hntIdForms = [mkRdrUnqual (mkVarOcc "id")],
      hntAndForms = [mkRdrUnqual (mkVarOcc "&&")],
      hntBindForms = [mkRdrUnqual (mkVarOcc ">>=")],
      hntReturnForms = [mkRdrUnqual (mkVarOcc "return")],
      hntPureForms = [mkRdrUnqual (mkVarOcc "pure")],
      hntPlusForms = [mkRdrUnqual (mkVarOcc "+")],
      hntTimesForms = [mkRdrUnqual (mkVarOcc "*")]
    }

hsExprMapFusionLaw :: HsExprNameTable -> ConvertedModule -> Either HsExprLawEmitError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
hsExprMapFusionLaw nameTable convertedModule =
  renamerLaw
    hsExprMapFusionLawId
    (Set.fromList [hsExprComposeOracleKey, hsExprMapOracleKey])
    convertedModule
    =<< traverse
      (\(instantiationIndex, (mapName, composeName)) -> mapFusionRule hsExprMapFusionLawId instantiationIndex mapName composeName)
      ( zip
          [0 ..]
          [ (mapName, composeName)
          | mapName <- hntMapForms nameTable,
            composeName <- hntComposeForms nameTable
          ]
      )

mapFusionRule :: LawId -> Int -> RdrName -> RdrName -> Either HsExprLawEmitError HsExprLawRule
mapFusionRule lawIdValue instantiationIndex mapName composeName =
  equationRule
    lawIdValue
    instantiationIndex
    ["f", "g", "xs"]
    [("map", mapName), (".", composeName)]
    "map f (map g xs) = map (f . g) xs"

hsExprFmapFusionLaw :: HsExprNameTable -> ConvertedModule -> Either HsExprLawEmitError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
hsExprFmapFusionLaw nameTable convertedModule =
  renamerLaw
    hsExprFmapFusionLawId
    (Set.fromList [hsExprComposeOracleKey, hsExprFmapOracleKey])
    convertedModule
    =<< traverse
      ( \(instantiationIndex, (fmapName, composeName)) ->
          fmapFunctorCondition <$> mapFusionRule hsExprFmapFusionLawId instantiationIndex fmapName composeName
      )
      ( zip
          [0 ..]
          [ (fmapName, composeName)
          | fmapName <- hntFmapForms nameTable,
            composeName <- hntComposeForms nameTable
          ]
      )

fmapFunctorCondition :: HsExprLawRule -> HsExprLawRule
fmapFunctorCondition rule =
  rule {rrCondition = Just (RewriteCondition (guardHasFact hsExprLawfulFunctorFactId [GuardRoot]))}

hsExprFilterFusionLaw :: HsExprNameTable -> ConvertedModule -> Either HsExprLawEmitError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
hsExprFilterFusionLaw nameTable convertedModule =
  renamerLaw
    hsExprFilterFusionLawId
    (Set.fromList [hsExprFilterOracleKey, hsExprAndOracleKey])
    convertedModule
    =<< traverse
      (\(instantiationIndex, (filterName, andName)) -> filterFusionRule instantiationIndex filterName andName)
      (zip [0 ..] [(filterName, andName) | filterName <- hntFilterForms nameTable, andName <- hntAndForms nameTable])

filterFusionRule :: Int -> RdrName -> RdrName -> Either HsExprLawEmitError HsExprLawRule
filterFusionRule instantiationIndex filterName andName =
  equationRule
    hsExprFilterFusionLawId
    instantiationIndex
    ["outerPredicate", "innerPredicate", "xs"]
    [("filter", filterName), ("&&", andName), ("x", mkRdrUnqual (mkVarOcc "nebulaFilterArg"))]
    "filter outerPredicate (filter innerPredicate xs) = filter (\\x -> innerPredicate x && outerPredicate x) xs"

hsExprMapFilterInterchangeLaw :: HsExprNameTable -> ConvertedModule -> Either HsExprLawEmitError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
hsExprMapFilterInterchangeLaw nameTable convertedModule =
  renamerLaw
    hsExprMapFilterInterchangeLawId
    (Set.fromList [hsExprFilterOracleKey, hsExprMapOracleKey, hsExprComposeOracleKey])
    convertedModule
    =<< traverse
      (\(instantiationIndex, (filterName, mapName, composeName)) -> mapFilterInterchangeRule instantiationIndex filterName mapName composeName)
      ( zip
          [0 ..]
          [(filterName, mapName, composeName) | filterName <- hntFilterForms nameTable, mapName <- hntMapForms nameTable, composeName <- hntComposeForms nameTable]
      )

mapFilterInterchangeRule :: Int -> RdrName -> RdrName -> RdrName -> Either HsExprLawEmitError HsExprLawRule
mapFilterInterchangeRule instantiationIndex filterName mapName composeName =
  equationRule
    hsExprMapFilterInterchangeLawId
    instantiationIndex
    ["predicate", "function", "xs"]
    [("filter", filterName), ("map", mapName), (".", composeName)]
    "filter predicate (map function xs) = map function (filter (predicate . function) xs)"

hsExprAppendRightIdentityLaw :: HsExprNameTable -> ConvertedModule -> Either HsExprLawEmitError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
hsExprAppendRightIdentityLaw nameTable convertedModule =
  renamerLaw
    hsExprAppendRightIdentityLawId
    (Set.singleton hsExprAppendOracleKey)
    convertedModule
    =<< traverse
      (uncurry appendRightIdentityRule)
      (zip [0 ..] (hntAppendForms nameTable))

appendRightIdentityRule :: Int -> RdrName -> Either HsExprLawEmitError HsExprLawRule
appendRightIdentityRule instantiationIndex appendName =
  equationRule hsExprAppendRightIdentityLawId instantiationIndex ["xs"] [("++", appendName)] "xs ++ [] = xs"

hsExprAppendLeftIdentityLaw :: HsExprNameTable -> ConvertedModule -> Either HsExprLawEmitError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
hsExprAppendLeftIdentityLaw nameTable convertedModule =
  renamerLaw
    hsExprAppendLeftIdentityLawId
    (Set.singleton hsExprAppendOracleKey)
    convertedModule
    =<< traverse
      (uncurry appendLeftIdentityRule)
      (zip [0 ..] (hntAppendForms nameTable))

appendLeftIdentityRule :: Int -> RdrName -> Either HsExprLawEmitError HsExprLawRule
appendLeftIdentityRule instantiationIndex appendName =
  equationRule hsExprAppendLeftIdentityLawId instantiationIndex ["xs"] [("++", appendName)] "[] ++ xs = xs"

hsExprAppendAssociativityLaw :: HsExprNameTable -> ConvertedModule -> Either HsExprLawEmitError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
hsExprAppendAssociativityLaw nameTable convertedModule =
  renamerLaw
    hsExprAppendAssociativityLawId
    (Set.singleton hsExprAppendOracleKey)
    convertedModule
    =<< traverse
      (uncurry appendAssociativityRule)
      (zip [0 ..] (hntAppendForms nameTable))

appendAssociativityRule :: Int -> RdrName -> Either HsExprLawEmitError HsExprLawRule
appendAssociativityRule instantiationIndex appendName =
  equationRule
    hsExprAppendAssociativityLawId
    instantiationIndex
    ["xs", "ys", "zs"]
    [("++", appendName)]
    "(xs ++ ys) ++ zs = xs ++ (ys ++ zs)"

hsExprMapAppendFactorLaw :: HsExprNameTable -> ConvertedModule -> Either HsExprLawEmitError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
hsExprMapAppendFactorLaw nameTable convertedModule =
  renamerLaw
    hsExprMapAppendFactorLawId
    (Set.fromList [hsExprMapOracleKey, hsExprAppendOracleKey])
    convertedModule
    =<< traverse
      (\(instantiationIndex, (mapName, appendName)) -> mapAppendFactorRule instantiationIndex mapName appendName)
      (zip [0 ..] [(mapName, appendName) | mapName <- hntMapForms nameTable, appendName <- hntAppendForms nameTable])

mapAppendFactorRule :: Int -> RdrName -> RdrName -> Either HsExprLawEmitError HsExprLawRule
mapAppendFactorRule instantiationIndex mapName appendName =
  equationRule
    hsExprMapAppendFactorLawId
    instantiationIndex
    ["function", "left", "right"]
    [("map", mapName), ("++", appendName)]
    "map function left ++ map function right = map function (left ++ right)"

hsExprConcatMapLaw :: HsExprNameTable -> ConvertedModule -> Either HsExprLawEmitError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
hsExprConcatMapLaw nameTable convertedModule =
  renamerLaw
    hsExprConcatMapLawId
    (Set.fromList [hsExprConcatOracleKey, hsExprConcatMapOracleKey, hsExprMapOracleKey])
    convertedModule
    =<< traverse
      (\(instantiationIndex, (concatName, concatMapName, mapName)) -> concatMapRule instantiationIndex concatName concatMapName mapName)
      ( zip
          [0 ..]
          [(concatName, concatMapName, mapName) | concatName <- hntConcatForms nameTable, concatMapName <- hntConcatMapForms nameTable, mapName <- hntMapForms nameTable]
      )

concatMapRule :: Int -> RdrName -> RdrName -> RdrName -> Either HsExprLawEmitError HsExprLawRule
concatMapRule instantiationIndex concatName concatMapName mapName =
  equationRule
    hsExprConcatMapLawId
    instantiationIndex
    ["function", "xs"]
    [("concat", concatName), ("concatMap", concatMapName), ("map", mapName)]
    "concat (map function xs) = concatMap function xs"

hsExprReverseInvolutionLaw :: HsExprNameTable -> ConvertedModule -> Either HsExprLawEmitError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
hsExprReverseInvolutionLaw nameTable convertedModule =
  renamerLaw
    hsExprReverseInvolutionLawId
    (Set.singleton hsExprReverseOracleKey)
    convertedModule
    =<< traverse
      (uncurry reverseInvolutionRule)
      (zip [0 ..] (hntReverseForms nameTable))

reverseInvolutionRule :: Int -> RdrName -> Either HsExprLawEmitError HsExprLawRule
reverseInvolutionRule instantiationIndex reverseName =
  equationRule
    hsExprReverseInvolutionLawId
    instantiationIndex
    ["xs"]
    [("reverse", reverseName)]
    "reverse (reverse xs) = xs"

hsExprMapIdLaw :: HsExprNameTable -> ConvertedModule -> Either HsExprLawEmitError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
hsExprMapIdLaw nameTable convertedModule =
  renamerLaw
    hsExprMapIdLawId
    (Set.fromList [hsExprMapOracleKey, hsExprIdOracleKey])
    convertedModule
    =<< traverse
      (\(instantiationIndex, (mapName, idName)) -> mapIdRule hsExprMapIdLawId instantiationIndex mapName idName)
      (zip [0 ..] [(mapName, idName) | mapName <- hntMapForms nameTable, idName <- hntIdForms nameTable])

mapIdRule :: LawId -> Int -> RdrName -> RdrName -> Either HsExprLawEmitError HsExprLawRule
mapIdRule lawIdValue instantiationIndex mapName idName =
  equationRule lawIdValue instantiationIndex ["xs"] [("map", mapName), ("id", idName)] "map id xs = xs"

hsExprFmapIdLaw :: HsExprNameTable -> ConvertedModule -> Either HsExprLawEmitError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
hsExprFmapIdLaw nameTable convertedModule =
  guardedRenamerLaw
    hsExprFmapIdLawId
    (Set.fromList [hsExprFmapOracleKey, hsExprIdOracleKey])
    hsExprLawfulFunctorFactId
    convertedModule
    =<< traverse
      (\(instantiationIndex, (fmapName, idName)) -> mapIdRule hsExprFmapIdLawId instantiationIndex fmapName idName)
      (zip [0 ..] [(fmapName, idName) | fmapName <- hntFmapForms nameTable, idName <- hntIdForms nameTable])

hsExprMonadLeftIdentityLaw :: HsExprNameTable -> ConvertedModule -> Either HsExprLawEmitError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
hsExprMonadLeftIdentityLaw nameTable convertedModule =
  mconcat
    <$> sequence
      [ guardedRenamerLaw
          hsExprMonadLeftIdentityLawId
          (Set.fromList [hsExprBindOracleKey, hsExprReturnOracleKey])
          hsExprLawfulMonadFactId
          convertedModule
          =<< traverse
            (\(bindName, returnName) -> monadLeftIdentityRule 0 bindName returnName)
            [(bindName, returnName) | bindName <- hntBindForms nameTable, returnName <- hntReturnForms nameTable],
        guardedRenamerLaw
          hsExprMonadLeftIdentityLawId
          (Set.fromList [hsExprBindOracleKey, hsExprPureOracleKey])
          hsExprLawfulMonadFactId
          convertedModule
          =<< traverse
            (\(bindName, pureName) -> monadLeftIdentityRule 1 bindName pureName)
            [(bindName, pureName) | bindName <- hntBindForms nameTable, pureName <- hntPureForms nameTable]
      ]

monadLeftIdentityRule :: Int -> RdrName -> RdrName -> Either HsExprLawEmitError HsExprLawRule
monadLeftIdentityRule instantiationIndex bindName returnName =
  equationRule
    hsExprMonadLeftIdentityLawId
    instantiationIndex
    ["value", "continuation"]
    [(">>=", bindName), ("return", returnName)]
    "return value >>= continuation = continuation value"

hsExprMonadRightIdentityLaw :: HsExprNameTable -> ConvertedModule -> Either HsExprLawEmitError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
hsExprMonadRightIdentityLaw nameTable convertedModule =
  mconcat
    <$> sequence
      [ guardedRenamerLaw
          hsExprMonadRightIdentityLawId
          (Set.fromList [hsExprBindOracleKey, hsExprReturnOracleKey])
          hsExprLawfulMonadFactId
          convertedModule
          =<< traverse
            (\(bindName, returnName) -> monadRightIdentityRule 0 bindName returnName)
            [(bindName, returnName) | bindName <- hntBindForms nameTable, returnName <- hntReturnForms nameTable],
        guardedRenamerLaw
          hsExprMonadRightIdentityLawId
          (Set.fromList [hsExprBindOracleKey, hsExprPureOracleKey])
          hsExprLawfulMonadFactId
          convertedModule
          =<< traverse
            (\(bindName, pureName) -> monadRightIdentityRule 1 bindName pureName)
            [(bindName, pureName) | bindName <- hntBindForms nameTable, pureName <- hntPureForms nameTable]
      ]

monadRightIdentityRule :: Int -> RdrName -> RdrName -> Either HsExprLawEmitError HsExprLawRule
monadRightIdentityRule instantiationIndex bindName returnName =
  equationRule
    hsExprMonadRightIdentityLawId
    instantiationIndex
    ["m"]
    [(">>=", bindName), ("return", returnName)]
    "m >>= return = m"

hsExprPlusAssociativityLaw :: HsExprNameTable -> ConvertedModule -> Either HsExprLawEmitError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
hsExprPlusAssociativityLaw nameTable convertedModule =
  guardedRenamerLaw
    hsExprPlusAssociativityLawId
    (Set.singleton hsExprPlusOracleKey)
    hsExprLawfulNumTypeFactId
    convertedModule
    =<< traverse
      (uncurry (associativityRule hsExprPlusAssociativityLawId "+"))
      (zip [0 ..] (hntPlusForms nameTable))

hsExprPlusCommutativityLaw :: HsExprNameTable -> ConvertedModule -> Either HsExprLawEmitError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
hsExprPlusCommutativityLaw nameTable convertedModule =
  guardedRenamerLaw
    hsExprPlusCommutativityLawId
    (Set.singleton hsExprPlusOracleKey)
    hsExprLawfulNumTypeFactId
    convertedModule
    =<< traverse
      (uncurry (commutativityRule hsExprPlusCommutativityLawId "+"))
      (zip [0 ..] (hntPlusForms nameTable))

hsExprTimesAssociativityLaw :: HsExprNameTable -> ConvertedModule -> Either HsExprLawEmitError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
hsExprTimesAssociativityLaw nameTable convertedModule =
  guardedRenamerLaw
    hsExprTimesAssociativityLawId
    (Set.singleton hsExprTimesOracleKey)
    hsExprLawfulNumTypeFactId
    convertedModule
    =<< traverse
      (uncurry (associativityRule hsExprTimesAssociativityLawId "*"))
      (zip [0 ..] (hntTimesForms nameTable))

hsExprTimesCommutativityLaw :: HsExprNameTable -> ConvertedModule -> Either HsExprLawEmitError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
hsExprTimesCommutativityLaw nameTable convertedModule =
  guardedRenamerLaw
    hsExprTimesCommutativityLawId
    (Set.singleton hsExprTimesOracleKey)
    hsExprLawfulNumTypeFactId
    convertedModule
    =<< traverse
      (uncurry (commutativityRule hsExprTimesCommutativityLawId "*"))
      (zip [0 ..] (hntTimesForms nameTable))

hsExprPlusUnitLaw :: HsExprNameTable -> ConvertedModule -> Either HsExprLawEmitError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
hsExprPlusUnitLaw nameTable convertedModule =
  mconcat
    <$> sequence
      [ guardedRenamerLaw
          hsExprPlusUnitLawId
          (Set.singleton hsExprPlusOracleKey)
          hsExprLawfulNumTypeFactId
          convertedModule
          =<< traverse
            (\plusName -> unitRightRule hsExprPlusUnitLawId 0 "+" plusName "0")
            (hntPlusForms nameTable),
        guardedRenamerLaw
          hsExprPlusUnitLawId
          (Set.singleton hsExprPlusOracleKey)
          hsExprLawfulNumTypeFactId
          convertedModule
          =<< traverse
            (\plusName -> unitLeftRule hsExprPlusUnitLawId 1 "+" plusName "0")
            (hntPlusForms nameTable)
      ]

hsExprTimesUnitLaw :: HsExprNameTable -> ConvertedModule -> Either HsExprLawEmitError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
hsExprTimesUnitLaw nameTable convertedModule =
  mconcat
    <$> sequence
      [ guardedRenamerLaw
          hsExprTimesUnitLawId
          (Set.singleton hsExprTimesOracleKey)
          hsExprLawfulNumTypeFactId
          convertedModule
          =<< traverse
            (\timesName -> unitRightRule hsExprTimesUnitLawId 0 "*" timesName "1")
            (hntTimesForms nameTable),
        guardedRenamerLaw
          hsExprTimesUnitLawId
          (Set.singleton hsExprTimesOracleKey)
          hsExprLawfulNumTypeFactId
          convertedModule
          =<< traverse
            (\timesName -> unitLeftRule hsExprTimesUnitLawId 1 "*" timesName "1")
            (hntTimesForms nameTable)
      ]

associativityRule :: LawId -> String -> Int -> RdrName -> Either HsExprLawEmitError HsExprLawRule
associativityRule lawIdValue operatorText instantiationIndex operatorName =
  equationRule
    lawIdValue
    instantiationIndex
    ["x", "y", "z"]
    [(operatorText, operatorName)]
    ("(x " <> operatorText <> " y) " <> operatorText <> " z = x " <> operatorText <> " (y " <> operatorText <> " z)")

commutativityRule :: LawId -> String -> Int -> RdrName -> Either HsExprLawEmitError HsExprLawRule
commutativityRule lawIdValue operatorText instantiationIndex operatorName =
  equationRule lawIdValue instantiationIndex ["x", "y"] [(operatorText, operatorName)] ("x " <> operatorText <> " y = y " <> operatorText <> " x")

unitRightRule :: LawId -> Int -> String -> RdrName -> String -> Either HsExprLawEmitError HsExprLawRule
unitRightRule lawIdValue instantiationIndex operatorText operatorName unitText =
  equationRule lawIdValue instantiationIndex ["x"] [(operatorText, operatorName)] ("x " <> operatorText <> " " <> unitText <> " = x")

unitLeftRule :: LawId -> Int -> String -> RdrName -> String -> Either HsExprLawEmitError HsExprLawRule
unitLeftRule lawIdValue instantiationIndex operatorText operatorName unitText =
  equationRule lawIdValue instantiationIndex ["x"] [(operatorText, operatorName)] (unitText <> " " <> operatorText <> " x = x")

renamerLaw ::
  LawId ->
  Set.Set OracleKey ->
  ConvertedModule ->
  [HsExprLawRule] ->
  Either HsExprLawEmitError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
renamerLaw lawIdValue oracleKeys convertedModule rules =
  Right
    ( LawBook
        [ renamerLawSpec lawIdValue oracleKeys convertedModule rule
        | rule <- rules
        ]
    )

guardedRenamerLaw ::
  LawId ->
  Set.Set OracleKey ->
  FactId ->
  ConvertedModule ->
  [HsExprLawRule] ->
  Either HsExprLawEmitError (LawBook (SupportedRuleSpec ScopeCtx HsExprLawRule))
guardedRenamerLaw lawIdValue oracleKeys factId convertedModule rules =
  renamerLaw lawIdValue oracleKeys convertedModule
    [ rule {rrCondition = Just (RewriteCondition (guardHasFact factId [GuardRoot]))}
    | rule <- rules
    ]

renamerLawSpec ::
  LawId ->
  Set.Set OracleKey ->
  ConvertedModule ->
  HsExprLawRule ->
  LawSpec (SupportedRuleSpec ScopeCtx HsExprLawRule)
renamerLawSpec lawIdValue oracleKeys convertedModule rule =
  LawSpec
    { lawId = lawIdValue,
      lawTier = RegistryTrusted,
      lawFidelity = Observational,
      lawOracle = RequiresOracle oracleKeys,
      lawRule =
        SupportedRuleSpec
          { srsSupport = principalSupport (scopeBottomCtx (cmScopeIndex convertedModule)),
            srsRule = rule
          }
    }

siteLawSpec :: HsExprSiteRuleKind -> SupportedRuleSpec ScopeCtx HsExprLawRule -> LawSpec (SupportedRuleSpec ScopeCtx HsExprLawRule)
siteLawSpec ruleKind ruleSpec =
  case ruleKind of
    HsExprEtaRule ->
      LawSpec
        { lawId = hsExprEtaLawId,
          lawTier = ParserVerified,
          lawFidelity = UpToBottom,
          lawOracle = NoOracleRequired,
          lawRule = ruleSpec
        }
    HsExprCompositionRule ->
      LawSpec
        { lawId = hsExprCompositionLawId,
          lawTier = RegistryTrusted,
          lawFidelity = UpToBottom,
          lawOracle = RequiresOracle (Set.singleton hsExprComposeOracleKey),
          lawRule = ruleSpec
        }
    HsExprBetaRule ->
      LawSpec
        { lawId = hsExprBetaLawId,
          lawTier = ParserVerified,
          lawFidelity = Observational,
          lawOracle = NoOracleRequired,
          lawRule = ruleSpec
        }
    HsExprLetRule ->
      LawSpec
        { lawId = hsExprLetInlineLawId,
          lawTier = ParserVerified,
          lawFidelity = Observational,
          lawOracle = NoOracleRequired,
          lawRule = ruleSpec
        }
