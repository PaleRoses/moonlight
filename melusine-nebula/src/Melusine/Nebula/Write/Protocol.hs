{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.Write.Protocol
  ( ProtocolRewriteKind (..),
    ProtocolRewritePlan (..),
    ProtocolRewriteSkip (..),
    ProtocolSealObligation (..),
    planProtocolRewrites,
    sealProtocolObligations,
    sealProtocolObligationsFromParsedModule,
  )
where

import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.Foldable (fold, traverse_)
import Data.Kind (Type)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Hs qualified as Ghc
import Melusine.Nebula.Core (NebulaError (..))
import Melusine.Nebula.Source.Ast
  ( LocatedBinding (..),
    RecordConstruction (..),
    bindingGlobalNames,
    bindingRecordConstructions,
    locatedValueBindingsFromParsedModule,
  )
import Melusine.Nebula.Write.Patch (SourceSplice (..))
import Moonlight.Pale.Ghc.ModuleSurface (parseHsModule)

type ProtocolRewriteKind :: Type
data ProtocolRewriteKind
  = ProtocolRedundantPatternClassCanonicalization
  | ProtocolScopedRegionExtraction
  deriving stock (Eq, Ord, Show)

type ProtocolRewritePlan :: Type
data ProtocolRewritePlan = ProtocolRewritePlan
  { prpSpliceGroups :: ![(String, [SourceSplice])],
    prpObligations :: ![ProtocolSealObligation]
  }
  deriving stock (Eq, Show)

instance Semigroup ProtocolRewritePlan where
  leftPlan <> rightPlan =
    ProtocolRewritePlan
      { prpSpliceGroups = prpSpliceGroups leftPlan <> prpSpliceGroups rightPlan,
        prpObligations = prpObligations leftPlan <> prpObligations rightPlan
      }

instance Monoid ProtocolRewritePlan where
  mempty =
    ProtocolRewritePlan
      { prpSpliceGroups = [],
        prpObligations = []
      }

type ProtocolRewriteSkip :: Type
data ProtocolRewriteSkip = ProtocolRewriteSkip
  { prsName :: !String,
    prsFailure :: !NebulaError
  }
  deriving stock (Eq, Show)

type ProtocolSealObligation :: Type
data ProtocolSealObligation
  = RedundantPatternClassCanonicalizationRemoved
  | ScopedRegionExtractionDelegates
  deriving stock (Eq, Ord, Show)

type ProtocolModule :: Type
data ProtocolModule = ProtocolModule
  { pmPath :: !FilePath,
    pmBindings :: ![LocatedBinding]
  }

planProtocolRewrites :: FilePath -> String -> Set ProtocolRewriteKind -> ([ProtocolRewriteSkip], ProtocolRewritePlan)
planProtocolRewrites path source requestedKinds =
  case protocolModule path source of
    Left failure ->
      (fmap (`ProtocolRewriteSkip` failure) (fmap protocolRewriteKindName (Set.toAscList requestedKinds)), mempty)
    Right parsed ->
      planProtocolRewritesFromModule parsed requestedKinds

planProtocolRewritesFromModule :: ProtocolModule -> Set ProtocolRewriteKind -> ([ProtocolRewriteSkip], ProtocolRewritePlan)
planProtocolRewritesFromModule parsed requestedKinds =
  (protocolSkips, fold protocolPlans)
  where
    canonicalizationAttempts =
      requestedPlan requestedKinds ProtocolRedundantPatternClassCanonicalization (redundantPatternClassCanonicalizationPlan parsed)
    scopedExtractionAttempts =
      requestedPlan requestedKinds ProtocolScopedRegionExtraction (scopedRegionExtractionPlan parsed)
    protocolAttempts =
      canonicalizationAttempts <> scopedExtractionAttempts
    (protocolSkips, protocolPlans) =
      partitionEithers protocolAttempts

requestedPlan :: Set ProtocolRewriteKind -> ProtocolRewriteKind -> Either NebulaError ProtocolRewritePlan -> [Either ProtocolRewriteSkip ProtocolRewritePlan]
requestedPlan requestedKinds rewriteKind rewritePlan =
  [first (ProtocolRewriteSkip (protocolRewriteKindName rewriteKind)) rewritePlan | rewriteKind `Set.member` requestedKinds]

protocolRewriteKindName :: ProtocolRewriteKind -> String
protocolRewriteKindName = \case
  ProtocolRedundantPatternClassCanonicalization ->
    "regionCandidateSites"
  ProtocolScopedRegionExtraction ->
    "extractScopeRegionProjection"

redundantPatternClassCanonicalizationPlan :: ProtocolModule -> Either NebulaError ProtocolRewritePlan
redundantPatternClassCanonicalizationPlan parsed = do
  regionSites <- requireBinding parsed "regionCandidateSites" redundantPatternClassCanonicalizationProtocol
  pure
    ProtocolRewritePlan
      { prpSpliceGroups = [("regionCandidateSites", [replaceBinding regionSites regionCandidateSitesWithoutCanonicalizationSource])],
        prpObligations = [RedundantPatternClassCanonicalizationRemoved]
      }

scopedRegionExtractionPlan :: ProtocolModule -> Either NebulaError ProtocolRewritePlan
scopedRegionExtractionPlan parsed = do
  scopedExtraction <- requireBinding parsed "extractScopeRegionProjection" scopedRegionExtractionProtocol
  pure
    ProtocolRewritePlan
      { prpSpliceGroups = [("extractScopeRegionProjection", [replaceBinding scopedExtraction extractScopeRegionProjectionDelegatingSource])],
        prpObligations = [ScopedRegionExtractionDelegates]
      }

replaceBinding :: LocatedBinding -> String -> SourceSplice
replaceBinding binding replacement =
  SourceSplice
    { ssRegion = lbRegion binding,
      ssReplacement = replacement
    }

requireBinding :: ProtocolModule -> String -> (LocatedBinding -> Bool) -> Either NebulaError LocatedBinding
requireBinding parsed bindingName predicate = do
  binding <-
    case filter ((== bindingName) . lbName) (pmBindings parsed) of
      [singleBinding] ->
        Right singleBinding
      [] ->
        Left (NebulaWriteBackError ("protocol binding not found in " <> pmPath parsed <> ": " <> bindingName))
      duplicates ->
        Left (NebulaWriteBackError ("protocol binding is ambiguous in " <> pmPath parsed <> ": " <> bindingName <> " has " <> show (length duplicates) <> " definitions"))
  if predicate binding
    then Right binding
    else Left (NebulaWriteBackError ("protocol source shape mismatch in " <> pmPath parsed <> ": " <> bindingName))

redundantPatternClassCanonicalizationProtocol :: LocatedBinding -> Bool
redundantPatternClassCanonicalizationProtocol binding =
  all (`Set.member` bindingGlobalNames binding) ["resolvePatternClass", "normalizedRegionProjection", "canonicalizeClassId"]

scopedRegionExtractionProtocol :: LocatedBinding -> Bool
scopedRegionExtractionProtocol binding =
  all (`Set.member` bindingGlobalNames binding) ["sizeExtractAt", "extractionSignatureAt", "erTerm"]
    && bindingHasRecordConstructor "RegionProjection" binding

bindingHasRecordConstructor :: String -> LocatedBinding -> Bool
bindingHasRecordConstructor constructorName binding =
  any ((== constructorName) . rcConstructorName) (bindingRecordConstructions binding)

sealProtocolObligations :: FilePath -> String -> [ProtocolSealObligation] -> Either NebulaError ()
sealProtocolObligations path patchedSource obligations = do
  parsedModule <-
    either
      (Left . NebulaParseError)
      Right
      (parseHsModule path patchedSource)
  sealProtocolObligationsFromParsedModule path parsedModule obligations

sealProtocolObligationsFromParsedModule :: FilePath -> Ghc.HsModule Ghc.GhcPs -> [ProtocolSealObligation] -> Either NebulaError ()
sealProtocolObligationsFromParsedModule path parsedModule =
  traverse_ (sealProtocolObligation (protocolModuleFromParsedModule path parsedModule))

sealProtocolObligation :: ProtocolModule -> ProtocolSealObligation -> Either NebulaError ()
sealProtocolObligation parsed = \case
  RedundantPatternClassCanonicalizationRemoved -> do
    _ <- requireBinding parsed "regionCandidateSites" regionCandidateSitesUseResolvedClass
    Right ()
  ScopedRegionExtractionDelegates -> do
    _ <- requireBinding parsed "extractScopeRegionProjection" scopedRegionExtractionDelegates
    Right ()

regionCandidateSitesUseResolvedClass :: LocatedBinding -> Bool
regionCandidateSitesUseResolvedClass binding =
  Set.member "resolvePatternClass" names
    && Set.member "normalizedRegionProjection" names
    && not (Set.member "canonicalizeClassId" names)
  where
    names =
      bindingGlobalNames binding

scopedRegionExtractionDelegates :: LocatedBinding -> Bool
scopedRegionExtractionDelegates binding =
  Set.member "resolvePatternClass" names
    && Set.member "normalizedRegionProjection" names
    && Set.member "rpTerm" names
    && not (Set.member "sizeExtractAt" names)
    && not (Set.member "extractionSignatureAt" names)
    && not (bindingHasRecordConstructor "RegionProjection" binding)
  where
    names =
      bindingGlobalNames binding

protocolModule :: FilePath -> String -> Either NebulaError ProtocolModule
protocolModule path source =
  protocolModuleFromParsedModule path
    <$> either (Left . NebulaParseError) Right (parseHsModule path source)

protocolModuleFromParsedModule :: FilePath -> Ghc.HsModule Ghc.GhcPs -> ProtocolModule
protocolModuleFromParsedModule path parsedModule =
  ProtocolModule path (locatedValueBindingsFromParsedModule parsedModule)

regionCandidateSitesWithoutCanonicalizationSource :: String
regionCandidateSitesWithoutCanonicalizationSource =
  renderRegionCandidateSites regionCandidateSiteDirectRows

renderRegionCandidateSites :: [String] -> String
renderRegionCandidateSites resultRows =
  renderTopLevel
    ( [ "regionCandidateSites ::",
        "  NebulaConfig ->",
        "  EGraph HsExprF NebulaAnalysis ->",
        "  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->",
        "  NebulaSizeExtractionSections ->",
        "  BindingHarvestRow ->",
        "  ChosenBinding ->",
        "  Either NebulaError [CandidateSite]",
        "regionCandidateSites config baseGraph contextGraph sizeSections bindingRow chosenBinding =",
        "  fmap (filter siteLargeEnough) $",
        "    nestedRegionSites (tlbRegion binding) (tlbScopedTerm binding) (tlbSpannedTerm binding)",
        "  where",
        "    binding =",
        "      bhrBinding bindingRow",
        "",
        "    siteLargeEnough site =",
        "      csSize site >= max 1 (ncDiagnosticMinShared config + 1)",
        "",
        "    nestedRegionSites rootRegion scopedExpr spannedExpr =",
        "      (<>) <$> currentSite rootRegion scopedExpr spannedExpr",
        "        <*> fmap concat",
        "          ( traverse",
        "              (uncurry (nestedRegionSites rootRegion))",
        "              (zip (toList (seNode scopedExpr)) (toList (sxNode spannedExpr)))",
        "          )",
        "",
        "    currentSite rootRegion scopedExpr spannedExpr = do",
        "      case sxRegion spannedExpr of",
        "        Nothing ->",
        "          Right []",
        "        Just region",
        "          | Just region == rootRegion ->",
        "              Right []",
        "          | otherwise ->",
        "              case resolvePatternClass baseGraph (eraseScopedExpr scopedExpr) of",
        "                Nothing ->",
        "                  Right []",
        "                Just classId ->",
        "                  case normalizedRegionProjection config contextGraph sizeSections scopedExpr classId of",
        "                    Left obstruction ->",
        "                      Left obstruction",
        "                    Right Nothing ->",
        "                      Right []",
        "                    Right (Just projection) ->",
        "                      Right"
      ]
        <> resultRows
    )

regionCandidateSiteDirectRows :: [String]
regionCandidateSiteDirectRows =
  [ "                        [ CandidateSite",
    "                            { csOrdinal = -1,",
    "                              csBindingName = cbName chosenBinding,",
    "                              csSiteKind = RegionCandidateSite,",
    "                              csRegion = Just region,",
    "                              csContext = ActualScope (seOccScope scopedExpr),",
    "                              csClass = classId,",
    "                              csTerm = rpTerm projection,",
    "                              csSourceTerm = fixScopedExpr scopedExpr,",
    "                              csOriginalSize = patternNodeCount (eraseScopedExpr scopedExpr),",
    "                              csSize = termSize (rpTerm projection),",
    "                              csFreeScopeWidth = scopedFreeScopeWidth scopedExpr,",
    "                              csSignature = rpSignature projection,",
    "                              csTypeEvidence = classTypeEvidence baseGraph classId",
    "                            }",
    "                        ]"
  ]

extractScopeRegionProjectionDelegatingSource :: String
extractScopeRegionProjectionDelegatingSource =
  renderTopLevel
    [ "extractScopeRegionProjection ::",
      "  NebulaConfig ->",
      "  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->",
      "  NebulaSizeExtractionSections ->",
      "  ScopedExpr ->",
      "  Pattern HsExprF ->",
      "  Maybe RegionProjection",
      "extractScopeRegionProjection config contextGraph sizeSections scopedExpr regionPattern = do",
      "  regionClass <- resolvePatternClass (cegBase contextGraph) regionPattern",
      "  projection <-",
      "    either (const Nothing) id $",
      "      normalizedRegionProjection config contextGraph sizeSections scopedExpr regionClass",
      "  if termSize (rpTerm projection) < patternNodeCount regionPattern",
      "    then Just projection",
      "    else Nothing"
    ]

renderTopLevel :: [String] -> String
renderTopLevel rows =
  unlines rows
