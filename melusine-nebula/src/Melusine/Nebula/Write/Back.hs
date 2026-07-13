{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.Write.Back
  ( WriteStatus (..),
    writeStatusKey,
    WriteOutcome (..),
    WriteBackRefusal (..),
    writeBackRefusalKey,
    renderRefusalKey,
    hsOpaqueTagKey,
    hsPatOpaqueTagKey,
    SourceQualityRefusal (..),
    sourceQualityRefusalKey,
    LineOnlyMinificationEvidence (..),
    SourceLineQualityEvidence (..),
    AppendedDefinition (..),
    ModulePatch (..),
    planWriteBack,
    refuseTypeIncompatible,
    modulePatchHasContent,
    patchedModuleSource,
  )
where

import Control.Applicative ((<|>))
import Data.Bifunctor (second)
import Data.Either (partitionEithers)
import Data.Kind (Type)
import Data.List (find, intercalate, isInfixOf, isPrefixOf, isSuffixOf, tails)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import Melusine.Nebula.Proof.Audit (StepTypeConflict, TypeVerdict (..))
import Melusine.Nebula.Discovery.Choose (ChosenBinding (..))
import Melusine.Nebula.Core (ModuleWorkload (..), NebulaError (..))
import Melusine.Nebula.Source.Ingest (IngestedModule (..))
import Melusine.Nebula.Source.Ast (sourceRegionText)
import Melusine.Nebula.Write.Patch (SourceSplice (..), applySplices)
import Melusine.Nebula.Synthesis.Core
  ( CandidateRejection (..),
    RecordOwnershipFinding (..),
    RejectedCandidate (..),
    SynthesisOutcome (..),
    SynthesizedDefinition (..),
    SynthesizedName (..),
  )
import Melusine.Nebula.Write.Declaration
  ( DeclarationPatch (..),
    DeclarationSealObligation,
    RecordSelectorRewrite (..),
    planRecordOwnershipRewrite,
  )
import Melusine.Nebula.Write.Protocol
  ( ProtocolRewriteKind (..),
    ProtocolRewritePlan (..),
    ProtocolSealObligation,
    ProtocolRewriteSkip (..),
    planProtocolRewrites,
  )
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( BinderAnn,
    ConvertedModule (..),
    HsExprF (..),
    HsOpaqueTag (..),
    HsPatF,
    HsPatOpaqueTag (..),
    HsVarRef (..),
    RenderRefusal (..),
    SourceRegion (..),
    SpannedExpr (..),
    TopLevelBinding (..),
    eraseSpannedExpr,
  )
import Moonlight.EGraph.Pure.Rewrite.Instantiate (patternFromFix)
import Moonlight.Core (Pattern)
import Data.Fix (Fix (..))
import Moonlight.Pale.Ghc.Expr (renderReadableHsExpr, renderReadableTopLevelBinding, renderRoundTripEquivalent)

type WriteStatus :: Type
data WriteStatus
  = WriteWritten
  | WriteCandidateWritten
  | WriteRefused
  | WriteSkipped
  | WriteIoError
  deriving stock (Eq, Ord, Show)

writeStatusKey :: WriteStatus -> String
writeStatusKey = \case
  WriteWritten ->
    "written"
  WriteCandidateWritten ->
    "candidate-written"
  WriteRefused ->
    "refused"
  WriteSkipped ->
    "skipped"
  WriteIoError ->
    "io-error"

type WriteOutcome :: Type
data WriteOutcome = WriteOutcome
  { woPath :: !FilePath,
    woStatus :: !WriteStatus,
    woReasonKey :: !(Maybe String),
    woMessage :: !(Maybe String)
  }
  deriving stock (Eq, Show)

type WriteBackRefusal :: Type
data WriteBackRefusal
  = RefusedUnchanged
  | RefusedRender !RenderRefusal
  | RefusedMultiName
  | RefusedNoRegion
  | RefusedTypeIncompatible ![StepTypeConflict]
  | RefusedDeclarationRewrite !NebulaError
  | RefusedProtocolRewrite !NebulaError
  | RefusedSourceQuality !SourceQualityRefusal
  deriving stock (Eq, Show)

renderRefusalKey :: RenderRefusal -> String
renderRefusalKey = \case
  RenderOpaque opaqueTag ->
    "render:" <> hsOpaqueTagKey opaqueTag
  RenderGuardedExpression ->
    "render:guarded-expression"
  RenderWhereExpression ->
    "render:where-expression"
  RenderPatternVariable ->
    "render:pattern-variable"
  RenderNonVarOperator ->
    "render:non-var-operator"
  RenderPatOpaque patTag ->
    "render:" <> hsPatOpaqueTagKey patTag
  RenderEmptyBindingName ->
    "render:empty-binding-name"
  RenderClausesShape ->
    "render:clauses-shape"

hsOpaqueTagKey :: HsOpaqueTag -> String
hsOpaqueTagKey = \case
  OpaqueOverLabel -> "opaque-over-label"
  OpaqueIPVar -> "opaque-ip-var"
  OpaqueAppType -> "opaque-app-type"
  OpaqueExplicitSum -> "opaque-explicit-sum"
  OpaqueMultiIf -> "opaque-multi-if"
  OpaqueRecordUpd -> "opaque-record-update"
  OpaqueGetField -> "opaque-get-field"
  OpaqueProjection -> "opaque-projection"
  OpaqueExprWithTySig -> "opaque-expression-with-type-signature"
  OpaqueArithSeq -> "opaque-arithmetic-sequence"
  OpaqueTypedBracket -> "opaque-typed-bracket"
  OpaqueUntypedBracket -> "opaque-untyped-bracket"
  OpaqueTypedSplice -> "opaque-typed-splice"
  OpaqueUntypedSplice -> "opaque-untyped-splice"
  OpaqueProc -> "opaque-proc"
  OpaqueStatic -> "opaque-static"
  OpaquePragE -> "opaque-expression-pragma"
  OpaqueEmbTy -> "opaque-embedded-type"
  OpaqueHole -> "opaque-hole"
  OpaqueForAll -> "opaque-for-all"
  OpaqueQual -> "opaque-qualified-type"
  OpaqueFunArr -> "opaque-function-arrow"
  OpaqueXExpr -> "opaque-expression-extension"
  OpaqueLambdaMatchGroup -> "opaque-lambda-match-group"
  OpaqueCaseAlternative -> "opaque-case-alternative"
  OpaqueLocalIPBinds -> "opaque-local-implicit-parameter-bindings"
  OpaqueXLocalBinds -> "opaque-local-bindings-extension"
  OpaqueValBindsExtension -> "opaque-value-bindings-extension"
  OpaqueUnsupportedBind -> "opaque-unsupported-binding"
  OpaqueUnsupportedStmt -> "opaque-unsupported-statement"
  OpaqueUnsupportedGuard -> "opaque-unsupported-guard"
  OpaqueMissingGuardFallback -> "opaque-missing-guard-fallback"
  OpaqueUnsupportedRecordField -> "opaque-unsupported-record-field"

hsPatOpaqueTagKey :: HsPatOpaqueTag -> String
hsPatOpaqueTagKey = \case
  PatOpaqueOr -> "pattern-opaque-or"
  PatOpaqueSum -> "pattern-opaque-sum"
  PatOpaqueView -> "pattern-opaque-view"
  PatOpaqueSplice -> "pattern-opaque-splice"
  PatOpaqueNPlusK -> "pattern-opaque-n-plus-k"
  PatOpaqueSig -> "pattern-opaque-signature"
  PatOpaqueEmbTy -> "pattern-opaque-embedded-type"
  PatOpaqueInvis -> "pattern-opaque-invisible"
  PatOpaqueRecCon -> "pattern-opaque-record-constructor"
  PatOpaqueNegativeLit -> "pattern-opaque-negative-literal"
  PatOpaqueUnboxedTuple -> "pattern-opaque-unboxed-tuple"
  PatOpaqueExtension -> "pattern-opaque-extension"

writeBackRefusalKey :: WriteBackRefusal -> String
writeBackRefusalKey = \case
  RefusedUnchanged ->
    "unchanged"
  RefusedRender refusal ->
    renderRefusalKey refusal
  RefusedMultiName ->
    "multi-name"
  RefusedNoRegion ->
    "no-region"
  RefusedTypeIncompatible {} ->
    "type-incompatible"
  RefusedDeclarationRewrite {} ->
    "declaration-rewrite-refused"
  RefusedProtocolRewrite {} ->
    "protocol-rewrite-refused"
  RefusedSourceQuality {} ->
    "source-quality-refused"

type SourceQualityRefusal :: Type
data SourceQualityRefusal
  = SourceQualityCompactBlockSyntax !String
  | SourceQualityLineOnlyMinification !LineOnlyMinificationEvidence
  | SourceQualityOverlongGeneratedLine !SourceLineQualityEvidence
  | SourceQualityInlineListLayout !SourceLineQualityEvidence
  | SourceQualityInlineConsPatternLayout !SourceLineQualityEvidence
  deriving stock (Eq, Show)

sourceQualityRefusalKey :: SourceQualityRefusal -> String
sourceQualityRefusalKey = \case
  SourceQualityCompactBlockSyntax {} ->
    "compact-block-syntax"
  SourceQualityLineOnlyMinification {} ->
    "line-only-minification"
  SourceQualityOverlongGeneratedLine {} ->
    "overlong-generated-line"
  SourceQualityInlineListLayout {} ->
    "inline-list-layout"
  SourceQualityInlineConsPatternLayout {} ->
    "inline-cons-pattern-layout"

type LineOnlyMinificationEvidence :: Type
data LineOnlyMinificationEvidence = LineOnlyMinificationEvidence
  { lomOriginalLines :: !Int,
    lomReplacementLines :: !Int,
    lomOriginalBytes :: !Int,
    lomReplacementBytes :: !Int
  }
  deriving stock (Eq, Show)

type SourceLineQualityEvidence :: Type
data SourceLineQualityEvidence = SourceLineQualityEvidence
  { slqeLineLimit :: !Int,
    slqeLineNumber :: !Int,
    slqeOriginalMaxLineLength :: !Int,
    slqeReplacementLineLength :: !Int,
    slqeReplacementLinePreview :: !String
  }
  deriving stock (Eq, Show)

type AppendedDefinition :: Type
data AppendedDefinition = AppendedDefinition
  { adName :: !String,
    adTerm :: !(Fix HsExprF),
    adSource :: !String
  }

type ModulePatch :: Type
data ModulePatch = ModulePatch
  { mpPath :: !FilePath,
    mpSplices :: ![SourceSplice],
    mpSpliceGroups :: ![(String, [SourceSplice])],
    mpDeclarationSpliceGroups :: ![(String, [SourceSplice])],
    mpDeclarationObligations :: ![DeclarationSealObligation],
    mpProtocolSpliceGroups :: ![(String, [SourceSplice])],
    mpProtocolObligations :: ![ProtocolSealObligation],
    mpAppendedDefinitions :: ![AppendedDefinition],
    mpSpliced :: ![(String, Fix HsExprF)],
    mpSkipped :: ![(String, WriteBackRefusal)]
  }

planWriteBack :: ModuleWorkload -> IngestedModule -> SynthesisOutcome -> Either NebulaError ModulePatch
planWriteBack workload ingested outcome = do
  rows <-
    if length bindings == length chosen
      then Right (zip bindings chosen)
      else Left (NebulaWriteBackError ("binding rows diverge from synthesis rows: " <> show (length bindings) <> " vs " <> show (length chosen)))
  let (skipped, acceptedBeforeDefinitionQuality) = partitionEithers (fmap (classifyRow (mwSource workload)) rows)
      referencedNames = foldMap (globalReferenceNames . snd . snd) acceptedBeforeDefinitionQuality
  renderedDefinitions <- traverse renderDefinition (selectReferencedDefinitions referencedNames (soDefinitions outcome))
  let (declarationSkipped, declarationAccepted) =
        partitionEithers
          ( fmap
              (planDeclarationOwnershipRewrite (mwPath workload) (mwSource workload))
              (declarationOwnershipRequests outcome)
          )
      declarationSplices =
        foldMap (dpSplices . snd) declarationAccepted
      (protocolSkipped, protocolPatch) =
        planProtocolRewrites (mwPath workload) (mwSource workload) (protocolRewriteKinds outcome)
      protocolSplices =
        foldMap snd (prpSpliceGroups protocolPatch)
      definitionQualityRefusals =
        propagatedDefinitionQualityRefusals renderedDefinitions (directDefinitionQualityRefusals renderedDefinitions)
      (definitionBlockedRows, accepted) =
        partitionEithers (fmap (classifyAcceptedByDefinitionQuality definitionQualityRefusals) acceptedBeforeDefinitionQuality)
      appended =
        filter (not . (`Map.member` definitionQualityRefusals) . adName) renderedDefinitions
      definitionSkipped =
        fmap (\(definitionName, refusal) -> (definitionName, RefusedSourceQuality refusal)) (Map.toList definitionQualityRefusals)
  pure
    ModulePatch
      { mpPath = imPath ingested,
        mpSplices = foldMap fst accepted <> declarationSplices <> protocolSplices,
        mpSpliceGroups = fmap (\(splices, (bindingName, _)) -> (bindingName, splices)) accepted,
        mpDeclarationSpliceGroups = fmap (second dpSplices) declarationAccepted,
        mpDeclarationObligations = foldMap (dpObligations . snd) declarationAccepted,
        mpProtocolSpliceGroups = prpSpliceGroups protocolPatch,
        mpProtocolObligations = prpObligations protocolPatch,
        mpAppendedDefinitions = appended,
        mpSpliced = fmap snd accepted,
        mpSkipped = skipped <> definitionBlockedRows <> definitionSkipped <> declarationSkipped <> fmap protocolRewriteSkipped protocolSkipped
      }
  where
    bindings = cmBindings (imConverted ingested)
    chosen = soBindings outcome

type DeclarationOwnershipRequest :: Type
data DeclarationOwnershipRequest = DeclarationOwnershipRequest
  { dorConstructorName :: !String,
    dorSelectorRewrites :: !(Map String RecordSelectorRewrite)
  }

mergeDeclarationOwnershipRequest :: DeclarationOwnershipRequest -> DeclarationOwnershipRequest -> DeclarationOwnershipRequest
mergeDeclarationOwnershipRequest leftRequest rightRequest =
  DeclarationOwnershipRequest
    { dorConstructorName = dorConstructorName leftRequest,
      dorSelectorRewrites = dorSelectorRewrites leftRequest <> dorSelectorRewrites rightRequest
    }

declarationOwnershipRequests :: SynthesisOutcome -> [DeclarationOwnershipRequest]
declarationOwnershipRequests outcome =
  Map.elems $
    Map.fromListWith
      mergeDeclarationOwnershipRequest
      [ (rofConstructorName finding, requestFromFinding finding)
      | rejection <- soRejected outcome,
        RejectedRecordOwnershipDiagnostic findings <- [rejReason rejection],
        finding <- findings
      ]

requestFromFinding :: RecordOwnershipFinding -> DeclarationOwnershipRequest
requestFromFinding finding =
  DeclarationOwnershipRequest
    { dorConstructorName = rofConstructorName finding,
      dorSelectorRewrites =
        Map.singleton
          (rofDerivedField finding)
          RecordSelectorRewrite
            { rsrDeletedSelector = rofDerivedField finding,
              rsrOwnerSelector = rofOwnerField finding,
              rsrProjectedSelector = rofProjectionName finding
            }
    }

directDefinitionQualityRefusals :: [AppendedDefinition] -> Map String SourceQualityRefusal
directDefinitionQualityRefusals =
  Map.fromList . mapMaybe definitionQualityRefusalRow

definitionQualityRefusalRow :: AppendedDefinition -> Maybe (String, SourceQualityRefusal)
definitionQualityRefusalRow definition =
  fmap ((,) (adName definition)) (generatedSourceQualityRefusal (adSource definition))

propagatedDefinitionQualityRefusals ::
  [AppendedDefinition] ->
  Map String SourceQualityRefusal ->
  Map String SourceQualityRefusal
propagatedDefinitionQualityRefusals definitions refusals
  | expanded == refusals =
      refusals
  | otherwise =
      propagatedDefinitionQualityRefusals definitions expanded
  where
    expanded =
      refusals <> Map.fromList (mapMaybe inheritedRefusal definitions)
    inheritedRefusal definition
      | adName definition `Map.member` refusals =
          Nothing
      | otherwise =
          fmap ((,) (adName definition)) (firstReferencedDefinitionRefusal refusals (adTerm definition))

firstReferencedDefinitionRefusal :: Map String SourceQualityRefusal -> Fix HsExprF -> Maybe SourceQualityRefusal
firstReferencedDefinitionRefusal refusals termValue =
  firstJust (fmap (`Map.lookup` refusals) (Set.toList (globalReferenceNames termValue)))

firstJust :: [Maybe value] -> Maybe value
firstJust =
  foldr (<|>) Nothing

classifyAcceptedByDefinitionQuality ::
  Map String SourceQualityRefusal ->
  ([SourceSplice], (String, Fix HsExprF)) ->
  Either (String, WriteBackRefusal) ([SourceSplice], (String, Fix HsExprF))
classifyAcceptedByDefinitionQuality refusals row@(_, (bindingName, termValue)) =
  maybe
    (Right row)
    (\refusal -> Left (bindingName, RefusedSourceQuality refusal))
    (firstReferencedDefinitionRefusal refusals termValue)

protocolRewriteKinds :: SynthesisOutcome -> Set ProtocolRewriteKind
protocolRewriteKinds outcome =
  Set.fromList (catMaybes (fmap (protocolRewriteKindForRejection . rejReason) (soRejected outcome)))

protocolRewriteKindForRejection :: CandidateRejection -> Maybe ProtocolRewriteKind
protocolRewriteKindForRejection = \case
  RejectedRedundantPatternClassCanonicalizationDiagnostic ->
    Just ProtocolRedundantPatternClassCanonicalization
  RejectedScopedRegionExtractionProtocolDiagnostic ->
    Just ProtocolScopedRegionExtraction
  _ ->
    Nothing

planDeclarationOwnershipRewrite ::
  FilePath ->
  String ->
  DeclarationOwnershipRequest ->
  Either (String, WriteBackRefusal) (String, DeclarationPatch)
planDeclarationOwnershipRewrite path source request =
  case planRecordOwnershipRewrite path source constructorName deletedFields selectorRewrites of
    Left failure ->
      Left (constructorName, RefusedDeclarationRewrite failure)
    Right patch ->
      Right (constructorName, patch)
  where
    constructorName =
      dorConstructorName request
    selectorRewriteMap =
      dorSelectorRewrites request
    deletedFields =
      Map.keysSet selectorRewriteMap
    selectorRewrites =
      Map.elems selectorRewriteMap

protocolRewriteSkipped :: ProtocolRewriteSkip -> (String, WriteBackRefusal)
protocolRewriteSkipped skipped =
  (prsName skipped, RefusedProtocolRewrite (prsFailure skipped))

refuseTypeIncompatible :: Map String TypeVerdict -> ModulePatch -> ModulePatch
refuseTypeIncompatible verdicts modulePatch =
  modulePatch
    { mpSplices =
        foldMap snd acceptedSpliceGroups
          <> foldMap snd (mpDeclarationSpliceGroups modulePatch)
          <> foldMap snd (mpProtocolSpliceGroups modulePatch),
      mpSpliceGroups = acceptedSpliceGroups,
      mpSpliced = filter ((`Set.member` acceptedSpliceNames) . fst) (mpSpliced modulePatch),
      mpAppendedDefinitions = acceptedDefinitions,
      mpSkipped = mpSkipped modulePatch <> refusedSplicedRows <> refusedDefinitionRows
    }
  where
    (refusedSplicePairs, acceptedSpliceGroups) =
      partitionEithers (fmap classifySpliceGroup (mpSpliceGroups modulePatch))
    refusedSplicedRows =
      fmap snd refusedSplicePairs
    acceptedSpliceNames =
      Set.fromList (fmap fst acceptedSpliceGroups)
    (refusedDefinitions, acceptedDefinitions) =
      partitionEithers (fmap classifyDefinition (mpAppendedDefinitions modulePatch))
    refusedDefinitionRows =
      fmap snd refusedDefinitions
    classifySpliceGroup row@(bindingName, _) =
      classifyTypeCompatible verdicts bindingName row
    classifyDefinition definition =
      classifyTypeCompatible verdicts (adName definition) definition

classifyTypeCompatible ::
  Map String TypeVerdict ->
  String ->
  row ->
  Either (row, (String, WriteBackRefusal)) row
classifyTypeCompatible verdicts bindingName row =
  maybe
    (Right row)
    (\conflicts -> Left (row, (bindingName, RefusedTypeIncompatible conflicts)))
    (incompatibleConflicts verdicts bindingName)

incompatibleConflicts :: Map String TypeVerdict -> String -> Maybe [StepTypeConflict]
incompatibleConflicts verdicts bindingName =
  case Map.lookup bindingName verdicts of
    Just (TypeIncompatible conflicts) ->
      Just conflicts
    _ ->
      Nothing

classifyRow ::
  String ->
  (TopLevelBinding, ChosenBinding) ->
  Either (String, WriteBackRefusal) ([SourceSplice], (String, Fix HsExprF))
classifyRow source (binding, chosenBinding) =
  case tlbNames binding of
    [_] ->
      case tlbRegion binding of
        Nothing ->
          skip RefusedNoRegion
        Just region ->
          case classifyBodyRegionRow source binding chosenBinding of
            Right accepted ->
              case sourceQualityGroupRefusal source region (fst accepted) of
                Nothing ->
                  Right accepted
                Just qualityRefusal ->
                  skip (RefusedSourceQuality qualityRefusal)
            Left (_, RefusedSourceQuality qualityRefusal) ->
              skip (RefusedSourceQuality qualityRefusal)
            Left _ ->
              classifyWholeBindingRow source region bindingName extractedPattern chosenBinding binding
    _ ->
      skip RefusedMultiName
  where
    bindingName = cbName chosenBinding
    extractedPattern = patternFromFix (cbTerm chosenBinding)
    skip = Left . (,) bindingName

classifyWholeBindingRow ::
  String ->
  SourceRegion ->
  String ->
  Pattern HsExprF ->
  ChosenBinding ->
  TopLevelBinding ->
  Either (String, WriteBackRefusal) ([SourceSplice], (String, Fix HsExprF))
classifyWholeBindingRow source region bindingName extractedPattern chosenBinding binding =
  case renderReadableTopLevelBinding bindingName extractedPattern of
    Left refusal ->
      skip (RefusedRender refusal)
    Right rendered
      | renderRoundTripEquivalent extractedPattern (tlbTerm binding) ->
          skip RefusedUnchanged
      | otherwise ->
          case qualityCheckedSplice source region rendered of
            Left refusal ->
              skip refusal
            Right splice ->
              Right ([splice], (bindingName, cbTerm chosenBinding))
  where
    skip = Left . (,) bindingName

classifyBodyRegionRow ::
  String ->
  TopLevelBinding ->
  ChosenBinding ->
  Either (String, WriteBackRefusal) ([SourceSplice], (String, Fix HsExprF))
classifyBodyRegionRow source binding chosenBinding =
  case classifyLambdaBodyRegionRow source bindingName binding chosenBinding of
    Left refusal ->
      skip refusal
    Right (Just accepted) ->
      Right accepted
    Right Nothing ->
      case classifyClausesBodyRegionRow source bindingName binding chosenBinding of
        Left refusal ->
          skip refusal
        Right Nothing ->
          skip RefusedUnchanged
        Right (Just accepted) ->
          Right accepted
  where
    bindingName = cbName chosenBinding
    skip = Left . (,) bindingName

classifyLambdaBodyRegionRow ::
  String ->
  String ->
  TopLevelBinding ->
  ChosenBinding ->
  Either WriteBackRefusal (Maybe ([SourceSplice], (String, Fix HsExprF)))
classifyLambdaBodyRegionRow source bindingName binding chosenBinding =
  case (spannedLambdaBody (tlbSpannedTerm binding), fixLambdaBody (cbTerm chosenBinding)) of
    (Just (sourceBinders, sourceBody), Just (extractedBinders, extractedBody))
      | sourceBinders == extractedBinders ->
          case changedBodySplice source sourceBody extractedBody of
            Just (Left refusal) ->
              Left refusal
            Just (Right (Just splice)) ->
              Right (Just ([splice], (bindingName, cbTerm chosenBinding)))
            Just (Right Nothing) ->
              Right Nothing
            Nothing ->
              Right Nothing
    _ ->
      Right Nothing

spannedLambdaBody :: SpannedExpr -> Maybe ([BinderAnn], SpannedExpr)
spannedLambdaBody =
  leadingLambdaBody sxNode

leadingLambdaBody :: (expr -> HsExprF expr) -> expr -> Maybe ([BinderAnn], expr)
leadingLambdaBody nodeOf expr =
  case nodeOf expr of
    LamF binder body ->
      Just (leadingLambdaBodyWith nodeOf binder body)
    _ ->
      Nothing

leadingLambdaBodyWith :: (expr -> HsExprF expr) -> BinderAnn -> expr -> ([BinderAnn], expr)
leadingLambdaBodyWith nodeOf binder body =
  case nodeOf body of
    LamF nextBinder nextBody ->
      let (binders, finalBody) = leadingLambdaBodyWith nodeOf nextBinder nextBody
       in (binder : binders, finalBody)
    _ ->
      ([binder], body)

fixLambdaBody :: Fix HsExprF -> Maybe ([BinderAnn], Fix HsExprF)
fixLambdaBody =
  leadingLambdaBody fixNode

classifyClausesBodyRegionRow ::
  String ->
  String ->
  TopLevelBinding ->
  ChosenBinding ->
  Either WriteBackRefusal (Maybe ([SourceSplice], (String, Fix HsExprF)))
classifyClausesBodyRegionRow source bindingName binding chosenBinding =
  case (spannedClauseBodies (tlbSpannedTerm binding), fixClauseBodies (cbTerm chosenBinding)) of
    (Just sourceClauses, Just extractedClauses)
      | fmap fst sourceClauses == fmap fst extractedClauses ->
          case traverse (changedClauseSplice source) (zip sourceClauses extractedClauses) of
            Just maybeSplices ->
              case sequenceA maybeSplices of
                Left refusal ->
                  Left refusal
                Right spliceOptions ->
                  case catMaybes spliceOptions of
                    [] ->
                      Right Nothing
                    splices ->
                      Right (Just (splices, (bindingName, cbTerm chosenBinding)))
            Nothing ->
              Right Nothing
    _ ->
      Right Nothing

spannedClauseBodies :: SpannedExpr -> Maybe [([HsPatF], SpannedExpr)]
spannedClauseBodies =
  clauseBodies sxNode

fixClauseBodies :: Fix HsExprF -> Maybe [([HsPatF], Fix HsExprF)]
fixClauseBodies =
  clauseBodies fixNode

clauseBodies :: (expr -> HsExprF expr) -> expr -> Maybe [([HsPatF], expr)]
clauseBodies nodeOf expr =
  case nodeOf expr of
    ClausesF clauses ->
      Just clauses
    _ ->
      Nothing

fixNode :: Fix HsExprF -> HsExprF (Fix HsExprF)
fixNode (Fix nodeValue) =
  nodeValue

changedClauseSplice :: String -> (([HsPatF], SpannedExpr), ([HsPatF], Fix HsExprF)) -> Maybe (Either WriteBackRefusal (Maybe SourceSplice))
changedClauseSplice source ((_, sourceBody), (_, extractedBody)) =
  changedBodySplice source sourceBody extractedBody

changedBodySplice :: String -> SpannedExpr -> Fix HsExprF -> Maybe (Either WriteBackRefusal (Maybe SourceSplice))
changedBodySplice source sourceBody extractedBody
  | renderRoundTripEquivalent (patternFromFix extractedBody) (eraseSpannedExpr sourceBody) =
      Just (Right Nothing)
  | otherwise = do
      region <- sxRegion sourceBody
      rendered <- either (const Nothing) Just (renderReadableHsExpr (patternFromFix extractedBody))
      Just (Just <$> qualityCheckedSplice source region rendered)

selectReferencedDefinitions :: Set String -> [SynthesizedDefinition] -> [SynthesizedDefinition]
selectReferencedDefinitions seedNames definitions =
  filter ((`Set.member` reachableNames seedNames) . synthesizedNameText . sdName) definitions
  where
    reachableNames activeNames =
      let expanded =
            activeNames
              <> foldMap
                (globalReferenceNames . sdTerm)
                (filter ((`Set.member` activeNames) . synthesizedNameText . sdName) definitions)
       in if expanded == activeNames then activeNames else reachableNames expanded

renderDefinition :: SynthesizedDefinition -> Either NebulaError AppendedDefinition
renderDefinition definition =
  either
    (\refusal -> Left (NebulaWriteBackError ("synthesized definition " <> definitionName <> " refused to render: " <> show refusal)))
    (Right . AppendedDefinition definitionName (sdTerm definition))
    (renderReadableTopLevelBinding definitionName (patternFromFix (sdTerm definition)))
  where
    definitionName =
      synthesizedNameText (sdName definition)

qualityCheckedSplice :: String -> SourceRegion -> String -> Either WriteBackRefusal SourceSplice
qualityCheckedSplice source region rendered =
  maybe
    (Right splice)
    (Left . RefusedSourceQuality)
    (sourceQualityRefusal source splice)
  where
    splice =
      SourceSplice region (alignReplacementToRegion region rendered)

alignReplacementToRegion :: SourceRegion -> String -> String
alignReplacementToRegion region rendered =
  case lines rendered of
    [] ->
      rendered
    firstLine : remainingLines ->
      intercalate "\n" (firstLine : fmap (regionIndent <>) remainingLines)
  where
    regionIndent =
      replicate (max 0 (srStartCol region - 1)) ' '

sourceQualityRefusal :: String -> SourceSplice -> Maybe SourceQualityRefusal
sourceQualityRefusal source splice =
  either
    (const Nothing)
    (`sourceQualityRefusalFromText` replacement)
    (sourceRegionText source (ssRegion splice))
  where
    replacement =
      ssReplacement splice

sourceQualityGroupRefusal :: String -> SourceRegion -> [SourceSplice] -> Maybe SourceQualityRefusal
sourceQualityGroupRefusal source region splices =
  either
    (const Nothing)
    groupRefusal
    (sourceRegionText source region)
  where
    groupRefusal original =
      case traverse (relativeSplice region) splices >>= (`applySplices` original) of
        Left _ ->
          Nothing
        Right replacement ->
          sourceQualityRefusalFromText original replacement

sourceQualityRefusalFromText :: String -> String -> Maybe SourceQualityRefusal
sourceQualityRefusalFromText original replacement =
  compactSyntaxRefusal original replacement
    <|> inlineConsPatternLayoutRefusal original replacement
    <|> inlineListLayoutRefusal original replacement
    <|> overlongGeneratedLineRefusal original replacement
    <|> lineOnlyMinificationRefusal original replacement

generatedSourceQualityRefusal :: String -> Maybe SourceQualityRefusal
generatedSourceQualityRefusal generated =
  compactSyntaxRefusal "" generated
    <|> inlineConsPatternLayoutRefusal "" generated
    <|> inlineListLayoutRefusal "" generated
    <|> overlongGeneratedLineRefusal "" generated

relativeSplice :: SourceRegion -> SourceSplice -> Either NebulaError SourceSplice
relativeSplice parent splice
  | srStartLine child < srStartLine parent || srEndLine child < srStartLine parent =
      Left (NebulaSpliceError "splice starts before parent region")
  | otherwise =
      Right
        SourceSplice
          { ssRegion =
              SourceRegion
                { srStartLine = srStartLine child - srStartLine parent + 1,
                  srStartCol = relativeCol (srStartLine child) (srStartCol child),
                  srEndLine = srEndLine child - srStartLine parent + 1,
                  srEndCol = relativeCol (srEndLine child) (srEndCol child)
                },
            ssReplacement = ssReplacement splice
          }
  where
    child =
      ssRegion splice
    relativeCol lineNumber columnNumber
      | lineNumber == srStartLine parent =
          columnNumber - srStartCol parent + 1
      | otherwise =
          columnNumber

compactSyntaxRefusal :: String -> String -> Maybe SourceQualityRefusal
compactSyntaxRefusal original replacement =
  SourceQualityCompactBlockSyntax
    <$> find (\marker -> marker `isInfixOf` replacement && not (marker `isInfixOf` original)) compactBlockSyntaxMarkers

compactBlockSyntaxMarkers :: [String]
compactBlockSyntaxMarkers =
  ["do {", "let {", "\\case {", "\\cases {", "where {"]

inlineConsPatternLayoutRefusal :: String -> String -> Maybe SourceQualityRefusal
inlineConsPatternLayoutRefusal original replacement =
  SourceQualityInlineConsPatternLayout
    <$> firstLineQualityEvidence inlineConsPatternLineLimit original replacement isInlineConsPatternLine

inlineListLayoutRefusal :: String -> String -> Maybe SourceQualityRefusal
inlineListLayoutRefusal original replacement =
  SourceQualityInlineListLayout
    <$> firstLineQualityEvidence inlineListLineLimit original replacement isInlineListLayoutLine

overlongGeneratedLineRefusal :: String -> String -> Maybe SourceQualityRefusal
overlongGeneratedLineRefusal original replacement =
  SourceQualityOverlongGeneratedLine
    <$> firstLineQualityEvidence readableGeneratedLineLimit original replacement (const True)

firstLineQualityEvidence :: Int -> String -> String -> (String -> Bool) -> Maybe SourceLineQualityEvidence
firstLineQualityEvidence lineLimit original replacement linePredicate =
  evidenceFromLine
    <$> find
      lineOffends
      (zip [1 ..] (lines replacement))
  where
    originalMaxLineLength =
      maximumLineLength original
    lineOffends (_, lineValue) =
      length lineValue > lineLimit
        && length lineValue > originalMaxLineLength
        && linePredicate lineValue
    evidenceFromLine (lineNumber, lineValue) =
      SourceLineQualityEvidence
        { slqeLineLimit = lineLimit,
          slqeLineNumber = lineNumber,
          slqeOriginalMaxLineLength = originalMaxLineLength,
          slqeReplacementLineLength = length lineValue,
          slqeReplacementLinePreview = take sourceQualityLinePreviewLimit lineValue
        }

maximumLineLength :: String -> Int
maximumLineLength =
  foldr (max . length) 0 . lines

isInlineListLayoutLine :: String -> Bool
isInlineListLayoutLine lineValue =
  "[" `isInfixOf` lineValue
    && "," `isInfixOf` lineValue
    && "]" `isInfixOf` lineValue

isInlineConsPatternLine :: String -> Bool
isInlineConsPatternLine lineValue =
  infixOccurrenceCount " : " lineValue >= 3
    && (") :" `isInfixOf` lineValue || "((" `isInfixOf` lineValue || "->" `isInfixOf` lineValue)

infixOccurrenceCount :: String -> String -> Int
infixOccurrenceCount needle =
  length . filter (needle `isPrefixOf`) . tails

readableGeneratedLineLimit :: Int
readableGeneratedLineLimit =
  100

inlineListLineLimit :: Int
inlineListLineLimit =
  80

inlineConsPatternLineLimit :: Int
inlineConsPatternLineLimit =
  80

sourceQualityLinePreviewLimit :: Int
sourceQualityLinePreviewLimit =
  160

lineOnlyMinificationRefusal :: String -> String -> Maybe SourceQualityRefusal
lineOnlyMinificationRefusal original replacement
  | replacementLineCount < originalLineCount && replacementByteCount > originalByteCount =
      Just
        ( SourceQualityLineOnlyMinification
            LineOnlyMinificationEvidence
              { lomOriginalLines = originalLineCount,
                lomReplacementLines = replacementLineCount,
                lomOriginalBytes = originalByteCount,
                lomReplacementBytes = replacementByteCount
              }
        )
  | otherwise =
      Nothing
  where
    originalLineCount =
      length (lines original)
    replacementLineCount =
      length (lines replacement)
    originalByteCount =
      length original
    replacementByteCount =
      length replacement

globalReferenceNames :: Fix HsExprF -> Set String
globalReferenceNames (Fix nodeValue) =
  (
    case nodeValue of
      VarF (GlobalName rdrName) -> Set.singleton (occNameString (rdrNameOcc rdrName))
      _ -> Set.empty
  ) <> foldMap globalReferenceNames nodeValue

patchedModuleSource :: ModulePatch -> String -> Either NebulaError String
patchedModuleSource modulePatch source = do
  spliced <- applySplices (mpSplices modulePatch) source
  pure (appendDefinitions spliced (mpAppendedDefinitions modulePatch))

modulePatchHasContent :: ModulePatch -> Bool
modulePatchHasContent modulePatch =
  not (null (mpSplices modulePatch) && null (mpAppendedDefinitions modulePatch))

appendDefinitions :: String -> [AppendedDefinition] -> String
appendDefinitions spliced = \case
  [] ->
    spliced
  definitions ->
    spliced
      <> (if "\n" `isSuffixOf` spliced then "" else "\n")
      <> "\n"
      <> intercalate "\n\n" (fmap adSource definitions)
      <> "\n"
