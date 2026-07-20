module Melusine.Nebula.Spec.WriteBackSpec (spec) where

import Data.List (isInfixOf, zipWith5)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import GHC.Types.Name.Occurrence (mkVarOcc)
import GHC.Types.Name.Reader (mkRdrUnqual)
import Melusine.Nebula
  ( AppendedDefinition (..),
    HunkBlockReason (..),
    HunkDisposition (..),
    LineOnlyMinificationEvidence (..),
    ModuleImprovement (..),
    ModulePatch (..),
    ModuleReport (..),
    ModuleWorkload (..),
    NebulaError (..),
    SealOutcome (..),
    SourceLineQualityEvidence (..),
    SourceQualityRefusal (..),
    WriteBackRefusal (..),
    defaultNebulaConfig,
    improveModule,
    modulePatchHasContent,
    patchedModuleSource,
    renderModuleDiff,
    renderModuleReport,
    sealModulePatch,
    sealPatchedSourceParseCount,
    sealedSourceText,
  )
import Melusine.Nebula.Discovery.Choose (ChosenBinding (..))
import Melusine.Nebula.Rewrite.Corpus (deriveRuleCorpus)
import Melusine.Nebula.Source.Ast (RecordConstruction (..), RecordConstructionField (..), RecordFieldValue (..), locatedRecordConstructions)
import Melusine.Nebula.Source.Ingest (IngestedModule (..), ingestModule)
import Melusine.Nebula.Write.Patch (SourceSplice (..), applySplices)
import Melusine.Nebula.Write.Seal (sealModulePatchOutcome)
import Melusine.Nebula.Rewrite.Saturate (SaturatedModule, defaultSaturationOptions, saturateModule)
import Melusine.Nebula.Synthesis.Core (PlanStagingReport (..), SynthesizedDefinition (..), SynthesizedName (..), SynthesisOutcome (..))
import Melusine.Nebula.Write.Back (planWriteBack)
import Melusine.Nebula.Write.Declaration
  ( DeclarationPatch (..),
    RecordDeclaration (..),
    RecordSelectorRewrite (..),
    planRecordFieldDeletion,
    planRecordOwnershipRewrite,
    recordDeclarations,
    sealDeclarationPatch,
  )
import Moonlight.Core (Pattern (..))
import Moonlight.EGraph.Introspection.Core.HsExpr (ConvertedModule (..), HsExprF (..), HsPatF (..), HsVarRef (..), RenderRefusal (..), SourceRegion (..), SpannedExpr (..), TopLevelBinding (..))
import Data.Fix (Fix (..))
import Moonlight.Pale.Ghc.Hie.SourceKey (OracleLookup (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

spec :: TestTree
spec =
  testGroup
    "nebula.writeback"
    [ planCases,
      spliceCases,
      declarationCases,
      sealCases,
      sealFirstCases,
      idempotentCases
    ]

writeBackWorkload :: ModuleWorkload
writeBackWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/WriteBackFixture.hs",
      mwSource =
        unlines
          [ "{-# LANGUAGE Arrows #-}",
            "module Melusine.Nebula.WriteBackFixture where",
            "",
            "-- a comment that must survive the splice",
            "shadow = let g = \\x -> use x x in g alpha",
            "",
            "still = alpha",
            "",
            "opaqueBoth = proc x -> useArrow -< x"
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

guardedBindingWorkload :: ModuleWorkload
guardedBindingWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/GuardedBindingFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.GuardedBindingFixture where",
            "",
            "guarded x",
            "  | Just y <- lookupThing x = y",
            "  | otherwise = fallback x"
          ],
      mwOracleLookup = OracleMissing []
    }

whereBindingWorkload :: ModuleWorkload
whereBindingWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/WhereBindingFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.WhereBindingFixture where",
            "",
            "shadowWhere x = use y y where y = project x",
            "shadowSibling x = use (project x) (project x)"
          ],
      mwOracleLookup = OracleMissing []
    }

patternWhereBindingWorkload :: ModuleWorkload
patternWhereBindingWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/PatternWhereBindingFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.PatternWhereBindingFixture where",
            "",
            "patternWhere x = combine y y where",
            "  (y, kept) = splitPair x"
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
            "multi n = (\\x -> use x x) alpha",
            "multi m = keep m"
          ],
      mwOracleLookup = OracleMissing []
    }

lineOnlyMinifierWorkload :: ModuleWorkload
lineOnlyMinifierWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/LineOnlyMinifierFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.LineOnlyMinifierFixture where",
            "",
            "minify =",
            "  x"
          ],
      mwOracleLookup = OracleMissing []
    }

lineOnlyMinifierReplacementWorkload :: ModuleWorkload
lineOnlyMinifierReplacementWorkload =
  withoutOracleWorkload
    "Melusine/Nebula/LineOnlyMinifierFixture.hs"
    ( unlines
        [ "module Melusine.Nebula.LineOnlyMinifierFixture where",
          "",
          "minify = veryVeryVeryVeryLongFunctionNameDesignedToBeWorseThanTheOriginal x"
        ]
    )

mixedQualityWorkload :: ModuleWorkload
mixedQualityWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/MixedQualityFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.MixedQualityFixture where",
            "",
            "good = let g = \\x -> use x x in g alpha",
            "",
            "minify =",
            "  x"
          ],
      mwOracleLookup = OracleMissing []
    }

mixedQualityReplacementWorkload :: ModuleWorkload
mixedQualityReplacementWorkload =
  withoutOracleWorkload
    "Melusine/Nebula/MixedQualityFixture.hs"
    ( unlines
        [ "module Melusine.Nebula.MixedQualityFixture where",
          "",
          "good = use alpha alpha",
          "",
          "minify = veryVeryVeryVeryLongFunctionNameDesignedToBeWorseThanTheOriginal x"
        ]
    )

overlongGeneratedLineWorkload :: ModuleWorkload
overlongGeneratedLineWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/OverlongGeneratedLineFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.OverlongGeneratedLineFixture where",
            "",
            "longLine =",
            "  alpha"
          ],
      mwOracleLookup = OracleMissing []
    }

overlongGeneratedLineReplacementWorkload :: ModuleWorkload
overlongGeneratedLineReplacementWorkload =
  withoutOracleWorkload
    "Melusine/Nebula/OverlongGeneratedLineFixture.hs"
    ( unlines
        [ "module Melusine.Nebula.OverlongGeneratedLineFixture where",
          "",
          "longLine = veryVeryVeryLongFunctionNameDesignedToBeWorseThanTheOriginal alpha beta gamma delta epsilon zeta eta theta iota kappa"
        ]
    )

appendedDefinitionQualityWorkload :: ModuleWorkload
appendedDefinitionQualityWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/AppendedDefinitionQualityFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.AppendedDefinitionQualityFixture where",
            "",
            "target = alpha"
          ],
      mwOracleLookup = OracleMissing []
    }

consPatternLayoutWorkload :: ModuleWorkload
consPatternLayoutWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/ConsPatternLayoutFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.ConsPatternLayoutFixture where",
            "",
            "decode raw =",
            "  case raw of",
            "    first : second : third : fourth : fifth -> consume first second third fourth fifth",
            "    _ -> fallback raw"
          ],
      mwOracleLookup = OracleMissing []
    }

consPatternLayoutReplacementWorkload :: ModuleWorkload
consPatternLayoutReplacementWorkload =
  withoutOracleWorkload
    "Melusine/Nebula/ConsPatternLayoutFixture.hs"
    ( unlines
        [ "module Melusine.Nebula.ConsPatternLayoutFixture where",
          "",
          "decode raw =",
          "  case raw of",
          "    ((((firstComponent : secondComponent) : thirdComponent) : fourthComponent) : fifthComponent) -> consume firstComponent secondComponent thirdComponent fourthComponent fifthComponent",
          "    _ -> fallback raw"
        ]
    )

synthesisWorkload :: ModuleWorkload
synthesisWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/SynthesisFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.SynthesisFixture where",
            "",
            "etaSite = \\evt -> processEvent evt",
            "letSite = let conn = getConnection in query conn",
            "shareLeft = combine (transform alpha) (transform alpha)",
            "shareRight = combine (transform beta) (transform beta)"
          ],
      mwOracleLookup = OracleMissing []
    }

multiIfWorkload :: ModuleWorkload
multiIfWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/MultiIfFixture.hs",
      mwSource =
        unlines
          [ "{-# LANGUAGE MultiWayIf #-}",
            "module Melusine.Nebula.MultiIfFixture where",
            "",
            "multiIfBinding = if | enabled -> (\\x -> use x x) alpha",
            "                    | otherwise -> fallback enabled"
          ],
      mwOracleLookup = OracleMissing []
    }

recordPatternWorkload :: ModuleWorkload
recordPatternWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/RecordPatternFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.RecordPatternFixture where",
            "",
            "data Box = Box { payload :: Int } | Empty { emptyTag :: Int }",
            "",
            "recordCase box = case box of",
            "  Box { payload = value } -> combine value",
            "  Empty {} -> (\\x -> use x x) alpha"
          ],
      mwOracleLookup = OracleMissing []
    }

typedSignatureWorkload :: ModuleWorkload
typedSignatureWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/TypedSignatureFixture.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.TypedSignatureFixture where",
            "",
            "typedSignature = ((\\x -> use x x) alpha :: Int)"
          ],
      mwOracleLookup = OracleMissing []
    }

requireImproved :: ModuleWorkload -> IO ModuleImprovement
requireImproved workload =
  either
    (\(modulePath, moduleFailure) -> assertFailure ("improve failed for " <> modulePath <> ": " <> show moduleFailure))
    pure
    (improveModule defaultNebulaConfig workload)

withoutOracleWorkload :: FilePath -> String -> ModuleWorkload
withoutOracleWorkload modulePath sourceText =
  ModuleWorkload
    { mwPath = modulePath,
      mwSource = sourceText,
      mwOracleLookup = OracleMissing []
    }

rewrittenMultiClauseTerm :: IngestedModule -> IO (Fix HsExprF)
rewrittenMultiClauseTerm ingested =
  case cmBindings (imConverted ingested) of
    [bindingValue] ->
      case spannedFix (tlbSpannedTerm bindingValue) of
        Fix (ClausesF [firstClause, (changedPatterns, _), finalClause]) ->
          pure (Fix (ClausesF [firstClause, (changedPatterns, useAlphaAlpha), finalClause]))
        otherTerm ->
          assertFailure ("expected a three-clause ClausesF binding, got: " <> show (fixNodeCount otherTerm) <> " nodes")
    bindings ->
      assertFailure ("expected exactly one converted binding, got " <> show (length bindings))

syntheticOutcomeForSingleBinding :: IngestedModule -> Fix HsExprF -> IO SynthesisOutcome
syntheticOutcomeForSingleBinding ingested replacementTerm = do
  saturated <- requireSyntheticSaturated ingested
  case (imBindingNames ingested, imBindingContexts ingested, imSeedClasses ingested, imOriginalSizes ingested) of
    ([bindingName], [bindingContext], [seedClass], [originalSize]) ->
      let replacementSize = fixNodeCount replacementTerm
       in pure
            SynthesisOutcome
              { soDefinitions = [],
                soEstimatedWin = originalSize - replacementSize,
                soRealizedWin = originalSize - replacementSize,
                soPreExtractedTotal = originalSize,
                soPostExtractedTotal = replacementSize,
                soRejected = [],
                soStagingReport =
                  PlanStagingReport
                    { psrLocalizedMerges = 0,
                      psrGlobalFallbackMerges = 0,
                      psrLocalizedDefinitionMerges = 0,
                      psrLocalizedApplicationMerges = 0,
                      psrGlobalDefinitionFallbackMerges = 0,
                      psrGlobalApplicationFallbackMerges = 0,
                      psrDirtyContextCount = 0
                    },
                soHarvestDecision = Nothing,
                soBindings =
                  [ ChosenBinding
                      { cbName = bindingName,
                        cbContext = bindingContext,
                        cbSeedClass = seedClass,
                        cbOriginalSize = originalSize,
                        cbTerm = replacementTerm,
                        cbExtractedSize = replacementSize,
                        cbExtractionCost = replacementSize,
                        cbSignature = mempty
                      }
                  ],
                soSaturatedModule = saturated
              }
    _ ->
      assertFailure "expected exactly one ingested binding row"

syntheticOutcomeForBindingReplacements :: IngestedModule -> [(String, Fix HsExprF)] -> IO SynthesisOutcome
syntheticOutcomeForBindingReplacements ingested replacements = do
  saturated <- requireSyntheticSaturated ingested
  case (imBindingNames ingested, imBindingContexts ingested, imSeedClasses ingested, imOriginalSizes ingested, cmBindings (imConverted ingested)) of
    (bindingNames, bindingContexts, seedClasses, originalSizes, bindings)
      | equalLengths [length bindingNames, length bindingContexts, length seedClasses, length originalSizes, length bindings] ->
          let replacementTerm bindingName bindingValue =
                fromMaybe (spannedFix (tlbSpannedTerm bindingValue)) (lookup bindingName replacements)
              replacementTerms =
                zipWith replacementTerm bindingNames bindings
              replacementSizes =
                fmap fixNodeCount replacementTerms
              originalTotal =
                sum originalSizes
              replacementTotal =
                sum replacementSizes
           in pure
                SynthesisOutcome
                  { soDefinitions = [],
                    soEstimatedWin = originalTotal - replacementTotal,
                    soRealizedWin = originalTotal - replacementTotal,
                    soPreExtractedTotal = originalTotal,
                    soPostExtractedTotal = replacementTotal,
                    soRejected = [],
                    soStagingReport =
                      PlanStagingReport
                        { psrLocalizedMerges = 0,
                          psrGlobalFallbackMerges = 0,
                          psrLocalizedDefinitionMerges = 0,
                          psrLocalizedApplicationMerges = 0,
                          psrGlobalDefinitionFallbackMerges = 0,
                          psrGlobalApplicationFallbackMerges = 0,
                          psrDirtyContextCount = 0
                        },
                    soHarvestDecision = Nothing,
                    soBindings =
                      zipWith5
                        ( \bindingName bindingContext seedClass originalSize replacementTermValue ->
                            ChosenBinding
                              { cbName = bindingName,
                                cbContext = bindingContext,
                                cbSeedClass = seedClass,
                                cbOriginalSize = originalSize,
                                cbTerm = replacementTermValue,
                                cbExtractedSize = fixNodeCount replacementTermValue,
                                cbExtractionCost = fixNodeCount replacementTermValue,
                                cbSignature = mempty
                              }
                        )
                        bindingNames
                        bindingContexts
                        seedClasses
                        originalSizes
                        replacementTerms,
                    soSaturatedModule = saturated
                  }
    _ ->
      assertFailure "expected aligned ingested binding rows"

requireSyntheticSaturated :: IngestedModule -> IO SaturatedModule
requireSyntheticSaturated ingested = do
  corpus <-
    requireRight
      "synthetic outcome corpus derivation"
      (deriveRuleCorpus defaultNebulaConfig (imSpanRows ingested) Nothing (imConverted ingested))
  requireRight
    "synthetic outcome saturation"
    (saturateModule defaultSaturationOptions defaultNebulaConfig ingested corpus)

equalLengths :: [Int] -> Bool
equalLengths lengths =
  case lengths of
    [] ->
      True
    firstLength : restLengths ->
      all (== firstLength) restLengths

replacementBindingTerm :: ModuleWorkload -> String -> IO (Fix HsExprF)
replacementBindingTerm workload bindingName = do
  ingested <- requireRight ("ingesting replacement for " <> bindingName) (ingestModule workload)
  case [spannedFix (tlbSpannedTerm bindingValue) | (name, bindingValue) <- zip (imBindingNames ingested) (cmBindings (imConverted ingested)), name == bindingName] of
    [termValue] ->
      pure termValue
    terms ->
      assertFailure ("expected exactly one converted binding for " <> bindingName <> ", got " <> show (length terms))

assertLineOnlyMinificationRefusal :: Maybe WriteBackRefusal -> IO ()
assertLineOnlyMinificationRefusal = \case
  Just
    ( RefusedSourceQuality
        ( SourceQualityLineOnlyMinification
            LineOnlyMinificationEvidence
              { lomOriginalLines = originalLines,
                lomReplacementLines = replacementLines,
                lomOriginalBytes = originalBytes,
                lomReplacementBytes = replacementBytes
              }
          )
      ) -> do
      originalLines @?= 2
      replacementLines @?= 1
      assertBool "replacement must be byte-larger" (replacementBytes > originalBytes)
  otherRefusal ->
    assertFailure ("expected line-only source-quality refusal, got: " <> show otherRefusal)

assertOverlongGeneratedLineRefusal :: Maybe WriteBackRefusal -> IO ()
assertOverlongGeneratedLineRefusal =
  assertLineQualityRefusal "overlong generated line" $ \case
    SourceQualityOverlongGeneratedLine evidence ->
      Just evidence
    _ ->
      Nothing

assertInlineListLayoutRefusal :: Maybe WriteBackRefusal -> IO ()
assertInlineListLayoutRefusal =
  assertLineQualityRefusal "inline list layout" $ \case
    SourceQualityInlineListLayout evidence ->
      Just evidence
    _ ->
      Nothing

assertInlineConsPatternLayoutRefusal :: Maybe WriteBackRefusal -> IO ()
assertInlineConsPatternLayoutRefusal =
  assertLineQualityRefusal "inline cons-pattern layout" $ \case
    SourceQualityInlineConsPatternLayout evidence ->
      Just evidence
    _ ->
      Nothing

assertLineQualityRefusal ::
  String ->
  (SourceQualityRefusal -> Maybe SourceLineQualityEvidence) ->
  Maybe WriteBackRefusal ->
  IO ()
assertLineQualityRefusal label evidenceOf = \case
  Just (RefusedSourceQuality refusal)
    | Just evidence <- evidenceOf refusal -> do
        assertBool
          (label <> " line should exceed the configured limit")
          (slqeReplacementLineLength evidence > slqeLineLimit evidence)
        assertBool
          (label <> " line should be newly worse than the original")
          (slqeReplacementLineLength evidence > slqeOriginalMaxLineLength evidence)
  otherRefusal ->
    assertFailure ("expected " <> label <> " source-quality refusal, got: " <> show otherRefusal)

inlineListDefinitionTerm :: Fix HsExprF
inlineListDefinitionTerm =
  Fix
    ( ExplicitListF
        ( fmap
            globalVariable
            [ "firstVeryLongListElement",
              "secondVeryLongListElement",
              "thirdVeryLongListElement",
              "fourthVeryLongListElement",
              "fifthVeryLongListElement",
              "sixthVeryLongListElement"
            ]
        )
    )

inlineListDefinition :: IngestedModule -> IO SynthesizedDefinition
inlineListDefinition ingested =
  case imSeedClasses ingested of
    seedClass : _ ->
      pure
        SynthesizedDefinition
          { sdName = SynthesizedName "badInlineListHelper",
            sdSites = [],
            sdClass = seedClass,
            sdTerm = inlineListDefinitionTerm,
            sdSize = fixNodeCount inlineListDefinitionTerm,
            sdEstimatedWin = 1
          }
    [] ->
      assertFailure "expected at least one seed class for synthesized definition fixture"

spannedFix :: SpannedExpr -> Fix HsExprF
spannedFix spannedExpr =
  Fix (fmap spannedFix (sxNode spannedExpr))

useAlphaAlpha :: Fix HsExprF
useAlphaAlpha =
  Fix (AppF (Fix (AppF (globalVariable "use") (globalVariable "alpha"))) (globalVariable "alpha"))

globalVariable :: String -> Fix HsExprF
globalVariable variableName =
  Fix (VarF (GlobalName (mkRdrUnqual (mkVarOcc variableName))))

fixNodeCount :: Fix HsExprF -> Int
fixNodeCount (Fix layer) =
  1 + sum (fmap fixNodeCount layer)

requireRight :: String -> Either NebulaError result -> IO result
requireRight stageName =
  either
    (\stageFailure -> assertFailure (stageName <> " failed: " <> show stageFailure))
    pure

requireSealedBinding :: String -> ModuleImprovement -> IO (Fix HsExprF)
requireSealedBinding bindingName improvement = do
  case lookup bindingName (mrDispositions (miReport improvement)) of
    Just HunkSealed ->
      pure ()
    Just disposition ->
      assertFailure ("expected " <> bindingName <> " to seal, got: " <> show disposition)
    Nothing ->
      assertFailure ("missing disposition for " <> bindingName)
  case lookup bindingName (mpSpliced (miPatch improvement)) of
    Just termValue ->
      pure termValue
    Nothing ->
      assertFailure ("missing sealed splice for " <> bindingName)

singleConvertedBindingTerm :: String -> IngestedModule -> IO (Pattern HsExprF)
singleConvertedBindingTerm bindingName ingested =
  case cmBindings (imConverted ingested) of
    [bindingValue] ->
      pure (tlbTerm bindingValue)
    bindings ->
      assertFailure ("expected exactly one converted binding for " <> bindingName <> ", got " <> show (length bindings))

fixHasOpaque :: Fix HsExprF -> Bool
fixHasOpaque (Fix nodeValue) =
  case nodeValue of
    OpaqueF _ ->
      True
    _ ->
      any fixHasOpaque nodeValue

fixHasMultiIf :: Fix HsExprF -> Bool
fixHasMultiIf (Fix nodeValue) =
  case nodeValue of
    MultiIfF _ ->
      True
    _ ->
      any fixHasMultiIf nodeValue

fixHasExprWithTySig :: Fix HsExprF -> Bool
fixHasExprWithTySig (Fix nodeValue) =
  case nodeValue of
    ExprWithTySigF {} ->
      True
    _ ->
      any fixHasExprWithTySig nodeValue

patternHasExprWithTySig :: Pattern HsExprF -> Bool
patternHasExprWithTySig = \case
  PatternVar {} ->
    False
  PatternNode nodeValue ->
    case nodeValue of
      ExprWithTySigF {} ->
        True
      _ ->
        any patternHasExprWithTySig nodeValue

patternHasCaseRecordPattern :: Pattern HsExprF -> Bool
patternHasCaseRecordPattern = \case
  PatternVar {} ->
    False
  PatternNode nodeValue ->
    ownCaseRecordPattern nodeValue || any patternHasCaseRecordPattern nodeValue
  where
    ownCaseRecordPattern :: HsExprF (Pattern HsExprF) -> Bool
    ownCaseRecordPattern = \case
      CaseF _ alternatives ->
        any (patternHasRecord . fst) alternatives
      _ ->
        False

patternHasCaseEmptyRecordPattern :: Pattern HsExprF -> Bool
patternHasCaseEmptyRecordPattern = \case
  PatternVar {} ->
    False
  PatternNode nodeValue ->
    ownCaseEmptyRecordPattern nodeValue || any patternHasCaseEmptyRecordPattern nodeValue
  where
    ownCaseEmptyRecordPattern :: HsExprF (Pattern HsExprF) -> Bool
    ownCaseEmptyRecordPattern = \case
      CaseF _ alternatives ->
        any (patternHasEmptyRecord . fst) alternatives
      _ ->
        False

patternHasRecord :: HsPatF -> Bool
patternHasRecord = \case
  PRecP {} ->
    True
  PConP _ subPatterns ->
    any patternHasRecord subPatterns
  PTupleP subPatterns ->
    any patternHasRecord subPatterns
  PListP subPatterns ->
    any patternHasRecord subPatterns
  PAsP _ subPattern ->
    patternHasRecord subPattern
  PBangP subPattern ->
    patternHasRecord subPattern
  PLazyP subPattern ->
    patternHasRecord subPattern
  PParP subPattern ->
    patternHasRecord subPattern
  _ ->
    False

patternHasEmptyRecord :: HsPatF -> Bool
patternHasEmptyRecord = \case
  PRecP _ [] ->
    True
  PRecP _ fieldPatterns ->
    any (patternHasEmptyRecord . snd) fieldPatterns
  PConP _ subPatterns ->
    any patternHasEmptyRecord subPatterns
  PTupleP subPatterns ->
    any patternHasEmptyRecord subPatterns
  PListP subPatterns ->
    any patternHasEmptyRecord subPatterns
  PAsP _ subPattern ->
    patternHasEmptyRecord subPattern
  PBangP subPattern ->
    patternHasEmptyRecord subPattern
  PLazyP subPattern ->
    patternHasEmptyRecord subPattern
  PParP subPattern ->
    patternHasEmptyRecord subPattern
  _ ->
    False

planCases :: TestTree
planCases =
  testGroup
    "nebula.writeback.plan"
    [ testCase "the eligibility partition is exact and every skip carries its reason" $ do
        improvement <- requireImproved writeBackWorkload
        let modulePatch = miPatch improvement
        mpPath modulePatch @?= mwPath writeBackWorkload
        fmap fst (mpSpliced modulePatch) @?= ["shadow"]
        length (mpSplices modulePatch) @?= 1
        case mpSkipped modulePatch of
          [(stillName, RefusedUnchanged), (opaqueName, RefusedRender (RenderOpaque _))] -> do
            stillName @?= "still"
            opaqueName @?= "opaqueBoth"
          otherPartition ->
            assertFailure ("unexpected skip partition: " <> show otherPartition)
        case mrDispositions (miReport improvement) of
          [ ("shadow", HunkSealed),
            ("still", HunkBlocked (BlockedWriteBack RefusedUnchanged)),
            ("opaqueBoth", HunkBlocked (BlockedWriteBack (RefusedRender (RenderOpaque _))))
            ] ->
              pure ()
          otherDispositions ->
            assertFailure ("unexpected disposition partition: " <> show otherDispositions)
        assertBool
          "a module without shared structure appends no definitions"
          (null (mpAppendedDefinitions modulePatch)),
      testCase "a constructor-pattern case is no longer blocked by the render path" $ do
        improvement <- requireImproved constructorCaseWorkload
        let modulePatch = miPatch improvement
            classifyDisposition =
              lookup "classify" (mrDispositions (miReport improvement))
        assertBool
          "the constructor-pattern binding is not refused by rendering"
          ( case classifyDisposition of
              Just (HunkBlocked (BlockedWriteBack (RefusedRender _))) -> False
              _ -> True
          )
        assertBool
          "the constructor-pattern binding either splices or is left unchanged"
          ( "classify" `elem` fmap fst (mpSpliced modulePatch)
              || (lookup "classify" (mpSkipped modulePatch) == Just RefusedUnchanged)
          ),
      testCase "a guarded binding is no longer blocked by the render path" $ do
        improvement <- requireImproved guardedBindingWorkload
        case lookup "guarded" (mrDispositions (miReport improvement)) of
          Just (HunkBlocked (BlockedWriteBack (RefusedRender refusal))) ->
            assertFailure ("guarded binding was refused by rendering: " <> show refusal)
          Just _ ->
            pure ()
          Nothing ->
            assertFailure "guarded binding was missing from the writeback report",
      testCase "a where binding is no longer blocked by the render path" $ do
        improvement <- requireImproved whereBindingWorkload
        case lookup "shadowWhere" (mrDispositions (miReport improvement)) of
          Just (HunkBlocked (BlockedWriteBack (RefusedRender refusal))) ->
            assertFailure ("where binding was refused by rendering: " <> show refusal)
          Just _ ->
            pure ()
          Nothing ->
            assertFailure "where binding was missing from the writeback report",
      testCase "a tuple-pattern where binding is no longer blocked by the render path" $ do
        improvement <- requireImproved patternWhereBindingWorkload
        case lookup "patternWhere" (mrDispositions (miReport improvement)) of
          Just (HunkBlocked (BlockedWriteBack (RefusedRender refusal))) ->
            assertFailure ("tuple-pattern where binding was refused by rendering: " <> show refusal)
          Just _ ->
            pure ()
          Nothing ->
            assertFailure "tuple-pattern where binding was missing from the writeback report",
      testCase "line-only minification is refused as source-quality sludge" $ do
        original <- requireRight "ingesting minifier workload" (ingestModule lineOnlyMinifierWorkload)
        replacement <- replacementBindingTerm lineOnlyMinifierReplacementWorkload "minify"
        outcome <- syntheticOutcomeForSingleBinding original replacement
        modulePatch <- requireRight "writeback planning" (planWriteBack lineOnlyMinifierWorkload original outcome)
        assertBool "minifier candidate writes no splice" (null (mpSplices modulePatch))
        assertLineOnlyMinificationRefusal (lookup "minify" (mpSkipped modulePatch)),
      testCase "source-quality refusal is per group and does not poison clean sibling splices" $ do
        original <- requireRight "ingesting mixed workload" (ingestModule mixedQualityWorkload)
        replacement <- replacementBindingTerm mixedQualityReplacementWorkload "minify"
        outcome <- syntheticOutcomeForBindingReplacements original [("good", useAlphaAlpha), ("minify", replacement)]
        modulePatch <- requireRight "writeback planning" (planWriteBack mixedQualityWorkload original outcome)
        assertBool "clean sibling splice survives" ("good" `elem` fmap fst (mpSpliced modulePatch))
        assertLineOnlyMinificationRefusal (lookup "minify" (mpSkipped modulePatch)),
      testCase "overlong generated lines are refused before seal theater" $ do
        original <- requireRight "ingesting overlong-line workload" (ingestModule overlongGeneratedLineWorkload)
        replacement <- replacementBindingTerm overlongGeneratedLineReplacementWorkload "longLine"
        outcome <- syntheticOutcomeForSingleBinding original replacement
        modulePatch <- requireRight "writeback planning" (planWriteBack overlongGeneratedLineWorkload original outcome)
        assertBool "overlong candidate writes no splice" (null (mpSplices modulePatch))
        assertOverlongGeneratedLineRefusal (lookup "longLine" (mpSkipped modulePatch)),
      testCase "inline list helper definitions block their referencing splices" $ do
        original <- requireRight "ingesting appended-definition quality workload" (ingestModule appendedDefinitionQualityWorkload)
        outcomeBase <- syntheticOutcomeForSingleBinding original (globalVariable "badInlineListHelper")
        badDefinition <- inlineListDefinition original
        let outcome =
              outcomeBase
                { soDefinitions = [badDefinition]
                }
        modulePatch <- requireRight "writeback planning" (planWriteBack appendedDefinitionQualityWorkload original outcome)
        assertBool "bad helper writes no splice" (null (mpSplices modulePatch))
        assertBool "bad helper is not appended" (null (mpAppendedDefinitions modulePatch))
        assertInlineListLayoutRefusal (lookup "badInlineListHelper" (mpSkipped modulePatch))
        assertInlineListLayoutRefusal (lookup "target" (mpSkipped modulePatch)),
      testCase "inline cons-pattern spaghetti is refused before writeback" $ do
        original <- requireRight "ingesting cons-pattern workload" (ingestModule consPatternLayoutWorkload)
        replacement <- replacementBindingTerm consPatternLayoutReplacementWorkload "decode"
        outcome <- syntheticOutcomeForSingleBinding original replacement
        modulePatch <- requireRight "writeback planning" (planWriteBack consPatternLayoutWorkload original outcome)
        assertBool "cons-pattern candidate writes no splice" (null (mpSplices modulePatch))
        assertInlineConsPatternLayoutRefusal (lookup "decode" (mpSkipped modulePatch)),
      testCase "the diff is read straight off the patch plan" $ do
        improvement <- requireImproved writeBackWorkload
        let diffLines = renderModuleDiff (mwPath writeBackWorkload) (mwSource writeBackWorkload) (miPatch improvement) (miSeal improvement)
        assertBool
          "the hunk removes the original shadow binding"
          ("-shadow = let g = \\x -> use x x in g alpha" `elem` diffLines)
        assertBool
          "the hunk adds the contracted form"
          ("+shadow = use alpha alpha" `elem` diffLines)
        assertBool
          "untouched bindings produce no hunks"
          (not (any ("still" `isInfixOf`) diffLines))
    ]

spliceCases :: TestTree
spliceCases =
  testGroup
    "nebula.writeback.splice"
    [ testCase "offsets resolve through the line table with end-exclusive columns" $ do
        let source = unlines ["alpha", "beta", "gamma"]
        applySplices [SourceSplice (SourceRegion 2 1 2 5) "BETA"] source
          @?= Right (unlines ["alpha", "BETA", "gamma"])
        applySplices [SourceSplice (SourceRegion 1 1 2 5) "delta"] source
          @?= Right (unlines ["delta", "gamma"]),
      testCase "overlapping regions are rejected into the error channel" $ do
        let source = unlines ["alpha", "beta", "gamma"]
            overlapping =
              [ SourceSplice (SourceRegion 1 1 2 5) "one",
                SourceSplice (SourceRegion 2 1 3 6) "two"
              ]
        case applySplices overlapping source of
          Left (NebulaSpliceError _) ->
            pure ()
          Left otherFailure ->
            assertFailure ("expected a splice error, got: " <> show otherFailure)
          Right spliced ->
            assertFailure ("expected a splice error, got spliced text: " <> spliced),
      testCase "positions outside the source are rejected into the error channel" $
        case applySplices [SourceSplice (SourceRegion 99 1 99 2) "ghost"] (unlines ["alpha"]) of
          Left (NebulaSpliceError _) ->
            pure ()
          Left otherFailure ->
            assertFailure ("expected a splice error, got: " <> show otherFailure)
          Right spliced ->
            assertFailure ("expected a splice error, got spliced text: " <> spliced),
      testCase "non-binding text survives byte-identically outside the spliced regions" $ do
        improvement <- requireImproved writeBackWorkload
        patched <-
          requireRight "patching" (patchedModuleSource (miPatch improvement) (mwSource writeBackWorkload))
        let patchedLines = lines patched
        take 3 patchedLines
          @?= [ "{-# LANGUAGE Arrows #-}",
                "module Melusine.Nebula.WriteBackFixture where",
                ""
              ]
        take 4 patchedLines
          @?= [ "{-# LANGUAGE Arrows #-}",
                "module Melusine.Nebula.WriteBackFixture where",
                "",
                "-- a comment that must survive the splice"
              ]
        assertBool "the unchanged binding survives byte-identically" ("still = alpha" `elem` patchedLines)
        assertBool
          "the opaque binding survives byte-identically"
          ("opaqueBoth = proc x -> useArrow -< x" `elem` patchedLines)
        assertBool "the contracted binding replaced its region" ("shadow = use alpha alpha" `elem` patchedLines)
        assertBool "the original shadow text is gone" (not (any ("let g" `isInfixOf`) patchedLines)),
      testCase "multi-clause writeback splices only the changed clause body" $ do
        ingested <- requireRight "ingesting multi-clause workload" (ingestModule multiClauseWorkload)
        replacementTerm <- rewrittenMultiClauseTerm ingested
        outcome <- syntheticOutcomeForSingleBinding ingested replacementTerm
        modulePatch <- requireRight "writeback planning" (planWriteBack multiClauseWorkload ingested outcome)
        case mpSplices modulePatch of
          [] ->
            assertFailure "expected a clause-body splice"
          splices ->
            assertBool
              "the splice region is confined to the reducible clause body line"
              (all ((== 4) . srStartLine . ssRegion) splices && all ((== 4) . srEndLine . ssRegion) splices)
        patched <-
          requireRight "patching" (patchedModuleSource modulePatch (mwSource multiClauseWorkload))
        sealed <-
          requireRight
            "sealing"
            (sealModulePatch (mwPath multiClauseWorkload) (mwSource multiClauseWorkload) modulePatch)
        sealedSourceText sealed @?= patched
        let patchedLines = lines patched
        assertBool "the first clause survives byte-identically" ("multi 0 = zero" `elem` patchedLines)
        assertBool "the unchanged trailing clause survives byte-identically" ("multi m = keep m" `elem` patchedLines)
        assertBool "the reducible clause body is spliced" ("multi n = use alpha alpha" `elem` patchedLines)
        assertBool "the original reducible body is gone" (not ("multi n = (\\x -> use x x) alpha" `elem` patchedLines))
    ]

declarationCases :: TestTree
declarationCases =
  testGroup
    "nebula.writeback.declaration"
    [ testCase "record field deletion is planned from declaration spans and sealed by reparsed field sets" $ do
        patch <-
          requireRight
            "declaration field deletion planning"
            ( planRecordFieldDeletion
                (mwPath privateRecordDeclarationWorkload)
                (mwSource privateRecordDeclarationWorkload)
                "PrivateCandidate"
                (Set.fromList ["cachedName", "cachedClass"])
            )
        length (dpSplices patch) @?= 2
        patched <-
          requireRight
            "declaration patch sealing"
            (sealDeclarationPatch (mwPath privateRecordDeclarationWorkload) (mwSource privateRecordDeclarationWorkload) patch)
        let patchedLines = lines patched
        assertBool "kept owner field survives" (any ("pcSite :: CandidateSite" `isInfixOf`) patchedLines)
        assertBool "kept result field survives" (any ("pcResult :: Result" `isInfixOf`) patchedLines)
        assertBool "cachedName line is deleted" (not (any ("cachedName" `isInfixOf`) patchedLines))
        assertBool "cachedClass line is deleted" (not (any ("cachedClass" `isInfixOf`) patchedLines))
        declarations <-
          requireRight
            "re-reading declaration records"
            (recordDeclarations (mwPath privateRecordDeclarationWorkload) patched)
        fmap rdTypeName declarations @?= ["PrivateCandidate"],
      testCase "record ownership rewrite deletes constructor fields and rewrites selector applications" $ do
        patch <-
          requireRight
            "record ownership rewrite planning"
            ( planRecordOwnershipRewrite
                (mwPath privateRecordOwnershipWorkload)
                (mwSource privateRecordOwnershipWorkload)
                "PrivateCandidate"
                (Set.fromList ["cachedName", "cachedClass"])
                [ RecordSelectorRewrite "cachedName" "pcSite" "siteName",
                  RecordSelectorRewrite "cachedClass" "pcSite" "siteClass"
                ]
            )
        patched <-
          requireRight
            "record ownership rewrite sealing"
            (sealDeclarationPatch (mwPath privateRecordOwnershipWorkload) (mwSource privateRecordOwnershipWorkload) patch)
        let patchedLines = lines patched
        assertBool "cachedName is gone from declaration, constructors, and selector uses" (not (any ("cachedName" `isInfixOf`) patchedLines))
        assertBool "cachedClass is gone from declaration, constructors, and selector uses" (not (any ("cachedClass" `isInfixOf`) patchedLines))
        assertBool "constructor kept owner field remains" (any ("pcSite = site" `isInfixOf`) patchedLines)
        assertBool "selector use routes through the owner field" (any ("siteName (pcSite (candidate))" `isInfixOf`) patchedLines)
        assertBool "second selector use routes through the owner field" (any ("siteClass (pcSite (candidate))" `isInfixOf`) patchedLines)
        assertBool "selector use inside an instance method routes through the owner field" (any ("show (siteName (pcSite (candidate)))" `isInfixOf`) patchedLines),
      testCase "record ownership rewrite removes trailing commas before a deleted field suffix" $ do
        patch <-
          requireRight
            "trailing-comma record ownership rewrite planning"
            ( planRecordOwnershipRewrite
                (mwPath trailingCommaRecordOwnershipWorkload)
                (mwSource trailingCommaRecordOwnershipWorkload)
                "PrivateCandidate"
                (Set.fromList ["cachedName", "cachedClass"])
                [ RecordSelectorRewrite "cachedName" "pcSite" "siteName",
                  RecordSelectorRewrite "cachedClass" "pcSite" "Qualified.siteClass"
                ]
            )
        constructions <-
          requireRight
            "reading qualified record projection"
            (locatedRecordConstructions (mwPath trailingCommaRecordOwnershipWorkload) (mwSource trailingCommaRecordOwnershipWorkload))
        assertBool
          "record construction discovery preserves qualified projection names"
          ( any
              ((== RecordFieldProjection "Qualified.siteClass" "site") . rcfValue)
              (foldMap rcFieldRows constructions)
          )
        patched <-
          requireRight
            "trailing-comma record ownership rewrite sealing"
            (sealDeclarationPatch (mwPath trailingCommaRecordOwnershipWorkload) (mwSource trailingCommaRecordOwnershipWorkload) patch)
        let patchedLines = lines patched
        assertBool "the retained declaration field has no dangling comma" ("    pcSite :: CandidateSite" `elem` patchedLines)
        assertBool "the retained constructor field has no dangling comma" ("      pcSite = site" `elem` patchedLines)
        assertBool "deleted declaration and constructor fields are absent" (not (any ("cachedName" `isInfixOf`) patchedLines) && not (any ("cachedClass" `isInfixOf`) patchedLines))
        assertBool "selector use routes through the retained owner" (any ("siteName (pcSite (candidate))" `isInfixOf`) patchedLines)
        assertBool "selector use inside an instance method routes through the retained owner" (any ("show (Qualified.siteClass (pcSite (candidate)))" `isInfixOf`) patchedLines),
      testCase "record ownership diagnostics feed the normal sealed module writeback" $ do
        improvement <- requireImproved privateRecordOwnershipWorkload
        assertBool
          "the normal module patch carries a declaration splice group"
          (not (null (mpDeclarationSpliceGroups (miPatch improvement))))
        case miSeal improvement of
          Sealed sealedSource -> do
            let patchedLines = lines (sealedSourceText sealedSource)
            assertBool "cachedName is erased by the normal writeback path" (not (any ("cachedName" `isInfixOf`) patchedLines))
            assertBool "cachedClass is erased by the normal writeback path" (not (any ("cachedClass" `isInfixOf`) patchedLines))
            assertBool "normal writeback reroutes selector use" (any ("siteName (pcSite (candidate))" `isInfixOf`) patchedLines)
            assertBool "normal writeback reroutes second selector use" (any ("siteClass (pcSite (candidate))" `isInfixOf`) patchedLines)
          otherSeal ->
            assertFailure ("expected declaration-aware normal writeback to seal, got: " <> show otherSeal),
      testCase "stale derived fields feed the normal sealed declaration writeback" $ do
        improvement <- requireImproved staleDerivedFieldWriteBackWorkload
        assertBool
          "the normal module patch carries a stale-field declaration splice group"
          (not (null (mpDeclarationSpliceGroups (miPatch improvement))))
        case miSeal improvement of
          Sealed sealedSource -> do
            let patched = sealedSourceText sealedSource
                patchedLines = lines patched
            assertBool "derived field is erased from declaration, constructors, and selector uses" (not (any ("crCachedName" `isInfixOf`) patchedLines))
            assertBool "selector use routes through the owner field and projector" ("ownerName (crOwner (row))" `isInfixOf` patched)
            assertBool "payload selector use survives" ("crPayload row" `isInfixOf` patched)
          otherSeal ->
            assertFailure ("expected stale derived field writeback to seal, got: " <> show otherSeal),
      testCase "record field deletion refuses to split a multi-name declaration row" $ do
        case planRecordFieldDeletion "Melusine/Nebula/MultiFieldRow.hs" multiNameRecordDeclarationSource "Pair" (Set.singleton "leftCached") of
          Left (NebulaWriteBackError messageText) ->
            assertBool "the refusal names row splitting" ("split a multi-name row" `isInfixOf` messageText)
          Left otherFailure ->
            assertFailure ("expected declaration writeback refusal, got: " <> show otherFailure)
          Right DeclarationPatch {} ->
            assertFailure "expected declaration writeback to refuse partial multi-name row deletion"
    ]

privateRecordDeclarationWorkload :: ModuleWorkload
privateRecordDeclarationWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/PrivateRecordDeclaration.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.PrivateRecordDeclaration where",
            "",
            "data CandidateSite = CandidateSite",
            "data Result = Result",
            "",
            "data PrivateCandidate = PrivateCandidate",
            "  { pcSite :: CandidateSite",
            "  , cachedName :: String",
            "  , cachedClass :: Int",
            "  , pcResult :: Result",
            "  }"
          ],
      mwOracleLookup = OracleMissing []
    }

privateRecordOwnershipWorkload :: ModuleWorkload
privateRecordOwnershipWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/PrivateRecordOwnership.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.PrivateRecordOwnership where",
            "",
            "data CandidateSite = CandidateSite",
            "data Result = Result",
            "",
            "data PrivateCandidate = PrivateCandidate",
            "  { pcSite :: CandidateSite",
            "  , cachedName :: String",
            "  , cachedClass :: Int",
            "  , pcResult :: Result",
            "  }",
            "",
            "mkCandidate site =",
            "  PrivateCandidate",
            "    { pcSite = site",
            "    , cachedName = siteName site",
            "    , cachedClass = siteClass site",
            "    , pcResult = Result",
            "    }",
            "",
            "summarize candidate = combine (cachedName candidate) (cachedClass candidate)",
            "",
            "instance Show PrivateCandidate where",
            "  show candidate = show (cachedName candidate)"
          ],
      mwOracleLookup = OracleMissing []
    }

trailingCommaRecordOwnershipWorkload :: ModuleWorkload
trailingCommaRecordOwnershipWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/TrailingCommaRecordOwnership.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.TrailingCommaRecordOwnership where",
            "",
            "data CandidateSite = CandidateSite",
            "data Result = Result",
            "",
            "data PrivateCandidate = PrivateCandidate",
            "  { pcSite :: CandidateSite,",
            "    cachedName :: String,",
            "    cachedClass :: Int",
            "  }",
            "",
            "mkCandidate site =",
            "  PrivateCandidate",
            "    { pcSite = site,",
            "      cachedName = siteName site,",
            "      cachedClass = Qualified.siteClass site",
            "    }",
            "",
            "summarize candidate = combine (cachedName candidate) (cachedClass candidate)",
            "",
            "instance Show PrivateCandidate where",
            "  show candidate = show (cachedClass candidate)"
          ],
      mwOracleLookup = OracleMissing []
    }

staleDerivedFieldWriteBackWorkload :: ModuleWorkload
staleDerivedFieldWriteBackWorkload =
  ModuleWorkload
    { mwPath = "Melusine/Nebula/StaleDerivedFieldWriteBack.hs",
      mwSource =
        unlines
          [ "module Melusine.Nebula.StaleDerivedFieldWriteBack where",
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
            "    , crCachedName = ownerName owner",
            "    , crPayload = payload",
            "    }",
            "",
            "summarize row = combine (crCachedName row) (crPayload row)"
          ],
      mwOracleLookup = OracleMissing []
    }

multiNameRecordDeclarationSource :: String
multiNameRecordDeclarationSource =
  unlines
    [ "module Melusine.Nebula.MultiFieldRow where",
      "",
      "data Pair = Pair",
      "  { leftCached, rightCached :: Int",
      "  , kept :: Int",
      "  }"
    ]

sealCases :: TestTree
sealCases =
  testGroup
    "nebula.writeback.seal"
    [ testCase "a faithfully patched module seals before it can be emitted" $ do
        improvement <- requireImproved writeBackWorkload
        patched <-
          requireRight "patching" (patchedModuleSource (miPatch improvement) (mwSource writeBackWorkload))
        sealed <-
          requireRight
            "sealing"
            (sealModulePatch (mwPath writeBackWorkload) (mwSource writeBackWorkload) (miPatch improvement))
        sealedSourceText sealed @?= patched
        miSeal improvement @?= Sealed sealed
        sealPatchedSourceParseCount (miSeal improvement) @?= 1
        assertBool
          "the text report records the single patched-source parse"
          (any ("patched-source-parses=1" `isInfixOf`) (renderModuleReport (miReport improvement))),
      testCase "synthesized definitions append, splice, and seal together" $ do
        improvement <- requireImproved synthesisWorkload
        let modulePatch = miPatch improvement
        fmap adName (mpAppendedDefinitions modulePatch) @?= ["shareLeftRight"]
        assertBool
          "the share bindings splice through the named abstraction"
          (all (`elem` fmap fst (mpSpliced modulePatch)) ["shareLeft", "shareRight"])
        patched <-
          requireRight "patching" (patchedModuleSource modulePatch (mwSource synthesisWorkload))
        assertBool
          "the patched module carries the appended definition"
          (any ("shareLeftRight" `isInfixOf`) (lines patched))
        assertBool
          "the patched module has no generated-oatmeal helper names"
          (not ("nebulaAbs" `isInfixOf` patched))
        assertBool
          "the patched module has no generated-oatmeal parameter names"
          (not ("nebulaArg" `isInfixOf` patched))
        assertBool
          "generated helpers use layout do syntax"
          (not ("do {" `isInfixOf` patched))
        assertBool
          "generated helpers use layout let syntax"
          (not ("let {" `isInfixOf` patched))
        sealed <-
          requireRight
            "sealing"
            (sealModulePatch (mwPath synthesisWorkload) (mwSource synthesisWorkload) modulePatch)
        sealedSourceText sealed @?= patched,
      testCase "a binding whose RHS is a multi-way if seals without becoming opaque" $ do
        improvement <- requireImproved multiIfWorkload
        termValue <- requireSealedBinding "multiIfBinding" improvement
        assertBool
          "the sealed multi-way if binding stays in the structural frontier"
          (fixHasMultiIf termValue)
        assertBool
          "the sealed multi-way if binding is not opaque"
          (not (fixHasOpaque termValue)),
      testCase "record-pattern case alternatives including Con {} do not hit the render frontier" $ do
        ingested <- requireRight "ingesting record-pattern workload" (ingestModule recordPatternWorkload)
        originalTerm <- singleConvertedBindingTerm "recordCase" ingested
        assertBool
          "the record-pattern case reaches PRecP"
          (patternHasCaseRecordPattern originalTerm)
        assertBool
          "the empty record-pattern case reaches PRecP"
          (patternHasCaseEmptyRecordPattern originalTerm)
        improvement <- requireImproved recordPatternWorkload
        case lookup "recordCase" (mrDispositions (miReport improvement)) of
          Just (HunkBlocked (BlockedWriteBack (RefusedRender refusal))) ->
            assertFailure ("record-pattern binding was refused by rendering: " <> show refusal)
          Just _ ->
            pure ()
          Nothing ->
            assertFailure "record-pattern binding was missing from the writeback report",
      testCase "an expression type signature seals through a structural child frontier" $ do
        ingested <- requireRight "ingesting typed-signature workload" (ingestModule typedSignatureWorkload)
        originalTerm <- singleConvertedBindingTerm "typedSignature" ingested
        assertBool
          "the expression type signature reaches ExprWithTySigF"
          (patternHasExprWithTySig originalTerm)
        improvement <- requireImproved typedSignatureWorkload
        termValue <- requireSealedBinding "typedSignature" improvement
        assertBool
          "the sealed typed-signature binding preserves the type-signature node"
          (fixHasExprWithTySig termValue)
        assertBool
          "the sealed typed-signature binding is not opaque"
          (not (fixHasOpaque termValue)),
      testCase "a corrupted replacement refuses report and diff emission instead of leaking hunks" $ do
        improvement <- requireImproved writeBackWorkload
        let modulePatch = miPatch improvement
            corruptedPatch =
              modulePatch
                { mpSplices = fmap (\splice -> splice {ssReplacement = "shadow = use alpha beta"}) (mpSplices modulePatch)
                }
            refused =
              sealModulePatchOutcome (mwPath writeBackWorkload) (mwSource writeBackWorkload) corruptedPatch
            diffLines =
              renderModuleDiff (mwPath writeBackWorkload) (mwSource writeBackWorkload) corruptedPatch refused
        case refused of
          SealRefused _ (NebulaSealError sealName _) ->
            sealName @?= "shadow"
          SealRefused _ otherFailure ->
            assertFailure ("expected a seal error, got: " <> show otherFailure)
          otherOutcome ->
            assertFailure ("expected the corrupted patch to fail the seal, got: " <> show otherOutcome)
        assertBool "refused diffs render only the seal obstruction" (all (not . ("@@" `isInfixOf`)) diffLines)
        assertBool "the refusal is report content" (any ("seal status=refused" `isInfixOf`) (renderModuleReport ((miReport improvement) {mrSeal = refused}))),
      testCase "a patched-source parse failure records its single parse attempt" $ do
        improvement <- requireImproved writeBackWorkload
        let malformedPatch =
              (miPatch improvement)
                { mpSplices = fmap (\splice -> splice {ssReplacement = "shadow = ("}) (mpSplices (miPatch improvement))
                }
            refused =
              sealModulePatchOutcome (mwPath writeBackWorkload) (mwSource writeBackWorkload) malformedPatch
        sealPatchedSourceParseCount refused @?= 1
        case refused of
          SealRefused _ (NebulaParseError _) ->
            pure ()
          SealRefused _ otherFailure ->
            assertFailure ("expected the parser's typed refusal, got: " <> show otherFailure)
          otherOutcome ->
            assertFailure ("expected malformed patched source to refuse sealing, got: " <> show otherOutcome),
      testCase "an empty patch has an empty seal outcome and no write content" $ do
        firstPass <- requireImproved writeBackWorkload
        patched <-
          requireRight "patching" (patchedModuleSource (miPatch firstPass) (mwSource writeBackWorkload))
        secondPass <- requireImproved (withoutOracleWorkload (mwPath writeBackWorkload) patched)
        miSeal secondPass @?= SealEmpty
        sealPatchedSourceParseCount (miSeal secondPass) @?= 0
        assertBool "empty patch writes nothing" (not (modulePatchHasContent (miPatch secondPass))),
      testCase "a splice failure is refused before the patched source is parsed" $ do
        improvement <- requireImproved writeBackWorkload
        let invalidPatch =
              (miPatch improvement)
                { mpSplices = [SourceSplice (SourceRegion 99 1 99 2) "ghost"]
                }
            refused =
              sealModulePatchOutcome (mwPath writeBackWorkload) (mwSource writeBackWorkload) invalidPatch
        sealPatchedSourceParseCount refused @?= 0
        assertBool
          "the text report records that splicing failed before parsing"
          ( any
              ("patched-source-parses=0" `isInfixOf`)
              (renderModuleReport ((miReport improvement) {mrSeal = refused}))
          )
        case refused of
          SealRefused _ (NebulaSpliceError _) ->
            pure ()
          SealRefused _ otherFailure ->
            assertFailure ("expected a pre-parse splice error, got: " <> show otherFailure)
          otherOutcome ->
            assertFailure ("expected the invalid splice to refuse sealing, got: " <> show otherOutcome)
    ]

sealFirstCases :: TestTree
sealFirstCases =
  testGroup
    "nebula.writeback.sealfirst"
    [ testCase "corrupted patches refuse before diff hunks can escape" $ do
        improvement <- requireImproved writeBackWorkload
        let modulePatch = miPatch improvement
            corruptedPatch =
              modulePatch
                { mpSplices = fmap (\splice -> splice {ssReplacement = "shadow = use alpha beta"}) (mpSplices modulePatch)
                }
            refused =
              sealModulePatchOutcome (mwPath writeBackWorkload) (mwSource writeBackWorkload) corruptedPatch
            diffLines =
              renderModuleDiff (mwPath writeBackWorkload) (mwSource writeBackWorkload) corruptedPatch refused
        case refused of
          SealRefused _ (NebulaSealError sealName _) ->
            sealName @?= "shadow"
          SealRefused _ otherFailure ->
            assertFailure ("expected a seal error, got: " <> show otherFailure)
          otherOutcome ->
            assertFailure ("expected the corrupted patch to fail the seal, got: " <> show otherOutcome)
        assertBool "refused diffs render only the seal obstruction" (all (not . ("@@" `isInfixOf`)) diffLines)
        assertBool "the refusal is report content" (any ("seal status=refused" `isInfixOf`) (renderModuleReport ((miReport improvement) {mrSeal = refused}))),
      testCase "sealed reports carry the exact patched bytes and empty patches stay empty" $ do
        firstPass <- requireImproved writeBackWorkload
        patched <-
          requireRight "patching" (patchedModuleSource (miPatch firstPass) (mwSource writeBackWorkload))
        sealed <-
          requireRight
            "sealing"
            (sealModulePatch (mwPath writeBackWorkload) (mwSource writeBackWorkload) (miPatch firstPass))
        sealedSourceText sealed @?= patched
        secondPass <- requireImproved (withoutOracleWorkload (mwPath writeBackWorkload) patched)
        miSeal secondPass @?= SealEmpty
        sealPatchedSourceParseCount (miSeal secondPass) @?= 0
        assertBool "empty patch writes nothing" (not (modulePatchHasContent (miPatch secondPass)))
    ]

idempotentCases :: TestTree
idempotentCases =
  testGroup
    "nebula.writeback.idempotent"
    [ testCase "improving the patched module is the identity" $ do
        firstPass <- requireImproved writeBackWorkload
        patched <-
          requireRight "patching" (patchedModuleSource (miPatch firstPass) (mwSource writeBackWorkload))
        secondPass <- requireImproved (withoutOracleWorkload (mwPath writeBackWorkload) patched)
        let secondPatch = miPatch secondPass
        fmap fst (mpSpliced secondPatch) @?= []
        assertBool "no definitions are appended on the second pass" (null (mpAppendedDefinitions secondPatch))
        rePatched <- requireRight "re-patching" (patchedModuleSource secondPatch patched)
        rePatched @?= patched,
      testCase "the synthesized module is a fixed point of improvement" $ do
        firstPass <- requireImproved synthesisWorkload
        patched <-
          requireRight "patching" (patchedModuleSource (miPatch firstPass) (mwSource synthesisWorkload))
        secondPass <- requireImproved (withoutOracleWorkload (mwPath synthesisWorkload) patched)
        let secondPatch = miPatch secondPass
        fmap fst (mpSpliced secondPatch) @?= []
        assertBool "no definitions are appended on the second pass" (null (mpAppendedDefinitions secondPatch))
        rePatched <- requireRight "re-patching" (patchedModuleSource secondPatch patched)
        rePatched @?= patched
    ]
