{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Melusine.Nebula.Spec.RealizeSpec (spec) where

import Control.Exception (evaluate)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import Melusine.Nebula
  ( HunkCertificate (..),
    ModuleImprovement (..),
    ModuleReport (..),
    ModuleWorkload (..),
    NebulaError,
    SealOutcome (..),
    defaultNebulaConfig,
    improveModule,
    renderModuleReport,
    sealedSourceText,
  )
import Melusine.Nebula.Report.Text (BindingReport (..), NebulaLedger (..), moduleLedger)
import Melusine.Nebula.Harvest.Maintain
  ( HarvestAdvanceDecision (..),
    HarvestFallbackReason (..),
  )
import Melusine.Nebula.Rewrite.Saturate
  ( SaturationLifecycleCounts (..),
    smLifecycleCounts,
  )
import Melusine.Nebula.Synthesis.Core (SynthesisOutcome (..))
import Moonlight.Pale.Bench.Measure
  ( FreshMeasurement (..),
    FreshMeasurementFailure (..),
    measureFreshSample,
  )
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( ConvertedModuleMetrics (..),
    HsExprBindingRuleMetrics (..),
    HsExprSupportRuleMetrics (..),
    convertHaskellSource,
    convertedModuleMetrics,
    hsExprSupportRuleMetrics,
  )
import Moonlight.Homology
  ( CellCarrierError,
    DirectionFieldError,
    DirectionSymmetryOrderError,
    HomologicalDegree (..),
    MacroScaffoldIR (..),
    MorseReebScaffold (..),
    PotentialNormalization (..),
    RawCellData,
    RealizationBudget (..),
    ScalarPotentialFieldError,
    mkCellCarrier,
    mkDirectionAngleField,
    mkDirectionSymmetryOrder,
    mkScalarPotentialFieldFromSamples,
    realizeScaffoldRaw,
  )
import Moonlight.Flow.Model.Schema.Digest (StableDigest128 (..), stableDigest128)
import Moonlight.Pale.Ghc.Hie.Oracle (ModuleNameOracle (..), mkResolvedOrigin)
import Moonlight.Pale.Ghc.Hie.SourceKey (HieSourceKeyKind (..), OracleLookup (..))
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (readFile')
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

spec :: TestTree
spec =
  testGroup
    "nebula.realize"
    [ testCase "public homology realization API realizes an empty scaffold" publicRealizationApiSpec,
      testCase "Moonlight.Homology.Topology survives the full self-compression loop" publicTopologyLoopSpec,
      testCase "production path emits paper timing receipts (opt-in)" productionPathTimingReceiptSpec
    ]

type PublicRealizationFixtureFailure :: Type
data PublicRealizationFixtureFailure
  = PublicCarrierFailure CellCarrierError
  | PublicScalarPotentialFailure ScalarPotentialFieldError
  | PublicDirectionSymmetryFailure DirectionSymmetryOrderError
  | PublicDirectionFieldFailure DirectionFieldError
  deriving stock (Eq, Show)

publicRealizationApiSpec :: Assertion
publicRealizationApiSpec =
  case emptyPublicScaffold of
    Left fixtureFailure ->
      assertFailure ("public homology fixture failed: " <> show fixtureFailure)
    Right scaffold ->
      case realizeScaffoldRaw scaffold (RealizationBudget 0) of
        Left realizationFailure ->
          assertFailure ("public homology realization failed: " <> show realizationFailure)
        Right rawCells ->
          assertEqual "empty scaffold realizes to empty raw cells" (mempty :: RawCellData) rawCells

emptyPublicScaffold :: Either PublicRealizationFixtureFailure MacroScaffoldIR
emptyPublicScaffold = do
  carrier <- first PublicCarrierFailure (mkCellCarrier (HomologicalDegree 0) [])
  scalarPotential <-
    first
      PublicScalarPotentialFailure
      (mkScalarPotentialFieldFromSamples carrier NativePotentialScale Map.empty)
  directionSymmetry <-
    first
      PublicDirectionSymmetryFailure
      (mkDirectionSymmetryOrder 1)
  directionField <-
    first
      PublicDirectionFieldFailure
      (mkDirectionAngleField carrier directionSymmetry Map.empty)
  pure
    MacroScaffoldIR
      { macroScaffoldScalarPotential = scalarPotential,
        macroScaffoldReeb = MorseReebScaffold [] [],
        macroScaffoldDirectionField = directionField,
        macroScaffoldSingularities = [],
        macroScaffoldHarmonicLoops = []
      }

publicTopologyFileCandidates :: [FilePath]
publicTopologyFileCandidates =
  [ "../../foundation/moonlight-homology/src-public/Moonlight/Homology/Topology.hs",
    "foundation/moonlight-homology/src-public/Moonlight/Homology/Topology.hs",
    "compiler/foundation/moonlight-homology/src-public/Moonlight/Homology/Topology.hs"
  ]

publicTopologyLoopSpec :: Assertion
publicTopologyLoopSpec =
  lookupEnv "MELUSINE_NEBULA_REALIZE_SPEC" >>= \case
    Nothing ->
      putStrLn
        "Skipping full-file public topology nebula loop; set MELUSINE_NEBULA_REALIZE_SPEC=1 to enable."
    Just _ -> do
      topologyPath <-
        findFirstExistingPathForCurrentDirectory publicTopologyFileCandidates
          >>= maybe (assertFailure "Moonlight.Homology.Topology fixture not found from the current directory") pure
      topologySource <- readFile' topologyPath
      report <-
        either
          (\(modulePath, moduleFailure) -> assertFailure ("improve failed for " <> modulePath <> ": " <> show moduleFailure))
          (pure . miReport)
          (improveModule defaultNebulaConfig (ModuleWorkload topologyPath topologySource (OracleMissing [])))
      let bindings = mrBindingReports report
      assertBool "Moonlight.Homology.Topology yields more than one binding" (length bindings > 1)
      traverse_
        ( \binding ->
            assertBool
              (brName binding <> " never inflates under the size cost")
              (brExtractedSize binding <= brOriginalSize binding)
        )
        bindings
      let rendered = renderModuleReport report
      assertBool "the report is non-empty" (not (null rendered))
      assertBool
        "the report names its termination"
        (any (("termination" ==) . takeWhile (/= ' ')) rendered)
      traverse_ putStrLn rendered

type ProductionPathReceipt :: Type
data ProductionPathReceipt = ProductionPathReceipt
  { pprBindingCount :: !Int,
    pprOriginalNodes :: !Int,
    pprFinalNodes :: !Int,
    pprIterations :: !Int,
    pprMatchesApplied :: !Int,
    pprScheduledTotal :: !Int,
    pprPlanPreparations :: !Int,
    pprFreshRuns :: !Int,
    pprResumptions :: !Int,
    pprHarvestAdvance :: !(Maybe HarvestAdvanceDecision),
    pprSiteRules :: !Int,
    pprCompositionRules :: !Int,
    pprBindingFrontRules :: !Int,
    pprRealizedNodesSaved :: !Int,
    pprRenderedLineCount :: !Int,
    pprRenderedCharCount :: !Int,
    pprCertificateDigests :: ![StableDigest128],
    pprSealEvidence :: !ProductionSealEvidence
  }
  deriving stock (Eq, Show)

type ProductionSealEvidence :: Type
data ProductionSealEvidence
  = ProductionSealEmpty
  | ProductionSealSealed !ProductionSealedSourceEvidence
  | ProductionSealRefused !NebulaError
  deriving stock (Eq, Show)

type ProductionSealedSourceEvidence :: Type
data ProductionSealedSourceEvidence = ProductionSealedSourceEvidence
  { psseCharacterCount :: !Int,
    psseSourceDigest :: !StableDigest128
  }
  deriving stock (Eq, Show)

type TimedProductionPath :: Type
data TimedProductionPath = TimedProductionPath
  { tppElapsedNs :: !Word64,
    tppAllocatedBytes :: !Word64,
    tppPeakLiveBytes :: !Word64,
    tppOutcome :: !(Either String ProductionPathReceipt)
  }
  deriving stock (Eq, Show)

productionPathTimingReceiptSpec :: Assertion
productionPathTimingReceiptSpec =
  lookupEnv "MELUSINE_NEBULA_PRODUCTION_TIMING_SPEC" >>= \case
    Nothing ->
      putStrLn
        "Skipping production-path timing receipts; set MELUSINE_NEBULA_PRODUCTION_TIMING_SPEC=1 to enable."
    Just _ -> do
      compositionRows <- traverse measureCompositionProductionPath [4 .. 7]
      putStrLn "Nebula production-path composition timing"
      putStrLn "  N total allocatedBytes peakLiveBytes bindingCount rules compositionRules bindingFrontRules iterations matchesApplied scheduled planPreparations freshRuns resumptions harvestAdvance realized certificateDigests seal"
      traverse_ (putStrLn . renderCompositionTimingRow) compositionRows
      dogfoodPath <-
        findFirstExistingPathForCurrentDirectory dogfoodFileCandidates
          >>= maybe (assertFailure "91-context dogfood fixture not found from the current directory") pure
      dogfoodSource <- readFile' dogfoodPath
      dogfoodMetrics <- moduleMetricReceipt dogfoodPath dogfoodSource
      dogfoodOracle <- either assertFailure pure (compositionOracleFor dogfoodPath)
      dogfoodTiming <- measureProductionPath (ModuleWorkload dogfoodPath dogfoodSource (OracleFound GivenPathKey dogfoodPath dogfoodOracle))
      putStrLn "Nebula production-path 91-context dogfood timing"
      putStrLn (renderModuleMetricReceipt dogfoodMetrics)
      putStrLn ("  " <> renderTimedProductionPath dogfoodTiming)
      case tppOutcome dogfoodTiming of
        Left failureText ->
          assertFailure failureText
        Right _ ->
          pure ()

measureCompositionProductionPath :: Int -> IO AssertionTimingRow
measureCompositionProductionPath lambdaCount =
  case compositionWorkload lambdaCount of
    Left failureText ->
      pure (AssertionTimingRow lambdaCount (failedTimedProductionPath failureText))
    Right workload ->
      AssertionTimingRow lambdaCount <$> measureProductionPath workload

type AssertionTimingRow :: Type
data AssertionTimingRow = AssertionTimingRow
  { atrLambdaCount :: !Int,
    atrTiming :: !TimedProductionPath
  }
  deriving stock (Eq, Show)

measureProductionPath :: ModuleWorkload -> IO TimedProductionPath
measureProductionPath workload =
  measureFreshSample
    1
    workload
    (pure . productionPathOutcome)
    productionPathReceiptDemand
    >>= pure . either freshMeasurementFailure timedProductionPath

productionPathOutcome :: ModuleWorkload -> Either String ProductionPathReceipt
productionPathOutcome workload =
  case improveModule defaultNebulaConfig workload of
    Left (modulePath, moduleFailure) ->
      Left ("improve failed for " <> modulePath <> ": " <> show moduleFailure)
    Right improvement ->
      Right (productionPathReceipt improvement)

timedProductionPath :: FreshMeasurement ProductionPathReceipt -> TimedProductionPath
timedProductionPath measurement =
  TimedProductionPath
    { tppElapsedNs = freshMeasurementElapsedNanoseconds measurement,
      tppAllocatedBytes = freshMeasurementAllocatedBytes measurement,
      tppPeakLiveBytes = freshMeasurementPeakLiveBytes measurement,
      tppOutcome = Right (freshMeasurementValue measurement)
    }

freshMeasurementFailure :: FreshMeasurementFailure String -> TimedProductionPath
freshMeasurementFailure = \case
  FreshMeasurementRtsStatsDisabled ->
    failedTimedProductionPath "RTS statistics are disabled; rerun with +RTS -T -RTS"
  FreshMeasurementActionFailed failureText ->
    failedTimedProductionPath failureText

failedTimedProductionPath :: String -> TimedProductionPath
failedTimedProductionPath failureText =
  TimedProductionPath
    { tppElapsedNs = 0,
      tppAllocatedBytes = 0,
      tppPeakLiveBytes = 0,
      tppOutcome = Left failureText
    }

productionPathReceipt :: ModuleImprovement -> ProductionPathReceipt
productionPathReceipt improvement =
  let report = miReport improvement
      rendered = renderModuleReport report
      ledger = moduleLedger report
      lifecycleCounts = smLifecycleCounts (soSaturatedModule (mrSynthesis report))
   in ProductionPathReceipt
        { pprBindingCount = length (mrBindingReports report),
          pprOriginalNodes = mrOriginalTotal report,
          pprFinalNodes = mrFinalTotal report,
          pprIterations = mrIterations report,
          pprMatchesApplied = mrMatchesApplied report,
          pprScheduledTotal = mrScheduledTotal report,
          pprPlanPreparations = slcPlanPreparations lifecycleCounts,
          pprFreshRuns = slcFreshRuns lifecycleCounts,
          pprResumptions = slcResumptions lifecycleCounts,
          pprHarvestAdvance = soHarvestDecision (mrSynthesis report),
          pprSiteRules = hsrmTotalRuleCount (mrSiteMetrics report),
          pprCompositionRules = hsrmCompositionRuleCount (mrSiteMetrics report),
          pprBindingFrontRules = maybe 0 bindingFrontRuleCount (mrBindingFrontMetrics report),
          pprRealizedNodesSaved = nlRealizedNodesSaved ledger,
          pprRenderedLineCount = length rendered,
          pprRenderedCharCount = sum (fmap length rendered),
          pprCertificateDigests = fmap hcDigest (miCertificates improvement),
          pprSealEvidence = productionSealEvidence (miSeal improvement)
        }

productionSealEvidence :: SealOutcome -> ProductionSealEvidence
productionSealEvidence = \case
  SealEmpty ->
    ProductionSealEmpty
  Sealed sealedSource ->
    let sourceText = sealedSourceText sealedSource
        characterCount = length sourceText
     in ProductionSealSealed
          ( ProductionSealedSourceEvidence
              { psseCharacterCount = characterCount,
                psseSourceDigest =
                  stableDigest128
                    ( fromIntegral characterCount
                        : fmap (fromIntegral . fromEnum) sourceText
                    )
              }
          )
  SealRefused _ sealFailure ->
    ProductionSealRefused sealFailure

productionPathReceiptDemand :: ProductionPathReceipt -> Int
productionPathReceiptDemand receipt =
  pprBindingCount receipt
    `seq` pprOriginalNodes receipt
    `seq` pprFinalNodes receipt
    `seq` pprIterations receipt
    `seq` pprMatchesApplied receipt
    `seq` pprScheduledTotal receipt
    `seq` pprPlanPreparations receipt
    `seq` pprFreshRuns receipt
    `seq` pprResumptions receipt
    `seq` forceHarvestAdvanceDecision (pprHarvestAdvance receipt)
    `seq` pprSiteRules receipt
    `seq` pprCompositionRules receipt
    `seq` pprBindingFrontRules receipt
    `seq` pprRealizedNodesSaved receipt
    `seq` pprRenderedLineCount receipt
    `seq` pprRenderedCharCount receipt
    `seq` forceStableDigests (pprCertificateDigests receipt)
    `seq` forceProductionSealEvidence (pprSealEvidence receipt)
    `seq` pprRenderedCharCount receipt

forceHarvestAdvanceDecision :: Maybe HarvestAdvanceDecision -> ()
forceHarvestAdvanceDecision = \case
  Nothing -> ()
  Just HarvestAdvanced -> ()
  Just (HarvestFellBack reason) -> forceHarvestFallbackReason reason

forceHarvestFallbackReason :: HarvestFallbackReason -> ()
forceHarvestFallbackReason = \case
  HarvestFallbackGlobalPlanMerge -> ()
  HarvestFallbackDirtyRatio dirtyCount totalCount ratio ->
    dirtyCount `seq` totalCount `seq` ratio `seq` ()
  HarvestFallbackStageSectionObstruction obstruction ->
    foldr seq () obstruction
  HarvestFallbackSaturationSectionObstruction obstruction ->
    foldr seq () obstruction

forceStableDigests :: [StableDigest128] -> ()
forceStableDigests =
  foldr
    (\(StableDigest128 high low) rest -> high `seq` low `seq` rest)
    ()

forceProductionSealEvidence :: ProductionSealEvidence -> ()
forceProductionSealEvidence = \case
  ProductionSealEmpty ->
    ()
  ProductionSealSealed (ProductionSealedSourceEvidence characterCount (StableDigest128 high low)) ->
    characterCount `seq` high `seq` low `seq` ()
  ProductionSealRefused sealFailure ->
    length (show sealFailure) `seq` ()

bindingFrontRuleCount :: HsExprBindingRuleMetrics -> Int
bindingFrontRuleCount metrics =
  hbrmGeneratedRuleCount metrics + hbrmFactRuleCount metrics

renderCompositionTimingRow :: AssertionTimingRow -> String
renderCompositionTimingRow row =
  "  "
    <> show (atrLambdaCount row)
    <> " "
    <> renderTimedProductionPath (atrTiming row)

renderTimedProductionPath :: TimedProductionPath -> String
renderTimedProductionPath timing =
  intercalate
    " "
    [ "total=" <> formatSeconds (tppElapsedNs timing),
      "allocatedBytes=" <> show (tppAllocatedBytes timing),
      "peakLiveBytes=" <> show (tppPeakLiveBytes timing),
      renderProductionTimingOutcome (tppOutcome timing)
    ]

renderProductionTimingOutcome :: Either String ProductionPathReceipt -> String
renderProductionTimingOutcome = \case
  Left failureText ->
    "failed=" <> show failureText
  Right receipt ->
    intercalate
      " "
      [ "bindingCount=" <> show (pprBindingCount receipt),
        "rules=" <> show (pprSiteRules receipt),
        "compositionRules=" <> show (pprCompositionRules receipt),
        "bindingFrontRules=" <> show (pprBindingFrontRules receipt),
        "iterations=" <> show (pprIterations receipt),
        "matchesApplied=" <> show (pprMatchesApplied receipt),
        "scheduled=" <> show (pprScheduledTotal receipt),
        "planPreparations=" <> show (pprPlanPreparations receipt),
        "freshRuns=" <> show (pprFreshRuns receipt),
        "resumptions=" <> show (pprResumptions receipt),
        "harvestAdvance=" <> show (pprHarvestAdvance receipt),
        "nodes=" <> show (pprOriginalNodes receipt) <> "->" <> show (pprFinalNodes receipt),
        "realized=" <> show (pprRealizedNodesSaved receipt),
        "certificateDigests=" <> show (pprCertificateDigests receipt),
        "seal=" <> show (pprSealEvidence receipt)
      ]

formatSeconds :: Word64 -> String
formatSeconds ns =
  show (fromIntegral ns / (1000000000 :: Double) :: Double) <> "s"

type ModuleMetricReceipt :: Type
data ModuleMetricReceipt = ModuleMetricReceipt
  { mmrBindingCount :: !Int,
    mmrObservedContextCount :: !Int,
    mmrScopedExprCount :: !Int,
    mmrLambdaSiteCount :: !Int,
    mmrLetSiteCount :: !Int,
    mmrSiteRuleCount :: !Int,
    mmrCompositionRuleCount :: !Int
  }
  deriving stock (Eq, Show)

moduleMetricReceipt :: FilePath -> String -> IO ModuleMetricReceipt
moduleMetricReceipt sourcePath sourceText =
  case first show (convertHaskellSource sourcePath sourceText) of
    Left failureText ->
      assertFailure ("failed to parse dogfood module for metrics: " <> failureText)
    Right convertedModule -> do
      let conversionMetrics = convertedModuleMetrics convertedModule
          ruleMetrics = hsExprSupportRuleMetrics convertedModule
      evaluate
        ModuleMetricReceipt
          { mmrBindingCount = cmmBindingCount conversionMetrics,
            mmrObservedContextCount = cmmObservedContextCount conversionMetrics,
            mmrScopedExprCount = cmmScopedExprCount conversionMetrics,
            mmrLambdaSiteCount = hsrmLambdaSiteCount ruleMetrics,
            mmrLetSiteCount = hsrmLetSiteCount ruleMetrics,
            mmrSiteRuleCount = hsrmTotalRuleCount ruleMetrics,
            mmrCompositionRuleCount = hsrmCompositionRuleCount ruleMetrics
          }

renderModuleMetricReceipt :: ModuleMetricReceipt -> String
renderModuleMetricReceipt metrics =
  intercalate
    " "
    [ "  bindings=" <> show (mmrBindingCount metrics),
      "contexts=" <> show (mmrObservedContextCount metrics),
      "scopedExprs=" <> show (mmrScopedExprCount metrics),
      "lambdaSites=" <> show (mmrLambdaSiteCount metrics),
      "letSites=" <> show (mmrLetSiteCount metrics),
      "siteRules=" <> show (mmrSiteRuleCount metrics),
      "compositionRules=" <> show (mmrCompositionRuleCount metrics)
    ]

compositionWorkload :: Int -> Either String ModuleWorkload
compositionWorkload lambdaCount = do
  oracle <- compositionOracleFor sourcePath
  pure
    ModuleWorkload
      { mwPath = sourcePath,
        mwSource = compositionSource lambdaCount,
        mwOracleLookup = OracleFound GivenPathKey sourcePath oracle
      }
  where
    sourcePath =
      "Melusine/Nebula/Composition" <> show lambdaCount <> ".hs"

compositionOracleFor :: FilePath -> Either String ModuleNameOracle
compositionOracleFor sourcePath = do
  compositionOrigin <- first show (mkResolvedOrigin "base" "GHC.Internal.Base" ".")
  pure
    ModuleNameOracle
      { mnoSourcePath = sourcePath,
        mnoGlobalUses = Map.singleton "." (Set.singleton compositionOrigin),
        mnoEvidenceAtSpan = Map.empty,
        mnoTypeAtSpan = Map.empty
      }

compositionSource :: Int -> String
compositionSource lambdaCount =
  unlines
    ( [ "module Melusine.Nebula.Composition" <> show lambdaCount <> " where",
        ""
      ]
        <> fmap compositionBinding [1 .. lambdaCount]
    )

compositionBinding :: Int -> String
compositionBinding indexValue =
  "compose" <> show indexValue <> " = \\x -> f" <> show indexValue <> " (g" <> show indexValue <> " x)"

dogfoodFileCandidates :: [FilePath]
dogfoodFileCandidates =
  [ "../../foundation/moonlight-homology/src/Moonlight/Homology/Pure/Topology/Realize.hs",
    "foundation/moonlight-homology/src/Moonlight/Homology/Pure/Topology/Realize.hs",
    "compiler/foundation/moonlight-homology/src/Moonlight/Homology/Pure/Topology/Realize.hs",
    "../../foundation/moonlight-homology/src-topology/Moonlight/Homology/Pure/Topology/Realize.hs",
    "foundation/moonlight-homology/src-topology/Moonlight/Homology/Pure/Topology/Realize.hs",
    "compiler/foundation/moonlight-homology/src-topology/Moonlight/Homology/Pure/Topology/Realize.hs",
    "../../foundation/moonlight-homology/src-core/Moonlight/Homology/Pure/Topology/Realize.hs",
    "foundation/moonlight-homology/src-core/Moonlight/Homology/Pure/Topology/Realize.hs",
    "compiler/foundation/moonlight-homology/src-core/Moonlight/Homology/Pure/Topology/Realize.hs"
  ]

findFirstExistingPathForCurrentDirectory :: [FilePath] -> IO (Maybe FilePath)
findFirstExistingPathForCurrentDirectory relativePaths = do
  currentDirectory <- getCurrentDirectory
  findFirstExistingPath (fmap (currentDirectory </>) relativePaths)

findFirstExistingPath :: [FilePath] -> IO (Maybe FilePath)
findFirstExistingPath = \case
  [] ->
    pure Nothing
  candidatePath : remainingPaths -> do
    candidateExists <- doesFileExist candidatePath
    if candidateExists
      then pure (Just candidatePath)
      else findFirstExistingPath remainingPaths
