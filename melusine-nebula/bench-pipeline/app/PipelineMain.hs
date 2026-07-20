{-# LANGUAGE LambdaCase #-}

-- | Nebula pipeline ablation benchmark: a driver spawns one worker subprocess per arm.
module Main (main) where

import Data.Bifunctor (first)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Word (Word64)
import Bench.Pipeline.Lift
  ( CaseFactDiscovery (..),
    CaseLiftOutcome (..),
    LiftMerge (..),
    StructuralRewriteCandidate (..),
    acceptedLiftMerges,
    benchLevelCaseLift,
    discoverCaseFacts,
    stageCaseFacts,
    stageLiftMergeGoal,
    stageStructuralRewrites,
  )
import Bench.Pipeline.ProductSubstrate
  ( ContextEqualityGoal (..),
    ExternalConjunctionGoal (..),
    ExternalGoalResult (..),
    ProductAggregateReport (..),
    ProductGoalResult (..),
    ProductGoalSearch (..),
    ProductGoalSpec (..),
    ProductSubstrateError,
    ProductSubstrateReport (..),
    resolveProductContextEqualityGoal,
    runProductSubstrate,
  )
import Melusine.Nebula (enumerateModuleWorkloads)
import Melusine.Nebula.Core
  ( ModuleWorkload (..),
    NebulaAnalysis,
    NebulaConfig (..),
    defaultNebulaConfig,
    workloadOracle,
  )
import Melusine.Nebula.Rewrite.Corpus (RuleCorpus, deriveRuleCorpusWithOracleKeysAndReason)
import Melusine.Nebula.Rewrite.Saturate
  ( NebulaMatchingStrategy,
    SaturatedModule,
    SaturationLifecycleCounts (..),
    SaturationOptions (..),
    contextEqualityGoal,
    defaultSaturationOptions,
    saturateContextGraph,
    saturateEditedContextGraph,
    smContextGraph,
    smFinalClassCount,
    smFinalNodeCount,
    smIterations,
    smLifecycleCounts,
    smTermination,
  )
import Melusine.Nebula.Source.Ingest (IngestedModule (..), ingestModule)
import Moonlight.Pale.Bench.Measure
  ( FreshMeasurement (..),
    FreshMeasurementFailure (..),
    FreshRtsDelta (..),
    measureFreshSample,
  )
import Moonlight.EGraph.Bench.Harness.Report
  ( Align (..),
    Card (..),
    Column (..),
    Table,
    renderCsvRow,
  )
import Moonlight.EGraph.Bench.Harness.Run (BenchFailure, ScaleBench (..), abortBench, runScaleBench)
import Moonlight.EGraph.Introspection.Core.HsExpr
  ( HsExprF,
    ScopeCtx,
    hsExprScopeGuardCapabilityResolver,
  )
import Moonlight.EGraph.Pure.Context
  ( ContextEGraph,
  )
import Moonlight.EGraph.Pure.Saturation.Matching
  ( FrontierRefreshPosture (SkipUntouchedContextSnapshotRefresh),
    MatchingStrategy (CustomMatchingAlgebra, GenericJoinMatching, GenericJoinPerContextMatching),
    setPreparedWcojFrontierRefreshPosture,
    wcojMatchingAlgebra,
  )
import Moonlight.Saturation.Core (SaturationBudget (..), SaturationTermination (..))
import Moonlight.Saturation.Matching (maInitialState)
import Moonlight.Pale.Ghc.Hie.SourceKey (oracleAttachFailure)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Process (proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)
import Text.Printf (printf)

-- | Closed universe of ablation arms.
data Arm
  = ArmFull
  | ArmNoLift
  | ArmNoRegionNative
  | ArmNoFrontierRefresh
  | ArmProduct
  deriving stock (Eq, Show, Enum, Bounded)

allArms :: [Arm]
allArms = [minBound .. maxBound]

armName :: Arm -> String
armName = \case
  ArmFull -> "full"
  ArmNoLift -> "nolift"
  ArmNoRegionNative -> "no-region-native-emission"
  ArmNoFrontierRefresh -> "no-frontier-refresh"
  ArmProduct -> "product"

parseArm :: String -> Maybe Arm
parseArm nameText = List.find ((== nameText) . armName) allArms

main :: IO ()
main =
  getArgs >>= \case
    ["--arm", nameText, "--fixture", fixturePath] ->
      case parseArm nameText of
        Just arm -> runWorker arm fixturePath
        Nothing -> abortBench ("unknown arm: " <> nameText)
    [] -> runDriver
    other -> abortBench ("usage: bench-pipeline [--arm <name> --fixture <path>]; got " <> show other)

pipelineConfig :: NebulaConfig
pipelineConfig =
  defaultNebulaConfig
    { ncSaturationBudget = SaturationBudget 4 1600,
      ncSynthesisRounds = 1
    }

armBudgetMicroseconds :: Int
armBudgetMicroseconds = 30 * 1000 * 1000

-- | The result-round column carries the arm's semantics: an internal parent
-- equality for the nebula arms, an external covering conjunction for product.
data ResultFound
  = ResultFoundInternal !(Maybe Int)
  | ResultFoundExternal !(Maybe Int)

data PipelineOutcome = PipelineOutcome
  { poRounds :: !Int,
    poNodes :: !Int,
    poClasses :: !Int,
    poCompleted :: !Bool,
    poResultFound :: !ResultFound,
    poPlanPreparations :: !Int,
    poFreshRuns :: !Int,
    poResumptions :: !Int
  }

outcomeDigest :: PipelineOutcome -> Int
outcomeDigest outcome =
  sum
    [ poRounds outcome,
      poNodes outcome,
      poClasses outcome,
      fromEnum (poCompleted outcome),
      resultFoundDigest (poResultFound outcome)
    ]

resultFoundDigest :: ResultFound -> Int
resultFoundDigest = \case
  ResultFoundInternal (Just roundValue) -> roundValue
  ResultFoundInternal Nothing -> -1
  ResultFoundExternal (Just roundValue) -> roundValue
  ResultFoundExternal Nothing -> -2

data Prefix = Prefix
  { pfIngested :: !IngestedModule,
    pfCorpus :: !RuleCorpus,
    pfDiscovery :: !CaseFactDiscovery,
    pfMerge :: !LiftMerge
  }

buildPrefix :: NebulaConfig -> ModuleWorkload -> Either BenchFailure Prefix
buildPrefix config workload = do
  ingested <- first show (ingestModule workload)
  corpus <-
    first show $
      deriveRuleCorpusWithOracleKeysAndReason
        config
        Set.empty
        (oracleAttachFailure (mwOracleLookup workload))
        (imSpanRows ingested)
        (workloadOracle workload)
        (imConverted ingested)
  let discovery = discoverCaseFacts ingested
  merge <-
    requireHead
      "fixture produced no accepted parent-lift merge"
      (acceptedLiftMerges (cfdAlternatives discovery) (cfdStructuralRewrites discovery))
  pure (Prefix ingested corpus discovery merge)

runPreparedArm :: NebulaConfig -> ModuleWorkload -> Arm -> Prefix -> Either BenchFailure PipelineOutcome
runPreparedArm config workload arm prefix =
  case arm of
    ArmProduct -> runProductArm config workload prefix
    _ -> runNebulaPipeline config arm prefix

runNebulaPipeline :: NebulaConfig -> Arm -> Prefix -> Either BenchFailure PipelineOutcome
runNebulaPipeline config arm prefix = do
  let ingested = pfIngested prefix
      corpus = pfCorpus prefix
      discovery = pfDiscovery prefix
      merge = pfMerge prefix
      optionsFor graph goalValue =
        defaultSaturationOptions
          { soMatchingStrategy = armMatchingStrategy arm graph,
            soGoal = goalValue
          }
  (_, graphWithFacts) <- stageCaseFacts (cfdCandidates discovery) (imContextGraph ingested)
  (_, graphWithStructural) <- stageStructuralRewrites (cfdStructuralRewrites discovery) graphWithFacts
  if armDoLift arm
    then do
      let phase1Graph = graphWithStructural
      phase1 <- first show (saturateContextGraph (optionsFor phase1Graph mempty) config phase1Graph corpus)
      liftedGraph <-
        cloGraph (benchLevelCaseLift (cfdAlternatives discovery) (cfdStructuralRewrites discovery) (smContextGraph phase1))
      (goalContext, leftClass, rightClass, goalGraph) <- stageLiftMergeGoal merge liftedGraph
      let phase2Graph = goalGraph
      phase2 <-
        first show (saturateEditedContextGraph (contextEqualityGoal goalContext leftClass rightClass) corpus phase2Graph phase1)
      pure (twoPhaseOutcome phase1 phase2)
    else do
      (goalContext, leftClass, rightClass, goalGraph) <- stageLiftMergeGoal merge graphWithStructural
      let singleGraph = goalGraph
      phase <-
        first show (saturateContextGraph (optionsFor singleGraph (contextEqualityGoal goalContext leftClass rightClass)) config singleGraph corpus)
      pure (singlePhaseOutcome phase)

armDoLift :: Arm -> Bool
armDoLift = \case
  ArmNoLift -> False
  _ -> True

armMatchingStrategy :: Arm -> ContextEGraph HsExprF NebulaAnalysis ScopeCtx -> NebulaMatchingStrategy
armMatchingStrategy arm graph = case arm of
  ArmNoRegionNative -> GenericJoinPerContextMatching
  ArmNoFrontierRefresh -> frontierAblatedStrategy graph
  _ -> GenericJoinMatching

-- | The frontier posture is only settable on a wcoj matching state, and the
-- egraph surface delivers a custom initial state exclusively through
-- 'CustomMatchingAlgebra', which the substrate runs under per-context emission.
frontierAblatedStrategy :: ContextEGraph HsExprF NebulaAnalysis ScopeCtx -> NebulaMatchingStrategy
frontierAblatedStrategy graph =
  let baseAlgebra = wcojMatchingAlgebra (hsExprScopeGuardCapabilityResolver graph)
   in CustomMatchingAlgebra
        baseAlgebra
          { maInitialState =
              setPreparedWcojFrontierRefreshPosture
                SkipUntouchedContextSnapshotRefresh
                (maInitialState baseAlgebra)
          }

twoPhaseOutcome :: SaturatedModule -> SaturatedModule -> PipelineOutcome
twoPhaseOutcome phase1 phase2 =
  let cumulativeRounds = smIterations phase1 + smIterations phase2
   in PipelineOutcome
        { poRounds = cumulativeRounds,
          poNodes = smFinalNodeCount phase2,
          poClasses = smFinalClassCount phase2,
          poCompleted = terminationCompleted (smTermination phase2),
          poResultFound = ResultFoundInternal (goalRound cumulativeRounds phase2),
          poPlanPreparations = slcPlanPreparations lifecycleCounts,
          poFreshRuns = slcFreshRuns lifecycleCounts,
          poResumptions = slcResumptions lifecycleCounts
        }
  where
    lifecycleCounts = smLifecycleCounts phase2

singlePhaseOutcome :: SaturatedModule -> PipelineOutcome
singlePhaseOutcome phase =
  PipelineOutcome
    { poRounds = smIterations phase,
      poNodes = smFinalNodeCount phase,
      poClasses = smFinalClassCount phase,
      poCompleted = terminationCompleted (smTermination phase),
      poResultFound = ResultFoundInternal (goalRound (smIterations phase) phase),
      poPlanPreparations = slcPlanPreparations lifecycleCounts,
      poFreshRuns = slcFreshRuns lifecycleCounts,
      poResumptions = slcResumptions lifecycleCounts
    }
  where
    lifecycleCounts = smLifecycleCounts phase

goalRound :: Int -> SaturatedModule -> Maybe Int
goalRound cumulativeRounds phase =
  case smTermination phase of
    ReachedGoal -> Just cumulativeRounds
    _ -> Nothing

terminationCompleted :: SaturationTermination -> Bool
terminationCompleted = \case
  ReachedFixedPoint -> True
  ReachedGoal -> True
  _ -> False

runProductArm :: NebulaConfig -> ModuleWorkload -> Prefix -> Either BenchFailure PipelineOutcome
runProductArm config workload prefix = do
  goalSpec <-
    first show (buildProductGoal (pfIngested prefix) (pfDiscovery prefix) (pfMerge prefix))
  report <- first show (runProductSubstrate config workload goalSpec)
  pure (productOutcome (psrAggregate report))

productOutcome :: ProductAggregateReport -> PipelineOutcome
productOutcome aggregate =
  PipelineOutcome
    { poRounds = parRounds aggregate,
      poNodes = parNodes aggregate,
      poClasses = parClasses aggregate,
      poCompleted = parCompleted aggregate,
      poResultFound = ResultFoundExternal (externalGoalRound (parGoalResult aggregate)),
      poPlanPreparations = 0,
      poFreshRuns = 0,
      poResumptions = 0
    }

externalGoalRound :: ProductGoalResult -> Maybe Int
externalGoalRound = \case
  ExternalConjunctionGoalResult external ->
    case egrSearch external of
      ProductGoalFoundAtRound roundValue -> Just roundValue
      ProductGoalNotFound -> Nothing
  _ -> Nothing

-- | The product target is the parent context; its covers are the branch alt
-- contexts, each proving a local structural equality. The substrate cannot glue
-- them into a parent result, so this is an external conjunction only.
buildProductGoal :: IngestedModule -> CaseFactDiscovery -> LiftMerge -> Either ProductSubstrateError (Maybe ProductGoalSpec)
buildProductGoal ingested discovery merge = do
  let contextGraph = imContextGraph ingested
      targetContext = lmParentContext merge
      mergeCovers =
        [ candidate
        | candidate <- cfdStructuralRewrites discovery,
          srcModuleName candidate == lmModuleName merge,
          srcBindingName candidate == lmBindingName merge,
          srcParentContext candidate == targetContext
        ]
  coverGoals <-
    distinctByContext . mapMaybe id
      <$> traverse (resolveProductCover contextGraph) mergeCovers
  pure $ case coverGoals of
    firstCover : secondCover : rest ->
      Just
        ( ExternalCoveringConjunctionGoal
            (ExternalConjunctionGoal targetContext (firstCover :| (secondCover : rest)))
        )
    _ -> Nothing

resolveProductCover ::
  ContextEGraph HsExprF NebulaAnalysis ScopeCtx ->
  StructuralRewriteCandidate ->
  Either ProductSubstrateError (Maybe ContextEqualityGoal)
resolveProductCover contextGraph candidate =
  resolveProductContextEqualityGoal
    contextGraph
    (srcContext candidate)
    (srcRedexPattern candidate)
    (srcReplacementPattern candidate)

distinctByContext :: [ContextEqualityGoal] -> [ContextEqualityGoal]
distinctByContext = go Set.empty
  where
    go _ [] = []
    go seen (goalValue : rest)
      | Set.member (cegContext goalValue) seen = go seen rest
      | otherwise = goalValue : go (Set.insert (cegContext goalValue) seen) rest

runWorker :: Arm -> FilePath -> IO ()
runWorker arm fixturePath = do
  workloadResult <- loadWorkload fixturePath
  case workloadResult of
    Left failure -> abortBench ("worker " <> armName arm <> ": " <> failure)
    Right workload ->
      case buildPrefix pipelineConfig workload of
        Left failure -> abortBench ("worker " <> armName arm <> ": prefix setup failed: " <> failure)
        Right prefix -> do
          measured <-
            timeout armBudgetMicroseconds $
              measureFreshSample
                1
                (workload, prefix)
                (pure . (\(workloadValue, prefixValue) -> runPreparedArm pipelineConfig workloadValue arm prefixValue))
                (\outcome -> outcomeDigest outcome `seq` ())
                outcomeDigest
          case measured of
            Nothing -> abortBench ("worker " <> armName arm <> ": no sample within budget")
            Just (Left failure) -> abortBench ("worker " <> armName arm <> ": " <> renderFreshMeasurementFailure failure)
            Just (Right measurement) -> do
              let outcome = freshMeasurementValue measurement
                  wallUs = freshMeasurementElapsedNanoseconds measurement `div` 1000
                  peakMb = bytesToMb (freshMeasurementPeakLiveBytesThroughAction measurement)
              putStrLn
                ( renderCsvRow
                    pipelineTable
                    ( workerRow
                        arm
                        outcome
                        (freshMeasurementDigest measurement)
                        (freshRtsDeltaAllocatedBytes (freshMeasurementRtsDelta measurement))
                        wallUs
                        peakMb
                    )
                )

renderFreshMeasurementFailure :: FreshMeasurementFailure BenchFailure -> String
renderFreshMeasurementFailure = \case
  FreshMeasurementRtsStatsDisabled ->
    "RTS statistics are disabled; rerun with +RTS -T -RTS"
  FreshMeasurementActionFailed failure ->
    failure
  FreshMeasurementRtsDeltaFailed obstruction ->
    "RTS counter regression: " <> show obstruction

loadWorkload :: FilePath -> IO (Either BenchFailure ModuleWorkload)
loadWorkload fixturePath = do
  (workspaceErrors, workloads) <- enumerateModuleWorkloads [fixturePath] []
  pure $ case (workspaceErrors, workloads) of
    ([], [workload]) -> Right workload
    ([], others) -> Left ("expected exactly one workload, got " <> show (length others))
    (errors, _) -> Left ("workspace errors: " <> show errors)

bytesToMb :: Word64 -> Double
bytesToMb byteCount = fromIntegral byteCount / (1024 * 1024)

-- | One fully rendered CSV row; all cells are strings so the driver round-trips
-- the worker's line through the same table without reparsing typed fields.
data PipelineRow = PipelineRow
  { rowArm :: !String,
    rowRounds :: !String,
    rowNodes :: !String,
    rowClasses :: !String,
    rowCompleted :: !String,
    rowResultFound :: !String,
    rowPlanPreparations :: !String,
    rowFreshRuns :: !String,
    rowResumptions :: !String,
    rowSemanticDigest :: !String,
    rowAllocatedBytes :: !String,
    rowPeakMb :: !String,
    rowWallUs :: !String
  }

workerRow :: Arm -> PipelineOutcome -> Int -> Word64 -> Word64 -> Double -> PipelineRow
workerRow arm outcome semanticDigest allocatedBytes wallUs peakMb =
  PipelineRow
    { rowArm = armName arm,
      rowRounds = show (poRounds outcome),
      rowNodes = show (poNodes outcome),
      rowClasses = show (poClasses outcome),
      rowCompleted = if poCompleted outcome then "true" else "false",
      rowResultFound = renderResultFound (poResultFound outcome),
      rowPlanPreparations = show (poPlanPreparations outcome),
      rowFreshRuns = show (poFreshRuns outcome),
      rowResumptions = show (poResumptions outcome),
      rowSemanticDigest = show semanticDigest,
      rowAllocatedBytes = show allocatedBytes,
      rowPeakMb = printf "%.3f" peakMb,
      rowWallUs = show wallUs
    }

renderResultFound :: ResultFound -> String
renderResultFound = \case
  ResultFoundInternal (Just roundValue) -> show roundValue
  ResultFoundInternal Nothing -> "NONE"
  ResultFoundExternal (Just roundValue) -> "EXT-CONJ:" <> show roundValue
  ResultFoundExternal Nothing -> "EXT-CONJ:NONE"

pipelineTable :: Table PipelineRow
pipelineTable =
  [ Column "arm" AlignLeft rowArm,
    Column "rounds" AlignRight rowRounds,
    Column "nodes" AlignRight rowNodes,
    Column "classes" AlignRight rowClasses,
    Column "completed" AlignLeft rowCompleted,
    Column "result_found_round" AlignRight rowResultFound,
    Column "plan_preparations" AlignRight rowPlanPreparations,
    Column "fresh_runs" AlignRight rowFreshRuns,
    Column "resumptions" AlignRight rowResumptions,
    Column "semantic_digest" AlignRight rowSemanticDigest,
    Column "allocated_bytes" AlignRight rowAllocatedBytes,
    Column "peak_live_mb" AlignRight rowPeakMb,
    Column "wall_us" AlignRight rowWallUs
  ]

runDriver :: IO ()
runDriver = do
  executablePath <- getExecutablePath
  fixturePath <- resolveFixturePath
  runScaleBench
    ScaleBench
      { benchName = "nebula-pipeline",
        benchReproCommand = "cabal bench melusine-nebula:bench-pipeline",
        benchPoints = allArms,
        benchAnnounce = \arm -> "arm: " <> armName arm,
        benchRunPoint = driverRunArm executablePath fixturePath,
        benchCsv = pipelineTable,
        benchCard = pipelineCard
      }

driverRunArm :: FilePath -> FilePath -> Arm -> IO (Either BenchFailure [PipelineRow])
driverRunArm executablePath fixturePath arm = do
  (exitCode, stdoutText, stderrText) <-
    readCreateProcessWithExitCode
      (proc executablePath ["--arm", armName arm, "--fixture", fixturePath])
      ""
  pure $ case exitCode of
    ExitSuccess ->
      case parsePipelineRow (lastDataLine stdoutText) of
        Just row -> Right [row]
        Nothing -> Left (armName arm <> ": unparseable child row: " <> stdoutText)
    ExitFailure code ->
      Left (armName arm <> ": child exited " <> show code <> ": " <> stderrText)

parsePipelineRow :: String -> Maybe PipelineRow
parsePipelineRow line =
  case splitCommas line of
    [armField, roundsField, nodesField, classesField, completedField, resultField, planPreparationsField, freshRunsField, resumptionsField, semanticDigestField, allocatedBytesField, peakField, wallField] ->
      Just (PipelineRow armField roundsField nodesField classesField completedField resultField planPreparationsField freshRunsField resumptionsField semanticDigestField allocatedBytesField peakField wallField)
    _ -> Nothing

splitCommas :: String -> [String]
splitCommas text =
  case break (== ',') text of
    (field, []) -> [field]
    (field, _ : rest) -> field : splitCommas rest

lastDataLine :: String -> String
lastDataLine text =
  case filter (not . null) (lines text) of
    [] -> ""
    dataLines -> last dataLines

pipelineCard :: Card PipelineRow PipelineRow
pipelineCard =
  Card
    { cardVerdict = \rows -> "# nebula pipeline ablation (" <> show (length rows) <> " arms, one fresh process per arm)",
      cardSummarize = id,
      cardTable = pipelineTable,
      cardNotes = const cardNoteLines,
      cardMissing = const productGoalCaveat,
      cardNext = const (Just "Feed nebula-pipeline.csv into the paper's pipeline ablation table.")
    }

cardNoteLines :: [String]
cardNoteLines =
  [ "Each worker row is one fresh-process sample. allocated_bytes is the measured action's RTS allocation delta; peak_live_mb is process-lifetime peak live memory; semantic_digest covers rounds, nodes, classes, completion, and result. Strict plan lifecycle counts remain separate columns so operational changes cannot counterfeit a semantic digest change.",
    "Ingest, discovery, and corpus/support-program compilation happen before measurement. Production plan preparation remains inside the timed pipeline because its PlanSpec is owned by the production saturation boundary, not a benchmark clone.",
    "The 'full' arm authors a bench-level case lift between two saturation phases; result_found_round is cumulative (phase-1 iterations + phase-2 goal round), never phase-2 alone. This is a bench-level case lift, not production lift.",
    "The 'nolift' arm seeds the same goal in a single phase without the lift; a never-completing goal (result_found_round NONE while the graph still reaches a fixed point) is the honest row.",
    "The 'no-frontier-refresh' arm sets the wcoj frontier posture to SkipUntouchedContextSnapshotRefresh. The egraph surface only accepts a custom initial matching state through CustomMatchingAlgebra, which the substrate runs under per-context emission, so this arm also drops region-native emission.",
    "The 'product' arm reports an external covering conjunction (EXT-CONJ) over the branch contexts; the substrate has no internal parent result and never authors the parent lift target."
  ]

productGoalCaveat :: String
productGoalCaveat =
  "Product covering equalities are resolved against the merge-free ingested graph copied into each independent product engine; contextual equality in that prefix is a typed refusal. If fewer than two covers resolve, the product arm runs without a goal and reports EXT-CONJ:NONE."

requireHead :: BenchFailure -> [a] -> Either BenchFailure a
requireHead failure = \case
  value : _ -> Right value
  [] -> Left failure

resolveFixturePath :: IO FilePath
resolveFixturePath = do
  cwd <- getCurrentDirectory
  existing <- filterExisting (fixtureCandidates cwd)
  case existing of
    path : _ -> pure path
    [] -> abortBench "cannot locate bench-pipeline/fixtures/Composite.hs"

filterExisting :: [FilePath] -> IO [FilePath]
filterExisting = foldr keepIfExists (pure [])
  where
    keepIfExists candidate rest = do
      present <- doesFileExist candidate
      remaining <- rest
      pure (if present then candidate : remaining else remaining)

fixtureCandidates :: FilePath -> [FilePath]
fixtureCandidates cwd =
  [ cwd </> "bench-pipeline" </> "fixtures" </> "Composite.hs",
    cwd </> "engine" </> "melusine-nebula" </> "bench-pipeline" </> "fixtures" </> "Composite.hs",
    cwd </> "compiler" </> "engine" </> "melusine-nebula" </> "bench-pipeline" </> "fixtures" </> "Composite.hs",
    takeDirectory cwd </> "compiler" </> "engine" </> "melusine-nebula" </> "bench-pipeline" </> "fixtures" </> "Composite.hs"
  ]
