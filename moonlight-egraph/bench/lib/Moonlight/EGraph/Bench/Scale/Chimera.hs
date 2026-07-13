module Moonlight.EGraph.Bench.Scale.Chimera
  ( main,
    chimeraContextKernelBenchmarks,
  )
where

import Control.DeepSeq (NFData (..))
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Fix (Fix)
import Data.IntMap.Strict qualified as IntMap
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Numeric.Natural (Natural)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Moonlight.Control.Schedule (identitySchedulerRefinement)
import Moonlight.Core
  ( ClassId (..),
    RewriteRuleId,
  )
import Moonlight.EGraph.Bench.Corpus (nonOverlappingPairs)
import Moonlight.EGraph.Bench.Harness.Digest qualified as Digest
import Moonlight.EGraph.Bench.Harness.Driver qualified as Driver
import Moonlight.EGraph.Bench.Harness.Fixture qualified as Fixture
import Moonlight.EGraph.Bench.Harness.Measure qualified as Measure
import Moonlight.EGraph.Bench.Harness.Report qualified as Report
import Moonlight.EGraph.Bench.Harness.Run qualified as Run
import Moonlight.EGraph.Bench.Harness.ScaleDigest qualified as ScaleDigest
import Moonlight.Pale.Bench.Measure qualified as PaleMeasure
import Moonlight.EGraph.Pure.Context
  ( ContextDeltaError (..),
    ContextEGraph,
    activateContext,
    beginContextRebaseBatch,
    commitContextRebaseBatch,
    contextMerge,
    emptyContextEGraph,
    stageTermAtContext,
  )
import Moonlight.EGraph.Pure.Context.Core
  ( cegSite,
  )
import Moonlight.EGraph.Pure.Context.Proof qualified as Proof
import Moonlight.EGraph.Pure.Kernel.HashCons (addTerm)
import Moonlight.EGraph.Pure.Rebuild (merge, rebuild)
import Moonlight.EGraph.Pure.Saturation.Substrate (EGraphU)
import Moonlight.EGraph.Saturation.Context.State qualified as SaturationState
import Moonlight.EGraph.Pure.Types
  ( EGraph,
    eGraphAnalysis,
  )
import Moonlight.EGraph.Test.Chimera.Core qualified as Chimera
import Moonlight.EGraph.Test.Chimera.Population qualified as Population
import Moonlight.EGraph.Test.Saturation qualified as Saturation
import Moonlight.EGraph.Test.Scale.Site qualified as Scale
import Moonlight.FiniteLattice
  ( ContextLatticeLookupError,
    supportReachableLatticeContexts,
  )
import Moonlight.Rewrite.ProofContext (principalSupport)
import Moonlight.Rewrite.System (RewriteCondition)
import Moonlight.Rewrite.System (FactRule)
import Moonlight.Rewrite.System (RawRewriteRule)
import Moonlight.Saturation.Context.Error (SaturationError)
import Moonlight.Saturation.Context.Runtime.Engine qualified as RuntimeEngine
import Moonlight.Saturation.Core
  ( SaturationBudget (..),
    SaturationTermination (..),
  )
import Moonlight.Saturation.Substrate (TrivialContext)
import Moonlight.Saturation.Substrate qualified as SaturationSubstrate
import Moonlight.Saturation.Support.Core qualified as Support
import Moonlight.Sheaf.Descent.Context (descentAt)
import Moonlight.Sheaf.Context.Site
  ( ContextObjectKey (..),
    contextObjectKeyFor,
  )
import Test.Tasty.Bench (Benchmark)
import Test.Tasty.Bench qualified as Criterion
import System.Environment (getArgs)

type ChimeraPlainU = EGraphU () Chimera.TissueF Chimera.TissueCount TrivialContext
type ChimeraSupportU = EGraphU () Chimera.TissueF Chimera.TissueCount Scale.ScaleContext
type ChimeraPlainReport = Saturation.EGraphSaturationReport () Chimera.TissueF Chimera.TissueCount
type ChimeraContextGraph = ContextEGraph Chimera.TissueF Chimera.TissueCount Scale.ScaleContext
type ChimeraScaleReport = ScaleDigest.ScaleReport Chimera.TissueF Chimera.TissueCount
type ChimeraDriverFailure = Driver.DriverFailure Chimera.TissueF Chimera.TissueCount
type PlainRuleAssignment =
  ([FactRule () Chimera.TissueF], [RawRewriteRule (RewriteCondition () Chimera.TissueF) Chimera.TissueF])

data PointFixture = PointFixture
  { pfScaleFixture :: !(Fixture.ScaleFixture Chimera.TissueF Chimera.TissueCount),
    pfKeyedContexts :: ![(ContextObjectKey, Scale.ScaleContext)],
    pfBaseQueryClasses :: ![ClassId],
    pfBottomCovers :: ![Scale.ScaleContext],
    pfVisibleTargets :: !(Map Scale.ScaleContext [Scale.ScaleContext]),
    pfMergeSteps :: ![(Scale.ScaleContext, (ClassId, ClassId))],
    pfDeltaTerms :: ![(Scale.ScaleContext, Fix Chimera.TissueF)],
    pfProductRuleAssignments :: !(Map Scale.ScaleContext PlainRuleAssignment)
  }

data SemanticPointFixture = SemanticPointFixture
  { spfPointFixture :: !PointFixture,
    spfMergedContextQueries :: ![Digest.SemanticQuery Scale.ScaleContext],
    spfDeltaContextQueries :: ![Digest.SemanticQuery Scale.ScaleContext],
    spfProductComparisonQueries :: ![Digest.SemanticQuery Scale.ScaleContext]
  }

-- | Criterion owns the narrow kernel receipt while the scale runner owns the
-- full comparative card. Both consume the same validated fixture and semantic
-- query cover; there is no second benchmark model to drift.
data CriterionKernelFixture = CriterionKernelFixture
  { ckfPointFixture :: !PointFixture,
    ckfMergedContextQueries :: ![Digest.SemanticQuery Scale.ScaleContext],
    ckfDeltaContextQueries :: ![Digest.SemanticQuery Scale.ScaleContext],
    ckfDeltaGraph :: !ChimeraContextGraph
  }

instance NFData CriterionKernelFixture where
  rnf fixture =
    Digest.contextGraphDigest (ckfDeltaGraph fixture)
      `seq` length (ckfDeltaContextQueries fixture)
      `seq` ()

data ChimeraPoint
  = PlainEnginePoint
  | ContextGridPoint !Int

data Register
  = KernelRegister
  | DriverRegister
  | BaselineRegister
  deriving stock (Eq, Ord)

data Arm
  = OursContext
  | ProductReplay
  | PlainEngine
  deriving stock (Eq, Ord)

data Phase
  = RegionScopedMerges
  | AuthoredDeltaPerRegion
  | RepresentativeQuerySweep
  | DescentPlusLiftTopCover
  | DriverSetup
  | SupportSaturation
  | SupportSaturationFixpoint
  | ProductDriverSaturation
  | ProductDriverSaturationFixpoint
  | PlainEngineRound
  deriving stock (Eq, Ord)

data ChimeraCommand
  = RunComparativeCard
  | RunSupportDriverReceipt !SupportDriverReceiptScale

data SupportDriverReceiptScale
  = SupportDriverK128
  | SupportDriverK256

data ChimeraCommandObstruction
  = UnsupportedChimeraCommand ![String]

data SupportDriverReceiptObstruction
  = SupportDriverPreparationObstruction !ChimeraDriverFailure
  | SupportDriverRuntimeObstruction !ChimeraDriverFailure
  | SupportDriverSemanticObstruction !(Digest.SemanticDigestObstruction Scale.ScaleContext)
  deriving stock (Show)

data SupportDriverObservation = SupportDriverObservation
  { sdoReport :: !ChimeraScaleReport,
    sdoRuntimeTiming :: !RuntimeEngine.RuntimeIOTiming,
    sdoSemanticDigest :: !Int
  }

data BaselineAdmissionVerdict
  = BaselineAdmissionAcceptedWithZeroRefusals

data SupportDriverReceipt = SupportDriverReceipt
  { sdrScale :: !SupportDriverReceiptScale,
    sdrTermCount :: !Int,
    sdrAdmissionVerdict :: !BaselineAdmissionVerdict,
    sdrTermination :: !SaturationTermination,
    sdrIterations :: !Int,
    sdrMatchesApplied :: !Int,
    sdrInitialNodeCount :: !Int,
    sdrFinalNodeCount :: !Int,
    sdrInitialClassCount :: !Int,
    sdrFinalClassCount :: !Int,
    sdrSemanticDigest :: !Int,
    sdrAllocatedBytes :: !Word64,
    sdrPeakLiveBytes :: !Word64,
    sdrWallNanoseconds :: !Word64,
    sdrRoundBuildNanoseconds :: !Natural,
    sdrApplyNanoseconds :: !Natural,
    sdrRebuildNanoseconds :: !Natural,
    sdrCommitNanoseconds :: !Natural
  }

data CellValue
  = CellMeasured !Word64
  | CellTimeout
  deriving stock (Eq, Ord)

data TerminationCell
  = NotSaturationRun
  | SaturationStopped !SaturationTermination
  deriving stock (Eq, Ord)

data CsvRow = CsvRow
  { crRegister :: !Register,
    crArm :: !Arm,
    crContextCount :: !Int,
    crTermCount :: !Int,
    crPhase :: !Phase,
    crTermination :: !TerminationCell,
    crWall :: !CellValue,
    crRegionalStructure :: !(Maybe Digest.RegionalStructureObservation)
  }

data SummaryRow = SummaryRow
  { summaryContextCount :: !Int,
    summarySourceRows :: ![CsvRow]
  }

contextCounts :: [Int]
contextCounts = [8, 16, 32, 64, 128, 256]
populationSize :: Int
populationSize = 64
populationDepth :: Int
populationDepth = 3

comparableKernelPhases :: [Phase]
comparableKernelPhases =
  [ RegionScopedMerges,
    AuthoredDeltaPerRegion
  ]

chimeraFixpointBudget :: SaturationBudget
chimeraFixpointBudget =
  SaturationBudget
    { sbMaxIterations = 6,
      sbMaxNodes = 8000
    }

productBudgetNs :: Word64
productBudgetNs = 120 * 1000 * 1000 * 1000

chimeraSamplePolicy :: Measure.SamplePolicy
chimeraSamplePolicy = Measure.defaultSamplePolicy
type BenchIO value =
  IO (Either Run.BenchFailure value)

andThenBench :: BenchIO left -> (left -> BenchIO right) -> BenchIO right
andThenBench action next =
  action >>= either (pure . Left) next

main :: IO ()
main = do
  commandResult <- parseChimeraCommand <$> getArgs
  either
    (Run.abortBench . renderChimeraCommandObstruction)
    runCommand
    commandResult
  where
    corpusResult =
      Fixture.prepareScaleCorpus
        Chimera.tissueAnalysis
        (Population.tissueTermsAtDepth populationDepth populationSize)
    runCommand command =
      either Run.abortBench (executeValidatedCommand command) corpusResult
    executeValidatedCommand command corpus =
      either Run.abortBench (\() -> corpus `seq` executeCommand corpus command) semanticDigestSensitivityLaw
    executeCommand corpus = \case
      RunComparativeCard -> Run.runScaleBench (bench corpus)
      RunSupportDriverReceipt receiptScale ->
        runSupportDriverReceipt receiptScale corpus
    bench corpus =
      Run.ScaleBench
        { Run.benchName = "chimera-scale",
          Run.benchReproCommand = "cd compiler && cabal bench moonlight-egraph:chimera-scale-bench -j1",
          Run.benchPoints = PlainEnginePoint : fmap ContextGridPoint contextCounts,
          Run.benchAnnounce = \case
            PlainEnginePoint -> "chimera-scale plain-engine K=1 N=" <> show (Fixture.scTermCount corpus)
            ContextGridPoint contextCount ->
              "chimera-scale K=" <> show contextCount <> " N=" <> show (Fixture.scTermCount corpus),
          Run.benchRunPoint = \case
            PlainEnginePoint -> fmap (fmap pure) (runPlainEngineFloor corpus)
            ContextGridPoint contextCount -> runContextGridPoint contextCount corpus,
          Run.benchCsv = chimeraColumns,
          Run.benchCard = chimeraCard
        }

parseChimeraCommand :: [String] -> Either ChimeraCommandObstruction ChimeraCommand
parseChimeraCommand = \case
  [] -> Right RunComparativeCard
  ["--receipt", "support-driver-k128"] ->
    Right (RunSupportDriverReceipt SupportDriverK128)
  ["--receipt", "support-driver-k256"] ->
    Right (RunSupportDriverReceipt SupportDriverK256)
  unsupported -> Left (UnsupportedChimeraCommand unsupported)

renderChimeraCommandObstruction :: ChimeraCommandObstruction -> String
renderChimeraCommandObstruction = \case
  UnsupportedChimeraCommand unsupported ->
    "usage: chimera-scale-bench [--receipt support-driver-k128|support-driver-k256]; got "
      <> show unsupported

chimeraContextKernelBenchmarks :: [Benchmark]
chimeraContextKernelBenchmarks =
  [ Criterion.bgroup
      "equivalence-kernel"
      (fmap criterionEquivalenceKernelBench criterionContextCounts),
    Criterion.bgroup
      "representative-query-sweep"
      (fmap criterionRepresentativeQueryBench criterionContextCounts)
  ]

criterionContextCounts :: [Int]
criterionContextCounts =
  [128, 256]

criterionEquivalenceKernelBench :: Int -> Benchmark
criterionEquivalenceKernelBench contextCount =
  Criterion.env (prepareCriterionKernelFixture contextCount) $ \fixture ->
    Criterion.bench
      ("K=" <> show contextCount)
      (Criterion.nf criterionEquivalenceKernelDigest fixture)

criterionRepresentativeQueryBench :: Int -> Benchmark
criterionRepresentativeQueryBench contextCount =
  Criterion.env (prepareCriterionKernelFixture contextCount) $ \fixture ->
    Criterion.bench
      ("K=" <> show contextCount)
      (Criterion.nf criterionRepresentativeQueryDigest fixture)

prepareCriterionKernelFixture :: Int -> IO CriterionKernelFixture
prepareCriterionKernelFixture contextCount =
  either
    (ioError . userError . ("chimera Criterion fixture failed: " <>))
    pure
    $ do
      corpus <-
        Fixture.prepareScaleCorpus
          Chimera.tissueAnalysis
          (Population.tissueTermsAtDepth populationDepth populationSize)
      pointFixture <- preparePointFixture contextCount corpus
      semanticFixture <- prepareSemanticPointFixture pointFixture
      mergedGraph <- first show (buildOursMergedGraph pointFixture)
      deltaGraph <- first show (buildOursDeltaGraph pointFixture mergedGraph)
      pure
        CriterionKernelFixture
          { ckfPointFixture = pointFixture,
            ckfMergedContextQueries = spfMergedContextQueries semanticFixture,
            ckfDeltaContextQueries = spfDeltaContextQueries semanticFixture,
            ckfDeltaGraph = deltaGraph
          }

criterionEquivalenceKernelDigest :: CriterionKernelFixture -> Either String (Int, Int)
criterionEquivalenceKernelDigest fixture = do
  mergedGraph <- first show (buildOursMergedGraph (ckfPointFixture fixture))
  mergedDigest <-
    first show
      ( Digest.contextSemanticDigest
          tissueCountDigest
          (ckfMergedContextQueries fixture)
          mergedGraph
      )
  mergedDigest `seq` pure ()
  deltaGraph <- first show (buildOursDeltaGraph (ckfPointFixture fixture) mergedGraph)
  deltaDigest <-
    first show
      ( Digest.contextSemanticDigest
          tissueCountDigest
          (ckfDeltaContextQueries fixture)
          deltaGraph
      )
  pure (mergedDigest, deltaDigest)

criterionRepresentativeQueryDigest :: CriterionKernelFixture -> Either String Int
criterionRepresentativeQueryDigest fixture =
  first show
    ( Digest.contextQuotientDigest
        tissueCountDigest
        (ckfDeltaContextQueries fixture)
        (ckfDeltaGraph fixture)
    )

runPlainEngineFloor ::
  Fixture.ScaleCorpus Chimera.TissueF Chimera.TissueCount ->
  IO (Either Run.BenchFailure CsvRow)
runPlainEngineFloor corpus =
  Measure.samplePoint chimeraSamplePolicy probe corpus >>= traverse finish
  where
    probe =
      Measure.Probe
        { Measure.probeLabel = "plain engine round",
          Measure.probeRun = first show . runPlainEngine,
          Measure.probeDigest = ScaleDigest.plainReportDigest
        }
    runPlainEngine corpusValue =
      runPlainSaturation
        ( [Chimera.tissueCompatibilityFactRule],
          [ Chimera.graftCommuteRule,
            Chimera.graftAssociativityRule,
            Chimera.graftIdempotenceRule,
            Chimera.compatibleGraftReductionRule
          ]
        )
        (Fixture.scBaseGraph corpusValue)
    finish sampled = do
      let report = sampledValue sampled
          row =
            CsvRow
              { crRegister = BaselineRegister,
                crArm = PlainEngine,
                crContextCount = 1,
                crTermCount = Fixture.scTermCount corpus,
                crPhase = PlainEngineRound,
                crTermination = SaturationStopped (Saturation.srResult report),
                crWall = CellMeasured (Measure.sampledMedianNs sampled),
                crRegionalStructure = Nothing
              }
      putStrLn
        ( "# plain-engine-round K=1 matches-applied="
            <> show (Saturation.srMatchesApplied report)
            <> " iterations="
            <> show (Saturation.srIterations report)
        )
      pure row

runContextGridPoint ::
  Int ->
  Fixture.ScaleCorpus Chimera.TissueF Chimera.TissueCount ->
  IO (Either Run.BenchFailure [CsvRow])
runContextGridPoint contextCount corpus =
  either (pure . Left) runPrepared (preparePointFixture contextCount corpus)
  where
    runPrepared fixture =
      either (pure . Left) runValidated (prepareSemanticPointFixture fixture)
    runValidated semanticFixture =
      andThenBench (runOursPoint semanticFixture) $ \oursRows ->
        andThenBench (runProductPoint semanticFixture) $ \productRows ->
          andThenBench (runDriverPoint fixture) $ \driverRows ->
            fmap
              (fmap (\productDriverRows -> oursRows <> productRows <> driverRows <> productDriverRows))
              (runProductDriverPoint fixture)
      where
        fixture = spfPointFixture semanticFixture

runOursPoint :: SemanticPointFixture -> IO (Either Run.BenchFailure [CsvRow])
runOursPoint semanticFixture =
  andThenBench (Measure.samplePoint chimeraSamplePolicy mergeProbe fixture) $ \mergeSample ->
      let mergedGraph = fst (sampledValue mergeSample)
          mergeRow = measuredContextKernelRow fixture RegionScopedMerges mergedGraph mergeSample
       in andThenBench
            (Measure.samplePoint chimeraSamplePolicy deltaProbe (fixture, mergedGraph))
            ( \deltaSample ->
          let deltaGraph = fst (sampledValue deltaSample)
              deltaRow = measuredContextKernelRow fixture AuthoredDeltaPerRegion deltaGraph deltaSample
           in andThenBench
                (Measure.samplePoint chimeraSamplePolicy representativeQueryProbe deltaGraph)
                ( \representativeQuerySample ->
                    fmap
                      ( fmap
                          ( \descentSample ->
                              [ mergeRow,
                                deltaRow,
                                withRegionalStructure deltaGraph
                                  (measuredKernelRow OursContext fixture RepresentativeQuerySweep representativeQuerySample),
                                withRegionalStructure deltaGraph
                                  (measuredKernelRow OursContext fixture DescentPlusLiftTopCover descentSample)
                              ]
                          )
                      )
                      (Measure.samplePoint chimeraSamplePolicy descentProbe (fixture, deltaGraph))
                )
            )
  where
    fixture = spfPointFixture semanticFixture
    mergeProbe =
      Measure.Probe
        "ours region-scoped-merges"
        ( semanticContextSample
            (spfMergedContextQueries semanticFixture)
            buildOursMergedGraph
        )
        snd
    deltaProbe =
      Measure.Probe
        "ours authored-delta-per-region"
        ( semanticContextSample
            (spfDeltaContextQueries semanticFixture)
            (uncurry buildOursDeltaGraph)
        )
        snd
    representativeQueryProbe =
      Measure.Probe
        "ours representative-query-sweep"
        ( first show
            . Digest.contextQuotientDigest
              tissueCountDigest
              (spfDeltaContextQueries semanticFixture)
        )
        id
    descentProbe = Measure.Probe "ours descent-plus-lift-top-cover" (uncurry oursDescentLiftDigest) id

runProductPoint :: SemanticPointFixture -> IO (Either Run.BenchFailure [CsvRow])
runProductPoint semanticFixture = do
  budgetStart <- getMonotonicTimeNSec
  andThenBench
    (Measure.sampleWithinBudget chimeraSamplePolicy budgetStart productBudgetNs mergeProbe fixture)
    ( maybe
        ( pure
            ( Right
                [ timeoutRow ProductReplay fixture RegionScopedMerges,
                  timeoutRow ProductReplay fixture AuthoredDeltaPerRegion,
                  timeoutRow ProductReplay fixture RepresentativeQuerySweep
                ]
            )
        )
        ( \mergeSample ->
            let mergeGraphs = fst (sampledValue mergeSample)
                mergeRow = measuredKernelRow ProductReplay fixture RegionScopedMerges mergeSample
             in andThenBench
                  ( Measure.sampleWithinBudget
                      chimeraSamplePolicy
                      budgetStart
                      productBudgetNs
                      deltaProbe
                      (fixture, mergeGraphs)
                  )
                  (finishProductPoint budgetStart mergeRow)
        )
    )
  where
    fixture = spfPointFixture semanticFixture
    mergeProbe =
      Measure.Probe
        "product region-scoped-merges"
        ( semanticProductSample
            (spfProductComparisonQueries semanticFixture)
            productRegionScopedMerges
        )
        snd
    deltaProbe =
      Measure.Probe
        "product authored-delta-per-region"
        ( semanticProductSample
            (spfProductComparisonQueries semanticFixture)
            (uncurry productAuthoredDelta)
        )
        snd
    representativeQueryProbe =
      Measure.Probe
        "product representative-query-sweep"
        ( first show
            . Digest.productQuotientDigest
              tissueCountDigest
              (spfProductComparisonQueries semanticFixture)
        )
        id
    finishProductPoint _ mergeRow Nothing =
      pure
        ( Right
            [ mergeRow,
              timeoutRow ProductReplay fixture AuthoredDeltaPerRegion,
              timeoutRow ProductReplay fixture RepresentativeQuerySweep
            ]
        )
    finishProductPoint budgetStartValue mergeRow (Just deltaSample) =
      fmap
        ( fmap
            ( \maybeQuerySample ->
                [ mergeRow,
                  measuredKernelRow ProductReplay fixture AuthoredDeltaPerRegion deltaSample,
                  maybe
                    (timeoutRow ProductReplay fixture RepresentativeQuerySweep)
                    (measuredKernelRow ProductReplay fixture RepresentativeQuerySweep)
                    maybeQuerySample
                ]
            )
        )
        ( Measure.sampleWithinBudget
            chimeraSamplePolicy
            budgetStartValue
            productBudgetNs
            representativeQueryProbe
            (fst (sampledValue deltaSample))
        )

semanticContextSample ::
  Show failure =>
  [Digest.SemanticQuery Scale.ScaleContext] ->
  (input -> Either failure ChimeraContextGraph) ->
  input ->
  Either String (ChimeraContextGraph, Int)
semanticContextSample queries build input = do
  contextGraph <- first show (build input)
  semanticDigest <-
    first show
      (Digest.contextSemanticDigest tissueCountDigest queries contextGraph)
  pure (contextGraph, semanticDigest)

semanticProductSample ::
  [Digest.SemanticQuery Scale.ScaleContext] ->
  (input -> Either String (Map Scale.ScaleContext (EGraph Chimera.TissueF Chimera.TissueCount))) ->
  input ->
  Either String (Map Scale.ScaleContext (EGraph Chimera.TissueF Chimera.TissueCount), Int)
semanticProductSample queries build input = do
  productGraphs <- build input
  semanticDigest <-
    first show
      (Digest.productSemanticDigest tissueCountDigest queries productGraphs)
  pure (productGraphs, semanticDigest)

runDriverPoint :: PointFixture -> IO (Either Run.BenchFailure [CsvRow])
runDriverPoint fixture =
  andThenBench (measureDriver fixture DriverSetup (chimeraBudgetWithIterations 0)) $ \setupRow ->
    fmap
      (fmap (\roundRow -> [setupRow, roundRow]))
      (measureDriver fixture SupportSaturation (chimeraBudgetWithIterations 1))

measureDriver ::
  PointFixture ->
  Phase ->
  SaturationBudget ->
  IO (Either Run.BenchFailure CsvRow)
measureDriver fixture phase budget =
  case
    first show
      ( Driver.prepareSupportDriverFixture
          Saturation.emptyRewriteRuntimeCapabilities
          budget
          (pfScaleFixture fixture)
      )
  of
    Left preparationFailure -> pure (Left preparationFailure)
    Right preparedFixture ->
      Measure.samplePoint chimeraSamplePolicy probe preparedFixture >>= traverse finish
  where
    probe =
      Driver.supportDriverProbe ("production support driver " <> phaseLabel phase)
    finish sampled = do
      let report = sampledValue sampled
          termination = SaturationStopped (Saturation.srResult report)
          row =
            pointRow DriverRegister OursContext fixture phase termination
              (CellMeasured (Measure.sampledMedianNs sampled))
      putStrLn
        ( "# "
            <> phaseLabel phase
            <> " K="
            <> show (pointContextCount fixture)
            <> " matches-applied="
            <> show (Saturation.srMatchesApplied report)
            <> " iterations="
            <> show (Saturation.srIterations report)
            <> " termination="
            <> terminationLabel termination
        )
      pure row

runSupportDriverReceipt ::
  SupportDriverReceiptScale ->
  Fixture.ScaleCorpus Chimera.TissueF Chimera.TissueCount ->
  IO ()
runSupportDriverReceipt receiptScale corpus =
  case preparePointFixture (supportDriverReceiptContextCount receiptScale) corpus of
    Left fixtureFailure ->
      Run.abortBench ("support-driver receipt fixture: " <> fixtureFailure)
    Right fixture ->
      case
        first SupportDriverPreparationObstruction
          ( Driver.prepareSupportDriverFixture
              Saturation.emptyRewriteRuntimeCapabilities
              (chimeraBudgetWithIterations 1)
              (pfScaleFixture fixture)
          )
      of
        Left preparationFailure ->
          Run.abortBench ("support-driver receipt failed: " <> show preparationFailure)
        Right preparedFixture -> do
          measurementResult <-
            PaleMeasure.measureFreshSample
              1
              preparedFixture
              (measureSupportDriverObservation fixture)
              sdoSemanticDigest
          either
            (Run.abortBench . renderSupportDriverMeasurementFailure)
            ( putStr
                . Report.renderCsv supportDriverReceiptColumns
                . pure
                . supportDriverReceiptFromMeasurement receiptScale fixture
            )
            measurementResult

measureSupportDriverObservation ::
  PointFixture ->
  Driver.PreparedSupportFixture Chimera.TissueF Chimera.TissueCount ->
  IO (Either SupportDriverReceiptObstruction SupportDriverObservation)
measureSupportDriverObservation fixture preparedFixture = do
  observed <-
    Driver.runSupportDriverObserved preparedFixture
  pure (supportDriverObservationFromObserved fixture observed)

supportDriverObservationFromObserved ::
  PointFixture ->
  RuntimeEngine.RuntimeObservedResult ChimeraDriverFailure ChimeraScaleReport ->
  Either SupportDriverReceiptObstruction SupportDriverObservation
supportDriverObservationFromObserved fixture observed = do
  report <-
    first SupportDriverRuntimeObstruction
      (RuntimeEngine.rorResult observed)
  semanticDigest <-
    first SupportDriverSemanticObstruction
      (supportDriverSemanticDigest fixture report)
  pure
    SupportDriverObservation
      { sdoReport = report,
        sdoRuntimeTiming = RuntimeEngine.rorTiming observed,
        sdoSemanticDigest = semanticDigest
      }

supportDriverSemanticDigest ::
  PointFixture ->
  ChimeraScaleReport ->
  Either (Digest.SemanticDigestObstruction Scale.ScaleContext) Int
supportDriverSemanticDigest fixture report = do
  let contextGraph =
        SaturationState.sceContextGraph
          (Proof.pgGraph (Saturation.srCarrier report))
  semanticQueries <-
    Digest.deriveContextSemanticQueries
      (pfKeyedContexts fixture)
      contextGraph
  Digest.contextSemanticDigest
    tissueCountDigest
    semanticQueries
    contextGraph

supportDriverReceiptFromMeasurement ::
  SupportDriverReceiptScale ->
  PointFixture ->
  PaleMeasure.FreshMeasurement SupportDriverObservation ->
  SupportDriverReceipt
supportDriverReceiptFromMeasurement receiptScale fixture measurement =
  SupportDriverReceipt
    { sdrScale = receiptScale,
      sdrTermCount = Fixture.sfTermCount (pfScaleFixture fixture),
      sdrAdmissionVerdict = BaselineAdmissionAcceptedWithZeroRefusals,
      sdrTermination = Saturation.srResult report,
      sdrIterations = Support.srmIterations metrics,
      sdrMatchesApplied = Support.srmMatchesApplied metrics,
      sdrInitialNodeCount = Support.srmInitialNodeCount metrics,
      sdrFinalNodeCount = Support.srmFinalNodeCount metrics,
      sdrInitialClassCount = Support.srmInitialClassCount metrics,
      sdrFinalClassCount = Support.srmFinalClassCount metrics,
      sdrSemanticDigest = PaleMeasure.freshMeasurementDigest measurement,
      sdrAllocatedBytes = PaleMeasure.freshMeasurementAllocatedBytes measurement,
      sdrPeakLiveBytes = PaleMeasure.freshMeasurementPeakLiveBytes measurement,
      sdrWallNanoseconds = PaleMeasure.freshMeasurementElapsedNanoseconds measurement,
      sdrRoundBuildNanoseconds = RuntimeEngine.ritRoundBuildNanoseconds runtimeTiming,
      sdrApplyNanoseconds = RuntimeEngine.ritApplyNanoseconds runtimeTiming,
      sdrRebuildNanoseconds = RuntimeEngine.ritRebuildNanoseconds runtimeTiming,
      sdrCommitNanoseconds = RuntimeEngine.ritCommitNanoseconds runtimeTiming
    }
  where
    observation = PaleMeasure.freshMeasurementValue measurement
    report = sdoReport observation
    runtimeTiming = sdoRuntimeTiming observation
    metrics =
      Support.supportSaturationMetricsFromReport
        (SaturationSubstrate.proofGraphContext @ChimeraSupportU @())
        (Fixture.sfProofGraph (pfScaleFixture fixture))
        report

renderSupportDriverMeasurementFailure ::
  PaleMeasure.FreshMeasurementFailure SupportDriverReceiptObstruction ->
  String
renderSupportDriverMeasurementFailure = \case
  PaleMeasure.FreshMeasurementRtsStatsDisabled ->
    "support-driver receipt requires RTS statistics; rerun with +RTS -T -RTS"
  PaleMeasure.FreshMeasurementActionFailed obstruction ->
    "support-driver receipt failed: " <> show obstruction

supportDriverReceiptContextCount :: SupportDriverReceiptScale -> Int
supportDriverReceiptContextCount = \case
  SupportDriverK128 -> 128
  SupportDriverK256 -> 256

-- Dormant paper capability retained deliberately: the current 61-row grid
-- prices setup and one round, while this probe preserves the matched-budget
-- fixpoint experiment without creating a second driver implementation.
chimeraDriverFixpointProbe ::
  Measure.Probe
    (Driver.PreparedSupportFixture Chimera.TissueF Chimera.TissueCount)
    (ScaleDigest.ScaleReport Chimera.TissueF Chimera.TissueCount)
chimeraDriverFixpointProbe =
  Driver.supportDriverProbe "production support driver fixpoint"

runProductDriverPoint :: PointFixture -> IO (Either Run.BenchFailure [CsvRow])
runProductDriverPoint fixture =
  Measure.samplePoint chimeraSamplePolicy probe fixture >>= traverse finish
  where
    probe =
      Measure.Probe
        "product product-driver-round"
        (first show . runChimeraProductDriver)
        (sum . fmap ScaleDigest.plainReportDigest)
    finish sampled = do
      let reports = sampledValue sampled
          termination = SaturationStopped (driverTermination reports)
          row =
            pointRow DriverRegister ProductReplay fixture ProductDriverSaturation termination
              (CellMeasured (Measure.sampledMedianNs sampled))
      putStrLn (diagnostic termination reports)
      pure [row]
    driverTermination reports
      | any ((== HitNodeLimit) . Saturation.srResult) reports = HitNodeLimit
      | any ((== HitIterationLimit) . Saturation.srResult) reports = HitIterationLimit
      | any ((== ReachedGoal) . Saturation.srResult) reports = ReachedGoal
      | otherwise = ReachedFixedPoint
    diagnostic termination reports =
      "# "
        <> phaseLabel ProductDriverSaturation
        <> " K=" <> show (pointContextCount fixture)
        <> " engines=" <> show (length reports)
        <> " total-matches-applied=" <> show (sum (fmap Saturation.srMatchesApplied reports))
        <> " total-iterations=" <> show (sum (fmap Saturation.srIterations reports))
        <> " max-iterations=" <> show (foldr (max . Saturation.srIterations) 0 reports)
        <> " termination=" <> terminationLabel termination

sampledValue :: Measure.Sampled value -> value
sampledValue =
  Measure.timedSampleValue . Measure.sampledFirst
measuredContextKernelRow ::
  PointFixture ->
  Phase ->
  ChimeraContextGraph ->
  Measure.Sampled value ->
  CsvRow
measuredContextKernelRow fixture phase contextGraph sampled =
  withRegionalStructure contextGraph
    (measuredKernelRow OursContext fixture phase sampled)

withRegionalStructure :: ChimeraContextGraph -> CsvRow -> CsvRow
withRegionalStructure contextGraph row =
  row
    { crRegionalStructure =
        Just (Digest.contextRegionalStructureObservation contextGraph)
    }

measuredKernelRow :: Arm -> PointFixture -> Phase -> Measure.Sampled value -> CsvRow
measuredKernelRow arm fixture phase sampled =
  pointRow KernelRegister arm fixture phase NotSaturationRun (CellMeasured (Measure.sampledMedianNs sampled))

timeoutRow :: Arm -> PointFixture -> Phase -> CsvRow
timeoutRow arm fixture phase =
  pointRow KernelRegister arm fixture phase NotSaturationRun CellTimeout
pointRow :: Register -> Arm -> PointFixture -> Phase -> TerminationCell -> CellValue -> CsvRow
pointRow register arm fixture phase termination wall =
  CsvRow
    { crRegister = register,
      crArm = arm,
      crContextCount = pointContextCount fixture,
      crTermCount = Fixture.sfTermCount (pfScaleFixture fixture),
      crPhase = phase,
      crTermination = termination,
      crWall = wall,
      crRegionalStructure = Nothing
    }

pointContextCount :: PointFixture -> Int
pointContextCount =
  Scale.scaleSiteContextCount . Fixture.sfSite . pfScaleFixture
preparePointFixture ::
  Int ->
  Fixture.ScaleCorpus Chimera.TissueF Chimera.TissueCount ->
  Either Run.BenchFailure PointFixture
preparePointFixture contextCount corpus = do
  siteAndProbes@(_, probes) <- Fixture.buildScaleSite (Fixture.TreeSite contextCount)
  secondaryAnchor <-
    maybe (Left "scaled tree secondary probe missing") Right (Fixture.probeSecondary probes)
  let ruleSpecs =
        [ (Fixture.probeBottom probes, Chimera.graftCommuteRule),
          (Fixture.probePrimary probes, Chimera.graftAssociativityRule),
          (secondaryAnchor, Chimera.graftIdempotenceRule),
          (secondaryAnchor, Chimera.compatibleGraftReductionRule)
        ]
      factSpecs =
        [(Fixture.probePrimary probes, Chimera.tissueCompatibilityFactRule)]
  scaleFixture <-
    Fixture.prepareScaleFixture
      siteAndProbes
      Fixture.ScaleFixtureSpec
        { Fixture.sfsCorpus = Right corpus,
          Fixture.sfsRules = const (Right ruleSpecs),
          Fixture.sfsFacts = const (Right factSpecs)
        }
  let site = Fixture.sfSite scaleFixture
      contexts = NonEmpty.toList (Scale.scaleSiteContexts site)
      authoredContexts = filter (/= Fixture.probeBottom probes) contexts
  keyedContexts <-
    traverse
      ( \contextValue ->
          first
            (\lookupError -> "chimera context key: " <> show lookupError)
            ( fmap
                (\contextKey -> (contextKey, contextValue))
                (contextObjectKeyFor (cegSite (Fixture.sfContextGraph scaleFixture)) contextValue)
            )
      )
      contexts
  visibleTargets <-
    first (\lookupError -> "scaled tree visible targets: " <> show lookupError) $
      fmap Map.fromList $
        traverse
          ( \contextValue ->
              fmap ((,) contextValue) $
                supportReachableLatticeContexts
                  (Scale.scaleSiteLattice site)
                  (principalSupport contextValue)
          )
          authoredContexts
  mergeSteps <-
    maybe
      (Left "chimera-scale non-empty merge-pair cycle missing")
      (Right . zip authoredContexts . NonEmpty.toList . NonEmpty.cycle)
      (NonEmpty.nonEmpty (nonOverlappingPairs (Fixture.scClassIds corpus)))
  productRuleAssignments <-
    first (\lookupError -> "chimera product rule assignments: " <> show lookupError) $
      productRuleAssignmentsFor site factSpecs ruleSpecs
  pure
    PointFixture
      { pfScaleFixture = scaleFixture,
        pfKeyedContexts = keyedContexts,
        pfBaseQueryClasses = ClassId <$> IntMap.keys (eGraphAnalysis (Fixture.sfBaseGraph scaleFixture)),
        pfBottomCovers = [Fixture.probePrimary probes, secondaryAnchor],
        pfVisibleTargets = visibleTargets,
        pfMergeSteps = mergeSteps,
        pfDeltaTerms =
          zip
            authoredContexts
            ( Population.tissueTermAtDepth populationDepth
                <$> [ Fixture.scTermCount corpus
                      .. Fixture.scTermCount corpus + length authoredContexts - 1
                    ]
            ),
        pfProductRuleAssignments = productRuleAssignments
      }

prepareSemanticPointFixture :: PointFixture -> Either Run.BenchFailure SemanticPointFixture
prepareSemanticPointFixture fixture = do
  mergedContextGraph <- first show (buildOursMergedGraph fixture)
  deltaContextGraph <- first show (buildOursDeltaGraph fixture mergedContextGraph)
  mergedContextQueries <-
    first show
      ( Digest.deriveContextSemanticQueries
          (pfKeyedContexts fixture)
          mergedContextGraph
      )
  deltaContextQueries <-
    first show
      ( Digest.deriveContextSemanticQueries
          (pfKeyedContexts fixture)
          deltaContextGraph
      )
  productMergedGraphs <- productRegionScopedMerges fixture
  productDeltaGraphs <- productAuthoredDelta fixture productMergedGraphs
  let productComparisonQueries =
        Digest.semanticQueriesForClasses
          (pfKeyedContexts fixture)
          (pfBaseQueryClasses fixture)
  validateProductAgreement
    "merged"
    productComparisonQueries
    mergedContextGraph
    productMergedGraphs
  validateProductAgreement
    "delta"
    productComparisonQueries
    deltaContextGraph
    productDeltaGraphs
  mergedSemanticDigest <-
    first show
      (Digest.contextSemanticDigest tissueCountDigest mergedContextQueries mergedContextGraph)
  deltaSemanticDigest <-
    first show
      (Digest.contextSemanticDigest tissueCountDigest deltaContextQueries deltaContextGraph)
  productMergedSemanticDigest <-
    first show
      (Digest.productSemanticDigest tissueCountDigest productComparisonQueries productMergedGraphs)
  productDeltaSemanticDigest <-
    first show
      (Digest.productSemanticDigest tissueCountDigest productComparisonQueries productDeltaGraphs)
  mergedSemanticDigest
    `seq` deltaSemanticDigest
    `seq` productMergedSemanticDigest
    `seq` productDeltaSemanticDigest
    `seq` pure ()
  pure
    SemanticPointFixture
      { spfPointFixture = fixture,
        spfMergedContextQueries = mergedContextQueries,
        spfDeltaContextQueries = deltaContextQueries,
        spfProductComparisonQueries = productComparisonQueries
      }

validateProductAgreement ::
  String ->
  [Digest.SemanticQuery Scale.ScaleContext] ->
  ChimeraContextGraph ->
  Map Scale.ScaleContext (EGraph Chimera.TissueF Chimera.TissueCount) ->
  Either Run.BenchFailure ()
validateProductAgreement phase queries contextGraph productGraphs =
  do
    contextObservation <-
      first show
        (Digest.contextQuotientObservations tissueCountDigest queries contextGraph)
    productObservation <-
      first show
        (Digest.productQuotientObservations tissueCountDigest queries productGraphs)
    if contextObservation == productObservation
      then Right ()
      else Left ("chimera " <> phase <> " regional/product quotient mismatch")

tissueCountDigest :: Chimera.TissueCount -> Int
tissueCountDigest (Chimera.TissueCount countValue) =
  countValue

semanticDigestSensitivityLaw :: Either Run.BenchFailure ()
semanticDigestSensitivityLaw =
  case List.find ((== baselineDigest) . snd) perturbedDigests of
    Nothing -> Right ()
    Just (fieldName, _) ->
      Left ("chimera semantic digest is insensitive to " <> fieldName)
  where
    contextKey =
      ContextObjectKey 0
    baselineRegionalStructure =
      Digest.RegionalStructureObservation
        { Digest.regionalParentChildCount = 1,
          Digest.regionalParentEdgeCount = 1,
          Digest.regionalParentRegionCubeCount = 1,
          Digest.regionalVariantRowCount = 1,
          Digest.regionalAbsorbedRowCount = 1,
          Digest.regionalFingerprint = 101,
          Digest.regionalActiveAnalysisDeltaCount = 1,
          Digest.regionalAnalysisDeltaEntryCount = 1
        }
    baselineObservation =
      Digest.ContextSemanticObservation
        { Digest.contextObservedQuotients =
            [Digest.QuotientObservation contextKey [(1, 1)] [(1, Just 5)]],
          Digest.contextObservedVariantRows =
            [Digest.ContextRowObservation contextKey 0 1 [2]],
          Digest.contextObservedAbsorbedRows =
            [Digest.ContextRowObservation contextKey 1 2 [3]],
          Digest.contextObservedRegionalStructure = baselineRegionalStructure
        }
    baselineDigest =
      Digest.semanticObservationDigest baselineObservation
    perturbedDigests =
      fmap
        (fmap Digest.semanticObservationDigest)
        [ ( "regional representative answers",
            baselineObservation
              { Digest.contextObservedQuotients =
                  [Digest.QuotientObservation contextKey [(1, 2)] [(2, Just 5)]]
              }
          ),
          ( "context analysis answers",
            baselineObservation
              { Digest.contextObservedQuotients =
                  [Digest.QuotientObservation contextKey [(1, 1)] [(1, Just 6)]]
              }
          ),
          ( "semantic variant rows",
            baselineObservation
              { Digest.contextObservedVariantRows =
                  [Digest.ContextRowObservation contextKey 0 1 [4]]
              }
          ),
          ( "semantic absorbed rows",
            baselineObservation
              { Digest.contextObservedAbsorbedRows =
                  [Digest.ContextRowObservation contextKey 1 2 [4]]
              }
          ),
          ( "regional parent metrics",
            baselineObservation
              { Digest.contextObservedRegionalStructure =
                  baselineRegionalStructure
                    { Digest.regionalParentEdgeCount = 2
                    }
              }
          ),
          ( "regional structural fingerprint",
            baselineObservation
              { Digest.contextObservedRegionalStructure =
                  baselineRegionalStructure
                    { Digest.regionalFingerprint = 102
                    }
              }
          ),
          ( "active analysis-delta count",
            baselineObservation
              { Digest.contextObservedRegionalStructure =
                  baselineRegionalStructure
                    { Digest.regionalActiveAnalysisDeltaCount = 2
                    }
              }
          ),
          ( "analysis-delta entry count",
            baselineObservation
              { Digest.contextObservedRegionalStructure =
                  baselineRegionalStructure
                    { Digest.regionalAnalysisDeltaEntryCount = 2
                    }
              }
          )
        ]

productRuleAssignmentsFor ::
  Scale.ScaleSite ->
  [(Scale.ScaleContext, FactRule () Chimera.TissueF)] ->
  [(Scale.ScaleContext, RawRewriteRule (RewriteCondition () Chimera.TissueF) Chimera.TissueF)] ->
  Either (ContextLatticeLookupError Scale.ScaleContext) (Map Scale.ScaleContext PlainRuleAssignment)
productRuleAssignmentsFor site factSpecs ruleSpecs =
  assignSupported addFact reachableFrom emptyAssignments factSpecs
    >>= \withFacts -> assignSupported addRule reachableFrom withFacts ruleSpecs
  where
    emptyAssignments =
      Map.fromList
        [ (contextValue, ([], []))
        | contextValue <- NonEmpty.toList (Scale.scaleSiteContexts site)
        ]
    reachableFrom anchor =
      supportReachableLatticeContexts
        (Scale.scaleSiteLattice site)
        (principalSupport anchor)
    addFact factRule (factRules, rewriteRules) =
      (factRules <> [factRule], rewriteRules)
    addRule ruleValue (factRules, rewriteRules) =
      (factRules, rewriteRules <> [ruleValue])

assignSupported ::
  (value -> PlainRuleAssignment -> PlainRuleAssignment) ->
  (Scale.ScaleContext -> Either failure [Scale.ScaleContext]) ->
  Map Scale.ScaleContext PlainRuleAssignment ->
  [(Scale.ScaleContext, value)] ->
  Either failure (Map Scale.ScaleContext PlainRuleAssignment)
assignSupported inject reachable =
  foldM
    ( \assignments (anchor, value) ->
        reachable anchor
          >>= \targets ->
            pure (List.foldl' (\current context -> Map.adjust (inject value) context current) assignments targets)
    )

buildOursMergedGraph ::
  PointFixture ->
  Either (ContextDeltaError Chimera.TissueF Scale.ScaleContext) ChimeraContextGraph
buildOursMergedGraph fixture =
  foldM
    ( \graphValue (contextValue, (leftClass, rightClass)) ->
        contextMerge contextValue leftClass rightClass graphValue
    )
    ( emptyContextEGraph
        (Scale.scaleSiteLattice (Fixture.sfSite scaleFixture))
        (Fixture.sfBaseGraph scaleFixture)
    )
    (pfMergeSteps fixture)
  where
    scaleFixture =
      pfScaleFixture fixture

buildOursDeltaGraph ::
  PointFixture ->
  ChimeraContextGraph ->
  Either (ContextDeltaError Chimera.TissueF Scale.ScaleContext) ChimeraContextGraph
buildOursDeltaGraph fixture graph = do
  (_, deltaGraph) <-
    foldM
      ( \batch (contextValue, termValue) ->
          snd <$> stageTermAtContext contextValue termValue batch
      )
      (beginContextRebaseBatch graph)
      (pfDeltaTerms fixture)
      >>= commitContextRebaseBatch
  foldM
    ( \contextGraph (_, contextValue) ->
        first ContextSupportSiteFailed (activateContext contextValue contextGraph)
    )
    deltaGraph
    (pfKeyedContexts fixture)

oursDescentLiftDigest ::
  PointFixture ->
  ChimeraContextGraph ->
  Either String Int
oursDescentLiftDigest fixture contextGraph = do
  topContextKey <-
    maybe
      (Left "chimera top context key missing from prepared semantic cover")
      (Right . fst)
      (List.find ((== Scale.scaleSiteTop site) . snd) (pfKeyedContexts fixture))
  topQueryDigest <-
    first show
      ( Digest.contextQuotientDigest
          tissueCountDigest
          [ Digest.SemanticQuery
              topContextKey
              (Scale.scaleSiteTop site)
              (pfBaseQueryClasses fixture)
          ]
          contextGraph
      )
  pure
    ( Digest.searchVerdictDigest (descentAt (Scale.scaleSiteBottom site) contextGraph)
        + topQueryDigest
        + length (pfBottomCovers fixture)
    )
  where
    site =
      Fixture.sfSite (pfScaleFixture fixture)

productRegionScopedMerges :: PointFixture -> Either String (Map Scale.ScaleContext (EGraph Chimera.TissueF Chimera.TissueCount))
productRegionScopedMerges fixture =
  foldM
    (applyVisibleStep (\(leftClass, rightClass) -> Right . rebuild . merge leftClass rightClass) (pfVisibleTargets fixture))
    ( Map.fromList
        [ (contextValue, Fixture.sfBaseGraph scaleFixture)
        | contextValue <- NonEmpty.toList (Scale.scaleSiteContexts (Fixture.sfSite scaleFixture))
        ]
    )
    (pfMergeSteps fixture)
  where
    scaleFixture = pfScaleFixture fixture

applyVisibleStep ::
  (payload -> EGraph Chimera.TissueF Chimera.TissueCount -> Either String (EGraph Chimera.TissueF Chimera.TissueCount)) ->
  Map Scale.ScaleContext [Scale.ScaleContext] ->
  Map Scale.ScaleContext (EGraph Chimera.TissueF Chimera.TissueCount) ->
  (Scale.ScaleContext, payload) ->
  Either String (Map Scale.ScaleContext (EGraph Chimera.TissueF Chimera.TissueCount))
applyVisibleStep applyPayload visibleTargets graphs (contextValue, payload) =
  foldM
    ( \currentGraphs targetContext ->
        maybe
          (Right currentGraphs)
          (\graph -> fmap (\updatedGraph -> Map.insert targetContext updatedGraph currentGraphs) (applyPayload payload graph))
          (Map.lookup targetContext currentGraphs)
    )
    graphs
    (Map.findWithDefault [] contextValue visibleTargets)

productAuthoredDelta ::
  PointFixture ->
  Map Scale.ScaleContext (EGraph Chimera.TissueF Chimera.TissueCount) ->
  Either String (Map Scale.ScaleContext (EGraph Chimera.TissueF Chimera.TissueCount))
productAuthoredDelta fixture graphs =
  foldM
    (applyVisibleStep (\termValue graph -> first show (snd <$> addTerm termValue graph)) (pfVisibleTargets fixture))
    graphs
    (pfDeltaTerms fixture)

runPlainSaturation ::
  PlainRuleAssignment ->
  EGraph Chimera.TissueF Chimera.TissueCount ->
  Either (SaturationError ChimeraPlainU RewriteRuleId) ChimeraPlainReport
runPlainSaturation =
  runPlainSaturationWithBudget (chimeraBudgetWithIterations 1)

runPlainSaturationFixpoint ::
  PlainRuleAssignment ->
  EGraph Chimera.TissueF Chimera.TissueCount ->
  Either (SaturationError ChimeraPlainU RewriteRuleId) ChimeraPlainReport
runPlainSaturationFixpoint =
  runPlainSaturationWithBudget chimeraFixpointBudget

runPlainSaturationWithBudget ::
  SaturationBudget ->
  PlainRuleAssignment ->
  EGraph Chimera.TissueF Chimera.TissueCount ->
  Either (SaturationError ChimeraPlainU RewriteRuleId) ChimeraPlainReport
runPlainSaturationWithBudget budget (factRules, rewriteRules) =
  Saturation.saturateWithSchedulerRefinement
    identitySchedulerRefinement
    (Saturation.genericJoinSaturationConfig budget)
    factRules
    rewriteRules

runChimeraProductDriver ::
  PointFixture ->
  Either (SaturationError ChimeraPlainU RewriteRuleId) [ChimeraPlainReport]
runChimeraProductDriver fixture =
  traverse
    (`runPlainSaturation` Fixture.sfBaseGraph (pfScaleFixture fixture))
    (Map.elems (pfProductRuleAssignments fixture))

runChimeraProductDriverFixpoint ::
  PointFixture ->
  Either (SaturationError ChimeraPlainU RewriteRuleId) [ChimeraPlainReport]
runChimeraProductDriverFixpoint fixture =
  traverse
    (`runPlainSaturationFixpoint` Fixture.sfBaseGraph (pfScaleFixture fixture))
    (Map.elems (pfProductRuleAssignments fixture))

chimeraBudgetWithIterations :: Int -> SaturationBudget
chimeraBudgetWithIterations iterationBudget =
  SaturationBudget
    { sbMaxIterations = iterationBudget,
      sbMaxNodes = 50000
    }

supportDriverReceiptColumns :: Report.Table SupportDriverReceipt
supportDriverReceiptColumns =
  [ Report.Column "receipt" Report.AlignLeft (supportDriverReceiptScaleLabel . sdrScale),
    Report.Column "K" Report.AlignRight (show . supportDriverReceiptContextCount . sdrScale),
    Report.Column "N" Report.AlignRight (show . sdrTermCount),
    Report.Column "proof_retention" Report.AlignLeft (const "keep-no-proof"),
    Report.Column "baseline_refusal_count" Report.AlignRight (show . baselineAdmissionRefusalCount . sdrAdmissionVerdict),
    Report.Column "termination" Report.AlignLeft (terminationLabel . SaturationStopped . sdrTermination),
    Report.Column "iterations" Report.AlignRight (show . sdrIterations),
    Report.Column "matches_applied" Report.AlignRight (show . sdrMatchesApplied),
    Report.Column "initial_nodes" Report.AlignRight (show . sdrInitialNodeCount),
    Report.Column "final_nodes" Report.AlignRight (show . sdrFinalNodeCount),
    Report.Column "initial_classes" Report.AlignRight (show . sdrInitialClassCount),
    Report.Column "final_classes" Report.AlignRight (show . sdrFinalClassCount),
    Report.Column "semantic_digest" Report.AlignRight (show . sdrSemanticDigest),
    Report.Column "allocated_bytes" Report.AlignRight (show . sdrAllocatedBytes),
    Report.Column "peak_live_bytes" Report.AlignRight (show . sdrPeakLiveBytes),
    Report.Column "wall_ns" Report.AlignRight (show . sdrWallNanoseconds),
    Report.Column "round_build_ns" Report.AlignRight (show . sdrRoundBuildNanoseconds),
    Report.Column "apply_ns" Report.AlignRight (show . sdrApplyNanoseconds),
    Report.Column "rebuild_ns" Report.AlignRight (show . sdrRebuildNanoseconds),
    Report.Column "commit_ns" Report.AlignRight (show . sdrCommitNanoseconds)
  ]

supportDriverReceiptScaleLabel :: SupportDriverReceiptScale -> String
supportDriverReceiptScaleLabel = \case
  SupportDriverK128 -> "support-driver-k128"
  SupportDriverK256 -> "support-driver-k256"

baselineAdmissionRefusalCount :: BaselineAdmissionVerdict -> Int
baselineAdmissionRefusalCount = \case
  BaselineAdmissionAcceptedWithZeroRefusals -> 0

chimeraColumns :: Report.Table CsvRow
chimeraColumns =
  [ Report.Column "register" Report.AlignLeft (registerLabel . crRegister),
    Report.Column "arm" Report.AlignLeft (armLabel . crArm),
    Report.Column "K" Report.AlignRight (show . crContextCount),
    Report.Column "N" Report.AlignRight (show . crTermCount),
    Report.Column "phase" Report.AlignLeft (phaseLabel . crPhase),
    Report.Column "termination" Report.AlignLeft (terminationLabel . crTermination),
    Report.Column "wall_ms" Report.AlignRight (cellCsv . crWall),
    Report.Column "regional_parent_children" Report.AlignRight (regionalMetricCell Digest.regionalParentChildCount),
    Report.Column "regional_parent_edges" Report.AlignRight (regionalMetricCell Digest.regionalParentEdgeCount),
    Report.Column "regional_parent_cubes" Report.AlignRight (regionalMetricCell Digest.regionalParentRegionCubeCount),
    Report.Column "regional_variant_rows" Report.AlignRight (regionalMetricCell Digest.regionalVariantRowCount),
    Report.Column "regional_absorbed_rows" Report.AlignRight (regionalMetricCell Digest.regionalAbsorbedRowCount),
    Report.Column "regional_fingerprint" Report.AlignRight (regionalMetricCell Digest.regionalFingerprint),
    Report.Column "analysis_delta_contexts" Report.AlignRight (regionalMetricCell Digest.regionalActiveAnalysisDeltaCount),
    Report.Column "analysis_delta_entries" Report.AlignRight (regionalMetricCell Digest.regionalAnalysisDeltaEntryCount)
  ]

chimeraCard :: Report.Card CsvRow SummaryRow
chimeraCard =
  Report.Card
    { Report.cardVerdict = verdictLine,
      Report.cardSummarize = summaryRows,
      Report.cardTable = summaryColumns,
      Report.cardNotes =
        \rows ->
          [ "Kernel columns sum region-scoped merges and authored deltas only, so the two arms stay comparable. The product arm has no descent column: its per-context materialization IS the answer at every context, so descent-and-lift work exists only on the shared-substrate arm.",
            "Every construction sample is forced through the exact regional semantic surface: least base-canonical representatives, exact context analysis, semantic variant/absorbed rows, regional metrics/fingerprint, and active analysis-delta values. No eager contextual graph or overlay map participates. The separately reported representative-query sweep repeats the identical representative+analysis forcing used by the honest old-kernel receipt.",
            "Regional metric columns are populated from the authoritative post-phase regional cache. Product and driver rows report n/a rather than fabricating analogous storage. The card reports the post-delta edge/cube/row-form/fingerprint and analysis-delta totals so a timing win cannot hide fragmentation.",
            "The support-driver column is budgeted to a single iteration (sbMaxIterations = 1); iteration-limit termination is by construction, not a convergence failure. The driver runs the throughput posture (proof retention KeepNoProof): proofs are opt-in, and a consumer retaining them additionally pays per-match witness extraction.",
            "The driver-setup column runs the identical driver at a zero-iteration budget: plan compilation, book preparation, and per-context machinery without a matching round. Single-round minus setup is one round's marginal cost.",
            "The single-round product-driver column is the trivial instantiation of the same rule semantics: K independent plain-engine saturations on the identical corpus at the identical single-iteration budget, engine c running exactly the rules whose support reaches context c; the cell sums all K runs. Every shared match is re-derived at every context that sees it (the bench log prints the per-K re-derivation total), where the support driver applies each shared match once under region-annotated support. The ratio of the two driver columns is the brute-force verdict at this N: a mutable-engine port of the product arm divides its column by a per-operation constant, while the gap between the columns grows with K.",
            plainEngineLine rows
          ],
      Report.cardMissing = missingLine,
      Report.cardNext = Just . nextLine
    }

summaryColumns :: Report.Table SummaryRow
summaryColumns =
  [ Report.Column "K" Report.AlignRight (show . summaryContextCount),
    Report.Column "N" Report.AlignRight (const (show populationSize)),
    Report.Column "kernel ours ms" Report.AlignRight (summaryCell KernelRegister OursContext comparableKernelPhases),
    Report.Column "kernel product ms" Report.AlignRight (summaryCell KernelRegister ProductReplay comparableKernelPhases),
    Report.Column "ours query ms" Report.AlignRight (summaryCell KernelRegister OursContext [RepresentativeQuerySweep]),
    Report.Column "product query ms" Report.AlignRight (summaryCell KernelRegister ProductReplay [RepresentativeQuerySweep]),
    Report.Column "ours descent ms" Report.AlignRight (summaryCell KernelRegister OursContext [DescentPlusLiftTopCover]),
    Report.Column "regional edges" Report.AlignRight (summaryRegionalMetric Digest.regionalParentEdgeCount),
    Report.Column "regional cubes" Report.AlignRight (summaryRegionalMetric Digest.regionalParentRegionCubeCount),
    Report.Column "regional row forms" Report.AlignRight (summaryRegionalMetric (\metrics -> Digest.regionalVariantRowCount metrics + Digest.regionalAbsorbedRowCount metrics)),
    Report.Column "regional fingerprint" Report.AlignRight (summaryRegionalMetric Digest.regionalFingerprint),
    Report.Column "analysis delta contexts" Report.AlignRight (summaryRegionalMetric Digest.regionalActiveAnalysisDeltaCount),
    Report.Column "analysis delta entries" Report.AlignRight (summaryRegionalMetric Digest.regionalAnalysisDeltaEntryCount),
    Report.Column "support termination" Report.AlignLeftMarked summaryTermination,
    Report.Column "driver setup ms" Report.AlignRight (summaryCell DriverRegister OursContext [DriverSetup]),
    Report.Column "support driver (1 round) ms" Report.AlignRight (summaryCell DriverRegister OursContext [SupportSaturation]),
    Report.Column "product driver (K engines) ms" Report.AlignRight (summaryCell DriverRegister ProductReplay [ProductDriverSaturation])
  ]

registerLabel :: Register -> String
registerLabel = \case
    KernelRegister -> "kernel"
    DriverRegister -> "driver"
    BaselineRegister -> "baseline"
armLabel :: Arm -> String
armLabel = \case
    OursContext -> "ours-context"
    ProductReplay -> "product-replay"
    PlainEngine -> "plain-engine"
phaseLabel :: Phase -> String
phaseLabel = \case
    RegionScopedMerges -> "region-scoped-merges"
    AuthoredDeltaPerRegion -> "authored-delta-per-region"
    RepresentativeQuerySweep -> "representative-query-sweep"
    DescentPlusLiftTopCover -> "descent-plus-lift-top-cover"
    DriverSetup -> "driver-setup"
    SupportSaturation -> "production-support-driver"
    SupportSaturationFixpoint -> "production-support-driver-fixpoint"
    ProductDriverSaturation -> "product-driver-round"
    ProductDriverSaturationFixpoint -> "product-driver-fixpoint"
    PlainEngineRound -> "plain-engine-round"
terminationLabel :: TerminationCell -> String
terminationLabel = \case
    NotSaturationRun -> "n/a"
    SaturationStopped ReachedFixedPoint -> "fixed-point"
    SaturationStopped ReachedGoal -> "goal"
    SaturationStopped HitIterationLimit -> "iteration-limit"
    SaturationStopped HitNodeLimit -> "node-limit"
cellCsv :: CellValue -> String
cellCsv = \case
  CellMeasured nanoseconds -> Report.formatMillis nanoseconds
  CellTimeout -> "TIMEOUT"

regionalMetricCell :: (Digest.RegionalStructureObservation -> Int) -> CsvRow -> String
regionalMetricCell project =
  maybe "n/a" (show . project) . crRegionalStructure

verdictLine :: [CsvRow] -> String
verdictLine rows =
  case (productTimeoutCount rows, driverNodeLimitedCount rows) of
    (0, 0) ->
      "VERDICT: the kernel grid completed; every support-driver cell completed with its recorded termination."
    (timeoutCount, 0) ->
      "VERDICT: product-replay hit "
        <> show timeoutCount
        <> " TIMEOUT cells; every support-driver cell completed with its recorded termination."
    (0, nodeLimitedCount) ->
      "VERDICT: the kernel grid completed; "
        <> show nodeLimitedCount
        <> " support-driver points hit the node limit."
    (timeoutCount, nodeLimitedCount) ->
      "VERDICT: product-replay hit "
        <> show timeoutCount
        <> " TIMEOUT cells and "
        <> show nodeLimitedCount
        <> " support-driver points hit the node limit."

productTimeoutCount :: [CsvRow] -> Int
productTimeoutCount rows =
  length (filter isProductTimeout rows)
  where
    isProductTimeout row =
      crRegister row == KernelRegister && crArm row == ProductReplay && crWall row == CellTimeout

driverNodeLimitedCount :: [CsvRow] -> Int
driverNodeLimitedCount rows =
  length (filter isDriverNodeLimit rows)
  where
    isDriverNodeLimit row =
      crRegister row == DriverRegister && crTermination row == SaturationStopped HitNodeLimit

missingLine :: [CsvRow] -> String
missingLine rows =
  if expectedRowCount == length rows
    then "none"
    else "expected " <> show expectedRowCount <> " rows, found " <> show (length rows)
  where
    expectedRowCount =
      1 + length contextCounts * 10

plainEngineLine :: [CsvRow] -> String
plainEngineLine rows =
  case [row | row <- rows, crPhase row == PlainEngineRound] of
    [row] ->
      "Plain-engine floor: the TrivialContext base engine on the identical corpus and rules at the same single-iteration budget — no site, no support books, no proof annotations — takes "
        <> cellCsv (crWall row)
        <> " ms ("
        <> terminationLabel (crTermination row)
        <> "). It is K-free by construction and recorded once at K=1; driver round minus this floor is the per-round price of the context machinery."
    _ ->
      "Plain-engine floor row missing."

nextLine :: [CsvRow] -> String
nextLine rows =
  if productTimeoutCount rows == 0
    then "The regional transaction and production driver are sealed; extend the grid only when a new semantic workload dimension earns another row."
    else "Profile the first product TIMEOUT frontier against annotated-bucket emission and product replay fanout."

summaryRows :: [CsvRow] -> [SummaryRow]
summaryRows rows =
  fmap (\contextCount -> SummaryRow contextCount rows) contextCounts

summaryCell :: Register -> Arm -> [Phase] -> SummaryRow -> String
summaryCell register arm phases summary =
  totalCell (summarySourceRows summary) register arm phases (summaryContextCount summary)

summaryRegionalMetric ::
  (Digest.RegionalStructureObservation -> Int) ->
  SummaryRow ->
  String
summaryRegionalMetric project summary =
  case
      [ metrics
        | row <- summarySourceRows summary,
          crContextCount row == summaryContextCount summary,
          crArm row == OursContext,
          crPhase row == AuthoredDeltaPerRegion,
          Just metrics <- [crRegionalStructure row]
      ]
    of
      [metrics] -> show (project metrics)
      _ -> "n/a"

summaryTermination :: SummaryRow -> String
summaryTermination summary =
  driverTerminationCell (summarySourceRows summary) (summaryContextCount summary)

totalCell :: [CsvRow] -> Register -> Arm -> [Phase] -> Int -> String
totalCell rows register arm phases contextCount =
  case traverse measuredNs matchingRows of
    Just values
      | length values == length phases ->
          Report.formatMillis (sum values)
    _ ->
      "TIMEOUT"
  where
    matchingRows =
      [ crWall row
      | row <- rows,
        crRegister row == register,
        crArm row == arm,
        crContextCount row == contextCount,
        crTermCount row == populationSize,
        crPhase row `elem` phases
      ]

driverTerminationCell :: [CsvRow] -> Int -> String
driverTerminationCell rows contextCount =
  case
      [ crTermination row
      | row <- rows,
        crRegister row == DriverRegister,
        crContextCount row == contextCount,
        crTermCount row == populationSize,
        crPhase row == SupportSaturation
      ]
    of
    [termination] ->
      terminationLabel termination
    _ ->
      "missing"

measuredNs :: CellValue -> Maybe Word64
measuredNs = \case
  CellMeasured nanoseconds -> Just nanoseconds
  CellTimeout -> Nothing
