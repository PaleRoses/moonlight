module Moonlight.EGraph.Bench.Scale.SDF
  ( main,
  )
where

import Data.Bifunctor (first)
import Data.Fix (Fix)
import Data.Word (Word64)
import Moonlight.EGraph.Bench.Harness.Driver qualified as Driver
import Moonlight.EGraph.Bench.Harness.Fixture qualified as Fixture
import Moonlight.EGraph.Bench.Harness.Measure qualified as Measure
import Moonlight.EGraph.Bench.Harness.Report qualified as Report
import Moonlight.EGraph.Bench.Harness.Run qualified as Run
import Moonlight.EGraph.Bench.Harness.ScaleDigest qualified as ScaleDigest
import Moonlight.EGraph.Test.Saturation (emptyRewriteRuntimeCapabilities)
import Moonlight.EGraph.Test.Scale.Site qualified as Scale
import Moonlight.EGraph.Test.SDF.Core qualified as SDF
import Moonlight.Saturation.Core (SaturationBudget (..))

newtype PopulationSize = PopulationSize
  { populationSizeValue :: Int
  }

data CsvRow = CsvRow
  { crContextCount :: !Int,
    crPopulationSize :: !PopulationSize,
    crTermCount :: !Int,
    crWallNanoseconds :: !Word64
  }

populationSizes :: [PopulationSize]
populationSizes =
  fmap PopulationSize [64, 128, 256, 512, 1024, 2048]

main :: IO ()
main =
  either
    Run.abortBench
    (Run.runScaleBench . sdfBench)
    (Fixture.buildScaleSite (Fixture.DiamondStackSite 16))

sdfBench ::
  (Scale.ScaleSite, Fixture.ScaleProbes) ->
  Run.ScaleBench PopulationSize CsvRow CsvRow
sdfBench siteAndProbes@(site, _) =
  Run.ScaleBench
    { Run.benchName = "sdf-scale",
      Run.benchReproCommand = "cd compiler && cabal bench moonlight-egraph:sdf-scale-bench -j1",
      Run.benchPoints = populationSizes,
      Run.benchAnnounce = \populationSize ->
        "sdf-scale K="
          <> show (Scale.scaleSiteContextCount site)
          <> " N="
          <> show (populationSizeValue populationSize),
      Run.benchRunPoint = runSdfPoint siteAndProbes,
      Run.benchCsv = sdfColumns,
      Run.benchCard = sdfCard
    }

runSdfPoint ::
  (Scale.ScaleSite, Fixture.ScaleProbes) ->
  PopulationSize ->
  IO (Either Run.BenchFailure [CsvRow])
runSdfPoint siteAndProbes populationSize =
  case Fixture.prepareScaleFixture siteAndProbes (sdfSpec populationSize) of
    Left failure -> pure (Left failure)
    Right fixture ->
      case
        first show
          ( Driver.prepareSupportDriverFixture
              emptyRewriteRuntimeCapabilities
              sdfBudget
              fixture
          )
      of
        Left failure -> pure (Left failure)
        Right preparedFixture ->
          fmap
            (fmap (pure . sdfRow populationSize fixture))
            ( Measure.samplePoint
                Measure.fixedThreeSamplePolicy
                (Measure.digestOnlyProbe sdfProbe)
                preparedFixture
            )

sdfProbe ::
  Measure.Probe
    (Driver.PreparedSupportFixture SDF.SDFF SDF.Depth)
    (ScaleDigest.ScaleReport SDF.SDFF SDF.Depth)
sdfProbe =
  Driver.requireFixedPoint
    (Driver.supportDriverProbe "sdf support driver")

sdfBudget :: SaturationBudget
sdfBudget =
  SaturationBudget
    { sbMaxIterations = 8,
      sbMaxNodes = 250000
    }

sdfSpec :: PopulationSize -> Fixture.ScaleFixtureSpec SDF.SDFF SDF.Depth
sdfSpec populationSize =
  Fixture.ScaleFixtureSpec
    { Fixture.sfsCorpus =
        Fixture.prepareScaleCorpus SDF.depthAnalysis (sdfFixtureTerms populationSize),
      Fixture.sfsRules = \probes -> do
        secondaryProbe <-
          maybe
            (Left "sdf-scale: scaled diamond-stack secondary probe missing")
            Right
            (Fixture.probeSecondary probes)
        pure
          ( fmap
              ((,) (Fixture.probeBottom probes) . SDF.sdfRawRewriteRule)
              (SDF.sdfLatticeLaws <> SDF.sdfComplementLaws <> SDF.sdfSmoothBlendLaws)
              <> [(secondaryProbe, SDF.sdfCoarseApproximationRule)]
          ),
      Fixture.sfsFacts = \probes ->
        Right [(Fixture.probePrimary probes, SDF.nonDegenerateRadiusFactRule)]
    }

sdfFixtureTerms :: PopulationSize -> [Fix SDF.SDFF]
sdfFixtureTerms populationSize =
  [ SDF.smoothUnion 0.5 (SDF.sphere 2.0) (SDF.box 1.0 2.0 3.0),
    SDF.sdfUnion (SDF.sphere 2.0) (SDF.box 1.0 2.0 3.0),
    SDF.sdfUnion (SDF.sphere 3.0) SDF.sdfEmpty,
    SDF.sphere 3.0
  ]
    <> sdfPopulation populationSize

sdfPopulation :: PopulationSize -> [Fix SDF.SDFF]
sdfPopulation (PopulationSize termCount) =
  let perDepthCount = (termCount + 1) `div` 2
   in take termCount $
        SDF.seededSDFTerms 1103 1 perDepthCount
          <> SDF.seededSDFTerms 2909 2 perDepthCount

sdfRow ::
  PopulationSize ->
  Fixture.ScaleFixture SDF.SDFF SDF.Depth ->
  Measure.Sampled value ->
  CsvRow
sdfRow populationSize fixture sampled =
  CsvRow
    { crContextCount = Scale.scaleSiteContextCount (Fixture.sfSite fixture),
      crPopulationSize = populationSize,
      crTermCount = Fixture.sfTermCount fixture,
      crWallNanoseconds = Measure.sampledMedianNs sampled
    }

sdfColumns :: Report.Table CsvRow
sdfColumns =
  [ Report.Column "register" Report.AlignLeft (const "driver"),
    Report.Column "context_count" Report.AlignRight (show . crContextCount),
    Report.Column "population_size" Report.AlignRight (show . populationSizeValue . crPopulationSize),
    Report.Column "term_count" Report.AlignRight (show . crTermCount),
    Report.Column "phase" Report.AlignLeft (const "support-saturation"),
    Report.Column "wall_ms" Report.AlignRight (Report.formatMillis . crWallNanoseconds)
  ]

sdfCard :: Report.Card CsvRow CsvRow
sdfCard =
  Report.Card
    { Report.cardVerdict = sdfVerdict,
      Report.cardSummarize = id,
      Report.cardTable =
        [ Report.Column "K" Report.AlignRight (show . crContextCount),
          Report.Column "population" Report.AlignRight (show . populationSizeValue . crPopulationSize),
          Report.Column "terms" Report.AlignRight (show . crTermCount),
          Report.Column "support driver ms" Report.AlignRight (Report.formatMillis . crWallNanoseconds)
        ],
      Report.cardNotes = const [],
      Report.cardMissing = sdfMissing,
      Report.cardNext = const Nothing
    }

sdfVerdict :: [CsvRow] -> String
sdfVerdict [] =
  "VERDICT: production support-driver saturation produced no SDF population rows."
sdfVerdict rows@(firstRow : remainingRows)
  | all ((== crContextCount firstRow) . crContextCount) remainingRows =
      "VERDICT: production support-driver saturation completed the "
        <> show (crContextCount firstRow)
        <> "-context SDF population grid."
  | otherwise =
      "VERDICT: production support-driver saturation completed the SDF population grid at context counts "
        <> show (fmap crContextCount rows)
        <> "."

sdfMissing :: [CsvRow] -> String
sdfMissing rows
  | length rows == length populationSizes = "none"
  | otherwise =
      "expected "
        <> show (length populationSizes)
        <> " rows, found "
        <> show (length rows)
