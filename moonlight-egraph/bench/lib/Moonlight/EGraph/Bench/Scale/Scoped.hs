module Moonlight.EGraph.Bench.Scale.Scoped
  ( main,
  )
where

import Data.Bifunctor (first)
import Data.Fix (Fix)
import Data.Word (Word64)
import Moonlight.Core
  ( BinderId (..),
    RewriteRuleId (..),
  )
import Moonlight.EGraph.Bench.Harness.Driver
  ( PreparedSupportFixture,
    prepareSupportDriverFixture,
    requireFixedPoint,
    supportDriverProbe,
  )
import Moonlight.EGraph.Bench.Harness.Fixture
  ( ScaleFixtureSpec (..),
    ScaleProbes (..),
    SiteSpec (..),
    buildScaleSite,
    prepareScaleCorpus,
    prepareScaleFixture,
  )
import Moonlight.EGraph.Bench.Harness.Measure
  ( Probe,
    Sampled,
    digestOnlyProbe,
    fixedThreeSamplePolicy,
    samplePoint,
    sampledMedianNs,
  )
import Moonlight.EGraph.Bench.Harness.Report
  ( Align (..),
    Card (..),
    Column (..),
    Table,
    formatMillis,
  )
import Moonlight.EGraph.Bench.Harness.Run
  ( BenchFailure,
    ScaleBench (..),
    runScaleBench,
  )
import Moonlight.EGraph.Bench.Harness.ScaleDigest (ScaleReport)
import Moonlight.EGraph.Test.Saturation (emptyRewriteRuntimeCapabilities)
import Moonlight.EGraph.Test.Scoped.Core
  ( ScopedF,
    scopedAnalysisSpec,
    scopedApp,
    scopedBetaRule,
    scopedBinderIndependentFactRule,
    scopedBinderSubstAlgebra,
    scopedEtaContractum,
    scopedEtaRedex,
    scopedFactGatedEtaRule,
    scopedFree,
    scopedLam,
    scopedLocal,
    scopedLocalEtaRule,
  )
import Moonlight.Rewrite.Runtime
  ( RewriteRuntimeCapabilities,
    withRuntimeBinderSubstAlgebra,
  )
import Moonlight.Rewrite.System (GuardCapabilityResolver)
import Moonlight.Rewrite.System (FactRuleId (..))
import Moonlight.Saturation.Core (SaturationBudget (..))

data ScalePoint
  = DeepChain !Int
  | WideTree !Int
  deriving stock (Eq)

data CsvRow = CsvRow
  { crPoint :: !ScalePoint,
    crTermCount :: !Int,
    crWallNanoseconds :: !Word64
  }

scalePoints :: [ScalePoint]
scalePoints =
  [DeepChain, WideTree] <*> [8, 16, 32, 64, 128, 256]

scopedTerms :: [Fix ScopedF]
scopedTerms =
  [ scopedEtaRedex inlineBinder "inline",
    scopedEtaContractum "inline",
    scopedEtaRedex pureBinder "pure",
    scopedEtaContractum "pure"
  ]
    <> fmap scopedBetaPopulationTerm [0 .. 63]

main :: IO ()
main =
  runScaleBench
    ScaleBench
      { benchName = "scoped-scale",
        benchReproCommand = "cd compiler && cabal bench moonlight-egraph:scoped-scale-bench -j1",
        benchPoints = scalePoints,
        benchAnnounce = \point ->
          scopedPointLabel point <> " N=" <> show (length scopedTerms),
        benchRunPoint = runScalePoint,
        benchCsv = scopedColumns,
        benchCard = scopedCard
      }

runScalePoint :: ScalePoint -> IO (Either BenchFailure [CsvRow])
runScalePoint point =
  either
    (pure . Left)
    ( fmap (fmap (pure . rowFromSample point))
        . samplePoint fixedThreeSamplePolicy (digestOnlyProbe scopedProbe)
    )
    ( first
        (\failure -> scopedPointLabel point <> ": " <> failure)
        ( do
            siteAndProbes <- buildScaleSite (siteSpecForPoint point)
            fixture <- prepareScaleFixture siteAndProbes scopedFixtureSpec
            first show
              (prepareSupportDriverFixture scopedCapabilities scopedBudget fixture)
        )
    )

siteSpecForPoint :: ScalePoint -> SiteSpec
siteSpecForPoint (DeepChain requestedContextCount) = ChainSite requestedContextCount
siteSpecForPoint (WideTree requestedContextCount) = TreeSite requestedContextCount

scopedFixtureSpec :: ScaleFixtureSpec ScopedF ()
scopedFixtureSpec =
  ScaleFixtureSpec
    { sfsCorpus = prepareScaleCorpus scopedAnalysisSpec scopedTerms,
      sfsRules = \probes ->
        Right
          [ (probeBottom probes, scopedBetaRule (RewriteRuleId 0) globalBinder),
            (probePrimary probes, scopedLocalEtaRule (RewriteRuleId 1) inlineBinder "inline"),
            (probePrimary probes, scopedFactGatedEtaRule (RewriteRuleId 2) pureBinder)
          ],
      sfsFacts = \probes ->
        Right
          [ ( probePrimary probes,
              scopedBinderIndependentFactRule (FactRuleId 0) "pure"
            )
          ]
    }

scopedBetaPopulationTerm :: Int -> Fix ScopedF
scopedBetaPopulationTerm termIndex =
  scopedApp
    ( scopedLam
        globalBinder
        ( scopedApp
            (scopedFree ("function-" <> show termIndex))
            (scopedLocal globalBinder)
        )
    )
    (scopedFree ("argument-" <> show termIndex))

globalBinder, inlineBinder, pureBinder :: BinderId
globalBinder = BinderId 0
inlineBinder = BinderId 1
pureBinder = BinderId 2

scopedProbe :: Probe (PreparedSupportFixture ScopedF ()) (ScaleReport ScopedF ())
scopedProbe =
  requireFixedPoint
    (supportDriverProbe "scoped support driver")

scopedCapabilities :: RewriteRuntimeCapabilities (GuardCapabilityResolver ()) ScopedF
scopedCapabilities =
  withRuntimeBinderSubstAlgebra
    scopedBinderSubstAlgebra
    emptyRewriteRuntimeCapabilities

scopedBudget :: SaturationBudget
scopedBudget =
  SaturationBudget
    { sbMaxIterations = 12,
      sbMaxNodes = 100000
    }

rowFromSample ::
  ScalePoint ->
  Sampled Int ->
  CsvRow
rowFromSample point sampled =
  CsvRow
    { crPoint = point,
      crTermCount = length scopedTerms,
      crWallNanoseconds = sampledMedianNs sampled
    }

scopedColumns :: Table CsvRow
scopedColumns =
  [ Column "register" AlignLeft (const "driver"),
    Column "shape" AlignLeft (shapeLabel . crPoint),
    Column "context_count" AlignRight (show . contextCount . crPoint),
    Column "term_count" AlignRight (show . crTermCount),
    Column "phase" AlignLeft (const "support-saturation"),
    Column "wall_ms" AlignRight (formatMillis . crWallNanoseconds)
  ]

scopedCard :: Card CsvRow CsvRow
scopedCard =
  Card
    { cardVerdict = scopedVerdict,
      cardSummarize = id,
      cardTable =
        [ Column "shape" AlignLeft (shapeLabel . crPoint),
          Column "K" AlignRight (show . contextCount . crPoint),
          Column "N" AlignRight (show . crTermCount),
          Column "support driver ms" AlignRight (formatMillis . crWallNanoseconds)
        ],
      cardNotes = const [],
      cardMissing = scopedMissing,
      cardNext = const Nothing
    }

scopedVerdict :: [CsvRow] -> String
scopedVerdict rows =
  if scopedGridComplete rows
    then "VERDICT: production support-driver saturation completed the deep-chain and wide-tree scoped grid."
    else "VERDICT: production support-driver saturation did not complete the scoped grid."

scopedMissing :: [CsvRow] -> String
scopedMissing rows =
  if scopedGridComplete rows
    then "none"
    else
      unwords
        [ "expected",
          show (length scalePoints),
          "rows, found",
          show (length rows)
        ]

scopedGridComplete :: [CsvRow] -> Bool
scopedGridComplete rows =
  fmap crPoint rows == scalePoints

scopedPointLabel :: ScalePoint -> String
scopedPointLabel point =
  "scoped-scale shape="
    <> shapeLabel point
    <> " K="
    <> show (contextCount point)

shapeLabel :: ScalePoint -> String
shapeLabel (DeepChain _) = "deep-chain"
shapeLabel (WideTree _) = "wide-tree"

contextCount :: ScalePoint -> Int
contextCount (DeepChain count) = count
contextCount (WideTree count) = count
