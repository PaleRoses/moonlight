-- | E-graph scale report digests.
module Moonlight.EGraph.Bench.Harness.ScaleDigest
  ( ScaleReport,
    scaleReportDigest,
    plainReportDigest,
  )
where

import Moonlight.EGraph.Bench.Harness.Digest
  ( contextGraphDigest,
    graphDigest,
    saturationTerminationDigest,
  )
import Moonlight.EGraph.Pure.Context.Proof qualified as Proof
import Moonlight.EGraph.Saturation.Context.State qualified as SaturationState
import Moonlight.EGraph.Pure.Saturation.Logic.Run qualified as Logic
import Moonlight.EGraph.Pure.Saturation.Substrate qualified as Substrate
import Moonlight.EGraph.Test.Saturation qualified as Saturation
import Moonlight.EGraph.Test.Scale.Site qualified as Scale
import Moonlight.Saturation.Substrate qualified as Saturation
import Moonlight.Saturation.Support.Core qualified as Support

type ScaleReport owner f analysis =
  Support.SupportSaturationReportFor
    (Substrate.EGraphU owner () f analysis Scale.ScaleContext)
    (SaturationState.SaturatingProofEGraph owner () f analysis Scale.ScaleContext ())

scaleReportDigest :: ScaleReport owner f analysis -> Int
scaleReportDigest report =
  contextGraphDigest
    ( SaturationState.sceContextGraph
        (Proof.pgGraph (Saturation.srCarrier report))
    )
    + Saturation.srIterations report
    + Saturation.srMatchesApplied report
    + saturationTerminationDigest (Saturation.srResult report)

plainReportDigest ::
  Logic.EGraphLogicConstraints () f analysis Saturation.TrivialContext =>
  Saturation.EGraphSaturationReport () f analysis ->
  Int
plainReportDigest report =
  graphDigest (Saturation.saturationReportBaseGraph report)
    + Saturation.srIterations report
    + Saturation.srMatchesApplied report
    + saturationTerminationDigest (Saturation.srResult report)
