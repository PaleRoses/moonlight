-- | Canonical production support-driver probe and fixed-point gate.
module Moonlight.EGraph.Bench.Harness.Driver
  ( DriverFailure,
    PreparedSupportFixture,
    prepareSupportDriverFixture,
    supportDriverProbe,
    runSupportDriverObserved,
    requireFixedPoint,
  )
where

import Data.Bifunctor (first)
import Moonlight.Core qualified as Core
import Moonlight.EGraph.Bench.Harness.Fixture qualified as Fixture
import Moonlight.EGraph.Bench.Harness.Measure (Probe (..))
import Moonlight.EGraph.Bench.Harness.ScaleDigest (ScaleReport, scaleReportDigest)
import Moonlight.EGraph.Pure.Saturation.Logic.Run qualified as Logic
import Moonlight.EGraph.Pure.Saturation.Matching qualified as Matching
import Moonlight.EGraph.Pure.Saturation.Substrate qualified as Substrate
import Moonlight.EGraph.Saturation.Context.State qualified as SaturationState
import Moonlight.EGraph.Test.Saturation qualified as Saturation
import Moonlight.EGraph.Test.Scale.Site qualified as Scale
import Moonlight.Rewrite.ProofContext qualified as Proof
import Moonlight.Rewrite.Runtime qualified as Runtime
import Moonlight.Rewrite.System qualified as Guard
import Moonlight.Saturation.Context.Error qualified as Error
import Moonlight.Saturation.Context.Driver qualified as ContextDriver
import Moonlight.Saturation.Context.Program.Plan (Plan)
import Moonlight.Saturation.Context.Program.Spec qualified as Program
import Moonlight.Saturation.Context.Runtime.Engine qualified as RuntimeEngine
import Moonlight.Saturation.Support.Core qualified as Support

type DriverGraph f analysis =
  Substrate.EGraphU () f analysis Scale.ScaleContext

type DriverFailure f analysis =
  Error.SaturationError
    (DriverGraph f analysis)
    (Support.SupportScheduleGroup (DriverGraph f analysis))

data PreparedSupportFixture f analysis = PreparedSupportFixture
  { preparedSupportPlan ::
      !( Plan
           (DriverGraph f analysis)
           (SaturationState.SaturatingProofEGraph () f analysis Scale.ScaleContext ())
           (Support.SupportScheduleGroup (DriverGraph f analysis))
       ),
    preparedSupportCarrier ::
      !(SaturationState.SaturatingProofEGraph () f analysis Scale.ScaleContext ())
  }

prepareSupportDriverFixture ::
  ( Logic.EGraphLogicConstraints () f analysis Scale.ScaleContext,
    Core.ZipMatch f
  ) =>
  Runtime.RewriteRuntimeCapabilities (Guard.GuardCapabilityResolver ()) f ->
  Saturation.SaturationBudget ->
  Fixture.ScaleFixture f analysis ->
  Either (DriverFailure f analysis) (PreparedSupportFixture f analysis)
prepareSupportDriverFixture capabilities budget fixture = do
  supportPlan <-
    Saturation.prepareEGraphSupportPlan
      Nothing
      (const (Program.staticRewriteContextSnapshot capabilities))
      ( Program.withSchedulerConfig
          Saturation.deterministicSchedulerConfig
          (Program.planSpec budget Matching.GenericJoinMatching capabilities)
      )
      (Fixture.sfRuleBook fixture)
      (Fixture.sfFactBook fixture)
      (Fixture.sfProofGraph fixture)
  pure
    PreparedSupportFixture
      { preparedSupportPlan = supportPlan,
        preparedSupportCarrier = Fixture.sfProofGraph fixture
      }

supportDriverProbe ::
  ( Logic.EGraphLogicConstraints () f analysis Scale.ScaleContext,
    Core.ZipMatch f,
    Show (DriverFailure f analysis)
  ) =>
  String ->
  Probe (PreparedSupportFixture f analysis) (ScaleReport f analysis)
supportDriverProbe label =
  Probe
    { probeLabel = label,
      probeRun = \preparedFixture ->
        first show $
          ContextDriver.crrResult
            <$> Saturation.runEGraphSupportPlan
              Proof.defaultProofAnnotationBuilder
              mempty
              (preparedSupportPlan preparedFixture)
              (preparedSupportCarrier preparedFixture),
      probeDigest = scaleReportDigest
    }

runSupportDriverObserved ::
  ( Logic.EGraphLogicConstraints () f analysis Scale.ScaleContext,
    Core.ZipMatch f
  ) =>
  PreparedSupportFixture f analysis ->
  IO
    ( RuntimeEngine.RuntimeObservedResult
        (DriverFailure f analysis)
        (ScaleReport f analysis)
    )
runSupportDriverObserved preparedFixture =
  fmap
    (RuntimeEngine.mapRuntimeObservedResult (fmap ContextDriver.crrResult))
    ( Saturation.runEGraphSupportPlanObserved
        Proof.defaultProofAnnotationBuilder
        mempty
        (preparedSupportPlan preparedFixture)
        (preparedSupportCarrier preparedFixture)
    )

requireFixedPoint :: Probe input (ScaleReport f analysis) -> Probe input (ScaleReport f analysis)
requireFixedPoint probe =
  probe
    { probeRun = \input -> do
        report <- probeRun probe input
        case Saturation.srResult report of
          Saturation.ReachedFixedPoint -> Right report
          termination -> Left ("did not reach fixed point: " <> show termination)
    }
