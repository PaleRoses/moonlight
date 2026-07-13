module Main (main) where

import BatchDenseSpec qualified as BatchDenseSpec
import CircuitSpec qualified as CircuitSpec
import RowsCacheSpec qualified as RowsCacheSpec
import Moonlight.Differential.Effect.Laws qualified as DifferentialLaws
import AlgebraSpec qualified as AlgebraLawSpec
import ArrangementSpec qualified as ArrangementLawSpec
import BatchTraceSpec qualified as BatchTraceLawSpec
import CollectionSpec qualified as CollectionLawSpec
import DeltaSpec qualified as DeltaLawSpec
import IndexSpec qualified as IndexLawSpec
import OperatorSpec qualified as OperatorLawSpec
import SupportedViewSpec qualified as SupportedViewSpec
import ScheduleSpec qualified as ScheduleSpec
import ProjectionSpec qualified as ProjectionLawSpec
import RowSpec qualified as RowBlockSpec
import StreamSpec qualified as StreamLawSpec
import TimeFrontierSpec qualified as TimeFrontierLawSpec
import WCOJDeltaSpec qualified as WCOJDeltaSpec
import WCOJSpec qualified as WCOJLawSpec
import TraceCompactionLawSpec qualified as TraceCompactionLawSpec
import Test.Tasty
  ( TestTree,
    defaultMain,
    testGroup,
  )

main :: IO ()
main =
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "moonlight-differential-runtime"
    [ DifferentialLaws.tests,
      AlgebraLawSpec.tests,
      BatchTraceLawSpec.tests,
      TraceCompactionLawSpec.tests,
      RowsCacheSpec.tests,
      StreamLawSpec.tests,
      DeltaLawSpec.tests,
      CollectionLawSpec.tests,
      OperatorLawSpec.tests,
      TimeFrontierLawSpec.tests,
      ArrangementLawSpec.tests,
      IndexLawSpec.tests,
      RowBlockSpec.tests,
      WCOJLawSpec.tests,
      WCOJDeltaSpec.tests,
      SupportedViewSpec.tests,
      ScheduleSpec.tests,
      ProjectionLawSpec.tests,
      CircuitSpec.tests,
      BatchDenseSpec.tests
    ]
