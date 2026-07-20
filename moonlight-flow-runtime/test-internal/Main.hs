module Main (main) where

import Moonlight.Flow.Runtime.CarrierDelta.FactorSpec qualified as FactorCarrierSpec
import Moonlight.Flow.Runtime.DiagnosticsDataflowSpec qualified as DiagnosticsDataflowSpec
import Moonlight.Flow.Runtime.GeneratedPatchSequenceSpec qualified as GeneratedPatchSequenceSpec
import Moonlight.Flow.Runtime.ModuleBoundarySpec qualified as ModuleBoundarySpec
import Moonlight.Flow.Runtime.ProjectionSoakSpec qualified as ProjectionSoakSpec
import Moonlight.Flow.Runtime.QuotientSourceSpec qualified as QuotientSourceSpec
import Moonlight.Flow.Runtime.ReplaySelectionSpec qualified as ReplaySelectionSpec
import Moonlight.Flow.Runtime.SchedulerLocalitySpec qualified as SchedulerLocalitySpec
import Moonlight.Flow.Runtime.SubsumptionSpec qualified as SubsumptionSpec
import Moonlight.Flow.Runtime.TopologyValidationSpec qualified as TopologyValidationSpec
import Test.Moonlight.Flow.Property.Debug.CacheEviction qualified as CacheEvictionSpec
import Test.Moonlight.Flow.Property.Debug.CompactionFrontier qualified as CompactionFrontierSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-flow-runtime-internal"
        [ FactorCarrierSpec.tests,
          QuotientSourceSpec.tests,
          GeneratedPatchSequenceSpec.tests,
          DiagnosticsDataflowSpec.tests,
          SchedulerLocalitySpec.tests,
          ModuleBoundarySpec.tests,
          ReplaySelectionSpec.tests,
          ProjectionSoakSpec.tests,
          TopologyValidationSpec.tests,
          SubsumptionSpec.tests,
          CompactionFrontierSpec.tests,
          CacheEvictionSpec.tests
        ]
    )
