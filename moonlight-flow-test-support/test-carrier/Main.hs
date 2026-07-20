module Main (main) where

import Moonlight.Flow.Carrier.Morphism.AmalgamationSpec qualified as CarrierAmalgamateSpec
import Moonlight.Flow.Carrier.Boundary.CoverageAtomRowsSpec qualified as BoundaryCoverageAtomRowsSpec
import Moonlight.Flow.Carrier.CarrierSpec qualified as CarrierSpec
import Moonlight.Flow.Carrier.Store.Engine.ReplaySpec qualified as CarrierStoreReplaySpec
import Moonlight.Flow.Carrier.Morphism.RestrictionSpec qualified as CarrierRestrictCompileSpec
import Moonlight.Flow.Carrier.Morphism.SubsumptionCoverageSpec qualified as ContainmentCoverageSpec
import Test.Tasty
  ( defaultMain,
    testGroup,
  )

main :: IO ()
main =
  defaultMain
    ( testGroup
        "moonlight-flow-carrier"
        [ BoundaryCoverageAtomRowsSpec.tests,
          CarrierAmalgamateSpec.tests,
          CarrierSpec.tests,
          CarrierStoreReplaySpec.tests,
          CarrierRestrictCompileSpec.tests,
          ContainmentCoverageSpec.spec
        ]
    )
