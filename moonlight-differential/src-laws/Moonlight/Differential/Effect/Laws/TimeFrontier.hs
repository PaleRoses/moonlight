module Moonlight.Differential.Effect.Laws.TimeFrontier
  ( lawBundles,
  )
where

import Moonlight.Differential.Effect.Harness.TimeFrontier qualified as Harness
import Moonlight.Differential.Effect.LawNames (LawName (..), lawName)
import Moonlight.Differential.Time
  ( RuntimeTime,
    emptyRuntimeScope,
    enterRuntimeTimeScope,
    frontierStamp,
  )
import Moonlight.Differential.Time qualified as DifferentialTime
import Moonlight.Pale.Test.LawSuite (LawBundle, hUnitLaw, lawBundleQuickCheck, quickCheckLawDefinition, renderedLawBundle)
import Test.Tasty.QuickCheck qualified as QC

lawBundles :: [LawBundle String]
lawBundles =
  [ renderedLawBundle
      "time-frontier"
      [ hUnitLaw (lawName RuntimeTimePartialOrderLaws) Harness.runtimeTimeScopeLaws,
        hUnitLaw (lawName RuntimeFrontierNormalizedAntichains) Harness.runtimeFrontierStoresProductAntichains,
        hUnitLaw (lawName LocalFactAntichainDominanceNormalized) Harness.localFactConstructionAndAntichainLaws
      ],
    lawBundleQuickCheck
      "time-frontier-capability"
      [ quickCheckLawDefinition CapabilityDowngradeMonotoneAccepted propCapabilityDowngradeMonotoneAccepted,
        quickCheckLawDefinition CapabilityAdvanceRegressionTyped propCapabilityAdvanceRegressionTyped
      ]
  ]

propCapabilityDowngradeMonotoneAccepted :: QC.Property
propCapabilityDowngradeMonotoneAccepted =
  QC.forAll ((,) <$> capabilityTimeGen <*> capabilityTimeGen) $
    \(sourceTime, targetTime) ->
      Harness.capabilityDowngradeMonotoneAccepted sourceTime targetTime

propCapabilityAdvanceRegressionTyped :: QC.Property
propCapabilityAdvanceRegressionTyped =
  QC.forAll ((,) <$> capabilityTimeGen <*> capabilityTimeGen) $
    \(sourceTime, targetTime) ->
      Harness.capabilityAdvanceRegressionTyped sourceTime targetTime

capabilityTimeGen :: QC.Gen (RuntimeTime Int Int Int)
capabilityTimeGen = do
  contextValue <- QC.chooseInt (0, 1)
  scopeDepth <- QC.chooseInt (0, 1)
  epochValue <- QC.chooseInt (0, 2)
  phaseValue <- QC.chooseInt (0, 2)
  stampValue <- QC.chooseInt (0, 2)
  let rootTime =
        DifferentialTime.runtimeTime
          contextValue
          emptyRuntimeScope
          epochValue
          phaseValue
          (frontierStamp (fromIntegral stampValue))
  pure
    ( if scopeDepth == 0
        then rootTime
        else enterRuntimeTimeScope 3 rootTime
    )
