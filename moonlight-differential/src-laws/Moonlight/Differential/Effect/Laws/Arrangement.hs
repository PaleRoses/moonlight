{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Differential.Effect.Laws.Arrangement
  ( lawBundles,
  )
where

import Moonlight.Differential.Effect.Harness.Arrangement qualified as Harness
import Moonlight.Differential.Effect.LawNames (LawName (..))
import Moonlight.Differential.Update
  ( Update (..),
  )
import Moonlight.Pale.Test.LawSuite (LawBundle, lawBundleQuickCheck, quickCheckLawDefinition)
import Test.Tasty.QuickCheck qualified as QC

newtype TestArrangementUpdates = TestArrangementUpdates
  { unTestArrangementUpdates :: [Harness.TestTraceUpdate]
  }
  deriving stock (Eq, Show)

instance QC.Arbitrary TestArrangementUpdates where
  arbitrary =
    TestArrangementUpdates
      <$> QC.listOf
        ( Update
            <$> QC.chooseInt (0, 8)
            <*> QC.elements ["left", "right", "join", "other"]
            <*> QC.elements ['a' .. 'd']
            <*> QC.chooseInt (-8, 8)
        )

newtype TestArrangementKey = TestArrangementKey
  { unTestArrangementKey :: String
  }
  deriving stock (Eq, Show)

instance QC.Arbitrary TestArrangementKey where
  arbitrary =
    TestArrangementKey <$> QC.elements ["left", "right", "join", "other", "missing"]

propArrangeByKeyDenotesReplay :: TestArrangementUpdates -> QC.Property
propArrangeByKeyDenotesReplay (TestArrangementUpdates updates) =
  Harness.arrangementCellMap updates QC.=== Harness.updateCellMap updates

propAppendArrangementBatchDenotesReplay :: TestArrangementUpdates -> TestArrangementUpdates -> QC.Property
propAppendArrangementBatchDenotesReplay (TestArrangementUpdates initialUpdates) (TestArrangementUpdates batchUpdates) =
  Harness.arrangementAppendCellMap initialUpdates batchUpdates QC.=== Harness.updateCellMap (initialUpdates <> batchUpdates)

propFoldArrangementKeyDenotesReplayFilter :: TestArrangementKey -> TestArrangementUpdates -> QC.Property
propFoldArrangementKeyDenotesReplayFilter (TestArrangementKey key) (TestArrangementUpdates updates) =
  Harness.arrangementKeyCellMap key updates QC.=== Harness.oracleKeyCellMap (const True) key updates

propArrangementSlicesDenoteReplayFilters :: TestArrangementKey -> TestArrangementUpdates -> QC.NonNegative Int -> QC.Property
propArrangementSlicesDenoteReplayFilters (TestArrangementKey key) (TestArrangementUpdates updates) (QC.NonNegative cutoff) =
  QC.conjoin
    [ QC.counterexample "slice through" $
        Harness.arrangementSliceThroughCellMap cutoff key updates
          QC.=== Harness.oracleKeyCellMap (<= cutoff) key updates,
      QC.counterexample "slice after" $
        Harness.arrangementSliceAfterCellMap cutoff key updates
          QC.=== Harness.oracleKeyCellMap (> cutoff) key updates
    ]

lawBundles :: [LawBundle String]
lawBundles =
  [ lawBundleQuickCheck
      "arrangement"
      [ quickCheckLawDefinition ArrangeByKeyDenotesReplayedTrace propArrangeByKeyDenotesReplay,
        quickCheckLawDefinition ArrangementAppendDenotesAppendThenArrange propAppendArrangementBatchDenotesReplay,
        quickCheckLawDefinition ArrangementKeyFoldFiltersReplayOracle propFoldArrangementKeyDenotesReplayFilter,
        quickCheckLawDefinition ArrangementSliceFoldsFilterReplayOracleByTime propArrangementSlicesDenoteReplayFilters
      ]
  ]
