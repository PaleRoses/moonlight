{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Differential.Effect.Laws.Projection
  ( lawBundles,
  )
where

import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Differential.Delta
  ( deltaApplyMany,
    deltaCombineMany,
    deltaIsEmpty,
  )
import Moonlight.Differential.Effect.Harness.Projection qualified as Harness
import Moonlight.Differential.Effect.LawNames (LawName (..))
import Moonlight.Differential.Projection.Delta
  ( ProjectionDelta,
    projectionDeltaOps,
  )
import Moonlight.Differential.Projection.Maintenance qualified as ProjectionMaintenance
import Moonlight.Differential.Projection.Propagation
  ( ProjectionCommit (..),
    ProjectionPropagationState (..),
  )
import Moonlight.Differential.Projection.Work
  ( ProjectionPhase (..),
    ProjectionWork,
    projectionWorkDeltaOps,
  )
import Moonlight.Pale.Test.LawSuite (LawBundle, lawBundleQuickCheck, quickCheckLawDefinition)
import Test.Tasty.QuickCheck qualified as QC

newtype TestProjectionWork = TestProjectionWork
  { unTestProjectionWork :: ProjectionWork
  }
  deriving stock (Eq, Show)

instance QC.Arbitrary TestProjectionWork where
  arbitrary =
    TestProjectionWork
      <$> ( Harness.makeProjectionWork
              <$> QC.arbitrary
              <*> smallIntSet
              <*> smallIntSet
              <*> smallIntSet
          )

newtype TestProjectionDelta = TestProjectionDelta
  { unTestProjectionDelta :: ProjectionDelta Int IntSet.IntSet
  }
  deriving stock (Eq, Show)

instance QC.Arbitrary TestProjectionDelta where
  arbitrary =
    TestProjectionDelta
      <$> ( Harness.makeProjectionDelta
              <$> smallIntSet
              <*> smallIntSet
              <*> smallIntSet
              <*> QC.chooseInt (0, 5)
              <*> smallIntSet
              <*> QC.chooseInt (0, 5)
              <*> smallIntSet
              <*> QC.chooseInt (0, 5)
              <*> smallIntSet
          )

newtype TestProjectionMaintenanceRun = TestProjectionMaintenanceRun
  { unTestProjectionMaintenanceRun :: Harness.TestProjectionMaintenanceRun
  }
  deriving stock (Eq, Show)

instance QC.Arbitrary TestProjectionMaintenanceRun where
  arbitrary =
    TestProjectionMaintenanceRun
      <$> ( Harness.TestProjectionMaintenanceRun
              <$> QC.listOf arbitraryProjectionPhase
              <*> (Set.fromList <$> QC.sublistOf [0 .. 6])
              <*> ( Map.fromListWith (<>)
                      <$> QC.listOf
                        ((,) <$> QC.chooseInt (0, 6) <*> (unTestProjectionWork <$> QC.arbitrary))
                  )
          )

newtype TestProjectionCommit = TestProjectionCommit
  { unTestProjectionCommit :: Harness.TestProjectionCommit
  }
  deriving stock (Eq, Show)

instance QC.Arbitrary TestProjectionCommit where
  arbitrary =
    TestProjectionCommit
      <$> ( Harness.TestProjectionCommit
              <$> QC.chooseInt (0, 6)
              <*> QC.arbitrary
              <*> smallIntSet
              <*> smallIntSet
              <*> smallIntSet
              <*> smallIntSet
              <*> smallIntSet
              <*> QC.arbitrary
          )

arbitraryProjectionPhase :: QC.Gen ProjectionPhase
arbitraryProjectionPhase =
  QC.elements [Project, Prune, Restrict]

smallIntSet :: QC.Gen IntSet.IntSet
smallIntSet =
  IntSet.fromList <$> QC.listOf (QC.chooseInt (0, 12))

propProjectionWorkDeltaOpsGenerated :: TestProjectionWork -> TestProjectionWork -> TestProjectionWork -> QC.Property
propProjectionWorkDeltaOpsGenerated (TestProjectionWork left) (TestProjectionWork middle) (TestProjectionWork right) =
  QC.conjoin
    [ QC.counterexample "combineMany follows semigroup order" $
        deltaCombineMany projectionWorkDeltaOps [left, middle, right]
          QC.=== left <> middle <> right,
      QC.counterexample "applyMany replays ordered work deltas" $
        deltaApplyMany projectionWorkDeltaOps [middle, right] left
          QC.=== left <> middle <> right,
      QC.counterexample "emptiness is identity honesty" $
        deltaIsEmpty projectionWorkDeltaOps left
          QC.=== (left == mempty)
    ]

propProjectionDeltaOpsGenerated :: TestProjectionDelta -> TestProjectionDelta -> TestProjectionDelta -> QC.Property
propProjectionDeltaOpsGenerated (TestProjectionDelta left) (TestProjectionDelta middle) (TestProjectionDelta right) =
  QC.conjoin
    [ QC.counterexample "combineMany follows semigroup order" $
        deltaCombineMany projectionDeltaOps [left, middle, right]
          QC.=== left <> middle <> right,
      QC.counterexample "applyMany replays ordered projection deltas" $
        deltaApplyMany projectionDeltaOps [middle, right] left
          QC.=== left <> middle <> right,
      QC.counterexample "emptiness is identity honesty" $
        deltaIsEmpty projectionDeltaOps left
          QC.=== (left == mempty)
    ]

propProjectionMaintenanceMatchesRecompute :: TestProjectionMaintenanceRun -> QC.Property
propProjectionMaintenanceMatchesRecompute (TestProjectionMaintenanceRun runValue) =
  case Harness.runGeneratedProjectionMaintenance runValue of
    Left obstruction ->
      QC.counterexample obstruction False
    Right resultValue ->
      QC.conjoin
        [ ProjectionMaintenance.pwrGraph resultValue
            QC.=== Harness.expectedProjectionMaintenanceTrace runValue,
          ProjectionMaintenance.pwrJobs resultValue
            QC.=== Harness.expectedProjectionMaintenanceJobs runValue
        ]

propProjectionCommitMatchesRecompute :: TestProjectionCommit -> QC.Property
propProjectionCommitMatchesRecompute (TestProjectionCommit commitValue) =
  QC.conjoin
    [ cpcSectionChanged projectionCommitValue QC.=== Harness.tpcSectionChanged commitValue,
      cpsContextGraph committedState QC.=== Harness.expectedProjectionCommitSite commitValue,
      cpsContextViews committedState QC.=== Harness.expectedProjectionCommitViews commitValue,
      cpsDirtyResults committedState QC.=== Harness.expectedProjectionCommitDirtyResults commitValue,
      Harness.affectedContextsForCommitKeys committedState commitValue
        QC.=== Harness.expectedAffectedContextsForCommitKeys commitValue
    ]
  where
    projectionCommitValue =
      Harness.runGeneratedProjectionCommit commitValue

    committedState =
      cpcState projectionCommitValue

lawBundles :: [LawBundle String]
lawBundles =
  [ lawBundleQuickCheck
      "projection"
      [ quickCheckLawDefinition ProjectionWorkObeysSharedActionAlgebra propProjectionWorkDeltaOpsGenerated,
        quickCheckLawDefinition ProjectionDeltaObeysSharedActionAlgebra propProjectionDeltaOpsGenerated,
        quickCheckLawDefinition ProjectionMaintenanceMatchesRecomputation propProjectionMaintenanceMatchesRecompute,
        quickCheckLawDefinition ProjectionCommitMatchesSupportRecomputation propProjectionCommitMatchesRecompute
      ]
  ]
