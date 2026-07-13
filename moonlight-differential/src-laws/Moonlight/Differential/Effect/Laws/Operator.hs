{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Differential.Effect.Laws.Operator
  ( lawBundles,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Core (AdditiveGroup (..))
import Moonlight.Differential.Algebra.ZSet qualified as ZSet
import Moonlight.Differential.Effect.Harness.Operator qualified as Harness
import Moonlight.Differential.Effect.LawNames (LawName (..))
import Moonlight.Differential.Operator.Aggregate
  ( GroupChange (..),
    countByKey,
    distinctDelta,
    distinctZSet,
    groupViewAdvance,
    groupViewIntegrated,
    groupViewReduced,
  )
import Moonlight.Differential.Operator.Fixpoint
  ( SemiNaiveBudget (..),
    semiNaiveFixpoint,
  )
import Moonlight.Differential.Operator.Join
  ( indexedDeltaJoin,
    indexedDeltaJoinArranged,
  )
import Moonlight.Differential.Operator.Linear
  ( filterZSet,
    mapZSet,
  )
import Moonlight.Differential.Update
  ( Update (..),
  )
import Moonlight.Pale.Test.LawSuite (LawBundle, lawBundleQuickCheck, quickCheckLawDefinition)
import Test.Tasty.QuickCheck qualified as QC

newtype TestZSet = TestZSet
  { unTestZSet :: ZSet.ZSet Int Int
  }
  deriving stock (Eq, Show)

newtype TestSmallZSet = TestSmallZSet
  { unTestSmallZSet :: ZSet.ZSet Int Int
  }
  deriving stock (Eq, Show)

newtype TestOperatorUpdates = TestOperatorUpdates
  { unTestOperatorUpdates :: [Harness.TestTraceUpdate]
  }
  deriving stock (Eq, Show)

instance QC.Arbitrary TestZSet where
  arbitrary =
    TestZSet . ZSet.zsetFromList <$> QC.listOf ((,) <$> QC.arbitrary <*> QC.chooseInt (-32, 32))

instance QC.Arbitrary TestSmallZSet where
  arbitrary =
    TestSmallZSet . ZSet.zsetFromList
      <$> QC.resize 8 (QC.listOf ((,) <$> QC.chooseInt (0, 11) <*> QC.chooseInt (-8, 8)))

instance QC.Arbitrary TestOperatorUpdates where
  arbitrary =
    TestOperatorUpdates
      <$> QC.listOf
        ( Update
            <$> QC.chooseInt (0, 5)
            <*> QC.elements ["join", "other", "skip"]
            <*> QC.elements ['a' .. 'd']
            <*> QC.chooseInt (-6, 6)
        )

propLinearOperatorsDeltaTransparent :: TestZSet -> TestZSet -> QC.Property
propLinearOperatorsDeltaTransparent (TestZSet left) (TestZSet right) =
  QC.conjoin
    [ QC.counterexample "map distributes over addition" (mapZSet Harness.operatorMap (left <> right) QC.=== mapZSet Harness.operatorMap left <> mapZSet Harness.operatorMap right),
      QC.counterexample "map commutes with inverse" (mapZSet Harness.operatorMap (neg left) QC.=== neg (mapZSet Harness.operatorMap left)),
      QC.counterexample "filter distributes over addition" (filterZSet Harness.operatorKeep (left <> right) QC.=== filterZSet Harness.operatorKeep left <> filterZSet Harness.operatorKeep right),
      QC.counterexample "filter commutes with inverse" (filterZSet Harness.operatorKeep (neg left) QC.=== neg (filterZSet Harness.operatorKeep left))
    ]

propIndexByPartitions :: TestZSet -> QC.Property
propIndexByPartitions (TestZSet values) =
  Harness.flattenOperatorIndex (Harness.operatorIndex values) QC.=== values

propIndexedDeltaJoinIntegrates :: TestZSet -> TestZSet -> TestZSet -> TestZSet -> QC.Property
propIndexedDeltaJoinIntegrates (TestZSet integratedLeftRows) (TestZSet deltaLeftRows) (TestZSet integratedRightRows) (TestZSet deltaRightRows) =
  indexedDeltaJoin integratedLeft deltaLeft integratedRight deltaRight
    QC.=== Harness.operatorJoinIntegrationOracle integratedLeft deltaLeft integratedRight deltaRight
  where
    integratedLeft =
      Harness.operatorIndex integratedLeftRows

    deltaLeft =
      Harness.operatorIndex deltaLeftRows

    integratedRight =
      Harness.operatorIndex integratedRightRows

    deltaRight =
      Harness.operatorIndex deltaRightRows

propStarDeltaJoinDecomposition ::
  TestSmallZSet ->
  TestSmallZSet ->
  TestSmallZSet ->
  TestSmallZSet ->
  TestSmallZSet ->
  TestSmallZSet ->
  QC.Property
propStarDeltaJoinDecomposition (TestSmallZSet integratedLeftRows) (TestSmallZSet deltaLeftRows) (TestSmallZSet integratedMiddleRows) (TestSmallZSet deltaMiddleRows) (TestSmallZSet integratedRightRows) (TestSmallZSet deltaRightRows) =
  Harness.operatorOrderedStarDelta integratedLeft deltaLeft integratedMiddle deltaMiddle integratedRight deltaRight
    QC.=== Harness.operatorStarDeltaOracle integratedLeft deltaLeft integratedMiddle deltaMiddle integratedRight deltaRight
  where
    integratedLeft =
      Harness.operatorIndex integratedLeftRows

    deltaLeft =
      Harness.operatorIndex deltaLeftRows

    integratedMiddle =
      Harness.operatorIndex integratedMiddleRows

    deltaMiddle =
      Harness.operatorIndex deltaMiddleRows

    integratedRight =
      Harness.operatorIndex integratedRightRows

    deltaRight =
      Harness.operatorIndex deltaRightRows

propArrangedIndexedJoinAgrees :: TestZSet -> TestZSet -> QC.Property
propArrangedIndexedJoinAgrees (TestZSet integratedRows) (TestZSet deltaRows) =
  QC.conjoin
    [ QC.counterexample "arrangedKeyZSet reconstructs indexed sections" (Harness.operatorArrangementIndexedSections integrated arrangement QC.=== integrated),
      QC.counterexample "arranged delta join agrees with the unarranged indexed delta join" $
        indexedDeltaJoinArranged arrangement deltaRight
          QC.=== indexedDeltaJoin integrated Harness.emptyOperatorIndex Harness.emptyOperatorIndex deltaRight
    ]
  where
    integrated =
      Harness.operatorIndex integratedRows

    deltaRight =
      Harness.operatorIndex deltaRows

    arrangement =
      Harness.operatorArrangementFromIndex integrated

propFoldDeltaJoinMaterializedView :: TestOperatorUpdates -> TestOperatorUpdates -> QC.Property
propFoldDeltaJoinMaterializedView (TestOperatorUpdates leftUpdates) (TestOperatorUpdates rightUpdates) =
  Harness.materializedFoldDeltaJoinBatchUpdates leftUpdates rightUpdates QC.=== Harness.foldedDeltaJoinBatchUpdates leftUpdates rightUpdates

propDistinctOperatorsFollowSupportOracle :: TestZSet -> TestZSet -> QC.Property
propDistinctOperatorsFollowSupportOracle (TestZSet integrated) (TestZSet delta) =
  QC.conjoin
    [ QC.counterexample "distinctZSet keeps exactly positive support" (distinctZSet integrated QC.=== Harness.operatorSupportZSet (Harness.positiveZSetSupport integrated)),
      QC.counterexample "distinctZSet weights are one" (Harness.operatorAllWeightsOne (distinctZSet integrated)),
      QC.counterexample "distinctDelta integrates to the recomputed distinct view" $
        distinctZSet (integrated <> delta)
          QC.=== distinctZSet integrated <> distinctDelta integrated delta
    ]

propGroupViewAdvanceRebuilds :: TestZSet -> TestZSet -> QC.Property
propGroupViewAdvanceRebuilds (TestZSet integratedRows) (TestZSet deltaRows) =
  QC.conjoin
    [ QC.counterexample "advanced view equals rebuild" (advancedView QC.=== rebuiltView),
      QC.counterexample "advanced integrated section is the old section plus delta" (groupViewIntegrated advancedView QC.=== advancedIntegrated),
      QC.counterexample "advanced reduced map equals rebuilt reduced map" (groupViewReduced advancedView QC.=== groupViewReduced rebuiltView),
      QC.counterexample "vanished groups are exactly the delta keys absent after integration" $
        Map.filter (== GroupVanished) changes QC.=== Harness.operatorExpectedVanishedChanges delta advancedIntegrated
    ]
  where
    integrated =
      Harness.operatorIndex integratedRows

    delta =
      Harness.operatorIndex deltaRows

    view =
      Harness.operatorGroupView integrated

    (changes, advancedView) =
      groupViewAdvance Harness.operatorGroupReducer delta view

    advancedIntegrated =
      integrated <> delta

    rebuiltView =
      Harness.operatorGroupView advancedIntegrated

propCountByKeyLinear :: TestZSet -> TestZSet -> QC.Property
propCountByKeyLinear (TestZSet leftRows) (TestZSet rightRows) =
  QC.conjoin
    [ QC.counterexample "countByKey distributes over addition" (countByKey (left <> right) QC.=== countByKey left <> countByKey right),
      QC.counterexample "countByKey commutes with inverse" (countByKey (neg left) QC.=== neg (countByKey left))
    ]
  where
    left =
      Harness.operatorIndex leftRows

    right =
      Harness.operatorIndex rightRows

data TestReachability = TestReachability
  { trSeedNodes :: !(Set Int),
    trEdges :: !(Set (Int, Int))
  }
  deriving stock (Eq, Show)

instance QC.Arbitrary TestReachability where
  arbitrary =
    TestReachability
      <$> (Set.fromList <$> QC.listOf (QC.chooseInt (0, 7)))
      <*> (Set.fromList <$> QC.listOf ((,) <$> QC.chooseInt (0, 7) <*> QC.chooseInt (0, 7)))

propSemiNaiveMatchesReachabilityOracle :: TestReachability -> QC.Property
propSemiNaiveMatchesReachabilityOracle reachability =
  case semiNaiveFixpoint (SemiNaiveBudget 16) (Harness.reachabilityStep (trEdges reachability)) seed of
    Left divergence ->
      QC.counterexample ("unexpected semi-naive divergence: " <> show divergence) False
    Right actual ->
      actual QC.=== Harness.operatorSupportZSet (Harness.naiveReachabilitySupport (trEdges reachability) (trSeedNodes reachability))
  where
    seed =
      Harness.operatorSupportZSet (trSeedNodes reachability)

propSemiNaiveArrangementReachabilityOracle :: TestReachability -> QC.Property
propSemiNaiveArrangementReachabilityOracle reachability =
  case semiNaiveFixpoint (SemiNaiveBudget 16) (Harness.arrangedReachabilityStep arrangement) seed of
    Left divergence ->
      QC.counterexample ("unexpected arranged semi-naive divergence: " <> show divergence) False
    Right actual ->
      actual QC.=== Harness.operatorSupportZSet (Harness.naiveReachabilitySupport (trEdges reachability) (trSeedNodes reachability))
  where
    seed =
      Harness.operatorSupportZSet (trSeedNodes reachability)

    arrangement =
      Harness.reachabilityArrangement (trEdges reachability)

lawBundles :: [LawBundle String]
lawBundles =
  [ lawBundleQuickCheck
      "operator"
      [ quickCheckLawDefinition LinearOperatorsDeltaTransparent propLinearOperatorsDeltaTransparent,
        quickCheckLawDefinition IndexByPartitionsReflatten propIndexByPartitions,
        quickCheckLawDefinition IndexedDeltaJoinIntegratesBilinearDeltas propIndexedDeltaJoinIntegrates,
        quickCheckLawDefinition StarDeltaDecompositionEqualsRecomputation propStarDeltaJoinDecomposition,
        quickCheckLawDefinition ArrangedJoinsAgreeWithUnarranged propArrangedIndexedJoinAgrees,
        quickCheckLawDefinition FoldDeltaJoinConsolidatesThroughBatch propFoldDeltaJoinMaterializedView,
        quickCheckLawDefinition DistinctFollowsSupportOracle propDistinctOperatorsFollowSupportOracle,
        quickCheckLawDefinition GroupViewAdvanceRebuildsIntegratedView propGroupViewAdvanceRebuilds,
        quickCheckLawDefinition CountByKeyLinear propCountByKeyLinear,
        quickCheckLawDefinition SemiNaiveFixpointMatchesReachabilityOracle propSemiNaiveMatchesReachabilityOracle,
        quickCheckLawDefinition SemiNaiveArrangedFixpointMatchesReachabilityOracle propSemiNaiveArrangementReachabilityOracle
      ]
  ]
