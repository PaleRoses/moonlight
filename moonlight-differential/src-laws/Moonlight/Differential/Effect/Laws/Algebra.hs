{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Differential.Effect.Laws.Algebra
  ( lawBundles,
  )
where

import Data.IntSet qualified as IntSet
import Data.Kind
  ( Type,
  )
import Moonlight.Differential.Algebra.FiniteMap qualified as FiniteMap
import Moonlight.Differential.Algebra.ZSet
  ( Timed (..),
  )
import Moonlight.Differential.Algebra.ZSet qualified as ZSet
import Moonlight.Differential.Effect.Harness.Algebra qualified as Harness
import Moonlight.Differential.Effect.LawNames (LawName (..))
import Moonlight.Differential.Projection.Delta
  ( ProjectionDelta,
    bootstrapQueries,
    projectQuery,
    projectionDeltaOps,
    projectionOnly,
    pruneQuery,
    restrictQuery,
  )
import Moonlight.Differential.Projection.Work
  ( ProjectionWork,
    bootstrapProjection,
    projectKeys,
    projectionWorkDeltaOps,
    pruneKeys,
    restrictKeys,
  )
import Moonlight.Pale.Test.LawSuite (LawBundle, lawBundleQuickCheck, quickCheckLawDefinition)
import Test.Tasty.QuickCheck qualified as QC

lawBundles :: [LawBundle String]
lawBundles =
  [ lawBundleQuickCheck
      "algebra"
      [ quickCheckLawDefinition FiniteMapAbelianGroup propFiniteMapAbelianGroupLaws,
        quickCheckLawDefinition ZSetGroupCancellation propZSetRoundTripAndCancellation,
        quickCheckLawDefinition ZSetCanonicalSupportSize propZSetSizeTracksSupport,
        quickCheckLawDefinition IndexedZSetCanonicalSupportCellCount propIndexedZSetCellCountTracksSupport,
        quickCheckLawDefinition IndexedZSetUnionsDenoteIndexedAddition propIndexedZSetUnionsDenoteAddition,
        quickCheckLawDefinition IndexedGroupingDistributesOverAddition propIndexedGroupingDistributesOverAddition,
        quickCheckLawDefinition CursorPreservesTimedCanonicalOrder propCursorTimedZSetRoundTrip,
        quickCheckLawDefinition CursorMergeDenotesTimedAddition propCursorMergeDenotesTimedZSetAddition
      ],
    lawBundleQuickCheck
      "delta/projection-work"
      [ quickCheckLawDefinition DeltaIdentityNeutral projectionWorkDeltaIdentityNeutral,
        quickCheckLawDefinition DeltaCompositionAssociative projectionWorkDeltaCompositionAssociative,
        quickCheckLawDefinition DeltaIdentityAction projectionWorkDeltaIdentityAction,
        quickCheckLawDefinition DeltaCompositionActsHomomorphically projectionWorkDeltaCompositionActsHomomorphically,
        quickCheckLawDefinition DeltaNullHonestyExtensional projectionWorkDeltaNullHonestyExtensional
      ],
    lawBundleQuickCheck
      "delta/projection-delta"
      [ quickCheckLawDefinition DeltaIdentityNeutral projectionDeltaIdentityNeutral,
        quickCheckLawDefinition DeltaCompositionAssociative projectionDeltaCompositionAssociative,
        quickCheckLawDefinition DeltaIdentityAction projectionDeltaIdentityAction,
        quickCheckLawDefinition DeltaCompositionActsHomomorphically projectionDeltaCompositionActsHomomorphically,
        quickCheckLawDefinition DeltaNullHonestyExtensional projectionDeltaNullHonestyExtensional
      ]
  ]

newtype TestFiniteMap = TestFiniteMap
  { unTestFiniteMap :: FiniteMap.FiniteMap Int Int
  }
  deriving stock (Eq, Show)

instance QC.Arbitrary TestFiniteMap where
  arbitrary =
    TestFiniteMap . FiniteMap.fromList <$> QC.listOf ((,) <$> QC.arbitrary <*> QC.chooseInt (-32, 32))

newtype TestZSet = TestZSet
  { unTestZSet :: ZSet.ZSet Int Int
  }
  deriving stock (Eq, Show)

instance QC.Arbitrary TestZSet where
  arbitrary =
    TestZSet . ZSet.zsetFromList <$> QC.listOf ((,) <$> QC.arbitrary <*> QC.chooseInt (-32, 32))

newtype TestIndexedEntries = TestIndexedEntries
  { unTestIndexedEntries :: [(Int, Int, Int)]
  }
  deriving stock (Eq, Show)

instance QC.Arbitrary TestIndexedEntries where
  arbitrary =
    TestIndexedEntries
      <$> QC.listOf
        ((,,) <$> QC.chooseInt (-16, 16) <*> QC.chooseInt (-16, 16) <*> QC.chooseInt (-32, 32))

newtype TestTimedZSet = TestTimedZSet
  { unTestTimedZSet :: ZSet.ZSet (Timed Int Char) Int
  }
  deriving stock (Eq, Show)

instance QC.Arbitrary TestTimedZSet where
  arbitrary =
    TestTimedZSet . ZSet.zsetFromList
      <$> QC.listOf
        ( (,)
            <$> (Timed <$> QC.chooseInt (0, 12) <*> QC.elements ['a' .. 'f'])
            <*> QC.chooseInt (-16, 16)
        )

propFiniteMapAbelianGroupLaws :: TestFiniteMap -> TestFiniteMap -> TestFiniteMap -> QC.Property
propFiniteMapAbelianGroupLaws (TestFiniteMap left) (TestFiniteMap middle) (TestFiniteMap right) =
  Harness.finiteMapAbelianGroupLaws left middle right

propZSetRoundTripAndCancellation :: TestZSet -> QC.Property
propZSetRoundTripAndCancellation (TestZSet values) =
  Harness.zSetRoundTripAndCancellation values

propZSetSizeTracksSupport :: TestZSet -> QC.Property
propZSetSizeTracksSupport (TestZSet values) =
  Harness.zSetSizeTracksSupport values

propIndexedZSetCellCountTracksSupport :: TestIndexedEntries -> QC.Property
propIndexedZSetCellCountTracksSupport (TestIndexedEntries entries) =
  Harness.indexedZSetCellCountTracksSupport entries

propIndexedZSetUnionsDenoteAddition :: [TestIndexedEntries] -> QC.Property
propIndexedZSetUnionsDenoteAddition sections =
  Harness.indexedZSetUnionsDenoteAddition (unTestIndexedEntries <$> sections)

propIndexedGroupingDistributesOverAddition :: TestZSet -> TestZSet -> QC.Property
propIndexedGroupingDistributesOverAddition (TestZSet left) (TestZSet right) =
  Harness.indexedGroupingDistributesOverAddition left right

propCursorTimedZSetRoundTrip :: TestTimedZSet -> QC.Property
propCursorTimedZSetRoundTrip (TestTimedZSet rows) =
  Harness.cursorTimedZSetRoundTrip rows

propCursorMergeDenotesTimedZSetAddition :: TestTimedZSet -> TestTimedZSet -> QC.Property
propCursorMergeDenotesTimedZSetAddition (TestTimedZSet left) (TestTimedZSet right) =
  Harness.cursorMergeDenotesTimedZSetAddition left right

projectionWorkDeltaIdentityNeutral :: QC.Property
projectionWorkDeltaIdentityNeutral =
  QC.forAll genProjectionWork $ \deltaValue ->
    Harness.deltaIdentityNeutral projectionWorkDeltaOps deltaValue

projectionWorkDeltaCompositionAssociative :: QC.Property
projectionWorkDeltaCompositionAssociative =
  QC.forAll ((,,) <$> genProjectionWork <*> genProjectionWork <*> genProjectionWork) $ \(left, middle, right) ->
    Harness.deltaCompositionAssociative projectionWorkDeltaOps (left, middle, right)

projectionWorkDeltaIdentityAction :: QC.Property
projectionWorkDeltaIdentityAction =
  QC.forAll genProjectionWork $ \sectionValue ->
    Harness.deltaIdentityAction projectionWorkDeltaOps sectionValue

projectionWorkDeltaCompositionActsHomomorphically :: QC.Property
projectionWorkDeltaCompositionActsHomomorphically =
  QC.forAll ((,,) <$> genProjectionWork <*> genProjectionWork <*> genProjectionWork) $ \(leftDelta, rightDelta, sectionValue) ->
    Harness.deltaCompositionActsHomomorphically projectionWorkDeltaOps (leftDelta, rightDelta, sectionValue)

projectionWorkDeltaNullHonestyExtensional :: QC.Property
projectionWorkDeltaNullHonestyExtensional =
  QC.forAll ((,) <$> genProjectionWork <*> genProjectionWork) $ \(deltaValue, sectionValue) ->
    Harness.deltaNullHonestyExtensional projectionWorkDeltaOps (deltaValue, sectionValue)

projectionDeltaIdentityNeutral :: QC.Property
projectionDeltaIdentityNeutral =
  QC.forAll genProjectionDelta $ \deltaValue ->
    Harness.deltaIdentityNeutral projectionDeltaOps deltaValue

projectionDeltaCompositionAssociative :: QC.Property
projectionDeltaCompositionAssociative =
  QC.forAll ((,,) <$> genProjectionDelta <*> genProjectionDelta <*> genProjectionDelta) $ \(left, middle, right) ->
    Harness.deltaCompositionAssociative projectionDeltaOps (left, middle, right)

projectionDeltaIdentityAction :: QC.Property
projectionDeltaIdentityAction =
  QC.forAll genProjectionDelta $ \sectionValue ->
    Harness.deltaIdentityAction projectionDeltaOps sectionValue

projectionDeltaCompositionActsHomomorphically :: QC.Property
projectionDeltaCompositionActsHomomorphically =
  QC.forAll ((,,) <$> genProjectionDelta <*> genProjectionDelta <*> genProjectionDelta) $ \(leftDelta, rightDelta, sectionValue) ->
    Harness.deltaCompositionActsHomomorphically projectionDeltaOps (leftDelta, rightDelta, sectionValue)

projectionDeltaNullHonestyExtensional :: QC.Property
projectionDeltaNullHonestyExtensional =
  QC.forAll ((,) <$> genProjectionDelta <*> genProjectionDelta) $ \(deltaValue, sectionValue) ->
    Harness.deltaNullHonestyExtensional projectionDeltaOps (deltaValue, sectionValue)

type SmallProjectionQuery :: Type
type SmallProjectionQuery = Int

genProjectionWork :: QC.Gen ProjectionWork
genProjectionWork =
  mconcat
    <$> sequenceA
      [ genBootstrapWork,
        projectKeys <$> genSmallIntSet,
        pruneKeys <$> genSmallIntSet,
        restrictKeys <$> genSmallIntSet
      ]

genBootstrapWork :: QC.Gen ProjectionWork
genBootstrapWork =
  QC.elements [mempty, bootstrapProjection]

genSmallIntSet :: QC.Gen IntSet.IntSet
genSmallIntSet =
  IntSet.fromList <$> QC.listOf (QC.chooseInt (0, 12))

genProjectionDelta :: QC.Gen (ProjectionDelta SmallProjectionQuery ())
genProjectionDelta =
  mconcat <$> QC.listOf genProjectionDeltaFragment

genProjectionDeltaFragment :: QC.Gen (ProjectionDelta SmallProjectionQuery ())
genProjectionDeltaFragment =
  QC.oneof
    [ projectionOnly <$> genSmallIntSet <*> genSmallIntSet,
      projectQuery <$> QC.chooseInt (0, 5) <*> genSmallIntSet,
      pruneQuery <$> QC.chooseInt (0, 5) <*> genSmallIntSet,
      restrictQuery <$> QC.chooseInt (0, 5) <*> genSmallIntSet,
      bootstrapQueries . (: []) <$> QC.chooseInt (0, 5)
    ]
