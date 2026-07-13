module Moonlight.Differential.Effect.Harness.Algebra
  ( cursorMergeDenotesTimedZSetAddition,
    cursorTimedZSetRoundTrip,
    deltaCompositionActsHomomorphically,
    deltaCompositionAssociative,
    deltaIdentityAction,
    deltaIdentityNeutral,
    deltaNullHonestyExtensional,
    finiteMapAbelianGroupLaws,
    indexedGroupingDistributesOverAddition,
    indexedZSetCellCountTracksSupport,
    indexedZSetUnionsDenoteAddition,
    zSetRoundTripAndCancellation,
    zSetSizeTracksSupport,
  )
where

import Moonlight.Core (AdditiveGroup (..), AdditiveMonoid (..))
import Moonlight.Differential.Algebra.FiniteMap qualified as FiniteMap
import Moonlight.Differential.Algebra.ZSet
  ( Timed,
  )
import Moonlight.Differential.Algebra.ZSet qualified as ZSet
import Moonlight.Differential.Cursor
  ( cursorFromZSet,
    cursorMerge,
    cursorToZSet,
  )
import Moonlight.Differential.Delta
  ( DeltaOps,
    deltaApply,
    deltaCombine,
    deltaIdentity,
    deltaIsEmpty,
  )
import Test.Tasty.QuickCheck qualified as QC

finiteMapAbelianGroupLaws ::
  FiniteMap.FiniteMap Int Int ->
  FiniteMap.FiniteMap Int Int ->
  FiniteMap.FiniteMap Int Int ->
  QC.Property
finiteMapAbelianGroupLaws left middle right =
  QC.conjoin
    [ QC.counterexample "left identity" (zero <> left QC.=== left),
      QC.counterexample "right identity" (left <> zero QC.=== left),
      QC.counterexample "commutative" (left <> middle QC.=== middle <> left),
      QC.counterexample "associative" ((left <> middle) <> right QC.=== left <> (middle <> right)),
      QC.counterexample "inverse" (left <> neg left QC.=== zero),
      QC.counterexample "zero weights absent" (all (not . (== (0 :: Int)) . snd) (FiniteMap.toAscList left))
    ]

zSetRoundTripAndCancellation :: ZSet.ZSet Int Int -> QC.Property
zSetRoundTripAndCancellation values =
  QC.conjoin
    [ QC.counterexample "fromList . toAscList" (ZSet.zsetFromList (ZSet.zsetToAscList values) QC.=== values),
      QC.counterexample "difference self" (ZSet.zsetDifference values values QC.=== zero)
    ]

zSetSizeTracksSupport :: ZSet.ZSet Int Int -> QC.Property
zSetSizeTracksSupport values =
  ZSet.zsetSize values QC.=== length (ZSet.zsetToAscList values)

indexedZSetCellCountTracksSupport :: [(Int, Int, Int)] -> QC.Property
indexedZSetCellCountTracksSupport entries =
  ZSet.indexedZSetCellCount rows QC.=== length (flattenedIndexedRows rows)
  where
    rows =
      ZSet.indexedZSetFromList entries :: ZSet.IndexedZSet Int Int Int

indexedZSetUnionsDenoteAddition :: [[(Int, Int, Int)]] -> QC.Property
indexedZSetUnionsDenoteAddition entrySections =
  ZSet.indexedZSetUnions (ZSet.indexedZSetFromList <$> entrySections)
    QC.=== (ZSet.indexedZSetFromList (foldMap id entrySections) :: ZSet.IndexedZSet Int Int Int)

flattenedIndexedRows :: ZSet.IndexedZSet Int Int Int -> [(Int, Int, Int)]
flattenedIndexedRows =
  ZSet.indexedZSetFold
    ( \acc key values ->
        acc <> fmap (\(value, weight) -> (key, value, weight)) (ZSet.zsetToAscList values)
    )
    []

indexedGroupingDistributesOverAddition :: ZSet.ZSet Int Int -> ZSet.ZSet Int Int -> QC.Property
indexedGroupingDistributesOverAddition left right =
  QC.conjoin
    [ QC.counterexample "groupBy distributes over addition" groupedSumMatches,
      QC.counterexample "flatten . groupBy preserves relation" flattenedMatches
    ]
  where
    groupedSumMatches =
      groupByParity (left <> right) QC.=== groupByParity left <> groupByParity right

    flattenedMatches =
      flattenParityGroups (groupByParity left) QC.=== left
        QC..&&. flattenParityGroups (groupByParity right) QC.=== right

groupByParity :: ZSet.ZSet Int Int -> ZSet.IndexedZSet Bool Int Int
groupByParity =
  ZSet.zsetFold
    (\indexed value weight -> ZSet.indexedZSetInsert (even value) value weight indexed)
    ZSet.indexedZSetEmpty

flattenParityGroups :: ZSet.IndexedZSet Bool Int Int -> ZSet.ZSet Int Int
flattenParityGroups =
  ZSet.indexedZSetFold (\acc _key values -> add acc values) ZSet.zsetEmpty

cursorTimedZSetRoundTrip :: ZSet.ZSet (Timed Int Char) Int -> QC.Property
cursorTimedZSetRoundTrip rows =
  cursorToZSet (cursorFromZSet rows) QC.=== rows

cursorMergeDenotesTimedZSetAddition :: ZSet.ZSet (Timed Int Char) Int -> ZSet.ZSet (Timed Int Char) Int -> QC.Property
cursorMergeDenotesTimedZSetAddition left right =
  cursorToZSet (cursorMerge (cursorFromZSet left) (cursorFromZSet right)) QC.=== left <> right

deltaIdentityNeutral ::
  (Eq delta, Show delta) =>
  DeltaOps section delta ->
  delta ->
  QC.Property
deltaIdentityNeutral ops deltaValue =
  QC.conjoin
    [ deltaCombine ops (deltaIdentity ops) deltaValue QC.=== deltaValue,
      deltaCombine ops deltaValue (deltaIdentity ops) QC.=== deltaValue
    ]

deltaCompositionAssociative ::
  (Eq delta, Show delta) =>
  DeltaOps section delta ->
  (delta, delta, delta) ->
  QC.Property
deltaCompositionAssociative ops (left, middle, right) =
  deltaCombine ops left (deltaCombine ops middle right)
    QC.=== deltaCombine ops (deltaCombine ops left middle) right

deltaIdentityAction ::
  (Eq section, Show section) =>
  DeltaOps section delta ->
  section ->
  QC.Property
deltaIdentityAction ops sectionValue =
  deltaApply ops (deltaIdentity ops) sectionValue QC.=== sectionValue

deltaCompositionActsHomomorphically ::
  (Eq section, Show section) =>
  DeltaOps section delta ->
  (delta, delta, section) ->
  QC.Property
deltaCompositionActsHomomorphically ops (leftDelta, rightDelta, sectionValue) =
  deltaApply ops (deltaCombine ops leftDelta rightDelta) sectionValue
    QC.=== deltaApply ops rightDelta (deltaApply ops leftDelta sectionValue)

deltaNullHonestyExtensional ::
  (Eq section, Show section) =>
  DeltaOps section delta ->
  (delta, section) ->
  QC.Property
deltaNullHonestyExtensional ops (deltaValue, sectionValue) =
  if deltaIsEmpty ops deltaValue
    then deltaApply ops deltaValue sectionValue QC.=== sectionValue
    else QC.property True
