module Moonlight.Flow.Runtime.GeneratedPatchSequenceSpec
  ( tests,
  )
where

import Data.Bifunctor (first)
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( QuotientEpoch,
    mkAtomId,
    mkQueryId,
    mkQuotientEpoch,
    nextQuotientEpoch,
  )
import Moonlight.Flow.Model.Delta
  ( atomPatchRows,
    QuotientPatch (..)
  )
import Moonlight.Delta.Signed
  ( Multiplicity (..),
    zeroMultiplicity
  )
import Moonlight.Differential.Row.Patch
  ( applyPlainRowPatchWith
  )
import Moonlight.Flow.Plan.Query.Core
  ( mkQueryAtomId,
    mkSourceAtomId,
  )

import Moonlight.Flow.Runtime.Topology.Site.Quotient.Source
  ( QuotientPatchBuildResult (..),
    QuotientPatchSource (..),
    buildQuotientPatchMaybe,
  )
import Moonlight.Flow.Runtime.Topology.Subscription
  ( QueryAtomSubscription (..),
  )
import Moonlight.Differential.Row.Tuple
import Test.QuickCheck
  ( Gen,
    forAll,
    listOf,
    sublistOf,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( testCase,
    (@?=),
  )
import Test.Tasty.QuickCheck
  ( testProperty,
  )

tests :: TestTree
tests =
  testGroup
    "generated canonical patch sequences"
    [ testCase "folds generated patch deltas to the final snapshot" fixedSequenceAssertion,
      testProperty "snapshot adjacent deltas fold to the final generated snapshot" $
        forAll generatedSnapshots $ \snapshots ->
          case foldSnapshots snapshots of
            Left _ ->
              False
            Right folded ->
              folded == lastSnapshot snapshots
    ]

fixedSequenceAssertion :: IO ()
fixedSequenceAssertion = do
  let snapshots =
        [ IntMap.empty,
          IntMap.singleton 0 (Map.singleton row1 (Multiplicity 1)),
          IntMap.singleton 0 (Map.fromList [(row1, Multiplicity 1), (row2, Multiplicity 1)]),
          IntMap.singleton 0 (Map.singleton row2 (Multiplicity 1))
        ]
  foldSnapshots snapshots @?= Right (lastSnapshot snapshots)

foldSnapshots ::
  [IntMap (Map RowTupleKey Multiplicity)] ->
  Either String (IntMap (Map RowTupleKey Multiplicity))
foldSnapshots snapshots =
  case snapshots of
    [] ->
      Right IntMap.empty
    firstSnapshot : remaining ->
      go (mkQuotientEpoch 0) firstSnapshot firstSnapshot remaining
  where
    go _epoch _prior folded [] =
      Right (normalizeSnapshot folded)
    go epoch prior folded (nextSnapshot : rest) = do
      maybePatch <-
        first show $
          buildQuotientPatchMaybe
            (sourceWithSnapshots epoch prior nextSnapshot)
      case maybePatch of
        Nothing ->
          go (nextQuotientEpoch epoch) nextSnapshot folded rest
        Just result -> do
          foldedNext <-
            IntMap.foldlWithKey'
              applyAtomPatch
              (Right folded)
              (qpEvents (qpbrPatch result))
          go (nextQuotientEpoch epoch) nextSnapshot (normalizeSnapshot foldedNext) rest

    applyAtomPatch eitherSnapshot atomKey atomPatch = do
      snapshot <- eitherSnapshot
      nextRows <-
        applyPlainRowPatchWith
          (\rowValue oldMultiplicity deltaMultiplicity -> show (atomKey, rowValue, oldMultiplicity, deltaMultiplicity))
          (atomPatchRows atomPatch)
          (IntMap.findWithDefault Map.empty atomKey snapshot)
      Right (IntMap.insert atomKey nextRows snapshot)

normalizeSnapshot ::
  IntMap (Map RowTupleKey Multiplicity) ->
  IntMap (Map RowTupleKey Multiplicity)
normalizeSnapshot =
  IntMap.filter (not . Map.null)
    . IntMap.map (Map.filter (/= zeroMultiplicity))

sourceWithSnapshots ::
  QuotientEpoch ->
  IntMap (Map RowTupleKey Multiplicity) ->
  IntMap (Map RowTupleKey Multiplicity) ->
  QuotientPatchSource
sourceWithSnapshots epoch beforeRows afterRows =
  QuotientPatchSource
    { qpsEpochBefore = epoch,
      qpsRowsBefore = beforeRows,
      qpsRowsAfter = afterRows,
      qpsCanonicalRepOf = Just,
      qpsExpectedRowWidth = const (Just 1),
      qpsTopoForDirtyKey = const IntSet.empty,
      qpsTopoForAtomKey = const IntSet.empty,
      qpsExplicitDirtyTopo = IntSet.empty,
      qpsSubscriptions =
        [ QueryAtomSubscription
            { qasSourceAtomId = mkSourceAtomId (mkAtomId 0),
              qasQueryId = mkQueryId 0,
              qasQueryAtomId = mkQueryAtomId 0
            }
        ]
    }

generatedSnapshots :: Gen [IntMap (Map RowTupleKey Multiplicity)]
generatedSnapshots =
  listOf generatedSnapshot

generatedSnapshot :: Gen (IntMap (Map RowTupleKey Multiplicity))
generatedSnapshot =
  rowsToSnapshot <$> sublistOf generatedRows

rowsToSnapshot :: [RowTupleKey] -> IntMap (Map RowTupleKey Multiplicity)
rowsToSnapshot rows =
  if null rows
    then IntMap.empty
    else
      IntMap.singleton
        0
        (Map.fromList (fmap (\rowValue -> (rowValue, Multiplicity 1)) rows))

generatedRows :: [RowTupleKey]
generatedRows =
  fmap
    (\key -> tupleKeyFromRepKeys [RepKey key])
    [0 .. 16]

lastSnapshot ::
  [IntMap (Map RowTupleKey Multiplicity)] ->
  IntMap (Map RowTupleKey Multiplicity)
lastSnapshot snapshots =
  case reverse snapshots of
    [] ->
      IntMap.empty
    snapshot : _ ->
      snapshot

row1 :: RowTupleKey
row1 =
  tupleKeyFromRepKeys [RepKey 1]

row2 :: RowTupleKey
row2 =
  tupleKeyFromRepKeys [RepKey 2]
