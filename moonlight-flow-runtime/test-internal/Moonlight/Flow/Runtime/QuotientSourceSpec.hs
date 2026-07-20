module Moonlight.Flow.Runtime.QuotientSourceSpec
  ( tests,
  )
where

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
  ( AtomId,
    QueryId,
    QuotientEpoch,
    mkAtomId,
    mkQueryId,
    mkQuotientEpoch,
  )
import Moonlight.Flow.Model.Delta
  ( AtomEvent (..),
    atomPatchRows,
    QuotientPatch (..),
    ScopedAtomEvents (..),
    mkAtomPatch
  )
import Moonlight.Delta.Signed
  ( Multiplicity (..),
    MultiplicityChange (..)
  )
import Moonlight.Differential.Row.Patch
  (
    EpochTransition (..),
    plainRowPatchFromList,
  )
import Moonlight.Flow.Plan.Query.Core
  ( mkQueryAtomId,
    mkSourceAtomId,
  )
import Moonlight.Flow.Runtime.Core.Patch.Validation
  ( CanonicalityOracle (..),
    PatchRowPolarity (..),
    PatchValidationError (..),
    validateQuotientPatch,
  )
import Moonlight.Flow.Runtime.Topology.Routing.Events
  ( quotientPatchEvents,
  )
import Moonlight.Flow.Runtime.Topology.Routing
  ( RuntimeRouting,
    RuntimeRoutingError,
    Shard (..),
  )
import Moonlight.Flow.Runtime.Topology.Routing.Internal
  ( mkRuntimeRouting,
  )
import Moonlight.Flow.Runtime.Topology.Site.Quotient.Source
  ( QuotientPatchBuildError (..),
    QuotientPatchBuildResult (..),
    QuotientPatchSource (..),
    buildQuotientPatchMaybe,
  )
import Moonlight.Flow.Carrier.Core.Topology
  ( emptyCarrierTopology,
  )
import Moonlight.Flow.Runtime.Topology.Subscription
  ( AtomSubscriptionError (..),
    QueryAtomSubscription (..),
    buildAtomSubscribers,
  )
import Moonlight.Differential.Row.Tuple
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertBool,
    assertFailure,
    testCase,
    (@?=),
  )
import Moonlight.Flow.Model.Scope

tests :: TestTree
tests =
  testGroup
    "quotient patch source"
    [ testCase "constructs inserted rows with positive inserted multiplicities and dirty support" insertPatchAssertion,
      testCase "constructs removed rows with negative canonical row delta" removePatchAssertion,
      testCase "returns Nothing when the repaired quotient snapshot has no row changes" noChangeAssertion,
      testCase "rejects non-canonical representatives at source" nonCanonicalRejectionAssertion,
      testCase "rejects non-positive snapshot multiplicity" nonPositiveMultiplicityRejectionAssertion,
      testCase "rejects row width mismatch" widthMismatchRejectionAssertion,
      testCase "rejects duplicate atom subscriptions" duplicateSubscriptionRejectionAssertion,
      testCase "installed subscribers fan out only to subscribed query atoms" installedSubscriberFanoutAssertion,
      testCase "runtime patch validation rejects stale epoch" runtimeValidationRejectsStaleEpochAssertion,
      testCase "runtime patch validation rejects non-canonical inserted rows" runtimeValidationRejectsNonCanonicalInsertedRowsAssertion,
      testCase "runtime patch validation recomputes dirty sets before routing" runtimeValidationRecomputesDirtySetsAssertion
    ]

insertPatchAssertion :: IO ()
insertPatchAssertion = do
  let source =
        sourceWithRows
          IntMap.empty
          (IntMap.singleton 0 (Map.singleton row7 (Multiplicity 1)))
  case buildQuotientPatchMaybe source of
    Left err ->
      assertFailure (show err)
    Right Nothing ->
      assertFailure "expected non-empty patch"
    Right (Just result) -> do
      qpbrAtomSubscribers result
        @?= IntMap.singleton 0 [(mkQueryId 0, mkAtomId 0)]
      etBefore (qpEpoch (qpbrPatch result)) @?= mkQuotientEpoch 0
      etAfter (qpEpoch (qpbrPatch result)) @?= mkQuotientEpoch 1
      scopeDeps (qpScope (qpbrPatch result)) @?= IntSet.singleton 7
      scopeTopo (qpScope (qpbrPatch result))
        @?= IntSet.fromList [107, 1000]
      fmap scopeDeps (qpAtomScopeByAtom (qpbrPatch result)) @?= IntMap.singleton 0 (IntSet.singleton 7)
      fmap scopeTopo (qpAtomScopeByAtom (qpbrPatch result)) @?= IntMap.singleton 0 (IntSet.fromList [107, 1000])
      fmap atomPatchRows (qpEvents (qpbrPatch result))
        @?= IntMap.singleton 0 (plainRowPatchFromList [(row7, MultiplicityChange 1)])

removePatchAssertion :: IO ()
removePatchAssertion = do
  let source =
        sourceWithRows
          (IntMap.singleton 0 (Map.singleton row7 (Multiplicity 1)))
          IntMap.empty
  case buildQuotientPatchMaybe source of
    Left err ->
      assertFailure (show err)
    Right Nothing ->
      assertFailure "expected non-empty patch"
    Right (Just result) ->
      fmap atomPatchRows (qpEvents (qpbrPatch result))
        @?= IntMap.singleton 0 (plainRowPatchFromList [(row7, MultiplicityChange (-1))])

noChangeAssertion :: IO ()
noChangeAssertion = do
  let rows =
        IntMap.singleton 0 (Map.singleton row7 (Multiplicity 1))
  buildQuotientPatchMaybe (sourceWithRows rows rows) @?= Right Nothing

nonCanonicalRejectionAssertion :: IO ()
nonCanonicalRejectionAssertion = do
  let source =
        (sourceWithRows IntMap.empty (IntMap.singleton 0 (Map.singleton row7 (Multiplicity 1))))
          { qpsCanonicalRepOf =
              \key ->
                if key == 7
                  then Just 3
                  else Just key
          }
  assertBool "expected noncanonical representative rejection" (isNonCanonical (buildQuotientPatchMaybe source))

nonPositiveMultiplicityRejectionAssertion :: IO ()
nonPositiveMultiplicityRejectionAssertion = do
  let source =
        sourceWithRows
          IntMap.empty
          (IntMap.singleton 0 (Map.singleton row7 (Multiplicity 0)))
  assertBool "expected non-positive multiplicity rejection" (isNonPositiveMultiplicity (buildQuotientPatchMaybe source))

widthMismatchRejectionAssertion :: IO ()
widthMismatchRejectionAssertion = do
  let source =
        (sourceWithRows IntMap.empty (IntMap.singleton 0 (Map.singleton row7 (Multiplicity 1))))
          { qpsExpectedRowWidth = const (Just 2)
          }
  assertBool "expected row width mismatch" (isWidthMismatch (buildQuotientPatchMaybe source))

duplicateSubscriptionRejectionAssertion :: IO ()
duplicateSubscriptionRejectionAssertion =
  buildAtomSubscribers [subscription0, subscription0]
    @?= Left (DuplicateAtomSubscription subscription0)

installedSubscriberFanoutAssertion :: IO ()
installedSubscriberFanoutAssertion = do
  let source =
        sourceWithRows
          IntMap.empty
          ( IntMap.fromList
              [ (0, Map.singleton row7 (Multiplicity 1)),
                (1, Map.singleton row8 (Multiplicity 1))
              ]
          )
  case buildQuotientPatchMaybe source of
    Left err ->
      assertFailure (show err)
    Right Nothing ->
      assertFailure "expected non-empty patch"
    Right (Just result) ->
      case testRouting (qpbrAtomSubscribers result) of
        Left err ->
          assertFailure (show err)
        Right routing ->
          let events =
                saeEvents (quotientPatchEvents routing (qpbrPatch result))
           in fmap aeAtomId events @?= [mkAtomId 0]

runtimeValidationRejectsStaleEpochAssertion :: IO ()
runtimeValidationRejectsStaleEpochAssertion =
  case validPatch of
    Left failure ->
      assertFailure failure
    Right patch ->
      validateQuotientPatch identityRuntimeOracle (mkQuotientEpoch 9) patch
        @?= Left (StaleQuotientPatch (mkQuotientEpoch 9) (mkQuotientEpoch 0))

runtimeValidationRejectsNonCanonicalInsertedRowsAssertion :: IO ()
runtimeValidationRejectsNonCanonicalInsertedRowsAssertion =
  case validPatch of
    Left failure ->
      assertFailure failure
    Right patch ->
      case validateQuotientPatch nonCanonicalInsertedOracle (mkQuotientEpoch 0) patch of
        Left (NonCanonicalPatchRow PatchRowInserted epoch atomId rowValue canonicalRow) -> do
          epoch @?= mkQuotientEpoch 1
          atomId @?= mkAtomId 0
          rowValue @?= row7
          canonicalRow @?= row8
        other ->
          assertFailure ("expected non-canonical inserted row rejection, got " <> show other)

runtimeValidationRecomputesDirtySetsAssertion :: IO ()
runtimeValidationRecomputesDirtySetsAssertion =
  case validPatch of
    Left failure ->
      assertFailure failure
    Right patch ->
      case validateQuotientPatch identityRuntimeOracle (mkQuotientEpoch 0) patch of
        Left failure ->
          assertFailure (show failure)
        Right validated -> do
          scopeDeps (qpScope validated) @?= IntSet.singleton 7
          scopeTopo (qpScope validated) @?= IntSet.fromList [107, 1000]
          fmap scopeDeps (qpAtomScopeByAtom validated) @?= IntMap.singleton 0 (IntSet.singleton 7)
          fmap scopeTopo (qpAtomScopeByAtom validated) @?= IntMap.singleton 0 (IntSet.fromList [107, 1000])

sourceWithRows ::
  IntMap (Map RowTupleKey Multiplicity) ->
  IntMap (Map RowTupleKey Multiplicity) ->
  QuotientPatchSource
sourceWithRows beforeRows afterRows =
  QuotientPatchSource
    { qpsEpochBefore = mkQuotientEpoch 0,
      qpsRowsBefore = beforeRows,
      qpsRowsAfter = afterRows,
      qpsCanonicalRepOf = Just,
      qpsExpectedRowWidth = const (Just 1),
      qpsTopoForDirtyKey = \key -> IntSet.singleton (key + 100),
      qpsTopoForAtomKey = \atomKey -> IntSet.singleton (atomKey + 1000),
      qpsExplicitDirtyTopo = IntSet.empty,
      qpsSubscriptions = [subscription0]
    }

validPatch :: Either String QuotientPatch
validPatch =
  case mkAtomPatch Map.empty (Map.singleton row7 (Multiplicity 1)) of
    Left failure ->
      Left (show failure)
    Right atomDelta ->
      Right
        QuotientPatch
          { qpEpoch = EpochTransition { etBefore = mkQuotientEpoch 0, etAfter = mkQuotientEpoch 1 },
            qpScope =
              mempty
                { rsDeps = DepsDelta IntSet.empty,
                  rsTopo = TopoDelta IntSet.empty
                },
            qpAtomScopeByAtom = IntMap.empty,
            qpEvents = IntMap.singleton 0 atomDelta
          }

identityRuntimeOracle :: CanonicalityOracle RowTupleKey
identityRuntimeOracle =
  CanonicalityOracle
    { isCanonicalRowAt = \_epoch _row -> True,
      canonicalizeRowAt = \_epoch row -> row,
      expectedRowWidthAt = \_epoch _atomId -> Just 1,
      dirtyKeysOfRowAt = \_epoch _row -> IntSet.singleton 7,
      dirtyTopoForDirtyKey = \key -> IntSet.singleton (key + 100),
      dirtyTopoForAtom = \_atomId -> IntSet.singleton 1000
    }

nonCanonicalInsertedOracle :: CanonicalityOracle RowTupleKey
nonCanonicalInsertedOracle =
  CanonicalityOracle
    { isCanonicalRowAt = \epoch row -> canonicalRuntimeRow epoch row == row,
      canonicalizeRowAt = canonicalRuntimeRow,
      expectedRowWidthAt = \_epoch _atomId -> Just 1,
      dirtyKeysOfRowAt = \_epoch _row -> IntSet.singleton 7,
      dirtyTopoForDirtyKey = \key -> IntSet.singleton (key + 100),
      dirtyTopoForAtom = \_atomId -> IntSet.singleton 1000
    }

canonicalRuntimeRow :: QuotientEpoch -> RowTupleKey -> RowTupleKey
canonicalRuntimeRow epoch row
  | epoch == mkQuotientEpoch 1 && row == row7 =
      row8
  | otherwise =
      row

subscription0 :: QueryAtomSubscription
subscription0 =
  QueryAtomSubscription
    { qasSourceAtomId = mkSourceAtomId (mkAtomId 0),
      qasQueryId = mkQueryId 0,
      qasQueryAtomId = mkQueryAtomId 0
    }

testRouting ::
  IntMap [(QueryId, AtomId)] ->
  Either (RuntimeRoutingError Int Int) (RuntimeRouting Int Int)
testRouting subscribers =
  mkRuntimeRouting
    subscribers
    emptyCarrierTopology
    (IntMap.singleton 0 0)
    (IntMap.singleton 0 (Shard 0))
    Map.empty
    Map.empty
    Map.empty

isNonCanonical ::
  Either QuotientPatchBuildError value ->
  Bool
isNonCanonical eitherValue =
  case eitherValue of
    Left QuotientPatchNonCanonicalRepresentative {} ->
      True
    _ ->
      False

isNonPositiveMultiplicity ::
  Either QuotientPatchBuildError value ->
  Bool
isNonPositiveMultiplicity eitherValue =
  case eitherValue of
    Left QuotientPatchNonPositiveSnapshotMultiplicity {} ->
      True
    _ ->
      False

isWidthMismatch ::
  Either QuotientPatchBuildError value ->
  Bool
isWidthMismatch eitherValue =
  case eitherValue of
    Left QuotientPatchRowWidthMismatch {} ->
      True
    _ ->
      False

row7 :: RowTupleKey
row7 =
  tupleKeyFromRepKeys [RepKey 7]

row8 :: RowTupleKey
row8 =
  tupleKeyFromRepKeys [RepKey 8]
