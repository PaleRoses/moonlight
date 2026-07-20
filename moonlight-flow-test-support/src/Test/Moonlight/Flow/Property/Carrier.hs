module Test.Moonlight.Flow.Property.Carrier
  ( carrierMultiplicityIdentity,
    carrierFifoConsume,
    reinsertNonRevival,
    rowMultiplicityUnderflow,
    carrierProperties,
  )
where

import Data.Bifunctor (first)
import Control.Monad (foldM)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Word (Word64)
import Moonlight.Core
  ( mkLiveEpoch,
    mkQueryId,
    mkQuotientEpoch,
    mkSlotId,
  )
import Moonlight.Differential.Proposition (PropositionKey (..))
import Moonlight.Differential.Time (frontierStamp)
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    queryRootCarrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
    carrierAddr,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Flow.Carrier.Fact
  ( carrierLiveEvidenceAt,
    carrierFactRowsAt,
  )
import Moonlight.Flow.Carrier.Store
  ( CarrierStore,
    commitCarrierDelta,
    emptyCarrierStore,
  )
import Moonlight.Flow.Carrier.Core.Origin
  ( OriginEvent (OriginLocal),
    RelationalOrigin (..),
    emptyDerivationRoute,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    mkRelationalCarrierTime,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Delta.Signed
  ( MultiplicityChange (..)
  )
import Moonlight.Differential.Row.Patch
  (
    plainRowPatchFromList,
  )
import Moonlight.Flow.Model.Phase (RelationalPhase (PhaseProject))
import Moonlight.Differential.Row.Tuple
  ( RowTupleKey,
    RepKey (..),
    tupleKeyFromRepKeys,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
    RuntimeBoundaryError,
    mkRuntimeBoundary,
  )
import Test.Moonlight.Flow.Gen.Carrier
  ( CarrierWorkload (..),
    genValidCarrierWorkload,
  )
import Test.Moonlight.Flow.Oracle.Carrier
  ( oracleFinalMultiplicity,
    oracleLiveEvidence,
  )
import Test.Moonlight.Flow.Property.Carrier.Restrict qualified as CarrierRestrict
import Test.Moonlight.Flow.Property.Carrier.RestrictionChain qualified as CarrierRestrictionChain
import Test.Moonlight.Flow.Property.Carrier.Visible.CacheAccounting qualified as CarrierVisibleCacheAccounting
import Test.Moonlight.Flow.Workload
  ( carrierParams,
    mediumParams,
    smallParams,
  )
import Test.QuickCheck
  ( Property,
    counterexample,
    forAll,
    ioProperty,
    property,
    (===),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Moonlight.Flow.Model.Scope
import Moonlight.FiniteLattice
  ( ContextLattice,
    singletonContextLattice
  )
import Moonlight.FiniteLattice
  ( principalSupport
  )

type Ctx = Int

type Prop = Int

type Evidence = String

type TestEventTime = RelationalCarrierTime Ctx

-- Proves semantic-surface invariant: generated insert/retract workloads conserve
-- row multiplicity against a structurally independent arithmetic oracle.
carrierMultiplicityIdentity :: Property
carrierMultiplicityIdentity =
  forAll (genValidCarrierWorkload (carrierParams mediumParams)) $ \workload ->
    ioProperty $ do
      result <- runCarrierWorkload workload
      pure $ case result of
        Left err -> counterexample (show err) False
        Right store ->
          carrierFactRowsAt sourceAddr store
            === expectedRows workload

-- Proves semantic-surface invariant: multiplicity deletion consumes oldest live
-- installed first, checked by evidence labels instead of carrier internals.
carrierFifoConsume :: Property
carrierFifoConsume =
  forAll (genValidCarrierWorkload (carrierParams smallParams)) $ \workload ->
    ioProperty $ do
      result <- runCarrierWorkload workload
      pure $ case result of
        Left err -> counterexample (show err) False
        Right store -> carrierLiveEvidenceAt sourceAddr store === oracleLiveEvidence workload

-- Proves semantic correction: reinsert after consume does not revive consumed seed.
reinsertNonRevival :: Property
reinsertNonRevival =
  ioProperty $ do
    result <- runCarrierWorkload CarrierWorkload {cwInsertCount = 1, cwRetractCount = 1}
    case result of
      Left err -> pure (counterexample err False)
      Right store0 -> do
        reinserted <- insertOne "reinsert" (MultiplicityChange 1) store0
        pure $ case reinserted of
          Left err -> counterexample err False
          Right store -> carrierLiveEvidenceAt sourceAddr store === ["reinsert"]

-- Proves targeted negative invariant: row multiplicity underflow is rejected.
rowMultiplicityUnderflow :: Property
rowMultiplicityUnderflow =
  ioProperty $
    pure $ case genericBoundary of
      Left err -> counterexample (show err) False
      Right boundary ->
        case commitCarrierDelta testLattice (testDelta boundary sourceAddr "bad" (plainRowPatchFromList [(rowA, MultiplicityChange (-1))])) emptyCarrierStore of
          Left _ -> property True
          Right _ -> counterexample "expected row underflow" False


carrierProperties :: TestTree
carrierProperties =
  testGroup
    "carrier"
    [ testProperty "multiplicity/medium/generated" carrierMultiplicityIdentity,
      testProperty "fifo/small/generated" carrierFifoConsume,
      testProperty "no-revival/small" reinsertNonRevival,
      testProperty "row multiplicity underflow" rowMultiplicityUnderflow,
      CarrierRestrict.tests,
      CarrierRestrictionChain.tests,
      CarrierVisibleCacheAccounting.tests
    ]

runCarrierWorkload :: CarrierWorkload -> IO (Either String (CarrierStore Ctx Carrier Prop RuntimeBoundary Evidence))
runCarrierWorkload workload =
  pure $ do
    boundary <- first show genericBoundary
    foldM
      (\store delta -> first show (commitCarrierDelta testLattice delta store))
      emptyCarrierStore
      (workloadDeltas boundary workload)

workloadDeltas :: RuntimeBoundary -> CarrierWorkload -> [RelationalCarrierDelta Ctx Carrier Prop RuntimeBoundary Evidence]
workloadDeltas boundary workload =
  inserts <> retracts
  where
    inserts =
      fmap
        (\ix -> testDelta boundary sourceAddr ("seed-" <> show ix) (plainRowPatchFromList [(rowA, MultiplicityChange 1)]))
        [0 .. cwInsertCount workload - 1]
    retracts =
      fmap
        (\ix -> testDelta boundary sourceAddr ("retract-" <> show ix) (plainRowPatchFromList [(rowA, MultiplicityChange (-1))]))
        [0 .. cwRetractCount workload - 1]

insertOne :: Evidence -> MultiplicityChange -> CarrierStore Ctx Carrier Prop RuntimeBoundary Evidence -> IO (Either String (CarrierStore Ctx Carrier Prop RuntimeBoundary Evidence))
insertOne evidence multiplicity store =
  pure $ do
    boundary <- first show genericBoundary
    first show (commitCarrierDelta testLattice (testDelta boundary sourceAddr evidence (plainRowPatchFromList [(rowA, multiplicity)])) store)


expectedRows :: CarrierWorkload -> RowDelta
expectedRows workload =
  let remaining = oracleFinalMultiplicity workload
   in if remaining == 0
        then plainRowPatchFromList []
        else plainRowPatchFromList [(rowA, MultiplicityChange (fromIntegral remaining))]

testDelta ::
  RuntimeBoundary ->
  CarrierAddr Ctx Carrier Prop ->
  Evidence ->
  RowDelta ->
  RelationalCarrierDelta Ctx Carrier Prop RuntimeBoundary Evidence
testDelta boundary addr evidence rows =
  RelationalCarrierDelta
    { deAddr = addr,
      deTime = eventTime 0,
      deSupport = principalSupport (caContext addr),
      deBoundary = boundary,
      deEvidence = evidence,
      deRows = rows,
      deOrigin = RelationalOrigin {roEvent = OriginLocal (mkQueryId 0), roRoute = emptyDerivationRoute},
      deScope = mempty {rsDeps = DepsDelta IntSet.empty, rsTopo = TopoDelta IntSet.empty},
      dePayload = ()
    }

sourceAddr :: CarrierAddr Ctx Carrier Prop
sourceAddr =
  carrierAddr 0 (PropositionKey 0) (queryRootCarrier (mkQueryId 0))

rowA :: RowTupleKey
rowA =
  tupleKeyFromRepKeys [RepKey 7]

genericBoundary :: Either RuntimeBoundaryError RuntimeBoundary
genericBoundary =
  mkRuntimeBoundary [mkSlotId 0] IntSet.empty IntMap.empty

testLattice :: ContextLattice Ctx
testLattice =
  singletonContextLattice 0

eventTime :: Word64 -> TestEventTime
eventTime seqValue =
  mkRelationalCarrierTime
    0
    (mkQuotientEpoch 1)
    (mkLiveEpoch 1)
    PhaseProject
    (frontierStamp (fromIntegral seqValue))
