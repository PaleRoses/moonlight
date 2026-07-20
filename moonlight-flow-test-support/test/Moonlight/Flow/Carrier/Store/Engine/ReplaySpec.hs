module Moonlight.Flow.Carrier.Store.Engine.ReplaySpec
  ( tests,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Word
  ( Word64,
  )
import Moonlight.Core
  ( mkLiveEpoch,
    mkQueryId,
    mkQuotientEpoch,
    mkSlotId,
  )
import Moonlight.Differential.Proposition
  ( PropositionKey (..),
  )
import Moonlight.Differential.Time
  ( frontierStamp,
  )
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
import Moonlight.Flow.Carrier.Core.Origin
  ( OriginEvent (OriginLocal),
    RelationalOrigin (..),
    emptyDerivationRoute,
  )
import Moonlight.Flow.Carrier.Core.Time
  ( RelationalCarrierTime,
    mkRelationalCarrierTime,
  )
import Moonlight.Flow.Carrier.Store
  ( CarrierStore,
    commitCarrierDelta,
    emptyCarrierStore,
    validateCarrierStore,
  )
import Moonlight.Differential.Row.Delta
  ( RowDelta
  )
import Moonlight.Delta.Signed
  ( MultiplicityChange (..)
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchFromList
  )
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (PhaseProject),
  )
import Moonlight.Flow.Model.Scope
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
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( testCase,
    (@?=),
  )
import Moonlight.FiniteLattice
  ( ContextLattice,
    singletonContextLattice
  )
import Moonlight.FiniteLattice
  ( principalSupport
  )


type Ctx = Int

type Prop = Int

type Boundary = RuntimeBoundary

type Evidence = ()

type TestEventTime = RelationalCarrierTime Ctx

tests :: TestTree
tests =
  testGroup
    "carrier-store replay"
    [ testCase "accepts trace-replayable stores" replayAcceptsValidStoreAssertion
    ]

replayAcceptsValidStoreAssertion :: IO ()
replayAcceptsValidStoreAssertion = do
  store <- expectRight validStore
  validateCarrierStore testLattice store @?= Right ()

validStore :: Either String (CarrierStore Ctx Carrier Prop Boundary Evidence)
validStore =
  do
    boundary <- firstShow testBoundary
    firstShow $
      commitCarrierDelta testLattice (testDelta boundary sourceAddr (plainRowPatchFromList [(rowA, MultiplicityChange 1)])) emptyCarrierStore
        >>= commitCarrierDelta testLattice (testDelta boundary sourceAddr (plainRowPatchFromList [(rowB, MultiplicityChange 1)]))

firstShow :: Show error => Either error value -> Either String value
firstShow =
  either (Left . show) Right

testLattice :: ContextLattice Ctx
testLattice =
  singletonContextLattice 1

eventTime :: Word64 -> TestEventTime
eventTime seqValue =
  mkRelationalCarrierTime
    0
    (mkQuotientEpoch 1)
    (mkLiveEpoch 1)
    PhaseProject
    (frontierStamp (fromIntegral seqValue))

sourceAddr :: CarrierAddr Ctx Carrier Prop
sourceAddr =
  carrierAddr 1 (PropositionKey 0) (queryRootCarrier (mkQueryId 0))

testBoundary :: Either RuntimeBoundaryError Boundary
testBoundary =
  mkRuntimeBoundary
    [mkSlotId 0]
    IntSet.empty
    (IntMap.singleton 0 (IntSet.singleton 7))

testDelta ::
  Boundary ->
  CarrierAddr Ctx Carrier Prop ->
  RowDelta ->
  RelationalCarrierDelta Ctx Carrier Prop Boundary Evidence
testDelta boundary addr rows =
  RelationalCarrierDelta
    { deAddr = addr,
      deTime = eventTime 0,
      deSupport = principalSupport (caContext addr),
      deBoundary = boundary,
      deEvidence = (),
      deRows = rows,
      deOrigin = RelationalOrigin {roEvent = OriginLocal (mkQueryId 0), roRoute = emptyDerivationRoute},
      deScope =
        mempty
          { rsDeps = DepsDelta IntSet.empty,
            rsTopo = TopoDelta IntSet.empty
          },
      dePayload = ()
    }

rowA :: RowTupleKey
rowA =
  tupleKeyFromRepKeys [RepKey 7]

rowB :: RowTupleKey
rowB =
  tupleKeyFromRepKeys [RepKey 9]

expectRight :: Show error => Either error value -> IO value
expectRight eitherValue =
  case eitherValue of
    Left errorValue ->
      fail (show errorValue)
    Right value ->
      pure value
