module Moonlight.Flow.Carrier.Morphism.AmalgamationSpec
  ( tests,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty
  ( NonEmpty (..),
  )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word
  ( Word64,
  )
import Moonlight.Core
  ( initialLiveEpoch,
    initialQuotientEpoch,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    queryRootCarrier,
  )
import Moonlight.Differential.Carrier.Address
  ( caContext,
    carrierAddr,
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Flow.Plan.Query.Core
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
    mkRuntimeBoundary,
  )
import Moonlight.Differential.Carrier.Topology
  ( CarrierCover,
    carrierCover,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDelta,
    RelationalCarrierDeltaP (..),
  )
import Moonlight.Differential.Row.Patch
  (
    plainRowPatchFromList,
    positivePlainRowPatchRows,
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
import Moonlight.Flow.Model.Phase
  ( RelationalPhase (PhaseProject),
  )
import Moonlight.Flow.Carrier.Morphism.Amalgamation
  ( AmalgamationResult (..),
  )
import Moonlight.Flow.Carrier.Morphism.Engine
  ( runCarrierAmalgamation,
  )
import Moonlight.Delta.Signed
  ( Multiplicity (..),
    MultiplicityChange (..)
  )
import Moonlight.Differential.Proposition
  ( PropositionKey (..),
  )
import Moonlight.Differential.Time
  ( frontierStamp,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    (@?=),
  )
import Moonlight.Flow.Model.Scope
import Moonlight.FiniteLattice
  ( principalSupport
  )

type Ctx = Int

type Prop = Int

type Evidence = [String]

type TestEventTime = RelationalCarrierTime Ctx

eventTime :: Word64 -> TestEventTime
eventTime seqValue =
  mkRelationalCarrierTime
    0
    initialQuotientEpoch
    initialLiveEpoch
    PhaseProject
    (frontierStamp (fromIntegral seqValue))

tests :: TestTree
tests =
  testGroup
    "carrier-amalgamate"
    [ testCase "compatible complete family glues to ExactAmalgamated" exactCompleteFamily,
      testCase "compatible partial family produces LowerBound" partialFamily,
      testCase "incompatible row overlap produces obstruction" rowOverlapObstruction,
      testCase "incompatible boundary overlap produces obstruction" boundaryObstruction
    ]

exactCompleteFamily :: Assertion
exactCompleteFamily = do
  boundary <- boundaryWithKeys [7]
  let result =
        runCarrierAmalgamation
          completeCover
          (carrierDelta 1 boundary 7 :| [carrierDelta 2 boundary 7])
  case result of
    Right (ExactAmalgamatedDelta outputDelta) -> do
      caContext (deAddr outputDelta) @?= 0
      positivePlainRowPatchRows (deRows outputDelta)
        @?= Map.singleton (row 7) (Multiplicity 1)
      deEvidence outputDelta @?= ["ctx-1", "ctx-2"]
    other ->
      assertFailure ("expected ExactAmalgamatedDelta, got " <> show other)

partialFamily :: Assertion
partialFamily = do
  boundary <- boundaryWithKeys [7]
  let result =
        runCarrierAmalgamation
          completeCover
          (carrierDelta 1 boundary 7 :| [])
  case result of
    Right (LowerBoundDelta outputDelta) -> do
      caContext (deAddr outputDelta) @?= 0
    other ->
      assertFailure ("expected LowerBoundDelta, got " <> show other)

rowOverlapObstruction :: Assertion
rowOverlapObstruction = do
  boundary <- boundaryWithKeys [7]
  let result =
        runCarrierAmalgamation
          completeCover
          (carrierDelta 1 boundary 7 :| [carrierDelta 2 boundary 8])
  case result of
    Right (ObstructedAmalgamation obstructions) ->
      assertBool "expected at least one obstruction" (not (null obstructions))
    other ->
      assertFailure ("expected ObstructedAmalgamation, got " <> show other)

boundaryObstruction :: Assertion
boundaryObstruction = do
  leftBoundary <- boundaryWithKeys [7]
  rightBoundary <- boundaryWithKeys [8]
  let result =
        runCarrierAmalgamation
          completeCover
          (carrierDelta 1 leftBoundary 7 :| [carrierDelta 2 rightBoundary 7])
  case result of
    Right (ObstructedAmalgamation obstructions) ->
      assertBool "expected at least one obstruction" (not (null obstructions))
    other ->
      assertFailure ("expected ObstructedAmalgamation, got " <> show other)

completeCover :: CarrierCover Ctx
completeCover =
  carrierCover 0 (Set.fromList [1, 2]) True (principalSupport 0)

carrierDelta ::
  Ctx ->
  RuntimeBoundary ->
  Int ->
  RelationalCarrierDelta Ctx Carrier Prop RuntimeBoundary Evidence
carrierDelta contextValue boundaryValue rowValue =
  RelationalCarrierDelta
    { deAddr =
        carrierAddr contextValue (PropositionKey 0) (queryRootCarrier queryId),
      deTime = eventTime 0,
      deSupport = principalSupport contextValue,
      deBoundary = boundaryValue,
      deEvidence = ["ctx-" <> show contextValue],
      deRows = (plainRowPatchFromList [(row rowValue, MultiplicityChange 1)]),
      deOrigin = RelationalOrigin {roEvent = OriginLocal queryId, roRoute = emptyDerivationRoute},
      deScope =
        mempty
          { rsDeps = DepsDelta (IntSet.singleton rowValue),
            rsTopo = TopoDelta IntSet.empty
          },
      dePayload = ()
    }

boundaryWithKeys :: [Int] -> IO RuntimeBoundary
boundaryWithKeys keys =
  case mkRuntimeBoundary [mkSlotId 0] (IntSet.singleton 0) (IntMap.singleton 0 (IntSet.fromList keys)) of
    Left err ->
      assertFailure ("invalid boundary fixture: " <> show err)
    Right boundary ->
      pure boundary

row :: Int -> RowTupleKey
row value =
  tupleKeyFromRepKeys [RepKey value]

queryId :: QueryId
queryId =
  mkQueryId 0
