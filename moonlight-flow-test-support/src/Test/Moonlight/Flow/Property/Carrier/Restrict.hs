module Test.Moonlight.Flow.Property.Carrier.Restrict
  ( tests,
  )
where

import Data.Bifunctor (first)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Moonlight.Delta.Operator
  ( OpResult (..),
    Operator (..),
  )
import Moonlight.Delta.Time
  ( Timed (..),
  )
import Moonlight.Core
  ( mkQueryId,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    caContext,
    restrictKey,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
  )
import Moonlight.Flow.Carrier.Boundary.Restrict
  ( restrictRuntimeBoundary,
  )
import Moonlight.Flow.Carrier.Core.Delta
  ( RelationalCarrierDeltaP (..),
  )
import Moonlight.Differential.Row.Patch
  ( plainRowPatchFromList,
    positivePlainRowPatchRows,
  )
import Moonlight.Flow.Carrier.Core.Origin
  ( OriginEvent (OriginLocal),
    RelationalOrigin (..),
    emptyDerivationRoute,
    originConsRestriction,
  )
import Moonlight.Flow.Carrier.Morphism.Restriction
  ( CompiledCarrierRestriction (..),
    RestrictionDeltaError (..),
    StrictDescentWitness (..),
    restrictCarrierDelta,
  )
import Moonlight.Flow.Carrier.Morphism.Core.Program
  ( CarrierMorphismContext,
    carrierMorphismContextFromRestrictionPrograms,
    carrierMorphismRestrictionsBetweenFrom,
    lookupCarrierMorphismCompiledRestriction,
    mkCarrierMorphismRuntime,
  )
import Moonlight.Flow.Carrier.Morphism.Engine
  ( carrierMorphismOp,
  )
import Moonlight.Delta.Signed
  ( Multiplicity (..),
    MultiplicityChange (..)
  )
import Moonlight.Differential.Row.Tuple
import Test.Moonlight.Flow.Oracle.Carrier
  ( oracleCarrierAddr,
    oracleCarrierBoundary,
    oracleCarrierDelta,
    oracleCarrierRow,
    oracleCarrierTime,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertFailure,
    testCase,
    (@?=),
  )

type Ctx = Int
type Prop = Int
type Boundary = RuntimeBoundary
tests :: TestTree
tests =
  testGroup
    "carrier-restrict"
    [ testCase "rejects repeated restriction edge in origin path" restrictionLoopAssertion,
      testCase "row collision sums multiplicities" rowCollisionAssertion,
      testCase "restriction graph indexes by source and target" graphIndexesAssertion,
      testCase "restriction operator emits through the operator boundary" restrictionOperatorAssertion
    ]

restrictionLoopAssertion :: Assertion
restrictionLoopAssertion = do
  boundary <- expectRight oracleCarrierBoundary
  let sourceAddr = oracleCarrierAddr 1
      targetAddr = oracleCarrierAddr 2
      restrictionKey = restrictKey sourceAddr targetAddr
      delta =
        ( oracleCarrierDelta
            boundary
            sourceAddr
            (plainRowPatchFromList [(oracleCarrierRow [RepKey 1], MultiplicityChange 1)])
        )
          { deOrigin = originConsRestriction restrictionKey (RelationalOrigin {roEvent = OriginLocal (mkQueryId 0), roRoute = emptyDerivationRoute})
          }
  case restrictCarrierDelta (successfulRestriction sourceAddr targetAddr IntMap.empty) delta of
    Left RestrictionLoopDetected ->
      pure ()
    result ->
      assertFailure (show result)

rowCollisionAssertion :: Assertion
rowCollisionAssertion = do
  boundary <- expectRight oracleCarrierBoundary
  let sourceAddr = oracleCarrierAddr 1
      targetAddr = oracleCarrierAddr 2
      classMap = IntMap.fromList [(1, RepKey 0), (2, RepKey 0)]
      delta =
        oracleCarrierDelta
          boundary
          sourceAddr
          ( plainRowPatchFromList
              [ (oracleCarrierRow [RepKey 1], MultiplicityChange 1),
                (oracleCarrierRow [RepKey 2], MultiplicityChange 1)
              ]
          )
  case restrictCarrierDelta (successfulRestriction sourceAddr targetAddr classMap) delta of
    Left err ->
      assertFailure (show err)
    Right restricted ->
      positivePlainRowPatchRows (deRows restricted)
        @?= Map.singleton (oracleCarrierRow [RepKey 0]) (Multiplicity 2)

restrictionOperatorAssertion :: Assertion
restrictionOperatorAssertion = do
  boundary <- expectRight oracleCarrierBoundary
  let sourceAddr = oracleCarrierAddr 1
      targetAddr = oracleCarrierAddr 2
      classMap = IntMap.fromList [(1, RepKey 0)]
      program = successfulRestriction sourceAddr targetAddr classMap
      sourceRow =
        oracleCarrierRow [RepKey 1]
      targetRow =
        oracleCarrierRow [RepKey 0]
      delta =
        oracleCarrierDelta
          boundary
          sourceAddr
          (plainRowPatchFromList [(sourceRow, MultiplicityChange 1)])
  case
    opStep
      carrierMorphismOp
      (mkCarrierMorphismRuntime (carrierMorphismContextFromRestrictionPrograms [program]))
      (Timed (oracleCarrierTime 7) delta)
    of
    Left err ->
      assertFailure (show err)
    Right result ->
      case orEmit result of
        [Timed emittedAt restricted] -> do
          emittedAt @?= oracleCarrierTime 7
          deAddr restricted @?= targetAddr
          positivePlainRowPatchRows (deRows restricted)
            @?= Map.singleton targetRow (Multiplicity 1)
        _ ->
          assertFailure "expected one restricted emission"

graphIndexesAssertion :: Assertion
graphIndexesAssertion = do
  let sourceAddr = oracleCarrierAddr 1
      targetAddr = oracleCarrierAddr 2
      program =
        successfulRestriction sourceAddr targetAddr IntMap.empty
      contextValue :: CarrierMorphismContext Ctx Carrier Prop Boundary ()
      contextValue =
        carrierMorphismContextFromRestrictionPrograms [program]
      restrictionKey =
        restrictKey sourceAddr targetAddr
  fmap ccrKey (carrierMorphismRestrictionsBetweenFrom 1 2 sourceAddr contextValue) @?= [restrictionKey]
  fmap ccrKey (lookupCarrierMorphismCompiledRestriction restrictionKey contextValue) @?= Just restrictionKey


successfulRestriction ::
  CarrierAddr Ctx Carrier Prop ->
  CarrierAddr Ctx Carrier Prop ->
  IntMap.IntMap RepKey ->
  CompiledCarrierRestriction Ctx Carrier Prop Boundary
successfulRestriction source target classMap =
  CompiledCarrierRestriction
    { ccrKey = restrictKey source target,
      ccrTargetClasses = classMap,
      ccrBoundaryMap = first RestrictionBoundaryFailed . restrictRuntimeBoundary classMap,
      ccrDescentWitness =
        StrictDescentWitness
          { sdwSourceContext = caContext source,
            sdwTargetContext = caContext target,
            sdwRankBefore = 2,
            sdwRankAfter = 1
          }
    }

expectRight :: Show error => Either error value -> IO value
expectRight eitherValue =
  case eitherValue of
    Left errorValue ->
      assertFailure (show errorValue) *> fail "expected Right"
    Right value ->
      pure value
