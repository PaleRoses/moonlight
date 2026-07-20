{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Sheaf.Relational.Carrier.FactSpec
  ( tests,
  )
where

import Control.Monad
  ( foldM,
  )
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
  ( mkLiveEpoch,
    mkQuotientEpoch,
  )
import Moonlight.Differential.Context.Restriction
  ( ContextRestrictionEdge (..),
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
    caProp,
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
import Moonlight.Flow.Carrier.Fact
  ( CarrierFactComparison (..),
    CarrierFactRestrictionError (..),
    CarrierFactRuntime (..),
    CarrierFactSection,
    carrierFactCellRows,
    carrierFactComparisonEmpty,
    carrierFactSectionAddresses,
    carrierFactSectionCellAt,
    carrierFactSectionNow,
    commonCarrierFactSection,
    compareCarrierFactSections,
    mergeCarrierFactSections,
    reconcileCarrierFactRestriction,
    restrictCarrierFactSection,
    restrictedCarrierFactSection,
  )
import Moonlight.Flow.Carrier.Morphism.Restriction
  ( ContextRank (..),
    compileCarrierRestriction,
    CompiledCarrierRestriction,
  )
import Moonlight.Flow.Carrier.Morphism.Core.Program
  ( CarrierMorphismContext,
    carrierMorphismContextFromRestrictionPrograms,
  )
import Moonlight.Flow.Carrier.Store
  ( CarrierStore,
    commitCarrierDelta,
    emptyCarrierStore,
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
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
    RuntimeBoundaryError,
    mkRuntimeBoundary,
  )
import Moonlight.Flow.Plan.Query.Core
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
import Moonlight.FiniteLattice
  ( ContextLattice,
    ContextLatticeCompileError,
    compileContextLattice,
    contextOrderDecl
  )
import Moonlight.FiniteLattice
  ( principalSupport
  )


type Ctx = Int

type Prop = Int

type Evidence = String

type TestEventTime = RelationalCarrierTime Ctx

type FactState = CarrierStore Ctx Carrier Prop RuntimeBoundary Evidence

type FactSection = CarrierFactSection Ctx Carrier Prop RuntimeBoundary Evidence

type FactMorphism = CarrierMorphismContext Ctx Carrier Prop RuntimeBoundary ()

tests :: TestTree
tests =
  testGroup
    "carrier fact subsystem"
    [ testCase "section readout returns only requested context" sectionReadoutAssertion,
      testCase "restriction and reconciliation replace proven target conflict and preserve extras" restrictionReconcileAssertion,
      testCase "missing restriction program is a typed error" missingRestrictionProgramAssertion,
      testCase "comparison distinguishes presence from conflict" comparisonPresenceAssertion,
      testCase "merge keeps non-overlapping addresses" mergeNonOverlappingAssertion,
      testCase "common core keeps only shared equal rows" commonCoreAssertion
    ]

sectionReadoutAssertion :: Assertion
sectionReadoutAssertion = do
  state <-
    stateWithAddressFacts
      [ (sourceAddr, 7, sourceRow, "source"),
        (targetAddr, 9, targetRow, "target")
      ]
  let sourceSection = carrierFactSectionNow 0 state
  carrierFactSectionAddresses sourceSection @?= Set.singleton sourceAddr
  rowsAt sourceAddr sourceSection @?= plainRowPatchFromList [(sourceRow, MultiplicityChange 1)]

restrictionReconcileAssertion :: Assertion
restrictionReconcileAssertion = do
  program <- compiledRestrictionProgram
  runtime <- runtimeWithPrograms [program]
  store0 <-
    stateWithAddressFacts
      [ (sourceAddr, 7, sourceRow, "source"),
        (targetAddr, 8, badTargetRow, "bad-target"),
        (targetAltAddr, 10, altTargetRow, "target-extra")
      ]
  restricted <- expectRight (restrictCarrierFactSection runtime edge01 (carrierFactSectionNow 0 store0))
  repaired <- expectRight (reconcileCarrierFactRestriction runtime restricted (carrierFactSectionNow 1 store0))
  rowsAt targetAddr repaired @?= plainRowPatchFromList [(targetRow, MultiplicityChange 1)]
  rowsAt targetAltAddr repaired @?= plainRowPatchFromList [(altTargetRow, MultiplicityChange 1)]

missingRestrictionProgramAssertion :: Assertion
missingRestrictionProgramAssertion = do
  runtime <- runtimeWithPrograms []
  sourceState <- stateWithFacts [(0, 7, sourceRow, "source")]
  case restrictCarrierFactSection runtime edge01 (carrierFactSectionNow 0 sourceState) of
    Left (CarrierFactMissingRestrictionProgram edge address :| []) -> do
      edge @?= edge01
      address @?= sourceAddr
    Left errors -> assertFailure (show errors)
    Right restricted -> assertFailure (show (carrierFactSectionAddresses (restrictedCarrierFactSection restricted)))

comparisonPresenceAssertion :: Assertion
comparisonPresenceAssertion = do
  leftState <- stateWithAddressFacts [(targetAddr, 9, targetRow, "left")]
  rightState <- stateWithAddressFacts [(targetAltAddr, 10, altTargetRow, "right")]
  let comparison = compareCarrierFactSections (carrierFactSectionNow 1 leftState) (carrierFactSectionNow 1 rightState)
  assertBool "presence differences are not empty comparison" (not (carrierFactComparisonEmpty comparison))
  cfcLeftOnly comparison @?= Set.singleton targetAddr
  cfcRightOnly comparison @?= Set.singleton targetAltAddr
  Map.null (cfcRowConflicts comparison) @?= True
  Map.null (cfcFactConflicts comparison) @?= True

mergeNonOverlappingAssertion :: Assertion
mergeNonOverlappingAssertion = do
  leftState <- stateWithAddressFacts [(targetAddr, 9, targetRow, "left")]
  rightState <- stateWithAddressFacts [(targetAltAddr, 10, altTargetRow, "right")]
  merged <- expectRight (mergeCarrierFactSections (carrierFactSectionNow 1 leftState :| [carrierFactSectionNow 1 rightState]))
  carrierFactSectionAddresses merged @?= Set.fromList [targetAddr, targetAltAddr]

commonCoreAssertion :: Assertion
commonCoreAssertion = do
  leftState <- stateWithAddressFacts [(targetAddr, 9, targetRow, "left"), (targetAltAddr, 10, altTargetRow, "left-extra")]
  rightState <- stateWithAddressFacts [(targetAddr, 9, targetRow, "right")]
  common <- expectRight (commonCarrierFactSection (carrierFactSectionNow 1 leftState :| [carrierFactSectionNow 1 rightState]))
  carrierFactSectionAddresses common @?= Set.singleton targetAddr
  rowsAt targetAddr common @?= plainRowPatchFromList [(targetRow, MultiplicityChange 1)]

rowsAt :: CarrierAddr Ctx Carrier Prop -> FactSection -> RowDelta
rowsAt address section =
  maybe emptyRows carrierFactCellRows (carrierFactSectionCellAt address section)

emptyRows :: RowDelta
emptyRows =
  plainRowPatchFromList []

runtimeWithPrograms :: [CompiledCarrierRestriction Ctx Carrier Prop RuntimeBoundary] -> IO (CarrierFactRuntime Ctx Carrier Prop RuntimeBoundary)
runtimeWithPrograms programs = do
  lattice <- expectRight testLattice
  pure
    CarrierFactRuntime
      { cfrLattice = lattice,
        cfrMorphism = factMorphism programs
      }

factMorphism :: [CompiledCarrierRestriction Ctx Carrier Prop RuntimeBoundary] -> FactMorphism
factMorphism =
  carrierMorphismContextFromRestrictionPrograms

stateWithFacts :: [(Ctx, Int, RowTupleKey, Evidence)] -> IO FactState
stateWithFacts entries =
  stateWithAddressFacts
    (fmap (\(contextValue, boundaryKey, rowValue, evidence) -> (testAddr contextValue, boundaryKey, rowValue, evidence)) entries)

stateWithAddressFacts ::
  [(CarrierAddr Ctx Carrier Prop, Int, RowTupleKey, Evidence)] ->
  IO FactState
stateWithAddressFacts entries = do
  lattice <- expectRight testLattice
  fmap fst (foldM (insertFact lattice) (emptyCarrierStore, 0) entries)
  where
    insertFact lattice (stateValue, sequenceValue) (address, boundaryKey, rowValue, evidence) = do
      boundary <- expectRight (testBoundary boundaryKey)
      let delta =
            (testDeltaAt address boundary evidence (plainRowPatchFromList [(rowValue, MultiplicityChange 1)]))
              { deTime = eventTime sequenceValue
              }
      nextState <- expectRight (commitCarrierDelta lattice delta stateValue)
      pure (nextState, sequenceValue + 1)

compiledRestrictionProgram :: IO (CompiledCarrierRestriction Ctx Carrier Prop RuntimeBoundary)
compiledRestrictionProgram = do
  lattice <- expectRight testLattice
  expectRight
    ( compileCarrierRestriction
        lattice
        testRank
        edge01
        sourceAddr
        (IntMap.singleton 7 (RepKey 9))
    )

edge01 :: ContextRestrictionEdge Ctx
edge01 =
  ContextRestrictionEdge 0 1

testDeltaAt ::
  CarrierAddr Ctx Carrier Prop ->
  RuntimeBoundary ->
  Evidence ->
  RowDelta ->
  RelationalCarrierDelta Ctx Carrier Prop RuntimeBoundary Evidence
testDeltaAt address boundary evidence rows =
  RelationalCarrierDelta
    { deAddr = address,
      deTime = eventTime 0,
      deSupport = principalSupport (caContext address),
      deBoundary = boundary,
      deEvidence = evidence,
      deRows = rows,
      deOrigin = RelationalOrigin {roEvent = OriginLocal (mkQueryId 0), roRoute = emptyDerivationRoute},
      deScope =
        mempty
          { rsDeps = DepsDelta IntSet.empty,
            rsTopo = TopoDelta IntSet.empty
          },
      dePayload = ()
    }

sourceAddr :: CarrierAddr Ctx Carrier Prop
sourceAddr =
  testAddr 0

targetAddr :: CarrierAddr Ctx Carrier Prop
targetAddr =
  testAddr 1

targetAltAddr :: CarrierAddr Ctx Carrier Prop
targetAltAddr =
  (testAddr 1) {caProp = PropositionKey 1}

testAddr :: Ctx -> CarrierAddr Ctx Carrier Prop
testAddr contextValue =
  carrierAddr contextValue (PropositionKey 0) (queryRootCarrier (mkQueryId 0))

sourceRow :: RowTupleKey
sourceRow =
  tupleKeyFromRepKeys [RepKey 7]

targetRow :: RowTupleKey
targetRow =
  tupleKeyFromRepKeys [RepKey 9]

badTargetRow :: RowTupleKey
badTargetRow =
  tupleKeyFromRepKeys [RepKey 8]

altTargetRow :: RowTupleKey
altTargetRow =
  tupleKeyFromRepKeys [RepKey 10]

testBoundary :: Int -> Either RuntimeBoundaryError RuntimeBoundary
testBoundary key =
  mkRuntimeBoundary
    [mkSlotId 0]
    (IntSet.singleton 0)
    (IntMap.singleton 0 (IntSet.singleton key))

testLattice :: Either (ContextLatticeCompileError Ctx) (ContextLattice Ctx)
testLattice =
  compileContextLattice
    (Set.fromList [0, 1])
    (contextOrderDecl 0 1 [(1, 0)])

testRank :: ContextRank Ctx
testRank =
  ContextRank
    (\contextValue ->
      case contextValue of
        0 -> 1
        _ -> 0
    )

eventTime :: Word64 -> TestEventTime
eventTime seqValue =
  mkRelationalCarrierTime
    0
    (mkQuotientEpoch 1)
    (mkLiveEpoch 1)
    PhaseProject
    (frontierStamp (fromIntegral seqValue))

expectRight :: Show error => Either error value -> IO value
expectRight eitherValue =
  case eitherValue of
    Left errorValue -> assertFailure (show errorValue) *> fail "expected Right"
    Right value -> pure value
