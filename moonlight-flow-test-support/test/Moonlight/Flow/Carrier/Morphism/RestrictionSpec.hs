module Moonlight.Flow.Carrier.Morphism.RestrictionSpec
  ( tests,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Set qualified as Set
import Moonlight.Core
  ( mkAtomId,
    mkQueryId,
    mkSlotId,
  )
import Moonlight.Flow.Carrier.Core.Address
  ( Carrier (..),
    queryAtomCarrier,
  )
import Moonlight.Differential.Carrier.Address
  ( CarrierAddr,
    carrierAddr,
  )
import Moonlight.Flow.Model.Schema.Boundary
  ( RuntimeBoundary,
    RuntimeBoundaryError,
    mkRuntimeBoundary,
  )
import Moonlight.Flow.Carrier.Boundary.Restrict
  ( restrictRuntimeBoundary,
  )
import Moonlight.Flow.Carrier.Morphism.Compile
  ( CarrierMorphismCompileError (..),
    compileCarrierMorphism,
  )
import Moonlight.Flow.Carrier.Morphism.Config
  ( CarrierMorphismConfig (..),
    defaultCarrierMorphismConfig,
  )
import Moonlight.Flow.Carrier.Morphism.Restriction
  ( CarrierRestrictionCompileError (..),
    CarrierRestrictionEdgeSpec (..),
    CarrierRestrictionInstallError (..),
    ContextRank (..),
    compileCarrierRestriction,
  )
import Moonlight.Differential.Row.Tuple
import Moonlight.Differential.Context.Restriction
  ( ContextRestrictionEdge (..),
  )
import Moonlight.Differential.Proposition
  ( PropositionKey (..),
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
import Moonlight.FiniteLattice
  ( ContextLattice,
    ContextLatticeCompileError,
    compileContextLattice,
    contextOrderDecl
  )

tests :: TestTree
tests =
  testGroup
    "carrier-restrict compile"
    [ testCase "strict refinement edge compiles" strictEdgeAssertion,
      testCase "source context mismatch is rejected" sourceMismatchAssertion,
      testCase "self edge is rejected" selfEdgeAssertion,
      testCase "non-refinement edge is rejected" nonRefinementAssertion,
      testCase "directed cycle is rejected before install" cycleAssertion,
      testCase "boundary collision is an admitted restriction" boundaryCollisionAssertion,
      testCase "injective boundary restriction rewrites boundary keys" injectiveBoundaryAssertion
    ]

strictEdgeAssertion :: Assertion
strictEdgeAssertion = do
  lattice <- expectRight testLattice
  case compileCarrierRestriction lattice testRank strictEdge (testAddr 2) emptyClassMap of
    Left err ->
      assertFailure (show err)
    Right _ ->
      pure ()

sourceMismatchAssertion :: Assertion
sourceMismatchAssertion = do
  lattice <- expectRight testLattice
  case compileCarrierRestriction lattice testRank strictEdge (testAddr 1) emptyClassMap of
    Left CarrierRestrictionSourceContextMismatch {} ->
      pure ()
    Left _ ->
      assertFailure "expected source mismatch"
    other ->
      assertFailure (unexpectedSuccess other)

selfEdgeAssertion :: Assertion
selfEdgeAssertion = do
  lattice <- expectRight testLattice
  case compileCarrierRestriction lattice testRank (ContextRestrictionEdge 1 1) (testAddr 1) emptyClassMap of
    Left CarrierRestrictionNotStrict {} ->
      pure ()
    Left _ ->
      assertFailure "expected self edge rejection"
    other ->
      assertFailure (unexpectedSuccess other)

nonRefinementAssertion :: Assertion
nonRefinementAssertion = do
  lattice <- expectRight testLattice
  case compileCarrierRestriction lattice testRank (ContextRestrictionEdge 1 2) (testAddr 1) emptyClassMap of
    Left CarrierRestrictionNotRefinement {} ->
      pure ()
    Left _ ->
      assertFailure "expected non-refinement rejection"
    other ->
      assertFailure (unexpectedSuccess other)

cycleAssertion :: Assertion
cycleAssertion = do
  lattice <- expectRight testLattice
  let specs :: [CarrierRestrictionEdgeSpec Int Carrier Int Int]
      specs =
        [ CarrierRestrictionEdgeSpec (ContextRestrictionEdge 2 1) [testAddr 2] IntMap.empty,
          CarrierRestrictionEdgeSpec (ContextRestrictionEdge 1 2) [testAddr 1] IntMap.empty
        ]
      config :: CarrierMorphismConfig Int Int Int ()
      config =
        defaultCarrierMorphismConfig {cmcfgRestrictions = specs}
  case compileCarrierMorphism lattice testRank config of
    Left (CarrierMorphismRestrictionCompileError CarrierRestrictionCycleDetected {}) ->
      pure ()
    Left _ ->
      assertFailure "expected cycle rejection"
    other ->
      assertFailure (unexpectedInstallSuccess other)

boundaryCollisionAssertion :: Assertion
boundaryCollisionAssertion = do
  let classMap = IntMap.fromList [(1, RepKey 0), (2, RepKey 0)]
  boundary <- expectRight collisionBoundary
  expectedBoundary <-
    expectRight
      ( mkRuntimeBoundary
          [mkSlotId 0]
          (IntSet.singleton 0)
          (IntMap.singleton 0 (IntSet.singleton 0))
      )
  restrictRuntimeBoundary classMap boundary @?= Right expectedBoundary

injectiveBoundaryAssertion :: Assertion
injectiveBoundaryAssertion = do
  let classMap = IntMap.fromList [(1, RepKey 10), (2, RepKey 20)]
  boundary <- expectRight collisionBoundary
  expectedBoundary <-
    expectRight
      ( mkRuntimeBoundary
          [mkSlotId 0]
          (IntSet.singleton 0)
          (IntMap.singleton 0 (IntSet.fromList [10, 20]))
      )
  restrictRuntimeBoundary classMap boundary @?= Right expectedBoundary

testLattice :: Either (ContextLatticeCompileError Int) (ContextLattice Int)
testLattice =
  compileContextLattice
    (Set.fromList [0, 1, 2, 3])
    (contextOrderDecl 3 0 [(0, 1), (1, 2), (2, 3)])

testRank :: ContextRank Int
testRank =
  ContextRank id

strictEdge :: ContextRestrictionEdge Int
strictEdge =
  ContextRestrictionEdge 2 1

testAddr :: Int -> CarrierAddr Int Carrier Int
testAddr contextValue =
  carrierAddr contextValue (PropositionKey 0) (queryAtomCarrier (mkQueryId 0) (mkAtomId 0))

collisionBoundary :: Either RuntimeBoundaryError RuntimeBoundary
collisionBoundary =
  mkRuntimeBoundary
    [mkSlotId 0]
    (IntSet.singleton 0)
    (IntMap.singleton 0 (IntSet.fromList [1, 2]))

expectRight :: Show error => Either error value -> IO value
expectRight eitherValue =
  case eitherValue of
    Left errorValue ->
      assertFailure (show errorValue) *> fail "unreachable"
    Right value ->
      pure value

emptyClassMap :: IntMap.IntMap Int
emptyClassMap =
  IntMap.empty

unexpectedSuccess ::
  Either error value ->
  String
unexpectedSuccess eitherValue =
  case eitherValue of
    Left _ ->
      "expected different compile rejection"
    Right _ ->
      "expected compile rejection, got success"

unexpectedInstallSuccess ::
  Either error value ->
  String
unexpectedInstallSuccess eitherValue =
  case eitherValue of
    Left _ ->
      "expected different install rejection"
    Right _ ->
      "expected install rejection, got success"
