module Moonlight.Cosheaf.Chain.GradedComplexSpec
  ( tests,
  )
where

import Data.Map.Strict qualified as Map
import Moonlight.Homology
  ( BoundaryIncidence,
    emptyBoundaryIncidenceOf,
    identityBoundaryIncidenceOf,
  )
import Moonlight.Sheaf.Kernel.Basis
  ( mkSheafBasis,
  )
import Moonlight.Sheaf.Operator.BuildError
  ( SheafOperatorBuildError (..),
  )
import Moonlight.Homology
  ( HomologicalDegree (..),
    incrementDegree,
  )
import Moonlight.Sheaf.Operator.GradedComplex
  ( GradedComplex,
    GradedDirection (..),
    mkGradedComplex,
    mkGradedOperator,
  )
import Moonlight.Sheaf.Operator.LinearBasis
  ( LinearBasis,
    mkLinearBasis,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "graded linear complex"
    [ testCase "decreasing direction accepts chain adjacency" testDecreasingAcceptsChainAdjacency,
      testCase "increasing direction rejects chain adjacency" testIncreasingRejectsChainAdjacency,
      testCase "decreasing direction rejects non-nilpotent adjacent boundaries" testDecreasingRejectsNonNilpotent
    ]

testDecreasingAcceptsChainAdjacency :: Assertion
testDecreasingAcceptsChainAdjacency =
  case mkDecreasingComplex zeroBoundary of
    Right _ ->
      pure ()
    Left failureValue ->
      assertFailure ("unexpected failure: " <> show failureValue)

testIncreasingRejectsChainAdjacency :: Assertion
testIncreasingRejectsChainAdjacency =
  case mkIncreasingComplex zeroBoundary of
    Left (OperatorIntermediateBasisMismatch degreeValue adjacentDegreeValue)
      | degreeValue == (HomologicalDegree 1) && adjacentDegreeValue == incrementDegree (HomologicalDegree 1) ->
          pure ()
    Left failureValue ->
      assertFailure ("unexpected failure: " <> show failureValue)
    Right _ ->
      assertFailure "expected increasing-direction adjacency failure"

testDecreasingRejectsNonNilpotent :: Assertion
testDecreasingRejectsNonNilpotent =
  case mkDecreasingComplex identityBoundary of
    Left (OperatorNonNilpotent degreeValue adjacentDegreeValue 0 0)
      | degreeValue == incrementDegree (HomologicalDegree 1) && adjacentDegreeValue == (HomologicalDegree 1) ->
          pure ()
    Left failureValue ->
      assertFailure ("unexpected failure: " <> show failureValue)
    Right _ ->
      assertFailure "expected decreasing-direction nilpotence failure"

mkIncreasingComplex ::
  BoundaryIncidence Int ->
  Either (SheafOperatorBuildError String) (GradedComplex String Int)
mkIncreasingComplex firstBoundary =
  mkComplex DegreeIncreasing firstBoundary

mkDecreasingComplex ::
  BoundaryIncidence Int ->
  Either (SheafOperatorBuildError String) (GradedComplex String Int)
mkDecreasingComplex firstBoundary =
  mkComplex DegreeDecreasing firstBoundary

mkComplex ::
  GradedDirection ->
  BoundaryIncidence Int ->
  Either (SheafOperatorBuildError String) (GradedComplex String Int)
mkComplex direction firstBoundary = do
  basisC0 <- dimensionOneBasis "c0"
  basisC1 <- dimensionOneBasis "c1"
  basisC2 <- dimensionOneBasis "c2"
  lowerOperator <- mkGradedOperator (HomologicalDegree 1) basisC1 basisC0 firstBoundary
  upperOperator <- mkGradedOperator (incrementDegree (HomologicalDegree 1)) basisC2 basisC1 identityBoundary
  mkGradedComplex
    direction
    ( Map.fromList
        [ ((HomologicalDegree 1), lowerOperator),
          (incrementDegree (HomologicalDegree 1), upperOperator)
        ]
    )

dimensionOneBasis :: String -> Either (SheafOperatorBuildError String) (LinearBasis String)
dimensionOneBasis cell =
  mkLinearBasis (const 1) (mkSheafBasis [cell])

zeroBoundary :: BoundaryIncidence Int
zeroBoundary =
  emptyBoundaryIncidenceOf 1 1

identityBoundary :: BoundaryIncidence Int
identityBoundary =
  identityBoundaryIncidenceOf 1
