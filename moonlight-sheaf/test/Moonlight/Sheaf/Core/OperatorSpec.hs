module Moonlight.Sheaf.Core.OperatorSpec
  ( tests,
  )
where

import Data.Map.Strict qualified as Map
import Moonlight.Homology
  ( BoundaryEntry,
    BoundaryIncidence,
    HomologicalDegree (..),
    boundaryCoefficient,
    boundaryEntries,
    sourceIndex,
    targetIndex,
  )
import Moonlight.Sheaf.Kernel.Basis
  ( mkSheafBasis,
  )
import Moonlight.Sheaf.Operator.BuildError
  ( OperatorBasisRole (..),
    SheafOperatorBuildError (..),
  )
import Moonlight.Sheaf.Operator.GradedComplex
  ( GradedDirection (..),
    GradedOperator,
    mkGradedComplexFromList,
    mkGradedOperator,
  )
import Moonlight.Sheaf.Operator.LinearBasis
  ( LinearBasis,
    LinearCoordinate,
    linearBasisCardinality,
    linearBasisCellDimension,
    linearBasisCellOffset,
    linearBasisCellSlotOrError,
    linearBasisCoordinates,
    linearCoordinateCell,
    linearCoordinateLocalIndex,
    mkLinearBasis,
  )
import Moonlight.Sheaf.Operator.Sparse
  ( BoundaryPairConvention (..),
    mkBoundaryIncidenceFromPairs,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "operator"
    [ testCase "linear basis expands stalk dimensions into stable coordinates" testLinearBasisCoordinates,
      testCase "linear basis rejects negative stalk dimensions" testLinearBasisRejectsNegativeDimension,
      testCase "linear basis reports absent cells as typed operator errors" testLinearBasisAbsentCell,
      testCase "boundary pair conventions orient entries consistently" testBoundaryPairConventionOrientsEntries,
      testCase "graded complex accepts nilpotent adjacent differentials" testGradedComplexAcceptsNilpotentChain,
      testCase "graded complex rejects non-nilpotent adjacent differentials" testGradedComplexRejectsNonNilpotent,
      testCase "graded complex rejects duplicate differential degrees" testGradedComplexRejectsDuplicateDegree,
      testCase "graded complex rejects intermediate basis mismatches" testGradedComplexRejectsBasisMismatch
    ]

testLinearBasisCoordinates :: Assertion
testLinearBasisCoordinates =
  case mkLinearBasis cellDimension (mkSheafBasis ["root", "edge", "face"]) of
    Left buildError ->
      fail ("expected linear basis, got " <> show buildError)
    Right basis -> do
      linearBasisCardinality basis @?= 3
      fmap coordinatePair (linearBasisCoordinates basis)
        @?= [ ("root", 0),
              ("root", 1),
              ("face", 0)
            ]
      linearBasisCellOffset "root" basis @?= Just 0
      linearBasisCellDimension "edge" basis @?= Just 0
      linearBasisCellOffset "face" basis @?= Just 2
  where
    cellDimension :: String -> Int
    cellDimension cell =
      case cell of
        "root" -> 2
        "edge" -> 0
        "face" -> 1
        _ -> 0

    coordinatePair :: LinearCoordinate String -> (String, Int)
    coordinatePair coordinate =
      (linearCoordinateCell coordinate, linearCoordinateLocalIndex coordinate)

testLinearBasisRejectsNegativeDimension :: Assertion
testLinearBasisRejectsNegativeDimension =
  mkLinearBasis (const (-1)) (mkSheafBasis ["bad"])
    @?= Left (OperatorNegativeStalkDimension "bad" (-1))

testLinearBasisAbsentCell :: Assertion
testLinearBasisAbsentCell =
  case mkLinearBasis (const 1) (mkSheafBasis ["present"]) of
    Left buildError ->
      fail ("expected linear basis, got " <> show buildError)
    Right basis ->
      linearBasisCellSlotOrError OperatorSourceBasis basis "missing"
        @?= Left (OperatorCellAbsentFromBasis OperatorSourceBasis "missing")

expectOperator :: Show errorValue => Either errorValue value -> IO value
expectOperator =
  either (assertFailure . ("expected operator construction, received " <>) . show) pure

entryTriple :: BoundaryEntry Int -> (Int, Int, Int)
entryTriple entry =
  (sourceIndex entry, targetIndex entry, boundaryCoefficient entry)

testBoundaryPairConventionOrientsEntries :: Assertion
testBoundaryPairConventionOrientsEntries = do
  sourceTarget <-
    expectOperator
      (operatorIncidence 3 2 SourceTargetPairs (Map.singleton (2, 1) 5))
  rowColumn <-
    expectOperator
      (operatorIncidence 3 2 RowColumnPairs (Map.singleton (1, 2) 5))
  fmap entryTriple (boundaryEntries sourceTarget) @?= [(2, 1, 5)]
  fmap entryTriple (boundaryEntries rowColumn) @?= [(2, 1, 5)]
  where
    operatorIncidence ::
      Int ->
      Int ->
      BoundaryPairConvention ->
      Map.Map (Int, Int) Int ->
      Either (SheafOperatorBuildError String) (BoundaryIncidence Int)
    operatorIncidence =
      mkBoundaryIncidenceFromPairs

scalarBasis :: String -> Either (SheafOperatorBuildError String) (LinearBasis String)
scalarBasis cell =
  mkLinearBasis (const 1) (mkSheafBasis [cell])

planeBasis :: String -> Either (SheafOperatorBuildError String) (LinearBasis String)
planeBasis cell =
  mkLinearBasis (const 2) (mkSheafBasis [cell])

chainDifferentials :: [((Int, Int), Int)] -> Either (SheafOperatorBuildError String) (GradedOperator String Int, GradedOperator String Int)
chainDifferentials collapseEntries = do
  lineBasis <- scalarBasis "x"
  squareBasis <- planeBasis "y"
  expandIncidence <-
    mkBoundaryIncidenceFromPairs 1 2 SourceTargetPairs (Map.fromList [((0, 0), 1), ((0, 1), 1)])
  collapseIncidence <-
    mkBoundaryIncidenceFromPairs 2 1 SourceTargetPairs (Map.fromList collapseEntries)
  expandOperator <- mkGradedOperator (HomologicalDegree 0) lineBasis squareBasis expandIncidence
  collapseOperator <- mkGradedOperator (HomologicalDegree 1) squareBasis lineBasis collapseIncidence
  pure (expandOperator, collapseOperator)

testGradedComplexAcceptsNilpotentChain :: Assertion
testGradedComplexAcceptsNilpotentChain = do
  (expandOperator, collapseOperator) <- expectOperator (chainDifferentials [((0, 0), 1), ((1, 0), -1)])
  case mkGradedComplexFromList DegreeIncreasing [expandOperator, collapseOperator] of
    Right _ ->
      pure ()
    Left buildError ->
      assertFailure ("expected nilpotent chain acceptance, received " <> show buildError)

testGradedComplexRejectsNonNilpotent :: Assertion
testGradedComplexRejectsNonNilpotent = do
  (expandOperator, collapseOperator) <- expectOperator (chainDifferentials [((0, 0), 1), ((1, 0), 1)])
  case mkGradedComplexFromList DegreeIncreasing [expandOperator, collapseOperator] of
    Left (OperatorNonNilpotent (HomologicalDegree 0) (HomologicalDegree 1) _ _) ->
      pure ()
    other ->
      assertFailure ("expected non-nilpotent rejection, received " <> show other)

testGradedComplexRejectsDuplicateDegree :: Assertion
testGradedComplexRejectsDuplicateDegree = do
  (expandOperator, _) <- expectOperator (chainDifferentials [((0, 0), 1), ((1, 0), -1)])
  case mkGradedComplexFromList DegreeIncreasing [expandOperator, expandOperator] of
    Left (OperatorDuplicateDifferentialDegree (HomologicalDegree 0)) ->
      pure ()
    other ->
      assertFailure ("expected duplicate degree rejection, received " <> show other)

testGradedComplexRejectsBasisMismatch :: Assertion
testGradedComplexRejectsBasisMismatch = do
  (expandOperator, _) <- expectOperator (chainDifferentials [((0, 0), 1), ((1, 0), -1)])
  mismatchedCollapse <- expectOperator $ do
    wideBasis <- planeBasis "z"
    lineBasis <- scalarBasis "x"
    collapseIncidence <-
      mkBoundaryIncidenceFromPairs 2 1 SourceTargetPairs (Map.fromList [((0, 0), 1), ((1, 0), -1)])
    mkGradedOperator (HomologicalDegree 1) wideBasis lineBasis collapseIncidence
  case mkGradedComplexFromList DegreeIncreasing [expandOperator, mismatchedCollapse] of
    Left (OperatorIntermediateBasisMismatch (HomologicalDegree 0) (HomologicalDegree 1)) ->
      pure ()
    other ->
      assertFailure ("expected intermediate basis mismatch, received " <> show other)
