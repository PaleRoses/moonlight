module SparseSolverSpec
  ( tests,
  )
where

import Moonlight.Core (MoonlightError)
import Moonlight.LinAlg.Sparse
  ( IC0Config (..),
    SparseConjugateGradientConfig (..),
    SparseGMRESConfig (..),
    SparsePreconditionerFamily (..),
    SparseIterativeFailure (..),
    SparseStationaryIterationConfig (..),
    sparseIterations,
    sparseResidualNorm,
    sparseSolution,
    solveSparseCG,
    solveSparseGMRES,
    solveSparseJacobi,
    solveSparseRichardson,
  )
import Moonlight.LinAlg.Sparse
  ( SparseCSR,
    cooToCSR,
    mkSparseCOO,
  )
import Moonlight.LinAlg.Pure.Sparse.Solver.Preconditioner
  ( compileSparsePreconditioner,
  )
import qualified Data.Vector.Unboxed as U
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "SparseSolver"
    [ testCase "diagonal preconditioner rejects zero diagonal" testDiagonalPreconditionerRejectsZeroDiagonal,
      testCase "identity preconditioner family compiles without matrix assumptions" testIdentityPreconditionerFamilyCompiles,
      testCase "shifted diagonal preconditioner regularizes zero diagonals" testShiftedDiagonalPreconditionerRegularizesZeroDiagonal,
      testCase "SSOR preconditioner rejects invalid relaxation" testSsorPreconditionerRejectsInvalidRelaxation,
      testCase "IC(0) preconditioner rejects nonpositive pivot" testIC0PreconditionerRejectsNonpositivePivot,
      testCase "sparse CG solves a small SPD system" testSparseCGSolvesSpd,
      testCase "sparse CG with diagonal family solves a small SPD system" testSparseCGWithFamilySolvesSpd,
      testCase "sparse CG with shifted diagonal family solves a small SPD system" testSparseCGWithShiftedFamilySolvesSpd,
      testCase "sparse CG with SSOR family solves a small SPD system" testSparseCGWithSsorFamilySolvesSpd,
      testCase "sparse CG with IC(0) family solves an anchored SPD Laplacian" testSparseCGWithIC0FamilySolvesAnchoredLaplacian,
      testCase "restarted sparse GMRES accumulates Arnoldi work across cycles" testRestartedSparseGmresAccumulatesArnoldiWork,
      testCase "sparse Jacobi solves a diagonal system" testSparseJacobiSolvesDiagonal,
      testCase "sparse Richardson solves a diagonal system" testSparseRichardsonSolvesDiagonal
    ]

testDiagonalPreconditionerRejectsZeroDiagonal :: Assertion
testDiagonalPreconditionerRejectsZeroDiagonal =
  withSparseCSRFixture (csrFixture 2 2 [(0, 0, 1.0), (1, 0, 2.0)]) $ \matrixValue ->
    case compileSparsePreconditioner DiagonalJacobiSparsePreconditionerFamily matrixValue of
      Left (SparseInvalidInput _) ->
        pure ()
      Left failureValue ->
        assertFailure ("unexpected sparse preconditioner failure: " <> show failureValue)
      Right _ ->
        assertFailure "expected diagonal preconditioner to reject a zero diagonal"

testIdentityPreconditionerFamilyCompiles :: Assertion
testIdentityPreconditionerFamilyCompiles =
  withSparseCSRFixture (csrFixture 2 2 [(0, 1, 2.0)]) $ \matrixValue ->
    case compileSparsePreconditioner IdentitySparsePreconditionerFamily matrixValue of
      Left failureValue ->
        assertFailure ("identity preconditioner family failed: " <> show failureValue)
      Right _ ->
        pure ()

testShiftedDiagonalPreconditionerRegularizesZeroDiagonal :: Assertion
testShiftedDiagonalPreconditionerRegularizesZeroDiagonal =
  withSparseCSRFixture (csrFixture 2 2 [(0, 1, 1.0), (1, 1, 3.0)]) $ \matrixValue ->
    case compileSparsePreconditioner (ShiftedDiagonalJacobiSparsePreconditionerFamily 0.5) matrixValue of
      Left failureValue ->
        assertFailure ("shifted diagonal preconditioner failed: " <> show failureValue)
      Right _ ->
        pure ()

testSsorPreconditionerRejectsInvalidRelaxation :: Assertion
testSsorPreconditionerRejectsInvalidRelaxation =
  withSparseCSRFixture smallSpdMatrix $ \matrixValue ->
    case compileSparsePreconditioner (SsorSparsePreconditionerFamily 2.0) matrixValue of
      Left (SparseInvalidInput _) ->
        pure ()
      Left failureValue ->
        assertFailure ("unexpected SSOR preconditioner failure: " <> show failureValue)
      Right _ ->
        assertFailure "expected SSOR preconditioner to reject relaxation outside (0, 2)"

testIC0PreconditionerRejectsNonpositivePivot :: Assertion
testIC0PreconditionerRejectsNonpositivePivot =
  withSparseCSRFixture indefiniteSymmetricMatrix $ \matrixValue ->
    case compileSparsePreconditioner (IncompleteCholesky0SparsePreconditionerFamily (IC0Config Nothing)) matrixValue of
      Left (SparseNonpositivePivot 1 _) ->
        pure ()
      Left failureValue ->
        assertFailure ("unexpected IC(0) preconditioner failure: " <> show failureValue)
      Right _ ->
        assertFailure "expected IC(0) preconditioner to reject a nonpositive pivot"

testSparseCGSolvesSpd :: Assertion
testSparseCGSolvesSpd =
  withSparseCSRFixture smallSpdMatrix $ \matrixValue ->
    case solveSparseCG (cgConfigWith IdentitySparsePreconditionerFamily) matrixValue (U.fromList [1.0, 2.0]) (U.fromList [0.0, 0.0]) of
      Left failureValue ->
        assertFailure ("sparse CG failed: " <> show failureValue)
      Right resultValue ->
        assertVectorApprox [1.0 / 11.0, 7.0 / 11.0] (sparseSolution resultValue)

testSparseCGWithFamilySolvesSpd :: Assertion
testSparseCGWithFamilySolvesSpd =
  withSparseCSRFixture smallSpdMatrix $ \matrixValue ->
    case solveSparseCG (cgConfigWith DiagonalJacobiSparsePreconditionerFamily) matrixValue (U.fromList [1.0, 2.0]) (U.fromList [0.0, 0.0]) of
      Left failureValue ->
        assertFailure ("sparse CG with family failed: " <> show failureValue)
      Right resultValue ->
        assertVectorApprox [1.0 / 11.0, 7.0 / 11.0] (sparseSolution resultValue)

testSparseCGWithShiftedFamilySolvesSpd :: Assertion
testSparseCGWithShiftedFamilySolvesSpd =
  withSparseCSRFixture smallSpdMatrix $ \matrixValue ->
    case solveSparseCG (cgConfigWith (ShiftedDiagonalJacobiSparsePreconditionerFamily 0.25)) matrixValue (U.fromList [1.0, 2.0]) (U.fromList [0.0, 0.0]) of
      Left failureValue ->
        assertFailure ("sparse CG with shifted family failed: " <> show failureValue)
      Right resultValue ->
        assertVectorApprox [1.0 / 11.0, 7.0 / 11.0] (sparseSolution resultValue)

testSparseCGWithSsorFamilySolvesSpd :: Assertion
testSparseCGWithSsorFamilySolvesSpd =
  withSparseCSRFixture smallSpdMatrix $ \matrixValue ->
    case solveSparseCG (cgConfigWith (SsorSparsePreconditionerFamily 1.0)) matrixValue (U.fromList [1.0, 2.0]) (U.fromList [0.0, 0.0]) of
      Left failureValue ->
        assertFailure ("sparse CG with SSOR family failed: " <> show failureValue)
      Right resultValue ->
        assertVectorApprox [1.0 / 11.0, 7.0 / 11.0] (sparseSolution resultValue)

testSparseCGWithIC0FamilySolvesAnchoredLaplacian :: Assertion
testSparseCGWithIC0FamilySolvesAnchoredLaplacian =
  withSparseCSRFixture matrixValue $ \csrValue ->
    case solveSparseCG ic0AnchoredLaplacianConfig csrValue rhsValues initialGuess of
      Left failureValue ->
        assertFailure ("sparse CG with IC(0) family failed: " <> show failureValue)
      Right resultValue ->
        assertBool
          ("IC(0) PCG residual too large: " <> show (sparseResidualNorm resultValue))
          (sparseResidualNorm resultValue <= scgcTolerance ic0AnchoredLaplacianConfig)
  where
    dimension = 32
    matrixValue = anchoredPathLaplacian dimension
    rhsValues = anchoredPathRightHandSide dimension
    initialGuess = U.replicate dimension 0.0

testRestartedSparseGmresAccumulatesArnoldiWork :: Assertion
testRestartedSparseGmresAccumulatesArnoldiWork =
  let dimension = 64
      restartDimension = 8
      matrixValue = anchoredPathLaplacian dimension
      rhsValues = anchoredPathRightHandSide dimension
      initialGuess = U.replicate dimension 0.0
   in withSparseCSRFixture matrixValue $ \csrValue ->
        let cgResult = solveSparseCG cgRestartComparisonConfig csrValue rhsValues initialGuess
            gmresResult = solveSparseGMRES (gmresConfigWith restartDimension) csrValue rhsValues initialGuess
         in case (cgResult, gmresResult) of
              (Left failureValue, _) ->
                assertFailure ("sparse CG comparison failed: " <> show failureValue)
              (_, Left failureValue) ->
                assertFailure ("restarted sparse GMRES failed: " <> show failureValue)
              (Right cgValue, Right gmresValue) -> do
                assertBool
                  ("GMRES iterations did not cross a restart boundary: " <> show (sparseIterations gmresValue))
                  (sparseIterations gmresValue > restartDimension)
                assertBool
                  ("GMRES true residual too large: " <> show (sparseResidualNorm gmresValue))
                  (sparseResidualNorm gmresValue <= 1.0e-8)
                assertVectorApproxWith 1.0e-4 (sparseSolution cgValue) (sparseSolution gmresValue)

testSparseJacobiSolvesDiagonal :: Assertion
testSparseJacobiSolvesDiagonal =
  withSparseCSRFixture diagonalMatrix $ \matrixValue ->
    case solveSparseJacobi stationaryConfig matrixValue (U.fromList [4.0, 9.0]) (U.fromList [0.0, 0.0]) of
      Left failureValue ->
        assertFailure ("sparse Jacobi failed: " <> show failureValue)
      Right resultValue ->
        assertVectorApprox [2.0, 3.0] (sparseSolution resultValue)

testSparseRichardsonSolvesDiagonal :: Assertion
testSparseRichardsonSolvesDiagonal =
  withSparseCSRFixture diagonalMatrix $ \matrixValue ->
    case solveSparseRichardson stationaryConfig matrixValue (U.fromList [4.0, 9.0]) (U.fromList [0.0, 0.0]) of
      Left failureValue ->
        assertFailure ("sparse Richardson failed: " <> show failureValue)
      Right resultValue ->
        assertVectorApprox [2.0, 3.0] (sparseSolution resultValue)

smallSpdMatrix :: Either MoonlightError (SparseCSR Double)
smallSpdMatrix =
  csrFixture
    2
    2
    [ (0, 0, 4.0),
      (0, 1, 1.0),
      (1, 0, 1.0),
      (1, 1, 3.0)
    ]

indefiniteSymmetricMatrix :: Either MoonlightError (SparseCSR Double)
indefiniteSymmetricMatrix =
  csrFixture
    2
    2
    [ (0, 0, 1.0),
      (0, 1, 2.0),
      (1, 0, 2.0),
      (1, 1, 1.0)
    ]

diagonalMatrix :: Either MoonlightError (SparseCSR Double)
diagonalMatrix =
  csrFixture
    2
    2
    [ (0, 0, 2.0),
      (1, 1, 3.0)
    ]

anchoredPathLaplacian :: Int -> Either MoonlightError (SparseCSR Double)
anchoredPathLaplacian dimension =
  csrFixture
    dimension
    dimension
    ((0, 0, 1.0) : concatMap anchoredPathEdgeEntries [0 .. dimension - 2])

anchoredPathEdgeEntries :: Int -> [(Int, Int, Double)]
anchoredPathEdgeEntries leftIndex =
  let rightIndex = leftIndex + 1
   in [ (leftIndex, leftIndex, 1.0),
        (leftIndex, rightIndex, -1.0),
        (rightIndex, leftIndex, -1.0),
        (rightIndex, rightIndex, 1.0)
      ]

anchoredPathRightHandSide :: Int -> U.Vector Double
anchoredPathRightHandSide dimension =
  U.generate
    dimension
    ( \indexValue ->
        let entryPhase = fromIntegral (indexValue + 1)
            entrySkew = fromIntegral ((indexValue * 7) `mod` 11)
         in 1.0 + sin entryPhase + 0.125 * entrySkew
    )

csrFixture :: Int -> Int -> [(Int, Int, Double)] -> Either MoonlightError (SparseCSR Double)
csrFixture rowCount columnCount entries =
  mkSparseCOO rowCount columnCount entries >>= cooToCSR

withSparseCSRFixture :: Either MoonlightError (SparseCSR Double) -> (SparseCSR Double -> Assertion) -> Assertion
withSparseCSRFixture fixtureValue onFixture =
  case fixtureValue of
    Left err -> assertFailure ("invalid sparse solver fixture: " <> show err)
    Right csrValue -> onFixture csrValue

cgConfigWith :: SparsePreconditionerFamily -> SparseConjugateGradientConfig
cgConfigWith preconditionerFamily =
  SparseConjugateGradientConfig
    { scgcTolerance = 1.0e-10,
      scgcIterationLimit = 32,
      scgcPreconditionerFamily = preconditionerFamily
    }

gmresConfigWith :: Int -> SparseGMRESConfig
gmresConfigWith restartDimension =
  SparseGMRESConfig
    { sgcTolerance = 1.0e-8,
      sgcIterationLimit = 4096,
      sgcRestartDimension = restartDimension,
      sgcPreconditionerFamily = DiagonalJacobiSparsePreconditionerFamily
    }

cgRestartComparisonConfig :: SparseConjugateGradientConfig
cgRestartComparisonConfig =
  SparseConjugateGradientConfig
    { scgcTolerance = 1.0e-10,
      scgcIterationLimit = 256,
      scgcPreconditionerFamily = IdentitySparsePreconditionerFamily
    }

ic0AnchoredLaplacianConfig :: SparseConjugateGradientConfig
ic0AnchoredLaplacianConfig =
  SparseConjugateGradientConfig
    { scgcTolerance = 1.0e-8,
      scgcIterationLimit = 128,
      scgcPreconditionerFamily = IncompleteCholesky0SparsePreconditionerFamily (IC0Config Nothing)
    }

stationaryConfig :: SparseStationaryIterationConfig
stationaryConfig =
  SparseStationaryIterationConfig
    { ssicTolerance = 1.0e-8,
      ssicIterationLimit = 128,
      ssicDamping = 1.0
    }

assertVectorApprox :: [Double] -> U.Vector Double -> Assertion
assertVectorApprox expectedValues actualValues =
  assertBool
    ("expected " <> show expectedValues <> " but received " <> show actualValues)
    ( and
        ( zipWith
            (\expectedValue actualValue -> abs (expectedValue - actualValue) <= 1.0e-5)
            expectedValues
            (U.toList actualValues)
        )
    )

assertVectorApproxWith :: Double -> U.Vector Double -> U.Vector Double -> Assertion
assertVectorApproxWith tolerance expectedValues actualValues =
  assertBool
    ("expected " <> show expectedValues <> " but received " <> show actualValues)
    ( U.length expectedValues == U.length actualValues
        && U.and (U.zipWith (\expectedValue actualValue -> abs (expectedValue - actualValue) <= tolerance) expectedValues actualValues)
    )
