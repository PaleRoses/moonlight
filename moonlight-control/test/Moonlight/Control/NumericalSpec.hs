module Moonlight.Control.NumericalSpec
  ( tests,
  )
where

import Data.Functor.Identity (Identity (..), runIdentity)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Control.Fixpoint.Numerical
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

type TestRelaxationResult state residual evidence =
  Either (RelaxationFailure ()) (RelaxationOutcome state residual evidence)

type ScalarAndersonResult =
  Either (AndersonFailure String String) (AndersonOutcome Double Double)

tests :: TestTree
tests =
  testGroup
    "numerical relaxation"
    [ testCase "runRelaxation stops on residual convergence" $
        ( runRelaxation
            RelaxationSpec {rsMaxIterations = 8, rsTolerance = 0 :: Int}
            ( \stateValue ->
                Right
                  RelaxationStep
                    { rstepState = stateValue + 1,
                      rstepResidual = 3 - min 3 (stateValue + 1),
                      rstepEvidence = stateValue + 1
                    }
            )
            (0 :: Int) ::
            TestRelaxationResult Int Int Int
        )
          @?= Right
            RelaxationOutcome
              { roStop = RelaxationConverged,
                roState = 3,
                roResidual = 0,
                roIterations = 3,
                roEvidence = 3
              },
      testCase "runRelaxation reports the last residual when the limit is reached" $
        ( runRelaxation
            RelaxationSpec {rsMaxIterations = 2, rsTolerance = 0 :: Int}
            ( \stateValue ->
                Right
                  RelaxationStep
                    { rstepState = stateValue + 1,
                      rstepResidual = 10 - stateValue,
                      rstepEvidence = ()
                    }
            )
            (0 :: Int) ::
            TestRelaxationResult Int Int ()
        )
          @?= Right
            RelaxationOutcome
              { roStop = RelaxationLimitReached,
                roState = 2,
                roResidual = 9,
                roIterations = 2,
                roEvidence = ()
              },
      testCase "runBlockRelaxation delegates boundary mixing to the caller" $
        ( runBlockRelaxation
            RelaxationSpec {rsMaxIterations = 4, rsTolerance = 0.25 :: Double}
            (\boundary -> Right (boundary + 4.0))
            id
            (\previous projected -> previous + 0.5 * (projected - previous))
            (\previous next -> abs (next - previous))
            (0.0 :: Double) ::
            TestRelaxationResult (BlockRelaxationState Double Double) Double ()
        )
          @?= Right
            RelaxationOutcome
              { roStop = RelaxationLimitReached,
                roState =
                  BlockRelaxationState
                    { brsSolved = 10.0,
                      brsBoundary = 8.0
                    },
                roResidual = 2.0,
                roIterations = 4,
                roEvidence = ()
              },
      testCase "step failures are preserved as typed obstructions" $
        ( runRelaxation
            RelaxationSpec {rsMaxIterations = 3, rsTolerance = 0 :: Int}
            (\_stateValue -> Left "step failed")
            (0 :: Int) ::
            Either (RelaxationFailure String) (RelaxationOutcome Int Int ())
        )
          @?= Left (RelaxationStepFailure "step failed"),
      testCase "non-positive iteration budgets are typed obstructions" $
        ( runRelaxation
            RelaxationSpec {rsMaxIterations = 0, rsTolerance = 0 :: Int}
            (\stateValue -> Right RelaxationStep {rstepState = stateValue, rstepResidual = 0, rstepEvidence = ()})
            (0 :: Int) ::
            TestRelaxationResult Int Int ()
        )
          @?= Left (RelaxationInvalidSpec (RelaxationMaxIterationsNonPositive 0)),
      testCase "Anderson defaults match the reviewed policy" $
        defaultAndersonSpec
          @?= AndersonSpec
            { asMaximumDepth = 3,
              asDamping = 0.8,
              asRelativeTikhonovFactor = 1.0e-8,
              asAbsoluteTikhonovFloor = 1.0e-14,
              asCoefficientInfinityCap = 10.0,
              asDivergenceFactor = 4.0,
              asDivergenceConsecutiveHits = 2,
              asStagnationWindow = 4,
              asMinimumRelativeImprovement = 1.0e-3,
              asMaximumRestarts = 2
            },
      testCase "boundary replacement is template based" $
        bvaReplaceVector metadataScalarAlgebra (7, "kept") (3 :| [])
          @?= Right (3, "kept"),
      testCase "Anderson beats Picard on a linear contraction" andersonBeatsPicard,
      testCase "Anderson history depth ramps to the configured maximum" andersonDepthRamps,
      testCase "damping cannot counterfeit fixed-point convergence" dampingCannotCounterfeitConvergence,
      testCase "collinear residual differences remain regularized" collinearResidualDifferencesRemainRegularized,
      testCase "coefficient cap triggers a typed restart event" coefficientCapTriggersRestart,
      testCase "divergence returns the strict best iterate" divergenceReturnsBestIterate,
      testCase "stagnation returns the earliest strict best iterate" stagnationReturnsBestIterate,
      testCase "dimension changes are typed failures" dimensionChangesAreTypedFailures,
      testCase "non-finite coordinates are typed failures" nonFiniteCoordinatesAreTypedFailures,
      testCase "boundary replacement law violations are typed failures" replacementLawViolationsAreTypedFailures,
      testCase "pure and monadic Anderson entry points agree" pureAndMonadicAndersonAgree,
      testCase "Anderson solver failures remain exact typed obstructions" andersonSolverFailureIsPreserved
    ]

scalarVectorAlgebra :: BoundaryVectorAlgebra Double String
scalarVectorAlgebra =
  BoundaryVectorAlgebra
    { bvaFlatten = \value -> Right (value :| []),
      bvaReplaceVector =
        \_template vectorValue ->
          case NonEmpty.toList vectorValue of
            [value] -> Right value
            coordinates -> Left ("expected one coordinate, got " <> show (length coordinates))
    }

metadataScalarAlgebra :: BoundaryVectorAlgebra (Double, String) String
metadataScalarAlgebra =
  BoundaryVectorAlgebra
    { bvaFlatten = \(value, _metadata) -> Right (value :| []),
      bvaReplaceVector =
        \(_oldValue, metadata) vectorValue ->
          case NonEmpty.toList vectorValue of
            [value] -> Right (value, metadata)
            coordinates -> Left ("expected one coordinate, got " <> show (length coordinates))
    }

listVectorAlgebra :: BoundaryVectorAlgebra [Double] String
listVectorAlgebra =
  BoundaryVectorAlgebra
    { bvaFlatten =
        \coordinates ->
          maybe (Left "empty vector") Right (NonEmpty.nonEmpty coordinates),
      bvaReplaceVector = \_template -> Right . NonEmpty.toList
    }

runScalarAnderson ::
  RelaxationSpec Double ->
  AndersonSpec ->
  (Double -> Double) ->
  Double ->
  ScalarAndersonResult
runScalarAnderson relaxationSpec andersonSpec fixedPointMap =
  runAndersonBlockRelaxation
    relaxationSpec
    andersonSpec
    scalarVectorAlgebra
    (Right . fixedPointMap)
    id
    (\_previous projected -> projected)
    (\previous projected -> abs (projected - previous))

andersonBeatsPicard :: IO ()
andersonBeatsPicard = do
  let relaxationSpec = RelaxationSpec {rsMaxIterations = 80, rsTolerance = 1.0e-9}
      fixedPointMap value = 0.5 * value + 1.0
      picardResult =
        runBlockRelaxation
          relaxationSpec
          (Right . fixedPointMap)
          id
          (\_previous projected -> projected)
          (\previous projected -> abs (projected - previous))
          0.0 ::
          Either (RelaxationFailure String) (RelaxationOutcome (BlockRelaxationState Double Double) Double ())
      andersonResult = runScalarAnderson relaxationSpec defaultAndersonSpec fixedPointMap 0.0
  case (picardResult, andersonResult) of
    (Right picardOutcome, Right andersonOutcome) -> do
      aoStop andersonOutcome @?= AndersonConverged
      assertBool
        ("Anderson evaluations " <> show (aoIterations andersonOutcome) <> " were not below Picard " <> show (roIterations picardOutcome))
        (aoIterations andersonOutcome < roIterations picardOutcome)
    resultPair ->
      assertFailure ("expected both solvers to succeed, got " <> show resultPair)

andersonDepthRamps :: IO ()
andersonDepthRamps =
  case runScalarAnderson relaxationSpec defaultAndersonSpec (+ 1.0) 0.0 of
    Left failure ->
      assertFailure (show failure)
    Right outcome ->
      aoDepthRamp outcome @?= [0, 1, 2, 3]
  where
    relaxationSpec = RelaxationSpec {rsMaxIterations = 4, rsTolerance = 0.0}

dampingCannotCounterfeitConvergence :: IO ()
dampingCannotCounterfeitConvergence =
  case runScalarAnderson relaxationSpec dampedSpec (+ 1.0) 0.0 of
    Left failure ->
      assertFailure (show failure)
    Right outcome -> do
      aoStop outcome @?= AndersonLimitReached
      aoLastResidual outcome @?= 1.0
  where
    relaxationSpec = RelaxationSpec {rsMaxIterations = 1, rsTolerance = 1.0e-6}
    dampedSpec = defaultAndersonSpec {asDamping = 1.0e-12}

collinearResidualDifferencesRemainRegularized :: IO ()
collinearResidualDifferencesRemainRegularized =
  case result of
    Left failure ->
      assertFailure (show failure)
    Right outcome -> do
      assertBool "expected a two-column collinear multisecant solve" (2 `elem` aoDepthRamp outcome)
      assertBool "regularized collinear solve produced a non-finite residual" (not (isNaN (aoLastResidual outcome)))
  where
    result =
      runAndersonBlockRelaxation
        RelaxationSpec {rsMaxIterations = 4, rsTolerance = 0.0}
        defaultAndersonSpec
        listVectorAlgebra
        (\coordinates -> Right (fmap (\value -> 0.5 * value + 1.0) coordinates))
        id
        (\_previous projected -> projected)
        maxAbsListDelta
        [0.0, 0.0] ::
        Either (AndersonFailure String String) (AndersonOutcome [Double] [Double])

coefficientCapTriggersRestart :: IO ()
coefficientCapTriggersRestart =
  case runScalarAnderson relaxationSpec cappedSpec (\value -> 0.5 * value + 1.0) 0.0 of
    Left failure ->
      assertFailure (show failure)
    Right outcome ->
      assertBool "expected a coefficient-cap restart" (any isCoefficientRestart (aoRestarts outcome))
  where
    relaxationSpec = RelaxationSpec {rsMaxIterations = 3, rsTolerance = 0.0}
    cappedSpec = defaultAndersonSpec {asCoefficientInfinityCap = 1.0e-6}

    isCoefficientRestart restartValue =
      case arReason restartValue of
        AndersonCoefficientLimitExceeded _ -> True
        _ -> False

divergenceReturnsBestIterate :: IO ()
divergenceReturnsBestIterate =
  case runScalarAnderson relaxationSpec divergenceSpec (\value -> 10.0 * value + 1.0) 0.0 of
    Left failure ->
      assertFailure (show failure)
    Right outcome -> do
      aoStop outcome @?= AndersonDiverged
      aoBestIteration outcome @?= 1
      assertBool "best residual should be strictly below the last residual" (aoBestResidual outcome < aoLastResidual outcome)
      assertBool
        "best and last iterates should differ after late divergence"
        (brsBoundary (aoBestState outcome) /= brsBoundary (aoLastState outcome))
  where
    relaxationSpec = RelaxationSpec {rsMaxIterations = 8, rsTolerance = 0.0}
    divergenceSpec =
      defaultAndersonSpec
        { asDivergenceConsecutiveHits = 1,
          asMaximumRestarts = 0
        }

stagnationReturnsBestIterate :: IO ()
stagnationReturnsBestIterate =
  case runScalarAnderson relaxationSpec stagnationSpec (+ 1.0) 0.0 of
    Left failure ->
      assertFailure (show failure)
    Right outcome -> do
      aoStop outcome @?= AndersonStagnated
      aoBestIteration outcome @?= 1
      aoBestResidual outcome @?= 1.0
  where
    relaxationSpec = RelaxationSpec {rsMaxIterations = 8, rsTolerance = 0.0}
    stagnationSpec = defaultAndersonSpec {asMaximumRestarts = 0}

dimensionChangesAreTypedFailures :: IO ()
dimensionChangesAreTypedFailures =
  result @?= Left (AndersonDimensionChanged 1 2)
  where
    result =
      runAndersonBlockRelaxation
        RelaxationSpec {rsMaxIterations = 2, rsTolerance = 0.0}
        defaultAndersonSpec
        listVectorAlgebra
        (\_boundary -> Right [0.0, 1.0])
        id
        (\_previous projected -> projected)
        maxAbsListDelta
        [0.0] ::
        Either (AndersonFailure String String) (AndersonOutcome [Double] [Double])

nonFiniteCoordinatesAreTypedFailures :: IO ()
nonFiniteCoordinatesAreTypedFailures =
  case result of
    Left (AndersonNonFiniteCoordinate 0 coordinateValue) ->
      assertBool "expected NaN coordinate evidence" (isNaN coordinateValue)
    otherResult ->
      assertFailure (show otherResult)
  where
    result =
      runScalarAnderson
        RelaxationSpec {rsMaxIterations = 2, rsTolerance = 0.0}
        defaultAndersonSpec
        (const (0.0 / 0.0))
        0.0

replacementLawViolationsAreTypedFailures :: IO ()
replacementLawViolationsAreTypedFailures =
  result @?= Left (AndersonReplacementMismatch 0 0.0 99.0)
  where
    dishonestAlgebra =
      BoundaryVectorAlgebra
        { bvaFlatten = \value -> Right (value :| []),
          bvaReplaceVector = \_template _vectorValue -> Right 99.0
        }
    result =
      runAndersonBlockRelaxation
        RelaxationSpec {rsMaxIterations = 2, rsTolerance = 0.0}
        defaultAndersonSpec
        dishonestAlgebra
        (\_boundary -> Right 1.0)
        id
        (\_previous projected -> projected)
        (\previous projected -> abs (projected - previous))
        0.0 ::
        ScalarAndersonResult

pureAndMonadicAndersonAgree :: IO ()
pureAndMonadicAndersonAgree =
  pureResult @?= monadicResult
  where
    relaxationSpec = RelaxationSpec {rsMaxIterations = 5, rsTolerance = 1.0e-9}
    fixedPointMap value = 0.5 * value + 1.0
    pureResult = runScalarAnderson relaxationSpec defaultAndersonSpec fixedPointMap 0.0
    monadicResult =
      runIdentity
        ( runAndersonBlockRelaxationM
            relaxationSpec
            defaultAndersonSpec
            scalarVectorAlgebra
            (\value -> Identity (Right (fixedPointMap value)))
            id
            (\_previous projected -> projected)
            (\previous projected -> abs (projected - previous))
            0.0
        ) ::
        ScalarAndersonResult

andersonSolverFailureIsPreserved :: IO ()
andersonSolverFailureIsPreserved =
  result @?= Left (AndersonSolveFailure "solve failed")
  where
    result =
      runAndersonBlockRelaxation
        RelaxationSpec {rsMaxIterations = 3, rsTolerance = 0.0}
        defaultAndersonSpec
        scalarVectorAlgebra
        (\_boundary -> Left "solve failed")
        id
        (\_previous projected -> projected)
        (\previous projected -> abs (projected - previous))
        0.0 ::
        ScalarAndersonResult

maxAbsListDelta :: [Double] -> [Double] -> Double
maxAbsListDelta leftValues rightValues =
  maximumOrZero (zipWith (\leftValue rightValue -> abs (rightValue - leftValue)) leftValues rightValues)

maximumOrZero :: [Double] -> Double
maximumOrZero = foldr max 0.0
