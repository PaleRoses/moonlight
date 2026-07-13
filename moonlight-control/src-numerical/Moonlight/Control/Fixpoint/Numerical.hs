{-# LANGUAGE BangPatterns #-}

module Moonlight.Control.Fixpoint.Numerical
  ( RelaxationSpec (..),
    RelaxationSpecError (..),
    RelaxationFailure (..),
    RelaxationStop (..),
    RelaxationStep (..),
    RelaxationOutcome (..),
    BlockRelaxationState (..),
    BoundaryVectorAlgebra (..),
    AndersonSpec (..),
    AndersonSpecError (..),
    AndersonFailure (..),
    AndersonStop (..),
    AndersonRestartReason (..),
    AndersonRestart (..),
    AndersonOutcome (..),
    defaultAndersonSpec,
    runRelaxation,
    runRelaxationM,
    runBlockRelaxation,
    runBlockRelaxationM,
    runAndersonBlockRelaxation,
    runAndersonBlockRelaxationM,
  )
where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.List (find, foldl')
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)

data RelaxationSpec residual = RelaxationSpec
  { rsMaxIterations :: !Int,
    rsTolerance :: !residual
  }
  deriving stock (Eq, Ord, Show, Read)

data RelaxationSpecError
  = RelaxationMaxIterationsNonPositive !Int
  deriving stock (Eq, Ord, Show, Read)

data RelaxationFailure err
  = RelaxationInvalidSpec !RelaxationSpecError
  | RelaxationStepFailure !err
  deriving stock (Eq, Ord, Show, Read)

data RelaxationStop
  = RelaxationConverged
  | RelaxationLimitReached
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

data RelaxationStep state residual evidence = RelaxationStep
  { rstepState :: !state,
    rstepResidual :: !residual,
    rstepEvidence :: !evidence
  }
  deriving stock (Eq, Ord, Show, Read)

data RelaxationOutcome state residual evidence = RelaxationOutcome
  { roStop :: !RelaxationStop,
    roState :: !state,
    roResidual :: !residual,
    roIterations :: !Int,
    roEvidence :: !evidence
  }
  deriving stock (Eq, Ord, Show, Read)

data BlockRelaxationState solved boundary = BlockRelaxationState
  { brsSolved :: !solved,
    brsBoundary :: !boundary
  }
  deriving stock (Eq, Ord, Show, Read)

data BoundaryVectorAlgebra boundary boundaryError = BoundaryVectorAlgebra
  { bvaFlatten :: boundary -> Either boundaryError (NonEmpty Double),
    bvaReplaceVector :: boundary -> NonEmpty Double -> Either boundaryError boundary
  }

data AndersonSpec = AndersonSpec
  { asMaximumDepth :: !Int,
    asDamping :: !Double,
    asRelativeTikhonovFactor :: !Double,
    asAbsoluteTikhonovFloor :: !Double,
    asCoefficientInfinityCap :: !Double,
    asDivergenceFactor :: !Double,
    asDivergenceConsecutiveHits :: !Int,
    asStagnationWindow :: !Int,
    asMinimumRelativeImprovement :: !Double,
    asMaximumRestarts :: !Int
  }
  deriving stock (Eq, Ord, Show, Read)

data AndersonSpecError
  = AndersonMaximumDepthNonPositive !Int
  | AndersonMaximumDepthTooLarge !Int
  | AndersonDampingInvalid !Double
  | AndersonRelativeTikhonovFactorInvalid !Double
  | AndersonAbsoluteTikhonovFloorInvalid !Double
  | AndersonCoefficientInfinityCapInvalid !Double
  | AndersonDivergenceFactorInvalid !Double
  | AndersonDivergenceConsecutiveHitsNonPositive !Int
  | AndersonStagnationWindowTooSmall !Int
  | AndersonMinimumRelativeImprovementInvalid !Double
  | AndersonMaximumRestartsNegative !Int
  | AndersonToleranceInvalid !Double
  deriving stock (Eq, Ord, Show, Read)

data AndersonFailure solveError boundaryError
  = AndersonInvalidRelaxationSpec !RelaxationSpecError
  | AndersonInvalidSpec !AndersonSpecError
  | AndersonSolveFailure !solveError
  | AndersonBoundaryFailure !boundaryError
  | AndersonDimensionChanged !Int !Int
  | AndersonNonFiniteCoordinate !Int !Double
  | AndersonReplacementMismatch !Int !Double !Double
  | AndersonNonFiniteResidual !Double
  | AndersonNegativeResidual !Double
  deriving stock (Eq, Ord, Show, Read)

data AndersonStop
  = AndersonConverged
  | AndersonLimitReached
  | AndersonDiverged
  | AndersonStagnated
  | AndersonRestartLimitReached !AndersonRestartReason
  deriving stock (Eq, Ord, Show, Read)

data AndersonRestartReason
  = AndersonRankDeficient
  | AndersonCoefficientLimitExceeded !Double
  | AndersonCandidateNonFinite
  | AndersonDivergenceDetected
  | AndersonStagnationDetected
  deriving stock (Eq, Ord, Show, Read)

data AndersonRestart = AndersonRestart
  { arIteration :: !Int,
    arActiveDepth :: !Int,
    arReason :: !AndersonRestartReason
  }
  deriving stock (Eq, Ord, Show, Read)

data AndersonOutcome solved boundary = AndersonOutcome
  { aoStop :: !AndersonStop,
    aoBestState :: !(BlockRelaxationState solved boundary),
    aoLastState :: !(BlockRelaxationState solved boundary),
    aoBestResidual :: !Double,
    aoBestIteration :: !Int,
    aoLastResidual :: !Double,
    aoIterations :: !Int,
    aoRestarts :: ![AndersonRestart],
    aoDepthRamp :: ![Int]
  }
  deriving stock (Eq, Ord, Show, Read)

defaultAndersonSpec :: AndersonSpec
defaultAndersonSpec =
  AndersonSpec
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
    }

data RelaxationProgress state residual evidence = RelaxationProgress
  { rpStop :: !(Maybe RelaxationStop),
    rpState :: !state,
    rpResidual :: !residual,
    rpIterations :: !Int,
    rpEvidence :: !evidence
  }

type DenseVector = [Double]

data AndersonSample = AndersonSample
  { sampleIterate :: !DenseVector,
    sampleResidualVector :: !DenseVector
  }

data AndersonBest solved boundary = AndersonBest
  { bestState :: !(BlockRelaxationState solved boundary),
    bestResidual :: !Double,
    bestIteration :: !Int
  }

data AndersonCursor boundary = AndersonCursor
  { cursorExpectedDimension :: !Int,
    cursorBoundary :: !boundary,
    cursorSamples :: ![AndersonSample],
    cursorResiduals :: ![Double],
    cursorDivergenceHits :: !Int,
    cursorRestarts :: ![AndersonRestart],
    cursorDepthRamp :: ![Int]
  }

data AndersonProgress solved boundary = AndersonProgress
  { progressCursor :: !(AndersonCursor boundary),
    progressBest :: !(AndersonBest solved boundary),
    progressLastState :: !(BlockRelaxationState solved boundary),
    progressLastResidual :: !Double,
    progressIterations :: !Int
  }

data AndersonRun solveError boundaryError solved boundary
  = AndersonRunning !(AndersonProgress solved boundary)
  | AndersonFinished !(AndersonOutcome solved boundary)
  | AndersonFailed !(AndersonFailure solveError boundaryError)

data HouseholderState = HouseholderState
  { hsColumns :: ![DenseVector],
    hsRightHandSide :: !DenseVector
  }

runRelaxation ::
  (Ord residual) =>
  RelaxationSpec residual ->
  (state -> Either err (RelaxationStep state residual evidence)) ->
  state ->
  Either (RelaxationFailure err) (RelaxationOutcome state residual evidence)
runRelaxation spec step initialState =
  runIdentity
    ( runRelaxationM
        spec
        (pure . step)
        initialState
    )

runRelaxationM ::
  (Monad m, Ord residual) =>
  RelaxationSpec residual ->
  (state -> m (Either err (RelaxationStep state residual evidence))) ->
  state ->
  m (Either (RelaxationFailure err) (RelaxationOutcome state residual evidence))
runRelaxationM spec step initialState =
  case validateRelaxationSpec spec of
    Left specError ->
      pure (Left (RelaxationInvalidSpec specError))
    Right () ->
      runRelaxationLoop spec step 1 initialState

runBlockRelaxation ::
  (Ord residual) =>
  RelaxationSpec residual ->
  (boundary -> Either err solved) ->
  (solved -> boundary) ->
  (boundary -> boundary -> boundary) ->
  (boundary -> boundary -> residual) ->
  boundary ->
  Either (RelaxationFailure err) (RelaxationOutcome (BlockRelaxationState solved boundary) residual ())
runBlockRelaxation spec solve project mix residual initialBoundary =
  runIdentity
    ( runBlockRelaxationM
        spec
        (pure . solve)
        project
        mix
        residual
        initialBoundary
    )

runBlockRelaxationM ::
  (Monad m, Ord residual) =>
  RelaxationSpec residual ->
  (boundary -> m (Either err solved)) ->
  (solved -> boundary) ->
  (boundary -> boundary -> boundary) ->
  (boundary -> boundary -> residual) ->
  boundary ->
  m (Either (RelaxationFailure err) (RelaxationOutcome (BlockRelaxationState solved boundary) residual ()))
runBlockRelaxationM spec solve project mix residual initialBoundary =
  case validateRelaxationSpec spec of
    Left specError ->
      pure (Left (RelaxationInvalidSpec specError))
    Right () ->
      runBlockRelaxationLoop spec solve project mix residual 1 initialBoundary

runAndersonBlockRelaxation ::
  RelaxationSpec Double ->
  AndersonSpec ->
  BoundaryVectorAlgebra boundary boundaryError ->
  (boundary -> Either solveError solved) ->
  (solved -> boundary) ->
  (boundary -> boundary -> boundary) ->
  (boundary -> boundary -> Double) ->
  boundary ->
  Either (AndersonFailure solveError boundaryError) (AndersonOutcome solved boundary)
runAndersonBlockRelaxation relaxationSpec andersonSpec vectorAlgebra solve project mix residual initialBoundary =
  runIdentity
    ( runAndersonBlockRelaxationM
        relaxationSpec
        andersonSpec
        vectorAlgebra
        (pure . solve)
        project
        mix
        residual
        initialBoundary
    )

runAndersonBlockRelaxationM ::
  (Monad m) =>
  RelaxationSpec Double ->
  AndersonSpec ->
  BoundaryVectorAlgebra boundary boundaryError ->
  (boundary -> m (Either solveError solved)) ->
  (solved -> boundary) ->
  (boundary -> boundary -> boundary) ->
  (boundary -> boundary -> Double) ->
  boundary ->
  m (Either (AndersonFailure solveError boundaryError) (AndersonOutcome solved boundary))
runAndersonBlockRelaxationM relaxationSpec andersonSpec vectorAlgebra solve project mix residual initialBoundary =
  case validateAndersonInputs relaxationSpec andersonSpec of
    Left failure ->
      pure (Left failure)
    Right () ->
      case flattenBoundary vectorAlgebra Nothing initialBoundary of
        Left failure ->
          pure (Left failure)
        Right initialVector -> do
          let !expectedDimension = length initialVector
              !initialCursor =
                AndersonCursor
                  { cursorExpectedDimension = expectedDimension,
                    cursorBoundary = initialBoundary,
                    cursorSamples = [],
                    cursorResiduals = [],
                    cursorDivergenceHits = 0,
                    cursorRestarts = [],
                    cursorDepthRamp = []
                  }
          firstRun <-
            evaluateAndersonIteration
              relaxationSpec
              andersonSpec
              vectorAlgebra
              solve
              project
              mix
              residual
              1
              initialCursor
              Nothing
          finalRun <-
            foldM
              (advanceAndersonRun relaxationSpec andersonSpec vectorAlgebra solve project mix residual)
              firstRun
              [2 .. rsMaxIterations relaxationSpec]
          pure (andersonRunResult finalRun)

validateRelaxationSpec ::
  RelaxationSpec residual ->
  Either RelaxationSpecError ()
validateRelaxationSpec spec
  | rsMaxIterations spec <= 0 =
      Left (RelaxationMaxIterationsNonPositive (rsMaxIterations spec))
  | otherwise =
      Right ()

validateAndersonInputs ::
  RelaxationSpec Double ->
  AndersonSpec ->
  Either (AndersonFailure solveError boundaryError) ()
validateAndersonInputs relaxationSpec andersonSpec = do
  first AndersonInvalidRelaxationSpec (validateRelaxationSpec relaxationSpec)
  first AndersonInvalidSpec (validateAndersonSpec relaxationSpec andersonSpec)

validateAndersonSpec :: RelaxationSpec Double -> AndersonSpec -> Either AndersonSpecError ()
validateAndersonSpec relaxationSpec spec
  | asMaximumDepth spec <= 0 =
      Left (AndersonMaximumDepthNonPositive (asMaximumDepth spec))
  | asMaximumDepth spec > 5 =
      Left (AndersonMaximumDepthTooLarge (asMaximumDepth spec))
  | not (isFinite (asDamping spec)) || asDamping spec <= 0.0 || asDamping spec > 1.0 =
      Left (AndersonDampingInvalid (asDamping spec))
  | not (isFinite (asRelativeTikhonovFactor spec)) || asRelativeTikhonovFactor spec < 0.0 =
      Left (AndersonRelativeTikhonovFactorInvalid (asRelativeTikhonovFactor spec))
  | not (isFinite (asAbsoluteTikhonovFloor spec)) || asAbsoluteTikhonovFloor spec <= 0.0 =
      Left (AndersonAbsoluteTikhonovFloorInvalid (asAbsoluteTikhonovFloor spec))
  | not (isFinite (asCoefficientInfinityCap spec)) || asCoefficientInfinityCap spec <= 0.0 =
      Left (AndersonCoefficientInfinityCapInvalid (asCoefficientInfinityCap spec))
  | not (isFinite (asDivergenceFactor spec)) || asDivergenceFactor spec <= 1.0 =
      Left (AndersonDivergenceFactorInvalid (asDivergenceFactor spec))
  | asDivergenceConsecutiveHits spec <= 0 =
      Left (AndersonDivergenceConsecutiveHitsNonPositive (asDivergenceConsecutiveHits spec))
  | asStagnationWindow spec < 2 =
      Left (AndersonStagnationWindowTooSmall (asStagnationWindow spec))
  | not (isFinite (asMinimumRelativeImprovement spec))
      || asMinimumRelativeImprovement spec < 0.0
      || asMinimumRelativeImprovement spec >= 1.0 =
      Left (AndersonMinimumRelativeImprovementInvalid (asMinimumRelativeImprovement spec))
  | asMaximumRestarts spec < 0 =
      Left (AndersonMaximumRestartsNegative (asMaximumRestarts spec))
  | not (isFinite (rsTolerance relaxationSpec)) || rsTolerance relaxationSpec < 0.0 =
      Left (AndersonToleranceInvalid (rsTolerance relaxationSpec))
  | otherwise =
      Right ()

runRelaxationLoop ::
  (Monad m, Ord residual) =>
  RelaxationSpec residual ->
  (state -> m (Either err (RelaxationStep state residual evidence))) ->
  Int ->
  state ->
  m (Either (RelaxationFailure err) (RelaxationOutcome state residual evidence))
runRelaxationLoop spec step !iteration state = do
  progressResult <-
    relaxationProgressFromStep step spec iteration state
  case progressResult of
    Left failure ->
      pure (Left failure)
    Right progress
      | relaxationShouldStop spec iteration progress ->
          pure (Right (progressOutcome progress))
      | otherwise ->
          runRelaxationLoop spec step (iteration + 1) (rpState progress)

runBlockRelaxationLoop ::
  (Monad m, Ord residual) =>
  RelaxationSpec residual ->
  (boundary -> m (Either err solved)) ->
  (solved -> boundary) ->
  (boundary -> boundary -> boundary) ->
  (boundary -> boundary -> residual) ->
  Int ->
  boundary ->
  m (Either (RelaxationFailure err) (RelaxationOutcome (BlockRelaxationState solved boundary) residual ()))
runBlockRelaxationLoop spec solve project mix residual !iteration boundary = do
  progressResult <-
    blockProgressFromStep solve project mix residual spec iteration boundary
  case progressResult of
    Left failure ->
      pure (Left failure)
    Right progress
      | relaxationShouldStop spec iteration progress ->
          pure (Right (progressOutcome progress))
      | otherwise ->
          runBlockRelaxationLoop
            spec
            solve
            project
            mix
            residual
            (iteration + 1)
            (brsBoundary (rpState progress))

advanceAndersonRun ::
  (Monad m) =>
  RelaxationSpec Double ->
  AndersonSpec ->
  BoundaryVectorAlgebra boundary boundaryError ->
  (boundary -> m (Either solveError solved)) ->
  (solved -> boundary) ->
  (boundary -> boundary -> boundary) ->
  (boundary -> boundary -> Double) ->
  AndersonRun solveError boundaryError solved boundary ->
  Int ->
  m (AndersonRun solveError boundaryError solved boundary)
advanceAndersonRun relaxationSpec andersonSpec vectorAlgebra solve project mix residual runState !iteration =
  case runState of
    AndersonRunning progress ->
      evaluateAndersonIteration
        relaxationSpec
        andersonSpec
        vectorAlgebra
        solve
        project
        mix
        residual
        iteration
        (progressCursor progress)
        (Just (progressBest progress))
    AndersonFinished outcome ->
      pure (AndersonFinished outcome)
    AndersonFailed failure ->
      pure (AndersonFailed failure)

evaluateAndersonIteration ::
  (Monad m) =>
  RelaxationSpec Double ->
  AndersonSpec ->
  BoundaryVectorAlgebra boundary boundaryError ->
  (boundary -> m (Either solveError solved)) ->
  (solved -> boundary) ->
  (boundary -> boundary -> boundary) ->
  (boundary -> boundary -> Double) ->
  Int ->
  AndersonCursor boundary ->
  Maybe (AndersonBest solved boundary) ->
  m (AndersonRun solveError boundaryError solved boundary)
evaluateAndersonIteration relaxationSpec andersonSpec vectorAlgebra solve project mix residual !iteration cursor previousBest = do
  solvedResult <- solve (cursorBoundary cursor)
  let iterationResult = do
        solved <- first AndersonSolveFailure solvedResult
        currentVector <-
          flattenBoundary
            vectorAlgebra
            (Just (cursorExpectedDimension cursor))
            (cursorBoundary cursor)
        let !projectedBoundary = project solved
            !plainBoundary = mix (cursorBoundary cursor) projectedBoundary
        plainVector <-
          flattenBoundary
            vectorAlgebra
            (Just (cursorExpectedDimension cursor))
            plainBoundary
        residualVector <-
          validateCalculatedVector
            (cursorExpectedDimension cursor)
            (zipWith (-) plainVector currentVector)
        fixedPointResidual <- validateResidual (residual (cursorBoundary cursor) plainBoundary)
        evaluatedBoundary <-
          replaceBoundary
            vectorAlgebra
            (cursorExpectedDimension cursor)
            plainBoundary
            currentVector
        let !currentState =
              BlockRelaxationState
                { brsSolved = solved,
                  brsBoundary = evaluatedBoundary
                }
            !currentBest = updateAndersonBest iteration currentState fixedPointResidual previousBest
            !currentSample = AndersonSample currentVector residualVector
            !samples =
              retainLast
                (asMaximumDepth andersonSpec + 1)
                (cursorSamples cursor <> [currentSample])
            !activeDepth = max 0 (length samples - 1)
            !activeDepths =
              take
                (asMaximumDepth andersonSpec + 1)
                (cursorDepthRamp cursor <> [activeDepth])
            !residuals =
              retainLast
                (asStagnationWindow andersonSpec)
                (cursorResiduals cursor <> [fixedPointResidual])
            !divergenceHits =
              nextDivergenceHits
                andersonSpec
                fixedPointResidual
                previousBest
                (cursorDivergenceHits cursor)
            !progress =
              AndersonProgress
                { progressCursor =
                    cursor
                      { cursorSamples = samples,
                        cursorResiduals = residuals,
                        cursorDivergenceHits = divergenceHits,
                        cursorDepthRamp = activeDepths
                      },
                  progressBest = currentBest,
                  progressLastState = currentState,
                  progressLastResidual = fixedPointResidual,
                  progressIterations = iteration
                }
            !safeguardReason =
              detectSafeguardRestart andersonSpec divergenceHits residuals
        if fixedPointResidual <= rsTolerance relaxationSpec
          then Right (AndersonFinished (andersonOutcome AndersonConverged progress))
          else
            case safeguardReason of
              Just restartReason ->
                continueAfterRestart
                  andersonSpec
                  vectorAlgebra
                  plainBoundary
                  currentSample
                  activeDepth
                  restartReason
                  progress
              Nothing
                | iteration >= rsMaxIterations relaxationSpec ->
                    Right (AndersonFinished (andersonOutcome AndersonLimitReached progress))
                | otherwise ->
                    continueWithAndersonCandidate
                      andersonSpec
                      vectorAlgebra
                      plainBoundary
                      currentSample
                      samples
                      activeDepth
                      progress
  pure
    ( case iterationResult of
        Left failure -> AndersonFailed failure
        Right runState -> runState
    )

continueWithAndersonCandidate ::
  AndersonSpec ->
  BoundaryVectorAlgebra boundary boundaryError ->
  boundary ->
  AndersonSample ->
  [AndersonSample] ->
  Int ->
  AndersonProgress solved boundary ->
  Either
    (AndersonFailure solveError boundaryError)
    (AndersonRun solveError boundaryError solved boundary)
continueWithAndersonCandidate spec vectorAlgebra plainBoundary currentSample samples !activeDepth progress = do
  plainCandidate <-
    validateCalculatedVector
      (cursorExpectedDimension (progressCursor progress))
      ( dampedPicardCandidate
          (asDamping spec)
          (sampleIterate currentSample)
          (sampleResidualVector currentSample)
      )
  case andersonCandidate spec (sampleResidualVector currentSample) samples plainCandidate of
    Left restartReason ->
      continueAfterNumericalRestart
        spec
        vectorAlgebra
        plainBoundary
        currentSample
        plainCandidate
        activeDepth
        restartReason
        progress
    Right acceleratedCandidate -> do
      nextBoundary <-
        replaceBoundary
          vectorAlgebra
          (cursorExpectedDimension (progressCursor progress))
          plainBoundary
          acceleratedCandidate
      Right
        ( AndersonRunning
            progress
              { progressCursor =
                  (progressCursor progress)
                    { cursorBoundary = nextBoundary
                    }
              }
        )

continueAfterRestart ::
  AndersonSpec ->
  BoundaryVectorAlgebra boundary boundaryError ->
  boundary ->
  AndersonSample ->
  Int ->
  AndersonRestartReason ->
  AndersonProgress solved boundary ->
  Either
    (AndersonFailure solveError boundaryError)
    (AndersonRun solveError boundaryError solved boundary)
continueAfterRestart spec vectorAlgebra plainBoundary currentSample !activeDepth restartReason progress = do
  plainCandidate <-
    validateCalculatedVector
      (cursorExpectedDimension (progressCursor progress))
      ( dampedPicardCandidate
          (asDamping spec)
          (sampleIterate currentSample)
          (sampleResidualVector currentSample)
      )
  continueAfterNumericalRestart
    spec
    vectorAlgebra
    plainBoundary
    currentSample
    plainCandidate
    activeDepth
    restartReason
    progress

continueAfterNumericalRestart ::
  AndersonSpec ->
  BoundaryVectorAlgebra boundary boundaryError ->
  boundary ->
  AndersonSample ->
  DenseVector ->
  Int ->
  AndersonRestartReason ->
  AndersonProgress solved boundary ->
  Either
    (AndersonFailure solveError boundaryError)
    (AndersonRun solveError boundaryError solved boundary)
continueAfterNumericalRestart spec vectorAlgebra plainBoundary currentSample plainCandidate !activeDepth restartReason progress =
  let !restart =
        AndersonRestart
          { arIteration = progressIterations progress,
            arActiveDepth = activeDepth,
            arReason = restartReason
          }
      !restarts = cursorRestarts (progressCursor progress) <> [restart]
      !stop = restartLimitStop restartReason
   in if length (cursorRestarts (progressCursor progress)) >= asMaximumRestarts spec
        then
          Right
            ( AndersonFinished
                ( andersonOutcome
                    stop
                    progress
                      { progressCursor =
                          (progressCursor progress)
                            { cursorRestarts = restarts
                            }
                      }
                )
            )
        else do
          nextBoundary <-
            replaceBoundary
              vectorAlgebra
              (cursorExpectedDimension (progressCursor progress))
              plainBoundary
              plainCandidate
          Right
            ( AndersonRunning
                progress
                  { progressCursor =
                      (progressCursor progress)
                        { cursorBoundary = nextBoundary,
                          cursorSamples = [currentSample],
                          cursorResiduals = [progressLastResidual progress],
                          cursorDivergenceHits = 0,
                          cursorRestarts = restarts
                        }
                  }
            )

restartLimitStop :: AndersonRestartReason -> AndersonStop
restartLimitStop restartReason =
  case restartReason of
    AndersonDivergenceDetected -> AndersonDiverged
    AndersonStagnationDetected -> AndersonStagnated
    numericalReason -> AndersonRestartLimitReached numericalReason

andersonCandidate :: AndersonSpec -> DenseVector -> [AndersonSample] -> DenseVector -> Either AndersonRestartReason DenseVector
andersonCandidate spec currentResidualVector samples plainCandidate =
  let !samplePairs = zip samples (drop 1 samples)
      !deltaIterates = fmap (uncurry sampleIterateDelta) samplePairs
      !deltaResiduals = fmap (uncurry sampleResidualDelta) samplePairs
      !activeDepth = length deltaResiduals
   in if activeDepth <= 0
        then Right plainCandidate
        else do
          coefficients <-
            first
              (const AndersonRankDeficient)
              ( solveRegularizedLeastSquares
                  spec
                  deltaResiduals
                  currentResidualVector
              )
          let !coefficientInfinityNorm = vectorInfinityNorm coefficients
          if coefficientInfinityNorm > asCoefficientInfinityCap spec
            then Left (AndersonCoefficientLimitExceeded coefficientInfinityNorm)
            else
              let !correctionColumns =
                    zipWith
                      (\deltaIterate deltaResidual ->
                         zipWith
                           (+)
                           deltaIterate
                           (fmap (asDamping spec *) deltaResidual))
                      deltaIterates
                      deltaResiduals
                  !correction =
                    foldl'
                      (zipWith (+))
                      (replicate (length plainCandidate) 0.0)
                      (zipWith (\coefficient -> fmap (coefficient *)) coefficients correctionColumns)
                  !candidate = zipWith (-) plainCandidate correction
               in if all isFinite candidate
                    then Right candidate
                    else Left AndersonCandidateNonFinite

sampleIterateDelta :: AndersonSample -> AndersonSample -> DenseVector
sampleIterateDelta previousSample nextSample =
  zipWith (-) (sampleIterate nextSample) (sampleIterate previousSample)

sampleResidualDelta :: AndersonSample -> AndersonSample -> DenseVector
sampleResidualDelta previousSample nextSample =
  zipWith (-) (sampleResidualVector nextSample) (sampleResidualVector previousSample)

dampedPicardCandidate :: Double -> DenseVector -> DenseVector -> DenseVector
dampedPicardCandidate dampingValue iterateVector residualVector =
  zipWith (+) iterateVector (fmap (dampingValue *) residualVector)

solveRegularizedLeastSquares ::
  AndersonSpec ->
  [DenseVector] ->
  DenseVector ->
  Either () DenseVector
solveRegularizedLeastSquares spec deltaResidualColumns residualVector =
  let !activeDepth = length deltaResidualColumns
      !frobeniusNormSquared = sum (fmap square (concat deltaResidualColumns))
      !regularization =
        max
          (asAbsoluteTikhonovFloor spec)
          (asRelativeTikhonovFactor spec * frobeniusNormSquared / fromIntegral activeDepth)
      !regularizationScale = sqrt regularization
      !augmentedColumns =
        zipWith
          (\columnIndex columnValues ->
             columnValues
               <> [ if rowIndex == columnIndex then regularizationScale else 0.0
                  | rowIndex <- [0 .. activeDepth - 1]
                  ])
          [0 ..]
          deltaResidualColumns
      !augmentedRightHandSide = residualVector <> replicate activeDepth 0.0
   in householderLeastSquares augmentedColumns augmentedRightHandSide

householderLeastSquares :: [DenseVector] -> DenseVector -> Either () DenseVector
householderLeastSquares columns rightHandSide = do
  transformed <-
    foldM
      applyHouseholderColumn
      HouseholderState
        { hsColumns = columns,
          hsRightHandSide = rightHandSide
        }
      [0 .. length columns - 1]
  backSubstitute (hsColumns transformed) (hsRightHandSide transformed)

applyHouseholderColumn :: HouseholderState -> Int -> Either () HouseholderState
applyHouseholderColumn state !columnIndex = do
  pivotColumn <- maybe (Left ()) Right (listAt columnIndex (hsColumns state))
  case drop columnIndex pivotColumn of
    [] ->
      Left ()
    pivotValue : remainingValues ->
      let !pivotNorm = sqrt (sum (fmap square (pivotValue : remainingValues)))
          !signedNorm = if pivotValue >= 0.0 then negate pivotNorm else pivotNorm
          !reflector = (pivotValue - signedNorm) : remainingValues
          !reflectorNormSquared = sum (fmap square reflector)
       in if pivotNorm <= householderRankTolerance || reflectorNormSquared <= square householderRankTolerance
            then Left ()
            else
              Right
                HouseholderState
                  { hsColumns =
                      zipWith
                        (\candidateIndex candidateColumn ->
                           if candidateIndex < columnIndex
                             then candidateColumn
                             else applyHouseholder reflector reflectorNormSquared columnIndex candidateColumn)
                        [0 ..]
                        (hsColumns state),
                    hsRightHandSide =
                      applyHouseholder
                        reflector
                        reflectorNormSquared
                        columnIndex
                        (hsRightHandSide state)
                  }

applyHouseholder :: DenseVector -> Double -> Int -> DenseVector -> DenseVector
applyHouseholder reflector reflectorNormSquared !offset vectorValue =
  let !prefix = take offset vectorValue
      !suffix = drop offset vectorValue
      !scale = 2.0 * dotProduct reflector suffix / reflectorNormSquared
   in prefix <> zipWith (\coordinate reflectorCoordinate -> coordinate - scale * reflectorCoordinate) suffix reflector

backSubstitute :: [DenseVector] -> DenseVector -> Either () DenseVector
backSubstitute columns rightHandSide =
  fmap
    (fmap snd)
    (foldM solveRow [] (reverse [0 .. length columns - 1]))
  where
    solveRow solved !rowIndex = do
      diagonalColumn <- maybe (Left ()) Right (listAt rowIndex columns)
      diagonal <- maybe (Left ()) Right (listAt rowIndex diagonalColumn)
      rightValue <- maybe (Left ()) Right (listAt rowIndex rightHandSide)
      contributions <-
        traverse
          (\(columnIndex, solvedValue) -> do
             upperColumn <- maybe (Left ()) Right (listAt columnIndex columns)
             coefficient <- maybe (Left ()) Right (listAt rowIndex upperColumn)
             Right (coefficient * solvedValue))
          solved
      if abs diagonal <= householderRankTolerance
        then Left ()
        else
          Right ((rowIndex, (rightValue - sum contributions) / diagonal) : solved)

householderRankTolerance :: Double
householderRankTolerance = 1.0e-12

detectSafeguardRestart :: AndersonSpec -> Int -> [Double] -> Maybe AndersonRestartReason
detectSafeguardRestart spec !divergenceHits residuals
  | divergenceHits >= asDivergenceConsecutiveHits spec =
      Just AndersonDivergenceDetected
  | residualWindowStagnated spec residuals =
      Just AndersonStagnationDetected
  | otherwise =
      Nothing

nextDivergenceHits :: AndersonSpec -> Double -> Maybe (AndersonBest solved boundary) -> Int -> Int
nextDivergenceHits spec !residualValue previousBest !previousHits =
  case previousBest of
    Just bestValue
      | residualValue > asDivergenceFactor spec * bestResidual bestValue ->
          previousHits + 1
    _ ->
      0

residualWindowStagnated :: AndersonSpec -> [Double] -> Bool
residualWindowStagnated spec residuals
  | length residuals < asStagnationWindow spec =
      False
  | otherwise =
      case retainLast (asStagnationWindow spec) residuals of
        [] ->
          False
        oldestResidual : remainingResiduals ->
          let !windowBest = foldl' min oldestResidual remainingResiduals
              !relativeImprovement =
                (oldestResidual - windowBest)
                  / max 1.0e-300 (abs oldestResidual)
           in relativeImprovement < asMinimumRelativeImprovement spec

updateAndersonBest ::
  Int ->
  BlockRelaxationState solved boundary ->
  Double ->
  Maybe (AndersonBest solved boundary) ->
  AndersonBest solved boundary
updateAndersonBest !iteration state residualValue previousBest =
  case previousBest of
    Just bestValue
      | bestResidual bestValue <= residualValue ->
          bestValue
    _ ->
      AndersonBest
        { bestState = state,
          bestResidual = residualValue,
          bestIteration = iteration
        }

andersonOutcome :: AndersonStop -> AndersonProgress solved boundary -> AndersonOutcome solved boundary
andersonOutcome stop progress =
  AndersonOutcome
    { aoStop = stop,
      aoBestState = bestState (progressBest progress),
      aoLastState = progressLastState progress,
      aoBestResidual = bestResidual (progressBest progress),
      aoBestIteration = bestIteration (progressBest progress),
      aoLastResidual = progressLastResidual progress,
      aoIterations = progressIterations progress,
      aoRestarts = cursorRestarts (progressCursor progress),
      aoDepthRamp = cursorDepthRamp (progressCursor progress)
    }

andersonRunResult ::
  AndersonRun solveError boundaryError solved boundary ->
  Either (AndersonFailure solveError boundaryError) (AndersonOutcome solved boundary)
andersonRunResult runState =
  case runState of
    AndersonRunning progress ->
      Right (andersonOutcome AndersonLimitReached progress)
    AndersonFinished outcome ->
      Right outcome
    AndersonFailed failure ->
      Left failure

flattenBoundary ::
  BoundaryVectorAlgebra boundary boundaryError ->
  Maybe Int ->
  boundary ->
  Either (AndersonFailure solveError boundaryError) DenseVector
flattenBoundary vectorAlgebra expectedDimension boundary = do
  vectorValue <- first AndersonBoundaryFailure (bvaFlatten vectorAlgebra boundary)
  validateVector expectedDimension (NonEmpty.toList vectorValue)

replaceBoundary ::
  BoundaryVectorAlgebra boundary boundaryError ->
  Int ->
  boundary ->
  DenseVector ->
  Either (AndersonFailure solveError boundaryError) boundary
replaceBoundary vectorAlgebra !expectedDimension template vectorValue = do
  validatedVector <- validateVector (Just expectedDimension) vectorValue
  nonEmptyVector <-
    maybe
      (Left (AndersonDimensionChanged expectedDimension 0))
      Right
      (NonEmpty.nonEmpty validatedVector)
  replacedBoundary <-
    first
      AndersonBoundaryFailure
      (bvaReplaceVector vectorAlgebra template nonEmptyVector)
  replacedVector <- flattenBoundary vectorAlgebra (Just expectedDimension) replacedBoundary
  case find (\(_coordinateIndex, (expectedValue, actualValue)) -> expectedValue /= actualValue) (zip [0 ..] (zip validatedVector replacedVector)) of
    Just (coordinateIndex, (expectedValue, actualValue)) ->
      Left (AndersonReplacementMismatch coordinateIndex expectedValue actualValue)
    Nothing ->
      Right replacedBoundary

validateCalculatedVector ::
  Int ->
  DenseVector ->
  Either (AndersonFailure solveError boundaryError) DenseVector
validateCalculatedVector expectedDimension =
  validateVector (Just expectedDimension)

validateVector ::
  Maybe Int ->
  DenseVector ->
  Either (AndersonFailure solveError boundaryError) DenseVector
validateVector expectedDimension vectorValue =
  case expectedDimension of
    Just expected
      | length vectorValue /= expected ->
          Left (AndersonDimensionChanged expected (length vectorValue))
    _ ->
      case find (not . isFinite . snd) (zip [0 ..] vectorValue) of
        Just (coordinateIndex, coordinateValue) ->
          Left (AndersonNonFiniteCoordinate coordinateIndex coordinateValue)
        Nothing ->
          Right vectorValue

validateResidual :: Double -> Either (AndersonFailure solveError boundaryError) Double
validateResidual residualValue
  | not (isFinite residualValue) =
      Left (AndersonNonFiniteResidual residualValue)
  | residualValue < 0.0 =
      Left (AndersonNegativeResidual residualValue)
  | otherwise =
      Right residualValue

relaxationShouldStop ::
  RelaxationSpec residual ->
  Int ->
  RelaxationProgress state residual evidence ->
  Bool
relaxationShouldStop spec !iteration progress =
  case rpStop progress of
    Just _ ->
      True
    Nothing ->
      iteration >= rsMaxIterations spec

relaxationProgressFromStep ::
  (Monad m, Ord residual) =>
  (state -> m (Either err (RelaxationStep state residual evidence))) ->
  RelaxationSpec residual ->
  Int ->
  state ->
  m (Either (RelaxationFailure err) (RelaxationProgress state residual evidence))
relaxationProgressFromStep step spec !iteration state = do
  stepResult <- step state
  pure
    ( fmap
        (progressFromRelaxationStep spec iteration)
        (first RelaxationStepFailure stepResult)
    )

progressFromRelaxationStep ::
  (Ord residual) =>
  RelaxationSpec residual ->
  Int ->
  RelaxationStep state residual evidence ->
  RelaxationProgress state residual evidence
progressFromRelaxationStep spec !iteration RelaxationStep {rstepState, rstepResidual, rstepEvidence} =
  RelaxationProgress
    { rpStop =
        if rstepResidual <= rsTolerance spec
          then Just RelaxationConverged
          else Nothing,
      rpState = rstepState,
      rpResidual = rstepResidual,
      rpIterations = iteration,
      rpEvidence = rstepEvidence
    }

blockProgressFromStep ::
  (Monad m, Ord residual) =>
  (boundary -> m (Either err solved)) ->
  (solved -> boundary) ->
  (boundary -> boundary -> boundary) ->
  (boundary -> boundary -> residual) ->
  RelaxationSpec residual ->
  Int ->
  boundary ->
  m (Either (RelaxationFailure err) (RelaxationProgress (BlockRelaxationState solved boundary) residual ()))
blockProgressFromStep solve project mix residual spec !iteration previousBoundary = do
  solvedResult <- solve previousBoundary
  pure
    ( fmap
        ( \solved ->
            let !projectedBoundary = project solved
                !nextBoundary = mix previousBoundary projectedBoundary
                !residualValue = residual previousBoundary nextBoundary
             in progressFromRelaxationStep
                  spec
                  iteration
                  RelaxationStep
                    { rstepState =
                        BlockRelaxationState
                          { brsSolved = solved,
                            brsBoundary = nextBoundary
                          },
                      rstepResidual = residualValue,
                      rstepEvidence = ()
                    }
        )
        (first RelaxationStepFailure solvedResult)
    )

progressOutcome :: RelaxationProgress state residual evidence -> RelaxationOutcome state residual evidence
progressOutcome RelaxationProgress {rpStop, rpState, rpResidual, rpIterations, rpEvidence} =
  RelaxationOutcome
    { roStop = fromMaybe RelaxationLimitReached rpStop,
      roState = rpState,
      roResidual = rpResidual,
      roIterations = rpIterations,
      roEvidence = rpEvidence
    }

retainLast :: Int -> [value] -> [value]
retainLast !retainedCount values =
  drop (max 0 (length values - retainedCount)) values

listAt :: Int -> [value] -> Maybe value
listAt !indexValue values
  | indexValue < 0 = Nothing
  | otherwise =
      case drop indexValue values of
        value : _ -> Just value
        [] -> Nothing

dotProduct :: DenseVector -> DenseVector -> Double
dotProduct leftVector rightVector =
  sum (zipWith (*) leftVector rightVector)

vectorInfinityNorm :: DenseVector -> Double
vectorInfinityNorm =
  foldl' (\currentMaximum coordinate -> max currentMaximum (abs coordinate)) 0.0

square :: Double -> Double
square value = value * value

isFinite :: Double -> Bool
isFinite value = not (isNaN value || isInfinite value)
