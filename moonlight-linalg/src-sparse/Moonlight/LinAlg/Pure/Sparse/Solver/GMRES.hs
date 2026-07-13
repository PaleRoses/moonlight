{-# LANGUAGE BangPatterns #-}

module Moonlight.LinAlg.Pure.Sparse.Solver.GMRES
  ( solveSparseGMRES,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Kind (Type)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import Moonlight.LinAlg.Pure.Sparse.Solver.Common
  ( solverEpsilon,
    validateSparseSystemInput,
  )
import Moonlight.LinAlg.Pure.Sparse.Solver.Mutable
  ( MutableDoubleVector,
    addScaledMutableVector,
    copyMutableVector,
    csrMatVecIntoMutable,
    dotMutableVector,
    freezeMutableDoubleVector,
    newMutableDoubleVector,
    normMutableVector,
    residualIntoMutable,
    scaleMutableVector,
    thawMutableDoubleVector,
  )
import Moonlight.LinAlg.Pure.Sparse.Solver.Preconditioner
  ( SparsePreconditioner,
    applySparsePreconditionerMutable,
    compileSparsePreconditioner,
    preconditionerDimension,
  )
import Moonlight.LinAlg.Pure.Sparse.Solver.Types
  ( SparseGMRESConfig (..),
    SparseIterativeFailure (..),
    SparseIterativeResult (..),
  )
import Moonlight.LinAlg.Pure.Sparse.Types (SparseCSR, csrRows)
import Prelude

solveSparseGMRES ::
  SparseGMRESConfig ->
  SparseCSR Double ->
  U.Vector Double ->
  U.Vector Double ->
  Either SparseIterativeFailure SparseIterativeResult
solveSparseGMRES config sparseMatrix rhsValues initialGuess = do
  preconditioner <- compileSparsePreconditioner (sgcPreconditionerFamily config) sparseMatrix
  solveSparseGMRESPreconditioned config preconditioner sparseMatrix rhsValues initialGuess

solveSparseGMRESPreconditioned ::
  SparseGMRESConfig ->
  SparsePreconditioner ->
  SparseCSR Double ->
  U.Vector Double ->
  U.Vector Double ->
  Either SparseIterativeFailure SparseIterativeResult
solveSparseGMRESPreconditioned config preconditioner sparseMatrix rhsValues initialGuess = do
  validateSparseSystemInput sparseMatrix rhsValues initialGuess
  validateGmresPreconditioner sparseMatrix preconditioner
  if sgcRestartDimension config <= 0
    then Left (SparseInvalidInput "GMRES restart dimension must be positive")
    else
      if sgcIterationLimit config < 0
        then Left (SparseInvalidInput "GMRES iteration limit must be non-negative")
        else runST (solveSparseGMRESMutable config preconditioner sparseMatrix rhsValues initialGuess)

solveSparseGMRESMutable ::
  SparseGMRESConfig ->
  SparsePreconditioner ->
  SparseCSR Double ->
  U.Vector Double ->
  U.Vector Double ->
  ST s (Either SparseIterativeFailure SparseIterativeResult)
solveSparseGMRESMutable config preconditioner sparseMatrix rhsValues initialGuess = do
  let !dimension = csrRows sparseMatrix
      !restartDimension = sgcRestartDimension config
      !restartCycles = restartCycleCount (sgcIterationLimit config) restartDimension
  workspace <- newGMRESWorkspace dimension restartDimension
  currentGuess <- thawMutableDoubleVector initialGuess
  finalState <-
    U.foldM'
      (gmresRestartCycle config preconditioner sparseMatrix rhsValues currentGuess workspace)
      (GmresRunning 0)
      (U.enumFromN 0 restartCycles)
  gmresResultFromState (sgcIterationLimit config) currentGuess finalState

type GMRESWorkspace :: Type -> Type
data GMRESWorkspace s = GMRESWorkspace
  { gmresBasisPayload :: !(MutableDoubleVector s),
    gmresPreconditionedPayload :: !(MutableDoubleVector s),
    gmresHessenbergPayload :: !(MutableDoubleVector s),
    gmresCosines :: !(MutableDoubleVector s),
    gmresSines :: !(MutableDoubleVector s),
    gmresProjectedResidual :: !(MutableDoubleVector s),
    gmresYValues :: !(MutableDoubleVector s),
    gmresResidualVector :: !(MutableDoubleVector s),
    gmresImageVector :: !(MutableDoubleVector s),
    gmresWorkVector :: !(MutableDoubleVector s),
    gmresPreconditionerScratchA :: !(MutableDoubleVector s),
    gmresPreconditionerScratchB :: !(MutableDoubleVector s),
    gmresDimension :: !Int,
    gmresRestartDimension :: !Int
  }

newGMRESWorkspace :: Int -> Int -> ST s (GMRESWorkspace s)
newGMRESWorkspace !dimension !restartDimension = do
  basisPayload <- newMutableDoubleVector ((restartDimension + 1) * dimension)
  preconditionedPayload <- newMutableDoubleVector (restartDimension * dimension)
  hessenbergPayload <- newMutableDoubleVector ((restartDimension + 1) * restartDimension)
  cosines <- newMutableDoubleVector restartDimension
  sines <- newMutableDoubleVector restartDimension
  projectedResidual <- newMutableDoubleVector (restartDimension + 1)
  yValues <- newMutableDoubleVector restartDimension
  residualVector <- newMutableDoubleVector dimension
  imageVector <- newMutableDoubleVector dimension
  workVector <- newMutableDoubleVector dimension
  preconditionerScratchA <- newMutableDoubleVector dimension
  preconditionerScratchB <- newMutableDoubleVector dimension
  pure
    GMRESWorkspace
      { gmresBasisPayload = basisPayload,
        gmresPreconditionedPayload = preconditionedPayload,
        gmresHessenbergPayload = hessenbergPayload,
        gmresCosines = cosines,
        gmresSines = sines,
        gmresProjectedResidual = projectedResidual,
        gmresYValues = yValues,
        gmresResidualVector = residualVector,
        gmresImageVector = imageVector,
        gmresWorkVector = workVector,
        gmresPreconditionerScratchA = preconditionerScratchA,
        gmresPreconditionerScratchB = preconditionerScratchB,
        gmresDimension = dimension,
        gmresRestartDimension = restartDimension
      }

type GmresState :: Type
data GmresState
  = GmresRunning !Int
  | GmresConverged !Int !Double
  | GmresFailed !SparseIterativeFailure

gmresRestartCycle ::
  SparseGMRESConfig ->
  SparsePreconditioner ->
  SparseCSR Double ->
  U.Vector Double ->
  MutableDoubleVector s ->
  GMRESWorkspace s ->
  GmresState ->
  Int ->
  ST s GmresState
gmresRestartCycle config preconditioner sparseMatrix rhsValues currentGuess workspace stateValue _ =
  case stateValue of
    GmresConverged _ _ -> pure stateValue
    GmresFailed _ -> pure stateValue
    GmresRunning totalIterations ->
      if totalIterations >= sgcIterationLimit config
        then pure stateValue
        else do
          residualIntoMutable sparseMatrix rhsValues currentGuess (gmresImageVector workspace) (gmresResidualVector workspace)
          betaValue <- normMutableVector (gmresResidualVector workspace)
          if betaValue <= sgcTolerance config
            then pure (GmresConverged totalIterations betaValue)
            else do
              prepareRestartBasis betaValue workspace
              arnoldiState <-
                U.foldM'
                  (gmresArnoldiStep config preconditioner sparseMatrix workspace)
                  (ArnoldiRunning 0 betaValue)
                  (U.enumFromN 0 (min (gmresRestartDimension workspace) (sgcIterationLimit config - totalIterations)))
              applyArnoldiCorrection currentGuess workspace arnoldiState
              trueResidualNorm <- gmresTrueResidualNorm sparseMatrix rhsValues currentGuess workspace
              pure (gmresStateAfterArnoldi config totalIterations arnoldiState trueResidualNorm)

type ArnoldiState :: Type
data ArnoldiState
  = ArnoldiRunning !Int !Double
  | ArnoldiConverged !Int !Double
  | ArnoldiHappyBreakdown !Int !Double

gmresArnoldiStep ::
  SparseGMRESConfig ->
  SparsePreconditioner ->
  SparseCSR Double ->
  GMRESWorkspace s ->
  ArnoldiState ->
  Int ->
  ST s ArnoldiState
gmresArnoldiStep config preconditioner sparseMatrix workspace stateValue _ =
  case stateValue of
    ArnoldiConverged _ _ -> pure stateValue
    ArnoldiHappyBreakdown _ _ -> pure stateValue
    ArnoldiRunning completedSteps residualNormValue ->
      if residualNormValue <= sgcTolerance config
        then pure (ArnoldiConverged completedSteps residualNormValue)
        else do
          let basisVector = basisColumn workspace completedSteps
              preconditionedVector = preconditionedColumn workspace completedSteps
          applySparsePreconditionerMutable
            preconditioner
            basisVector
            (gmresPreconditionerScratchA workspace)
            (gmresPreconditionerScratchB workspace)
            preconditionedVector
          csrMatVecIntoMutable sparseMatrix preconditionedVector (gmresWorkVector workspace)
          orthogonalizeAgainstBasis workspace completedSteps
          nextBasisNorm <- normMutableVector (gmresWorkVector workspace)
          writeHessenbergEntry workspace (completedSteps + 1) completedSteps nextBasisNorm
          writeNextBasisColumn workspace completedSteps nextBasisNorm
          applyPreviousRotations workspace completedSteps
          residualAfterRotation <- applyNextRotation workspace completedSteps
          let !nextCompletedSteps = completedSteps + 1
          if nextBasisNorm <= solverEpsilon
            then pure (ArnoldiHappyBreakdown nextCompletedSteps residualAfterRotation)
            else
              if residualAfterRotation <= sgcTolerance config
                then pure (ArnoldiConverged nextCompletedSteps residualAfterRotation)
                else pure (ArnoldiRunning nextCompletedSteps residualAfterRotation)

prepareRestartBasis :: Double -> GMRESWorkspace s -> ST s ()
prepareRestartBasis !betaValue workspace = do
  MU.set (gmresProjectedResidual workspace) 0.0
  MU.unsafeWrite (gmresProjectedResidual workspace) 0 betaValue
  copyMutableVector (gmresResidualVector workspace) (basisColumn workspace 0)
  scaleMutableVector (1.0 / betaValue) (basisColumn workspace 0)

orthogonalizeAgainstBasis :: GMRESWorkspace s -> Int -> ST s ()
orthogonalizeAgainstBasis workspace stepIndex =
  U.foldM' orthogonalizeColumn () (U.enumFromN 0 (stepIndex + 1))
  where
    orthogonalizeColumn () basisIndex = do
      coefficientValue <- dotMutableVector (gmresWorkVector workspace) (basisColumn workspace basisIndex)
      writeHessenbergEntry workspace basisIndex stepIndex coefficientValue
      addScaledMutableVector (negate coefficientValue) (basisColumn workspace basisIndex) (gmresWorkVector workspace)

writeNextBasisColumn :: GMRESWorkspace s -> Int -> Double -> ST s ()
writeNextBasisColumn workspace stepIndex nextBasisNorm =
  if nextBasisNorm <= solverEpsilon
    then pure ()
    else do
      copyMutableVector (gmresWorkVector workspace) (basisColumn workspace (stepIndex + 1))
      scaleMutableVector (1.0 / nextBasisNorm) (basisColumn workspace (stepIndex + 1))

applyPreviousRotations :: GMRESWorkspace s -> Int -> ST s ()
applyPreviousRotations workspace stepIndex =
  U.foldM' applyRotation () (U.enumFromN 0 stepIndex)
  where
    applyRotation () rotationIndex = do
      cosValue <- MU.unsafeRead (gmresCosines workspace) rotationIndex
      sinValue <- MU.unsafeRead (gmresSines workspace) rotationIndex
      firstEntry <- readHessenbergEntry workspace rotationIndex stepIndex
      secondEntry <- readHessenbergEntry workspace (rotationIndex + 1) stepIndex
      let !firstRotated = cosValue * firstEntry + sinValue * secondEntry
          !secondRotated = negate sinValue * firstEntry + cosValue * secondEntry
      writeHessenbergEntry workspace rotationIndex stepIndex firstRotated
      writeHessenbergEntry workspace (rotationIndex + 1) stepIndex secondRotated

applyNextRotation :: GMRESWorkspace s -> Int -> ST s Double
applyNextRotation workspace stepIndex = do
  diagonalEntry <- readHessenbergEntry workspace stepIndex stepIndex
  subdiagonalEntry <- readHessenbergEntry workspace (stepIndex + 1) stepIndex
  let GivensRotation cosValue sinValue rValue = gmresGivensCoefficients diagonalEntry subdiagonalEntry
  MU.unsafeWrite (gmresCosines workspace) stepIndex cosValue
  MU.unsafeWrite (gmresSines workspace) stepIndex sinValue
  writeHessenbergEntry workspace stepIndex stepIndex rValue
  writeHessenbergEntry workspace (stepIndex + 1) stepIndex 0.0
  projectedEntry <- MU.unsafeRead (gmresProjectedResidual workspace) stepIndex
  projectedNext <- MU.unsafeRead (gmresProjectedResidual workspace) (stepIndex + 1)
  let !rotatedEntry = cosValue * projectedEntry + sinValue * projectedNext
      !rotatedNext = negate sinValue * projectedEntry + cosValue * projectedNext
  MU.unsafeWrite (gmresProjectedResidual workspace) stepIndex rotatedEntry
  MU.unsafeWrite (gmresProjectedResidual workspace) (stepIndex + 1) rotatedNext
  pure (abs rotatedNext)

applyArnoldiCorrection :: MutableDoubleVector s -> GMRESWorkspace s -> ArnoldiState -> ST s ()
applyArnoldiCorrection currentGuess workspace arnoldiState =
  case arnoldiStepCount arnoldiState of
    0 -> pure ()
    stepCount -> do
      solveProjectedUpperTriangular workspace stepCount
      MU.set (gmresWorkVector workspace) 0.0
      U.foldM' addColumnContribution () (U.enumFromN 0 stepCount)
      addScaledMutableVector 1.0 (gmresWorkVector workspace) currentGuess
  where
    addColumnContribution () columnIndex = do
      coefficientValue <- MU.unsafeRead (gmresYValues workspace) columnIndex
      addScaledMutableVector coefficientValue (preconditionedColumn workspace columnIndex) (gmresWorkVector workspace)

gmresTrueResidualNorm ::
  SparseCSR Double ->
  U.Vector Double ->
  MutableDoubleVector s ->
  GMRESWorkspace s ->
  ST s Double
gmresTrueResidualNorm sparseMatrix rhsValues currentGuess workspace = do
  residualIntoMutable sparseMatrix rhsValues currentGuess (gmresImageVector workspace) (gmresResidualVector workspace)
  normMutableVector (gmresResidualVector workspace)

solveProjectedUpperTriangular :: GMRESWorkspace s -> Int -> ST s ()
solveProjectedUpperTriangular workspace stepCount =
  U.foldM' solveRow () (U.enumFromN 0 stepCount)
  where
    solveRow () reverseOffset = do
      let rowIndex = stepCount - reverseOffset - 1
      laterProduct <- projectedLaterProduct workspace stepCount rowIndex
      rhsValue <- MU.unsafeRead (gmresProjectedResidual workspace) rowIndex
      diagonalEntry <- readHessenbergEntry workspace rowIndex rowIndex
      MU.unsafeWrite (gmresYValues workspace) rowIndex ((rhsValue - laterProduct) / diagonalEntry)

projectedLaterProduct :: GMRESWorkspace s -> Int -> Int -> ST s Double
projectedLaterProduct workspace stepCount rowIndex =
  U.foldM' accumulateLater 0.0 (U.enumFromN (rowIndex + 1) (stepCount - rowIndex - 1))
  where
    accumulateLater !accumulator columnIndex = do
      hEntry <- readHessenbergEntry workspace rowIndex columnIndex
      yValue <- MU.unsafeRead (gmresYValues workspace) columnIndex
      pure (accumulator + hEntry * yValue)

gmresStateAfterArnoldi :: SparseGMRESConfig -> Int -> ArnoldiState -> Double -> GmresState
gmresStateAfterArnoldi config totalIterations arnoldiState trueResidualNorm =
  let !nextTotal = totalIterations + arnoldiStepCount arnoldiState
   in if not (finiteDouble trueResidualNorm)
        then GmresFailed (SparseInvalidInput "GMRES restart produced a non-finite true residual")
        else
          case arnoldiState of
            ArnoldiHappyBreakdown _ projectedResidualNorm
              | trueResidualNorm > sgcTolerance config ->
                  GmresFailed
                    ( SparseInvalidInput
                        ( "GMRES happy breakdown did not certify the true residual; projected residual "
                            <> show projectedResidualNorm
                            <> ", true residual "
                            <> show trueResidualNorm
                        )
                    )
            _ ->
              if trueResidualNorm <= sgcTolerance config
                then GmresConverged nextTotal trueResidualNorm
                else GmresRunning nextTotal

gmresResultFromState :: Int -> MutableDoubleVector s -> GmresState -> ST s (Either SparseIterativeFailure SparseIterativeResult)
gmresResultFromState iterationLimit currentGuess stateValue =
  case stateValue of
    GmresRunning _ -> pure (Left (SparseIterationBudgetExceeded iterationLimit))
    GmresFailed failureValue -> pure (Left failureValue)
    GmresConverged iterationCount residualNormValue -> do
      solutionVector <- freezeMutableDoubleVector currentGuess
      pure
        ( Right
            SparseIterativeResult
              { sparseSolution = solutionVector,
                sparseIterations = iterationCount,
                sparseResidualNorm = residualNormValue
              }
        )

arnoldiStepCount :: ArnoldiState -> Int
arnoldiStepCount stateValue =
  case stateValue of
    ArnoldiRunning stepCount _ -> stepCount
    ArnoldiConverged stepCount _ -> stepCount
    ArnoldiHappyBreakdown stepCount _ -> stepCount

type GivensRotation :: Type
data GivensRotation = GivensRotation !Double !Double !Double

gmresGivensCoefficients :: Double -> Double -> GivensRotation
gmresGivensCoefficients diagonalEntry subdiagonalEntry
  | abs subdiagonalEntry <= solverEpsilon = GivensRotation 1.0 0.0 diagonalEntry
  | abs diagonalEntry <= solverEpsilon = GivensRotation 0.0 (signum subdiagonalEntry) (abs subdiagonalEntry)
  | otherwise =
      let radius = sqrt (diagonalEntry * diagonalEntry + subdiagonalEntry * subdiagonalEntry)
       in GivensRotation (diagonalEntry / radius) (subdiagonalEntry / radius) radius

basisColumn :: GMRESWorkspace s -> Int -> MutableDoubleVector s
basisColumn workspace columnIndex =
  MU.unsafeSlice (columnIndex * gmresDimension workspace) (gmresDimension workspace) (gmresBasisPayload workspace)
{-# INLINE basisColumn #-}

preconditionedColumn :: GMRESWorkspace s -> Int -> MutableDoubleVector s
preconditionedColumn workspace columnIndex =
  MU.unsafeSlice (columnIndex * gmresDimension workspace) (gmresDimension workspace) (gmresPreconditionedPayload workspace)
{-# INLINE preconditionedColumn #-}

readHessenbergEntry :: GMRESWorkspace s -> Int -> Int -> ST s Double
readHessenbergEntry workspace rowIndex columnIndex =
  MU.unsafeRead (gmresHessenbergPayload workspace) (hessenbergOffset workspace rowIndex columnIndex)
{-# INLINE readHessenbergEntry #-}

writeHessenbergEntry :: GMRESWorkspace s -> Int -> Int -> Double -> ST s ()
writeHessenbergEntry workspace rowIndex columnIndex value =
  MU.unsafeWrite (gmresHessenbergPayload workspace) (hessenbergOffset workspace rowIndex columnIndex) value
{-# INLINE writeHessenbergEntry #-}

hessenbergOffset :: GMRESWorkspace s -> Int -> Int -> Int
hessenbergOffset workspace rowIndex columnIndex =
  rowIndex + columnIndex * (gmresRestartDimension workspace + 1)
{-# INLINE hessenbergOffset #-}

restartCycleCount :: Int -> Int -> Int
restartCycleCount iterationLimit restartDimension =
  if iterationLimit <= 0
    then 0
    else (iterationLimit + restartDimension - 1) `div` restartDimension

validateGmresPreconditioner :: SparseCSR Double -> SparsePreconditioner -> Either SparseIterativeFailure ()
validateGmresPreconditioner sparseMatrix preconditioner =
  if preconditionerDimension preconditioner == csrRows sparseMatrix
    then Right ()
    else Left (SparseInvalidInput "GMRES requires preconditioner dimension equal to matrix dimension")

finiteDouble :: Double -> Bool
finiteDouble value =
  not (isNaN value || isInfinite value)
