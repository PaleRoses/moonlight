{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Moonlight.LinAlg.Pure.Sparse.Solver.Stationary
  ( solveSparseJacobi,
    solveSparseRichardson,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Kind (Type)
import Data.Vector.Unboxed qualified as U
import Moonlight.LinAlg.Pure.Sparse.Solver.Common
  ( solverEpsilon,
    sparseDiagonal,
    validateSparseSystemInput,
  )
import Moonlight.LinAlg.Pure.Sparse.Solver.Mutable
  ( MutableDoubleVector,
    addScaledMutableVector,
    freezeMutableDoubleVector,
    newMutableDoubleVector,
    normMutableVector,
    residualIntoMutable,
    scaledCopyMutableVector,
    thawMutableDoubleVector,
  )
import Moonlight.LinAlg.Pure.Sparse.Solver.Preconditioner
  ( SparsePreconditioner,
    applySparsePreconditionerMutable,
    compileSparsePreconditioner,
  )
import Moonlight.LinAlg.Pure.Sparse.Solver.Types
  ( SparseIterativeFailure (..),
    SparseIterativeResult (..),
    SparsePreconditionerFamily (..),
    SparseStationaryIterationConfig (..),
  )
import Moonlight.LinAlg.Pure.Sparse.Types (SparseCSR, csrRows)
import Prelude

solveSparseJacobi ::
  SparseStationaryIterationConfig ->
  SparseCSR Double ->
  U.Vector Double ->
  U.Vector Double ->
  Either SparseIterativeFailure SparseIterativeResult
solveSparseJacobi config sparseMatrix rhsValues initialGuess = do
  preconditioner <- compileSparsePreconditioner DiagonalJacobiSparsePreconditionerFamily sparseMatrix
  solveSparseStationary config sparseMatrix rhsValues initialGuess (JacobiStationaryStep preconditioner)

solveSparseRichardson ::
  SparseStationaryIterationConfig ->
  SparseCSR Double ->
  U.Vector Double ->
  U.Vector Double ->
  Either SparseIterativeFailure SparseIterativeResult
solveSparseRichardson config sparseMatrix rhsValues initialGuess = do
  stepSize <- stableRichardsonStep sparseMatrix
  solveSparseStationary config sparseMatrix rhsValues initialGuess (RichardsonStationaryStep (ssicDamping config * stepSize))

solveSparseStationary ::
  SparseStationaryIterationConfig ->
  SparseCSR Double ->
  U.Vector Double ->
  U.Vector Double ->
  StationaryStep ->
  Either SparseIterativeFailure SparseIterativeResult
solveSparseStationary SparseStationaryIterationConfig {..} sparseMatrix rhsValues initialGuess stepKind = do
  validateSparseSystemInput sparseMatrix rhsValues initialGuess
  if ssicIterationLimit < 0
    then Left (SparseInvalidInput "stationary sparse solver iteration limit must be non-negative")
    else runST (solveSparseStationaryMutable ssicTolerance ssicIterationLimit ssicDamping sparseMatrix rhsValues initialGuess stepKind)

type StationaryStep :: Type
data StationaryStep
  = JacobiStationaryStep !SparsePreconditioner
  | RichardsonStationaryStep !Double

solveSparseStationaryMutable ::
  Double ->
  Int ->
  Double ->
  SparseCSR Double ->
  U.Vector Double ->
  U.Vector Double ->
  StationaryStep ->
  ST s (Either SparseIterativeFailure SparseIterativeResult)
solveSparseStationaryMutable !toleranceValue !iterationLimit !dampingValue sparseMatrix rhsValues initialGuess stepKind = do
  let !dimension = csrRows sparseMatrix
  currentVector <- thawMutableDoubleVector initialGuess
  residualVector <- newMutableDoubleVector dimension
  stepVector <- newMutableDoubleVector dimension
  imageVector <- newMutableDoubleVector dimension
  preconditionerScratchA <- newMutableDoubleVector dimension
  preconditionerScratchB <- newMutableDoubleVector dimension
  residualIntoMutable sparseMatrix rhsValues currentVector imageVector residualVector
  initialResidualNorm <- normMutableVector residualVector
  if initialResidualNorm <= toleranceValue
    then Right <$> freezeStationaryResult 0 initialResidualNorm currentVector
    else do
      finalState <-
        U.foldM'
          (stationaryIteration toleranceValue dampingValue sparseMatrix rhsValues currentVector residualVector stepVector imageVector preconditionerScratchA preconditionerScratchB stepKind)
          StationaryRunning
          (U.enumFromN 0 iterationLimit)
      stationaryResultFromState iterationLimit currentVector finalState

type StationaryState :: Type
data StationaryState
  = StationaryRunning
  | StationaryConverged !Int !Double

stationaryIteration ::
  Double ->
  Double ->
  SparseCSR Double ->
  U.Vector Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  StationaryStep ->
  StationaryState ->
  Int ->
  ST s StationaryState
stationaryIteration !toleranceValue !dampingValue sparseMatrix rhsValues currentVector residualVector stepVector imageVector preconditionerScratchA preconditionerScratchB stepKind stepState iterationIndex =
  case stepState of
    StationaryConverged _ _ -> pure stepState
    StationaryRunning -> do
      writeStationaryStep dampingValue stepKind residualVector preconditionerScratchA preconditionerScratchB stepVector
      addScaledMutableVector 1.0 stepVector currentVector
      residualIntoMutable sparseMatrix rhsValues currentVector imageVector residualVector
      nextResidualNorm <- normMutableVector residualVector
      if nextResidualNorm <= toleranceValue
        then pure (StationaryConverged (iterationIndex + 1) nextResidualNorm)
        else pure StationaryRunning

writeStationaryStep ::
  Double ->
  StationaryStep ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s ()
writeStationaryStep !dampingValue stepKind residualVector scratchA scratchB stepVector =
  case stepKind of
    JacobiStationaryStep preconditioner -> do
      applySparsePreconditionerMutable preconditioner residualVector scratchA scratchB stepVector
      scaledCopyMutableVector dampingValue stepVector stepVector
    RichardsonStationaryStep richardsonScale ->
      scaledCopyMutableVector richardsonScale residualVector stepVector

stationaryResultFromState :: Int -> MutableDoubleVector s -> StationaryState -> ST s (Either SparseIterativeFailure SparseIterativeResult)
stationaryResultFromState iterationLimit currentVector stepState =
  case stepState of
    StationaryRunning -> pure (Left (SparseIterationBudgetExceeded iterationLimit))
    StationaryConverged iterationCount residualNormValue -> Right <$> freezeStationaryResult iterationCount residualNormValue currentVector

freezeStationaryResult :: Int -> Double -> MutableDoubleVector s -> ST s SparseIterativeResult
freezeStationaryResult iterationCount residualNormValue currentVector = do
  solutionVector <- freezeMutableDoubleVector currentVector
  pure
    SparseIterativeResult
      { sparseSolution = solutionVector,
        sparseIterations = iterationCount,
        sparseResidualNorm = residualNormValue
      }

stableRichardsonStep :: SparseCSR Double -> Either SparseIterativeFailure Double
stableRichardsonStep sparseMatrix = do
  diagonalValues <- sparseDiagonal sparseMatrix
  let spectralBound =
        U.maximum
          (U.cons 0.0 (U.map abs (U.filter (\value -> abs value > solverEpsilon) diagonalValues)))
  if spectralBound <= 0.0
    then Left (SparseInvalidInput "Richardson iteration requires a non-zero diagonal spectral bound")
    else Right (0.5 / spectralBound)
