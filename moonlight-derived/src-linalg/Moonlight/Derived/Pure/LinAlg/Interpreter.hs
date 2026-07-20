module Moonlight.Derived.Pure.LinAlg.Interpreter
  ( fieldRankBackend
  , gf2PackedRankBackend
  , smithDense
  , rankDense
  , kernelDense
  , leftKernelDense
  , inverseDense
  , solveDense
  , leftSolveDense
  ) where

import qualified Data.Vector as V
import Moonlight.Algebra (EuclideanDomain, IntegralDomain (..))
import Moonlight.Core
  ( AdditiveMonoid (..)
  , Field (..)
  , MoonlightError (..)
  , requireInvertible
  )
import Moonlight.Derived.Pure.LinAlg.Rank
  ( RankBackend
  , rankBackendWithSparse
  , rankDenseWith
  , rankSparseDefault
  , rankSparseGF2Packed
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix (DenseMat (..), matMul, transposeMat, zeroMat)
import Moonlight.LinAlg.Dense (mkDynMatrix, toListMatrix, toListVector, withDynMatrix)
import Moonlight.LinAlg.Dense.Field (DenseRankBackend, KernelBasis (..), kernel, rank)
import Moonlight.LinAlg.Dense.GF2 (GF2, mkGF2PackedMatrixFromRowMajor, rankGF2PackedMatrix)
import Moonlight.LinAlg.Domain (SmithNormalForm (..), smithNormalForm)

fieldRankBackend ::
  (Eq a, Field a, Num a, DenseRankBackend a) =>
  RankBackend a
fieldRankBackend =
  rankBackendWithSparse rankDenseDefault rankSparseDefault

gf2PackedRankBackend :: RankBackend GF2
gf2PackedRankBackend =
  rankBackendWithSparse
    ( \denseMat -> do
        packedMatrix <-
          liftBackendResult
            "gf2PackedRankBackend"
            (mkGF2PackedMatrixFromRowMajor (fromIntegral (dmRows denseMat)) (fromIntegral (dmCols denseMat)) (denseToFlat denseMat))
        Right (rankGF2PackedMatrix packedMatrix)
    )
    rankSparseGF2Packed

smithDense ::
  (Field a, EuclideanDomain a) =>
  DenseMat a ->
  Either MoonlightError (DenseMat a, DenseMat a, DenseMat a, Int)
smithDense denseMat = do
  validatedMat <- ensureDenseFieldValues "smithDense" denseMat
  let rowCount = dmRows validatedMat
      columnCount = dmCols validatedMat
  (leftEntries, diagonalEntries, rightEntries) <-
    liftBackendResult
      "smithDense"
      ( mkDynMatrix rowCount columnCount (denseToFlat validatedMat)
          >>= ( \dynamicMatrix ->
                  withDynMatrix dynamicMatrix
                    ( \typedMatrix ->
                        smithNormalForm typedMatrix
                          >>= \smithValue ->
                            Right
                              ( toListMatrix (smithLeft smithValue)
                              , toListMatrix (smithDiagonal smithValue)
                              , toListMatrix (smithRight smithValue)
                              )
                    )
                  >>= id
              )
      )
  leftTransform <- flatToDenseExact "smithDense" rowCount rowCount leftEntries
  diagonalMatrix <- flatToDenseExact "smithDense" rowCount columnCount diagonalEntries
  rightTransform <- flatToDenseExact "smithDense" columnCount columnCount rightEntries
  let diagonalValues =
        fmap
          (\diagonalIndex -> matrixEntry diagonalMatrix diagonalIndex diagonalIndex)
          [0 .. min rowCount columnCount - 1]
      diagonalRank = length (filter (not . isZero) diagonalValues)
  pure (leftTransform, diagonalMatrix, rightTransform, diagonalRank)
  where
    matrixEntry :: DenseMat a -> Int -> Int -> a
    matrixEntry DenseMat{dmData} rowIndex columnIndex =
      (dmData V.! rowIndex) V.! columnIndex

rankDense ::
  (Eq a, Field a, Num a, DenseRankBackend a) =>
  DenseMat a ->
  Either MoonlightError Int
rankDense =
  rankDenseWith fieldRankBackend

kernelDense ::
  (Eq a, Field a) =>
  DenseMat a ->
  Either MoonlightError [V.Vector a]
kernelDense denseMat = do
  validatedMat <- ensureDenseFieldValues "kernelDense" denseMat
  liftBackendResult
    "kernelDense"
    ( mkDynMatrix (dmRows validatedMat) (dmCols validatedMat) (denseToFlat validatedMat)
        >>= ( \dynamicMatrix ->
                withDynMatrix dynamicMatrix
                  ( \typedMatrix ->
                      kernel typedMatrix
                        >>= \kernelBasis ->
                          Right (fmap (V.fromList . toListVector) (kernelBasisVectors kernelBasis))
                  )
                >>= id
            )
    )

leftKernelDense ::
  (Eq a, Field a) =>
  DenseMat a ->
  Either MoonlightError [V.Vector a]
leftKernelDense denseMat =
  kernelDense (transposeMat denseMat)

inverseDense ::
  (Field a, EuclideanDomain a, Num a) =>
  DenseMat a ->
  Either MoonlightError (Maybe (DenseMat a))
inverseDense denseMat = do
  validatedMat <- ensureDenseFieldValues "inverseDense" denseMat
  if dmRows validatedMat /= dmCols validatedMat
    then pure Nothing
    else do
      let size = dmRows validatedMat
      (leftTransform, diagonalMatrix, rightTransform, rankValue) <- smithDense validatedMat
      if rankValue == size
        then
          do
            diagonalInverseMatrix <- diagonalInverse size diagonalMatrix
            pure
              ( Just
                  ( matMul rightTransform
                      (matMul diagonalInverseMatrix leftTransform)
                  )
              )
        else pure Nothing
  where
    diagonalInverse :: (Field a, IntegralDomain a) => Int -> DenseMat a -> Either MoonlightError (DenseMat a)
    diagonalInverse size diagonalMat =
      DenseMat size size
        <$> V.generateM
          size
          ( \rowIndex ->
              V.generateM
                size
                ( \columnIndex ->
                    if rowIndex == columnIndex
                      then invertDiagonalEntry rowIndex ((dmData diagonalMat V.! rowIndex) V.! rowIndex)
                      else pure zero
                )
          )

    invertDiagonalEntry :: (Field a, IntegralDomain a) => Int -> a -> Either MoonlightError a
    invertDiagonalEntry diagonalIndex diagonalValue
      | isZero diagonalValue =
          Left
            ( InvariantViolation
                ( "inverseDense: smith normal form reported full rank but diagonal entry "
                    <> show diagonalIndex
                    <> " was zero"
                )
            )
      | otherwise =
          requireInvertible
            ( InvariantViolation
                ( "inverseDense: Field.tryInv failed on Smith diagonal entry "
                    <> show diagonalIndex
                )
            )
            diagonalValue

solveDense ::
  (Field a, EuclideanDomain a, Num a) =>
  DenseMat a ->
  DenseMat a ->
  Either MoonlightError (Maybe (DenseMat a))
solveDense coefficientMat rhsMat
  | dmRows coefficientMat /= dmRows rhsMat =
      Left
        ( InvariantViolation
            ( "solveDense: coefficient rows "
                <> show (dmRows coefficientMat)
                <> " do not match right-hand-side rows "
                <> show (dmRows rhsMat)
            )
        )
  | dmCols coefficientMat == 0 =
      pure
        ( if allRowsZero rhsMat
            then Just (zeroMat 0 (dmCols rhsMat))
            else Nothing
        )
  | dmRows coefficientMat == 0 =
      pure (Just (zeroMat (dmCols coefficientMat) (dmCols rhsMat)))
  | otherwise = do
      (leftTransform, diagonalMatrix, rightTransform, rankValue) <- smithDense coefficientMat
      let transformedRhs = matMul leftTransform rhsMat
      if V.all (V.all isZero) (V.drop rankValue (dmData transformedRhs))
        then do
          pivotInverses <-
            V.generateM
              rankValue
              ( \pivotIndex ->
                  requireInvertible
                    ( InvariantViolation
                        ( "solveDense: Field.tryInv failed on Smith diagonal entry "
                            <> show pivotIndex
                        )
                    )
                    ((dmData diagonalMatrix V.! pivotIndex) V.! pivotIndex)
              )
          let unknownCount = dmCols coefficientMat
              solutionWidth = dmCols rhsMat
              particularRow rowIndex
                | rowIndex < rankValue =
                    fmap ((pivotInverses V.! rowIndex) *) (dmData transformedRhs V.! rowIndex)
                | otherwise = V.replicate solutionWidth zero
              particularSolution =
                DenseMat unknownCount solutionWidth (V.generate unknownCount particularRow)
          pure (Just (matMul rightTransform particularSolution))
        else pure Nothing
  where
    allRowsZero = V.all (V.all isZero) . dmData

leftSolveDense ::
  (Field a, EuclideanDomain a, Num a) =>
  DenseMat a ->
  DenseMat a ->
  Either MoonlightError (Maybe (DenseMat a))
leftSolveDense coefficientMat rhsMat =
  (fmap . fmap)
    transposeMat
    (solveDense (transposeMat coefficientMat) (transposeMat rhsMat))

denseToFlat :: DenseMat a -> [a]
denseToFlat denseMat =
  concatMap V.toList (V.toList (dmData denseMat))

flatToDenseExact :: String -> Int -> Int -> [a] -> Either MoonlightError (DenseMat a)
flatToDenseExact context rowCount columnCount values =
  let expectedEntryCount = rowCount * columnCount
      flatValues = V.fromList values
      observedEntryCount = V.length flatValues
   in if observedEntryCount /= expectedEntryCount
        then
          Left
            ( InvariantViolation
                ( context
                    <> ": backend returned "
                    <> show observedEntryCount
                    <> " entries for shape "
                    <> show (rowCount, columnCount)
                )
            )
        else
          Right
            ( DenseMat
                rowCount
                columnCount
                ( V.generate
                    rowCount
                    (\rowIndex -> V.slice (rowIndex * columnCount) columnCount flatValues)
                )
            )

backendFailure :: Show err => String -> err -> MoonlightError
backendFailure context err =
  InvariantViolation (context <> ": " <> show err)

liftBackendResult :: Show err => String -> Either err a -> Either MoonlightError a
liftBackendResult context =
  either (Left . backendFailure context) Right

ensureDenseFieldValues :: Field a => String -> DenseMat a -> Either MoonlightError (DenseMat a)
ensureDenseFieldValues context denseMat =
  if all fieldValueValid (denseToFlat denseMat)
    then Right denseMat
    else Left (InvariantViolation (context <> ": matrix contains invalid field values"))

rankDenseDefault ::
  (Eq a, Field a, DenseRankBackend a) =>
  DenseMat a ->
  Either MoonlightError Int
rankDenseDefault denseMat =
  do
    validatedMat <- ensureDenseFieldValues "rankDense" denseMat
    liftBackendResult
      "rankDense"
      ( mkDynMatrix (dmRows validatedMat) (dmCols validatedMat) (denseToFlat validatedMat)
          >>= (\dynamicMatrix -> withDynMatrix dynamicMatrix rank >>= id)
      )
