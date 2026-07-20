{-# LANGUAGE RecordWildCards #-}

module Moonlight.Analysis.Dynamics.Biomechanics.SheafRefinement.Operator
  ( SparseNormalEquation (..),
    SparseNormalSystem (..),
    assembleSparseNormalSystem,
    solveSparseNormalSystemCG,
    solveSparseNormalSystemPCG,
    solveSparseNormalSystemPCGWithFamily,
    objectiveBreakdownOf,
    sparseDot,
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector.Unboxed qualified as U
import Moonlight.Core (MoonlightError)
import Moonlight.LinAlg
  ( SparseConjugateGradientConfig (..),
    SparsePreconditionerFamily (..),
    defaultSparsePreconditionerFamily,
    sparseSolution,
    solveSparseCG,
  )
import Moonlight.LinAlg.Sparse
  ( mkSparseCOO,
    SparseCSR,
    cooToCSR,
  )

type SparseNormalEquation :: Type -> Type
data SparseNormalEquation component = SparseNormalEquation
  { sneWeight :: Double,
    sneComponent :: component,
    sneTerms :: [(Int, Double)],
    sneRhs :: Double
  }

type SparseNormalSystem :: Type
data SparseNormalSystem = SparseNormalSystem
  { snsMatrix :: SparseCSR Double,
    snsRhs :: [Double]
  }

assembleSparseNormalSystem :: Double -> Int -> [SparseNormalEquation component] -> Either MoonlightError SparseNormalSystem
assembleSparseNormalSystem regularizationWeight dimension equations =
  (\normalMatrix ->
      SparseNormalSystem
        { snsMatrix = normalMatrix,
          snsRhs =
            fmap
              (\entryIndex -> Map.findWithDefault 0.0 entryIndex rhsEntries)
              [0 :: Int .. dimension - 1]
        }
  )
    <$> ( mkSparseCOO
            dimension
            dimension
            ( regularizationEntries regularizationWeight dimension
                <> foldMap equationMatrixEntries equations
            )
            >>= cooToCSR
        )
  where
    rhsEntries =
      foldr
        (\equationValue accumulatedEntries ->
            Map.unionWith (+) (equationRhsEntries equationValue) accumulatedEntries
        )
        Map.empty
        equations

solveSparseNormalSystemCG :: Int -> Double -> SparseNormalSystem -> [Double] -> Maybe [Double]
solveSparseNormalSystemCG iterationLimit toleranceValue SparseNormalSystem {..} initialGuess =
  either
    (const Nothing)
    (Just . U.toList . sparseSolution)
    (solveSparseCG (sparseConfig IdentitySparsePreconditionerFamily) snsMatrix (U.fromList snsRhs) (U.fromList initialGuess))
  where
    sparseConfig preconditionerFamily =
      SparseConjugateGradientConfig
        { scgcTolerance = toleranceValue,
          scgcIterationLimit = iterationLimit,
          scgcPreconditionerFamily = preconditionerFamily
        }

solveSparseNormalSystemPCG :: Int -> Double -> SparseNormalSystem -> [Double] -> Maybe [Double]
solveSparseNormalSystemPCG iterationLimit toleranceValue normalSystem initialGuess =
  solveSparseNormalSystemPCGWithFamily defaultSparsePreconditionerFamily iterationLimit toleranceValue normalSystem initialGuess

solveSparseNormalSystemPCGWithFamily :: SparsePreconditionerFamily -> Int -> Double -> SparseNormalSystem -> [Double] -> Maybe [Double]
solveSparseNormalSystemPCGWithFamily preconditionerFamily iterationLimit toleranceValue SparseNormalSystem {..} initialGuess =
  let sparseConfig =
        SparseConjugateGradientConfig
          { scgcTolerance = toleranceValue,
            scgcIterationLimit = iterationLimit,
            scgcPreconditionerFamily = preconditionerFamily
          }
   in either
        (const Nothing)
        (Just . U.toList . sparseSolution)
        (solveSparseCG sparseConfig snsMatrix (U.fromList snsRhs) (U.fromList initialGuess))

objectiveBreakdownOf :: Ord component => [SparseNormalEquation component] -> [Double] -> Map component Double
objectiveBreakdownOf equations solutionVector =
  foldr
    (\equationValue accumulatedBreakdown ->
        Map.insertWith (+) (sneComponent equationValue) (equationEnergy equationValue) accumulatedBreakdown
    )
    Map.empty
    equations
  where
    equationEnergy SparseNormalEquation {..} =
      let residualValue = sparseDot sneTerms solutionVector - sneRhs
       in sneWeight * residualValue * residualValue

sparseDot :: [(Int, Double)] -> [Double] -> Double
sparseDot sparseTerms vectorValues =
  sum
    ( fmap
        (\(entryIndex, coefficientValue) -> coefficientValue * valueAtIndex entryIndex vectorValues)
        sparseTerms
    )

regularizationEntries :: Double -> Int -> [(Int, Int, Double)]
regularizationEntries diagonalValue dimension =
  fmap (\entryIndex -> (entryIndex, entryIndex, diagonalValue)) [0 :: Int .. dimension - 1]

equationMatrixEntries :: SparseNormalEquation component -> [(Int, Int, Double)]
equationMatrixEntries SparseNormalEquation {..} =
  sneTerms
    >>= \(rowIndex, rowValue) ->
      fmap
        (\(columnIndex, columnValue) -> (rowIndex, columnIndex, sneWeight * rowValue * columnValue))
        sneTerms

equationRhsEntries :: SparseNormalEquation component -> Map Int Double
equationRhsEntries SparseNormalEquation {..} =
  Map.fromListWith (+)
    (fmap (\(entryIndex, coefficientValue) -> (entryIndex, sneWeight * sneRhs * coefficientValue)) sneTerms)

valueAtIndex :: Int -> [Double] -> Double
valueAtIndex indexValue vectorValues =
  case drop indexValue vectorValues of
    value : _ ->
      value
    [] ->
      0.0
