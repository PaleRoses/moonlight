{-# LANGUAGE NamedFieldPuns #-}

module Moonlight.Derived.Pure.Pruning.LaplacianGate
  ( localSheafLaplacian
  , pruningGapOfSymmetricDenseMat
  , pruningGapAt
  , laplacianGate
  ) where

import Data.Vector qualified as V
import Moonlight.Core (MoonlightError)
import Moonlight.Derived.Pure.Site.InjectiveComplex
  ( Derived (..)
  , InjectiveComplex (..)
  )
import Moonlight.Derived.Pure.Site.LabeledMatrix
  ( BlockedMat
  , DenseMat (..)
  , GroupedAxis
  , axisSize
  , bmCols
  , bmRows
  , emptyAxis
  , matAdd
  , matMul
  , restrictAxis
  , starView
  , transposeBlockedMat
  , transposeMat
  , zeroMat
  )
import Moonlight.Derived.Pure.Site.Poset
  ( DerivedPoset
  , FinObjectId
  , star
  )
import Moonlight.Homology (HomologicalDegree (..))
import Moonlight.LinAlg (GF2, gf2ToBool)
import Moonlight.LinAlg.Dense.Decomposition (symmetricEigenPairs)

diffAtDegree :: Int -> InjectiveComplex GF2 -> Maybe (BlockedMat GF2)
diffAtDegree degreeValue InjectiveComplex {icStart, icDiffs}
  | degreeIndex < 0 = Nothing
  | degreeIndex >= V.length icDiffs = Nothing
  | otherwise = Just (icDiffs V.! degreeIndex)
  where
    degreeIndex = degreeValue - icStart

incomingDiffAtDegree :: Int -> InjectiveComplex GF2 -> Maybe (BlockedMat GF2)
incomingDiffAtDegree degreeValue =
  diffAtDegree (degreeValue - 1)

axisAtDegree :: Int -> InjectiveComplex GF2 -> GroupedAxis
axisAtDegree degreeValue injectiveComplex =
  case diffAtDegree degreeValue injectiveComplex of
    Just outgoingDiff -> bmCols outgoingDiff
    Nothing ->
      case incomingDiffAtDegree degreeValue injectiveComplex of
        Just incomingDiff -> bmRows incomingDiff
        Nothing -> emptyAxis

denseGF2ToDouble :: DenseMat GF2 -> DenseMat Double
denseGF2ToDouble DenseMat {dmRows, dmCols, dmData} =
  DenseMat
    { dmRows = dmRows
    , dmCols = dmCols
    , dmData =
        V.map
          (V.map (\entryValue -> if gf2ToBool entryValue then 1.0 else 0.0))
          dmData
    }

outgoingLocalMatrix :: DerivedPoset -> FinObjectId -> Int -> InjectiveComplex GF2 -> Maybe (DenseMat Double)
outgoingLocalMatrix posetValue cellValue degreeValue injectiveComplex =
  fmap
    (denseGF2ToDouble . starView posetValue cellValue)
    (diffAtDegree degreeValue injectiveComplex)

incomingLocalMatrix :: DerivedPoset -> FinObjectId -> Int -> InjectiveComplex GF2 -> Maybe (DenseMat Double)
incomingLocalMatrix posetValue cellValue degreeValue injectiveComplex =
  fmap
    ( transposeMat
        . denseGF2ToDouble
        . starView posetValue cellValue
        . transposeBlockedMat
    )
    (incomingDiffAtDegree degreeValue injectiveComplex)

localAxisSize :: DerivedPoset -> FinObjectId -> Int -> InjectiveComplex GF2 -> Int
localAxisSize posetValue cellValue degreeValue injectiveComplex =
  axisSize
    (restrictAxis (star posetValue cellValue) (axisAtDegree degreeValue injectiveComplex))

localSheafLaplacian :: DerivedPoset -> HomologicalDegree -> FinObjectId -> Derived GF2 -> DenseMat Double
localSheafLaplacian posetValue (HomologicalDegree degreeValue) cellValue Derived {getDerived = injectiveComplex} =
  let localDimension = localAxisSize posetValue cellValue degreeValue injectiveComplex
      leftTerm =
        case incomingLocalMatrix posetValue cellValue degreeValue injectiveComplex of
          Nothing -> zeroMat localDimension localDimension
          Just incomingMatrix ->
            matMul incomingMatrix (transposeMat incomingMatrix)
      rightTerm =
        case outgoingLocalMatrix posetValue cellValue degreeValue injectiveComplex of
          Nothing -> zeroMat localDimension localDimension
          Just outgoingMatrix ->
            matMul (transposeMat outgoingMatrix) outgoingMatrix
   in matAdd leftTerm rightTerm

frobeniusNorm :: DenseMat Double -> Double
frobeniusNorm DenseMat{dmData} =
  sqrt
    ( V.sum
        ( V.map
            (V.sum . V.map (\entryValue -> entryValue * entryValue))
            dmData
        )
    )

pruningTolerance :: DenseMat Double -> Double
pruningTolerance matrixValue =
  max 1.0e-12
    (1.0e-12 * frobeniusNorm matrixValue / fromIntegral (max 1 (dmRows matrixValue)))

clampEigenvalue :: Double -> Double -> Double
clampEigenvalue toleranceValue eigenvalueValue
  | abs eigenvalueValue <= toleranceValue = 0.0
  | eigenvalueValue < 0.0 = 0.0
  | otherwise = eigenvalueValue

minimumPositiveEigenvalue :: Double -> [Double] -> Double
minimumPositiveEigenvalue toleranceValue eigenvalueValues =
  case filter (> toleranceValue) eigenvalueValues of
    [] -> 0.0
    firstValue : remainingValues ->
      foldr min firstValue remainingValues

pruningGapOfSymmetricDenseMat :: DenseMat Double -> Either MoonlightError Double
pruningGapOfSymmetricDenseMat localLaplacian = do
  eigenpairs <-
    symmetricEigenPairs
      (dmRows localLaplacian)
      (fmap V.toList (V.toList (dmData localLaplacian)))
  let toleranceValue = pruningTolerance localLaplacian
      eigenvalues =
        fmap
          (clampEigenvalue toleranceValue . fst)
          eigenpairs
  Right
    ( if null eigenvalues || any (<= toleranceValue) eigenvalues
        then 0.0
        else minimumPositiveEigenvalue toleranceValue eigenvalues
    )

pruningGapAt :: DerivedPoset -> HomologicalDegree -> FinObjectId -> Derived GF2 -> Either MoonlightError Double
pruningGapAt posetValue degreeValue cellValue derivedComplex =
  pruningGapOfSymmetricDenseMat
    (localSheafLaplacian posetValue degreeValue cellValue derivedComplex)

laplacianGate ::
  Double ->
  (seed -> FinObjectId) ->
  HomologicalDegree ->
  DerivedPoset ->
  Derived GF2 ->
  seed ->
  Either MoonlightError Bool
laplacianGate thresholdValue projectCell degreeValue posetValue derivedComplex seedValue
  | thresholdValue <= 0.0 = Right True
  | otherwise =
      fmap
        (< thresholdValue)
        ( pruningGapAt
            posetValue
            degreeValue
            (projectCell seedValue)
            derivedComplex
        )
