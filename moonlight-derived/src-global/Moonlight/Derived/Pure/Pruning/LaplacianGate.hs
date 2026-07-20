{-# LANGUAGE NamedFieldPuns #-}

-- | Spectral pruning over a real @0/1@ lift of local GF2 differentials.
-- 'denseGF2ToDouble' preserves entries but not field rank: the real rank may
-- exceed the GF2 rank, so a trivial real harmonic kernel does not certify
-- trivial GF2 cohomology. The gate deliberately trades fidelity for speed and
-- may prune cells carrying genuine GF2 cohomology. A conservative alternative
-- computes the exact GF2 kernel dimension and uses the real gap only to break
-- ties.
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

-- | Compute the local real-lifted Laplacian gap; a positive gap certifies only
-- a trivial real kernel, not trivial GF2 cohomology.
pruningGapAt :: DerivedPoset -> HomologicalDegree -> FinObjectId -> Derived GF2 -> Either MoonlightError Double
pruningGapAt posetValue degreeValue cellValue derivedComplex =
  pruningGapOfSymmetricDenseMat
    (localSheafLaplacian posetValue degreeValue cellValue derivedComplex)

-- | Keep gaps below the threshold and prune larger ones using the speed-biased
-- real lift; exact GF2 kernel dimension with this gap as a tie-break is the
-- conservative alternative.
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
